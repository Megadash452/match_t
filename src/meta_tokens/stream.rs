use std::{collections::VecDeque, ops::{Deref, DerefMut}, str::FromStr as _};

use proc_macro2::Group;
use quote::{quote, ToTokens as _};

use super::*;

pub struct MetaTokenStream(Vec<MetaToken>);
impl MetaTokenStream {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    /// Converts the [`MetaTokenStream`] back to a normal Rust [`TokenStream`].
    ///
    /// The **metavariables** (if any) are resolved to the **concrete type** that is passed in.
    fn to_token_stream(&self, ty: &Type) -> TokenStream {
        let mut stream = TokenStream::new();
        self.to_tokens(ty, &mut stream);
        stream
    }

    /// Same as [`Self::to_token_stream()`], but appends the tokens directly to an existing [`TokenStream`].
    pub fn to_tokens(&self, ty: &Type, stream: &mut TokenStream) {
        for meta_token in self.0.iter() {
            match meta_token {
                MetaToken::Group {
                    delim,
                    span,
                    tokens,
                } => {
                    let mut inner_stream = TokenStream::new();
                    tokens.to_tokens(ty, &mut inner_stream);
                    let mut group = Group::new(*delim, inner_stream);
                    group.set_span(span.join());
                    group.to_tokens(stream);
                },
                MetaToken::MetaCast(MetaCast { expr, ty: ty_tokens, t, cast_ty, .. }) => {
                    let (from_ty, to_ty);
                    let generic_ty = ty_tokens.to_token_stream(&Type::Verbatim(TokenStream::from_str(t).unwrap()));
                    let resolved_ty = ty_tokens.to_token_stream(ty);
                    match cast_ty {
                        MetaCastType::GenericToConcrete => {
                            from_ty = generic_ty;
                            to_ty = resolved_ty;
                        },
                        MetaCastType::ConcreteToGeneric => {
                            from_ty = resolved_ty;
                            to_ty = generic_ty;
                        },
                    }

                    let value = expr.to_token_stream(ty);
                    
                    /* SAFETY: We know that `T` is the **resolved_ty** because we checked it with TypeId.
                               So casting something like `[T]` to `[resolved_ty]` is safe. */
                    quote! { unsafe { ::std::mem::transmute::<#from_ty, #to_ty>(#value) } }.to_tokens(stream);
                },
                MetaToken::MetaVar { .. } => clear_span(ty.to_token_stream()).to_tokens(stream),
                MetaToken::Ident(ident) => ident.to_tokens(stream),
                MetaToken::Lit(lit) => lit.to_tokens(stream),
                MetaToken::Punct(punct) => punct.to_tokens(stream),
            }
        }
    }
}
impl IntoIterator for MetaTokenStream {
    type Item = MetaToken;
    type IntoIter = <Vec<MetaToken> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl PartialEq for MetaTokenStream {
    fn eq(&self, other: &Self) -> bool {
        self.iter_bfs()
            .zip(other.iter_bfs())
            .all(|(this, other)| this == other)
    }
}
impl Deref for MetaTokenStream {
    type Target = Vec<MetaToken>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for MetaTokenStream {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl From<Vec<MetaToken>> for MetaTokenStream {
    fn from(value: Vec<MetaToken>) -> Self {
        Self(value)
    }
}
impl Display for MetaTokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for meta_token in &self.0 {
            <MetaToken as Display>::fmt(meta_token, f)?;
        }
        Ok(())
    }
}
impl Debug for MetaTokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            f.write_str("<empty>")
        } else {
            f.debug_list()
                .entries(&self.0)
                .finish()
        }
    }
}

pub fn clear_span(stream: TokenStream) -> TokenStream {
    let mut tokens = TokenStream::new();

    for tt in stream {
        match tt {
            TokenTree::Group(group) => {
                let new_inner = clear_span(group.stream());
                Group::new(group.delimiter(), new_inner).to_tokens(&mut tokens);
            },
            mut tt => {
                tt.set_span(Span::call_site());
                tt.to_tokens(&mut tokens);
            },
        }
    }

    tokens
}

/// An iterator implementing the **Breadth First Search** algorithm.
/// 
/// The iterator can be over [`MetaToken`] or [`TokenTree`].
pub struct TokensIterBfs<T: IterBfsElement> {
    /* Apparently a Vec would perform better than a LinkedList in this case.
    https://stackoverflow.com/questions/40848918/are-there-queue-and-stack-collections-in-rust */
    queue: VecDeque<T>,
}
impl<T: IterBfsElement> TokensIterBfs<T> {
    pub fn new(token: T) -> Self {
        let mut queue = VecDeque::new();
        // Step 1: Enqueue the root.
        queue.push_back(token);
        Self { queue }
    }
    /// Same as [`Self::new()`], but allows passing in multiple tokens.
    pub fn new_multi(tokens: impl IntoIterator<Item = T>) -> Self {
        let mut queue = VecDeque::new();
        // Step 1: Enqueue the root.
        queue.extend(tokens);
        Self { queue }
    }
}
impl<T: IterBfsElement> Iterator for TokensIterBfs<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        // Step 2: Get next from queue.
        let popped = self.queue.pop_front();
        if let Some(popped) = &popped {
            // Step 3: Enqueue its children.
            popped.append_children(&mut self.queue);
        }
        popped
    }
}
pub trait IterBfsElement: Sized {
    fn append_children(&self, queue: &mut VecDeque<Self>);
}
impl IterBfsElement for &MetaToken {
    fn append_children(&self, queue: &mut VecDeque<Self>) {
        match self {
            MetaToken::Group { tokens, .. } => queue.extend(tokens.0.iter()),
            MetaToken::MetaCast(metacast) => queue.extend(metacast.ty.0.iter()),
            // No children
            MetaToken::MetaVar(_) => { },
            MetaToken::Ident(_) => { },
            MetaToken::Punct(_) => { },
            MetaToken::Lit(_) => { },
        }
    }
}
impl IterBfsElement for TokenTree {
    fn append_children(&self, queue: &mut VecDeque<Self>) {
        match self {
            TokenTree::Group(group) => queue.extend(group.stream().into_iter()),
            // No children
            TokenTree::Ident(_) => { },
            TokenTree::Punct(_) => { },
            TokenTree::Literal(_) => { },
        }
    }
}

pub trait ToIterBfs<'a, T: IterBfsElement + 'a> {
    fn iter_bfs(&'a self) -> TokensIterBfs<T>;
}
impl<'a> ToIterBfs<'a, &'a MetaToken> for MetaTokenStream {
    fn iter_bfs(&'a self) -> TokensIterBfs<&'a MetaToken> {
        TokensIterBfs::new_multi(self.0.iter())
    }
}
impl<'a> ToIterBfs<'a, Self> for &'a MetaToken {
    fn iter_bfs(&'a self) -> TokensIterBfs<Self> {
        TokensIterBfs::new(self)
    }
}
impl ToIterBfs<'_, TokenTree> for TokenStream {
    fn iter_bfs(&self) -> TokensIterBfs<TokenTree> {
        TokensIterBfs::new_multi(self.clone().into_iter())
    }
}
impl ToIterBfs<'_, TokenTree> for TokenTree {
    fn iter_bfs(&self) -> TokensIterBfs<TokenTree> {
        TokensIterBfs::new(self.clone())
    }
}
