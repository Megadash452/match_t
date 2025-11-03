use std::borrow::Cow;

use super::*;
use quote::ToTokens as _;

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

fn advance_by(iter: &mut impl Iterator, i: usize) {
    for _ in 0..i {
        let _ = iter.next();
    }
}

/// Converts all tokens in a [`TokenStream`] into [`MetaToken`].
/// 
/// **metavar_name** is the name of the [`MetaVariable`] that will be resolved to a concrete type.
/// An [`Err`] will be returned if any [`MetaVariable`]s are found with a *different name*.
/// If **metavar_name** is [`None`], the value will be set to the [`MetaVariable`]'s name that is found (if any).
/// 
/// **parsing_type** is set to `true` if a [`MetaCastType`] is being parsed
/// and should return [`Err`] if another *cast* (`as` keyword) is found.
///
/// [`MetaVariable`]: MetaToken::MetaVar
pub fn parse_as_metatokens(stream: TokenStream, metavar_name: &mut Option<Cow<'_, str>>) -> syn::Result<MetaTokenStream> {
    let mut tokens = MetaTokenStream::new();
    let mut token_iter = stream.into_iter();
    fn set_metavar_name(metavar_name: &mut Option<Cow<'_, str>>, obtained_name: &Ident) -> syn::Result<()> {
        match &metavar_name {
            // Parsed tokens returned with a metavar_name different from the one set in the current frame.
            Some(metavar_name) => if obtained_name != metavar_name {
                return Err(syn::Error::new(
                    obtained_name.span(),
                    format!("Only one Metavariable/Generic Type name can be used within a branch. Found ${obtained_name} while ${metavar_name} exists.")
                ));
            },
            // The metavar_name has not yet been set.
            None => *metavar_name = Some(Cow::Owned(obtained_name.to_string())),
        }
        Ok(())
    }

    while let Some(tt) = token_iter.next() {
        match tt {
            TokenTree::Group(group) => tokens.push(MetaToken::from_group(group, |stream| parse_as_metatokens(stream, metavar_name))?),
            TokenTree::Punct(punct) => {
                // Speculatively advance cursor
                let mut fork = token_iter.clone();
                
                if punct.as_char() == '$'
                && let Some(TokenTree::Ident(ident)) = fork.next() {
                    // Matched a metavariable! Parse the Ident next.
                    token_iter = fork;
                    set_metavar_name(metavar_name, &ident)?;

                    tokens.push(MetaToken::MetaVar {
                        dollar: syn::token::Dollar { spans: [punct.span()] },
                        t: ident,
                    });
                    continue;
                }

                // Did not match a metavariable, rerun iteration with the same tokens
                // (do not advance cursor)
                tokens.push(MetaToken::Punct(punct))
            },
            TokenTree::Ident(ident) => {
                if ident == "as" {
                    // Cast Type turns out to be a Rust type with the generic.
                    // Parse tokens for the Type and look for the Generic Ident within the Type.
                    if let Ok(ty) = syn::parse2::<Type>(token_iter.clone().collect()) {
                        let ty_tokens = ty.to_token_stream();
                        // Parse the syn::Type as MetaTokens, replacing all instances of the generic `T` with the metavariable `$T`.
                        let ty = if let Some(name) = metavar_name.as_deref() {
                            let (ty, found_generic_t) = type_to_metatokens(ty_tokens.clone(), name);
                            // If a generic with metavar_name was not found in the tokens, assume the cast is a normal cast.
                            if !found_generic_t {
                                // Ignore anything that was parsed, just add the 'as' token and move on.
                                tokens.push(MetaToken::Ident(ident));
                                continue;
                            }
                            ty
                        } else {
                            // MetaVar name has not been set, so don't know what to look for....
                            todo!()
                        };

                        // Advance tokens to the length of the parsed tokens
                        advance_by(&mut token_iter, ty_tokens.into_iter().count());

                        let token = MetaToken::MetaCast {
                            expr: parse_metacast_value(&mut tokens, ident.span())?,
                            as_token: syn::token::As { span: ident.span() },
                            ty,
                            t: todo!(),
                            cast_ty: MetaCastType::ConcreteToGeneric,
                        };
                        tokens.push(token);
                    } else {
                        // syn::Type failed to parse, so the Type may contain a metavariable. It must be parsed manually.
                        let mut fork = token_iter.clone();
                        let mut metavar_name_inner = None;
                        let ty_tokens = parse_metacast_type(&mut fork, &mut metavar_name_inner)
                            .map_err(|err| syn::Error::new(err.span(), format!("Error parsing MetaType: {err}")))?;

                        let obtained_name = match metavar_name_inner {
                            Some(name) => name.to_string(),
                            // If a Metavar was not found in the tokens, it means the user provided invalid tokens
                            None => {
                                // Ignore anything that was parsed, just add the 'as' token and move on.
                                tokens.push(MetaToken::Ident(ident));
                                continue;
                            }
                        };
                        set_metavar_name(metavar_name, &Ident::new(&obtained_name, Span::call_site()))?;

                        // Advance tokens to the length of the parsed tokens
                        token_iter = fork;

                        let token = MetaToken::MetaCast {
                            expr: parse_metacast_value(&mut tokens, ident.span())?,
                            as_token: syn::token::As { span: ident.span() },
                            ty: ty_tokens,
                            t: obtained_name,
                            cast_ty: MetaCastType::GenericToConcrete,
                        };
                        tokens.push(token);
                    }
                } else {
                    tokens.push(MetaToken::Ident(ident))
                }
            },
            TokenTree::Literal(lit) => tokens.push(MetaToken::Lit(lit)),
        }
    }

    Ok(tokens)
}

/// Take previous tokens as value until a certain token is reach, or the end.
fn parse_metacast_value(tokens: &mut MetaTokenStream, as_token_span: Span) -> syn::Result<MetaTokenStream> {
    let mut value_tokens = MetaTokenStream::new();
    // Keep last token to append later.
    // Teh loop's first element should be the seocnd to last token.
    if tokens.is_empty() {
        return Err(syn::Error::new(as_token_span, "There must be a Cast value *before* the 'as' keyword."));
    }
    let last_token = {
        let i = tokens.len() - 1;
        tokens.remove(i)
    };
    /// Whether to break or not to break. If punct is alone.
    fn is_alone(punct: &Punct, tokens: &[MetaToken]) -> bool {
        if tokens.len() < 2 {
            return true;
        }

        if punct.spacing() == Spacing::Alone
        && let Some(MetaToken::Punct(prev)) = tokens.last_chunk::<2>().map(|array| array.first().unwrap())
        && prev.as_char() == punct.as_char() && prev.spacing() == Spacing::Joint {
            false
        } else {
            true
        }
    }
    while let Some(token) = tokens.last() {
        match token {
            MetaToken::Group { delim, .. } => match delim {
                Delimiter::Brace => break,
                _ => { },
            },
            MetaToken::Punct(punct) => match punct.as_char() {
                ':' if is_alone(punct, &tokens) => break,
                '=' if is_alone(punct, &tokens) => break,
                '|' if is_alone(punct, &tokens) => break,
                ';' => break,
                _ => { },
            },
            MetaToken::MetaVar { .. } => { },
            MetaToken::MetaCast { .. } => { },
            MetaToken::Ident(_) => { },
            MetaToken::Lit(_) => { },
        }

        value_tokens.push({
            let i = tokens.len() - 1;
            tokens.remove(i)
        });
    }
    // Put back last token
    value_tokens.push(last_token);

    Ok(value_tokens)
}

/// Parses a [`syn::Type`]'s tokens into a [`MetaTokenStream`].
/// 
/// Since a [`syn::Type`] does not contain any [`Metavariable`]s or [`MetaCast`][MetaToken::MetaCast],
/// this function will not look for them and just take the [`TokenTree`] as-is.
/// 
/// This function converts all instances of the *generic type* `T` into [`MetaVariable`]s
/// so that the *generic type* `T` can be later replaced to some *concrete type* for the cast.
/// 
/// Returns the [`MetaTokenStream`] and whether an instance of the *generic type* `T` was found,
/// and thus whether the [`MetaTokenStream`] con.
/// 
/// [`MetaVariable`]: MetaToken::MetaVar
fn type_to_metatokens(ty: TokenStream, metavar_name: &str) -> (MetaTokenStream, bool) {
    let mut tokens = MetaTokenStream::new();
    let mut token_iter = ty.to_token_stream().into_iter();
    let mut found_generic_t = false;

    while let Some(tt) = token_iter.next() {
        tokens.push(match tt {
            TokenTree::Group(group) => MetaToken::Group {
                delim: group.delimiter(),
                span: group.delim_span(),
                tokens: {
                    let (tokens, found) = type_to_metatokens(group.stream(), metavar_name);
                    if found {
                        found_generic_t = true;
                    }
                    tokens
                },
            },
            TokenTree::Ident(ident) => {
                if ident == metavar_name {
                    found_generic_t = true;
                    MetaToken::MetaVar {
                        dollar: syn::token::Dollar { spans: [ident.span()] },
                        t: ident,
                    }
                } else {
                    MetaToken::Ident(ident)
                }
            },
            TokenTree::Punct(punct) => MetaToken::Punct(punct),
            TokenTree::Literal(lit) => MetaToken::Lit(lit),
        })
    }

    (tokens, found_generic_t)
}

/// MetaTypes (used in [`MetaCast`][MetaToken::MetaCast]) are recursive, so a separate function is required.
/// 
/// This basically tries to emulate the behavior of [`syn::Type`]'s [`parse()`][syn::Type::parse()] to guess where it should stop parsing.
/// 
/// Returns [`Err`] if the parsed tokens are not a [`MetaCastType`].
/// That is, the tokens don't contain a [`MetaVar`].
fn parse_metacast_type<I: Iterator<Item = TokenTree> + Clone>(token_iter: &mut I, metavar_name: &mut Option<Cow<'_, str>>) -> syn::Result<MetaTokenStream> {
    // ZAMNNN this really is some spaghetti code. Could it be worse than yanderedev?
    fn parse_angle_bracket_group<I: Iterator<Item = TokenTree> + Clone>(token_iter: &mut I, metavar_name: &mut Option<Cow<'_, str>>) -> syn::Result<MetaTokenStream> {
        let mut fork = token_iter.clone();

        match fork.next() {
            // Treat angle brackets as acceptable Group.
            Some(TokenTree::Punct(punct)) if punct.as_char() == '<' => {
                let mut tokens = MetaTokenStream::new();
                tokens.push(MetaToken::Punct(punct));
                // This stores the tokens collected from within the angle bracketed group <...>
                let mut group_tokens = TokenStream::new();

                // Advance until the closing angle bracket (while pushing tokens)
                let mut levels: usize = 1;

                while let Some(tt) = fork.next() {
                    match &tt {
                        TokenTree::Punct(punct) if punct.as_char() == '<' => {
                            // Add opening angle bracket to the stack
                            levels += 1;
                        },
                        TokenTree::Punct(punct) if punct.as_char() == '>' => {
                            // Take closing angle bracket from the stack
                            levels -= 1;
                            if levels == 0 {
                                break;
                            }
                        },
                        _ => { }
                    }

                    tt.to_tokens(&mut group_tokens);
                }

                // Parse the collected tokens
                tokens.extend(parse_inner(group_tokens, metavar_name)?);

                *token_iter = fork;

                Ok(tokens)
            },
            tt => {
                let span = match tt {
                    Some(tt) => tt.span(),
                    None => Span::call_site(),
                };
                Err(syn::Error::new(span, "Unexpected tokens: Expected opening angle bracket (<) for Group."))
            },
        }
    }

    /// Can also return tokens of a macro
    fn parse_path<I: Iterator<Item = TokenTree> + Clone>(token_iter: &mut I, metavar_name: &mut Option<Cow<'_, str>>) -> syn::Result<MetaTokenStream> {
        let mut tokens = MetaTokenStream::new();

        let mut fork = token_iter.clone();

        while let Some(tt) = fork.next() {
            match tt {
                TokenTree::Group(group) => return Err(syn::Error::new(group.span(), format!("Grouped tokens (with delimiter {:?}) are not allowed in a Type Path.", group.delimiter()))),
                TokenTree::Punct(punct) => match punct.as_char() {
                    // Treat angle brackets as acceptable Group.
                    '<' => tokens.extend(parse_angle_bracket_group(&mut fork, metavar_name)?),
                    '>' => return Err(syn::Error::new(punct.span(), "Found an unpaired closing angle bracket (>).")),
                    ':' if punct.spacing() == Spacing::Joint => {
                        if let Some(TokenTree::Punct(punct_next)) = fork.next()
                        && punct.spacing() == Spacing::Alone {
                            // Found '::' for path, push tokens and keep parsing
                            tokens.push(MetaToken::Punct(punct));
                            tokens.push(MetaToken::Punct(punct_next));
                        } else {
                            break;
                        }
                    },
                    // Found macro
                    '!' => match fork.next() {
                        Some(TokenTree::Group(group)) => {
                            tokens.push(MetaToken::from_group(group, |stream| parse_inner(stream, metavar_name))?);
                            *token_iter = fork;
                            return Ok(tokens)
                        },
                        tt => {
                            let span = match tt {
                                Some(tt) => tt.span(),
                                None => Span::call_site(),
                            };
                            return Err(syn::Error::new(span, "Unexpected tokens: Expected Delimiters for macro."));
                        }
                    },
                    // Unknown token; not part of Type. Stop parsing :D
                    _ => break,
                },
                // Found Ident for path, pusho tokens and keep parsing
                TokenTree::Ident(ident) => tokens.push(MetaToken::Ident(ident)),
                // Unknown token; not part of Type. Stop parsing :D
                TokenTree::Literal(_) => break,
            }
        }

        *token_iter = fork;

        Ok(tokens)
    }

    fn parse_fn<I: Iterator<Item = TokenTree> + Clone>(token_iter: &mut I, metavar_name: &mut Option<Cow<'_, str>>) -> syn::Result<MetaTokenStream> {
        let mut tokens = MetaTokenStream::new();
        let mut fork = token_iter.clone();

        if let Some(TokenTree::Ident(ident)) = fork.clone().next()
        && ident == "for" {
            fork.next().unwrap();
            tokens.push(MetaToken::Ident(ident));
            tokens.extend(parse_angle_bracket_group(&mut fork, metavar_name)?);
        }

        if let Some(TokenTree::Ident(ident)) = fork.clone().next()
        && ident == "unsafe" {
            fork.next().unwrap();
            tokens.push(MetaToken::Ident(ident));
        }

        if let Some(TokenTree::Ident(ident)) = fork.clone().next()
        && ident == "extern" {
            fork.next().unwrap();
            if let Some(TokenTree::Literal(lit)) = fork.next()
            && lit.to_string().starts_with("\"") {
                tokens.push(MetaToken::Ident(ident));
                tokens.push(MetaToken::Lit(lit));
            } else {
                return Err(syn::Error::new(ident.span(), "Unexpected tokens: Expected string literal after 'extern' for ABI."));
            }
        }

        match fork.next() {
            Some(TokenTree::Ident(ident))
            if ident == "fn" => tokens.push(MetaToken::Ident(ident)),
            tt => {
                let span = match tt {
                    Some(tt) => tt.span(),
                    None => Span::call_site(),
                };
                return Err(syn::Error::new(span, "Unexpected tokens: Expected 'fn'"));
            }
        }

        match fork.next() {
            Some(TokenTree::Group(group))
            if group.delimiter() == Delimiter::Parenthesis => tokens.push(MetaToken::from_group(group, |stream| parse_inner(stream, metavar_name))?),
            tt => {
                let span = match tt {
                    Some(tt) => tt.span(),
                    None => Span::call_site(),
                };
                return Err(syn::Error::new(span, "Unexpected tokens: Expected parameters Parenthesis (...)"));
            }
        }

        if let Some(TokenTree::Punct(dash)) = fork.clone().next()
        && dash.as_char() == '-' && dash.spacing() == Spacing::Joint {
            fork.next().unwrap();
            if let Some(TokenTree::Punct(right_bracket)) = fork.next()
            && right_bracket.as_char() == '>' && right_bracket.spacing() == Spacing::Alone {
                tokens.push(MetaToken::Punct(dash));
                tokens.push(MetaToken::Punct(right_bracket));
                // RECURSION HERE vvv
                tokens.extend(parse_metacast_type(&mut fork, metavar_name)?);
            } else {
                return Err(syn::Error::new(dash.span(), "Unexpected tokens: Expected arrow after dash for return type (...) -> Type"));
            }
        }

        *token_iter = fork;

        Ok(tokens)
    }

    /// Parses the tokens within a `MetaCast`'s type group.
    /// 
    /// This will interpret any metavariables found in the tokens,
    /// but any other tokens will take as-is.
    fn parse_inner(stream: TokenStream, metavar_name: &mut Option<Cow<'_, str>>) -> syn::Result<MetaTokenStream> {
        let mut tokens = MetaTokenStream::new();
        let mut token_iter = stream.into_iter();

        while let Some(tt) = token_iter.next() {
            tokens.push(match tt {
                TokenTree::Group(group) => MetaToken::from_group(group, |stream| parse_inner(stream, metavar_name))?,
                TokenTree::Ident(ident) => {
                    if ident == "as" {
                        return Err(syn::Error::new(ident.span(), "Casts are not allowed within the Type of another Cast"));
                    }
                    MetaToken::Ident(ident)
                },
                TokenTree::Punct(punct) => MetaToken::Punct(punct),
                TokenTree::Literal(lit) => MetaToken::Lit(lit),
            })
        }

        Ok(tokens)
    }

    let mut fork = token_iter.clone();

    // Peek tokens to see which function is the parser
    match fork.next() {
        Some(tt) => match tt {
            // Found group: only the group is the type.
            TokenTree::Group(group) => {
                *token_iter = fork;
                parse_inner(group.stream(), metavar_name)
            },
            TokenTree::Ident(ident) => match ident.to_string().as_str() {
                "for" => parse_fn(token_iter, metavar_name),
                "fn" => parse_fn(token_iter, metavar_name),
                "impl" => Err(syn::Error::new(ident.span(), "Unexpected tokens: 'impl' trait types are not valid MetaCast types.")),
                "dyn" => Err(syn::Error::new(ident.span(), "Unexpected tokens: 'dyn' trait types are not valid MetaCast types.")),
                "_" => Err(syn::Error::new(ident.span(), "Unexpected tokens: Infer type (_) is not a valid MetaCast type.")),
                _ => parse_path(token_iter, metavar_name),
            },
            TokenTree::Punct(punct) => match punct.as_char() {
                ':' if punct.spacing() == Spacing::Joint => match fork.next() {
                    Some(TokenTree::Punct(punct))
                    if punct.as_char() == ':' && punct.spacing() == Spacing::Alone
                        => parse_path(token_iter, metavar_name),
                    // This token is not part of the Type
                    _ => Err(syn::Error::new(punct.span(), "Unexpected tokens: Invalid token ':' while parsing Type."))
                },
                '$' => match fork.next() {
                    // MetaType is MetaVariable alone
                    Some(TokenTree::Ident(ident)) => {
                        *token_iter = fork;

                        Ok(MetaTokenStream::from(vec![
                            MetaToken::MetaVar {
                                dollar: syn::token::Dollar { spans: [punct.span()] },
                                t: ident,
                            },
                        ]))
                    },
                    _ => Err(syn::Error::new(punct.span(), "Error parsing MetaVariable: Expected an Ident after the '$'"))
                },
                '&' if punct.spacing() == Spacing::Alone => {
                    let mut tokens = MetaTokenStream::new();
                    tokens.push(MetaToken::Punct(punct));
                    // Parse reference

                    if let Some(TokenTree::Punct(punct)) = fork.clone().next()
                    && punct.as_char() == '\'' {
                        fork.next().unwrap();
                        if let Some(TokenTree::Ident(ident)) = fork.next() {
                            tokens.push(MetaToken::Punct(punct));
                            tokens.push(MetaToken::Ident(ident));
                        } else {
                            return Err(syn::Error::new(punct.span(), "Unexpected tokens: Expected Ident for lifetime."));
                        }
                    }

                    if let Some(TokenTree::Ident(ident)) = fork.clone().next()
                    && ident == "mut" {
                        fork.next().unwrap();
                        tokens.push(MetaToken::Ident(ident));
                    }

                    // RECURSION HERE vvv
                    tokens.extend(parse_metacast_type(&mut fork, metavar_name)?);

                    *token_iter = fork;

                    Ok(tokens)
                },
                '*' if punct.spacing() == Spacing::Alone => {
                    let mut tokens = MetaTokenStream::new();
                    tokens.push(MetaToken::Punct(punct));
                    // Parse raw pointer

                    match fork.next() {
                        Some(TokenTree::Ident(ident))
                        if ident == "const" || ident == "mut" => tokens.push(MetaToken::Ident(ident)),
                        tt => {
                            let span = match tt {
                                Some(tt) => tt.span(),
                                None => Span::call_site(),
                            };
                            return Err(syn::Error::new(span, "Unexpected tokens: Expected 'const' or 'mut' modifier for pointer"));
                        }
                    }

                    // RECURSION HERE vvv
                    tokens.extend(parse_metacast_type(&mut fork, metavar_name)?);

                    *token_iter = fork;

                    Ok(tokens)
                }
                '!' => Err(syn::Error::new(punct.span(), "Unexpected tokens: Never type (!) is not a valid MetaCast type.")),
                _ => Err(syn::Error::new(punct.span(), "Unexpected tokens: Expected a Type."))
            },
            TokenTree::Literal(lit) => Err(syn::Error::new(lit.span(), "Unexpected tokens: Expected a Type."))
        },
        _ => Err(syn::Error::new(Span::call_site(), "Unexpected end of input: Expected tokens for Type, but there are none."))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    #[test]
    fn parse_metatype_test() {
        // I HAD to make unit tests for this garbage 🤭
        let types = [
            quote! { [T; N] },
            quote! { [[$T; N]; N] },
            quote! { [T] },
            quote! { [[$T]] },
            quote! { fn(usize) -> bool },
            quote! { fn() -> $T },
            quote! { syn::Token![hello] },
            quote! { (A) },
            quote! { (A, B, C) },
            quote! { ::std::any::MyType<$T, T, A> },
            quote! { &T },
            quote! { &::std::any::MyType },
            quote! { &'local T },
            quote! { &'local mut ::std::any::MyType },
            quote! { *const T },
            quote! { *const::std::any::MyType },
            quote! { *mut ::std::any::MyType },
        ];
        for ty in types {
            parse_metacast_type(&mut ty.into_iter(), &mut None).unwrap();
        }

        let fail_types = [
            quote! { },
            quote! { impl A + B + C + 'static },
            quote! { dyn A + B + C + 'static },
            quote! { _ },
            quote! { ! },
            quote! { [T] as [hahah] },
        ];
        for ty in fail_types {
            parse_metacast_type(&mut ty.into_iter(), &mut None).unwrap_err();
        }
    }
}
