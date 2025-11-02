use crate::meta_tokens::stream::ToIterBfs as _;

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

/// Converts all tokens in a [`TokenStream`] into [`MetaToken`].
/// 
/// **parsing_type** is set to `true` if a [`MetaCastType`] is being parsed
/// and should return [`Err`] if another *cast* (`as` keyword) is found.
///
/// Returns [`Err`] if more than 1 **metavariable** names are found.
pub fn parse_all(stream: TokenStream, parsing_type: bool) -> syn::Result<MetaExprInner> {
    let mut tokens = MetaTokenStream::new();
    let mut metavar_name = None::<String>;
    fn set_metavar_name(metavar_name: &mut Option<String>, obtained_name: Option<&Ident>) -> syn::Result<()> {
        match (&metavar_name, obtained_name) {
            // Parsed tokens returned with a metavar_name different from the one set in the current frame.
            (Some(metavar_name), Some(obtained_name)) => if obtained_name != metavar_name {
                return Err(syn::Error::new(
                    obtained_name.span(),
                    format!("Only one Metavariable/Generic Type name can be used within a branch. Found ${obtained_name} while ${metavar_name} exists.")
                ));
            },
            // The metavar_name has not yet been set in the current frame. INherit it from the tokens just parsed.
            (None, Some(obtained_name)) => *metavar_name = Some(obtained_name.to_string()),
            // There is nothing to compare the current metavar_name with, so do nothing.
            (_, None) => { }
        }
        Ok(())
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
        tokens.push(last_token);
        // Keep last token to append later
        if value_tokens.is_empty() {
            return Err(syn::Error::new(as_token_span, "Error parsing Cast value tokens."));
        }

        Ok(value_tokens)
    }

    let mut token_iter = stream.into_iter();

    while let Some(tt) = token_iter.next() {
        match tt {
            TokenTree::Group(group) => {
                let inner_expr = utils::parse_all(group.stream(), parsing_type)?;
                let obtained_name = inner_expr.metavar_name
                    .as_ref()
                    .map(|str| Ident::new(&str, Span::call_site()));

                // This frame inherits the inner MetaExpr's metavar_name (unless it already has one)
                set_metavar_name(&mut metavar_name, obtained_name.as_ref())?;

                tokens.push(MetaToken::Group {
                    delim: group.delimiter(),
                    span: group.delim_span(),
                    tokens: inner_expr.tokens,
                })
            },
            TokenTree::Punct(punct) => {
                // Speculatively advance cursor
                if punct.as_char() == '$'
                && let Some(TokenTree::Ident(ident)) = token_iter.clone().next() {
                    // Matched a metavariable! Push it to tokens and advance Cursor.
                    token_iter.next().unwrap();

                    set_metavar_name(&mut metavar_name, Some(&ident))?;

                    tokens.push(MetaToken::MetaVar {
                        dollar: syn::token::Dollar { spans: [punct.span()] },
                        t: ident,
                    });
                } else {
                    // Did not match a metavariable, rerun iteration with the same tokens
                    // (do not advance cursor)
                    tokens.push(MetaToken::Punct(punct))
                }
            },
            TokenTree::Ident(ident) => {
                if ident == "as" {
                    if parsing_type {
                        return Err(syn::Error::new(ident.span(), "Casts are not allowed within the Type of another Cast"));
                    }

                    // Cast Type turns out to be a Rust type with the generic.
                    // Parse tokens for the Type and look for the Generic Ident within the Type.
                    if let Ok(ty) = syn::parse2::<Type>(token_iter.clone().collect()) {
                        // If a metavariable_name has been set for this frame, look for the generic type within the Type
                        let generic = if let Some(name) = &metavar_name {
                            // Look for an Ident with the same name as the metavariable within the parsed tokens.
                            let mut obtained_name = None;
                            for token in ty.to_token_stream().iter_bfs() {
                                if let TokenTree::Ident(ident) = token
                                && ident == name {
                                    obtained_name = Some(ident);
                                    break;
                                }
                            }
                            // Set the metavar name for the current frame
                            set_metavar_name(&mut metavar_name, obtained_name.as_ref())?;

                            match obtained_name {
                                Some(t) => t,
                                None => {
                                    // An Ident with the same name as the metavariable was NOT found in Type, so these tokens do not count as MetaCast.
                                    // Ignore anything that was parsed, just add the 'as' token and move on.
                                    tokens.push(MetaToken::Ident(ident));
                                    continue;
                                }
                            }
                        } else {
                            // MetaVar name has not been set in this frame, so don't know what to look for....
                            todo!();
                            let a = 5;
                        };

                        // Advance tokens to the length of the parsed tokens
                        for _ in 0..ty.to_token_stream().into_iter().count() {
                            token_iter.next().unwrap();
                        }

                        let token = MetaToken::MetaCast {
                            expr: parse_metacast_value(&mut tokens, ident.span())?,
                            as_token: syn::token::As { span: ident.span() },
                            ty: utils::parse_all(ty.to_token_stream(), true)?.tokens,
                            t: generic.to_string(),
                            cast_ty: MetaCastType::ConcreteToGeneric,
                        };
                        tokens.push(token);
                    } else {
                        // syn::Type failed to parse, so the Type may contain a metavariable. It must be parsed manually.
                        let mut fork = token_iter.clone();
                        let ty_tokens = parse_metatype(&mut fork)
                            .map_err(|err| syn::Error::new(err.span(), format!("Error parsing MetaType: {err}")))?;

                        // Look for the MetaVar (obtained_name) within the parsed tokens.
                        let mut obtained_name = None;
                        for token in ty_tokens.iter_bfs() {
                            if let MetaToken::MetaVar { t, .. } = token {
                                obtained_name = Some(t.clone());
                                break;
                            }
                        }
                        // Set the metavar name for the current frame
                        set_metavar_name(&mut metavar_name, obtained_name.as_ref())?;

                        let metavar_name = match obtained_name {
                            Some(name) => name.to_string(),
                            // If a Metavar was not found in the tokens, it means the user provided invalid tokens (assumption, not verified)
                            None => {
                                // Ignore anything that was parsed, just add the 'as' token and move on.
                                tokens.push(MetaToken::Ident(ident));
                                continue;
                            }
                        };

                        // Advance tokens to the length of the parsed tokens
                        for _ in 0..ty_tokens.len() {
                            token_iter.next().unwrap();
                        }

                        let token = MetaToken::MetaCast {
                            expr: parse_metacast_value(&mut tokens, ident.span())?,
                            as_token: syn::token::As { span: ident.span() },
                            ty: ty_tokens,
                            t: metavar_name,
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

    Ok(MetaExprInner {
        tokens,
        metavar_name,
    })
}

/// MetaTypes (used in [`MetaCast`][MetaToken::MetaCast]) are recursive, so a separate function is required.
/// 
/// This basically tries to emulate the behavior of [`syn::Type`]'s [`parse()`][syn::Type::parse()] to guess where it should stop parsing.
/// 
/// Returns [`Err`] if the parsed tokens are not a [`MetaCastType`].
/// That is, the tokens don't contain a [`MetaVar`].

// TODO: make unit tests for this garbage 🤭.
pub fn parse_metatype<I: Iterator<Item = TokenTree> + Clone>(token_iter: &mut I) -> syn::Result<MetaTokenStream> {
    // ZAMNNN this really is some spaghetti code. Could it be worse than yanderedev?
    fn parse_angle_bracket_group<I: Iterator<Item = TokenTree> + Clone>(token_iter: &mut I) -> syn::Result<MetaTokenStream> {
        let mut tokens = MetaTokenStream::new();
        let mut fork = token_iter.clone();

        match fork.next() {
            // Treat angle brackets as acceptable Group.
            Some(TokenTree::Punct(punct)) if punct.as_char() == '<' => {
                token_iter.next().unwrap();
                // Advance until the closing angle bracket (while pushing tokens)
                tokens.push(MetaToken::Punct(punct));
                let mut levels: usize = 1;

                while let Some(tt) = token_iter.next() {
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

                    tokens.push(unsafe {  MetaToken::from_tt(tt, true)? });
                }

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

    fn parse_path<I: Iterator<Item = TokenTree> + Clone>(token_iter: &mut I) -> syn::Result<MetaTokenStream> {
        let mut tokens = MetaTokenStream::new();

        let mut fork = token_iter.clone();

        while let Some(tt) = fork.next() {
            match tt {
                TokenTree::Group(group) => return Err(syn::Error::new(group.span(), format!("Grouped tokens (with delimiter {:?}) are not allowed in a Type Path.", group.delimiter()))),
                TokenTree::Punct(punct) => match punct.as_char() {
                    // Treat angle brackets as acceptable Group.
                    '<' => tokens.extend(parse_angle_bracket_group(token_iter)?),
                    '>' => return Err(syn::Error::new(punct.span(), "Found an unpaired closing angle bracket (>).")),
                    ':' if punct.spacing() == Spacing::Joint
                    && matches!(fork.next(), Some(TokenTree::Punct(punct)) if punct.spacing() == Spacing::Alone) => {
                        // Found '::' for path, pusho tokens and keep parsing
                        for _ in 0..2 {
                            tokens.push(unsafe { MetaToken::from_tt(token_iter.next().unwrap(), true)? });
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

        Ok(tokens)
    }

    fn parse_fn<I: Iterator<Item = TokenTree> + Clone>(token_iter: &mut I) -> syn::Result<MetaTokenStream> {
        let mut tokens = MetaTokenStream::new();

        if let Some(TokenTree::Ident(ident)) = token_iter.clone().next()
        && ident == "for" {
            token_iter.next().unwrap();
            tokens.push(MetaToken::Ident(ident));

            tokens.extend(parse_angle_bracket_group(token_iter)?);
        }

        if let Some(TokenTree::Ident(ident)) = token_iter.clone().next()
        && ident == "unsafe" {
            token_iter.next().unwrap();
            tokens.push(MetaToken::Ident(ident));
        }

        if let Some(TokenTree::Ident(ident)) = token_iter.clone().next()
        && ident == "extern" {
            token_iter.next().unwrap();
            
            if let Some(TokenTree::Literal(lit)) = token_iter.next()
            && lit.to_string().starts_with("\"") {
                tokens.push(MetaToken::Ident(ident));
                tokens.push(MetaToken::Lit(lit));
            } else {
                return Err(syn::Error::new(ident.span(), "Unexpected tokens: Expected string literal after 'extern' for ABI."));
            }
        }

        if let Some(TokenTree::Ident(ident)) = token_iter.clone().next()
        && ident == "fn" {
            token_iter.next().unwrap();
            tokens.push(MetaToken::Ident(ident));
        } else {
            let span = match token_iter.next() {
                Some(tt) => tt.span(),
                None => Span::call_site(),
            };
            return Err(syn::Error::new(span, "Unexpected tokens: Expected 'fn'"));
        }

        if let Some(TokenTree::Group(group)) = token_iter.clone().next()
        && group.delimiter() == Delimiter::Parenthesis {
            tokens.push(unsafe { MetaToken::from_tt(token_iter.next().unwrap(), true)? });
        } else {
            let span = match token_iter.next() {
                Some(tt) => tt.span(),
                None => Span::call_site(),
            };
            return Err(syn::Error::new(span, "Unexpected tokens: Expected parameters Parenthesis (...)"));
        }

        let mut fork = token_iter.clone();
        if let Some(TokenTree::Punct(punct)) = fork.next()
        && punct.as_char() == '-' && punct.spacing() == Spacing::Joint
        && matches!(fork.next(), Some(TokenTree::Punct(punct)) if punct.as_char() == '>') {
            for _ in 0..2 {
                tokens.push(unsafe { MetaToken::from_tt(token_iter.next().unwrap(), true)? })
            }

            tokens.extend(
                parse_metatype(token_iter)
                    .map_err(|err| syn::Error::new(err.span(), format!("Error parsing return Type of bare fn: {err}")))?
            );
        }

        Ok(tokens)
    }

    // WARNING: may not be filled with parsed tokens
    let mut tokens = MetaTokenStream::new();

    let mut fork = token_iter.clone();

    // Peek tokens to see which function is the parser
    match fork.next() {
        Some(tt) => match tt {
            // Found group: only the group is the type.
            TokenTree::Group(group) => utils::parse_all(group.stream(), true)
                .map(|meta_expr| meta_expr.tokens),
            TokenTree::Ident(ident) => match ident.to_string().as_str() {
                "for" => parse_fn(token_iter),
                "fn" => parse_fn(token_iter),
                "impl" => Err(syn::Error::new(ident.span(), "Unexpected tokens: 'impl' trait types are not valid MetaCast types.")),
                "dyn" => Err(syn::Error::new(ident.span(), "Unexpected tokens: 'dyn' trait types are not valid MetaCast types.")),
                "_" => Err(syn::Error::new(ident.span(), "Unexpected tokens: Infer type (_) is not a valid MetaCast type.")),
                _ => parse_path(token_iter),
            },
            TokenTree::Punct(punct) => match punct.as_char() {
                ':' if punct.spacing() == Spacing::Joint => match fork.next() {
                    Some(TokenTree::Punct(punct))
                    if punct.as_char() == ':' && punct.spacing() == Spacing::Alone
                        => parse_path(token_iter),
                    // This token is not part of the Type
                    _ => Err(syn::Error::new(punct.span(), "Unexpected tokens: Invalid token ':' while parsing Type."))
                },
                '$' => match fork.next() {
                    // MetaType is MetaVariable alone
                    Some(TokenTree::Ident(ident)) => {
                        // Advance token_iter to fork.
                        for _ in 0..2 {
                            token_iter.next().unwrap();
                        }

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
                    // Advance token_iter to fork.
                    token_iter.next().unwrap();
                    tokens.push(MetaToken::Punct(punct));

                    // Parse reference
                    if let Some(TokenTree::Punct(punct)) = token_iter.clone().next()
                    && punct.as_char() == '\'' {
                        token_iter.next().unwrap();

                        if let Some(TokenTree::Ident(ident)) = token_iter.next() {
                            tokens.push(MetaToken::Punct(punct));
                            tokens.push(MetaToken::Ident(ident));
                        } else {
                            return Err(syn::Error::new(punct.span(), "Unexpected tokens: Expected Ident for lifetime."));
                        }
                    }

                    if let Some(TokenTree::Ident(ident)) = token_iter.clone().next()
                    && ident == "mut" {
                        token_iter.next().unwrap();
                        tokens.push(MetaToken::Ident(ident));
                    }

                    // RECURSION HERE vvv
                    tokens.extend(parse_metatype(token_iter)?);

                    Ok(tokens)
                },
                '*' if punct.spacing() == Spacing::Alone => {
                    // Advance token_iter to fork.
                    token_iter.next().unwrap();
                    tokens.push(MetaToken::Punct(punct));

                    // Parse raw pointer
                    if let Some(TokenTree::Ident(ident)) = token_iter.clone().next()
                    && ident == "const" {
                        token_iter.next().unwrap();
                        tokens.push(MetaToken::Ident(ident));
                    }

                    if let Some(TokenTree::Ident(ident)) = token_iter.clone().next()
                    && ident == "mut" {
                        token_iter.next().unwrap();
                        tokens.push(MetaToken::Ident(ident));
                    }

                    // RECURSION HERE vvv
                    tokens.extend(parse_metatype(token_iter)?);

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
