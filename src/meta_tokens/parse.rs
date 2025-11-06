use super::*;
use quote::ToTokens as _;
use syn::spanned::Spanned;

fn advance_by(iter: &mut impl Iterator, i: usize) {
    for _ in 0..i {
        let _ = iter.next();
    }
}

/// Converts all tokens in a [`TokenStream`] into [`MetaToken`].
///
/// **metavar_name** is the name of the [`MetaVar`] that will be resolved to a concrete type.
/// An [`Err`] will be returned if any [`MetaVar`]s are found with a *different name*.
pub(super) fn parse_as_metatokens(
    stream: TokenStream,
    metavar_name: &str,
) -> syn::Result<MetaTokenStream> {
    let mut tokens = MetaTokenStream::new();
    let mut token_iter = stream.into_iter();

    let mut fork = token_iter.clone();
    while let Some(tt) = fork.next() {
        match &tt {
            TokenTree::Punct(punct) if punct.as_char() == '$' => {
                if let Some(metacast) = parse_metacast(&mut token_iter, &mut tokens, metavar_name)? {
                    fork = token_iter.clone();
                    tokens.push(MetaToken::MetaCast(metacast));
                    continue;
                } else if let Some(metavar) = parse_metavar(&mut token_iter, metavar_name)? {
                    fork = token_iter.clone();
                    tokens.push(MetaToken::MetaVar(metavar));
                    continue;
                }
            },
            _ => { },
        }
        token_iter = fork.clone();

        // Default token conversion
        tokens.push(match tt {
            TokenTree::Group(group) => MetaToken::from_group(group, |stream| parse_as_metatokens(stream, metavar_name))?,
            TokenTree::Punct(punct) => MetaToken::Punct(punct),
            TokenTree::Ident(ident) => MetaToken::Ident(ident),
            TokenTree::Literal(lit) => MetaToken::Lit(lit),
        })
    }

    Ok(tokens)
}

/// Parses expression `$T`, only advancing the tokens if **successfully parsed**.
///
/// Returns [`None`] if the initial tokens is not `$T`, where T is **metavar_name**.
fn parse_metavar(
    token_iter: &mut (impl Iterator<Item = TokenTree> + Clone),
    metavar_name: &str,
) -> syn::Result<Option<MetaVar>> {
    // Speculatively advance cursor
    let mut fork = token_iter.clone();

    if let Some(TokenTree::Punct(punct)) = fork.next()
    && punct.as_char() == '$'
    // If Ident was not found, don't return error, just ignore these tokens.
    && let Some(TokenTree::Ident(ident)) = fork.next() {
        if ident == "as" {
            return Err(syn::Error::new(ident.span(), format!("Did not expect MetaCast when parsing MetaVariable '${metavar_name}'.")))
        }

        if ident != metavar_name {
            return Err(syn::Error::new(
                ident.span(),
                format!("Only one Metavariable/Generic Type name can exist within a branch. Found ${ident} while looking for ${metavar_name}")
            ));
        }

        *token_iter = fork;

        Ok(Some(MetaVar {
            dollar: syn::token::Dollar {
                spans: [punct.span()],
            },
            t: ident,
        }))
    } else {
        Ok(None)
    }
}

/// Returns [`None`] if the initial tokens are not `$as`.
pub fn parse_metacast(
    token_iter: &mut (impl Iterator<Item = TokenTree> + Clone),
    prev_tokens: &mut MetaTokenStream,
    metavar_name: &str,
) -> syn::Result<Option<MetaCast>> {
    let mut fork = token_iter.clone();

    if let Some(TokenTree::Punct(dollar)) = fork.next()
    && dollar.as_char() == '$'
    && let Some(TokenTree::Ident(ident)) = fork.next()
    && ident == "as" {
        let (ty, cast_ty) = if let Ok(ty) = syn::parse2::<Type>(fork.clone().collect()) {
            // Cast Type turns out to be a Rust type with the generic.
            // Parse tokens for the Type and look for the Generic Ident within the Type.
            let ty_tokens = ty.to_token_stream();
            // Parse the syn::Type as MetaTokens, replacing all instances of the generic `T` with the metavariable `$T`.
            let ty = type_to_metatokens(ty_tokens.clone(), metavar_name)?;

            // Advance tokens to the length of the parsed tokens
            *token_iter = fork;
            advance_by(token_iter, ty_tokens.into_iter().count());

            (ty, MetaCastType::ConcreteToGeneric)
        } else {
            // syn::Type failed to parse, so the Type may contain a metavariable. It must be parsed manually.
            let ty_tokens = parse_metacast_type(&mut fork, metavar_name)
                .map_err(|err| syn::Error::new(err.span(), format!("Error parsing MetaType: {err}")))?;

            // Advance tokens to the length of the parsed tokens
            *token_iter = fork;
            (ty_tokens, MetaCastType::GenericToConcrete)
        };

        Ok(Some(MetaCast {
            expr: parse_metacast_value(prev_tokens),
            dollar: syn::token::Dollar { spans: [dollar.span()] },
            as_token: syn::token::As { span: ident.span() },
            ty,
            t: metavar_name.to_string(),
            cast_ty,
        }))
    } else {
        Ok(None)
    }
}

/// Take previous tokens as value until a certain token is reach, or the end.
///
/// Keep in mind that this can return an **empty** [`MetaTokenStream`].
fn parse_metacast_value(tokens: &mut MetaTokenStream) -> MetaTokenStream {
    let mut value_tokens = MetaTokenStream::new();
    // Keep last token to append later.
    // The loop's first element should be the second to last token.
    if tokens.is_empty() {
        return value_tokens;
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
        && let Some(MetaToken::Punct(prev)) = tokens.last_chunk::<2>()
            .map(|array| array.first().unwrap())
        && prev.as_char() == punct.as_char()
        && prev.spacing() == Spacing::Joint {
            false
        } else {
            true
        }
    }
    while let Some(token) = tokens.last() {
        match token {
            MetaToken::Group { delim, .. } if *delim == Delimiter::Brace => break,
            MetaToken::Punct(punct) => match punct.as_char() {
                ':' if is_alone(punct, tokens) => break,
                '=' if is_alone(punct, tokens) => break,
                '|' if is_alone(punct, tokens) => break,
                '>' if is_alone(punct, tokens) && tokens.len() > 1 => {
                    let mut i = tokens.len() - 2;
                    let mut levels = 1u32;

                    loop {
                        match &tokens[i] {
                            MetaToken::Punct(punct) if punct.as_char() == '>' => levels += 1,
                            // Take opening angle bracket from the stack
                            MetaToken::Punct(punct) if punct.as_char() == '<' => levels -= 1,
                            _ => { }
                        }

                        if i == 0 || levels == 0 {
                            break;
                        }
                        i -= 1;
                    }

                    if levels > 0 /* i == 0 */ {
                        // Did not find matching opening bracket, so the closing bracket is actually a greater than operator.
                        // This means the whole thing is an expression, which is the metacast value.
                        let mut owned_tokens = MetaTokenStream::new();
                        std::mem::swap(&mut owned_tokens, tokens);
                        value_tokens.extend(owned_tokens);
                        break;
                    } else /* i > 0 */ {
                        // Found a matching opening angle bracket. Add the group and keep parsing.
                        let group = tokens.split_off(i);
                        value_tokens.extend(group);
                    }
                },
                ';' => break,
                ',' => break,
                _ => { },
            },
            MetaToken::Group { .. }
            | MetaToken::MetaVar { .. }
            | MetaToken::MetaCast { .. }
            | MetaToken::Ident(_)
            | MetaToken::Lit(_) => { },
        }

        value_tokens.push({
            let i = tokens.len() - 1;
            tokens.remove(i)
        });
    }
    // Put back last token
    value_tokens.push(last_token);

    value_tokens
}

/// Parses a [`syn::Type`]'s tokens into a [`MetaTokenStream`].
///
/// Since a [`syn::Type`] does not contain any [`MetaVar`]s or [`MetaCast`],
/// this function will not look for them and just take the [`TokenTree`] as-is.
///
/// This function converts all instances of the *generic type* `T` into [`MetaVar`]s
/// so that the *generic type* `T` can be later replaced to some *concrete type* for the cast.
///
/// Returns [`Err`] if no instance of the *generic type* `T` was found,
/// and thus if the [`MetaTokenStream`] does not contain [`MetaVar`]s.
fn type_to_metatokens(ty: TokenStream, metavar_name: &str) -> syn::Result<MetaTokenStream> {
    fn type_to_metatokens_main(ty: TokenStream, found_generic_t: &mut bool, metavar_name: &str) -> MetaTokenStream {
        let mut tokens = MetaTokenStream::new();
        let mut token_iter = ty.into_iter();

        while let Some(tt) = token_iter.next() {
            match tt {
                TokenTree::Group(group) => tokens.push(MetaToken::Group {
                    delim: group.delimiter(),
                    span: group.delim_span(),
                    tokens: type_to_metatokens_main(group.stream(), found_generic_t, metavar_name),
                }),
                TokenTree::Ident(ident) => {
                    // The Ident is in a TypePath if it has module separators.
                    // If it is, it is not the generic type.
                    let is_in_path = {
                        let mut fork = token_iter.clone();

                        if tokens.len() >= 2
                        && let Some(MetaToken::Punct(prev)) = tokens.last()
                        && let Some(MetaToken::Punct(prev2)) = tokens.get(tokens.len() - 2)
                        && prev.as_char() == ':'
                        && prev2.as_char() == ':'
                        && prev2.spacing() == Spacing::Joint {
                            true
                        } else if let Some(TokenTree::Punct(next)) = fork.clone().next()
                        && next.as_char() == '<' {
                            true
                        } else if let Some(TokenTree::Punct(next)) = fork.next()
                        && let Some(TokenTree::Punct(next2)) = fork.next()
                        && next.as_char() == ':'
                        && next.spacing() == Spacing::Joint
                        && next2.as_char() == ':' {
                            true
                        } else {
                            false
                        }
                    };

                    tokens.push(if ident == metavar_name && !is_in_path {
                        *found_generic_t = true;
                        MetaToken::MetaVar(MetaVar {
                            dollar: syn::token::Dollar {
                                spans: [ident.span()],
                            },
                            t: ident,
                        })
                    } else {
                        MetaToken::Ident(ident)
                    })
                },
                TokenTree::Punct(punct) => tokens.push(MetaToken::Punct(punct)),
                TokenTree::Literal(lit) => tokens.push(MetaToken::Lit(lit)),
            }
        }

        tokens
    }

    let span = ty.span();
    let mut found_generic_t = false;
    let tokens = type_to_metatokens_main(ty, &mut found_generic_t, metavar_name);

    if !found_generic_t {
        // A generic with metavar_name was not found in the tokens
        return Err(syn::Error::new(span, format!("Expected to find generic type '{metavar_name}' within the MetaCast Type.")));
    }

    Ok(tokens)
}

/// MetaTypes (used in [`MetaCast`]) are recursive, so a separate function is required.
///
/// This basically tries to emulate the behavior of [`syn::Type`]'s [`parse()`][syn::Type::parse()] to guess where it should stop parsing.
///
/// This function will convert [`TokenTree`] to [`MetaToken`] as is (except for [`MetaVar`], that will be parsed).
/// This is to say that this function WILL NOT look for instances of the **generic type** `T` with *metavar_name*.
/// For this behavior, use [`type_to_metatokens()`].
///
/// Returns [`Err`] if the parsed tokens could not be parsed into a [`Type`] with [`MetaVar`]s.
/// Also returns [`Err`] if there are not [`MetaVar`]s in the parsed tokens.
fn parse_metacast_type(
    token_iter: &mut (impl Iterator<Item = TokenTree> + Clone),
    metavar_name: &str,
) -> syn::Result<MetaTokenStream> {
    // ZAMNNN this really is some spaghetti code. Could it be worse than yanderedev?
    fn parse_angle_bracket_group(
        token_iter: &mut (impl Iterator<Item = TokenTree> + Clone),
        found_metavar: &mut bool,
        metavar_name: &str,
    ) -> syn::Result<MetaTokenStream> {
        let mut fork = token_iter.clone();

        match fork.next() {
            // Treat angle brackets as acceptable Group.
            Some(TokenTree::Punct(punct)) if punct.as_char() == '<' => {
                let mut tokens = MetaTokenStream::new();
                tokens.push(MetaToken::Punct(punct));
                // This stores the tokens collected from within the angle bracketed group <...>
                let mut group_tokens = TokenStream::new();
                let mut closing_angle_bracket = None;

                // Advance until the closing angle bracket (while pushing tokens)
                let mut levels: usize = 1;

                for tt in &mut fork {
                    match &tt {
                        // Add opening angle bracket to the stack
                        TokenTree::Punct(punct) if punct.as_char() == '<' => levels += 1,
                        // Take closing angle bracket from the stack
                        TokenTree::Punct(punct) if punct.as_char() == '>' => {
                            levels -= 1;
                            if levels == 0 {
                                closing_angle_bracket = Some(MetaToken::Punct(punct.clone()));
                                break;
                            }
                        },
                        _ => { }
                    }

                    tt.to_tokens(&mut group_tokens);
                }

                if levels > 0 {
                    return Err(syn::Error::new(tokens.last().unwrap().span(), "Unexpected end of input: AngleBracket group is not properly closed; expected '>'"));
                }

                // Parse the collected tokens
                tokens.extend(parse_group(group_tokens, found_metavar, metavar_name)?);
                tokens.push(closing_angle_bracket.unwrap());

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
    fn parse_path(
        token_iter: &mut (impl Iterator<Item = TokenTree> + Clone),
        found_metavar: &mut bool,
        metavar_name: &str,
    ) -> syn::Result<MetaTokenStream> {
        let mut tokens = MetaTokenStream::new();
        let mut fork = token_iter.clone();

        while let Some(tt) = fork.clone().next() {
            // while loop eats the '<', need to pass it in somehow ...
            if let TokenTree::Punct(punct) = &tt
            && punct.as_char() == '<' {
                tokens.extend(parse_angle_bracket_group(&mut fork, found_metavar, metavar_name)?);
                continue;
            } else {
                fork.next().unwrap();
            }

            match tt {
                TokenTree::Group(group) => return Err(syn::Error::new(group.span(), format!("Grouped tokens (with delimiter {:?}) are not allowed in a Type Path.", group.delimiter()))),
                TokenTree::Punct(punct) => match punct.as_char() {
                    '<' => return Err(syn::Error::new(punct.span(), "Found an unpaired opening angle bracket (<).")),
                    '>' => return Err(syn::Error::new(punct.span(), "Found an unpaired closing angle bracket (>).")),
                    ':' if punct.spacing() == Spacing::Joint => {
                        if let Some(TokenTree::Punct(punct_next)) = fork.next()
                        && punct_next.as_char() == ':'
                        && punct_next.spacing() == Spacing::Alone {
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
                            tokens.push(MetaToken::from_group(group, |stream| parse_group(stream, found_metavar, metavar_name))?);
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

    fn parse_fn(
        token_iter: &mut (impl Iterator<Item = TokenTree> + Clone),
        found_metavar: &mut bool,
        metavar_name: &str,
    ) -> syn::Result<MetaTokenStream> {
        let mut tokens = MetaTokenStream::new();
        let mut fork = token_iter.clone();

        if let Some(TokenTree::Ident(ident)) = fork.clone().next()
        && ident == "for" {
            fork.next().unwrap();
            tokens.push(MetaToken::Ident(ident));
            tokens.extend(parse_angle_bracket_group(&mut fork, found_metavar, metavar_name)?);
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
            Some(TokenTree::Ident(ident)) if ident == "fn"
                => tokens.push(MetaToken::Ident(ident)),
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
            if group.delimiter() == Delimiter::Parenthesis
                => tokens.push(MetaToken::from_group(group, |stream| parse_group(stream, found_metavar, metavar_name))?),
            tt => {
                let span = match tt {
                    Some(tt) => tt.span(),
                    None => Span::call_site(),
                };
                return Err(syn::Error::new(span, "Unexpected tokens: Expected parameters Parenthesis (...)"));
            }
        }

        if let Some(TokenTree::Punct(dash)) = fork.clone().next()
        && dash.as_char() == '-'
        && dash.spacing() == Spacing::Joint {
            fork.next().unwrap();
            if let Some(TokenTree::Punct(right_bracket)) = fork.next()
            && right_bracket.as_char() == '>'
            && right_bracket.spacing() == Spacing::Alone {
                tokens.push(MetaToken::Punct(dash));
                tokens.push(MetaToken::Punct(right_bracket));
                // RECURSION HERE vvv
                tokens.extend(parse_metacast_type_main(&mut fork, found_metavar, metavar_name)?);
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
    fn parse_group(
        stream: TokenStream,
        found_metavar: &mut bool,
        metavar_name: &str
    ) -> syn::Result<MetaTokenStream> {
        let mut tokens = MetaTokenStream::new();
        let mut token_iter = stream.into_iter();

        let mut fork = token_iter.clone();
        while let Some(tt) = fork.next() {
            // Try to parse a metavariable
            if let Some(metavar) = parse_metavar(&mut token_iter, metavar_name)? {
                tokens.push(MetaToken::MetaVar(metavar));
                *found_metavar = true;
                fork = token_iter.clone();
                continue;
            }
            token_iter = fork.clone();

            tokens.push(match tt {
                TokenTree::Group(group) => MetaToken::from_group(group, |stream| parse_group(stream, found_metavar, metavar_name))?,
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

    fn parse_metacast_type_main(
        token_iter: &mut (impl Iterator<Item = TokenTree> + Clone),
        found_metavar: &mut bool,
        metavar_name: &str,
    ) -> syn::Result<MetaTokenStream> {
        let mut fork = token_iter.clone();

        // Peek tokens to see which function is the parser
        match fork.next() {
            Some(tt) => match tt {
                // Found group: only the group is the type.
                TokenTree::Group(group) => {
                    *token_iter = fork;
                    Ok(MetaTokenStream::from(vec![
                        MetaToken::from_group(group, |stream| parse_group(stream, found_metavar, metavar_name))?
                    ]))
                },
                TokenTree::Ident(ident) => match ident.to_string().as_str() {
                    "for" => parse_fn(token_iter, found_metavar, metavar_name),
                    "fn" => parse_fn(token_iter, found_metavar, metavar_name),
                    "impl" => Err(syn::Error::new(ident.span(), "Unexpected tokens: 'impl' trait types are not valid MetaCast types.")),
                    "dyn" => Err(syn::Error::new(ident.span(), "Unexpected tokens: 'dyn' trait types are not valid MetaCast types.")),
                    "_" => Err(syn::Error::new(ident.span(), "Unexpected tokens: Infer type (_) is not a valid MetaCast type.")),
                    _ => parse_path(token_iter, found_metavar, metavar_name),
                },
                TokenTree::Punct(punct) => match punct.as_char() {
                    ':' if punct.spacing() == Spacing::Joint => match fork.next() {
                        Some(TokenTree::Punct(punct))
                        if punct.as_char() == ':' && punct.spacing() == Spacing::Alone
                            => parse_path(token_iter, found_metavar, metavar_name),
                        // This token is not part of the Type
                        _ => Err(syn::Error::new(punct.span(), "Unexpected tokens: Invalid token ':' while parsing Type."))
                    },
                    '$' => {
                        parse_metavar(token_iter, metavar_name)?
                            .inspect(|_| *found_metavar = true)
                            .map(|metavar| MetaTokenStream::from(vec![MetaToken::MetaVar(metavar)]))
                            .ok_or(syn::Error::new(punct.span(), "Unexpected tokens: Found '$', but was not a metavariable."))
                    },
                    '<' => parse_angle_bracket_group(token_iter, found_metavar, metavar_name),
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
                        tokens.extend(parse_metacast_type_main(&mut fork, found_metavar, metavar_name)?);

                        *token_iter = fork;

                        Ok(tokens)
                    },
                    '*' if punct.spacing() == Spacing::Alone => {
                        let mut tokens = MetaTokenStream::new();
                        tokens.push(MetaToken::Punct(punct));
                        // Parse raw pointer

                        match fork.next() {
                            Some(TokenTree::Ident(ident))
                            if ident == "const" || ident == "mut"
                                => tokens.push(MetaToken::Ident(ident)),
                            tt => {
                                let span = match tt {
                                    Some(tt) => tt.span(),
                                    None => Span::call_site(),
                                };
                                return Err(syn::Error::new(span, "Unexpected tokens: Expected 'const' or 'mut' modifier for pointer"));
                            }
                        }

                        // RECURSION HERE vvv
                        tokens.extend(parse_metacast_type_main(&mut fork, found_metavar, metavar_name)?);

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

    let mut found_metavar = false;
    let result = parse_metacast_type_main(token_iter, &mut found_metavar, metavar_name);

    if let Ok(tokens) = &result
    && !found_metavar {
        return Err(syn::Error::new(tokens[0].span(), format!("Expected to find MetaVariable '${metavar_name}' within the MetaCast Type.")));
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    #[test]
    fn type_to_metatokens_test() {
        let mut ty;
        let mut tokens;

        ty = quote! { T };
        tokens = type_to_metatokens(ty, "T");
        assert!(matches!(&tokens.unwrap()[0], MetaToken::MetaVar(_)));

        ty = quote! { ::std::my::Type<SomeType> };
        tokens = type_to_metatokens(ty, "T");
        tokens.unwrap_err();

        ty = quote! { ::std::my::Type<T> };
        tokens = type_to_metatokens(ty, "T");
        assert!(matches!(&tokens.unwrap()[10], MetaToken::MetaVar(_)));

        ty = quote! { ::std::my::T };
        tokens = type_to_metatokens(ty, "T");
        tokens.unwrap_err();

        ty = quote! { T::AssocType };
        tokens = type_to_metatokens(ty, "T");
        tokens.unwrap_err();

        ty = quote! { T<Something> };
        tokens = type_to_metatokens(ty, "T");
        tokens.unwrap_err();
    }

    #[test]
    fn parse_metacast_type_test() {
        // I HAD to make unit tests for this garbage 🤭
        let types = [
            quote! { $T },
            quote! { [$T; N] },
            quote! { [[$T; N]; N] },
            quote! { [$T] },
            quote! { [[$T]] },
            quote! { &[$T] },
            quote! { &[&[$T]] },
            quote! { fn(usize, $T) -> bool },
            quote! { fn() -> $T },
            quote! { syn::Token![$T] },
            quote! { syn::Token![::std::any::MyType<$T>] },
            quote! { ($T) },
            quote! { ($T, B, C) },
            quote! { ::std::any::MyType<$T> },
            quote! { ::std::any::MyType<$T, T, A> },
            quote! { &$T },
            quote! { &::std::any::MyType<$T> },
            quote! { &'local $T },
            quote! { &'local mut $T },
            quote! { &'local mut ::std::any::MyType<$T> },
            quote! { *const $T },
            quote! { *mut $T },
            quote! { *const::std::any::MyType<$T> },
            quote! { <$T> },
            quote! { <::std::any::MyType<$T>> },
            quote! { Result<Option<Box<[$T]>>, &'static str> },
        ];
        for ty in types {
            parse_metacast_type(&mut ty.into_iter(), "T").unwrap();
        }

        let fail_types = [
            quote! { },
            quote! { $G },
            quote! { impl A + B + C + 'static },
            quote! { dyn A + B + C + 'static },
            quote! { _ },
            quote! { ! },
            quote! { T },
            quote! { $MyType },
            quote! { Option<T> },
        ];
        for ty in fail_types {
            parse_metacast_type(&mut ty.into_iter(), "T").unwrap_err();
        }
    }
}
