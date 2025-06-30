use crate::tokenize::{TokType, TokenSpan, TokenizationError};

#[derive(Debug, PartialEq, Clone)]
pub struct TokenContext {
    pub start_lineno: usize,
    pub start_colno: usize,
    pub start_byte_offset: usize,
    pub start_char_index: usize,
    pub end_byte_offset: usize,
}

/// Represents a source token
///
/// Unlike the spans found in [crate::tokenize::Tokens], these tokens are
/// of an owned type containing an owned String of the lexeme from source.
///
/// The typical way to obtain a [Token] is from the [source_to_tokens] function. But
/// tokens can be created without source documents, too.
///
///
#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    /// The contents of the token, exactly as it appears in source
    pub lexeme: String,

    /// the type of the token
    pub tok_type: TokType,

    /// Contextual information about the Token's position in the source document (if available)
    /// Because tokens can be created without a source doc, this field is an [Option] and may be [None].
    pub context: Option<TokenContext>
}

/// Generate a Vec of [Token]s from a str.
///
///
/// This function is complementary with [tokens_to_source]. The typical workflow
/// is to use this function to generate a Vec of tokens, do something to modify it (
/// e.g., add/remove/replace tokens) then use [tokens_to_source] to turn it back into JSON5 source.
///
/// Unlike the tokenizing functions available in the [crate::tokenize] module, this function
/// produces owned [Token] objects containing (among other fields) an owned String of the lexeme,
/// rather than a [crate::tokenize::Tokens] struct.
///
/// # Examples
///
/// ```rust
/// use json_five::{source_to_tokens, tokens_to_source};
/// use json_five::rt::tokenize::Token;
/// use json_five::tokenize::TokType::Whitespace;
///
/// let tokens = source_to_tokens("  {my: 'json5'}  ").unwrap();
///
/// // remove all Whitespace tokens
/// let new_tokens:Vec<Token> = tokens.into_iter().filter(|tok| tok.tok_type != Whitespace).collect();
///
/// // turn tokens back into source
/// let new_source = tokens_to_source(&new_tokens);
/// assert_eq!(new_source, String::from("{my:'json5'}"))
/// ```
///
pub fn source_to_tokens(text: &str) -> Result<Vec<Token>, TokenizationError> {
    use crate::tokenize::tokenize_rt_str;
    // TODO: instead of going through the entire input to get the tokens
    //       and then going through it again to get context
    //       we should write a solution that does everything in one pass

    let tokens = tokenize_rt_str(text)?;
    let mut source_tokens: Vec<Token> = Vec::with_capacity(tokens.tok_spans.len());
    let mut spans = tokens.tok_spans.iter();
    let mut current_span = spans.next().unwrap(); // there will always at least be EOF
    let mut next_span: &TokenSpan;
    match spans.next() {
        None => {
            // empty doc
            source_tokens.push(Token{lexeme: String::new(), tok_type:TokType::EOF, context: Some(TokenContext{start_byte_offset: 0, start_colno: 1, start_lineno: 1, start_char_index: 0, end_byte_offset: 0})});
            return Ok(source_tokens)
        }
        Some(span) => {
            next_span = span;
        }
    }
    let mut lineno: usize = 1;
    let mut colno: usize = 0;

    let mut start_lineno = lineno;
    let mut start_colno = 1;
    let mut start_char_index = 0;

    let mut current_token_buffer = String::with_capacity(current_span.2 - current_span.0);
    for (codepoint_off, (byte_off, char)) in text.char_indices().enumerate() {
        colno += 1;
        current_token_buffer.push(char);
        if char == '\n' {
            if current_token_buffer.len() == current_token_buffer.capacity() {
                let context = TokenContext{start_byte_offset: current_span.0, start_lineno, start_colno, start_char_index, end_byte_offset: current_span.2};
                let token = Token{lexeme: current_token_buffer, tok_type: current_span.1.clone(), context: Some(context)};
                source_tokens.push(token);
                match spans.next() {
                    None => {
                        assert_eq!(next_span.1, TokType::EOF, "Unexpected end of document while token remaining {next_span:?}");
                        let context = TokenContext{start_lineno:lineno, start_colno: colno, start_byte_offset: next_span.0, start_char_index: codepoint_off, end_byte_offset: next_span.0};
                        let token = Token{lexeme: String::with_capacity(0), context: Some(context), tok_type: TokType::EOF};
                        source_tokens.push(token);
                        return Ok(source_tokens)
                    }
                    Some(span) => {
                        current_span = next_span;
                        next_span = span;
                        current_token_buffer = String::with_capacity(current_span.2 - current_span.0);
                        lineno += 1;
                        colno = 0;
                        start_lineno = lineno;
                        start_colno = 1;
                        start_char_index = codepoint_off + 1;
                        continue
                    }
                }
            } else {
                lineno += 1;
                colno = 0;
            }
        }
        if current_token_buffer.len() < current_token_buffer.capacity() {
            continue
        }
        if current_token_buffer.len() == current_token_buffer.capacity() {

            let context = TokenContext{start_byte_offset: current_span.0, start_lineno, start_colno, start_char_index, end_byte_offset: current_span.2};
            let token = Token{lexeme: current_token_buffer, tok_type: current_span.1.clone(), context: Some(context)};
            source_tokens.push(token);
            match spans.next() {
                None => {
                    assert_eq!(next_span.1, TokType::EOF, "Unexpected end of document while token remaining {next_span:?}");
                    let context = TokenContext{start_lineno:lineno, start_colno: colno, start_byte_offset: next_span.0, start_char_index: codepoint_off, end_byte_offset: next_span.0};
                    let token = Token{lexeme: String::with_capacity(0), context: Some(context), tok_type: TokType::EOF};
                    source_tokens.push(token);
                    return Ok(source_tokens)
                }
                Some(span) => {
                    current_span = next_span;
                    next_span = span;
                    current_token_buffer = String::with_capacity(current_span.2 - current_span.0);
                    start_lineno = lineno;
                    start_colno = colno + 1;
                    start_char_index = codepoint_off + 1;
                    continue
                }
            }
        }
        if byte_off > current_span.2 {
            unreachable!("moved beyond current span")
        }
    }
    unreachable!("Unexpected end of document");
}

/// Generate a String from a Vec of [Token]s
///
/// This function is complementary with [source_to_tokens]. The typical workflow
/// is to use [source_to_tokens] to generate a Vec of tokens, do something to modify it (
/// e.g., add/remove/replace tokens) then use this function to turn it back into JSON5 source.
///
///
/// # Examples
///
/// ```rust
/// use json_five::{source_to_tokens, tokens_to_source};
/// use json_five::rt::tokenize::Token;
/// use json_five::tokenize::TokType::Whitespace;
///
/// let tokens = source_to_tokens("  {my: 'json5'}  ").unwrap();
///
/// // remove all Whitespace tokens
/// let new_tokens:Vec<Token> = tokens.into_iter().filter(|tok| tok.tok_type != Whitespace).collect();
///
/// // turn tokens back into source
/// let new_source = tokens_to_source(&new_tokens);
/// assert_eq!(new_source, String::from("{my:'json5'}"))
/// ```
pub fn tokens_to_source(tokens: &Vec<Token>) -> String {
    let mut size = 0_usize;
    for tok in tokens.iter() {
        size += tok.lexeme.len()
    }
    let mut ret = String::with_capacity(size);
    for tok in tokens.iter() {
        ret.push_str(&tok.lexeme)
    }
    ret
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test() {
        let text = "{\"foo\":\"bar\"}";
        let tokens = source_to_tokens(text).unwrap();
        assert_eq!(text, tokens_to_source(&tokens));
    }

    #[test]
    fn test_rt() {
        let text = r#"// A JSON5 document
    {my: "value",
//               ^^^^^
// There is trailing whitespace above that will be formatted
     another: "value"
     }  // there is also trailing whitespace at the end of the doc
    "#;
        let tokens = source_to_tokens(text).unwrap();
        println!("{:?}", tokens);
        assert_eq!(text, tokens_to_source(&tokens));

    }
}