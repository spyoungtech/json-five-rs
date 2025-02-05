use crate::tokenize::{TokType, TokenSpan, TokenizationError};

#[derive(Debug, PartialEq, Clone)]
pub struct TokenContext {
    pub start_lineno: usize,
    pub start_colno: usize,
    pub start_byte_offset: usize,
    pub start_char_index: usize,
    pub end_byte_offset: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub lexeme: String,
    pub tok_type: TokType,
    pub context: Option<TokenContext>
}


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
                        assert_eq!(next_span.1, TokType::EOF, "Unexpected end of document while token remaining {:?}", next_span);
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
                    assert_eq!(next_span.1, TokType::EOF, "Unexpected end of document while token remaining {:?}", next_span);
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