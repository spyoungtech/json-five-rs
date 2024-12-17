use std::fmt::{Display, Formatter};
use std::iter::{Peekable, Filter};
use std::str::{CharIndices, Chars};

#[derive(PartialEq, Clone, Debug)]
enum TokType {
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Colon,
    Semi,
    Name,
    SingleQuotedString,
    DoubleQuotedString,
    BlockComment,
    LineComment,
    Whitespace,
    True,
    False,
    Null,
    Integer,
    Float,
    Infinity,
    Nan,
    Exponent,
    Hexadecimal,
    Octal,
    Plus,
    Minus,
    EOF,
}



#[derive(PartialEq, Debug)]
struct Token<'input> {
    toktype: TokType,
    start_index: usize,
    end_index: usize,
    source: &'input str,
}

fn slice_by_char_indices(s: &str, start: usize, end: usize) -> &str {
    let start_byte = s.char_indices()
        .nth(start)
        .map(|(i, _)| i)
        .unwrap_or(s.len());

    let end_byte = s.char_indices()
        .nth(end)
        .map(|(i, _)| i)
        .unwrap_or(s.len());

    &s[start_byte..end_byte]
}

impl <'input> Token<'input> {
    pub fn lexeme(&self) -> &str {
        slice_by_char_indices(self.source, self.start_index, self.end_index)
    }
}


#[derive(Debug, PartialEq, Clone)]
enum TokenizerState {
    Start,
    End,
    Default,
    InSingleQuoteString,
    InDoubleQuoteString,
    InNumber,

}

#[derive(Debug)]
struct TokenizationError<'input> {
    message: String,
    index: usize,
    source: &'input str,
}

// impl Display for TokenizationError {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//
//         write!(f, "TokenizationError: {}", self.message)
//     }
// }

struct Source {
    filename: Option<String>

}

#[derive(Debug)]
struct Tokenizer<'input> {
    tokenizer_state: TokenizerState,
    include_whitespace: bool,
    text: &'input str,
    chars: Peekable<CharIndices<'input>>,
    lookahead: Option<(usize, char)>,
}



impl <'input> Tokenizer<'input> {
    fn new(text: &'input str) -> Self {
        Tokenizer { tokenizer_state: TokenizerState::Start, include_whitespace: false, text: text, chars: text.char_indices().peekable(), lookahead: None}
    }
    fn with_whitespace(text: &'input str) -> Self {
        Tokenizer { tokenizer_state: TokenizerState::Start, include_whitespace: true, text: text, chars: text.char_indices().peekable(), lookahead: None}
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        self.lookahead = self.chars.next();
        self.lookahead
    }

    fn at_end(&mut self) -> bool {
        self.chars.peek().is_none()
    }


    fn process_string(&mut self) -> Result<Token<'input>, TokenizationError<'input>> {
        let (start_idx, quote_char) = self.lookahead.expect("Expected quote character");

        let string_type: TokType = match quote_char {
            '"' => TokType::DoubleQuotedString,
            '\'' => TokType::SingleQuotedString,
            _ => panic!("Expected quote character, but got {:?}", quote_char)
        };

        let mut last_char = quote_char; // to keep track of escapes

        loop {
            let maybe_next = self.advance();
            match maybe_next {
                None => break Err(TokenizationError { message: "Unterminated string starting at".to_string(), index: start_idx, source: self.text }),
                Some((idx, char)) => {
                    match char {
                        '\n' => {
                            if last_char != '\\' {
                                break Err(TokenizationError{ message: "Unexpected unescaped newline in string started at".to_string(), index: idx+1, source: self.text})
                            }
                            continue
                        },
                        c if c == quote_char && last_char != '\\' => {
                            break Ok(Token { toktype: string_type, start_index: start_idx, end_index: idx+1, source: self.text })
                        },
                        _ => continue
                    }
                }
            }
        }
    }

    fn process_whitespace(&mut self) -> Result<Token<'input>, TokenizationError<'input>> {
        let (start_idx, _) = self.lookahead.expect("Unexpected end of input, was expecting whitespace char");
        let mut last_index = start_idx;
        loop {
            match self.chars.peek() {
                None => break Ok(Token{toktype: TokType::Whitespace, start_index: start_idx, end_index: last_index + 1, source: self.text}),
                Some((peeked_idx, peeked_char)) => {
                    if peeked_char.is_whitespace() {
                        last_index = *peeked_idx;
                        self.advance();
                        continue
                    } else {
                        break Ok(Token{toktype: TokType::Whitespace, start_index: start_idx, end_index: last_index+1, source: self.text})
                    }
                }
            }
        }
    }

    fn process_octal(&mut self) -> Result<Token<'input>, TokenizationError<'input>> {
        todo!()
    }


    fn process_number(&mut self) -> Result<Token<'input>, TokenizationError<'input>>{
        let (start_idx, start_char) = self.lookahead.expect("Unexpected end of input, was expecting numeric char");

        let maybe_second_char = self.chars.peek();
        match maybe_second_char {
            None => return Ok(Token{toktype: TokType::Integer, start_index: start_idx, end_index: start_idx + 1, source: self.text}),
            Some((second_idx, second_char)) if start_char == '0' => {
                match second_char {
                    'x' | 'X' => {todo!()}
                    _ => {},
                }
            }
            _ => {}
        }

        let mut last_index = start_idx;
        let mut decimal_seen: bool = false;
        loop {
            match self.chars.peek() {
                None => {
                    todo!()
                },
                Some((next_idx, next_char)) => {
                    if next_char.is_ascii_digit() {
                        last_index = *next_idx;
                        self.advance();
                        continue
                    } else if *next_char == '.' {
                        if decimal_seen {
                            return Err(TokenizationError{message: "Invalid number literal".to_string(), source: self.text, index: start_idx})
                        }
                        decimal_seen = true;
                        last_index = *next_idx;
                        self.advance();
                        continue
                    } else {
                        if decimal_seen {
                            break Ok(Token{toktype: TokType::Float, start_index: start_idx, end_index: last_index+1, source: self.text})
                        } else {
                            break Ok(Token{toktype: TokType::Integer, start_index: start_idx, end_index: last_index+1, source: self.text})
                        }
                    }
                }
            }
        }

    }

    fn tok_from_indices(&self, start: usize, end: usize) -> Result<Token<'input>, TokenizationError<'input>> {
        let lexeme= slice_by_char_indices(self.text, start, end);
        match lexeme {
            "true" => Ok(Token{toktype: TokType::True, start_index: start, end_index: end, source: self.text}),
            "false" => Ok(Token{toktype: TokType::False, start_index: start, end_index: end, source: self.text}),
            "NaN" => Ok(Token{toktype: TokType::Nan, start_index: start, end_index: end, source: self.text}),
            "Infinity" => Ok(Token{toktype: TokType::Infinity, start_index: start, end_index: end, source: self.text}),
            "null" => Ok(Token{toktype: TokType::Null, start_index: start, end_index: end, source: self.text}),
            _ => {
                Ok(Token{toktype: TokType::Name, start_index: start, end_index: end, source: self.text})
            }
        }
    }

    fn process_identifier_or_const(&mut self) -> Result<Token<'input>, TokenizationError<'input>> {
        let (start_idx, _) = self.lookahead.expect("Unexpected end of input, was expecting identifier/const char");
        let mut last_idx = start_idx;
        loop {
            match self.chars.peek() {
                None => break self.tok_from_indices(start_idx, last_idx+1),
                Some((next_idx, next_char)) => {
                    if next_char.is_whitespace() {
                        break self.tok_from_indices(start_idx, last_idx+1)
                    } else if next_char.is_alphanumeric() {
                        last_idx = *next_idx;
                        self.advance();
                        continue
                    } else {
                        break self.tok_from_indices(start_idx, last_idx + 1)
                    }
                }
            }
        }

    }

    fn next_token(&mut self) -> Result<Token<'input>, TokenizationError<'input>> {
        let maybe_last = self.lookahead;
        let maybe_next = self.advance();
        match maybe_next {
            None => {
                match maybe_last {
                    Some((last_idx, _)) => Ok(Token {toktype: TokType::EOF, start_index: last_idx+1, end_index: last_idx+1, source:self.text}),
                    None => Ok(Token {toktype: TokType::EOF, start_index: 0, end_index: 0, source: self.text}),
                }
            }
            Some((next_idx, next)) => {
                match next {
                    '{' => Ok(Token{toktype: TokType:: LeftBrace, start_index: next_idx, end_index: next_idx + 1, source: self.text}),
                    '}' => Ok(Token{toktype: TokType:: RightBrace, start_index: next_idx, end_index: next_idx + 1, source: self.text}),
                    '[' => Ok(Token{toktype: TokType:: LeftBracket, start_index: next_idx, end_index: next_idx + 1, source: self.text}),
                    ']' => Ok(Token{toktype: TokType:: RightBracket, start_index: next_idx, end_index: next_idx + 1, source: self.text}),
                    ',' => Ok(Token{toktype: TokType:: Comma, start_index: next_idx, end_index: next_idx + 1, source: self.text}),
                    ':' => Ok(Token{toktype: TokType:: Colon, start_index: next_idx, end_index: next_idx + 1, source: self.text}),
                    ';' => Ok(Token{toktype: TokType:: Semi, start_index: next_idx, end_index: next_idx + 1, source: self.text}),
                    '+' => Ok(Token{toktype: TokType:: Plus, start_index: next_idx, end_index: next_idx + 1, source: self.text}),
                    '-' => Ok(Token{toktype: TokType:: Minus, start_index: next_idx, end_index: next_idx + 1, source: self.text}),
                    '\'' | '"' => self.process_string(),
                    c if c.is_ascii_digit() => self.process_number(),
                    c if c.is_whitespace() => {
                        let whitespace_tok = self.process_whitespace()?;
                        if self.include_whitespace {
                            Ok(whitespace_tok)
                        } else {
                            self.next_token()
                        }
                    },
                    '\\' if *self.chars.peek().map(|(_, x)| x).unwrap_or(&'!') == '\\' {
                        todo!()
                    }
                    _ => self.process_identifier_or_const()
                }
            }
        }
    }

    fn tokenize(&mut self) -> Result<Vec<Token<'input>>, TokenizationError<'input>> {
        if self.tokenizer_state != TokenizerState::Start {
            panic!("Can only tokenize once!")
        }

        self.tokenizer_state = TokenizerState::Default;

        let mut tokens: Vec<Token> = Vec::new();
        loop {
            let tok = self.next_token()?;
            if tok.toktype == TokType::EOF {
                tokens.push(tok);
                break
            } else {
                tokens.push(tok);
            }
        }
        Ok(tokens)
    }
}

pub fn tokenize(text: &'_ str) -> Vec<Token<'_>> {
    Tokenizer::new(text).tokenize().unwrap()
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_foo() {
        let text = "";
        let toks = tokenize(text);
        let expected = vec![Token{toktype: TokType::EOF, start_index: 0, end_index: 0, source: text}];
        assert_eq!(toks, expected)
    }

    #[test]
    fn test_heck() {
        let text = "{}";
        let toks = tokenize(text);
        println!("{:?}", toks)
    }

    #[test]
    fn test_heck2() {
        let text = "{\"foo\":\"bar\"}";
        let toks = tokenize(text);
        println!("{:?}", toks)
    }
}

