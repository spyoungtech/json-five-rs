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



type Token = (usize, TokType, usize);

#[derive(Debug, PartialEq)]
struct Tokens<'input> {
    tokens: Vec<Token>,
    source: &'input str
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



#[derive(Debug, PartialEq, Clone)]
enum TokenizerState {
    Start,
    End,
    Default,
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


    fn process_string(&mut self) -> Result<Token, TokenizationError<'input>> {
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
                            break Ok((start_idx, string_type, idx+1))
                        },
                        _ => continue
                    }
                }
            }
        }
    }

    fn process_whitespace(&mut self) -> Result<Token, TokenizationError<'input>> {
        let (start_idx, _) = self.lookahead.expect("Unexpected end of input, was expecting whitespace char");
        let mut last_index = start_idx;
        loop {
            match self.chars.peek() {
                None => break Ok((start_idx, TokType::Whitespace, last_index + 1)),
                Some((peeked_idx, peeked_char)) => {
                    if peeked_char.is_whitespace() {
                        last_index = *peeked_idx;
                        self.advance();
                        continue
                    } else {
                        break Ok((start_idx, TokType::Whitespace, last_index+1))
                    }
                }
            }
        }
    }

    fn process_octal(&mut self) -> Result<Token, TokenizationError<'input>> {
        todo!()
    }


    fn process_number(&mut self) -> Result<Token, TokenizationError<'input>>{
        let (start_idx, start_char) = self.lookahead.expect("Unexpected end of input, was expecting numeric char");

        let maybe_second_char = self.chars.peek();
        match maybe_second_char {
            None => return Ok((start_idx, TokType::Integer, start_idx + 1)),
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
                            break Ok((start_idx, TokType::Float, last_index+1))
                        } else {
                            break Ok((start_idx, TokType::Integer, last_index+1))
                        }
                    }
                }
            }
        }

    }

    fn tok_from_indices(&self, start: usize, end: usize) -> Result<Token, TokenizationError<'input>> {
        let lexeme= slice_by_char_indices(self.text, start, end);
        match lexeme {
            "true" => Ok((start, TokType::True, end)),
            "false" => Ok((start, TokType::False, end)),
            "NaN" => Ok((start, TokType::Nan, end)),
            "Infinity" => Ok((start, TokType::Infinity, end)),
            "null" => Ok((start, TokType::Null, end)),
            _ => {
                Ok((start, TokType::Name, end))
            }
        }
    }

    fn process_identifier_or_const(&mut self) -> Result<Token, TokenizationError<'input>> {
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

    fn next_token(&mut self) -> Result<Token, TokenizationError<'input>> {
        let maybe_last = self.lookahead;
        let maybe_next = self.advance();
        match maybe_next {
            None => {
                match maybe_last {
                    Some((last_idx, _)) => Ok((last_idx+1, TokType::EOF, last_idx+1)),
                    None => Ok((0, TokType::EOF, 0)),
                }
            }
            Some((next_idx, next)) => {
                match next {
                    '{' => Ok((next_idx, TokType:: LeftBrace, next_idx + 1)),
                    '}' => Ok((next_idx, TokType:: RightBrace, next_idx + 1)),
                    '[' => Ok((next_idx, TokType:: LeftBracket, next_idx + 1)),
                    ']' => Ok((next_idx, TokType:: RightBracket, next_idx + 1)),
                    ',' => Ok((next_idx, TokType:: Comma, next_idx + 1)),
                    ':' => Ok((next_idx, TokType:: Colon, next_idx + 1)),
                    ';' => Ok((next_idx, TokType:: Semi, next_idx + 1)),
                    '+' => Ok((next_idx, TokType:: Plus, next_idx + 1)),
                    '-' => Ok((next_idx, TokType:: Minus, next_idx + 1)),
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
                    '\\' if *self.chars.peek().map(|(_, x)| x).unwrap_or(&'!') == '\\' => {
                        todo!()
                    }
                    _ => self.process_identifier_or_const()
                }
            }
        }
    }

    fn tokenize(&mut self) -> Result<Tokens<'input>, TokenizationError<'input>> {
        if self.tokenizer_state != TokenizerState::Start {
            panic!("Can only tokenize once!")
        }

        self.tokenizer_state = TokenizerState::Default;

        let mut tokens: Vec<Token> = Vec::new();
        loop {
            let tok = self.next_token()?;
            if tok.1 == TokType::EOF {
                tokens.push(tok);
                break
            } else {
                tokens.push(tok);
            }
        }
        Ok(Tokens{tokens: tokens, source: self.text})
    }
}

pub fn tokenize(text: &'_ str) -> Tokens<'_> {
    Tokenizer::new(text).tokenize().unwrap()
}

#[cfg(test)]
mod test {
    use crate::tokenize::TokType::*;
    use super::*;
    #[test]
    fn test_foo() {
        let text = "";
        let toks = tokenize(text);
        let expected = Tokens{tokens: vec![(0, EOF, 0)], source: text};
        assert_eq!(toks, expected);
    }

    #[test]
    fn test_heck() {
        let text = "{}";
        let toks = tokenize(text);
        let expected = Tokens{tokens: vec![(0, LeftBrace, 1), (1, RightBrace, 2), (2, EOF, 2)], source: text};
        assert_eq!(toks, expected);
    }

    #[test]
    fn test_heck2() {
        let text = "{\"foo\":\"bar\"}";
        let toks = tokenize(text);
        let expected = Tokens{tokens: vec![(0, LeftBrace, 1), (1, DoubleQuotedString, 6), (6, Colon, 7), (7, DoubleQuotedString, 12), (12, RightBrace, 13), (13, EOF, 13)], source: text};
        assert_eq!(toks, expected)
    }
}

