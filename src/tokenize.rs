use std::fmt::{Display, Formatter};
use std::iter::{Peekable, Filter};
use std::str::{CharIndices, Chars};
use crate::utils::get_line_col_char;

#[derive(PartialEq, Clone, Debug)]
pub(crate) enum TokType {
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Colon,
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


// Start byte offset, token type, end byte offset (noninclusive)
pub(crate) type TokenSpan = (usize, TokType, usize);




#[derive(Debug, PartialEq)]
pub(crate) struct Tokens<'input> {
    tok_spans: Vec<TokenSpan>,
    pub (crate) source: &'input str
}


impl<'input> Tokens<'input> {
    pub(crate) fn spans_with_source(&self) -> Vec<(&TokenSpan, &'input str)> {
        self.tok_spans.iter().map(|span| (span, &self.source[span.0 .. span.2])).collect()
    }
}

#[derive(Debug)]
pub (crate) struct TokenizationError {
    message: String,
    index: usize, // byte offset
    pub(crate) lineno: usize,
    pub(crate) colno: usize,
    pub(crate) char_index: usize // char offset
}

impl<'input> Display for TokenizationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "TokenizationError: {}: line {} column {} (char {})", self.message, self.lineno, self.colno, self.char_index)
    }
}

#[derive(Debug)]
pub(crate) struct Tokenizer<'input> {
    include_whitespace: bool,
    text: &'input str,
    chars: Peekable<CharIndices<'input>>,
    lookahead: Option<(usize, char)>,
}


const HEX_CHARS: &str = "0123456789abcdefABCDEF";
const IDENTIFIER_SYMBOLS: &str = "$_";

impl <'input> Tokenizer<'input> {
    pub(crate) fn new(text: &'input str) -> Self {
        Tokenizer {include_whitespace: false, text: text, chars: text.char_indices().peekable(), lookahead: None}
    }
    fn with_whitespace(text: &'input str) -> Self {
        Tokenizer {include_whitespace: true, text: text, chars: text.char_indices().peekable(), lookahead: None}
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        self.lookahead = self.chars.next();
        self.lookahead
    }

    fn at_end(&mut self) -> bool {
        self.chars.peek().is_none()
    }

    fn make_error(&self, message: String, start_index: usize) -> TokenizationError {
        let (lineno, colno, char_index) = get_line_col_char(self.text, start_index);
        TokenizationError{message: message, index: start_index, lineno: lineno, colno: colno, char_index: char_index}
    }

    fn process_string(&mut self) -> Result<TokenSpan, TokenizationError> {
        let (start_idx, quote_char) = self.lookahead.expect("Expected quote character");

        let string_type: TokType = match quote_char {
            '"' => TokType::DoubleQuotedString,
            '\'' => TokType::SingleQuotedString,
            _ => panic!("Expected quote character, but got {:?}", quote_char)
        };

        let mut last_char = quote_char; // to keep track of escapes

        loop {
            match self.advance() {
                None => {
                    break Err(self.make_error("Unterminated string starting at".to_string(), start_idx))
                },
                Some((idx, char)) => {
                    match char {
                        '\n' | '\r' | '\u{2028}' | '\u{2029}' => {
                            if last_char != '\\' {
                                let (lineno, colno, char_index) = get_line_col_char(self.text, start_idx);
                                break Err(self.make_error("Unexpected line terminator without continuation in string literal at".to_string(), idx))
                            }
                            last_char = char;
                            continue
                        },
                        c if c == quote_char && last_char != '\\' => {
                            break Ok((start_idx, string_type, idx+1))
                        },
                        _ => {
                            last_char = char;
                            continue
                        }
                    }
                }
            }
        }
    }

    fn process_whitespace(&mut self) -> Result<TokenSpan, TokenizationError> {
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

    fn process_octal(&mut self) -> Result<TokenSpan, TokenizationError> {
        let (start_idx, start_char) = self.lookahead.expect("Unexpected end of input, was processing octal");
        Err(self.make_error("Octals are forbidden".to_string(), start_idx))
    }

    fn process_hexadecimal(&mut self) -> Result<TokenSpan, TokenizationError> {
        let (start_idx, start_char) = self.lookahead.expect("Unexpected end of input, was expecting numeric char");
        let (_, x_char) = self.advance().expect("Expected hex x");
        assert_eq!(start_char, '0');
        if x_char != 'x' && x_char != 'X' {
            panic!("Invalid hexadecimal here")
        }

        match self.advance() {
            None => {
                return Err(self.make_error("Expected at least one digit in hexadecimal literal".to_string(), start_idx))
            }
            Some((mut last_idx, first_digit)) => {
                if !HEX_CHARS.contains(first_digit) {
                    return Err(self.make_error(format!("Invalid hexadecimal character {:?} in literal starting at", first_digit), start_idx))
                }
                loop {
                    match self.chars.peek() {
                        None => break Ok((start_idx, TokType::Hexadecimal, last_idx+1)),
                        Some((offset, char)) => {
                            if !HEX_CHARS.contains(*char) {
                                break Ok((start_idx, TokType::Hexadecimal, last_idx+1))
                            }
                            last_idx = *offset;
                            self.advance();
                            continue
                        }
                    }
                }
            }
        }
    }

    fn process_number(&mut self) -> Result<TokenSpan, TokenizationError>{
        let (start_idx, start_char) = self.lookahead.expect("Unexpected end of input, was expecting numeric char");

        let maybe_second_char = self.chars.peek();
        match maybe_second_char {
            None => return Ok((start_idx, TokType::Integer, start_idx + 1)),
            Some((second_idx, second_char)) if start_char == '0' => {
                match second_char {
                    'x' | 'X' => {return self.process_hexadecimal()}
                    sc if sc.is_ascii_digit() => {
                        return self.process_octal()
                    },
                    _ => {}
                }
            }
            _ => {}
        }

        let mut last_index = start_idx;
        let mut decimal_seen: bool = false;
        let mut exponent_seen: bool = false;
        let mut unary_seen: bool = false;
        match start_char {
            '.' => {decimal_seen = true}
            '+' | '-' => {unary_seen = true}
            _ => {}
        }
        loop {
            match self.chars.peek() {
                None => {
                    if unary_seen || exponent_seen {
                        let (_, last_char) = self.lookahead.unwrap();
                        if "+-eE".contains(last_char) {
                            return Err(self.make_error(format!("Invalid number literal (missing digit after {:?})", last_char), start_idx))
                        }
                    }
                    if exponent_seen {
                        break Ok((start_idx, TokType::Exponent, last_index+1))
                    } else if decimal_seen {
                        if start_idx == last_index {
                            return Err(self.make_error("Lone decimal is an invalid number literal".to_string(), start_idx))
                        }
                        break Ok((start_idx, TokType::Float, last_index+1))
                    } else {
                        break Ok((start_idx, TokType::Integer, last_index+1))
                    }
                },
                Some((next_idx, next_char)) => {
                    match *next_char {
                        c if c.is_ascii_digit() => {
                            last_index = *next_idx;
                            self.advance();
                            continue
                        },
                        '.' => {
                            if decimal_seen {
                                return Err(self.make_error("Invalid number literal (unexpected decimal)".to_string(), start_idx))
                            }
                            decimal_seen = true;
                            if exponent_seen {
                                return Err(self.make_error("Invalid exponent literal (float exponents forbidden) at".to_string(), start_idx))
                            }
                            last_index = *next_idx;
                            self.advance();
                            continue
                        },
                        'e' | 'E' => {
                            if exponent_seen {
                                return Err(self.make_error("Invalid number literal (only one exponent part is allowed)".to_string(), start_idx))
                            }
                            exponent_seen = true;
                            last_index = *next_idx;
                            self.advance();
                        }
                        '+' | '-' => {
                            let (_, previous_char) = self.lookahead.unwrap();
                            unary_seen = true;
                            match previous_char {
                                'e' | 'E' => {
                                    last_index = *next_idx;
                                    self.advance();
                                }
                                _ => {
                                    return Err(self.make_error("Unary within number literal only allowed after exponent part".to_string(), start_idx))
                                }
                            }
                        }
                        _ => {
                            // The peeked character can't be part of a number
                            // Verify the number is valid
                            if unary_seen || exponent_seen {
                                let (_, last_char) = self.lookahead.unwrap();
                                if "+-eE".contains(last_char) {
                                    return Err(self.make_error(format!("Invalid number literal (missing digit after {:?})", last_char), start_idx))
                                }
                            }
                            if exponent_seen {
                                break Ok((start_idx, TokType::Exponent, last_index+1))
                            } else if decimal_seen {
                                if start_idx == last_index {
                                    return Err(self.make_error("Lone decimal is an invalid number literal".to_string(), start_idx))
                                }
                                break Ok((start_idx, TokType::Float, last_index+1))
                            } else {
                                break Ok((start_idx, TokType::Integer, last_index+1))
                            }
                        }
                    }
                }
            }
        }

    }

    fn tok_from_indices(&self, start: usize, end: usize) -> Result<TokenSpan, TokenizationError> {
        let lexeme= &self.text[start .. end];
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

    fn process_identifier_or_const(&mut self) -> Result<TokenSpan, TokenizationError> {
        let (start_idx, start_char) = self.lookahead.expect("Unexpected end of input, was expecting identifier/const char");
        let mut last_idx = start_idx;

        // TODO: ensure that the first character is a valid identifier start character

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
                    } else if IDENTIFIER_SYMBOLS.contains(*next_char) {
                        last_idx = *next_idx;
                        self.advance();
                        continue
                    } else {
                        break self.tok_from_indices(start_idx, last_idx + 1)
                    } // TODO: handle unicode escape sequences
                }
            }
        }
    }

    fn process_comment(&mut self) -> Result<TokenSpan, TokenizationError> {
        let (start_idx, char) = self.lookahead.expect("Expected comment start");
        let (mut last_idx, star_or_slash) = self.advance().expect("Expected second comment char");
        match star_or_slash {
            '/' => {
                loop {
                    match self.chars.peek() {
                        None => {
                            return Ok((start_idx, TokType::LineComment, last_idx))
                        },
                        Some((peeked_idx, peeked_char)) => {
                            match peeked_char {
                                '\n' | '\r' | '\u{2028}' | '\u{2029}' => {
                                    (last_idx, _) = self.advance().unwrap();
                                    return Ok((start_idx, TokType::LineComment, last_idx))
                                }
                                _ => {
                                    last_idx = *peeked_idx;
                                    self.advance();
                                }
                            }
                        }
                    }
                }
            },
            '*' => {
                loop {
                    match self.chars.peek() {
                        None => {
                            return Err(self.make_error("Unexpected end of input while processing block comment".to_string(), start_idx))
                        }
                        Some((peeked_idx, peeked_char)) => {
                            match peeked_char {
                                '*' => {
                                    last_idx = *peeked_idx;
                                    self.advance();
                                    let maybe_next_next = self.chars.peek();
                                    match maybe_next_next {
                                        None => {
                                            return Err(self.make_error("Unexpected end of input while processing block comment".to_string(), start_idx))
                                        },
                                        Some((next_peeked_idx, next_peeked_char)) => {
                                            match next_peeked_char {
                                                '/' => {
                                                    (last_idx, _) = self.advance().unwrap();
                                                    return Ok((start_idx, TokType::BlockComment, last_idx))
                                                }
                                                _ => {
                                                    continue
                                                }
                                            }
                                        }
                                    }
                                }
                                _ => {
                                    last_idx = *peeked_idx;
                                    self.advance();
                                    continue
                                }
                            }
                        }
                    }
                }
            }
            _ => panic!("Invalid second comment char")
        }
    }

    fn next_token(&mut self) -> Result<TokenSpan, TokenizationError> {
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
                    '+' => Ok((next_idx, TokType:: Plus, next_idx + 1)),
                    '-' => Ok((next_idx, TokType:: Minus, next_idx + 1)),
                    '\'' | '"' => self.process_string(),
                    '.' => self.process_number(),
                    c if c.is_ascii_digit() => self.process_number(),
                    c if c.is_whitespace() => {
                        let whitespace_tok = self.process_whitespace()?;
                        if self.include_whitespace {
                            Ok(whitespace_tok)
                        } else {
                            self.next_token()
                        }
                    },
                    '/' => {
                        let (_, next_next) = self.chars.peek().unwrap_or(&(usize::MAX, '!'));
                        match next_next {
                            '/' | '*' => self.process_comment(),
                            _ => {
                                return Err(self.make_error("unexpected token '/'".to_string(), next_idx))
                            }
                        }
                    }
                    _ => self.process_identifier_or_const()
                }
            }
        }
    }

    pub(crate) fn tokenize(&mut self) -> Result<Tokens<'input>, TokenizationError> {
        let mut tokens: Vec<TokenSpan> = Vec::new();
        loop {
            let tok = self.next_token()?;
            if tok.1 == TokType::EOF {
                tokens.push(tok);
                break
            } else {
                tokens.push(tok);
            }
        }
        Ok(Tokens{ tok_spans: tokens, source: self.text})
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
        let expected = Tokens{ tok_spans: vec![(0, EOF, 0)], source: text};
        assert_eq!(toks, expected);
    }

    #[test]
    fn test_heck() {
        let text = "{}";
        let toks = tokenize(text);
        let expected = Tokens{ tok_spans: vec![(0, LeftBrace, 1), (1, RightBrace, 2), (2, EOF, 2)], source: text};
        assert_eq!(toks, expected);
    }

    #[test]
    fn test_heck2() {
        let text = "{\"foo\":\"bar\"}";
        let toks = tokenize(text);
        let expected = Tokens{ tok_spans: vec![(0, LeftBrace, 1), (1, DoubleQuotedString, 6), (6, Colon, 7), (7, DoubleQuotedString, 12), (12, RightBrace, 13), (13, EOF, 13)], source: text};
        assert_eq!(toks, expected)
    }

    #[test]
    fn test_single_quoted_string() {
        let text = "{'foo':'bar'}";
        let toks = tokenize(text);
        let expected = Tokens{ tok_spans: vec![(0, LeftBrace, 1), (1, SingleQuotedString, 6), (6, Colon, 7), (7, SingleQuotedString, 12), (12, RightBrace, 13), (13, EOF, 13)], source: text};
        assert_eq!(toks, expected);
    }

    #[test]
    fn test_array() {
        let text = "[1,2,3]";
        let toks = tokenize(text);
        let expected = Tokens{ tok_spans: vec![(0, LeftBracket, 1), (1, Integer, 2), (2, Comma, 3), (3, Integer, 4), (4, Comma, 5), (5, Integer, 6), (6, RightBracket, 7), (7, EOF, 7)], source: text};
        assert_eq!(toks, expected);
    }

    #[test]
    fn test_float_number() {
        let text = "[1.23,4.56]";
        let toks = tokenize(text);
        let expected = Tokens{ tok_spans: vec![(0, LeftBracket, 1), (1, Float, 5), (5, Comma, 6), (6, Float, 10), (10, RightBracket, 11), (11, EOF, 11)], source: text};
        assert_eq!(toks, expected);
    }

    #[test]
    fn test_exponent_number() {
        let text = "[1e10,2e-5]";
        let toks = tokenize(text);
        let expected = Tokens{ tok_spans: vec![(0, LeftBracket, 1), (1, Exponent, 5), (5, Comma, 6), (6, Exponent, 10), (10, RightBracket, 11), (11, EOF, 11)], source: text};
        assert_eq!(toks, expected);
    }

    #[test]
    fn test_whitespace() {
        let text = " {\n\t} ";
        let toks = tokenize(text);
        let expected = Tokens{ tok_spans: vec![(1, LeftBrace, 2), (4, RightBrace, 5), (6, EOF, 6)], source: text};
        assert_eq!(toks, expected);
    }

    #[test]
    fn test_true_false_null() {
        let text = "[true,false,null]";
        let toks = tokenize(text);
        let expected = Tokens{source: text, tok_spans: vec![(0, LeftBracket, 1), (1, True, 5), (5, Comma, 6), (6, False, 11), (11, Comma, 12), (12, Null, 16), (16, RightBracket, 17), (17, EOF, 17)]};
        assert_eq!(toks, expected);
        }

    #[test]
    fn test_number() {
        let text = "123";
        let toks = tokenize(text);
        let expected = Tokens{source: text, tok_spans: vec![(0, Integer, 3), (3, EOF, 3)]};
        assert_eq!(toks, expected);

    }
}