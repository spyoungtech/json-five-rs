use std::fmt::{Display, Formatter};
use std::iter::{Peekable};
use std::str::{CharIndices};
use crate::utils::get_line_col_char;

#[derive(PartialEq, Clone, Debug)]
pub enum TokType {
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
    // Octal,
    Plus,
    Minus,
    EOF,
}


// Start byte offset, token type, end byte offset (noninclusive)
pub(crate) type TokenSpan = (usize, TokType, usize);




#[derive(Debug, PartialEq)]
pub struct Tokens<'input> {
    pub tok_spans: Vec<TokenSpan>,
    pub(crate) source: &'input str
}


#[derive(Debug)]
pub struct TokenizationError {
    pub message: String,
    pub index: usize, // byte offset
    pub lineno: usize,
    pub colno: usize,
    pub char_index: usize // char offset
}

impl<'input> Display for TokenizationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "TokenizationError: {}: line {} column {} (char {})", self.message, self.lineno, self.colno, self.char_index)
    }
}

#[derive(Debug)]
pub(crate) struct Tokenizer<'input> {
    configuration: TokenizerConfig,
    text: &'input str,
    chars: Peekable<CharIndices<'input>>,
    lookahead: Option<(usize, char)>,
}


const HEX_CHARS: &str = "0123456789abcdefABCDEF";
const IDENTIFIER_START_SYMBOLS: &str = "$_";
const IDENTIFIER_PARTS: &str = "$_\u{200C}\u{200D}\u{005F}\u{203F}\u{2040}\u{2054}\u{FE33}\u{FE34}\u{FE4D}\u{FE4E}\u{FE4F}\u{FF3F}";
#[derive(Debug)]
pub struct TokenizerConfig {
    pub include_whitespace: bool,
    pub include_comments: bool,
    pub allow_octal: bool,
}

impl Default for TokenizerConfig {
    fn default() -> Self {
        Self::new()
    }
}

impl TokenizerConfig {
    pub fn new() -> Self {
        TokenizerConfig {include_whitespace: false, include_comments: false, allow_octal: false}
    }
}

impl <'input> Tokenizer<'input> {
    pub fn new(text: &'input str) -> Self {
        Tokenizer {configuration: TokenizerConfig::new(), text, chars: text.char_indices().peekable(), lookahead: None}
    }

    pub fn with_configuration(text: &'input str, configuration: TokenizerConfig) -> Self {
        Tokenizer {configuration, text, chars: text.char_indices().peekable(), lookahead: None}
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        self.lookahead = self.chars.next();
        self.lookahead
    }

    fn make_error(&self, message: String, start_index: usize) -> TokenizationError {
        let (lineno, colno, char_index) = get_line_col_char(self.text, start_index);
        TokenizationError{message, index: start_index, lineno, colno, char_index}
    }

    fn process_string(&mut self) -> Result<TokenSpan, TokenizationError> {
        let (start_idx, quote_char) = self.lookahead.expect("Expected quote character");

        let string_type: TokType = match quote_char {
            '"' => TokType::DoubleQuotedString,
            '\'' => TokType::SingleQuotedString,
            _ => unreachable!("Expected quote character, but got {:?}", quote_char)
        };

        let mut last_char = quote_char;

        let mut escaping = false;
        loop {
            match self.advance() {
                None => {
                    break Err(self.make_error("Unterminated string starting at".to_string(), start_idx))
                },
                Some((idx, char)) => {
                    match char {
                        '\\' => {
                            escaping = !escaping;
                            last_char = char;
                            continue
                        }
                        '\n' => {
                            if !escaping && last_char != '\r' {
                                break Err(self.make_error("Unexpected line terminator without continuation in string literal at".to_string(), idx))
                            }
                            escaping = false;
                            last_char = char;
                            continue
                        }
                        '\r' | '\u{2028}' | '\u{2029}' => {
                            if !escaping {
                                break Err(self.make_error("Unexpected line terminator without continuation in string literal at".to_string(), idx))
                            }
                            escaping = false;
                            last_char = char;
                            continue
                        },
                        c if c == quote_char && !escaping => {
                            break Ok((start_idx, string_type, idx+1))
                        },
                        _ => {
                            escaping = false;
                            last_char = char;
                            continue
                        }
                    }
                }
            }
        }
    }

    fn process_whitespace(&mut self) -> Result<TokenSpan, TokenizationError> {
        let (start_idx, start_char) = self.lookahead.expect("Unexpected end of input, was expecting whitespace char");
        let mut last_index = start_idx;
        let mut last_char = start_char;
        loop {
            match self.chars.peek() {
                None => break Ok((start_idx, TokType::Whitespace, last_index + last_char.len_utf8())),
                Some((peeked_idx, peeked_char)) => {
                    if peeked_char.is_whitespace() {
                        last_index = *peeked_idx;
                        last_char = *peeked_char;
                        self.advance();
                        continue
                    } else {
                        break Ok((start_idx, TokType::Whitespace, last_index + last_char.len_utf8()))
                    }
                }
            }
        }
    }

    fn process_octal(&mut self) -> Result<TokenSpan, TokenizationError> {
        let (start_idx, _start_char) = self.lookahead.expect("Unexpected end of input, was processing octal");
        if self.configuration.allow_octal {
            todo!()
        } else {
            Err(self.make_error("Octal literals are forbidden".to_string(), start_idx))
        }
    }

    fn process_hexadecimal(&mut self) -> Result<TokenSpan, TokenizationError> {
        let (start_idx, start_char) = self.lookahead.expect("Unexpected end of input, was expecting numeric char");
        let (_, x_char) = self.advance().expect("Expected hex x");
        assert_eq!(start_char, '0');
        if x_char != 'x' && x_char != 'X' {
            unreachable!("Invalid hexadecimal here")
        }

        match self.advance() {
            None => {
                Err(self.make_error("Expected at least one digit in hexadecimal literal".to_string(), start_idx))
            }
            Some((mut last_idx, first_digit)) => {
                if !HEX_CHARS.contains(first_digit) {
                    return Err(self.make_error(format!("Invalid hexadecimal character {first_digit:?} in literal starting at"), start_idx))
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
        let mut last_index = start_idx;
        let mut decimal_seen: bool = false;
        let mut exponent_seen: bool = false;
        let mut unary_seen: bool = false;
        if start_char == '.' {
            decimal_seen = true
        }

        let maybe_second_char = self.chars.peek();
        match maybe_second_char {
            None => {
                if decimal_seen {
                    return Err(self.make_error("Lone decimal is an invalid literal".to_string(), start_idx))
                }
                return Ok((start_idx, TokType::Integer, start_idx + 1))
            },
            Some((_second_idx, second_char)) if start_char == '0' => {
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

        loop {
            match self.chars.peek() {
                None => {
                    if unary_seen || exponent_seen {
                        let (_, last_char) = self.lookahead.unwrap();
                        if "+-eE".contains(last_char) {
                            return Err(self.make_error(format!("Invalid number literal (missing digit after {last_char:?})"), start_idx))
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
                                    return Err(self.make_error(format!("Invalid number literal (missing digit after {last_char:?})"), start_idx))
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
        use crate::utils::read_hex_digits;
        let (start_idx, start_char) = self.lookahead.expect("Unexpected end of input, was expecting identifier/const char");
        let mut last_idx = start_idx;
        use unicode_general_category::{get_general_category, GeneralCategory};
        match start_char {
            c if c.is_alphabetic() => {}
            c if IDENTIFIER_START_SYMBOLS.contains(c) => {}
            '\\' => {
                match self.chars.peek() {
                    None => {return Err(self.make_error("Unexpected EOF".to_string(), start_idx))}
                    Some((_, c)) => {
                        match c {
                            'u' => {
                                let mut ubuffer = String::with_capacity(4);
                                self.advance();
                                for _ in 0..4 {
                                    match self.advance() {
                                        None => {
                                            return Err(self.make_error("Invalid identifier start".to_string(), start_idx))
                                        }
                                        Some((idx, c)) => {
                                            ubuffer.push(c);
                                            last_idx = idx;
                                            if !HEX_CHARS.contains(c) {
                                                return Err(self.make_error("Invalid identifier start".to_string(), start_idx))
                                            }
                                        }
                                    }
                                }
                                let maybe_hex_val = read_hex_digits(&mut ubuffer.chars().peekable(), 4, ubuffer.as_str());
                                match maybe_hex_val {
                                    Err(_) => {
                                        return Err(self.make_error(format!("invalid unicode escape: \\u{ubuffer}"), start_idx))
                                    }
                                    Ok(hex_val) => {
                                        let maybe_c = char::from_u32(hex_val);
                                        match maybe_c {
                                            None => {
                                                return Err(self.make_error(format!("invalid unicode escape value: \\u{ubuffer}"), start_idx))
                                            }
                                            Some(c) => {
                                                if !c.is_alphabetic() && !IDENTIFIER_START_SYMBOLS.contains(c) {
                                                    return Err(self.make_error(format!("Illegal identifier start from unicode escape sequence: \\u{ubuffer}"), start_idx))
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            _ => {
                                return Err(self.make_error("Invalid identifier start".to_string(), start_idx))
                            }
                        }
                    }
                }
            }
            _ => {
                return Err(self.make_error(format!("Invalid character {start_char}"), start_idx))
            }
        }
        let mut last_char = start_char;
        loop {
            match self.chars.peek() {
                None => break self.tok_from_indices(start_idx, last_idx + last_char.len_utf8()),
                Some((next_idx, next_char)) => {
                    if next_char.is_whitespace() {
                        break self.tok_from_indices(start_idx, last_idx + last_char.len_utf8())
                    } else if next_char.is_alphanumeric() {
                        last_idx = *next_idx;
                        last_char = *next_char;
                        self.advance();
                        continue
                    } else if IDENTIFIER_PARTS.contains(*next_char) {
                        last_idx = *next_idx;
                        last_char = *next_char;
                        self.advance();
                        continue
                    } else if *next_char == '\\' {
                        self.advance();
                        match self.advance() {
                            None => {return Err(self.make_error("Unexpected EOF".to_string(), start_idx))}
                            Some((_, c)) => {
                                match c {
                                    'u' => {
                                        for _ in 0..4 {
                                            match self.advance() {
                                                None => {
                                                    return Err(self.make_error("Invalid unquoted key1".to_string(), start_idx))
                                                }
                                                Some((_, c)) => {
                                                    if !HEX_CHARS.contains(c) {

                                                        return Err(self.make_error("Invalid unquoted key2".to_string(), start_idx))
                                                    }

                                                }
                                            }
                                        }
                                        (last_idx, last_char) = self.lookahead.unwrap()
                                    }
                                    _ => {
                                        return Err(self.make_error("Invalid unquoted key3".to_string(), start_idx))
                                    }
                                }
                            }
                        }
                    } else {
                        match get_general_category(*next_char) {
                            GeneralCategory::NonspacingMark | GeneralCategory::SpacingMark => {
                                last_idx = *next_idx;
                                last_char = *next_char;
                                self.advance();
                                continue
                            }
                            _ => break self.tok_from_indices(start_idx, last_idx + last_char.len_utf8())
                        }
                    }
                }
            }
        }
    }

    fn process_comment(&mut self) -> Result<TokenSpan, TokenizationError> {
        let (start_idx, _char) = self.lookahead.expect("Expected comment start");
        let (mut last_idx, star_or_slash) = self.advance().expect("Expected second comment char");
        match star_or_slash {
            '/' => {
                // line comment
                loop {
                    match self.chars.peek() {
                        None => {
                            return Ok((start_idx, TokType::LineComment, last_idx+1))
                        },
                        Some((peeked_idx, peeked_char)) => {
                            match peeked_char {
                                '\n' | '\r' | '\u{2028}' | '\u{2029}' => {
                                    (last_idx, _) = self.advance().unwrap();
                                    return Ok((start_idx, TokType::LineComment, last_idx+1))
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
                // block comment
                loop {
                    match self.chars.peek() {
                        None => {
                            return Err(self.make_error("Unexpected end of input while processing block comment".to_string(), start_idx))
                        }
                        Some((_peeked_idx, peeked_char)) => {
                            match peeked_char {
                                '*' => {
                                    self.advance();
                                    let maybe_next_next = self.chars.peek();
                                    match maybe_next_next {
                                        None => {
                                            return Err(self.make_error("Unexpected end of input while processing block comment".to_string(), start_idx))
                                        },
                                        Some((_next_peeked_idx, next_peeked_char)) => {
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
                                    self.advance();
                                    continue
                                }
                            }
                        }
                    }
                }
            }
            _ => unreachable!("Invalid second comment char")
        }
    }

    fn next_token(&mut self) -> Result<TokenSpan, TokenizationError> {
        let maybe_last = self.lookahead;
        let maybe_next = self.advance();
        match maybe_next {
            None => {
                match maybe_last {
                    Some((last_idx, last_char)) => Ok((last_idx + last_char.len_utf8(), TokType::EOF, last_idx + last_char.len_utf8())),
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
                    '\u{FEFF}' => {
                        let whitespace_tok = self.process_whitespace()?;
                        if self.configuration.include_whitespace {
                            Ok(whitespace_tok)
                        } else {
                            self.next_token()
                        }
                    }
                    c if c.is_whitespace() => {
                        let whitespace_tok = self.process_whitespace()?;
                        if self.configuration.include_whitespace {
                            Ok(whitespace_tok)
                        } else {
                            self.next_token()
                        }
                    },
                    c if c.is_ascii_digit() => self.process_number(),
                    '/' => {
                        let (_, next_next) = self.chars.peek().unwrap_or(&(usize::MAX, '!'));
                        match next_next {
                            '/' | '*' => {
                                if self.configuration.include_comments {
                                    self.process_comment()
                                } else {
                                    self.process_comment()?;
                                    self.next_token()
                                }
                            },
                            _ => {
                                Err(self.make_error("unexpected token '/'".to_string(), next_idx))
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

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<TokenSpan, TokenizationError>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(span) => {
                match span.1 {
                    TokType::EOF => {
                        None
                    }
                    _ => Some(Ok(span))
                }
            }
            Err(e) => {
                Some(Err(e))
            }
        }
    }
}


/// Turn str into [Tokens].
///
/// Usually not used directly.
/// Token spans will not include whitespace and comment tokens
pub fn tokenize_str(text: &'_ str) -> Result<Tokens<'_>, TokenizationError> {
    Tokenizer::new(text).tokenize()
}

/// Like [tokenize_str] but includes whitespace and comment tokens
pub fn tokenize_rt_str(text: &'_ str) -> Result<Tokens<'_>, TokenizationError> {
    let config = TokenizerConfig{include_comments: true, include_whitespace: true, allow_octal: false};
    Tokenizer::with_configuration(text, config).tokenize()
}

/// Like [tokenize_str] but for bytes
pub fn tokenize_bytes(bytes: &'_ [u8]) -> Result<Tokens<'_>, TokenizationError> {
    let maybe_text = std::str::from_utf8(bytes);
    match maybe_text {
        Ok(text) => {
            Tokenizer::new(text).tokenize()
        }
        Err(e) => {
            let valid_point = e.valid_up_to();
            if valid_point > 0 {
                let valid_text = std::str::from_utf8(&bytes[..valid_point]).unwrap();
                let (lineno, colno, char_index) = get_line_col_char(valid_text, valid_point);
                Err(TokenizationError{message: "Invalid UTF8 at".to_string(), lineno, colno, char_index, index: valid_point})
            } else {
                Err(TokenizationError{message: "Invalid UTF8 at".to_string(), lineno: 1, colno: 0, char_index: 0, index: 0})
            }
        }
    }
}

/// Like [tokenize_rt_str] but for bytes
pub fn tokenize_rt_bytes(bytes: &'_ [u8]) -> Result<Tokens<'_>, TokenizationError> {
    let maybe_text = std::str::from_utf8(bytes);
    match maybe_text {
        Ok(text) => {
            let config = TokenizerConfig{include_comments: true, include_whitespace: true, allow_octal: false};
            Tokenizer::with_configuration(text, config).tokenize()
        }
        Err(e) => {
            let valid_point = e.valid_up_to();
            if valid_point > 0 {
                let valid_text = std::str::from_utf8(&bytes[..valid_point]).unwrap();
                let (lineno, colno, char_index) = get_line_col_char(valid_text, valid_point);
                Err(TokenizationError{message: "Invalid UTF8 at".to_string(), lineno, colno, char_index, index: valid_point})
            } else {
                Err(TokenizationError{message: "Invalid UTF8 at".to_string(), lineno: 1, colno: 0, char_index: 0, index: 0})
            }
        }
    }
}



#[cfg(test)]
mod test {
    use crate::tokenize::TokType::*;
    use super::*;
    #[test]
    fn test_foo() {
        let text = "";
        let toks = tokenize_str(text).unwrap();
        let expected = Tokens{ tok_spans: vec![(0, EOF, 0)], source: text};
        assert_eq!(toks, expected);
    }

    #[test]
    fn test_heck() {
        let text = "{}";
        let toks = tokenize_str(text).unwrap();
        let expected = Tokens{ tok_spans: vec![(0, LeftBrace, 1), (1, RightBrace, 2), (2, EOF, 2)], source: text};
        assert_eq!(toks, expected);
    }

    #[test]
    fn test_heck2() {
        let text = "{\"foo\":\"bar\"}";
        let toks = tokenize_str(text).unwrap();
        let expected = Tokens{ tok_spans: vec![(0, LeftBrace, 1), (1, DoubleQuotedString, 6), (6, Colon, 7), (7, DoubleQuotedString, 12), (12, RightBrace, 13), (13, EOF, 13)], source: text};
        assert_eq!(toks, expected)
    }
    #[test]
    fn test_heck3() {
        let text = "{\"foo\":\"bar\"}";
        let toks = tokenize_rt_str(text).unwrap();
        let expected = Tokens{ tok_spans: vec![(0, LeftBrace, 1), (1, DoubleQuotedString, 6), (6, Colon, 7), (7, DoubleQuotedString, 12), (12, RightBrace, 13), (13, EOF, 13)], source: text};
        assert_eq!(toks, expected)
    }


    #[test]
    fn test_single_quoted_string() {
        let text = "{'foo':'bar'}";
        let toks = tokenize_str(text).unwrap();
        let expected = Tokens{ tok_spans: vec![(0, LeftBrace, 1), (1, SingleQuotedString, 6), (6, Colon, 7), (7, SingleQuotedString, 12), (12, RightBrace, 13), (13, EOF, 13)], source: text};
        assert_eq!(toks, expected);
    }

    #[test]
    fn test_array() {
        let text = "[1,2,3]";
        let toks = tokenize_str(text).unwrap();
        let expected = Tokens{ tok_spans: vec![(0, LeftBracket, 1), (1, Integer, 2), (2, Comma, 3), (3, Integer, 4), (4, Comma, 5), (5, Integer, 6), (6, RightBracket, 7), (7, EOF, 7)], source: text};
        assert_eq!(toks, expected);
    }

    #[test]
    fn test_float_number() {
        let text = "[1.23,4.56]";
        let toks = tokenize_str(text).unwrap();
        let expected = Tokens{ tok_spans: vec![(0, LeftBracket, 1), (1, Float, 5), (5, Comma, 6), (6, Float, 10), (10, RightBracket, 11), (11, EOF, 11)], source: text};
        assert_eq!(toks, expected);
    }

    #[test]
    fn test_exponent_number() {
        let text = "[1e10,2e-5]";
        let toks = tokenize_str(text).unwrap();
        let expected = Tokens{ tok_spans: vec![(0, LeftBracket, 1), (1, Exponent, 5), (5, Comma, 6), (6, Exponent, 10), (10, RightBracket, 11), (11, EOF, 11)], source: text};
        assert_eq!(toks, expected);
    }

    #[test]
    fn test_whitespace() {
        let text = " {\n\t} ";
        let toks = Tokenizer::with_configuration(text, TokenizerConfig{include_whitespace: true, include_comments: true, allow_octal: false}).tokenize().unwrap();
        let expected = Tokens{ tok_spans: vec![(0, Whitespace, 1), (1, LeftBrace, 2), (2, Whitespace, 4), (4, RightBrace, 5), (5, Whitespace, 6), (6, EOF, 6)], source: text};
        assert_eq!(toks, expected);
    }

    #[test]
    fn test_true_false_null() {
        let text = "[true,false,null]";
        let toks = tokenize_str(text).unwrap();
        let expected = Tokens{source: text, tok_spans: vec![(0, LeftBracket, 1), (1, True, 5), (5, Comma, 6), (6, False, 11), (11, Comma, 12), (12, Null, 16), (16, RightBracket, 17), (17, EOF, 17)]};
        assert_eq!(toks, expected);
        }

    #[test]
    fn test_number() {
        let text = "123";
        let toks = tokenize_str(text).unwrap();
        let expected = Tokens{source: text, tok_spans: vec![(0, Integer, 3), (3, EOF, 3)]};
        assert_eq!(toks, expected);

    }

    #[test]
    fn test_unexpected_symbol() {
        let text = "1!2";
        tokenize_str(text).unwrap_err();
    }

    #[test]
    fn test_special_things() {
        let text = r#"{$_:1,_$:2,a\u200C:3}"#;
        let toks = tokenize_str(text).unwrap();
        let expected = Tokens{source: text, tok_spans: vec![(0, LeftBrace, 1), (1, Name, 3), (3, Colon, 4), (4, Integer, 5), (5, Comma, 6), (6, Name, 8), (8, Colon, 9), (9, Integer, 10), (10, Comma, 11), (11, Name, 18), (18, Colon, 19), (19, Integer, 20), (20, RightBrace, 21), (21, EOF, 21)]};
        assert_eq!(toks, expected)
    }

    #[test]
    fn test_eof_after_multibyte() {
        let text = r#"ë"#;
        let toks = tokenize_str(text).unwrap();
        let expected = Tokens{source: text, tok_spans: vec![(0, Name, 2), (2, EOF, 2)]};
        assert_eq!(toks, expected)

    }
}