use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::vec::IntoIter;

use crate::tokenize::{TokenSpan, TokType};
use crate::tokenize::Tokens;
use crate::tokenize::TokType::{Colon, Comma, RightBrace};
use crate::utils::get_line_col_char;


#[derive(PartialEq, Debug)]
struct JSONKeyValuePair {
    key: JSONValue,
    value: JSONValue,
}


#[derive(PartialEq, Debug)]
enum UnaryOperator {
    Plus,
    Minus,
}

#[derive(PartialEq, Debug)]
struct Unary {
    operator: UnaryOperator,
    value: JSONValue,
}
#[derive(PartialEq, Debug)]
enum JSONValue {
    JSONObject { key_value_pairs: Vec<JSONKeyValuePair> },
    JSONArray { values: Vec<JSONValue> },
    Integer(isize),
    Float(f64), // also covers exponent
    Exponent(String),
    Null,
    Infinity,
    NaN,
    Bool(bool),
    DoubleQuotedString(String),
    SingleQuotedString(String),
    Unary { operator: UnaryOperator, value: Box<JSONValue> },
    Identifier(String), // XXX: for keys only!
}

#[derive(PartialEq, Debug)]
struct JSONText {
    value: JSONValue,
}

#[derive(Debug, PartialEq)]
struct ParsingError {
    index: usize, // byte offset
    message: String,
    lineno: usize,
    colno: usize,
    char_index: usize, // character offset
    
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ParsingError: {}: line {} column {} (char {})", self.message, self.lineno, self.colno, self.char_index)
    }
}

struct JSON5Parser<'toks, 'input> {
    // source_tokens: Vec<(&'toks (usize, TokType, usize), &'input str)>,
    source: &'input str,
    source_tokens: Peekable<IntoIter<(&'toks TokenSpan, &'input str)>>,
    lookahead: Option<(&'toks TokenSpan, &'input str)>,
}


impl<'toks, 'input> JSON5Parser<'toks, 'input> {
    fn new(tokens: &'toks Tokens<'input>) -> Self {
        JSON5Parser { source_tokens: tokens.tokens_with_source().into_iter().peekable(), lookahead: None, source: tokens.source }
    }

    fn advance(&mut self) -> Option<(&'toks TokenSpan, &'input str)> {
        match self.source_tokens.next() {
            None => {
                self.lookahead = None;
                None
            }
            Some((span, lexeme)) => {
                match span.1 {
                    TokType::BlockComment | TokType::LineComment | TokType::Whitespace => {
                        return self.advance()
                    }
                    _ => {
                        self.lookahead = Some((span, lexeme));
                        self.lookahead
                    }
                }
            }
        }
    }

    fn peek(&mut self) -> Option<&(&'toks TokenSpan, &'input str)> {
        self.source_tokens.peek()
    }

    fn position(&mut self) -> usize {
        match self.peek() {
            None => {
                match self.lookahead {
                    None => 0, // XXX: ???? might be end, actually?
                    Some((span, _)) => {span.2}
                }
            }
            Some((span, _)) => {
                span.0
            }
        }
    }

    fn make_error(&self, message: String, index: usize) -> ParsingError {
        let (lineno, colno, char_index) = get_line_col_char(self.source, index);
        ParsingError {
            index,
            message,
            lineno,
            colno,
            char_index,
        }
    }

    // fn check(&mut self, types: Vec<TokType>) -> Option<&(&'toks TokenSpan, &'input str)> {
    //     let (span, lexeme) = self.peek().unwrap();
    //     for toktype in types {
    //         if span.1 == toktype {
    //             return Some(&(*span, *lexeme));
    //         }
    //     }
    //     None
    // }

    fn check_and_consume(&mut self, types: Vec<TokType>) -> Option<(&'toks TokenSpan, &'input str)> {
        let (next_tok, lexeme) = self.peek().unwrap();
        for toktype in types {
            if next_tok.1 == toktype {
                return self.advance();
            }
        }
        None
    }

    fn parse_identifier(&mut self) -> Result<JSONValue, ParsingError> {
        match self.check_and_consume(vec![TokType::Name]) {
            None => self.parse_unary(),
            Some((span, lexeme)) => {
                Ok(JSONValue::Identifier(lexeme.to_string()))
            }
        }
    }

    fn parse_key(&mut self) -> Result<JSONValue, ParsingError>{
        self.parse_identifier()
    }



    fn parse_object(&mut self) -> Result<JSONValue, ParsingError> {
        let mut kvps: Vec<JSONKeyValuePair> = Vec::new();
        loop {
            match self.check_and_consume(vec![RightBrace]) {
                None => {
                    let key = self.parse_key()?;
                    match self.check_and_consume(vec![Colon]) {

                        None => {
                            let idx = self.position();
                            return Err(self.make_error("Expecting ':' delimiter".to_string(), idx))
                        }
                        Some(_) => {
                            let val = self.parse_value()?;
                            let kvp = JSONKeyValuePair{key: key, value: val};
                            kvps.push(kvp);
                            match self.check_and_consume(vec![Comma]) {
                                None => {
                                    break Ok(JSONValue::JSONObject {key_value_pairs: kvps})
                                }
                                Some(_) => {
                                    continue
                                }
                            }
                        }
                    }
                }
                Some(_) => {
                    break Ok(JSONValue::JSONObject {key_value_pairs: kvps})
                }
            }
        }
    }

    fn parse_array(&mut self) -> Result<JSONValue, ParsingError> {
        let mut values:Vec<JSONValue> = Vec::new();
        loop {
            match self.check_and_consume(vec![TokType::RightBracket]) {
                None => {
                    let val = self.parse_value()?;
                    values.push(val);
                    match self.check_and_consume(vec![Comma]) {
                        None => {
                            break Ok(JSONValue::JSONArray {values: values})
                        }
                        Some(_) => {
                            continue
                        }
                    }
                }
                Some(_) => {
                    break Ok(JSONValue::JSONArray {values: values})
                }
            }
        }
    }

    fn parse_primary(&mut self) -> Result<JSONValue, ParsingError> {
        let (span, lexeme) = self.advance().unwrap();
        match &span.1 {
            TokType::Integer => {
                let maybe_i: Result<isize, _> = lexeme.parse();
                match maybe_i {
                    Err(parse_err) => {
                        Err(self.make_error(format!("Unable to parse {:?} as integer", lexeme), span.0))
                    }
                    Ok(i) => { Ok(JSONValue::Integer(i)) }
                }
            }
            TokType::Float => {
                let maybe_f: Result<f64, _> = lexeme.parse();
                match maybe_f {
                    Err(parse_err) => {
                        Err(self.make_error(format!("Unable to parse {:?} as float", lexeme), span.0))
                    }
                    Ok(f) => { Ok(JSONValue::Float(f)) }
                }
            }
            TokType::Exponent => {
                todo!()
            }
            TokType::SingleQuotedString => Ok(JSONValue::SingleQuotedString(lexeme[1..lexeme.len() - 1].to_string())),
            TokType::DoubleQuotedString => Ok(JSONValue::DoubleQuotedString(lexeme[1..lexeme.len() - 1].to_string())),
            TokType::True => Ok(JSONValue::Bool(true)),
            TokType::False => Ok(JSONValue::Bool(false)),
            TokType::Null => Ok(JSONValue::Null),
            TokType::Infinity => Ok(JSONValue::Infinity),
            TokType::Nan => Ok(JSONValue::NaN),
            TokType::Hexadecimal => todo!(),
            t => panic!("Unexpected token of type {:?}: {:?}", t, lexeme)
        }
    }

    fn parse_unary(&mut self) -> Result<JSONValue, ParsingError> {
        match self.check_and_consume(vec![TokType::Plus, TokType::Minus]) {
            None => self.parse_primary(),
            Some((span, lexeme)) => {
                match span.1 {
                    TokType::Plus => {
                        let value = self.parse_unary()?; // TODO: validate value is appropriate for unary
                        Ok(JSONValue::Unary {operator: UnaryOperator::Plus, value: Box::new(value)})
                    }
                    TokType::Minus => {
                        let value = self.parse_unary()?; // TODO: validate value is appropriate for unary
                        Ok(JSONValue::Unary {operator: UnaryOperator::Minus, value: Box::new(value)})
                    }
                    _ => panic!("no")
                }
            }
        }
    }

    fn parse_obj_or_array(&mut self) -> Result<JSONValue, ParsingError> {
        match self.check_and_consume(vec![TokType::LeftBracket, TokType::LeftBrace]) {
            None => self.parse_unary(),
            Some((span, lexeme)) => {
                match span.1 {
                    TokType::LeftBrace => self.parse_object(),
                    TokType::LeftBracket => self.parse_array(),
                    _ => panic!("no")
                }
            }
        }
    }


    fn parse_value(&mut self) -> Result<JSONValue, ParsingError> {
        self.parse_obj_or_array()
    }

    fn parse_text(&mut self) -> Result<JSONText, ParsingError> {
        let value = self.parse_value()?;
        Ok(JSONText { value: value })
    }
}

fn parse(tokens: &Tokens) -> Result<JSONText, ParsingError> {
    let mut parser = JSON5Parser::new(tokens);
    parser.parse_text()
}

fn parse_text(source: &str) -> Result<JSONText, ParsingError> {
    use crate::tokenize::tokenize;
    let toks = tokenize(source);
    parse(&toks)
}

#[cfg(test)]
mod tests {
    use crate::parser::JSONValue::*;
    use super::*;
    #[test]
    fn test_foo() {
        let res = parse_text("{}").unwrap();
        let expected = JSONText{value: JSONValue::JSONObject {key_value_pairs: vec![]}};
        assert_eq!(res, expected)
    }

    #[test]
    fn test_object() {
        let res = parse_text("{\"foo\": \"bar\"}").unwrap();
        let expected = JSONText{value: JSONValue::JSONObject {key_value_pairs: vec![JSONKeyValuePair{key: JSONValue::DoubleQuotedString("foo".to_string()), value: JSONValue::DoubleQuotedString("bar".to_string())}]}};
        assert_eq!(res, expected)
    }

    #[test]
    fn test_identifier(){
        let res = parse_text("{foo: \"bar\"}").unwrap();
        let expected = JSONText{value: JSONValue::JSONObject {key_value_pairs: vec![JSONKeyValuePair{key: JSONValue::Identifier("foo".to_string()), value: JSONValue::DoubleQuotedString("bar".to_string())}]}};
        assert_eq!(res, expected)
    }

    #[test]
    fn test_array() {
        let res = parse_text("[1,2,3]").unwrap();
        let expected = JSONText{value: JSONArray {values: vec![JSONValue::Integer(1), JSONValue::Integer(2), JSONValue::Integer(3)]}};
        assert_eq!(res, expected)
    }

    #[test]
    fn val_int() {
        let res = parse_text("1").unwrap();
        let expected = JSONText{value: Integer(1)};
        assert_eq!(res, expected)
    }

    #[test]
    fn val_float() {
        let res = parse_text("1.0").unwrap();
        let expected = JSONText{value: Float(1.0)};
        assert_eq!(res, expected)
    }

    #[test]
    fn val_string() {
        let res = parse_text("'foo'").unwrap();
        let expected = JSONText{value: SingleQuotedString("foo".to_string())};
        assert_eq!(res, expected)
    }
    #[test]
    fn test_empty_string() {
        let res = parse_text("\"\"").unwrap();
        let expected = JSONText { value: DoubleQuotedString("".to_string()) };
        assert_eq!(res, expected);
    }

    #[test]
    fn test_single_element_array() {
        let res = parse_text("[42]").unwrap();
        let expected = JSONText { value: JSONArray { values: vec![Integer(42)] } };
        assert_eq!(res, expected);
    }

    #[test]
    fn test_single_key_object() {
        let res = parse_text("{\"key\": \"value\"}").unwrap();
        let expected = JSONText {
            value: JSONObject {
                key_value_pairs: vec![
                    JSONKeyValuePair {
                        key: DoubleQuotedString("key".to_string()),
                        value: DoubleQuotedString("value".to_string()),
                    }
                ]
            }
        };
        assert_eq!(res, expected);
    }

    #[test]
    fn test_trailing_comma_in_array() {
        let res = parse_text("[1, 2, 3,]").unwrap();
        let expected = JSONText {
            value: JSONArray {
                values: vec![Integer(1), Integer(2), Integer(3)]
            }
        };
        assert_eq!(res, expected);
    }

    #[test]
    fn test_trailing_comma_in_object() {
        let res = parse_text("{\"a\": 1, \"b\": 2,}").unwrap();
        let expected = JSONText {
            value: JSONObject {
                key_value_pairs: vec![
                    JSONKeyValuePair {
                        key: DoubleQuotedString("a".to_string()),
                        value: Integer(1),
                    },
                    JSONKeyValuePair {
                        key: DoubleQuotedString("b".to_string()),
                        value: Integer(2),
                    }
                ]
            }
        };
        assert_eq!(res, expected);
    }

    #[test]
    fn test_unquoted_key() {
        let res = parse_text("{key: \"value\"}").unwrap();
        let expected = JSONText {
            value: JSONObject {
                key_value_pairs: vec![
                    JSONKeyValuePair {
                        key: Identifier("key".to_string()),
                        value: DoubleQuotedString("value".to_string()),
                    }
                ]
            }
        };
        assert_eq!(res, expected);
    }

    #[test]
    fn test_multiline_string() {
        let res = parse_text("\"multi\\\nline\"").unwrap();
        let expected = JSONText { value: DoubleQuotedString("multi\\\nline".to_string()) };
        assert_eq!(res, expected);
    }

    #[test]
    fn test_unicode_characters() {
        let res = parse_text("\"\\u2764\"").unwrap();
        let expected = JSONText { value: DoubleQuotedString("\\u2764".to_string()) };
        assert_eq!(res, expected);
    }
    #[test]
    fn test_trailing_comma_in_nested_array() {
        let res = parse_text("[[1, 2,],]").unwrap();
        let expected = JSONText {
            value: JSONArray {
                values: vec![
                    JSONArray { values: vec![Integer(1), Integer(2)] }
                ]
            }
        };
        assert_eq!(res, expected);
    }
}
