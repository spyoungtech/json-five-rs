use std::error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::vec::IntoIter;

use crate::tokenize::{TokenSpan, TokType};
use crate::tokenize::Tokens;
use crate::tokenize::TokType::{Colon, Comma, RightBrace};
use crate::utils::get_line_col_char;


#[derive(PartialEq, Debug)]
pub struct JSONKeyValuePair {
    pub(crate) key: JSONValue,
    pub(crate) value: JSONValue,
}


#[derive(PartialEq, Debug)]
pub enum UnaryOperator {
    Plus,
    Minus,
}


#[derive(PartialEq, Debug)]
pub enum JSONValue {
    JSONObject { key_value_pairs: Vec<JSONKeyValuePair> },
    JSONArray { values: Vec<JSONValue> },
    Integer(String),
    Float(String),
    Exponent(String),
    Null,
    Infinity,
    NaN,
    Hexadecimal(String),
    Bool(bool),
    DoubleQuotedString(String),
    SingleQuotedString(String),
    Unary { operator: UnaryOperator, value: Box<JSONValue> },
    Identifier(String), // XXX: for keys only!
}

#[derive(PartialEq, Debug)]
pub struct JSONText {
    pub(crate) value: JSONValue,
}

enum TrailingComma {
    ALL,
    OBJECTS,
    ARRAYS,
    NONE
}

struct StyleConfiguration {
    indent: Option<usize>,
    item_separator: String,
    key_separator: String,
    current_indent: usize,
    trailing_comma: TrailingComma
}

impl StyleConfiguration {
    pub fn new(indent: Option<usize>, item_separator: &str, key_separator: &str, trailing_comma: TrailingComma) -> Self {
        StyleConfiguration{indent: None, item_separator: item_separator.to_string(), key_separator: key_separator.to_string(), current_indent: 0, trailing_comma: trailing_comma}
    }

    pub fn with_indent(indent: usize, trailing_comma: TrailingComma) -> Self {
        todo!()
    }

    pub fn with_separators(item_separator: &str, key_separator: &str, trailing_comma: TrailingComma) -> Self {
        todo!()
    }

    pub fn default() -> Self {
        StyleConfiguration{indent: None, item_separator: ", ".to_string(), key_separator: ": ".to_string(), current_indent: 0, trailing_comma: TrailingComma::NONE}

    }
}

impl JSONKeyValuePair {
    fn to_string_styled(&self, style: &mut StyleConfiguration) -> String {
        format!("{}{}{}", self.key.to_string_styled(style), style.item_separator, self.value)
    }
}

impl JSONValue {
    fn to_string_styled(&self, style: &mut StyleConfiguration) -> String {
        match self {
            JSONValue::Identifier(s) | JSONValue::Integer(s) | JSONValue::Float(s) | JSONValue::Exponent(s) | JSONValue::Hexadecimal(s) => {
                format!("{}", s)
            }
            JSONValue::Bool(b) => {
                format!("{}", b)
            }
            JSONValue::DoubleQuotedString(s) => {
                format!("\"{}\"", s)
            }
            JSONValue::SingleQuotedString(s) => {
                format!("'{}'", s)
            }

            JSONValue::Null => {"null".to_string()}
            JSONValue::Infinity => {"Infinity".to_string()}
            JSONValue::NaN => {"NaN".to_string()}

            JSONValue::Unary { operator, value } => {
                let op_char = match operator {
                    UnaryOperator::Plus => {'+'}
                    UnaryOperator::Minus => {'-'}
                };
                let value_string = value.to_string();
                format!("{}{}", op_char, value_string)
            }
            JSONValue::JSONObject { key_value_pairs} => {
                let mut ret: String;

                match style.indent {
                    None => {
                        ret = String::from("{");
                    }
                    Some(ident) => {
                        style.current_indent += ident;
                        ret = format!("{{\n{}", style.current_indent);
                    }
                }
                for (idx, kvp) in key_value_pairs.iter().enumerate() {
                    ret.push_str(kvp.to_string_styled(style).as_str());
                    if idx < key_value_pairs.len() - 1 {
                        match style.indent {
                            None => {
                                ret.push_str(style.item_separator.as_str());
                            }
                            Some(ident) => {
                                ret.push_str(format!(",\n{}", style.current_indent).as_str())
                            }
                        }
                    }
                }
                match style.trailing_comma {
                    TrailingComma::ALL | TrailingComma::OBJECTS => {
                        ret.push(',');
                    }
                    _ => {}
                }
                match style.indent {
                    None => {
                        ret.push_str("}");
                    }
                    Some(ident) => {
                        style.current_indent -= ident;
                        ret.push_str(format!("\n{}}}", style.current_indent).as_str());
                    }
                }
                ret
            }
            JSONValue::JSONArray { .. } => {
                todo!()
            }
        }
    }
}


impl Display for JSONValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut style = StyleConfiguration::default();
        let res = self.to_string_styled(&mut style);
        write!(f, "{}", res)
    }
}


impl Display for JSONText {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value.to_string())
    }
}



#[derive(Debug, PartialEq)]
pub struct ParsingError {
    pub index: usize, // byte offset
    pub message: String,
    pub lineno: usize,
    pub colno: usize,
    pub char_index: usize, // character offset
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
        JSON5Parser { source_tokens: tokens.spans_with_source().into_iter().peekable(), lookahead: None, source: tokens.source }
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

    fn peek(&mut self) -> Option<(&'toks TokenSpan, &'input str)> {
        match self.source_tokens.peek() {
            None => None,
            Some((span, lexeme)) => {
                match span.1 {
                    TokType::BlockComment | TokType::LineComment | TokType::Whitespace => {
                        self.source_tokens.next();
                        return self.peek()
                    }
                    _ => {
                        Some((span, lexeme))
                    }
                }
            }
        }
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
        let (next_tok, _lexeme) = self.peek().unwrap();
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
                                    match self.check_and_consume(vec![RightBrace]) {
                                        None => {
                                            let idx = self.position();
                                            return Err(self.make_error("Expecting '}' at end of object".to_string(), idx))
                                        },
                                        Some(_) => {
                                            break Ok(JSONValue::JSONObject {key_value_pairs: kvps})
                                        }
                                    }

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
                            match self.check_and_consume(vec![TokType::RightBracket]) {
                                None => {
                                    let idx = self.position();
                                    return Err(self.make_error("Expecting ']' at end of array".to_string(), idx))
                                },
                                Some(_) => {
                                    break Ok(JSONValue::JSONArray {values: values})
                                }
                            }
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
            TokType::Integer => {Ok(JSONValue::Integer(lexeme.to_string()))}
            TokType::Float => {Ok(JSONValue::Float(lexeme.to_string()))}
            TokType::Exponent => { Ok(JSONValue::Exponent(lexeme.to_string()))}
            TokType::SingleQuotedString => Ok(JSONValue::SingleQuotedString(lexeme[1..lexeme.len() - 1].to_string())),
            TokType::DoubleQuotedString => Ok(JSONValue::DoubleQuotedString(lexeme[1..lexeme.len() - 1].to_string())),
            TokType::True => Ok(JSONValue::Bool(true)),
            TokType::False => Ok(JSONValue::Bool(false)),
            TokType::Null => Ok(JSONValue::Null),
            TokType::Infinity => Ok(JSONValue::Infinity),
            TokType::Nan => Ok(JSONValue::NaN),
            TokType::Hexadecimal => Ok(JSONValue::Hexadecimal(lexeme.to_string())),
            TokType::EOF => {
                match self.position() {
                    0 => Err(self.make_error("Unexpected EOF. Was expecting value.".to_string(), 0)),
                    pos => Err(self.make_error("Unexpected EOF".to_string(), pos-1))
                }
            },
            t => Err(self.make_error(format!("Unexpected token of type {:?}: {:?}", t, lexeme), span.0))
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
        match self.advance() {
            None => {}
            Some((span, lexeme)) => {
                if span.1 != TokType::EOF {
                    return Err(self.make_error(format!("Unexpected {:?} token after value", span.1), span.0 - 1))
                }
            }
        }
        Ok(JSONText { value: value })
    }
}

pub fn from_tokens(tokens: &Tokens) -> Result<JSONText, ParsingError> {
    let mut parser = JSON5Parser::new(tokens);
    parser.parse_text()
}

pub fn from_str(source: &str) -> Result<JSONText, ParsingError> {
    use crate::tokenize::tokenize_str;
    let maybe_toks = tokenize_str(source);
    match maybe_toks {
        Err(e) => {
            Err(ParsingError{index: e.index, message: e.message, char_index: e.char_index, lineno: e.lineno, colno: e.colno})
        }
        Ok(toks) => {
            from_tokens(&toks)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenize::Tokenizer;
    use crate::parser::JSONValue::*;
    use super::*;
    #[test]
    fn test_foo() {
        let res = from_str("{}").unwrap();
        let expected = JSONText{value: JSONValue::JSONObject {key_value_pairs: vec![]}};
        assert_eq!(res, expected)
    }

    #[test]
    fn test_object() {
        let res = from_str("{\"foo\": \"bar\"}").unwrap();
        let expected = JSONText{value: JSONValue::JSONObject {key_value_pairs: vec![JSONKeyValuePair{key: JSONValue::DoubleQuotedString("foo".to_string()), value: JSONValue::DoubleQuotedString("bar".to_string())}]}};
        assert_eq!(res, expected)
    }

    #[test]
    fn test_identifier(){
        let res = from_str("{foo: \"bar\"}").unwrap();
        let expected = JSONText{value: JSONValue::JSONObject {key_value_pairs: vec![JSONKeyValuePair{key: JSONValue::Identifier("foo".to_string()), value: JSONValue::DoubleQuotedString("bar".to_string())}]}};
        assert_eq!(res, expected)
    }

    #[test]
    fn test_array() {
        let res = from_str("[1,2,3]").unwrap();
        let expected = JSONText{value: JSONArray {values: vec![JSONValue::Integer("1".to_string()), JSONValue::Integer("2".to_string()), JSONValue::Integer("3".to_string())]}};
        assert_eq!(res, expected)
    }

    #[test]
    fn val_int() {
        let res = from_str("1").unwrap();
        let expected = JSONText{value: Integer("1".to_string())};
        assert_eq!(res, expected)
    }

    #[test]
    fn val_float() {
        let res = from_str("1.0").unwrap();
        let expected = JSONText{value: Float("1.0".to_string())};
        assert_eq!(res, expected)
    }

    #[test]
    fn val_string() {
        let res = from_str("'foo'").unwrap();
        let expected = JSONText{value: SingleQuotedString("foo".to_string())};
        assert_eq!(res, expected)
    }

    #[test]
    fn multiline_string() {
        let res = from_str("'foo\\\nbar'").unwrap();
        let expected = JSONText{value: SingleQuotedString("foo\\\nbar".to_string())};
        assert_eq!(res, expected)
    }

    #[test]
    fn test_empty_string() {
        let res = from_str("\"\"").unwrap();
        let expected = JSONText { value: DoubleQuotedString("".to_string()) };
        assert_eq!(res, expected);
    }

    #[test]
    fn test_single_element_array() {
        let res = from_str("[42]").unwrap();
        let expected = JSONText { value: JSONArray { values: vec![Integer("42".to_string())] } };
        assert_eq!(res, expected);
    }

    #[test]
    fn test_single_key_object() {
        let res = from_str("{\"key\": \"value\"}").unwrap();
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
        let res = from_str("[1, 2, 3,]").unwrap();
        let expected = JSONText {
            value: JSONArray {
                values: vec![Integer("1".to_string()), Integer("2".to_string()), Integer("3".to_string())]
            }
        };
        assert_eq!(res, expected);
    }

    #[test]
    fn test_trailing_comma_in_object() {
        let res = from_str("{\"a\": 1, \"b\": 2,}").unwrap();
        let expected = JSONText {
            value: JSONObject {
                key_value_pairs: vec![
                    JSONKeyValuePair {
                        key: DoubleQuotedString("a".to_string()),
                        value: Integer("1".to_string()),
                    },
                    JSONKeyValuePair {
                        key: DoubleQuotedString("b".to_string()),
                        value: Integer("2".to_string()),
                    }
                ]
            }
        };
        assert_eq!(res, expected);
    }

    #[test]
    fn test_unquoted_key() {
        let res = from_str("{key: \"value\"}").unwrap();
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
        let res = from_str("\"multi\\\nline\"").unwrap();
        let expected = JSONText { value: DoubleQuotedString("multi\\\nline".to_string()) };
        assert_eq!(res, expected);
    }

    #[test]
    fn test_unicode_characters() {
        let res = from_str("\"\\u2764\"").unwrap();
        let expected = JSONText { value: DoubleQuotedString("\\u2764".to_string()) };
        assert_eq!(res, expected);
    }
    #[test]
    fn test_trailing_comma_in_nested_array() {
        let res = from_str("[[1, 2,],]").unwrap();
        let expected = JSONText {
            value: JSONArray {
                values: vec![
                    JSONArray { values: vec![Integer("1".to_string()), Integer("2".to_string())] }
                ]
            }
        };
        assert_eq!(res, expected);
    }

    // Start Auto-generated tests
    // The following test cases are largely pulled from the json5-tests project https://github.com/json5/json5-tests
    // Used under MIT license  https://github.com/json5/json5-tests/blob/c9af328e6d77286d78b77b520c4622d588b544c0/LICENSE.md
    #[test]
    fn test_empty_array() {
        let sample = r#"[]"#;
        let res = from_str(sample).unwrap();
        let expected = JSONText{ value: JSONArray {values: vec![]}};
        assert_eq!(res, expected)
    }



    #[test]
    fn test_leading_comma_array() {
        let sample = r#"[
    ,null
]"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_lone_trailing_comma_array() {
        let sample = r#"[
    ,
]"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_no_comma_array() {
        let sample = r#"[
    true
    false
]"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_regular_array() {
        let sample = r#"[
    true,
    false,
    null
]"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_trailing_comma_array() {
        let sample = r#"[
    null,
]"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_block_comment_following_array_element() {
        let sample = r#"[
    false
    /*
        true
    */
]"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_block_comment_following_top_level_value() {
        let sample = r#"null
/*
    Some non-comment top-level value is needed;
    we use null above.
*/"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_block_comment_in_string() {
        let sample = r#""This /* block comment */ isn't really a block comment.""#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_block_comment_preceding_top_level_value() {
        let sample = r#"/*
    Some non-comment top-level value is needed;
    we use null below.
*/
null"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_block_comment_with_asterisks() {
        let sample = r#"/**
 * This is a JavaDoc-like block comment.
 * It contains asterisks inside of it.
 * It might also be closed with multiple asterisks.
 * Like this:
 **/
true"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_inline_comment_following_array_element() {
        let sample = r#"[
    false   // true
]"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_inline_comment_following_top_level_value() {
        let sample = r#"null // Some non-comment top-level value is needed; we use null here."#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_inline_comment_in_string() {
        let sample = r#""This inline comment // isn't really an inline comment.""#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_inline_comment_preceding_top_level_value() {
        let sample = r#"// Some non-comment top-level value is needed; we use null below.
null"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_top_level_block_comment() {
        let sample = r#"/*
    This should fail;
    comments cannot be the only top-level value.
*/"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_top_level_inline_comment() {
        let sample = r#"// This should fail; comments cannot be the only top-level value."#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_unterminated_block_comment() {
        let sample = r#"true
/*
    This block comment doesn't terminate.
    There was a legitimate value before this,
    but this is still invalid JS/JSON5.
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_empty() {
        let sample = r#""#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_npm_package() {
        let sample = r#"{
  "name": "npm",
  "publishConfig": {
    "proprietary-attribs": false
  },
  "description": "A package manager for node",
  "keywords": [
    "package manager",
    "modules",
    "install",
    "package.json"
  ],
  "version": "1.1.22",
  "preferGlobal": true,
  "config": {
    "publishtest": false
  },
  "homepage": "http://npmjs.org/",
  "author": "Isaac Z. Schlueter <i@izs.me> (http://blog.izs.me)",
  "repository": {
    "type": "git",
    "url": "https://github.com/isaacs/npm"
  },
  "bugs": {
    "email": "npm-@googlegroups.com",
    "url": "http://github.com/isaacs/npm/issues"
  },
  "directories": {
    "doc": "./doc",
    "man": "./man",
    "lib": "./lib",
    "bin": "./bin"
  },
  "main": "./lib/npm.js",
  "bin": "./bin/npm-cli.js",
  "dependencies": {
    "semver": "~1.0.14",
    "ini": "1",
    "slide": "1",
    "abbrev": "1",
    "graceful-fs": "~1.1.1",
    "minimatch": "~0.2",
    "nopt": "1",
    "node-uuid": "~1.3",
    "proto-list": "1",
    "rimraf": "2",
    "request": "~2.9",
    "which": "1",
    "tar": "~0.1.12",
    "fstream": "~0.1.17",
    "block-stream": "*",
    "inherits": "1",
    "mkdirp": "0.3",
    "read": "0",
    "lru-cache": "1",
    "node-gyp": "~0.4.1",
    "fstream-npm": "0 >=0.0.5",
    "uid-number": "0",
    "archy": "0",
    "chownr": "0"
  },
  "bundleDependencies": [
    "slide",
    "ini",
    "semver",
    "abbrev",
    "graceful-fs",
    "minimatch",
    "nopt",
    "node-uuid",
    "rimraf",
    "request",
    "proto-list",
    "which",
    "tar",
    "fstream",
    "block-stream",
    "inherits",
    "mkdirp",
    "read",
    "lru-cache",
    "node-gyp",
    "fstream-npm",
    "uid-number",
    "archy",
    "chownr"
  ],
  "devDependencies": {
    "ronn": "https://github.com/isaacs/ronnjs/tarball/master"
  },
  "engines": {
    "node": "0.6 || 0.7 || 0.8",
    "npm": "1"
  },
  "scripts": {
    "test": "node ./test/run.js",
    "prepublish": "npm prune; rm -rf node_modules/*/{test,example,bench}*; make -j4 doc",
    "dumpconf": "env | grep npm | sort | uniq"
  },
  "licenses": [
    {
      "type": "MIT +no-false-attribs",
      "url": "http://github.com/isaacs/npm/raw/master/LICENSE"
    }
  ]
}
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_npm_package2() {
        let sample = r#"{
  name: 'npm',
  publishConfig: {
    'proprietary-attribs': false,
  },
  description: 'A package manager for node',
  keywords: [
    'package manager',
    'modules',
    'install',
    'package.json',
  ],
  version: '1.1.22',
  preferGlobal: true,
  config: {
    publishtest: false,
  },
  homepage: 'http://npmjs.org/',
  author: 'Isaac Z. Schlueter <i@izs.me> (http://blog.izs.me)',
  repository: {
    type: 'git',
    url: 'https://github.com/isaacs/npm',
  },
  bugs: {
    email: 'npm-@googlegroups.com',
    url: 'http://github.com/isaacs/npm/issues',
  },
  directories: {
    doc: './doc',
    man: './man',
    lib: './lib',
    bin: './bin',
  },
  main: './lib/npm.js',
  bin: './bin/npm-cli.js',
  dependencies: {
    semver: '~1.0.14',
    ini: '1',
    slide: '1',
    abbrev: '1',
    'graceful-fs': '~1.1.1',
    minimatch: '~0.2',
    nopt: '1',
    'node-uuid': '~1.3',
    'proto-list': '1',
    rimraf: '2',
    request: '~2.9',
    which: '1',
    tar: '~0.1.12',
    fstream: '~0.1.17',
    'block-stream': '*',
    inherits: '1',
    mkdirp: '0.3',
    read: '0',
    'lru-cache': '1',
    'node-gyp': '~0.4.1',
    'fstream-npm': '0 >=0.0.5',
    'uid-number': '0',
    archy: '0',
    chownr: '0',
  },
  bundleDependencies: [
    'slide',
    'ini',
    'semver',
    'abbrev',
    'graceful-fs',
    'minimatch',
    'nopt',
    'node-uuid',
    'rimraf',
    'request',
    'proto-list',
    'which',
    'tar',
    'fstream',
    'block-stream',
    'inherits',
    'mkdirp',
    'read',
    'lru-cache',
    'node-gyp',
    'fstream-npm',
    'uid-number',
    'archy',
    'chownr',
  ],
  devDependencies: {
    ronn: 'https://github.com/isaacs/ronnjs/tarball/master',
  },
  engines: {
    node: '0.6 || 0.7 || 0.8',
    npm: '1',
  },
  scripts: {
    test: 'node ./test/run.js',
    prepublish: 'npm prune; rm -rf node_modules/*/{test,example,bench}*; make -j4 doc',
    dumpconf: 'env | grep npm | sort | uniq',
  },
  licenses: [
    {
      type: 'MIT +no-false-attribs',
      url: 'http://github.com/isaacs/npm/raw/master/LICENSE',
    },
  ],
}
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_readme_example() {
        let sample = r#"{
    foo: 'bar',
    while: true,

    this: 'is a \
multi-line string',

    // this is an inline comment
    here: 'is another', // inline comment

    /* this is a block comment
       that continues on another line */

    hex: 0xDEADbeef,
    half: .5,
    delta: +10,
    to: Infinity,   // and beyond!

    finally: 'a trailing comma',
    oh: [
        "we shouldn't forget",
        'arrays can have',
        'trailing commas too',
    ],
}
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_valid_whitespace() {
        let sample = r#"{
    // An invalid form feed character (\x0c) has been entered before this comment.
    // Be careful not to delete it.
  "a": true
}
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_comment_cr() {
        let sample = r#"{
    // This comment is terminated with `\r`.
}
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_comment_crlf() {
        let sample = r#"{
    // This comment is terminated with `\r\n`.
}
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_comment_lf() {
        let sample = r#"{
    // This comment is terminated with `\n`.
}
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_escaped_cr() {
        let sample = r#"{
    // the following string contains an escaped `\r`
    a: 'line 1 \
line 2'
}
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_escaped_crlf() {
        let sample = r#"{
    // the following string contains an escaped `\r\n`
    a: 'line 1 \
line 2'
}
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_escaped_lf() {
        let sample = r#"{
    // the following string contains an escaped `\n`
    a: 'line 1 \
line 2'
}
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_float_leading_decimal_point() {
        let sample = r#".5
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_float_leading_zero() {
        let sample = r#"0.5
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_float_trailing_decimal_point_with_integer_exponent() {
        let sample = r#"5.e4
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_float_trailing_decimal_point() {
        let sample = r#"5.
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_float_with_integer_exponent() {
        let sample = r#"1.2e3
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_float() {
        let sample = r#"1.2
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_hexadecimal_empty() {
        let sample = r#"0x
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_hexadecimal_lowercase_letter() {
        let sample = r#"0xc8
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_hexadecimal_uppercase_x() {
        let sample = r#"0XC8
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_hexadecimal_with_integer_exponent() {
        let sample = r#"0xc8e4
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_hexadecimal() {
        let sample = r#"0xC8
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_infinity() {
        let sample = r#"Infinity
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_integer_with_float_exponent() {
        let sample = r#"1e2.3
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err(), "{:?}", res.unwrap());
        }
    }


    #[test]
    fn test_integer_with_hexadecimal_exponent() {
        let sample = r#"1e0x4
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_integer_with_integer_exponent() {
        let sample = r#"2e23
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_integer_with_negative_float_exponent() {
        let sample = r#"1e-2.3
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_integer_with_negative_hexadecimal_exponent() {
        let sample = r#"1e-0x4
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err(), "{:?}", res.unwrap());
        }
    }


    #[test]
    fn test_integer_with_negative_integer_exponent() {
        let sample = r#"2e-23
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_integer_with_negative_zero_integer_exponent() {
        let sample = r#"5e-0
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_integer_with_positive_float_exponent() {
        let sample = r#"1e+2.3
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_integer_with_positive_hexadecimal_exponent() {
        let sample = r#"1e+0x4
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_integer_with_positive_integer_exponent() {
        let sample = r#"1e+2
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_integer_with_positive_zero_integer_exponent() {
        let sample = r#"5e+0
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_integer_with_zero_integer_exponent() {
        let sample = r#"5e0
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_integer() {
        let sample = r#"15
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_lone_decimal_point() {
        let sample = r#".
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err(), "{:?}", res.unwrap());
        }
    }


    #[test]
    fn test_nan() {
        let sample = r#"NaN
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_float_leading_decimal_point() {
        let sample = r#"-.5
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_float_leading_zero() {
        let sample = r#"-0.5
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_float_trailing_decimal_point() {
        let sample = r#"-5.
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_float() {
        let sample = r#"-1.2
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_hexadecimal() {
        let sample = r#"-0xC8
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_infinity() {
        let sample = r#"-Infinity
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_integer() {
        let sample = r#"-15
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_noctal() {
        let sample = r#"-098
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_negative_octal() {
        let sample = r#"-0123
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_negative_zero_float_leading_decimal_point() {
        let sample = r#"-.0
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_zero_float_trailing_decimal_point() {
        let sample = r#"-0.
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_zero_float() {
        let sample = r#"-0.0
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_zero_hexadecimal() {
        let sample = r#"-0x0
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_zero_integer() {
        let sample = r#"-0
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_zero_octal() {
        let sample = r#"-00
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_noctal_with_leading_octal_digit() {
        let sample = r#"0780
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_noctal() {
        let sample = r#"080
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_octal() {
        let sample = r#"010
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_positive_float_leading_decimal_point() {
        let sample = r#"+.5
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_float_leading_zero() {
        let sample = r#"+0.5
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_float_trailing_decimal_point() {
        let sample = r#"+5.
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_float() {
        let sample = r#"+1.2
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_hexadecimal() {
        let sample = r#"+0xC8
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_infinity() {
        let sample = r#"+Infinity
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_integer() {
        let sample = r#"+15
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_noctal() {
        let sample = r#"+098
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_positive_octal() {
        let sample = r#"+0123
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_positive_zero_float_leading_decimal_point() {
        let sample = r#"+.0
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_zero_float_trailing_decimal_point() {
        let sample = r#"+0.
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_zero_float() {
        let sample = r#"+0.0
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_zero_hexadecimal() {
        let sample = r#"+0x0
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_zero_integer() {
        let sample = r#"+0
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_zero_octal() {
        let sample = r#"+00
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_zero_float_leading_decimal_point() {
        let sample = r#".0
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_zero_float_trailing_decimal_point() {
        let sample = r#"0.
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_zero_float() {
        let sample = r#"0.0
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_zero_hexadecimal() {
        let sample = r#"0x0
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_zero_integer_with_integer_exponent() {
        let sample = r#"0e23
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_zero_integer() {
        let sample = r#"0
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_zero_octal() {
        let sample = r#"00
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_duplicate_keys() {
        let sample = r#"{
    "a": true,
    "a": false
}
"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_empty_object() {
        let sample = r#"{}"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_illegal_unquoted_key_number() {
        let sample = r#"{
    10twenty: "ten twenty"
}"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_illegal_unquoted_key_symbol() {
        let sample = r#"{
    multi-word: "multi-word"
}"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_leading_comma_object() {
        let sample = r#"{
    ,"foo": "bar"
}"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_lone_trailing_comma_object() {
        let sample = r#"{
    ,
}"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_no_comma_object() {
        let sample = r#"{
    "foo": "bar"
    "hello": "world"
}"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }


    #[test]
    fn test_reserved_unquoted_key() {
        let sample = r#"{
    while: true
}"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_single_quoted_key() {
        let sample = r#"{
    'hello': "world"
}"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_trailing_comma_object() {
        let sample = r#"{
    "foo": "bar",
}"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_unquoted_keys() {
        let sample = r#"{
    hello: "world",
    _: "underscore",
    $: "dollar sign",
    one1: "numerals",
    _$_: "multiple symbols",
    $_$hello123world_$_: "mixed"
}"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_escaped_single_quoted_string() {
        let sample = r#"'I can\'t wait'"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_multi_line_string() {
        let sample = r#"'hello\
 world'"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_single_quoted_string() {
        let sample = r#"'hello world'"#;
        let res = from_str(sample).unwrap();
    }



    #[test]
    fn test_unescaped_multi_line_string() {
        let sample = r#""foo
bar"
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            return
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            assert!(res.is_err());
        }
    }
    // Start error tests



    #[test]
    fn test_error_no_comma_array_lineno() {
        let sample = r#"[
    true
    false
]"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.lineno, 3_usize, "{:?}", err);
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.lineno, 3_usize, "{:?}", err);
        }
    }


    #[test]
    fn test_error_no_comma_array_index() {
        let sample = r#"[
    true
    false
]"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.char_index, 15_usize, "{:?}", err)
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.char_index, 15_usize, "{:?}", err);
        }
    }

    #[test]
    fn test_error_no_comma_array_colno() {
        let sample = r#"[
    true
    false
]"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.colno, 5_usize, "{:?}", err);
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.colno, 5_usize, "{:?}", err);
        }
    }


    #[test]
    fn test_error_top_level_block_comment_lineno() {
        let sample = r#"/*
    This should fail;
    comments cannot be the only top-level value.
*/"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.lineno, 4_usize, "{:?}", err);
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.lineno, 4_usize, "{:?}", err);
        }
    }


    #[test]
    #[ignore]
    fn test_error_top_level_block_comment_index() {
        let sample = r#"/*
    This should fail;
    comments cannot be the only top-level value.
*/"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.char_index, 76_usize, "{:?}", err)
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.char_index, 76_usize, "{:?}", err);
        }
    }

    #[test]
    #[ignore]
    fn test_error_top_level_block_comment_colno() {
        let sample = r#"/*
    This should fail;
    comments cannot be the only top-level value.
*/"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.colno, 3_usize, "{:?}", err);
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.colno, 3_usize, "{:?}", err);
        }
    }



    #[test]
    fn test_error_top_level_inline_comment_lineno() {
        let sample = r#"// This should fail; comments cannot be the only top-level value."#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.lineno, 1_usize, "{:?}", err);
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.lineno, 1_usize, "{:?}", err);
        }
    }


    #[test]
    #[ignore]
    fn test_error_top_level_inline_comment_index() {
        let sample = r#"// This should fail; comments cannot be the only top-level value."#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.char_index, 65_usize, "{:?}", err)
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.char_index, 65_usize, "{:?}", err);
        }
    }

    #[test]
    #[ignore]
    fn test_error_top_level_inline_comment_colno() {
        let sample = r#"// This should fail; comments cannot be the only top-level value."#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.colno, 66_usize, "{:?}", err);
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.colno, 66_usize, "{:?}", err);
        }
    }

    #[test]
    fn test_error_illegal_unquoted_key_number_lineno() {
        let sample = r#"{
    10twenty: "ten twenty"
}"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.lineno, 2_usize, "{:?}", err);
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.lineno, 2_usize, "{:?}", err);
        }
    }


    #[test]
    #[ignore]
    fn test_error_illegal_unquoted_key_number_index() {
        let sample = r#"{
    10twenty: "ten twenty"
}"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.char_index, 6_usize, "{:?}", err)
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.char_index, 6_usize, "{:?}", err);
        }
    }

    #[test]
    #[ignore]
    fn test_error_illegal_unquoted_key_number_colno() {
        let sample = r#"{
    10twenty: "ten twenty"
}"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.colno, 5_usize, "{:?}", err);
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.colno, 5_usize, "{:?}", err);
        }
    }



    #[test]
    fn test_error_illegal_unquoted_key_symbol_lineno() {
        let sample = r#"{
    multi-word: "multi-word"
}"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.lineno, 2_usize, "{:?}", err);
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.lineno, 2_usize, "{:?}", err);
        }
    }


    #[test]
    fn test_error_illegal_unquoted_key_symbol_index() {
        let sample = r#"{
    multi-word: "multi-word"
}"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.char_index, 11_usize, "{:?}", err)
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.char_index, 11_usize, "{:?}", err);
        }
    }

    #[test]
    fn test_error_illegal_unquoted_key_symbol_colno() {
        let sample = r#"{
    multi-word: "multi-word"
}"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.colno, 10_usize, "{:?}", err);
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.colno, 10_usize, "{:?}", err);
        }
    }



    #[test]
    fn test_error_leading_comma_object_lineno() {
        let sample = r#"{
    ,"foo": "bar"
}"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.lineno, 2_usize, "{:?}", err);
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.lineno, 2_usize, "{:?}", err);
        }
    }


    #[test]
    fn test_error_leading_comma_object_index() {
        let sample = r#"{
    ,"foo": "bar"
}"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.char_index, 6_usize, "{:?}", err)
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.char_index, 6_usize, "{:?}", err);
        }
    }

    #[test]
    fn test_error_leading_comma_object_colno() {
        let sample = r#"{
    ,"foo": "bar"
}"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.colno, 5_usize, "{:?}", err);
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.colno, 5_usize, "{:?}", err);
        }
    }

    #[test]
    fn test_error_unescaped_multi_line_string_lineno() {
        let sample = r#""foo
bar"
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.lineno, 1_usize, "{:?}", err);
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.lineno, 1_usize, "{:?}", err);
        }
    }


    #[test]
    fn test_error_unescaped_multi_line_string_index() {
        let sample = r#""foo
bar"
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.char_index, 4_usize, "{:?}", err)
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.char_index, 4_usize, "{:?}", err);
        }
    }

    #[test]
    fn test_error_unescaped_multi_line_string_colno() {
        let sample = r#""foo
bar"
"#;
        let maybe_tokens = Tokenizer::new(sample).tokenize();
        if maybe_tokens.is_err() {
            let err = maybe_tokens.unwrap_err();
            assert_eq!(err.colno, 5_usize, "{:?}", err);
        } else {
            let toks = maybe_tokens.unwrap();
            let res = from_tokens(&toks);
            let err = res.unwrap_err();
            assert_eq!(err.colno, 5_usize, "{:?}", err);
        }
    }


}
