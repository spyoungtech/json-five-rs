use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::slice::Iter;
use crate::tokenize::{TokenSpan, TokType};
use crate::tokenize::Tokens;
use crate::tokenize::TokType::{Colon, Comma, RightBrace};
use crate::utils::get_line_col_char;


#[derive(PartialEq, Debug)]
pub struct JSONKeyValuePair<'input> {
    pub(crate) key: JSONValue<'input>,
    pub(crate) value: JSONValue<'input>,
}


#[derive(PartialEq, Debug)]
pub enum UnaryOperator {
    Plus,
    Minus,
}


#[derive(PartialEq, Debug)]
pub enum JSONValue<'input> {
    JSONObject { key_value_pairs: Vec<JSONKeyValuePair<'input>> },
    JSONArray { values: Vec<JSONValue<'input>> },
    Integer(&'input str),
    Float(&'input str),
    Exponent(&'input str),
    Null,
    Infinity,
    NaN,
    Hexadecimal(&'input str),
    Bool(bool),
    DoubleQuotedString(&'input str),
    SingleQuotedString(&'input str),
    Unary { operator: UnaryOperator, value: Box<JSONValue<'input>> },
    Identifier(&'input str), // XXX: for keys only!
}

#[derive(PartialEq, Debug)]
pub struct JSONText<'input> {
    pub(crate) value: JSONValue<'input>,
}
#[allow(dead_code)]
pub enum TrailingComma {
    ALL,
    OBJECTS,
    ARRAYS,
    NONE
}

pub struct StyleConfiguration {
    pub(crate) indent: Option<usize>,
    pub(crate) item_separator: String,
    pub(crate) key_separator: String,
    pub(crate) current_indent: usize,
    pub(crate) trailing_comma: TrailingComma
}

#[allow(dead_code)]
impl StyleConfiguration {
    pub fn new(indent: Option<usize>, item_separator: &str, key_separator: &str, trailing_comma: TrailingComma) -> Self {
        StyleConfiguration{indent: indent, item_separator: item_separator.to_string(), key_separator: key_separator.to_string(), current_indent: 0, trailing_comma: trailing_comma}
    }

    pub fn with_indent(indent: usize, trailing_comma: TrailingComma) -> Self {
        StyleConfiguration{indent: Some(indent), item_separator: ",".to_string(), key_separator: ": ".to_string(), trailing_comma, current_indent: 0}
    }

    pub fn with_separators(item_separator: &str, key_separator: &str, trailing_comma: TrailingComma) -> Self {
        StyleConfiguration{indent: Some(0), key_separator: key_separator.to_string(), trailing_comma, item_separator: item_separator.to_string(), current_indent: 0}
    }

    pub fn default() -> Self {
        StyleConfiguration{indent: None, item_separator: ", ".to_string(), key_separator: ": ".to_string(), current_indent: 0, trailing_comma: TrailingComma::NONE}

    }
}

impl<'input> JSONKeyValuePair<'input> {
    fn to_string_styled(&self, style: &mut StyleConfiguration) -> String {
        format!("{}{}{}", self.key.to_string_styled(style), style.key_separator, self.value)
    }
}

impl<'input> JSONValue<'input> {
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
                            Some(_) => {
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
            JSONValue::JSONArray { values } => {
                let mut ret: String;

                match style.indent {
                    None => {
                        ret = String::from("[");
                    }
                    Some(ident) => {
                        style.current_indent += ident;
                        ret = format!("{{\n{}", style.current_indent);
                    }
                }
                for (idx, value) in values.iter().enumerate() {
                    ret.push_str(value.to_string_styled(style).as_str());
                    if idx < values.len() - 1 {
                        match style.indent {
                            None => {
                                ret.push_str(style.item_separator.as_str());
                            }
                            Some(_) => {
                                ret.push_str(format!(",\n{}", style.current_indent).as_str())
                            }
                        }
                    }
                }
                match style.trailing_comma {
                    TrailingComma::ALL | TrailingComma::ARRAYS => {
                        ret.push(',');
                    }
                    _ => {}
                }
                match style.indent {
                    None => {
                        ret.push_str("]");
                    }
                    Some(ident) => {
                        style.current_indent -= ident;
                        ret.push_str(format!("\n{}}}", style.current_indent).as_str());
                    }
                }
                ret
            }
        }
    }
}


impl<'input> Display for JSONValue<'input> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut style = StyleConfiguration::default();
        let res = self.to_string_styled(&mut style);
        write!(f, "{}", res)
    }
}


impl<'input> Display for JSONText<'input> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value.to_string_styled(&mut StyleConfiguration::default()))
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
    source: &'input str,
    source_tokens: Peekable<Iter<'toks, TokenSpan>>,
    lookahead: Option<&'toks TokenSpan>,
}


impl<'toks, 'input> JSON5Parser<'toks, 'input> {
    fn new(tokens: &'toks Tokens<'input>) -> Self {
        JSON5Parser { source_tokens: tokens.tok_spans.iter().peekable(), lookahead: None, source: tokens.source }
    }

    fn advance(&mut self) -> Option<&'toks TokenSpan> {
        match self.source_tokens.next() {
            None => {
                self.lookahead = None;
                None
            }
            Some(span) => {
                match span.1 {
                    TokType::BlockComment | TokType::LineComment | TokType::Whitespace => {
                        return self.advance()
                    }
                    _ => {

                        self.lookahead = Some(span);
                        self.lookahead
                    }
                }
            }
        }
    }

    #[inline]
    fn get_tok_source(&self, span: &'toks TokenSpan) -> &'input str {
        &self.source[span.0 .. span.2]
    }


    fn peek(&mut self) -> Option<&'toks TokenSpan> {
        match self.source_tokens.peek() {
            None => None,
            Some(span) => {
                match span.1 {
                    TokType::BlockComment | TokType::LineComment | TokType::Whitespace => {
                        self.source_tokens.next();
                        self.peek()
                    }
                    _ => {
                        Some(span)
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
                    Some(span) => {span.2}
                }
            }
            Some(span) => {
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

    fn check_and_consume(&mut self, types: Vec<TokType>) -> Option<&'toks TokenSpan> {
        let next_tok = self.peek()?;
        for toktype in types {
            if next_tok.1 == toktype {
                return self.advance();
            }
        }
        None
    }

    #[inline]
    fn check_and_consume_with_source(&mut self, types: Vec<TokType>) -> Option<(&'toks TokenSpan, &'input str)> {
        let tok = self.check_and_consume(types)?;
        let source = self.get_tok_source(tok);
        Some((tok, source))
    }

    fn parse_key(&mut self) -> Result<JSONValue<'input>, ParsingError>{
        // This is a terminal point
        // We either get a valid key or we bail.
        match self.check_and_consume_with_source(vec![TokType::Name, TokType::DoubleQuotedString, TokType::SingleQuotedString]) {
            None => {
                match self.peek() {
                    None => {
                        let idx = self.position();
                        Err(self.make_error("Unexpected EOF. Was expecting MemberName at".to_string(), idx))
                    }
                    Some(span) => {
                        let src = self.get_tok_source(span);
                        Err(self.make_error(format!("Invalid token for unquoted key ({}, {:?}) at", span.2, src), span.0))
                    }
                }
            },
            Some((span, lexeme)) => {
                match span.1 {
                    TokType::DoubleQuotedString => {
                        Ok(JSONValue::DoubleQuotedString(&lexeme[1..lexeme.len() - 1]))
                    },
                    TokType:: SingleQuotedString => {
                        Ok(JSONValue::SingleQuotedString(&lexeme[1..lexeme.len() - 1]))
                    }
                    TokType::Name => {
                        Ok(JSONValue::Identifier(lexeme))
                    }
                    _ => panic!("Programming error. Please report this as a bug")
                }
            }
        }
    }

    fn parse_object(&mut self) -> Result<JSONValue<'input>, ParsingError> {
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

    fn parse_array(&mut self) -> Result<JSONValue<'input>, ParsingError> {
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

    fn parse_primary(&mut self) -> Result<JSONValue<'input>, ParsingError> {
        let span = self.advance().unwrap();
        match &span.1 {
            TokType::Integer => {Ok(JSONValue::Integer(self.get_tok_source(span)))}
            TokType::Float => {Ok(JSONValue::Float(self.get_tok_source(span)))}
            TokType::Exponent => { Ok(JSONValue::Exponent(self.get_tok_source(span)))}
            TokType::SingleQuotedString => {
                let lexeme = self.get_tok_source(span);
                Ok(JSONValue::SingleQuotedString(&lexeme[1..lexeme.len() - 1]))
            },
            TokType::DoubleQuotedString => {
                let lexeme = self.get_tok_source(span);
                Ok(JSONValue::DoubleQuotedString(&lexeme[1..lexeme.len() - 1]))
            },
            TokType::True => Ok(JSONValue::Bool(true)),
            TokType::False => Ok(JSONValue::Bool(false)),
            TokType::Null => Ok(JSONValue::Null),
            TokType::Infinity => Ok(JSONValue::Infinity),
            TokType::Nan => Ok(JSONValue::NaN),
            TokType::Hexadecimal => Ok(JSONValue::Hexadecimal(self.get_tok_source(span))),
            TokType::EOF => {
                match self.position() {
                    0 => Err(self.make_error("Unexpected EOF. Was expecting value.".to_string(), 0)),
                    pos => Err(self.make_error("Unexpected EOF".to_string(), pos-1))
                }
            },
            t => Err(self.make_error(format!("Unexpected token of type {:?}: {:?}", t, self.get_tok_source(span)), span.0))
        }
    }

    fn parse_unary(&mut self) -> Result<JSONValue<'input>, ParsingError> {
        match self.check_and_consume(vec![TokType::Plus, TokType::Minus]) {
            None => self.parse_primary(),
            Some(span) => {
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

    fn parse_obj_or_array(&mut self) -> Result<JSONValue<'input>, ParsingError> {
        match self.check_and_consume(vec![TokType::LeftBracket, TokType::LeftBrace]) {
            None => self.parse_unary(),
            Some(span) => {
                match span.1 {
                    TokType::LeftBrace => self.parse_object(),
                    TokType::LeftBracket => self.parse_array(),
                    _ => panic!("no")
                }
            }
        }
    }


    fn parse_value(&mut self) -> Result<JSONValue<'input>, ParsingError> {
        self.parse_obj_or_array()
    }

    fn parse_text(&mut self) -> Result<JSONText<'input>, ParsingError> {
        let value = self.parse_value()?;
        match self.advance() {
            None => {}
            Some(span) => {
                if span.1 != TokType::EOF {
                    return Err(self.make_error(format!("Unexpected {:?} token after value", span.1), span.0 - 1))
                }
            }
        }
        Ok(JSONText { value })
    }
}

pub fn from_tokens<'toks, 'input>(tokens: &'toks Tokens<'input>) -> Result<JSONText<'input>, ParsingError> {
    let mut parser = JSON5Parser::new(tokens);
    parser.parse_text()
}

pub fn from_str<'input>(source: &'input str) -> Result<JSONText<'input>, ParsingError> {
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
        let expected = JSONText{value: JSONValue::JSONObject {key_value_pairs: vec![JSONKeyValuePair{key: JSONValue::DoubleQuotedString("foo"), value: JSONValue::DoubleQuotedString("bar")}]}};
        assert_eq!(res, expected)
    }

    #[test]
    fn test_identifier(){
        let res = from_str("{foo: \"bar\"}").unwrap();
        let expected = JSONText{value: JSONValue::JSONObject {key_value_pairs: vec![JSONKeyValuePair{key: JSONValue::Identifier("foo"), value: JSONValue::DoubleQuotedString("bar")}]}};
        assert_eq!(res, expected)
    }

    #[test]
    fn test_array() {
        let res = from_str("[1,2,3]").unwrap();
        let expected = JSONText{value: JSONArray {values: vec![JSONValue::Integer("1"), JSONValue::Integer("2"), JSONValue::Integer("3")]}};
        assert_eq!(res, expected)
    }

    #[test]
    fn val_int() {
        let res = from_str("1").unwrap();
        let expected = JSONText{value: Integer("1")};
        assert_eq!(res, expected)
    }

    #[test]
    fn val_float() {
        let res = from_str("1.0").unwrap();
        let expected = JSONText{value: Float("1.0")};
        assert_eq!(res, expected)
    }

    #[test]
    fn val_string() {
        let res = from_str("'foo'").unwrap();
        let expected = JSONText{value: SingleQuotedString("foo")};
        assert_eq!(res, expected)
    }

    #[test]
    fn multiline_string() {
        let res = from_str("'foo\\\nbar'").unwrap();
        let expected = JSONText{value: SingleQuotedString("foo\\\nbar")};
        assert_eq!(res, expected)
    }

    #[test]
    fn test_empty_string() {
        let res = from_str("\"\"").unwrap();
        let expected = JSONText { value: DoubleQuotedString("") };
        assert_eq!(res, expected);
    }

    #[test]
    fn test_single_element_array() {
        let res = from_str("[42]").unwrap();
        let expected = JSONText { value: JSONArray { values: vec![Integer("42")] } };
        assert_eq!(res, expected);
    }

    #[test]
    fn test_single_key_object() {
        let res = from_str("{\"key\": \"value\"}").unwrap();
        let expected = JSONText {
            value: JSONObject {
                key_value_pairs: vec![
                    JSONKeyValuePair {
                        key: DoubleQuotedString("key"),
                        value: DoubleQuotedString("value"),
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
                values: vec![Integer("1"), Integer("2"), Integer("3")]
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
                        key: DoubleQuotedString("a"),
                        value: Integer("1"),
                    },
                    JSONKeyValuePair {
                        key: DoubleQuotedString("b"),
                        value: Integer("2"),
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
                        key: Identifier("key"),
                        value: DoubleQuotedString("value"),
                    }
                ]
            }
        };
        assert_eq!(res, expected);
    }

    #[test]
    fn test_multiline_string() {
        let res = from_str("\"multi\\\nline\"").unwrap();
        let expected = JSONText { value: DoubleQuotedString("multi\\\nline") };
        assert_eq!(res, expected);
    }

    #[test]
    fn test_unicode_characters() {
        let res = from_str("\"\\u2764\"").unwrap();
        let expected = JSONText { value: DoubleQuotedString("\\u2764") };
        assert_eq!(res, expected);
    }
    #[test]
    fn test_trailing_comma_in_nested_array() {
        let res = from_str("[[1, 2,],]").unwrap();
        let expected = JSONText {
            value: JSONArray {
                values: vec![
                    JSONArray { values: vec![Integer("1"), Integer("2")] }
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
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_trailing_comma_array() {
        let sample = r#"[
    null,
]"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_block_comment_following_array_element() {
        let sample = r#"[
    false
    /*
        true
    */
]"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_block_comment_following_top_level_value() {
        let sample = r#"null
/*
    Some non-comment top-level value is needed;
    we use null above.
*/"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_block_comment_in_string() {
        let sample = r#""This /* block comment */ isn't really a block comment.""#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_block_comment_preceding_top_level_value() {
        let sample = r#"/*
    Some non-comment top-level value is needed;
    we use null below.
*/
null"#;
        let _res = from_str(sample).unwrap();
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
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_inline_comment_following_array_element() {
        let sample = r#"[
    false   // true
]"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_inline_comment_following_top_level_value() {
        let sample = r#"null // Some non-comment top-level value is needed; we use null here."#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_inline_comment_in_string() {
        let sample = r#""This inline comment // isn't really an inline comment.""#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_inline_comment_preceding_top_level_value() {
        let sample = r#"// Some non-comment top-level value is needed; we use null below.
null"#;
        let _res = from_str(sample).unwrap();
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
        let _res = from_str(sample).unwrap();
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
        let _res = from_str(sample).unwrap();
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
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_valid_whitespace() {
        let sample = r#"{
    // An invalid form feed character (\x0c) has been entered before this comment.
    // Be careful not to delete it.
  "a": true
}
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_comment_cr() {
        let sample = r#"{
    // This comment is terminated with `\r`.
}
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_comment_crlf() {
        let sample = r#"{
    // This comment is terminated with `\r\n`.
}
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_comment_lf() {
        let sample = r#"{
    // This comment is terminated with `\n`.
}
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_escaped_cr() {
        let sample = r#"{
    // the following string contains an escaped `\r`
    a: 'line 1 \
line 2'
}
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_escaped_crlf() {
        let sample = r#"{
    // the following string contains an escaped `\r\n`
    a: 'line 1 \
line 2'
}
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_escaped_lf() {
        let sample = r#"{
    // the following string contains an escaped `\n`
    a: 'line 1 \
line 2'
}
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_float_leading_decimal_point() {
        let sample = r#".5
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_float_leading_zero() {
        let sample = r#"0.5
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_float_trailing_decimal_point_with_integer_exponent() {
        let sample = r#"5.e4
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_float_trailing_decimal_point() {
        let sample = r#"5.
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_float_with_integer_exponent() {
        let sample = r#"1.2e3
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_float() {
        let sample = r#"1.2
"#;
        let _res = from_str(sample).unwrap();
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
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_hexadecimal_uppercase_x() {
        let sample = r#"0XC8
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_hexadecimal_with_integer_exponent() {
        let sample = r#"0xc8e4
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_hexadecimal() {
        let sample = r#"0xC8
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_infinity() {
        let sample = r#"Infinity
"#;
        let _res = from_str(sample).unwrap();
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
        let _res = from_str(sample).unwrap();
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
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_integer_with_negative_zero_integer_exponent() {
        let sample = r#"5e-0
"#;
        let _res = from_str(sample).unwrap();
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
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_integer_with_positive_zero_integer_exponent() {
        let sample = r#"5e+0
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_integer_with_zero_integer_exponent() {
        let sample = r#"5e0
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_integer() {
        let sample = r#"15
"#;
        let _res = from_str(sample).unwrap();
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
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_float_leading_decimal_point() {
        let sample = r#"-.5
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_float_leading_zero() {
        let sample = r#"-0.5
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_float_trailing_decimal_point() {
        let sample = r#"-5.
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_float() {
        let sample = r#"-1.2
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_hexadecimal() {
        let sample = r#"-0xC8
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_infinity() {
        let sample = r#"-Infinity
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_integer() {
        let sample = r#"-15
"#;
        let _res = from_str(sample).unwrap();
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
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_zero_float_trailing_decimal_point() {
        let sample = r#"-0.
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_zero_float() {
        let sample = r#"-0.0
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_zero_hexadecimal() {
        let sample = r#"-0x0
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_negative_zero_integer() {
        let sample = r#"-0
"#;
        let _res = from_str(sample).unwrap();
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
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_float_leading_zero() {
        let sample = r#"+0.5
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_float_trailing_decimal_point() {
        let sample = r#"+5.
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_float() {
        let sample = r#"+1.2
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_hexadecimal() {
        let sample = r#"+0xC8
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_infinity() {
        let sample = r#"+Infinity
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_integer() {
        let sample = r#"+15
"#;
        let _res = from_str(sample).unwrap();
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
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_zero_float_trailing_decimal_point() {
        let sample = r#"+0.
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_zero_float() {
        let sample = r#"+0.0
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_zero_hexadecimal() {
        let sample = r#"+0x0
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_positive_zero_integer() {
        let sample = r#"+0
"#;
        let _res = from_str(sample).unwrap();
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
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_zero_float_trailing_decimal_point() {
        let sample = r#"0.
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_zero_float() {
        let sample = r#"0.0
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_zero_hexadecimal() {
        let sample = r#"0x0
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_zero_integer_with_integer_exponent() {
        let sample = r#"0e23
"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_zero_integer() {
        let sample = r#"0
"#;
        let _res = from_str(sample).unwrap();
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
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_empty_object() {
        let sample = r#"{}"#;
        let _res = from_str(sample).unwrap();
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
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_single_quoted_key() {
        let sample = r#"{
    'hello': "world"
}"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_trailing_comma_object() {
        let sample = r#"{
    "foo": "bar",
}"#;
        let _res = from_str(sample).unwrap();
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
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_escaped_single_quoted_string() {
        let sample = r#"'I can\'t wait'"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_multi_line_string() {
        let sample = r#"'hello\
 world'"#;
        let _res = from_str(sample).unwrap();
    }



    #[test]
    fn test_single_quoted_string() {
        let sample = r#"'hello world'"#;
        let _res = from_str(sample).unwrap();
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
