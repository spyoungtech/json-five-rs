use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::slice::Iter;
use std::vec::IntoIter;
use crate::utils::get_line_col_char;
use crate::rt::tokenize::{Token};
use crate::tokenize::{TokType, TokenSpan, Tokens};
#[derive(PartialEq, Debug, Clone)]
pub enum UnaryOperator {
    Plus,
    Minus,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Plus => {write!(f, "+")}
            UnaryOperator::Minus => {write!(f, "-")}
        }
    }
}

type Wsc = String; // Whitespace and comment tokens

// {wsc.0} value {wsc.1}
#[derive(PartialEq, Debug, Clone)]
struct JSONTextContext {
    wsc: (Wsc, Wsc)
}

// LBRACE {wsc} [ key_value_pairs ] RBRACE
//                                 ^ any whitespace that would go here would be part of the KVP
#[derive(PartialEq, Debug, Clone)]
pub struct JSONObjectContext {
    wsc: (Wsc,)
}


// key {wsc.0} COLON {wsc.1} value {wsc.2} [ COMMA {wsc.3} ] [ next_kvp ]
#[derive(PartialEq, Debug, Clone)]
pub struct KeyValuePairContext {
    pub wsc: (Wsc, Wsc, Wsc, Option<Wsc>),
    //                       ^ Some() here represents the presence of a comma and its subsequent wsc
}

// LBRACKET {wsc.0} [ array_values ] RBRACKET
#[derive(PartialEq, Debug, Clone)]
pub struct JSONArrayContext {
    pub wsc: (Wsc,)
}

// value {wsc.0} [ COMMA {wsc.1} ] [ next_value ]
#[derive(PartialEq, Debug, Clone)]
pub struct ArrayValueContext {
    pub wsc: (Wsc, Option<Wsc>),
    //             ^ Some() here represents the presence of a comma and its subsequent wsc
}

#[derive(PartialEq, Debug, Clone)]
pub struct JSONKeyValuePair {
    pub key: JSONValue,
    pub value: JSONValue,
    pub context: Option<KeyValuePairContext>
}

#[derive(PartialEq, Debug, Clone)]
pub struct ArrayValue {
    pub value: JSONValue,
    pub context: Option<ArrayValueContext>
}


#[derive(PartialEq, Debug, Clone)]
pub enum JSONValue {
    JSONObject { key_value_pairs: Vec<JSONKeyValuePair>, context: Option<JSONObjectContext> },
    JSONArray { values: Vec<ArrayValue>, context: Option<JSONArrayContext> },
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
    pub value: JSONValue,
    pub(crate) context: Option<JSONTextContext>
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

impl JSONKeyValuePair {
    // key {wsc.0} COLON {wsc.1} value {wsc.2} [ COMMA {wsc.3} ] [ next_kvp ]
    fn to_string(&self) -> String {
        match &self.context {
            None => {
                format!("{}:{}", self.key.to_string(), self.value.to_string())
            }
            Some(ctx) => {
                match &ctx.wsc.3 {
                    None => {
                        format!("{}{}:{}{}{}", self.key.to_string(), ctx.wsc.0, ctx.wsc.1, self.value.to_string(), ctx.wsc.2)
                    }
                    Some(trailing_wsc) => {
                        format!("{}{}:{}{}{},{}", self.key.to_string(), ctx.wsc.0, ctx.wsc.1, self.value.to_string(), ctx.wsc.2, trailing_wsc)
                    }
                }
            }
        }
    }
}

struct JSONTextIterator {
    model: JSONText
}



impl JSONText {
    fn to_string(&self) -> String {
        match &self.context {
            None => {
                self.value.to_string()
            },
            Some(ctx) => {
                format!("{}{}{}", ctx.wsc.0, self.value.to_string(), ctx.wsc.1)
            }
        }
    }
}

// value {wsc.0} [ COMMA {wsc.1} ] [ next_value ]
impl ArrayValue {
    fn to_string(&self) -> String {
        match &self.context {
            None => {
                self.value.to_string()
            }
            Some(ctx) => {
                match &ctx.wsc.1 {
                    None => {
                        format!("{}{}", self.value.to_string(), ctx.wsc.0)
                    }
                    Some(trailing_whitespace) => {
                        format!("{}{},{}", self.value.to_string(), ctx.wsc.0, trailing_whitespace)
                    }
                }
            }
        }
    }
}

impl JSONValue {
    fn to_string(&self) -> String {
        match self {
            JSONValue::JSONObject { key_value_pairs, context } => {
                match context {
                    None => {
                        let mut s = String::from("{");
                        for kvp in key_value_pairs {
                            s.push_str(kvp.to_string().as_str())
                        }
                        s.push('}');
                        s
                    }
                    Some(ctx) => {
                        let mut s = format!("{{{}", ctx.wsc.0);
                        for kvp in key_value_pairs {
                            s.push_str(kvp.to_string().as_str());
                        }
                        s.push('}');
                        s
                    }
                }
            }
            JSONValue::JSONArray { values, context } => {
                match context {
                    None => {
                        let mut s = String::from('[');
                        for array_value in values {
                            s.push_str(array_value.to_string().as_str());
                        }
                        s.push(']');
                        s
                    }
                    Some(ctx) => {
                        let mut s = format!("[{}", ctx.wsc.0);
                        for array_value in values {
                            s.push_str(array_value.to_string().as_str());
                        }
                        s.push(']');
                        s
                    }
                }
            }
            JSONValue::Integer(s) => {s.clone()}
            JSONValue::Float(s) => {s.clone()}
            JSONValue::Exponent(s) => {s.clone()}
            JSONValue::Null => {String::from("null")}
            JSONValue::Infinity => {String::from("Infinity")}
            JSONValue::NaN => {String::from("Nan")}
            JSONValue::Hexadecimal(s) => {s.clone()}
            JSONValue::Bool(b) => b.to_string(),
            JSONValue::DoubleQuotedString(s) => {s.clone()}
            JSONValue::SingleQuotedString(s) => {s.clone()}
            JSONValue::Unary { operator, value} => {
                format!("{}{}", operator, value)
            }
            JSONValue::Identifier(s) => {s.clone()}
        }
    }
}


impl Display for JSONValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let res = self.to_string();
        write!(f, "{}", res)
    }
}


impl Display for JSONText {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
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
                self.lookahead = Some(span);
                self.lookahead
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
                Some(span)
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

    fn parse_key(&mut self) -> Result<JSONValue, ParsingError>{
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
                        Ok(JSONValue::DoubleQuotedString(lexeme[1..lexeme.len() - 1].to_string()))
                    },
                    TokType:: SingleQuotedString => {
                        Ok(JSONValue::SingleQuotedString(lexeme[1..lexeme.len() - 1].to_string()))
                    }
                    TokType::Name => {
                        Ok(JSONValue::Identifier(lexeme.to_string()))
                    }
                    _ => panic!("Programming error. Please report this as a bug")
                }
            }
        }
    }

    fn parse_object(&mut self) -> Result<JSONValue, ParsingError> {
        use crate::tokenize::TokType::*;
        let mut kvps: Vec<JSONKeyValuePair> = Vec::new();
        let leading_wsc = &self.consume_whitespace_and_comments();
        loop {
            match self.check_and_consume(vec![RightBrace]) {
                None => {
                    let key = self.parse_key()?;
                    let wsc_0 = self.consume_whitespace_and_comments();

                    match self.check_and_consume(vec![Colon]) {
                        None => {
                            let idx = self.position();
                            return Err(self.make_error("Expecting ':' delimiter".to_string(), idx))
                        }
                        Some(_) => {
                            let wsc_1 = self.consume_whitespace_and_comments();
                            let val = self.parse_value()?;
                            let wsc_2 = self.consume_whitespace_and_comments();
                            match self.check_and_consume(vec![Comma]) {
                                None => {
                                    let context = KeyValuePairContext{wsc: (
                                        self.collect_wsc_vec_to_string(&wsc_0),
                                        self.collect_wsc_vec_to_string(&wsc_1),
                                        self.collect_wsc_vec_to_string(&wsc_2),
                                        None
                                        )};
                                    let kvp = JSONKeyValuePair{key: key, value: val, context: Some(context)};
                                    kvps.push(kvp);
                                    match self.check_and_consume(vec![RightBrace]) {
                                        None => {
                                            let idx = self.position();
                                            return Err(self.make_error("Expecting '}' at end of object".to_string(), idx))
                                        },
                                        Some(_) => {
                                            break Ok(JSONValue::JSONObject {key_value_pairs: kvps, context: Some(JSONObjectContext{wsc: (self.collect_wsc_vec_to_string(&leading_wsc), )})})
                                        }
                                    }
                                }
                                Some(_) => {
                                    let wsc_3 = self.consume_whitespace_and_comments();
                                    let context = KeyValuePairContext{wsc: (
                                        self.collect_wsc_vec_to_string(&wsc_0),
                                        self.collect_wsc_vec_to_string(&wsc_1),
                                        self.collect_wsc_vec_to_string(&wsc_2),
                                        Some(self.collect_wsc_vec_to_string(&wsc_3)),
                                    )};
                                    let kvp = JSONKeyValuePair{key: key, value: val, context: Some(context)};
                                    kvps.push(kvp);
                                    continue
                                }
                            }
                        }
                    }
                }
                Some(_) => {
                    break Ok(JSONValue::JSONObject {key_value_pairs: kvps, context: Some(JSONObjectContext{wsc: (self.collect_wsc_vec_to_string(&leading_wsc), )})})
                }
            }
        }
    }


    fn collect_wsc_vec_to_string(&self, wsc: &Vec<&'toks TokenSpan>) -> String {
        if wsc.len() == 0 {
            return String::with_capacity(0);
        }

        let first = wsc.first().unwrap();
        if wsc.len() == 1 {
            self.get_tok_source(first).to_string()
        } else {
            let last = wsc.last().unwrap();
            let mut buff = String::with_capacity(last.2 - first.0);
            for span in wsc {
                let src = self.get_tok_source(span);
                buff.push_str(src);
            }
            buff
        }
    }

    fn parse_array(&mut self) -> Result<JSONValue, ParsingError> {
        use crate::tokenize::TokType::*;
        let mut values:Vec<ArrayValue> = Vec::new();
        let leading_wsc = self.consume_whitespace_and_comments();
        loop {
            match self.check_and_consume(vec![TokType::RightBracket]) {
                None => {
                    let val = self.parse_value()?;
                    let wsc_0 = self.consume_whitespace_and_comments();
                    match self.check_and_consume(vec![Comma]) {
                        None => {
                            let array_val_context = ArrayValueContext{wsc: (self.collect_wsc_vec_to_string(&wsc_0), None)};
                            let array_val = ArrayValue{value: val, context: Some(array_val_context)};
                            values.push(array_val);
                            match self.check_and_consume(vec![TokType::RightBracket]) {
                                None => {
                                    let idx = self.position();
                                    return Err(self.make_error("Expecting ']' at end of array".to_string(), idx))
                                },
                                Some(_) => {
                                    break Ok(JSONValue::JSONArray {values: values, context: Some(JSONArrayContext{wsc: (self.collect_wsc_vec_to_string(&leading_wsc), )})})
                                }
                            }
                        }
                        Some(_) => {
                            let wsc_1 = self.consume_whitespace_and_comments();
                            let array_val_context = ArrayValueContext{wsc: (self.collect_wsc_vec_to_string(&wsc_0), Some(self.collect_wsc_vec_to_string(&wsc_1)))};
                            let array_val = ArrayValue{value: val, context: Some(array_val_context)};
                            values.push(array_val);
                            continue
                        }
                    }
                }
                Some(_) => {
                    break Ok(JSONValue::JSONArray {values: values, context: Some(JSONArrayContext{wsc: (self.collect_wsc_vec_to_string(&leading_wsc), )})})
                }
            }
        }
    }

    fn parse_primary(&mut self) -> Result<JSONValue, ParsingError> {
        let span = self.advance().unwrap();
        match &span.1 {
            TokType::Integer => {Ok(JSONValue::Integer(self.get_tok_source(span).to_string()))}
            TokType::Float => {Ok(JSONValue::Float(self.get_tok_source(span).to_string()))}
            TokType::Exponent => { Ok(JSONValue::Exponent(self.get_tok_source(span).to_string()))}
            TokType::SingleQuotedString => {
                let lexeme = self.get_tok_source(span);
                Ok(JSONValue::SingleQuotedString(lexeme[1..lexeme.len() - 1].to_string()))
            },
            TokType::DoubleQuotedString => {
                let lexeme = self.get_tok_source(span);
                Ok(JSONValue::DoubleQuotedString(lexeme[1..lexeme.len() - 1].to_string()))
            },
            TokType::True => Ok(JSONValue::Bool(true)),
            TokType::False => Ok(JSONValue::Bool(false)),
            TokType::Null => Ok(JSONValue::Null),
            TokType::Infinity => Ok(JSONValue::Infinity),
            TokType::Nan => Ok(JSONValue::NaN),
            TokType::Hexadecimal => Ok(JSONValue::Hexadecimal(self.get_tok_source(span).to_string())),
            TokType::EOF => {
                match self.position() {
                    0 => Err(self.make_error("Unexpected EOF. Was expecting value.".to_string(), 0)),
                    pos => Err(self.make_error("Unexpected EOF".to_string(), pos-1))
                }
            },
            t => Err(self.make_error(format!("Unexpected token of type {:?}: {:?}", t, self.get_tok_source(span)), span.0))
        }
    }

    fn parse_unary(&mut self) -> Result<JSONValue, ParsingError> {
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

    fn parse_obj_or_array(&mut self) -> Result<JSONValue, ParsingError> {
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


    fn parse_value(&mut self) -> Result<JSONValue, ParsingError> {
        self.parse_obj_or_array()
    }

    fn parse_text(&mut self) -> Result<JSONText, ParsingError> {
        let wsc_0 = self.consume_whitespace_and_comments();
        let value = self.parse_value()?;
        let wsc_1 = self.consume_whitespace_and_comments();
        match self.advance() {
            None => {}
            Some(span) => {
                if span.1 != TokType::EOF {
                    return Err(self.make_error(format!("Unexpected {:?} token after value", span.1), span.0 - 1))
                }
            }
        }
        let context = JSONTextContext{wsc: (self.collect_wsc_vec_to_string(&wsc_0), self.collect_wsc_vec_to_string(&wsc_1))};
        Ok(JSONText { value, context: Some(context) })
    }

    fn consume_whitespace_and_comments(&mut self) -> Vec<&'toks TokenSpan> {
        let mut ret: Vec<&TokenSpan> = Vec::new();
        loop {
            match self.peek() {
                None => {return ret}
                Some(span) => {
                    match span.1 {
                        TokType::BlockComment | TokType::LineComment | TokType::Whitespace => {
                            ret.push(span);
                            self.advance();
                        }
                        _ => {return ret}
                    }
                }
            }
        }
    }
}

pub fn from_tokens<'toks, 'input>(tokens: &'toks Tokens<'input>) -> Result<JSONText, ParsingError> {
    let mut parser = JSON5Parser::new(tokens);
    parser.parse_text()
}

pub fn from_str(source: &str) -> Result<JSONText, ParsingError> {
    use crate::tokenize::tokenize_rt_str;
    let maybe_toks = tokenize_rt_str(source);
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
    use crate::rt::parser::JSONValue::*;
    use super::*;
    #[test]
    fn test_foo() {
        let res = from_str("{}").unwrap();
        let expected = JSONText{context: Some(JSONTextContext{wsc: (String::new(), String::new())}), value: JSONValue::JSONObject {key_value_pairs: vec![], context: Some(JSONObjectContext{wsc: (String::new(),)})}};
        assert_eq!(res.value, expected.value)
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
