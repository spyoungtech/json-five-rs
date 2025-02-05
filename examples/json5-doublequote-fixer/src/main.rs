use json_five::rt::tokenize::{Token, source_to_tokens, tokens_to_source};
use json_five::tokenize::TokType;
use regex::Regex;

fn escape_unescaped_double_quotes(s: &str) -> String {
    let escape_double_quotes = Regex::new(r#"([^\\]")"#).unwrap();
    let unescape_single_quote_escapes = Regex::new(r#"(?P<non_escape>[^\\])(\\')"#).unwrap();
    // technically, this may miss some cases, but for sake of example, this should be ok

    // first add escapes to any un-escaped double quotes
    let mut res = escape_double_quotes.replace_all(s, "\\\"").to_string();

    // then remove any unnecessary single quote escapes
    res = unescape_single_quote_escapes.replace_all(res.as_str(), "$non_escape'").to_string();

    res
}

fn replace_tokens(in_tokens: &Vec<Token>) -> Vec<Token> {
    let mut new_tokens = Vec::with_capacity(in_tokens.len());
    for tok in in_tokens {
        match tok.tok_type {
            TokType::SingleQuotedString => {
                let inner_lexeme = escape_unescaped_double_quotes(&tok.lexeme[1 .. tok.lexeme.len() - 1 ]);
                let lexeme = format!("\"{}\"", inner_lexeme);

                let new_token = Token{lexeme, tok_type: TokType::DoubleQuotedString, context: None};
                new_tokens.push(new_token);
            }
            TokType::Name => {
                // Identifier Name tokens are already valid directly in a string, so no conversion needed
                let new_token = Token{lexeme: format!("\"{}\"", tok.lexeme), tok_type: TokType::DoubleQuotedString, context: None};
                new_tokens.push(new_token);
            }
            _ => {
                // Append all other tokens as-is
                new_tokens.push(tok.clone());
            }
        }
    }
    new_tokens
}


fn format_str(s: &str) -> String {
    let tokens = source_to_tokens(s).unwrap();
    let new_tokens = replace_tokens(&tokens);
    tokens_to_source(&new_tokens)
}


fn main() {
    let doc = r#"// My Document
{
    breakfast: [ // <-- the `breakfast` key and all items will get double-quoted
        'bacon',
        'eggs',
        'spam'
    ],
    objekt: { // < -- this key, too
        nested: 'inner \"escaped double quotes\" will not be double-escaped',
        unescape: 'inner \'unnecessary escapes\' will be removed'
    }
}"#;
    println!("{}", format_str(doc));
}