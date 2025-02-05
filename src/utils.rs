pub(crate) fn get_line_col_char(doc: &str, byte_offset: usize) -> (usize, usize, usize) {
    let mut lineno: usize = 1;
    let mut colno: usize = 0;
    if byte_offset == 0 {
        return (1, 1, 0)
    }

    assert!(byte_offset <= doc.len(), "requested byteoffset {} is not less than or equal to doc length ({})", byte_offset, doc.len());

    if byte_offset == doc.len() {
        let last_char_pos = doc.char_indices().last().unwrap(); // (byte_off, char)
        let (lineno, mut colno, mut codepoint_off) = get_line_col_char(doc, last_char_pos.0);
        colno += 1;
        codepoint_off += 1;
        return (lineno, colno, codepoint_off);
    }

    for (codepoint_off, (byte_off, char)) in doc.char_indices().enumerate() {
        colno += 1;
        if char == '\n' {
            if byte_off == byte_offset {
                return (lineno, colno, codepoint_off)
            }
            lineno += 1;
            colno = 0;
        }
        if byte_off < byte_offset {
            continue
        }
        if byte_off == byte_offset {
            return (lineno, colno, codepoint_off)
        }
        if byte_off > byte_offset {
            unreachable!("Byteoffset lands in the middle of a character")
        }
    }
    unreachable!("Reached end of document")
}


pub (crate) fn escape_double_quoted(input: &str) -> String {
    // In the worst case (every char requires a backslash), the output could
    // be roughly twice the length of `input`.
    let mut escaped = String::with_capacity(input.len() * 2);

    for c in input.chars() {
        match c {
            '"'  => { escaped.push('\\'); escaped.push('"');  }
            '\\' => { escaped.push('\\'); escaped.push('\\'); }
            '\n' => { escaped.push('\\'); escaped.push('n');  }
            '\r' => { escaped.push('\\'); escaped.push('r');  }
            '\t' => { escaped.push('\\'); escaped.push('t');  }
            '/'  => { escaped.push('\\'); escaped.push('/');  }
            '\u{0008}' => { escaped.push('\\'); escaped.push('b'); }
            '\u{000c}' => { escaped.push('\\'); escaped.push('f'); }
            _ => escaped.push(c),
        }
    }

    escaped
}

#[allow(dead_code)]
pub (crate) fn escape_single_quoted(input: &str) -> String {
    let mut escaped = String::with_capacity(input.len() * 2);
    for c in input.chars() {
        match c {
            '\''  => { escaped.push('\\'); escaped.push('"');  }
            '\\' => { escaped.push('\\'); escaped.push('\\'); }
            '\n' => { escaped.push('\\'); escaped.push('n');  }
            '\r' => { escaped.push('\\'); escaped.push('r');  }
            '\t' => { escaped.push('\\'); escaped.push('t');  }
            '/'  => { escaped.push('\\'); escaped.push('/');  }
            '\u{0008}' => { escaped.push('\\'); escaped.push('b'); }
            '\u{000c}' => { escaped.push('\\'); escaped.push('f'); }
            _ => escaped.push(c),
        }
    }
    escaped
}

/// Unescape a JSON5-style (ES5.1-like) string.
/// - `\uXXXX` => exactly 4 hex digits (no `\u{...}`).
/// - `\xNN`   => exactly 2 hex digits.
/// - Single-char escapes like `\n`, `\r`, `\t`, `\b`, `\f`, `\v`, `\0`, `\\`, `\"`, `\/`.
/// Returns an owned `String` with all escapes resolved.
pub fn unescape(input: &str) -> Result<String, String> {
    let mut output = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch != '\\' {
            // Normal character
            output.push(ch);
        } else {
            // We have a backslash; look at the next char
            let esc = chars.next().ok_or_else(|| err("Incomplete escape at end of string"))?;
            match esc {
                'a' => output.push('\x07'),
                'n' =>  output.push('\n'),
                'r' =>  output.push('\r'),
                't' =>  output.push('\t'),
                'b' =>  output.push('\x08'), // backspace
                'f' =>  output.push('\x0C'), // form-feed
                'v' =>  output.push('\x0B'),
                '0' =>  output.push('\0'),
                '\\' => output.push('\\'),
                '\'' => output.push('\''),
                '"'  => output.push('\"'),
                '/'  => output.push('/'), // optional in JSON5
                '\n' | '\r' | '\u{2028}' | '\u{2029}' => {
                    output.push(esc);
                }
                'x' => {
                    // \xNN => exactly 2 hex digits
                    let val = read_hex_digits(&mut chars, 2, "\\x")?;
                    output.push(char_from_u32(val)?);
                }
                'u' => {
                    // \uXXXX => exactly 4 hex digits (ES5.1)
                    let val = read_hex_digits(&mut chars, 4, "\\u")?;
                    output.push(char_from_u32(val)?);
                }
                _ => {
                    // Unknown escape
                    return Err(format!("Unknown escape character: {}", esc));
                }
            }
        }
    }
    Ok(output)
}

/// Read exactly `count` hex digits from `chars`, returning the combined u32.
/// The `context` helps produce a clearer error message (like `"\u"` or `"\x"`).
pub (crate) fn read_hex_digits<I: Iterator<Item = char>>(
    chars: &mut std::iter::Peekable<I>,
    count: usize,
    context: &str
) -> Result<u32, String> {
    let mut val = 0u32;
    for _ in 0..count {
        let c = chars.next().ok_or_else(|| err(format!("Incomplete {} escape", context)))?;
        let digit = c
            .to_digit(16)
            .ok_or_else(|| err(format!("Invalid hex digit '{}' in {} escape", c, context)))?;
        val = (val << 4) | digit;
    }
    Ok(val)
}

/// Convert a u32 to a `char`, checking for valid Unicode scalar value range.
fn char_from_u32(u: u32) -> Result<char, String> {
    // In ES5.1, `\uXXXX` covers 0..=0xFFFF. If you need to disallow >0xFFFF, insert a check:
    // if u > 0xFFFF { return Err(err(format!("Code point out of range: U+{:X}", u))); }
    std::char::from_u32(u).ok_or_else(|| err(format!("Invalid Unicode code point U+{:X}", u)))
}

fn err<S: Into<String>>(message: S) -> String {
    message.into()
}