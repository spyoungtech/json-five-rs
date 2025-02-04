use json_five::rt::parser::{from_str, ArrayValueContext, JSONValue, KeyValuePairContext};

fn format_value(val: &mut JSONValue) {
    match val {
        JSONValue::JSONObject { key_value_pairs, context } => {
            let length = key_value_pairs.len();
            for (idx, kvp) in key_value_pairs.iter_mut().enumerate() {
                match kvp.value {
                    JSONValue::JSONObject { .. } | JSONValue::JSONArray { .. } => {
                        format_value(&mut kvp.value);
                    }
                    _ => {}
                }
                if idx == length - 1 {
                    match &kvp.context {
                        None => {
                            kvp.context = Some(KeyValuePairContext{wsc: (String::new(), String::from(" "), String::new(), Some(String::new()))});
                        }
                        Some(ctx) => {
                            match ctx.wsc.3 {
                                None => {
                                    kvp.context = Some(KeyValuePairContext{wsc: (ctx.wsc.0.clone(), ctx.wsc.1.clone(), String::new(), Some(ctx.wsc.2.clone()))});
                                }
                                Some(_) => {}
                            }
                        }
                    }
                }
            }
        }
        JSONValue::JSONArray { values, context} => {
            let length = values.len();
            for (idx, array_value) in values.iter_mut().enumerate() {
                match array_value.value {
                    JSONValue::JSONObject { .. } | JSONValue::JSONArray { .. } => {
                        format_value(&mut array_value.value);
                    }
                    _ => {}
                }
                if idx == length - 1 {
                    match &array_value.context {
                        None => {
                            array_value.context = Some(ArrayValueContext{wsc: (String::new(), Some(String::new()))});
                        }
                        Some(ctx) => {
                            match ctx.wsc.1 {
                                None => {
                                    array_value.context = Some(ArrayValueContext{wsc: (String::new(), Some(ctx.wsc.0.clone()))})
                                }
                                Some(_) => {}
                            }
                        }
                    }
                }
            }
        }
        _ => {}
    }
}

fn format_str(source: &str) -> String {
    let mut model = from_str(source).unwrap();
    format_value(&mut model.value);
    model.to_string()
}

fn main() {
    let doc = r#"// My Document
{
    breakfast: [
        'bacon',
        'eggs',
        'spam'  // <-- a trailing comma will be added here
    ],
    objekt: {
        nested: "value" // <-- and here
    } // <--- and here
}"#;
    println!("{}", format_str(doc));

}
