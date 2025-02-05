use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::fs;

use json_five::de::from_str as json_five_from_str;

use serde_json::Value as SerdeValue;

use json5;

fn bench_multiple_files(c: &mut Criterion) {
    let scenarios = vec![
        ("big", "data/big.json"),
        ("empty", "data/empty.json"),
        ("medium-ascii", "data/medium-ascii.json"),
        ("arrays", "data/arrays.json"),
        ("objects", "data/objects.json"),
        ("nested-array", "data/nested_array.json"),
        ("nested-objects", "data/nested_object.json"),
        ("string", "data/string.json"),
        ("number", "data/number.json"),
    ];

    for (label, path) in scenarios {
        // Read the file once outside the benchmark loop
        let file_content = fs::read_to_string(path).unwrap_or("".to_string());
        if file_content.is_empty() {
            eprintln!("Missing file {}. Did you setup test data?", path);
            continue
        }
        let mut group = c.benchmark_group(label);
        if label == "big" {
            group.sample_size(10);
        }
        group.bench_function(&format!("json_five"), |b| {
            b.iter(|| {
                let result: SerdeValue = json_five_from_str(black_box(&file_content))
                    .expect("json_five parse error");
                black_box(result);
            });
        });

        group.bench_function(&format!("serde_json"), |b| {
            b.iter(|| {
                let parsed: SerdeValue = serde_json::from_str(black_box(&file_content))
                    .expect("serde_json parse error");
                black_box(parsed);
            });
        });

        group.bench_function(&format!("json5"), |b| {
            b.iter(|| {
                let parsed: SerdeValue = json5::from_str(black_box(&file_content))
                    .expect("json5 parse error");
                black_box(parsed);
            });
        });
    }
}

criterion_group!(parser_benches, bench_multiple_files);
criterion_main!(parser_benches);