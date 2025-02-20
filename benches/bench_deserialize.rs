// Warning. You will need a lot of memory to run this bench
use criterion::{black_box, criterion_group, criterion_main, Criterion};

use json5;
use serde::{Deserialize, Serialize};
use json_five;

#[derive(Debug, Clone, Serialize, Deserialize)]
struct MyStruct {
    items: Vec<String>,
}
fn create_test_struct(num_items: u64) -> MyStruct {
    let mut items = Vec::new();

    for i in 0..num_items {
        items.push(format!("{:0100}", i))
    }

    MyStruct { items }
}

fn bench_struct_sizes(c: &mut Criterion) {
    let test_sizes = vec![10, 100, 1000, 10000, 100000, 1000000u64];


    for test_size in test_sizes.iter() {
        let test_struct = create_test_struct(*test_size);
        let serialized = json_five::to_string(&test_struct).unwrap();
        let mut deserialize_group = c.benchmark_group(format!("deserialize (size {test_size})"));
        if *test_size > 1000 {
            deserialize_group.sample_size(10);
        }
        deserialize_group.bench_function(&format!("json-five"), |b| {
            b.iter(|| {
                let json_deserialized: MyStruct = json_five::from_str(black_box(&serialized)).unwrap();
                black_box(json_deserialized)
            });
        });

        deserialize_group.bench_function(&format!("serde_json"), |b| {
            b.iter(|| {
                let parsed: MyStruct = serde_json::from_str(black_box(&serialized)).unwrap();
                black_box(parsed);
            });
        });

        deserialize_group.bench_function(&format!("json5"), |b| {
            b.iter(|| {
                let json5_deserialized: MyStruct = json5::from_str(black_box(&serialized)).unwrap();
                black_box(json5_deserialized);
            });
        });

    }

    for test_size in test_sizes {
        let test_struct = create_test_struct(test_size);
        let mut serialize_group = c.benchmark_group(format!("serialize (size {test_size})"));
        if test_size > 1000 {
            serialize_group.sample_size(10);
        }
        serialize_group.bench_function(&format!("json-five"), |b| {
            b.iter(|| {
                let result = json_five::to_string(black_box(&test_struct)).unwrap();
                black_box(result)
            });
        });

        serialize_group.bench_function(&format!("serde_json"), |b| {
            b.iter(|| {
                let result = serde_json::to_string(black_box(&test_struct)).unwrap();
                black_box(result);
            });
        });

        serialize_group.bench_function(&format!("json5"), |b| {
            b.iter(|| {
                let result = json5::to_string(black_box(&test_struct)).unwrap();
                black_box(result);
            });
        });
    }
}

criterion_group!(parser_benches, bench_struct_sizes);
criterion_main!(parser_benches);