use std::process::Command;

use criterion::{criterion_group, criterion_main, Criterion};

fn literal_string_search() {
    Command::new("./target/release/fst")
        .arg("Son of God")
        .arg("benches/bible.txt")
        .output()
        .expect("Failed to execute fst release build");
}

fn regex_with_literal_string_tail() {
    Command::new("./target/release/fst")
        .arg("[1-9]+:[1-9]+ And")
        .arg("benches/bible.txt")
        .output()
        .expect("Failed to execute fst release build");
}

fn regex_with_literal_string_head() {
    Command::new("./target/release/fst")
        .arg("Abraham.+(Sarai|Lazarus|Isaac)")
        .arg("benches/bible.txt")
        .output()
        .expect("Failed to execute fst release build");
}

fn regex_search_no_literal() {
    Command::new("./target/release/fst")
        .arg("[1-9]+:[1-9]+.*Jesus.*(God|Christ)+")
        .arg("benches/bible.txt")
        .output()
        .expect("Failed to execute fst release build");
}

pub fn cli_benchmark(c: &mut Criterion) {
    c.bench_function("literal string search", |b| {
        b.iter(|| literal_string_search())
    });
    c.bench_function("search with literal string tail", |b| {
        b.iter(|| regex_with_literal_string_tail())
    });
    c.bench_function("search with literal string head", |b| {
        b.iter(|| regex_with_literal_string_head())
    });
    c.bench_function("pure regex search", |b| {
        b.iter(|| regex_search_no_literal())
    });
}

criterion_group!(benches, cli_benchmark);
criterion_main!(benches);
