use criterion::{black_box, criterion_group, criterion_main, Criterion};
use pascal::ast::{Block, Program, Stmt};
use pascal::interpreter::Interpreter;

fn bench_interpreter_empty(c: &mut Criterion) {
    let program = Program {
        name: "Bench".to_string(),
        uses: vec![],
        block: Block::with_statements(vec![Stmt::Empty]),
    };
    c.bench_function("interpreter_empty_program", |b| {
        b.iter(|| {
            let mut interp = Interpreter::new(false);
            interp.run_program(&program).unwrap();
            black_box(());
        });
    });
}

criterion_group!(benches, bench_interpreter_empty);
criterion_main!(benches);
