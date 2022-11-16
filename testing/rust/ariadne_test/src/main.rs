fn main() {
    use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};

    let mut colors = ColorGenerator::new();

    // Generate & choose some colours for each of our elements
    let a = colors.next();
    let b = colors.next();
    let out = Color::Fixed(81);
    use core::ops::Range;

    let mut labels: Vec<Label<(&str, Range<usize>)>> = vec![];

    labels.push(
        Label::new(("sample.tao", 32..33))
            .with_message(format!("This is of type {}", "Nat".fg(a)))
            .with_color(a),
    );

    labels.push(
        Label::new(("sample.tao", 11..48))
            .with_message(format!(
                "The values are outputs of this {} expression",
                "match".fg(out),
            ))
            .with_color(out),
    );

    labels.push(
        Label::new(("sample.tao", 42..45))
            .with_message(format!("This is of type {}", "Str".fg(b)))
            .with_color(b),
    );

    let mut builder = Report::<(&str, Range<usize>)>::build(ReportKind::Error, "sample.tao", 12)
        .with_code(3)
        .with_message(format!("Incompatible types"))
        .with_note(format!(
            "Outputs of {} expressions must coerce to the same type",
            "match".fg(out)
        ));

    // Does not work because the library author forgot to implement `Copy` for what is effectively a POD data structure. 
    // I guess I learned enough about the syntax to figure out whether the library itself is appicable and to what extent.
    for label in labels.iter() {
        builder = builder.with_label(label.clone());
    }

    builder
        .finish()
        .print(("sample.tao", Source::from(include_str!("sample.tao"))))
        .unwrap()
}
