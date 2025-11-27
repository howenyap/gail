use std::fmt::Display;

pub fn join_with_separator(items: &[impl Display], separator: &str) -> String {
    items
        .iter()
        .map(|e| e.to_string())
        .collect::<Vec<String>>()
        .join(separator)
}

pub fn to_plural(s: &str, count: usize) -> String {
    if count == 1 {
        s.to_string()
    } else {
        format!("{s}s")
    }
}
