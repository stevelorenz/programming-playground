//! In-memory indexes.

#![allow(dead_code, unused_variables)]

fn tokenize(text: &str) -> Vec<&str> {
    text.split(|ch: char| !ch.is_alphabetic())
        .filter(|w| !w.is_empty())
        .collect()
}
