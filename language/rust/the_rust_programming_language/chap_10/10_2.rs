use std::fmt;

pub trait Summary {
    fn summerize(&self) -> String {
        // The default implementation
        return String::from("(Read more...)");
    }
}

pub struct NewsArticle {
    pub headline: String,
    pub location: String,
    pub author: String,
    pub content: String,
}

impl Summary for NewsArticle {
    fn summerize(&self) -> String {
        return format!("{}, by {}, ({})", self.headline, self.author, self.location);
    }
}

pub fn notify(item: &(impl Summary + fmt::Display)) {
    println!("Breaking news! {}", item.summerize());
}

pub fn main() {
    let article = NewsArticle {
        headline: String::from("Blabla"),
        location: String::from("Blabla"),
        author: String::from("Mr. Blabla"),
        content: String::from("Random Blabla"),
    };
    println!("{}", article.summerize());
}
