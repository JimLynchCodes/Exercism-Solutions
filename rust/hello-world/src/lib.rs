// The &'static here means the return type has a static lifetime.
pub fn hello() -> &'static str {
    "Hello, World!"
}
