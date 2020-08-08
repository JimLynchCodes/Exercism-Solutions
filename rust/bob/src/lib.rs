const DEFAULT_RESPONSE: &str = "Whatever.";
const RESPONSE_FOR_QUESTION: &str = "Sure.";
const RESPONSE_FOR_SHOUTING: &str = "Whoa, chill out!";
const RESPONSE_FOR_SHOUTED_QUESTION: &str = "Calm down, I know what I'm doing!";
const RESPONSE_FOR_NOTHING: &str = "Fine. Be that way!";

pub fn reply(message: &str) -> &str {
    let message = message.trim_end();
    let is_question = message.trim_end().ends_with('?');
    let is_shouted = message.to_uppercase() == message && message.contains(char::is_alphabetic);

    match message {
        _m if message.is_empty() => RESPONSE_FOR_NOTHING,
        _m if is_question && is_shouted => RESPONSE_FOR_SHOUTED_QUESTION,
        _m if is_question => RESPONSE_FOR_QUESTION,
        _m if is_shouted => RESPONSE_FOR_SHOUTING,
        _ => DEFAULT_RESPONSE,
    }
}
