const DEFAULT_RESPONSE: &str = "Whatever.";
const RESPONSE_FOR_QUESTION: &str = "Sure.";
const RESPONSE_FOR_SHOUTING: &str = "Whoa, chill out!";
const RESPONSE_FOR_SHOUTED_QUESTION: &str = "Calm down, I know what I'm doing!";
const RESPONSE_FOR_NOTHING: &str = "Fine. Be that way!";

pub fn reply(message: &str) -> &str {
    let is_empty = message.trim_end().is_empty();
    let is_yelled = message == message.to_uppercase();
    let is_question = message.trim_end().ends_with("?");
    let contains_characters = message.contains(char::is_alphabetic);

    match (
        is_empty,
        is_question,
        is_yelled,
        contains_characters,
    ) {
        (true, _, _, _) => RESPONSE_FOR_NOTHING,
        (_, true, true, true) => RESPONSE_FOR_SHOUTED_QUESTION,
        (_, false, true, true) => RESPONSE_FOR_SHOUTING,
        (_, true, _, _) => RESPONSE_FOR_QUESTION,
        _ => DEFAULT_RESPONSE,
    }

}
