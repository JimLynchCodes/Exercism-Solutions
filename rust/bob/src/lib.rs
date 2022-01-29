const DEFAULT_RESPONSE: &str = "Whatever.";
const RESPONSE_TO_QUESTION: &str = "Sure.";
const RESPONSE_TO_YELLED: &str = "Whoa, chill out!";
const RESPONSE_TO_YELLED_QUESTION: &str = "Calm down, I know what I'm doing!";
const REPONSE_TO_NOTHING: &str = "Fine. Be that way!";

pub fn reply(message: &str) -> &str {

    let is_question = message.trim_end().ends_with("?");
    let is_yelled = message == message.to_uppercase();
    let contains_letters = message.contains(char::is_alphabetic);
    let is_empty = message.trim_end().is_empty();

    match(
        is_empty,
        is_question,
        is_yelled,
        contains_letters
    ) {
        (true, _, _, _) => REPONSE_TO_NOTHING,
        (_, true, true, true) => RESPONSE_TO_YELLED_QUESTION,
        (_, false, true, true) => RESPONSE_TO_YELLED,
        (_, true, _, _) => RESPONSE_TO_QUESTION,
        _ => DEFAULT_RESPONSE,
    }

}
