const RESPONSE_FOR_QUESTION: &str = "Sure.";
const RESPONSE_FOR_SHOUTING: &str = "Whoa, chill out!";
const RESPONSE_FOR_SHOUTED_QUESTION: &str = "Calm down, I know what I'm doing!";
const RESPONSE_FOR_NOTHING: &str = "Fine. Be that way!";

pub fn reply(_message: &str) -> &str {
    let mut response = "Whatever.";

    let is_shouted = _message.to_uppercase() == _message;
    let is_question = _message.trim().ends_with('?');
    let is_nothing = _message.trim().is_empty();
    let contains_letters = _message.find(char::is_alphabetic).is_some();

    if is_question {
        response = RESPONSE_FOR_QUESTION;
    }

    if is_shouted && contains_letters {
        response = RESPONSE_FOR_SHOUTING;
    }

    if is_shouted && is_question && contains_letters {
        response = RESPONSE_FOR_SHOUTED_QUESTION;
    }

    if is_nothing {
        response = RESPONSE_FOR_NOTHING;
    }

    response
}
