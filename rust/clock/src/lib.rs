use std::fmt;

pub struct Clock{
    hours: i32,
    minutes: i32,
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        // unimplemented!(
        //     "Construct a new Clock from {} hours and {} minutes",
        //     hours,
        //     minutes
        // );
        let ret = Clock { hours, minutes };
        ret.mins_to_hours()
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        unimplemented!("Add {} minutes to existing Clock time", minutes);
    }

    fn mins_to_hours(&self) -> Self {
        let mut ret = Clock {
            hours: self.hours,
            minutes: self.minutes,
        };
        ret.hours += ret.minutes / 60;
        ret.minutes %= 60;
        if ret.minutes < 0 {
            ret.minutes += 60;
            ret.hours -= 1;
        }
        ret.hours %= 24;
        if ret.hours < 0 {
            ret.hours += 24
        };
        ret
    }
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:02}:{:02}", self.hours, self.minutes)
    }
}
