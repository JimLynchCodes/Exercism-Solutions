-module(two_fer).

-export([two_fer/0, two_fer/1]).

two_fer() -> two_fer("you").

two_fer(_Name) -> "One for " ++ _Name ++ ", one for me.".
