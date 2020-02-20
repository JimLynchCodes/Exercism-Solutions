-module(leap).

-export([leap_year/1]).

leap_year(Year) ->
    (Year rem 400 == 0) or
      (Year rem 4 == 0) and (Year rem 100 /= 0).
