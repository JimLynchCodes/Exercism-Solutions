module SpaceAge (Planet (..), ageOn) where

data Planet = Mercury | Venus | Earth | Mars | Jupiter | Saturn| Uranus | Neptune

secondsPerEarthYear :: Int
secondsPerEarthYear = 31557600

earthYearsForPlanet :: Fractional p => Planet -> p
earthYearsForPlanet planet = case planet of
  Earth -> 1
  Mercury -> 0.2408467
  Venus -> 0.61519726
  Mars -> 1.8808158
  Jupiter -> 11.862615
  Saturn -> 29.447498
  Uranus -> 84.016846
  Neptune -> 164.79132

earthYearsFromSeconds :: Float -> Float
earthYearsFromSeconds seconds = seconds / fromIntegral secondsPerEarthYear

ageOn :: Planet -> Float -> Float
ageOn planet seconds = earthYearsFromSeconds seconds / earthYearsForPlanet planet
