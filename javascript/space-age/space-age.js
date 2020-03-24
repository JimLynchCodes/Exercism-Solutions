
const SECONDS_IN_EARTH_YEAR = 365.25 * 24 * 60 * 60

// Number of "Earth years" for each planet's full orbit.
const ORBITAL_PERIODS = {
  'mercury': 0.2408467,
  'venus': 0.61519726,
  'earth': 1,
  'mars': 1.8808158,
  'jupiter': 11.862615,
  'saturn': 29.447498,
  'uranus': 84.016846,
  'neptune': 164.79132,
}

export const age = (planet, seconds) => Number((seconds / SECONDS_IN_EARTH_YEAR / ORBITAL_PERIODS[planet]).toFixed(2))
