# Simulated CPUE data

Simulated catch of lobsters per pot from 2000 to 2017, with changing
seasonal, depth, and soak-time coverage.

## Usage

``` r
data(lobsters_per_pot)
```

## Format

a `tibble` containing 5 fields including:

- lobsters:

  Number of lobsters caught in one pot.

- year:

  Factor identifying fishing year, from 2000 to 2017.

- month:

  Two-digit factor identifying calendar month.

- depth:

  Fishing depth in metres.

- soak:

  Pot soak time in hours.
