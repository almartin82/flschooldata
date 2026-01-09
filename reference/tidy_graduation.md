# Tidy graduation data

Converts processed graduation data to tidy format (long format). Keeps
one row per school-subgroup combination.

## Usage

``` r
tidy_graduation(processed, level = "all")
```

## Arguments

- processed:

  Processed data frame from process_graduation()

- level:

  One of "state", "district", "school", or "all" (default)

## Value

Data frame in tidy format
