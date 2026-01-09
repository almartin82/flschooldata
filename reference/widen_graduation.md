# Widen graduation data

Converts processed graduation data to wide format (one row per school,
with subgroup columns).

## Usage

``` r
widen_graduation(processed, level = "all")
```

## Arguments

- processed:

  Processed data frame from process_graduation()

- level:

  One of "state", "district", "school", or "all" (default)

## Value

Data frame in wide format
