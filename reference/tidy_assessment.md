# Tidy assessment data

Converts wide assessment data (with pct_level_1, pct_level_2, etc.
columns) to long (tidy) format with proficiency_level and pct columns.

## Usage

``` r
tidy_assessment(df)
```

## Arguments

- df:

  Processed assessment data frame

## Value

Tidy data frame with proficiency_level column

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- get_raw_assessment(2024, "ela", 3)
processed <- process_assessment(raw, 2024)
tidy <- tidy_assessment(processed)
} # }
```
