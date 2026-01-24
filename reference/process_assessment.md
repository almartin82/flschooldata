# Process raw assessment data

Cleans and standardizes raw assessment data from FLDOE.

## Usage

``` r
process_assessment(raw_df, end_year)
```

## Arguments

- raw_df:

  Raw data frame from get_raw_assessment()

- end_year:

  School year end

## Value

Processed data frame with standardized schema

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- get_raw_assessment(2024, "ela", 3)
processed <- process_assessment(raw, 2024)
} # }
```
