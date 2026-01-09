# Fetch Florida graduation rate data

Downloads and processes 4-year cohort graduation rate data from the
Florida Department of Education (FLDOE).

## Usage

``` r
fetch_graduation(end_year, level = "all", tidy = TRUE, use_cache = FALSE)
```

## Arguments

- end_year:

  School year end (e.g., 2024 for 2023-24 school year). Valid years:
  2016-2024. See get_available_grad_years().

- level:

  One of "state", "district", "school", or "all" (default). Controls
  which geographic levels to return.

- tidy:

  If TRUE (default), returns long format (one row per school-subgroup).
  If FALSE, returns wide format (one row per school, subgroup columns).

- use_cache:

  If TRUE, uses cached data if available. Default is FALSE.

## Value

Data frame with graduation rate data.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all 2024 graduation data in tidy format
grad_2024 <- fetch_graduation(2024)

# Get state-level only
grad_state <- fetch_graduation(2024, level = "state")

# Get district-level in wide format
grad_district_wide <- fetch_graduation(2024, level = "district", tidy = FALSE)

# See available years
get_available_grad_years()
} # }
```
