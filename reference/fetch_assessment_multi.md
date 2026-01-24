# Fetch assessment data for multiple years

Downloads and combines assessment data for multiple school years.

## Usage

``` r
fetch_assessment_multi(
  end_years,
  subject = "ela",
  grade = NULL,
  level = "district",
  tidy = TRUE,
  use_cache = TRUE
)
```

## Arguments

- end_years:

  Vector of school year ends (e.g., c(2022, 2023, 2024))

- subject:

  Subject area: "ela" (default) or "math"

- grade:

  Grade level (3-10 for ELA, 3-8 for Math). NULL fetches all grades.

- level:

  Aggregation level: "district" (default) or "school"

- tidy:

  If TRUE (default), returns data in long (tidy) format.

- use_cache:

  If TRUE (default), uses locally cached data when available.

## Value

Combined data frame with assessment data for all requested years

## Examples

``` r
if (FALSE) { # \dontrun{
# Get 3 years of ELA data
assess_multi <- fetch_assessment_multi(c(2022, 2023, 2024), "ela")

# Track proficiency trends at state level
library(dplyr)
assess_multi |>
  filter(is_state, is_proficient, grade == "03") |>
  group_by(end_year) |>
  summarize(pct_proficient = sum(pct))
} # }
```
