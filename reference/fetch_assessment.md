# Fetch Florida assessment data

Downloads and processes assessment data from the Florida Department of
Education. Returns FSA (2019-2022) or FAST (2023-2025) data.

## Usage

``` r
fetch_assessment(
  end_year,
  subject = "ela",
  grade = NULL,
  level = "district",
  tidy = TRUE,
  use_cache = TRUE
)
```

## Arguments

- end_year:

  School year end (2023-24 = 2024). Valid years: 2019, 2022-2025.

- subject:

  Subject area: "ela" (default) or "math"

- grade:

  Grade level (3-10 for ELA, 3-8 for Math). NULL fetches all grades.

- level:

  Aggregation level: "district" (default) or "school"

- tidy:

  If TRUE (default), returns data in long (tidy) format with
  proficiency_level column. If FALSE, returns wide format.

- use_cache:

  If TRUE (default), uses locally cached data when available.

## Value

Data frame with assessment data

## Examples

``` r
if (FALSE) { # \dontrun{
# Get 2024 FAST ELA data for all grades (district level)
assess_2024 <- fetch_assessment(2024, "ela")

# Get 2024 Math Grade 3 school-level data
math_g3 <- fetch_assessment(2024, "math", grade = 3, level = "school")

# Get wide format (proficiency levels as columns)
assess_wide <- fetch_assessment(2024, "ela", tidy = FALSE)

# Get 2019 FSA data
fsa_2019 <- fetch_assessment(2019, "ela")

# Filter to Miami-Dade County
library(dplyr)
miami <- assess_2024 |>
  filter(district_id == "13")

# Get state-level proficiency rates
state_prof <- assess_2024 |>
  filter(is_state, is_proficient) |>
  group_by(grade) |>
  summarize(pct_proficient = sum(pct))
} # }
```
