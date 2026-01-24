# Fetch assessment data for a specific district

Convenience function to fetch assessment data for a single district.

## Usage

``` r
fetch_district_assessment(
  end_year,
  district_id,
  subject = "ela",
  grade = NULL,
  tidy = TRUE,
  use_cache = TRUE
)
```

## Arguments

- end_year:

  School year end

- district_id:

  2-digit district ID (e.g., "13" for Miami-Dade)

- subject:

  Subject area: "ela" (default) or "math"

- grade:

  Grade level. NULL fetches all grades.

- tidy:

  If TRUE (default), returns tidy format

- use_cache:

  If TRUE (default), uses cached data

## Value

Data frame filtered to specified district

## Examples

``` r
if (FALSE) { # \dontrun{
# Get Miami-Dade County (district 13) assessment data
miami_assess <- fetch_district_assessment(2024, "13")

# Get Broward County (district 06) Math data
broward_math <- fetch_district_assessment(2024, "06", subject = "math")
} # }
```
