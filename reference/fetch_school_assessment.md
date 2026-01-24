# Fetch assessment data for a specific school

Convenience function to fetch assessment data for a single school.

## Usage

``` r
fetch_school_assessment(
  end_year,
  district_id,
  school_id,
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

  2-digit district ID

- school_id:

  4-digit school ID

- subject:

  Subject area: "ela" (default) or "math"

- grade:

  Grade level. NULL fetches all grades.

- tidy:

  If TRUE (default), returns tidy format

- use_cache:

  If TRUE (default), uses cached data

## Value

Data frame filtered to specified school

## Examples

``` r
if (FALSE) { # \dontrun{
# Get a specific school's assessment data
school_assess <- fetch_school_assessment(2024, "13", "0021")
} # }
```
