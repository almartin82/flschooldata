# Download raw assessment data from FLDOE

Downloads assessment data (FSA or FAST) from Florida Department of
Education.

## Usage

``` r
get_raw_assessment(end_year, subject = "ela", grade = NULL, level = "district")
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

## Value

Data frame with raw assessment data

## Examples

``` r
if (FALSE) { # \dontrun{
# Get 2024 ELA Grade 3 district data
raw_ela <- get_raw_assessment(2024, "ela", 3, "district")

# Get all 2024 Math district data
raw_math <- get_raw_assessment(2024, "math", NULL, "district")
} # }
```
