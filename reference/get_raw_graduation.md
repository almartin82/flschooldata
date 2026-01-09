# Download raw graduation data from FLDOE

Downloads graduation rate data from the Florida Department of Education
as an Excel file with two sheets (school and district levels).

## Usage

``` r
get_raw_graduation(end_year)
```

## Arguments

- end_year:

  School year end (2023-24 = 2024). Valid years: 2016-2024.

## Value

List with two data frames: \$school and \$district

## Examples

``` r
if (FALSE) { # \dontrun{
# Download 2024 graduation data
raw <- get_raw_graduation(2024)

# Access school-level data
school_data <- raw$school

# Access district-level data
district_data <- raw$district
} # }
```
