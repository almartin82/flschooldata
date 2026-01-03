# Get available years for Florida enrollment data

Returns the range of years available from the Florida Department of
Education (FLDOE). Data is available from the 2007-08 school year
(end_year = 2008) through 2023-24 (end_year = 2024).

## Usage

``` r
get_available_years()
```

## Value

A list with components:

- min_year:

  Integer. The earliest available year (2008).

- max_year:

  Integer. The most recent available year (2024).

- description:

  Character. A description of the data availability.

## Details

Note: Years 2008-2013 have limited demographic data (FTE files only).
Full demographic and grade-level data is available from 2014 onward.

## Examples

``` r
get_available_years()
#> $min_year
#> [1] 2008
#> 
#> $max_year
#> [1] 2024
#> 
#> $description
#> [1] "FLDOE enrollment data (2007-08 through 2023-24 school years; 2008-2013 have limited demographics)"
#> 
# Returns list(min_year = 2008, max_year = 2024, description = "...")
```
