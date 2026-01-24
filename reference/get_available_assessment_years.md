# Get available assessment years

Returns information about which years have assessment data available.

## Usage

``` r
get_available_assessment_years()
```

## Value

List with years, min_year, max_year, and note about gaps

## Examples

``` r
get_available_assessment_years()
#> $years
#> [1] 2019 2022 2023 2024 2025
#> 
#> $min_year
#> [1] 2019
#> 
#> $max_year
#> [1] 2025
#> 
#> $note
#> [1] "2020 and 2021 data not available due to COVID-19 testing waiver"
#> 
```
