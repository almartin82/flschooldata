# Download membership data (2014-present)

Downloads the "Membership by School by Grade by Race/Ethnicity" Excel
files from FLDOE. These provide the most detailed enrollment data.

## Usage

``` r
download_membership_data(end_year)
```

## Arguments

- end_year:

  School year end (2014-2025)

## Value

List with campus and district data frames

## Details

The Excel file has 3 sheets: State (summary), District, and School. Row
1 is a privacy notice, row 2 contains headers.
