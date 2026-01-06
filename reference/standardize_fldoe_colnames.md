# Standardize FLDOE column names

Converts various FLDOE column name formats to a consistent standard.
FLDOE membership files have paired columns like "District \#" (ID) and
"District" (name), which are mapped to DISTRICT_ID and DISTRICT_NAME.

## Usage

``` r
standardize_fldoe_colnames(colnames)
```

## Arguments

- colnames:

  Character vector of column names

## Value

Character vector of standardized column names
