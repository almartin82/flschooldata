# Process raw graduation data

Converts raw FLDOE Excel data to a standardized intermediate format.
Handles type conversions, ID extraction, and subgroup reshaping.

## Usage

``` r
process_graduation(raw_data, end_year)
```

## Arguments

- raw_data:

  List with \$district and \$school data frames from
  get_raw_graduation()

- end_year:

  School year end

## Value

Data frame with processed graduation data
