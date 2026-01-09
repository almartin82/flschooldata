# Clear the flschooldata cache

Removes cached data files.

## Usage

``` r
clear_cache(key = NULL)
```

## Arguments

- key:

  Optional cache key pattern to clear (e.g., "enr", "graduation",
  "2024"). If NULL, clears all cached files.

## Value

Invisibly returns the number of files removed

## Examples

``` r
if (FALSE) { # \dontrun{
# Clear all cached data
clear_cache()

# Clear only 2024 data
clear_cache("2024")

# Clear only enrollment data
clear_cache("enr")

# Clear only graduation data
clear_cache("graduation")
} # }
```
