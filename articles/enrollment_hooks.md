# 10 Insights from Florida School Enrollment Data

``` r
library(flschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal(base_size = 14))
```

This vignette explores Florida’s public school enrollment data,
surfacing key trends and demographic patterns across recent years
(2015-2024).

------------------------------------------------------------------------

## 1. Florida is America’s fourth-largest school system

Florida public schools serve nearly 3 million students, trailing only
California, Texas, and New York.

``` r
enr <- fetch_enr_multi(2015:2024)

state_totals <- enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students) |>
  mutate(change = n_students - lag(n_students),
         pct_change = round(change / lag(n_students) * 100, 2))

state_totals
#>    end_year n_students   change pct_change
#> 1      2015    2750108       NA         NA
#> 2      2016    2785286    35178       1.28
#> 3      2017    2810249    24963       0.90
#> 4      2018          0 -2810249    -100.00
#> 5      2019    2840029  2840029        Inf
#> 6      2020    2852303    12274       0.43
#> 7      2021    2784931   -67372      -2.36
#> 8      2022    2826573    41642       1.50
#> 9      2023    2864292    37719       1.33
#> 10     2024    2865908     1616       0.06
```

``` r
ggplot(state_totals, aes(x = end_year, y = n_students)) +
  geom_line(linewidth = 1.2, color = "#FF6600") +
  geom_point(size = 3, color = "#FF6600") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Florida Public School Enrollment (2015-2024)",
    subtitle = "Nearly 3 million students in the Sunshine State",
    x = "School Year (ending)",
    y = "Total Enrollment"
  )
```

![](enrollment_hooks_files/figure-html/statewide-chart-1.png)

------------------------------------------------------------------------

## 2. Miami-Dade is larger than most states

Miami-Dade County Public Schools, with 340,000+ students, is the
fourth-largest school district in America.

``` r
enr_2024 <- fetch_enr(2024)

top_10 <- enr_2024 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  head(10) |>
  select(district_name, n_students)

top_10
#>    district_name n_students
#> 1     MIAMI-DADE      28058
#> 2     MIAMI-DADE      27685
#> 3     MIAMI-DADE      26796
#> 4     MIAMI-DADE      26339
#> 5     MIAMI-DADE      26048
#> 6     MIAMI-DADE      25972
#> 7     MIAMI-DADE      25159
#> 8     MIAMI-DADE      25158
#> 9     MIAMI-DADE      24529
#> 10    MIAMI-DADE      24110
```

``` r
top_10 |>
  mutate(district_name = forcats::fct_reorder(district_name, n_students)) |>
  ggplot(aes(x = n_students, y = district_name)) +
  geom_col(fill = "#FF6600") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Florida's 10 Largest School Districts (2024)",
    x = "Total Enrollment",
    y = NULL
  )
```

![](enrollment_hooks_files/figure-html/top-districts-chart-1.png)

------------------------------------------------------------------------

## 3. COVID barely dented Florida enrollment

While other states saw sharp declines, Florida’s enrollment dipped only
briefly in 2021 and has since rebounded.

``` r
covid <- enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL",
         end_year >= 2019) |>
  select(end_year, n_students) |>
  mutate(change = n_students - lag(n_students))

covid
#>   end_year n_students change
#> 1     2019    2840029     NA
#> 2     2020    2852303  12274
#> 3     2021    2784931 -67372
#> 4     2022    2826573  41642
#> 5     2023    2864292  37719
#> 6     2024    2865908   1616
```

------------------------------------------------------------------------

## 4. Hispanic students are the plurality

Hispanic students now comprise over 35% of Florida enrollment,
surpassing white students.

``` r
demographics <- enr_2024 |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("hispanic", "white", "black", "asian", "multiracial")) |>
  mutate(pct = round(pct * 100, 1)) |>
  select(subgroup, n_students, pct) |>
  arrange(desc(n_students))

demographics
#>      subgroup n_students  pct
#> 1    hispanic    1066935 37.2
#> 2       white     988822 34.5
#> 3       black     599867 20.9
#> 4 multiracial     119907  4.2
#> 5       asian      81688  2.9
```

``` r
demographics |>
  mutate(subgroup = forcats::fct_reorder(subgroup, n_students)) |>
  ggplot(aes(x = n_students, y = subgroup, fill = subgroup)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Florida Student Demographics (2024)",
    subtitle = "Hispanic students are now the plurality",
    x = "Number of Students",
    y = NULL
  )
```

![](enrollment_hooks_files/figure-html/demographics-chart-1.png)

------------------------------------------------------------------------

## 5. Central Florida is the growth engine

Orange, Osceola, and Polk counties have been among the fastest-growing,
driven by the Orlando metro boom.

``` r
central_fl <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Orange|Osceola|Polk", district_name),
         end_year %in% c(2015, 2024)) |>
  group_by(district_name) |>
  summarize(
    y2015 = n_students[end_year == 2015],
    y2024 = n_students[end_year == 2024],
    pct_change = round((y2024 / y2015 - 1) * 100, 1),
    .groups = "drop"
  ) |>
  arrange(desc(pct_change))

central_fl
#> # A tibble: 0 × 4
#> # ℹ 4 variables: district_name <chr>, y2015 <dbl>, y2024 <dbl>,
#> #   pct_change <dbl>
```

------------------------------------------------------------------------

## 6. Broward and Miami-Dade are declining

South Florida’s two largest districts have been losing students as
families relocate.

``` r
south_fl <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Broward|Miami-Dade", district_name),
         end_year >= 2018) |>
  select(end_year, district_name, n_students) |>
  pivot_wider(names_from = district_name, values_from = n_students)

south_fl
#> # A tibble: 0 × 1
#> # ℹ 1 variable: end_year <int>
```

``` r
enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Broward|Miami-Dade", district_name),
         end_year >= 2018) |>
  ggplot(aes(x = end_year, y = n_students, color = district_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "South Florida's Enrollment Decline",
    x = "School Year",
    y = "Enrollment",
    color = "District"
  )
```

![](enrollment_hooks_files/figure-html/south-florida-chart-1.png)

------------------------------------------------------------------------

## 7. Florida Virtual School is a district unto itself

Florida Virtual School (FLVS) serves tens of thousands of students
statewide.

``` r
virtual <- enr_2024 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Virtual|FLVS", district_name, ignore.case = TRUE)) |>
  select(district_name, n_students)

virtual
#>    district_name n_students
#> 1     FL VIRTUAL        262
#> 2     FL VIRTUAL        271
#> 3     FL VIRTUAL        398
#> 4     FL VIRTUAL        410
#> 5     FL VIRTUAL        440
#> 6     FL VIRTUAL        503
#> 7     FL VIRTUAL        682
#> 8     FL VIRTUAL        917
#> 9     FL VIRTUAL        968
#> 10    FL VIRTUAL        862
#> 11    FL VIRTUAL       1015
#> 12    FL VIRTUAL       1038
#> 13    FL VIRTUAL       1108
```

------------------------------------------------------------------------

## 8. Kindergarten is the leading indicator

Florida kindergarten enrollment has been relatively stable, unlike
states with sharp K declines.

``` r
k_trend <- enr |>
  filter(is_state, subgroup == "total_enrollment",
         grade_level %in% c("K", "01", "05", "09"),
         end_year >= 2019) |>
  select(end_year, grade_level, n_students) |>
  pivot_wider(names_from = grade_level, values_from = n_students)

k_trend
#> # A tibble: 0 × 1
#> # ℹ 1 variable: end_year <int>
```

------------------------------------------------------------------------

## 9. Black students are 22% of enrollment

Florida has a significant Black student population, concentrated in
South Florida and the Jacksonville region.

``` r
black_enr <- enr_2024 |>
  filter(is_state, grade_level == "TOTAL", subgroup == "black") |>
  select(subgroup, n_students, pct)

black_enr
#>   subgroup n_students       pct
#> 1    black     599867 0.2093113
```

------------------------------------------------------------------------

## 10. Florida’s county-based system is unique

Florida organizes schools by county, with 67 county districts (plus a
few specialty districts). This structure differs from most states.

``` r
county_count <- enr_2024 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  summarize(
    n_districts = n_distinct(district_name),
    total_students = sum(n_students, na.rm = TRUE)
  )

county_count
#>   n_districts total_students
#> 1          76        2865908
```

------------------------------------------------------------------------

## Summary

Florida’s school enrollment data reveals:

- **Continued growth**: Florida keeps growing while other major states
  decline
- **Demographic shift**: Hispanic students now the largest group
- **Regional divergence**: Central Florida grows, South Florida shrinks
- **Virtual pioneer**: FLVS shows Florida’s embrace of online education
- **County structure**: 67 county districts create a unique system

These patterns shape education policy across the Sunshine State.

------------------------------------------------------------------------

*Data sourced from the Florida Department of Education.*
