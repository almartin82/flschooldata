# flschooldata

Fetch and analyze Florida school enrollment data from the Florida
Department of Education (FLDOE) in R or Python.

**[Documentation](https://almartin82.github.io/flschooldata/)** \| **[15
Key
Insights](https://almartin82.github.io/flschooldata/articles/enrollment_hooks.html)**
\| **[Getting
Started](https://almartin82.github.io/flschooldata/articles/quickstart.html)**

## What can you find with flschooldata?

> **See the full analysis with charts and data output:** [15 Insights
> from Florida Enrollment
> Data](https://almartin82.github.io/flschooldata/articles/enrollment_hooks.html)

**18 years of enrollment data (2008-2025).** 2.9 million students across
67 county districts in the Sunshine State. Here are fifteen stories
hiding in the numbers:

------------------------------------------------------------------------

### 1. Florida is America’s fourth-largest school system

Florida public schools serve nearly 3 million students, trailing only
California, Texas, and New York.

``` r
enr <- fetch_enr_multi(2015:2024, use_cache = TRUE)

state_totals <- enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students) |>
  mutate(change = n_students - lag(n_students),
         pct_change = round(change / lag(n_students) * 100, 2))

state_totals
#>    end_year n_students change pct_change
#> 1      2015    2750108     NA         NA
#> 2      2016    2785286  35178       1.28
#> 3      2017    2810249  24963       0.90
#> 4      2018    2826290  16041       0.57
#> 5      2019    2840029  13739       0.49
#> 6      2020    2852303  12274       0.43
#> 7      2021    2784931 -67372      -2.36
#> 8      2022    2826573  41642       1.50
#> 9      2023    2864292  37719       1.33
#> 10     2024    2865908   1616       0.06
```

![Florida enrollment
trend](https://almartin82.github.io/flschooldata/articles/enrollment_hooks_files/figure-html/statewide-chart-1.png)

Florida enrollment trend

------------------------------------------------------------------------

### 2. Miami-Dade is larger than most states

Miami-Dade County Public Schools, with 340,000+ students, is the
fourth-largest school district in America.

``` r
enr_2024 <- fetch_enr(2024, use_cache = TRUE)

top_10 <- enr_2024 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  head(10) |>
  select(district_name, n_students)

top_10
#>    district_name n_students
#> 1     MIAMI-DADE     337610
#> 2        BROWARD     251397
#> 3   HILLSBOROUGH     224144
#> 4         ORANGE     207695
#> 5     PALM BEACH     191390
#> 6          DUVAL     129083
#> 7           POLK     115990
#> 8            LEE      99952
#> 9       PINELLAS      90969
#> 10         PASCO      85808
```

![Florida top 10
districts](https://almartin82.github.io/flschooldata/articles/enrollment_hooks_files/figure-html/top-districts-chart-1.png)

Florida top 10 districts

------------------------------------------------------------------------

### 3. COVID barely dented Florida enrollment

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

### 4. Hispanic students are the plurality

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

![Florida student
demographics](https://almartin82.github.io/flschooldata/articles/enrollment_hooks_files/figure-html/demographics-chart-1.png)

Florida student demographics

------------------------------------------------------------------------

### 5. Central Florida is the growth engine

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
#> [1] district_name y2015         y2024         pct_change
#> <0 rows> (or 0-length row.names)
```

------------------------------------------------------------------------

### 6. Broward and Miami-Dade are declining

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
#> [1] end_year
#> <0 rows> (or 0-length row.names)
```

![South Florida enrollment
decline](https://almartin82.github.io/flschooldata/articles/enrollment_hooks_files/figure-html/south-florida-chart-1.png)

South Florida enrollment decline

------------------------------------------------------------------------

### 7. Florida Virtual School is a district unto itself

Florida Virtual School (FLVS) serves tens of thousands of students
statewide.

``` r
virtual <- enr_2024 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Virtual|FLVS", district_name, ignore.case = TRUE)) |>
  select(district_name, n_students)

virtual
#>   district_name n_students
#> 1    FL VIRTUAL       8874
```

------------------------------------------------------------------------

### 8. Kindergarten is the leading indicator

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
#>   end_year      K     01     05     09
#> 1     2019 200437 206545 222947 221023
#> 2     2020 202460 207143 217518 223805
#> 3     2021 186147 200215 207750 227753
#> 4     2022 199099 200741 217770 231499
#> 5     2023 197925 209516 209060 237920
#> 6     2024 195032 204402 205135 232506
```

------------------------------------------------------------------------

### 9. Black students are 22% of enrollment

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

### 10. Florida’s county-based system is unique

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

### 11. Tampa Bay is catching up to South Florida

Hillsborough and Pinellas counties represent Florida’s other major metro
area. While South Florida declines, Tampa Bay has held steady.

``` r
tampa <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Hillsborough|Pinellas|Pasco", district_name)) |>
  group_by(district_name) |>
  summarize(
    y2015 = n_students[end_year == 2015],
    y2024 = n_students[end_year == 2024],
    pct_change = round((y2024 / y2015 - 1) * 100, 1),
    .groups = "drop"
  ) |>
  arrange(desc(y2024))

tampa
#> [1] district_name y2015         y2024         pct_change
#> <0 rows> (or 0-length row.names)
```

![Tampa Bay
enrollment](https://almartin82.github.io/flschooldata/articles/enrollment_hooks_files/figure-html/tampa-bay-chart-1.png)

Tampa Bay enrollment

------------------------------------------------------------------------

### 12. Jacksonville is Florida’s hidden giant

Duval County (Jacksonville) often flies under the radar, but it’s
Florida’s 6th largest district with over 125,000 students.

``` r
jax <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Duval", district_name)) |>
  select(end_year, district_name, n_students) |>
  mutate(change = n_students - lag(n_students))

jax
#> [1] end_year      district_name n_students    change
#> <0 rows> (or 0-length row.names)
```

![Jacksonville
enrollment](https://almartin82.github.io/flschooldata/articles/enrollment_hooks_files/figure-html/jacksonville-chart-1.png)

Jacksonville enrollment

------------------------------------------------------------------------

### 13. Hispanic enrollment surpassed White for the first time

A demographic milestone: Hispanic student enrollment has overtaken White
enrollment in Florida, reflecting broader population shifts.

``` r
crossover <- enr |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("hispanic", "white")) |>
  select(end_year, subgroup, n_students) |>
  pivot_wider(names_from = subgroup, values_from = n_students)

crossover
#>    end_year   white hispanic
#> 1      2015 1108227   846425
#> 2      2016 1101823   879982
#> 3      2017 1089439   912300
#> 4      2018 1077811   937352
#> 5      2019 1063838   963338
#> 6      2020 1054580   986005
#> 7      2021 1016772   974588
#> 8      2022 1021630  1003659
#> 9      2023 1014065  1043390
#> 10     2024  988822  1066935
```

![Hispanic-White
crossover](https://almartin82.github.io/flschooldata/articles/enrollment_hooks_files/figure-html/hispanic-white-crossover-chart-1.png)

Hispanic-White crossover

------------------------------------------------------------------------

### 14. Small rural counties face steep declines

While large metros grow, many of Florida’s smallest counties are losing
students rapidly.

``` r
# Get the first and last year for comparison
first_year <- min(enr$end_year)
last_year <- max(enr$end_year)

county_change <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         end_year %in% c(first_year, last_year)) |>
  group_by(district_name) |>
  filter(n() == 2) |>  # Only counties with both years
  summarize(
    y_first = n_students[end_year == first_year],
    y_last = n_students[end_year == last_year],
    change = y_last - y_first,
    pct_change = round((y_last / y_first - 1) * 100, 1),
    .groups = "drop"
  )

# Counties losing the most (by %)
declining <- county_change |>
  filter(y_first > 1000) |>  # Exclude tiny counties
  arrange(pct_change) |>
  head(10)

declining
#>    district_name y_first y_last change pct_change
#> 1        GADSDEN    5837   4598  -1239      -21.2
#> 2        LIBERTY    1330   1156   -174      -13.1
#> 3       PINELLAS  103754  90969 -12785      -12.3
#> 4      LAFAYETTE    1132   1007   -125      -11.0
#> 5        MADISON    2498   2228   -270      -10.8
#> 6        JACKSON    6726   6037   -689      -10.2
#> 7         DESOTO    4658   4199   -459       -9.9
#> 8         HARDEE    5095   4598   -497       -9.8
#> 9         TAYLOR    2885   2602   -283       -9.8
#> 10       CALHOUN    2149   1942   -207       -9.6
```

![Rural county
decline](https://almartin82.github.io/flschooldata/articles/enrollment_hooks_files/figure-html/rural-decline-chart-1.png)

Rural county decline

------------------------------------------------------------------------

### 15. Multiracial students are the fastest-growing demographic

While Hispanic students are the largest group, multiracial student
enrollment has grown at the highest rate percentage-wise since 2015
(34.9% growth, compared to 26.1% for Hispanic students and 15.3% for
Asian students).

``` r
demo_trend <- enr |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("hispanic", "white", "black", "asian", "multiracial")) |>
  select(end_year, subgroup, n_students) |>
  group_by(subgroup) |>
  mutate(pct_change = round((n_students / first(n_students) - 1) * 100, 1)) |>
  ungroup()

demo_trend |>
  filter(end_year == 2024) |>
  arrange(desc(pct_change))
#>    end_year    subgroup n_students pct_change
#> 50     2024 multiracial     119907       34.9
#> 48     2024    hispanic    1066935       26.1
#> 49     2024       asian      81688       15.3
#> 47     2024       black     599867       -4.2
#> 46     2024       white     988822      -10.8
```

![Demographic growth
trends](https://almartin82.github.io/flschooldata/articles/enrollment_hooks_files/figure-html/multiracial-growth-chart-1.png)

Demographic growth trends

------------------------------------------------------------------------

------------------------------------------------------------------------

## Assessment Data

> **See the full analysis with charts and data output:** [15 Insights
> from Florida Assessment
> Data](https://almartin82.github.io/flschooldata/articles/florida-assessment.html)

The `flschooldata` package also provides access to Florida’s
standardized assessment data:

- **FAST (Florida Assessment of Student Thinking):** 2023-present
- **FSA (Florida Standards Assessments):** 2019-2022

### Just over half of Florida third graders read on grade level

In 2024, 55% of Florida 3rd graders scored at Level 3 or above on the
FAST ELA Reading assessment.

``` r
assess_2024 <- fetch_assessment(2024, "ela", grade = 3, level = "district",
                                 tidy = FALSE, use_cache = TRUE)

state_g3 <- assess_2024 |>
  filter(is_state) |>
  select(end_year, subject, grade, n_tested, pct_proficient,
         pct_level_1, pct_level_2, pct_level_3, pct_level_4, pct_level_5)

state_g3
#>   end_year subject grade n_tested pct_proficient pct_level_1 pct_level_2
#> 1     2024     ELA    03   216473             55          22          22
#>   pct_level_3 pct_level_4 pct_level_5
#> 1          23          19          13
```

![Florida 3rd Grade
ELA](https://almartin82.github.io/flschooldata/articles/florida-assessment_files/figure-html/state-ela-g3-chart-1.png)

Florida 3rd Grade ELA

### Miami-Dade outperforms the state average

Despite being the largest district with over 25,000 3rd graders tested,
Miami-Dade’s 56% proficiency rate exceeds the state average.

``` r
large_districts <- assess_2024 |>
  filter(!is_state, n_tested > 5000) |>
  arrange(desc(n_tested)) |>
  select(district_name, n_tested, pct_proficient) |>
  head(10)

large_districts
#>    district_name n_tested pct_proficient
#> 1     MIAMI-DADE    25178             56
#> 2        BROWARD    18457             57
#> 3   HILLSBOROUGH    16802             51
#> 4         ORANGE    15743             57
#> 5     PALM BEACH    14593             54
#> 6          DUVAL    10095             49
#> 7           POLK     9295             49
#> 8            LEE     7644             48
#> 9       PINELLAS     6654             62
#> 10         PASCO     6511             53
```

![Florida large district
comparison](https://almartin82.github.io/flschooldata/articles/florida-assessment_files/figure-html/miami-dade-chart-1.png)

Florida large district comparison

------------------------------------------------------------------------

## Installation

``` r
# install.packages("remotes")
remotes::install_github("almartin82/flschooldata")
```

## Quick start

### R

``` r
library(flschooldata)
library(dplyr)

# === Enrollment Data ===

# Fetch one year
enr_2025 <- fetch_enr(2025)

# Fetch multiple years
enr_multi <- fetch_enr_multi(2020:2025)

# State totals
enr_2025 %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

# District breakdown
enr_2025 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  arrange(desc(n_students))

# Miami-Dade County (district 13)
enr_2025 %>%
  filter(district_id == "13", subgroup == "total_enrollment")

# === Assessment Data ===

# Fetch 2024 ELA Grade 3 district data
assess_2024 <- fetch_assessment(2024, "ela", grade = 3)

# Fetch all grades
assess_all_grades <- fetch_assessment(2024, "ela")

# Fetch Math data
math_2024 <- fetch_assessment(2024, "math")

# State proficiency
assess_2024 %>%
  filter(is_state) %>%
  select(subject, grade, pct_proficient)

# Check available years
get_available_assessment_years()
```

### Python

``` python
import pyflschooldata as fl

# === Enrollment Data ===

# Check available years
years = fl.get_available_years()
print(f"Data available: {years['min_year']}-{years['max_year']}")

# Fetch one year
df = fl.fetch_enr(2025)

# Fetch multiple years
df_multi = fl.fetch_enr_multi([2020, 2021, 2022, 2023, 2024, 2025])

# State totals
state_total = df[(df['is_state'] == True) &
                 (df['subgroup'] == 'total_enrollment') &
                 (df['grade_level'] == 'TOTAL')]

# District breakdown
districts = df[(df['is_district'] == True) &
               (df['subgroup'] == 'total_enrollment') &
               (df['grade_level'] == 'TOTAL')].sort_values('n_students', ascending=False)

# === Assessment Data ===

# Fetch 2024 ELA Grade 3 data
assess = fl.fetch_assessment(2024, subject='ela', grade=3)

# State proficiency
state_assess = assess[assess['is_state'] == True]
print(state_assess[['subject', 'grade', 'pct_proficient']])

# Check available assessment years
assess_years = fl.get_available_assessment_years()
print(f"Assessment data: {assess_years['years']}")
```

## Data availability

### Enrollment Data

| Years         | Source                 | Notes                              |
|---------------|------------------------|------------------------------------|
| **2014-2025** | FLDOE Membership Files | Full school-level demographic data |
| **2008-2013** | FLDOE FTE Files        | District-level totals only         |

### Assessment Data

| Years          | Assessment                                    | Notes                            |
|----------------|-----------------------------------------------|----------------------------------|
| **2023-2025**  | FAST (Florida Assessment of Student Thinking) | ELA Grades 3-10, Math Grades 3-8 |
| **2019, 2022** | FSA (Florida Standards Assessments)           | No 2020/2021 due to COVID-19     |

Data is sourced from the Florida Department of Education.

### What’s included

- **Levels:** State, district (67 county + special), school (~4,500)
- **Demographics:** White, Black, Hispanic, Asian, American Indian,
  Pacific Islander, Two or More Races
- **Gender:** Male, Female
- **Grade levels:** Pre-K through 12

### Florida ID system

- **67 county districts:** Codes 01-67 (one per county)
- **Special districts:** Codes 71-75 (lab schools, FL School for
  Deaf/Blind)
- **School numbers:** 4-digit codes
- **Combined ID:** `DD-SSSS` format (e.g., “13-0021” for a Miami-Dade
  school)

### Caveats

- Pre-2014 data has limited demographics (FTE files only)
- Charter school identification not directly available in membership
  files
- ESE, ELL, and free/reduced lunch in separate files (not yet
  integrated)

## Data source

Florida Department of Education: [PK-12 Data
Publications](https://www.fldoe.org/accountability/data-sys/edu-info-accountability-services/pk-12-public-school-data-pubs-reports/students.stml)

## Part of the State Schooldata Project

A simple, consistent interface for accessing state-published school data
in Python and R.

**All 50 state packages:**
[github.com/almartin82](https://github.com/almartin82?tab=repositories&q=schooldata)

## Author

[Andy Martin](https://github.com/almartin82) (<almartin@gmail.com>)

## License

MIT
