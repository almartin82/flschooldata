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
California, Texas, and New York. One function call pulls it all into R.

``` r
library(flschooldata)
library(dplyr)

enr <- fetch_enr_multi(2014:2025)

enr %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, n_students) %>%
  mutate(change = n_students - lag(n_students))
```

![Florida enrollment
trend](https://almartin82.github.io/flschooldata/articles/enrollment_hooks_files/figure-html/statewide-chart-1.png)

Florida enrollment trend

------------------------------------------------------------------------

### 2. Miami-Dade is larger than most states

Miami-Dade County Public Schools, with 340,000+ students, is the
fourth-largest school district in America and enrolls more students than
several entire states.

``` r
enr_2025 <- fetch_enr(2025)

enr_2025 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  arrange(desc(n_students)) %>%
  head(10) %>%
  select(district_name, n_students)
```

![Florida top 10
districts](https://almartin82.github.io/flschooldata/articles/enrollment_hooks_files/figure-html/top-districts-chart-1.png)

Florida top 10 districts

------------------------------------------------------------------------

### 3. COVID barely dented Florida enrollment

While other states saw sharp declines, Florida’s enrollment dipped only
briefly in 2021 and has since rebounded, fueled by in-migration from
other states.

``` r
enr <- fetch_enr_multi(2019:2025)

enr %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, n_students) %>%
  mutate(change = n_students - lag(n_students))
```

------------------------------------------------------------------------

### 4. Hispanic students are the plurality

Hispanic students now comprise over 35% of Florida enrollment,
surpassing white students to become the largest demographic group
statewide.

``` r
enr_2025 %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("hispanic", "white", "black", "asian", "multiracial")) %>%
  mutate(pct = round(pct * 100, 1)) %>%
  select(subgroup, n_students, pct) %>%
  arrange(desc(n_students))
```

![Florida student
demographics](https://almartin82.github.io/flschooldata/articles/enrollment_hooks_files/figure-html/demographics-chart-1.png)

Florida student demographics

------------------------------------------------------------------------

### 5. Central Florida is the growth engine

Orange, Osceola, and Polk counties in Central Florida have been among
the fastest-growing in the state, driven by the Orlando metro boom.

``` r
enr <- fetch_enr_multi(2014:2025)

enr %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Orange|Osceola|Polk", district_name)) %>%
  group_by(district_name) %>%
  summarize(
    y2014 = n_students[end_year == 2014],
    y2025 = n_students[end_year == 2025],
    pct_change = round((y2025 / y2014 - 1) * 100, 1)
  ) %>%
  arrange(desc(pct_change))
```

------------------------------------------------------------------------

### 6. Broward and Miami-Dade are declining

South Florida’s two largest districts have been losing students for
years as families relocate to Central Florida or out of state.

``` r
enr <- fetch_enr_multi(2018:2025)

enr %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Broward|Miami-Dade", district_name)) %>%
  select(end_year, district_name, n_students) %>%
  tidyr::pivot_wider(names_from = district_name, values_from = n_students)
```

![South Florida enrollment
decline](https://almartin82.github.io/flschooldata/articles/enrollment_hooks_files/figure-html/south-florida-chart-1.png)

South Florida enrollment decline

------------------------------------------------------------------------

### 7. Florida Virtual School is a district unto itself

Florida Virtual School (FLVS) serves tens of thousands of students
statewide, making Florida a pioneer in virtual education.

``` r
enr_2025 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Virtual|FLVS", district_name, ignore.case = TRUE)) %>%
  select(district_name, n_students)
```

------------------------------------------------------------------------

### 8. Kindergarten is the leading indicator

Florida kindergarten enrollment has been relatively stable, unlike
states with sharp K declines–reflecting Florida’s continued population
growth.

``` r
enr <- fetch_enr_multi(2019:2025)

enr %>%
  filter(is_state, subgroup == "total_enrollment",
         grade_level %in% c("K", "01", "05", "09")) %>%
  select(end_year, grade_level, n_students) %>%
  tidyr::pivot_wider(names_from = grade_level, values_from = n_students)
```

------------------------------------------------------------------------

### 9. Black students are 22% of enrollment

Florida has a significant Black student population, concentrated in
South Florida and the Jacksonville/North Florida region.

``` r
enr_2025 %>%
  filter(is_district, subgroup == "black", grade_level == "TOTAL") %>%
  mutate(pct = round(pct * 100, 1)) %>%
  arrange(desc(pct)) %>%
  head(10) %>%
  select(district_name, n_students, pct)
```

------------------------------------------------------------------------

### 10. Florida has one district per county

Unlike states with hundreds of small districts, Florida has exactly 67
county-based school districts (plus a few special-purpose districts),
making statewide analysis cleaner.

``` r
enr_2025 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  summarize(
    n_districts = n(),
    total_students = sum(n_students, na.rm = TRUE),
    avg_per_district = round(mean(n_students, na.rm = TRUE))
  )
```

------------------------------------------------------------------------

### 11. Tampa Bay is catching up to South Florida

Hillsborough, Pinellas, and Pasco counties represent Florida’s other
major metro area. While South Florida declines, Tampa Bay has held
steady or grown.

``` r
enr <- fetch_enr_multi(2015:2025)

enr %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Hillsborough|Pinellas|Pasco", district_name)) %>%
  group_by(district_name) %>%
  summarize(
    y2015 = n_students[end_year == 2015],
    y2025 = n_students[end_year == 2025],
    pct_change = round((y2025 / y2015 - 1) * 100, 1)
  ) %>%
  arrange(desc(y2025))
```

![Tampa Bay
enrollment](https://almartin82.github.io/flschooldata/articles/enrollment_hooks_files/figure-html/tampa-bay-chart-1.png)

Tampa Bay enrollment

------------------------------------------------------------------------

### 12. Jacksonville is Florida’s hidden giant

Duval County (Jacksonville) often flies under the radar, but it’s
Florida’s 6th largest district with over 125,000 students.

``` r
enr <- fetch_enr_multi(2015:2025)

enr %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Duval", district_name)) %>%
  select(end_year, district_name, n_students) %>%
  mutate(change = n_students - lag(n_students))
```

![Jacksonville
enrollment](https://almartin82.github.io/flschooldata/articles/enrollment_hooks_files/figure-html/jacksonville-chart-1.png)

Jacksonville enrollment

------------------------------------------------------------------------

### 13. Hispanic enrollment surpassed White for the first time

A demographic milestone: Hispanic student enrollment has overtaken White
enrollment in Florida, reflecting the state’s changing population.

``` r
enr <- fetch_enr_multi(2015:2025)

enr %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("hispanic", "white")) %>%
  select(end_year, subgroup, n_students) %>%
  tidyr::pivot_wider(names_from = subgroup, values_from = n_students)
```

![Hispanic-White
crossover](https://almartin82.github.io/flschooldata/articles/enrollment_hooks_files/figure-html/hispanic-white-crossover-chart-1.png)

Hispanic-White crossover

------------------------------------------------------------------------

### 14. Small rural counties face steep declines

While large metros grow, many of Florida’s smallest counties are losing
students rapidly, often by double-digit percentages.

``` r
enr <- fetch_enr_multi(2015:2025)

enr %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         end_year %in% c(2015, 2025)) %>%
  group_by(district_name) %>%
  summarize(
    y2015 = n_students[end_year == 2015],
    y2025 = n_students[end_year == 2025],
    pct_change = round((y2025 / y2015 - 1) * 100, 1)
  ) %>%
  arrange(pct_change) %>%
  head(10)
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
library(flschooldata)
library(dplyr)

enr <- fetch_enr_multi(2015:2024)

enr %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("hispanic", "white", "black", "asian", "multiracial")) %>%
  group_by(subgroup) %>%
  summarize(
    y2015 = n_students[end_year == 2015],
    y2024 = n_students[end_year == 2024],
    pct_change = round((y2024 / y2015 - 1) * 100, 1)
  ) %>%
  arrange(desc(pct_change))
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
library(flschooldata)
library(dplyr)

assess_2024 <- fetch_assessment(2024, "ela", grade = 3, level = "district",
                                 tidy = FALSE, use_cache = TRUE)

assess_2024 |>
  filter(is_state) |>
  select(end_year, subject, grade, n_tested, pct_proficient,
         pct_level_1, pct_level_2, pct_level_3, pct_level_4, pct_level_5)
#> # A tibble: 1 x 10
#>   end_year subject grade n_tested pct_proficient pct_level_1 pct_level_2
#>      <dbl> <chr>   <chr>    <dbl>          <dbl>       <dbl>       <dbl>
#> 1     2024 ELA     03      216473             55          22          22
#>   pct_level_3 pct_level_4 pct_level_5
#>         <dbl>       <dbl>       <dbl>
#> 1          23          19          13
```

![Florida 3rd Grade
ELA](https://almartin82.github.io/flschooldata/articles/florida-assessment_files/figure-html/state-ela-g3-chart-1.png)

Florida 3rd Grade ELA

### Miami-Dade outperforms the state average

Despite being the largest district with over 25,000 3rd graders tested,
Miami-Dade’s 56% proficiency rate exceeds the state average.

``` r
assess_2024 |>
  filter(!is_state, n_tested > 5000) |>
  arrange(desc(n_tested)) |>
  select(district_name, n_tested, pct_proficient) |>
  head(10)
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
