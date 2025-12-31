# flschooldata

Fetch and process Florida school enrollment data from the Florida Department of Education (FLDOE).

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("almartin82/flschooldata")
```

## Quick Start

```r
library(flschooldata)

# Get 2024 enrollment data (2023-24 school year)
enr <- fetch_enr(2024)

# View state totals
enr %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

# Miami-Dade County data (district 13)
miami <- enr %>%
  filter(district_id == "13")

# Get multiple years
enr_multi <- fetch_enr_multi(2020:2024)
```

## Data Availability

### Format Era 1: Membership Files (2014-present)

Full school-level data with detailed breakdowns:
- **Membership by School by Grade by Race/Ethnicity** - Most comprehensive
- Includes: district, school, grade, race/ethnicity
- ~4,500 schools per year

### Format Era 2: FTE Files (2008-2013)

District-level Full-Time Equivalent (FTE) enrollment:
- Limited demographic breakdowns
- Program-based counts rather than headcounts
- No school-level detail

### Data Columns

#### Identifiers
| Column | Description |
|--------|-------------|
| `end_year` | School year end (2024 = 2023-24) |
| `type` | "State", "District", or "Campus" |
| `district_id` | 2-digit district code (01-67, 71-75) |
| `campus_id` | District-School ID (e.g., "13-0021") |
| `district_name` | County/district name |
| `campus_name` | School name |

#### Demographics (wide format)
| Column | Description |
|--------|-------------|
| `row_total` | Total enrollment |
| `white` | White students |
| `black` | Black/African American students |
| `hispanic` | Hispanic/Latino students |
| `asian` | Asian students |
| `pacific_islander` | Native Hawaiian/Pacific Islander |
| `native_american` | American Indian/Alaska Native |
| `multiracial` | Two or more races |
| `male` | Male students |
| `female` | Female students |

#### Grade Levels (wide format)
| Column | Description |
|--------|-------------|
| `grade_pk` | Pre-Kindergarten |
| `grade_k` | Kindergarten |
| `grade_01` - `grade_12` | Grades 1-12 |

#### Tidy Format Columns
| Column | Description |
|--------|-------------|
| `grade_level` | "TOTAL", "PK", "K", "01"-"12" |
| `subgroup` | "total_enrollment", "hispanic", etc. |
| `n_students` | Count of students |
| `pct` | Percentage of total |
| `is_state` | TRUE if state-level row |
| `is_district` | TRUE if district-level row |
| `is_campus` | TRUE if school-level row |

## Florida ID System

Florida uses a county-based district system:

- **67 county districts**: Codes 01-67 (one per county)
- **Special districts**: Codes 71-75 (lab schools, FL School for Deaf/Blind)
- **School numbers**: 4-digit codes (0001-9899)
- **Combined ID**: `DD-SSSS` format (e.g., "13-0021" for a Miami-Dade school)

Major districts by enrollment:
| Code | District | Students (approx) |
|------|----------|-------------------|
| 13 | Miami-Dade | 340,000 |
| 06 | Broward | 260,000 |
| 29 | Hillsborough | 220,000 |
| 48 | Orange | 210,000 |
| 50 | Palm Beach | 180,000 |
| 16 | Duval | 130,000 |
| 52 | Pinellas | 100,000 |

## Caching

Downloaded data is cached locally for 30 days:

```r
# View cache status
cache_status()

# Clear all cached data
clear_cache()

# Clear specific year
clear_cache(2024)

# Force fresh download
fetch_enr(2024, use_cache = FALSE)
```

## Data Source

All data is sourced from the Florida Department of Education:

- **Students Data**: https://www.fldoe.org/accountability/data-sys/edu-info-accountability-services/pk-12-public-school-data-pubs-reports/students.stml
- **FTE History**: https://www.fldoe.org/finance/fl-edu-finance-program-fefp/fte-info/student-enrollment.stml
- **Master School ID**: https://eds.fldoe.org/EDS/MasterSchoolID/
- **Archive**: https://www.fldoe.org/accountability/data-sys/edu-info-accountability-services/pk-12-public-school-data-pubs-reports/archive.stml

## Key Insights About Florida School Data

1. **Fourth largest school system in the US** with approximately 2.8 million students

2. **Significant demographic diversity**: Florida has one of the most diverse student populations, with Hispanic students comprising about 35% and Black students about 22%

3. **County-based districts**: Unlike many states, Florida has exactly one school district per county (plus a few special-purpose districts)

4. **Strong charter sector**: Florida has significant charter school enrollment, though charter vs traditional status is not directly available in these membership files

5. **Survey-based data collection**: FLDOE collects enrollment data through Survey 2 (October) and Survey 3 (February) each year

6. **Virtual education**: Florida has substantial virtual/online enrollment including Florida Virtual School (FLVS)

## Caveats and Limitations

1. **Pre-2014 data is limited**: FTE files have district totals only, no school-level or demographic breakdowns

2. **Suppression**: Small counts may be suppressed for privacy (marked as NA)

3. **Charter data**: Charter schools are included in the membership files but there's no explicit charter flag in the raw data

4. **Special programs**: ESE (Exceptional Student Education), ELL (English Language Learners), and Free/Reduced Lunch data are in separate files not yet integrated

5. **Survey timing**: Data represents enrollment at Survey 2 (October), not end-of-year

6. **File format changes**: FLDOE occasionally changes file formats and column names, which may cause issues with older years

## License

MIT
