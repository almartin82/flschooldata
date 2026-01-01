# flschooldata: Fetch and Process Florida School Data

Downloads and processes school data from the Florida Department of
Education (FLDOE). Provides functions for fetching enrollment data
including membership by school, grade, and race/ethnicity, and
transforming it into tidy format for analysis.

## Main functions

- [`fetch_enr`](https://almartin82.github.io/flschooldata/reference/fetch_enr.md):

  Fetch enrollment data for a school year

- [`fetch_enr_multi`](https://almartin82.github.io/flschooldata/reference/fetch_enr_multi.md):

  Fetch enrollment data for multiple years

- [`tidy_enr`](https://almartin82.github.io/flschooldata/reference/tidy_enr.md):

  Transform wide data to tidy (long) format

- [`id_enr_aggs`](https://almartin82.github.io/flschooldata/reference/id_enr_aggs.md):

  Add aggregation level flags

- [`enr_grade_aggs`](https://almartin82.github.io/flschooldata/reference/enr_grade_aggs.md):

  Create grade-level aggregations

## Cache functions

- [`cache_status`](https://almartin82.github.io/flschooldata/reference/cache_status.md):

  View cached data files

- [`clear_cache`](https://almartin82.github.io/flschooldata/reference/clear_cache.md):

  Remove cached data files

## ID System

Florida uses a county-based district system with:

- District Codes: 2 digits (01-67 for counties, 71-75 for special
  districts)

- School Numbers: 4 digits (0001-9899 for regular schools)

- Combined ID: District code + School number (e.g., "13-0021" for
  Miami-Dade)

## Data Sources

Data is sourced from the Florida Department of Education:

- Students Data:
  <https://www.fldoe.org/accountability/data-sys/edu-info-accountability-services/pk-12-public-school-data-pubs-reports/students.stml>

- FTE History:
  <https://www.fldoe.org/finance/fl-edu-finance-program-fefp/fte-info/student-enrollment.stml>

- Master School ID: <https://eds.fldoe.org/EDS/MasterSchoolID/>

## Data Availability

- Membership by Grade/Race: 2014-present (current format)

- FTE Enrollment: 2008-present (Excel format)

- Historical archive data goes back further but format varies

## See also

Useful links:

- <https://github.com/almartin82/flschooldata>

- Report bugs at <https://github.com/almartin82/flschooldata/issues>

## Author

**Maintainer**: Al Martin <almartin@example.com>
