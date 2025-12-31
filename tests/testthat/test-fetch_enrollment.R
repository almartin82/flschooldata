# Tests for enrollment functions
# Note: Most tests are marked as skip_on_cran since they require network access

test_that("safe_numeric handles various inputs", {
  # Normal numbers
  expect_equal(safe_numeric("100"), 100)
  expect_equal(safe_numeric("1,234"), 1234)

  # Suppressed values
  expect_true(is.na(safe_numeric("*")))
  expect_true(is.na(safe_numeric("-1")))
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric("<10")))
  expect_true(is.na(safe_numeric("")))
  expect_true(is.na(safe_numeric("N/A")))
  expect_true(is.na(safe_numeric("S")))

  # Whitespace handling
  expect_equal(safe_numeric("  100  "), 100)
})

test_that("fetch_enr validates year parameter", {
  expect_error(fetch_enr(2000), "end_year must be between")
  expect_error(fetch_enr(2030), "end_year must be between")
})

test_that("get_cache_dir returns valid path", {
  cache_dir <- get_cache_dir()
  expect_true(is.character(cache_dir))
  expect_true(grepl("flschooldata", cache_dir))
})

test_that("cache functions work correctly", {
  # Test cache path generation
  path <- get_cache_path(2024, "tidy")
  expect_true(grepl("enr_tidy_2024.rds", path))

  # Test cache_exists returns FALSE for non-existent cache
  # (Assuming no cache exists for year 9999)
  expect_false(cache_exists(9999, "tidy"))
})

test_that("get_fl_district_names returns all 67 counties plus special districts", {
  names <- get_fl_district_names()

  # Should have 67 counties plus some special districts
  expect_true(length(names) >= 67)

  # Check specific counties
  expect_equal(names["01"], "Alachua")
  expect_equal(names["13"], "Miami-Dade")
  expect_equal(names["16"], "Duval")
  expect_equal(names["29"], "Hillsborough")
  expect_equal(names["48"], "Orange")
  expect_equal(names["50"], "Palm Beach")
  expect_equal(names["52"], "Pinellas")
  expect_equal(names["06"], "Broward")

  # Check special districts
  expect_equal(names["71"], "FL School for Deaf/Blind")
})

test_that("format_fl_school_id creates correct format", {
  expect_equal(format_fl_school_id(13, 21), "13-0021")
  expect_equal(format_fl_school_id(1, 1), "01-0001")
  expect_equal(format_fl_school_id("06", "0123"), "06-0123")
})

test_that("parse_fl_school_id extracts components correctly", {
  result <- parse_fl_school_id("13-0021")
  expect_equal(result$district_num, "13")
  expect_equal(result$school_num, "0021")

  # Test without dash
  result2 <- parse_fl_school_id("130021")
  expect_equal(result2$district_num, "13")
  expect_equal(result2$school_num, "0021")
})

test_that("standardize_fldoe_colnames handles various formats", {
  # Test basic standardization
  cols <- c("District", "DIST_NO", "School", "PK", "KG", "White", "TOTAL")
  result <- standardize_fldoe_colnames(cols)

  expect_true("DISTRICT" %in% result)
  expect_true("SCHOOL" %in% result)
  expect_true("GRADE_PK" %in% result)
  expect_true("GRADE_K" %in% result)
  expect_true("WHITE" %in% result)
  expect_true("TOTAL" %in% result)
})

test_that("get_fldoe_url constructs valid URLs", {
  url <- get_fldoe_url(2024, "race")
  expect_true(grepl("fldoe.org", url))
  expect_true(grepl("2324", url))
  expect_true(grepl("MembBySchoolByGradeByRace", url))
})

# Integration tests (require network access)
test_that("fetch_enr downloads and processes data", {
  skip_on_cran()
  skip_if_offline()

  # Use a recent year
  result <- fetch_enr(2024, tidy = FALSE, use_cache = FALSE)

  # Check structure
  expect_true(is.data.frame(result))
  expect_true("district_id" %in% names(result))
  expect_true("type" %in% names(result))

  # Check we have all levels
  expect_true("State" %in% result$type)
  expect_true("District" %in% result$type)

  # Check district IDs are 2 digits
  districts <- result[result$type == "District" & !is.na(result$district_id), ]
  expect_true(all(nchar(districts$district_id) == 2))
})

test_that("tidy_enr produces correct long format", {
  skip_on_cran()
  skip_if_offline()

  # Get wide data
  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  # Tidy it
  tidy_result <- tidy_enr(wide)

  # Check structure
  expect_true("grade_level" %in% names(tidy_result))
  expect_true("subgroup" %in% names(tidy_result))
  expect_true("n_students" %in% names(tidy_result))
  expect_true("pct" %in% names(tidy_result))

  # Check subgroups include expected values
  subgroups <- unique(tidy_result$subgroup)
  expect_true("total_enrollment" %in% subgroups)
})

test_that("id_enr_aggs adds correct flags", {
  skip_on_cran()
  skip_if_offline()

  # Get tidy data with aggregation flags
  result <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Check flags exist
  expect_true("is_state" %in% names(result))
  expect_true("is_district" %in% names(result))
  expect_true("is_campus" %in% names(result))

  # Check flags are boolean
  expect_true(is.logical(result$is_state))
  expect_true(is.logical(result$is_district))
  expect_true(is.logical(result$is_campus))

  # Check mutual exclusivity (each row is only one type)
  type_sums <- result$is_state + result$is_district + result$is_campus
  expect_true(all(type_sums == 1))
})

test_that("Florida has approximately 67-75 districts", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  # Filter to district rows only
  districts <- result[result$type == "District", ]

  # Florida has 67 county districts plus 4-5 special districts (lab schools, deaf/blind)
  n_districts <- nrow(districts)
  expect_true(n_districts >= 67 && n_districts <= 80)
})
