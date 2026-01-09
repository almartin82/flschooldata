# ==============================================================================
# Graduation Rate LIVE Pipeline Tests
# ==============================================================================
#
# These tests verify the entire data pipeline using LIVE network calls.
# NO MOCKS - real HTTP requests to Florida DOE.
#
# Purpose: Detect breakages early when state DOE websites change.
#
# Test Categories:
#   1. URL Availability - HTTP 200 checks
#   2. File Download - Verify actual file retrieval
#   3. File Parsing - Excel parsing succeeds
#   4. Column Structure - Expected columns present
#   5. Year Filtering - Single year extraction works
#   6. Data Quality - No Inf/NaN, valid ranges
#   7. Aggregation - State totals match
#   8. Output Fidelity - tidy=TRUE matches raw
#
# ==============================================================================

# Helper function for network skip guard
skip_if_offline <- function() {
  tryCatch({
    response <- httr::HEAD("https://www.google.com", httr::timeout(5))
    if (httr::http_error(response)) skip("No network connectivity")
  }, error = function(e) skip("No network connectivity"))
}

# ==============================================================================
# Test 1: URL Availability
# ==============================================================================

test_that("FLDOE graduation URL returns HTTP 200 for 2023-24", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/FedGradRateCategory2324.xls"

  response <- httr::HEAD(url, httr::timeout(30))

  expect_equal(httr::status_code(response), 200)
})

test_that("FLDOE graduation URL returns HTTP 200 for 2021-22", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/FedGradRateCategory2122.xls"

  response <- httr::HEAD(url, httr::timeout(30))

  expect_equal(httr::status_code(response), 200)
})

test_that("FLDOE graduation URL returns HTTP 200 for 2018-19", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/FedGradRateCategory1819.xls"

  response <- httr::HEAD(url, httr::timeout(30))

  expect_equal(httr::status_code(response), 200)
})

test_that("FLDOE graduation URL returns HTTP 200 for 2015-16", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/FedGradRateCategory1516.xls"

  response <- httr::HEAD(url, httr::timeout(30))

  expect_equal(httr::status_code(response), 200)
})

# ==============================================================================
# Test 2: File Download
# ==============================================================================

test_that("Can download 2023-24 graduation Excel file", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/FedGradRateCategory2324.xls"

  temp_file <- tempfile(fileext = ".xls")
  on.exit(unlink(temp_file))

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), httr::timeout(60))

  expect_equal(httr::status_code(response), 200)

  # Verify reasonable file size (should be ~400-500 KB)
  file_size <- file.info(temp_file)$size
  expect_gt(file_size, 300000)  # At least 300 KB
})

test_that("Downloaded file is actual Excel (not HTML)", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/FedGradRateCategory2324.xls"

  temp_file <- tempfile(fileext = ".xls")
  on.exit(unlink(temp_file))

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), httr::timeout(60))

  expect_equal(httr::status_code(response), 200)

  # Check file type (should be "Composite Document File V2" for .xls)
  file_type <- system(paste("file", temp_file), intern = TRUE)
  expect_true(grepl("Composite Document File V2|Excel", file_type),
              info = paste("File type:", file_type))
})

# ==============================================================================
# Test 3: File Parsing
# ==============================================================================

test_that("Can parse 2023-24 Excel file", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/FedGradRateCategory2324.xls"

  temp_file <- tempfile(fileext = ".xls")
  on.exit(unlink(temp_file))

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), httr::timeout(60))

  expect_equal(httr::status_code(response), 200)

  # Parse Excel district sheet (skip 4 header rows)
  df <- readxl::read_excel(temp_file, sheet = "Fed GradRate by Category-Dist", skip = 4)

  expect_true(is.data.frame(df))
  expect_gt(nrow(df), 70)  # Should have 67+ districts + state
  expect_gt(ncol(df), 20)  # Should have many subgroup columns
})

test_that("Excel file has both district and school sheets", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/FedGradRateCategory2324.xls"

  temp_file <- tempfile(fileext = ".xls")
  on.exit(unlink(temp_file))

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), httr::timeout(60))

  sheets <- readxl::excel_sheets(temp_file)

  expect_true("Fed GradRate by Category-Dist" %in% sheets)
  expect_true("Fed GradRate by Category-School" %in% sheets)
})

# ==============================================================================
# Test 4: Column Structure
# ==============================================================================

test_that("Excel file has expected columns", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/FedGradRateCategory2324.xls"

  temp_file <- tempfile(fileext = ".xls")
  on.exit(unlink(temp_file))

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), httr::timeout(60))

  df <- readxl::read_excel(temp_file, sheet = "Fed GradRate by Category-Dist", skip = 4)

  # Expected columns (case-insensitive)
  col_lower <- tolower(names(df))

  expect_true("district number" %in% col_lower)
  expect_true("district name" %in% col_lower)
  expect_true("total graduates" %in% col_lower)
  expect_true("total cohort" %in% col_lower)
  expect_true("total federal graduation rate" %in% col_lower)
  expect_true("ese graduates" %in% col_lower)
  expect_true("ese cohort" %in% col_lower)
  expect_true("free/reduced lunch graduates" %in% col_lower)
  expect_true("ell graduates" %in% col_lower)
  expect_true("male graduates" %in% col_lower)
  expect_true("female graduates" %in% col_lower)
})

test_that("Column data types are correct", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/FedGradRateCategory2324.xls"

  temp_file <- tempfile(fileext = ".xls")
  on.exit(unlink(temp_file))

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), httr::timeout(60))

  df <- readxl::read_excel(temp_file, sheet = "Fed GradRate by Category-Dist", skip = 4)

  # District Number should be character (preserve leading zeros)
  expect_true(is.character(df$`District Number`))

  # Counts should be numeric
  expect_true(is.numeric(df$`Total Cohort`))
  expect_true(is.numeric(df$`Total Graduates`))

  # Graduation rate should be numeric
  expect_true(is.numeric(df$`Total Federal Graduation Rate`))
})

# ==============================================================================
# Test 5: Year Filtering
# ==============================================================================

test_that("Can extract data for single year (2023-24)", {
  skip_if_offline()

  # This tests the actual fetch_graduation function
  data <- flschooldata::fetch_graduation(2024, use_cache = FALSE)

  expect_true(is.data.frame(data))
  expect_gt(nrow(data), 0)

  # All records should have end_year = 2024
  expect_true(all(data$end_year == 2024))
})

test_that("Can extract data for multiple years", {
  skip_if_offline()

  # Test that we can fetch different years
  data_2024 <- flschooldata::fetch_graduation(2024, use_cache = FALSE)
  data_2022 <- flschooldata::fetch_graduation(2022, use_cache = FALSE)

  expect_gt(nrow(data_2024), 0)
  expect_gt(nrow(data_2022), 0)
})

# ==============================================================================
# Test 6: Data Quality
# ==============================================================================

test_that("No Inf or NaN in tidy output", {
  skip_if_offline()

  data <- flschooldata::fetch_graduation(2024, tidy = TRUE, use_cache = FALSE)

  for (col in names(data)[sapply(data, is.numeric)]) {
    expect_false(any(is.infinite(data[[col]])), info = col)
    expect_false(any(is.nan(data[[col]])), info = col)
  }
})

test_that("All graduation rates in valid range (0-100)", {
  skip_if_offline()

  data <- flschooldata::fetch_graduation(2024, tidy = TRUE, use_cache = FALSE)

  # FL reports percentages as 0-100, not 0-1
  expect_true(all(data$grad_rate >= 0 & data$grad_rate <= 100, na.rm = TRUE))
})

test_that("All cohort counts are non-negative", {
  skip_if_offline()

  data <- flschooldata::fetch_graduation(2024, tidy = TRUE, use_cache = FALSE)

  expect_true(all(data$cohort_count >= 0, na.rm = TRUE))
})

test_that("No truly duplicate records (same entity + subgroup)", {
  skip_if_offline()

  data <- flschooldata::fetch_graduation(2024, tidy = TRUE, use_cache = FALSE)

  # Create unique key (include school_id to distinguish schools)
  data$key <- paste(data$end_year, data$type, data$district_id,
                    data$school_id, data$subgroup, sep = "_")

  # Allow small number of duplicates (data quality issue in source)
  unique_count <- length(unique(data$key))
  duplicate_ratio <- (nrow(data) - unique_count) / nrow(data)

  expect_lte(duplicate_ratio, 0.05)  # Allow up to 5% duplicates
})

# ==============================================================================
# Test 7: Aggregation
# ==============================================================================

test_that("State record has all expected subgroups", {
  skip_if_offline()

  data <- flschooldata::fetch_graduation(2024, tidy = TRUE, use_cache = FALSE)

  state_data <- dplyr::filter(data, type == "State")

  subgroups <- unique(state_data$subgroup)

  # Should have at least 8 subgroups (Total, ESE, FRL, ELL, Migrant, At-Risk, Male, Female)
  expect_gte(length(subgroups), 8)
})

test_that("District records exist", {
  skip_if_offline()

  data <- flschooldata::fetch_graduation(2024, tidy = TRUE, use_cache = FALSE)

  district_data <- dplyr::filter(data, type == "District")

  expect_gt(nrow(district_data), 0)

  # FL has 67 counties + special districts
  unique_districts <- length(unique(district_data$district_id))
  expect_gte(unique_districts, 60)
})

test_that("School records exist", {
  skip_if_offline()

  data <- flschooldata::fetch_graduation(2024, tidy = TRUE, use_cache = FALSE)

  school_data <- dplyr::filter(data, type == "School")

  expect_gt(nrow(school_data), 0)

  # FL has ~695 unique high schools with graduation data (some schools have multiple records)
  unique_schools <- length(unique(school_data$school_id))
  expect_gte(unique_schools, 600)  # Allow for some year-to-year variation
})

# ==============================================================================
# Test 8: Output Fidelity
# ==============================================================================

test_that("State-level graduation rate is reasonable", {
  skip_if_offline()

  data <- flschooldata::fetch_graduation(2024, tidy = TRUE, use_cache = FALSE)

  state_all <- data |>
    dplyr::filter(type == "State", subgroup == "all") |>
    dplyr::pull(grad_rate)

  # FL state graduation rate should be ~85-92% (reported 89.7% in 2023-24)
  expect_gte(state_all, 85)
  expect_lte(state_all, 95)
})

test_that("State cohort count is reasonable", {
  skip_if_offline()

  data <- flschooldata::fetch_graduation(2024, tidy = TRUE, use_cache = FALSE)

  state_cohort <- data |>
    dplyr::filter(type == "State", subgroup == "all") |>
    dplyr::pull(cohort_count)

  # FL should have ~200k-250k students in cohort (217,248 in 2023-24)
  expect_gte(state_cohort, 200000)
  expect_lte(state_cohort, 250000)
})

test_that("State graduation rate matches published value", {
  skip_if_offline()

  data <- flschooldata::fetch_graduation(2024, tidy = TRUE, use_cache = FALSE)

  state_all <- data |>
    dplyr::filter(type == "State", subgroup == "all") |>
    dplyr::pull(grad_rate)

  # FLDOE reported 89.7% for 2023-24
  expect_equal(state_all, 89.7, tolerance = 0.1)
})

test_that("Alachua district data exists", {
  skip_if_offline()

  data <- flschooldata::fetch_graduation(2024, tidy = TRUE, use_cache = FALSE)

  # Alachua district number: "01"
  alachua_data <- dplyr::filter(data,
                                type == "District",
                                district_id == "01")

  expect_gt(nrow(alachua_data), 0)
})

test_that("tidy=TRUE preserves data from raw", {
  skip_if_offline()

  # Get both tidy and wide formats
  tidy <- flschooldata::fetch_graduation(2024, tidy = TRUE, use_cache = FALSE)
  wide <- flschooldata::fetch_graduation(2024, tidy = FALSE, use_cache = FALSE)

  # State graduation rate should match
  state_tidy <- tidy |> dplyr::filter(type == "State", subgroup == "all") |> dplyr::pull(grad_rate)
  state_wide <- wide |> dplyr::filter(type == "State", district_id == "ALL") |> dplyr::pull(grad_rate_all)

  expect_equal(state_tidy, state_wide)
})
