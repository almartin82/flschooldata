# ==============================================================================
# Live Pipeline Tests for FLschooldata
# ==============================================================================
#
# These tests verify each step of the data pipeline using LIVE network calls.
# The goal is to detect breakages early when FLDOE website changes.
#
# Test categories (following project standard):
# 1. URL Availability Tests
# 2. File Download Tests
# 3. File Parsing Tests
# 4. Column Structure Tests
# 5. Year Filtering Tests
# 6. Aggregation Tests
# 7. Data Quality Tests
# 8. Output Fidelity Tests
#
# ==============================================================================

# Helper function: skip if offline
skip_if_offline <- function() {
  tryCatch({
    response <- httr::HEAD("https://www.google.com", httr::timeout(5))
    if (httr::http_error(response)) skip("No network connectivity")
  }, error = function(e) skip("No network connectivity"))
}

# ==============================================================================
# 1. URL Availability Tests
# ==============================================================================

test_that("FLDOE membership URL returns HTTP 200 for 2024", {
  skip_on_cran()
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/2324MembBySchoolByGradeByRace.xlsx"
  response <- httr::HEAD(url, httr::timeout(30))

  expect_equal(httr::status_code(response), 200,
               label = paste("URL:", url))
})

test_that("FLDOE membership URL returns HTTP 200 for 2025 (current year)", {
  skip_on_cran()
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/2425MembBySchoolByGradeByRace.xlsx"
  response <- httr::HEAD(url, httr::timeout(30))

  expect_equal(httr::status_code(response), 200,
               label = paste("URL:", url))
})

test_that("FLDOE membership URLs work for years 2015-2024", {
  skip_on_cran()
  skip_if_offline()

  years <- 2015:2024
  for (yr in years) {
    year_code <- paste0(sprintf("%02d", (yr - 1) %% 100), sprintf("%02d", yr %% 100))
    url <- paste0("https://www.fldoe.org/core/fileparse.php/7584/urlt/",
                  year_code, "MembBySchoolByGradeByRace.xlsx")
    response <- httr::HEAD(url, httr::timeout(30))

    expect_equal(httr::status_code(response), 200,
                 label = paste("Year:", yr, "URL:", url))
  }
})

test_that("FLDOE FTE URLs work for years 2008-2013", {
  skip_on_cran()
  skip_if_offline()

  years <- 2008:2013
  for (yr in years) {
    year_str <- paste0(yr - 1, "-", sprintf("%02d", yr %% 100))
    url <- paste0("https://www.fldoe.org/core/fileparse.php/7508/urlt/",
                  year_str, "FinalFTEx.xls")
    response <- httr::HEAD(url, httr::timeout(30))

    expect_equal(httr::status_code(response), 200,
                 label = paste("Year:", yr, "URL:", url))
  }
})

# ==============================================================================
# 2. File Download Tests
# ==============================================================================

test_that("Can download 2024 membership Excel file", {
  skip_on_cran()
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/2324MembBySchoolByGradeByRace.xlsx"
  temp_file <- tempfile(fileext = ".xlsx")

  response <- httr::GET(url,
                        httr::write_disk(temp_file, overwrite = TRUE),
                        httr::timeout(120))

  expect_equal(httr::status_code(response), 200)
  expect_true(file.exists(temp_file))
  expect_gt(file.info(temp_file)$size, 100000,
            label = "File should be > 100KB (actual enrollment file)")

  # Verify it's a real Excel file, not an HTML error page
  file_type <- system2("file", temp_file, stdout = TRUE)
  expect_true(grepl("Microsoft|Excel|Zip|OpenDocument", file_type, ignore.case = TRUE),
              label = paste("File type:", file_type))

  unlink(temp_file)
})

test_that("Can download 2013 FTE Excel file", {
  skip_on_cran()
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7508/urlt/2012-13FinalFTEx.xls"
  temp_file <- tempfile(fileext = ".xls")

  response <- httr::GET(url,
                        httr::write_disk(temp_file, overwrite = TRUE),
                        httr::timeout(120))

  expect_equal(httr::status_code(response), 200)
  expect_true(file.exists(temp_file))
  expect_gt(file.info(temp_file)$size, 10000,
            label = "FTE file should be > 10KB")

  unlink(temp_file)
})

# ==============================================================================
# 3. File Parsing Tests
# ==============================================================================

test_that("Can parse 2024 membership Excel with readxl", {
  skip_on_cran()
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/2324MembBySchoolByGradeByRace.xlsx"
  temp_file <- tempfile(fileext = ".xlsx")
  httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), httr::timeout(120))

  # List sheets succeeds
  sheets <- readxl::excel_sheets(temp_file)
  expect_gt(length(sheets), 0)
  expect_true("School" %in% sheets, label = "Should have 'School' sheet")
  expect_true("District" %in% sheets, label = "Should have 'District' sheet")
  expect_true("State" %in% sheets, label = "Should have 'State' sheet")

  # Read data returns valid data frame
  df <- readxl::read_excel(temp_file, sheet = "School", skip = 2, col_types = "text")
  expect_true(is.data.frame(df))
  expect_gt(nrow(df), 20000, label = "School sheet should have > 20k rows")

  unlink(temp_file)
})

test_that("Can parse 2015 membership Excel (older year)", {
  skip_on_cran()
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/1415MembBySchoolByGradeByRace.xlsx"
  temp_file <- tempfile(fileext = ".xlsx")
  httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), httr::timeout(120))

  sheets <- readxl::excel_sheets(temp_file)
  expect_true("School" %in% sheets)

  df <- readxl::read_excel(temp_file, sheet = "School", skip = 2, col_types = "text")
  expect_true(is.data.frame(df))
  expect_gt(nrow(df), 15000)

  unlink(temp_file)
})

# ==============================================================================
# 4. Column Structure Tests
# ==============================================================================

test_that("2024 School sheet has expected columns", {
  skip_on_cran()
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/2324MembBySchoolByGradeByRace.xlsx"
  temp_file <- tempfile(fileext = ".xlsx")
  httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), httr::timeout(120))

  df <- readxl::read_excel(temp_file, sheet = "School", skip = 2, col_types = "text")

  # Expected columns (case-insensitive check)
  expected <- c("District", "School", "Grade", "White", "Black", "Hispanic", "Asian")
  for (exp in expected) {
    expect_true(any(grepl(exp, names(df), ignore.case = TRUE)),
                label = paste("Missing column pattern:", exp))
  }

  unlink(temp_file)
})

test_that("2024 District sheet has expected columns", {
  skip_on_cran()
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/2324MembBySchoolByGradeByRace.xlsx"
  temp_file <- tempfile(fileext = ".xlsx")
  httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), httr::timeout(120))

  df <- readxl::read_excel(temp_file, sheet = "District", skip = 2, col_types = "text")

  # Expected columns
  expected <- c("District", "Grade", "White", "Black", "Hispanic")
  for (exp in expected) {
    expect_true(any(grepl(exp, names(df), ignore.case = TRUE)),
                label = paste("Missing column pattern:", exp))
  }

  unlink(temp_file)
})

# ==============================================================================
# 5. Year Filtering Tests
# ==============================================================================

test_that("get_raw_enr returns data for 2024", {
  skip_on_cran()
  skip_if_offline()

  raw <- get_raw_enr(2024)

  expect_true(is.list(raw))
  expect_true("campus" %in% names(raw))
  expect_true("district" %in% names(raw))
  expect_gt(nrow(raw$campus), 20000)
  expect_gt(nrow(raw$district), 500)

  # Check end_year is set
  expect_true(all(raw$campus$end_year == 2024))
  expect_true(all(raw$district$end_year == 2024))
})

test_that("get_raw_enr returns data for 2015", {
  skip_on_cran()
  skip_if_offline()

  raw <- get_raw_enr(2015)

  expect_true(is.list(raw))
  expect_gt(nrow(raw$campus), 15000)
  expect_true(all(raw$campus$end_year == 2015))
})

test_that("get_raw_enr returns FTE data for 2013", {
  skip_on_cran()
  skip_if_offline()

  raw <- get_raw_enr(2013)

  expect_true(is.list(raw))
  # FTE files are district-only
  expect_gt(nrow(raw$district), 50)
  expect_true(all(raw$district$end_year == 2013))
})

# ==============================================================================
# 6. Aggregation Tests
# ==============================================================================

test_that("fetch_enr returns all three entity types", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_enr(2024, tidy = FALSE, use_cache = FALSE)

  expect_true("type" %in% names(result))
  types <- unique(result$type)

  expect_true("State" %in% types, label = "Should have State rows")
  expect_true("District" %in% types, label = "Should have District rows")
  expect_true("Campus" %in% types, label = "Should have Campus rows")
})

test_that("State total approximately equals sum of districts", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  # Get state total
  state_row <- result[result$type == "State", ]
  if ("row_total" %in% names(state_row) && !is.na(state_row$row_total)) {
    state_total <- state_row$row_total

    # Get district totals
    districts <- result[result$type == "District", ]
    if ("row_total" %in% names(districts)) {
      district_sum <- sum(districts$row_total, na.rm = TRUE)

      # State should approximately equal sum of districts (within 1%)
      expect_true(abs(state_total - district_sum) / state_total < 0.01,
                  label = paste("State:", state_total, "District sum:", district_sum))
    }
  }
})

test_that("Number of unique districts is between 67-80", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  districts <- result[result$type == "District", ]
  n_unique <- length(unique(districts$district_id))

  expect_true(n_unique >= 67 && n_unique <= 80,
              label = paste("Found", n_unique, "unique districts"))
})

# ==============================================================================
# 7. Data Quality Tests
# ==============================================================================

test_that("No Inf or NaN in numeric columns", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  for (col in names(result)) {
    if (is.numeric(result[[col]])) {
      expect_false(any(is.infinite(result[[col]]), na.rm = TRUE),
                   label = paste("Inf found in column:", col))
      expect_false(any(is.nan(result[[col]]), na.rm = TRUE),
                   label = paste("NaN found in column:", col))
    }
  }
})

test_that("Demographics sum approximately to total", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  # Check state row
  state_row <- result[result$type == "State", ]

  demo_cols <- c("white", "black", "hispanic", "asian",
                 "pacific_islander", "native_american", "multiracial")
  demo_cols <- demo_cols[demo_cols %in% names(state_row)]

  if (length(demo_cols) > 0 && "row_total" %in% names(state_row)) {
    demo_sum <- sum(unlist(state_row[demo_cols]), na.rm = TRUE)
    total <- state_row$row_total

    # Demographics should be within 5% of total (some suppression expected)
    if (!is.na(total) && total > 0) {
      ratio <- demo_sum / total
      expect_true(ratio > 0.90 && ratio <= 1.05,
                  label = paste("Demo sum:", demo_sum, "Total:", total, "Ratio:", ratio))
    }
  }
})

test_that("District IDs are valid 2-digit strings", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  districts <- result[result$type == "District", ]
  valid_ids <- !is.na(districts$district_id) & nchar(districts$district_id) == 2

  expect_true(all(valid_ids),
              label = paste("Invalid district IDs found"))
})

test_that("Campus IDs follow DD-SSSS format", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  campuses <- result[result$type == "Campus" & !is.na(result$campus_id), ]

  if (nrow(campuses) > 0) {
    valid_format <- grepl("^\\d{2}-\\d{4}$", campuses$campus_id)
    expect_true(sum(valid_format) / nrow(campuses) > 0.95,
                label = paste("Most campus IDs should be DD-SSSS format"))
  }
})

# ==============================================================================
# 8. Output Fidelity Tests
# ==============================================================================

test_that("2024 state enrollment matches expected range", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  state_row <- result[result$type == "State", ]

  if ("row_total" %in% names(state_row) && !is.na(state_row$row_total)) {
    # Florida has ~2.8-3.2 million students
    total <- state_row$row_total
    expect_true(total > 2500000 && total < 3500000,
                label = paste("State total:", total))
  }
})

test_that("tidy format has expected columns", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expected_cols <- c("end_year", "type", "subgroup", "n_students",
                     "is_state", "is_district", "is_campus")

  for (col in expected_cols) {
    expect_true(col %in% names(result),
                label = paste("Missing column:", col))
  }
})

test_that("tidy format includes expected subgroups", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  subgroups <- unique(result$subgroup)

  expected_subgroups <- c("total_enrollment", "white", "black", "hispanic")

  for (sg in expected_subgroups) {
    expect_true(sg %in% subgroups,
                label = paste("Missing subgroup:", sg))
  }
})

test_that("Raw data matches processed totals", {
  skip_on_cran()
  skip_if_offline()

  # Get raw data
  raw <- get_raw_enr(2024)

  # Get processed data
  processed <- fetch_enr(2024, tidy = FALSE, use_cache = FALSE)

  # Campus counts: raw has multiple rows per school (one per grade)
  # Processed aggregates to one row per school
  # So processed rows should be roughly raw rows / average grades per school
  raw_campus_rows <- nrow(raw$campus)
  processed_campus_rows <- nrow(processed[processed$type == "Campus", ])

  # For Florida, not all schools have all grade levels
  # Elementary schools (K-5), middle schools (6-8), high schools (9-12), etc.
  # Average is around 6 grade levels per school
  ratio <- raw_campus_rows / processed_campus_rows
  expect_true(ratio > 3 && ratio < 15,
              label = paste("Ratio raw/processed:", round(ratio, 1),
                            "Raw:", raw_campus_rows, "Processed:", processed_campus_rows))
})

# ==============================================================================
# Multi-year consistency tests
# ==============================================================================

test_that("Multiple years can be fetched together", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_enr_multi(2022:2024, tidy = FALSE, use_cache = TRUE)

  expect_true("end_year" %in% names(result))
  years <- unique(result$end_year)

  expect_true(all(2022:2024 %in% years))
})

test_that("Year-over-year enrollment change is reasonable", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_enr_multi(2022:2024, tidy = FALSE, use_cache = TRUE)

  # Get state totals by year
  state_rows <- result[result$type == "State", ]

  if ("row_total" %in% names(state_rows) && nrow(state_rows) >= 2) {
    totals <- state_rows$row_total
    years <- state_rows$end_year

    # Year-over-year change should be < 10%
    for (i in 2:length(totals)) {
      if (!is.na(totals[i]) && !is.na(totals[i-1]) && totals[i-1] > 0) {
        change <- abs(totals[i] / totals[i-1] - 1)
        expect_true(change < 0.10,
                    label = paste("YoY change from", years[i-1], "to", years[i], ":", round(change*100, 1), "%"))
      }
    }
  }
})
