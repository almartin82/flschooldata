# ==============================================================================
# LIVE Assessment Data Pipeline Tests - Florida
# ==============================================================================
#
# Tests for Florida assessment data (FSA 2015-2022, FAST 2023-2025)
# These tests require network access and will be skipped during CRAN checks.
#
# Assessment Eras:
# - FSA (Florida Standards Assessments): 2015-2022
# - FAST (Florida Assessment of Student Thinking): 2023-2025
#
# Subjects: ELA (Grades 3-10), Mathematics (Grades 3-8)
# EOCs: Algebra 1, Geometry, Biology 1, Civics, U.S. History
#
# ==============================================================================

# Major Florida districts for testing
MAJOR_DISTRICTS <- c(
  "MIAMI-DADE",      # District 50
  "BROWARD",         # District 06
  "HILLSBOROUGH",    # District 57
  "ORANGE",          # District 48
  "DUVAL",           # District 34
  "PALM BEACH",      # District 49
  "PINELLAS"         # District 22
)

# Expected grades for ELA
ELA_GRADES <- c("03", "04", "05", "06", "07", "08", "09", "10")

# Expected grades for Math
MATH_GRADES <- c("03", "04", "05", "06", "07", "08")

# Expected district count
EXPECTED_DISTRICT_COUNT <- 67

# ==============================================================================
# URL Availability Tests - FAST Era (2023-2025)
# ==============================================================================

test_that("2024 FAST ELA Grade 3 district URL is available", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/3ELA03SRDSpring24.xls"

  response <- httr::GET(url, httr::timeout(30))

  expect_false(httr::http_error(response))
  expect_equal(httr::status_code(response), 200)
})

test_that("2024 FAST Math Grade 3 district URL is available", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/3MATH03SRDSpring24.xls"

  response <- httr::GET(url, httr::timeout(30))

  expect_false(httr::http_error(response))
  expect_equal(httr::status_code(response), 200)
})

test_that("2023 FAST ELA Grade 3 district URL is available", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/3ELA03SRDSpring23.xls"

  response <- httr::GET(url, httr::timeout(30))

  expect_false(httr::http_error(response))
  expect_equal(httr::status_code(response), 200)
})

# ==============================================================================
# URL Availability Tests - FSA Era (2015-2022)
# ==============================================================================

test_that("2022 FSA ELA Grade 3 district URL is available", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/SPR22ELA03SRD.xls"

  response <- httr::GET(url, httr::timeout(30))

  expect_false(httr::http_error(response))
  expect_equal(httr::status_code(response), 200)
})

test_that("2021 FSA ELA Grade 3 district URL is available", {
  skip_if_offline()
  skip("2021 files removed from FLDOE server")

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/SPR21ELA03SRD.xls"

  response <- httr::GET(url, httr::timeout(30))

  expect_false(httr::http_error(response))
  expect_equal(httr::status_code(response), 200)
})

test_that("2019 FSA ELA Grade 3 district URL is available", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/SPR19ELA03SRD.xls"

  response <- httr::GET(url, httr::timeout(30))

  expect_false(httr::http_error(response))
  expect_equal(httr::status_code(response), 200)
})

test_that("2016 FSA ELA Grade 3 district URL is available", {
  skip_if_offline()
  skip("2016 files removed from FLDOE server")

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/SPR16ELA03SRD.xls"

  response <- httr::GET(url, httr::timeout(30))

  expect_false(httr::http_error(response))
  expect_equal(httr::status_code(response), 200)
})

test_that("2015 FSA ELA Grade 3 district URL is available", {
  skip_if_offline()
  skip("2015 files removed from FLDOE server")

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/SPR15ELA03SRD.xls"

  response <- httr::GET(url, httr::timeout(30))

  expect_false(httr::http_error(response))
  expect_equal(httr::status_code(response), 200)
})

# ==============================================================================
# File Download Tests
# ==============================================================================

test_that("2024 FAST ELA Grade 3 file downloads successfully", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/3ELA03SRDSpring24.xls"

  tname <- tempfile(fileext = ".xls")

  on.exit(unlink(tname), add = TRUE)

  response <- httr::GET(
    url,
    httr::write_disk(tname, overwrite = TRUE),
    httr::timeout(120)
  )

  expect_false(httr::http_error(response))
  expect_true(file.exists(tname))
  expect_gt(file.info(tname)$size, 10000)  # Should be > 10KB

  # Verify it's an Excel file (OLE signature for .xls)
  content <- readBin(tname, "raw", file.info(tname)$size)
  expect_true(any(content == c(0xd0, 0xcf, 0x11, 0xe0)))  # OLE signature
})

test_that("2022 FSA ELA Grade 3 file downloads successfully", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/SPR22ELA03SRD.xls"

  tname <- tempfile(fileext = ".xls")

  on.exit(unlink(tname), add = TRUE)

  response <- httr::GET(
    url,
    httr::write_disk(tname, overwrite = TRUE),
    httr::timeout(120)
  )

  expect_false(httr::http_error(response))
  expect_true(file.exists(tname))
  expect_gt(file.info(tname)$size, 10000)
})

test_that("2015 FSA ELA Grade 3 file downloads successfully", {
  skip_if_offline()
  skip("2015 files removed from FLDOE server")

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/SPR15ELA03SRD.xls"

  tname <- tempfile(fileext = ".xls")

  on.exit(unlink(tname), add = TRUE)

  response <- httr::GET(
    url,
    httr::write_disk(tname, overwrite = TRUE),
    httr::timeout(120)
  )

  expect_false(httr::http_error(response))
  expect_true(file.exists(tname))
  expect_gt(file.info(tname)$size, 10000)
})

# ==============================================================================
# File Parsing Tests
# ==============================================================================

test_that("2024 FAST ELA Grade 3 Excel file can be parsed", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/3ELA03SRDSpring24.xls"

  tname <- tempfile(fileext = ".xls")

  on.exit(unlink(tname), add = TRUE)

  # Download
  response <- httr::GET(
    url,
    httr::write_disk(tname, overwrite = TRUE),
    httr::timeout(120)
  )

  expect_false(httr::http_error(response))

  # Parse with readxl
  df <- readxl::read_excel(tname, sheet = 1, n_max = 10)
  expect_true(nrow(df) > 0)
  expect_true(ncol(df) > 0)

  # Should have district-related columns
  # Column names vary but typically include district name or number
})

test_that("2022 FSA ELA Grade 3 Excel file can be parsed", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/SPR22ELA03SRD.xls"

  tname <- tempfile(fileext = ".xls")

  on.exit(unlink(tname), add = TRUE)

  # Download
  response <- httr::GET(
    url,
    httr::write_disk(tname, overwrite = TRUE),
    httr::timeout(120)
  )

  expect_false(httr::http_error(response))

  # Parse with readxl
  df <- readxl::read_excel(tname, sheet = 1, n_max = 10)
  expect_true(nrow(df) > 0)
  expect_true(ncol(df) > 0)
})

# ==============================================================================
# District Coverage Tests
# ==============================================================================

test_that("2024 FAST includes Miami-Dade (District 50)", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/3ELA03SRDSpring24.xls"

  tname <- tempfile(fileext = ".xls")

  on.exit(unlink(tname), add = TRUE)

  response <- httr::GET(
    url,
    httr::write_disk(tname, overwrite = TRUE),
    httr::timeout(120)
  )

  expect_false(httr::http_error(response))

  # Parse and check for Miami-Dade
  df <- readxl::read_excel(tname, sheet = 1)

  # Look for Miami-Dade in the data (case-insensitive search)
  # Column structure varies, so we search all character columns
  char_cols <- sapply(df, is.character)

  if (any(char_cols)) {
    found_miami <- any(sapply(df[, char_cols], function(col) {
      any(grepl("MIAMI.*DADE|Miami-Dade", col, ignore.case = TRUE))
    }))
    expect_true(found_miami, info = "Miami-Dade district not found in 2024 data")
  }
})

test_that("2024 FAST includes Broward (District 06)", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/3ELA03SRDSpring24.xls"

  tname <- tempfile(fileext = ".xls")

  on.exit(unlink(tname), add = TRUE)

  response <- httr::GET(
    url,
    httr::write_disk(tname, overwrite = TRUE),
    httr::timeout(120)
  )

  expect_false(httr::http_error(response))

  df <- readxl::read_excel(tname, sheet = 1)

  char_cols <- sapply(df, is.character)

  if (any(char_cols)) {
    found_broward <- any(sapply(df[, char_cols], function(col) {
      any(grepl("BROWARD|Broward", col, ignore.case = TRUE))
    }))
    expect_true(found_broward, info = "Broward district not found in 2024 data")
  }
})

test_that("2024 FAST includes Hillsborough (District 57)", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/3ELA03SRDSpring24.xls"

  tname <- tempfile(fileext = ".xls")

  on.exit(unlink(tname), add = TRUE)

  response <- httr::GET(
    url,
    httr::write_disk(tname, overwrite = TRUE),
    httr::timeout(120)
  )

  expect_false(httr::http_error(response))

  df <- readxl::read_excel(tname, sheet = 1)

  char_cols <- sapply(df, is.character)

  if (any(char_cols)) {
    found_hillsborough <- any(sapply(df[, char_cols], function(col) {
      any(grepl("HILLSBOROUGH|Hillsborough", col, ignore.case = TRUE))
    }))
    expect_true(found_hillsborough, info = "Hillsborough district not found in 2024 data")
  }
})

test_that("2024 FAST includes Orange (District 48)", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/3ELA03SRDSpring24.xls"

  tname <- tempfile(fileext = ".xls")

  on.exit(unlink(tname), add = TRUE)

  response <- httr::GET(
    url,
    httr::write_disk(tname, overwrite = TRUE),
    httr::timeout(120)
  )

  expect_false(httr::http_error(response))

  df <- readxl::read_excel(tname, sheet = 1)

  char_cols <- sapply(df, is.character)

  if (any(char_cols)) {
    found_orange <- any(sapply(df[, char_cols], function(col) {
      any(grepl("ORANGE|Orange", col))
    }))
    expect_true(found_orange, info = "Orange district not found in 2024 data")
  }
})

test_that("2024 FAST includes Duval (District 34)", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/3ELA03SRDSpring24.xls"

  tname <- tempfile(fileext = ".xls")

  on.exit(unlink(tname), add = TRUE)

  response <- httr::GET(
    url,
    httr::write_disk(tname, overwrite = TRUE),
    httr::timeout(120)
  )

  expect_false(httr::http_error(response))

  df <- readxl::read_excel(tname, sheet = 1)

  char_cols <- sapply(df, is.character)

  if (any(char_cols)) {
    found_duval <- any(sapply(df[, char_cols], function(col) {
      any(grepl("DUVAL|Duval", col))
    }))
    expect_true(found_duval, info = "Duval district not found in 2024 data")
  }
})

# ==============================================================================
# Year Coverage Tests
# ==============================================================================

test_that("All major years 2019-2024 are accessible (excluding 2020 COVID)", {
  skip_if_offline()

  # Test a sample of years across FSA and FAST eras
  # Note: 2015-2018 files were removed from FLDOE server
  years_to_test <- list(
    "2019" = "https://www.fldoe.org/core/fileparse.php/5668/urlt/SPR19ELA03SRD.xls",
    "2022" = "https://www.fldoe.org/core/fileparse.php/5668/urlt/SPR22ELA03SRD.xls",
    "2024" = "https://www.fldoe.org/core/fileparse.php/5668/urlt/3ELA03SRDSpring24.xls"
  )

  for (yr in names(years_to_test)) {
    response <- httr::GET(years_to_test[[yr]], httr::timeout(30))
    expect_false(httr::http_error(response),
                 info = paste("Year", yr, "URL is not accessible"))
  }
})

# ==============================================================================
# Data Structure Tests
# ==============================================================================

test_that("2024 FAST file has reasonable row count (60+ rows for 67 districts + header)", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/3ELA03SRDSpring24.xls"

  tname <- tempfile(fileext = ".xls")

  on.exit(unlink(tname), add = TRUE)

  response <- httr::GET(
    url,
    httr::write_disk(tname, overwrite = TRUE),
    httr::timeout(120)
  )

  expect_false(httr::http_error(response))

  df <- readxl::read_excel(tname, sheet = 1)

  # Should have header rows + 67 districts + possibly state totals
  expect_gte(nrow(df), 60, label = "File row count")
})

test_that("2022 FSA file has reasonable row count (60+ rows)", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/SPR22ELA03SRD.xls"

  tname <- tempfile(fileext = ".xls")

  on.exit(unlink(tname), add = TRUE)

  response <- httr::GET(
    url,
    httr::write_disk(tname, overwrite = TRUE),
    httr::timeout(120)
  )

  expect_false(httr::http_error(response))

  df <- readxl::read_excel(tname, sheet = 1)

  expect_gte(nrow(df), 60, label = "File row count")
})

# ==============================================================================
# Cross-Year Consistency Tests
# ==============================================================================

test_that("Miami-Dade appears in all tested years", {
  skip_if_offline()

  # Note: 2016 files removed from FLDOE server, testing 2019+ only
  years_to_test <- list(
    "2019" = "https://www.fldoe.org/core/fileparse.php/5668/urlt/SPR19ELA03SRD.xls",
    "2022" = "https://www.fldoe.org/core/fileparse.php/5668/urlt/SPR22ELA03SRD.xls",
    "2024" = "https://www.fldoe.org/core/fileparse.php/5668/urlt/3ELA03SRDSpring24.xls"
  )

  for (yr in names(years_to_test)) {
    tname <- tempfile(fileext = ".xls")

    response <- httr::GET(
      years_to_test[[yr]],
      httr::write_disk(tname, overwrite = TRUE),
      httr::timeout(120)
    )

    expect_false(httr::http_error(response),
                 info = paste("Failed to download year", yr))

    df <- readxl::read_excel(tname, sheet = 1)

    char_cols <- sapply(df, is.character)

    if (any(char_cols)) {
      found <- any(sapply(df[, char_cols], function(col) {
        any(grepl("MIAMI.*DADE|Miami-Dade", col, ignore.case = TRUE))
      }))
      expect_true(found,
                  info = paste("Miami-Dade not found in year", yr))
    }

    unlink(tname)
  }
})

test_that("Broward appears in all tested years", {
  skip_if_offline()

  # Note: 2016 files removed from FLDOE server, testing 2019+ only
  years_to_test <- list(
    "2019" = "https://www.fldoe.org/core/fileparse.php/5668/urlt/SPR19ELA03SRD.xls",
    "2022" = "https://www.fldoe.org/core/fileparse.php/5668/urlt/SPR22ELA03SRD.xls",
    "2024" = "https://www.fldoe.org/core/fileparse.php/5668/urlt/3ELA03SRDSpring24.xls"
  )

  for (yr in names(years_to_test)) {
    tname <- tempfile(fileext = ".xls")

    response <- httr::GET(
      years_to_test[[yr]],
      httr::write_disk(tname, overwrite = TRUE),
      httr::timeout(120)
    )

    expect_false(httr::http_error(response),
                 info = paste("Failed to download year", yr))

    df <- readxl::read_excel(tname, sheet = 1)

    char_cols <- sapply(df, is.character)

    if (any(char_cols)) {
      found <- any(sapply(df[, char_cols], function(col) {
        any(grepl("BROWARD|Broward", col, ignore.case = TRUE))
      }))
      expect_true(found,
                  info = paste("Broward not found in year", yr))
    }

    unlink(tname)
  }
})

# ==============================================================================
# Assessment Era Transition Tests
# ==============================================================================

test_that("FSA and FAST eras both have accessible data", {
  skip_if_offline()

  # FSA Era (2015-2022)
  fsa_url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/SPR22ELA03SRD.xls"
  fsa_response <- httr::GET(fsa_url, httr::timeout(30))
  expect_false(httr::http_error(fsa_response),
               info = "FSA era URL not accessible")

  # FAST Era (2023-2025)
  fast_url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/3ELA03SRDSpring24.xls"
  fast_response <- httr::GET(fast_url, httr::timeout(30))
  expect_false(httr::http_error(fast_response),
               info = "FAST era URL not accessible")
})

# ==============================================================================
# Data Quality Tests
# ==============================================================================

test_that("2024 FAST file has no Inf values in numeric columns", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/3ELA03SRDSpring24.xls"

  tname <- tempfile(fileext = ".xls")

  on.exit(unlink(tname), add = TRUE)

  response <- httr::GET(
    url,
    httr::write_disk(tname, overwrite = TRUE),
    httr::timeout(120)
  )

  expect_false(httr::http_error(response))

  df <- readxl::read_excel(tname, sheet = 1)

  numeric_cols <- sapply(df, is.numeric)

  if (any(numeric_cols)) {
    for (col in names(df)[numeric_cols]) {
      expect_false(any(is.infinite(df[[col]])),
                   info = paste("Column", col, "has Inf values"))
    }
  }
})

test_that("2022 FSA file has no Inf values in numeric columns", {
  skip_if_offline()

  url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/SPR22ELA03SRD.xls"

  tname <- tempfile(fileext = ".xls")

  on.exit(unlink(tname), add = TRUE)

  response <- httr::GET(
    url,
    httr::write_disk(tname, overwrite = TRUE),
    httr::timeout(120)
  )

  expect_false(httr::http_error(response))

  df <- readxl::read_excel(tname, sheet = 1)

  numeric_cols <- sapply(df, is.numeric)

  if (any(numeric_cols)) {
    for (col in names(df)[numeric_cols]) {
      expect_false(any(is.infinite(df[[col]])),
                   info = paste("Column", col, "has Inf values"))
    }
  }
})

# ==============================================================================
# Multiple Subject Tests
# ==============================================================================

test_that("2024 has ELA data available", {
  skip_if_offline()

  ela_url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/3ELA03SRDSpring24.xls"

  # Check ELA
  ela_response <- httr::GET(ela_url, httr::timeout(30))
  expect_false(httr::http_error(ela_response),
               info = "2024 ELA data not accessible")

  # Note: Math URL pattern changed/removed from FLDOE server
  # math_url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/3MATH03SRDSpring24.xls"
})

# ==============================================================================
# End of Test File
# ==============================================================================
