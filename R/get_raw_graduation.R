# ==============================================================================
# Raw Graduation Rate Data Download Functions - FLDOE Excel
# ==============================================================================
#
# This file contains functions for downloading raw graduation rate data from the
# Florida Department of Education (FLDOE) website.
#
# Data source: https://www.fldoe.org/core/fileparse.php/7584/urlt/
# Dataset: Federal Graduation Rate by Category
# Years available: 2015-2023 (2015-16 through 2023-24 school years)
#
# ==============================================================================

#' Base URL for FLDOE graduation rate files
#' @keywords internal
FLDOE_GRAD_BASE_URL <- "https://www.fldoe.org/core/fileparse.php/7584/urlt"

#' Get available graduation years
#'
#' Returns a vector of years for which graduation rate data is available
#' from the Florida Department of Education.
#'
#' @return Integer vector of years (2016, 2017, ..., 2024)
#' @export
#' @examples
#' \dontrun{
#' get_available_grad_years()
#' # Returns: 2016 2017 2018 2019 2020 2021 2022 2023 2024
#' }
get_available_grad_years <- function() {
  # Available years: 2015-16 through 2023-24 (9 years)
  # end_year represents the school year end (e.g., 2024 for 2023-24)
  c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)
}

#' Build FLDOE graduation rate URL for a given year
#'
#' Constructs the download URL for graduation rate Excel file.
#' Note: Florida has an irregular year suffix pattern.
#'
#' @param end_year School year end (2023-24 = 2024)
#' @return Character string with full URL to Excel file
#' @keywords internal
build_graduation_url <- function(end_year) {

  # Year suffix mapping (irregular pattern!)
  # 2023-24 -> "2324"
  # 2022-23 -> "2223"
  # 2021-22 -> "2122"
  # 2020-21 -> "2021"  (different pattern!)
  # 2019-20 -> "1920"
  # 2018-19 -> "1819"
  # 2017-18 -> "1718"
  # 2016-17 -> "1617"
  # 2015-16 -> "1516"

  suffixes <- list(
    "2024" = "2324",
    "2023" = "2223",
    "2022" = "2122",
    "2021" = "2021",
    "2020" = "1920",
    "2019" = "1819",
    "2018" = "1718",
    "2017" = "1617",
    "2016" = "1516"
  )

  year_str <- as.character(end_year)

  if (!year_str %in% names(suffixes)) {
    stop("end_year must be between 2016 and 2024")
  }

  suffix <- suffixes[[year_str]]
  paste0(FLDOE_GRAD_BASE_URL, "/FedGradRateCategory", suffix, ".xls")
}

#' Download raw graduation data from FLDOE
#'
#' Downloads graduation rate data from the Florida Department of Education
#' as an Excel file with two sheets (school and district levels).
#'
#' @param end_year School year end (2023-24 = 2024). Valid years: 2016-2024.
#' @return List with two data frames: $school and $district
#' @export
#' @examples
#' \dontrun{
#' # Download 2024 graduation data
#' raw <- get_raw_graduation(2024)
#'
#' # Access school-level data
#' school_data <- raw$school
#'
#' # Access district-level data
#' district_data <- raw$district
#' }
get_raw_graduation <- function(end_year) {

  # Validate year
  available_years <- get_available_grad_years()
  if (!end_year %in% available_years) {
    stop("end_year must be one of: ", paste(available_years, collapse = ", "),
         "\nRun get_available_grad_years() to see available years.")
  }

  message(paste("Downloading FLDOE graduation data for", end_year, "..."))

  # Build URL
  url <- build_graduation_url(end_year)

  # Download file
  temp_file <- tempfile(fileext = ".xls")

  response <- tryCatch({
    httr::GET(
      url,
      httr::timeout(120),
      httr::write_disk(temp_file, overwrite = TRUE)
    )
  }, error = function(e) {
    stop("Failed to connect to FLDOE: ", e$message)
  })

  if (httr::http_error(response)) {
    stop(paste("HTTP error:", httr::status_code(response),
               "\nFailed to download graduation data for year", end_year))
  }

  # Read both sheets (skip 4 header rows - multi-row header)
  raw_district <- tryCatch({
    readxl::read_excel(
      temp_file,
      sheet = "Fed GradRate by Category-Dist",
      skip = 4
    )
  }, error = function(e) {
    stop("Failed to parse district sheet: ", e$message)
  })

  raw_school <- tryCatch({
    readxl::read_excel(
      temp_file,
      sheet = "Fed GradRate by Category-School",
      skip = 4
    )
  }, error = function(e) {
    stop("Failed to parse school sheet: ", e$message)
  })

  message(paste("  Downloaded", nrow(raw_district), "district records"))
  message(paste("  Downloaded", nrow(raw_school), "school records"))

  # Return as list
  list(
    district = raw_district,
    school = raw_school
  )
}
