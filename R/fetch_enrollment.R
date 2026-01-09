# ==============================================================================
# Enrollment Data Fetching Functions
# ==============================================================================
#
# This file contains the main user-facing functions for downloading enrollment
# data from the Florida Department of Education (FLDOE) website.
#
# ==============================================================================

#' Fetch Florida enrollment data
#'
#' Downloads and processes enrollment data from the Florida Department of
#' Education (FLDOE). Returns school membership data including breakdowns
#' by grade level and race/ethnicity.
#'
#' @param end_year A school year. Year is the end of the academic year - eg 2023-24
#'   school year is year '2024'. Valid values are 2008-2025.
#'   - 2014-2025: Full demographic and grade-level data from membership files
#'   - 2008-2013: District-level FTE data only (limited demographics)
#' @param tidy If TRUE (default), returns data in long (tidy) format with subgroup
#'   column. If FALSE, returns wide format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from FLDOE.
#' @return Data frame with enrollment data. Wide format includes columns for
#'   district_id, campus_id, names, and enrollment counts by demographic/grade.
#'   Tidy format pivots these counts into subgroup and grade_level columns.
#' @export
#' @examples
#' \dontrun{
#' # Get 2024 enrollment data (2023-24 school year)
#' enr_2024 <- fetch_enr(2024)
#'
#' # Get wide format
#' enr_wide <- fetch_enr(2024, tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' enr_fresh <- fetch_enr(2024, use_cache = FALSE)
#'
#' # Filter to Miami-Dade County (district 13)
#' miami_dade <- enr_2024 |>
#'   dplyr::filter(district_id == "13")
#'
#' # Get state totals
#' state_totals <- enr_2024 |>
#'   dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")
#' }
fetch_enr <- function(end_year, tidy = TRUE, use_cache = TRUE) {

  # Validate year
  if (end_year < 2008 || end_year > 2025) {
    stop("end_year must be between 2008 and 2025")
  }

  # Warn about limited data for older years

if (end_year < 2014) {
    message("Note: Years 2008-2013 have limited demographic data (FTE files only).")
  }

  # Determine cache key
  cache_key <- paste0("enr_", if (tidy) "tidy" else "wide", "_", end_year)

  # Check cache first
  if (use_cache && cache_exists(cache_key)) {
    message(paste("Using cached data for", end_year))
    return(cache_read(cache_key))
  }

  # Get raw data from FLDOE
  raw <- get_raw_enr(end_year)

  # Process to standard schema
  processed <- process_enr(raw, end_year)

  # Optionally tidy
  if (tidy) {
    processed <- tidy_enr(processed) |>
      id_enr_aggs()
  }

  # Cache the result
  if (use_cache) {
    cache_write(cache_key, processed)
  }

  processed
}


#' Fetch enrollment data for multiple years
#'
#' Downloads and combines enrollment data for multiple school years.
#'
#' @param end_years Vector of school year ends (e.g., c(2022, 2023, 2024))
#' @param tidy If TRUE (default), returns data in long (tidy) format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#' @return Combined data frame with enrollment data for all requested years
#' @export
#' @examples
#' \dontrun{
#' # Get 3 years of data
#' enr_multi <- fetch_enr_multi(2022:2024)
#'
#' # Track enrollment trends
#' enr_multi |>
#'   dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
#'   dplyr::select(end_year, n_students)
#'
#' # Compare Hispanic enrollment growth across years
#' enr_multi |>
#'   dplyr::filter(is_state, subgroup == "hispanic", grade_level == "TOTAL") |>
#'   dplyr::select(end_year, n_students, pct)
#' }
fetch_enr_multi <- function(end_years, tidy = TRUE, use_cache = TRUE) {

  # Validate years
  invalid_years <- end_years[end_years < 2008 | end_years > 2025]
  if (length(invalid_years) > 0) {
    stop(paste("Invalid years:", paste(invalid_years, collapse = ", "),
               "\nend_year must be between 2008 and 2025"))
  }

  # Fetch each year
  results <- purrr::map(
    end_years,
    function(yr) {
      message(paste("Fetching", yr, "..."))
      fetch_enr(yr, tidy = tidy, use_cache = use_cache)
    }
  )

  # Combine
  dplyr::bind_rows(results)
}
