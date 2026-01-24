# ==============================================================================
# Assessment Data Fetching Functions
# ==============================================================================
#
# This file contains the main user-facing functions for fetching Florida
# assessment data (FSA and FAST).
#
# Available Years: 2019, 2022, 2023, 2024, 2025
# (No 2020/2021 due to COVID-19)
#
# Assessment Systems:
# - FSA (Florida Standards Assessments): 2019-2022
# - FAST (Florida Assessment of Student Thinking): 2023-2025
#
# ==============================================================================

#' Fetch Florida assessment data
#'
#' Downloads and processes assessment data from the Florida Department of
#' Education. Returns FSA (2019-2022) or FAST (2023-2025) data.
#'
#' @param end_year School year end (2023-24 = 2024). Valid years: 2019, 2022-2025.
#' @param subject Subject area: "ela" (default) or "math"
#' @param grade Grade level (3-10 for ELA, 3-8 for Math). NULL fetches all grades.
#' @param level Aggregation level: "district" (default) or "school"
#' @param tidy If TRUE (default), returns data in long (tidy) format with
#'   proficiency_level column. If FALSE, returns wide format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#' @return Data frame with assessment data
#' @export
#' @examples
#' \dontrun{
#' # Get 2024 FAST ELA data for all grades (district level)
#' assess_2024 <- fetch_assessment(2024, "ela")
#'
#' # Get 2024 Math Grade 3 school-level data
#' math_g3 <- fetch_assessment(2024, "math", grade = 3, level = "school")
#'
#' # Get wide format (proficiency levels as columns)
#' assess_wide <- fetch_assessment(2024, "ela", tidy = FALSE)
#'
#' # Get 2019 FSA data
#' fsa_2019 <- fetch_assessment(2019, "ela")
#'
#' # Filter to Miami-Dade County
#' library(dplyr)
#' miami <- assess_2024 |>
#'   filter(district_id == "13")
#'
#' # Get state-level proficiency rates
#' state_prof <- assess_2024 |>
#'   filter(is_state, is_proficient) |>
#'   group_by(grade) |>
#'   summarize(pct_proficient = sum(pct))
#' }
fetch_assessment <- function(end_year, subject = "ela", grade = NULL,
                              level = "district", tidy = TRUE, use_cache = TRUE) {

  # Validate year
  available <- get_available_assessment_years()
  if (!end_year %in% available$years) {
    if (end_year %in% c(2020, 2021)) {
      stop(paste0(
        "Assessment data not available for ", end_year, ".\n",
        available$note
      ))
    }
    stop(paste0(
      "end_year must be one of: ", paste(available$years, collapse = ", "), "\n",
      "Got: ", end_year
    ))
  }

  # Validate subject
  subject <- tolower(subject)
  if (!subject %in% c("ela", "math")) {
    stop("subject must be 'ela' or 'math'")
  }

  # Validate level
  level <- tolower(level)
  if (!level %in% c("district", "school")) {
    stop("level must be 'district' or 'school'")
  }

  # Build cache key
  grade_str <- if (is.null(grade)) "all" else sprintf("g%02d", grade)
  cache_key <- paste0(
    "assess_", if (tidy) "tidy_" else "wide_",
    end_year, "_", subject, "_", grade_str, "_", level
  )

  # Check cache first
  if (use_cache && cache_exists(cache_key)) {
    message(paste("Using cached assessment data for", end_year))
    return(cache_read(cache_key))
  }

  # Get raw data
  raw <- get_raw_assessment(end_year, subject, grade, level)

  # Process to standard schema
  processed <- process_assessment(raw, end_year)

  # Optionally tidy
  if (tidy) {
    processed <- tidy_assessment(processed)
  }

  # Cache the result
  if (use_cache) {
    cache_write(cache_key, processed)
  }

  processed
}


#' Fetch assessment data for multiple years
#'
#' Downloads and combines assessment data for multiple school years.
#'
#' @param end_years Vector of school year ends (e.g., c(2022, 2023, 2024))
#' @param subject Subject area: "ela" (default) or "math"
#' @param grade Grade level (3-10 for ELA, 3-8 for Math). NULL fetches all grades.
#' @param level Aggregation level: "district" (default) or "school"
#' @param tidy If TRUE (default), returns data in long (tidy) format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#' @return Combined data frame with assessment data for all requested years
#' @export
#' @examples
#' \dontrun{
#' # Get 3 years of ELA data
#' assess_multi <- fetch_assessment_multi(c(2022, 2023, 2024), "ela")
#'
#' # Track proficiency trends at state level
#' library(dplyr)
#' assess_multi |>
#'   filter(is_state, is_proficient, grade == "03") |>
#'   group_by(end_year) |>
#'   summarize(pct_proficient = sum(pct))
#' }
fetch_assessment_multi <- function(end_years, subject = "ela", grade = NULL,
                                    level = "district", tidy = TRUE, use_cache = TRUE) {

  # Validate years
  available <- get_available_assessment_years()

  # Check for invalid years
  if (any(end_years %in% c(2020, 2021))) {
    warning("2020 and 2021 excluded: ", available$note)
    end_years <- end_years[!end_years %in% c(2020, 2021)]
  }

  invalid_years <- end_years[!end_years %in% available$years]
  if (length(invalid_years) > 0) {
    stop(paste0(
      "Invalid years: ", paste(invalid_years, collapse = ", "), "\n",
      "Valid years are: ", paste(available$years, collapse = ", ")
    ))
  }

  if (length(end_years) == 0) {
    stop("No valid years to fetch")
  }

  # Fetch each year
  results <- purrr::map(
    end_years,
    function(yr) {
      message(paste("Fetching", yr, "..."))
      tryCatch({
        fetch_assessment(yr, subject = subject, grade = grade,
                         level = level, tidy = tidy, use_cache = use_cache)
      }, error = function(e) {
        warning(paste("Failed to fetch year", yr, ":", e$message))
        data.frame()
      })
    }
  )

  # Combine, filtering out empty data frames
  results <- results[!sapply(results, function(x) nrow(x) == 0)]
  dplyr::bind_rows(results)
}


#' Fetch assessment data for a specific district
#'
#' Convenience function to fetch assessment data for a single district.
#'
#' @param end_year School year end
#' @param district_id 2-digit district ID (e.g., "13" for Miami-Dade)
#' @param subject Subject area: "ela" (default) or "math"
#' @param grade Grade level. NULL fetches all grades.
#' @param tidy If TRUE (default), returns tidy format
#' @param use_cache If TRUE (default), uses cached data
#' @return Data frame filtered to specified district
#' @export
#' @examples
#' \dontrun{
#' # Get Miami-Dade County (district 13) assessment data
#' miami_assess <- fetch_district_assessment(2024, "13")
#'
#' # Get Broward County (district 06) Math data
#' broward_math <- fetch_district_assessment(2024, "06", subject = "math")
#' }
fetch_district_assessment <- function(end_year, district_id, subject = "ela",
                                       grade = NULL, tidy = TRUE, use_cache = TRUE) {

  # Normalize district_id
  district_id <- sprintf("%02d", as.integer(district_id))

  # Fetch district-level data
  df <- fetch_assessment(end_year, subject = subject, grade = grade,
                          level = "district", tidy = tidy, use_cache = use_cache)

  # Filter to requested district
  df |>
    dplyr::filter(district_id == !!district_id)
}


#' Fetch assessment data for a specific school
#'
#' Convenience function to fetch assessment data for a single school.
#'
#' @param end_year School year end
#' @param district_id 2-digit district ID
#' @param school_id 4-digit school ID
#' @param subject Subject area: "ela" (default) or "math"
#' @param grade Grade level. NULL fetches all grades.
#' @param tidy If TRUE (default), returns tidy format
#' @param use_cache If TRUE (default), uses cached data
#' @return Data frame filtered to specified school
#' @export
#' @examples
#' \dontrun{
#' # Get a specific school's assessment data
#' school_assess <- fetch_school_assessment(2024, "13", "0021")
#' }
fetch_school_assessment <- function(end_year, district_id, school_id, subject = "ela",
                                     grade = NULL, tidy = TRUE, use_cache = TRUE) {

  # Normalize IDs
  district_id <- sprintf("%02d", as.integer(district_id))
  school_id <- sprintf("%04d", as.integer(school_id))

  # Fetch school-level data
  df <- fetch_assessment(end_year, subject = subject, grade = grade,
                          level = "school", tidy = tidy, use_cache = use_cache)

  # Filter to requested school
  df |>
    dplyr::filter(district_id == !!district_id, school_id == !!school_id)
}
