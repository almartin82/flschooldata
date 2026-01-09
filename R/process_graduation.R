# ==============================================================================
# Process Graduation Rate Data - Convert raw Excel to standard format
# ==============================================================================
#
# This file contains functions for processing raw graduation rate data from
# FLDOE Excel format into a standardized intermediate format.
#
# Processing steps:
#   1. Combine district and school sheets
#   2. Extract ID fields (district_id, school_id)
#   3. Convert string columns to numeric
#   4. Handle NA and suppressed values
#   5. Reshape from wide to long format (one row per subgroup)
#   6. Add level categorization (state/district/school)
#   7. Add end_year
#
# ==============================================================================

#' Process raw graduation data
#'
#' Converts raw FLDOE Excel data to a standardized intermediate format.
#' Handles type conversions, ID extraction, and subgroup reshaping.
#'
#' @param raw_data List with $district and $school data frames from get_raw_graduation()
#' @param end_year School year end
#' @return Data frame with processed graduation data
#' @keywords internal
process_graduation <- function(raw_data, end_year) {

  # Process district sheet
  district <- raw_data$district
  district <- process_sheet(district, "District", end_year)

  # Process school sheet
  school <- raw_data$school
  school <- process_sheet(school, "School", end_year)

  # Combine
  processed <- dplyr::bind_rows(district, school)

  processed
}

#' Process a single sheet (district or school)
#'
#' @param sheet_data Data frame from Excel sheet
#' @param level Either "District" or "School"
#' @param end_year School year end
#' @return Data frame with processed data
#' @keywords internal
process_sheet <- function(sheet_data, level, end_year) {

  # Standardize column names (remove leading/trailing spaces)
  names(sheet_data) <- trimws(names(sheet_data))

  # Extract identifiers
  # District Number: 2-digit code (preserve as character for leading zeros)
  # School Number: 4-digit code (blank for district sheet)
  district_id <- as.character(sheet_data$`District Number`)
  school_id <- as.character(sheet_data$`School Number`)

  # For district level, school_id should be NA
  if (level == "District") {
    school_id <- NA_character_
  }

  # Extract names
  district_name <- sheet_data$`District Name`
  school_name <- sheet_data$`School Name`

  # Extract total cohort data
  n_graduates_total <- as.numeric(sheet_data$`Total Graduates`)
  cohort_count_total <- as.numeric(sheet_data$`Total Cohort`)
  grad_rate_total <- as.numeric(sheet_data$`Total Federal Graduation Rate`)

  # Extract ESE (Exceptional Student Education)
  n_graduates_ese <- as.numeric(sheet_data$`ESE Graduates`)
  cohort_count_ese <- as.numeric(sheet_data$`ESE Cohort`)
  grad_rate_ese <- as.numeric(sheet_data$`ESE Graduation Rate`)

  # Extract Free/Reduced Lunch
  n_graduates_frl <- as.numeric(sheet_data$`Free/Reduced Lunch Graduates`)
  cohort_count_frl <- as.numeric(sheet_data$`Free/Reduced Lunch Cohort`)
  grad_rate_frl <- as.numeric(sheet_data$`Free/Reduced Lunch Graduation Rate`)

  # Extract ELL (English Language Learners)
  n_graduates_ell <- as.numeric(sheet_data$`ELL Graduates`)
  cohort_count_ell <- as.numeric(sheet_data$`ELL Cohort`)
  grad_rate_ell <- as.numeric(sheet_data$`ELL Graduation Rate`)

  # Extract Migrant
  n_graduates_migrant <- as.numeric(sheet_data$`Migrant Graduates`)
  cohort_count_migrant <- as.numeric(sheet_data$`Migrant Cohort`)
  grad_rate_migrant <- as.numeric(sheet_data$`Migrant Graduation Rate`)

  # Extract At-Risk
  n_graduates_atrisk <- as.numeric(sheet_data$`At-Risk Graduates`)
  cohort_count_atrisk <- as.numeric(sheet_data$`At-Risk Cohort`)
  grad_rate_atrisk <- as.numeric(sheet_data$`At-Risk Graduation Rate`)

  # Extract Male
  n_graduates_male <- as.numeric(sheet_data$`Male Graduates`)
  cohort_count_male <- as.numeric(sheet_data$`Male Cohort`)
  grad_rate_male <- as.numeric(sheet_data$`Male Graduation Rate`)

  # Extract Female
  n_graduates_female <- as.numeric(sheet_data$`Female Graduates`)
  cohort_count_female <- as.numeric(sheet_data$`Female Cohort`)
  grad_rate_female <- as.numeric(sheet_data$`Female Graduation Rate`)

  # Replicate rows for each entity in the sheet
  n_entities <- nrow(sheet_data)

  # This is a wide-to-long conversion for each entity
  result <- data.frame()

  for (i in 1:n_entities) {
    entity_data <- dplyr::tibble(
      end_year = end_year,
      type = level,
      district_id = district_id[i],
      school_id = school_id[i],
      district_name = district_name[i],
      school_name = school_name[i],
      subgroup = c("all", "ese", "frl", "ell", "migrant", "atrisk", "male", "female"),
      n_graduates = c(
        n_graduates_total[i],
        n_graduates_ese[i],
        n_graduates_frl[i],
        n_graduates_ell[i],
        n_graduates_migrant[i],
        n_graduates_atrisk[i],
        n_graduates_male[i],
        n_graduates_female[i]
      ),
      grad_rate = c(
        grad_rate_total[i],
        grad_rate_ese[i],
        grad_rate_frl[i],
        grad_rate_ell[i],
        grad_rate_migrant[i],
        grad_rate_atrisk[i],
        grad_rate_male[i],
        grad_rate_female[i]
      ),
      cohort_count = c(
        cohort_count_total[i],
        cohort_count_ese[i],
        cohort_count_frl[i],
        cohort_count_ell[i],
        cohort_count_migrant[i],
        cohort_count_atrisk[i],
        cohort_count_male[i],
        cohort_count_female[i]
      )
    )

    result <- dplyr::bind_rows(result, entity_data)
  }

  # For state total (district_id is "0" in district sheet)
  # Set type to "State" and district_id to "ALL" for state record
  result$type[result$district_id == "0" & result$type == "District"] <- "State"
  result$district_id[result$district_id == "0"] <- "ALL"
  result$school_id[is.na(result$school_id)] <- "ALL"

  # Clean up district_id and school_id (remove leading/trailing whitespace)
  result$district_id <- trimws(result$district_id)
  result$school_id <- trimws(result$school_id)

  result
}

#' Validate processed graduation data
#'
#' Performs data quality checks on processed graduation data.
#'
#' @param data Processed data frame from process_graduation()
#' @return Invisible TRUE if validation passes, otherwise throws error
#' @keywords internal
validate_graduation <- function(data) {

  # Check for required columns
  required_cols <- c("end_year", "type", "district_id", "school_id",
                     "subgroup", "n_graduates", "grad_rate", "cohort_count")

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check graduation rate range (FL uses 0-100, not 0-1)
  if (any(data$grad_rate < 0 | data$grad_rate > 100, na.rm = TRUE)) {
    warning("Some graduation rates are outside 0-100 range (FL reports percentages)")
  }

  # Check for negative cohort sizes
  if (any(data$cohort_count < 0, na.rm = TRUE)) {
    stop("Cohort sizes cannot be negative")
  }

  # Check for negative graduate counts
  if (any(data$n_graduates < 0, na.rm = TRUE)) {
    stop("Graduate counts cannot be negative")
  }

  # Check that n_graduates <= cohort_size (with tolerance for rounding)
  if (any(data$n_graduates > data$cohort_count, na.rm = TRUE)) {
    warning("Some records have more graduates than cohort size (possible data error)")
  }

  invisible(TRUE)
}
