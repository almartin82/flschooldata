# ==============================================================================
# Assessment Data Processing Functions
# ==============================================================================
#
# This file contains functions for processing raw assessment data into a
# standardized schema.
#
# ==============================================================================

#' Process raw assessment data
#'
#' Cleans and standardizes raw assessment data from FLDOE.
#'
#' @param raw_df Raw data frame from get_raw_assessment()
#' @param end_year School year end
#' @return Processed data frame with standardized schema
#' @export
#' @examples
#' \dontrun{
#' raw <- get_raw_assessment(2024, "ela", 3)
#' processed <- process_assessment(raw, 2024)
#' }
process_assessment <- function(raw_df, end_year) {

  if (nrow(raw_df) == 0) {
    return(raw_df)
  }

  # Remove any completely empty rows
  raw_df <- raw_df[rowSums(!is.na(raw_df)) > 0, ]

  # Convert numeric columns
  numeric_cols <- c("n_tested", "mean_scale_score", "pct_proficient",
                    "pct_level_1", "pct_level_2", "pct_level_3",
                    "pct_level_4", "pct_level_5")

  for (col in numeric_cols) {
    if (col %in% names(raw_df)) {
      raw_df[[col]] <- safe_numeric(raw_df[[col]])
    }
  }

  # Ensure required ID columns exist
  if (!"district_id" %in% names(raw_df)) {
    raw_df$district_id <- NA_character_
  }
  if (!"district_name" %in% names(raw_df)) {
    raw_df$district_name <- NA_character_
  }

  # Clean district IDs (remove leading zeros for consistency, then pad back)
  raw_df$district_id <- sprintf("%02d", as.integer(raw_df$district_id))

  # Clean district names
  raw_df$district_name <- trimws(raw_df$district_name)

  # Add is_state flag (district 00 is state totals)
  raw_df$is_state <- raw_df$district_id == "00"

  # Add aggregation level flag
  if ("school_id" %in% names(raw_df) && any(!is.na(raw_df$school_id))) {
    raw_df$aggregation_flag <- ifelse(
      raw_df$is_state, "state",
      ifelse(!is.na(raw_df$school_id) & raw_df$school_id != "", "campus", "district")
    )
  } else {
    raw_df$aggregation_flag <- ifelse(raw_df$is_state, "state", "district")
  }

  # Create is_district and is_campus flags
  raw_df$is_district <- raw_df$aggregation_flag == "district"
  raw_df$is_campus <- raw_df$aggregation_flag == "campus"

  # Ensure end_year is present
  if (!"end_year" %in% names(raw_df)) {
    raw_df$end_year <- end_year
  }

  # Calculate n_proficient from pct_proficient and n_tested
  if ("pct_proficient" %in% names(raw_df) && "n_tested" %in% names(raw_df)) {
    raw_df$n_proficient <- round(raw_df$pct_proficient / 100 * raw_df$n_tested)
  }

  # Calculate n_at_level counts from percentages
  for (i in 1:5) {
    pct_col <- paste0("pct_level_", i)
    n_col <- paste0("n_level_", i)
    if (pct_col %in% names(raw_df) && "n_tested" %in% names(raw_df)) {
      raw_df[[n_col]] <- round(raw_df[[pct_col]] / 100 * raw_df$n_tested)
    }
  }

  # Reorder columns
  standard_cols <- c(
    "end_year", "assessment_system", "subject", "grade", "level",
    "district_id", "district_name", "school_id", "school_name",
    "n_tested", "mean_scale_score",
    "pct_proficient", "n_proficient",
    "pct_level_1", "pct_level_2", "pct_level_3", "pct_level_4", "pct_level_5",
    "n_level_1", "n_level_2", "n_level_3", "n_level_4", "n_level_5",
    "is_state", "is_district", "is_campus", "aggregation_flag"
  )

  existing_cols <- intersect(standard_cols, names(raw_df))
  other_cols <- setdiff(names(raw_df), standard_cols)

  raw_df <- raw_df[, c(existing_cols, other_cols)]

  raw_df
}


#' Identify assessment aggregation levels
#'
#' Adds aggregation flags to assessment data.
#'
#' @param df Assessment data frame
#' @return Data frame with aggregation flags
#' @export
id_assessment_aggs <- function(df) {

  if (nrow(df) == 0) return(df)

  # Add flags if not present
  if (!"is_state" %in% names(df)) {
    df$is_state <- df$district_id == "00"
  }

  if (!"is_district" %in% names(df)) {
    df$is_district <- !df$is_state & (
      !("school_id" %in% names(df)) |
      is.na(df$school_id) |
      df$school_id == ""
    )
  }

  if (!"is_campus" %in% names(df)) {
    df$is_campus <- !df$is_state & !df$is_district
  }

  if (!"aggregation_flag" %in% names(df)) {
    df$aggregation_flag <- dplyr::case_when(
      df$is_state ~ "state",
      df$is_campus ~ "campus",
      TRUE ~ "district"
    )
  }

  df
}
