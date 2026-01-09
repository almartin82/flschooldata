# ==============================================================================
# Tidy Graduation Rate Data - Convert to tidy format
# ==============================================================================
#
# This file contains functions for converting processed graduation data to
# tidy format (long format: one row per school-subgroup combination).
#
# Tidy schema:
#   end_year | type | district_id | school_id | district_name | school_name |
#   subgroup | n_graduates | grad_rate | cohort_count
#
# ==============================================================================

#' Tidy graduation data
#'
#' Converts processed graduation data to tidy format (long format).
#' Keeps one row per school-subgroup combination.
#'
#' @param processed Processed data frame from process_graduation()
#' @param level One of "state", "district", "school", or "all" (default)
#' @return Data frame in tidy format
#' @keywords internal
tidy_graduation <- function(processed, level = "all") {

  # Normalize level to match data (capitalize first letter)
  level_normalized <- if (level == "all") "all" else {
    paste0(toupper(substr(level, 1, 1)), substr(level, 2, nchar(level)))
  }

  # Filter by level if specified
  if (level %in% c("State", "District", "School")) {
    processed <- processed[processed$type == level_normalized, ]
  }

  # Data is already in tidy format (long), just need to ensure column order
  tidy <- processed %>%
    dplyr::select(
      end_year,
      type,
      district_id,
      school_id,
      district_name,
      school_name,
      subgroup,
      n_graduates,
      grad_rate,
      cohort_count
    ) %>%
    dplyr::arrange(end_year, type, district_id, school_id, subgroup)

  tidy
}

#' Widen graduation data
#'
#' Converts processed graduation data to wide format (one row per school,
#' with subgroup columns).
#'
#' @param processed Processed data frame from process_graduation()
#' @param level One of "state", "district", "school", or "all" (default)
#' @return Data frame in wide format
#' @keywords internal
widen_graduation <- function(processed, level = "all") {

  # Normalize level to match data (capitalize first letter)
  level_normalized <- if (level == "all") "all" else {
    paste0(toupper(substr(level, 1, 1)), substr(level, 2, nchar(level)))
  }

  # Filter by level if specified
  if (level %in% c("State", "District", "School")) {
    processed <- processed[processed$type == level_normalized, ]
  }

  # Pivot wider
  wide <- processed %>%
    tidyr::pivot_wider(
      id_cols = c(end_year, type, district_id, school_id,
                  district_name, school_name),
      names_from = subgroup,
      values_from = c(n_graduates, grad_rate, cohort_count),
      names_sep = "_"
    )

  wide
}
