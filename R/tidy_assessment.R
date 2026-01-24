# ==============================================================================
# Assessment Data Tidying Functions
# ==============================================================================
#
# This file contains functions for converting wide assessment data to tidy format.
#
# ==============================================================================

#' Tidy assessment data
#'
#' Converts wide assessment data (with pct_level_1, pct_level_2, etc. columns)
#' to long (tidy) format with proficiency_level and pct columns.
#'
#' @param df Processed assessment data frame
#' @return Tidy data frame with proficiency_level column
#' @export
#' @examples
#' \dontrun{
#' raw <- get_raw_assessment(2024, "ela", 3)
#' processed <- process_assessment(raw, 2024)
#' tidy <- tidy_assessment(processed)
#' }
tidy_assessment <- function(df) {

  if (nrow(df) == 0) return(df)

  # Identify level columns to pivot
  pct_level_cols <- grep("^pct_level_[1-5]$", names(df), value = TRUE)
  n_level_cols <- grep("^n_level_[1-5]$", names(df), value = TRUE)

  if (length(pct_level_cols) == 0) {
    warning("No proficiency level columns found to tidy")
    return(df)
  }

  # Get ID columns (everything that's not a level column or derived)
  id_cols <- setdiff(names(df), c(pct_level_cols, n_level_cols))

  # Pivot percentages
  df_long <- tidyr::pivot_longer(
    df,
    cols = dplyr::all_of(pct_level_cols),
    names_to = "proficiency_level",
    values_to = "pct"
  )

  # Clean proficiency level names
  df_long$proficiency_level <- gsub("pct_level_", "level_", df_long$proficiency_level)

  # Add friendly proficiency level labels
  df_long$proficiency_label <- dplyr::case_when(
    df_long$proficiency_level == "level_1" ~ "Inadequate",
    df_long$proficiency_level == "level_2" ~ "Below Satisfactory",
    df_long$proficiency_level == "level_3" ~ "Satisfactory",
    df_long$proficiency_level == "level_4" ~ "Proficient",
    df_long$proficiency_level == "level_5" ~ "Mastery",
    TRUE ~ df_long$proficiency_level
  )

  # Add is_proficient flag (Level 3+ is proficient)
  df_long$is_proficient <- df_long$proficiency_level %in% c("level_3", "level_4", "level_5")

  # Calculate n_students at each level from percentage
  if ("n_tested" %in% names(df_long)) {
    df_long$n_students <- round(df_long$pct / 100 * df_long$n_tested)
  }

  # Remove the n_level_* columns from output (we've calculated n_students)
  df_long <- df_long[, !names(df_long) %in% n_level_cols]

  # Reorder columns
  standard_cols <- c(
    "end_year", "assessment_system", "subject", "grade", "level",
    "district_id", "district_name", "school_id", "school_name",
    "proficiency_level", "proficiency_label", "is_proficient",
    "pct", "n_students", "n_tested", "mean_scale_score", "pct_proficient",
    "is_state", "is_district", "is_campus", "aggregation_flag"
  )

  existing_cols <- intersect(standard_cols, names(df_long))
  other_cols <- setdiff(names(df_long), standard_cols)

  df_long <- df_long[, c(existing_cols, other_cols)]

  df_long
}
