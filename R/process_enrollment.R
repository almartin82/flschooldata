# ==============================================================================
# Enrollment Data Processing Functions
# ==============================================================================
#
# This file contains functions for processing raw FLDOE enrollment data into a
# clean, standardized format.
#
# ==============================================================================

#' Convert to numeric, handling suppression markers
#'
#' FLDOE uses various markers for suppressed data (*, <, -, etc.)
#' and may use commas in large numbers.
#'
#' @param x Vector to convert
#' @return Numeric vector with NA for non-numeric values
#' @keywords internal
safe_numeric <- function(x) {
  # Remove commas and whitespace
  x <- gsub(",", "", x)
  x <- trimws(x)

  # Handle common suppression markers
  x[x %in% c("*", ".", "-", "-1", "<5", "<10", "N/A", "NA", "", "n/a", "N", "S")] <- NA_character_

  # Handle < and > prefixes
  x <- gsub("^[<>]\\s*", "", x)

  suppressWarnings(as.numeric(x))
}


#' Process raw FLDOE enrollment data
#'
#' Transforms raw FLDOE data into a standardized schema combining campus
#' and district data.
#'
#' @param raw_data List containing campus and district data frames from get_raw_enr
#' @param end_year School year end
#' @return Processed data frame with standardized columns
#' @keywords internal
process_enr <- function(raw_data, end_year) {

  # Process campus data if available
  if (nrow(raw_data$campus) > 0) {
    campus_processed <- process_campus_enr(raw_data$campus, end_year)
  } else {
    campus_processed <- data.frame()
  }

  # Process district data
  district_processed <- process_district_enr(raw_data$district, end_year)

  # Create state aggregate
  state_processed <- create_state_aggregate(district_processed, end_year)

  # Combine all levels
  result <- dplyr::bind_rows(state_processed, district_processed, campus_processed)

  result
}


#' Process campus-level enrollment data
#'
#' @param df Raw campus data frame
#' @param end_year School year end
#' @return Processed campus data frame
#' @keywords internal
process_campus_enr <- function(df, end_year) {

  cols <- names(df)
  n_rows <- nrow(df)

  # Helper to find column by pattern (case-insensitive)
  find_col <- function(patterns) {
    for (pattern in patterns) {
      matched <- grep(paste0("^", pattern, "$"), cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Build result dataframe with same number of rows as input
  result <- data.frame(
    end_year = rep(end_year, n_rows),
    type = rep("Campus", n_rows),
    stringsAsFactors = FALSE
  )

  # District ID (2 digits)
  district_col <- find_col(c("DISTRICT", "DIST", "DIST_NO", "DISTRICT_NUMBER"))
  if (!is.null(district_col)) {
    result$district_id <- sprintf("%02d", as.integer(trimws(df[[district_col]])))
  }

  # School ID (4 digits)
  school_col <- find_col(c("SCHOOL", "SCH", "SCHOOL_NUMBER", "SCHOOL_NO"))
  if (!is.null(school_col)) {
    result$school_num <- sprintf("%04d", as.integer(trimws(df[[school_col]])))
    # Create combined campus_id
    if (!is.null(district_col)) {
      result$campus_id <- paste0(result$district_id, "-", result$school_num)
    }
  }

  # Names
  school_name_col <- find_col(c("SCHOOL_NAME", "SCHNAME", "NAME"))
  if (!is.null(school_name_col)) {
    result$campus_name <- trimws(df[[school_name_col]])
  }

  district_name_col <- find_col(c("DISTRICT_NAME", "DISTNAME"))
  if (!is.null(district_name_col)) {
    result$district_name <- trimws(df[[district_name_col]])
  } else if (!is.null(district_col)) {
    # Use lookup table for district names
    dist_names <- get_fl_district_names()
    result$district_name <- dist_names[result$district_id]
  }

  # Total enrollment
  total_col <- find_col(c("TOTAL", "TOT", "MEMBERSHIP", "ENROLLMENT", "TOTAL_MEMBERSHIP"))
  if (!is.null(total_col)) {
    result$row_total <- safe_numeric(df[[total_col]])
  }

  # Demographics - Race/Ethnicity
  demo_map <- list(
    white = c("WHITE", "W", "WHITE_TOTAL"),
    black = c("BLACK", "B", "BLACK_TOTAL", "AFRICAN_AMERICAN"),
    hispanic = c("HISPANIC", "H", "HISP", "HISPANIC_TOTAL"),
    asian = c("ASIAN", "A", "ASIAN_TOTAL"),
    pacific_islander = c("PACIFIC_ISLANDER", "P", "PACIFIC", "HAWAIIAN_PACIFIC"),
    native_american = c("NATIVE_AMERICAN", "I", "AMERICAN_INDIAN", "INDIAN"),
    multiracial = c("MULTIRACIAL", "M", "TWO_OR_MORE", "MULTI")
  )

  for (name in names(demo_map)) {
    col <- find_col(demo_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Gender
  gender_map <- list(
    male = c("MALE", "M", "MALE_TOTAL"),
    female = c("FEMALE", "F", "FEMALE_TOTAL")
  )

  for (name in names(gender_map)) {
    col <- find_col(gender_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Grade levels
  grade_map <- list(
    grade_pk = c("GRADE_PK", "PK", "PRE_K", "PREK"),
    grade_k = c("GRADE_K", "KG", "K", "KINDERGARTEN"),
    grade_01 = c("GRADE_01", "GR_1", "GR1", "1"),
    grade_02 = c("GRADE_02", "GR_2", "GR2", "2"),
    grade_03 = c("GRADE_03", "GR_3", "GR3", "3"),
    grade_04 = c("GRADE_04", "GR_4", "GR4", "4"),
    grade_05 = c("GRADE_05", "GR_5", "GR5", "5"),
    grade_06 = c("GRADE_06", "GR_6", "GR6", "6"),
    grade_07 = c("GRADE_07", "GR_7", "GR7", "7"),
    grade_08 = c("GRADE_08", "GR_8", "GR8", "8"),
    grade_09 = c("GRADE_09", "GR_9", "GR9", "9"),
    grade_10 = c("GRADE_10", "GR_10", "GR10", "10"),
    grade_11 = c("GRADE_11", "GR_11", "GR11", "11"),
    grade_12 = c("GRADE_12", "GR_12", "GR12", "12")
  )

  for (name in names(grade_map)) {
    col <- find_col(grade_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Filter out invalid rows (no district or school ID)
  if ("district_id" %in% names(result)) {
    result <- result[!is.na(result$district_id) & result$district_id != "NA", ]
  }

  result
}


#' Process district-level enrollment data
#'
#' @param df Raw district data frame
#' @param end_year School year end
#' @return Processed district data frame
#' @keywords internal
process_district_enr <- function(df, end_year) {

  if (nrow(df) == 0) {
    return(data.frame(
      end_year = integer(0),
      type = character(0),
      district_id = character(0),
      stringsAsFactors = FALSE
    ))
  }

  cols <- names(df)
  n_rows <- nrow(df)

  # Helper to find column by pattern (case-insensitive)
  find_col <- function(patterns) {
    for (pattern in patterns) {
      matched <- grep(paste0("^", pattern, "$"), cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Build result dataframe with same number of rows as input
  result <- data.frame(
    end_year = rep(end_year, n_rows),
    type = rep("District", n_rows),
    stringsAsFactors = FALSE
  )

  # District ID
  district_col <- find_col(c("DISTRICT", "DIST", "DIST_NO", "DISTRICT_NUMBER"))
  if (!is.null(district_col)) {
    result$district_id <- sprintf("%02d", as.integer(trimws(df[[district_col]])))
  }

  # Campus ID is NA for district rows
  result$campus_id <- rep(NA_character_, n_rows)

  # Names
  district_name_col <- find_col(c("DISTRICT_NAME", "DISTNAME"))
  if (!is.null(district_name_col)) {
    result$district_name <- trimws(df[[district_name_col]])
  } else if (!is.null(district_col)) {
    # Use lookup table for district names
    dist_names <- get_fl_district_names()
    result$district_name <- dist_names[result$district_id]
  }

  result$campus_name <- rep(NA_character_, n_rows)

  # Total enrollment
  total_col <- find_col(c("TOTAL", "TOT", "MEMBERSHIP", "ENROLLMENT", "TOTAL_MEMBERSHIP"))
  if (!is.null(total_col)) {
    result$row_total <- safe_numeric(df[[total_col]])
  }

  # Demographics - Race/Ethnicity
  demo_map <- list(
    white = c("WHITE", "W", "WHITE_TOTAL"),
    black = c("BLACK", "B", "BLACK_TOTAL", "AFRICAN_AMERICAN"),
    hispanic = c("HISPANIC", "H", "HISP", "HISPANIC_TOTAL"),
    asian = c("ASIAN", "A", "ASIAN_TOTAL"),
    pacific_islander = c("PACIFIC_ISLANDER", "P", "PACIFIC", "HAWAIIAN_PACIFIC"),
    native_american = c("NATIVE_AMERICAN", "I", "AMERICAN_INDIAN", "INDIAN"),
    multiracial = c("MULTIRACIAL", "M", "TWO_OR_MORE", "MULTI")
  )

  for (name in names(demo_map)) {
    col <- find_col(demo_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Gender
  gender_map <- list(
    male = c("MALE", "M", "MALE_TOTAL"),
    female = c("FEMALE", "F", "FEMALE_TOTAL")
  )

  for (name in names(gender_map)) {
    col <- find_col(gender_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Grade levels
  grade_map <- list(
    grade_pk = c("GRADE_PK", "PK", "PRE_K", "PREK"),
    grade_k = c("GRADE_K", "KG", "K", "KINDERGARTEN"),
    grade_01 = c("GRADE_01", "GR_1", "GR1", "1"),
    grade_02 = c("GRADE_02", "GR_2", "GR2", "2"),
    grade_03 = c("GRADE_03", "GR_3", "GR3", "3"),
    grade_04 = c("GRADE_04", "GR_4", "GR4", "4"),
    grade_05 = c("GRADE_05", "GR_5", "GR5", "5"),
    grade_06 = c("GRADE_06", "GR_6", "GR6", "6"),
    grade_07 = c("GRADE_07", "GR_7", "GR7", "7"),
    grade_08 = c("GRADE_08", "GR_8", "GR8", "8"),
    grade_09 = c("GRADE_09", "GR_9", "GR9", "9"),
    grade_10 = c("GRADE_10", "GR_10", "GR10", "10"),
    grade_11 = c("GRADE_11", "GR_11", "GR11", "11"),
    grade_12 = c("GRADE_12", "GR_12", "GR12", "12")
  )

  for (name in names(grade_map)) {
    col <- find_col(grade_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Filter out invalid rows
  if ("district_id" %in% names(result)) {
    result <- result[!is.na(result$district_id) & result$district_id != "NA", ]
  }

  result
}


#' Create state-level aggregate from district data
#'
#' @param district_df Processed district data frame
#' @param end_year School year end
#' @return Single-row data frame with state totals
#' @keywords internal
create_state_aggregate <- function(district_df, end_year) {

  # Columns to sum
  sum_cols <- c(
    "row_total",
    "white", "black", "hispanic", "asian",
    "pacific_islander", "native_american", "multiracial",
    "male", "female",
    "econ_disadv", "lep", "special_ed",
    "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12"
  )

  # Filter to columns that exist
  sum_cols <- sum_cols[sum_cols %in% names(district_df)]

  # Create state row
  state_row <- data.frame(
    end_year = end_year,
    type = "State",
    district_id = NA_character_,
    campus_id = NA_character_,
    district_name = NA_character_,
    campus_name = NA_character_,
    stringsAsFactors = FALSE
  )

  # Sum each column
  for (col in sum_cols) {
    state_row[[col]] <- sum(district_df[[col]], na.rm = TRUE)
  }

  state_row
}
