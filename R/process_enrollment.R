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

  # The raw data structure:
  # - Rows: (District, School, Grade) combinations
  # - Columns: Grade, White, Black, Hispanic, Asian, Pacific Islander, Native American, Multiracial
  # - Each demographic value is the count FOR THAT SPECIFIC GRADE
  #
  # We need to transform this to:
  # - One row per school
  # - Demographic columns: totals across all grades
  # - Grade columns: total enrollment for each grade

  cols <- names(df)

  # Helper to find column by pattern (case-insensitive)
  find_col <- function(patterns) {
    for (pattern in patterns) {
      matched <- grep(paste0("^", pattern, "$"), cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Find demographic columns
  demo_cols <- c(
    find_col(c("WHITE", "W", "WHITE_TOTAL")),
    find_col(c("BLACK_OR_AFRICAN_AMERICAN", "BLACK", "B", "BLACK_TOTAL", "AFRICAN_AMERICAN")),
    find_col(c("HISPANIC_LATINO", "HISPANIC", "H", "HISP", "HISPANIC_TOTAL")),
    find_col(c("ASIAN", "A", "ASIAN_TOTAL")),
    find_col(c("NATIVE_HAWAIIAN_OR_OTHER_PACIFIC_ISLANDER", "PACIFIC_ISLANDER", "P", "PACIFIC", "HAWAIIAN_PACIFIC")),
    find_col(c("NATIVE_AMERICAN", "I", "AMERICAN_INDIAN", "INDIAN")),
    find_col(c("MULTIRACIAL", "M", "TWO_OR_MORE", "MULTI"))
  )
  demo_cols <- demo_cols[!is.na(demo_cols)]

  # Find grade column
  grade_col <- find_col(c("GRADE", "GRADE_LEVEL"))
  if (is.null(grade_col)) {
    stop("Could not find GRADE column in raw campus data")
  }

  # Find identifier columns
  district_col <- find_col(c("DISTRICT_ID", "DISTRICT", "DIST", "DIST_NO", "DISTRICT_NUMBER"))
  school_col <- find_col(c("SCHOOL_ID", "SCHOOL", "SCH", "SCHOOL_NUMBER", "SCHOOL_NO"))
  district_name_col <- find_col(c("DISTRICT_NAME", "DISTNAME", "DISTRICT"))
  school_name_col <- find_col(c("SCHOOL_NAME", "SCHNAME", "NAME", "SCHOOL"))

  # Convert demographic columns to numeric
  df_demo <- df
  for (col in demo_cols) {
    df_demo[[paste0(col, "_num")]] <- safe_numeric(df[[col]])
  }
  demo_num_cols <- paste0(demo_cols, "_num")

  # Step 1: Calculate grade totals (sum of demographics for each grade)
  df_grade_total <- df_demo |>
    dplyr::mutate(
      grade_total = rowSums(dplyr::across(dplyr::all_of(demo_num_cols)), na.rm = TRUE)
    )

  # Step 2: Aggregate to one row per school
  # Demographic totals: sum across all grades for each school
  result <- df_grade_total |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(district_col, school_col, district_name_col, school_name_col)))
    ) |>
    dplyr::summarize(
      dplyr::across(dplyr::all_of(demo_num_cols), list(total = ~sum(.x, na.rm = TRUE))),
      .groups = "drop"
    ) |>
    dplyr::rename_with(~gsub("_num_total", "", .x))

  # Step 3: Pivot grade data wider to create grade_pk, grade_k, grade_01, etc. columns
  grade_pivot <- df_grade_total |>
    dplyr::select(
      dplyr::all_of(c(district_col, school_col, grade_col, "grade_total"))
    ) |>
    dplyr::mutate(
      # Standardize grade names
      grade_std = dplyr::case_when(
        .data[[grade_col]] %in% c("PK", "PRE-K", "PRE_K", "PREK") ~ "PK",
        .data[[grade_col]] %in% c("K", "KG", "KINDERGARTEN") ~ "K",
        .data[[grade_col]] %in% c("1", "01", "GR1", "GR_1") ~ "01",
        .data[[grade_col]] %in% c("2", "02", "GR2", "GR_2") ~ "02",
        .data[[grade_col]] %in% c("3", "03", "GR3", "GR_3") ~ "03",
        .data[[grade_col]] %in% c("4", "04", "GR4", "GR_4") ~ "04",
        .data[[grade_col]] %in% c("5", "05", "GR5", "GR_5") ~ "05",
        .data[[grade_col]] %in% c("6", "06", "GR6", "GR_6") ~ "06",
        .data[[grade_col]] %in% c("7", "07", "GR7", "GR_7") ~ "07",
        .data[[grade_col]] %in% c("8", "08", "GR8", "GR_8") ~ "08",
        .data[[grade_col]] %in% c("9", "09", "GR9", "GR_9") ~ "09",
        .data[[grade_col]] %in% c("10", "GR10", "GR_10") ~ "10",
        .data[[grade_col]] %in% c("11", "GR11", "GR_11") ~ "11",
        .data[[grade_col]] %in% c("12", "GR12", "GR_12") ~ "12",
        TRUE ~ .data[[grade_col]]
      )
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(district_col, school_col, "grade_std")))
    ) |>
    dplyr::summarize(
      n_students = sum(.data$grade_total, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = "grade_std",
      values_from = "n_students",
      names_prefix = "grade_"
    )

  # Join demographic totals with grade-level pivots
  result <- result |>
    dplyr::left_join(grade_pivot, by = c(district_col, school_col))

  # Rename columns to standard names BEFORE adding metadata
  result <- result |>
    dplyr::rename_with(~tolower(gsub(" ", "_", .x)))

  # Now standardize demographic column names (already lowercase from rename_with)
  names(result) <- gsub("^white$", "white", names(result), ignore.case = TRUE)
  names(result) <- gsub("^black_or_african_american$", "black", names(result), ignore.case = TRUE)
  names(result) <- gsub("^hispanic_latino$", "hispanic", names(result), ignore.case = TRUE)
  names(result) <- gsub("^asian$", "asian", names(result), ignore.case = TRUE)
  names(result) <- gsub("^native_hawaiian_or_other_pacific_islander$", "pacific_islander", names(result), ignore.case = TRUE)
  names(result) <- gsub("^native_american$", "native_american", names(result), ignore.case = TRUE)
  names(result) <- gsub("^multiracial$", "multiracial", names(result), ignore.case = TRUE)

  # Update column references to lowercase after rename (if they exist)
  district_col_lc <- if (!is.null(district_col)) tolower(gsub(" ", "_", district_col)) else NULL
  school_col_lc <- if (!is.null(school_col)) tolower(gsub(" ", "_", school_col)) else NULL
  district_name_col_lc <- if (!is.null(district_name_col)) tolower(gsub(" ", "_", district_name_col)) else NULL
  school_name_col_lc <- if (!is.null(school_name_col)) tolower(gsub(" ", "_", school_name_col)) else NULL

  # Build mutate arguments dynamically based on available columns
  mutate_args <- list(
    end_year = end_year,
    type = "Campus"
  )

  if (!is.null(district_col_lc)) {
    mutate_args$district_id = sprintf("%02d", as.integer(result[[district_col_lc]]))
  }
  if (!is.null(school_col_lc)) {
    mutate_args$school_num = sprintf("%04d", as.integer(result[[school_col_lc]]))
  }
  if (!is.null(district_col_lc) && !is.null(school_col_lc)) {
    mutate_args$campus_id = paste0(
      sprintf("%02d", as.integer(result[[district_col_lc]])), "-",
      sprintf("%04d", as.integer(result[[school_col_lc]]))
    )
  }
  if (!is.null(district_name_col_lc)) {
    mutate_args$district_name = result[[district_name_col_lc]]
  }
  if (!is.null(school_name_col_lc)) {
    mutate_args$campus_name = result[[school_name_col_lc]]
  }

  # Add metadata columns
  result <- result |>
    dplyr::mutate(!!!mutate_args)

  # Calculate row_total from standardized demographic columns
  demo_cols_std <- c("white", "black", "hispanic", "asian",
                     "pacific_islander", "native_american", "multiracial")
  demo_cols_std <- demo_cols_std[demo_cols_std %in% names(result)]
  if (length(demo_cols_std) > 0) {
    result <- result |>
      dplyr::mutate(row_total = rowSums(dplyr::pick(dplyr::all_of(demo_cols_std)), na.rm = TRUE))
  }

  # Select final columns
  result <- result |>
    dplyr::select(
      end_year, type, district_id, campus_id, district_name, campus_name,
      dplyr::any_of(c("white", "black", "hispanic", "asian",
                      "pacific_islander", "native_american", "multiracial",
                      "grade_pk", "grade_k", "grade_01", "grade_02", "grade_03",
                      "grade_04", "grade_05", "grade_06", "grade_07", "grade_08",
                      "grade_09", "grade_10", "grade_11", "grade_12", "row_total"))
    )

  # Filter out invalid rows
  result <- result |>
    dplyr::filter(!is.na(district_id) & district_id != "NA")

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

  # The raw data structure is the same as campus: (District, Grade) rows
  # with demographic counts per grade

  cols <- names(df)

  # Helper to find column by pattern (case-insensitive)
  find_col <- function(patterns) {
    for (pattern in patterns) {
      matched <- grep(paste0("^", pattern, "$"), cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Find demographic columns
  demo_cols <- c(
    find_col(c("WHITE", "W", "WHITE_TOTAL")),
    find_col(c("BLACK_OR_AFRICAN_AMERICAN", "BLACK", "B", "BLACK_TOTAL", "AFRICAN_AMERICAN")),
    find_col(c("HISPANIC_LATINO", "HISPANIC", "H", "HISP", "HISPANIC_TOTAL")),
    find_col(c("ASIAN", "A", "ASIAN_TOTAL")),
    find_col(c("NATIVE_HAWAIIAN_OR_OTHER_PACIFIC_ISLANDER", "PACIFIC_ISLANDER", "P", "PACIFIC", "HAWAIIAN_PACIFIC")),
    find_col(c("NATIVE_AMERICAN", "I", "AMERICAN_INDIAN", "INDIAN")),
    find_col(c("MULTIRACIAL", "M", "TWO_OR_MORE", "MULTI"))
  )
  demo_cols <- demo_cols[!is.na(demo_cols)]

  # Find grade column
  grade_col <- find_col(c("GRADE", "GRADE_LEVEL"))
  if (is.null(grade_col)) {
    stop("Could not find GRADE column in raw district data")
  }

  # Find identifier columns
  district_col <- find_col(c("DISTRICT_ID", "DISTRICT", "DIST", "DIST_NO", "DISTRICT_NUMBER"))
  district_name_col <- find_col(c("DISTRICT_NAME", "DISTNAME", "DISTRICT"))

  # Convert demographic columns to numeric
  df_demo <- df
  for (col in demo_cols) {
    df_demo[[paste0(col, "_num")]] <- safe_numeric(df[[col]])
  }
  demo_num_cols <- paste0(demo_cols, "_num")

  # Step 1: Calculate grade totals (sum of demographics for each grade)
  df_grade_total <- df_demo |>
    dplyr::mutate(
      grade_total = rowSums(dplyr::across(dplyr::all_of(demo_num_cols)), na.rm = TRUE)
    )

  # Step 2: Aggregate to one row per district
  # Demographic totals: sum across all grades for each district
  result <- df_grade_total |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(district_col, district_name_col)))
    ) |>
    dplyr::summarize(
      dplyr::across(dplyr::all_of(demo_num_cols), list(total = ~sum(.x, na.rm = TRUE))),
      .groups = "drop"
    ) |>
    dplyr::rename_with(~gsub("_num_total", "", .x))

  # Step 3: Pivot grade data wider to create grade_pk, grade_k, grade_01, etc. columns
  grade_pivot <- df_grade_total |>
    dplyr::select(
      dplyr::all_of(c(district_col, grade_col, "grade_total"))
    ) |>
    dplyr::mutate(
      # Standardize grade names
      grade_std = dplyr::case_when(
        .data[[grade_col]] %in% c("PK", "PRE-K", "PRE_K", "PREK") ~ "PK",
        .data[[grade_col]] %in% c("K", "KG", "KINDERGARTEN") ~ "K",
        .data[[grade_col]] %in% c("1", "01", "GR1", "GR_1") ~ "01",
        .data[[grade_col]] %in% c("2", "02", "GR2", "GR_2") ~ "02",
        .data[[grade_col]] %in% c("3", "03", "GR3", "GR_3") ~ "03",
        .data[[grade_col]] %in% c("4", "04", "GR4", "GR_4") ~ "04",
        .data[[grade_col]] %in% c("5", "05", "GR5", "GR_5") ~ "05",
        .data[[grade_col]] %in% c("6", "06", "GR6", "GR_6") ~ "06",
        .data[[grade_col]] %in% c("7", "07", "GR7", "GR_7") ~ "07",
        .data[[grade_col]] %in% c("8", "08", "GR8", "GR_8") ~ "08",
        .data[[grade_col]] %in% c("9", "09", "GR9", "GR_9") ~ "09",
        .data[[grade_col]] %in% c("10", "GR10", "GR_10") ~ "10",
        .data[[grade_col]] %in% c("11", "GR11", "GR_11") ~ "11",
        .data[[grade_col]] %in% c("12", "GR12", "GR_12") ~ "12",
        TRUE ~ .data[[grade_col]]
      )
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(district_col, "grade_std")))
    ) |>
    dplyr::summarize(
      n_students = sum(.data$grade_total, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = "grade_std",
      values_from = "n_students",
      names_prefix = "grade_"
    )

  # Join demographic totals with grade-level pivots
  result <- result |>
    dplyr::left_join(grade_pivot, by = district_col)

  # Rename columns to standard names BEFORE adding metadata
  result <- result |>
    dplyr::rename_with(~tolower(gsub(" ", "_", .x)))

  # Now standardize demographic column names (already lowercase from rename_with)
  names(result) <- gsub("^white$", "white", names(result), ignore.case = TRUE)
  names(result) <- gsub("^black_or_african_american$", "black", names(result), ignore.case = TRUE)
  names(result) <- gsub("^hispanic_latino$", "hispanic", names(result), ignore.case = TRUE)
  names(result) <- gsub("^asian$", "asian", names(result), ignore.case = TRUE)
  names(result) <- gsub("^native_hawaiian_or_other_pacific_islander$", "pacific_islander", names(result), ignore.case = TRUE)
  names(result) <- gsub("^native_american$", "native_american", names(result), ignore.case = TRUE)
  names(result) <- gsub("^multiracial$", "multiracial", names(result), ignore.case = TRUE)

  # Update column references to lowercase after rename
  district_col_lc <- tolower(gsub(" ", "_", district_col))
  district_name_col_lc <- tolower(gsub(" ", "_", district_name_col))

  # Add metadata columns
  result <- result |>
    dplyr::mutate(
      end_year = end_year,
      type = "District",
      district_id = sprintf("%02d", as.integer(.data[[district_col_lc]])),
      campus_id = NA_character_,
      district_name = .data[[district_name_col_lc]],
      campus_name = NA_character_
    )

  # Calculate row_total from standardized demographic columns
  demo_cols_std <- c("white", "black", "hispanic", "asian",
                     "pacific_islander", "native_american", "multiracial")
  demo_cols_std <- demo_cols_std[demo_cols_std %in% names(result)]
  if (length(demo_cols_std) > 0) {
    result <- result |>
      dplyr::mutate(row_total = rowSums(dplyr::pick(dplyr::all_of(demo_cols_std)), na.rm = TRUE))
  }

  # Select final columns
  result <- result |>
    dplyr::select(
      end_year, type, district_id, campus_id, district_name, campus_name,
      dplyr::any_of(c("white", "black", "hispanic", "asian",
                      "pacific_islander", "native_american", "multiracial",
                      "grade_pk", "grade_k", "grade_01", "grade_02", "grade_03",
                      "grade_04", "grade_05", "grade_06", "grade_07", "grade_08",
                      "grade_09", "grade_10", "grade_11", "grade_12", "row_total"))
    )

  # Filter out invalid rows
  result <- result |>
    dplyr::filter(!is.na(district_id) & district_id != "NA")

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
