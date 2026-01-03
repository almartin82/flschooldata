# ==============================================================================
# Raw Enrollment Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw enrollment data from FLDOE.
# Data comes from several sources depending on year:
#
# ERA 1 (2014-present): Membership by School/Grade/Race Excel files
#   - URL pattern: https://www.fldoe.org/core/fileparse.php/7584/urlt/{YYMM}MembBySchoolByGradeByRace.xlsx
#   - Most detailed data with school-level breakdowns by grade and race
#
# ERA 2 (2008-2013): FTE Enrollment Excel files (district-level only)
#   - URL pattern: https://www.fldoe.org/core/fileparse.php/7508/urlt/{YYYY-YY}FinalFTEx.xls
#   - Contains FTE counts by program, less demographic detail
#
# Note: Earlier data exists in archives but with inconsistent formats
#
# ==============================================================================

#' Download raw enrollment data from FLDOE
#'
#' Downloads school enrollment data from Florida Department of Education.
#' Uses membership files for 2014+ and FTE files for 2008-2013.
#'
#' @param end_year School year end (2023-24 = 2024)
#' @return List with campus and district data frames
#' @keywords internal
get_raw_enr <- function(end_year) {

  # Validate year
  if (end_year < 2008 || end_year > 2025) {
    stop("end_year must be between 2008 and 2025")
  }

  message(paste("Downloading FLDOE enrollment data for", end_year, "..."))

  # Use appropriate download function based on year
  if (end_year >= 2014) {
    # Modern membership files (2014+)
    result <- download_membership_data(end_year)
  } else {
    # FTE enrollment files (2008-2013)
    result <- download_fte_data(end_year)
  }

  # Add end_year column to both data frames (only if they have rows)
  if (nrow(result$campus) > 0) {
    result$campus$end_year <- end_year
  }
  if (nrow(result$district) > 0) {
    result$district$end_year <- end_year
  }

  result
}


#' Download membership data (2014-present)
#'
#' Downloads the "Membership by School by Grade by Race/Ethnicity" Excel files
#' from FLDOE. These provide the most detailed enrollment data.
#'
#' @param end_year School year end (2014-2025)
#' @return List with campus and district data frames
#' @keywords internal
download_membership_data <- function(end_year) {

  message("  Downloading membership by school/grade/race data...")

  # Build URL - format varies slightly by year
  # Pattern: {YY}{YY}MembBySchoolByGradeByRace.xlsx
  year_code <- paste0(
    sprintf("%02d", (end_year - 1) %% 100),
    sprintf("%02d", end_year %% 100)
  )

  # FLDOE uses fileparse.php endpoint
  url <- paste0(
    "https://www.fldoe.org/core/fileparse.php/7584/urlt/",
    year_code, "MembBySchoolByGradeByRace.xlsx"
  )

  # Download to temp file
  tname <- tempfile(
    pattern = paste0("fldoe_memb_", year_code, "_"),
    tmpdir = tempdir(),
    fileext = ".xlsx"
  )

  tryCatch({
    response <- httr::GET(
      url,
      httr::write_disk(tname, overwrite = TRUE),
      httr::timeout(300)
    )

    if (httr::http_error(response)) {
      # Try alternate URL pattern for older years
      url_alt <- paste0(
        "https://www.fldoe.org/core/fileparse.php/7584/urlt/",
        year_code, "MembBySchByGradeByRace.xlsx"
      )

      response <- httr::GET(
        url_alt,
        httr::write_disk(tname, overwrite = TRUE),
        httr::timeout(300)
      )

      if (httr::http_error(response)) {
        stop(paste("HTTP error:", httr::status_code(response)))
      }
    }

    # Check file is valid Excel
    file_info <- file.info(tname)
    if (file_info$size < 1000) {
      stop("Downloaded file is too small - may be an error page")
    }

  }, error = function(e) {
    stop(paste("Failed to download membership data for year", end_year,
               "\nURL:", url,
               "\nError:", e$message))
  })

  # Read Excel file - FLDOE files typically have header rows to skip
  # Try to detect the actual data start
  campus_data <- tryCatch({
    # First try reading with headers
    df <- readxl::read_excel(tname, sheet = 1, col_types = "text")

    # Find the header row (look for "District" or "DISTRICT")
    header_row <- which(apply(df, 1, function(row) {
      any(grepl("^District$|^DISTRICT$", row, ignore.case = TRUE))
    }))[1]

    if (!is.na(header_row) && header_row > 1) {
      # Re-read with proper skip
      df <- readxl::read_excel(tname, sheet = 1, skip = header_row - 1, col_types = "text")
    }

    df
  }, error = function(e) {
    # Fallback: try reading without skip
    readxl::read_excel(tname, sheet = 1, col_types = "text")
  })

  unlink(tname)

  # Standardize column names
  names(campus_data) <- standardize_fldoe_colnames(names(campus_data))

  # Create district aggregates from campus data
  district_data <- aggregate_to_district(campus_data, end_year)

  list(
    campus = campus_data,
    district = district_data
  )
}


#' Download FTE enrollment data (2008-2013)
#'
#' Downloads FTE (Full-Time Equivalent) enrollment files from FLDOE.
#' These files have district-level data with less demographic detail.
#'
#' @param end_year School year end (2008-2013)
#' @return List with campus and district data frames
#' @keywords internal
download_fte_data <- function(end_year) {

  message("  Downloading FTE enrollment data...")

  # URL patterns vary by year
  # Older years: {YYYY-YY}FinalFTEx.xls
  # Newer years: {YYMM}FinalCalcFTE.xlsx

  year_str <- paste0(end_year - 1, "-", sprintf("%02d", end_year %% 100))

  # Try multiple URL patterns
  urls <- c(
    paste0("https://www.fldoe.org/core/fileparse.php/7508/urlt/", year_str, "FinalFTEx.xls"),
    paste0("https://www.fldoe.org/core/fileparse.php/7508/urlt/", year_str, "FinalFTE.xls"),
    paste0("https://www.fldoe.org/core/fileparse.php/7508/urlt/", year_str, "FinalFTE.XLS")
  )

  tname <- tempfile(
    pattern = paste0("fldoe_fte_", end_year, "_"),
    tmpdir = tempdir(),
    fileext = ".xls"
  )

  downloaded <- FALSE

  for (url in urls) {
    tryCatch({
      response <- httr::GET(
        url,
        httr::write_disk(tname, overwrite = TRUE),
        httr::timeout(300)
      )

      if (!httr::http_error(response)) {
        file_info <- file.info(tname)
        if (file_info$size > 1000) {
          downloaded <- TRUE
          break
        }
      }
    }, error = function(e) {
      # Try next URL
    })
  }

  if (!downloaded) {
    stop(paste("Failed to download FTE data for year", end_year,
               "\nTried URLs:", paste(urls, collapse = "\n")))
  }

  # Read Excel file
  district_data <- tryCatch({
    df <- readxl::read_excel(tname, sheet = 1, col_types = "text")

    # Find header row
    header_row <- which(apply(df, 1, function(row) {
      any(grepl("District|DISTRICT", row, ignore.case = TRUE))
    }))[1]

    if (!is.na(header_row) && header_row > 1) {
      df <- readxl::read_excel(tname, sheet = 1, skip = header_row - 1, col_types = "text")
    }

    df
  }, error = function(e) {
    readxl::read_excel(tname, sheet = 1, col_types = "text")
  })

  unlink(tname)

  # Standardize column names
  names(district_data) <- standardize_fldoe_colnames(names(district_data))

  # FTE files are district-only, create empty campus frame
  campus_data <- data.frame(
    district_id = character(0),
    school_id = character(0),
    stringsAsFactors = FALSE
  )

  list(
    campus = campus_data,
    district = district_data
  )
}


#' Standardize FLDOE column names
#'
#' Converts various FLDOE column name formats to a consistent standard.
#'
#' @param colnames Character vector of column names
#' @return Character vector of standardized column names
#' @keywords internal
standardize_fldoe_colnames <- function(colnames) {

  # Clean up: trim, uppercase, remove special chars
  colnames <- toupper(trimws(colnames))
  colnames <- gsub("[^A-Z0-9_]", "_", colnames)
  colnames <- gsub("_+", "_", colnames)
  colnames <- gsub("^_|_$", "", colnames)

  # Common mappings
  colnames <- gsub("^DIST$|^DIST_NO$|^DISTRICT_NUMBER$|^DISTRICT_NO$", "DISTRICT", colnames)
  colnames <- gsub("^DISTRICT_NAME$|^DISTNAME$", "DISTRICT_NAME", colnames)
  colnames <- gsub("^SCHOOL$|^SCH$|^SCHOOL_NUMBER$|^SCHOOL_NO$", "SCHOOL", colnames)
  colnames <- gsub("^SCHOOL_NAME$|^SCHNAME$", "SCHOOL_NAME", colnames)

  # Grade columns
  colnames <- gsub("^PK$|^PRE_K$|^PREK$", "GRADE_PK", colnames)
  colnames <- gsub("^KG$|^K$|^KINDER$", "GRADE_K", colnames)
  colnames <- gsub("^GR_?(0?1)$|^GRADE_?(0?1)$", "GRADE_01", colnames)
  colnames <- gsub("^GR_?(0?2)$|^GRADE_?(0?2)$", "GRADE_02", colnames)
  colnames <- gsub("^GR_?(0?3)$|^GRADE_?(0?3)$", "GRADE_03", colnames)
  colnames <- gsub("^GR_?(0?4)$|^GRADE_?(0?4)$", "GRADE_04", colnames)
  colnames <- gsub("^GR_?(0?5)$|^GRADE_?(0?5)$", "GRADE_05", colnames)
  colnames <- gsub("^GR_?(0?6)$|^GRADE_?(0?6)$", "GRADE_06", colnames)
  colnames <- gsub("^GR_?(0?7)$|^GRADE_?(0?7)$", "GRADE_07", colnames)
  colnames <- gsub("^GR_?(0?8)$|^GRADE_?(0?8)$", "GRADE_08", colnames)
  colnames <- gsub("^GR_?(0?9)$|^GRADE_?(0?9)$", "GRADE_09", colnames)
  colnames <- gsub("^GR_?(10)$|^GRADE_?(10)$", "GRADE_10", colnames)
  colnames <- gsub("^GR_?(11)$|^GRADE_?(11)$", "GRADE_11", colnames)
  colnames <- gsub("^GR_?(12)$|^GRADE_?(12)$", "GRADE_12", colnames)

  # Race/Ethnicity columns
  colnames <- gsub("^WHITE$|^W$", "WHITE", colnames)
  colnames <- gsub("^BLACK$|^B$|^AFRICAN_AMERICAN$", "BLACK", colnames)
  colnames <- gsub("^HISPANIC$|^H$|^HISP$", "HISPANIC", colnames)
  colnames <- gsub("^ASIAN$|^A$", "ASIAN", colnames)
  colnames <- gsub("^AMERICAN_INDIAN.*|^NATIVE_AMERICAN$|^I$", "NATIVE_AMERICAN", colnames)
  colnames <- gsub("^PACIFIC.*|^P$", "PACIFIC_ISLANDER", colnames)
  colnames <- gsub("^MULTI.*|^TWO.*|^M$", "MULTIRACIAL", colnames)

  # Total
  colnames <- gsub("^TOTAL$|^TOT$|^MEMBERSHIP$|^ENROLLMENT$", "TOTAL", colnames)

  colnames
}


#' Aggregate campus data to district level
#'
#' Creates district-level summaries from campus-level data.
#'
#' @param campus_df Campus-level data frame
#' @param end_year School year end
#' @return District-level data frame
#' @keywords internal
aggregate_to_district <- function(campus_df, end_year) {

  # Find the district ID column
  dist_col <- grep("^DISTRICT$|^DIST$", names(campus_df), value = TRUE, ignore.case = TRUE)[1]

  if (is.na(dist_col) || is.null(dist_col)) {
    # Return empty frame if no district column
    return(data.frame(DISTRICT = character(0), stringsAsFactors = FALSE))
  }

  # Identify numeric columns to sum
  num_cols <- names(campus_df)[sapply(campus_df, function(x) {
    suppressWarnings(!all(is.na(as.numeric(x))))
  })]

  # Exclude ID columns from summing
  num_cols <- setdiff(num_cols, c(dist_col, "SCHOOL", "SCHOOL_NUMBER"))

  if (length(num_cols) == 0) {
    # No numeric columns to aggregate
    district_data <- campus_df |>
      dplyr::select(dplyr::all_of(dist_col)) |>
      dplyr::distinct()
    return(district_data)
  }

  # Convert to numeric and aggregate
  district_data <- campus_df |>
    dplyr::mutate(dplyr::across(dplyr::all_of(num_cols), ~suppressWarnings(as.numeric(.x)))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(dist_col))) |>
    dplyr::summarize(
      dplyr::across(dplyr::all_of(num_cols), ~sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  # Add district name if available
  dist_name_col <- grep("^DISTRICT_NAME$", names(campus_df), value = TRUE, ignore.case = TRUE)[1]
  if (!is.na(dist_name_col)) {
    dist_names <- campus_df |>
      dplyr::select(dplyr::all_of(c(dist_col, dist_name_col))) |>
      dplyr::distinct() |>
      dplyr::filter(!is.na(.[[1]]))

    district_data <- dplyr::left_join(district_data, dist_names, by = dist_col)
  }

  district_data
}


#' Get URL for FLDOE membership file
#'
#' Constructs the URL for downloading membership data files.
#'
#' @param end_year School year end
#' @param file_type Type of file: "membership", "grade", or "race"
#' @return URL string
#' @keywords internal
get_fldoe_url <- function(end_year, file_type = "membership") {

  year_code <- paste0(
    sprintf("%02d", (end_year - 1) %% 100),
    sprintf("%02d", end_year %% 100)
  )

  base_url <- "https://www.fldoe.org/core/fileparse.php/7584/urlt/"

  url <- switch(file_type,
    "membership" = paste0(base_url, year_code, "MembInFLPublicSchools.xlsx"),
    "grade" = paste0(base_url, year_code, "MembBySchoolByGrade.xlsx"),
    "race" = paste0(base_url, year_code, "MembBySchoolByGradeByRace.xlsx"),
    stop("Unknown file_type: ", file_type)
  )

  url
}
