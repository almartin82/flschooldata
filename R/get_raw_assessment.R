# ==============================================================================
# Raw Assessment Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw assessment data from FLDOE.
#
# Assessment Eras:
# - FAST (Florida Assessment of Student Thinking): 2023-2025
#   - Grades 3-10 for ELA, Grades 3-8 for Math
#   - Achievement Levels 1-5
# - FSA (Florida Standards Assessments): 2019-2022
#   - Grades 3-10 for ELA, Grades 3-8 for Math
#   - Achievement Levels 1-5
#
# Note: No 2020 or 2021 data due to COVID-19
#
# ==============================================================================

#' Get available assessment years
#'
#' Returns information about which years have assessment data available.
#'
#' @return List with years, min_year, max_year, and note about gaps
#' @export
#' @examples
#' get_available_assessment_years()
get_available_assessment_years <- function() {
  list(
    years = c(2019, 2022, 2023, 2024, 2025),
    min_year = 2019,
    max_year = 2025,
    note = "2020 and 2021 data not available due to COVID-19 testing waiver"
  )
}


#' Download raw assessment data from FLDOE
#'
#' Downloads assessment data (FSA or FAST) from Florida Department of Education.
#'
#' @param end_year School year end (2023-24 = 2024). Valid years: 2019, 2022-2025.
#' @param subject Subject area: "ela" (default) or "math"
#' @param grade Grade level (3-10 for ELA, 3-8 for Math). NULL fetches all grades.
#' @param level Aggregation level: "district" (default) or "school"
#' @return Data frame with raw assessment data
#' @export
#' @examples
#' \dontrun{
#' # Get 2024 ELA Grade 3 district data
#' raw_ela <- get_raw_assessment(2024, "ela", 3, "district")
#'
#' # Get all 2024 Math district data
#' raw_math <- get_raw_assessment(2024, "math", NULL, "district")
#' }
get_raw_assessment <- function(end_year, subject = "ela", grade = NULL, level = "district") {

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

  # Determine grade range based on subject
  if (subject == "ela") {
    valid_grades <- 3:10
  } else {
    valid_grades <- 3:8
  }

  # If grade is NULL, fetch all grades
  if (is.null(grade)) {
    grades_to_fetch <- valid_grades
  } else {
    if (!grade %in% valid_grades) {
      stop(paste0(
        "Invalid grade ", grade, " for ", toupper(subject), ". ",
        "Valid grades: ", paste(valid_grades, collapse = ", ")
      ))
    }
    grades_to_fetch <- grade
  }

  message(paste("Downloading FLDOE assessment data for", end_year, "..."))

  # Fetch each grade and combine
  results <- lapply(grades_to_fetch, function(g) {
    message(paste("  Fetching", toupper(subject), "Grade", g, "..."))
    tryCatch({
      download_assessment_file(end_year, subject, g, level)
    }, error = function(e) {
      warning(paste("Failed to fetch Grade", g, ":", e$message))
      NULL
    })
  })

  # Remove NULLs and combine
  results <- results[!sapply(results, is.null)]

  if (length(results) == 0) {
    warning("No assessment data could be fetched")
    return(data.frame())
  }

  dplyr::bind_rows(results)
}


#' Download a single assessment file
#'
#' Downloads and parses a single assessment Excel file from FLDOE.
#'
#' @param end_year School year end
#' @param subject "ela" or "math"
#' @param grade Grade level
#' @param level "district" or "school"
#' @return Data frame with assessment data
#' @keywords internal
download_assessment_file <- function(end_year, subject, grade, level) {

  # Build URL
  url <- build_assessment_url(end_year, subject, grade, level)

  # Download to temp file
  tname <- tempfile(
    pattern = paste0("fldoe_assess_", end_year, "_", subject, "_g", grade, "_"),
    tmpdir = tempdir(),
    fileext = ".xls"
  )

  tryCatch({
    response <- httr::GET(
      url,
      httr::write_disk(tname, overwrite = TRUE),
      httr::timeout(120)
    )

    if (httr::http_error(response)) {
      stop(paste("HTTP error:", httr::status_code(response)))
    }

    # Check file is valid
    file_info <- file.info(tname)
    if (file_info$size < 1000) {
      stop("Downloaded file is too small - may be an error page")
    }

  }, error = function(e) {
    unlink(tname)
    stop(paste("Failed to download:", e$message, "\nURL:", url))
  })

  # Parse Excel file
  df <- tryCatch({
    # Read with skip=4 to get header row
    readxl::read_excel(tname, sheet = 1, skip = 4, col_types = "text")
  }, error = function(e) {
    unlink(tname)
    stop(paste("Failed to parse Excel file:", e$message))
  })

  unlink(tname)

  # Standardize column names
  names(df) <- standardize_assessment_colnames(names(df))

  # Add metadata columns
  df$end_year <- end_year
  df$subject <- toupper(subject)
  df$grade <- sprintf("%02d", grade)
  df$level <- level

  # Determine assessment system
  if (end_year >= 2023) {
    df$assessment_system <- "FAST"
  } else {
    df$assessment_system <- "FSA"
  }

  df
}


#' Build assessment URL
#'
#' Constructs the URL for downloading assessment data files based on year and pattern.
#'
#' @param end_year School year end
#' @param subject "ela" or "math"
#' @param grade Grade level
#' @param level "district" or "school"
#' @return URL string
#' @keywords internal
build_assessment_url <- function(end_year, subject, grade, level) {

  base_url <- "https://www.fldoe.org/core/fileparse.php/5668/urlt/"
  year_suffix <- sprintf("%02d", end_year %% 100)
  grade_str <- sprintf("%02d", grade)

  # URL patterns vary significantly by year and subject
  # FAST Era (2023-2025): Numeric prefix + Subject + Grade + SRD/SRS + Spring + YY
  # FSA Era (2019, 2022): SPRyy + Subject + Grade + SRD/SRS OR Numeric prefix patterns

  if (end_year >= 2023) {
    # FAST Era URLs
    filename <- build_fast_url(end_year, subject, grade, level, grade_str, year_suffix)
  } else {
    # FSA Era URLs
    filename <- build_fsa_url(end_year, subject, grade, level, grade_str, year_suffix)
  }

  paste0(base_url, filename)
}


#' Build FAST era URL (2023-2025)
#'
#' @keywords internal
build_fast_url <- function(end_year, subject, grade, level, grade_str, year_suffix) {

  # FAST URL patterns have numeric prefixes that vary by year
  # 2024 ELA District: 3ELA03SRDSpring24.xls, 4ELA04SRDSpring24.xls, ...
  # 2024 ELA School: 11ELA03SRSSpring24.xls, 12ELA04SRSSpring24.xls, ...
  # 2024 Math District: 21Math03SRDSpring24.xls, 22Math04SRDSpring24.xls, ...
  # 2024 Math School: 27Math03SRSSpring24.xls, 28Math04SRSSpring24.xls, ...
  #
  # 2023 ELA District: 2ELA03SRDSpring23.xls, 3ELA04SRDSpring23.xls, ...
  # 2023 ELA School: 10ELA03SRSSpring23.xls, 11ELA04SRSSpring23.xls, ...
  # 2023 Math District: 19Math03SRDSpring23.xls, 20Math04SRDSpring23.xls, ...
  # 2023 Math School: 25Math03SRSSpring23.xls, 26Math04SRSSpring23.xls, ...

  suffix_code <- if (level == "district") "SRD" else "SRS"
  subject_cap <- if (subject == "ela") "ELA" else "Math"

  # Calculate numeric prefix based on year, subject, level, and grade
  if (end_year == 2025) {
    # 2025 pattern (assuming similar to 2024)
    if (subject == "ela" && level == "district") {
      prefix <- grade  # 3, 4, 5, 6, 7, 8, 9, 10
    } else if (subject == "ela" && level == "school") {
      prefix <- grade + 8  # 11, 12, 13, 14, 15, 16, 17, 18
    } else if (subject == "math" && level == "district") {
      prefix <- grade + 18  # 21, 22, 23, 24, 25, 26
    } else {  # math school
      prefix <- grade + 24  # 27, 28, 29, 30, 31, 32
    }
  } else if (end_year == 2024) {
    if (subject == "ela" && level == "district") {
      prefix <- grade  # 3, 4, 5, 6, 7, 8, 9, 10
    } else if (subject == "ela" && level == "school") {
      prefix <- grade + 8  # 11, 12, 13, 14, 15, 16, 17, 18
    } else if (subject == "math" && level == "district") {
      prefix <- grade + 18  # 21, 22, 23, 24, 25, 26
    } else {  # math school
      prefix <- grade + 24  # 27, 28, 29, 30, 31, 32
    }
  } else {  # 2023
    if (subject == "ela" && level == "district") {
      prefix <- grade - 1  # 2, 3, 4, 5, 6, 7, 8, 9
    } else if (subject == "ela" && level == "school") {
      prefix <- grade + 7  # 10, 11, 12, 13, 14, 15, 16, 17
    } else if (subject == "math" && level == "district") {
      prefix <- grade + 16  # 19, 20, 21, 22, 23, 24
    } else {  # math school
      prefix <- grade + 22  # 25, 26, 27, 28, 29, 30
    }
  }

  paste0(prefix, subject_cap, grade_str, suffix_code, "Spring", year_suffix, ".xls")
}


#' Build FSA era URL (2019, 2022)
#'
#' @keywords internal
build_fsa_url <- function(end_year, subject, grade, level, grade_str, year_suffix) {

  suffix_code <- if (level == "district") "SRD" else "SRS"

  if (end_year == 2022) {
    # 2022 has mixed patterns
    # Grade 3 ELA uses SPR22ELA03SRD.xls pattern
    # Other grades use numeric prefix pattern
    if (grade == 3 && subject == "ela") {
      paste0("SPR22ELA03", suffix_code, ".xls")
    } else if (subject == "ela") {
      # Grades 4-10: 2ELA04SRDSpring22.xls, 3ELA05SRDSpring22.xls, ...
      prefix <- if (level == "district") grade - 2 else grade + 5
      paste0(prefix, "ELA", grade_str, suffix_code, "Spring22.xls")
    } else {
      # Math: 17Math03SRDSpring22.xls, 18Math04SRDSpring22.xls, ...
      prefix <- if (level == "district") grade + 14 else grade + 20
      paste0(prefix, "Math", grade_str, suffix_code, "Spring22.xls")
    }
  } else if (end_year == 2019) {
    # 2019 FSA patterns
    # Grade 3 ELA uses SPR19ELA03SRD.xls pattern
    if (grade == 3 && subject == "ela") {
      paste0("SPR19ELA03", suffix_code, ".xls")
    } else if (subject == "ela") {
      # Grades 4-10: 2Spring19ELA04SRD.xls, 3Spring19ELA05SRD.xls, ...
      prefix <- if (level == "district") grade - 2 else grade + 5
      paste0(prefix, "Spring19ELA", grade_str, suffix_code, ".xls")
    } else {
      # Math: 17Spring19Math03SRD.xls, etc.
      prefix <- if (level == "district") grade + 14 else grade + 20
      paste0(prefix, "Spring19Math", grade_str, suffix_code, ".xls")
    }
  } else {
    stop(paste("Unsupported FSA year:", end_year))
  }
}


#' Standardize assessment column names
#'
#' Converts FLDOE assessment column names to a consistent standard.
#'
#' @param colnames Character vector of column names
#' @return Character vector of standardized column names
#' @keywords internal
standardize_assessment_colnames <- function(colnames) {

  # Clean up: trim, handle newlines
  colnames <- gsub("\n", " ", colnames)
  colnames <- trimws(colnames)

  # Standard mappings
  colnames <- gsub("^District Number$", "district_id", colnames, ignore.case = TRUE)
  colnames <- gsub("^District Name$", "district_name", colnames, ignore.case = TRUE)
  colnames <- gsub("^School Number$", "school_id", colnames, ignore.case = TRUE)
  colnames <- gsub("^School Name$", "school_name", colnames, ignore.case = TRUE)
  colnames <- gsub("^Grade$", "test_grade", colnames, ignore.case = TRUE)
  colnames <- gsub("^Number of Students$", "n_tested", colnames, ignore.case = TRUE)
  colnames <- gsub("^Mean Scale Score$", "mean_scale_score", colnames, ignore.case = TRUE)
  colnames <- gsub("^Percentage in Level 3 or Above$", "pct_proficient", colnames, ignore.case = TRUE)
  colnames <- gsub("^Percentage in  Level 3 or Above$", "pct_proficient", colnames, ignore.case = TRUE)
  colnames <- gsub(".*Level 3 or Above.*", "pct_proficient", colnames, ignore.case = TRUE)

  # Achievement level columns (1-5)
  colnames <- gsub("^1$", "pct_level_1", colnames)
  colnames <- gsub("^2$", "pct_level_2", colnames)
  colnames <- gsub("^3$", "pct_level_3", colnames)
  colnames <- gsub("^4$", "pct_level_4", colnames)
  colnames <- gsub("^5$", "pct_level_5", colnames)

  # Handle any remaining special characters
  colnames <- gsub("[^a-zA-Z0-9_]", "_", colnames)
  colnames <- gsub("_+", "_", colnames)
  colnames <- gsub("^_|_$", "", colnames)

  # Convert to lowercase
  tolower(colnames)
}
