# ==============================================================================
# Utility Functions
# ==============================================================================

#' @importFrom rlang .data
NULL


#' Florida District Name Mapping
#'
#' Returns a named vector mapping 2-digit district codes to county names.
#' Florida has 67 county-based districts (01-67) plus special districts (71-75).
#'
#' @return Named character vector
#' @keywords internal
get_fl_district_names <- function() {
  c(
    "01" = "Alachua",
    "02" = "Baker",
    "03" = "Bay",
    "04" = "Bradford",
    "05" = "Brevard",
    "06" = "Broward",
    "07" = "Calhoun",
    "08" = "Charlotte",
    "09" = "Citrus",
    "10" = "Clay",
    "11" = "Collier",
    "12" = "Columbia",
    "13" = "Miami-Dade",
    "14" = "DeSoto",
    "15" = "Dixie",
    "16" = "Duval",
    "17" = "Escambia",
    "18" = "Flagler",
    "19" = "Franklin",
    "20" = "Gadsden",
    "21" = "Gilchrist",
    "22" = "Glades",
    "23" = "Gulf",
    "24" = "Hamilton",
    "25" = "Hardee",
    "26" = "Hendry",
    "27" = "Hernando",
    "28" = "Highlands",
    "29" = "Hillsborough",
    "30" = "Holmes",
    "31" = "Indian River",
    "32" = "Jackson",
    "33" = "Jefferson",
    "34" = "Lafayette",
    "35" = "Lake",
    "36" = "Lee",
    "37" = "Leon",
    "38" = "Levy",
    "39" = "Liberty",
    "40" = "Madison",
    "41" = "Manatee",
    "42" = "Marion",
    "43" = "Martin",
    "44" = "Monroe",
    "45" = "Nassau",
    "46" = "Okaloosa",
    "47" = "Okeechobee",
    "48" = "Orange",
    "49" = "Osceola",
    "50" = "Palm Beach",
    "51" = "Pasco",
    "52" = "Pinellas",
    "53" = "Polk",
    "54" = "Putnam",
    "55" = "St. Johns",
    "56" = "St. Lucie",
    "57" = "Santa Rosa",
    "58" = "Sarasota",
    "59" = "Seminole",
    "60" = "Sumter",
    "61" = "Suwannee",
    "62" = "Taylor",
    "63" = "Union",
    "64" = "Volusia",
    "65" = "Wakulla",
    "66" = "Walton",
    "67" = "Washington",
    "71" = "FL School for Deaf/Blind",
    "72" = "FAU Lab School",
    "73" = "FSU Lab School",
    "74" = "FAMU Lab School",
    "75" = "UF Lab School"
  )
}


#' Format Florida School ID
#'
#' Creates a standardized school ID from district and school numbers.
#'
#' @param district_num District number (2 digits)
#' @param school_num School number (4 digits)
#' @return Character string in format "DD-SSSS"
#' @keywords internal
format_fl_school_id <- function(district_num, school_num) {
  district_num <- sprintf("%02d", as.integer(district_num))
  school_num <- sprintf("%04d", as.integer(school_num))
  paste0(district_num, "-", school_num)
}


#' Parse Florida School ID
#'
#' Extracts district and school numbers from combined ID.
#'
#' @param school_id School ID in format "DD-SSSS" or "DDSSSS"
#' @return List with district_num and school_num
#' @keywords internal
parse_fl_school_id <- function(school_id) {
  # Handle both "DD-SSSS" and "DDSSSS" formats
  if (grepl("-", school_id)) {
    parts <- strsplit(school_id, "-")[[1]]
    list(district_num = parts[1], school_num = parts[2])
  } else if (nchar(school_id) == 6) {
    list(
      district_num = substr(school_id, 1, 2),
      school_num = substr(school_id, 3, 6)
    )
  } else {
    list(district_num = NA_character_, school_num = NA_character_)
  }
}


#' Get available years for Florida enrollment data
#'
#' Returns the range of years available from the Florida Department of
#' Education (FLDOE). Data is available from the 2007-08 school year
#' (end_year = 2008) through 2024-25 (end_year = 2025).
#'
#' Note: Years 2008-2013 have limited demographic data (FTE files only).
#' Full demographic and grade-level data is available from 2014 onward.
#'
#' @return A list with components:
#'   \describe{
#'     \item{min_year}{Integer. The earliest available year (2008).}
#'     \item{max_year}{Integer. The most recent available year (2025).}
#'     \item{description}{Character. A description of the data availability.}
#'   }
#' @export
#' @examples
#' get_available_years()
#' # Returns list(min_year = 2008, max_year = 2025, description = "...")
get_available_years <- function() {
  list(
    min_year = 2008L,
    max_year = 2025L,
    description = "FLDOE enrollment data (2007-08 through 2024-25 school years; 2008-2013 have limited demographics)"
  )
}
