# ==============================================================================
# Fetch Graduation Rate Data - High-level API
# ==============================================================================
#
# This file provides the user-facing API for fetching graduation rate data.
#
# Main function: fetch_graduation()
#   - Downloads data from FLDOE
#   - Processes to standard format
#   - Returns tidy (long) or wide format
#
# ==============================================================================

#' Fetch Florida graduation rate data
#'
#' Downloads and processes 4-year cohort graduation rate data from the Florida
#' Department of Education (FLDOE).
#'
#' @param end_year School year end (e.g., 2024 for 2023-24 school year).
#'   Valid years: 2016-2024. See get_available_grad_years().
#' @param level One of "state", "district", "school", or "all" (default).
#'   Controls which geographic levels to return.
#' @param tidy If TRUE (default), returns long format (one row per school-subgroup).
#'   If FALSE, returns wide format (one row per school, subgroup columns).
#' @param use_cache If TRUE, uses cached data if available. Default is FALSE.
#'
#' @return Data frame with graduation rate data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all 2024 graduation data in tidy format
#' grad_2024 <- fetch_graduation(2024)
#'
#' # Get state-level only
#' grad_state <- fetch_graduation(2024, level = "state")
#'
#' # Get district-level in wide format
#' grad_district_wide <- fetch_graduation(2024, level = "district", tidy = FALSE)
#'
#' # See available years
#' get_available_grad_years()
#' }
fetch_graduation <- function(end_year,
                             level = "all",
                             tidy = TRUE,
                             use_cache = FALSE) {

  # Validate parameters
  level <- match.arg(level, c("all", "state", "district", "school"))

  # Normalize level to match data (capitalize first letter)
  level_normalized <- if (level == "all") "all" else {
    paste0(toupper(substr(level, 1, 1)), substr(level, 2, nchar(level)))
  }

  # Build cache key
  cache_key <- paste0("graduation_", end_year)

  # Check cache
  if (use_cache) {
    cached_data <- cache_read(cache_key)

    if (!is.null(cached_data)) {
      message("Using cached data for ", end_year)

      # Apply level filter
      if (level != "all") {
        cached_data <- cached_data[cached_data$type == level_normalized, ]
      }

      # Apply tidy/widen
      if (tidy) {
        return(tidy_graduation(cached_data, level))
      } else {
        return(widen_graduation(cached_data, level))
      }
    }
  }

  # Download raw data
  raw <- get_raw_graduation(end_year)

  # Process
  processed <- process_graduation(raw, end_year)

  # Validate
  validate_graduation(processed)

  # Write to cache
  cache_write(cache_key, processed)

  # Apply level filter
  if (level != "all") {
    processed <- processed[processed$type == level_normalized, ]
  }

  # Return tidy or wide format
  if (tidy) {
    return(tidy_graduation(processed, level))
  } else {
    return(widen_graduation(processed, level))
  }
}
