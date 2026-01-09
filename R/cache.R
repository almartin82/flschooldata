# ==============================================================================
# Caching Functions
# ==============================================================================
#
# This file contains functions for caching downloaded data locally to avoid
# repeated downloads from FLDOE.
#
# ==============================================================================

#' Get cache directory path
#'
#' Returns the path to the cache directory, creating it if necessary.
#' Uses rappdirs for cross-platform cache location.
#'
#' @return Path to cache directory
#' @keywords internal
get_cache_dir <- function() {
  cache_dir <- file.path(
    rappdirs::user_cache_dir("flschooldata"),
    "data"
  )

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  cache_dir
}


#' Get cache file path for given key
#'
#' @param key Cache key (e.g., "enr_tidy_2024" or "graduation_2024")
#' @return Full path to cache file
#' @keywords internal
get_cache_path <- function(key) {
  cache_dir <- get_cache_dir()
  file.path(cache_dir, paste0(key, ".rds"))
}


#' Check if cached data exists and is valid
#'
#' @param key Cache key (e.g., "enr_tidy_2024" or "graduation_2024")
#' @param max_age Maximum age in days (default 30)
#' @return TRUE if valid cache exists
#' @keywords internal
cache_exists <- function(key, max_age = 30) {
  cache_path <- get_cache_path(key)

  if (!file.exists(cache_path)) {
    return(FALSE)
  }

  # Check age
  file_info <- file.info(cache_path)
  age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))

  age_days <= max_age
}


#' Read data from cache
#'
#' @param key Cache key (e.g., "enr_tidy_2024" or "graduation_2024")
#' @return Cached data frame or NULL if not found
#' @keywords internal
cache_read <- function(key) {
  cache_path <- get_cache_path(key)

  if (!file.exists(cache_path)) {
    return(NULL)
  }

  readRDS(cache_path)
}


#' Write data to cache
#'
#' @param key Cache key (e.g., "enr_tidy_2024" or "graduation_2024")
#' @param df Data frame to cache
#' @return Invisibly returns the cache path
#' @keywords internal
cache_write <- function(key, df) {
  cache_path <- get_cache_path(key)
  saveRDS(df, cache_path)
  invisible(cache_path)
}


#' Clear the flschooldata cache
#'
#' Removes cached data files.
#'
#' @param key Optional cache key pattern to clear (e.g., "enr", "graduation", "2024").
#'   If NULL, clears all cached files.
#' @return Invisibly returns the number of files removed
#' @export
#' @examples
#' \dontrun{
#' # Clear all cached data
#' clear_cache()
#'
#' # Clear only 2024 data
#' clear_cache("2024")
#'
#' # Clear only enrollment data
#' clear_cache("enr")
#'
#' # Clear only graduation data
#' clear_cache("graduation")
#' }
clear_cache <- function(key = NULL) {
  cache_dir <- get_cache_dir()

  if (!is.null(key)) {
    # Clear files matching the key pattern
    files <- list.files(cache_dir, pattern = paste0(key, "\\.rds$"), full.names = TRUE)
  } else {
    # Clear all
    files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  }

  if (length(files) > 0) {
    file.remove(files)
    message(paste("Removed", length(files), "cached file(s)"))
  } else {
    message("No cached files to remove")
  }

  invisible(length(files))
}


#' Show cache status
#'
#' Lists all cached data files with their size and age.
#'
#' @return Data frame with cache information (invisibly)
#' @export
#' @examples
#' \dontrun{
#' cache_status()
#' }
cache_status <- function() {
  cache_dir <- get_cache_dir()
  files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(files) == 0) {
    message("Cache is empty")
    return(invisible(data.frame()))
  }

  info <- file.info(files)
  info$file <- basename(files)
  info$key <- gsub("\\.rds$", "", info$file)
  info$size_mb <- round(info$size / 1024 / 1024, 2)
  info$age_days <- round(as.numeric(difftime(Sys.time(), info$mtime, units = "days")), 1)

  result <- info[, c("key", "size_mb", "age_days")]
  result <- result[order(result$key), ]
  rownames(result) <- NULL

  print(result)
  invisible(result)
}
