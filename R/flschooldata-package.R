#' flschooldata: Fetch and Process Florida School Data
#'
#' Downloads and processes school data from the Florida Department of Education
#' (FLDOE). Provides functions for fetching enrollment data including membership
#' by school, grade, and race/ethnicity, and transforming it into tidy format
#' for analysis.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{fetch_enr}}}{Fetch enrollment data for a school year}
#'   \item{\code{\link{fetch_enr_multi}}}{Fetch enrollment data for multiple years}
#'   \item{\code{\link{tidy_enr}}}{Transform wide data to tidy (long) format}
#'   \item{\code{\link{id_enr_aggs}}}{Add aggregation level flags}
#'   \item{\code{\link{enr_grade_aggs}}}{Create grade-level aggregations}
#' }
#'
#' @section Cache functions:
#' \describe{
#'   \item{\code{\link{cache_status}}}{View cached data files}
#'   \item{\code{\link{clear_cache}}}{Remove cached data files}
#' }
#'
#' @section ID System:
#' Florida uses a county-based district system with:
#' \itemize{
#'   \item District Codes: 2 digits (01-67 for counties, 71-75 for special districts)
#'   \item School Numbers: 4 digits (0001-9899 for regular schools)
#'   \item Combined ID: District code + School number (e.g., "13-0021" for Miami-Dade)
#' }
#'
#' @section Data Sources:
#' Data is sourced from the Florida Department of Education:
#' \itemize{
#'   \item Students Data: \url{https://www.fldoe.org/accountability/data-sys/edu-info-accountability-services/pk-12-public-school-data-pubs-reports/students.stml}
#'   \item FTE History: \url{https://www.fldoe.org/finance/fl-edu-finance-program-fefp/fte-info/student-enrollment.stml}
#'   \item Master School ID: \url{https://eds.fldoe.org/EDS/MasterSchoolID/}
#' }
#'
#' @section Data Availability:
#' \itemize{
#'   \item Membership by Grade/Race: 2014-present (current format)
#'   \item FTE Enrollment: 2008-present (Excel format)
#'   \item Historical archive data goes back further but format varies
#' }
#'
#' @docType package
#' @name flschooldata-package
#' @aliases flschooldata
#' @keywords internal
"_PACKAGE"

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
