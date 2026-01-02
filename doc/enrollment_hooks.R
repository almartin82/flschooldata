## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 8,
  fig.height = 5,
  eval = nzchar(Sys.getenv("FLSCHOOLDATA_VIGNETTES")) || interactive()
)

# To build vignettes with live data, set FLSCHOOLDATA_VIGNETTES=true
# or run interactively in R

## ----load-packages------------------------------------------------------------
# library(flschooldata)
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# 
# theme_set(theme_minimal(base_size = 14))

## ----statewide-trend----------------------------------------------------------
# enr <- fetch_enr_multi(2014:2025)
# 
# state_totals <- enr |>
#   filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
#   select(end_year, n_students) |>
#   mutate(change = n_students - lag(n_students),
#          pct_change = round(change / lag(n_students) * 100, 2))
# 
# state_totals

## ----statewide-chart----------------------------------------------------------
# ggplot(state_totals, aes(x = end_year, y = n_students)) +
#   geom_line(linewidth = 1.2, color = "#FF6600") +
#   geom_point(size = 3, color = "#FF6600") +
#   scale_y_continuous(labels = scales::comma) +
#   labs(
#     title = "Florida Public School Enrollment (2014-2025)",
#     subtitle = "Nearly 3 million students in the Sunshine State",
#     x = "School Year (ending)",
#     y = "Total Enrollment"
#   )

## ----top-districts------------------------------------------------------------
# enr_2025 <- fetch_enr(2025)
# 
# top_10 <- enr_2025 |>
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
#   arrange(desc(n_students)) |>
#   head(10) |>
#   select(district_name, n_students)
# 
# top_10

## ----top-districts-chart------------------------------------------------------
# top_10 |>
#   mutate(district_name = forcats::fct_reorder(district_name, n_students)) |>
#   ggplot(aes(x = n_students, y = district_name)) +
#   geom_col(fill = "#FF6600") +
#   scale_x_continuous(labels = scales::comma) +
#   labs(
#     title = "Florida's 10 Largest School Districts (2025)",
#     x = "Total Enrollment",
#     y = NULL
#   )

## ----covid-impact-------------------------------------------------------------
# covid <- enr |>
#   filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL",
#          end_year >= 2019) |>
#   select(end_year, n_students) |>
#   mutate(change = n_students - lag(n_students))
# 
# covid

## ----demographics-------------------------------------------------------------
# demographics <- enr_2025 |>
#   filter(is_state, grade_level == "TOTAL",
#          subgroup %in% c("hispanic", "white", "black", "asian", "multiracial")) |>
#   mutate(pct = round(pct * 100, 1)) |>
#   select(subgroup, n_students, pct) |>
#   arrange(desc(n_students))
# 
# demographics

## ----demographics-chart-------------------------------------------------------
# demographics |>
#   mutate(subgroup = forcats::fct_reorder(subgroup, n_students)) |>
#   ggplot(aes(x = n_students, y = subgroup, fill = subgroup)) +
#   geom_col(show.legend = FALSE) +
#   geom_text(aes(label = paste0(pct, "%")), hjust = -0.1) +
#   scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
#   scale_fill_brewer(palette = "Set2") +
#   labs(
#     title = "Florida Student Demographics (2025)",
#     subtitle = "Hispanic students are now the plurality",
#     x = "Number of Students",
#     y = NULL
#   )

## ----central-florida----------------------------------------------------------
# central_fl <- enr |>
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
#          grepl("Orange|Osceola|Polk", district_name),
#          end_year %in% c(2014, 2025)) |>
#   group_by(district_name) |>
#   summarize(
#     y2014 = n_students[end_year == 2014],
#     y2025 = n_students[end_year == 2025],
#     pct_change = round((y2025 / y2014 - 1) * 100, 1),
#     .groups = "drop"
#   ) |>
#   arrange(desc(pct_change))
# 
# central_fl

## ----south-florida------------------------------------------------------------
# south_fl <- enr |>
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
#          grepl("Broward|Miami-Dade", district_name),
#          end_year >= 2018) |>
#   select(end_year, district_name, n_students) |>
#   pivot_wider(names_from = district_name, values_from = n_students)
# 
# south_fl

## ----south-florida-chart------------------------------------------------------
# enr |>
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
#          grepl("Broward|Miami-Dade", district_name),
#          end_year >= 2018) |>
#   ggplot(aes(x = end_year, y = n_students, color = district_name)) +
#   geom_line(linewidth = 1.2) +
#   geom_point(size = 2) +
#   scale_y_continuous(labels = scales::comma) +
#   labs(
#     title = "South Florida's Enrollment Decline",
#     x = "School Year",
#     y = "Enrollment",
#     color = "District"
#   )

## ----virtual------------------------------------------------------------------
# virtual <- enr_2025 |>
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
#          grepl("Virtual|FLVS", district_name, ignore.case = TRUE)) |>
#   select(district_name, n_students)
# 
# virtual

## ----kindergarten-------------------------------------------------------------
# k_trend <- enr |>
#   filter(is_state, subgroup == "total_enrollment",
#          grade_level %in% c("K", "01", "05", "09"),
#          end_year >= 2019) |>
#   select(end_year, grade_level, n_students) |>
#   pivot_wider(names_from = grade_level, values_from = n_students)
# 
# k_trend

## ----black-enrollment---------------------------------------------------------
# black_enr <- enr_2025 |>
#   filter(is_state, grade_level == "TOTAL", subgroup == "black") |>
#   select(subgroup, n_students, pct)
# 
# black_enr

## ----county-count-------------------------------------------------------------
# county_count <- enr_2025 |>
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
#   summarize(
#     n_districts = n_distinct(district_name),
#     total_students = sum(n_students, na.rm = TRUE)
#   )
# 
# county_count

