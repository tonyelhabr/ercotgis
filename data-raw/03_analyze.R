
library(tidyverse)
# data('commish', package = 'ercotgis')
# data('inactive', package = 'ercotgis')
# data('cancel', package = 'ercotgis')
data('details', package = 'ercotgis')
data('gis_status', package = 'ercotgis')

project_names <-
  details %>%
  group_by(inr) %>%
  # filter(date_report == max(date_report)) %>%
  slice_max(date_report, n = 1L) %>%
  ungroup() %>%
  select(inr, name, ie = interconnecting_entity)
project_names

.f_details <- function(f, ...) {
  details %>%
    group_by(inr) %>%
    filter(date_report == f(date_report, ...)) %>%
    ungroup()
}

latest_details <- .f_details(max)
earliest_details <- .f_details(min)

# .date_diff_frac <- function(d1, d2, unit) {
#   {d1 - d2} %>% lubridate::time_length(unit = unit)
# }

.month_diff_frac <- partial(.date_diff_frac, unit = 'months', ... = )

# .date_diff <- function(d1, d2, unit) {
#   {d1 - d2} %>% lubridate::time_length(unit = unit) %>% round() %>% as.integer()
# }

# .day_diff <- partial(.date_diff, unit = 'days', ... = )
.month_diff <- partial(.date_diff, unit = 'months', ... = )

dates <-
  details %>%
  select(inr, date_report, projected_cod) %>% # , fis_approved, fis_requested, approved_for_energization, approved_for_synchronization) %>%
  left_join(
    earliest_details %>%
      select(
        inr,
        date_report,
        projected_cod
      ) %>%
      rename_with(~sprintf('%s_earliest', .x), c(date_report, projected_cod))
  ) %>%
  arrange(inr, date_report) %>%
  group_by(inr) %>%
  mutate(across(c(date_report, projected_cod), list(prev = dplyr::lag))) %>%
  ungroup() %>%
  mutate(
    .date_report = .add_months(date_report, 1),
    months_since_last_report = .month_diff(date_report, date_report_prev),
    months_till_cod = .month_diff_frac(projected_cod, .date_report)
  ) %>%
  arrange(inr, date_report) %>%
  mutate(
    month_of_last_cod_change = case_when(
      (round(months_since_last_report) == 1) & (projected_cod != projected_cod_prev) ~ date_report,
      TRUE ~ NA_real_
    ),
    month_diff_of_last_cod_change = if_else(!is.na(month_of_last_cod_change), .month_diff_frac(projected_cod, projected_cod_prev), NA_real_)
  ) %>%
  group_by(inr) %>%
  fill(month_of_last_cod_change, month_diff_of_last_cod_change, .direction = 'down') %>%
  ungroup() %>%
  mutate(
    months_since_last_cod_change = .month_diff(.date_report, month_of_last_cod_change)
  ) %>%
  select(-c(date_report_prev, date_report_earliest, .date_report, months_since_last_report, month_of_last_cod_change), -matches('projected_cod_')) %>%
  arrange(inr, date_report)
dates
# dates %>% filter(months_since_last_cod_change > 0) # months after cod change
# dates %>% filter(months_since_last_cod_change == 0) # month of cod change

gis_status_minmax <-
  gis_status %>%
  # In case a unit is reported twice. This is definitely true for inactive units.
  group_by(gis_status, inr) %>%
  summarize(
    across(date, list(earliest = min, latest = max))
  ) %>%
  ungroup()
gis_status_minmax

# All inactive units.
# gis_status_minmax %>% filter(date_earliest != date_latest)

# These are un-suffixed/prefixed to indicate that they are not in the original "Details" worksheet.
gis_status_minmax_wide <-
  gis_status_minmax %>%
  select(-date_latest) %>%
  pivot_wider(
    names_from = gis_status,
    values_from = date_earliest,
    names_glue = '{gis_status}'
  ) %>%
  mutate(
    months_sync2comm = .month_diff_frac(commercial, synchronization),
    months_ener2sync = .month_diff_frac(synchronization, energization)
  )
gis_status_minmax_wide

inrs_reactivated <-
  gis_status_minmax_wide %>%
  filter(inactive < energization) %>%
  distinct(inr) %>%
  pull(inr)
inrs_reactivated

details_w_status <-
  details %>%
  left_join(
    gis_status_minmax_wide
  ) %>%
  inner_join(dates) %>%
  # Cancelled projects.
  filter(!(date_report >= cancelled & !is.na(cancelled))) %>%
  # Those that were made inactive and not re-activated. (This is actually missing those that have been re-activated but not yet energized, which is harder to catch.)
  filter(inr %in% inrs_reactivated | !(date_report >= inactive & !is.na(inactive))) %>%
  # Eliminate those kept in reports after they go commercial (should be 0 anyways).
  filter(!(date_report >= commercial & !is.na(commercial))) %>%
  mutate(
    months_fis = .month_diff_frac(fis_approved, fis_requested),
    months_fis2ener = .month_diff_frac(approved_for_energization, fis_approved),
    months_ener2sync = .month_diff_frac(approved_for_synchronization, approved_for_energization),
    .date_report = .add_months(date_report, 1),
    months_since_screen = .month_diff_frac(.date_report, screening_complete),
    months_since_ia = .month_diff_frac(.date_report, ia_signed),
    months_since_6_9_1 = .month_diff_frac(.date_report, meets_planning_guide_6_9_1),
    months_since_6_9 = .month_diff_frac(.date_report, meets_all_planning_guide_section_6_9),
    months_since_qsa = .month_diff_frac(.date_report, meets_planning_guide_qsa_prereqs),
    months_since_sync_apprv = .month_diff_frac(.date_report, approved_for_synchronization),
    months_since_ener_apprv = .month_diff_frac(.date_report, approved_for_energization),
    months_since_sync = .month_diff_frac(.date_report, synchronization),
    months_since_ener = .month_diff_frac(.date_report, energization)
  ) %>%
  group_by(inr, date_report) %>%
  rowwise() %>%
  mutate(
    months_since_sync = max(months_since_sync_apprv, months_since_sync),
    months_since_ener = max(months_since_ener_apprv, months_since_ener)
  ) %>%
  select(-.date_report, -matches('months_since.*apprv$')) %>%
  mutate(across(matches('^months_since_(sync|ener)$'), ~if_else(.x < 0, NA_real_, .x)))
details_w_status

details_w_status %>%
  select(months_since_ener, energization) %>%
  arrange(months_since_ener)
details_w_status %>% ggplot() + aes(x = months_fis) + geom_histogram()
# cols_date <- sapply(details_w_status, class) %>% enframe() %>% filter(value == 'Date') %>% pull(name)
# 18INR0079: Woodward I repower is weird
# # A tibble: 1 x 9
# date_report inr       name               months_since_sync_apprv months_since_sync approved_for_synchronization synchronization n_nona_sync_apprv n_nona_sync
# <date>      <chr>     <chr>                                <dbl>             <dbl> <date>                       <date>                             <int>       <int>
#   1 2019-11-01  18INR0079 Woodward I repower                    11.4             0.591 2018-12-18                   2019-11-13                            24          14
# details_w_status %>%
#   select(date_report, inr, name, matches('^months_since_sync'), approved_for_synchronization, synchronization) %>%
#   arrange(inr, date_report) %>%
#   group_by(inr) %>%
#   mutate(
#     n_nona_sync_apprv = sum(!is.na(months_since_sync_apprv)),
#     n_nona_sync = sum(!is.na(months_since_sync))
#   ) %>%
#   ungroup() %>%
#   filter(n_nona_sync > 0) %>%
#   filter(months_since_sync_apprv != months_since_sync)
# .month_diff_frac(lubridate::ymd('2021-03-01'), lubridate::ymd('2021-01-01'))
# .month_diff_frac(lubridate::ymd('2021-03-01'), .subtract_months(lubridate::ymd('2021-01-01'), 1))

# # Just for info purposes since we are actually going to predict months till sync
# sync2comm_agg <-
#   details_w_status %>%
#   # filter(date_report > '2020-01-01') %>%
#   # filter(months_since_ener < 1) %>%
#   filter(!is.na(commercial) & !is.na(energization)) %>%
#   select(inr, name, date_report, matches('$'), matches('^months_.*2')) %>%
#   group_by(inr) %>%
#   slice_max(date_report) %>%
#   ungroup() %>%
#   # skimr::skim(matches('^months_.*2comm'))
#   summarize(
#     n = n(),
#     across(
#       months_sync2comm,
#       list(
#         min = min,
#         mean = mean,
#         max = max,
#         median = median
#       ),
#       .names = '{fn}')
#   )
# sync2comm_agg
#
# actual_syncs <-
#   details_w_status %>%
#   filter(!is.na(approved_for_synchronization)) %>%
#   group_by(inr) %>%
#   slice_min(date_report) %>%
#   ungroup()
# actual_syncs

usethis::use_data(details_w_status, overwrite = TRUE)
