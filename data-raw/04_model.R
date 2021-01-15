
library(tidyverse)
data('details_w_status')

.add_season_cols <- function(data, col) {
  col_sym <- col %>% sym()
  data %>%
    mutate(
      .mm = !!col_sym %>% lubridate::month(),
      !!sym(sprintf('%s_win', col)) := if_else(.mm %in% c(12, 1, 2), 1L, 0L),
      !!sym(sprintf('%s_spr', col)) := if_else(.mm %in% 3:5, 1L, 0L),
      !!sym(sprintf('%s_sum', col)) := if_else(.mm %in% 6:9, 1L, 0L),
      !!sym(sprintf('%s_fal', col)) := if_else(.mm %in% 10:11, 1L, 0L)
    ) %>%
    select(-.mm)
}

df <-
  details_w_status %>%
  # select(ginr_study_phase) %>%
  # filter(!(fuel %in% c('COA', 'COAL'))) %>%
  separate(ginr_study_phase, into = c('ss', 'fis', 'ia'), sep = '\\,\\s+') %>%
  mutate(
    across(fuel, ~if_else(.x == 'COAL', 'COA', .x)),
    # across(c(fuel, cdr_zone), factor),
    fuel_coa = if_else(fuel == 'COA', 1L, 0L),
    fuel_gas = if_else(fuel == 'GAS', 1L, 0L),
    fuel_nuc = if_else(fuel == 'NUC', 1L, 0L),
    fuel_oth = if_else(fuel == 'OTH', 1L, 0L),
    fuel_sol = if_else(fuel == 'SOL', 1L, 0L),
    fuel_win = if_else(fuel == 'WIN', 1L, 0L),
    cdr_zone_coast = if_else(cdr_zone == 'COASTAL', 1L, 0L),
    cdr_zone_east = if_else(cdr_zone == 'EAST-SPP', 1L, 0L),
    cdr_zone_houston = if_else(cdr_zone == 'HOUSTON', 1L, 0L),
    cdr_zone_north = if_else(cdr_zone == 'NORTH', 1L, 0L),
    cdr_zone_pan = if_else(cdr_zone == 'PANHANDLE', 1L, 0L),
    cdr_zone_south = if_else(cdr_zone == 'SOUTH', 1L, 0L),
    cdr_zone_west = if_else(cdr_zone == 'WEST', 1L, 0L),
    across(
      c(ss, fis),
      ~if_else(.x %>% str_detect('Completed'), 1L, 0L)
    ),
    across(ia, ~if_else(.x %>% str_detect('No'), 0L, 1L))
  ) %>%
  .add_season_cols('projected_cod') %>%
  .add_season_cols('date_report') %>%
  select(
    inr,
    name,
    date_report,
    cancelled, inactive, energization, synchronization, commercial,
    cdr_zone,
    fuel,
    ss, fis, ia,
    matches('^fuel_'),
    matches('^cdr_zone_'),
    capacity_mw,
    matches('^months_'),
    matches('_(spg|sum|fal|win)$')
  )
df

actual_final_status <-
  df %>%
  # filter(!is.na(inactive) | !is.na(synchronization) | !is.na(cancelled)) %>%
  select(inr, name, date_report, cancelled, inactive, synchronization) %>%
  group_by(inr) %>%
  slice_max(date_report) %>%
  rowwise() %>%
  mutate(date_max = max(cancelled, inactive, synchronization, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    across(
      date_max,
      list(final_status = ~case_when(
        .x == synchronization ~ 1L,
        !is.na(.x) & (.x == cancelled | .x == inactive) ~ 0L,
        # NAs are for projects that we don't have "labels" for yet.
        TRUE ~ NA_integer_
      )
      ),
      .names = '{fn}'
    )
  )
actual_final_status %>% filter(!is.na(final_status))
actual_final_status %>% count(final_status)

# # This has additional records because the Details start at a certain point (currently Jan. 2019), but don't go back further.
# actual_final_status_2 <-
#   gis_status_minmax_wide %>%
#   select(-matches('^months_.*$')) %>%
#   filter(!is.na(inactive) | !is.na(synchronization) | !is.na(cancelled)) %>%
#   group_by(inr) %>%
#   rowwise() %>%
#   mutate(date_max = max(cancelled, inactive, synchronization, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(
#     across(
#       date_max,
#       list(final_status = ~case_when(
#         .x == synchronization ~ 1L,
#         TRUE ~ 0L
#       )
#       ),
#       .names = '{fn}'
#     )
#   )
# actual_final_status_2
# actual_final_status_2 %>%
#   anti_join(actual_final_status %>% filter(!is.na(final_status)))
# actual_final_status %>%
#   filter(!is.na(final_status)) %>%
#   anti_join(actual_final_status_2)

cols_lst_by_unit <-
  c(
    col_y = 'final_status',
    cols_id = c('inr', 'name'),
    cols_extra = c('cancelled', 'inactive', 'energization', 'synchronization', 'commercialization')
  )

col_y_by_unit_sym <- cols_lst_by_unit$col_y %>% sym()
features_by_unit_mat <-
  model.matrix(
    ~.+0,
    data =
      actual_final_status %>%
      filter(!is.na(cols_lst$col_y))
      left_join(df) %>%
      select(-one_of(c(cols_lst$col_y, cols_lst$cols_id, cols_lst$cols_extra)))
  )
features_mat

cols_lst_by_unit_month <-
  c(
    col_y = 'months_till_sync',
    cols_id = c('date_report', 'inr', 'name'),
    cols_extra = c('cancelled', 'inactive', 'energization', 'synchronization', 'commercialization')
  )


# unused ----

