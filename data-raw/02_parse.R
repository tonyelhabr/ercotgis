
library(tidyverse)

paths <-
  fs::dir_ls(
    'c:/users/aelhabr/downloads',
    regexp = 'GIS_.*'
  ) %>%
  tibble(path = .) %>%
  mutate(across(path, as.character)) %>%
  filter(str_detect(basename(path), '^GIS_'))
paths

# .import_xl_robustly <- safely(import_xl_cleanly, otherwise = NULL)
.import_xl_robustly <- quietly(possibly(import_xl_cleanly, otherwise = NULL))

.convert_month <- function(month) {
  which(month == month.name) %>% as.integer()
}
.convert_month_v <- Vectorize(.convert_month)

.get_valid_sheet_type <- function() {
  c('Commissioning', 'Inactive', 'Cancel', 'Details')
}

.switch_skip <- function(sheet_type) {
  .validate_sheet_type(sheet_type)
  ifelse(sheet_type != 'Details', 7, 28)
}

.switch_sheet_type <- function(sheet_type) {
  valid_sheet_types <- .get_valid_sheet_type()
  .validate_sheet_type(sheet_type)
  lst <- setNames(c('commish', 'inactive', 'cancel', 'details'), valid_sheet_types)
  lst[[sheet_type]]
}

.switch_col_names <- function(sheet_type) {
  valid_sheet_types <- .get_valid_sheet_type()
  .validate_sheet_type(sheet_type)
  ifelse(sheet_type == 'Details', FALSE, TRUE)
}

.switch_col_date <- function(sheet_type) {
  valid_sheet_types <- .get_valid_sheet_type()
  .validate_sheet_type(sheet_type)
  lst <- setNames(sprintf('%s_date', c('approval', 'inactive', 'cancel')), setdiff(valid_sheet_types, 'Details'))
  lst[[sheet_type]]
}

.validate_sheet_type <- function(x = .get_valid_sheet_type(), ...) {
  match.arg(x, ...)
}

.postprocess_gis_sheet <- function(data, sheet_type, date) {

  if(sheet_type != 'Details') {
    col_date <- .switch_col_date(sheet_type)
    # browser()
    res <-
      data %>%
      janitor::clean_names() %>%
      mutate_all(as.character) %>%
      filter(!is.na(project_name)) %>%
      select(-matches('^x')) %>%
      rename(date = !!sym(col_date)) %>%
      mutate(across(date, ~lubridate::ymd_hms(.x) %>% lubridate::date()))
    return(res)
  }

  nms <-
    c('inr', 'name', 'ginr_study_phase', 'interconnecting_entity', 'poi_location', 'county', 'cdr_zone', 'projected_cod', 'fuel', 'tech', 'capacity_mw', 'change_indicator', 'approval_date_for_submission_of_proof_of_site_control', 'screening_start', 'screening_complete', 'fis_requested', 'fis_approved', 'economic_study_required', 'ia_signed', 'fs_n_to_proceed_provided', 'air_permit', 'ghg_permit', 'water_avail', 'meets_planning_guide_6_9_1', 'meets_all_planning_guide_section_6_9', 'meets_planning_guide_qsa_prereqs', 'construction_start', 'construction_end', 'approved_for_energization', 'approved_for_synchronization', 'comment')

  if(date < lubridate::ymd('2020-06-01')) {
    nms <- setdiff(nms, c('economic_study_required', 'approval_date_for_submission_of_proof_of_site_control'))
  } else if (date == lubridate::ymd('2020-06-01')) {
    nms <- setdiff(nms, 'economic_study_required')
  } else if (date == lubridate::ymd('2020-09-01')) {
    nms <- setdiff(nms, 'fis_requested')
  }

  n_col <- data %>% ncol()
  n_nms <- nms %>% length()
  is_equal <- n_col == n_nms
  is_one_less <- (n_col + 1) == n_nms
  if(!(is_equal | is_one_less)) {
    .display_error('Number of names ({n_nms}) is not equal to or one more than names in `data` ({n_col}) for `date = "{date}"`.')
  }
  if(is_one_less) {
    nms <- setdiff(nms, 'comment')
  }
  names(data) <- nms
  res <- data %>% mutate_all(as.character)
  res
}

import_by_sheet <- function(sheet_type = .get_valid_sheet_types(), skip = .switch_skip(sheet_type), col_names = .switch_col_names(sheet_type), ...) {
  .validate_sheet_type(sheet_type)
  # sheet_type <- 'Details'
  # skip <- 28
  .display_info('Importing data or `sheet_type = "{sheet_type}"`.')
  res_init <-
    paths %>%
    # tail(5) %>%
    mutate(
      data =
        map(
          path,
          ~.import_xl_robustly(
            .x,
            # Sheets used to have longer names, but recent ones are just "Commissioning"
            sheet = str_subset(readxl::excel_sheets(.x), !!sheet_type), skip = !!skip, col_names = !!col_names
          ) # , ...)
        )
    ) %>%
    unnest_wider(data)

  if(!('result' %in% names(res_init)) ) {
    .display_error('Could not import any month\'s data for `sheet_type = "{sheet_type}"`.')
  }

  res <-
    res_init %>%
    mutate(is_bad = map_lgl(result, is.null)) %>%
    filter(!is_bad) %>%
    mutate(
      name = path %>% basename() %>% tools::file_path_sans_ext(),
      year_report = name %>% str_replace('(^.*_)(20[0-9]{2}$)', '\\2') %>% as.integer(),
      month_report = name %>% str_replace_all('(^GIS_Report_)(.*)(_20[0-9]{2}$)', '\\2') %>% .convert_month_v(),
      date_report = sprintf('%04d-%02d-01', year_report, month_report) %>% lubridate::ymd(),
      result = pmap(list(result, date_report), ~.postprocess_gis_sheet(data = ..1, date = ..2, sheet_type = !!sheet_type))
    ) %>%
    arrange(date_report) %>%
    select(year_report, month_report, date_report, result) %>%
    unnest(result)
  res
}

assign_sheet_gis <- function(sheet_type, name = .switch_sheet_type(sheet_type), ...) {
  res <- import_by_sheet(sheet_type = sheet_type, ...)
  .do_assign(res, name = name, envir = .GlobalEnv)
  res
}

valid_sheet_types <- .get_valid_sheet_type()
assign_sheet_gis_robustly <- possibly(assign_sheet_gis, otherwise = NULL)
walk(valid_sheet_types, assign_sheet_gis_robustly)
walk('Details', assign_sheet_gis_robustly)

details <-
  details %>%
  # 'construction_start', 'construction_end' should be dropped here.
  janitor::remove_empty(which = 'cols') %>%
  drop_na(inr, projected_cod) %>%
  # arrange(inr, date_report) %>%
  mutate(idx = row_number()) %>%
  relocate(idx)
z <- details %>% select(idx, date_report, inr, name, projected_cod) %>% arrange(inr, date_report) %>% head(10)
z

cols_date <- c('projected_cod', 'approval_date_for_submission_of_proof_of_site_control', 'screening_start', 'screening_complete', 'fis_requested', 'fis_approved', 'ia_signed', 'air_permit', 'ghg_permit', 'water_avail', 'meets_planning_guide_6_9_1', 'meets_all_planning_guide_section_6_9', 'meets_planning_guide_qsa_prereqs', 'construction_start', 'construction_end', 'approved_for_energization', 'approved_for_synchronization')

details_date_init <-
  details %>%
  mutate(
    across(
      any_of(cols_date),
      list(
        a = ~as.double(.x) %>% janitor::excel_numeric_to_date(),
        b = lubridate::ymd,
        c = lubridate::mdy,
        d = ~lubridate::ymd_hms(.x) %>% lubridate::date()
      )
    )
  )

rgx_suffix <- '[abcd]$'
details_date <-
  details_date_init %>%
  select(idx, matches(sprintf('_%s', rgx_suffix))) %>%
  pivot_longer(
    -idx
  ) %>%
  mutate(
    across(
      name,
      list(
        prefix = ~.x %>% str_remove(sprintf('_%s', rgx_suffix)),
        prefix_idx = ~.x %>% str_replace(sprintf('(^.*)(%s)', rgx_suffix), '\\2')
      ),
      .names = '{fn}'
    )
  ) %>%
  select(-name) %>%
  pivot_wider(
    names_from = prefix_idx,
    values_from = value
  ) %>%
  mutate(value = coalesce(a, b, c, d)) %>%
  select(-matches(sprintf('^%s', rgx_suffix))) %>%
  # pivot_wider(names_from = prefix, names_glue = '{prefix}_impute', values_from = value)
  pivot_wider(names_from = prefix, values_from = value)
details_date

details %>% select(any_of(names(z))) %>% semi_join(z %>% select(-projected_cod))

# .coalesce_col <- function(data, col) {
#   col_other <- sprintf('%s_impute', col)
#   col_sym <- col %>% sym()
#   col_other_sym <- col_other %>% sym()
#   data %>%
#     select(idx, !!col_sym, !!col_other_sym) %>%
#     mutate(!!col_sym := coalesce(!!col_sym, as.character(!!col_other_sym)) %>% lubridate::ymd())
# }
# cols_date %>%
#   map(~.coalesce_col(details_clean, col = .x)) %>%
#   reduce(bind_rows)

details <-
  details %>%
  mutate(
    across(capacity_mw, as.double),
    across(fs_n_to_proceed_provided, ~if_else(str_detect(.x, 'es'), TRUE, FALSE))
  ) %>%
  select(-any_of(cols_date)) %>%
  left_join(details_date)
details
# details %>% filter(is.na(projected_cod))
usethis::use_data(details, commish, cancel, inactive, overwrite = TRUE)
