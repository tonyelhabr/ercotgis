
library(tidyverse)

# 2019 Jan - present ----
today <- lubridate::today()
today_minus_month <- today %m-% lubridate::period(1, unit = 'month')
report_names <-
  crossing(
    m = 1:12,
    y = 2019:lubridate::year(today)
  ) %>%
  mutate(
    mmm = m %>% lubridate::month(label = TRUE, abbr = FALSE),
    date = sprintf('%04d-%02d-01', y, m) %>% lubridate::ymd()
  ) %>%
  filter(date <= today_minus_month) %>%
  transmute(report_name = sprintf('GIS_Report_%s_%s', mmm, y)) %>%
  pull(report_name)
report_names

base_url <- 'http://mis.ercot.com/misapp/GetReports.do?reportTypeId=15933&reportTitle=GIS%20Report&showHTMLView=&mimicKey'
page <- base_url %>% xml2::read_html()
# page %>% rvest::html_nodes('a') %>% rvest::html_attr('href')
links <- page %>% rvest::html_nodes('tr tr td div a') %>% rvest::html_attr('href')
names <- page %>% rvest::html_nodes('tr tr')
labs <- names %>% rvest::html_text() %>% str_replace_all('(^.*)(Co[-]located.*|GIS.*)([.]xlsx.*xlsx.*$)', '\\2') %>% str_trim()
labs
dates <- names %>% rvest::html_text() %>% str_replace_all('(^RPT[.]00015933[.]0000000000000000.)([0-9]{8})(.*$)', '\\2')
idx_keep <- labs %>% str_which('^(Co[-]|GIS_)')
labs_keep <- labs[idx_keep]
labs_keep
links_keep <- links
dates_keep <- dates[idx_keep] %>% lubridate::ymd()

report_links <- map_dfr(
  report_names,
  ~ {
    idx <- str_which(labs_keep, .x)
    # idx_link <- which(.x == report_names)
    my <- str_remove(.x, '^GIS_Report_')
    mmm <- str_remove(my, '_.*$')
    m <- which(mmm == month.name)
    y <- my %>% str_remove('^.*_') %>% as.integer()
    tibble(
      report_name = .x,
      month = m,
      year = y,
      actual_lab = labs_keep[idx],
      report_date = dates_keep[idx],
      n = length(idx),
      # idx = idx,
      url = links_keep[idx]
    )
  }
) %>%
  # Only keep the latest GIS report if there were revisions.
  group_by(report_name) %>%
  filter(row_number(desc(report_date)) == 1L) %>%
  ungroup() %>%
  # Drop the co-located battery reports
  filter(report_name %>% str_detect('^GIS')) %>%
  mutate(across(url, ~sprintf('http://mis.ercot.com/%s', .x)))
report_links

.download_xlsx <-
  function(url,
           file = str_remove(url, '^.*lookupId[=]'),
           ext = 'xlsx',
           # dir = getwd(),
           dir = 'c:/users/aelhabr/downloads/',
           path = file.path(dir, sprintf('%s.%s', file, ext)),
           ...,
           quiet = TRUE,
           mode = 'wb',
           overwrite = TRUE) {
    .display_info('Trying to download from {url}.')
    fs::dir_create(dir)
    status <-
      download.file(
        url = url,
        destfile = path,
        quiet = quiet,
        mode = mode
      )
    path_exists <- file.exists(path)
    if (!path_exists) {
      .display_warning('No files downloaded from {url}.')
      return(NA_character_)
    }
    path
  }

report_links %>%
  # slice(1) %>%
  mutate(res = map2(url, report_name, ~.download_xlsx(url = ..1, file = ..2)))
report_links

# 2018 Aug - 2018 Dec ----
url_base <- 'http://www.ercot.com/gridinfo/resource/'
page <- sprintf('%s/2018', url_base) %>% xml2::read_html()
links <- page %>% rvest::html_nodes('a') %>% rvest::html_attr('href')
links_keep <-
  links %>%
  str_subset('GIS_Report')
links_keep

# 2017 Feb - 2018 Jul ----
# 2017 Feb is missing Interconnecting Entity in column C of IA Table sheet and also does not have a "New and Cancelled Units" sheet.
# 2017 Mar Revised's "New and Cancelled Units" sheet is just "New Units"
# 2017 Apr and beyond look the same I think
links_keep_2018 <-
  links %>%
  str_subset('GIS_REPORT')
links_keep_2018

page <- sprintf('%s/2017', url_base) %>% xml2::read_html()
links <- page %>% rvest::html_nodes('a') %>% rvest::html_attr('href')
links_keep_2017 <-
  links %>%
  str_subset('GIS_')
links_keep_2017

idx_last <- links_keep_2017 %>% str_which('March_2017_Revised')
idx_seq <- 1:idx_last
links_keep_2017 <- links_keep_2017[idx_seq]
