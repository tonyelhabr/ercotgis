
library(tidyverse)
data('commish', package = 'ercotgis')
data('inactive', package = 'ercotgis')
data('cancel', package = 'ercotgis')
data('details', package = 'ercotgis')

project_names <-
  details %>%
  group_by(inr) %>%
  filter(date_report == max(date_report)) %>%
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
first_details <- .f_details(min)

.month_diff <- function(m1, m2) {
  {m1 - m2} %>% lubridate::time_length(unit = 'months') %>% round() %>% as.integer()
}

projected_cods <-
  details %>%
  select(inr, fuel, date_report, projected_cod) %>%
  left_join(
    first_details %>%
      select(
        inr,
        date_report,
        projected_cod
      ) %>%
      rename_with(~sprintf('%s_earliest', .x), c(date_report, projected_cod))
  ) %>%
  left_join(project_names) %>%
  arrange(name, date_report) %>%
  mutate(across(projected_cod, list(prev = dplyr::lag))) %>%
  mutate(
    diff_cod = -.month_diff(projected_cod, date_report),
    diff_cod_prev = .month_diff(projected_cod, projected_cod_prev)
  )
projected_cods

gis_status_latest <-
  list(
    commish %>%
      select(inr, date_report, commissioning_category) %>%
      mutate(gis_status = 'commissioned'),
    inactive %>%
      select(inr, date_report) %>%
      mutate(gis_status = 'inactive'),
    cancel %>%
      select(inr, date_report) %>%
      mutate(gis_status = 'cancelled')
  ) %>%
  reduce(bind_rows) %>%
  group_by(gis_status, commissioning_category, inr) %>%
  filter(row_number(desc(date_report)) == 1L) %>%
  ungroup() %>%
  rename_with(~sprintf('%s_latest', .x), -c(inr))
gis_status_latest

details_w_status_latest <-
  details %>%
  inner_join(gis_status_latest) %>%
  # These are already in details
  inner_join(projected_cods %>% select(-fuel, -name, -ie))
details_w_status_latest

.subtract_months <- function(x, n = 1) {
  res <- x %m-% lubridate::period(n, unit = 'month')
  res
}

y_info <-
  details_w_status_latest %>%
  # gis_status_latest %>% # Can't use this even tho it's simpler cuz we will need info in details.
  filter(is.na(commissioning_category_latest) | commissioning_category_latest == 'Synchronization Approved by ERCOT') %>%
  mutate(
    y = if_else(is.na(commissioning_category_latest), 0L, 1L) # ,
    # month_slice = date_report_latest %>% .subtract_months(7)
  ) %>%
  select(inr, y, date_y = date_report_latest, commissioning_category_latest) %>%
  # Some are listed in multiple months?
  group_by(inr, y, commissioning_category_latest) %>%
  filter(row_number(date_y) == 1L) %>%
  ungroup() %>%
  # If a unit went inactive, but then came back, take the later status.
  group_by(inr) %>%
  filter(row_number(desc(date_y)) == 1L) %>%
  distinct()
y_info

# Prelim checking on how to join this back
y_info %>%
  mutate(date_report = date_y %>% .subtract_months(7)) %>%
  full_join(details %>% select(inr, fuel, name, date_report)) # %>%
# filter(is.na(name))

# We don't have all of these since some go back to earlier times (i.e. pre-2019) I think
details %>%
  inner_join(y_info %>% rename(date_report = date_y))

.add_season_col <- function(data, col, suffix = 'season') {
  col_sym <- col %>% sym()
  col_out <- sprintf('%s_%s', col, suffix)
  col_out_sym <- col_out %>% sym()
  data %>%
    mutate(
      .mm = !!col_sym %>% lubridate::month(),
      !!col_out_sym := case_when(
        .mm %in% c(12, 1, 2) ~ 'winter',
        .mm %in% 3:5 ~ 'spring',
        .mm %in% 6:9 ~ 'summer',
        .mm %in% 10:11 ~ 'fall'
      ) %>%
        factor()
    ) %>%
    select(-.mm)
}

dt1 <- details %>% mutate(..date_report = date_report) %>% data.table::as.data.table()
dt2 <- y_info %>% mutate(.date_report = date_y) %>% data.table::as.data.table()

latest_details_solar <-
  latest_details %>%
  filter(fuel == 'SOL') %>%
  filter(date_report == max(date_report)) %>%
  # Take out stuff that's already been commissioned.
  filter(projected_cod > date_report)
latest_details_solar
# latest_details_solar %>% filter(name %>% str_detect('Wagyu')) %>% glimpse()

# y_info %>%
#   inner_join(
#     latest_details_solar %>%
#       filter(projected_cod < date_report)
#   )

prep_data <- function(data) {
  data %>%
    select(
      inr,
      name,
      ginr_study_phase,
      cdr_zone,
      fuel,
      capacity_mw,
      month_x,
      date_y,
      y
    ) %>%
    .add_season_col('date_y') %>%
    .add_season_col('month_x') %>%
    separate(ginr_study_phase, into = c('ss', 'fis', 'ia'), sep = '\\,\\s+') %>%
    filter(!(fuel %in% c('COA', 'COAL'))) %>%
    mutate(
      months_till = .month_diff(date_y, month_x),
      across(
        c(ss, fis),
        ~if_else(.x %>% str_detect('Completed'), 1L, 0L)
      ),
      across(ia, ~if_else(.x %>% str_detect('No'), 0L, 1L)),
      across(c(y, ss, fis, ia, cdr_zone), factor)
    )
}

df <-
  dt1[dt2, on=.(inr = inr, date_report < .date_report), allow.cartesian = TRUE] %>%
  as_tibble() %>%
  select(-date_report) %>%
  rename(month_x = ..date_report) %>%
  # Honestly not sure why there are so many duplicates. I do need to allow cartesian since one date_y-inr pair will likely have many records.
  distinct() %>%
  prep_data() %>%
  # No 20INR0294 here.
  drop_na()
df

unlabelled <-
  latest_details_solar %>%
  rename(month_x = date_report) %>%
  # `y = 0` is just so that we have a non-NA value.
  # `date_y` is an approximation of when they will synce, given a baseline assumption of 2 months prior to projected cod
  mutate(date_y = .subtract_months(projected_cod, 0), y = 0) %>%
  prep_data()
unlabelled

col_y <- 'y'
fmla <- paste0(col_y, ' ~ cdr_zone + month_x_season + date_y_season + months_till + ss + fis + ia + fuel') %>% as.formula()
set.seed(42)
# split <- df %>% rsample::initial_split(strata = all_of(col_y))
# trn <- split %>% rsample::training()
# tst <- split %>% rsample::testing()
trn <- df
tst <- unlabelled

rec <-
  recipes::recipe(fmla, data = trn) %>%
  # recipes::step_dummy(quarter, down) %>%
  # recipes::update_role(
  #   # inr, name, date_report, date_report_latest,
  #   new_role = 'id'
  # ) %>%
  recipes::step_dummy(month_x_season, date_y_season, ss, fis, ia, cdr_zone, fuel)
rec

# tuning ----
spec <-
  parsnip::decision_tree(
    cost_complexity = tune::tune(),
    tree_depth = tune::tune(),
    min_n = tune::tune()
  ) %>%
  parsnip::set_mode('classification') %>%
  parsnip::set_engine('rpart')
spec
folds <- trn %>% rsample::vfold_cv(strata = all_of(col_y))

grid_params <-
  # dials::grid_regular(
  dials::grid_max_entropy(
    dials::cost_complexity(),
    # dials::tree_depth(),
    dials::tree_depth(range = c(4, 6)),
    # dials::min_n(),
    dials::min_n(range = c(5, 20)),
    size = 20
  )
grid_params

require(yardstick)
res_grid <-
  tune::tune_grid(
    spec,
    fmla,
    resamples = folds,
    control = tune::control_grid(verbose = TRUE),
    grid = grid_params,
  )
res_grid
pacman::p_unload('yardstick')

res_grid %>% autoplot()
metrics <- res_grid %>% tune::collect_metrics()
metrics
metrics %>%
  # filter(.metric == 'roc_auc') %>%
  arrange(-mean)

res_grid %>% tune::select_best('roc_auc')
res_grid %>% tune::select_best('accuracy')
fit_best <- res_grid %>% tune::select_best('roc_auc')
fit_best

spec_final <- tune::finalize_model(spec, fit_best)
spec_final

fit_final <- spec_final %>% parsnip::fit(formula = fmla, data = trn)
fit_final

# fit_final$fit %>% ggparty::ggparty()
fit_final$fit %>% rpart.plot::rpart.plot()
fit_final$fit %>% rpart.plot::rpart.rules()

fit_final %>% vip::vip(num_features = 100L)

# preds_tst <-
#   fit_final %>%
#   predict(new_data = tst, type = 'prob') %>%
#   bind_cols(tst)
# preds_tst
#
# preds_tst %>% ggplot() + aes(x = .pred_1) + geom_histogram()

# simple ----
spec <-
  parsnip::decision_tree(
    # cost_complexity = 0.00000007497,
    tree_depth = 7,
    min_n = 6
  ) %>%
  parsnip::set_mode('classification') %>%
  parsnip::set_engine('rpart')
spec

wf <-
  workflows::workflow() %>%
  workflows::add_recipe(rec) %>%
  workflows::add_model(spec)

fit <- parsnip::fit(wf, trn)
fit

fit_wf <- fit %>% workflows::pull_workflow_fit()
# fit_wf$fit %>% ggparty::ggparty()
# fit_wf$fit %>% rpart.plot::rpart.plot()
fit_wf$fit$call

# fit_rp <-
#   rpart::rpart(
#     formula = fmla,
#     data = trn,
#     # cost = 0.00000007497,
#     maxdepth = 7,
#     minsplit = 6
#   )
# fit_rp$call
# fit_rp %>% rpart.plot::rpart.plot(cex = 0.9)
rules_init <- fit_final$fit %>% rpart.plot::rpart.rules()
nms <- c('p', sprintf('x%02d', 1:(ncol(rules_init) - 1)))
rules <- rules_init %>% set_names(nms) %>% as_tibble()
rules
rules_united <-
  rules %>%
  unite(matches('.*'), col = 'rule', sep = ' ') %>%
  mutate(
    across(
      rule,
      ~.x %>% str_replace_all(
        c(
          'ia ' = 'has interconnection agreement (IA) ',
          'ss' = 'has completed screening study (SS)',
          'fs' = 'has completed full interconnection study (FIS)',
          'cdr_zone' = 'CDR zone',
          'date_y_season' = 'projected COD season',
          'month_x_season' = 'current season',
          'months_till' = 'months between current month and projected COD month',
          'SOL' = 'solar',
          'GAS' = 'gas',
          'WIN' = 'wind',
          'OTH' = 'battery'
        )
      ) %>%
        str_squish()
    ),
    p = rule %>% str_remove_all('\\swhen.*$') %>% as.numeric()
  ) %>%
  mutate(idx_rule = row_number()) %>%
  relocate(idx_rule, p)
rules_united

# pred with rpart ----
.pred <- function(set, nm = deparse(substitute(set))) {
  fit_final$fit %>%
    predict(newdata = set) %>%
    as_tibble() %>%
    set_names(sprintf('.pred_%d', 0:1)) %>%
    bind_cols(set) %>%
    mutate(set = !!nm)
}

preds <-
  bind_rows(
    .pred(trn),
    .pred(tst)
  )
preds

preds_filt <-
  preds %>%
  filter(set == 'tst') %>%
  arrange(date_y)
preds_filt
preds_filt %>% count(.pred_1)

preds_export <-
  preds_filt %>%
  mutate(
    p = .pred_1 %>% round(2)
  ) %>%
  left_join(
    rules_united %>%
      group_by(p) %>%
      summarize(
        n = n(),
        rule = paste0(rule, collapse = ' OR ')
        # rule = ifelse(n > 1L, 'More than 1 rule', rule)
      ) %>%
      ungroup()
  ) %>%
  mutate(
    across(
      p,
      list(
        grp = ~case_when(
          .x >= 0.95 ~ 'Extremely Likely',
          .x >= 0.85 ~ 'Highly Likely',
          .x >= 0.6 ~ 'Somewhat Likely',
          .x > 0.3 ~ 'Uncertain',
          .x > 0.05 ~ 'Unlikely',
          TRUE ~ 'No Chance OR Not Enough Information'
        )
      )
    )
  ) %>%
  select(
    `INR` = inr,
    `Name` = name,
    `Probability of Completion` = p,
    `Probability Group` = p_grp,
    `Projected COD` = date_y,
    `Capacity MW` = capacity_mw,
    `Has IA` = ia,
    `Completed FIS` = fis,
    `Completed SS` = ss,
    `CDR Zone` = cdr_zone,
    `Fuel` = fuel,
    `Months Till Projected COD` = months_till,
    `Current Month` = month_x,
    `Projected COD Season` = date_y_season,
    `Current Month Season` = month_x_season,
    `Decision Tree Rule` = rule
  )
preds_export %>% arrange(`Projected COD`)
preds_export %>% filter(`Probability of Completion` == 0) %>% select(`Decision Tree Rule`)
preds_export %>% count(`Probability of Completion`)
preds_export %>% count(`Probability Group`, `Probability of Completion`)
preds_export %>% filter(`Probability Group` == 'Uncertain')
preds_export %>% filter(`Probability Group` == 'Unlikely') %>% arrange(`Projected COD`)
preds_export %>% filter(`Probability Group` == 'Uncertain') %>% arrange(`Projected COD`)
preds_export %>%
  arrange(`Projected COD`, `Probability of Completion`) %>%
  rio::export('Solar GIS Probability of Completion.xlsx')
# pred with tidymodels ----
rec_prep <- rec %>% recipes::prep()
# trn_jui <- rec_prep %>% recipes::bake(trn)
# trn_jui
# tst_jui <- rec_prep %>% recipes::bake(tst)
# tst_jui

.pred <- function(set, nm = deparse(substitute(set))) {
  set_jui <- rec_prep %>% recipes::bake(set)
  fit_wf %>%
    predict(new_data = set_jui, type = 'prob') %>%
    mutate(.pred = if_else(.pred_0 > .pred_1, 0L, 1L)) %>%
    bind_cols(set) %>%
    mutate(set = !!nm)
}

preds <-
  bind_rows(
    .pred(trn),
    .pred(tst)
  )
preds
# preds %>% ggplot() + aes(x = .pred_0) + geom_histogram() + facet_wrap(~set)
preds_filt <-
  preds %>%
  filter(set == 'trn') %>%
  arrange(date_y)
preds_filt %>% count(.pred_1)
preds_filt
wagyu <- '18INR0062'
preds_filt %>% filter(inr %in% wagyu)
df %>% filter(inr %in% wagyu) -> z

# probability of being built comes out to 1 of 9 values: 0.05, 0.08, 0.09, 0.79, 0.82, 0.86, 1
preds_filt %>%
  filter(ia == '0')
preds_filt %>%
  filter(ia == '0') %>%
  count(.pred_0)
details %>%
  filter(name == 'CITY WEST Solar') %>%
  select(date_report, ginr_study_phase)
