
# setup ---
library(tidyverse)
data('details_w_status')

.path_x <- function(dir, file, ext = NULL) {
  if(!is.null(ext)) {
    ext <- sprintf('.%s', ext)
  } else {
    ext <- ''
  }
  file.path(dir, sprintf('%s%s', file, ext))
}
.path_data <- partial(.path_x, dir = file.path('inst'), ... = )
.path_figs <- partial(.path_x, dir = file.path('inst'), ... = )
.path_figs_png <- partial(.path_x, dir = file.path('inst'), ext = 'png', ... = )
.add_season_cols <- function(data, col) {
  col_sym <- col %>% sym()
  data %>%
    mutate(
      .mm = !!col_sym %>% lubridate::month(),
      !!sym(sprintf('%s_sesaon', col)) := case_when(
        .mm %in% c(12, 1, 2) ~ 'winter',
        .mm %in% 3:5 ~ 'spring',
        .mm %in% 6:9 ~ 'summer',
        .mm %in% 10:11 ~ 'fall'
      ),
      !!sym(sprintf('%s_win', col)) := if_else(.mm %in% c(12, 1, 2), 1L, 0L),
      !!sym(sprintf('%s_spg', col)) := if_else(.mm %in% 3:5, 1L, 0L),
      !!sym(sprintf('%s_sum', col)) := if_else(.mm %in% 6:9, 1L, 0L),
      !!sym(sprintf('%s_fal', col)) := if_else(.mm %in% 10:11, 1L, 0L)
    ) %>%
    select(-.mm)
}

.df2mat <- function(data) {
  model.matrix(
    ~.+0,
    data =
      model.frame(
        ~ .+0,
        data,
        na.action = na.pass
      )
  )
}
# data ----
df_init <-
  details_w_status %>%
  # select(ginr_study_phase) %>%
  # filter(!(fuel %in% c('COA', 'COAL'))) %>%
  separate(ginr_study_phase, into = c('ss', 'fis', 'ia'), sep = '\\,\\s+') %>%
  mutate(
    idx = row_number(),
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
    idx,
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
    matches('_(spg|sum|fal|win|season)$')
  )
df_init

actual_final_status <-
  df_init %>%
  # filter(!is.na(inactive) | !is.na(synchronization) | !is.na(cancelled)) %>%
  select(inr, date_report, cancelled, inactive, synchronization) %>%
  mutate(.date_max = lubridate::ymd('1900-01-01')) %>%
  group_by(inr) %>%
  slice_max(date_report) %>%
  rowwise() %>%
  mutate(date_max = max(c_across(cancelled:.date_max), na.rm = TRUE)) %>%
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
  ) %>%
  select(inr, final_status)
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

nms <- df_init %>% names()
cols_season_extra <- nms %>% str_subset('_season$')
col_projected_cod_extra <- 'projected_cod'
cols_projected_cod <- nms %>% str_subset(sprintf('%s_', col_projected_cod_extra))
col_date_report_extra <- 'date_report'
cols_date_report <- nms %>% str_subset(sprintf('%s_', col_date_report_extra))
col_fuel_extra <- 'fuel'
cols_fuel <- nms %>% str_subset(sprintf('^%s_', col_fuel_extra))
col_zone_extra <- 'cdr_zone'
cols_zone <- nms %>% str_subset(sprintf('^%s_', col_zone_extra))
cols_phase <- c('ss', 'fis', 'ia')
cols_ginr_status_extra <- c('cancelled', 'inactive', 'energization', 'synchronization', 'commercial')
cols_months <- nms %>% str_subset('^months_')
cols_lst <-
  list(
    col_y = 'final_status',
    cols_x = c(
      cols_months,
      cols_projected_cod,
      cols_date_report,
      cols_fuel,
      cols_zone,
      cols_phase
    ),
    cols_id_model = c('idx'),
    cols_id = c('inr', 'name', 'date_report'),
    cols_extra = unique(
      c(
        cols_season_extra,
        col_projected_cod_extra,
        # col_date_report_extra,
        col_fuel_extra,
        col_zone_extra,
        cols_ginr_status_extra
      )
    )
  )

.generate_path <- function(path = NULL, dir, file, ext) {
  if(!is.null(path)) {
    return(path)
  }
  file.path(dir, sprintf('%s.%s', file, ex))
}

.do_tune_cv <-
  function(grid_params,
           x_dmat,
           booster,
           objective,
           eval_metrics,
           stem,
           dir = 'inst'
           file = sprintf('res_tune_cv_%s', stem),
           ext = 'rds',
           path = NULL,
           overwrite = FALSE) {
    path <- .generate_path(path = path, dir = dir, file = file, ext = ext)
    basename <- basename(path)
    file <- tools::file_path_sans_ext(path)
    dir <- dirname(path)
    ext <- tools::file_ext(path)

    .get_metrics <-
      function(idx = 1,
               .path = .generate_path(dir = dir, file = sprintf('%s_%d', file, idx), ext = ext)) {
        # row = 1; data <- grid_params %>% slice(idx)

        path_exists <- path %>% file.exists()
        if(path_exists & !.overwrite) {
          .display_info('Returning early for `idx = {idx}`.')
          return(readr::read_rds(.path))
        }
        .display_info('Row {cli::bg_cyan(idx)} (of {cli::bg_cyan(n_row)})')
        params <-
          list(
            booster = booster,
            objective = objective,
            eval_metric = eval_metrics,
            eta = data$learn_rate,
            gamma = data$loss_reduction,
            subsample = data$sample_size,
            colsample_bytree = data$mtry,
            max_depth = data$tree_depth,
            min_child_weight = data$min_n
          )

        fit_cv <-
          xgboost::xgb.cv(
            data = x_dmat_nona,
            params = params,
            nrounds = .nrounds,
            folds = folds,
            metrics = .eval_metrics,
            early_stopping_rounds = 10,
            print_every_n = 10
          )

        res <- params
        res$iter = fit_cv$best_iteration
        .eval_metric <- .eval_metrics[1]
        col_trn <- sprintf('train_%s_mean', .eval_metrics[1])
        col_tst <- sprintf('test_%s_mean', .eval_metrics[1])
        res[[sprintf('%s_trn', .eval_metric)]] = fit_cv$evaluation_log[res$iter][[sprintf('train_%s_mean', .eval_metric)]]
        res[[sprintf('%s_tst', .eval_metric)]] = fit_cv$evaluation_log[res$iter][[sprintf('test_%s_mean', .eval_metric)]]

        res[['eval_metric']] <- NULL
        res <- bind_rows(res)
      readr::write_rds(res, path)
      res
    }
  # res_tune_cv <-
  #   grid_params %>%
  #   nest(params = -idx) %>%
  #   mutate(metrics = map2(params, idx, ~.get_metrics(data = ..1, idx = ..2))) %>%
  #   select(-params) %>%
  #   unnest(metrics)
  # res_tune_cv
  # write_rds(res_tune_cv, path_res_tune_cv)
  res <-
    grid_params %>%
    nest(params = -idx) %>%
    mutate(metrics = map2(params, idx, ~.get_metrics(data = ..1, idx = ..2))) %>%
    select(-params) %>%
    unnest(metrics)
  res
  write_rds(res, path)
  res
}

do_fit_prob_model <- function(data, cols_lst, stem, overwrite = FALSE) {
  data = actual_final_status_nona
  stem = 'sync'
  overwrite = FALSE

  col_y_sym <- cols_lst$col_y %>% sym()
  data_nona <-
    data %>%
    filter(!is.na(!!col_y_sym))
  data_nona

  df_all <-
    data %>%
    left_join(df_init) %>%
    mutate(idx = row_number())
  df_all

  .df2mat_x <- function(data) {
    data %>% select(one_of(c(cols_lst$cols_x))) %>% .df2mat()
  }

  x_mat_all <- df_all %>% .df2mat_x()

  df_nona <-
    df_all %>%
    drop_na(!!col_y_sym) %>%
    mutate(idx = row_number())

  x_mat_nona <- df_nona %>% .df2mat_x()

  x_dmat_nona <-
    xgboost::xgb.DMatrix(
      x_mat_nona,
      label = df_nona[[cols_lst$col_y]]
    )
  x_dmat_nona

  set.seed(42)
  folds_ids <-
    caret::createFolds(
      data_nona[['inr']],
      k = 10,
      list = FALSE,
      returnTrain = FALSE
    )
  folds_ids

  folds <-
    data_nona %>%
    bind_cols(tibble(fold = folds_ids)) %>%
    left_join(df_nona %>% select(inr, idx)) %>%
    select(fold, idx) %>%
    split(.$fold) %>%
    purrr::map(~select(.x, -fold) %>% pull(idx))
  folds

  n_obs <- folds %>% flatten_int() %>% length()
  max_idx <- folds %>% flatten_int() %>% max()
  assertthat::assert_that(n_obs == max_idx)

  .nrounds <- 500
  .booster <- 'gbtree'
  .objective <- 'binary:logistic'
  .eval_metrics <- list('auc')

  set.seed(42)
  n_row <- 50
  grid_params <-
    dials::grid_latin_hypercube(
      dials::finalize(dials::mtry(), df_nona),
      dials::min_n(),
      dials::tree_depth(),
      dials::learn_rate(),
      dials::loss_reduction(),
      sample_size = dials::sample_prop(),
      size = n_row
    ) %>%
    mutate(
      learn_rate = 0.1 * ((1:n_row) / n_row),
      mtry = mtry / ncol(df_nona),
      idx = row_number()
    ) %>%
    relocate(idx)
  grid_params

  res_tune_cv <- .do_tune_cv(
    grid_params = grid_params,
    x_dmat = x_dmat_nona,
    booster = .booster,
    objective = .objective,
    eval_metrics = .eval_metrics
  )




  .path_data_x <- function(file, ext = NULL) {
    .path_data(file = sprintf('%s_%s', file, stem), ext = ext)
  }
  .path_figs_png_x <- function(file) {
    .path_figs_png(file = sprintf('%s_%s', file, stem))
  }
  .path_data_parquet_x <- purrr::partial(.path_data_x, ext = 'parquet', ... = )
  path_res_tune_cv <- .path_data_x('res_tune_cv', ext = 'rds')
  path_fit <- .path_data_x('fit')
  path_res_cv <- .path_data_parquet_x('res_cv')
  path_probs <- .path_data_parquet_x('probs')
  # path_probs_all <- .path_data_parquet_x('probs_all')
  path_probs_oob <- .path_data_parquet_x('probs_oob')
  path_shap <- .path_data_parquet_x('shap')
  path_shap_agg <- .path_figs_png_x('viz_shap_agg')
  path_metrics <- .path_data_x('metrics', ext = 'csv')


  # res_tune_cv %>% ggplot() + aes(x = auc_tst) + geom_histogram(binwidth = 0.005)
  res_cv_best <- res_tune_cv %>% slice_max(auc_tst)
  res_cv_best

  .f <- function(x) {
    res_cv_best %>% purrr::pluck(x)
  }

  params_best <-
    list(
      booster = .booster,
      objective = .objective,
      eval_metric = .eval_metrics,
      eta = .f('eta'),
      gamma = .f('gamma'),
      subsample = .f('subsample'),
      colsample_bytree = .f('colsample_bytree'),
      max_depth = .f('max_depth'),
      min_child_weight = .f('min_child_weight')
    )
  params_best

  fit <-
    xgboost::xgboost(
      params = params_best,
      data = x_mat_nona,
      nrounds = .nrounds,
      early_stopping_rounds = 10,
      print_every_n = 10,
      verbose = 2
    )
  xgboost::xgb.save(fit, path_fit)

  .augment_probs <- function(v, data, cols_id = 'idx', cols_extra = NULL, col_y, export = TRUE, path) {
    # browser()
    col_y_sym <- col_y %>% sym()
    probs <-
      v %>%
      tibble(.prob_1 = .) %>%
      mutate(.prob_0 = 1 - .prob_1) %>%
      bind_cols(
        data %>%
          select(
            all_of(cols_id),
            one_of(cols_extra),
            all_of(col_y)
          )
      )
    if(!export) {
      return(probs)
    }
    probs %>% arrow::write_parquet(path)
    probs
  }

  .augment_probs_x <-
    purrr::partial(
      .augment_probs,
      cols_id = cols_lst$cols_id,
      cols_extra = cols_lst$cols_id_model,
      col_y = cols_lst$col_y,
      export = TRUE,
      ... =
    )

  probs <-
    fit %>%
    predict(x_mat_all, type = 'prob') %>%
    .augment_probs_x(
      data = df_all,
      path = path_probs
    )
  probs

  feature_values_init <-
    x_mat_nona %>%
    as.data.frame() %>%
    mutate_all(scale) %>%
    gather('feature', 'feature_value') %>%
    as_tibble()
  feature_values_init

  feature_values <-
    feature_values_init %>%
    pull(feature_value)
  feature_values

  shap_init <-
    fit %>%
    predict(newdata = x_mat_nona, predcontrib = TRUE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(-matches('BIAS'))
  shap_init

  shap <-
    shap_init %>%
    bind_cols(df_nona %>% select(idx)) %>%
    left_join(
      probs %>%
        select(prob = .prob_1) %>%
        mutate(idx = row_number())
    ) %>%
    pivot_longer(-c(idx, prob), names_to = 'feature', values_to = 'shap_value')
  shap

  shap_agg_by_feature <-
    shap %>%
    group_by(feature) %>%
    summarize(
      across(shap_value, ~mean(abs(.x))),
    ) %>%
    ungroup() %>%
    mutate(
      across(shap_value, list(rnk = ~row_number(desc(.x))))
    ) %>%
    arrange(shap_value_rnk)
  shap_agg_by_feature

  # feature_labs <- .get_feature_labs()

  .prep_viz_data <- function(data) {
    data %>%
      # left_join(feature_labs) %>%
      mutate(feature_lab = feature) %>%
      mutate(
        across(
          feature_lab, ~forcats::fct_reorder(.x, -shap_value_rnk)
        )
      )
  }

  viz_shap_agg <-
    shap_agg_by_feature %>%
    .prep_viz_data() %>%
    ggplot() +
    aes(y = feature_lab, x = shap_value) +
    geom_col() +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 30)) +
    theme(
      axis.text.y = element_text(size = 12, lineheight = 1),
      panel.grid.major.y = element_blank()
    ) +
    labs(
      title = 'Probability model feature importance',
      x = 'mean(|SHAP value|)',
      y = NULL
    )
  viz_shap_agg

  probs %>%
    slice_max(date_report) %>%
    filter(is.na(final_status)) %>%
    # left_join(df_nona %>% select(-idx))
    left_join(details_w_status %>% select(-idx)) %>%
    arrange(desc(projected_cod)) %>%
    select(inr, name, interconnecting_entity, cdr_zone, fuel, projected_cod, energization, .prob_1) %>%
    filter(.prob_1 > 0.5)

  probs %>%
    slice_max(date_report) %>%
    filter(!is.na(final_status)) %>%
    left_join(df_nona %>% select(-idx)) %>%
    arrange(synchronization) %>%
    select(.prob_1, inr, name, capacity_mw, energization, synchronization, commercial) %>%
    head(20)
  details_w_status %>%
    filter(inr == '18INR0033') -> z

  probs %>%
    slice_max(date_report) %>%
    filter(is.na(final_status)) %>%
    # arrange(desc(.prob_1))
    ggplot() +
    aes(x = .prob_1) +
    geom_density()

  # probs %>% ggplot() + aes(x = .prob_1) + geom_histogram()

  fit_cv <-
    xgboost::xgb.cv(
      prediction = TRUE,
      data = x_mat_nona,
      params = params_best,
      nrounds = .nrounds,
      folds = folds,
      metrics = .eval_metrics,
      early_stopping_rounds = 10,
      print_every_n = 1
    )

  res_cv <-
    fit_cv$evaluation_log %>%
    as_tibble()
  res_cv %>% arrow::write_parquet(path_res_cv)

  probs_oob <-
    fit_cv$pred %>%
    .augment_probs_x(data = df_nona, path = path_probs_oob)
  probs_oob
}
