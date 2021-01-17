
.get_x <-
  function(...,
           f = NULL,
           file = tempfile(),
           ext = 'rds',
           dir = getwd(),
           path = NULL,
           f_import = readr::read_rds,
           f_export = readr::write_rds
           overwrite = FALSE,
           export = TRUE) {
    if (is.null(path)) {
      .display_info('Generating `path` from `dir`, `file`, and `ext`.')
      path <- file.path(dir, sprintf('%s.%s', file, ext))
    }

    path_exists <- file.exists(path)
    if(path_exists & !overwrite) {
      return(f_import(path))
    }

    f_safe <- purrr::safely(f, otherwise = NULL)
    res <- f_safe(...)
    if (is.null(res)) {
      .display_warning('Something went wrong with function call `f`!')
      return(NULL)
    }

    if(export) {
      .display_info('Exporting `x` to `path = "{path}"`.')
      dir <- dirname(path)
      if(!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
      }
      f_export(res, path)
    }
    res

  }
