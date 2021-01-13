
.do_assign <-
  function(x,
           name = NULL,
           prefix = NULL,
           suffix = NULL,
           sep = '_',
           assign = TRUE,
           envir = rlang::caller_env(2),
           verbose = TRUE,
           ...) {

    if(is.null(name)) {
      name <- rev(as.character(sys.call()))[1]
    }

    if (!assign) {
      return(invisible(x))
    }
    if(is.null(envir)) {
      .display_warning('Cannot assign because `envir` is NULL.')
      return(invisible(x))
    }
    if (!is.null(prefix)) {
      name <- sprintf('%s%s%s', prefix, sep, name)
    }
    if (!is.null(suffix)) {
      name <- sprintf('%s%s%s', name, sep, suffix)
    }
    assign(x = name, value = x, envir = envir, ...)
    .display_info('{name} assigned to {environmentName(envir)}.', verbose = verbose)
  }
