
import_xl_cleanly <- function(path, ...) {
  path %>%
    readxl::read_excel(...) %>%
    janitor::clean_names()
}
