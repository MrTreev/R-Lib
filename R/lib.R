f_log <- function(...) {
  message(sprintf(...))
}

f_dirstr <- function(dirstr) {
  dir.create(
    dirstr,
    showWarnings = FALSE,
    recursive = TRUE
  )
  dirstr
}

f_newpath <- function(...) {
  f_dirstr(file.path(...))
}

f_findpath <- function(...) {
  file.path(...)
}

.f_data_string <- function(...) {
  sprintf("%s.Rdata", paste(..., sep = "-"))
}

f_newdata <- function(indir, ...) {
  file.path(f_dirstr(indir), .f_data_string(...))
}

f_finddata <- function(indir, ...) {
  file.path(indir, .f_data_string(...))
}

.f_plot_string <- function(...) {
  sprintf("%s.png", paste(..., sep = "-"))
}

f_newplot <- function(indir, ...) {
  file.path(f_dirstr(indir), .f_plot_string(...))
}

f_findplot <- function(indir, ...) {
  file.path(indir, .f_plot_string(...))
}

.f_txt_string <- function(...) {
  sprintf("%s.txt", paste(..., sep = "-"))
}

f_newtext <- function(indir, ...) {
  file.path(f_dirstr(indir), .f_txt_string(...))
}

f_findtext <- function(indir, ...) {
  file.path(indir, .f_txt_string(...))
}

f_setvar <- function(varname, setval, envir = .GlobalEnv) {
  assign(varname, setval, envir = envir)
}

f_default <- function(varname, default, envir = .GlobalEnv) {
  if (!exists(varname, envir = envir)) {
    f_setvar(varname, default, envir)
  }
}

f_cache_column <- function(name, func, outpath, params, bool_param, data) {
  if (!exists(name)) {
    .filename <- f_newdata(outpath, name)
    if (!file.exists(.filename)) {
      f_log("Calculating %s list", name)
      .vals <- lapply(params, function(.param) func(.param, bool_param, data))
      f_log("Caching %s list", name)
      saveRDS(.vals, file = .filename)
    } else {
      f_log("Loading %s list", name)
      .vals <- readRDS(.filename)
    }
    .vals
  } else {
    get(name)
  }
}