#' Backend loading functions for file extensions
#' @rdname default.load.fn
#' @export
reset.load.fn <- function(){
  options(data.load.fn = list(
    csv = list(fn.call = read.csv, fn.arg = list(colClasses = 'character')),
    xls = list(fn.call = load_excel, fn.arg = list(guess_max = 3000)),
    xlsx = list(fn.call = load_excel, fn.arg = list(guess_max = 3000)),
    db = list(fn.call = load_sqlite),
    mbd = list(fn.call = load_mdb)
  ))
}

#' @rdname default.load.fn
#' @param path path to data file, based on which will return a loading function
#' @return a function
#' @export
default.load.fn <- function(path, ...){
  if (missing(path)) return(getOption('data.load.fn'))
  load.fn <- getOption('data.load.fn')
  ext <- tools::file_ext(path)
  switch_ <- as.list(c(ext, load.fn))
  fn <- do.call(switch, switch_)
  if (!length(fn)) {warning('No suitable method!'); return(NULL)}
  out <- purrr::partial(fn$fn.call, !!!{{fn$fn.arg}})
  out(path, ...)
}

#' @description set/get/reset loading functions
#' @rdname default.load.fn
#' @param x string, file extension.
#' @param value a named list with two elements: fn.call for function call, fn.arg for argument passed to fn.call
#' @export
set.default.load.fn <- function(x, value){
  data.load.fn <- getOption('data.load.fn')
  value <- c(list(), value) #coerce value into a list
  if (length(value) > 1) {
    if (!setequal(names(value), c('fn.call', 'fn.arg'))) names(value) <- c('fn.call', 'fn.arg')
  } else names(value) <- 'fn.call'
  data.load.fn <- modifyList(data.load.fn, structure(list(value), names = x))
  options(data.load.fn = data.load.fn)
}

#' Remove potential NAs
#' @description A function to remove potential NAs
#' @param v A vector
#' @return A vector
#' @example na.blank.omit(c('a', 'b', 'a', 'b', NA, 'c', NA, NA))
#' @export
na.blank.omit <- function(x){
  x[!is.na(x) & !x %in% c("", "nan", "NaN", "NAN", "na", "NA", "Na", "-", " ")]
}

