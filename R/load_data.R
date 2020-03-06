#' Smart data loading, auto assignment, and taking into account the data dictionaries
#' @description A function to samrtly load data basing on file extensions. Data are subsequently formatted based on dictionaries.
#' @param path path to data file, currently support csv, xls, xlsx, and sqlite/db
#' @param data_dict an object containing data dict, default is call a data dictionary laid inside options, defined by load_dict()
#' @param format Default is TRUE: automatically format the data based on data_dict.
#' @param auto_extract Only respected when number of datasets within the database > 1. Default is TRUE: if the database loaded contains > 1 datasets, automatically extract it to the caller environment.
#' @param log Default is TRUE: generate a log containing issues regarding the data based on data dictionary.
#' @param lower_case_names Default is FALSE. If TRUE and auto_extract==TRUE: automatically convert dataset names to lower case
#' @param vebose Default is TRUE: whether to print warnings, messages during loading/formatting process
#' @param ... additional parameters passed to respective loading function
#' @param .loadfn Internal, default is default.load.fn; specifying which function to use for which file extension
#' @param .auto_fill_dict Internal. Default is TRUE. Auto fill data dictionary based on the loaded data for variables not mentioned within the dictionaries
#' @param envir Internal. Default it parent.frame(): if auto_extract==TRUE, specifying which environment to extract the datasets.
#' @return if auto_extract == TRUE, return TRUE, else return the loaded dataset.
#' @seealso \link{load_dict} \link{default.load.fn}
#' @export
load_data <- function(path, data_dict = get_dict(), format = TRUE, auto_extract = TRUE, log = TRUE, lower_case_names = FALSE, verbose = TRUE, ..., .loadfn = default.load.fn, .auto_fill_dict = TRUE, envir = parent.frame())
{
  auto_extract <- as.logical(auto_extract)
  format <- as.logical(format)
  lower_case_names <- as.logical(lower_case_names)
  is.dir <- utils::file_test('-d', path)
  if (is.dir) {
    files <- list.files(path)
    out <- purrr::flatten(sapply(files, load_data,
                                 data_dict = data_dict, format = format,
                                 auto_extract = auto_extract, log = log,
                                 lower_case_names = lower_case_names, verbose = verbose,
                                 .loadfn = .loadfn, .auto_fill_dict = .auto_fill_dict, ..., envir = envir))
    return(out)
  }

  # browser()
  cat('- Load data at:', tools::file_path_as_absolute(path), '\n')
  loaded_data <- .loadfn(path, ...)
  if (is.data.frame(loaded_data))
    loaded_data <- structure(list(loaded_data),
                             names = gsub(paste0('.',tools::file_ext(path),'$'), '', basename(path)))

  formatted_data <- sapply(loaded_data,
                           function(dt)
                             if (ncol(dt)) dplyr::as_tibble(as.data.frame(apply(dt, 2, ._parse_text_), stringsAsFactors = FALSE)),
                           simplify = FALSE)
  if (format)
    formatted_data <- ._format_data_(loaded_data, data_dict = data_dict, .auto_fill_dict = .auto_fill_dict, log = log, verbose = verbose, envir = envir)

  if (!auto_extract) {
    cat('Extract data to environment\n')
    if (length(formatted_data)==1) return(formatted_data[[1]])
    return(formatted_data)
  }

  for (i in seq_along(formatted_data)){
    v <- names(formatted_data)[[i]]
    if (lower_case_names) v <- tolower(v)
    assign(v, formatted_data[[i]], envir = envir)
  }

  if (lower_case_names & log) {
    ._data_log_ <- get('._data_log_', envir = envir)
    names(._data_log_) <- tolower(names(._data_log_))
    assign('._data_log_', ._data_log_, envir = envir)
  }

  invisible(TRUE)
}

._parse_text_ <- function(dt){
  if (!isTRUE(ncol(dt)>0)) return(dt)

  dt <- apply(dt, 2,
              function(col){
                col.res <- as.character(col)
                col.res <- iconv(col.res, to  = 'UTF-8//TRANSLITE')
                as.is <- any(length(grepl("\\s", col.res, perl = TRUE)) >= 2)
                col.res <- utils::type.convert(col.res, as.is = as.is)
                col.res <- gsub('^\\s*$', NA_character_, col.res, perl = TRUE)
              })

  dt <- as.data.frame(dt, stringsAsFactors = FALSE)
}



._format_data_ <- function(dt, data_dict, .auto_fill_dict = .auto_fill_dict, log = TRUE, verbose = TRUE, envir = parent.frame(2)){
  if (!length(data_dict) | .auto_fill_dict) data_dict <- ._fill_dict_(dt, data_dict)
    # .data_dict <- ._generate_dicts_(dt)
  # browser()
  dt_name <- names(dt)
  not_in_dict <- setdiff(dt_name, names(data_dict))
  if (verbose) for (v in not_in_dict) cat('- Skipping table ', v, ' due to no definition in dictionary.\n')
  dt_name <- intersect(dt_name, names(data_dict))
  dt_dict <- data_dict[dt_name]
  dt <- sapply(dt_name,
               function(._dt_name_)
                 ._format_dt_(dt = dt[[._dt_name_]], dt_dict = dt_dict[[._dt_name_]], log = log, verbose = verbose),
               simplify = FALSE)

  if (log){
    cat('- Generate log data at ._data_log_\n')
    log_dt <- sapply(dt_name,
                 function(._dt_name_)
                   ._extract_log_(dt = dt[[._dt_name_]]),
                 simplify = FALSE)
    assign('._data_log_', log_dt, envir = envir)
  }

  dt
}

._extract_log_ <- function(dt){
  dt_var <- colnames(dt)
  log <- sapply(dt,
                function(v){
                  problem <- as.list(attr(v, 'problem'))
                  out <- as.list(rep('-', 7))
                  names(out) <- c('not.num', 'not.int', 'not.defined.lv', 'not.bin',
                                  'not.matched.pattern', 'not.matched.nchar', 'not.date.time')
                  new <- list(
                    not.num = problem$not.num,
                    not.int = problem$not.int,
                    not.defined.lv = problem$not.lv,
                    not.bin = problem$not.bin,
                    not.matched.pattern = problem$not.match.pattern,
                    not.matched.nchar = problem$not.match.nchar,
                    not.date.time = problem$not.date.time
                  )

                  new <- new[!sapply(new, is.null)]
                  out <- modifyList(
                    out,
                    as.list(new)
                  )
                })
  # browser()
  log
}

._format_dt_ <- function(dt, dt_dict, log = log, verbose = TRUE){
  # browser()
  dt_var <- colnames(dt)
  not_in_dict <- setdiff(dt_var, unlist(dt_dict$variable))
  if (verbose) for (v in not_in_dict) cat('- Skipping variable ', v,' due to no definition in dictionary.\n')
  dt_var <- intersect(dt_var,unlist(dt_dict$variable))
  dt_dict <- subset(dt_dict, variable %in% dt_var)
  # browser()
  dt[dt_var] <- sapply(seq_along(dt_var),
                       function(i) {
                         v <- dt_var[[i]]
                         dt_v <- dt[[v]]
                         dict_v <- subset(dt_dict, variable == v)
                         type_v <- as.character(unlist(dict_v$type))
                         fmt_v <- unlist(dict_v$fmt)
                         fmt <-
                           switch(type_v,
                                  string = ._fmt_string_(dt_v, fmt_v),
                                  categorical = ._fmt_cat_(dt_v, fmt_v),
                                  binary = ._fmt_bin_(dt_v, fmt_v),
                                  float = ._fmt_float_(dt_v, fmt_v),
                                  int = ._fmt_int_(dt_v, fmt_v),
                                  datetime = ._fmt_datetime_(dt_v, fmt_v))

                         fmt_dt_v <- fmt$value
                         fmt_problem <- fmt$problem
                         if (verbose) ._generate_problem_msg_(fmt_problem)
                         if (log) attr(fmt_dt_v, 'problem') <- fmt_problem
                         fmt_dt_v
                       }, simplify = FALSE)
  if (log)
    dt[not_in_dict] <- sapply(dt[not_in_dict],
                              function(v) {
                                attr(v, 'problem') <- 'skipped'
                                v
                              }, simplify = FALSE)
  dt
}

._check_pattern <- function(x, fmt){
  if (length(fmt)>1) print(fmt)
  which(!grepl(fmt, x, perl = TRUE))
}

._fmt_string_ <- function(x, fmt = NA){
  x <- as.character(x)
  not.match.pattern <- not.match.nchar <- NULL

  if (!is.na(fmt)) {
    if(!is.na(suppressWarnings(as.numeric(fmt))))
      not.match.nchar <- which(nchar(x) != fmt)
    else
      not.match.pattern <- ._check_pattern(x, fmt)
  }
  list(value = x, problem = list(not.match.pattern = not.match.pattern,
                                 not.match.nchar = not.match.nchar
                                 ))
}

._fmt_float_ <- function(x, fmt = NA){
  num.x <- suppressWarnings(as.numeric(x))
  not.num <- which(num.x != x)
  not.match.pattern <- if (!any(is.na(fmt), length(fmt) > 1)) ._check_pattern(x, fmt) else NULL
  list(value = num.x, problem = list(not.num = not.num, not.match.pattern = not.match.pattern))
}

._fmt_int_ <- function(x, fmt = NA){
  num.x <- ._fmt_float(x)
  x <- num.x$value
  problem <- num.x$problem
  floor.x <- floor(x)
  not.int <- which(x != floor.x)
  problem <- append(problem, list(not.int = not.int))
  list(value = floor.x, problem = problem)
}

._fmt_cat_ <- function(x, fmt = NA){
  if (is.na(fmt)) return(list(value = as.factor(x), problem = NULL))
  cat <- get_cat(fmt)
  lv <- cat$value
  not.lv <- setdiff(unique(x), lv)
  x <- factor(x, levels = lv, labels = cat$label)
  list(value = x, problem = list(not.lv = not.lv))
}

._fmt_bin_ <- function(x, fmt = NA){
  cat.x <- ._fmt_cat_(x, fmt = fmt)
  problem <- cat.x$problem
  cat.x <- cat.x$value
  lv.x <- levels(cat.x)
  if (length(lv.x) > 2) problem <- append(problem, list(not.bin = TRUE))
  list(value = cat.x, problem = problem)
}

._fmt_datetime_ <- function(x, fmt = NA){
  is.time <- if(!is.na(fmt)) grepl('[HMS]', fmt, perl = TRUE) else any(grepl('\\d{1,2}\\:\\d{1,2}', x, perl = TRUE))
  # browser()
  dt.x <-
    if (!is.na(fmt))
      suppressWarnings(lubridate::parse_date_time(x, orders = fmt)) else
      suppressWarnings(as.POSIXct(x))
  # browser()
  if (!is.time) dt.x <- as.Date(dt.x)
  not.datetime <- which(is.na(dt.x) & !is.na(x))
  list(value = dt.x, problem = list(not.datetime = not.datetime))
}

._generate_problem_msg_ <- function(problems){
  for (prob_type in problems){
    # msg <- switch()
  }
}
