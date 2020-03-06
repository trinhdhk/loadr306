#' Load/push/remove data dictionaries
#' @description These functions add new dictionary, append more dictionary or remove them.
#' @param path path to dictionary
#' @param ext either "rds", "xls", or "xlsx", specify format of dictionary.
#' If missing, will be automatically regconised using file extension.
#' @param ... if ext == "xls" or "xlsx", addtional parameters passed to loading function.
#' One of them is template, default is clires_auto, which smartly selects between 2 versions of CliRes.
#' @return TRUE
#' @export
load_dict <- function(path, ext, ...)
{
  if (missing(ext)) ext <- tools::file_ext(path)
  if (tolower(ext) == 'rds') dd <- ._load_dict_rds_(path)
  else if ((tolower(ext) %in% c('xls', 'xlsx'))) dd <- ._load_dict_excel_(path, ...)
  else stop('Unrecognised format!\n')
  options(data_dict = dd)
  cat('- Data dictionary loaded!\n')
  invisible(TRUE)
}

#' @rdname load_dict
#' @export
push_dict <- function(path, ext, ...){
  old_dd <- getOption('data_dict')
  if (!length(old_dd)) {
    warning('Current data dict is empty')
    return(load_dict(path, ext, ...))
  }
  if (missing(ext)) ext <- tools::file_ext(path)
  if (tolower(ext) == 'rds') dd <- ._load_dict_rds_(path)
  else if ((tolower(ext) %in% c('xls', 'xlsx'))) dd <- ._load_dict_excel_(path, ...)
  else stop('Unrecognised format!')
  options(data_dict = modifyList(old_dd, dd))
  cat('- Data dictionary pushed!\n')
  invisible(TRUE)
}

._load_dict_rds_ <- function(path)
{
  cat('- Load dictionary at:', tools::file_path_as_absolute(path),'\n')
  dd <- readRDS(path)
  return(dd)
}

._load_dict_excel_ <- function(path, template = 'clires_auto', ...)
{
  cat('- Load dictionary at:', tools::file_path_as_absolute(path),'\n')
  dd_sheets <- readxl::excel_sheets(path)
  if (is.character(template)) template <- get(template)
  if (is.function(template)) template <- template(path, dd_sheets)
  if (!length(template)) stop('No template found!')
  #load
  index_sheet <- readxl::read_excel(path, sheet=template$index_sheet)
  cat_sheet <- readxl::read_excel(path, sheet=template$category_sheet)
  dd <- sapply(index_sheet[[1]],
               function(dd_sheet){
                 .dd <- readxl::read_excel(path, sheet=dd_sheet, ...)
                 ._convert_dd_(.dd, template = template)
               }, simplify = FALSE)
  # browser()
  cat_sheet <- do.call(data.frame,
                       append(sapply(template$category_cols,
                              function(catcol) if (is.function(catcol)) catcol(cat_sheet) else catcol,
                              simplify = FALSE), list(stringsAsFactors = FALSE)))
  colnames(cat_sheet) <- c('category', 'value', 'label')
  rownames(cat_sheet) <- NULL
  Category <- as.factor(cat_sheet$category)
  cats <- by(cat_sheet, Category, `[`, c('value', 'label'), simplify = FALSE)
  # cats <- structure(dplyr::group_map(dplyr::group_by(cat_sheet, Category), ~ .x), names = unique(cat_sheet$Category))
  return(list(dict = dd, cat = cats))
}

._convert_dd_ <- function(dd, template){
  include_cols <- template$included_cols
  var_name <- if (is.function(include_cols$var_name)) include_cols$var_name(dd) else dd[[include_cols$var_name]]
  var_type <- if (is.function(include_cols$var_type)) include_cols$var_type(dd) else dd[[include_cols$var_type]]
  var_format <- if (is.function(include_cols$var_format)) include_cols$var_format(dd) else dd[[include_cols$var_format]]
  var_cat <- if (is.function(include_cols$var_cat)) include_cols$var_cat(dd) else dd[[include_cols$var_cat]]

  data_types <- sapply(template$data_types,
                       function(data_type){
                         if (is.function(data_type)) data_type(var_type) else data_type
                       }, simplify = FALSE)
  data_types <- data_types[as.logical(sapply(data_types, length))]
  if (identical(data_types$integer,data_types$float)) data_types <- data_types[names(data_types)!='integer']
  if (identical(data_types$string, data_types$categorical)) data_types <- data_types[names(data_types)!='categorical']
  converted_var_type <- C306::simple_recode(var_type, data_types)

  var_dd <- data.frame(variable = var_name, type = converted_var_type, format = var_format, cat = var_cat,
                       stringsAsFactors = FALSE)
  var_dd <- subset(var_dd, type %in% names(data_types))
  var_dd$fmt <- ._recode_format_(var_dd)
  var_dd[c('variable', 'type', 'fmt')]
}

._recode_format_ <- function(dd){
  apply(dd, 1,
        function(r){
          type <- r['type']
          fmt <- r['format']
          cat <- r['cat']

          fmt_parsed <- switch(type,
                               string = ._parse_str_fmt_(fmt),
                               float = ._parse_number_fmt_(fmt),
                               integer = ._parse_number_fmt_(fmt),
                               datetime = ._parse_datetime_fmt_(fmt),
                               categorical = cat,
                               binary = cat)
        })
}

._parse_str_fmt_ <- function(fmt){
  parsed_fmt <- fmt
  parsed_fmt <- gsub('\\', '\\\\', parsed_fmt, fixed = TRUE)
  parsed_fmt <- gsub('/', '\\/', parsed_fmt, fixed = TRUE)
  parsed_fmt <- gsub('-', '\\-', parsed_fmt, fixed = TRUE)
  parsed_fmt <- gsub('_', '\\_', parsed_fmt, fixed = TRUE)
  parsed_fmt <- gsub('(', '\\(', parsed_fmt, fixed = TRUE)
  parsed_fmt <- gsub(')', '\\)', parsed_fmt, fixed = TRUE)
  parsed_fmt <- gsub('[', '\\[', parsed_fmt, fixed = TRUE)
  parsed_fmt <- gsub(']', '\\]', parsed_fmt, fixed = TRUE)
  parsed_fmt <- gsub('.', '\\.', parsed_fmt, fixed = TRUE)
  parsed_fmt <- gsub('*', '.', parsed_fmt, fixed = TRUE)
  parsed_fmt <- gsub('#', '\\d', parsed_fmt, fixed = TRUE)
  parsed_fmt
}

._parse_number_fmt_ <- function(fmt){
  parsed_fmt <- strsplit(fmt, '-')[[1]]
  structure(list(min = parsed_fmt[1], max = parsed_fmt[2]), class = 'num.range')
}

._parse_datetime_fmt_ <- function(fmt){
  parsed_fmt <- tolower(fmt)
  parsed_fmt <- gsub('(?<=\\:)m{1,2}','M',parsed_fmt, perl = T)
  parsed_fmt <- gsub('h{1,2}(?=\\:)','H',parsed_fmt, perl = T)
  parsed_fmt <- gsub('d{1,2}','d',parsed_fmt, perl = T)
  parsed_fmt <- gsub('m{1,2}','m',parsed_fmt, perl = T)
  parsed_fmt <- gsub('y{1,2}','y',parsed_fmt, perl = T)
  parsed_fmt <- gsub('[\\/-:;,.]', '', parsed_fmt, perl = T)
  parsed_fmt
}

#' CliRes version auto-recognition
#' @description A function to automatically recognise which version of CliRes is the data dictionary
#' @param dd_path path to data dictionary
#' @param dd_sheets sheet list of data dictionary
#' @return an object of class "dict_template" or NULL
clires_auto <- function(dd_path, dd_sheets){
  sample_sheet <-
    readxl::read_excel(dd_path,
                       sheet = dd_sheets[!dd_sheets %in%  c('Events', 'CRFs', 'Pages', 'Grids', 'Categories', 'Category')][[1]])
  cols <- colnames(sample_sheet)
  version <-
    if (setequal(dd_sheets[1:5], c('Events', 'CRFs', 'Pages', 'Grids', 'Categories')) &
        setequal(cols[1:6], c('Variable', 'Submission value', 'Data type', 'Format', 'Category', 'Grid'))) 2 else
          if (setequal(dd_sheets[c(1, length(dd_sheets))], c('CRFs','Category')) &
             setequal(cols[1:10], c('Question No', 'Data Type', 'Field Name', 'Value range',
                              'Default Value', 'Primary Key', 'NULL allowed', 'Sub question',
                              'Note', 'BLIND'))) 1 else 0
  if (version==2) {cat('- Using Clires version 2 template.\n'); return(clires_2)}
  if (version==1) {cat('- Using Clires version 1 template.\n'); return(clires_1)}
  NULL
}

#' Create a dictionary template
#' @description A function to create a dict template
#' @param ... matching function for index_sheet, category_sheet, category_cols, included_cols, and data_types
#' See clires_1 for more information
#' @return A list of class "dict_template"
#' @export
dict_template <- function(...){
  structure(list(...), class = 'dict_template')
}

#' Helpers for dictionary template matching functions.
#' @description Functions that will return a name/index of cols pointing to specific columns in data dictionaries.
#' @param value matching pattern
#' @param position fall-back integer that will be return if no matched columm found.
#' @param exact default is TRUE, the value is matched as-is.
#' @return for match_val.pos: a function that do the matching, for match_pattern: same as grep
#' @seealso \link{dict_template}
#' @export
match_val.pos <- function(value, position = NULL, exact = TRUE){
  if (exact) .f <- function(x, value, position) {
    if (sum(names(x)==value)) x[[which(names(x)==value)[[1]]]] else x[[position]]
  } else .f <- function(x, value, position){
    if (sum(grep(value, names(x), perl = TRUE))) x[[grep(value, names(x), perl = TRUE)[[1]]]] else x[[position]]
  }

  purrr::partial(.f, value = value, position = position)
}

#' @rdname match_val.pos
#' @param pattern,perl,ignore.case,exact parameters passed to grep
#' @export
match_pattern <- function(pattern, perl = TRUE, ignore.case = TRUE, exact = TRUE){
  .f <- function(x, pattern = pattern, perl = perl, ignore.case = ignore.case, exact = exact){
    out <- unlist(lapply(x, function(.x)
      unique(suppressWarnings(grep(pattern = pattern, x = .x, ignore.case = ignore.case, perl = perl, fixed = exact, value = TRUE)))))
    out <- out[as.logical(sapply(out, length))]
    if (length(out) > 1) return(out[[1]])
    out
  }
  purrr::partial(.f, pattern = pattern, perl = perl, ignore.case = ignore.case, exact = exact)
}

#' CliRes dict_templates
#' @rdname clires
#' @export
clires_1 <- dict_template(
  index_sheet = 'CRFs',
  category_sheet = 'Category',
  category_cols = c(
    cat_name = match_val.pos('Category Code', 1),
    cat_code = match_val.pos('Database value', 4),
    cat_lbl = match_val.pos('Title EN', 5)
  ),
  included_cols = c(
    var_name = match_val.pos('Field Name', 3),
    var_type = match_val.pos('Data Type', 2, exact = FALSE),
    var_format = match_val.pos('Value range', 4),
    var_cat = match_val.pos('Value range', 4)
  ),
  data_types = c(
    integer = match_pattern('Integer') ,
    float = match_pattern('Float'),
    binary = match_pattern('Check'),
    categorical = match_pattern('(Ex)?Category', exact = FALSE),
    datetime = match_pattern('SDateTime'),
    string = match_pattern('Free Text')
  )
)

#' @rdname clires
#' @export
clires_2 <- dict_template(
  index_sheet = 'CRFs',
  category_sheet = 'Categories',
  category_cols = c(
    cat_name = match_val.pos('Category', 1),
    cat_code = match_val.pos('Submission value', 3),
    cat_lbl = match_val.pos('Caption', 4)
  ),
  included_cols = c(
    var_name = match_val.pos('Variable', 1),
    var_type = match_val.pos('Data type', 3, exact = FALSE),
    var_format = match_val.pos('Format', 4),
    var_cat = match_val.pos('Category', 5)
  ),
  data_types = c(
    integer = match_pattern('Number') ,
    float = match_pattern('Number'),
    binary = match_pattern('Check'),
    categorical = match_pattern('(RadioList)|(Category)', exact = FALSE),
    datetime = match_pattern('DateTime'),
    string = match_pattern('Free Text')
  )
)

#' @rdname load_dict
#' @export
get_dict <- function(dataset){
  if (missing(dataset)) return(getOption('data_dict')$dict)
  getOption('data_dict')$dict[dataset]
}

#' @rdname load_dict
#' @param cat category names. If missing, print all categories.
#' @export
get_cat <- function(cat){
  if (missing(cat)) return(getOption('data_dict')$cat)
  getOption('data_dict')$cat[[cat]]
}
