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



#' @rdname load_dict
#' @export
get_dict <- function(dataset){
  if (missing(dataset)) return(getOption('data_dict')$dict)
  getOption('data_dict')$dict[dataset]
}

#' @rdname add_cat
#' @param cat_name category names. If missing, print all categories.
#' @export
get_cat <- function(cat_name){
  if (missing(cat_name)) return(getOption('data_dict')$cat)
  getOption('data_dict')$cat[[cat]]
}
