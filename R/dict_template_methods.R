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

#' @rdname dict_template
#' @export
print.dict_template <- function(x, ...){
  namae <- names(x)
  for (i in seq_along(x)){
    cat(namae[i],': ', sep = '')
    if (is.list(x[[i]])){
      cat('\n')
      print(x[[i]])
    }
    else
      cat(x[[i]], '\n')
  }
  invisible(x)
}
