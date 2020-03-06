#' @rdname load_dict
#' @param file path to save dictionary
#' @export
save_dict <- function(file){
  dd <- getOption('data_dict')
  saveRDS(dd, file = file, ascii = TRUE)
  cat('- Data dictionary saved at:', tools::file_path_as_absolute(file), '\n')
  invisible(TRUE)
}

#' @rdname load_dict
#' @param dataset which dataset'dictionary should be removed. If missing, all exisiting dictionaries are removed.
#' @export
clear_dict <- function(dataset){
  if (missing(dataset)) options(data_dict = NULL)
  else .Options$data_dict$dict <- getOption('data_dict')$dict[names(getOption('data_dict')$dict)==dataset]
  invisible(TRUE)
}


