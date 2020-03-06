#' Load all excel sheets
#' @description A function to automatically load several sheets in excel files.
#' @param path path to excel file
#' @param sheets if NULL (default): all sheets are loaded. Otherwise, mentioned sheets are loaded
#' @param ... additional parameters passed to readxl::read_excel
#' @return a list of data.frame
#' @export
load_excel <- function(path, sheets = NULL, ...){
  if (!length(sheets)) sheets <- readxl::excel_sheets(path)
  sapply(sheets, function(sheet) readxl::read_excel(path, sheet = sheet, ...), simplify = FALSE)
}

#' Load all tables in sqlite files
#' @description A function to automatically load several tables in sqlite database.
#' @param path path to db file
#' @param tbls if NULL (default): all tables are loaded. Otherwise, mentioned ones are loaded
#' @param ... addtionnal parameters passed to DBI::dbConnect
#' @return a list of data frame
#' @export
load_sqlite <- function(path, tbls = NULL,...){
  con <- DBI::dbConnect(RSQLite::SQLite(), path, ...)
  tbl_list <- if (length(tbls)) tbls else DBI::dbListTables(con)
  out <- sapply(tbl_list, function(tbl) DBI::dbReadTable(con, tbl), simplify = FALSE)
  DBI::dbDisconnect(con)
  out
}

load_mdb <- function(path, ...){
  stop('Loading MSAccess files is currently not supported')
}
