new_cat <- function(cat_name, cat_table){
  cat <- get_cat()
  cat_table <- as.data.frame(cat_table, stringsAsFactor = FALSE)
  if (!length(cat)) cat <- ._empty_cat_()
  dim <- attr(cat, 'dim') + 1
  dimnames <- attr(cat, 'dimnames')
  if (!cat_name %in% dimnames$Category) dimnames$Category <- append(dimnames$Category, cat_name)
  if (!cat_name %in% dimnames$Category)
    cat <- append(cat, list(cat_table)) else
      cat[[which(dimnames == cat_name)]] <- {
        dt <- cat[[which(dimnames == cat_name)]]
        delta <- if(is.numeric(dt$value)) max(dt$value) else 0
        lb <- append(dt$label, setdiff(cat_table$label, dt$label))
        val <- append(val, length(lb) - length(val) + delta)
        data.frame(value = val, label = lb, stringsAsFactors = FALSE)
      }

  class(cat) <- 'by'
  attr(cat, 'dim') <- dim
  attr(cat, 'dimnames') <- dimnames
  data_dict <- getOption('data_dict')
  data_dict$cat <- cat
  options(data_dict = data_dict)
  invisible(TRUE)
}

remove_cat <- function(cat_name){
  data_dict <- getOption('data_dict')
  cat <- data_dict$cat
  dim <- attr(cat, 'dim') - 1
  dimnames <- attr(cat, 'dimnames')
  dimnames$Category <- dimnames$Category[dimnames$Category != cat_name]
  cat <- cat[names(cat) != cat_name]
  class(cat) <- 'by'
  attr(cat, 'dim') <- dim
  attr(cat, 'dimnames') <- dimnames
  data_dict$cat <- cat
  options(data_dict = data_dict)
  invisible(TRUE)
}

._empty_cat_ <- function(){
  out <- list()
  class(out) <- 'by'
  attr(out, 'dim') <- 0
  attr(out, 'dimnames') <- list(Category = vector('character'))
  out
}