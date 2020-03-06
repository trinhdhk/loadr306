decode_fct <- function(x, ...) {
  if (missing(x)) return(._decode_all)
  UseMethod('decode_fct')
}

decode_fct.data.frame <- function(x, data_dict = get_dict(), exclude = NULL)
{
  x_name <- deparse(substitute(x))
  x_dict <- data_dict$dict[[x_name]]
  cats <- data_dict$cat
  x_fct <- subset(x_dict, type %in% c('categorical', 'binary'))
  browser()
  x[names(x) %in% x_fct$variable] <-
    apply(x_fct, 1,
          function(r){
            v.name <- r['variable']
            v.cat <- cats[[r['fmt']]]
            v <- x[[which(names(x) == v.name)]]
            attr(v, 'levels') <- v.cat
            class(v) <- c('pre_decoded_factor', class(v))
            return(v)
          })
}
