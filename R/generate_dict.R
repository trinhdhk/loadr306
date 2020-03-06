._generate_dicts_ <- function(dt){
  dicts <- sapply(dt, ._generate_dict_, simplify = FALSE)
  dicts
}

._generate_dict_ <- function(dt){
  as.data.frame(do.call(rbind, lapply(names(dt), function(n) ._guess_col_format_(dt[n]))), stringsAsFactors = FALSE)
}

._guess_col_format_ <- function(v){
  v.name <- names(v)
  v <- unlist(v)
  if (!is.character(v)) v <- as.character(v)
  v <- na.omit(v)
  if (!length(v)) return(list(variable = v.name, type = 'string', fmt = NA))

  pattern.float <- '(^-?|^)((\\d+\\.$)|(\\d*\\.\\d+$))'
  pattern.datetime <-
    '((\\s|^)((([12]\\d{3}|\\d{2})((\\-)|[\\/.])(0?[1-9]|[12]\\d|3[01])((\\-)|[\\/.])(0?[1-9]|[12]\\d|3[01]))|((0?[1-9]|[12]\\d|3[01])((\\-)|[\\/.])(0?[1-9]|[12]\\d|3[01])((\\-)|[\\/.])([12]\\d{3}|\\d{2}))))|((\\s|^)([0-2]?\\d)\\s?[:]\\s?\\d{1,2})'

  threshold <- 0.7
  is.float <- grepl(pattern.float, v, perl = TRUE)
  is.float <- sum(is.float)/length(is.float) >= threshold
  is.integer <- grepl(pattern.float, v, perl = TRUE)
  is.integer <- sum(is.integer)/length(is.integer) >= threshold
  is.integer <- is.integer & !is.float
  is.datetime <- grepl(pattern.datetime, v, perl = TRUE)
  is.datetime <- sum(is.datetime)/length(is.datetime) >= threshold
  is.categorical <- length(unique(v)) <= 5

  type <- c('integer', 'float', 'datetime', 'categorical')[c(is.integer, is.float, is.datetime, is.categorical)]
  if (!length(type)) type <- 'string'
  type <- type[1]

  if (is.datetime) {
    v.orders <- ._guess_datetime_order_(v, v.name)
    if (!length(v.orders)) type <- 'string'
  }
  fmt <- switch(type,
                integer = structure(c(min = min(as.numeric(v)), max = max(as.numeric(v))), class = 'num.range'),
                float = structure(c(min = min(as.numeric(v)), max = max(as.numeric(v))), class = 'num.range'),
                datetime = v.orders,
                string = NA,
                categorical = NA)
  list(variable = v.name, type = type, fmt = fmt)
}

fill_dict <- function(dts){
  available.dicts <- get_dict()
  options(data_dict = list(dict = ._fill_dict_(dts, available.dicts), cat = get_cat()))
  invisible(TRUE)
}

._fill_dict_ <- function(dts, available.dicts){
  generated.dicts <- ._generate_dicts_(dts)
  combined.dicts <- ._add_na_vars_(generated.dicts, available.dicts)
  combined.dicts <- ._add_na_dicts_(generated.dicts, combined.dicts)
  return(combined.dicts)
}

._add_na_dicts_ <- function(newd, oldd){
  nad <- newd[setdiff(names(newd), names(oldd))]
  cat('- Add dictionary for table(s)', paste(setdiff(names(newd), names(oldd)), collapse = ', ', sep = ''), '\n')
  append(oldd, nad)
}

._add_na_vars_ <- function(newd, oldd){
  # availd.newd <- newd[names(newd)[names(newd) %in% names(oldd)]]
  newdictname <- names(oldd)[names(oldd) %in% names(newd)]
  sapply(names(oldd),
         function(n){
            if (!n %in% newdictname) return(oldd[[n]])
            availd.oldd <- oldd[[n]]
            availd.newd <- newd[[n]]
            newv <- setdiff(newd[[n]]$variable, newd[[n]]$variable)
            if (length(newv)) cat('- Add dictionary for variable(s)', paste(newv, collapse = ', ', sep = ''), '\n')
            rbind(availd.oldd, subset(availd.newd, variable = newv))
         })
}
