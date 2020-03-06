is.falsified <- function(x, recursive = FALSE){
  l <- length(x)
  if (!l) return(TRUE)
  if (l == 1 | !recursive) return(is.na(x))
  sapply(x, is.falsified, simplify = is.atomic(x))
}

na.blank.omit <- function(v){
  x[!is.na(x) & !x %in% c("", "nan", "NaN", "NAN", "na", "NA", "Na", "-", " ")]
}

str_to_expr <- function(str){
  pre_parsed <- strsplit(str, split = "\\s?[;,]\\s?", perl = TRUE)
  sapply(pre_parsed, function(text) parse(text = text))
}

getDefaultFn <- function(v, model = c('adjusted', 'boxplot', 'custom')){
  requireNamespace('robustbase')
  MC <- robustbase::mc(v)
  mean <- mean(v)
  quartiles <- stats::quantile(v)
  min <- quartiles[[1]]
  Q1 <- quartiles[[2]]
  med <- quartiles[[3]]
  Q3 <- quartiles[[4]]
  max <- quartiles[[5]]
  IQR <- Q3 - Q1
  SIQRl <- med - Q1
  SIQRu <- Q3 - med
  QS <- ((SIQRu - SIQRl))/IQR
  paramList <- c(MC = MC, mean = mean, med = med,
                 min = min, Q1 = Q1, Q3 = Q3, max = max,
                 IQR = IQR, SIQRl = SIQRl, SIQRu = SIQRu, QS = QS)

  model <- match.arg(model)
  fnList <- switch(model,
                   'adjusted' = c(parse(text = 'Q1 - 1.5*exp(a*MC)*IQR'), parse(text = 'Q3 + 1.5*exp(b*MC)*IQR')),
                   'boxplot' = c(parse(text = 'Q1 - 1.5*IQR'), parse(text = 'Q3 + 1.5*IQR')),
                   'custom' = NULL)

  out <- structure(list(fnList = fnList, paramList = paramList), class = 'robustFn')
  return(out)
}

setCustomFn <- function(fn = NULL, param = NULL){
  fnList <- if(!is.null(fn)) str_to_expr(fn) else NULL
  paramList <- if(!is.null(param)) str_to_expr(param) else NULL

  out <- structure(list(fnList = fnList, paramList = paramList), class = 'robustFn')
  return(out)
}
