
#' Guess date-time values' order
#' @param x a vector of type "character" which contains data-time values.
#' @return A vector of order candidates, if no candidates, return NULL
#' @export
guess_datetime_order <- function(x){
  x <- na.blank.omit(x)
  if (grepl('((?=\\d)T(?=\\d))|((\\?=\\d)Z$)', '', x, perl = TRUE)) #ISO format
    return(lubridate::parse_date_time(x, orders = 'ymdHMS'))
  seps <- ._findseps_(x)
  unique.seps <- unique(unlist(seps))
  if (length(unique.seps) > 2 || length(unique.seps) < 1) return(NULL)
  twoparts <- if (length(unique.seps) == 2) TRUE else FALSE
  # browser()
  if (twoparts){
    split.x <- strsplit(x, '\\s+', perl = TRUE)
    date <- sapply(split.x, `[`, 1)
    time <- sapply(split.x, `[`, 2)
    seps.date <- ._findseps_(date)
    seps.time <- ._findseps_(time)
    split.date <- strsplit(date, seps.date, fixed = TRUE)
    split.time <- strsplit(time, seps.time, fixed = TRUE)
    n.date <- unique(sapply(split.date, length))
    n.time <- unique(sapply(split.time, length))
    if (length(n.date)>1|length(n.time)>1) return(NULL)
    if (n.date > 3 | n.time > 3) return(NULL)
    split.date <- purrr::transpose(split.date)
    split.time <- purrr::transpose(split.time)
    guess.date <- sapply(split.date,
                          function(date){
                            m <- all(na.omit(suppressWarnings(as.numeric(date))) <= 12)
                            d <- all(na.omit(suppressWarnings(as.numeric(date))) <= 31)
                            list(y = TRUE, m = m, d = d)
                          })

    ymd_matrix <- ._ymd_matrix_(n.date)
    guess.date.filter <- apply(guess.date, 2,
                               function(v)
                                 if (sum(unlist(v)) == 1) c('m', 'd')[v == TRUE] else
                                   if (sum(unlist(v)) == 0) 'y' else NA)
    matched.matrix <- apply(ymd_matrix, 1,
                          function(r)
                            sapply(seq_along(r),
                                   function(i)
                                     is.na(guess.date.filter[i]) | r[i] == guess.date.filter[i]))
    ymd_matrix <- ymd_matrix[apply(matched.matrix,2,all),,drop=FALSE]
    ymd_orders <- apply(ymd_matrix, 1, paste0, collapse = '')

    if (n.time == 3) hms_orders <- 'HMS'
    if (n.time == 2) {
      have.hours <- sapply(split.time, function(x) all(x < 24))
      if (sum(have.hours) == 2) hms_orders <- 'HM'
      else if (sum(have.hours) == 1) hms_orders <- paste0(c('H', 'M')[which(have.hours), which(!have.hours)], collapse = '')
      else hms_orders <- 'MS'
    }

    orders <- paste0(ymd_orders, hms_orders)
  } else {
    split.x <- strsplit(x, seps, fixed = TRUE)
    n.x <- unique(sapply(split.x, length))
    if (n.x > 3) return(NULL)
    split.x <- purrr::transpose(split.x)
    is.time <- all(seps %in% c('.', ':'))
    if (is.time){
      if (n.x == 3) orders <- 'HMS'
      if (n.x == 2) {
        have.hours <- sapply(split.x, function(x) all(x < 24))
        if (sum(have.hours) == 2) orders <- 'HM'
        else if (sum(have.hours) == 1) orders <- paste0(c('H', 'M')[which(have.hours), which(!have.hours)], collapse = '')
        else orders <- 'MS'
      }
    } else {
      guess.date <- sapply(split.x,
                           function(date){
                             m <- all(na.omit(suppressWarnings(as.numeric(date))) <= 12)
                             d <- all(na.omit(suppressWarnings(as.numeric(date))) <= 31)
                             list(m = m, d = d)
                           })

      ymd_matrix <- ._ymd_matrix_(n.x)

      guess.date.filter <- apply(guess.date, 2,
                                 function(v)
                                   if (sum(unlist(v)) == 1) c('m', 'd')[v == TRUE] else
                                     if (sum(unlist(v)) == 0) 'y' else NA)
      matched.matrix <- apply(ymd_matrix, 1,
                              function(r)
                                sapply(seq_along(r),
                                       function(i)
                                         is.na(guess.date.filter[i]) | r[i] == guess.date.filter[i]))
      ymd_matrix <- ymd_matrix[apply(matched.matrix,2,all),,drop=FALSE]
      orders <- apply(ymd_matrix, 1, paste0, collapse = '')
    }
  }
  orders
}


._findseps_ <- function(x){
  unlist(lapply(x, function(.x){
    bslash <- grepl('\\\\', .x, perl=TRUE)
    fslash <- grepl('/', .x, perl=TRUE)
    colon <- grepl('\\:', .x, perl=TRUE)
    dot <- grepl('\\.', .x, perl=TRUE)
    hyphen <- grepl('\\-', .x, perl=TRUE)
    c('\\', '/', ':', '.', '-')[c(bslash, fslash, colon, dot, hyphen)]
  }))
}

._ymd_matrix_ <- function(n){
  if (n == 3)
    ymd_matrix <- matrix(c('y', 'm', 'd',
                           'y', 'd', 'm',
                           'm', 'd', 'y',
                           'd', 'm', 'y'),
                         byrow = TRUE, ncol = 3)
  if (n == 2)
    ymd_matrix <- matrix(c('y', 'm',
                           'm', 'y',
                           'm', 'd',
                           'd', 'm'),
                         byrow = TRUE, ncol = 2)
  if (n == 1)
    ymd_matrix <- matrix(c('y', 'm', 'd'), byrow = TRUE, ncol = 1)

  ymd_matrix
}
