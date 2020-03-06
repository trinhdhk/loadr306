check_missing <- function(v, upLimit = .7){
  if (is.numeric(upLimit)) while (upLimit > 1) upLimit <- upLimit/10
  else if (any(is.na(upLimit), is.null(upLimit), upLimit != 'auto')) upLimit <- 0.5

  problem <- FALSE
  index <- NULL
  value <- NULL
  check_name <- 'missing values'

  test1 <- dataMaid::identifyMissing(as.character(v), nMax = Inf)
  test2 <- is.na(v)
  is.missing <- any(test1$problem, test2)

  if (is.missing) {
    problem <- TRUE
    index <- c(which(is.na(v) | v %in% test1$problemValues))
    value <- test1$problemValues
  }

  if (length(index)/length(v) > upLimit){
    warning('Number of ', check_name, ' surpass upper limit. Treated as non-problem.')
    problem <- FALSE
    index <- NULL
    value <- NULL
  }

  list(problem = problem, index = index, value = value)
}

###### Check functions for strings ######
check_textcase <- function(v){
  problem <- FALSE
  index <- NULL
  value <- NULL
  check_name <- 'case issues'

  #### Apply the check on variable ####
  v <- as.character(v)
  test <- dataMaid::identifyCaseIssues(v, nMax = Inf)

  if (test$problem){
    problem <- TRUE
    index <- which(v %in% test[[3]])
    value <- test[[3]]
  }

  #### Return the output ####
  list(problem = problem, index = index, value = value)
}

check_spelling <- function(v, upLimit = .7){
  if (is.numeric(upLimit)) while (upLimit > 1) upLimit <- upLimit/10
  else if (any(is.na(upLimit), is.null(upLimit), upLimit != 'auto')) upLimit <- 0.5

  problem <- FALSE
  index <- NULL
  value <- NULL
  check_name <- 'spelling issues'

  #### Apply the check on variable ####
  v.noNA <- na.blank.omit(v)
  v.noNA <- as.character(v.noNA)
  strings <- hunspell::hunspell_parse(v.noNA)
  is.Wrong <- sapply(strings, function(string){
    !hunspell::hunspell_check(string)
  })
  is.Wrong.wholeWord <- sapply(is.Wrong, any)
  wrongWords.wholeWord <- v.noNA[is.Wrong.wholeWord]

  if (any(is.Wrong.wholeWord)){
    problem <- TRUE
    value <- wrongWords.wholeWord
    index <-  which(v %in% value)
  }

  if (sum(is.Wrong.wholeWord)/length(v.noNA) > upLimit){
    warning('Number of ', check_name, ' surpass upper limit. Treated as non-problem.')
    problem <- FALSE
    index <- value <- NULL
  }

  #### Return the output ####
  list(problem = problem, index = index, value = value)
}

##### Check functions for numeric #####
check_outliers <- function(v, model = c('adjusted', 'boxplot', 'custom'),
                           skewParam = list(a = -4, b = 3), customFn = setCustomFn(),
                           accept.negative = FALSE, accept.zero = FALSE){
  model <- match.arg(model)
  accept.negative <- as.logical(accept.negative)
  if (any(is.na(accept.negative), is.null(accept.negative))) accept.negative <- FALSE
  accept.zero = as.logical(accept.zero)
  if (any(is.na(accept.zero), is.null(accept.zero))) accept.zero <- FALSE
  if (any(is.na(skewParam), is.null(skewParam))) skewParam <- list(a = -4, b = 3)

  problem <- FALSE
  index <- value <- NULL
  check_name <- 'outliers'

  ##### Simple check for Model custom #####
  if (model == 'custom') {
    if (all(is.na(customFn))) customFn <- NULL
    if (is.null(customFn)) {
      warning('No custom function set. Switch back to "adjusted" model.')
      model <- 'adjusted'
    }

    if (length(customFn$fnList) > 2) {
      warning('More than 2 functions provided. Only the first and second will be used.')
    }
  }

  ##### Model skew parameters preparation #####
  if (model == 'adjusted') {
    a <-
      tryCatch(skewParam$a, error = function(e){
        warning('Parameter a is not well-defined. Switch back to default a = -4')
        return(-4)
      })

    b <-
      tryCatch(skewParam$b, error = function(e){
        warning('Parameter b is not well-defined. Switch back to default b = 3')
        return(3)
      })
  }

  #### Apply the check on specific variable ####
  ##### Get default functions and parameters #####
  v <- suppressWarnings(as.numeric(v))
  v.noNA <- na.blank.omit(v)
  defaultFn <- getDefaultFn(v = v.noNA, model = model)

  if (model != 'custom') {
    fn.lower <- defaultFn$fnList[1]
    fn.upper <- defaultFn$fnList[2]
  }
  defaultParams <- defaultFn$paramList

  Q1 <- defaultParams['Q1']
  Q3 <- defaultParams['Q3']
  min <- defaultParams['min']
  max <- defaultParams['max']
  mean <- defaultParams['mean']
  med <- defaultParams['med']
  IQR <- defaultParams['IQR']
  SIQRl <- defaultParams['SIQRl']
  SIQRu <- defaultParams['SIQRu']
  QS <- defaultParams['QS']
  MC <- defaultParams['MC']

  ##### Get custom functions and parameters #####
  if (model == 'custom') {
    fn.lower <- customFn$fnList[1]
    fn.upper <- customFn$fnList[2]
    customParams <- customFn$paramList

    if (length(customParams))
      for (param in customParams)
        tryCatch(eval(customParams),
                 error = function(e) stop('Parameters defining seems faulty. Stop.'))
  }

  tryCatch({
    upperLimit <- eval(fn.upper)
    lowerLimit <- eval(fn.lower)
  }, error = function(e) {
    stop('Cannot apply limit determinating functions. Please check if your have provided all custom parameters. Stop.\n Original error: ', e$message)
  })

  outlierPlaces <- v.noNA < lowerLimit | v.noNA > upperLimit
  if (!accept.negative) outlierPlaces <- outlierPlaces | (v.noNA < 0)
  if (!accept.zero) outlierPlaces <- outlierPlaces | (v.noNA == 0)

  if (any(outlierPlaces)) {
    problem <- TRUE
    value <- v.noNA[outlierPlaces]
    index <- which(v %in% value)
  }

  list(problem = problem, index = index, value = value)
}

check_int <- function(v, upLimit = .7){
  if (is.numeric(upLimit)) while (upLimit > 1) upLimit <- upLimit/10
  else if (any(is.na(upLimit), is.null(upLimit), upLimit != 'auto')) upLimit <- 0.5

  problem <- FALSE
  index <- value <- NULL
  check_name <- 'integer issues'

  v <- suppressWarnings(as.numeric(v))
  v.noNA <- na.blank.omit(v)
  is.int <- v.noNA == floor(v.noNA)

  if (!all(is.int)){
    problem <- TRUE
    value <- v.noNA[!is.int]
    index <- which(v == value)
  }

  if (sum(!is.int)/length(v.noNA) > upLimt){
    warning('Number of ', check_name, ' surpass upper limit. Treated as non-problem.')
    problem <- FALSE
    index <- value <- NULL
  }

  list(problem = problem, index = index, value = value)
}

#### Check functions for factors #####
check_loners <- function(v, threshold = 5, upLimit = 0.7){
  threshold <- suppressWarnings(as.numeric(threshold))
  if (any(is.na(threshold), is.null(threshold))) threshold <- 5
  if (is.numeric(upLimit)) while (upLimit > 1) upLimit <- upLimit/10
  else if (any(is.na(upLimit), is.null(upLimit), upLimit != 'auto')) upLimit <- 0.5
  if (any(is.na(upLimit), is.null(upLimit))) upLimit <- 0.7
  if (upLimit > 1) while (upLimit > 1) upLimit <- upLimit/10

  #### Preparation of Outputs ####
  problem <- FALSE
  index <- value <- NULL
  check_name <- 'loners'

  #### Apply the check on specific variable####
  v.noNA <- na.blank.omit(v)
  v.noNA <- as.factor(v.noNA)
  v.levels <- levels(v.noNA)
  is.loners <- table(v.noNA) < threshold

  if (any(is.loners)){
    problem <- TRUE
    value <- v.levels[is.loners]
    index <-  which(v %in% value)
  }

  if (sum(is.loners)/length(is.loners) > upLimit){
    warning('Number of ', check_name, ' surpass upper limit. Treated as non-problem.')
    problem <- FALSE
    index <- value <- NULL
  }

  list(problem = problem, index = index, value = value)
}

check_binary <- function(v, upLimit = 0.5){
  if (is.numeric(upLimit)) while (upLimit > 1) upLimit <- upLimit/10
  else if (any(is.na(upLimit), is.null(upLimit), upLimit != 'auto')) upLimit <- 0.5

  #### Preparation of Outputs ####
  problem <- FALSE
  index <- value <- NULL
  check_name <- 'redundant levels'

  v.noNA <- na.blank.omit(v)
  v.noNA <- factor(v.noNA, levels = sort(unique(v.noNA)))
  v.levels <- levels(v.noNA)
  is.binary <- length(v.levels) == 2

  if (!is.binary) {
    problem <- TRUE
    table_tmp <- table(v.noNA)
    table_tmp.order <- order(table(v.noNA), decreasing = TRUE)
    is.suspected <- table_tmp.order > 2
    suspectedAmount <- sum(table_tmp[is.suspected])
    suspectedValues <- v.levels[is.suspected]
    if (upLimit == 'auto') upLimit <- sum(table_tmp[!is.suspected])/sum(table_tmp)
    value <- suspectedValues
    index <- which(v %in% value)
  }

  if (suspectedAmount/length(v.noNA) > upLimit){
    warning('Number of ', check_name, ' surpass upper limit. Treated as non-problem.')
    problem <- FALSE
    index <- value <- NULL
  }

  list(problem = problem, index = index, value = value)
}
