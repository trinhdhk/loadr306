# Check missing values using dataMaid
check_missing <- function(v, verbose = FALSE){

  #### Arguments check ####
  if (!requireNamespace('dataMaid')) stop('Please install dataMaid package before continuing.')

  verbose <- as.logical(verbose)

  #### Preparation of Outputs ####
  problem <- logical(0)
  problemValues <- NULL
  problemIndexes <- NULL
  message <- list()
  res <- v

  #### Apply the check on specific variable####
  test1 <- dataMaid::identifyMissing(as.character(v), nMax = Inf)
  test2 <- is.na(v)
  is.missing <- any(test1$problem, test2)

  if (is.missing) {
    problem <- TRUE
    message <- 'These $display$ might $behave$ missing values:'
    problemValues <- unique(c(test1$problemValues, if (any(test2)) NA))
    problemIndexes <- c(which(is.na(v) | v %in% test1$problemValues))

  } else {
    problem <- FALSE
    message <- 'No problems found'
  }

  #### Return the output ####
  out <- chkRes_init(testName = 'missing data',
                     problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemIndexes,
                     message = message)

  return(out)
}
