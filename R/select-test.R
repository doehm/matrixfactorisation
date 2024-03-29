#' @title Matrix factorisation test set
#' @description Select a test set to pass to \code{matrix_factorisation}
#' @param Y Matrix with missing values. Missing values should be \code{NA}
#' @param r Ratio i.e. proption of observations held out for testing
#' @param min_row The minimum observations to leave in each row. This protects against leaving an entire row missing.
#' @details Selects a test to be used with \code{matrix_factorisation}. It returns the train set - the original matrix with NA's for cells
#' held out for the test and the test id vector. This can be useful to monitor the convergence and prediction accuracy.
#' @importFrom progress progress_bar
#' @examples
#' \dontrun{
#' m <- matrix(sample(c(NA, 1:5), 60, replace = TRUE, prob = c(0.2, rep(0.8/5, 5))), nrow = 10)
#' id <- select_test(m, 0.2)
#' mf <- matrix_factorisation(m, 2, test = id$test)
#' mf$pred
#' }
#' @export

select_test <- function(Y, r, min_row = 1){

  # hold out min_row observations from each row
  hold_out <- lapply(1:nrow(Y), function(x) sample(which(!is.na(Y[x,])), min(min_row, length(which(!is.na(Y[x,]))))))

  # replace hold_out with NA's so they don't get selected
  train <- Y
  for(k in 1:nrow(Y)){
    train[k,hold_out[[k]]] <- NA
  }

  # select test
  id <- sample(which(!is.na(Y)), length(which(!is.na(Y)))*r)
  train[id] <- NA

  # put back hold out
  for(k in 1:nrow(Y)){
    train[k,hold_out[[k]]] <- Y[k,hold_out[[k]]]
  }

  list(train = train, test = id)
}
