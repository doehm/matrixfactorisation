#' @title Alternating Least Squares
#' @description Implements alternating least squares matrix factorisation on a given matrix
#' @param Y Matrix with missing values. Missing values should be \code{NA}
#' @param k_dim Number of latent variables
#' @param test The select test. See \code{select_test}
#' @param epochs Maximum iterations
#' @param lambda Regularisation parameter
#' @param tol Iteration tolerance
#' @param pbar Progress bar toggle
#' @details Implements alternating least squares. Matrix \code{Y} should have \code{NA}'s as missing values. The matrix will be factorised
#' into two matrices U and V, the user and feature matrix. The U matrix essentially contains the weight each user gives to a certain feature. The
#' function will output the accuracy of the selected test sample by using \code{test}. The prediction matrix is return as \code{pred} but can be
#' calculated by UV^T. See \code{select_test} for more information.
#' @importFrom progress progress_bar
#' @importFrom stats rnorm
#' @examples
#' \dontrun{
#' m <- matrix(sample(c(NA, 1:5), 60, replace = TRUE, prob = c(0.2, rep(0.8/5, 5))), nrow = 10)
#' id <- select_test(m, 0.2)
#' mf <- als(m, 2, test = id$test)
#' mf$pred
#' }
#' @export


# als

als <- function(Y, k_dim, test = NULL, epochs = 10, lambda = 0.01, tol = 1e-6, pbar = FALSE){

  # set train
  Y_train <- Y
  if(!is.null(test)) Y_train[test] <- NA

  # initialise matrices and id values
  U <- matrix(rnorm(k_dim*nrow(Y), 0, 0.01), nrow = nrow(Y), ncol = k_dim, dimnames = list(rownames(Y)))
  V <- matrix(rnorm(k_dim*ncol(Y), 0, 0.01), nrow = ncol(Y), ncol = k_dim, dimnames = list(colnames(Y)))

  observed_rating_id <- matrix(!is.na(Y_train), nrow = nrow(Y_train), ncol = ncol(Y_train))
  observed_user_list <- lapply(1:ncol(Y), function(x) which(observed_rating_id[,x] == 1))
  observed_item_list <- lapply(1:nrow(Y), function(x) which(observed_rating_id[x,] == 1))
  n_ratings <- sum(observed_rating_id)

  error <- (Y_train - U %*% t(V)) # only want the error from the observed ratings
  rmse <- sqrt(sum(error[observed_rating_id]^2)/n_ratings)
  rmse_test <- sqrt(sum((Y - U %*% t(V))[test]^2)/length(test))
  delta <- Inf
  test_delta <- Inf
  k <- 1

  fmt <- ":elapsedfull // dimensions :kdim // epoch :epoch // train :trainrmse // test :testrmse // delta :delta"
  if(is.null(test)) fmt <- ":elapsedfull // dimensions :kdim // epoch :epoch // train :trainrmse // delta :delta"
  pb <- progress_bar$new(format = fmt, clear = FALSE, total = NA)

  # als
  while (k <= epochs & tol < delta) {

    # loop through each item where there exists a rating and update U
    for (j in 1:ncol(Y)) {
      ur <- observed_user_list[[j]]
      u <- U[ur,]
      if(length(ur) == 1) u <- matrix(U[ur,], ncol = k_dim)
      V[j, ] <- t((solve(t(u) %*% u + lambda*diag(k_dim)) %*% t(u)) %*% Y_train[ur, j])
    }

    # loop through each user where there exists a rating and update V
    for (i in 1:nrow(Y)) {
      ir <- observed_item_list[[i]]
      v <- V[ir,]
      if(length(ir) == 1) v <- matrix(V[ir,], ncol = k_dim)
      U[i, ] <- t((solve(t(v) %*% v + lambda*diag(k_dim)) %*% t(v)) %*% Y_train[i, ir])
    }

    error <- (Y_train - U %*% t(V))
    rmse <- c(rmse, sqrt(sum(error[observed_rating_id]^2)/n_ratings))
    rmse_test <- c(rmse_test, sqrt(sum((Y - U %*% t(V))[test]^2)/length(test)))
    k <- k + 1
    delta <- abs(rmse[k-1] - rmse[k])
    tokens <- list(kdim = k_dim, epoch = k, trainrmse = format(round(rmse[k], 4), nsmall = 4),
                   testrmse = format(round(rmse_test[k], 4), nsmall = 4),
                   delta = format(delta, digits = 4, nsmall = 4, scientific = TRUE))
    if(pbar) pb$tick(tokens = tokens)
  }
  out <- list(pred = U %*% t(V), u = U, v = V, epochs = epochs, train = rmse, test = rmse_test)
  cat("\n")
  class(out) <- append(class(out), "mf")
  return(out)
}

