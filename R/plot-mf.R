#' @title Plot convergence
#' @description Plots the convergence of the matrix factorisation
#' @param x Matix factorisation object \code{mf}
#' @param ... Dots
#' @details Plots the convergence profile.
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot geom_line labs aes
#' @examples
#' \dontrun{
#' m <- matrix(sample(c(NA, 1:5), 60, replace = TRUE, prob = c(0.2, rep(0.8/5, 5))), nrow = 10)
#' id <- select_test(m, 0.2)
#' mf <- matrix_factorisation(m, 2, test = id$test)
#' plot(mf)
#' }
#' @export

# plot mat fact
plot.mf <- function(x, ...){
  if(all(is.nan(x$test))){
    df <- tibble(epoch = seq_along(x$train), rmse = x$train, set = "train")
  }else{
    df <- tibble(epoch = rep(seq_along(x$train), 2), rmse = c(x$train, x$test),
                 set = c(rep("train", length(x$train)), rep("test", length(x$test))))
  }
  gmf <- ggplot(df, aes(x = epoch, y = rmse, col = set)) +
    geom_line() +
    labs(
      title = "RMSE convergence",
      x = "Epoch",
      y = "RMSE"
    )
  return(gmf)
}
