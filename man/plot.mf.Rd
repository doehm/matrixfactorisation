% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-mf.R
\name{plot.mf}
\alias{plot.mf}
\title{Plot convergence}
\usage{
\method{plot}{mf}(x, ...)
}
\arguments{
\item{x}{Matix factorisation object \code{mf}}

\item{...}{Dots}
}
\description{
Plots the convergence of the matrix factorisation
}
\details{
Plots the convergence profile.
}
\examples{
\dontrun{
m <- matrix(sample(c(NA, 1:5), 60, replace = TRUE, prob = c(0.2, rep(0.8/5, 5))), nrow = 10)
id <- select_test(m, 0.2)
mf <- matrix_factorisation(m, 2, test = id$test)
plot(mf)
}
}
