#' A training function for the IRLS - Iterative Reweighted Least Squares algorithm
#'
#' @param X A numeric data.frame object used for the fitting of the method.
#' @param y A numeric vector describing the target column.
#' @param tol
#' @param max_iter An integer describing the maximum iteration number for the
#' algorithm. Default value: 100.
#' @param interactions A numeric vector introducing the interactions for the model.
#' Default value: NULL.
#' @param verbose A logical value indicating whether we want to have prints or not.
#'
#' @return An object of class irls containing the objects:
#' \item{\code{tol}}{}
#' \item{\code{max_iter}}{An integer describing the maximum iteration number for the
#' algorithm.}
#' \item{\code{n_iter}}{An integer describing the actual number of iterations for the
#' algorithm.}
#' \item{\code{beta}}{A numeric matrix with coefficient values for the data set columns.}
#' \item{\code{colnames}}{A character vector with column names from the data frame.}
#' \item{\code{interactions}}{A numeric vector describing the interactions for the model.}
#' @export
#'
#' @examples
#' X1 <- data.frame(x1 = rnorm(100), x2 = rnorm(100, 2, 5), x3 = rnorm(100, 4))
#' y1 <- as.matrix(rep(0, 100))
#' X2 <- data.frame(x1 = rnorm(100, 1, 2), x2 = rnorm(100, 12, 5), x3 = rnorm(100, 14))
#' y2 <- as.matrix(rep(1, 100))
#' X  <- rbind(X1, X2)
#' y  <- rbind(y1, y2)
#' m  <- irls(X, y)

irls <- function(X, y, tol = 0.0001, max_iter = 100, interactions = NULL, verbose = FALSE) {
  if (is.null(interactions)) {
    X_train <- X
    verbose_cat('Training the irls model without interactions \n\n', verbose = verbose)
  } else {
    verbose_cat('Training the irls model with interactions \n\n', verbose = verbose)
    X_train <- data.frame(X)

    for (j in 1:nrow(interactions)) {
      tmp     <- X_train[, interactions[j, 1]] * X_train[, interactions[j, 2]]
      X_train <- cbind(X_train, tmp = tmp)
      colnames(X_train)[ncol(X_train)] <- paste0(interactions[j, 1], "__",
                                                 interactions[j, 2])
    }
  }

  colnames <- colnames(X_train)
  X1       <- as.matrix(cbind(bias = 1, X_train))
  beta     <- rep(0, ncol(X1))

  i     <- 0
  delta <- Inf
  pi    <- logit(X1 %*% beta)
  log_likelihood_old <- Inf

  while (i < max_iter && delta > tol) {
    verbose_cat('Fitting the model, iteration:', i, 'log likelihood:', log_likelihood_old, '\n', verbose = verbose)
    pi1  <- 1 - pi
    W    <- diag(as.vector(pi) * as.vector(pi1))

    z    <- X1 %*% beta + solve(W) %*% (y - pi)
    beta <- solve(t(X1) %*% W %*% X1) %*% t(X1) %*% W %*% z

    pi   <- logit(X1 %*% beta)
    log_likelihood_current <- log_likelihood(pi, y)

    if (is.infinite(log_likelihood_old)) {
      delta <- abs(log_likelihood_current)
    } else {
      delta <- abs(log_likelihood_old - log_likelihood_current)
    }
    log_likelihood_old <- log_likelihood_current
    i <- i + 1
  }

  out <- list(
    tol          = tol,
    max_iter     = max_iter,
    n_iter       = i,
    beta         = beta,
    colnames     = colnames,
    interactions = interactions
  )

  verbose_cat('Training has finished. \n \n', verbose = verbose)
  class(out) <- "irls"
  return(out)
}
