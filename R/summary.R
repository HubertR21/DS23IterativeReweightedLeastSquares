#' Describes an irls object briefly
#'
#' @param object An irls object from `irls()` function.
#' @param ... Further arguments passed to or from other methods (currently ignored).
#'
#' @return A brief description of the irls object.
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
#' summary(m)
summary.irls <- function(object, ...){
  cat("The Logistic Regression model trained with Iterative Least Squares algorithm.\n")
  cat("It required", object$n_iter, "iterations to finish calculations.\n")
  cat("\n")
  cat("Model coefficients:\n")
  cat("Intercept:", object$beta[1], "\n")
  for (i in 2:length(object$beta)) {
    cat(object$colnames[i - 1], ":", object$beta[i], "\n")
  }
}
