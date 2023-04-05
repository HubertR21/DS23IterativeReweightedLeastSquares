#' Predict function for irls model
#'
#' @param object An irls object from `irls()` function.
#' @param X A data.frame object with a dataset to predict.
#' @param prob A logical value determining if we want probabilities values in the output.
#' @param ... Further arguments passed to or from other methods (currently ignored).
#'
#' @return A data.frame with predictions (or their probabilities).
#' @export
#'
#' @examples
#' X1 <- data.frame(x1 = rnorm(100), x2 = rnorm(100, 2, 5), x3 = rnorm(100, 4))
#' y1 <- as.matrix(rep(0, 100))
#' X2 <- data.frame(x1 = rnorm(100, 1, 2), x2 = rnorm(100, 12, 5), x3 = rnorm(100, 14))
#' y2 <- as.matrix(rep(1, 100))
#' X  <- rbind(X1, X2)
#' y  <- rbind(y1, y2)
#' X_train <- X
#' m <- irls(X, y)
#' preds <- predict(m, X, prob = FALSE)
#' preds
predict.irls <- function(object, X, prob = TRUE, ...) {

  if (class(object) != 'irls') {
    stop(cat(crayon::red('\u2716'), 'Provided object `object` is not an irls object.'))
  } else if (!'data.frame' %in% class(X)) {
    stop(cat(crayon::red('\u2716'), 'Provided data set is not a `data.frame` object.'))
  } else if (! class(prob) == 'logical') {
    stop(cat(crayon::red('\u2716'), 'Provided `prob` value is not logical.'))
  }

  if (is.null(object$interactions)) {
    X_train <- X
  } else {
    X_train <- data.frame(X)

    for (j in 1:nrow(object$interactions)) {
      tmp     <- X_train[, object$interactions[j, 1]] * X_train[, object$interactions[j, 2]]
      X_train <- cbind(X_train, tmp = tmp)
      colnames(X_train)[ncol(X_train)] <- paste0(object$interactions[j, 1], "__",
                                                 object$interactions[j, 2])
    }
  }

  X1        <- X_train[, object$colnames]
  ds_matrix <- cbind(bias = 1, X1)
  y         <- logit(as.matrix(ds_matrix) %*% object$beta)

  if (!prob) {
    y <- ifelse(y > 0.5, 1, 0)
  }

  y <- as.data.frame(y)
  colnames(y) <- c("y_pred")

  return(y)
}
