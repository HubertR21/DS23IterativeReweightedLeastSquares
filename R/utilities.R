#' Calculate log likelihood value
#'
#' @param p Probability for positive class.
#' @param y True labels.
#'
#' @return Log-likelihood value.
#' @export
log_likelihood <- function(p, y) {
  return(sum(y * log(p) + (1 - y) * log(1 - p)))
}

#' Calculate logit value
#'
#' @param q Input for logit function.
#'
#' @return Logit function value.
#' @export
logit <- function(q) {
  return(1 / (1 + exp(-1 * q)))
}

#' Splits dataset into train, and test subsets
#'
#' @param df A data.frame object, the data set you want to split.
#' @param p A numeric vector of length 2 which variables sum to 1 indicating
#' the proportions for train and test subsets. Default value: c(0.8, 0.2).
#' @param seed An integer describing the seed for the splitting algorithm.
#' Default value NULL.
#'
#' @return A list of two data.frames for train and test subsets.
#' @export
#'
#' @examples
#' train_test_split(banknote)
train_test_split <- function(df, p = c(0.8, 0.2), seed = NULL) {
  target <- df[, ncol(df)]
  inds   <- splitTools::partition(target, p = c(train = p[1], test = p[2]), seed = seed)
  train  <- df[inds$train, ]
  test   <- df[inds$test, ]

  return(list(
    train = train,
    test  = test
  ))
}

#' Evaluated the model based on the observed and predicted values.
#'
#' @param predictions A numeric vector with predictions.
#' @param observed A numeric vector with osberved values.
#'
#' @return A list with numeric values describing the accuracy, recall, precision,
#' and f1 scores.
#' @export
#'
#' @examples
#' observed    <- c(1, 1, 1, 1, 0, 0 , 0, 0)
#' predictions <- c(0, 1, 0, 1, 0, 1 , 0, 0)
#' evaluate(predictions, observed)
evaluate <- function(predictions, observed) {
  tp = sum((observed == 1) * (as.numeric(unlist(predictions)) >= 0.5))
  fp = sum((observed == 0) * (as.numeric(unlist(predictions)) >= 0.5))
  tn = sum((observed == 0) * (as.numeric(unlist(predictions)) < 0.5))
  fn = sum((observed == 1) * (as.numeric(unlist(predictions)) < 0.5))

  accuracy  = (tp + tn) / (tp + fp + tn + fn)
  recall    = tp / (tp + fn)
  precision = tp / (tp + fp)
  f1        = 2 * (precision * recall) / (precision + recall)

  return(list(
    accuracy  = accuracy,
    recall    = recall,
    precision = precision,
    f1        = f1
  ))

}

#' Print the provided cat-like input if verbose is TRUE
#'
#' @param ... R objects - strings, (see `cat` documentation).
#' @param sep A character vector of strings to append after each element.
#' @param verbose A logical value indicating whether we want to print the string
#' or not.
#'
#' @export
verbose_cat <- function(..., sep = ' ', verbose = TRUE) {
  if (verbose) {
    cat(..., sep = sep)
  }
}




