#' A function used to compare different models to our method.
#'
#' The function trains 6 different models:
#' \describe{
#'   \item{\code{DS23IRLS::irls}}{Our irls model without interactions.}
#'   \item{\code{DS23IRLS::irls}}{Our irls model with interactions.}
#'   \item{\code{msme::irls}}{The irls model from msme package.}
#'   \item{\code{MASS::lda}}{The lda model from MASS package.}
#'   \item{\code{MASS::qda}}{The qda model from MASS package.}
#'   \item{\code{class::knn}}{The knn model from class package.}
#' }
#' And later evaluates the with 4 metrics: accuracy, recall, precision, and f1.
#'
#' @param train A data.frame object being the training subset with target column
#' as the last one.
#' @param test A data.frame object being the testing subset with target column
#' as the last one.
#' @param tol A difference between log-likelihood function values between two iterations below which the algorithm should stop.
#' @param interactions A numeric vector introducing the interactions for the model.
#' Default value: NULL.
#' @param verbose A logical value indicating whether we want to have prints or not.
#'
#' @return A data.frame object with the models names and metrics values.
#' @export
compare_models <- function(train, test, tol = 0.0001, interactions = NULL, verbose = FALSE) {

  verbose_cat('Training the models...\n', verbose = verbose)

  form_string <- capture.output(cat(names(train)[length(train)], '~ .'))

  our_irls     <- DS23IRLS::irls(X = train[, -ncol(train)],
                                 y = train[, ncol(train)],
                                 tol = tol,
                                 verbose = verbose)
  our_irls_int <- DS23IRLS::irls(X = train[, -ncol(train)],
                                 y = train[, ncol(train)],
                                 tol = tol,
                                 interactions = interactions,
                                 verbose = verbose)
  msme_irls    <- msme::irls(as.formula(form_string),
                             tol = tol,
                             data = train,
                             family = 'binomial',
                             link = 'logit')
  LDA          <- MASS::lda(as.formula(form_string),
                      tol = tol,
                      data = train)
  QDA          <- MASS::qda(as.formula(form_string),
                      tol = tol,
                      data = train)
  KNN          <- class::knn(train = train[, -ncol(train)],
                      test = test[, -ncol(test)],
                      cl = as.factor(train[, ncol(train)]),
                      k = 3)

  verbose_cat('Creating predictions...\n', verbose = verbose)

  preds_our_irls     <- predict(our_irls, test[, -ncol(test)], prob = FALSE)
  preds_our_irls_int <- predict(our_irls_int, test[, -ncol(test)], prob = FALSE)
  preds_msme_irls    <- logit(as.matrix(test[, -ncol(test)]) %*% as.matrix(msme_irls$coefficients)[-1] + as.matrix(msme_irls$coefficients)[1])
  preds_msme_irls    <- ifelse(preds_msme_irls > 0.5, 1, 0)
  preds_LDA          <- predict(LDA, test[, -ncol(test)], prob = FALSE)
  preds_QDA          <- predict(QDA, test[, -ncol(test)], prob = FALSE)
  preds_KNN          <- KNN

  verbose_cat('Evaluating the models...\n', verbose = verbose)

  eval_our_irls      <- evaluate(preds_our_irls, test[, ncol(test)])
  eval_our_irls_int  <- evaluate(preds_our_irls_int, test[, ncol(test)])
  eval_msme_irls     <- evaluate(preds_msme_irls, test[, ncol(test)])
  eval_LDA           <- evaluate(preds_LDA, test[, ncol(test)])
  eval_QDA           <- evaluate(preds_QDA, test[, ncol(test)])
  eval_KNN           <- evaluate(preds_KNN, as.factor(test[, ncol(test)]))

  model_names <- c('our_irls', 'our_irls_int', 'msme_irls', 'LDA', 'QDA', 'KNN')
  accuracy    <- c(eval_our_irls$accuracy, eval_our_irls_int$accuracy, eval_msme_irls$accuracy,
                   eval_LDA$accuracy, eval_QDA$accuracy, eval_KNN$accuracy)
  recall      <- c(eval_our_irls$recall, eval_our_irls_int$recall, eval_msme_irls$recall,
                   eval_LDA$recall, eval_QDA$recall, eval_KNN$recall)
  precision   <- c(eval_our_irls$precision, eval_our_irls_int$precision, eval_msme_irls$precision,
                   eval_LDA$precision, eval_QDA$precision, eval_KNN$precision)
  f1          <- c(eval_our_irls$f1, eval_our_irls_int$f1, eval_msme_irls$f1,
                   eval_LDA$f1, eval_QDA$f1, eval_KNN$f1)

  df <- data.frame(model_names, accuracy, recall, precision, f1)

  verbose_cat('DONE \n', verbose = verbose)
  return(df)
}

#' Runs a multiple experiment on different train and test subset sizes.
#'
#' @param p A numeric vector, where every value is between 0 and 1. Describes
#' the training set size (the testing is 1 - p).
#' @param include_occupancy A logical value, describing if we should include
#' computations for a time consuming occupancy dataset.
#' @param verbose A logical value indicating whether we want to print the string
#' or not.
#'
#' @return A list of data frames with scores from multiple expermients.
#' @export
#' @importFrom stats as.formula predict
#' @importFrom utils capture.output data
multiple_exp <- function(p = c(0.9, 0.8, 0.7, 0.6, 0.5), include_occupancy = TRUE, verbose = FALSE) {
  out_raisin    <- data.frame()
  out_occupancy <- data.frame()
  out_banknote  <- data.frame()

  raisin    <- DS23IRLS::raisin
  occupancy <- DS23IRLS::occupancy
  banknote  <- DS23IRLS::banknote

  raisin    <- raisin[, -c(1, 2, 3, 5)]
  occupancy <- occupancy[, -5]
  banknote  <- banknote[, -3]

  occupancy <- occupancy[1:2000, ]

  for (i in 1:length(p)) {
    verbose_cat('Iteration', i, 'out of', length(p), '\n', verbose = verbose)
    raisin_split    <- train_test_split(raisin, p = c(p[i], 1 - p[i]))
    occupancy_split <- train_test_split(occupancy, p = c(p[i], 1 - p[i]))
    banknote_split  <- train_test_split(banknote, p = c(p[i], 1 - p[i]))

    train           <- raisin_split$train
    test            <- raisin_split$test
    comp_raisin     <- compare_models(train, test, interactions = data.frame(a = c('Perimeter'), b = c('Extent')))

    if (include_occupancy) {
      train           <- occupancy_split$train
      test            <- occupancy_split$test
      comp_occupancy  <- compare_models(train, test, interactions = data.frame(a = c('Light'), b = c('Temperature')))
    } else {
      comp_occupancy <- NULL
    }

    train           <- banknote_split$train
    test            <- banknote_split$test
    comp_banknote   <- compare_models(train, test, interactions = data.frame(a = c('X4'), b = c('X2')))

    out_raisin      <- rbind(out_raisin, comp_raisin)
    out_occupancy   <- rbind(out_occupancy, comp_occupancy)
    out_banknote    <- rbind(out_banknote, comp_banknote)
  }

  multiple_eval <- list(out_raisin = out_raisin,
                        out_occupancy = out_occupancy,
                        out_banknote = out_banknote)
  return(multiple_eval)
}

#' Runs a multiple experiment on artificial data in order to retrieve model coefficients.
#'
#' @param ds A function generating a dataset, the features should be in columns 'x1', 'x2', ..., and the target variable should be in column 'y'
#' @param L Number of repetitions.
#' @param verbose A logical value indicating whether we want to print the string
#' or not.
#'
#' @return A dataframe with model coefficients
#' @export
multiple_coef <- function(ds, L = 50, verbose = FALSE) {
  df <- ds()
  n_col <- length(colnames(df)) -1

  betas <- sapply(
    1:L,
    function(i){
      df <- ds()
      X <- as.matrix(df[paste0('x', 1:n_col)])
      model <- DS23IRLS::irls(X, df$y, verbose=verbose)
      model$beta
    }
  )

  betas_names <- paste0('b', 0:n_col)
  coefficient <- c()
  value <- c()
  for(i in 0:n_col){
    coefficient <- c(coefficient, rep(paste0('b', i), L))
    value <- c(value, betas[i+1,])
  }

  data.frame(
    coefficient = coefficient,
    value = value
  )
}

#' Runs a multiple experiment on artificial data in order to check if method converges.
#'
#' @param ds A function generating a dataset, the features should be in columns 'x1', 'x2', ..., and the target variable should be in column 'y'
#' @param L Number of repetitions.
#' @param verbose A logical value indicating whether we want to print the string
#' or not.
#'
#' @return A percentage value of the experiments in which the method did not converge.
#' @export
multiple_not_converge <- function(ds, L = 50, verbose = FALSE) {
  df <- ds()
  n_col <- length(colnames(df)) -1

  results <- unlist(lapply(
    1:L,
    function(i){
      df <- ds()
      X <- as.matrix(df[paste0('x', 1:n_col)])
      model <- try(DS23IRLS::irls(X, df$y))
      if(inherits(model, "try-error")){
        TRUE
      } else {
        FALSE
      }
    }
  ))

  mean(results) * 100
}

#' Runs a multiple experiment on artificial data in order to check the influence of adding interactions to model on model accuracy.
#'
#' @param ds A function generating a dataset, the target variable should be in column 'y'.
#' @param L Number of repetitions.
#' @param verbose A logical value indicating whether we want to print the string
#' or not.
#' @param interactions A dataframe in which rows define the interactions.
#'
#' @return A dataframe containing the accuracy scores for models with interactions and without interactions.
#' @export
multiple_compare_interactions <- function(ds, interactions, L = 50, verbose = FALSE) {
  df <- ds()
  cols <- colnames(df)
  cols <- cols[!cols %in% c('y')]

  results <- sapply(
    1:L,
    function(i){
      df <- ds()

      split <- DS23IRLS::train_test_split(df, seed=i)
      X_train <- as.matrix(split$train[cols])
      y_train <- split$train$y
      X_test <- split$test[cols]
      y_test <- split$test$y


      model <-DS23IRLS::irls(X_train, y_train)
      acc1 <- DS23IRLS::evaluate(y_test, predict(model, X_test, prob=FALSE))$accuracy

      model <-DS23IRLS::irls(X_train, y_train, interactions =interactions)
      acc2 <- DS23IRLS::evaluate(y_test, predict(model, X_test, prob=FALSE))$accuracy

      c(acc1, acc2)
    })

  results <- data.frame(
    interactions = c(rep('without', L), rep('with', L)),
    accuracy = c(results[1,], results[2,])
  )

  results
}
