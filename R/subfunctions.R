#' Calculate log likelihood value
#'
#' @param p
#' @param y
#'
#' @return
#' @export
log_likelihood <- function(p, y) {
  sum(y * log(p) + (1 - y) * log(1 - p))
}

#' Calculate logit value
#'
#' @param q
#'
#' @return
#' @export
logit <- function(q) {
  1 / (1 + exp(-1 * q))
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

#' Creates plots for the correlation heatmaps
#'
#' @param cor_raisin The NxN matrix, describing the correlation matrix for the
#' raisin dataset.
#' @param cor_occupancy The NxN matrix, describing the correlation matrix for
#' the occupancy dataset.
#' @param cor_banknote The NxN matrix, describing the correlation matrix for
#' the banknote dataset.
#'
#' @export

plot_heatmaps <- function(cor_raisin, cor_occupancy, cor_banknote) {

  raisin_heatmap <- ggplot2::ggplot(cor_raisin, ggplot2::aes(cor_raisin$Var1, cor_raisin$Var2)) +
    ggplot2::geom_tile(ggplot2::aes(fill = cor_raisin$value), alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = round(cor_raisin$value, 2))) +
    ggplot2::scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', limits = c(-1, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = 'Correlation heatmap for Raisin dataset',
         x = '',
         y = '') +
    ggplot2::theme(plot.title = ggplot2::element_text(colour = 'black', size = 25),
          legend.position = 'none')

  occupancy_heatmap <- ggplot2::ggplot(cor_occupancy, ggplot2::aes(cor_occupancy$Var1, cor_occupancy$Var2)) +
    ggplot2::geom_tile(ggplot2::aes(fill = cor_occupancy$value), alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = round(cor_occupancy$value, 2))) +
    ggplot2::scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', limits = c(-1, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = 'Correlation heatmap for Occupancy dataset',
         x = '',
         y = '') +
    ggplot2::theme(plot.title = ggplot2::element_text(colour = 'black', size = 25),
          legend.position = 'none')

  banknote_heatmap <- ggplot2::ggplot(cor_banknote, ggplot2::aes(cor_banknote$Var1, cor_banknote$Var2)) +
    ggplot2::geom_tile(ggplot2::aes(fill = cor_banknote$value), alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = round(cor_banknote$value, 2))) +
    ggplot2::scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', limits = c(-1, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = 'Correlation heatmap for Banknote dataset',
         x = '',
         y = '') +
    ggplot2::theme(plot.title = ggplot2::element_text(colour = 'black', size = 25))

  raisin_heatmap + occupancy_heatmap + banknote_heatmap
}
