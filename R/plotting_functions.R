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
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient2 theme_minimal labs theme
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

#' Create boxplots from multiple experiment
#'
#' @param df A data.frame from multiple experiment.
#'
#' @return A grid of 4 boxplots for accuracy, recall, precision and f1.
#' @export
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient2 theme_minimal labs theme geom_boxplot element_text
boxplots <- function(df) {
  b_acc <- ggplot(data = df, aes(x = df$model_names, y = df$accuracy)) +
    geom_boxplot(alpha = 0.3, size = 1) +
    theme_minimal() +
    labs(title = 'Box plot of the accuracy for different models',
         x = 'Model name',
         y = 'Accuracy') +
    theme(plot.title = element_text(colour = 'black', size = 25),
          axis.title.x = element_text(colour = 'black', size = 15),
          axis.title.y = element_text(colour = 'black', size = 15))

  b_rec <- ggplot(data = df, aes(x = df$model_names, y = df$recall)) +
    geom_boxplot(alpha = 0.3, size = 1) +
    theme_minimal() +
    labs(title = 'Box plot of the recall for different models',
         x = 'Model name',
         y = 'Recall') +
    theme(plot.title = element_text(colour = 'black', size = 25),
          axis.title.x = element_text(colour = 'black', size = 15),
          axis.title.y = element_text(colour = 'black', size = 15))

  b_pre <- ggplot(data = df, aes(x = df$model_names, y = df$precision)) +
    geom_boxplot(alpha = 0.3, size = 1) +
    theme_minimal() +
    labs(title = 'Box plot of the precision for different models',
         x = 'Model name',
         y = 'Precision') +
    theme(plot.title = element_text(colour = 'black', size = 25),
          axis.title.x = element_text(colour = 'black', size = 15),
          axis.title.y = element_text(colour = 'black', size = 15))

  b_f1 <- ggplot(data = df, aes(x = df$model_names, y = df$f1)) +
    geom_boxplot(alpha = 0.3, size = 1) +
    theme_minimal() +
    labs(title = 'Box plot of the f1 for different models',
         x = 'Model name',
         y = 'Precision') +
    theme(plot.title = element_text(colour = 'black', size = 25),
          axis.title.x = element_text(colour = 'black', size = 15),
          axis.title.y = element_text(colour = 'black', size = 15))

  (b_acc + b_rec) / (b_pre + b_f1)
}
