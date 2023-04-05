#' Banknote dataset
#'
#' Data were extracted from images that were taken from genuine and forged
#' banknote-like specimens. For digitization, an industrial camera usually
#' used for print inspection was used. The final images have 400x 400 pixels.
#' Due to the object lens and distance to the investigated object gray-scale
#' pictures with a resolution of about 660 dpi were gained. Wavelet Transform
#' tool were used to extract features from images.
#'
#'
#' @format A data frame with 1372 rows and 5 variables:
#' \describe{
#'   \item{X1}{numeric, variance of Wavelet Transformed image}
#'   \item{X2}{numeric, skewness of Wavelet Transformed image}
#'   \item{X3}{numeric, curtosis of Wavelet Transformed image}
#'   \item{X4}{numeric, ntropy of image}
#'   \item{y}{integer, class}
#' }
#' @name banknote
#' @docType data
#' @usage data(banknote)
#'
#' @source Data from UCL \url{https://archive.ics.uci.edu/ml/datasets/banknote+authentication}
#'
NULL
