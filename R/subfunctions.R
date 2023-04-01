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
