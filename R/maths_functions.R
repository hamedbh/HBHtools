#' Compute Harmonic Mean of a Dataset
#'
#' This will compute the HM for a set of data
#' @param x An atomic vector of numeric type.
#' @export
#' @examples
#' hm_mean(1:10)
#' 3.414172

hm_mean <- function(x) {
        (mean(x^(-1), na.rm = TRUE))^-1
}

#' Compute Geometric Mean of a Dataset
#'
#' This will compute the GM for a set of data
#' @param x An atomic vector of numeric type.
#' @export
#' @examples
#' gm_mean(1:10)
#' 4.528729

gm_mean <- function(x, na.rm = TRUE) {
        exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))

}
