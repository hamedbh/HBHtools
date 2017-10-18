#' Compute Harmonic Mean of a Dataset
#' 
#' This will compute the HM for a set of data
#' @param x An atomic vector of numeric type.
#' @export
#' @examples 
#' hm_mean(1:10)
#' [1] 3.414172

hm_mean <- function(x) {
        (mean(x^(-1), na.rm = TRUE))^-1
}
