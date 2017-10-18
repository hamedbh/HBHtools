#' Compute Geometric Mean of a Dataset
#' 
#' This will compute the GM for a set of data
#' @param x An atomic vector of numeric type.
#' @export
#' @examples 
#' gm_mean(1:10)
#' [1] 4.528729

gm_mean <- function(x, na.rm = TRUE) {
        exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
        
}
