#' A Function to Convert Decimal Time to Days, Hours, Minutes
#' 
#' This function allows for easy conversion of time give as decimals to days, 
#' minutes, and hours. Useful for switching between different modes of tracking 
#' time.
#' 
#' @param days Number of days, must be numeric and non-negative
#' @param hours Number of hours, must be numeric and non-negative
#' @export
#' @examples 
#' convert_decimal_time(days = 3, hours = 2.5)
#' [1] "Total time is 3 days, 2 hours, and 30 minutes."
#' 
#' convert_decimal_time(days = 1.5)
#' [1] "Total time is 1 days, 3 hours, and 42 minutes."
#' 
#' convert_decimal_time(hours = 6.25)
#' [1] "Total time is 0 days, 6 hours, and 15 minutes."

convert_decimal_time <- function(days = 0, hours = 0.0) {
        full_days <- 0L
        full_hrs <- 0L
        full_mins <- 0L
        stopifnot(is.numeric(days), 
                  is.numeric(hours), 
                  days >= 0, 
                  hours >= 0)
        
        if (days > 0) {
                hours <- hours + ((as.integer(days) + (days %% 1)) * 7.4)
        }
        
        if (hours > 0) {
                full_days <- hours %/% 7.4
                full_hrs <- (hours %% 7.4) %/% 1
                full_mins <- round(((hours %% 7.4) %% 1) * 60)
        }
        
        paste0("Total time is ", 
               full_days, 
               " days, ", 
               full_hrs, 
               " hours, and ", 
               full_mins, 
               " minutes.")
}
