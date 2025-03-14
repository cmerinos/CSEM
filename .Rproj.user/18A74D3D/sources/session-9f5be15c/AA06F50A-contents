#' Calculate Overlapping Index (OVI) with Bootstrap Confidence Interval
#'
#' @description
#' This function calculates the Overlapping Index (OVI) between two numeric vectors.
#' The OVI measures the similarity between two distributions, with a value of 1 indicating
#' complete overlap and 0 indicating no overlap. A bootstrap procedure is used to calculate
#' confidence intervals for the OVI.
#'
#' @param x A numeric vector representing the first distribution (e.g., total scores from half1).
#' @param y A numeric vector representing the second distribution (e.g., total scores from half2).
#' @param B An integer specifying the number of bootstrap resamples for the confidence interval. Default is 500.
#' @param conf A numeric value between 0 and 1 indicating the confidence level for the confidence interval. Default is 0.95.
#'
#' @details
#' The Overlapping Index (OVI) is calculated using the `overlapping::overlap()` function.
#' A bootstrap procedure is applied to estimate confidence intervals. If the bootstrap samples
#' are constant or if the procedure fails, the confidence intervals will be reported as `NA`.
#'
#' @return A list with the following elements:
#' \itemize{
#'   \item \strong{OVI}: The Overlapping Index (value between 0 and 1).
#'   \item \strong{CI.lower}: The lower bound of the bootstrap confidence interval.
#'   \item \strong{CI.upper}: The upper bound of the bootstrap confidence interval.
#' }
#'
#' @seealso
#' \code{\link{check.distribution}} for more comprehensive distribution tests.
#' \code{\link[overlapping]{overlap}} for the underlying OVI calculation.
#'
#' @examples
#' # Example with random normal distributions
#' set.seed(123)
#' x <- rnorm(100, mean = 5, sd = 2)
#' y <- rnorm(100, mean = 5.5, sd = 2)
#' calculate.OVI(x, y, B = 500, conf = 0.95)
#'
#' # Example with overlapping distributions
#' x <- rnorm(100, mean = 0, sd = 1)
#' y <- rnorm(100, mean = 0, sd = 1)
#' calculate.OVI(x, y)
#'
#' @export
calculate.OVI <- function(x, y, B = 500, conf = 0.95) {
  
  # Calculate the observed OVI
  observed.OVI <- overlapping::overlap(list(x, y))$OV
  
  # Bootstrap function
  ovi.fun <- function(data, indices) {
    sample1 <- data[indices, 1]
    sample2 <- data[indices, 2]
    overlapping::overlap(list(sample1, sample2))$OV
  }
  
  # Prepare data for bootstrapping
  data.ovi <- cbind(x, y)
  
  # Run bootstrap and handle potential errors
  boot.result <- tryCatch({
    boot::boot(data.ovi, ovi.fun, R = B)
  }, error = function(e) NULL)  # Return NULL if bootstrap fails
  
  # Calculate confidence interval if bootstrap was successful
  if (is.null(boot.result)) {
    ci <- c(NA, NA)
    message("⚠️ Bootstrap failed; returning NA for confidence intervals.")
  } else if (all(boot.result$t == boot.result$t0)) {
    # All bootstrap samples are identical; cannot calculate CI
    ci <- c(NA, NA)
    message("⚠️ Bootstrap samples are identical; confidence intervals cannot be calculated.")
  } else {
    ci <- boot::boot.ci(boot.result, conf = conf, type = "perc")$percent[4:5]
  }
  
  # Round results to 3 decimal places
  result <- list(
    OVI = round(observed.OVI, 3),
    CI.lower = ifelse(!is.na(ci[1]), round(ci[1], 3), NA),
    CI.upper = ifelse(!is.na(ci[2]), round(ci[2], 3), NA)
  )
  
  return(result)
}
