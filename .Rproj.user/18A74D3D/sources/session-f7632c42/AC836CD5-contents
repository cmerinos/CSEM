#' Compute Adjusted Squared Differences for MF-CSEM
#'
#' @description
#' This function calculates the adjusted squared differences between two test halves.
#' It is used as a key step in the Mollenkopf-Feldt Conditional Standard Error of
#' Measurement (MF-CSEM) method.
#'
#' @param half1 A numeric matrix or data frame containing item scores for the first test half.
#' @param half2 A numeric matrix or data frame containing item scores for the second test half.
#'
#' @details
#' The function computes adjusted differences for each examinee:
#'
#' \deqn{D_i = (X_{half1} - X_{half2}) - (\bar{X}_{half1} - \bar{X}_{half2})}
#'
#' where:
#' - \eqn{X_{half1}}, \eqn{X_{half2}} are the total scores for each half.
#' - \eqn{\bar{X}_{half1}}, \eqn{\bar{X}_{half2}} are the mean scores of each half.
#'
#' The final squared differences are obtained as:
#' \deqn{D_i^2}
#'
#' @return A numeric vector containing the squared adjusted differences for each examinee.
#'
#' @examples
#' # Simulated test halves
#' half1 <- matrix(sample(1:5, 30, replace = TRUE), nrow = 10)
#' half2 <- matrix(sample(1:5, 30, replace = TRUE), nrow = 10)
#'
#' # Compute adjusted squared differences
#' calc.adjusted.diff(half1, half2)
#'
#' @export
calc.adjusted.diff <- function(half1, half2) {
  mean_half1 <- mean(rowSums(half1, na.rm = TRUE))
  mean_half2 <- mean(rowSums(half2, na.rm = TRUE))
  adjusted_diff <- (rowSums(half1) - rowSums(half2)) - (mean_half1 - mean_half2)
  squared_diff <- adjusted_diff^2
  return(squared_diff)
}
