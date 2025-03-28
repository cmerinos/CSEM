#' Compute Cronbach's Alpha for Two Test Halves
#'
#' @description
#' This function calculates Cronbach's alpha (Cronbach, 1951) for each test half and estimates
#' confidence intervals using bootstrap resampling.
#'
#' @param half1 A numeric matrix or data frame containing item scores for the first test half.
#' @param half2 A numeric matrix or data frame containing item scores for the second test half.
#' @param B Integer. The number of bootstrap resamples for confidence interval estimation (default = 1000).
#' @param conf Numeric. The confidence level for the interval estimation (default = 0.95).
#'
#' @details
#' Cronbach's alpha is a measure of internal consistency reliability, computed as:
#'
#' \deqn{\alpha = \frac{k \bar{r}}{1 + (k - 1) \bar{r}}}
#'
#' where:
#' - \eqn{k} is the number of items in the test half.
#' - \eqn{\bar{r}} is the average inter-item correlation.
#'
#' The function estimates Cronbach's alpha separately for both halves and provides
#' confidence intervals using bootstrap resampling.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{Half}: Indicates which half was analyzed ("Half 1" or "Half 2").
#'   \item \code{Coefficient}: The name of the reliability coefficient ("alpha").
#'   \item \code{Estimate}: The estimated Cronbach's alpha.
#'   \item \code{CI.lower}: The lower bound of the confidence interval.
#'   \item \code{CI.upper}: The upper bound of the confidence interval.
#' }
#'
#' @examples
#' # Simulated test halves
#' set.seed(123)
#' half1 <- matrix(sample(1:5, 50, replace = TRUE), nrow = 10)
#' half2 <- matrix(sample(1:5, 50, replace = TRUE), nrow = 10)
#'
#' # Compute Cronbach's alpha with bootstrapped confidence intervals
#' check.alpha(half1, half2, B = 1000, conf = 0.95)
#'
#' @references
#' Cronbach, L. J. (1951). Coefficient alpha and the internal structure of tests.
#' Psychometrika, 16(3), 297-334.
#'
#' @seealso
#' \code{\link[lavaan]{cfa}}, \code{\link[scripty]{mimic}}
#'
#' @export
check.alpha <- function(half1, half2, B = 1000, conf = 0.95) {

  # Calculate Cronbach's alpha for a given half
  cronbach.alpha <- function(x) {
    k <- ncol(x)
    r_avg <- mean(cor(x, use = "pairwise.complete.obs"))
    (k * r_avg) / (1 + (k - 1) * r_avg)
  }

  # Calculate alpha for half1
  alpha1 <- cronbach.alpha(half1)

  # Bootstrap for confidence interval for alpha(half1)
  boot.alpha1 <- boot::boot(half1, statistic = function(d, i) {
    cronbach.alpha(d[i, ])
  }, R = B)

  ci1 <- tryCatch({
    boot::boot.ci(boot.alpha1, conf = conf, type = "perc")$percent[4:5]
  }, error = function(e) c(NA, NA))

  # Calculate alpha for half2
  alpha2 <- cronbach.alpha(half2)

  # Bootstrap for confidence interval for alpha(half2)
  boot.alpha2 <- boot::boot(half2, statistic = function(d, i) {
    cronbach.alpha(d[i, ])
  }, R = B)

  ci2 <- tryCatch({
    boot::boot.ci(boot.alpha2, conf = conf, type = "perc")$percent[4:5]
  }, error = function(e) c(NA, NA))

  # Return results
  result <- data.frame(
    Half = c("Half 1", "Half 2"),
    Coefficient = "alpha",
    Estimate = round(c(alpha1, alpha2), 4),
    CI.lower = c(ci1[1], ci2[1]),
    CI.upper = c(ci1[2], ci2[2])
  )

  return(result)
}

