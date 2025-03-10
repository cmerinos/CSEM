#' @title  Calculate Cronbach's Alpha for Two Halves
#'
#' @param half1 First half of the items
#' @param half2 Second half of the items
#' @param B Number of bootstrap resamples (default = 1000)
#' @param conf Confidence level for the confidence interval (default = 0.95)
#'
#' @return Data frame with Cronbach's alpha estimates and confidence intervals
#'
#' @details
#' `check.alpha` calcula el coeficient alfa (Cronbach, 1951) para cada mitad, más
#' intervalos de confianza usando bootstrap resamples.
#'
#' @examples
#'### Example 1 ----------
#'check.alpha(half1 = dbase[, 1:5], half2 = dbase[, 6:10], B = 1000, conf = .95)
#'
#'
#'
#' @references
#' Cronbach, L. J. (1951). Coefficient alpha and the internal structure of tests.
#'  Psychometrika, 16, 297-334.
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

