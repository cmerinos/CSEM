#' Calculate Angoff Coefficient
#'
#' @param half1 First half of the items
#' @param half2 Second half of the items
#' @param B Number of bootstrap resamples (default = 1000)
#' @param conf Confidence level for the confidence interval (default = 0.95)
#'
#' @return Data frame with the Angoff coefficient and confidence interval
#'
#' @export
check.angoff <- function(half1, half2, B = 1000, conf = 0.95) {

  # Calculate total scores
  total1 <- rowSums(half1, na.rm = TRUE)
  total2 <- rowSums(half2, na.rm = TRUE)

  # Calculate Angoff coefficient using the original formula
  cov.val <- cov(total1, total2, use = "complete.obs")
  var1 <- var(total1, na.rm = TRUE)
  var2 <- var(total2, na.rm = TRUE)

  # Apply the original formula
  angoff.val <- 4 * cov.val / (var1 + var2 + 2 * cov.val)

  # Bootstrap for confidence interval
  boot.angoff <- boot::boot(data = cbind(total1, total2),
                            statistic = function(data, i) {
                              t1 <- data[i, 1]
                              t2 <- data[i, 2]
                              cov.val <- cov(t1, t2, use = "complete.obs")
                              var1 <- var(t1, na.rm = TRUE)
                              var2 <- var(t2, na.rm = TRUE)
                              4 * cov.val / (var1 + var2 + 2 * cov.val)
                            }, R = B)

  ci <- tryCatch({
    boot::boot.ci(boot.angoff, conf = conf, type = "perc")$percent[4:5]
  }, error = function(e) c(NA, NA))

  # Return results
  result <- data.frame(
    Coefficient = "Angoff Coefficient",
    Estimate = round(angoff.val, 4),
    CI.lower = ci[1],
    CI.upper = ci[2]
  )

  return(result)
}
