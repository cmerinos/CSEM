#' Distribution Tests: Anderson-Darling, Overlapping Index, Kendall's W
#'
#' @param half1 First half of the items
#' @param half2 Second half of the items
#' @param B Number of bootstrap resamples (default = 2000)
#' @param conf Confidence level for the confidence interval (default = 0.95)
#'
#' @return Data frame with the results of the distribution tests
#'
#' @export
check.distribution <- function(half1, half2, B = 2000, conf = 0.95) {

  # Calculate total scores
  total1 <- rowSums(half1, na.rm = TRUE)
  total2 <- rowSums(half2, na.rm = TRUE)
  n <- length(total1)

  # 1️⃣ Anderson-Darling Test (only version 1)
  ad.test <- kSamples::ad.test(total1, total2)
  ad.stat <- ad.test$ad[1, 1]  # Column 1: AD statistic
  ad.p <- ad.test$ad[1, 3]     # Column 3: p-value

  # Calculate experimental effect size (ES)
  es.ad <- (ad.stat / sqrt(n)) / log(n + 1)

  # 2️⃣ Overlapping Index (OVI) with bootstrap confidence interval
  ovi.result <- calculate.OVI(total1, total2, B = B, conf = conf)

  # 3️⃣ Kendall's W with bootstrap confidence interval
  kendall.res <- rcompanion::kendallW(t(rbind(total1, total2)), ci = TRUE, R = B, type = "perc", conf = conf)
  kendall.val <- kendall.res$W
  ci.kendall <- c(kendall.res$lower.ci, kendall.res$upper.ci)

  # Create final dataframe
  distribution.tests <- data.frame(
    Test = c("Anderson-Darling Test", "Overlapping Index (OVI)", "Kendall W"),
    Statistic = c(round(ad.stat, 3), round(ovi.result$OVI, 3), NA),
    p.value = c(round(ad.p, 5), NA, NA),
    CI.lower = c(NA, ovi.result$CI.lower, ci.kendall[1]),
    CI.upper = c(NA, ovi.result$CI.upper, ci.kendall[2]),
    ES = c(round(es.ad, 3), round(ovi.result$OVI, 3), round(kendall.val, 3))
  )

  return(distribution.tests)
}
