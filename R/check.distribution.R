#' Distribution Comparisons Between Test Halves
#'
#' @description
#' This function evaluates distributional differences between two test halves using:
#'
#' - **Anderson-Darling Test**: A non-parametric test for comparing distributions.
#' - **Overlapping Index (OVI)**: A measure of the overlap between two distributions.
#' - **Kendall's W**: A non-parametric measure of concordance between ranked data.
#'
#' @param half1 A numeric matrix or data frame containing item scores for the first test half.
#' @param half2 A numeric matrix or data frame containing item scores for the second test half.
#' @param B Integer. Number of bootstrap resamples for confidence interval estimation (default = 2000).
#' @param conf Numeric. Confidence level for the confidence interval (default = 0.95).
#'
#' @details
#' This function performs three different distributional tests:
#'
#' - **Anderson-Darling Test**: A modification of the Kolmogorov-Smirnov test that places
#'   more weight on the tails of the distributions. A low p-value indicates that the two
#'   distributions are significantly different.
#'
#' - **Overlapping Index (OVI)**: Measures the proportion of overlap between the two distributions.
#'   Higher values indicate greater similarity between the distributions.
#'
#' - **Kendall's W**: Measures agreement between two ranked distributions. Values range
#'   from 0 (no agreement) to 1 (perfect agreement).
#'
#' Confidence intervals for OVI and Kendall's W are estimated using **bootstrap resampling**.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{Test}: The name of the statistical test.
#'   \item \code{Statistic}: The test statistic (if applicable).
#'   \item \code{p.value}: The p-value associated with the test (for Anderson-Darling only).
#'   \item \code{CI.lower}: The lower bound of the confidence interval.
#'   \item \code{CI.upper}: The upper bound of the confidence interval.
#'   \item \code{ES}: The estimated effect size for each test.
#' }
#'
#' @references
#' Anderson, T. W., & Darling, D. A. (1952). Asymptotic theory of certain "goodness of fit" criteria based on stochastic processes.
#' *Annals of Mathematical Statistics, 23*(2), 193-212. \doi{10.1214/aoms/1177729437}
#'
#' Mason, S. J., & Graham, N. E. (2002). Areas beneath the relative operating characteristics
#' (ROC) and relative operating levels (ROL) curves: Statistical significance and interpretation.
#' *Quarterly Journal of the Royal Meteorological Society, 128*(601), 2145-2166. \doi{10.1256/qj.01.192}
#'
#' Kendall, M. G., & Smith, B. B. (1939). The problem of m rankings.
#' *Annals of Mathematical Statistics, 10*(3), 275-287. \doi{10.1214/aoms/1177732186}
#'
#' @examples
#' # Simulated test halves
#' set.seed(123)
#' half1 <- matrix(sample(1:5, 30, replace = TRUE), nrow = 10)
#' half2 <- matrix(sample(1:5, 30, replace = TRUE), nrow = 10)
#'
#' # Compare distributional differences
#' check.distribution(half1, half2, B = 2000, conf = 0.95)
#'
#' @export
check.distribution <- function(half1, half2, B = 1000, conf = 0.95) {

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
