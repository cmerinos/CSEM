#' Compare Location Differences Between Test Halves
#'
#' @description
#' This function evaluates differences in central tendency between two test halves
#' using both parametric and non-parametric tests. It includes the Wilcoxon signed-rank test
#' and Yuen's robust t-test, along with effect size calculations.
#'
#' @param half1 A numeric matrix or data frame containing item scores for the first test half.
#' @param half2 A numeric matrix or data frame containing item scores for the second test half.
#' @param B Integer. The number of bootstrap resamples for confidence intervals (default = 2000).
#' @param conf Numeric. The confidence level for the interval estimation (default = 0.95).
#'
#' @details
#' This function calculates two different tests for assessing location differences between test halves:
#'
#' - **Wilcoxon Signed-Rank Test**: A non-parametric test that compares paired samples.
#' - **Yuen's Robust T-Test**: A robust alternative to the paired t-test that handles heteroscedasticity.
#'
#' The function also estimates effect sizes:
#'
#' - **Wilcoxon Rank Correlation (`rc`)**: Obtained from `rcompanion::wilcoxonPairedRC`.
#' - **AKP Robust Effect Size**: Based on Algina, Keselman, & Penfield (2005), extracted from `WRS2::dep.effect`.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{Test}: The name of the statistical test.
#'   \item \code{Statistic}: The computed test statistic.
#'   \item \code{p_value}: The p-value associated with the test.
#'   \item \code{CI_lower}: The lower bound of the confidence interval for the effect size.
#'   \item \code{CI_upper}: The upper bound of the confidence interval for the effect size.
#'   \item \code{ES}: The effect size estimate.
#' }
#'
#' @examples
#' # Simulated test halves
#' set.seed(123)
#' half1 <- matrix(sample(1:5, 30, replace = TRUE), nrow = 10)
#' half2 <- matrix(sample(1:5, 30, replace = TRUE), nrow = 10)
#'
#' # Compare location differences
#' check.location(half1, half2, B = 1000, conf = 0.95)
#'
#' @export
check.location <- function(half1, half2, B = 2000, conf = 0.95) {
  # Calcular puntuaciones totales
  total1 <- rowSums(half1, na.rm = TRUE)
  total2 <- rowSums(half2, na.rm = TRUE)
  n <- length(total1)

  ## 1️⃣ Prueba de Wilcoxon
  combined <- c(total1, total2)
  g <- factor(rep(c("Half1", "Half2"), each = n))

  # Calcular coeficiente rc con IC
  rc_result <- rcompanion::wilcoxonPairedRC(
    x = combined,
    g = g,
    ci = TRUE,
    R = B,
    type = "perc",
    conf = conf
  )

  wilcox_test <- wilcox.test(total1, total2, paired = TRUE, exact = FALSE)

  ## 2️⃣ Prueba robusta: Yuen's T-test
  yuen_test <- PairedData::yuen.t.test(total1, total2, alternative = "two.sided", paired = TRUE, conf.level = conf)
  effect_robust <- WRS2::dep.effect(x = total1, y = total2)

  # Extraer resultados usando nombres
  akp <- effect_robust[1, c("Est", "ci.low", "ci.up")]

  # Acceder correctamente a los nombres
  ci_low <- akp["ci.low"]
  ci_up <- akp["ci.up"]
  est_akp <- akp["Est"]

  ## 📊 Construir dataframe final
  location_tests <- data.frame(
    Test = c("Wilcoxon Test", "Yuen Robust Test"),
    Statistic = c(wilcox_test$statistic, yuen_test$statistic),
    p_value = c(wilcox_test$p.value, yuen_test$p.value),
    CI_lower = c(rc_result$lower.ci, ci_low),
    CI_upper = c(rc_result$upper.ci, ci_up),
    ES = c(rc_result$rc, est_akp)
  )

  return(location_tests)
}
