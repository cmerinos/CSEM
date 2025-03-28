#' Assess Scale Differences Between Test Halves
#'
#' @description
#' This function evaluates differences in variability between two test halves using:
#'
#' - **Bonett-Seier Test**: A statistical test for comparing variances in paired samples.
#' - **Log-Transformed Variability Ratio (lnVR)**: A standardized effect size for variability differences.
#'
#' @param half1 A numeric matrix or data frame containing item scores for the first test half.
#' @param half2 A numeric matrix or data frame containing item scores for the second test half.
#' @param conf Numeric. Confidence level for the interval estimation (default = 0.95).
#'
#' @details
#' This function computes two key statistics to assess variability differences:
#'
#' - **Bonett-Seier Test**: A robust method for testing variance equality between paired samples.
#'   The test statistic follows an asymptotic chi-squared distribution.
#'
#' - **Log-Transformed Variability Ratio (lnVR)**: Defined as:
#'
#'   \deqn{\ln(VR) = \ln\left(\frac{SD_1}{SD_2}\right)}
#'
#'   where:
#'   - \eqn{SD_1} and \eqn{SD_2} are the standard deviations of the two halves.
#'
#'   The function estimates **confidence intervals for lnVR** using `metafor::escalc()`.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{Test}: The name of the statistical test.
#'   \item \code{Statistic}: The test statistic value.
#'   \item \code{p_value}: The p-value associated with the Bonett-Seier test.
#'   \item \code{CI_lower}: The lower bound of the confidence interval for lnVR.
#'   \item \code{CI_upper}: The upper bound of the confidence interval for lnVR.
#'   \item \code{ES}: The estimated log-transformed variability ratio (lnVR).
#' }
#'
#' @references
#' Bonett, D. G., & Seier, E. (2002). Confidence intervals for variance and standard deviation
#' ratios. *Computational Statistics & Data Analysis, 40*(3), 603-608. \doi{10.1016/S0167-9473(02)00069-3}
#'
#' Nakagawa, S., & Cuthill, I. C. (2007). Effect size, confidence interval and statistical significance:
#' A practical guide for biologists. *Biological Reviews, 82*(4), 591-605. \doi{10.1111/j.1469-185X.2007.00027.x}
#'
#' @examples
#' # Simulated test halves
#' set.seed(123)
#' half1 <- matrix(sample(1:5, 30, replace = TRUE), nrow = 10)
#' half2 <- matrix(sample(1:5, 30, replace = TRUE), nrow = 10)
#'
#' # Compare scale differences
#' check.scale(half1, half2, conf = 0.95)
#'
#' @export
check.scale <- function(half1, half2, conf = 0.95) {
  # Calcular puntuaciones totales
  total1 <- rowSums(half1, na.rm = TRUE)
  total2 <- rowSums(half2, na.rm = TRUE)

  # Calcular prueba Bonett-Seier para varianzas
  bs_test <- PairedData::bonettseier.Var.test(total1, total2, conf.level = conf)

  # Calcular log‐transformed variability ratio (lnVR) con metafor
  lnvr <- metafor::escalc(measure = "VRC",
                          sd1i = sd(total1),
                          sd2i = sd(total2),
                          ri = cor(total1, total2),
                          ni = NROW(total1))

  # Resumen para obtener IC
  lnvr_summary <- summary(lnvr)

  # Extraer resultados relevantes
  scale_tests <- data.frame(
    Test = "Bonett-Seier Test (con lnVR)",
    Statistic = round(bs_test$statistic, 3),
    p_value = round(bs_test$p.value, 5),
    CI_lower = round(lnvr_summary$ci.lb, 3),
    CI_upper = round(lnvr_summary$ci.ub, 3),
    ES = round(lnvr_summary$yi, 3)
  )

  return(scale_tests)
}
