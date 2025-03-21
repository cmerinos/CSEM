#' Compute Angoff Coefficient for Reliability Estimation
#'
#' @description
#' This function calculates the **Angoff-Feldt coefficient**, a reliability estimate
#' used for tests split into two halves of possibly **unequal lengths**. It provides
#' a more flexible alternative to **Spearman-Brown**, particularly when the two halves
#' of the test are not strictly parallel.
#'
#' @param half1 A numeric matrix or data frame containing item scores for the first test half.
#' @param half2 A numeric matrix or data frame containing item scores for the second test half.
#' @param B Integer. Number of bootstrap resamples for confidence interval estimation (default = 1000).
#' @param conf Numeric. Confidence level for the confidence interval (default = 0.95).
#'
#' @details
#' The **Angoff coefficient** is computed using the formula:
#'
#' \deqn{\rho_{Angoff} = \frac{4 \text{cov}(X_1, X_2)}{\text{Var}(X_1) + \text{Var}(X_2) + 2 \text{cov}(X_1, X_2)}}
#'
#' where:
#' - \eqn{X_1} and \eqn{X_2} are the total scores from each test half.
#' - \eqn{\text{Var}(X_1)} and \eqn{\text{Var}(X_2)} are their variances.
#' - \eqn{\text{cov}(X_1, X_2)} is the covariance between the two halves.
#'
#' Confidence intervals are estimated via **bootstrap resampling**.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{Coefficient}: The name of the reliability coefficient ("Angoff Coefficient").
#'   \item \code{Estimate}: The estimated Angoff reliability coefficient.
#'   \item \code{CI.lower}: The lower bound of the confidence interval.
#'   \item \code{CI.upper}: The upper bound of the confidence interval.
#' }
#'
#' @references
#' Domínguez-Lara, S., Merino-Soto, C. & Navarro-Loli, J. (2016). Estimación de la Confiabilidad en Mediciones de Dos ítems: el Coeficiente Angoff-Feldt.
#' Revista Digital de Investigación en Docencia Universitaria, 10(1), 34-40. \doi{10.19083/ridu.10.463}
#'
#' Angoff, W. H. (1953). Test reliability and effective test length.
#' Psychometrika, 18(1), 1-14. \doi{10.1007/BF02289023}
#'
#' Feldt, L. S. (2002). Reliability Estimation When a Test Is Split Into Two Parts of Unknown Effective Length.
#' Measurement in Education, 15(3), 295-308. \doi{10.1207/S15324818AME1503_4}
#'
#' Feldt, L. S., & Brennan, R. L. (1989). Reliability. In Linn, R. L. (Ed), Educational Measurement (pp. 105 – 146). New York: Macmillan.
#'
#' Feldt, L. S., & Charter, R. A. (2003). Estimating the reliability of a test split into two parts of equal or unequal length.
#' Psychological Methods, 8(1), 102-109. \doi{10.1037/1082-989X.8.1.102}
#'
#' @seealso
#' \code{\link[lambda4]{angoff}}, \code{\link[psych]{alpha}}
#'
#' @examples
#' # Simulated test halves
#' set.seed(123)
#' half1 <- matrix(sample(1:5, 30, replace = TRUE), nrow = 10)
#' half2 <- matrix(sample(1:5, 30, replace = TRUE), nrow = 10)
#'
#' # Compute Angoff coefficient
#' check.angoff(half1, half2, B = 1000, conf = 0.95)
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
