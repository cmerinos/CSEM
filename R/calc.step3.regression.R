#' Polynomial Regression Model for Conditional Standard Error of Measurement (CSEM)
#'
#' @description
#' This function fits a **third-degree polynomial regression model** to predict the squared adjusted
#' differences as a function of the total test score. This step is essential in the calculation of the
#' **Mollenkopf-Feldt Conditional Standard Error of Measurement (MF-CSEM)**, as it allows estimating
#' CSEM values for any possible score level.
#'
#' @param squared.diff A numeric vector containing the squared adjusted differences between test halves.
#' @param half1 A matrix or data frame containing the responses for the first half of the test.
#' @param half2 A matrix or data frame containing the responses for the second half of the test.
#'
#' @details
#' This function is part of the MF-CSEM procedure, specifically implementing **Step 3**, where
#' a polynomial regression model is fitted to predict the squared adjusted differences using
#' the total observed test score.
#'
#' The model equation is as follows:
#'
#' \deqn{ Y = \beta_1 X + \beta_2 X^2 + \beta_3 X^3 }
#'
#' where:
#' - \( Y \) represents the squared adjusted differences.
#' - \( X \) is the total test score (sum of both halves).
#' - \( \beta_1, \beta_2, \beta_3 \) are the regression coefficients.
#'
#' The polynomial regression is **forced through the origin (-1 constraint)** to prevent
#' unnecessary bias in the prediction of squared differences.
#'
#' @return
#' A fitted polynomial regression model of class `"lm"`, which can be used to predict
#' squared adjusted differences for any test score.
#'
#' @examples
#' # Simulated example
#' set.seed(123)
#' half1 <- matrix(sample(0:5, 100, replace = TRUE), ncol = 5)
#' half2 <- matrix(sample(0:5, 100, replace = TRUE), ncol = 5)
#'
#' # Compute squared adjusted differences
#' squared.diff <- calc.adjusted.diff(half1, half2)
#'
#' # Fit the polynomial regression model
#' model <- calc.step3.regression(squared.diff, half1, half2)
#'
#' # View model summary
#' summary(model)
#'
#' @references
#' - Mollenkopf, W. G. (1950). The use of item variance in estimating test reliability.
#'   *Psychometrika, 15*(3), 267-273. https://doi.org/10.1007/BF02288893
#' - Feldt, L. S., & Qualls, A. L. (1996). A procedure for estimating the standard error of
#'   measurement for a test score. *Journal of Educational Measurement, 33*(2), 157-172.
#'
#' @seealso
#' - \code{\link{MF.CSEM}} for the complete Mollenkopf-Feldt CSEM procedure.
#' - \code{\link{calc.adjusted.diff}} for calculating squared adjusted differences.
#'
#' @export
calc.step3.regression <- function(squared.diff, half1, half2) {
  total.score <- rowSums(half1) + rowSums(half2)
  df <- data.frame(
    Y = squared.diff,
    X1 = total.score,
    X2 = total.score^2,
    X3 = total.score^3
  )
  model <- lm(Y ~ X1 + X2 + X3 - 1, data = df) # Force through origin
  return(model)
}
