#' Polynomial Regression Model for CSEM
#'
#' @param squared.diff Vector of adjusted squared differences
#' @param half1 First half of the items
#' @param half2 Second half of the items
#'
#' @return Fitted polynomial regression model
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
  model <- lm(Y ~ X1 + X2 + X3 - 1, data = df)
  return(model)
}
