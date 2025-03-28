#' Compute Omega Total Reliability for Test Halves
#'
#' @description
#' This function estimates **McDonald's Omega Total** for two test halves using
#' **confirmatory factor analysis (CFA)** with the `lavaan` package. It provides
#' confidence intervals via bootstrap resampling.
#'
#' @param half1 A numeric matrix or data frame containing item scores for the first test half.
#' @param half2 A numeric matrix or data frame containing item scores for the second test half.
#' @param B Integer. Number of bootstrap resamples for confidence interval estimation (default = 1000).
#' @param conf Numeric. Confidence level for the confidence interval (default = 0.95).
#'
#' @details
#' **McDonald's Omega Total** is an alternative to **Cronbach's Alpha**, based on a **single-factor CFA model**:
#'
#' - The model is estimated using `lavaan::cfa()`, with **ULSMV** as the estimator.
#' - Factor loadings (\eqn{\lambda}) are extracted and used to compute **Omega Total**:
#'
#' \deqn{\omega = \frac{(\sum \lambda)^2}{(\sum \lambda)^2 + \sum (1 - h^2)}}
#'
#' where:
#' - \eqn{\lambda} represents standardized factor loadings.
#' - \eqn{h^2} is the communality of each item.
#'
#' If the CFA model fails to converge, the function returns `NA` for the affected test half.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{Half}: Indicates whether the estimate is for "Half 1" or "Half 2".
#'   \item \code{Coefficient}: The reliability coefficient ("Omega Total").
#'   \item \code{Estimate}: The estimated Omega Total value.
#'   \item \code{CI_lower}: The lower bound of the confidence interval.
#'   \item \code{CI_upper}: The upper bound of the confidence interval.
#' }
#'
#' @references
#' McDonald, R. P. (1999). Test theory: A unified treatment. Mahwah, NJ: Erlbaum.
#'
#' Revelle, W., & Zinbarg, R. E. (2009). Coefficients Alpha, Beta, Omega, and the glb:
#' Comments on Sijtsma. *Psychometrika, 74*(1), 145–154. \doi{10.1007/s11336-008-9102-z}
#'
#' Rodriguez, A., Reise, S. P., & Haviland, M. G. (2016). Evaluating the performance
#' of McDonald's Omega with structural equation modeling. *Psychological Methods, 21*(2), 137-150.
#' \doi{10.1037/met0000045}
#'
#' @examples
#' # Simulated test halves
#' set.seed(123)
#' half1 <- matrix(sample(1:5, 30, replace = TRUE), nrow = 10)
#' half2 <- matrix(sample(1:5, 30, replace = TRUE), nrow = 10)
#'
#' # Compute Omega Total for each half
#' check.omega(half1, half2, B = 1000, conf = 0.95)
#'
#' @export
check.omega <- function(half1, half2, B = 1000, conf = 0.95) {
  # Función interna para calcular Omega usando lavaan
  calculate_omega_lavaan <- function(data) {
    # Definir modelo unifactorial dinámico
    items <- colnames(data)
    model <- paste0("Factor =~ ", paste(items, collapse = " + "))

    # Ajustar el modelo con lavaan
    fit <- tryCatch(
      lavaan::cfa(model, data = data, estimator = "ulsmv", ordered = FALSE, std.lv = TRUE),
      error = function(e) return(NULL)
    )

    if (is.null(fit)) return(NA)

    # Extraer cargas factoriales
    lambda <- lavaan::inspect(fit, "std")$lambda
    communalities <- rowSums(lambda^2)
    unique_vars <- 1 - communalities

    # Calcular Omega
    omega <- sum(lambda)^2 / (sum(lambda)^2 + sum(unique_vars))
    return(omega)
  }

  ## 1️⃣ Calcular Omega para half1
  omega1 <- calculate_omega_lavaan(half1)

  # Bootstrap para IC de Omega (Half1)
  boot_omega1 <- boot::boot(data = half1, statistic = function(d, i) {
    calculate_omega_lavaan(d[i, ])
  }, R = B)

  ci1 <- tryCatch({
    boot::boot.ci(boot_omega1, conf = conf, type = "perc")$percent[4:5]
  }, error = function(e) c(NA, NA))


  ## 2️⃣ Calcular Omega para half2
  omega2 <- calculate_omega_lavaan(half2)

  # Bootstrap para IC de Omega (Half2)
  boot_omega2 <- boot::boot(data = half2, statistic = function(d, i) {
    calculate_omega_lavaan(d[i, ])
  }, R = B)

  ci2 <- tryCatch({
    boot::boot.ci(boot_omega2, conf = conf, type = "perc")$percent[4:5]
  }, error = function(e) c(NA, NA))


  ## 📦 Devolver resultados
  result <- data.frame(
    Half = c("Half 1", "Half 2"),
    Coefficient = "Omega total",
    Estimate = round(c(omega1, omega2), 4),
    CI_lower = c(ci1[1], ci2[1]),
    CI_upper = c(ci1[2], ci2[2])
  )

  return(result)
}
