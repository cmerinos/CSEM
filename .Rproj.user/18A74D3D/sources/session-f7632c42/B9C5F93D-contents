#' Compute Spearman-Brown Reliability coefficient
#'
#' @description
#' This function calculates the Spearman-Brown reliability coefficient for test halves.
#' It also estimates confidence intervals using bootstrap resampling.
#'
#' @param half1 A numeric matrix or data frame where rows represent examinees
#' and columns represent item scores for the first test half.
#' @param half2 A numeric matrix or data frame where rows represent examinees
#' and columns represent item scores for the other test half.
#' @param B Integer. Number of bootstrap resamples for confidence interval estimation (default = 1000).
#' @param conf Numeric. Confidence level for the interval estimation (default = 0.95).
#'
#' @details
#' The Spearman-Brown coefficient is calculated as:
#'
#' \deqn{r_{SB} = \frac{2r}{1 + r}}
#'
#' where:
#' - \eqn{r} is the correlation between the two test halves.
#'
#' Bootstrap resampling is used to estimate confidence intervals for \eqn{r_{SB}}.
#' If the bootstrap fails, `NA` values are returned for the confidence interval.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{Coefficient}: The name of the reliability coefficient ("Spearman-Brown").
#'   \item \code{Estimate}: The estimated Spearman-Brown reliability coefficient.
#'   \item \code{CI_lower}: The lower bound of the confidence interval.
#'   \item \code{CI_upper}: The upper bound of the confidence interval.
#' }
#'
#' @examples
#' # Simulated test halves
#' set.seed(123)
#' half1 <- matrix(sample(1:5, 30, replace = TRUE), nrow = 10)
#' half2 <- matrix(sample(1:5, 30, replace = TRUE), nrow = 10)
#'
#' # Compute Spearman-Brown reliability
#' check.spearmanbrown(half1, half2, B = 1000, conf = 0.95)
#'
#' @export
check.spearmanbrown <- function(half1, half2, B = 1000, conf = 0.95) {

  # 1️⃣ Validar que half1 y half2 sean matrices o data frames
  if (!is.matrix(half1) && !is.data.frame(half1)) {
    stop("⚠️ 'half1' must be a matrix or a data frame.")
  }

  if (!is.matrix(half2) && !is.data.frame(half2)) {
    stop("⚠️ 'half2' must be a matrix or a data frame.")
  }

  # 2️⃣ Calcular puntuaciones totales
  total1 <- rowSums(half1, na.rm = TRUE)
  total2 <- rowSums(half2, na.rm = TRUE)

  # 3️⃣ Calcular el coeficiente de Spearman-Brown
  r_halves <- cor(total1, total2, use = "complete.obs")
  sb_reliability <- (2 * r_halves) / (1 + r_halves)

  # 4️⃣ Bootstrap para IC
  boot_sb <- boot::boot(data = cbind(total1, total2),
                        statistic = function(data, i) {
                          t1 <- data[i, 1]
                          t2 <- data[i, 2]
                          r <- cor(t1, t2, use = "complete.obs")
                          (2 * r) / (1 + r)
                        },
                        R = B)

  ci <- tryCatch({
    boot::boot.ci(boot_sb, conf = conf, type = "perc")$percent[4:5]
  }, error = function(e) c(NA, NA))

  # 5️⃣ Devolver resultados
  result <- data.frame(
    Coefficient = "Spearman-Brown",
    Estimate = round(sb_reliability, 4),
    CI_lower = ci[1],
    CI_upper = ci[2]
  )

  return(result)
}

