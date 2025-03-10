#' Calculate Standardized Conditional SEM (Raju et al.)
#'
#' @description
#' This function calculates the examinee-level reliability based on the formula proposed by Raju et al. (2007).
#'
#' @param csem.parameter Numeric vector with CSEM values (e.g., MF.CSEM.parameters$CSEM).
#' @param var Numeric value indicating the variance of the observed scores.
#'
#' @details
#' The formula is defined as:
#' \deqn{\rho_{x_sx_s} = \frac{\sigma_x^2 - SEM_s^2}{\sigma_x^2}}
#' where:
#' - \eqn{\sigma_x^2}: variance of the observed scores.
#' - \eqn{SEM_s^2}: squared Conditional SEM for each observed score.
#'
#' @return A numeric vector of examinee-level reliability estimates.
#'
#' @examples
#' # Example:
#' csem_vals <- c(2.0, 2.5, 3.0)
#' var_obs <- 25
#' std.CSEM(csem.parameter = csem_vals, var = var_obs)
#'
#' @export
std.CSEM <- function(csem.parameter, var) {
  
  # 1️⃣ Validar parámetros
  if (!is.numeric(csem.parameter) || !is.numeric(var)) {
    stop("⚠️ Both csem.parameter and var must be numeric.")
  }
  
  if (var <= 0) {
    stop("⚠️ Variance (var) must be positive.")
  }
  
  # 2️⃣ Calcular la fiabilidad nivel-examinado
  stdcsem <- 1 - (csem.parameter^2) / var
  
  # 3️⃣ Asegurar que estén en el rango [0, 1]
  stdcsem <- pmin(pmax(stdcsem, 0), 1)
  
  # 4️⃣ Mensaje si hubo correcciones
  if (any(stdcsem == 0 | stdcsem == 1)) {
    message("⚠️ Some reliability values were adjusted to 0 or 1 due to boundary constraints.")
  }
  
  # 📦 Devolver resultado
  return(round(data.frame(stdcsem), 3))
}
