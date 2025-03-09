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
