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
