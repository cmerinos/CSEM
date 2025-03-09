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