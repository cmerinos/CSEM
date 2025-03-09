check.spearmanbrown <- function(half1, half2, B = 1000, conf = 0.95) {
  # Calcular las puntuaciones totales
  total1 <- rowSums(half1, na.rm = TRUE)
  total2 <- rowSums(half2, na.rm = TRUE)
  
  # Calcular el coeficiente de Spearman-Brown
  r_halves <- cor(total1, total2, use = "complete.obs")
  sb_reliability <- (2 * r_halves) / (1 + r_halves)
  
  # Bootstrap para IC
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
  
  ## 📦 Devolver resultados
  result <- data.frame(
    Coefficient = "Spearman-Brown",
    Estimate = round(sb_reliability, 4),
    CI_lower = ci[1],
    CI_upper = ci[2]
  )
  
  return(result)
}