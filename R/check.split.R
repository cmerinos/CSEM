#' Dividir ítems en mitades
#'
#' @description Esta función divide los ítems en dos mitades usando distintos métodos.
#'
#' @param data Dataframe con los ítems
#' @param method Método de división ("ritc", "diff", "optimized")
#' @param half.eq Indica si las mitades deben tener igual número de ítems
#' @param seed Semilla para la reproducibilidad
#'
#' @return Una lista con las mitades y estadísticas relacionadas
#' @export
check.split <- function(data, method = c("ritc", "optimized", "diff"),
                        n_iter = 1000, seed = 123, half.eq = c("equal", "unequal")) {
  set.seed(seed)

  method <- match.arg(method)
  half.eq <- match.arg(half.eq)

  ## 1️⃣ Validar estructura
  if (!is.data.frame(data)) stop("The argument 'data' must be a dataframe.")
  n_items <- ncol(data)
  if (n_items < 2) stop("At least 2 items are required to split.")

  ## 2️⃣ Método basado en ritc
  if (method == "ritc") {
    total_score <- rowSums(data, na.rm = TRUE)
    cor_ritc <- sapply(data, function(item) cor(item, total_score, use = "complete.obs"))
    ritc_order <- order(cor_ritc, decreasing = TRUE)
    half1 <- names(data)[ritc_order[c(TRUE, FALSE)]]
    half2 <- names(data)[ritc_order[c(FALSE, TRUE)]]
  }

  ## 3️⃣ Método optimized
  if (method == "optimized") {
    results <- list()
    cor_values <- c()
    for (i in 1:n_iter) {
      idx <- sample(rep(c(1, 2), length.out = n_items))
      half1 <- names(data)[idx == 1]
      half2 <- names(data)[idx == 2]
      total1 <- rowSums(data[, half1], na.rm = TRUE)
      total2 <- rowSums(data[, half2], na.rm = TRUE)
      current_cor <- cor(total1, total2, use = "complete.obs")
      if (!is.na(current_cor)) {
        results[[i]] <- list(half1 = half1, half2 = half2, cor = current_cor)
        cor_values <- c(cor_values, current_cor)
      }
    }
    top5_indices <- order(cor_values, decreasing = TRUE)[1:5]
    top5_results <- results[top5_indices]
    top5_summary <- data.frame(
      Iteration = top5_indices,
      Correlation = round(cor_values[top5_indices], 4),
      Half1 = sapply(top5_results, function(x) paste(x$half1, collapse = ", ")),
      Half2 = sapply(top5_results, function(x) paste(x$half2, collapse = ", "))
    )
    best_index <- top5_indices[1]
    best_split <- results[[best_index]]
    half1 <- best_split$half1
    half2 <- best_split$half2
  }

  ## 4️⃣ Método  diff
  if (method == "diff") {
    item_means <- sapply(data, mean, na.rm = TRUE)
    diff_order <- order(item_means)
    half1 <- names(data)[diff_order[c(TRUE, FALSE)]]
    half2 <- names(data)[diff_order[c(FALSE, TRUE)]]
  }

  ## 5️⃣ Ajustar tamaños si el usuario elige "equal"
  if (half.eq == "equal") {
    len1 <- length(half1)
    len2 <- length(half2)

    if (len1 != len2) {
      # Identificar la mitad más larga y eliminar el último ítem
      if (len1 > len2) {
        half1 <- half1[1:len2]
      } else {
        half2 <- half2[1:len1]
      }
      message("⚠️ Adjustment made: An item was removed to equalize the halves.")
    }
  } else {
    # ⚠️ Mostrar mensaje si las mitades son desiguales
    len1 <- length(half1)
    len2 <- length(half2)
    if (len1 != len2) {
      message("⚠️ Warning: Unequal number of items in each half. MF-CSEM requires equal halves.")
    }
  }

  ## 6️⃣ Devolver resultados
  return(list(half1 = half1, half2 = half2))
}
