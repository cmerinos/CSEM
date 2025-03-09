#' Plot MF-CSEM results
#'
#' @param MF.CSEM.output Object returned by the MF.CSEM function.
#' @param score.type Character: "observed" or "ETS". Determines the x-axis variable.
#' @param plot.type Character: "CSEM" or "CI". Determines whether to show CSEM only or with confidence intervals.
#' @param title Optional: Title of the plot.
#' @param xlab Optional: X-axis label.
#' @param ylab Optional: Y-axis label.
#' @param save.path Optional: Path to save the plot as PNG (default = NULL, plot not saved).
#'
#' @return A ggplot2 object with the desired plot.
#'
#' @import ggplot2
#'
#' @export

check.plot <- function(MF.CSEM.output,
                       score.type = c("observed", "ETS"),
                       plot.type = c("CSEM", "CI", "band"),
                       color.line = "black",
                       color.points = "darkred",
                       color.band = "lightblue",
                       line.type = "solid") {

  # 1️⃣ Validar parámetros
  score.type <- match.arg(score.type)
  plot.type <- match.arg(plot.type)

  # 2️⃣ Seleccionar el dataframe adecuado
  if (score.type == "observed") {
    df <- MF.CSEM.output$MF.CSEM.score
    x_var <- "Score"
    y_var <- ifelse(plot.type == "band", "CI_band", "Score")
  } else {
    if (plot.type == "CSEM") {
      df <- MF.CSEM.output$MF.CSEM.score  # CSEM solo está aquí
      x_var <- "Score"
      y_var <- "CSEM"
    } else {
      df <- MF.CSEM.output$MF.CSEM.ETS
      x_var <- "Score"
      y_var <- ifelse(plot.type == "band", "CI_band", "ETS")
    }
  }

  # 3️⃣ Generar el gráfico
  library(ggplot2)

  if (plot.type == "CSEM") {
    p <- ggplot(df, aes_string(x = x_var, y = "CSEM")) +
      geom_line(color = color.line, size = 1, linetype = line.type) +
      geom_point(color = color.points, size = 2) +
      labs(title = paste("MF-CSEM -", score.type),
           x = score.type, y = "MF-CSEM") +
      theme_minimal()

  } else if (plot.type == "CI") {
    p <- ggplot(df, aes_string(x = x_var, y = y_var)) +
      geom_errorbar(aes(ymin = lwr.ci, ymax = upp.ci), width = 0.3, color = color.band) +
      geom_point(color = color.points, size = 2) +
      geom_line(color = color.line, linetype = line.type) +
      labs(title = paste("Score with Confidence Intervals -", score.type),
           x = score.type, y = y_var) +
      theme_minimal()

  } else if (plot.type == "band") {
    p <- ggplot(df, aes_string(x = x_var, y = "CI_band")) +
      geom_point(color = color.points, size = 2) +
      geom_smooth(method = "loess", color = color.line, linetype = line.type, se = FALSE) +
      labs(title = paste("Width of Confidence Bands -", score.type),
           x = score.type, y = "CI_band (Width)") +
      theme_minimal()
  }

  # 4️⃣ Mostrar el gráfico
  print(p)
}
