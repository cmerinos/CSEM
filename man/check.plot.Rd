% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.plot.R
\name{check.plot}
\alias{check.plot}
\title{Plot MF-CSEM results}
\usage{
check.plot(
  MF.CSEM.output,
  score.type = c("observed", "ETS"),
  plot.type = c("CSEM", "CI", "band"),
  color.line = "black",
  color.points = "darkred",
  color.band = "lightblue",
  line.type = "solid"
)
}
\arguments{
\item{MF.CSEM.output}{Object returned by the MF.CSEM function.}

\item{score.type}{Character: "observed" or "ETS". Determines the x-axis variable.}

\item{plot.type}{Character: "CSEM" or "CI". Determines whether to show CSEM only or with confidence intervals.}

\item{title}{Optional: Title of the plot.}

\item{xlab}{Optional: X-axis label.}

\item{ylab}{Optional: Y-axis label.}

\item{save.path}{Optional: Path to save the plot as PNG (default = NULL, plot not saved).}
}
\value{
A ggplot2 object with the desired plot.
}
\description{
Plot MF-CSEM results
}
