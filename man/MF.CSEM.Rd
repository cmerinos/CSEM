% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MF.CSEM.R
\name{MF.CSEM}
\alias{MF.CSEM}
\title{Calculate MF-CSEM (Mollenkopf-Feldt Conditional Standard Error of Measurement)}
\usage{
MF.CSEM(
  half1,
  half2,
  data,
  reliability.coef,
  n.items,
  min.score.item,
  max.score.item,
  conf.level = 95
)
}
\arguments{
\item{half1}{A vector specifying the names or indices of the columns corresponding to the first half of the items.}

\item{half2}{A vector specifying the names or indices of the columns corresponding to the second half of the items.}

\item{data}{A data frame containing the item responses for all examinees.}

\item{reliability.coef}{A numeric value indicating the reliability coefficient to be used in the calculation
(e.g., Cronbach's α, Omega, Angoff, or Spearman-Brown).}

\item{n.items}{Integer. The total number of items in the test.}

\item{min.score.item}{Integer. The minimum possible score for a single item.}

\item{max.score.item}{Integer. The maximum possible score for a single item.}

\item{conf.level}{Numeric. The confidence level for the confidence intervals (default = 95).}
}
\value{
A list containing three data frames:
\itemize{
  \item \strong{MF.CSEM.parameters}: Summary statistics of the MF-CSEM.
  \item \strong{MF.CSEM.score}: MF-CSEM calculated with observed scores.
  \item \strong{MF.CSEM.ETS}: MF-CSEM calculated with estimated true scores (ETS).
}
}
\description{
This function calculates the Conditional Standard Error of Measurement (CSEM)
using the Mollenkopf-Feldt (MF) procedure. The CSEM is computed for each possible score
level, based on the selected halves of items.
}
\details{
The MF-CSEM is calculated using a multi-step procedure:
1. Compute squared adjusted differences.
2. Fit a polynomial regression model to predict the squared differences from the total scores.
3. Calculate the MF-CSEM for each possible score.
4. Adjust the CSEM values based on the chosen reliability coefficient.
5. Compute confidence intervals for the CSEM.
}
\examples{
# Example with a simulated dataset
set.seed(123)
data <- data.frame(matrix(sample(3:5, 100 * 10, replace = TRUE), ncol = 10))
MF.CSEM(half1 = 1:5, half2 = 6:10, data = data, reliability.coef = 0.85,
        n.items = 10, min.score.item = 1, max.score.item = 5)

}
