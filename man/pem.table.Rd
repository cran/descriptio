\name{pem.table}

\alias{pem.table}

\title{Computes the local and global Percentages of Maximum Deviation from Independence (pem)}

\description{Computes the local and global Percentages of Maximum Deviation from Independence (pem) of a contingency table.}

\usage{
pem.table(x, y, weights = NULL, sort = FALSE, na.rm = FALSE, na.value = "NAs", digits = 1)
}

\arguments{
  \item{x}{the first categorical variable}
  \item{y}{the second categorical variable}
  \item{weights}{an optional numeric vector of weights (by default, a vector of 1 for uniform weights)}
  \item{sort}{logical. Whether rows and columns are sorted according to a correspondence analysis or not (default is FALSE).}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. If FALSE (default), an additional level is added to the variables (see na.value argument).}
  \item{na.value}{character. Name of the level for NA category. Default is "NAs". Only used if na.rm = FALSE.}
  \item{digits}{integer. The number of digits (default is 1). If NULL, the results are not rounded.}
}

\details{
The Percentage of Maximum Deviation from Independence (pem) is an association measure for contingency tables and also provides attraction (resp. repulsion) measures in each cell of the crosstabulation (see Cibois, 1993). 
It is an alternative to khi2, Cramer's V coefficient, etc.
}

\value{
Returns a list:
  \item{peml }{Table with local percentages of maximum deviation from independence}
  \item{pemg }{Numeric value, i.e. the global percentage of maximum deviation from independence}
}

\references{
Cibois P., 1993, Le pem, pourcentage de l'ecart maximum : un indice de liaison entre modalites d'un tableau de contingence, 
\emph{ Bulletin de methodologie sociologique}, n40, p.43-63. \url{https://ciboispagesperso.fr/bms93.pdf}
}

\author{Nicolas Robette}

\seealso{\code{\link{table}}, \code{\link{chisq.test}}, \code{\link{phi.table}}, \code{\link[vcd]{assocstats}}}

\examples{
data(Movies)
pem.table(Movies$Country, Movies$ArtHouse)
}

\keyword{ multivariate }
\keyword{ misc }
