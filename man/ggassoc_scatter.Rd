\name{ggassoc_scatter}

\alias{ggassoc_scatter}

\title{Scatter plot with a smoothing line}

\description{
Displays of scatter plot and adds a smoothing line, using ggplot2.
}

\usage{
ggassoc_scatter(data, mapping, na.rm = FALSE,
axes.labs = TRUE, ticks.labs = TRUE, text.size = 3)
}

\arguments{
  \item{data}{dataset to use for plot}
  \item{mapping}{aesthetic being used. It must specify x and y.}
  \item{na.rm}{logical, indicating whether NA values should be silently removed before the computation proceeds. Default is FALSE.}
  \item{axes.labs}{Whether to display the labels of the axes, i.e. the names of x and y. Default is TRUE.}
  \item{ticks.labs}{Whether to display the labels of the categories of x and y. Default is TRUE.}
  \item{text.size}{Size of the association measure. If NULL, the text is not added to the plot.}  
}

\details{
Kendall's tau rank correlation between x and y is displayed in upper-left corner of the plot.

Smoothing is performed with gam.

This function can be used as a high-level plot with \code{ggduo} and \code{ggpairs} functions of the \code{GGally} package. 
}

\value{
a ggplot object
}

\author{Nicolas Robette}

\seealso{
 \code{\link{assoc.twocont}}, \code{\link{condesc}}, \code{\link{assoc.yx}},
 \code{\link{darma}}, \code{\link[GGally]{ggpairs}}
}

\examples{
data(Movies)
ggassoc_scatter(Movies, mapping = ggplot2::aes(x = Budget, y = Critics))
}

\keyword{ multivariate }
\keyword{ aplot }
