\name{diagram-package}
\alias{diagram-package}
\alias{diagram}
\docType{package}
\title{
  Functions for visualising simple graphs (networks), plotting flow diagrams
}
\description{
  Visualises simple graphs (networks) based on a transition matrix, utilities
  to plot flow diagrams, visualising webs,...
     
  Support for the book "A practical guide to ecological modelling -
  using R as a simulation platform"
  by Karline Soetaert and Peter M.J. Herman (2009). Springer.
}

\details{
  \tabular{ll}{
    Package: \tab diagram\cr
    Type: \tab Package\cr
    Version: \tab 1.5\cr
    Date: \tab 2009-10-10\cr
    License: \tab GNU Public License 2 or above\cr
  }
  This package is used in R-package ecolMod, which includes many more examples.
}

\author{
  Karline Soetaert (Maintainer)
}
\seealso{

  \code{\link{plotmat}}, \code{\link{plotweb}}, \code{\link{coordinates}},
  \code{\link{openplotmat}},  \code{\link{bentarrow}},
  \code{\link{curvedarrow}}, \code{\link{segmentarrow}},
  \code{\link{selfarrow}}, \code{\link{splitarrow}},
  \code{\link{straightarrow}}, \code{\link{treearrow}},
  \code{\link{shadowbox}}, \code{\link{textdiamond}},
  \code{\link{textellipse}}, \code{\link{textempty}}, \code{\link{texthexa}},
  \code{\link{textdiamond}}, \code{\link{textplain}}, \code{\link{textrect}},
  \code{\link{textround}}.
}

\examples{
\dontrun{
## show examples (see respective help pages for details)
example(plotmat)
example(plotweb)

## run demos
demo("flowchart") # creating flow charts
demo("plotmat")   # plotting diagrams inputted as a matrix
demo("plotweb")   # plotting webs inputted as a matrix

## open the directory with source code of demos
browseURL(paste(system.file(package="diagram"), "/demo", sep=""))

## show package vignette
vignette("diagram")
edit(vignette("diagram"))
browseURL(paste(system.file(package="diagram"), "/doc", sep=""))
}
}

\keyword{ package }
