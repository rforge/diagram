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

  and for the book "Solving Differential Equations in R"
  by Karline Soetaert, Jeff R. Cash and Francesca Mazzia (in press). Springer.
}

\details{
  \tabular{ll}{
    Package: \tab diagram\cr
    Type: \tab Package\cr
    Version: \tab 1.6\cr
    Date: \tab 2011-06-01\cr
    License: \tab GNU Public License 2 or above\cr
  }
  This package is used in R-package ecolMod, which includes many more examples.
}

\author{
  Karline Soetaert (Maintainer)
}
\seealso{

  \code{\link{plotmat}}, \code{\link{plotweb}}, \code{\link{coordinates}},
  \code{\link{openplotmat}},  
  
  arrows:
  
  \code{\link{bentarrow}},
  \code{\link{curvedarrow}}, \code{\link{segmentarrow}},
  \code{\link{selfarrow}}, \code{\link{splitarrow}},
  \code{\link{straightarrow}}, \code{\link{treearrow}},
 
 boxes and text:
 
  \code{\link{shadowbox}}, \code{\link{textdiamond}},
  \code{\link{textellipse}}, \code{\link{textempty}}, \code{\link{texthexa}},
  \code{\link{textdiamond}}, \code{\link{textplain}}, \code{\link{textrect}},
  \code{\link{textround}}.

 electrical networks:
 
 \code{\link{en.Resistor}},\code{\link{en.Capacitator}},\code{\link{en.Node}},
 \code{\link{en.Amplifier}},\code{\link{en.Signal}} \code{\link{en.Ground}}.

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
