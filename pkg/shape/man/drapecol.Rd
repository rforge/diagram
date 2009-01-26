\name{drapecol}
\alias{drapecol}
\title{draping colors over a persp plot}
\description{generates color(s) that will appear on the surface facets of "persp" plot
    \cr inspired by similar function in package fields, 
    unfortunately made unavailable in most recent version of fields}
\usage{drapecol(A, col=femmecol(100), NAcol="white")}
\arguments{
  \item{A }{matrix with input grid}
  \item{col }{color palette}
  \item{NAcol }{color of 'NA' elements}
}
\value{
  a vector of character strings giving the colors in hexadecimal format, one for each surface facet
}
\author{Karline Soetaert <k.soetaert@nioo.knaw.nl>}
\examples{persp(volcano,theta = 135, phi = 30, col = drapecol(volcano),main="drapecol")
  persp(volcano,theta = 135, phi = 30, col = drapecol(volcano),border=NA,
        main="drapecol")}
\seealso{\code{\link{persp}}}
\keyword{aplot}

