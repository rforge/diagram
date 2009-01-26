\name{plotcircle}
\alias{plotcircle}
\title{adds part of a colored circle to a plot}
\description{adds (part of) a colored circle to a plot; 
    an arrow can be drawn at a specified position}
\usage{plotcircle(r=1, ...)}
\arguments{
  \item{r }{radius of circle}
  \item{... }{arguments passed to function "plotellipse"}
}
\author{Karline Soetaert <k.soetaert@nioo.knaw.nl>}
\examples{emptyplot(c(0,1),c(0,2),main="plotcircle")	
  plotcircle(mid=c(0.5,0.5),r=0.25,from=0,to=3*pi/2,
             arrow=TRUE,arr.pos=0.5,col="red")}
\details{see \code{\link{plotellipse}} for details}
\seealso{\code{\link{plotellipse}} to draw ellipses}
\keyword{aplot}

