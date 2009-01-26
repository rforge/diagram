\name{textdiamond}
\alias{textdiamond}
\title{adds lines of text in a diamand-shaped box to a plot}
\description{adds one or more lines of text, in a diamond-shaped box}
\usage{textdiamond(mid, radx, rady=NULL, lwd=1, shadow.size=0.01,
  adj=c(0.5,0.5), lab="", box.col="white", lcol="black", 
  shadow.col="grey", angle=0, ...)}
\arguments{
  \item{mid }{midpoint (x,y) of the box}
  \item{radx }{horizontal radius of the box}
  \item{rady }{vertical radius of the box}
  \item{lwd }{line width of line surrounding the box}
  \item{shadow.size }{relative size of shadow}
  \item{adj }{text adjustment}
  \item{lab }{one label or a vector string of labels to be added in box}
  \item{box.col }{fill color of the box}
  \item{lcol }{line color surrounding box}
  \item{shadow.col }{color of shadow}
  \item{angle }{rotation angle, degrees}
  \item{... }{other arguments passed to function textplain}
}
\author{Karline Soetaert <k.soetaert@nioo.knaw.nl>}
\examples{
  openplotmat(xlim=c(-0.1,1.1),main="textdiamond")
  for (i in 1:10) textdiamond(mid=runif(2),col=i,radx=0.1,rady=0.05,
                              lab=LETTERS[i],cex=2,angle=runif(1)*360)}
\details{see \code{\link{shadowbox}} for specifications of the diamond-shaped box and its shadow}
\seealso{\code{\link{textellipse}}, \code{\link{textempty}},\code{\link{texthexa}}, 
    \code{\link{textmulti}}, \code{\link{textplain}}, \code{\link{textrect}}, \code{\link{textround}}}
\keyword{aplot}

