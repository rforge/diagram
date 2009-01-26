\name{splitarrow}
\alias{splitarrow}
\title{adds a branched arrow between several points}
\description{connects two sets of points with a star-like structure, 
   \cr adds an arrowhead at a certain distance}
\usage{splitarrow(from, to, lwd=2, lty=1, lcol="black", 
  arr.side=2, arr.pos=0.5, centre=NULL, dd=0.5, ...)}
\arguments{
  \item{from }{matrix of coordinates (x,y) of points *from* which to draw arrow}
  \item{to }{matrix of coordinates (x,y) of points *to* which to draw arrow}
  \item{lwd }{line width}
  \item{lty }{line type}
  \item{lcol }{line color}
  \item{arr.side }{segment number on which arrowhead is drawn (1,2)}
  \item{arr.pos }{relative position of arrowhead on segment on which arrowhead is drawn}
  \item{centre }{coordinates (x,y) of central point}
  \item{dd }{relative position of central point, only when centre=NULL}
  \item{... }{other arguments passed to function straightarrow}
}
\value{
  coordinates (x,y) where arrowheads are drawn
}
\author{Karline Soetaert <k.soetaert@nioo.knaw.nl>}
\examples{
  openplotmat(main="splitarrow")
  pos <- coordinates(c(1,2,2,4,1))
  splitarrow(from=pos[1,],to=pos[2:10,],arr.side=1,centre=c(0.5,0.625))
  for (i in 1:10) textrect(pos[i,],lab=i,cex=2,radx=0.05)
  
  
  openplotmat(main="splitarrow")
  pos <- coordinates(c(1,3))
  splitarrow(from=pos[1,],to=pos[2:4,],arr.side=1)
  splitarrow(from=pos[1,],to=pos[2:4,],arr.side=2)
  for (i in 1:4) textrect(pos[i,],lab=i,cex=2,radx=0.05)
  
  openplotmat(main="splitarrow")
  pos <- coordinates(N=6)
  pos <- rbind(c(0.5,0.5),pos)
  splitarrow(from=pos[1,],to=pos[2:7,],arr.side=2)
  for (i in 1:7) textrect(pos[i,],lab=i,cex=2,radx=0.05)}
\details{a branched arrow is drawn between points '(from, to)', 
    where both from and to can be several points.  
    \cr The arrow segments radiate into a central point. Either the (x,y) coordinates of this central point are set with "centre" or
    it is estimated at a certain distance (dd >0,<1) between the centroid of the *from* points and the centroid of the *to* points
    
    The segment(s) on which the arrow head is drawn is set with arr.side, which is one or more values in (1, 2)
    \cr arr.side=1 sets the arrow head on the segment *from* -> central point
    \cr arr.side=2 sets the arrow head on the segment  central point -> *to* 
    
    
    The position of the arrowhead on the segment on which it is drawn, 
    is set with arr.pos, a value between 0(start of segment) and 1(end of segment)
   
    The type of the arrowhead is set with "arr.type" which can take the values:
  \itemize{
    \item "simple"  : uses comparable R function arrows  
    \item "triangle": uses filled triangle
    \item "curved"  : draws arrowhead with curved edges
    \item "circle"  : draws circular head 
}
  	see \code{\link[shape]{Arrowhead}} from package shape for details on arrow head}
\seealso{\code{\link{straightarrow}}, \code{\link{segmentarrow}}, \code{\link{curvedarrow}}, 
       \code{\link{selfarrow}}, \code{\link{bentarrow}}, \code{\link{treearrow}},    
       \cr \code{\link{arrows}}: the comparable R function,
       \cr \code{\link[shape]{Arrows}}: more complicated arrow function from package shape}
\keyword{aplot}

