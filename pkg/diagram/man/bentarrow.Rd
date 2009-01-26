\name{bentarrow}
\alias{bentarrow}
\title{adds 2-segmented arrow between two points}
\description{Connects two points with 2 segments (default = horizontal-vertical)
      \cr adds an arrowhead on one of the segments and at a certain distance}
\usage{bentarrow(from, to, lwd=2, lty=1, lcol="black",
  arr.side=2, arr.pos=0.5,  path="H", ...)}
\arguments{
  \item{from }{coordinates (x,y) of the point *from* which to draw arrow}
  \item{to }{coordinates (x,y) of the point *to* which to draw arrow}
  \item{lwd }{line width}
  \item{lty }{line type}
  \item{lcol }{line color}
  \item{arr.side }{segment number on which arrowhead is drawn (1,2)}
  \item{arr.pos }{relative position of arrowhead on segment on which arrowhead is drawn}
  \item{path }{first segment to be drawn (V=Vertical, H=Horizontal)}
  \item{... }{other arguments passed to function straightarrow}
}
\value{
  coordinates (x,y) where arrowhead is drawn
}
\author{Karline Soetaert <k.soetaert@nioo.knaw.nl>}
\examples{
  openplotmat(main="bentarrow")
  pos <-cbind(A<-seq(0.1,0.9,by=0.2),rev(A))
  text(pos,LETTERS[1:5],cex=2)
  for (i in 1:4) bentarrow(from=pos[i,]+c(0.05,0),to=pos[i+1,]+c(0,0.05),
                           arr.pos=1,arr.adj=1)
  for (i in 1:2) bentarrow(from=pos[i,]+c(0.05,0),to=pos[i+1,]+c(0,0.05),
                           arr.pos=0.5,path="V",lcol="lightblue")
   bentarrow(from=pos[3,]+c(0.05,0),to=pos[4,]+c(0,0.05),arr.pos=0.7,
             arr.side=1,path="V",lcol="darkblue")
   bentarrow(from=pos[4,]+c(0.05,0),to=pos[5,]+c(0,0.05),arr.pos=0.7,
             arr.side=1:2,path="V",lcol="blue")}
\details{a two-segmented arrow is drawn between two points '(from, to)'
    
    how the segments are drawn is set with "path" which can take on the values:
  \itemize{
    \item H:   (horizontal): first left or right, then vertical
    \item V:   (vertical)  : first down- or upward, then horizontal
}
    The segment(s) on which the arrow head is drawn is set with arr.side, which is one or more values in (1, 2)
    \cr The position of the arrowhead on the segment on which it is drawn, 
    is set with arr.pos, a value between 0(start of segment) and 1(end of segment)
  
    The type of the arrowhead is set with "arr.type" which can take the values:
  \itemize{
    \item "simple"  : uses comparable R function arrows  
    \item "triangle": uses filled triangle
    \item "curved"  : draws arrowhead with curved edges
    \item "circle" : draws circular head 
}
    see \code{\link[shape]{Arrowhead}} from package shape for details on arrow head}
\seealso{\code{\link{straightarrow}}, \code{\link{segmentarrow}}, \code{\link{curvedarrow}}, 
       \code{\link{selfarrow}}, \code{\link{treearrow}}, \code{\link{splitarrow}},    
        \cr \code{\link[graphics]{arrows}}: the comparable R function,
        \cr \code{\link[shape]{Arrows}}: more complicated arrow function from package shape}
\keyword{aplot}

