\name{splitarrow}
\alias{splitarrow}
\title{adds a branched arrow between several points}
\description{
  connects two sets of points with a star-like structure,
  adds an arrowhead at a certain distance
}
\usage{
splitarrow(from, to, lwd = 2, lty = 1, lcol = "black", arr.col = lcol, 
           arr.side = 2, arr.pos = 0.5, centre = NULL, dd = 0.5, ...)
}
\arguments{
  \item{from }{matrix of coordinates (x,y) of points *from* which to draw arrow.
  }
  \item{to }{matrix of coordinates (x,y) of points *to* which to draw arrow.
  }
  \item{lwd }{line width.
  }
  \item{lty }{line type.
  }
  \item{lcol }{line color.
  }
  \item{arr.col }{arrow color.
  }
  \item{arr.side }{segment number on which arrowhead is drawn (1,2).
  }
  \item{arr.pos }{relative position of arrowhead on segment on which
    arrowhead is drawn.
  }
  \item{centre }{coordinates (x,y) of central point.
  }
  \item{dd }{relative position of central point, only when \code{centre=NULL}.
  }
  \item{... }{other arguments passed to function \link{straightarrow}.
  }
}
\value{
  coordinates (x,y) where arrowheads are drawn
}
\author{
  Karline Soetaert <karline.soetaert@nioz.nl>

}

\examples{
openplotmat(main = "splitarrow")

pos <- coordinates(c(1, 2, 2, 4, 1))
splitarrow(from = pos[1, ], to = pos[2:10, ], 
           arr.side = 1, centre = c(0.5, 0.625))
for (i in 1:10) 
  textrect(pos[i, ], lab = i, cex = 2, radx = 0.05)
  
  
openplotmat(main = "splitarrow")

pos <- coordinates(c(1, 3))
splitarrow(from = pos[1,], to = pos[2:4, ], arr.side = 1)
splitarrow(from = pos[1,], to = pos[2:4, ], arr.side = 2)
for (i in 1:4) 
  textrect(pos[i, ], lab = i, cex = 2, radx = 0.05)
  

openplotmat(main = "splitarrow")
pos <- coordinates(N = 6)
pos <- rbind(c(0.5, 0.5), pos)
splitarrow(from = pos[1, ], to = pos[2:7, ], arr.side = 2)
for (i in 1:7)
  textrect(pos[i, ], lab = i, cex = 2, radx = 0.05)
}

\details{
  a branched arrow is drawn between points '(\code{from}, \code{to})',
  where both \code{from} and \code{to} can be several points.

  The arrow segments radiate into a central point. Either the (x,y)
  coordinates of this central point are set with \code{centre} or
  it is estimated at a certain distance (\code{dd} >0,<1) between the
  centroid of the *\code{from}* points and the centroid of the *\code{to}*
  points.
    
  The segment(s) on which the arrow head is drawn is set with \code{arr.side},
  which is one or more values in (1, 2)

  \itemize{
    \item \code{arr.side=1} sets the arrow head on the segment *from* ->
      central point
    \item \code{arr.side=2} sets the arrow head on the segment
      central point -> *to*
  }
    
  The position of the arrowhead on the segment on which it is drawn,
  is set with \code{arr.pos}, a value between 0(start of segment) and
  1(end of segment)
   
  The type of the arrowhead is set with \code{arr.type} which can take the
  values:
  \itemize{
    \item "simple"  : uses comparable R function \code{arrows}.
    \item "triangle": uses filled triangle.
    \item "curved"  : draws arrowhead with curved edges.
    \item "circle"  : draws circular head.
    \item "ellipse" : draws ellepsoid head
    \item "T"       : draws T-shaped (blunt) head
  }
  see \code{\link[shape]{Arrowhead}} from package shape for details on
  arrow head.
}

\seealso{
  \code{\link{straightarrow}}, \code{\link{segmentarrow}},
  \code{\link{curvedarrow}}, \code{\link{selfarrow}}, \code{\link{bentarrow}},
  \code{\link{treearrow}},

  \code{\link{arrows}}: the comparable R function,

  \code{\link[shape]{Arrows}}: more complicated arrow function from package
  shape.
}

\keyword{aplot}

