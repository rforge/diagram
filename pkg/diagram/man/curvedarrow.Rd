\name{curvedarrow}
\alias{curvedarrow}
\title{
  adds curved arrow between two points
}
\description{
  Connects two points with an ellipsoid line
  and adds an arrowhead at a certain distance
}
\usage{
curvedarrow(from, to, lwd = 2, lty = 1, lcol = "black", 
            arr.col = lcol, arr.pos = 0.5, curve = 1, dr = 0.01, 
            endhead = FALSE, segment = c(0, 1), ...)
}
\arguments{
  \item{from }{coordinates (x,y) of the point *from* which to draw arrow.
  }
  \item{to }{coordinates (x,y) of the point *to* which to draw arrow.
  }
  \item{lwd }{line width.
  }
  \item{lty }{line type.
  }
  \item{lcol }{line color.
  }
  \item{arr.col }{arrowhead color.
  }
  \item{arr.pos }{relative position of arrowhead.
  }
  \item{curve }{relative size of curve (fraction of points distance) -
    see details.
  }
  \item{dr }{size of segments, in radians, to draw ellipse (decrease for
    smoother).
  }
  \item{endhead }{if \code{TRUE}: the arrow line stops at the arrowhead;
    default = \code{FALSE}.
  }
  \item{segment }{if not \code{c(0,1)}: then the arrow line will cover only 
    part of the requested path, 
    e.g. if segment = c(0.2,0.8), it will start 0.2 from \code{from} and
    till 0.8.
  }
  \item{... }{arguments passed to function \code{\link[shape]{Arrows}}.
  }
}

\value{
  \item{default }{coordinates (x,y) where arrowhead is drawn.
  }
}

\author{Karline Soetaert <karline.soetaert@nioz.nl>}

\examples{
openplotmat(main = "curvedarrow")

pos <- coordinates(pos = 4, my = 0.2)
text(pos, LETTERS[1:4], cex = 2)

for (i in 1:3) 
  curvedarrow(from = pos[1, ] + c(0,-0.05), to = pos[i+1, ] + c(0,-0.05),
              curve = 0.5, arr.pos = 1)
for (i in 1:3) 
  curvedarrow(from = pos[1, ] + c(0, 0.05), to = pos[i+1, ] + c(0, 0.05),
              curve = -0.25, arr.adj = 1, arr.pos = 0.5, 
              arr.type = "triangle", arr.col = "blue")
}

\details{
  A curved arrow is drawn between two points '(from, to)'
  
  The position of the arrowhead, is set with \code{arr.pos}, a value between
  0(start point) and 1(endpoint)
  
  The line curvature is set with \code{curve} which expresses the ellipse
  radius as a fraction of the distance between the two points.
  For instance, \code{curve=0.5} will draw an ellepse with small radius
  half of a circle.
  
  The type of the arrowhead is set with \code{arr.type} which can take
  the values:
  \itemize{
    \item "none"    : skips the drawing of arrows.
    \item "simple"  : uses comparable R function arrows.
    \item "triangle": uses filled triangle.
    \item "curved"  : draws arrowhead with curved edges.
    \item "circle"  : draws circular head.
    \item "ellipse" : draws ellepsoid head.
    \item "T"       : draws T-shaped (blunt) head.
  }
  The size of the arrow head can be specified with the arguments 
  \code{arr.length} and \code{arr.width}.
  
  See \code{\link[shape]{Arrowhead}} from package shape for details on
  arrow head
}

\seealso{
  \code{\link{straightarrow}}, \code{\link{segmentarrow}},
  \code{\link{bentarrow}},
  \code{\link{selfarrow}}, \code{\link{treearrow}}, \code{\link{splitarrow}},

  \code{\link{arrows}}: the comparable R function,

  \code{\link[shape]{Arrows}}: more complicated arrow function from package
  shape.
}

\keyword{aplot}

