\name{selfarrow}
\alias{selfarrow}
\title{
  adds a circular, self-pointing arrow to a plot
}
\description{
  adds a circular arrow, from and to the same point
}
\usage{
selfarrow(pos, lwd = 2, lty = 1, lcol = "black", arr.pos = 0.5, 
          path = "L", curve = c(0.1, 0.1), dr = 0.01, code = 1, ...)
}
\arguments{
  \item{pos }{2-valued vector with coordinates (x,y) of points *from and to*
    which to draw arrow.
  }
  \item{lwd }{line width.
  }
  \item{lty }{line type.
  }
  \item{lcol }{line color.
  }
  \item{arr.pos }{relative position of arrowhead.
  }
  \item{path }{position of circle: R, L, U, D for right, left, up and down
    respectively.
  }
  \item{curve }{relative size of curve (fraction of arrow length).
  }
  \item{dr }{size of segments, in radians, to draw ellipse (decrease for
    smoother).
  }
  \item{code }{how to put the arrowhead.
  }
  \item{... }{arguments passed to function Arrows.
  }
}
\value{
  coordinates (x,y) where arrowhead is drawn
}
\author{Karline Soetaert <k.soetaert@nioo.knaw.nl>}
\examples{
openplotmat(main = "selfarrow")

pos <- coordinates(3, mx = 0.05)

text(pos, LETTERS[1:3], cex = 2)

for (i in 1:3) 
  selfarrow(pos = pos[i, ], path = "R", arr.pos = 0.2,
            curve = c(0.05, 0.1), lcol = "darkred")

for (i in 1:3) 
  selfarrow(pos = pos[i, ], path = "L", arr.pos = 0.7,
            lcol = "darkblue", curve = c(0.05, 0.05))

for (i in 1:3) 
  selfarrow(pos = pos[i, ], path = "L", arr.pos = 0.5,
            lcol = "darkgreen", code = i, arr.type = "triangle")
}

\details{
  draws a circular arrow from and to one point
    
  The position of the arrowhead on the circle is set with \code{arr.pos},
  a value between 0 (at start) and 1(at end of circle)
   
  The type of the arrowhead is set with \code{arr.type} which can take the
  values:
  \itemize{
    \item "simple"  : uses comparable \R function \link{arrows}.
    \item "triangle": uses filled triangle.
    \item "curved"  : draws arrowhead with curved edges.
    \item "circle" : draws circular head.
    \item "ellipse" : draws ellepsoid head
    \item "T"       : draws T-shaped (blunt) head
  }
  see \code{\link[shape]{Arrowhead}} for details on arrow head.
}

\seealso{
  \code{\link{straightarrow}}, \code{\link{segmentarrow}},
  \code{\link{curvedarrow}},
  \code{\link{bentarrow}},\code{\link{treearrow}}, \code{\link{splitarrow}},

  \code{\link{arrows}}: the comparable \R function,

  \code{\link[shape]{Arrows}}: more complicated arrow function from package
  shape.
}
\keyword{aplot}

