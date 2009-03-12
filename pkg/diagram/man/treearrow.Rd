\name{treearrow}
\alias{treearrow}
\title{
  adds a dendrogram-like branched arrow between several points
}
\description{
  connects two sets of points with a dendrogram-like structure,

  adds an arrowhead at a certain distance.
}

\usage{
treearrow(from, to, lwd=2, lty=1, lcol="black",
  arr.side=2, arr.pos=0.5, line.pos=0.5, path="H", ...)
}

\arguments{
  \item{from }{matrix of coordinates (x,y) of points *from* which to draw
    arrow.
  }
  \item{to }{matrix of coordinates (x,y) of points *to* which to draw arrow.
  }
  \item{lwd }{line width.
  }
  \item{lty }{line type.
  }
  \item{lcol }{line color.
  }
  \item{arr.side }{segment number on which arrowhead is drawn (1,2).
  }
  \item{arr.pos }{relative position of arrowhead on segment on which
    arrowhead is drawn.
  }
  \item{line.pos }{relative position of (horizontal/vertical) line.
  }
  \item{path }{Vertical, Horizontal.
  }
  \item{... }{other arguments passed to function \link{straightarrow}.
  }
}

\value{
  coordinates (x,y) where arrowhead is drawn
}

\author{
  Karline Soetaert <k.soetaert@nioo.knaw.nl>.
}

\examples{
  openplotmat(main="treearrow")
  pos <- coordinates(c(3,2,4,1))
  treearrow(from=pos[1:5,],to=pos[6:10,])
  for (i in 1:10) textrect(pos[i,],lab=i,cex=2,radx=0.05)
  
  openplotmat(main="treearrow")
  pos <- coordinates(c(2,4),hor=FALSE)
  treearrow(from=pos[1:2,],to=pos[3:6,],arr.side=1:2,path="V")
  for (i in 1:6) textrect(pos[i,],lab=i,cex=2,radx=0.05)
  
  
  openplotmat(main="treearrow")
  pos <- coordinates(c(3,5,7,7,5,3))
  treearrow(from=pos[1:15,],to=pos[15:30,],arr.side=0)
  for (i in 1:30) textrect(pos[i,],lab=i,cex=1.2,radx=0.025)
}

\details{
  a tree-shaped arrow is drawn between points '(\code{from}, \code{to})',
  where both \code{from} and \code{to} can be several points.

  How the segments are drawn is set with \code{path} which can take on the
  values:
  \itemize{
    \item "H": (horizontal): first left or right.
    
    \item "V": (vertical): first down- or upward.
  }
  The segment(s) on which the arrow head is drawn is set with \code{arr.side},
  which is one or more values in (1, 2)

  The position of the arrowhead on the segment on which it is drawn,
  is set with \code{arr.pos}, a value between 0(start of segment) and
  1(end of segment)
   
  The type of the arrowhead is set with \code{arr.type} which can take the
  values:
  \itemize{
    \item "simple"  : uses comparable \R function \link{arrows}.
    \item "triangle": uses filled triangle.
    \item "curved"  : draws arrowhead with curved edges.
    \item "circle"  : draws circular head.
  }
  see \code{\link[shape]{Arrowhead}} from package \code{shape} for details
  on arrow head.
}

\seealso{
  \code{\link{straightarrow}}, \code{\link{segmentarrow}},
  \code{\link{curvedarrow}},  \code{\link{selfarrow}},
  \code{\link{bentarrow}}, \code{\link{splitarrow}},

  \code{\link{arrows}}: the comparable R function,

  \code{\link[shape]{Arrows}}: more complicated arrow function from package
  shape.
}
\keyword{aplot}

