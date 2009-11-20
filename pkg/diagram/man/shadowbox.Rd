\name{shadowbox}
\alias{shadowbox}
\title{
  adds a box with a shadow to a plot
}
\description{
  adds a box, with shadow on a plot; used for writing text
}

\usage{
shadowbox(box.type="rect", mid, radx, rady=radx, shadow.size=0.01, 
  shadow.col="grey",box.col="white", lcol="black", lwd=1,  
  dr=0.01, angle=0,  len=1, nr=5, rx=rady, ...)
}

\arguments{
  \item{box.type }{shape of the box.
  }
  \item{mid }{midpoint (x,y) of the box.
  }
  \item{radx }{horizontal radius of the box.
  }
  \item{rady }{vertical radius of the box.
  }
  \item{shadow.size }{relative size of shadow.
  }
  \item{shadow.col }{color of shadow.
  }
  \item{box.col }{fill color of the box.
  }
  \item{lcol }{line color surrounding box.
  }
  \item{lwd }{line width of line surrounding the box.
  }
  \item{dr }{if box is curved: size of segments, in radians, to draw
    ellipse (decrease for smoother).
  }
  \item{angle }{rotation angle, degrees.
  }
  \item{len }{if \code{box.type}="cylinder": length of the cylinder.
  }
  \item{nr }{if \code{box.type}="multi": the number of angles.
  }
  \item{rx }{if \code{box.type}="round", the radius of the rounded part.
  }
  \item{... }{other arguments.
  }
}

\author{
  Karline Soetaert <k.soetaert@nioo.knaw.nl>
}

\examples{
    openplotmat(main="shadowbox")
    shadowbox(box.type="rect",mid=c(0.1,0.5),rady=0.1,radx=0.05,angle=25)
    shadowbox(box.type="round",mid=c(0.3,0.5),rady=0.05,radx=0.025,angle=90,
              shadow.col="darkred")
    shadowbox(box.type="ellipse",mid=c(0.5,0.5),rady=0.05,radx=0.075,
              box.col="blue")
    shadowbox(box.type="multi",mid=c(0.8,0.5),rady=0.05,radx=0.05,
              box.col="darkblue",nr=5)
}

\details{
  one box is drawn, centered aroung point \code{mid} and with horizontal and
  vertical radiusses \code{radx}, \code{rady}.
     
  By default, a shadow is drawn, in the right-lower corner of the box.

  The shadow color and relative size is specified with \code{shadow.col} and
    \code{shadow.size} respectively. 

  \code{shadow.size = 0} toggles off the drawing of the shadow.
     
  the type of the box is set with \code{box.type} which can take on the values:
  \itemize{
    \item "rect": a rectangle.
    \item "ellipse": an ellipse.
    \item "diamond": a diamond.
    \item "round": a rectangle with rounded sides.
    \item "hexa": a hexagonal shape.
    \item "multi": a multigonal shape; also input "nr", the number of angles.
    \item "cylinder": a cylindrical shape; also input "len", the length of
      the cylinder.
  }
  the fill-color of the box is specified with \code{box.col};

  the line width and color of the box are specified with \code{lwd} and
  \code{lcol}
}
\keyword{aplot}

