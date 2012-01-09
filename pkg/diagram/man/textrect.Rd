\name{textrect}
\alias{textrect}
\alias{textparallel}

\title{
  adds lines of text in a rectangular-shaped box or in a parallelogram to a plot
}

\description{
  Adds one or more lines of text, centered around "mid"
  in a rectangular box, or in a paralellogram
}

\usage{
textrect(mid, radx, rady = radx*length(lab), lwd = 1, 
         shadow.size = 0.01, adj = c(0.5, 0.5), 
         lab = "", box.col = "white", 
         lcol = "black", shadow.col = "grey", angle = 0, ...)
         
textparallel (mid, radx, rady = radx*length(lab), lwd = 1,
         shadow.size = 0.01, adj = c(0.5, 0.5),
         lab = "", box.col = "white",
         lcol = "black", shadow.col = "grey",
         angle = 0, theta = 90, ...)
}

\arguments{
  \item{mid }{midpoint (x,y) of the box.
  }
  \item{radx }{horizontal radius of the box.
  }
  \item{rady }{vertical radius of the box.
  }
  \item{lwd }{line width of line surrounding the box.
  }
  \item{shadow.size }{relative size of shadow.
  }
  \item{adj }{text adjustment.
  }
  \item{lab }{one label or a vector string of labels to be added in box.
  }
  \item{box.col }{fill color of the box.
  }
  \item{lcol }{line color surrounding box.
  }
  \item{shadow.col }{color of shadow.
  }
  \item{angle }{rotation angle, degrees.
  }
  \item{theta }{angle of the bottom, left corner of the parallelogram, in degrees.
  }
  \item{... }{other arguments passed to function textplain.
  }
}

\author{
  Karline Soetaert <karline.soetaert@nioz.nl>


  Thanks to Michael Folkes for the code of the parallelogram.
}

\examples{
  openplotmat(xlim = c(-0.1, 1.1), main = "textrect")
  for (i in 1:10) 
    textrect(mid = runif(2), col = i, radx = 0.1, rady = 0.1,
            lab = LETTERS[i], cex = 2)

  openplotmat(xlim = c(-0.1, 1.1), main = "textparallel")
  elpos <-coordinates (c(1, 1, 1, 1, 1))

  textparallel(mid = elpos[1,], col = 1, radx = 0.2, rady = 0.1,
            lab = "theta=20", theta = 20)
  textparallel(mid = elpos[2,], col = 1, radx = 0.2, rady = 0.1,
            lab = "theta=60", theta = 60)
  textparallel(mid = elpos[3,], col = 1, radx = 0.2, rady = 0.1,
            lab = "theta=100", theta = 100)
  textparallel(mid = elpos[4,], col = 1, radx = 0.2, rady = 0.1,
            lab = "theta=140", theta = 140)
  textparallel(mid = elpos[5,], col = 1, radx = 0.2, rady = 0.1,
            lab = "theta=170", theta = 170)
}

\details{
  see \code{\link{shadowbox}} for specifications of the rectangular box and
  its shadow.
  
}

\seealso{
  \code{\link{textdiamond}}, \code{\link{textellipse}}, \code{\link{textempty}},
  \code{\link{texthexa}}, \code{\link{textmulti}}, \code{\link{textplain}},
  \code{\link{textround}}
}
\keyword{aplot}

