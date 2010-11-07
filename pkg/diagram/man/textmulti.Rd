\name{textmulti}
\alias{textmulti}
\title{
  adds lines of text in an multigonal box to a plot
}

\description{
  adds one or more lines of text, centered around "mid"
  in an multigonal box
}
\usage{
textmulti(mid, radx, rady = radx*length(lab), lwd = 1, 
          shadow.size = 0.01, adj = c(0.5, 0.5), 
          lab = "", box.col = "white", lcol = "black", 
          shadow.col = "grey", angle = 0, nr = 6, ...)
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
  \item{nr }{the number of angles.}
  
  \item{... }{other arguments passed to function \link{textplain}.
  }
}

\author{
  Karline Soetaert <k.soetaert@nioo.knaw.nl>
}

\examples{
  openplotmat(xlim = c(-0.1, 1.1), main = "textmulti")

  for (i in 1:10) 
    textmulti(mid = runif(2), col = i, radx = 0.1, rady = 0.1,
             lab = LETTERS[i], cex = 2, nr = trunc(i/1.5)+3)
}

\details{
  see \code{\link{shadowbox}} for specifications of the multigonal box and
  its shadow.
}

\seealso{
  \code{\link{textdiamond}}, \code{\link{textellipse}}, \code{\link{textempty}},
  \code{\link{texthexa}}, \code{\link{textplain}}, \code{\link{textrect}},
  \code{\link{textround}}.
}

\keyword{aplot}

