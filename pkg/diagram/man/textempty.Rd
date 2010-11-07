\name{textempty}
\alias{textempty}
\title{
  adds lines of text, on a colored background to a plot
}
\description{
  adds one or more lines of text, with a colored background, no box
}

\usage{
textempty(mid, lab = "", adj = c(0.5, 0.5), 
          box.col = "white", cex = 1, ...)
}

\arguments{
  \item{mid }{midpoint (x,y) of the text.
  }
  \item{lab }{one label or a vector string of labels to be added in box.
  }
  \item{adj }{text adjustment.
  }
  \item{box.col }{background color.
  }
  \item{cex }{relative size of text.
  }
  \item{... }{other arguments passed to function \link{textplain}.
  }
}
\author{
  Karline Soetaert <k.soetaert@nioo.knaw.nl>
}

\examples{
openplotmat(xlim = c(-0.1, 1.1), col = "lightgrey", main = "textempty")

for (i in 1:10) 
  textempty(mid = runif(2), box.col = i, lab = LETTERS[i], cex = 2)

textempty(mid = c(0.5, 0.5), adj = c(0, 0), 
  lab = "textempty", box.col = "white")
}

\seealso{
  \code{\link{textdiamond}}, \code{\link{textellipse}}, \code{\link{texthexa}},
  \code{\link{textmulti}}, \code{\link{textplain}}, \code{\link{textrect}},
  \code{\link{textround}}
}

\keyword{aplot}

