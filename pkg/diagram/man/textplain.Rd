\name{textplain}
\alias{textplain}

\title{
  adds lines of text to a plot
}

\description{
  adds one or more lines of text, centered around "mid"
}

\usage{
  textplain(mid, height = 0.1, lab = "", adj = c(0.5, 0.5), ...)
}

\arguments{
  \item{mid }{central coordinates where to write the text.
  }
  \item{height }{height of text.
  }
  \item{lab }{one or more character strings or expressions specifying the
    *text* to be written.
  }
  \item{adj }{label adjustments.
  }
  \item{... }{other arguments passed to R-function \link{text}.
  }
}

\author{
  Karline Soetaert <k.soetaert@nioo.knaw.nl>
}

\examples{
  openplotmat(main = "textplain")
  textplain(mid = c(0.5, 0.5), 
            lab = c("this text is", "centered", "4 strings", "on 4 lines"))
  textplain(mid = c(0.5, 0.2), adj = c(0, 0.5), font = 2, height = 0.05,
            lab = c("this text is","left alligned"))
  textplain(mid = c(0.5, 0.8), adj = c(1, 0.5), font = 3, height = 0.05, 
            lab = c("this text is","right alligned"))
}

\seealso{
  \code{\link{textdiamond}}, \code{\link{textellipse}}, \code{\link{textempty}},
  \code{\link{texthexa}}, \code{\link{textmulti}}, \code{\link{textrect}},
  \code{\link{textround}}
}

\keyword{aplot}

