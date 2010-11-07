\name{getellipse}
\alias{getellipse}
\title{
  x-y coordinates of ellipse
}

\description{
  calculates x-y values for (part of) an ellipse; the ellipse can be rotated
}

\usage{
getellipse(rx = 1, ry = rx, mid = c(0, 0), dr = 0.01, 
           angle = 0, from = -pi, to = pi)
}

\arguments{
  \item{rx }{long radius of ellipse.
  }
  \item{ry }{short radius of ellipse.
  }
  \item{mid }{midpoint of ellipse.
  }
  \item{dr }{size of segments, in radians, to specify ellipse
    (decrease for smoother).
  }
  \item{angle }{rotation angle, degrees.
  }
  \item{from }{starting angle for ellipse segment, radians.
  }
  \item{to }{final angle for ellipse segment, radians.
  }
}

\value{
  a 2-column matrix with  x-y values of the ellipse
}

\author{
  Karline Soetaert <k.soetaert@nioo.knaw.nl>
}

\examples{
plot(getellipse(1, from = 0, to = pi/2), type = "l", col = "red",
     lwd = 2, main = "getellipse")
lines(getellipse(0.5, 0.25, mid = c(0.5, 0.5)), type = "l", 
      col = "blue", lwd = 2)
lines(getellipse(0.5, 0.25, mid = c(0.5, 0.5), angle = 45), 
      type = "l", col = "green", lwd = 2)

emptyplot(main = "getellipse")
col <- femmecol(90)
for (i in seq(0, 180, by = 2)) 
      lines(getellipse(0.5, 0.25, mid = c(0.5, 0.5), angle = i),
            type = "l", col = col[(i/2)+1], lwd = 2)
}

\details{
  \code{rx} and \code{ry} are the horizontal and vertical radiusses of the
  ellipses.
}
\seealso{
  \code{\link{plotellipse}}, \code{\link{filledellipse}}
}

\keyword{dplot}

