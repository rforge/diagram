\name{deepCmin}
\docType{data}
\alias{deepCmin}
\title{results of the calibration exercise from chapter 4.4.4}
\description{This datafile contains output from the calibration exercise from
chapter 4.4.4. They are:
\cr vectors \code{kseries}, \code{multser}
\cr vector \code{outcost}
\cr matrices \code{optpar}, \code{optpar20}, \code{optpar25}}
\usage{deepCmin}
\author{Karline Soetaert <karline.soetaert@nioz.nl>}
\examples{
pgr <- gray.colors(n = 25, start = 0.95, end = 0.0)
with (deepCmin,
filled.contour(x = multser, y = kseries, z = outcost,
   ylab = "k (/day)", xlab = "multiplication factor (-)",
   main = "Model cost landscape", col = pgr, nlevels = 25,
   plot.axes = {
     axis(1); axis(2);
     points(optpar20$poppar[,2], optpar20$poppar[,1], pch = "o", cex = .5);
     points(optpar25$poppar[,2], optpar25$poppar[,1], pch = "+", cex = 1);
     points(optpar$par[2], optpar$par[1], pch = 16, cex = 2)
   }
)
)}
\keyword{datasets}

