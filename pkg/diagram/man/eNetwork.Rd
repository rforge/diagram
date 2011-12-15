\name{Electrical}
\alias{en.Resistor}
\alias{en.Capacitator}
\alias{en.Transistor}
\alias{en.Node}
\alias{en.Capacitator}
\alias{en.Amplifier}
\alias{en.Signal}
\alias{en.Ground}

\title{
  electric network symbols
}

\description{
  Adds a resistor, capacitator, node, amplifier, ... to a diagrom
}

\usage{
en.Resistor (mid, width = 0.05, length = 0.1, lab = NULL, pos = 0,  
   dtext = 0., vert = TRUE, ...) 

en.Capacitator (mid, width = 0.025, length = 0.1, lab = NULL, 
   pos = 2.5, dtext = 0.04, vert = TRUE, ...) 

en.Transistor (mid, gate, drain, source, r = 0.05, lab = NULL, 
   pos = 0, dtext = 0, ...) 

en.Node(mid, cex = 1, lab = NULL, pos = 2.5, dtext = 0.025,  ...) 

en.Amplifier(mid, r = 0.05, lab = NULL, pos = 0, dtext = 0, ...) 

en.Signal(mid, r = 0.03, lab = NULL, pos = 0, dtext = 0.025, ...) 

en.Ground(mid, width = 0.075, length = 0.1, n = 4, dx = 0.2, ...) 
}

\arguments{
  \item{mid }{midpoint (x,y) of the symbol.
  }
  \item{width }{width of the symbol.
  }
  \item{length }{length of the symbol.
  }
  \item{lab }{one label to be added in the symbol.
  }
  \item{pos }{position of the label in the symbol; 1 = below; 2 = left; 
    3 = upper, 4 = right; 1.5  = below-left, ...
  }
  \item{dtext }{shift in x- and/or y-direction for the text
  }
  \item{vert }{if \code{TRUE } then vertically arranged
  }
  \item{gate }{position (x,y) of the gate terminal of the \code{en.Transistor}.
  }
  \item{drain }{position (x,y) of the drain terminal of the \code{en.Transistor}.
  }
  \item{source }{position (x,y) of the source terminal of the \code{en.Transistor}.
  }
  \item{r }{radius of \code{en.Signal} and \code{en.Amplifier}
  }
  \item{cex }{size of node pch (\code{en.Node})
  }
  \item{n }{number of horizontal lines in (\code{en.Ground})
  }
  \item{dx }{size reduction of horizontal lines in (\code{en.Ground})
  }
  \item{... }{other arguments passed to functions.
  }
}

\author{
  Karline Soetaert <k.soetaert@nioo.knaw.nl>.
}

\examples{
## =============================================================================
## en.Capacitator
## =============================================================================

emptyplot(main = "en.Capacitator")
straightarrow(c(0.5, 0.9), c(0.5, 0.1),
              arr.pos = 0.3, arr.length = 0.25, arr.type = "triangle")
en.Capacitator(c(0.5, 0.5), width = 0.075, length = 0.5, vert = TRUE)
text(0.4, 0.65, "i", font = 3, cex = 2)
straightarrow(c(0.8, 0.3), c(0.8, 0.77), arr.pos = 1,
              arr.length = 0.25, arr.type = "triangle", lwd = 1)
text(0.925, 0.65, "v", font = 3, cex = 2)
text(0.15, 0.5, "C", font = 3, cex = 2)
box(col = "grey")

## =============================================================================
## en.Resistor 
## =============================================================================

emptyplot(main = "en.Resistor")
straightarrow(c(0.5, 0.9), c(0.5, 0.1), arr.pos = 0.2,
              arr.length = 0.25, arr.type = "triangle", lwd = 1)
text(0.4, 0.85, "i", font = 3, cex = 2)
en.Resistor(c(0.5, 0.5), width = 0.25, length = 0.35 )
straightarrow(c(0.8, 0.3), c(0.8, 0.77), arr.pos = 1,
              arr.length = 0.25, arr.type = "triangle", lwd = 1)
text(0.925, 0.65, "v", font = 3, cex = 2)
text(0.5, 0.5, "R", font = 3, cex = 2)
box(col = "grey")

## =============================================================================
## en.Signal
## =============================================================================

emptyplot(main = "voltage source, en.Signal")
lines(c(0.5, 0.5), c(0.1, 0.9))
en.Signal(c(0.5, 0.5), r = 0.15)
straightarrow(c(0.8, 0.3), c(0.8, 0.77), arr.pos = 1,
              arr.length = 0.25, arr.type = "triangle", lwd = 1)
text(0.925, 0.65, "v", font = 3, cex = 2)
box(col = "grey")

## =============================================================================
## en.Ground
## =============================================================================

emptyplot(main = "en.Ground")
straightarrow(c(0.5, 0.7), c(0.5, 0.25), arr.pos = 1.0,
              arr.length = 0.25, arr.type = "triangle", lwd = 1)
en.Ground(c(0.5, 0.65), width = 0.25, length = 0.35 )
box(col = "grey")

## =============================================================================
## en.Node
## =============================================================================

emptyplot(main = "en.Node")
rect(0.2, 0.2, 0.8, 0.8)
en.Node(c(0.2, 0.2), lab = "N1" , pos = 1.5)
en.Node(c(0.2, 0.8), lab = "N2" , pos = 2.5)
en.Node(c(0.8, 0.8), lab = "N3" , pos = 3.5)
en.Node(c(0.8, 0.2), lab = "N2" , pos = 4.5)
box(col = "grey")


## =============================================================================
## en.Amplifier example
## =============================================================================

emptyplot(main = "en.Amplifier")
en.Amplifier(c(0.5, 0.5), r = 0.15)
box(col = "grey")

## =============================================================================
## en.Transistor example
## =============================================================================

emptyplot(main = "enTransistor")
gate <- c(0.1, 0.5)
mid <- c(0.5, 0.5)
drain <- c(0.9, 0.9)
source <- c(0.9, 0.1)
en.Transistor(mid = mid, gate = gate, drain = drain, 
    source = source, r = 0.15)
text(0.2, 0.4, "Gate", font = 3)
text(0.8, 0.9, "Drain", font = 3, adj = 1)
text(0.8, 0.1, "Source", font = 3, adj = 1)

box(col = "grey")

## =============================================================================
## position of text examples
## =============================================================================

mf <- par (mfrow = c(2, 2))
openplotmat(main = "pos")
segments(0, 0.5, 1, 0.5)
for (i in 4:0)
  en.Resistor( mid = c(0.5, 0.5), width = 0.25, length = 0.25,
               lab = i, pos = i, dtext = 0.2)

openplotmat(main = "pos")
segments(0, 0.5, 1, 0.5)
for (i in 1:4)
  en.Resistor( mid = c(0.5, 0.5), width = 0.25, length = 0.25,
               lab = i+0.5, pos = i+0.5, dtext = 0.25)

openplotmat(main = "vert = TRUE")
segments(0.5, 0, 0.5, 1)

for (i in 1:4)
  en.Resistor( mid = c(0.5, i/5), width = 0.075, length = 0.125,
               lab = substitute(R[i], list(i = i) ))


openplotmat(main = "vert = FALSE")
segments(0, 0.5, 1, 0.5)
for (i in 1:4)
  en.Resistor( mid = c(i/5, 0.5), width = 0.075, length = 0.125,
               lab = substitute(R[i], list(i = i) ), vert = FALSE)

par(mfrow = mf)

## =============================================================================
## A small transistor example
## =============================================================================

par(lwd = 1.5)
par(mar = c(0, 0, 2, 0))
emptyplot(main = "transistor Amplifier",
          ylim = c(-0.1, 1), xlim = c(-0.1, 1.1),
          asp = FALSE)
x1 <- 0; x2 <- 0.2; x3 <- 0.4; x4 <- 0.6; x5 <- 0.8; x6 <- 1;
y1 <- 0.05; y2 <- 0.4; y3 <- 0.5; y4 <- 0.6; y5 <- 0.95 
x23 <- (x2 + x3)/2
x56 <- (x5 + x6)/2
lines(c(x2, x6, x6, x2, x2, x1, x1, x23, x3, x3),
      c(y1, y1, y5, y5, y1, y1, y3, y3,  y4, y5))
lines(c(x3, x3),
      c(y2, y1))
lines(c(x3,  x4,  x4),
      c(y2,  y2,  y1))
lines(c(x3,  x5, x5),
      c(y4,  y4, y1))

en.Transistor(mid = c(x23, y3), gate = c(x2, y3), 
              drain = c(x3,y4), source = c(x3,y2), r = 0.035)

en.Signal(c(x1, 0.2), lab = expression("U"["in"]))
en.Signal(c(x6, y2), lab = expression("U"["b"]))
straightarrow(c(x1 - 0.05, 0.23), c(x1 - 0.05, 0.17),
              arr.pos =1, arr.type = "triangle", lwd = 1)
straightarrow(c(x6 + 0.05, y2 + 0.03), c(x6 + 0.05, y2 - 0.03),
              arr.pos = 1, arr.type = "triangle", lwd = 1)

en.Node(c(x1, y3), lab = "u1")
en.Node(c(x2, y3), lab = "u2")
en.Node(c(x3, y2), lab = "u3", pos = 1.5)
en.Node(c(x3, y4), lab = "u4", pos = 2.5)
en.Node(c(x5, y4), lab = "u5")

en.Capacitator(c(0.5*(x1 + x2),y3), lab = "C1", vert = FALSE)
en.Capacitator(c(x4, y4), lab = "C3", vert = FALSE)
en.Capacitator(c(x4, 0.5*(y1+y2)), lab = "C2", vert = TRUE)

en.Resistor(c(x1, y2), lab = "R0")
en.Resistor(c(x2, 0.5*(y1+y2)), lab = "R1")
en.Resistor(c(x2, 0.5*(y4+y5)), lab = "R2")
en.Resistor(c(x3, 0.5*(y4+y5)), lab = "R4")
en.Resistor(c(x3, 0.5*(y1+y2)), lab = "R3")
en.Resistor(c(x5, 0.5*(y1+y2)), lab = "R5")

en.Ground(c(1.0, 0.05))

}
\details{
Created for drawing the electrical network in the book 
Soetaert Karline, Jeff Cash and Francesca Mazzia. Solving differential 
equations in R. Springer.
}

\seealso{
  \code{\link{textdiamond}}, \code{\link{textellipse}}, \code{\link{textempty}},
  \code{\link{texthexa}}, \code{\link{textmulti}}, \code{\link{textplain}},
  \code{\link{textround}}
}
\keyword{aplot}

