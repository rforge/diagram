\name{plotweb}
\alias{plotweb}
\title{
  plots a web
}

\description{
  plots a web, based on a flow matrix
}

\usage{
plotweb(flowmat, names = NULL, lab.size = 1.5, add = FALSE, 
        fig.size = 1.3, main = "", sub = "", sub2 = "", 
        log = FALSE, mar = c(2, 2, 2, 2), 
        nullflow = NULL, minflow = NULL, maxflow = NULL,
        legend = TRUE, leg.digit = 5, leg.title = NULL, 
        lcol = "black", arr.col = "black",
        val = FALSE, val.digit = 5, val.size = 0.6, val.col = "red",
        val.title = NULL, val.ncol = 1, 
        budget = FALSE, bud.digit = 5, bud.size = 0.6,  
        bud.title = "budget", bud.ncol = 1, 
        maxarrow = 10, minarrow = 1, length = 0.1, dcirc = 1.2, bty = "o", ...)
}

\arguments{
  \item{flowmat }{flow matrix, rows=flow *from*, columns=flow *to*.
  }
  \item{names }{string vector with the names of components.
  }
  \item{lab.size }{relative size of name label text.
  }
  \item{add }{start a new plot (\code{FALSE}), or add to current (\code{TRUE}).
  }
  \item{fig.size }{if \code{add = FALSE}: relative size of figure.
  }
  \item{main }{if \code{add = FALSE}: main title.
  }
  \item{sub }{if \code{add = FALSE}: sub title.
  }
  \item{sub2 }{if\code{add = FALSE}: title in bottom.
  }
  \item{log }{logical indicating whether to scale the flow values
    logarithmically.
  }
  \item{mar }{the figure margins.
  }
  \item{nullflow }{either one value or a two-valued vector; if flow <
    nullflow[1] or flow > nullflow[2] (if two values): flow is assumed
    = 0 and the arrow is not drawn.
  }
  \item{minflow }{flowvalue corresponding to minimum arrow thickness.
  }
  \item{maxflow }{flowvalue corresponding to maximum arrow thickness.
  }
  \item{legend }{logical indicating whether to add a legend with arrow thickness.
  }
  \item{leg.digit }{nr of digits for writing legend - only if \code{legend}
    = TRUE.
  }
  \item{leg.title }{title for arrow legend, e.g to give units - only if
    \code{legend} =TRUE.
  }
  \item{lcol }{line color of arrow - not used.
  }
  \item{arr.col }{arrow color. One value or a matrix, with same dimensions as
    flowmat; if a matrix, each arrow can have a different color.
  }
  \item{val }{logical indicating whether to write flow values as a legend.
  }
  \item{val.digit }{nr of digits for writing values - only if \code{val =TRUE}.
  }
  \item{val.size }{relative size for writing values - only if \code{val =TRUE}.
  }
  \item{val.col }{color for writing values - only if \code{val =TRUE}.
  }
  \item{val.title }{title for values legend - only if \code{val =TRUE}.
  }
  \item{val.ncol }{number of columns for writing values - only if
    \code{val =TRUE}.
  }
  \item{budget }{logical indicating whether to calculate budget
    (sum of flows in - sum of flows out) per component.
  }
  \item{bud.digit }{nr of digits for writing budget - only if
    \code{budget =TRUE}.
  }
  \item{bud.size }{relative size for writing budget - only if
    \code{budget =TRUE}.
  }
  \item{bud.title }{title for budget legend - only if
    \code{budget =TRUE}.
  }
  \item{bud.ncol }{number of columns for writing budget - only if
    \code{budget =TRUE}.
  }
  \item{maxarrow }{maximal thickness of arrow.
  }
  \item{minarrow }{minimal thickness of arrow.
  }
  \item{length }{length of the edges of the arrow head (in inches).
  }
  \item{dcirc }{if cannibalism (flow from i to i), offset of circular 'arrow' -
    if \code{dcirc} = 0:no circle drawn.
  }
  \item{bty }{the type of box to be drawn around the legends (\code{legend},
    \code{val}, \code{budget}). The allowed values are "o" (the default)
    and "n".
  }
  \item{... }{extra arguments passed to R-function \link{arrows}.
  }
}
\author{
  Karline Soetaert <k.soetaert@nioo.knaw.nl>
}
\examples{
plotweb(Rigaweb, main = "Gulf of Riga food web",
        sub = "mgC/m3/d", val = TRUE)

ArrCol <- Rigaweb
ArrCol[] <- "black"
ArrCol[,"Sedimentation"] <- "green"

plotweb(Rigaweb, main = "Gulf of Riga food web",
        sub = "mgC/m3/d", val = FALSE, arr.col = ArrCol)

plotweb(diag(20), main = "plotweb")
}

\details{
  This function is less flexible than function \code{\link{plotmat}}

  It is meant for visualisation of food web flows, that are inputted as a
  flow matrix.

  It displays the elements on a circle, and, where there is a mass flow,
  two elements are connected,

  the magnitude of the web flows is reflected by the thickness of the arrow

  Note that the input matrices from function \code{plotmat} and \code{plotweb}
  are transposed.
}

\seealso{
  \code{\link{plotmat}},

  \code{\link{Rigaweb}}, \code{\link{Takapotoweb}}

  try: demo(plotweb)
}

\keyword{aplot}

