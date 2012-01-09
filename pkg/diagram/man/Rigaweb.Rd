\name{Rigaweb}
\docType{data}
\alias{Rigaweb}
\title{
  Gulf of Riga autumn planktonic food web
}

\description{
  Carbon flux matrix of the Gulf of Riga planktonic food web in autumn
  as reconstructed by inverse modelling by Donali et al. (1999).
  
  The Gulf of Riga is a highly eutrophic system in the Baltic Sea.
  
  The foodweb comprises 7 functional compartments: 
  \itemize{
    \item picoautotrophs (P1)
    \item non-picoautotrophs (P2)
    \item bacteria (B)
    \item heterotrophic nanoflagellates (N)
    \item zooplankton (Z)
    \item detritus, including virus (D)
    \item dissolved organic carbon (DOC)
  }
  and two external compartments:
  \itemize{
    \item CO2
    \item Sedimentation
  }
  These compartments are connected with 26 flows.

  Units of the flows are mg C/m3/day.
}

\usage{Rigaweb}

\format{
  matrix with flow values, where element ij denotes flow from compartment i to j

  rownames and columnames are the components.
}

\author{
  Karline Soetaert <karline.soetaert@nioz.nl>

}

\examples{
plotweb(Rigaweb, main = "Gulf of Riga planktonic food web",
        sub = "mgC/m3/day")
}

\references{
  Donali, E., Olli, K., Heiskanen, A.S., Andersen, T., 1999. Carbon flow
  patterns in the planktonic food web of the Gulf of Riga, the Baltic Sea: 
  a reconstruction by the inverse method.
  Journal of Marine Systems 23, pp. 251 268.
}

\seealso{
  \code{\link{Takapotoweb}}
}

\keyword{datasets}

