\name{Teasel}
\docType{data}
\alias{Teasel}
\title{Population dynamics model transition matrix of teasel}
\description{Transition matrix of the population dynamics model of teasel (Dipsacus sylvestris),
  a European perennial weed, as discussed in Caswell (2001), and in Soetaert and Herman, (in prep)
  
  The life cycle of teasel can be described by six stages:
  \itemize{
  \item dormant seeds < 1yr (DS 1yr)
  \item dormant seeds 1-2yr (DS 2yr)
  \item small rosettes <2.5cm (R small)
  \item medium rosettes 2.5-18.9 cm (R medium)
  \item large rosettes >19 cm (R large)
  \item flowering plants (F)
}
  The matrix contains the transition probabilities from one compartment (column) to another (row)}
\usage{Teasel}
\format{matrix with transition probabilities, where element ij denotes transition from compartment j to i
  \cr rownames and columnames are the component names}
\author{Karline Soetaert <k.soetaert@nioo.knaw.nl>}
\examples{
  curves <- matrix(nrow=ncol(Teasel),ncol=ncol(Teasel),0)
  curves[3,1]<- curves[1,6]<- -0.35
  curves[4,6]<-curves[6,4]<-curves[5,6]<-curves[6,5]<-0.08
  curves[3,6]<-  0.35
  
  plotmat(Teasel,pos=c(3,2,1),curve=curves,lwd=1,box.lwd=2,
          cex.txt=0.8,box.cex=0.8,box.size=0.08,arr.length=0.5,
          box.type="circle",box.prop=1,shadow.size = 0.01,self.cex=0.6,
          my=-0.075, mx=-0.01,relsize=0.9,self.shifty=0,
          self.shiftx=c(0,0,0.125,-0.12,0.125,0),
          main="Dispsacus sylvestris")}
\references{Caswell, H. 2001. Matrix population models: construction, analysis, and interpretation.
  Second edition. Sinauer, Sunderland, Mass. 
  
  Karline Soetaert and Peter Herman. A guide to ecological modelling. Book in preparation.}
\seealso{\code{\link{Rigaweb}}, \code{\link{Takapotoweb}}}
\keyword{datasets}

