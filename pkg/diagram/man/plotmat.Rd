\name{plotmat}
\alias{plotmat}
\title{
  plots a graph (network), based on a transition matrix
}
\description{
  visualises a transition matrix as a number of (labeled) boxes connected by
  (labeled) arrows.
}

\usage{
plotmat(A, pos=NULL, curve=NULL, name=NULL, absent=0,  
  relsize=1, lwd=2, lcol="black" , box.size=0.1, box.type="circle",  
  box.prop=1,box.col="white", box.lcol=lcol, box.lwd=lwd, 
  shadow.size=0.01, shadow.col="grey", dr=0.01, dtext=0.3,  
  self.lwd=1, self.cex=1, self.shiftx=box.size, self.shifty=NULL, 
  self.arrpos=NULL, arr.lwd=lwd, arr.lcol=lcol, arr.col="black",
  arr.type="curved",arr.pos=0.5, arr.length=0.4, arr.width=arr.length/2,
  endhead=FALSE, mx=0.0, my=0.0, box.cex=1, txt.col="black",
  prefix="", cex.txt=1, add=FALSE, main="", ...)
}
\arguments{
  \item{A }{square coefficient matrix, specifying the links (rows=to,
    cols=from).
  }
  \item{pos }{vector, specifying the number of elements in each graph row, 
    or a 2-column matrix with element position, or NULL.
  }
  \item{curve }{one value, or a matrix, same dimensions as \code{A}, specifying
    the arrow curvature; 0 for straight; NA for default curvature.
  }
  \item{name }{string vector, specifying the names of elements, dimension =
    number of rows (columns) of \code{A}.
  }
  \item{absent }{all elements in \code{A} different from this value are
    connected.
  }
  \item{relsize }{scaling factor for size of the graph.
  }
  \item{lwd }{default line width of arrow and box.
  }
  \item{lcol }{default color of arrow line and box line.
  }
  \item{box.size }{size of label box, one value or a vector with dimension =
    number of rows of \code{A}.
  }
  \item{box.type }{shape of label box (rect, ellipse, diamond, round, hexa,
    multi), one value or a vector with dimension=number of rows of \code{A}.
  }
  \item{box.prop }{length/width ratio of label box, one value or a vector
    with dimension=number of rows of \code{A}.
  }
  \item{box.col }{fill color of label box, one value or a vector with
    dimension=number of rows of \code{A}.
  }
  \item{box.lcol }{line color of label box, one value or a vector with
    dimension=number of rows of \code{A}.
  }
  \item{box.lwd }{line width of the box, one value or a vector with dimension
    = number of rows of \code{A}.
  }
  \item{shadow.size }{relative size of shadow of label box, one value or a
    vector with dimension=number of rows of \code{A}.
  }
  \item{shadow.col }{color of shadow of label box, one value or a vector with
    dimension=number of rows of \code{A}.
  }
  \item{dr }{size of segments, in radians, to draw ellipse (decrease for
    smoother ellipses).
  }
  \item{dtext }{controls the position of arrow text relative to arrowhead.
  }
  \item{self.lwd }{line width of self-arrow, (arrow from i to i), one value or
    a vector with dimension=number of rows of \code{A}.
  }
  \item{self.cex }{relative size of self-arrow, relative to box, one value or
    a vector with dimension=number of rows of \code{A}.
  }
  \item{self.shiftx }{relative shift of self-arrow, in x-direction, one value
    or a vector with dimension=number of rows of \code{A}.
  }
  \item{self.shifty }{relative shift of self-arrow, in x-direction, one value
    or a vector with dimension=number of rows of \code{A}.
  }
  \item{self.arrpos }{position of the self-arrow, angle in radians relative to
    x-direction, one value or a vector with dimension=number of rows of
    \code{A}.
  }
  \item{arr.lwd }{line width of arrow, connecting two different points, one
    value, or a matrix with same dimensions as \code{A}.}
  \item{arr.lcol }{color of arrow line, one value, or a matrix with same
    dimensions as \code{A}.
  }
  \item{arr.col }{color of arrowhead, one value, or a matrix with same
    dimensions as \code{A}.
  }
  \item{arr.type }{type of arrowhead (curved,triangle,circle,simple), one
    value, or a matrix with same dimensions as \code{A}.
  }
  \item{arr.pos }{relative position of arrowhead on arrow segment/curve, one
    value, or a matrix with same dimensions as \code{A}.
  }
  \item{arr.length }{arrow length, one value, or a matrix with same
    dimensions as \code{A}.
  }
  \item{arr.width }{arrow width, one value, or a matrix with same dimensions
    as \code{A}.
  }
  \item{endhead }{if TRUE: the arrow line stops at the arrowhead; default =
    \code{FALSE} and arrow line continues beyond the arrow head.
  }
  \item{mx }{horizontal shift of the boxes.
  }
  \item{my }{vertical shift of the boxes.
  }
  \item{box.cex }{relative size of text in boxes, one value or a vector with
    dimension=number of rows of \code{A}.
  }
  \item{txt.col }{color of text in boxes, one value or a vector with
    dimension=number of rows of \code{A}.
  }
  \item{prefix }{to be added in front of non-zero arrow labels.
  }
  \item{cex.txt }{relative size of arrow text, one value, or a matrix with
    same dimensions as \code{A}.
  }
  \item{add }{start a new plot (\code{FALSE}), or add to current plot
    (\code{TRUE}).
  }
  \item{main }{main title.
  }
  \item{... }{other arguments passed to function \code{shadowbox}.
  }
}
\value{
  a list containing:
  \item{arr }{ a data.frame with arrow information: 
    \itemize{
      \item nonzero: the elements between which an arrow is drawn.
      \item Angle: the angle of the arrow.
      \item Value: the value written next to the arrow head.
      \item rad: the radius of the arrow (if 0: straight line).
      \item ArrowX: the x-position of arrowhead.
      \item ArrowY: the y-position of arrowhead.
      \item TextX: the x-position of arrowtext.
      \item TextY: the y-position of arrowtext.
    }.
  }
  \item{comp }{a matrix with the element position (centre of the boxes).
  }
  \item{radii }{the radiusses in x- and y-direction of the boxes.
  }
  \item{rect }{the "xleft","ybot","xright",and "ytop" of the boxes - redundant.
  }
}
\author{
  Karline Soetaert <k.soetaert@nioo.knaw.nl>
}

\examples{
  M <- matrix(nrow=4,ncol=4,byrow=TRUE,data=0)
  pp<-plotmat(M,pos=c(1,2,1),name=c("A","B","C","D"),lwd=1,box.lwd=2,
              cex.txt=0.8,box.size=0.1,box.type="square",box.prop=0.5,
              main="plotmat")
              
  M[2,1]<-M[3,1]<-M[4,2]<-M[4,3] <- "flow"

  col   <- M
  col[] <- "red"
  col[2,1]<-col[3,1]<-"blue"
  pp<-plotmat(M,pos=c(1,2,1),curve=0,name=1:4,lwd=1,box.lwd=2,
              box.cex=1:4,cex.txt=0.8, arr.lcol=col,arr.col=col,
              box.type="circle",box.prop=1.0,main="plotmat")

  diag(M) <- "self"
  pp<-plotmat(M,pos=c(2,2),curve=0,name=LETTERS[1:4],lwd=1,box.lwd=2,
              cex.txt=0.8,self.cex=0.5,self.shiftx=c(-0.1,0.1,-0.1,0.1),
              box.type="diamond",box.prop=0.5,main="plotmat")
  
  M <- matrix(nrow=4,ncol=4,data=0)
  M[2,1]<-1  ;M[4,2]<-2;M[3,4]<-3;M[1,3]<-4
  pp<-plotmat(M,pos=c(1,2,1),curve=0.2,name=letters[1:4],lwd=1,box.lwd=2,
              cex.txt=0.8,arr.type="triangle",box.size=0.1,box.type="hexa",
              box.prop=0.5,main="plotmat")

  arrlwd <- M*2
  arr.length <- M*0.4
  cextxt <- M*0.8
  plotmat(M,pos=c(1,2,1),curve=0.2,name=letters[1:4],lwd=1,box.lwd=2,
          arr.type="triangle",box.size=0.1,box.type="hexa",
          box.prop=0.5,main="plotmat",arr.lwd=arrlwd,
          arr.length=arr.length,cex.txt=cextxt)

  M <- matrix(nrow=4,ncol=4,byrow=TRUE,data=0)
  M     <- as.data.frame(M)
  M[[2,1]]<- "k[si]"
  M[[3,1]]<- "k[N]"
  M[[4,2]]<- "sqrt(frac(2,3))"

  names <-
  c(expression(lambda[12]),"?",expression(lambda[13]),expression(lambda[23]))

  pp<-plotmat(M,pos=c(1,2,1),name=names,lwd=1,box.lwd=2, curve=0,
              cex.txt=0.8,box.size=0.1,box.type="square",box.prop=0.5,
              main="plotmat")
              
  plotmat(M,name=letters[1:4],curve=0,box.cex=1:4,box.lwd=4:1,
          box.size=0.075,arr.pos=0.7,
          box.col=c("lightblue","green","yellow","orange"))


              }
\details{
  The square transition matrix \code{A} determines the number of elements
  of \code{A} (rows of \code{A}) and which elements are connected
 (all values in \code{A} different from \code{absent}).

  \code{A} also provides the values of arrowlabels.
     
  The position of the elements are specified with \code{pos}, which is
  either \code{NULL},
  or a vector specifying the number of elements on a row, or a 2-columned
  matrix specifying the (x,y) position of each element.

  The ordering of elements is according to the row number of \code{A}
  \itemize{
    \item when \code{pos} is \code{NULL}, the elements will be arranged on a
      circle
    \item when \code{pos} is a vector, it specifies the number of elements in
      each row.
      For instance, with \code{pos = c(3,2,1)} the elements will be arranged
      in 3 rows (length of vector); on top row, 3 elements; on second row 2,
      and on third row 1 element will be positioned. All elements within a
      row are equally distributed horizontally, all rows are equally
      distributed vertically.
    \item when \code{pos} is a matrix, it specifies the x (1st column) and y
      (2nd column) position of each element.
  }

  The offset from x-axis and from y-axis can be changed with \code{mx} and
  \code{my}.
     
  The name of each element is given by vector \code{name}; this name is
  written in its respective box.

  The relative size of this text can be changed by \code{box.cex}.
  
  If \code{shadow} is \code{TRUE}, a shadow is drawn, in the right-lower
    corner of the box,

    its color and relative size specified with \code{shadow.col} and
    \code{shadow.size} respectively.

    both can be one value (equal shadows) or a vector, specifying one value
    for each box shadow.
  
  The type of the box is set with "box.type" which can take on the values:
  \itemize{
    \item "rect": a rectangle,
    \item "ellipse": an ellipse,
    \item "diamond": a diamond,
    \item "round": a rectangle with rounded left and right edges,
    \item "hexa": a hexagonal shape,
    \item "multi": a multigonal shape.
  }
  The length of the box is set with \code{box.size}, the proportionality
   (length/width) ratio with \code{box.prop}.

  The fill-color of the box is specified with \code{box.col};
  the line width of the box with \code{box.lwd} and the line color with
  \code{box.lcol};
  
  All box properties can be either one value (equal boxes) or a vector,
  specifying one value for each box.
  
  For all values A[i,j] of \code{A} which are not equal to \code{absent},
  one arrow is drawn *from* column-element j *to* the row-element i of \code{A}.
  
  The curvature of this arrow is specified with matrix element curve[i,j],

  where 'curve' is either NULL, one value, or has the same dimension as \code{A}.

  A straight arrow has curvature 0, \code{NA} (the default) chooses a default
  curvature,

  Positive or negative values of \code{curve} draws curved arrows.

  If the arrow is curved, then \code{dr} is the increment used to draw the
  ellipse; set to a lower value for smoother lines.
  
  The type of the arrowhead is set with \code{arr.type} which can take the
  values:
  \itemize{
    \item "simple"  : uses comparable R function arrows  
    \item "triangle": uses filled triangle
    \item "curved"  : draws arrowhead with curved edges
    \item "circle" : draws circular head 
  }
  The line color and width of the arrow line is set with \code{arr.lcol} and
  \code{arr.lwd}

  The size of the arrow head is specified with \code{arr.length} and
  \code{arr.width},

  the position of the arrow head is specified with \code{arr.pos} (value
  between [0,1]).

  see \code{\link[shape]{Arrowhead}} for details on arrow head
}

\seealso{
  \code{\link{shadowbox}},

  \code{\link[shape]{Arrowhead}} from package shape

  try: demo(plotmat)
}
\keyword{aplot}

