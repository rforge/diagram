################################################################################
##                                                                            ##
## Spheres/Cylindres/Ellipses/Arrows                                          ##
##    Karline Soetaert                                                        ##
##                                                                            ##
##                                                                            ##
## rotatexy        : rotates xy values around a midpoint                      ##
##                                                                            ##
## plotcircle      : plots a circle                                           ##
## circlesegment   : fills a segment of a circle                              ##
## filledcircle    : draws and colors circle; colors depend on radius         ##
##                                                                            ##
## plotellipse     : plots an ellipse                                         ##
## getellipse      : gets the data for an ellipse                             ##
## ellipsesegment  : fills a segment of an ellipse                            ##
## filledellipse   : draws and colors ellipse; colors depend on radius        ##
##                                                                            ##
## filledcylinder  : draws and colors cylinder colors depend on radius        ##
## filledrectangle : draws and colors rectangle;color depend on radius        ##
##                                                                            ##
## femmecol        : red-blue colors inspired by tim.colors (package fields)  ##
## drapecol        : colors for draping persp (surface) plots                 ##
## intpalette      : interpolates a palette                                   ##
## shadepalette    : creates a pallette that is suited for shading            ##
##                                                                            ##
## Arrows          : draws arrow with improved arrowhead                      ##
## Arrowhead       : draws arrowhead, various shapes                          ##
## A4              : Opens a window, A4 size                          
################################################################################

#########################################################################
# INTERNAL FUNCTION
#########################################################################

    val2col <- function (values,zlim,col)
    {
    if (! is.null(values)) {  # a matrix of radius, z-values
     if (min(values[,1])<0) stop ("cannot draw shape: radiusses in first column of *values* are not positive")
     values <- values [sort(values[,1],index=TRUE)$ix,]   # sort on first column (radiusses)
     
     if (is.null(zlim)) 
     {zlim<-range(values[,2])} else {
      values[,2]<-pmin(values[,2],zlim[2])
      values[,2]<-pmax(values[,2],zlim[1]) }
     
     x.to   <- (values[,2]-zlim[1])/(diff(zlim))
     Col    <- intpalette (inputcol=col,x.to = x.to)
     nrad   <- nrow(values)
     values[,1] <- values[,1]/values[nrad,1]
     intrad <- c(0,values[,1])
    } else {
    Col <- col
    nrad <- length(Col)
    intrad <- c(0, 1:nrad)/nrad

    ncol <- length(col)
    if (ncol < nrad) 
        Col <- intpalette(col, nrad)
           } 
    return(list(Col=Col,intrad=intrad,nrad=nrad))           
    } 

#########################################################################
## A4           : Opens a window , A4 size                             ##
#########################################################################

A4 <- function(...) 
{
X11(width=8.5,height=11,...)   ## A4
}


#########################################################################
## writelabel   : adds a label next to a plot                          ##
#########################################################################

writelabel <- function (text=NULL,  # text to write
                        nr=1,       # if text = NULL: nr is converted to uppercase letter
                        at=-0.1,    # relative distance of label position, from left margin of plot region
                        line=1,     # line above the plot region of label position
                        cex=1.5,    # size of label
                        ...)        # arguments passed to R-function "mtext" 
{
if (is.null(text)) text<-LETTERS[nr]

# scale factors
usr    <- par("usr")
xmin   <- usr[1]
xmax   <- usr[2]
xrange <- xmax-xmin
pos    <- xmin  + at * xrange

mtext(text=text,at=pos,line=line,cex=cex,...)

}  ## END writelabel   

#########################################################################
#  emptyplot: open plot region for shape plotting
#########################################################################

emptyplot <- function (xlim=c(0,1),    # the x limits (min,max) of the plot
                       ylim=xlim  ,    # the y limits (min,max) of the plot
                       asp=1,          # the y/x aspect ratio
                       frame.plot = FALSE, # to toggle off drawing of box
                       col=NULL,       # the background color
                       ...)            # arguments passed to R-function "plot" 
{
plot(0, type = "n",xlab="",ylab = "", asp=asp,axes=FALSE,frame.plot = frame.plot,
     xlim = xlim,ylim = ylim,xaxs="i",yaxs="i",...)
if(!is.null(col)) rect(xlim[1],ylim[1],xlim[2],ylim[2],col=col)   
}  

#**********************************************************************
#  COLORS...
#**********************************************************************

#########################################################################
## intpalette    : interpolates a palette                              ##
#########################################################################

intpalette <- function(inputcol,    # initial colors, *from* where to interpolate
                       numcol=length(x.to),      # number of colors to interpolate *to* 
                       x.from= NULL, # x-values *from* where to interpolate
                       x.to  = NULL) # x-values where to interpolate *to*
                       
{
    if (length(inputcol)<=1) return(rep(inputcol,numcol))
    ifelse (is.numeric(inputcol),rgb.col <- inputcol,rgb.col<-t(col2rgb(c(inputcol))))
    if (is.null(x.from)) x.from <- seq (0,1,length.out=nrow(rgb.col))
    if (is.null(x.to  )) x.to   <- seq (0,1,length.out=numcol)
   
    if (min(x.to)<min(x.from) |max(x.to)>max(x.from)) 
       stop ("intpalette: cannot interpolate; ranges x.to > ranges x.from")
    outcol   <- matrix(ncol=3,nrow=numcol)
    for (i in 1:3) outcol[,i] <- round(approx(x.from,rgb.col[,i],xout=x.to)$y)
    outcol[outcol<0]   <- 0
    outcol[outcol>255] <- 0
    color <- rgb(outcol[,1],outcol[,2],outcol[,3],maxColorValue = 255)
    return(color)
}

#########################################################################
## shadepalette    : creates a palette that is suited for shading      ##
#########################################################################

shadepalette <- function(n=100,                 # number of colors
                         endcol   = "red",      # final color
                         inicol   = "white",    # initial color
                         interval = c(0.0,1.0)) # interval *to* where to interpolate
                         
{
    x.to <- seq (interval[1],interval[2],length.out=n)
    return(intpalette(rbind(inicol,endcol),n,x.to=x.to))
}

#########################################################################
## femmecol: red-yellow-blue colors                                    ##
#########################################################################

femmecol <- function (n=100)                    # number of colors
{ 
  # red-green-blue colors on scale of 0 to 1
  rgb.col <- matrix(nrow=6,ncol=3,byrow=TRUE,
             data=c(0,0,143,
                    0,0,255,
                    0,255,255,
                    255,255,0,
                    255,0,0,
                    128,0,0))
  x.from <- c(0.0,seq(0.125,1,by=0.25),1)  # scale from 0-1
  return(intpalette(rgb.col,n,x.from=x.from) )

} 

#########################################################################
## greycol: black-white colors                                         ##
#########################################################################
greycol <- function (n=100,                 # number of colors
                     interval = c(0.0,0.7)) # interval *to* where to interpolate
{ 
  return(shadepalette(n=n,inicol="white",endcol="black",interval=interval))

} 

# alias for greycol...
graycol <- function (n=100,                 # number of colors
                     interval = c(0.0,0.7)) # interval *to* where to interpolate
  return(shadepalette(n=n,inicol="white",endcol="black",interval=interval))

#########################################################################
## drapecol: colors for draping persp (surface) plots                  ##
#########################################################################

drapecol <- function(A,                  # matrix with input grid
                     col=femmecol(100),  # color palette 
                     NAcol = "white")    # color of 'NA' elements
                     
# generates colors that can be draped on a persp plot
{
 nr <- nrow(A) ; nc <- ncol(A) ; ncol <- length(col)

# drape color matrix has one row and one column less than input matrix;
# take a weighted average
 AA <- 0.25*(A[1:(nr-1),1:(nc-1)]+A[1:(nr-1),2:nc]+A[2:nr,1:(nc-1)]+A[2:nr,2:nc])

 Ar <- range(AA, na.rm=TRUE)
 rn <- Ar[2]-Ar[1]

ifelse (rn != 0, drape <- col[1+trunc((AA-Ar[1])/rn*(ncol-1))] ,
                 drape <- rep(col[1],ncol) )
                 
drape [is.na(drape)] <- NAcol
return(drape)
} 



#**********************************************************************
#  ROTATIONS...
#**********************************************************************


#########################################################################
## rotatexy        : rotates xy values around midpoint                 ##
#########################################################################

rotatexy   <- function (xy,                # matrix with 2 columns, to be rotated
                        angle,             # angle of rotation, in degrees
                        mid=colMeans(xy),  # rotation point, default=centroid
                        asp=FALSE)         # if true: aspect ratio is kept
                        
{
xy    <- matrix(ncol=2,data=xy)
angpi <- angle / 180 *pi
cosa  <-cos(angpi)
sina  <-sin(angpi)

dx    <- xy[,1] - mid[1]
dy    <- xy[,2] - mid[2]

ex    <-mid[1] + cosa*dx-sina*dy
ey    <-mid[2] + sina*dx+cosa*dy

if (asp)
{
 user <- par("usr")
 pin  <- par("pin")
 sy   <- user[4]-user[3]
 sx   <- user[2]-user[1]
 ey   <- mid[2] + (ey -mid[2])*sy/sx*pin[1]/pin[2]
}

return(cbind(ex,ey))
}

#**********************************************************************
#  SHAPES
#**********************************************************************

##############################################################################
# roundrect: rectangular box with rounded left and right edges
##############################################################################

roundrect<- function(mid,radx,rady,rx=rady,dr=0.01,col="white",lcol="black",lwd=2,angle=0,...)
{
leftell  <- getellipse(ry=rady,rx=rx,mid=c(mid[1]-radx,mid[2]),dr=dr,from=pi/2,to=3/2*pi)
rightell <- getellipse(ry=rady,rx=rx,mid=c(mid[1]+radx,mid[2]),dr=dr,from=-pi/2,to=pi/2)
xyout<-rbind(leftell,c(mid[1]-radx,mid[2]-rady),c(mid[1]+radx,mid[2]-rady),
             rightell,c(mid[1]+radx,mid[2]+rady),c(mid[1]-radx,mid[2]+rady))
if (angle != 0) xyout <- rotatexy(xyout,angle=angle)               
filledshape(xyout,mid,col=col,lcol=lcol,lwd=lwd,...)
}

#########################################################################
## getellipse   : calculates the x-y values for (part of) an ellipse   ##
#########################################################################

getellipse <- function (rx=1,         # long radius of ellipse
                        ry=rx,        # short radius of ellipse
                        mid=c(0,0),   # midpoint of ellipse
                        dr=0.01,      # number of points to specify ellipse
                        angle=0,      # rotation angle, degrees
                        from=-pi,     # starting angle for ellipse segment, radians
                        to=pi)        # final angle for ellipse segment, radians

{
x  <- c(seq(from,to,by=dr),to)
if (x[length(x)]==x[length(x)-1])x<-x[-length(x)]
xy <- cbind(mid[1]+rx*cos(x), mid[2]+ry*sin(x))

if (angle != 0) xy <- rotatexy (xy,angle=angle,mid=mid)  # rotate around mid
return(xy)
}

#########################################################################
## plotellipse     : plots (part of) an ellipse                        ##
#########################################################################

plotellipse <- function (rx=1,             # long radius of ellipse
                         ry=0.2,           # short radius of ellipse
                         mid=c(0,0),       # midpoint of ellipse
                         dr=0.01,          # number of points to specify ellipse
                         angle=0,          # rotation angle, degrees
                         from=-pi,         # starting angle for ellipse segment, radians
                         to=pi,            # final angle for ellipse segment, radians
                         type="l",         # external line or points; "n" if no line
                         lwd=2,            # width of external line
                         lcol="black",     # line color
                         col=NULL,         # fill color
                         arrow=FALSE,      # drawing arrowhead yes/no
                         arr.length=0.4,    # size of arrow
                         arr.width=arr.length*0.5, # size of arrow
                         arr.type="curved", # type of arrow
                         arr.pos=1,         # position of arrow, 0=start,1=end
                         arr.code=2,        # integer code determining kind of arrows to draw
                         arr.adj=0.5,       # adjustment of arrow
                         arr.col="black",   # color of arrow head
                         ...)               # arguments passed to R-function "lines"


{
xy<-getellipse (rx,ry,mid,angle=angle,dr=dr,from=from,to=to)

if (! is.null(col)) polygon(xy,col=col,border=NA)
if (type != "n" )   lines(xy,type=type,lwd=lwd,col=lcol,...)
nr <- nrow(xy)

if (arrow) 
 {
  ilen <- length(arr.pos)
  if (ilen>1)
  {
  arr.code  <- rep(arr.code  ,len=ilen)
  arr.col   <- rep(arr.col   ,len=ilen)  
  arr.length<- rep(arr.length,len=ilen)
  arr.width <- rep(arr.width ,len=ilen)
  arr.type  <- rep(arr.type  ,len=ilen)
  arr.adj   <- rep(arr.adj   ,len=ilen)  
  }
  for (i in 1: ilen) 
  {ii <- max(2,trunc(nr*arr.pos[i]))
   Arrows(xy[ii-1,1],xy[ii-1,2],xy[ii,1],xy[ii,2],
     lcol=arr.col[i],code=arr.code[i],arr.col=arr.col[i],
     arr.length =arr.length[i],arr.width=arr.width[i],
     arr.type=arr.type[i],arr.adj=arr.adj[i])
     }
 }
}


#########################################################################
## plotcircle      : plots (part of) a circle                          ##
#########################################################################

plotcircle <- function (r=1,          # radius of circle
                        ...)          # arguments passed to function "plotellipse"
# see also: plotellipse
{
 user <- par("usr")
 pin  <- par("pin")
 sy   <- user[4]-user[3]
 sx   <- user[2]-user[1]
 ry   <- r*sy/sx*pin[1]/pin[2]

 plotellipse(rx=r,ry=ry,... )
}
#########################################################################
## cylindersegment      : plots (part of) a cylinder                   ##
#########################################################################


cylindersegment <- function (rx=1,          # horizontal radius of full cylinder
                            ry=rx,          # vertical radius  of full cylinder
                            from=pi,        # start radius of slice, radians 
                            to=3*pi/2,      # end radius of slice, radians
                            len=1,          # cylinder length
                            mid=c(0,0),     # midpoint of cylinder
                            angle=0,        # rotation angle, degrees 
                            dr=0.01,        # number of points to specify top/bottom ellipse
                            col="black",    # color of slice
                            delt =1.0,      # increase factor   
                            ...)            # arguments passed to polygon function
# internal function: creates a polygon as a slice of a cylinder and colors it
 
{
base1 <- mid ; base1[1]<-mid[1]-len/2  
base2 <- mid ; base2[1]<-mid[1]+len/2 

if (from >to)  { a<-from;from<-to;to<-a}

x <-c(seq(from,to,by=dr),to)
ex<- base1[1]+rx*cos(x)
ey<- base1[2]+ry*sin(x)
if (angle != 0) 
{xy <- rotatexy (cbind(ex,ey),angle=angle,mid=mid)  
 ex <- xy[,1];ey <- xy[,2] }


x<-rev(x)
x2 <-base2[1]+rx*cos(x)*delt
y2 <-base2[2]+ry*sin(x)*delt
if (angle != 0) 
{xy <- rotatexy (cbind(x2,y2),angle=angle,mid=mid)  
 x2 <- xy[,1];y2 <- xy[,2] }

ex<-c(ex,x2)
ey<-c(ey,y2)

polygon(ex,ey,col=col,border=col,...)

} # end cylindersegment

#########################################################################
## filledcylinder  : draws and colors cylinder colors depend on radius ##
#########################################################################

filledcylinder <- function(rx=1,                 # horizontal radius
                           ry=rx,                # vertical radius
                           len=1,                # length
                           col=femmecol(100),    # color palette to be used; also allowed are two extremes
                           lcol=NA,              # line color on external surface
                           lwd=2,                # only if lcol!=NA, width of external line
                           lcolint=NULL,         # only if lcol!=NA, line color on internal surface
                           ltyint=1,             # only if lcol!=NA, line type on internal surface
                           lwdint=lwd,           # only if dlcol!=NA, line width on internal surface
                           mid=c(0,0),           # midpoint of cylinder
                           angle=0,              # rotation angle, degrees
                           delt =1.0,            # increase factor
                           dr=0.01,              # number of points to specify top/bottom ellipse
                           topcol=NULL,          # color (palette) of top surface
                           botcol=NULL,          # color (palette) of bottom surface
                           ...)                  # arguments passed to function "filledellipse"


{

rx       <- max(1e-6,rx)  # avoid NANs
ncol     <- length (col)
if (ncol > 0)
{
#intrad   <- seq(0,pi,length.out=ncol+1)      # add point: ncol+1 points
intrad   <- seq(pi/2,3*pi/2,length.out=ncol+1)      # add point: ncol+1 points
Col      <- col

nval <- ncol

# main body of cylinder
for (i in 1:nval)
{
from<-intrad[i+1]
to<-intrad  [i]

 cylindersegment (rx=rx,ry=ry,from=from,to=to, len=len,
                  mid=mid,dr=dr,angle=angle,
                  col=Col[i],delt=delt)

}
}
base2 <- mid +c(len/2,0)
if (angle != 0) base2 <- rotatexy(base2,angle=angle,mid=mid)

base1 <-mid+c(-len/2,0)
if (angle != 0) base1 <- rotatexy(base1,angle=angle,mid=mid)

# color of top
if(! is.null (topcol))
  filledellipse(rx1=rx*delt,ry1=ry*delt,col=topcol,mid=base2,angle=angle,dr=dr,...)

if (! is.null(botcol))
  filledellipse(rx1=rx,ry1=ry,col=botcol,mid=base1,angle=angle,dr=dr,...)


if (! is.na(lcol))
{
  l1 <- rotatexy(getellipse( rx,ry,mid=mid+c(-len/2,0),dr=dr,from=pi/2,to=3*pi/2),angle=angle,mid)
  l2 <- rotatexy(getellipse( rx*delt,ry*delt,mid=mid+c(len/2,0),dr=dr),angle=angle,mid)

  lines(l1,col=lcol,lwd=lwd)
  lines(l2,col=lcol,lwd=lwd)
  if(! is.null(lcolint))         # line color, internal surface
  {
    l1 <- rotatexy(getellipse( rx,ry,mid=mid+c(-len/2,0),dr=dr,from=-pi/2,to=pi/2),angle=angle,mid)
    lines(l1,col=lcolint,lwd=lwdint,lty=ltyint)
  }

  l1 <- rotatexy(rbind(mid+c(len/2,ry*delt),mid+c(-len/2,ry)),angle,mid)
  l2 <- rotatexy(rbind(mid+c(len/2,-ry*delt),mid+c(-len/2,-ry)),angle,mid)
  lines(l1,col=lcol,lwd=lwd)
  lines(l2,col=lcol,lwd=lwd)
}

}

#########################################################################
## filledshape     : draws and colors a shape   color depend on radius ##
#########################################################################

filledshape <- function(xyouter,                    # 2-column matrix with x,y values of outer shape 
                        xyinner=colMeans(xyouter),  # 2-column matrix or vector with x,y values of inner shape 
                        col=femmecol(100),    # color palette to be used; also allowed are two extremes
                        values=NULL, # determines radius-z interpolation
                        zlim=NULL,   # range of z-values, if values not NULL
                        lcol=NA,              # line color
                        lwd=2,                # width of external line, only if lcol!=NA
                        ...)                  # arguments passed to R-function "polygon"

{

expand<-function(mat,npoints)
{
 mat   <- matrix(nc=2,mat)
 nfrom <- nrow(mat)
 nin   <- round(npoints/nfrom)
 nin   <- rep(nin,nfrom-1)
 nin   <- c(nin,npoints-sum(nin))
 mat   <-rbind(mat,mat[1,])
 out   <- NULL
 for (i in 1:nfrom)
 {
  x  <- approx(x=mat[c(i,i+1),1],n=nin[i])$y
  y  <- approx(x=mat[c(i,i+1),2],n=nin[i])$y
  out <- rbind(out,cbind(x,y) )
 }
 out
}

vv     <- val2col(values,zlim,col)
intrad <- vv$intrad
Col    <- vv$Col
nrad   <- vv$nrad

# check inner and outer points
npoint <- nrow(xyouter)
nmid   <- length(xyinner)/2
middle <- xyinner
extern <- xyouter

if ( nrad == 1 & nmid == 1) 
{
 polygon(xyouter[,1],xyouter[,2],col=Col,border=Col,...)
} else if (nrad==1)
{
 if (nmid < npoint) for (i in (nmid+1):npoint) middle <- rbind(middle ,xyinner)
 polygon(c(xyouter[,1],rev(middle[,1])),c(xyouter[,2],rev(middle[,2])),col=Col,border=Col,...)
 
}else {
if (nmid < npoint) middle <- expand(middle,npoint)
if (nmid > npoint) extern <- expand(extern,nmid  )
#
# start coloration
inner  <- middle 

for (i in 1:nrad)
{
relrad    <- intrad[i+1] 
outer     <- inner
inner[,1] <- middle [,1] + relrad * (extern[,1]-middle[,1])
inner[,2] <- middle [,2] + relrad * (extern[,2]-middle[,2])
polygon(c(outer[,1],rev(inner[,1])),c(outer[,2],rev(inner[,2])),col=Col[i],border=Col[i],...)
}
}
if (! is.na(lcol))
{
 lines(xyouter,lwd=lwd,col=lcol)
 if (length(xyinner)>2)lines(xyinner,lwd=lwd,col=lcol)
 
}
} # of filledshape


#########################################################################
## filledellipse   : draws and colors ellipse; colors depend on radius ##
#########################################################################

filledellipse <- function(rx1=1,       # long radius of outer ellipse
                          rx2=0,       # long radius of inner ellipse
                          ry1=rx1,     # short radius of outer ellipse
                          ry2=NULL,    # long radius of inner ellipse 
                          mid=c(0,0),  # midpoint of ellipse
                          dr=0.01,     # number of points to specify ellipse
                          angle=0,     # rotation angle, degrees
                          from=-pi,    # starting angle for ellipse segment, radians
                          to=pi,       # final angle for ellipse segment, radians
                          col=femmecol(100), # color palette to be used; also allowed are two extremes
                          values=NULL, # determines radius-z interpolation
                          zlim=NULL,   # range of z-values, if values not NULL
                          lwd=2,                # width of external line
                          lcol=NA,              # line color
                          ...)                  # arguments passed to R-function "polygon"

{

ncol   <- length (col)
if (is.null (ry2)) ry2 <- ry1*rx2/rx1   # same proportionality as long radius

# outer ellipse
xyouter   <- getellipse(rx=rx1,ry=ry1,mid=mid,dr=dr,angle=angle,from=from,to=to)

# inner ellipse
if (rx2==0) xyinner <- mid else {
  xyinner <- getellipse(rx=rx2,ry=ry2,mid=mid,dr=dr,angle=angle,from=from,to=to)
  }
filledshape(xyouter=xyouter,xyinner=xyinner,col=col,values=values,zlim=zlim,
            lwd=lwd,lcol=lcol,...)

}

#########################################################################
## filledcircle    : draws and colors circle; colors depend on radius  ##
#########################################################################

filledcircle <- function(r1=1,              # radius of outer circle
                         r2=0,              # radius of inner circle
                         mid=c(0,0),        # midpoint of circle
                         dr=0.01,           # number of points to specify circle
                         from=-pi,          # starting angle for circle segment, radians
                         to=pi,             # final angle for circle segment, radians
                         col=femmecol(100), # color palette to be used; also allowed are two extremes or one value
                          values=NULL, # determines radius-z interpolation
                          zlim=NULL,   # range of z-values, if values not NULL
                         lwd=2,             # width of external line
                         lcol=NA,           # line color
                         ...)               # arguments passed to R-function "polygon"
{
 user <- par("usr")             # to maintain the aspect ratio...
 pin  <- par("pin")
 sy   <- user[4]-user[3]
 sx   <- user[2]-user[1]
 ry1  <- r1*sy/sx*pin[1]/pin[2]
 ry2  <- r2*sy/sx*pin[1]/pin[2]
 filledellipse(rx1=r1,ry1=ry1,rx2=r2,ry2=ry2,mid=mid,dr=dr,from=from,to=to,
               col=col,lwd=lwd,lcol=lcol,values=values,zlim=zlim,...)

}

#########################################################################
## filledrectangle : draws and colors rectangle color can be palette   ##
#########################################################################

filledrectangle <- function(mid=c(0,0),        # midpoint of rectangle
                            wx=1,              # horizontal width 
                            wy=wx,             # vertical width 
                            col=femmecol(100), # color palette to be used; also allowed are two extremes or one value
                            values=NULL, # determines radius-z interpolation
                            zlim=NULL,   # range of z-values, if values not NULL

                            lwd=2,             # width of external line
                            lcol=NA,           # line color
                            angle = 0,         # angle of rotation, in degrees
                            ...)               # arguments passed to R-function "polygon"
{

vv     <- val2col(values,zlim,col)
intrad <- vv$intrad
Col    <- vv$Col

ncol   <- length (Col)

for (i in 1:ncol)
{
x1 <- mid[1] - wx/2  
x2 <- mid[1] + wx/2  
 
y1 <- mid[2] - wy/2  +wy*intrad[i]
y2 <- mid[2] - wy/2  +wy*intrad[i+1]
xy <- cbind(c(x1,x1,x2,x2),c(y1,y2,y2,y1))

if (angle != 0) xy <- rotatexy (xy,angle=angle,mid=mid) # rotate around mid
polygon(xy[,1],xy[,2],col=Col[i],border=Col[i])

}
if (!is.na(lcol)) 
{
x1 <- mid[1] - wx/2  
x2 <- mid[1] + wx/2  
 
y1 <- mid[2] - wy/2 
y2 <- mid[2] + wy/2 
xy <- cbind(c(x1,x1,x2,x2),c(y1,y2,y2,y1))

if (angle != 0) xy <- rotatexy (xy,angle=angle,mid=mid) 

polygon(xy[,1],xy[,2],border=lcol,col=NA)
}
}

#########################################################################
## filledmultigonal : draws and colors shape with equal-sized vertices color can be palette   ##
#########################################################################

filledmultigonal <- function(mid=c(0,0),        # midpoint of rectangle
                            rx=1,               # horizontal radius 
                            ry=rx,               # vertical radius 
                            nr=4,               # number of sides
                            col=femmecol(100),  # color palette to be used; also allowed are two extremes or one value
                          values=NULL, # determines radius-z interpolation
                          zlim=NULL,   # range of z-values, if values not NULL
                            lwd=2,              # width of external line
                            lcol=NA,            # line color
                            angle = 0,          # angle of rotation, in degrees
                            ...)                # arguments passed to R-function "polygon"
{

xy <- getellipse(mid=mid,rx=rx,ry=ry,dr=2*pi/nr,from=0,to=2*pi)
if (angle != 0) xy <- rotatexy (xy,angle=angle,mid=mid) # rotate around mid
filledshape(xyouter=xy,xyinner=mid,col=col,values=values,zlim=zlim,lwd=lwd,lcol=lcol,...) 

}

#########################################################################
## Arrowhead    : draws arrowhead, various shapes                      ##
#########################################################################

Arrowhead <- function(x0, y0,                # coordinates of points at which to draw arrowhead
                      angle=0,               # angle of arrowhead (anti-clockwise, relative to x-axis), in degrees
                      arr.length=0.4,        # approximate length of arrowhead, in cm
                      arr.width=arr.length/2,# approximate width of arrowhead, in cm
                      arr.adj=0.5,           # 0,0.5,1 specifying the adjustment of the arrowhead
                      arr.type="curved",     # type of arrowhead to draw
                      lcol="black",lty=1,    # line specifications
                      arr.col=lcol,          # color of arrowhead
                      npoint=5)              # if curved: number of points

{

# points of polygon, as drawn in graph with x- and y- ranges -5,5
  if ( arr.type=="curved")  # composed as section of circels
  {
  rad <- 0.7                                        # radius of outer circles
  len <- 0.25*pi             
  mid <- c(0,rad)
  
  x   <- seq(1.5*pi+len,1.5*pi,length.out=npoint)
  rr  <- cbind(mid[1]-rad*cos(x),mid[2]+rad*sin(x)) #part of circle
  mid <- c(0,-rad)
  x   <- rev(x)
  rr  <- rbind(rr,cbind(mid[1]-rad*cos(x),mid[2]-rad*sin(x)))
  mid <-c(rr[nrow(rr),1],0)
  rd  <-rr[1,2]
  x   <-seq(pi/2,3*pi/2,length.out=3*npoint)        #part of ellipse
  rr <- rbind(rr,cbind(mid[1]-rd*0.25*cos(x),mid[2]-rd*sin(x)))
  rr[,1] <- rr[,1]*2.6
  rr[,2] <- rr[,2]*3.45
   } else if (arr.type=="triangle")
  {               # triangle
  x   <- c(-0.2,0.0,-0.2)
  y   <- c(-0.1,0.0,0.1)
  rr  <- 6.22*cbind(x,y)
  }  else if (arr.type%in%c("circle","ellipse") )

  {    
  if (arr.type=="circle") arr.width=arr.length
  rad <- 0.1                # radius of circle 
  mid <- c(-rad,0)
  x<- seq(0,2*pi,length.out=15*npoint)
  rr <- 6.22*cbind(mid[1]+rad*sin(x),mid[2]+rad*cos(x))
 
  }
  
  if(arr.adj == 0.5) rr[,1] <- rr[,1]-min(rr[,1])/2
  if(arr.adj == 0)   rr[,1] <- rr[,1]-min(rr[,1]) 


  user <- par("usr")
  pcm  <- par("pin")*2.54

  sy<- (user[4]-user[3])/pcm[2] 
  sx<- (user[2]-user[1])/pcm[1] 
  nr <- max(length(x0),length(y0),length(angle),
            length(arr.length),length(arr.width),
            length(lcol),length(lty),length(arr.col))
  if (nr>1) 
  {
  x0         <- rep(x0        ,len=nr)
  y0         <- rep(y0        ,len=nr)    
  angle      <- rep(angle     ,len=nr)
  arr.length <- rep(arr.length,len=nr)
  arr.width  <- rep(arr.width,len=nr)
  lcol       <- rep(lcol      ,len=nr)
  lty        <- rep(lty       ,len=nr)
  arr.col    <- rep(arr.col   ,len=nr)          
  }
  RR<-rr
  for (i in 1:nr)
  {
# rotation around midpoint  
  dx <- rr[,1]*arr.length [i]
  dy <- rr[,2]*arr.width  [i]
    
# rotating 
  angpi <- angle[i] / 180 *pi 
  cosa  <-cos(angpi)
  sina  <-sin(angpi)

  RR[,1]<-  cosa*dx-sina*dy
  RR[,2]<-  sina*dx+cosa*dy

# rescaling and transposing 
  RR[,1]<- x0[i] +RR[,1]*sx 
  RR[,2]<- y0[i] +RR[,2]*sy 

# drawing...
  polygon(RR,col=arr.col[i],border=lcol[i],lty=lty[i])
  }
 }

#########################################################################
## Arrows       : draws arrow with improved arrowhead                  ##
#########################################################################

 Arrows <- function(x0,y0,        # coordinates of points *from* which to draw arrow
                    x1,y1,        # coordinates of points *to* which to draw arrow
                    code=2,       # integer code determining kind of arrows to draw
                    arr.length=0.4,         # approximate length of arrowhead, in cm
                    arr.width=arr.length/2, # approximate width of arrowhead, in cm
                    arr.adj=0.5,       #  0,0.5,1 specifying the adjustment of the arrowhead
                    arr.type="curved", # type of arrowhead to draw
                    segment=TRUE,      # logical specifying whether to draw line segments
                    lcol="black",lty=1, # line specifications
                    arr.col=lcol,       # color of arrowhead
                    ...)               # arguments passed to lines or segments function

 {
  if (arr.type=="simple") {
    arrows(x0,y0,x1,y1,code=code,length=arr.length/2.54,col=arr.col,lty=lty,...)
    return()
    }

  # draw segment
  if (segment) segments(x0,y0,x1,y1,col=lcol,lty=lty,...)    

  # scaling factor
  user<-par("usr")
  pin <-par("pin")
  pin <- pin/max(pin)
  sy<- (user[4]-user[3]) /pin[2]
  sx<- (user[2]-user[1]) /pin[1]

  # code = 2
  angle<- atan((y1-y0) /(x1-x0) *sx/sy)/pi*180
  angle[is.nan(angle)]<-0
  angle [x1<x0] <-180+angle[x1<x0]
  xx<-x1
  yy<-y1

  # code =3 draws two arrowheads
  if (code == 3) Arrowhead(x0=xx,y0=yy,angle=angle,
                           lcol=lcol,arr.col=arr.col,arr.adj=arr.adj,
                           lty=lty,arr.length=arr.length,arr.width=arr.width,
                           arr.type=arr.type)
 
  if (code != 2)   # code == 1
  { angle <-180 + angle
    xx<-x0 ; yy<-y0} 

  Arrowhead(x0=xx,y0=yy,angle=angle,lcol=lcol,arr.col=arr.col,
            arr.adj=arr.adj,lty=lty,arr.length=arr.length,
            arr.width=arr.width,arr.type=arr.type)
 }


#########################################################################
## colorlegend  : adds a color legend to a plot                        ##
#########################################################################

colorlegend <- function(col=femmecol(100),   # color palette to be used; also allowed are two extremes or one value
                        zlim,                # two-valued vector,  the minimum and maximum z values 
                        zlevels=5,           # number of z-levels, one value, ignored if "dz" or "zval" not equal to NULL
                        dz=NULL,             # increment in legend values, one value; ignored if "zval" not equal to NULL
                        zval=NULL,           # a vector of z-values to label legend
                        log=FALSE,           # logical indicating whether to log transform or not
                        posx=c(0.9,0.93),    # relative position of left and right edge of color bar on first axis, [0,1]
                        posy=c(0.05,0.9),    # relative position on lower and upper edge of colar bar on second axis, [0,1]
                        main=NULL,           # main title, written above the color bar
                        main.cex=1.0,        # relative size of main title
                        main.col="black",    # color of main title
                        lab.col="black",     # color of labels
                        digit=0,             # number of significant digits in labels
                        left=FALSE,          # logical indicating whether to put the labels on right (TRUE) or on the left (FALSE)
                        ...)                 # arguments passed to R-function "text" when writing labels

{
ncol   <- length (col)
par (new=TRUE)
omar <- nmar <- par("mar")
nmar[c(2,4)]<-0
par (mar = nmar) 

emptyplot()
pars   <- par("usr")
 
# Rectangle positions on x and y-axis
dx     <- pars[2]-pars[1]
xmin   <- pars[1]+posx[1]*dx
xmax   <- pars[1]+posx[2]*dx

dy     <- pars[4]-pars[3]
ymin   <- pars[3]+posy[1]*dy
ymax   <- pars[3]+posy[2]*dy

# z-values
if (!is.null(zval)) {zz<-zval;dz<-NULL}

if (is.null(dz)&is.null(zval))
if (! is.null(zlevels))
{ if (log) { zz <- 10^(pretty(log10(zlim),n=(zlevels+1)))
      } else zz <-     pretty(zlim,n=(zlevels+1))
} else zz <- NULL
if (!is.null(dz)) 
{
if (log) zz <- 10^(seq(log10(zlim[1]),log10(zlim[2]),by=dz))
if (!log)zz <- seq(zlim[1],zlim[2],by=dz)
}

if (log)
{
zlim <- log10(zlim)
if (! is.null(zz)) zz   <- log10(zz)
}

zmin   <- zlim[1]
zmax   <- zlim[2]

# colors
Y <- seq(ymin,ymax,len=ncol+1)
rect(xmin,Y[-(ncol+1)],xmax,Y[-1],col=col,border=NA)
rect(xmin,ymin,xmax,ymax,border=lab.col)

if (! is.null(zz)) 
{
# labels
dx     <- (xmax-xmin)
dy     <- (ymax-ymin)

if (left)
{
Dx  <-  -dx  # labels on left..
pos <-   2
xpos <- xmin+Dx*0.5
} else {
Dx  <- +dx  # labels on right..
pos <- 4
xpos <- xmax+Dx*0.5
}

Ypos <- ymin+(zz-zmin)/(zmax-zmin)*dy
segments(xmin,Ypos,xmax,Ypos,col=lab.col)  
segments(xpos+Dx*0.25,Ypos,xmin,Ypos,col=lab.col)
text (xpos,Ypos,formatC(zz,digits=digit,format="f"),pos=pos,col=lab.col,...)
}
 
if  (!is.null(main)) 
{
for (i in length(main):1)
 text (x=mean(c(xmin,xmax)),y=ymax+0.05*(length(main)-i+1),labels=main[i],
 adj=c(0.5,0.5),cex=main.cex,col=main.col)
}
par (new=FALSE)
par (mar=omar)

}  ########## END OF colorlegend ########## 
