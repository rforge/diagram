################################################################################
##                                                                            ##
## Plot diagrams and transition matrices                                      ##
##    Karline Soetaert                                                        ##
##                                                                            ##
##                                                                            ##
################################################################################


##############################################################################
# openplotmat: opens a plot, ready to plot diagrams or transition matrices
##############################################################################
openplotmat <- function(asp=NA,...) 
emptyplot(asp=asp,...)


##############################################################################
# straightarrow: Plot straight arrow at certain distance between two points
##############################################################################

straightarrow <- function(from,     # coordinates (x,y) of points *from* which to draw arrow
                    to,             # coordinates (x,y) of points *to* which to draw arrow
                    lwd=2,          # line width
                    lty=1,          # line type
                    lcol="black",   # line color
                    arr.pos=0.5,    # relative position of arrowhead 
                    endhead=FALSE,  # if true: line stops at arrowhead
                    ...)            # arguments passed to function Arrows
  {
   ifelse (endhead, To <- arr.pos*to+(1-arr.pos)*from, To<- to)

   segments(from[1],from[2],To[1],To[2],lwd=lwd,lty=lty,col=lcol)
   mid2  <- c((arr.pos-0.01)*to+(1-arr.pos+0.01)*from)
   mid1  <- c((arr.pos     )*to+(1-arr.pos     )*from)
   Arrows(mid2[1],mid2[2],mid1[1],mid1[2],lcol=lcol,...) 
   straightarrow <-mid1      # coordinates (x,y) where arrowhead is drawn
  }

##############################################################################
# curvedarrow: Plot curved arrow at certain distance between two points
##############################################################################

curvedarrow <- function(from,           # coordinates (x,y) of points *from* which to draw arrow
                        to,             # coordinates (x,y) of points *to* which to draw arrow
                        lwd=2,          # line width
                        lty=1,          # line type
                        lcol="black",   # line color
                        arr.pos=0.5,    # relative position of arrowhead
                        curve=1,        # relative size of curve (fraction of arrow length)
                        dr=0.01,        # number of points to specify ellipse
                        endhead=FALSE,  # if true: line stops at arrowhead
                        ...)            # arguments passed to function Arrows
  {

    dpos  <- to-from
    angle <- atan(dpos[2]/dpos[1])*180/pi         # angle between both
    if (is.nan(angle)) return
    mid   <- 0.5*(to+from)                        # midpoint of ellipsoid arrow
    dst   <- dist(rbind(to,from))                 # distance from-to
    ry    <- curve*dst                            # small radius of ellepsoid
    aFrom<-0;aTo<-pi                              # angle to and from
    if (from[1]<=to[1]) {aFrom<-pi;aTo<-2*pi}
    meanpi <- arr.pos*aTo+(1-arr.pos)*aFrom       # mean(c(from,to))
    ifelse(endhead,To <- meanpi, To<-aTo)
    plotellipse(rx=dst/2,ry=ry,mid=mid,angle=angle,from=aFrom,to=To,
               lwd=lwd,lty=lty,lcol=lcol)
    ell <- getellipse(rx=dst/2,ry=ry,mid=mid,angle=angle,
                      from=1.001*meanpi,to=0.999*meanpi,dr=-0.002)
    Arrows(ell[1,1],ell[1,2],ell[nrow(ell),1],ell[nrow(ell),2],code=1,lcol=lcol,...)
    curvedarrow <- c(ell[nrow(ell),1],ell[nrow(ell),2]) # coordinates (x,y) where arrowhead is drawn
  }

  
##############################################################################
# selfarrow: Plot circular arrow pointing at one point
##############################################################################

selfarrow <- function(pos,           # coordinates (x,y) of points *from and to* which to draw arrow
                      lwd=2,          # line width
                      lty=1,          # line type
                      lcol="black",   # line color
                      arr.pos=0.5,    # relative position of arrowhead
                      path="L",      # position of circle: R, L, U, D for right, left, up and down respectively
                      curve=c(0.1,0.1),   # relative size of curve (fraction of arrow length)
                      dr=0.01,        # number of points to specify ellipse
                      code=1,         # how to put the arrowhead 
                      ...)            # arguments passed to function Arrows
  {
    if (length(curve)==1) curve <- c(curve,curve)
    mid <- pos
   if (path == "L") mid[1] <- mid[1] -curve[1]  # left shift
   if (path == "R") mid[1] <- mid[1] +curve[1]  # right shift
   if (path == "U") mid[2] <- mid[2] +curve[2]  # up shift
   if (path == "D") mid[2] <- mid[2] -curve[2]  # down shift

    aFrom<-0;aTo<-2*pi                            # angle to and from
    meanpi <- arr.pos*aTo+(1-arr.pos)*aFrom       # pos of arrow
    plotellipse(rx=curve[1],ry=curve[2],mid=mid,from=aFrom,to=aTo,
               lwd=lwd,lty=lty,lcol=lcol)
    ell <- getellipse(rx=curve[1],ry=curve[2],mid=mid,
                      from=1.001*meanpi,to=0.999*meanpi,dr=-0.002)
    Arrows(ell[1,1],ell[1,2],ell[nrow(ell),1],ell[nrow(ell),2],code=code,lcol=lcol,...)
    selfarrow <- c(ell[nrow(ell),1],ell[nrow(ell),2])
  }

##############################################################################
# segmentarrorw: 3 segmented arrow between two points (left-vertical-right)
##############################################################################

segmentarrow <- function(from,          # coordinates (x,y) of points *from* which to draw arrow
                         to,            # coordinates (x,y) of points *to* which to draw arrow
                         lwd=2,          # line width
                         lty=1,          # line type
                         lcol="black",   # line color
                         arr.side=2,    # segment number on which arrowhead is drawn (1,2,3)
                         arr.pos=0.5,   # relative position of arrowhead on segment on which arrowhead is drawn
                         path = "LVR",  # outline of the 3 segments, default: left, vertical, right
                         dd=0.5,        # length of segment arm, directed away from endpoints
                         ...)           # arguments passed to function straightarrow


{ 
  sarr <- function(p1,p2,drawarr)
   {
   if (drawarr) m1<<-rbind(m1,straightarrow (from=p1,to=p2,arr.pos=arr.pos,lwd=lwd,
                                    lty=lty,lcol=lcol,...)) else 
   segments(p1[1],p1[2],p2[1],p2[2],lwd=lwd,lty=lty,col=lcol)                                    
   }
  m1 <- NULL
  if (is.null(path)) path <- ifelse (from[1]==to[1],"LVR","UHD")
  if (path == "LVR") {dx <- -dd; dy<-0}     # left vertical right
  if (path == "RVL") {dx <-  dd; dy<-0}     # right vertical left
  if (path == "UHD") {dx <- 0  ; dy<-dd}    # up horizontal down
  if (path == "DHU") {dx <- 0  ; dy<--dd}   # down horizontal up
  if (path %in% c("LVR","RVL"))
   {
   sarr(  from,                  c(from[1]+dx,from[2]   ),1 %in% arr.side)
   sarr(c(from[1]+dx,from[2]   ),c(from[1]+dx,  to[2]+dy),2 %in% arr.side)
   sarr(c(from[1]+dx,  to[2]+dy),to                      ,3 %in% arr.side) 
   } else {
   sarr(  from,                  c(from[1]   ,from[2]+dy),1 %in% arr.side)
   sarr(c(from[1]   ,from[2]+dy),c(to[1]     ,from[2]+dy),2 %in% arr.side)
   sarr(c(to[1]     ,from[2]+dy), to                     ,3 %in% arr.side) 
      }
  segmentarrow <- m1                   # coordinates (x,y) where arrowhead is drawn
}
##############################################################################
# bentarrorw: 2- segmented arrow between two points 
##############################################################################

bentarrow <- function(from,             # coordinates (x,y) of point *from* which to draw arrow
                      to,               # coordinates (x,y) of point *to* which to draw arrow
                      lwd=2,            # line width
                      lty=1,            # line type
                      lcol="black",     # line color
                      arr.side=2,       # segment number on which arrowhead is drawn (1,2)
                      arr.pos=0.5,      # relative position of arrowhead on segment on which arrowhead is drawn
                      path = "H",       # Vertical, Horizontal
                      ...)              # other arguments passed to function straightarrow

{ 
  sarr <- function(p1,p2,drawarr)
   {

   if (drawarr) m1<<-rbind(m1,straightarrow (from=p1,to=p2,arr.pos=arr.pos,lwd=lwd,
                                    lty=lty,lcol=lcol,...))  else
   segments(p1[1],p1[2],p2[1],p2[2],lwd=lwd,lty=lty,col=lcol)  
    }
  
  m1    <- NULL
   
# horizontal
  if (path == "H")
  {
   sarr(  from,                  c(to[1],from[2]   ),1 %in% arr.side)
   sarr(  c(to[1],from[2]   ),   to                 ,2 %in% arr.side)  
  } else {
   sarr(  from,                  c(from[1],to[2]   ),1 %in% arr.side)
   sarr(  c(from[1],to[2]   ),   to                 ,2 %in% arr.side)  
  }
  bentarrow <- m1                  # coordinates (x,y) where arrowhead is drawn
}

##############################################################################
# treearrow: segmented arrow between several points 
##############################################################################

treearrow <- function(from,             # matrix of coordinates (x,y) of points *from* which to draw arrow
                      to,               # matrix of coordinates (x,y) of points *to* which to draw arrow
                      lwd=2,            # line width
                      lty=1,            # line type
                      lcol="black",     # line color
                      arr.side=2,       # segment number on which arrowhead is drawn
                      arr.pos=0.5,      # relative position of arrowhead on segment on which arrowhead is drawn
                      line.pos=0.5,     # relative position of (horizontal/vertical) line
                      path = "H",       # Vertical, Horizontal
                      ...)              # other arguments passed to function straightarrow

{ 
  sarr <- function(p1,p2,drawarr)
   {
   if (drawarr) m1<<-rbind(m1,straightarrow (from=p1,to=p2,arr.pos=arr.pos,lwd=lwd,
                                    lty=lty,lcol=lcol,...)) else 
   segments(p1[1],p1[2],p2[1],p2[2],lwd=lwd,lty=lty,col=lcol)                                    
   }
  
  From <- matrix(ncol=2,data=from)
  To   <- matrix(ncol=2,data=to  )
  m1   <- NULL
  ifelse (path == "H", ii <- 2,ii<-1)
  rF   <- range(From[,ii]) ; rT <- range(To[,ii])
  ifelse (abs(min(rF)-max(rT)) <= abs(max(rF)-min(rT)),
          m2<-min(rF)+line.pos*(max(rT)-min(rF)),m2<-max(rF)+line.pos*(max(rF)-min(rT)))
  
# horizontal
  if (path == "H")
  {
  Line <- range(c(From[,1],To[,1])) 
  segments(Line[1],m2,Line[2],m2,lwd=lwd,lty=lty,col=lcol)       # horizontal line
  for (i in 1:nrow(From)) sarr(From[i,], c(From[i,1],m2),1 %in% arr.side)
  for (i in 1:nrow(To)  ) sarr(c(To  [i,1],m2),To[i,]   ,2 %in% arr.side)

  } else { # vertical
  Line <- range(c(From[,2],To[,2]))
  segments(m2,Line[1],m2,Line[2],lwd=lwd,lty=lty,col=lcol)
  for (i in 1:nrow(From)) sarr(From[i,], c(m2,From[i,2]),1 %in% arr.side)
  for (i in 1:nrow(To)  ) sarr(c(m2,To  [i,2]),To[i,]   ,2 %in% arr.side)
  }
  treearrow <- m1                  # coordinates (x,y) where arrowhead is drawn
}

##############################################################################
# splitarrow: segmented arrow between several points 
##############################################################################


splitarrow <- function(from,          # coordinates (x,y) of points *from* which to draw arrow
                       to,            # coordinates (x,y) of points *to* which to draw arrow
                       lwd=2,         # line width
                       lty=1,         # line type
                       lcol="black",  # line color
                       arr.side=2,    # segment number on which arrowhead is drawn (1,2)
                       arr.pos=0.5,   # relative position of arrowhead on segment on which arrowhead is drawn
                       centre=NULL,   # point from where radiation occurs
                       dd=0.5,        # only when centre == NULL: length of segment arm, directed away from start point
                       ...)           # arguments passed to function straightarrow


{
  sarr <- function(p1,p2,drawarr)
   {
   if (drawarr) m1<<-rbind(m1,straightarrow (from=p1,to=p2,arr.pos=arr.pos,lwd=lwd,
                                    lty=lty,lcol=lcol,...)) else
   segments(p1[1],p1[2],p2[1],p2[2],lwd=lwd,lty=lty,col=lcol)                                    
   }
  m1   <- NULL
  From <- matrix(ncol=2,data=from)
  To   <- matrix(ncol=2,data=to  )

  meanFrom <- colMeans(From)
  meanTo   <- colMeans(To)
  if (is.null(centre)) centre <- meanFrom+ dd*(meanTo-meanFrom)
  for (i in 1:nrow(From)) sarr(From[i,], centre,1 %in% arr.side)
  for (i in 1:nrow(To))   sarr(centre,To[i,]   ,2 %in% arr.side)
  splitarrow <- m1                   # coordinates (x,y) where arrowhead is drawn
}


##############################################################################
# textplain: One or several lines of text, no box  
##############################################################################

textplain <- function (mid,        # central coordinates where to write the text
                       height=0.1, # height of text 
                       lab="",     # one or more character strings or expressions specifying the *text* to be written.   
                       adj=c(0.5,0.5), # label adjustments; 
                       ...)

{
  if (length (lab) == 1) text(mid[1], mid[2],lab,adj=adj,...)
  else 
  {
   y1      <- mid[2]+height
   ddy     <- 2*height/(length(lab)+1)
   for (i in 1:length(lab)) text(mid[1], y1-ddy*i,lab[i],adj=adj,...)
  }
}


##############################################################################
# shadowbox: Boxes with shadow
##############################################################################

shadowbox <- function (box.type="rect",
                       mid,
                       radx,
                       rady=radx,
                       shadow.size=0.01,
                       shadow.col="grey",
                       box.col="white",
                       lcol="black",
                       lwd=1,
                       dr=0.01,
                       angle=0,
                       len=1,          # if box.type="cylinder"    
                       nr=5,           # if box.type="multiangle"
                       rx=rady,        # if box.type="round"
                       ...)
{
  pin   <- par ("pin")                 # size of plotting region, inches 
  dd    <- c(shadow.size,-shadow.size*pin[1]/pin[2])     # scaled size of shadow

  if (box.type %in% c("rect","square"))           {
    xy     <- cbind(c(mid[1]-radx,mid[1]-radx,mid[1]+radx,mid[1]+radx),
                    c(mid[2]-rady,mid[2]+rady,mid[2]+rady,mid[2]-rady))
    xyshad <- xy + matrix(nr=4,nc=2,byrow=TRUE,data=c(dd[1],dd[2]))
    if (angle != 0) {xy <- rotatexy  (xy,angle);xyshad <-rotatexy(xyshad,angle)}
    if (shadow.size>0) polygon(xyshad[,1],xyshad[,2],border=NA,col=shadow.col)
    polygon(xy[,1],xy[,2],lwd=lwd,col=box.col,border=lcol,...)
  } else if (box.type %in% c("ellipse","circle")) {
    if (shadow.size>0) filledellipse(mid=mid+dd,rx1=radx,ry1=rady,col=shadow.col,dr=dr,angle=angle)
    filledellipse(mid=mid,rx1=radx,ry1=rady,col=box.col,dr=dr,lwd=lwd,lcol=lcol,angle=angle,...)
  } else if (box.type == "round") {
    if (shadow.size>0) 
    roundrect(mid+dd,radx,rady,col=shadow.col,lcol=NA,dr=dr,rx=rx,angle=angle)
    roundrect(mid,radx,rady,lwd=lwd,col=box.col,lcol=lcol,rx=rx,angle=angle,...)
  } else if (box.type =="diamond")                {
    xx    <- c(mid[1]-radx,mid[1],mid[1]+radx,mid[1])
    yy    <- c(mid[2],mid[2]+rady,mid[2],mid[2]-rady) 
    xy    <- cbind(xx,yy)
    xyshad <- cbind(xx+dd[1],yy+dd[2])
    if (angle != 0) {xy <- rotatexy  (xy,angle);xyshad <-rotatexy(xyshad,angle)}
    if (shadow.size>0) polygon(xyshad[,1],xyshad[,2],border=NA,col=shadow.col)
    polygon(xy[,1],xy[,2],lwd=lwd,col=box.col,border=lcol,...)
  } else if (box.type == "cylinder")              {
    if (shadow.size>0)  filledcylinder(mid=mid+dd,rx=radx,ry=rady,len=len,col=shadow.col,dr=dr,angle=angle)
    filledcylinder(mid=mid,rx=radx,ry=rady,len=len,col=box.col,dr=dr,lcol=lcol,lwd=lwd,angle=angle,...)
  } else if (box.type== "hexa")             {
    if (shadow.size>0) filledmultigonal(mid=mid+dd,rx=radx,ry=rady,col=shadow.col,nr=6,angle=angle)
    filledmultigonal(mid=mid,rx=radx,ry=rady,col=box.col,lwd=lwd,nr=6,lcol=lcol,angle=angle,...)
  } else if (box.type== "multi")             {
    if (shadow.size>0) filledmultigonal(mid=mid+dd,rx=radx,ry=rady,col=shadow.col,nr=nr,angle=angle)
    filledmultigonal(mid=mid,rx=radx,ry=rady,col=box.col,lwd=lwd,nr=nr,lcol=lcol,angle=angle,...)
    } else if (box.type =="none")                   {
    return
  }
}

##############################################################################
# textempty: One or several lines of text, background colored, no box
##############################################################################

textempty <- function (mid,lab="",adj=c(0.5,0.5),box.col="white",cex=1,...)
{
  text.width <- max(strwidth(lab, units = "user", cex = cex))
  text.height<- strheight(lab, units = "user", cex = cex)
  rect(mid[1]-text.width*adj[1], mid[2]-text.height*adj[2],
       mid[1]+text.width*(1-adj[1]), mid[2]+text.height*(1-adj[2]),col=box.col,border=NA)

  textplain(mid=mid,height=text.height,lab=lab,adj=adj,cex=cex,...)
}

##############################################################################
# textellipse: One or several lines of text, in an ellipse
##############################################################################

textellipse <- function (mid,radx,rady=radx*length(lab),lwd=1,shadow.size=0.01,adj=c(0.5,0.5),
                         lab="",box.col="white",lcol="black",shadow.col="grey",angle=0,dr=0.01,...)
{
  shadowbox("ellipse",mid=mid,radx=radx,rady=rady,
            shadow.size=shadow.size,shadow.col=shadow.col,box.col=box.col,
            lcol=lcol,lwd=lwd,dr=dr,angle=angle)
  textplain(mid=mid,height=rady,lab=lab,adj=adj,...)
}

##############################################################################
# textround: One or several lines of text, in a rectangle with rounded sides
##############################################################################

textround <- function (mid,radx,rady=radx*length(lab),lwd=1,shadow.size=0.01,adj=c(0.5,0.5),
                      lab="",box.col="white",lcol="black",shadow.col="grey",angle=0,rx=rady,...)
{
  shadowbox("round",mid=mid,radx=radx,rady=rady,
            shadow.size=shadow.size,shadow.col=shadow.col,box.col=box.col,
            lcol=lcol,lwd=lwd,rx=rx,angle=angle)  
  textplain(mid,rady,lab,adj,...)
}

##############################################################################
# textrect: One or several lines of text, in a rectangle
##############################################################################

textrect <- function (mid,radx,rady=radx*length(lab),lwd=1,shadow.size=0.01,adj=c(0.5,0.5),
                      lab="",box.col="white",lcol="black",shadow.col="grey",angle=0,...)
{
  shadowbox("rect",mid=mid,radx=radx,rady=rady,
            shadow.size=shadow.size,shadow.col=shadow.col,box.col=box.col,
            lcol=lcol,lwd=lwd,angle=angle)  
  textplain(mid,rady,lab,adj,...)
}

##############################################################################
# textdiamond: One or several lines of text, in a diamond
##############################################################################

textdiamond <- function (mid,radx,rady=NULL,lwd=1,shadow.size=0.01,adj=c(0.5,0.5),
                      lab="",box.col="white",lcol="black",shadow.col="grey",angle=0,...)
{
  if (is.null(rady))
  {
  pin <- par("pin")
  rady  <- radx*pin[1]/pin[2]* length(lab)
  }

  shadowbox("diamond",mid=mid,radx=radx,rady=rady,
            shadow.size=shadow.size,shadow.col=shadow.col,box.col=box.col,
            lcol=lcol,lwd=lwd,angle=angle)  
  textplain(mid,rady,lab,adj,...)
}

##############################################################################
# texthexa: One or several lines of text, in a hexa thingie
##############################################################################

texthexa <- function (mid,radx,rady=radx*length(lab),lwd=1,shadow.size=0.01,adj=c(0.5,0.5),
                      lab="",box.col="white",lcol="black",shadow.col="grey",angle=0,...)
{
  shadowbox("hexa",mid=mid,radx=radx,rady=rady,
            shadow.size=shadow.size,shadow.col=shadow.col,box.col=box.col,
            lcol=lcol,lwd=lwd,angle=angle)
  textplain(mid=mid,height=rady,lab=lab,adj=adj,...)
}

##############################################################################
# textmulti: One or several lines of text, in a multi-angular box
##############################################################################

textmulti <- function (mid,radx,rady=radx*length(lab),lwd=1,shadow.size=0.01,adj=c(0.5,0.5),
                      lab="",box.col="white",lcol="black",shadow.col="grey",angle=0,nr=6,...)
{
  shadowbox("multi",mid=mid,radx=radx,rady=rady,
            shadow.size=shadow.size,shadow.col=shadow.col,box.col=box.col,
            lcol=lcol,lwd=lwd,nr=nr,angle=angle)
  textplain(mid=mid,height=rady,lab=lab,adj=adj,...)
}

##############################################################################
# coordinates: coordinates of components, based on number of elements per row
##############################################################################

coordinates <- function(pos=NULL,
                        mx=0.0,          # mx,my: shift x and y
                        my=0.0,
                        N=length(pos),
                        hor=TRUE,        # if false: vertical 
                        relsize=1)      # scaling factor for size of the graph
 {
   
  if (is.null(pos))   # positioned on a circle
   {
    alpha0  <- pi/2
    alpha   <- alpha0 - (1:N) * 2 * pi/N
    xl      <- cos(alpha)*0.4 + 0.5
    yl      <- sin(alpha)*0.4 + 0.5
    elpos   <- cbind(xl,yl)

   } else {          # row- or column wise specification or fully specified

    nr    <- length(pos)
    if (length(pos) != N*2)   # row- or columnwise specification
    {    
    dx    <- 1/(max(pos))
    dy    <- 1/(nr)        
    dr    <- min(dx,dy)
        
    elpos <-NULL
    ypos  <- rev(seq(dy*0.5,nr*dy,dy))

    for ( i in 1:nr)
    { 
      dx   <- 1/(pos[i]) 
      xpos <- dx*0.5+ seq(0,pos[i]*dx,dx)
      for (j in 1:pos[i]) elpos<-rbind(elpos,c(xpos[j],ypos[i]))
    }
    if (!hor) elpos <- rotatexy(elpos,angle=90,mid=c(0.5,0.5))
    } else {                  # full specification
      elpos<-pos
    }
  }
  usr <- par("usr")
  if (relsize != 1) {
    dx    <- (usr[2]-usr[1])*(1-relsize)/2
    usr[1]<-usr[1]+dx  ;  usr[2]<-usr[2]-dx 
    dy    <- (usr[4]-usr[3])*(1-relsize)/2
    usr[3]<-usr[3]+dy  ;  usr[4]<-usr[4]-dy 
   
    }
  elpos[,1]  <-usr[1]+elpos[,1]*(usr[2]-usr[1]) 
  elpos[,2]  <-usr[3]+elpos[,2]*(usr[4]-usr[3])    


  coordinates<-elpos+matrix(nr=nrow(elpos),nc=2,byrow=TRUE,data=c(mx,my))
     # 2-columned matrix with coordinates (x,y) of each element
} 


##############################################################################
# plotmat: plots transition matrices
##############################################################################
  
plotmat <- function(A,                      # coefficient matrix (rows=to,cols=from)
                    pos=NULL,               # vector, specifying the number of elements in each row, or 2-columned matrix with element position
                    curve=NULL,             # one value, or a matrix, same dimensions as A specifying the arrow curvature; 0 for straight; NA for default curvature
                    name=NULL,              # string vector, specifying the names of elements
                    absent=0,               # all elements in A different from this value are connected
                    relsize=1,              # scaling factor for diagram
                    lwd=2,                  # line width of arrow
                    lcol="black",           # default color of arrow line and box                    
                    box.size=0.1,           # size of label box
                    box.type ="circle",     # shape of label box
                    box.prop =1,            # ratio of height/width of box
                    box.col="white",        # fill color of label box
                    box.lcol=lcol,          # line color of box 
                    box.lwd=lwd,            # line width of box
                    shadow.size = 0.01,     # relative size of shadow
                    shadow.col="grey",      # color of shadow
                    dr=0.01,                # increment for drawing ellipses (arrows)
                    dtext= 0.3,             # control position of arrow text relative to arrowhead
                    self.lwd=1,             # line width of self-arrow, from i to i
                    self.cex=1,             # relative size of self-arrow, relative to box
                    self.shiftx=box.size,   # relative shift of self-arrow, in x-direction
                    self.shifty=NULL,       # relative shift of self-arrow, in x-direction                    
                    arr.lwd=lwd,            # line width of arrow, connecting two different points
                    arr.lcol=lcol,          # color of arrow line
                    arr.col="black",        # color of arrowhead
                    arr.type="curved",      # type of arrowhead (curved,triangle,circle,simple)
                    arr.pos=0.5,            # relative position of arrowhead on arrow segment/curve
                    arr.length=0.4,         # arrow length
                    arr.width=arr.length/2,   # arrow width   
                    endhead=FALSE,          # if true: arrow line stops at end arrowhead                 
                    mx=0.0,my=0.0,          # shift of left (mx) and lower margin (my) [0-1]
                    box.cex=1,              # relative size of text in boxes
                    prefix="",              # to be added in front of non-zero arrow labels
                    cex.txt=1,              # relative size of arrow text 
                    add  = FALSE,           # start a new plot (FALSE), or add to current (TRUE)
                    main="",                # main title
                    ...)                    # other arguments passed to function shadowbox
{                   
  ncomp <- nrow(A)
  if (is.null(name)) name <- rownames(A) 
  if (is.null(name)) name <- colnames(A)
  if (is.null(name)) name <- 1:max(dim(A))
  # remove column names and row names
  if (is.matrix(A)) A <- matrix(nrow=nrow(A),ncol=ncol(A),data=A)
  
  if (length (box.size)  < ncomp) box.size  <- rep(box.size,len=ncomp)
  if (length (box.prop)  < ncomp) box.prop  <- rep(box.prop,len=ncomp)
  if (length (box.type)  < ncomp) box.type  <- rep(box.type,len=ncomp)
  if (length (box.col)   < ncomp) box.col   <- rep(box.col ,len=ncomp)
  if (length (shadow.size)  < ncomp) shadow.size  <- rep(shadow.size,len=ncomp)
  if (length (shadow.col)   < ncomp) shadow.col   <- rep(shadow.col ,len=ncomp)
  if (length(self.shiftx)< ncomp) self.shiftx<- rep(self.shiftx,len=ncomp)
  if (is.null(self.shifty))    self.shifty <- self.shiftx*box.prop 
  if (length(self.shifty)< ncomp) self.shifty<- rep(self.shifty,len=ncomp)
  if (is.null(curve))     curve   <- NA
  if (length(curve)==1)   curve   <- matrix(nrow=ncomp,ncol=ncomp,curve)
  if (length(arr.pos)==1) arr.pos <- matrix(nrow=ncomp,ncol=ncomp,arr.pos)
  
  xlim <- c(0,1)
  if (relsize != 1) {xx <- 1/relsize - 1; xlim <- c(-xx,1+xx)}
  if (!add) openplotmat(main=main,xlim=xlim,ylim=xlim)

  # coordinates of boxes
  elpos <- coordinates(pos,mx,my,ncomp,relsize=relsize)
  if (nrow(elpos) != ncomp) stop ("element position and coefficient matrix not compatible")
  pin   <- par ("pin")        # size of plotting region, inches

  # maximal radius of box (circle,rectangele,...)
  rad   <- max(box.size)      # relative size of circle
  drad  <- rad*dtext
  rad2  <- rad*pin[1]/pin[2]  # rad2 to make circles round
  
  AA<-NULL;RR<-NULL ;DD<-NULL;GG<-NULL;TT<-NULL   # output matrices

  # ARROWS between boxes: all elements in A not equal to 'absent' 
  nonzero <- which(A != absent,arr.ind=TRUE)
  if (length(nonzero)>0)
  {
  for (i in 1:nrow(nonzero))
   {
    ii    <- nonzero[i,]
    arrpos <- arr.pos[ii[1],ii[2]]
    pos1  <- elpos[ii[1],]                          # pos to
    pos2  <- elpos[ii[2],]                          # pos from
    dpos  <- pos1-pos2
    angle <- atan(dpos[2]/dpos[1])*180/pi           # angle between both
    txt   <- paste(prefix,A[ii[1],ii[2]],sep="")    # text to write     
    AA    <- c(AA,angle)  
    mid   <- 0.5*(pos1+pos2)                        # midpoint of ellipsoid arrow

    if (is.nan(angle))        #  pos1=pos2: self arrow
    {
     rx     <- rad*self.cex
     ry     <- rad2*self.cex    
     shiftx <- self.shiftx[ii[1]]
     shifty <- self.shifty[ii[1]]*pin[1]/pin[2]
     mid    <- mid+c(shiftx,shifty)
     ifelse (shiftx < 0, meanpi <-3*pi/2,meanpi <-pi/2)  

     plotellipse(rx=rx,ry=ry,mid=mid,from=0,to=2*pi,lwd=self.lwd,dr=dr,lcol=arr.lcol)
    
     ell  <- getellipse(rx=ry,ry=ry,mid=mid,
                      from=1.01*meanpi,to=0.99*meanpi,dr=-0.002)
     Arrows(ell[1,1],ell[1,2],ell[nrow(ell),1],ell[nrow(ell),2],arr.col=arr.col,
            arr.length=arr.length*0.5,arr.width=arr.width,lwd=arr.lwd,arr.type=arr.type)
     DD   <- rbind(DD,c(ell[nrow(ell),1],ell[nrow(ell),2]))  
# karline: suggestion from Yvonnic NOEL: evaluate expressions
     if(cex.txt>0 && txt!= "") text(mid[1],mid[2],parse(text=txt),adj=c(0.5,0.5),cex=cex.txt)
     TT <- rbind(TT,c(mid[1],mid[2],0.5,0.5))
     cycle
   
    } else {                 # arrow between different components
     dst   <- dist(rbind(pos1,pos2))
     ry    <- curve[ii[1],ii[2]]*dst
     if (is.na(ry)) ry<-rad*dst 

     ifelse (angle<0,xadj <- 0,xadj <-1)
     ifelse (angle<0,yadj <- 0,yadj <-0.5)
     if(angle == 0) {xadj= 0.5;yadj=0}

     adj   <- c(xadj,yadj)
     if (ry==0)       # straight line
     {
       mid1<-straightarrow (from=pos2,to=pos1,lwd=arr.lwd,arr.type=arr.type,arr.length=arr.length,
                      arr.pos=arrpos,arr.width=arr.width,arr.col=arr.col,lcol=arr.lcol,endhead=endhead)
    
       DD <- rbind(DD,mid1)            
       if (angle>0) adj=c(0,1)
       mpos <- mid1- (adj-0.5)* drad  
# karline: suggestion from Yvonnic NOEL: evaluate expressions
       if(cex.txt>0&& txt!= "") text(mpos[1],mpos[2],parse(text=txt),adj=adj,cex=cex.txt)
       TT <- rbind(TT,c(mpos[1],mpos[2],adj))
     } else          # curved line

     {
       from<-0;to<-pi    
       if (pos2[1]==pos1[1] & pos2[2]>pos1[2]) adj <- c(1 ,1) 
       if (pos2[1]==pos1[1] & pos2[2]<pos1[2]) adj <- c(0 ,1) 
       if (pos2[1]<=pos1[1]) {from<-pi;to<-2*pi}
       if (pos2[1]<pos1[1] & angle>=0) adj <- c(0 ,1) 
       if (pos2[1]<pos1[1] & angle<0)  adj <- c(1 ,0) 
       meanpi <- arrpos*to+(1-arrpos)*from # mean(c(from,to))
       ifelse(endhead,To<-meanpi,To<-to)
       plotellipse(rx=dst/2,ry=ry,mid=mid,angle=angle,from=from,to=To,lwd=arr.lwd,dr=dr,lcol=arr.lcol)

       ell <- getellipse(rx=dst/2,ry=ry,mid=mid,angle=angle,
                         from=1.001*meanpi,to=0.999*meanpi,dr=-0.002)
       Arrows(ell[1,1],ell[1,2],ell[nrow(ell),1],ell[nrow(ell),2],arr.col=arr.col,                    
             code=1,arr.length=arr.length,arr.width=arr.width,lwd=arr.lwd,
             arr.type=arr.type)
       DD <- rbind(DD,c(ell[nrow(ell),1],ell[nrow(ell),2]))           
       ell <- getellipse(rx=dst/2,ry=ry+drad,mid=mid,angle=angle,      
                        from=meanpi,to=meanpi)
# karline: suggestion from Yvonnic NOEL: evaluate expressions
       if(cex.txt>0&& txt!= "") text(ell[1,1],ell[1,2],parse(text=txt),adj=adj,cex=cex.txt)
       TT <- rbind(TT,c(ell[1,1],ell[1,2],adj))
     } 
    }   # end i
  GG <-c(GG,txt)
  RR <-c(RR,ry)}

  } # end length (nonzero)

  # BOXES
  radii <- NULL
  for (i in 1:nrow(A))
    {
     p <- elpos[i,]
     # radius of box (circle)
     rad   <- box.size[i]                    # relative size of circle
     rad2  <- rad*pin[1]/pin[2]*box.prop[i]  # used to make circles round
     radii <- rbind(radii,c(rad,rad2))

     shadowbox(box.type=box.type[i],mid=p,radx=rad,rady=rad2,lcol=box.lcol,
               lwd=box.lwd,shadow.size=shadow.size[i],shadow.col=shadow.col[i],
               box.col=box.col[i],dr=dr,...)
     textplain(mid=p,height=rad2,lab=name[i],cex=box.cex)

    } # end i

  rect<-cbind(elpos-radii,elpos+radii)
  colnames(elpos)<- colnames(radii) <- c("x","y")
  colnames(rect) <- c("xleft","ybot","xright","ytop")
  plotmat <-list(arr=data.frame(nonzero,Angle=AA,Value=GG,rad=RR,ArrowX=DD[,1],ArrowY=DD[,2],
                        TextX=TT[,1],TextY=TT[,2]),
              comp=elpos,radii=radii,rect=rect)  
} # end function PLOTMAT


#########################################################################
## plotweb      : plots a web                                          ##
#########################################################################

plotweb    <- function (flowmat,             # flow matrix, rows=flow from, columns=flow TO
                        names=NULL,          # names of components 
                        lab.size = 1.5,      # relative size of labels
                        add  = FALSE,        # start a new plot (FALSE), or add to current (TRUE)
                        fig.size = 1.3,       # if add= FALSE: relative size of figure
                        main="",             # if add= FALSE: main title  
                        sub="",              # if add= FALSE: sub title
                        sub2="",             # if add= FALSE: title in bottom corner
                        log=FALSE,           # logical indicating whether to scale logarithmically
                        nullflow = NULL,     # below this; flow is assumed 0 and arrow not drawn
                        minflow = NULL,      # flowvalue corresponding to minimum arrow thickness
                        maxflow = NULL,      # flowvalue corresponding to maximum arrow thickness 
                        legend=TRUE,         # logical indicating whether to add a legend with arrows 
                        leg.digit=5,          # nr of digits for writing legend
                        leg.title=NULL,      # title for arrow legend, e.g to give units                      
                        lcol= "black",       # line color
                        arr.col= "black",    # arrow color
                        val=FALSE,           # logical indicating whether to write values as a legend ?
                        val.digit=5,          # nr of digits for writing values - only if val =TRUE
                        val.size=0.6,         # relative size for writing values - only if val =TRUE
                        val.col="red",        # color for writing values - only if val =TRUE
                        val.title=NULL,       # title for values legend - only if val =TRUE
                        val.ncol=1,           # number of columns for writing values - only if val =TRUE
                        budget = FALSE,      # logical indicating whether to calculate budget per component
                        bud.digit=5,          # nr of digits for writing budget - only if budget =TRUE
                        bud.size=0.6,         # relative size for writing budget - only if budget =TRUE
                        bud.title="budget",   # title for budget legend - only if budget =TRUE
                        bud.ncol=1,           # number of columns for writing budget - only if budget =TRUE
                        maxarrow = 10,       # maximal thickness of arrow
                        minarrow = 1,        # minimal thickness of arrow
                        length=0.1,          # length of the edges of the arrow head (in inches)
                        dcirc=1.2,           # if cannibalism, offset of circular 'arrow' - if dcirc=0:no circle drawn
                        bty = "o",           # type of legend box, one of "o" or "n"
                        ...)                 # extra arguments passed to R-function arrows

{
   
  ##-----------------------------------------------------------------
  ## constructing the names, flow matrix
  nm <- par("mar")
  if (ncol(flowmat) != nrow(flowmat))  stop("flowmat has to be square")

  components <- names
  if (is.null(components)) components <- colnames(flowmat )
  if (is.null(components)) components <- rownames(flowmat )
  if (is.null(components)) components <- as.character(1:ncol(flowmat)) 
  
  numcomp    <- length(components)  ## number of food web components
  if (ncol(flowmat) != numcomp) stop("flowmat and names not compatible")

  flowmatrix <- flowmat
  if (!is.null(nullflow )) {
    flowmatrix[flowmatrix<nullflow[1] ] <- 0
    if (length(nullflow == 2)) flowmatrix[flowmatrix>nullflow[2] ] <- 0   }

  zero <- 0
  if (log) {flowmatrix <- log10(flowmatrix +1e-20); flowmatrix[flowmatrix==-20]<-0 }

  if (is.null(maxflow)) maxflow  <- max(flowmatrix) else 
      if (log) maxflow <- log10(maxflow)
  if (is.null(minflow)) minflow  <- min(flowmatrix[flowmatrix != zero]) else 
      if (log) minflow <- log10(minflow)

  ##-----------------------------------------------------------------
  ## new empty plot

  if (! add)
  { 
    figlim  <- c(-fig.size,fig.size)   
    marg    <- par("mar")

    ifelse(val,    mar <- c(2,0,2,4), mar<-c(2,2,2,2))   # if values written:shift left
    par(mar=mar)

    plot(c(0, 0), type = "n", ylab = "", asp = 1, xaxt = "n", 
         yaxt = "n", frame.plot = FALSE, xlim = figlim,
         ylim = figlim,main=main,xlab="")
    mtext(side=3,line=-1,sub)     
    mtext(side=1,adj=0.5,text=sub2)
  }
  
  ##-----------------------------------------------------------------  
  ## component labels positioned on a circle 
  
  alpha0 <- pi/2
  alpha  <- alpha0 - (1:numcomp) * 2 * pi/numcomp  # all angles
  xl     <- cos(alpha)                             # all x-positions
  yl     <- sin(alpha)                             # all y-positions

  # adjust depending on 
  for (i in 1:numcomp) {                           # write labels
		    if (    xl[i]  > 0     ) adjustx = 0       # text adjustments
		    if (    xl[i]  < 0     ) adjustx = 1
		    if (abs(xl[i]) < 0.0001) adjustx = 0.5
  	    if (    yl[i]  > 0     ) adjusty = 0
		    if (    yl[i]  < 0     ) adjusty = 1
# karline: bug fix - 06-03-2008
		    if (abs(yl[i]) < 0.0001) adjusty = 0.5
        text(xl[i], yl[i], components[i],
              adj =c(adjustx,adjusty), cex = par("cex") * lab.size)
                        }

  ##-----------------------------------------------------------------
  ## arrows representing the flows

	circle <- function (i,lwd,col)   # circular arrow
	 {
    cx   <- xl[i]*dcirc
    cy   <- yl[i]*dcirc
    r    <- 0.1  # radius
    x    <-c(seq(-pi,pi,by=0.01),pi)  # adding the last element ensures circle is closed
    lines(cx+r*sin(x),cy+r*cos(x),lwd=lwd,col=lcol)
	 }

  par(lend=1)

  darrow   <- (maxarrow-minarrow)/(maxflow-minflow)
  dr       <- 0.02 

  xi       <- xl-dr* cos(alpha)   # arrow positions: shifted relative to labels
  yi       <- yl-dr* sin(alpha)
  iflow    <- 1
  offset   <- 1
  ltext    <- NULL
  for (i in 1:numcomp) 
		{x2 <- xi[i] 
     y2 <- yi[i]
     for (j in 1:i)  
      {if (flowmatrix[i,j] >zero | flowmatrix[j,i] >zero) 
        {x1 <- xi[j] 
         y1 <- yi[j] 
         dx <- x2-x1
         dy <- y2-y1;

         ifelse (i == j,fsize<-flowmatrix[i,j],fsize<-flowmatrix[i,j]-flowmatrix[j,i])
         ifelse (fsize>0 ,code<-1,code<-2) 
          
         size <- minarrow + darrow * (abs(fsize)-minflow) # arrow thickness
                              
			   if (i != j) arrows (x1+dr*dx,y1+dr*dy,x2-dr*dx,y2-dr*dy,
                             length=length,code=code,lwd=size,col=arr.col,...)
			   if (i == j) circle (i,lwd=size,col=arr.col)
         if (val) 
          {
           text(x=(x1+x2)*0.5,y=(y1+y2)*0.5,labels=iflow,offset=offset,col=val.col)
           ltext<- c(ltext,paste(iflow, ":", format.pval(abs(fsize),val.digit)))
          }
         iflow <- iflow+1
        }  # end flowmatrix>zero
      }  # end j
    }  # end i

  ##-----------------------------------------------------------------
  ## legends

  if (legend)   
  {
    sizeleg = par("cex") * lab.size
    ## size of largest and smallest arrows

    if (!log) {tmax <-    maxflow; tmin <-    minflow; title=leg.title} else
          {tmax <- 10^maxflow; tmin <- 10^minflow; title=paste("logarithmic scale",leg.title)}

    legend("bottomright",legend=c(format.pval(tmax,leg.digit),
           format.pval(tmin,leg.digit)),cex=sizeleg,title=title,
           lwd=c(maxarrow,minarrow),bty=bty)
   }

  if (!val & !budget) return   

  if (! add)
  { 
  par(mar=c(0,0,0,0))
  par(new=TRUE)
  plot(c(0, 0), type = "n", ylab = "", xaxt = "n", 
       yaxt = "n", frame.plot = FALSE,main="",xlab="")
  } 
    
  if (val) legend("topright",legend=ltext,cex=val.size,
           title=val.title,ncol=val.ncol,bty=bty)

  if (budget)
  {
   rate <- NULL
    # sum all flows in - sum of flow out 
   for (i in 1:numcomp) rate <- c(rate,paste(components[i], ":", 
                       format.pval(sum(flowmat[,i])-sum(flowmat[i,]),bud.digit)))
   legend("topleft",legend=rate,cex=bud.size,title=bud.title,
          ncol=bud.ncol,bty=bty)
  }
      par("mar"=nm)
}  ########## END OF plotweb  ########## 