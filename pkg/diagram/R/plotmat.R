
##==============================================================================
# plotmat: plots transition matrices
##==============================================================================

plotmat <- function(A, pos=NULL, curve=NULL, name=NULL, absent=0,
     relsize=1, lwd=2, lcol="black", box.size=0.1, box.type ="circle",
     box.prop =1, box.col="white", box.lcol=lcol, box.lwd=lwd,
     shadow.size = 0.01, shadow.col="grey", dr=0.01, dtext= 0.3,
     self.lwd=1, self.cex=1, self.shiftx=box.size, self.shifty=NULL,
     self.arrpos=NULL, arr.lwd=lwd, arr.lcol=lcol, arr.col="black",
     arr.type="curved", arr.pos=0.5, arr.length=0.4, arr.width=arr.length/2,
     endhead=FALSE, mx=0.0, my=0.0, box.cex=1, prefix="", cex.txt=1,
     add = FALSE, main="", ...)  {

  ncomp <- nrow(A)
  if (is.null(name))
    name <- rownames(A)
  if (is.null(name))
    name <- colnames(A)
  if (is.null(name))
    name <- 1:max(dim(A))

  # remove column names and row names
  if (is.matrix(A))
    A <- matrix(nrow=nrow(A), ncol=ncol(A), data=A)

  if (length (box.size)    < ncomp)
    box.size  <- rep(box.size, len=ncomp)
  if (length (box.prop)    < ncomp)
    box.prop  <- rep(box.prop, len=ncomp)
  if (length (box.type)    < ncomp)
    box.type  <- rep(box.type, len=ncomp)
  if (length (box.col)     < ncomp)
    box.col   <- rep(box.col , len=ncomp)
  if (length (box.lcol)    < ncomp)
    box.lcol  <- rep(box.lcol, len=ncomp)
  if (length (box.cex)     < ncomp)
    box.cex   <- rep(box.cex , len=ncomp)
  if (length (box.lwd)     < ncomp)
    box.lwd   <- rep(box.lwd , len=ncomp)

  if (length (shadow.size) < ncomp)
    shadow.size  <- rep(shadow.size, len=ncomp)
  if (length (shadow.col)  < ncomp)
    shadow.col   <- rep(shadow.col , len=ncomp)
  selflwd    <- self.lwd
  selfcex    <- self.cex
  selfarrpos <- self.arrpos
  if (length (selfarrpos)  < ncomp)
    selfarrpos<- rep(selfarrpos, len=ncomp)
  if (length (selflwd)     < ncomp)
    selflwd<- rep(selflwd, len=ncomp)
  if (length (selfcex)     < ncomp)
    selfcex<- rep(selfcex, len=ncomp)
  if (length (self.shiftx) < ncomp)
    self.shiftx<- rep(self.shiftx, len=ncomp)
  if (is.null(self.shifty))
    self.shifty <- self.shiftx*box.prop
  if (length(self.shifty)  < ncomp)
     self.shifty<- rep(self.shifty, len=ncomp)
  if (is.null(curve))
    curve   <- NA
  if (length(curve)==1)
    curve   <- matrix(nrow=ncomp, ncol=ncomp, curve)
  if (length(arr.pos)==1)
    arr.pos <- matrix(nrow=ncomp, ncol=ncomp, arr.pos)

  arrwidth  <- arr.width        # can be a matrix...
  arrlength <- arr.length
  arrlwd    <- arr.lwd
  arrlcol   <- arr.lcol
  arrcol    <- arr.col
  arrtype   <- arr.type
  cextxt    <- cex.txt

  if (length(arrwidth) ==1)
    arrwidth  <- matrix(nrow=ncomp, ncol=ncomp, arrwidth)
  if (length(arrlength)==1)
    arrlength <- matrix(nrow=ncomp, ncol=ncomp, arrlength)
  if (length(arrlwd)   ==1)
    arrlwd    <- matrix(nrow=ncomp, ncol=ncomp, arrlwd)
  if (length(arrlcol)  ==1)
    arrlcol   <- matrix(nrow=ncomp, ncol=ncomp, arrlcol)
  if (length(arrcol)   ==1)
    arrcol    <- matrix(nrow=ncomp, ncol=ncomp, arrcol)
  if (length(arrtype)  ==1)
    arrtype   <- matrix(nrow=ncomp, ncol=ncomp, arrtype)
  if (length(cextxt)   ==1)
    cextxt    <- matrix(nrow=ncomp, ncol=ncomp, cextxt)

  xlim <- c(0, 1)

  if (relsize != 1) {
    xx <- 1/relsize - 1
    xlim <- c(-xx, 1+xx)
  }
  if (!add)
    openplotmat(main=main, xlim=xlim, ylim=xlim)

  # coordinates of boxes
  elpos <- coordinates(pos, mx, my, ncomp, relsize=relsize)
  if (nrow(elpos) != ncomp)
    stop ("element position and coefficient matrix not compatible")
  pin   <- par ("pin")        # size of plotting region,  inches

  # maximal radius of box (circle, rectangele, ...)
  rad   <- max(box.size)      # relative size of circle
  drad  <- rad*dtext
  rad2  <- rad*pin[1]/pin[2]  # rad2 to make circles round

  AA<-NULL
  RR<-NULL
  DD<-NULL
  GG<-NULL
  TT<-NULL   # output matrices

  ## ARROWS between boxes: all elements in A not equal to 'absent'
  nonzero <- which (A != absent, arr.ind=TRUE)

  if (length(nonzero)>0)  {
    for (i in 1:nrow(nonzero))  {
      ii    <- nonzero[i, ]
      arrpos <- arr.pos[ii[1], ii[2]]
      arr.width  <- arrwidth[ii[1], ii[2]]
      arr.length <- arrlength[ii[1], ii[2]]
      arr.lwd    <- arrlwd[ii[1], ii[2]]
      arr.lcol   <- arrlcol[ii[1], ii[2]]
      arr.col    <- arrcol[ii[1], ii[2]]
      arr.type   <- arrtype[ii[1], ii[2]]
      cex.txt    <- cextxt[ii[1], ii[2]]

      pos1  <- elpos[ii[1], ]                          # pos to
      pos2  <- elpos[ii[2], ]                          # pos from
      dpos  <- pos1-pos2
      angle <- atan(dpos[2]/dpos[1])*180/pi           # angle between both
      txt   <- paste(prefix, A[ii[1], ii[2]], sep="") # text to write
      AA    <- c(AA, angle)
      mid   <- 0.5*(pos1+pos2)                   # midpoint of ellipsoid arrow

      if (is.nan(angle))   {      #  pos1=pos2: self arrow
        rx     <- rad*self.cex
        ry     <- rad2*self.cex
        shiftx <- self.shiftx[ii[1]]
        shifty <- self.shifty[ii[1]]*pin[1]/pin[2]
        self.lwd <- selflwd[ii[1]]
        self.cex <- selfcex[ii[1]]
        self.arrpos <- selfarrpos[ii[1]]
        mid    <- mid+c(shiftx, shifty)
        if (is.null(self.arrpos))  {
          ifelse (shiftx < 0,  meanpi <-3*pi/2, meanpi <-pi/2)
        }  else
          meanpi <- self.arrpos

        plotellipse(rx=rx, ry=ry, mid=mid, from=0, to=2*pi,
                    lwd=self.lwd, dr=dr, lcol=arr.lcol)

        ell  <- getellipse(rx=ry, ry=ry, mid=mid,
                      from=1.01*meanpi, to=0.99*meanpi, dr=-0.002)
        Arrows(ell[1,1], ell[1,2], ell[nrow(ell),1], ell[nrow(ell),2],
               arr.col=arr.col, arr.length=arr.length*0.5,
               arr.width=arr.width, lwd=arr.lwd, arr.type=arr.type)
        DD   <- rbind(DD, c(ell[nrow(ell), 1], ell[nrow(ell), 2]))

        if(cex.txt>0 && txt!= "")
          text(mid[1], mid[2], parse(text=txt), adj=c(0.5, 0.5), cex=cex.txt)
        TT <- rbind(TT, c(mid[1], mid[2], 0.5, 0.5))
        cycle

      } else {                 # arrow between different components
       dst   <- dist(rbind(pos1, pos2))
       ry    <- curve[ii[1], ii[2]]*dst
       if (is.na(ry))
         ry<-rad*dst

       ifelse (angle<0, xadj <- 0, xadj <-1)
       ifelse (angle<0, yadj <- 0, yadj <-0.5)
       if (angle == 0) {
         xadj= 0.5
         yadj=0
       }

       adj   <- c(xadj, yadj)
       if (ry==0)   {    # straight line
         mid1<-straightarrow (from=pos2, to=pos1, lwd=arr.lwd,
                              arr.type=arr.type, arr.length=arr.length,
                              arr.pos=arrpos, arr.width=arr.width,
                              arr.col=arr.col, lcol=arr.lcol, endhead=endhead)

         DD <- rbind(DD, mid1)
         if (angle>0) adj=c(0, 1)
         mpos <- mid1- (adj-0.5)* drad

         if(cex.txt>0&& txt!= "")
           text(mpos[1], mpos[2], parse(text=txt), adj=adj, cex=cex.txt)
         TT <- rbind(TT, c(mpos[1], mpos[2], adj))
       } else      {         # curved line

         from <- 0
         to <- pi
         if (pos2[1]==pos1[1] & pos2[2]>pos1[2])
           adj <- c(1 , 1)
         if (pos2[1]==pos1[1] & pos2[2]<pos1[2])
           adj <- c(0 , 1)
         if (pos2[1]<=pos1[1]) {
           from <- pi
           to <- 2*pi
         }
         if (pos2[1] < pos1[1] & angle>=0)
           adj <- c(0 , 1)
         if (pos2[1] < pos1[1] & angle<0)
           adj <- c(1 , 0)
         meanpi <- arrpos * to + (1-arrpos) * from
         ifelse(endhead, To<-meanpi, To<-to)
         plotellipse(rx=dst/2, ry=ry, mid=mid, angle=angle, from=from,
                     to=To, lwd=arr.lwd, dr=dr, lcol=arr.lcol)
         ell <- getellipse(rx=dst/2, ry=ry, mid=mid, angle=angle,
                         from=1.001*meanpi, to=0.999*meanpi, dr=-0.002)
         Arrows(ell[1,1], ell[1,2], ell[nrow(ell),1], ell[nrow(ell),2],
                arr.col=arr.col, code=1, arr.length=arr.length,
                arr.width=arr.width, lwd=arr.lwd, arr.type=arr.type)
         DD <- rbind(DD, c(ell[nrow(ell),1], ell[nrow(ell),2]))
         ell <- getellipse(rx=dst/2, ry=ry+drad, mid=mid, angle=angle,
                           from=meanpi, to=meanpi)
         if(cex.txt>0 && txt!= "")
           text(ell[1,1], ell[1,2], parse(text=txt), adj=adj, cex=cex.txt)
         TT <- rbind(TT, c(ell[1, 1], ell[1, 2], adj))
       }
    }   # end i
    GG <- c (GG, txt)
    RR <- c (RR, ry)}

  } # end length (nonzero)

  ## BOXES
  radii <- NULL
  for (i in 1:nrow(A)) {
    p <- elpos[i, ]
    # radius of box (circle)
    rad   <- box.size[i]                    # relative size of circle
    rad2  <- rad*pin[1]/pin[2]*box.prop[i]  # used to make circles round
    radii <- rbind(radii, c(rad, rad2))

    shadowbox(box.type=box.type[i], mid=p, radx=rad, rady=rad2,
              lcol=box.lcol[i], lwd=box.lwd[i], shadow.size=shadow.size[i],
              shadow.col=shadow.col[i], box.col=box.col[i], dr=dr, ...)
    textplain(mid=p, height=rad2, lab=name[i], cex=box.cex[i])

  } # end i

  rect <- cbind(elpos-radii, elpos+radii)
  colnames(elpos) <- colnames(radii) <- c("x", "y")
  colnames(rect) <- c("xleft", "ybot", "xright", "ytop")
  plotmat <-list (arr=data.frame(nonzero, Angle=AA, Value=GG, rad=RR,
                                 ArrowX=DD[,1], ArrowY=DD[,2],
                                 TextX=TT[,1], TextY=TT[,2]),
                                 comp=elpos, radii=radii, rect=rect)
} # end function PLOTMAT

