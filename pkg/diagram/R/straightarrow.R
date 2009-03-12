
##==============================================================================
# straightarrow: Plot straight arrow at certain distance between two points
##==============================================================================

straightarrow <- function(from, to, lwd=2, lty=1, lcol="black",
    arr.pos=0.5, endhead=FALSE, ...)    {

  ifelse (endhead, To <- arr.pos*to+(1-arr.pos)*from, To<- to)

  segments(from[1], from[2], To[1], To[2], lwd=lwd, lty=lty, col=lcol)

  mid2  <- c((arr.pos-0.01)*to+(1-arr.pos+0.01)*from)
  mid1  <- c((arr.pos     )*to+(1-arr.pos     )*from)

  Arrows(mid2[1], mid2[2], mid1[1], mid1[2], lcol=lcol, ...)
  straightarrow <- mid1      # coordinates (x,y) where arrowhead is drawn

}
