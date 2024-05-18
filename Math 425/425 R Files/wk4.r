# Wk 4


# Confidence -> Inside
# Significance -> Outside

# Set up the function:
tprob <- function(t=1, df=3, show.normal=TRUE, xlim=c(-4,4)){
  curve(dt(x, df), from=xlim[1], to=xlim[2], lwd=2)
  xlo = seq(xlim[1], -abs(t), length.out=100)
  xhi = seq(abs(t), xlim[2], length.out=100)
  polygon(c(xlo[1],xlo,xlo[100]), c(0,dt(xlo,df),0), col="lightblue", border=NA)
  polygon(c(xhi[1],xhi,xhi[100]), c(0,dt(xhi,df),0), col="lightblue", border=NA)  
  abline(h=0, v=c(-abs(t),abs(t)), col=c("red","orange","orange"), lwd=c(1,3,3))
  text(xlo[1], dt(.5,df), paste("Area = ", round(pt(-abs(t), df)*2,4)), pos=4)
  if(show.normal){
    curve(dnorm(x), add=TRUE, col="orangered")
  }
}

# Use the function
tprob(t=-2)
tprob(t=-8, xlim=c(-9,9))
tprob(t=-2, df=15)

curve(dt(x, 3), from=-4, to=4, lwd=2)
curve(dnorm(x), add=TRUE, col="gray")
abline(h=0, v=c(-1,1), col=c("gray","orange","orange"), lwd=c(1,2,2))

pt(-1, 3)*2 #gives the area more extreme than t=-1 for t-dist with 3 df.

# qt doesn't take a confidence, take's percentile. One tailed.

?pt
pt(qt(0.975,48),48)
qt(pt(0.975,48),48)

t = (-17.5761 -0)/6.758
# bc again it takes percentile. Look to see if it's a
# probability of t (pt) - percentile
# need to double two tailed
pt(-abs(t),)

pt(.095,13)

# quantile
# qt returns two values here, .025 is - and .975 is positive
3.9324 + qt(c(0.025,0.975),48)*.4155
# -> 3.096981 4.767819