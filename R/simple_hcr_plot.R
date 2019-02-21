# Just for checking HCR shapes when picking candidates for the tutorials

plot_hcr <- function(hcr, col="black"){
  lines(x=c(0, hcr[1]), y=c(hcr[3],hcr[3]), col=col)
  lines(x=c(hcr[2], 1.0), y=c(hcr[4],hcr[4]), col=col)
  lines(x=c(hcr[1],hcr[2]), y=c(hcr[3],hcr[4]), col=col)
}


plot(x=c(0,1), y=c(0, 200), type="n")
lines(x=c(0.5,0.5), y=c(0,200), lty=3)
lines(x=c(0.2,0.2), y=c(0,200), lty=3)
lines(x=c(0,1), y=c(100,100), lty=3)
  
hcr1 <- c(0.2,0.3,10,130)
hcr2 <- c(0.2,0.5,10,140)
hcr3 <- c(0.2,0.8,10,150)


plot_hcr(hcr1,"black")
plot_hcr(hcr2,"blue")
plot_hcr(hcr3,"red")

# Plot for the Intro to HCR tutorial

hcr <- c(0.2,0.5,10,150)
plot(x=c(0,1), y=c(0, 180), type="n", xlab = "SB/SBF=0", ylab="Catch limit",yaxs='i', xaxs='i')
lines(x=c(0, hcr[1]), y=c(hcr[3],hcr[3]), col="blue", lwd=3)
lines(x=c(hcr[2], 1.0), y=c(hcr[4],hcr[4]), col="blue", lwd=3)
lines(x=c(hcr[1],hcr[2]), y=c(hcr[3],hcr[4]), col="blue", lwd=3)
grid()
text(x=0.05,y=22,labels="Cmin")
text(x=0.8,y=170,labels="Cmax")
text(x=0.17,y=22,labels="Blim")
text(x=0.5,y=120,labels="Belbow")
