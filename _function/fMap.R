fMap <- function(W.DIR,
                 IN.DIR,
                 V.DYN,
                 R.L1,
                 R.S1,
                 R.L2,
                 R.S2,
                 R.K1,
                 R.K2,
                 R.SHD,
                 F.L,
                 F.S,
                 CROP,
                 D.L,
                 ALPHA,
                 SHP1,
                 SHP2,
                 PREFIX,
                 SUFFIX,
                 H,
                 W){
p <- read.csv2(file.path(W.DIR,IN.DIR,V.DYN))
l1 <- raster(file.path(W.DIR,IN.DIR,R.L1))
s1 <- raster(file.path(W.DIR,IN.DIR,R.S1))
l2 <- raster(file.path(W.DIR,IN.DIR,R.L2))
s2 <- raster(file.path(W.DIR,IN.DIR,R.S2))
k1 <- raster(file.path(W.DIR,IN.DIR,R.K1))
k2 <- raster(file.path(W.DIR,IN.DIR,R.K2))
shd <- raster(file.path(W.DIR,IN.DIR,R.SHD))
b1 <- shapefile(file.path(W.DIR,IN.DIR,SHP1))
b2 <- shapefile(file.path(W.DIR,IN.DIR,SHP2))
#Plotting
DOY <- DOY
D <- 0
LEGEND = c("sehr gering","gering","mittel","hoch","sehr hoch")
my.palette <- rev(brewer.pal(n = 5, name = "Spectral"))
breaks.qt <- classIntervals(values(l1*s1*k1), n = (5-1), style = "quantile")
pb <- txtProgressBar(min=1, max=length(DOY), style=3)


for(i in 1:length(DOY)){
  V.B <- round(p[DOY[i],]$SC,1)
  V.K <- round(b1@data$CI_K/(10*F.K),0)
  V.L <- round(b1@data$CI_L/(10*F.L),0)
  V.S <- round(b1@data$CI_S/(10*F.S),0)
  
  pdf(paste(W.DIR,OUT.DIR,PREFIX,CROP,"_",p[DOY[i],]$PHASE,"_",DOY[i],"_",SUFFIX,".pdf",sep=""), 
      height=H,width=W)
    par(xpd = T, mar = par()$mar + c(0,0,0,5))
    sl <- l2 * s2 * k2 * (1-p[DOY[i],]$SC)
    plot(shd,
         col = grey(100:1/100),
         box=FALSE,
         legend=FALSE,
         axes = TRUE)
    plot(sl,
         breaks = breaks.qt$brks,
         col=my.palette,
         legend=FALSE,
         axes = TRUE,
         box=FALSE,
         main=paste(CROP),
         alpha=ALPHA,
         add=TRUE)
    plot(b2,add=TRUE)
    x.max <- max(data.frame(coordinates(sl))$x)
    x.min <- min(data.frame(coordinates(sl))$x)
    y.max <- max(data.frame(coordinates(sl))$y)
    y.min <- min(data.frame(coordinates(sl))$y)

    #Legend
    legend(x.max+((x.max-x.min)/D.L),(y.max+y.min)/2, 
           title=expression(paste(bold(Potential))),
           legend=c(paste(LEGEND)),
           fill=my.palette,
           bty="n",
           xpd = TRUE)
    
    legend("topright", 
           legend=c(as.expression(bquote(.(paste(jul2date2(p[DOY[i],]$DOY,2018)$day,".",jul2date2(p[DOY[i],]$DOY,2018)$month,".",sep=""))))),
           bty="y",
           cex=1,
           bg="white")
    legend(x.max+((x.max-x.min)/D.L),y.max, 
           title=expression(paste(bold(Faktoren))), 
           legend=c(as.expression(bquote({italic(VI)^{K}} == .(V.K))),
                    as.expression(bquote({italic(VI)^{L}} == .(V.L))),
                    as.expression(bquote({italic(VI)^{S}} == .(V.S))),
                    as.expression(bquote({italic(B)} == .((V.B)))),
                    as.expression(bquote({italic(VI)^{E}} == .(round((V.K+V.L+V.S)/3*(1-V.B),0))))),
           text.col=c("black","black","black","black","red"),
           cex=1,
           bty = "n",
           xpd = TRUE)
  dev.off()  
  setTxtProgressBar(pb, i)
  }
}

