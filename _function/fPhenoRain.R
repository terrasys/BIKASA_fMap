fPhenoRain <- function(W.DIR,
                       IN.DIR,
                       V.DYN,
                       CROP,
                       DOY){
#Plotting
DOY <- DOY

if(CROP==202){
pb <- txtProgressBar(min=1, max=length(DOY), style=3)
for(i in 1:length(DOY)){
  #Winterweizen
  {#par(mar=c(5.1, 5.1, 1, 1))
    #color schema
    CT.P <- c(10,12,15,18,19,21,24)
    CT.P.Legend <- c("Bestellung","Auflaufen","Schossen","Ährenschieben","Milchreife","Gelbreife","Ernte")
        col.p <- c("#3288BD", #10
               "#e5f4e3", #12
             "#ABDDA4", #15
             "#66C2A5", #18
             "#feecb9", #19
             "#FDAE61", #21
             "#F46D43",#24
             "#DC143C") #100
  p <- read.csv2(file.path(W.DIR,IN.DIR,V.DYN))
  plot(p$DOY,p$MAX,
       xaxt="n",
       ylim=c(0.5,max(p$MAX)+0.7),
       xlim=c(min(p$DOY),max(p$DOY)+80),
       ylab=expression(italic(SNI),sep=""),
       #xlab=as.expression(bquote(italic(Kalendertag))),
       xlab="Kalendertag",
       cex.lab=1.2,
       col="white",
       main="")
  #plot axis
  x1 <- seq(min(p$DOY),max(p$DOY),10)
  x2 <- seq(min(p$DOY),max(p$DOY),2)
  axis(1, at=x2, col.tick="grey", las=1,labels=FALSE,cex=1.2)
  axis(1, at=x1, col.axis="black", las=2,cex=1.2)
  #plot phenological phases  
  rect(min(p$DOY),#min
       0,
       min(p[which(p$PHASE==15),]$DOY),#max
       max(p$MAX)+0.5,
       border = "white",
       col=col.p[2],
       density=100)
  rect(min(p[which(p$PHASE==15),]$DOY),#min
       0,
       min(p[which(p$PHASE==18),]$DOY),#max
       max(p$MAX)+0.5,
       border = "white",
       col=col.p[3],
       density=100)
  rect(min(p[which(p$PHASE==18),]$DOY),#min
       0,
       min(p[which(p$PHASE==19),]$DOY),#max
       max(p$MAX)+0.5,
       border = "white",
       col=col.p[4],
       density=100)
  rect(min(p[which(p$PHASE==19),]$DOY),#min
       0,
       min(p[which(p$PHASE==21),]$DOY),#max
       max(p$MAX)+0.5,
       border = "white",
       col=col.p[5],
       density=100)
  rect(min(p[which(p$PHASE==21),]$DOY),#min
       0,
       min(p[which(p$PHASE==24),]$DOY),#max
       max(p$MAX)+0.5,
       border = "white",
       col=col.p[6],
       density=100)
  rect(min(p[which(p$PHASE==24),]$DOY),#min
       0,
       max(p[which(p$PHASE==24),]$DOY),#max
       max(p$MAX)+0.5,
       border = "white",
       col=col.p[7],
       density=100)
  rect(min(p[which(p$PHASE==100),]$DOY),#min
       0,
       max(p[which(p$PHASE==100),]$DOY),#max
       max(p$MAX)+0.5,
       border = "white",
       col=col.p[8],
       density=100)
  rect(min(p[which(p$PHASE==10),]$DOY),#min
       0,
       max(p[which(p$PHASE==10),]$DOY),#max
       max(p$MAX)+0.5,
       border = "white",
       col=col.p[1],
       density=100)
  rect(max(p[which(p$PHASE==10),]$DOY),#min
       0,
       max(p$DOY),#max
       max(p$MAX)+0.5,
       border = "white",
       col=col.p[2],
       density=100)
  
  ##plot daily abline and date
  abline(v=p[DOY[i],]$DOY,col="red",lwd=3,lty=1)
  text(p[DOY[i],]$DOY,
             0.5,
             paste(jul2date2(p[DOY[i],]$DOY,2018)$day,".",jul2date2(p[DOY[i],]$DOY,2018)$month,".",sep=""),
             pos=4,
             srt=0)
  
  #plotting of months
  DIM <- data.frame("DIM"=NULL)
  for(j in 1:12){
    x <- days_in_month(j)[[1]]
    DIM <- rbind(DIM,x)
  }
  SUM <- data.frame("SUM"=NULL)
  for(j in 1:12){
    SUM <-  rbind(SUM,sum(DIM[1:j,]))
  }
  
  for(j in 1:11){
    abline(v=SUM[j,],col="grey")
  }
  abline(v=0,col="grey")
  abline(v=365,col="grey")
  text(x = SUM[[1]], y = max(p$MAX)+0.7, 
       label =  c("Jan","Feb","Mar","Apr","Mai","Jun","Jul","Aug","Sep","Okt","Nov","Dez"), 
       pos = 2, 
       cex = 0.8, 
       col = "grey")
  
  
  ##plot heavy rain events
  points(p$DOY,
         p$MAX,
         col="blue",pch=19,cex=1)
 
  #Legend
  legend("bottomright", title=expression(paste(bold(Phase))),
         legend=c(paste(CT.P.Legend),"Brache"),
         fill=col.p,bty="n")
  legend("topright",
         title=expression(paste(bold(Bodenbedeckung))),
         legend=c(as.expression(bquote({"Flächenanteil [%]"} == .((round(p[DOY[i],]$SC,1))*100)))),
         text.col=c(1,1),
         cex=1,
         bty = "n")
  setTxtProgressBar(pb, i)
}
}
}
if(CROP==215){
pb <- txtProgressBar(min=1, max=length(DOY), style=3)
for(i in 1:length(DOY)){
  #Maize
  {
    #par(mar=c(5.1, 5.1, 1, 1))
    #color schema
    CT.P <- c(10,12,67,5,65,19,20,21,24)
    CT.P.Legend <- c("Bestellung","Auflaufen","Längenwachstum","Blüte","Fahnenschieben","Milchreife","Gelbreife","Ernte")
    col.p <- c("#3288BD", #10
               "#e5f4e3", #12
               "#99c693", #67
               "#66C2A5", #65
               "#519b84", #5
               "#feecb9", #19
               "#fee08b", #20
               "#FDAE61", #21
               "#F46D43", #24
               "#DC143C",#100,
               "#8B4513") #0
    p <- read.csv2(file.path(W.DIR,IN.DIR,V.DYN))
    
    plot(p$DOY,p$MAX,
         xaxt="n",
         ylim=c(0.5,max(p$MAX)+0.7),
         xlim=c(min(p$DOY),max(p$DOY)+80),
         ylab=expression(italic(SNI),sep=""),
         xlab="Kalendertag",
         cex.lab=1.2,
         col="white")
         #main=paste("Datum = ",jul2date2(p[DOY[i],]$DOY,2018)$day,".",jul2date2(p[DOY[i],]$DOY,2018)$month,".",sep="")
         #main="Mais")
    #plot axis
    x1 <- seq(min(p$DOY),max(p$DOY),10)
    x2 <- seq(min(p$DOY),max(p$DOY),2)
    axis(1, at=x2, col.tick="grey", las=1,labels=FALSE,cex=1.2)
    axis(1, at=x1, col.axis="black", las=2,cex=1.2)
    #plot phenological phases  
    #P10
    rect(min(p$DOY),#min
         0,
         min(p[which(p$PHASE==10),]$DOY),#max
         max(p$MAX)+0.5,
         border = "white",
         col=col.p[11],
         density=100)
    rect(min(p[which(p$PHASE==10),]$DOY),#min
         0,
         min(p[which(p$PHASE==12),]$DOY),#max
         max(p$MAX)+0.5,
         border = "white",
         col=col.p[1],
         density=100)
    rect(min(p[which(p$PHASE==12),]$DOY),#min
         0,
         min(p[which(p$PHASE==67),]$DOY),#max
         max(p$MAX)+0.5,
         border = "white",
         col=col.p[2],
         density=100)
    rect(min(p[which(p$PHASE==67),]$DOY),#min
         0,
         min(p[which(p$PHASE==65),]$DOY),#max
         max(p$MAX)+0.5,
         border = "white",
         col=col.p[3],
         density=100)
    rect(min(p[which(p$PHASE==65),]$DOY),#min
         0,
         min(p[which(p$PHASE==5),]$DOY),#max
         max(p$MAX)+0.5,
         border = "white",
         col=col.p[4],
         density=100)
    rect(min(p[which(p$PHASE==5),]$DOY),#min
         0,
         min(p[which(p$PHASE==19),]$DOY),#max
         max(p$MAX)+0.5,
         border = "white",
         col=col.p[5],
         density=100)
    rect(min(p[which(p$PHASE==19),]$DOY),#min
         0,
         min(p[which(p$PHASE==20),]$DOY),#max
         max(p$MAX)+0.5,
         border = "white",
         col=col.p[6],
         density=100)
    rect(min(p[which(p$PHASE==20),]$DOY),#min
         0,
         min(p[which(p$PHASE==21),]$DOY),#max
         max(p$MAX)+0.5,
         border = "white",
         col=col.p[7],
         density=100)
    rect(min(p[which(p$PHASE==21),]$DOY),#min
         0,
         min(p[which(p$PHASE==24),]$DOY),#max
         max(p$MAX)+0.5,
         border = "white",
         col=col.p[8],
         density=100)
    rect(min(p[which(p$PHASE==24),]$DOY),#min
         0,
         max(p[which(p$PHASE==24),]$DOY),#max
         max(p$MAX)+0.5,
         border = "white",
         col=col.p[9],
         density=100)
    rect(max(p[which(p$PHASE==24),]$DOY),#min
         0,
         max(p$DOY),#max
         max(p$MAX)+0.5,
         border = "white",
         col=col.p[10],
         density=100)
    
    
    #plotting of months
    DIM <- data.frame("DIM"=NULL)
    for(j in 1:12){
      x <- days_in_month(j)[[1]]
      DIM <- rbind(DIM,x)
    }
    SUM <- data.frame("SUM"=NULL)
    for(j in 1:12){
      SUM <-  rbind(SUM,sum(DIM[1:j,]))
    }
    
    for(j in 1:11){
      abline(v=SUM[j,],col="grey")
    }
    abline(v=0,col="grey")
    abline(v=365,col="grey")
    text(x = SUM[[1]], y = max(p$MAX)+0.7, 
         label =  c("Jan","Feb","Mar","Apr","Mai","Jun","Jul","Aug","Sep","Okt","Nov","Dez"), 
         pos = 2, 
         cex = 0.8, 
         col = "grey")
    
    
    ##plot daily abline and date
    abline(v=p[DOY[i],]$DOY,col="red",lwd=3,lty=1)
    text(p[DOY[i],]$DOY,
         0.5,
         paste(jul2date2(p[DOY[i],]$DOY,2018)$day,".",jul2date2(p[DOY[i],]$DOY,2018)$month,".",sep=""),
         pos=4,
         srt=0)
    
    ##Plot heavy rain events
    points(p$DOY,
           p$MAX,
           col="blue",pch=19,cex=0.7)
    
    
    p$LS <- quantile(ls)[[4]]
    p$K <- quantile(k)[[4]]
    p$SL1 <- p$LS*p$K
    p$SL2 <- p$SL1*(1-p$SC)*(p$MAX)
    
    #Legend
    legend("bottomright", title=expression(paste(bold(Phase))),
           legend=c(paste(CT.P.Legend),"Saatbett","Brache"),
           fill=col.p,bty="n")
    legend("topright",
           title=expression(paste(bold(Bodenbedeckung))),
           legend=c(as.expression(bquote({"Flächenanteil [%]"} == .((round(p[DOY[i],]$SC,1))*100)))),
           text.col=c(1,1),
           cex=1,
           bty = "n")
  } 
setTxtProgressBar(pb, i)
}
}
}

