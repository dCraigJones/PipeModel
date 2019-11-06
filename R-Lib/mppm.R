# PF <- function(GPD) {
#   MGD <- GPD/1e6
#   
#   PF <- (18+sqrt(10*MGD))/(4+sqrt(10*MGD))
#   
#   return(PF)
#   
# }
# 
# PHF <- function(GPD) {
#   PF <- PF(GPD)
#   PHF <- GPD*PF/1440
#   
#   return(PHF)
#   
# }
# 
# ke <- function(D,C){return(10.44/C^1.85/D^4.87)}

Assign_ADF_to_Pipe_From_Basin <- function(n=3){
  for(i in 1:length(Basin[,1])) {
    #print(i, DSnode)
    DSnode <- Basin[i,2]
    while (DSnode > 0) {
      Pipe[DSnode, 7] <<- Pipe[DSnode, 7] + Basin[i,n]
      DSnode <- Pipe[DSnode,2]
    }
  }
}

Calc_Pipe_Hydraulics <- function(){
  for(i in 1:length(Pipe[,1])) {
    Pipe[i,6] <<- Pipe[i,3]*ke(Pipe[i,4],Pipe[i,5])
    Pipe[i,8] <<- round(PF(Pipe[i,7]),2)
    Pipe[i,9] <<- round(PHF(Pipe[i,7]),0)
    Pipe[i,10] <<- round(0.408*Pipe[i,9]/Pipe[i,4]^2,1)
    Pipe[i,11] <<- round(Pipe[i,6]*Pipe[i,9]^1.85,2)
    Pipe[i,12] <<- round(Pipe[i,11]/Pipe[i,3]*1000,2)
  }
}


GetP <- function(DS.Node) {
  for(i in 1:length(Pipe[,1])) {
    #print(c(i, DS.Node))
    if((Pipe[i,2]==DS.Node) && !(is.na(Pipe[i,2]))){#} && (Pipe[i,4]<99) && !(is.na(Pipe[17,2]))) {
      
#       DS.Pressure <- Pipe[DS.Node, 13]
#       
#       if(Pipe[i,4]>90) {DS.Pressure <<- 0}
# 
#       Pipe[i,13] <<- Pipe[i,11] + Pipe[DS.Node,14] + DS.Pressure
      
      ifelse(Pipe[i,4]==99,
          (Pipe[i,13] <<- Pipe[i,11] ),#♦+ Pipe[DS.Node,14] + 0),
          (Pipe[i,13] <<- Pipe[i,11] + Pipe[DS.Node,13])) #+ Pipe[DS.Node,14] + Pipe[DS.Node,13]))
      GetP(i)
    }    
  }
}


Add.Static <- function() {
  for(i in 1:length(Pipe[,1])) {
    Pipe[i,12] <<- Pipe[i,12] + (Pipe[1,13]-Pipe[i,13])
  }    
  
}

Reset_Pipe <- function() {Pipe[,6:13] <<- 0}


Run.Pipe.Model <- function() {
  PPipe <- NULL
  VPipe <- NULL
  for (i in 3:(length(Basin[1,]))) {
    Reset_Pipe()
    Assign_ADF_to_Pipe_From_Basin(i)
    Calc_Pipe_Hydraulics()
    GetP(1)
    #Add.Static()
    
    PPipe <- cbind(PPipe,Pipe[,13])
    VPipe <- cbind(VPipe,Pipe[,10])
  }
  
#  PPipe <<- PPipe
   colnames(PPipe) <- c(2018, seq(2020,2070,10))
   rownames(PPipe) <- Pipe[,1]
   colnames(VPipe) <- c(2018, seq(2020,2070,10))
   rownames(VPipe) <- Pipe[,1]
   
  P <- matrix(rep(0,
                  (length(PPipe[,1]))*length(2018:2070)
  ), nrow=length(PPipe[,1]))
  V <- P
  
  for (i in 1:length(PPipe[,1])) {
    P[i,] <- approx(c(2018, seq(2020,2070,10)),PPipe[i,], xout=2018:2070)$y
    V[i,] <- approx(c(2018, seq(2020,2070,10)),VPipe[i,], xout=2018:2070)$y 
  }
   
   colnames(P) <- 2018:2070
   rownames(P) <- Pipe[,1]
   colnames(V) <- 2018:2070
   rownames(V) <- Pipe[,1]
   
   PPipe <<- P
   VPipe <<- V
}

Draw.Pipe.Capacity.Curve <- function(rowindex, PLOT=FALSE) {
  
  Con <- matrix(c(seq(2018,2040,1),rep(60,23)), ncol=2, byrow=F)
  Cap <- matrix(c(seq(2018,2040,1),rep(20,23)), ncol=2, byrow=F)
  
  if (PLOT == TRUE) {
    #png(file=paste0(rowindex," - ", rownames(PPipe)[rowindex],".png"), width=900, height=500)
    pdf(file=paste0(rowindex," - ", rownames(PPipe)[rowindex],".pdf"), width=254, height=190 , paper="USr")
    #jpeg(file=paste0(rowindex," - ", rownames(PPipe)[rowindex],".jpg"), width=700, height=400, quality=100)
  }
  
  #barplot(Con[,2], space=0, col="lightyellow", axes=F, ylim=c(0,100))
  #barplot(Cap[,2], space=0, add=T, axes=F, col="lemonchiffon3")
  
  par(xpd=FALSE)
  par(mar=c(3,4,5,4)) #Btm, Left, Top, Right
  
  plot(0,ylim=c(0,100), xlim=c(1,23), xaxs="i", yaxs="i", type="n", axes=F, xlab="", ylab="")
  
  xx <- c(-1,24,24,-1,-1)
  yy <- c(60,60,110,110,60)
  
  polygon(xx,yy, col=paste0(JEA.Blue,"22"), border=paste0(JEA.Blue,"55"))
  #polygon(xx,yy, angle=-45, col=paste0(JEA.Blue,"55"), border=NA, density=5)
  polygon(xx,yy, angle=45, col=paste0(JEA.Blue,"55"), border=NA, density=5)
  
  yy <- c(20,20,-10,-10,20)
  
  polygon(xx,yy, col=paste0(JEA.Green,22), border=paste0(JEA.Green,"22"))
  polygon(xx,yy, angle=45, col=paste0(JEA.Green,55), border=NA, density=5)
  #polygon(xx,yy, angle=-45, col=paste0(JEA.Green,55), border=NA, density=5)
  
  abline(v=c(5,10,20), lwd=2, col=paste0(JEA.Grey,"55"), lty=2)
  
  #barplot(Con[,2], space=0, col="lavenderblush4", axes=F, ylim=c(0,100))
  #barplot(Cap[,2], space=0, add=T, axes=F, col="lemonchiffon3")
  
  axis(1, at=c(1,seq(2,23,5)), labels=c(2018,seq(2020,2040,5)), lwd=0, las=2, cex=1.5, line=0)
  
  axis(2, line=0, lwd=2, lwd.ticks=1, at=seq(0,100,20), labels=seq(0,100,20), las=1)
  mtext("Manifold Pressure (PSI)",2, line=2)
  
  axis(4, line=0, lwd=2, lwd.ticks=1, at=seq(0,100,20), labels=seq(0,10,2), las=1)
  mtext("Pipeline Velocity (FPS)",4, line=2)
  
  lines(c(-1,29),c(0,0), lwd=2)
  
  box(lwd=2)
  
  #abline(h=0, lwd=2)
  
  title(rownames(PPipe)[rowindex])
  mtext("This document is for JEA planning purposes only and should not be used for design.",side=1, line=4, cex=0.50, font=3)
  mtext(paste0(Sys.Date(), " (", rowindex,")"),side=3, line=2, cex=0.5, adj=1)
  
  
  lines(PPipe[rowindex,]/2.31-0.75, type="b", lwd=2, pch=17, col=rgb(109/255,110/255,113/255,0.40), cex=2.25)
  lines(PPipe[rowindex,]/2.31, type="b", lwd=4, pch=17, col=JEA.Blue, cex=2.0)
  
  lines(VPipe[rowindex,]*10-0.75, type="b", lwd=2, pch=15, col=rgb(109/255,110/255,113/255,0.40), cex=2.25)
  lines(VPipe[rowindex,]*10, type="b", lwd=4, pch=15, col=JEA.Green, cex=2.0)
  
  par(xpd=TRUE)
  
  legend("top"
         , c("Maximum Operating Pressure", "Minimum Pipeline Velocity", "Operating Pressure (PHF)", "Pipeline Velocity (PHF)")
         #, c("Design/Construction", "Permitted Capacity", "Projected WW Flow (PHF)", "Historical WW Flow (PHF)")
         , ncol=2
         #, fill=c("lightyellow","lemonchiffon3",NA,NA)
         , fill=c(paste0(JEA.Blue,55),paste0(JEA.Green,55),NA,NA)
         , border=c(paste0(JEA.Blue,22),paste0(JEA.Green,22), "white", "white")
         , pch=c(NA,NA,17,15)
         , col=c(NA,NA,JEA.Blue, JEA.Green)
         , lwd=c(NA,NA,2,2)
         , cex=0.70
         , pt.cex = c(1.5,1.5,1.5,1.5)
         , x.intersp = 1.15
         , bty="n"
         #, inset=-0.175
         , inset=-0.125
         , seg.len=c(2,2,2,2)
  )
  
  if (PLOT == TRUE) {
    dev.off()
  }
}

Draw.All.Pipe.Capacity.Curves <- function() {
  for (i in 2:length(Pipe[,1])) {
    Draw.Pipe.Capacity.Curve(i)
  }
}

PDF.All.Pipe.Capacity.Curves <- function() {
  for (i in 2:length(Pipe[,1])) {
    Draw.Pipe.Capacity.Curve(i, TRUE)
  }
}

n <- (1:length(PPipe[,1]))[apply(PPipe,1,max)>(60*2.31)]
