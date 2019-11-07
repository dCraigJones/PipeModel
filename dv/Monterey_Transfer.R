# Front Matter ------------------------------------------------------------

if (Sys.info()["nodename"]=="JEALT1300") {
  #setwd("G:/Financial Services/Corporate Planning/Planning Group/Craig/_JEA Master Plans/Southwest Basin - w LU Method/H1 - Capacity Curve Model/PipeModel")
  .libPaths("G:/Delivery/Shared/ACAD/JonesDC/_Support/RWD")
  #source("G:/Financial Services/Corporate Planning/Hydraulic Model Files/R Library/pump_v1.R")
  library(pumpR)
  source("./R-lib/mppm.R")
  
}

if (Sys.info()["nodename"]=="LAPTOP-Q1GSUFR7") {
  library(pumpR)
  source("./R-Lib/mppm.R")
  
}

if(!require("igraph")) install.packages("igraph"); library(igraph)

library(ggraph)
library(tidygraph)


# Load Graph --------------------------------------------------------------

Pipe <- read.csv("./data/holiday/Pipe.csv", header=T, sep=",")
Basin <- read.csv("./data/holiday/GISA.csv", header=T, sep=",")
Conn <- read.csv("./data/holiday/connectivity.csv", header=T, sep=",")


load("./data/holiday/layout.Rdata")

#year <- c(2018, seq(2020,2040,5))

A <- data.frame(from=Conn[,1], to=Conn[,2], type=Conn[,3])

g <- graph_from_data_frame(A, directed=T)

V(g)$size=5
V(g)$size2=10

V(g)[A$type=="Pipe"]$color="grey95"
V(g)[A$type=="Basin"]$color="white"

V(g)[A$type=="Basin"]$frame.color=NA

V(g)[A$type=="Pipe"]$shape="circle"
V(g)[A$type=="Basin"]$shape="square"
V(g)[A$type=="Pump"]$shape="square"

#textNum <- paste0(row(A)[A$Dia<99,1])
#text <- paste0(row(A)[,1],": ", A$from)
text <- paste0(A$from)

pipeNum <- A$type=="Pipe"
textNum <- match(Conn[pipeNum,1],Pipe[,1])
text[pipeNum] <- paste0(Pipe[textNum,4], "''")

V(g)$label <- text
V(g)$label[19] <- "WWTP"
V(g)$label[16] <- "30''/36''"
V(g)$label.degree <- pi/4
V(g)$label.cex <- 0.75
V(g)$label.font <- 2

E(g)$arrow.size <- 0.25

tkid <- tkplot(g, layout=l) #tkid is the id of the tkplot that will open
l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot

#save(l, file="./data/holiday/layout.RData")
#dev.new(width = 1900, height = 900, unit = "px")
par(mar=rep(0,4))
plot(g, layout=l)


# tidygraph sandbox -------------------------------------------------------
library(tidygraph)

g_tree <- as_tbl_graph(g)
e <- g_tree %>% activate(edges) %>% as_tibble()
g_tree <- g_tree %>% activate(nodes) %>% mutate(type=c(e$type,"Outfall"))

p <- g_tree %>% activate(nodes) %>% as_tibble() %>% select(name)

p$label <- NULL

m <- match(p$name, Pipe$Pipe)
mn <- !is.na(m)
p$label[mn] <- paste0(Pipe$D_inch[m[mn]], "''")

m <- match(p$name, Basin$Basin)
mn <- !is.na(m)
p$label[mn] <- as.character(Basin$Basin[m[mn]])

g_tree <- g_tree %>% activate(nodes) %>% mutate(label=p$label)

ggraph(g_tree, layout = "nicely") + 
  geom_edge_link(arrow=arrow(length=unit(3, 'mm'))
     , end_cap=circle(5,'mm')
  ) + 
  geom_node_point(size=10
     , col="white"
  ) +
  geom_node_text(aes(label=label))

# Run Pipe Model ----------------------------------------------------------
Basin <- read.csv("./data/holiday/STPO.csv", header=T, sep=",")

Basin <- Basin[1:8,1:9]

# Assign DS_Node
Pipe[,2] <- match(Conn[match(Pipe[,1], Conn[,1]),2],Pipe[,1])
Pipe[1,2] <- 0
Basin[,2] <- match(Conn[match(Basin[,1],Conn[,1]),2],Pipe[,1])

Basin[7,2] <- 0
Basin[8,3] <- 0
 
Run.Pipe.Model()

# n.60psi.10yr <- (1:length(PPipe[,1]))[apply(PPipe[,1:10],1,max)>(60*2.31)]
# nc.60psi.10yr <- match(rownames(PPipe[n.60psi.10yr,]),Conn[,1])
# 
# n.80psi.10yr <- (1:length(PPipe[,1]))[apply(PPipe[,1:10],1,max)>(80*2.31)]
# nc.80psi.10yr <- match(rownames(PPipe[n.80psi.10yr,]),Conn[,1])
# 
# 
# #lapply(n.5yr, function(x) Draw.Pipe.Capacity.Curve(x))
# 
# V(g)[nc.60psi.10yr]$color=JEA.Orange
# V(g)[nc.80psi.10yr]$color="red"


#plot(g, layout=l)
Draw.All.Pipe.Capacity.Curves()

PPipe.old <- PPipe
VPipe.old <- VPipe

Basin[8,3:ncol(Basin)] <- 0

Run.Pipe.Model()

PPipe.curr <- PPipe
VPipe.curr <- VPipe

PPipe <- PPipe.old
VPipe <- VPipe.old

PPipe.stpo <- PPipe
VPipe.stpo <- VPipe

Draw.Pipe.Capacity.Curve(3)
lines(PPipe.curr[2,1:23]/2.31, lwd=2, col="grey50", lty=2)
lines(PPipe.stpo[2,1:23]/2.31, lwd=2, col="grey50", lty=2)

Draw.Pipe.Capacity.Curve(9)
lines(VPipe.curr[9,1:23]*10, lwd=2, col="grey50", lty=2)
lines(VPipe.stpo[9,1:23]*10, lwd=2, col="grey50", lty=2)

Draw.Pipe.Capacity.Curve(4)
lines(PPipe.curr[4,1:23]/2.31, lwd=2, col="grey50", lty=2)
lines(PPipe.stpo[4,1:23]/2.31, lwd=2, col="grey50", lty=2)

Draw.Pipe.Capacity.Curve(6)
lines(PPipe.curr[6,1:23]/2.31, lwd=2, col="grey50", lty=2)
lines(PPipe.stpo[6,1:23]/2.31, lwd=2, col="grey50", lty=2)

Draw.Pipe.Capacity.Curve(8)
lines(PPipe.curr[8,1:23]/2.31, lwd=2, col="grey50", lty=2)
lines(PPipe.stpo[8,1:23]/2.31, lwd=2, col="grey50", lty=2)
