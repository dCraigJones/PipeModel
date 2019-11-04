setwd("G:/Financial Services/Corporate Planning/Planning Group/Craig/_JEA Master Plans/Southwest Basin - w LU Method/H1 - Capacity Curve Model/PipeModel")
.libPaths("G:/Delivery/Shared/ACAD/JonesDC/_Support/RWD")
source("G:/Financial Services/Corporate Planning/Hydraulic Model Files/R Library/pump_v1.R")
source("G:/Financial Services/Corporate Planning/Hydraulic Model Files/R Library/Master_Plan_Pipe_Model.R")

if(!require("igraph")) install.packages("igraph"); library(igraph)

Pipe <- read.csv("Pipe.csv", header=T, sep=",")
Basin <- read.csv("GISA.csv", header=T, sep=",")
Conn <- read.csv("connectivity.csv", header=T, sep=",")
# 
# . <- match("Brannan Field", Conn[,1])
# . <- match(Conn[.,2], Conn[,1])
# Conn[.,2] <- Pipe[17,1]
# 
# . <- match("Oakleaf", Conn[,1])
# . <- match(Conn[.,2], Conn[,1])
# Conn[.,2] <- Pipe[17,1]

load("SW.Rdata")
#Conn[64,2] <- NA
year <- c(2018, seq(2020,2040,5))

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
V(g)$label.degree <- pi/4
V(g)$label.cex <- 0.75
V(g)$label.font <- 2

E(g)$arrow.size <- 0.25

#tkid <- tkplot(g, layout=l) #tkid is the id of the tkplot that will open
#l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot

#save(l, l.old, file="SW.RData")
#dev.new(width = 1900, height = 900, unit = "px")
plot(g, layout=l)


Pipe[,2] <- match(Conn[match(Pipe[,1], Conn[,1]),2],Pipe[,1])

Pipe[1,2] <- 0


Basin[,2] <- match(Conn[match(Basin[,1],Conn[,1]),2],Pipe[,1])

Run.Pipe.Model()

n.60psi.10yr <- (1:length(PPipe[,1]))[apply(PPipe[,1:10],1,max)>(60*2.31)]
nc.60psi.10yr <- match(rownames(PPipe[n.60psi.10yr,]),Conn[,1])

n.80psi.10yr <- (1:length(PPipe[,1]))[apply(PPipe[,1:10],1,max)>(80*2.31)]
nc.80psi.10yr <- match(rownames(PPipe[n.80psi.10yr,]),Conn[,1])


#lapply(n.5yr, function(x) Draw.Pipe.Capacity.Curve(x))

V(g)[nc.60psi.10yr]$color=JEA.Orange
V(g)[nc.80psi.10yr]$color="red"


plot(g, layout=l)