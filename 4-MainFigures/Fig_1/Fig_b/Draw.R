rm(list = ls())
library(rstudioapi)
library(openxlsx)
library(ggplot2)
library(tidybayes)
library(see)
currentPath <- getSourceEditorContext()$path
charLocations <- gregexpr('/',currentPath)[[1]]
currentPath <- substring(currentPath,1,charLocations[length(charLocations)]-1)
setwd(currentPath)

##==== Function ====##
theme_custom <- function(){
  myTheme <- theme(panel.background = element_rect(fill = 'white',color = 'black',linewidth = 0.7),
                   panel.grid = element_blank(),
                   legend.position = 'none',
                   plot.margin = margin(3,3,3,3),
                   plot.background = element_blank(),
                   axis.ticks = element_line(linewidth = 0.3),
                   axis.ticks.length.x.bottom = unit(-0.12,'lines'),
                   axis.ticks.length.y.left = unit(-0.12,'lines'),
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 8,margin = margin(0,2,0,0),color = '#000000'),
                   axis.text.x = element_text(size = 8,margin = margin(2,0,0,0),color = '#000000'))
  return(myTheme)
}
##==== Function ====##

dataMatrix1 <- read.xlsx('./Data/FilteredData_CBS.xlsx',sheet = 1)
dataMatrix2 <- read.xlsx('./Data/FilteredData_QY.xlsx',sheet = 1)
dataMatrix3 <- read.xlsx('./Data/FilteredData_SNJ.xlsx',sheet = 1)
dataMatrix4 <- read.xlsx('./Data/FilteredData_GTS.xlsx',sheet = 1)
dataMatrix5 <- read.xlsx('./Data/FilteredData_XSBN.xlsx',sheet = 1)
dataMatrix <- rbind(dataMatrix1,dataMatrix2,dataMatrix3,dataMatrix4,dataMatrix5)

##==== Diameter ====##
dataMatrix$Diameter <- log(dataMatrix$Radius*200)
meanValue <- mean(dataMatrix$Diameter)
# Prepare data
drawMatrix <- data.frame()
breakValues <- seq(min(dataMatrix$Diameter),max(dataMatrix$Diameter),length.out = 51)
for(i in seq(1,length(breakValues)-1)){
  tempX <- (breakValues[i] + breakValues[i+1])/2
  selectIDs <- which((dataMatrix$Diameter >= breakValues[i]) & (dataMatrix$Diameter <= breakValues[i+1]))
  tempY <- length(selectIDs)
  tempMatrix <- data.frame(X = tempX,Y = tempY)
  drawMatrix <- rbind(drawMatrix,tempMatrix)
}
# Draw
minX <- min(drawMatrix$X) - (max(drawMatrix$X)-min(drawMatrix$X))*0.08
maxX <- max(drawMatrix$X) + (max(drawMatrix$X)-min(drawMatrix$X))*0.08
minY <- min(drawMatrix$Y) - (max(drawMatrix$Y)-min(drawMatrix$Y))*0.08
maxY <- max(drawMatrix$Y) + (max(drawMatrix$Y)-min(drawMatrix$Y))*0.15
Fig <- ggplot()+
  geom_col(data = drawMatrix,mapping = aes(x = X,y = Y,fill = Y),width = breakValues[2]-breakValues[1],color = '#000000',linewidth = 0.02)+
  geom_segment(mapping = aes(x = meanValue,xend = meanValue,y = minY,yend = maxY),color = '#c62424',linewidth = 0.3)+
  scale_fill_gradient(low = '#dadef2',high = '#50609b')+
  scale_x_continuous(limits = c(minX,maxX),breaks = c(log(1),log(10),log(100)),labels = c('1','10','100'),expand = c(0,0))+
  scale_y_continuous(limits = c(minY,maxY),breaks = c(0,200,400),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_b_1.pdf'
pdf(file <- outputFileName,width = 0.9,height = 0.8)
print(Fig)
dev.off()
##==== Diameter ====##

##==== Height ====##
dataMatrix$MaxPathLength <- log(dataMatrix$MaxPathLength)
meanValue <- mean(dataMatrix$MaxPathLength)
# Prepare data
drawMatrix <- data.frame()
breakValues <- seq(min(dataMatrix$MaxPathLength),max(dataMatrix$MaxPathLength),length.out = 51)
for(i in seq(1,length(breakValues)-1)){
  tempX <- (breakValues[i] + breakValues[i+1])/2
  selectIDs <- which((dataMatrix$MaxPathLength >= breakValues[i]) & (dataMatrix$MaxPathLength <= breakValues[i+1]))
  tempY <- length(selectIDs)
  tempMatrix <- data.frame(X = tempX,Y = tempY)
  drawMatrix <- rbind(drawMatrix,tempMatrix)
}
# Draw
minX <- min(drawMatrix$X) - (max(drawMatrix$X)-min(drawMatrix$X))*0.08
maxX <- max(drawMatrix$X) + (max(drawMatrix$X)-min(drawMatrix$X))*0.08
minY <- min(drawMatrix$Y) - (max(drawMatrix$Y)-min(drawMatrix$Y))*0.08
maxY <- max(drawMatrix$Y) + (max(drawMatrix$Y)-min(drawMatrix$Y))*0.15
Fig <- ggplot()+
  geom_col(data = drawMatrix,mapping = aes(x = X,y = Y,fill = Y),width = breakValues[2]-breakValues[1],color = '#000000',linewidth = 0.02)+
  geom_segment(mapping = aes(x = meanValue,xend = meanValue,y = minY,yend = maxY),color = '#c62424',linewidth = 0.3)+
  scale_fill_gradient(low = '#fffbe1',high = '#efd725')+
  scale_x_continuous(limits = c(minX,maxX),breaks = c(log(1),log(3),log(10)),labels = c('1','3','10'),expand = c(0,0))+
  scale_y_continuous(limits = c(minY,maxY),breaks = c(0,200,400),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_b_2.pdf'
pdf(file <- outputFileName,width = 0.9,height = 0.8)
print(Fig)
dev.off()
##==== Height ====##

##==== Volume ====##
dataMatrix$Volume <- log(dataMatrix$Volume/1000)
meanValue <- mean(dataMatrix$Volume)
# Prepare data
drawMatrix <- data.frame()
breakValues <- seq(min(dataMatrix$Volume),max(dataMatrix$Volume),length.out = 51)
for(i in seq(1,length(breakValues)-1)){
  tempX <- (breakValues[i] + breakValues[i+1])/2
  selectIDs <- which((dataMatrix$Volume >= breakValues[i]) & (dataMatrix$Volume <= breakValues[i+1]))
  tempY <- length(selectIDs)
  tempMatrix <- data.frame(X = tempX,Y = tempY)
  drawMatrix <- rbind(drawMatrix,tempMatrix)
}
# Draw
minX <- min(drawMatrix$X) - (max(drawMatrix$X)-min(drawMatrix$X))*0.08
maxX <- max(drawMatrix$X) + (max(drawMatrix$X)-min(drawMatrix$X))*0.08
minY <- min(drawMatrix$Y) - (max(drawMatrix$Y)-min(drawMatrix$Y))*0.08
maxY <- max(drawMatrix$Y) + (max(drawMatrix$Y)-min(drawMatrix$Y))*0.15
Fig <- ggplot()+
  geom_col(data = drawMatrix,mapping = aes(x = X,y = Y,fill = Y),width = breakValues[2]-breakValues[1],color = '#000000',linewidth = 0.02)+
  geom_segment(mapping = aes(x = meanValue,xend = meanValue,y = minY,yend = maxY),color = '#c62424',linewidth = 0.3)+
  scale_fill_gradient(low = '#e1f7f9',high = '#12b7dd')+
  scale_x_continuous(limits = c(minX,maxX),breaks = c(log(1),log(100),log(10000)),labels = c('1','100','10000'),expand = c(0,0))+
  scale_y_continuous(limits = c(minY,maxY),breaks = c(0,200,400),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_b_3.pdf'
pdf(file <- outputFileName,width = 0.9,height = 0.8)
print(Fig)
dev.off()
##==== Volume ====##

##==== TipNum ====##
selectIDs <- which(dataMatrix$TipNum > 6)
dataMatrix <- dataMatrix[selectIDs,]
dataMatrix$TipNum <- log(dataMatrix$TipNum)
meanValue <- mean(dataMatrix$TipNum)
# Prepare data
drawMatrix <- data.frame()
breakValues <- seq(min(dataMatrix$TipNum),max(dataMatrix$TipNum),length.out = 51)
for(i in seq(1,length(breakValues)-1)){
  tempX <- (breakValues[i] + breakValues[i+1])/2
  selectIDs <- which((dataMatrix$TipNum >= breakValues[i]) & (dataMatrix$TipNum <= breakValues[i+1]))
  tempY <- length(selectIDs)
  tempMatrix <- data.frame(X = tempX,Y = tempY)
  drawMatrix <- rbind(drawMatrix,tempMatrix)
}
# Draw
minX <- min(drawMatrix$X) - (max(drawMatrix$X)-min(drawMatrix$X))*0.08
maxX <- max(drawMatrix$X) + (max(drawMatrix$X)-min(drawMatrix$X))*0.08
minY <- min(drawMatrix$Y) - (max(drawMatrix$Y)-min(drawMatrix$Y))*0.08
maxY <- max(drawMatrix$Y) + (max(drawMatrix$Y)-min(drawMatrix$Y))*0.15
Fig <- ggplot()+
  geom_col(data = drawMatrix,mapping = aes(x = X,y = Y,fill = Y),width = breakValues[2]-breakValues[1],color = '#000000',linewidth = 0.02)+
  geom_segment(mapping = aes(x = meanValue,xend = meanValue,y = minY,yend = maxY),color = '#c62424',linewidth = 0.3)+
  scale_fill_gradient(low = '#eaeaea',high = '#6c6c6c')+
  scale_x_continuous(limits = c(minX,maxX),breaks = c(log(10),log(100),log(1000)),labels = c('10','100','1000'),expand = c(0,0))+
  scale_y_continuous(limits = c(minY,maxY),breaks = c(0,400,800),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_b_4.pdf'
pdf(file <- outputFileName,width = 0.9,height = 0.8)
print(Fig)
dev.off()
##==== TipNum ====##