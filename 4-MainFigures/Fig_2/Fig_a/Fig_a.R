rm(list = ls())
library(rstudioapi)
library(openxlsx)
library(ggplot2)
library(tidybayes)
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
                   axis.ticks.length = unit(-0.15,'lines'),
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 8,margin = margin(0,3,0,0),color = '#000000'),
                   axis.text.x = element_text(size = 8,margin = margin(3,0,0,0),color = '#000000'))
  return(myTheme)
}
##==== Function ====##

##==== WBE-Eta ====##
dataMatrix <- read.xlsx('../Data/ExponentConsistance.xlsx',sheet = 1)
pointMatrix <- data.frame()
for(i in seq(1,nrow(dataMatrix))){
  tempCenterX <- (as.numeric(dataMatrix$UnconstrainedEtaDown[i]) + as.numeric(dataMatrix$UnconstrainedEtaUp[i]))/2
  tempCenterY <- 2/3
  # Boundary
  tempRadius <- tempCenterX - as.numeric(dataMatrix$UnconstrainedEtaDown[i])
  for(j in seq(1,100)){
    tempX <- tempCenterX + tempRadius*cos(j*360/100/180*pi)
    tempY <- tempCenterY + tempRadius*sin(j*360/100/180*pi)
    tempMatrix <- data.frame(X = tempX,Y = tempY,ID = 2*(i-1)+1,Class = 1)
    pointMatrix <- rbind(pointMatrix,tempMatrix)
  }
  # Center
  tempRadius <- 0.015
  for(j in seq(1,100)){
    tempX <- tempCenterX + tempRadius*cos(j*360/100/180*pi)
    tempY <- tempCenterY + tempRadius*sin(j*360/100/180*pi)
    tempMatrix <- data.frame(X = tempX,Y = tempY,ID = 2*i,Class = 2)
    pointMatrix <- rbind(pointMatrix,tempMatrix)
  }
}
lineMatrix <- data.frame(X = seq(0,2,length.out = 100),Y = seq(0,2,length.out = 100))
# Draw
Fig <- ggplot()+
  geom_line(data = lineMatrix,mapping = aes(x = X,y = Y),linetype = 'dashed',linewidth = 0.6)+
  geom_hline(mapping = aes(yintercept = 2/3),linewidth = 0.6,color = '#c10830')+
  geom_vline(mapping = aes(xintercept = mean(pointMatrix$X)),linewidth = 0.6,color = '#c10830')+
  geom_polygon(data = pointMatrix,mapping = aes(x = X,y = Y,group = ID,fill = as.factor(Class),alpha = as.factor(Class)),
               linewidth = 0.1,color = '#000000')+
  scale_fill_manual(values = c('#56ae36','#000000'))+
  scale_alpha_manual(values = c(0.5,1))+
  scale_x_continuous(limits = c(0,2),breaks = seq(0,2,0.5),labels = c('0','0.5','1.0','1.5','2.0'),expand = c(0,0))+
  scale_y_continuous(limits = c(0,2),breaks = seq(0,2,0.5),labels = c('0','0.5','1.0','1.5','2.0'),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_a-1.pdf'
pdf(file <- outputFileName,width = 1.85,height = 1.75)
print(Fig)
dev.off()
##==== WBE-Eta ====##

##==== Symmetry-Eta ====##
dataMatrix <- read.xlsx('../Data/ExponentConsistance.xlsx',sheet = 1)
pointMatrix <- data.frame()
for(i in seq(1,nrow(dataMatrix))){
  tempCenterX <- (as.numeric(dataMatrix$UnconstrainedEtaDown[i]) + as.numeric(dataMatrix$UnconstrainedEtaUp[i]))/2
  tempCenterY <- as.numeric(dataMatrix$GWBEEta[i])
  # Boundary
  tempRadius <- tempCenterX - as.numeric(dataMatrix$UnconstrainedEtaDown[i])
  for(j in seq(1,100)){
    tempX <- tempCenterX + tempRadius*cos(j*360/100/180*pi)
    tempY <- tempCenterY + tempRadius*sin(j*360/100/180*pi)
    tempMatrix <- data.frame(X = tempX,Y = tempY,ID = 2*(i-1)+1,Class = 1)
    pointMatrix <- rbind(pointMatrix,tempMatrix)
  }
  # Center
  tempRadius <- 0.0225
  for(j in seq(1,100)){
    tempX <- tempCenterX + tempRadius*cos(j*360/100/180*pi)
    tempY <- tempCenterY + tempRadius*sin(j*360/100/180*pi)
    tempMatrix <- data.frame(X = tempX,Y = tempY,ID = 2*i,Class = 2)
    pointMatrix <- rbind(pointMatrix,tempMatrix)
  }
}
lineMatrix <- data.frame(X = seq(0,3,length.out = 100),Y = seq(0,3,length.out = 100))
# Draw
Fig <- ggplot()+
  geom_line(data = lineMatrix,mapping = aes(x = X,y = Y),linetype = 'dashed',linewidth = 0.6)+
  geom_hline(mapping = aes(yintercept = mean(pointMatrix$Y)),linewidth = 0.6,color = '#c10830')+
  geom_vline(mapping = aes(xintercept = mean(pointMatrix$X)),linewidth = 0.6,color = '#c10830')+
  geom_polygon(data = pointMatrix,mapping = aes(x = X,y = Y,group = ID,fill = as.factor(Class),alpha = as.factor(Class)),
               linewidth = 0.1,color = '#000000')+
  scale_fill_manual(values = c('#6a8dcd','#000000'))+
  scale_alpha_manual(values = c(0.5,1))+
  scale_x_continuous(limits = c(0,3),breaks = seq(0,3,1),labels = c('0','1.0','2.0','3.0'),expand = c(0,0))+
  scale_y_continuous(limits = c(0,3),breaks = seq(0,3,1),labels = c('0','1.0','2.0','3.0'),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_a-2.pdf'
pdf(file <- outputFileName,width = 1.85,height = 1.75)
print(Fig)
dev.off()
##==== Symmetry-Eta ====##

##==== Asymmetry-Eta ====##
dataMatrix <- read.xlsx('../Data/ExponentConsistance.xlsx',sheet = 1)
pointMatrix <- data.frame()
for(i in seq(1,nrow(dataMatrix))){
  tempCenterX <- (as.numeric(dataMatrix$UnconstrainedEtaDown[i]) + as.numeric(dataMatrix$UnconstrainedEtaUp[i]))/2
  tempCenterY <- as.numeric(dataMatrix$AsyGWBEEta[i])
  # Boundary
  tempRadius <- tempCenterX - as.numeric(dataMatrix$UnconstrainedEtaDown[i])
  for(j in seq(1,100)){
    tempX <- tempCenterX + tempRadius*cos(j*360/100/180*pi)
    tempY <- tempCenterY + tempRadius*sin(j*360/100/180*pi)
    tempMatrix <- data.frame(X = tempX,Y = tempY,ID = 2*(i-1)+1,Class = 1)
    pointMatrix <- rbind(pointMatrix,tempMatrix)
  }
  # Center
  tempRadius <- 0.0225
  for(j in seq(1,100)){
    tempX <- tempCenterX + tempRadius*cos(j*360/100/180*pi)
    tempY <- tempCenterY + tempRadius*sin(j*360/100/180*pi)
    tempMatrix <- data.frame(X = tempX,Y = tempY,ID = 2*i,Class = 2)
    pointMatrix <- rbind(pointMatrix,tempMatrix)
  }
}
lineMatrix <- data.frame(X = seq(0,3,length.out = 100),Y = seq(0,3,length.out = 100))
# Draw
Fig <- ggplot()+
  geom_line(data = lineMatrix,mapping = aes(x = X,y = Y),linetype = 'dashed',linewidth = 0.6)+
  geom_hline(mapping = aes(yintercept = mean(pointMatrix$Y)),linewidth = 0.6,color = '#c10830')+
  geom_vline(mapping = aes(xintercept = mean(pointMatrix$X)),linewidth = 0.6,color = '#c10830')+
  geom_polygon(data = pointMatrix,mapping = aes(x = X,y = Y,group = ID,fill = as.factor(Class),alpha = as.factor(Class)),
               linewidth = 0.1,color = '#000000')+
  scale_fill_manual(values = c('#fcdb54','#000000'))+
  scale_alpha_manual(values = c(0.5,1))+
  scale_x_continuous(limits = c(0,3),breaks = seq(0,3,1),labels = c('0','1.0','2.0','3.0'),expand = c(0,0))+
  scale_y_continuous(limits = c(0,3),breaks = seq(0,3,1),labels = c('0','1.0','2.0','3.0'),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_a-3.pdf'
pdf(file <- outputFileName,width = 1.85,height = 1.75)
print(Fig)
dev.off()
##==== Asymmetry-Eta ====##