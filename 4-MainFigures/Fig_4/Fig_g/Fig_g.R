rm(list = ls())
library(rstudioapi)
library(openxlsx)
library(ggplot2)
currentPath <- getSourceEditorContext()$path
charLocations <- gregexpr('/',currentPath)[[1]]
currentPath <- substring(currentPath,1,charLocations[length(charLocations)]-1)
setwd(currentPath)

##==== Function ====##
theme_custom <- function(){
  myTheme <- theme(panel.background = element_rect(fill = 'white',color = 'black',linewidth = 0.7),
                   panel.grid = element_blank(),
                   plot.margin = margin(3,3,3,3),
                   plot.background = element_blank(),
                   legend.position = 'none',
                   axis.ticks = element_line(linewidth = 0.3),
                   axis.ticks.length = unit(-0.15,'lines'),
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 8,margin = margin(0,3,0,0),color = '#000000'),
                   axis.text.x = element_text(size = 8,margin = margin(3,0,0,0),color = '#000000'))
  return(myTheme)
}
##==== Function ====##

dataMatrix <- read.xlsx('AllData.xlsx',sheet = 1)
dataMatrix$DiffEta <- dataMatrix$Eta - dataMatrix$B/dataMatrix$A
dataMatrix$DiffFai <- dataMatrix$Fai - dataMatrix$B/dataMatrix$A - 2
dataMatrix$DiffLambda <- dataMatrix$Lambda - 1/dataMatrix$A

##==== Draw ====##
# DiffEta ~ CDI
lmModel <- lm(DiffEta ~ CDI,data = dataMatrix)
tempX <- seq(min(dataMatrix$CDI),max(dataMatrix$CDI),length.out = 100)
tempY <- as.numeric(coef(lmModel)[1]) + as.numeric(coef(lmModel)[2])*tempX
lineMatrix <- data.frame(X = tempX,Y = tempY)
Fig <- ggplot()+
  geom_point(data = dataMatrix,mapping = aes(x = CDI,y = DiffEta,fill = factor(SiteID)),
             shape = 21,size = 1.1,stroke = 0.2,color = '#000000')+
  geom_line(data = lineMatrix,mapping = aes(x = X,y = Y),linewidth = 0.7,color = '#000000')+
  scale_fill_manual(values = c('#5772be','#6cab48','#cce798','#a8ecff','#c29c75'))+
  scale_x_continuous(limits = c(-0.7,11),breaks = seq(0,10,2),labels = c('0','2','4','6','8','10'),expand = c(0,0))+
  scale_y_continuous(limits = c(-1.5,0.8),breaks = seq(-1.5,0.5,0.5),labels = c('-1.5','-1.0','-0.5','0','0.5'),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_g_1.pdf'
pdf(file = outputFileName,width = 1.865,height = 1)
print(Fig)
dev.off()
# DiffFai ~ CDI
lmModel <- lm(DiffFai ~ CDI,data = dataMatrix)
tempX <- seq(min(dataMatrix$CDI),max(dataMatrix$CDI),length.out = 100)
tempY <- as.numeric(coef(lmModel)[1]) + as.numeric(coef(lmModel)[2])*tempX
lineMatrix <- data.frame(X = tempX,Y = tempY)
Fig <- ggplot()+
  geom_point(data = dataMatrix,mapping = aes(x = CDI,y = DiffFai,fill = factor(SiteID)),
             shape = 21,size = 1.1,stroke = 0.2,color = '#000000')+
  geom_line(data = lineMatrix,mapping = aes(x = X,y = Y),linewidth = 0.7,color = '#000000')+
  scale_fill_manual(values = c('#5772be','#6cab48','#cce798','#a8ecff','#c29c75'))+
  scale_x_continuous(limits = c(-0.7,11),breaks = seq(0,10,2),labels = c('0','2','4','6','8','10'),expand = c(0,0))+
  scale_y_continuous(limits = c(-2,0.4),breaks = seq(-2.0,0,1),labels = c('-2.0','-1.0','0'),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_g_2.pdf'
pdf(file = outputFileName,width = 1.865,height = 1)
print(Fig)
dev.off()
# DiffLambda ~ CDI
lmModel <- lm(DiffLambda ~ CDI,data = dataMatrix)
tempX <- seq(min(dataMatrix$CDI),max(dataMatrix$CDI),length.out = 100)
tempY <- as.numeric(coef(lmModel)[1]) + as.numeric(coef(lmModel)[2])*tempX
lineMatrix <- data.frame(X = tempX,Y = tempY)
Fig <- ggplot()+
  geom_point(data = dataMatrix,mapping = aes(x = CDI,y = DiffLambda,fill = factor(SiteID)),
             shape = 21,size = 1.1,stroke = 0.2,color = '#000000')+
  geom_line(data = lineMatrix,mapping = aes(x = X,y = Y),linewidth = 0.7,color = '#000000')+
  scale_fill_manual(values = c('#5772be','#6cab48','#cce798','#a8ecff','#c29c75'))+
  scale_x_continuous(limits = c(-0.7,11),breaks = seq(0,10,2),labels = c('0','2','4','6','8','10'),expand = c(0,0))+
  scale_y_continuous(limits = c(-2.7,0.8),breaks = seq(-2.0,0,1),labels = c('-2.0','-1.0','0'),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_g_3.pdf'
pdf(file = outputFileName,width = 1.865,height = 1)
print(Fig)
dev.off()
##==== Draw ====##