rm(list = ls())
library(rstudioapi)
library(openxlsx)
library(ggplot2)
library(ggpattern)
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
                   axis.text.x = element_text(size = 8,margin = margin(4,0,0,0),color = '#000000'))
  return(myTheme)
}
##==== Function ====##

dataMatrix1 <- read.csv('Data.csv')
dataMatrix1 <- dataMatrix1[,c('ParameterID','TheoryID','AbsDiff')]
dataMatrix1$Class <- 1
dataMatrix2 <- read.csv('Data_Adjustment.csv')
dataMatrix2 <- dataMatrix2[,c('ParameterID','TheoryID','AbsDiff')]
dataMatrix2$Class <- 2
dataMatrix <- rbind(dataMatrix1,dataMatrix2)
dataMatrix$X <- (dataMatrix$TheoryID-1)*2 + dataMatrix$Class
selectIDs <- which(dataMatrix$ParameterID == 2)
dataMatrix$X[selectIDs] <- dataMatrix$X[selectIDs] + 7*1
selectIDs <- which(dataMatrix$ParameterID == 3)
dataMatrix$X[selectIDs] <- dataMatrix$X[selectIDs] + 7*2

##==== Draw ====##
Fig <- ggplot()+
  geom_col_pattern(data = dataMatrix,mapping = aes(x = X,y = AbsDiff,fill = factor(TheoryID),pattern = factor(Class)),
                   pattern_density = 0.2, pattern_fill = '#333333',pattern_colour = NA,pattern_spacing = 0.04,
                   color = '#000000',linewidth = 0.2,width = 1)+
  scale_fill_manual(values = c('#56ae36','#6a8dcd','#fcdb54'))+
  scale_pattern_manual(values = c('none','stripe'))+
  scale_x_continuous(limits = c(-0.5,21.5),breaks = c(3.5,10.5,17.5),expand = c(0,0))+
  scale_y_continuous(limits = c(-0.048,1.2),breaks = c(0,0.4,0.8,1.2),labels = c('0','0.4','0.8','1.2'),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_a_1.pdf'
pdf(file <- outputFileName,width = 1.82,height = 1.2)
print(Fig)
dev.off()
##==== Draw ====##