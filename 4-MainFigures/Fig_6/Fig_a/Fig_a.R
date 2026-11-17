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

drawMatrix_1 <- read.csv('./Data/Agreement_CBS.csv')
drawMatrix_2 <- read.csv('./Data/Agreement_QY.csv')
drawMatrix_3 <- read.csv('./Data/Agreement_SNJ.csv')
drawMatrix_4 <- read.csv('./Data/Agreement_GTS.csv')
drawMatrix_5 <- read.csv('./Data/Agreement_XSBN.csv')
drawMatrix <- rbind(drawMatrix_1,drawMatrix_2,drawMatrix_3,drawMatrix_4,drawMatrix_5)

drawMatrix$X <- 0
dataGap <- 0.7
selectIDs <- which(drawMatrix$ParameterID == 1)
drawMatrix$X[selectIDs] <- drawMatrix$TheoryID[selectIDs]
selectIDs <- which(drawMatrix$ParameterID == 2)
drawMatrix$X[selectIDs] <- drawMatrix$TheoryID[selectIDs] + 4 + dataGap
selectIDs <- which(drawMatrix$ParameterID == 3)
drawMatrix$X[selectIDs] <- drawMatrix$TheoryID[selectIDs] + 8 + dataGap*2

dataGap <- 15.2
selectIDs <- which(drawMatrix$SiteID == 2)
drawMatrix$X[selectIDs] <- drawMatrix$X[selectIDs] + dataGap
selectIDs <- which(drawMatrix$SiteID == 3)
drawMatrix$X[selectIDs] <- drawMatrix$X[selectIDs] + dataGap*2
selectIDs <- which(drawMatrix$SiteID == 4)
drawMatrix$X[selectIDs] <- drawMatrix$X[selectIDs] + dataGap*3
selectIDs <- which(drawMatrix$SiteID == 5)
drawMatrix$X[selectIDs] <- drawMatrix$X[selectIDs] + dataGap*4

basicBreakValues <- c(2.5,7.2,11.9)
xbreakValues <- c(basicBreakValues,basicBreakValues+dataGap,basicBreakValues+dataGap*2,basicBreakValues+dataGap*3,basicBreakValues+dataGap*4)

Fig <- ggplot()+
  geom_vline(mapping = aes(xintercept = c(14.8,30.0,45.2,60.4)),color = '#000000',linewidth = 0.7)+
  geom_col(data = drawMatrix,mapping = aes(x = X,y = AbsDiff,fill = factor(TheoryID)),
           color = '#000000',linewidth = 0.2,width = 1)+
  scale_fill_manual(values = c('#56ae36','#6a8dcd','#fcdb54','#ab8ebe'))+
  scale_x_continuous(limits = c(-0.4,75.6),breaks = xbreakValues,expand = c(0,0))+
  scale_y_continuous(limits = c(-0.054,1.35),breaks = c(0,0.4,0.8,1.2),labels = c('0','0.4','0.8','1.2'),expand = c(0,0))+
  theme_custom()
outputFileName <- 'Fig_a.pdf'
pdf(file <- outputFileName,width = 6.1,height = 1.4)
print(Fig)
dev.off()