rm(list = ls())
library(rstudioapi)
library(stringr)
library(ggplot2)
library(openxlsx)
currentPath <- getSourceEditorContext()$path
charLocations <- gregexpr('/',currentPath)[[1]]
currentPath <- substring(currentPath,1,charLocations[length(charLocations)]-1)
setwd(currentPath)

##==== Function ====##
theme_custom <- function(){
  myTheme <- theme(panel.background = element_blank(),
                   panel.grid = element_blank(),
                   legend.position = 'none',
                   plot.margin = margin(3,3,3,3),
                   plot.background = element_blank(),
                   axis.ticks = element_line(linewidth = 0.3),
                   axis.ticks.length = unit(-0.2,'lines'),
                   axis.line.x.bottom = element_line(linewidth = 0.7),
                   axis.line.y.left = element_line(linewidth = 0.7),
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size = 8,margin = margin(0,3,0,0),color = '#000000'),
                   axis.text.x = element_text(size = 8,margin = margin(4,0,0,0),color = '#000000'))
  return(myTheme)
}
##==== Function ====##

dataMatrix <- read.xlsx('Data.xlsx',sheet = 1)

lmModel <- lm(Y ~ X,data = dataMatrix)
coeff1 <- as.numeric(coef(lmModel)[1])
coeff2 <- as.numeric(coef(lmModel)[2])
tempX <- seq(min(dataMatrix$X),max(dataMatrix$X),length.out = 100)
tempY <- coeff1 + coeff2*tempX
lineMatrix <- data.frame(X = tempX,Y = tempY)

Fig <- ggplot()+
  geom_line(data = lineMatrix,mapping = aes(x = X,y = Y),linewidth = 0.7)+
  geom_point(data = dataMatrix,mapping = aes(x = X,y = Y),
             shape = 21,size = 1.5,fill = '#627ebf',stroke = 0.3,color = '#000000')+
  scale_x_continuous(limits = c(-0.3,6.5),breaks = seq(0,6,2),expand = c(0,0))+
  scale_y_continuous(limits = c(-2,19),breaks = seq(0,15,5),expand = c(0,0))+
  theme_custom()
outputFileName <- 'BoxD.pdf'
pdf(file <- outputFileName,width = 1.85,height = 2.1)
print(Fig)
dev.off()