rm(list = ls())
library(rstudioapi)
library(openxlsx)
library(R2OpenBUGS)
currentPath <- getSourceEditorContext()$path
charLocations <- gregexpr('/',currentPath)[[1]]
currentPath <- substring(currentPath,1,charLocations[length(charLocations)]-1)
setwd(currentPath)

##==== Load data ====##
dataMatrix <- read.xlsx('FilteredData.xlsx',sheet = 1)
Size <- as.numeric(nrow(dataMatrix))
TreeID <- dataMatrix$TreeID
TreeNum <- length(unique(dataMatrix$TreeID))
SpID <- as.numeric()
for(i in seq(1,TreeNum)){
  selectIDs <- which(dataMatrix$TreeID == i)
  SpID <- c(SpID,dataMatrix$SpeciesID[selectIDs[1]])
}
SpNum <- length(unique(dataMatrix$SpeciesID))
logRadius <- log(dataMatrix$Radius*200)
logMaxPathLength <- log(dataMatrix$MaxPathLength)
logVolume <- log(dataMatrix$Volume/1000)
logTipNum <- log(dataMatrix$TipNum)
R <- diag(1,3,3)
data <- list('Size','TreeID','TreeNum','SpID','SpNum','logRadius','logMaxPathLength','logVolume','logTipNum','R')
##=== Load data ====##

##=== Load inits ====##
set.seed(1)
inits <- function(){
  list(muC1 = rnorm(1,0,1),
       muC2 = rnorm(1,0,1),
       muC3 = rnorm(1,0,1),
       sigmaC1 = 0.01,
       sigmaC2 = 0.01,
       sigmaC3 = 0.01,
       sigmaC4 = 0.01,
       sigmaC5 = 0.01,
       sigmaC6 = 0.01,
       muEta = rnorm(1,0,1),
       muFai = rnorm(1,0,1),
       muLambda = rnorm(1,0,1),
       sigmaEta1 = 0.01,
       sigmaFai1 = 0.01,
       sigmaLambda1 = 0.01,
       sigmaEta2 = 0.01,
       sigmaFai2 = 0.01,
       sigmaLambda2 = 0.01)
}
##=== Load inits ====##

resUnconstrained <- bugs(data,inits,
                         parameters.to.save = c('C1Tree','C2Tree','C3Tree','C1Species','C2Species','C3Species','muC1','muC2','muC3',
                                                'etaTree','faiTree','lambdaTree','etaSpecies','faiSpecies','lambdaSpecies','muEta','muFai','muLambda'),
                         model.file = 'UnconstrainedModel',
                         n.chains = 3,n.iter = 50000,n.burnin = 30000,debug = TRUE)
save(list = c('resUnconstrained'),file = 'UnconstrainedModel_Result.RData')

##==== Convergence ====##
library(coda)
X <- resUnconstrained$sims.array
X1 <- X[,1,'muC1']
X2 <- X[,2,'muC1']
X3 <- X[,3,'muC1']
Y <- mcmc.list(as.mcmc(X1),as.mcmc(X2),as.mcmc(X3))
gelman.diag(Y)
##==== Convergence ====##