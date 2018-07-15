


###===========================###
#Try sensitivity analysis
###===========================###  
library(Rpreles)
library(BayesianTools)
library(sensitivity)

##
#Load dqta
##

load("B:/modeling/project/06-PrelesData/EddyCovarianceDataBorealSites.rdata")

load("B:/modeling/project/06-PrelesData/parameterRanges.rdata")

##
#Run the model
##

PAR <- s1$PAR
Tair <- s1$TAir
D <- s1$VPD
Precip <- s1$Precip
co2 <- s1$CO2
fAPAR <- s1$fAPAR

o2 <- PRELES(PAR, Tair, D, Precip, co2, fAPAR,returncols=c("GPP", "ET", "SW", "fW", "fE"), LOGFLAG=0)

##
#Morris
##

refPars <- par
parSel <- c(1:30)

likelihood <- function(x, sum = TRUE){
  x <- createMixWithDefaults(x, refPars$def, parSel)
  predicted <- PRELES(PAR, Tair, D, Precip, co2, fAPAR,
                      returncols=c("GPP", "ET", "SW", "fW", "fE"),   LOGFLAG = 0, p = x[1:30])
  predicted[,1] = 1000 * predicted[,1]
  diff <- c(predicted[,1:4] - obs[,1:4])
  llValues <- dtriangle(diff, sd = x[12], log = T) 
  if (sum == FALSE) return(llValues)
  else return(sum(llValues))
}


#prior
parSel = c(1:32)
prior <- createUniformPrior(lower = refPars$min[parSel], upper = refPars$max[parSel], best = refPars$def[parSel])

bayesianSetup <- createBayesianSetup(likelihood, prior, names = rownames(refPars)[parSel])

bayesianSetup$prior$density(refPars$def[parSel])

bayesianSetup$likelihood$density(refPars$def[parSel])

bayesianSetup$posterior$density(refPars$def[parSel])


par(mfrow=c(1,1))

morrisOut <- morris(model = bayesianSetup$posterior$density, factors = rownames(refPars[parSel, ]), r = 2000, design = list(type = "oat", levels = 5, grid.jump = 3), binf = refPars$min[parSel], bsup = refPars$max[parSel], scale = TRUE)

plot(morrisOut)


print(morrisOut)  

#Task 2.

sensitivityTarget <- function(par){
  refPars$best[par] <- parSel
  predicted <- PRELES(refPars$def[1:11], PAR)
  return(mean(predicted[,2]))
}

result.best<-numeric(nrow(refPars))
for (i in 1:nrow(refPars)){
  parSel<-refPars$best[i]
  result.best[i]<-sensitivityTarget(i)
}
result.best




