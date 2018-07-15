devtools::install_github('MikkoPeltoniemi/Rpreles')
library(Rpreles)
?`Rpreles-package`
?Rpreles
?PRELES

## Run model with artificial inputs
CO2 <- 280:(2*380)
T=rep(18, length(CO2))
fAPAR=rep(1, length(CO2))
Precip=rep(3, length(CO2))
PAR=rep(20, length(CO2))

## Plot CO2 effect on GPP, ET, and SW. Feedbacks through soil
## eliminated with high precipitation
##pdf('testCO2.pdf', hei=10, wid=10)
op <- par(mfrow=c(4,4), mar=c(1,1,1,1), oma=c(4,4,4,4))
for (D in c(0, 0.5, 1, 1.5)) {
  D <- rep(D, length(CO2))
  o1 <- PRELES(PAR, T, D, Precip, CO2, fAPAR,
               returncols=c("GPP", "ET", "SW", "fW", "fE"), LOGFLAG=0)
  plot(CO2, o1$GPP)
  abline(v=380)
  plot(CO2, o1$ET)
  abline(v=380)
  plot(CO2, o1$GPP/o1$ET)
  abline(v=380)
  plot(CO2, o1$SW)
  abline(v=380)
}

# Run tge model with fake data downloaded from
#https://github.com/MikkoPeltoniemi/Rpreles/blob/master/data/s1.rdata
load("B:/modeling/project/06-PrelesData/EddyCovarianceDataBorealSites.rdata")

load("B:/modeling/project/06-PrelesData/parameterRanges.rdata")
## Plot CO2 effect on GPP, ET, and SW. Feedbacks through soil
## eliminated with high precipitation
##pdf('testCO2.pdf', hei=10, wid=10)

PAR <- s1$PAR
Tair <- s1$TAir
D <- s1$VPD
Precip <- s1$Precip
co2 <- s1$CO2
fAPAR <- s1$fAPAR

o2 <- PRELES(PAR, Tair, D, Precip, co2, fAPAR,returncols=c("GPP", "ET", "SW", "fW", "fE"), LOGFLAG=0)
  
plot(co2, o2$GPP)
  abline(v=380)
  plot(PAR, o2$GPP, ylab="GPP", xlab="PAR", main="PAR")
  abline(v=380)
  plot(Precip, o2$GPP, main= "precip")
  abline(v=380)
  plot(Tair, o2$GPP, main="Tair")
  abline(v=380)
  plot(fAPAR, o2$GPP,main ="fPAR")
  abline(v=380)
  
  library(randomForest)
  
  
  s1234 <- rbind(s1, s2, s3[, 1:9],s4[, 1:9])

  rf <- randomForest(GPPobs ~ PAR +TAir+ VPD+ Precip+ CO2+ fAPAR+ ETobs, data = s1234, na.action=na.omit)  

varImpPlot(rf, cex = 0.9, main = "Random Forest")

predict(rf, newdata = s1)

plot(s1$GPPobs, type="l")
lines(predict(rf, newdata = s1), col="red")
plot(s2$GPPobs, type="l")
lines(predict(rf, newdata = s2), col="red")
