install.packages("forecast")
library(forecast)
install.packages("astsa")
library(astsa)
data(AirPassengers)
class(AirPassengers)
AP <- AirPassengers
start(AP)
end(AP)
frequency(AP)
summary(AP)
plot(AP, ylab="Passengers (1000s)", type="o", pch =20)

#abline(reg=lm(AirPassengers~time(AirPassengers)))
#cycle(AirPassengers)
#plot(aggregate(AirPassengers,FUN=mean))
#boxplot(AirPassengers~cycle(AirPassengers))

AP.decompM <- decompose(AP, type = "multiplicative")
plot(AP.decompM)

#Inspecting the trend component in the decomposition plot suggests that the relationship is linear, thus fitting a linear model
t <- seq(1, 144, 1)
modelTrend <- lm(formula = AP.decompM$trend ~ t)
predT <- predict.lm(modelTrend, newdata = data.frame(t))

plot(AP.decompM$trend[7:138] ~ t[7:138], ylab="T(t)", xlab="t",
     type="p", pch=20, main = "Trend Component: Modelled vs Observed")
lines(predT, col="red")
layout(matrix(c(1,2,3,4),2,2))
plot(modelTrend)
summary(modelTrend)
#Therefore, the relationship between trend and time can be expressed as:
#  T(t)=2.667t+84.648
#
#And so for 1961 (time 145 to 156 inc.), the trend component (T) is:
 
Data1961 <- data.frame("T" = 2.667*seq(145, 156, 1) + 84.648, S=rep(0,12), e=rep(0,12),
                         row.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
Data1961


#Inspecting the seasonal (S) component of the decomposition reveals:
AP.decompM$seasonal

#Thus the seasonal (S) component to the new 1961 dataset is:
Data1961$S <- unique(AP.decompM$seasonal)
Data1961

#Ploting the density estimation of the random error (e) component of the decomposition shows an approximate normal distribution:
plot(density(AP.decompM$random[7:138]),
       main="Random Error") #Values 1:6 & 139:44 are NA

mean(AP.decompM$random[7:138])
Data1961$e <- 1
Data1961


sd_error <- sd(AP.decompM$random[7:138])
sd_error

#The 3-point esitmates (Realistic, Optimistic, Pessimistic) for the predictions is simply the expected prediction (T???S???e), and 95% CI interval either way using the standard deviation of the random error(95% CI = 1.95???sd)

Data1961$R <- Data1961$T * Data1961$S * Data1961$e                  #Realistic Estimation
Data1961$O <- Data1961$T * Data1961$S * (Data1961$e+1.95*sd_error)  #Optimistic Estimation
Data1961$P <- Data1961$T * Data1961$S * (Data1961$e-1.95*sd_error)  #Pessimistic Estimation
Data1961