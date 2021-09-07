data(AirPassengers)
class(AirPassengers)

start(AirPassengers)

end(AirPassengers)

frequency(AirPassengers)
summary(AirPassengers)

plot(AirPassengers)
abline(reg = lm(AirPassengers ~ time(AirPassengers)))
boxplot(AirPassengers~cycle(AirPassengers))
cycle(AirPassengers)

#ARIMA modelling
acf(AirPassengers)

#q-value
acf(diff(log(AirPassengers)))
#Take the value previous to value which is getting inverted
#So q=1

#p-value
pacf(diff(log(AirPassengers)))
#Calculate p same as q
#So p=0

plot(diff(log(AirPassengers)))
#d= 1 as we have differentiated once to make mean equal

#Model
apmodel <- arima(log(AirPassengers),c(0,1,1),seasonal = list(order = c(0,1,1)), period = )

