# BABS 502 - Forecasting and time series prediction
# Instructor: Martha Essak
# Week 02-03 examples: Decomposition


library(fpp) # load the Forecasting: Principles and Practice package, as well as all its dependencies

# Some time series examples
par(mfrow=c(2,2), mar=c(4,4,2,1))
plot(hsales,xlab="Year",ylab="Monthly housing sales (millions)")
plot(ustreas,xlab="Day",ylab="US treasury bill contracts")
plot(elec,xlab="Year",ylab="Australian monthly electricity production")
plot(diff(dj),xlab="Day",ylab="Daily change in Dow Jones index")

dev.off()

#########################
# Decomposition example
print(elecequip)

fit <- stl(elecequip, s.window=5) # Seasonal and Trend decomposition using Loess
plot(elecequip, col="gray", main="Electrical equipment manufacturing", ylab="New orders index", xlab="Year")
lines(fit$time.series[,2],col="red",ylab="Trend") # Graph the trend-cycle

# Decomposition plot
plot(fit)


# Seasonal deviation (sub-series) plot
monthplot(fit$time.series[,"seasonal"], main="", ylab="Seasonal", xlab="Month") # plot the seasonal component after you have taken out the trend and remainder

monthplot(elecequip) # This shows the seasonal components when the trend and remainder are present


# Seasonally adjusted data
plot(elecequip, col="grey", main="Equipment manufacturing", xlab="Year", ylab="New orders index")
lines(seasadj(fit),col="red",ylab="Seasonally adjusted")


#########################
# Simple moving averages

data.vec <- round(rnorm(10)*100) # note that since these are randomnly generated values, you will get different values from those in the lecture notes
table.out <- rbind(data.vec, ma(data.vec, order=5))
rownames(table.out) <- c("data", "5 MA")
table.out

# Odd-order moving averages
plot(elecsales, main="Residential electricity sales", ylab="GWh", xlab="Year")
lines(ma(elecsales,5),col="red")


# Even-order moving averages

beer2 <- window(ausbeer,start=1992)
table.out <- rbind(beer2, ma(beer2, order=4, centre=FALSE), ma(beer2, order=4, centre=TRUE))
rownames(table.out) <- c("data", "4 MA", "2x4 MA")
table.out[,1:5]


# Centered moving averages

plot(elecequip, ylab="New orders index", col="gray", main="Electrical equipment manufacturing (Euro area)")
lines(ma(elecequip, order=12), col="red") # note that since this is an even order MA, the default is to add a 2 MA after

plot(elecequip, ylab="New orders index", col="gray", main="Electrical equipment manufacturing (Euro area)")
lines(ma(elecequip, order=12, centre=TRUE), col="red") # adds 2 MA after 12 MA, giving 2 X 12 MA

# Try some different options for moving averages

plot(elecequip, ylab="New orders index", col="gray", main="Electrical equipment manufacturing (Euro area)")
lines(ma(elecequip, order=12, centre=FALSE), col="red") # since this is not symmetric, you can see that some of the seasonality has contamimnated the trend-cycle, making it less smooth

plot(elecequip, ylab="New orders index", col="gray", main="Electrical equipment manufacturing (Euro area)")
lines(ma(elecequip, order=13), col="red")


plot(elecequip, ylab="New orders index", col="gray", main="Electrical equipment manufacturing (Euro area)")
lines(ma(elecequip, order=11), col="red")


#########################
# Classical decomposition: Additive
# Tutorial online: https://anomaly.io/seasonal-trend-decomposition-in-r/

# Below is the code with some of my comments. Note that the style of the code is different (using = instead of <-, using _ instead of .) because it was originally written by someone else. Either style is correct; you can choose what you prefer.

# Step 1: Import data
data(ausbeer)
timeserie_beer = tail(head(ausbeer, 17*4+2),17*4-4)
plot(as.ts(timeserie_beer)) # plot the time series that we will be working with

# Step 2: Detect the trend
trend_beer = ma(timeserie_beer, order = 4, centre = T) # We use a moving average to get rid of the seasonality and detect the underlying trend. Why we we use a centered moving average of order 4?
plot(as.ts(timeserie_beer))
lines(trend_beer)
plot(as.ts(trend_beer))

# Step 3: De-trend the time series
detrend_beer = timeserie_beer - trend_beer # we are subtracting the trend (this is because it is additive, note that you would have remove the trend differently for multiplicative decomposition)
plot(as.ts(detrend_beer)) # this now includes the seasonal and remainder components only

# Step 4: Average seasonality 
m_beer = t(matrix(data = detrend_beer, nrow = 4))
seasonal_beer = colMeans(m_beer, na.rm = T)
seasonal_beer # This is a line of code I added. This is the seasonal component!!
plot(as.ts(rep(seasonal_beer,16))) # because we have 16 years of data, we repeat the seasonal component 16 times. Note that the seasonal component is not allowed to change over time.

# Step 5: Plot the remainder component
random_beer = timeserie_beer - trend_beer - seasonal_beer
plot(as.ts(random_beer))

# Step 6: Reconstruct the original signal
recomposed_beer = trend_beer+seasonal_beer+random_beer
plot(as.ts(recomposed_beer))

# Using the decompose function:
ts_beer = ts(timeserie_beer, frequency = 4)
decompose_beer = decompose(ts_beer, "additive")

plot(as.ts(decompose_beer$seasonal)) # you can plot each of the components on its own
plot(as.ts(decompose_beer$trend))
plot(as.ts(decompose_beer$random))
plot(decompose_beer)



#########################
# Classical decomposition: Multiplicative
fit <- decompose(elecequip, type="multiplicative")
plot(fit)




#########################
# STL decomposition
fit <- stl(elecequip, t.window=15, s.window="periodic", robust=TRUE)
plot(fit)


# Try some different values for the trend window and the seasonal window
fit <- stl(elecequip, t.window=5, s.window="periodic", robust=TRUE) 
plot(fit)

fit <- stl(elecequip, t.window=25, s.window="periodic", robust=TRUE)
plot(fit)


#########################
# Forecasting with decomposition: naive method
fit <- stl(elecequip, t.window=15, s.window="periodic", robust=TRUE)
eeadj <- seasadj(fit)
plot(naive(eeadj), ylab="New orders index", main="Naive forecasts of seasonally adjusted data") 


# Naive forecasts of data with seasonality included
fcast <- forecast(fit, method="naive")
plot(fcast, ylab="New orders index")








