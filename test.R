ARMAacf(ar = numeric(), ma = numeric(), lag.max = r, pacf = FALSE)


m111 <- arima(ds1.ddd, order = c(2, 0, 1), 
              seasonal = list(order = c(1, 0, 1), 
                                               period = 52))
tsdiag(m111)

m111


m112 <- arima(ds1, order = c(2, 1, 1), 
      seasonal = list(order = c(1, 1, 1), 
                      period = 52))

tsdiag(m112)



acf(ds1.ddd, lag.max = 100)
pacf(ds1.ddd, lag.max = 100)

m113 <- arima(ds1.ddd, order = c(2, 0, 1), 
              seasonal = list(order = c(0, 0, 1), 
                              period = 52))

m114 <-arima(ds1.ddd, order = c(2, 0, 1), 
                 seasonal = list(order = c(1, 0, 1), 
                                 period = 52))


m115 <-arima(ds1.ddd, order = c(2, 0, 2), 
             seasonal = list(order = c(0, 0, 1), 
                             period = 52))





L = 200
ph = c( -0.1137,  -0.1081,  -0.0711)
th = c(-0.4686, rep(0, 50),  -0.2248)
corrs = ARMAacf( ar = ph ,ma = th, lag.max = L)
par.corrs = ARMAacf(ar = ph, ma = th, lag.max = L, pacf = T)


par(mfrow = c(2, 1))
plot(x = 0:L, y = corrs, type = "h", xlab = "Lag k", ylab = "Autocorrelation")
abline(h = 0)
plot(x = 1:L, y = par.corrs, type = "h", xlab = "Lag k", ylab = "Partial Autocorrelation")
abline(h = 0)


