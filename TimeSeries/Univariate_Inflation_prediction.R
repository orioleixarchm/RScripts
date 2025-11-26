########################################################
# Advanced Time Series Analysis                        #
# Analysing Inlation, Unemployment and Interest Rates  #
# Oriol Eixarch Mejías r0872954 12/10/2025             #
########################################################

#   0 Preparation and Set Up
##  0.1 Loading packages
packages <- c("quantmod","dplyr","readxl","lubridate","CADFtest","forecast","zoo",'pander','urca','vars')
needed <- setdiff(packages,rownames(installed.packages()))
if(length(needed)) install.packages(needed, dependencies=TRUE)
invisible(lapply(packages,library,character.only=TRUE))

##  0.2 Utilities: Custom made functions
### 0.2.1 Styled tables for CADF tests including coefficients
panderOptions('table.split.table', Inf)
pander_display_test <- function(test){
  mymodel <- summary(test)$model
  my_coeff <- mymodel$coefficients
  colnames(my_coeff)
  coeff_table <- as.data.frame(my_coeff)
  coeff_table[,4] <- ifelse(coeff_table[,4] < 0.01, 
                            paste0(round(coeff_table[,4], 7), " ***"),
                            ifelse(coeff_table[,4] < 0.05,
                                   paste0(round(coeff_table[,4], 7), " **"),
                                   ifelse(coeff_table[,4] < 0.1,
                                          paste0(round(coeff_table[,4], 7), " *"),
                                          round(coeff_table[,4], 7))))
  coeff_table$Estimator <- rownames(coeff_table)
  coeff_table <- coeff_table %>% relocate(last_col(), .before=1)
  rownames(coeff_table) <- NULL
  pander(test)
  pander(coeff_table, caption="Test's Coefficients Significance")
}

### 0.2.2 Simulating "CheckResiduals" function from Forecast using in-course materials
residual_plots <- function(residuals,model_name, lags=48){
  bt <- Box.test(residuals,lag=lags,type='Ljung-Box')
  layout(matrix(c(1,2,3,3),nrow=2,byrow=TRUE))
  par(mar=c(4,2,3,1), oma=c(1,1,1,1))
  hist(residuals, xlab='Value of Residuals', main=paste("Histogram of the",model_name,"Residuals", sep=' '))
  grid()
  acf(residuals, main=paste("Autocorrelogram",model_name,"Residuals", sep=' '),
      ylab='',xaxt='n',xlab='Lag (Months)', lag.max=lags)
  axis(1, at=seq(0,lags,by=6)/12, labels=seq(0,lags,by=6))
  plot(residuals, ylab='Residuals',xlab='Time Period (Year and Months)', 
       main=paste("Residuals of a",model_name,"Model", sep=' '))
  grid()
  return(pander(bt))
  layout(1)
}

### 0.2.3 MAPE Computation
MAPE_comp <- function(original_ts,errors, multi=FALSE){
  if (multi){
    S <- round(nrow(original_ts)*0.80)
  } else {
    S <- round(length(original_ts)*0.80)
  }
  h <- 1
  start_date <- time(original_ts)[S + h]
  actual_serie <- window(original_ts, start=start_date)
  if (multi){
    MAPE <- round(colMeans(abs(errors/actual_serie)),3)
  } else {
    MAPE <- round(mean(abs(errors/actual_serie)),3)
  }
  return(MAPE)
}

### 0.2.4 Univariate Expanding Errors computation and optional plotting 
expanding_errors <- function(serie, arma_order, seasonal_order,horizon=1, plot_serie_name=NULL){
  S <- round(length(serie)*0.80)
  h <- horizon
  f <- frequency(serie)
  forecast_roll <- c()
  upper_ci_roll <- c()
  lower_ci_roll <- c()
  errors <- c()
  for (i in S:(length(serie)-h)){
    model <- arima(serie[1:i], order=arma_order, seasonal=list(order=seasonal_order, period=f),
                   include.mean=FALSE)
    predict_result <- predict(model, n.ahead=h)
    prediction <- predict_result$pred[h]
    forecast_roll <- c(forecast_roll,prediction)
    se_current <- predict_result$se[h]
    upper_ci <- prediction + qnorm(0.975)*se_current
    upper_ci_roll <- c(upper_ci_roll,upper_ci)
    lower_ci <- prediction - qnorm(0.975)*se_current
    lower_ci_roll <- c(lower_ci_roll,lower_ci)
    errors <- c(errors,serie[i+h]-prediction)
  }
  if (!is.null(plot_serie_name)){
    title = paste('Actual',plot_serie_name,'Vs Rolling Prediction')
    par(mfrow=c(1,1))
    start_date <- time(serie)[S+h]
    end_date <- time(serie)[length(serie)]
    test <- window(serie, start=start_date, end=end_date)
    forecast_roll_ts <- ts(forecast_roll, start=start_date, frequency=f)
    upper_ci_roll_ts <- ts(upper_ci_roll, start=start_date, frequency=f)
    lower_ci_roll_ts <- ts(lower_ci_roll, start=start_date, frequency=f)
    
    lower_ylim <- min(c(test,forecast_roll_ts,upper_ci_roll_ts,lower_ci_roll_ts))*0.99995
    upp_ylim <- max(c(test,forecast_roll_ts,upper_ci_roll_ts,lower_ci_roll_ts))*1.0005
    plot.ts(test,col='royalblue',type='l',lty=1,lwd=2, ylim=c(lower_ylim,upp_ylim),
            ylab=plot_serie_name, xlab=NULL, main=title)
    lines(forecast_roll_ts, col='red',type='l',lty=1,lwd=2)
    lines(upper_ci_roll_ts, lty=3, lwd=2, col='orange')
    lines(lower_ci_roll_ts, lty=3, lwd=2, col='orange')
    legend('bottomright', legend=c('Actual','Forecast','Bounds'),
           col=c('royalblue','red','orange'), lty=c(1,1,3), lwd=c(2,2,2))
    grid()
  }
  return(errors)
}

##  0.3 Getting raw data
# Harmonized Index of Consumer Prices (FRED via EUROSTAT): All Items for the Euro Area 19
# 3-Month or 90-Day Rates and Yields (FRED via OECD): Interbank Rates for the Euro Area 19
# Monthly Unemployment in the EU area (20 countries)
getSymbols(c("CP0000EZ19M086NEST","IR3TIB01EZM156N"),src='FRED')
path  <-  "C:/Users/User/OneDrive/UNI/MASTER/Adv Time Series Analysis/Assignment/"
path <- paste0(path,"Unemployment_monthly_2000_2025.xlsx")
EU_unemployment <- read_excel(path, sheet='Data_I_pct')
EU_unemployment$TIME <- ym(EU_unemployment$TIME)
str(CP0000EZ19M086NEST)
str(IR3TIB01EZM156N)
str(EU_unemployment)

##  0.4 Cleaning data
max_date_available <- min(max(index(CP0000EZ19M086NEST)),
                         max(index(IR3TIB01EZM156N)),
                         max(EU_unemployment$TIME))
txt_filter <- paste0('2000-01-01','/',max_date_available)
Prices_2000_2025 <- CP0000EZ19M086NEST[txt_filter]
colnames(Prices_2000_2025) <- 'ConsumerPriceIndex'
Rates_2000_2025 <- IR3TIB01EZM156N[txt_filter]
colnames(Rates_2000_2025) <- 'InterbankRates'
EU_unemployment_2000_2025 <- EU_unemployment %>% filter(TIME >= ymd('2000-01-01'), TIME <= max_date_available)

##  0.5 Single Data frame 2000/01-2025/08 and R Time Series
Data <- data.frame(
  Date = EU_unemployment_2000_2025$TIME,
  Prices = round(as.numeric(Prices_2000_2025$ConsumerPriceIndex),1),
  Rates = round(as.numeric(Rates_2000_2025$InterbankRates),1),
  Unemployment = round(as.numeric(EU_unemployment_2000_2025$`Euro area`),1),
  row.names = NULL
)

Prices_ts <- ts(data=Data$Prices, start=c(2000,1), frequency=12)
Rates_ts <- ts(data=Data$Rates, start=c(2000,1), frequency=12)
Unemployment_ts <- ts(data=Data$Unemployment, start=c(2000,1), frequency=12)

#   1 Univariate Serie Analysis
par(mfrow=c(3,1))
par(mar=c(3,4,2,1), oma=c(1,1,1,1))

##  1.1 Visualization
plot.ts(Prices_ts,col='blue',lty=1,lwd=3,xlab='',ylab='CPI Index (Base 2015)',
     main="Consumer Price Index (Base 2015) monthly Evolution in the EU Area 2000-2025")
grid()

##  1.2 Testing Series for unit root (ADF)
test_prices <- CADFtest(Prices_ts, type='trend', criterion='BIC', max.lag.y=round(sqrt(length(Prices_ts))))
pander_display_test(test_prices)
# Our Series seems to have a Stochastic trend (we do not reject H0 and trend is significant), so is the intercept.
# Our Series is not in % terms and we would like it to be, thus to solve both non-stationarity and the unit-scaling
# we will take log-differences. We do not test other scenarios (no trend and no trend or drift) as the visual confirms
# the presence of a trend.

Prices_ts_log <- log(Prices_ts)
plot.ts(Prices_ts_log, col='blue', lty=1, lwd=3,xlab='',ylab='Log CPI Index (Base 2015)',
        main="Log Consumer Price Index (Base 2015) monthly Evolution in the EU Area 2000-2025")
grid()
Prices_ts_dif_log <- diff(log(Prices_ts))*100                          
plot.ts(Prices_ts_dif_log,col='blue',lty=1,lwd=3,xlab='',ylab='Percentage Points (%)',
     main="Monthly Inflation in the EU Area 2000-2025 (%)")
grid()
par(mfrow=c(1,1))

test_prices_dif <- CADFtest(Prices_ts_dif_log, type='trend', criterion='BIC', max.lag.y=round(sqrt(length(Prices_ts_dif_log))))
pander_display_test(test_prices_dif)
# Trend and Intercept are not significant, suggests we may no need them.
test_prices_dif <- CADFtest(Prices_ts_dif_log, type='drift', criterion='BIC', max.lag.y=round(sqrt(length(Prices_ts_dif_log))))
pander_display_test(test_prices_dif)
# Intercept is significant, we can reject unit root, we have stationarity around a non-zero mean.

##  1.3 Checking for seasonality
par(mfrow=c(1,1))
monthplot(Prices_ts_dif_log,main='Infaltion Seasonality', xlab='', ylab='', labels=month.abb)
# Seasonality is present indeed, but stable, it does not drift (stable mean "horizontal bar" and recurring pattern)

##  1.4 AR-MA Orders
par(mfrow=c(3,1))
par(mar=c(4,4,4,1), oma=c(0,1,0,1))

### 1.4.1 Checking the MA order of Non-seasonalized ts
acf(Prices_ts, main='Autocorrelogram of Non-Stationary Prices',
    ylab='',xaxt='n',xlab='Lag (Months)', lag.max=48)
axis(1, at=seq(0,48,by=6)/12, labels=seq(0,48,by=6))
acf(Prices_ts_dif_log, main='Autocorrelogram of Stationary Price Variation (Inflation)',
    ylab='',xaxt='n',xlab='Lag (Months)', lag.max=48)
axis(1, at=seq(0,48,by=6)/12, labels=seq(0,48,by=6))

### 1.4.2 Checking the AR order of Non-seasonalized ts
pacf(Prices_ts_dif_log, main='Partial Autocorrelogram of Stationary Price Variation (Inflation)',
     ylab='',xaxt='n',xlab='Lag (Months)',lag.max=48)
axis(1, at=seq(0,48, by=6)/12, labels=seq(0,48, by=6))

##  1.5 Modelization
# PACF and ACF suggest AR order 1 and MA order 1, we will also check different orders of ARMA models; 
# there seems to be correlation at seasonal frequency (6 or 12)
# Models then Ljung-Box test on residuals (residuals correlated)?? H0: Residuals = White noise)
# We must not include a mean despite having found that the series has a drift (non-zero mean) as seasonal 
# differentiation removes the mean.

### 1.5.1 Models
# MA(1) and Ljung-Box test on residuals (residuals correlated?? H0: Residuals = White noise)
ma1 <- arima(Prices_ts_dif_log,order=c(0,0,1))
pander(ma1)
pander(data.frame('BIC' = BIC(ma1)))
residual_plots(ma1$residuals,'MA(1)')

# AR(1) and Ljung-Box test on residuals (residuals correlated?? H0: Residuals = White noise)
ar1 <- arima(Prices_ts_dif_log,order=c(1,0,0))
pander(ar1)
pander(data.frame('BIC' = BIC(ar1)))
residual_plots(ar1$residuals,'AR(1)')

# ARMA(1,1) and Ljung-Box test on residuals (residuals correlated?? H0: Residuals = White noise)
arma11 <- arima(Prices_ts_dif_log,order=c(1,0,1))
pander(arma11)
pander(data.frame('BIC' = BIC(arma11)))
residual_plots(arma11$residuals,'ARMA(1,1)')

# SARMA(1,1) (ARMA + MU(1) Seasonality) and Ljung-Box test on residuals (residuals correlated?? H0: Residuals = White noise)
sarma_11_ma <- arima(Prices_ts_dif_log,order=c(1,0,1), seasonal=list(order=c(0,0,1), period=12))
pander(sarma_11_ma)
pander(data.frame('BIC' = BIC(sarma_11_ma)))
residual_plots(sarma_11_ma$residuals,'SARMA(1,1)(0,1)')

# SARMA(1,1) (ARMA + AR(1) Seasonality) and Ljung-Box test on residuals (residuals correlated?? H0: Residuals = White noise)
sarma_11_ar <- arima(Prices_ts_dif_log, order=c(1,0,1), seasonal=list(order=c(1,0,0), period=12))
pander(sarma_11_ar)
pander(data.frame('BIC' = BIC(sarma_11_ar)))
residual_plots(sarma_11_ar$residuals,'SARMA(1,1)(1,0)')

# SARMA(1,1) (ARMA + ARMA (1,1) Seasonality 12 periods) and Ljung-Box test on residuals (residuals correlated?? H0: Residuals = White noise)
sarma_11_arma <- arima(Prices_ts_dif_log, order=c(1,0,1), seasonal=list(order=c(1,0,1), period=12))
pander(sarma_11_arma)
pander(data.frame('BIC' = BIC(sarma_11_arma)))
residual_plots(sarma_11_arma$residuals,'SARMA(1,1)(1,1)[12]')
# Huge reduction between initial (MA) Ljung_Box Q statistic value at 48 lags (1082.1 to 74), which shows the sum of squared autocorrelations
# The best model so far considering only AIC criteria.

# SARMA(1,1) (ARMA + ARIMA (2,0,1) Seasonality 12 periods) and Ljung-Box test on residuals (residuals correlated?? H0: Residuals = White noise)
sarma_11_arma <- arima(Prices_ts_dif_log, order=c(1,0,1), seasonal=list(order=c(2,0,1), period=12))
pander(sarma_11_arma)
pander(data.frame('BIC' = BIC(sarma_11_arma)))
residual_plots(sarma_11_arma$residuals,'SARMA(1,1)(2,0,1)[12]')
# Slight increase in AIC and reduction in Q statistic, thus this is the best model so far WITHOUT taking into consideration
# its increased complexity. Still, taking into account complexity, we can see that BIC delivers a higher value than its simpler predecessor 111 vs 105.

# SARMA(1,1) (ARMA + ARIMA (1,0,2) Seasonality 12 periods) and Ljung-Box test on residuals (residuals correlated?? H0: Residuals = White noise)
sarma_11_arma <- arima(Prices_ts_dif_log, order=c(1,0,1), seasonal=list(order=c(1,0,2), period=12, include.mean=FALSE))
pander(sarma_11_arma)
pander(data.frame('BIC' = BIC(sarma_11_arma)))
residual_plots(sarma_11_arma$residuals,'SARMA(1,1)(1,0,2)[12]')
# Further Updating our seasonal orders delivers a worse Q statistic, and worse AIC.

### 1.5.2 Conclusion
# Relative to MA(1) and ARMA(1,1), the SARIMA(1,0,1)(1,0,1)[12] specification yields a large reduction in residual autocorrelation. The Ljung–Box
# statistics drop from Q(48)=406 to 76.97 for the MA(1), and from Q(48)=410 to 76.97 for the ARMA(1,1), i.e., a substantial fall in the
# (weighted) sum of squared residual autocorrelations up to 48 months. Residual ACFs are largely within bounds, indicating the seasonal dynamics
# are now well captured although not completely, due th strictness of the test at 48 lags, we formally reject residuals being white noisebut the plots 
# may suggest otherwise. The AIC of this model is the lowest of all the ones tested.
# Despite those good results the SARIMA(1,0,1)(2,0,1)[12] presents a lower Q statistic at the expense of a higher BiC.

##  1.6 Forecast
par(mfrow=c(3,1))
par(mar=c(3,4,2,1), oma=c(1,1,1,1))

### 1.6.1 Inflation SARMA(1,1)
train_index <- round(length(Prices_ts_dif_log)*0.85)
date_train_index <- as.yearmon(time(Prices_ts_dif_log)[train_index])
train_Inflation <- window(Prices_ts_dif_log, end=c(2021,10))
test_Inflation <- window(Prices_ts_dif_log, start=c(2021,11))

forecast_model <- arima(train_Inflation,order=c(1,0,1), seasonal=list(order=c(2,0,1),
                        period=12, include.mean=FALSE))
forecast <- predict(forecast_model,n.ahead=length(test_Inflation))
inflation_pred <- forecast$pred
up_bound <- inflation_pred + qnorm(0.975)*forecast$se
low_bound <- inflation_pred - qnorm(0.975)*forecast$se

plot.ts(test_Inflation,col='blue',type="l", lty=1, lwd=2,xlab="",ylim=c(-1.0,2.6), 
     ylab="Infation (Pct Change in CPI)", main='Actual Inflation vs Forecasted from 2021-11 to 2025-08')
lines(inflation_pred,lty=1, lwd=2, col='red')
lines(up_bound,lty=3, lwd=2, col='orange')
lines(low_bound,lty=3, lwd=2, col='orange')
legend("topright",legend=c('Actual','Forecasted','Confidence Bounds'), 
       col=c('blue','red','orange'), lwd=2, lty=c(1,1,3), cex=0.7)
grid()

### 1.6.2 Inflation Confidence Interval from Forecast Package
forecast_illegal <- forecast(forecast_model,h=length(test_Inflation))
prev_periods=12
plot(forecast_illegal, prev_periods,main='SARIMA(1,0,1)(1,0,1) Inflation + forecast from 2020-11 to 2025',
     ylab='Infation (PCt Change in CPI)', ylim=c(-1,2.5))
lines(window(Prices_ts_dif_log,start=c(2020,11)), col="black", lwd=2)
legend("topright", legend=c("Actual",'Forecast'), col=c('black','royalblue'), lwd=2, lty=c(1,1,3), cex=0.7)
grid()

### 1.6.3 Inflation Convergence to constant
forecast_model_conv <- arima(Prices_ts_dif_log,order=c(1,0,1), seasonal=list(order=c(2,0,1),
                                                                        period=12, include.mean=FALSE))
forecast_conv <- predict(forecast_model_conv,n.ahead=500)
inflation_pred_conv <- forecast_conv$pred
up_bound_conv <- inflation_pred_conv + qnorm(0.975)*forecast_conv$se
low_bound_conv <- inflation_pred_conv - qnorm(0.975)*forecast_conv$se

plot.ts(inflation_pred_conv,col='blue',type="l", lty=1, lwd=2,xlab="", ylim=c(-1,1.5),
        ylab="Infation (Pct Change in CPI)", main='Inflation Prediction Convergence after 500 periods (Months)')
lines(up_bound_conv,lty=1, lwd=1, col='orange')
lines(low_bound_conv,lty=1, lwd=1, col='orange')
abline(h=mean(Prices_ts_dif_log), col='red', lwd=2, lty=2)
legend("topright",legend=c('Forecasted','Bounds','Series mean'), 
       col=c('blue','orange','red'), lwd=2, lty=c(1,1,2), cex=0.7)
grid()
par(mfrow=c(1,1))

#### 1.6.4 CPI using SARIMA
par(mfrow=c(2,1))
par(mar=c(3,4,2,1), oma=c(1,1,1,0))
train_index <- round(length(Prices_ts_log)*0.85)
date_train_index <- as.yearmon(time(Prices_ts_log)[train_index])
train_Prices <- window(Prices_ts_log, end=c(2021,10))
test_Prices <- window(Prices_ts, start=c(2021,11))

forecast_model <- arima(train_Prices,order=c(1,1,1), seasonal=list(order=c(2,0,1),
                        period=12, include.mean=FALSE))
forecast <- predict(forecast_model,n.ahead=length(test_Prices))
forecasted_CPI_arima <- exp(forecast$pred)
upper_arima <- exp(forecast$pred + qnorm(0.975)*forecast$se)
lower_arima <- exp(forecast$pred - qnorm(0.975)*forecast$se)

plot.ts(test_Prices,col='blue',type="l", lty=1, lwd=2,xlab="", ylim=c(105,130),
     ylab="Base 2015 CPI", main='CPI and SARIMA-Forecasted CPI from November 2021 to August 2025')
lines(forecasted_CPI_arima,lty=1, lwd=2, col='red')
lines(upper_arima,lty=2, lwd=2, col='orange')
lines(lower_arima,lty=2, lwd=2, col='orange')
legend("bottomright",legend=c('Actual','Forecasted','Bounds'), 
       col=c('blue','red','orange'), lwd=c(2,2,2), lty=c(1,1,3), cex=0.7)
grid()

### 1.6.5 CPI Confidence Interval from Forecast Package
forecast_illegal <- forecast(forecast_model,h=length(test_Prices))
forecast_illegal$mean <- exp(forecast_illegal$mean)
forecast_illegal$lower <- exp(forecast_illegal$lower)
forecast_illegal$upper <- exp(forecast_illegal$upper)
forecast_illegal$x <- exp(forecast_illegal$x)
prev_periods=12
plot(forecast_illegal, prev_periods, main='CPI and SARIMA-Forecasted Log CPI C.Intervals from November 2020 to August 2025', 
     ylim=c(105,130), ylab='Base 2015 CPI')
lines(window(Prices_ts,start=c(2020,11)), col="black", lwd=2)
legend("bottomright",legend=c("Actual","Forecast"), col=c('black','royalblue'), lwd=c(2,2), lty=c(1,1), cex=0.7)
grid()
par(mfrow=c(1,1))

##  1.7 Prediction validation
### 1.7.1 Estimated models: Errors
inflation_errors <- expanding_errors(Prices_ts_dif_log,c(1,0,1), c(2,0,1), plot_serie_name='Inflation (%)')
MSE_inflation <- mean(inflation_errors^2)
RMSE_inflation <- sqrt(mean(inflation_errors^2))
MAE_inflation <- mean(abs(inflation_errors))
MAPE_inflation <- MAPE_comp(Prices_ts_dif_log,inflation_errors)

cpi_errors <- expanding_errors(Prices_ts_log,c(1,1,1),c(2,0,1), plot_serie_name='Log CPI Base 2015')*100
MSE_cpi <- mean(cpi_errors^2)
RMSE_cpi <- sqrt(mean(cpi_errors^2))
MAE_cpi <- mean(abs(cpi_errors))
MAPE_cpi <- MAPE_comp(Prices_ts_log,cpi_errors)

### 1.7.2 Potential Automatically chosen models: Errors
# The "forecast" command is overrun when the "Forecast" package is loaded so we must use auto arima.
auto_model <- auto.arima(Prices_ts_log)
arma_params <- auto_model$arma # follows this structure (p,q,P,Q,m,d,D) not (p,d,q,P,D,Q,m)
arma_order <- c(arma_params[1],arma_params[6],arma_params[2])
seasonal_order <- c(arma_params[3],arma_params[7],arma_params[4])
auto_forecast_errors <- expanding_errors(Prices_ts_log,arma_order,seasonal_order)*100
MSE_cpi_auto <- mean(auto_forecast_errors^2)
RMSE_cpi_auto <- sqrt(mean(auto_forecast_errors^2))
MAE_cpi_auto <- mean(abs(auto_forecast_errors))
MAPE_cpi_auto <- MAPE_comp(Prices_ts_log,auto_forecast_errors)

errors <- matrix(round(c(MSE_inflation, RMSE_inflation, MAE_inflation, MAPE_inflation,
                         MSE_cpi, RMSE_cpi, MAE_cpi, MAPE_cpi,
                         MSE_cpi_auto, RMSE_cpi_auto, MAE_cpi_auto, MAPE_cpi_auto),2), nrow=4, ncol=3, byrow=FALSE)
df_errors <- as.data.frame(errors)
colnames(df_errors) <- c('Inflation','CPI 2022-2025 (*100)','Auto-ARIMA (*100)')
df_errors$Error <- c('MSE','RMSE','MAE','MAPE')
df_errors <- df_errors %>% relocate(last_col(), .before=1)
pander(df_errors, caption='Errors of Different predictions')

### 1.7.2 Testing for Equivalent models (Diebold-Mariano)
pander(dm.test(cpi_errors,auto_forecast_errors,h=1,power=1))
pander(dm.test(cpi_errors,auto_forecast_errors,h=1,power=2))

### 1.7.3 Conclusion:
# Inflation: Our RMSE shows that, on average our predicion is 0.41 pp off every time, that does not seem too large given the 
# scale of our data (bound between and 2,5 and -1). Computing the MAPE would allow as to check the % deviation from the
# true value, however since we deal with small magnitudes e.g 0 is not uncommon, we may get results hard to interpret or infinity. 
# CPI: Our RMSE Shows that, on average our model and the automatic one are both off by 0.041 units.
# The autogenerated model delivers slightly larger absolute Errors than our manual set SARIMA, despite, both predictions and models 
# are statistically equivalent regardless of the use of absolute or squared errors.


