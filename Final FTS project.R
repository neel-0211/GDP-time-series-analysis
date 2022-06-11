library(tseries)
library(xts)
library(quantmod)
library(TSstudio)
# library(ts)
library(forecast)

#------------------------------------------------------------------Preprocessing------------------------------------------------------------------

data=read.csv('FTS_data_clean_2.csv',na.string="-")
data_ts=xts(data[,-1],order.by=as.Date(data[,1], "%m/%d/%Y"))

# stationarity transformation
GDP_ret=monthlyReturn(data_ts$GDP.by.Month)
CPI_ret=monthlyReturn(data_ts$CPI)
brent_ret=monthlyReturn(data_ts$Brent.Oil)
unemploy_ret=monthlyReturn(data_ts$Unemployment.Rate)
VIX_ret=monthlyReturn(data_ts$VIX)
Nasdaq_ret=monthlyReturn(data_ts$NASDAQ)
retail_ret=monthlyReturn(data_ts$Retail.Sales)
PMI_ret=monthlyReturn(data_ts$PMI)
SP_ret=monthlyReturn(data_ts$S.P500)
Housing_ret=monthlyReturn(data_ts$Housing.Index)
PPI_ret=monthlyReturn(data_ts$PPI)
prime_ret=monthlyReturn(data_ts$PRIME.M.)

all_ret_xts_mat=merge.xts(data$GDP.by.Month,
                GDP_ret,
                CPI_ret,
                Nasdaq_ret,
                SP_ret,
                brent_ret,
                unemploy_ret,
                retail_ret,
                PMI_ret,
                VIX_ret,
                Housing_ret,
                PPI_ret,
                prime_ret,
                data_ts$X10YT_Ret.M)

GDP_ts=xts_to_ts(all_ret_xts_mat$monthly.returns, frequency = 12)
plot(data_ts[,1], main='GDP by Month', xlab='Date', ylab = 'GDP')
acf(GDP_ts, lag=48)
plot(all_ret_xts_mat$monthly.returns)

names(all_ret_xts_mat)=c("GDP level",
                 "GDP ret",
                 "CPI ret",
                 "Nasdaq ret",
                 "SP ret",
                 "Brent ret",
                 "unemployment ret",
                 "Retail ret",
                 "PMI ret",
                 "VIX ret", 
                 "Housing ret",
                 "PPI ret",
                 "Prime ret",
                 "10-Y T-Bond_CN ret")


all_ret_df_mat=data.frame(all_ret_xts_mat)
pairs(all_ret_df_mat)

# log transform unemployment
all_ret_df_mat_log=all_ret_df_mat
all_ret_df_mat_log$unemployment.ADF=log(1 + all_ret_df_mat_log$unemployment.ret)
pairs(all_ret_df_mat_log)
dat = all_ret_df_mat_log

#------------------------------------------------------------------Seasonality Check------------------------------------------------------------------

GDP_level_ts=ts(data=dat$GDP.level, start = 1, end=874, frequency = 12)
fit = stl(ts(dat$GDP.level, start=1, end=73, frequency = 12), s.window = 'periodic')
plot(fit)
# Based on the plot of seasonal decomposition we can see that there is a seasonality compononent to GDP level.
# However we are performing time series analysis on GDP returns which are stationary and do not need seasonal decomposition.

#------------------------------------------------------------------ADF Test------------------------------------------------------------------

ADCP=data.frame("GDP ADF"=adf.test(na.remove(GDP_ret))$p.value,
                "CPI ADF"=adf.test(na.remove(CPI_ret))$p.value,
                "Nasdaq ADF"=adf.test(na.remove(Nasdaq_ret))$p.value,
                "SP ADF"=adf.test(na.remove(SP_ret))$p.value,
                "Brent ADF"=adf.test(na.remove(brent_ret))$p.value,
                "unemployment ADF"=adf.test(na.remove(unemploy_ret))$p.value,
                "Retail ADF"=adf.test(na.remove(retail_ret))$p.value,
                "PMI ADF"=adf.test(na.remove(PMI_ret))$p.value,
                "VIX ADF"=adf.test(na.remove(VIX_ret))$p.value,
                "Housing ADF"=adf.test(na.remove(Housing_ret))$p.value,
                "PPI ADF"=adf.test(na.remove(PPI_ret))$p.value,
                "Prime ADF"=adf.test(na.remove(prime_ret))$p.value,
                "10-Y T-Bond_CN ADF"=adf.test(na.remove(data_ts$X10YT_Ret.M.))$p.value)

ADCP

#------------------------------------------------------------------Model Testing------------------------------------------------------------------

acf(dat$GDP.ret)
pacf(dat$GDP.ret)
# ACF has serial correlation up until 7 lags. Hence suggesting a MA(7) process since PACF tails off.

for (i in seq(1,10)){
  Test=arima(dat$GDP.ret,order=c(0,0,i),xreg = dat[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret',"X10.Y.T.Bond_CN.ret")])
  print(Test$aic)
}

# based on the acf plot and mannually testing ar order, we found ar ma 7 is actually adequate

w11dat=matrix(dat[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret')], ncol = 7)

Test_MA_7=arima(dat$GDP.ret,order=c(0,0,7),xreg = dat[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret')])
Test_MA_7
P1=Test_MA_7$coef/sqrt(diag(vcov(Test_MA_7)))
P1

# based on the significance of the MA t-stats, MA 3-7 are insignificant.
Test_MA_2=arima(dat$GDP.ret,order=c(0,0,2),xreg = dat[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret')])
Test_MA_2
P1=Test_MA_2$coef/sqrt(diag(vcov(Test_MA_2)))
P1

# After running MA(2), we do not have any insignificant MA components. Along with that some of the exogenous variables become significant relative to MA(7).
test=na.omit(Test_MA_2$residuals)
Box.test(test, type='Ljung-Box', lag=12)
adf.test(test)
tsdiag(Test_MA_2, gof.lag=20)

auto.arima(dat$GDP.ret)

for (i in seq(1,10)){
  Test=arima(dat$GDP.ret,order=c(3,1,i),xreg = dat[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret',"X10.Y.T.Bond_CN.ret")])
  print(Test$aic)
}

Test_ARIMA_315=arima(dat$GDP.ret,order=c(3,1,5), xreg = dat[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret')])
Test_ARIMA_315
P1=Test_ARIMA_315$coef/sqrt(diag(vcov(Test_ARIMA_315)))
P1

plot(Test_ARIMA_315$residuals)
# from the plot we see unequal varaince in the residual, run the adf  next step for stationarity 

test=na.omit(Test_ARIMA_315$residuals)
adf.test(test)
tsdiag(Test_ARIMA_315, gof.lag=20)
# based on adf test, residual is stationary at alpha = 0.05.

#-----------------------------------------------------------------Final model arima 3,1,5----------------------------------------------------------------

order = c(3,1,5)

#------------------------------------------------------------------MODEL 1------------------------------------------------------------------

# Model1 from 1950.2 to 1959.11
Model1=arima(dat$GDP.ret[12:129],order=order,xreg = dat[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret')][12:129,])
coef_df=data.frame(matrix(ncol=7,nrow = 0))
colnames(coef_df)=c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret')
coef_df[nrow(coef_df)+1,]=Model1$coef[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret')]
p=Model1$coef[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret')]/sqrt(diag(vcov(Model1)))[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret')]
p_abs=abs(p)
coef_df[nrow(coef_df)+1,]=p_abs>1.96

#Model1 from 1959.12-2019.12
for (i in seq(130,790,by=60)){
  if (i==730){
    Models=arima(dat$GDP.ret[745:(745+60)],order=order,xreg = dat[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret')][745:(745+60),], method='CSS')
    coef_df[nrow(coef_df)+1,]=rep('-',7)
    coef_df[nrow(coef_df)+1,]=Models$coef[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret')]
    t=Models$coef[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret')]/sqrt(abs(diag(vcov(Models))))[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret')]
    tabs=abs(t)
    coef_df[nrow(coef_df)+1,]=tabs>1.96
  }
  else{
    Models=arima(dat$GDP.ret[i:(i+60)],order=order,xreg = dat[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret')][i:(i+60),])
    coef_df[nrow(coef_df)+1,]=rep('-',7)
    coef_df[nrow(coef_df)+1,]=Models$coef[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret')]
    t=Models$coef[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret')]/sqrt(abs(diag(vcov(Models))))[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret')]
    tabs=abs(t)
    coef_df[nrow(coef_df)+1,]=tabs>1.96
  }
}

#------------------------------------------------------------------MODEL 2------------------------------------------------------------------

# Model 2 from 1990.3-1999.11

Model2=arima(dat$GDP.ret[493:609],order=order,xreg = dat[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret','Housing.ret','Nasdaq.ret','Brent.ret','VIX.ret')][493:609,])
coef2_df=data.frame(matrix(ncol=11,nrow = 0))
colnames(coef2_df)=c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret','Housing.ret','Nasdaq.ret','Brent.ret','VIX.ret')
coef2_df[nrow(coef2_df)+1,]=Model2$coef[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret','Housing.ret','Nasdaq.ret','Brent.ret','VIX.ret')]
t=Model2$coef[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret','Housing.ret','Nasdaq.ret','Brent.ret','VIX.ret')]/sqrt(abs(diag(vcov(Model2))))[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret','Housing.ret','Nasdaq.ret','Brent.ret','VIX.ret')]
tabs=abs(t)
coef2_df[nrow(coef2_df)+1,]=tabs>1.96

# Model 2 from 1999.12-2019.12
for (i in seq(610,790,by=60)){
  Models=arima(dat$GDP.ret[i:(i+120)],order=order,xreg = dat[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret','Housing.ret','Nasdaq.ret','Brent.ret','VIX.ret')][i:(i+120),])
  coef2_df[nrow(coef2_df)+1,]=rep('-',11)
  coef2_df[nrow(coef2_df)+1,]=Models$coef[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret','Housing.ret','Nasdaq.ret','Brent.ret','VIX.ret')]
  t=Models$coef[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret','Housing.ret','Nasdaq.ret','Brent.ret','VIX.ret')]/sqrt(abs(diag(vcov(Models))))[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret','Housing.ret','Nasdaq.ret','Brent.ret','VIX.ret')]
  tabs=abs(t)
  coef2_df[nrow(coef2_df)+1,]=tabs>1.96
}


#------------------------------------------------------------------MODEL 3------------------------------------------------------------------

# Model 3 from 1999.12-2019.12

coef3_df=data.frame(matrix(ncol=12,nrow = 0))
colnames(coef3_df)=c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret','Housing.ret','Nasdaq.ret','Brent.ret','VIX.ret','Retail.ret')

for (i in seq(610,790,by=60)){
  Models=arima(dat$GDP.ret[i:(i+120)],order=order,xreg = dat[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret','Housing.ret','Nasdaq.ret','Brent.ret','VIX.ret','Retail.ret')][i:(i+120),])
  coef3_df[nrow(coef3_df)+1,]=rep('-',12)
  coef3_df[nrow(coef3_df)+1,]=Models$coef[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret','Housing.ret','Nasdaq.ret','Brent.ret','VIX.ret','Retail.ret')]
  t=Models$coef[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret','Housing.ret','Nasdaq.ret','Brent.ret','VIX.ret','Retail.ret')]/sqrt(abs(diag(vcov(Models))))[c('CPI.ret','SP.ret','unemployment.ret','PMI.ret','PPI.ret','Prime.ret','X10.Y.T.Bond_CN.ret','Housing.ret','Nasdaq.ret','Brent.ret','VIX.ret','Retail.ret')]
  tabs=abs(t)
  coef3_df[nrow(coef3_df)+1,]=tabs>1.96
}

#------------------------------------------------------------------Exporting Output------------------------------------------------------------------

write.csv(coef_df,'Model 1.csv')
write.csv(coef2_df,'Model 2.csv')
write.csv(coef3_df,'Model 3.csv')

#------------------------------------------------------------------THE END------------------------------------------------------------------
