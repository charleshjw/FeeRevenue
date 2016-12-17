# 
# 
# Author: XBBKKL3
###############################################################################

detect_seasonality_method1 <- function(data) {
	## https://www.r-bloggers.com/detecting-seasonality/
	#	data <- historical_data
	require(forecast);
	fit1 <- ets(data);
	fit2 <- ets(data,model="ANN");
	
	deviance <- 2*c(logLik(fit1) - logLik(fit2))
	df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df 
	#P value
	p <- 1-pchisq(deviance,df);
	list(Result=p<0.05,Statistic=p);
}


detect_seasonality_method2 <- function(data) {
#	data <- historical_data
	fit <- tbats(data)
	result <- !is.null(fit$seasonal);
	if(!result) {
		statistic <- -1;
	} else {
		statistic <- fit$seasonal;
	}
		
	list(Result=result,Statistic=statistic);
}

revenue_analysis_seasonalmodel <- function(revenue,threshold=revenue_threshold) {
	
	if (nrow(revenue)==0) {
		return(FALSE);
	}
	
	#if last year's revenue is more than threshold
	if (determine_frequency_and_start(revenue[,1])$frequency==12) {
		criteria1 <- abs(sum(revenue[(nrow(revenue)-11):nrow(revenue),2], na.rm=TRUE)) > threshold&sd(revenue[,2],na.rm = TRUE)!=0;	
		criteria2 <- length(na.omit(revenue[,2])) > 24;
	} else {
		criteria1 <- abs(sum(revenue[(nrow(revenue)-3):nrow(revenue),2], na.rm=TRUE)) > threshold&sd(revenue[,2],na.rm = TRUE)!=0;
		criteria2 <- length(na.omit(revenue[,2])) > 8;
	}
	
	#if revenue period is more than 12 periods
	
	if (criteria1 & criteria2) {
		return(TRUE);
	} else {
		return(FALSE);
	}
#	return TRUE if pass threshold
}

forecast_ts_seasonalmodel <- function(historical_data) {
	#	.series <- revenue[,2];
	#	lookbackperiod <- 6
	#	forecastperiod <- 27;
	model <- stl(historical_data,"periodic");
	
	.actual <- historical_data;
	.backtesting <- ts(apply(model$time.series[,1:2],1,sum), start=start(model$time.series[,1]), frequency=frequency(model$time.series[,1]));
	.baseline <- forecast(model,method=c("ets"),level=c(0),h=60)$mean;
	.adverse <- .baseline;
	.severe <- .baseline;
	.bhc <- .baseline;
	
	.backtest_forecast <- merge_successive_ts(.backtesting,.baseline)
	.actual_forecast <- merge_successive_ts(.actual,.baseline)
	list(backtest_forecast=.backtest_forecast, actual_forecast=.actual_forecast, 
			actual=.actual, backtesting=.backtesting, 
			baseline=.baseline, adverse=.adverse, severe=.severe, bhc=.bhc,
			start_forecast = start(.baseline),
			model = "seasonaldecomposition");
}