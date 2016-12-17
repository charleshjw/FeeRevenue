# TODO: Add comment
# 
# Author: XBBKKL3
###############################################################################



library(forecast) #ARMAX


get_forecasts_arimax <- function(fit,regression_data,data,revenue,variables) {
	#			.variables <- variables;	
	
	#historical actual
	start_and_frequency <- determine_frequency_and_start(data[,1]);
	Historical <- ts(data[,2],start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#historical backtesting
	start_and_frequency <- determine_frequency_and_start(regression_data[,1]);
	.Backtesting <- ts(fitted(fit),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#Forecast baseline
	.forecast_data_baseline <- compile_forecast_data(determine_Macro(revenue[,1], Macro, Macro_Q),variables,revenue);
	start_and_frequency <- determine_frequency_and_start(.forecast_data_baseline[,1]);
	Forecast_baseline <- ts(predict(fit, newxreg=as.data.frame(.forecast_data_baseline)[,2])$pred,start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#Forecast advser
	.forecast_data_adverse <- compile_forecast_data(determine_Macro(revenue[,1], Macro_2, Macro_2_Q),variables,revenue);
	start_and_frequency <- determine_frequency_and_start(.forecast_data_adverse[,1]);	
	Forecast_adverse <- ts(predict(fit, newxreg=as.data.frame(.forecast_data_adverse)[,2])$pred,start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#Forecast severe
	.forecast_data_severe <- compile_forecast_data(determine_Macro(revenue[,1], Macro_3, Macro_3_Q),variables,revenue);
	start_and_frequency <- determine_frequency_and_start(.forecast_data_severe[,1]);
	Forecast_severe <- ts(predict(fit, newxreg=as.data.frame(.forecast_data_severe)[,2])$pred,start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#Forecast bhc
	.forecast_data_bhc <- compile_forecast_data(determine_Macro(revenue[,1], Macro_4, Macro_4_Q),variables,revenue);
	start_and_frequency <- determine_frequency_and_start(.forecast_data_bhc[,1]);
	Forecast_bhc <- ts(predict(fit, newxreg=as.data.frame(.forecast_data_bhc)[,2])$pred,start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	Forecast_data <- cbind(.forecast_data_baseline,.forecast_data_adverse,.forecast_data_severe,.forecast_data_bhc)
	
	list(backtest_forecast = merge_successive_ts(.Backtesting, Forecast_baseline), actual_forecast = merge_successive_ts(Historical, Forecast_baseline),
			actual=Historical,backtesting=.Backtesting,
			baseline=Forecast_baseline,adverse=Forecast_adverse,severe=Forecast_severe,bhc=Forecast_bhc, 
			start_forecast = as.numeric(as.yearmon(revenue[nrow(revenue),1])),
			
			forecast_data=Forecast_data,
			formula = formu_display_arima(data,fit),
			model = "arimax");
}

formu_display_arima <- function(data,fit) {
	var_name <- colnames(data)[3:ncol(data)];
	paste(paste(names(fit$coef),collapse="/"),paste(round(fit$coef,digit=1),collapse="/"),var_name,sep=": ")
}
