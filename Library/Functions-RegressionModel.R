# 
# 
# Author: XBBKKL3
###############################################################################
calculate_implied_yield <- function(revenue_lob,regressor_name,forecasts) {
	#revenue_lob is the list of revenues
	#regressor_name is like "Revenue.Total_Other"
#					regressor_name <- colnames(modelset)[j]
	
	revenue_lob_1 <- revenue_lob[[get_revenue_name(regressor_name)]];
	revenue_lob_1 <- clean_na_data_frame(revenue_lob_1);
	
	impliedyield <- revenue_lob_1[,2]/revenue_lob_1[,3];
	.lookback <- determine_yield_periods(revenue_lob_1[,1]);
	.iy_historical <- SMA(impliedyield,.lookback);
	.iy_historical <- c(NA,.iy_historical);

	for (k in 1:(length(forecasts$baseline)-1)) {
	#	k <- 1;
		.iy_historical <- c(.iy_historical,mean(.iy_historical[(length(.iy_historical)-.lookback+1):length(.iy_historical)]));
	}
	
	.iy <- ts(.iy_historical,start=determine_frequency_and_start(revenue_lob_1[,1])$start,frequency=determine_frequency_and_start(revenue_lob_1[,1])$frequency);
	.iy
}

get_forecasts <- function(fit,regression_data,data,revenue,variables) {
	#			.variables <- variables;	
	#historical actual
	start_and_frequency <- determine_frequency_and_start(data[,1]);
	Historical <- ts(data[,2],start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#historical backtesting
	start_and_frequency <- determine_frequency_and_start(regression_data[,1]);
	Backtesting <- ts(fit$fitted.values,start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#Forecast baseline
	.forecast_data_baseline <- compile_forecast_data(determine_Macro(revenue[,1], Macro, Macro_Q),variables,revenue);
	start_and_frequency <- determine_frequency_and_start(.forecast_data_baseline[,1]);
	Forecast_baseline <- ts(predict(fit,.forecast_data_baseline),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#Forecast advser
	.forecast_data_adverse <- compile_forecast_data(determine_Macro(revenue[,1], Macro_2, Macro_2_Q),variables,revenue);
	start_and_frequency <- determine_frequency_and_start(.forecast_data_adverse[,1]);	
	Forecast_adverse <- ts(predict(fit,.forecast_data_adverse),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#Forecast severe
	.forecast_data_severe <- compile_forecast_data(determine_Macro(revenue[,1], Macro_3, Macro_3_Q),variables,revenue);
	start_and_frequency <- determine_frequency_and_start(.forecast_data_severe[,1]);
	Forecast_severe <- ts(predict(fit,.forecast_data_severe),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#Forecast bhc
	.forecast_data_bhc <- compile_forecast_data(determine_Macro(revenue[,1], Macro_4, Macro_4_Q),variables,revenue);
	start_and_frequency <- determine_frequency_and_start(.forecast_data_bhc[,1]);
	Forecast_bhc <- ts(predict(fit,.forecast_data_bhc),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	Forecast_data <- cbind(.forecast_data_baseline,.forecast_data_adverse,.forecast_data_severe,.forecast_data_bhc)
	
	list(backtest_forecast = merge_successive_ts(Backtesting, Forecast_baseline), actual_forecast = merge_successive_ts(Historical, Forecast_baseline),
			actual=Historical,backtesting=Backtesting,
			baseline=Forecast_baseline,adverse=Forecast_adverse,severe=Forecast_severe,bhc=Forecast_bhc, 
			start_forecast = as.numeric(as.yearmon(revenue[nrow(revenue),1])),
			
			forecast_data=Forecast_data,
			formula = formu_display(data,fit),
			fit = fit,
			model = "regression"
	);
}

get_forecasts_diff <- function(fit,regression_data,data,revenue,variables) {
	
	start_and_frequency <- determine_frequency_and_start(data[,1]);
	.frequency <- start_and_frequency$frequency;
	.start <- start_and_frequency$start;
	Backtesting <- ts(predict(fit,newdata=data),start=.start,frequency=.frequency);
	
	Historical <- ts(data[,2],start=.start,frequency=.frequency);
	
	#########
	#Forecast baseline
	.forecast_data_baseline <- compile_forecast_data_diff(determine_Macro(revenue[,1], Macro, Macro_Q),variables,revenue);
	start_and_frequency <- determine_frequency_and_start(.forecast_data_baseline[,1]);
	Forecast_baseline <- ts(predict(fit,.forecast_data_baseline),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#Forecast advser
	.forecast_data_adverse <- compile_forecast_data_diff(determine_Macro(revenue[,1], Macro_2, Macro_2_Q),variables,revenue);
	start_and_frequency <- determine_frequency_and_start(.forecast_data_adverse[,1]);
	Forecast_adverse <- ts(predict(fit,.forecast_data_adverse),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#Forecast severe
	.forecast_data_severe <- compile_forecast_data_diff(determine_Macro(revenue[,1], Macro_3, Macro_3_Q),variables,revenue);
	start_and_frequency <- determine_frequency_and_start(.forecast_data_severe[,1]);
	Forecast_severe <- ts(predict(fit,.forecast_data_severe),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#Forecast bhc
	.forecast_data_bhc <- compile_forecast_data_diff(determine_Macro(revenue[,1], Macro_4, Macro_4_Q),variables,revenue);
	start_and_frequency <- determine_frequency_and_start(.forecast_data_bhc[,1]);
	Forecast_bhc <- ts(predict(fit,.forecast_data_bhc),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
	Forecast_data <- cbind(.forecast_data_baseline,.forecast_data_adverse,.forecast_data_severe,.forecast_data_bhc)
	
	list(backtest_forecast = merge_successive_ts(Backtesting, Forecast_baseline), actual_forecast = merge_successive_ts(Historical, Forecast_baseline),
			actual=Historical,backtesting=Backtesting,
			baseline=Forecast_baseline,adverse=Forecast_adverse,severe=Forecast_severe,bhc=Forecast_bhc, 
			start_forecast = as.numeric(as.yearmon(revenue[nrow(revenue),1])),
			
			forecast_data=Forecast_data,
			formula = formu_display(data,fit),
			fit = fit,
			model = "DiffRegression"
	);
}

convert_forecasts_diff_to_level <- function(forecasts, revenue, data) {
	forecasts_level <- forecasts;
	names(forecasts_level)
	forecasts_level$actual <- revenue_to_ts(revenue);
	
	tmp <- determine_frequency_and_start_DiffBacktest(revenue,data);
	start_value <- tmp$value;
	.start <- tmp$start;
	.frequency <- tmp$frequency;
	increments <- na.omit(as.vector(forecasts$backtesting));
	forecasts_level$backtesting <- diff_to_level(start_value,increments,.start,.frequency);
	
	start_value <- revenue[nrow(revenue),2];
	increments <- as.vector(forecasts$baseline);
	.start <- start(forecasts$baseline);
	.frequency <- frequency(forecasts$baseline);
	forecasts_level$baseline <- diff_to_level(start_value,increments,.start,.frequency,remove_first=TRUE);
	
	increments <- as.vector(forecasts$adverse);
	.start <- start(forecasts$adverse);
	.frequency <- frequency(forecasts$adverse);
	forecasts_level$adverse <- diff_to_level(start_value,increments,.start,.frequency,remove_first=TRUE);
	
	increments <- as.vector(forecasts$severe);
	.start <- start(forecasts$severe);
	.frequency <- frequency(forecasts$severe);
	forecasts_level$severe <- diff_to_level(start_value,increments,.start,.frequency,remove_first=TRUE);
	
	increments <- as.vector(forecasts$bhc);
	.start <- start(forecasts$bhc);
	.frequency <- frequency(forecasts$bhc);
	forecasts_level$bhc <- diff_to_level(start_value,increments,.start,.frequency,remove_first=TRUE);
	
	forecasts_level$actual_forecast <- merge_successive_ts(forecasts_level$actual, forecasts_level$baseline)
	forecasts_level$backtest_forecast <- merge_successive_ts(forecasts_level$backtesting, forecasts_level$baseline)
	forecasts_level;
}

plot_regression_model <- function(forecasts, plot_name, title_name, pngoutput=TRUE) {
	if (pngoutput) {
		png(plot_name,width=1000,height=600);
	}
	notation_baseline <- get_notation(forecasts$actual_forecast);
	ts.plot(forecasts$actual,forecasts$backtesting, forecasts$baseline, forecasts$adverse, forecasts$severe, forecasts$bhc,type="b",col=c(Color_history,Color_backtest,Color_baseline,Color_adverse,Color_severe,Color_bhc), ylab="", main=title_name, sub=forecasts$formula,gpars=list(yaxt="n",lwd=c(2,1,2,1,1,1)));
	axis(2,at=axTicks(2), labels=format_level_value(axTicks(2)), las=1)
	abline(v=forecasts$start_forecast);
	abline(v=notation_baseline$Year);
	text(notation_baseline$X,notation_baseline$Y,notation_baseline$Content,col=Color_history);			
#	legend("topleft",legend=c("history","backtest","baseline","adverse","severely adverse","BHC"),pch=1,col=c(Color_history,Color_backtest,Color_baseline,Color_adverse,Color_severe,Color_bhc));
	legend("topleft",legend=c("history","backtest","baseline","severely adverse"),pch=1,col=c(Color_history,Color_backtest,Color_baseline,Color_severe));
	if (pngoutput) {
		dev.off();
	}
}

plot_comparison_table_regression_model <- function(forecasts) {
	notation_baseline <- get_notation(merge_successive_ts(forecasts$actual,forecasts$baseline))$Table;
	rownames(notation_baseline)[1] <- "Baseline";
	notation_adverse <- get_notation(merge_successive_ts(forecasts$actual,forecasts$adverse))$Table;
	rownames(notation_adverse)[1] <- "Adverse";
	notation_severe <- get_notation(merge_successive_ts(forecasts$actual,forecasts$severe))$Table;
	rownames(notation_severe)[1] <- "Severe";
	notation_bhc <- get_notation(merge_successive_ts(forecasts$actual,forecasts$bhc))$Table;
	rownames(notation_bhc)[1] <- "BHC";
	Comparison <- rbind(notation_baseline,notation_adverse,notation_severe,notation_bhc);
	Comparison;
}


