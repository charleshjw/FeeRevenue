# 
# 
# Author: XBBKKL3
###############################################################################


ts_next_period <- function(time_series) {
	#time seris
#		time_series <- .actual;
	
	.frequency <- frequency(time_series);
	.end_time <- end(time_series);
	
	if (.frequency==12) {
		if(.end_time[2]==12) {
			return(c(.end_time[1]+1,1));
		} else {
			return(c(.end_time[1],.end_time[2]+1));
		}
	} else {
		if(.end_time[2]==4) {
			return(c(.end_time[1]+1,1));
		} else {
			return(c(.end_time[1],.end_time[2]+1));
		}
	}		
}

forecast_ts_mv <- function(revenue,lookbackperiod=6,forecastperiod=27) {
	#	.series <- revenue[,2];
	#	lookbackperiod <- 6
	#	forecastperiod <- 27;
	
	.sma <- SMA(revenue[,2],lookbackperiod);
	.backtest_ts <- c(NA,.sma[-length(.sma)]);
	
	.tmp_ts <- revenue[,2][(length(revenue[,2])-lookbackperiod+1):length(revenue[,2])];
	.forecast_ts <- c();
	for (i in 1:forecastperiod) {
		.tmp <- mean(.tmp_ts[(length(.tmp_ts)-lookbackperiod+1):length(.tmp_ts)]);
		.tmp_ts <- c(.tmp_ts,.tmp);
		.forecast_ts <- c(.forecast_ts,.tmp);
	}
	
	start_and_frequency <- determine_frequency_and_start(revenue[,1]);
	.frequency <- start_and_frequency$frequency;
	.start <- start_and_frequency$start;
	
	.actual <- ts(revenue[,2],start=.start,frequency=.frequency);
	.backtesting <- ts(.backtest_ts,start=.start,frequency=.frequency);
	.baseline <- ts(.forecast_ts, start=ts_next_period(.actual),frequency=.frequency);
	.adverse <- .baseline;
	.severe <- .baseline;
	.bhc <- .baseline;
	
	.backtest_forecast <- ts(c(.backtest_ts,.forecast_ts),start=.start,frequency=.frequency);
	.actual_forecast <- ts(c(revenue[,2],.forecast_ts),start=.start,frequency=.frequency);
		
	list(backtest_forecast=.backtest_forecast, actual_forecast=.actual_forecast, 
			actual=.actual, backtesting=.backtesting, 
			baseline=.baseline, adverse=.adverse, severe=.severe, bhc=.bhc,
			start_forecast = as.numeric(as.yearmon(revenue[nrow(revenue),1])),
			
			lookbackperiod = lookbackperiod,
			forecastperiod = forecastperiod,
			model = "movingaverage");
}

plot_mv_model <- function(forecasts, plot_name, title_name, .lookback) {
	notation_baseline <- get_notation(forecasts$actual_forecast);
	png(plot_name,width=800,height=600);
	ts.plot(forecasts$actual,forecasts$backtest_forecast,type="b",col=c(Color_history,Color_backtest), ylab="", main=title_name,gpars=list(yaxt="n",lwd=c(2,1)));
	axis(2,at=axTicks(2), labels=format_level_value(axTicks(2)), las=1);
	abline(v=forecasts$start_forecast);
	abline(v=notation_baseline$Year);
	text(notation_baseline$X,notation_baseline$Y,notation_baseline$Content,col=Color_history);			
	legend("topleft",legend=c("history",paste0("MV",.lookback)),pch=1,col=c(Color_history,Color_backtest));
	dev.off();				
}
