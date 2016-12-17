# 
# 
# Author: XBBKKL3
###############################################################################

if (!"Macro"%in%ls()) {
	Macro <- read.xlsx("./Data/Masterfile-Data Process.xlsx",sheetName="Monthly-baseline");
}
if (!"Macro_2"%in%ls()) {
	Macro_2 <- read.xlsx("./Data/Masterfile-Data Process.xlsx",sheetName="Monthly-adverse");
}
if (!"Macro_3"%in%ls()) {
	Macro_3 <- read.xlsx("./Data/Masterfile-Data Process.xlsx",sheetName="Monthly-severelyadverse");
}
if (!"Macro_4"%in%ls()) {
	Macro_4 <- read.xlsx("./Data/Masterfile-Data Process.xlsx",sheetName="Monthly-BHC");
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
get_variables <- function(Macro,variables,revenue) {
	.select_row <- Macro[,1]%in%revenue[,1];
	.select_colume <- colnames(Macro)%in%variables;
	if (sum(.select_colume)!=length(variables)) {
		stop("Variables names are not in the list");
	}
	Macro[.select_row,.select_colume];
}

get_forecasts <- function(fit,data,data_2,revenue,.variables) {
	#			.variables <- variables;	
	# data <- regression_data;
	#historical actual
	start_and_frequency <- determine_frequency_and_start(data[,1]);
	Historical_metric <- ts(data[,2],start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#historical backtesting
	start_and_frequency <- determine_frequency_and_start(data_2[,1]);
	Backtesting <- ts(fit$fitted.values,start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#Forecast baseline
	.forecast_data <- compile_forecast_data(Macro,.variables,revenue);
	start_and_frequency <- determine_frequency_and_start(.forecast_data[,1]);
	Forecast_baseline <- ts(predict(fit,.forecast_data),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#Forecast advser
	.forecast_data <- compile_forecast_data(Macro_2,.variables,revenue);
	start_and_frequency <- determine_frequency_and_start(.forecast_data[,1]);	
	Forecast_adverse <- ts(predict(fit,.forecast_data),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#Forecast severe
	.forecast_data <- compile_forecast_data(Macro_3,.variables,revenue);
	start_and_frequency <- determine_frequency_and_start(.forecast_data[,1]);
	Forecast_severe <- ts(predict(fit,.forecast_data),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	#Forecast bhc
	.forecast_data <- compile_forecast_data(Macro_4,.variables,revenue);
	start_and_frequency <- determine_frequency_and_start(.forecast_data[,1]);
	Forecast_bhc <- ts(predict(fit,.forecast_data),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
	
	list(actual=Historical_metric,backtesting=Backtesting,baseline=Forecast_baseline,
			adverse=Forecast_adverse,severe=Forecast_severe,bhc=Forecast_bhc);
}

Aggregate_Forecast <- function(segment2,output=TRUE) {
#	segment2 <- segment
	for (j in 1:length(segment2)) {
#			j <- 5;
		regressor_name <- colnames(modelset)[colnames(modelset)==segment2[j]];
		revenue <- get_revenue(revenue_lob,regressor_name);
		
		.variables_string <- as.character(unlist(modelset[colnames(modelset)==segment2[j]]));
		
		variables <- unlist(strsplit(.variables_string,","));
		variables <- trim(variables);
		
		regression_data <- cbind(revenue,get_variables(Macro,variables,revenue));
		.missingvalueflag <- !is.na(regression_data[,2]);
		for (k in 3:ncol(regression_data)) {
			.missingvalueflag <- .missingvalueflag&!is.na(regression_data[,k]);
		}
		data <- regression_data[.missingvalueflag,];
		fit <- fit_regression(regression_data);
		
		forecasts <- get_forecasts(fit,regression_data,data,revenue,variables);
		
		.forecast_matrix <- ts.union(forecasts$actual,forecasts$backtesting,forecasts$baseline,forecasts$adverse,forecasts$severe,forecasts$bhc);
		if (output) {
			file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",segment2[j],".csv",sep="");
			write.csv(.forecast_matrix,file_name);	
			
			file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",segment2[j],".png",sep="");
			png(file_name,width=1000,height=600);
			forecast_ts_month <- ts(.forecast_matrix,frequency=determine_frequency_and_start(as.Date(time(.forecast_matrix[,1])))$frequency,start=determine_frequency_and_start(as.Date(time(.forecast_matrix[,1])))$start);
			ts.plot(forecast_ts_month,col=c(Color_black,Color_blue,Color_green,Color_teal,Color_bronze,Color_red),type="b",ylab="Expense",sub=formu_display(data,fit))
			
			#add growth rate
			forecast_ts_annual <- aggregate(forecast_ts_month,nfrequency=1)
			baseline <- forecast_ts_annual[,1];
			baseline[is.na(baseline)] <- forecast_ts_annual[,3][is.na(baseline)];
			
			.x <- as.numeric(time(baseline)+0.5);
			.level <- format_level_value(baseline);
			.grwothrate <- paste("(",c(NA,round(diff(baseline)/baseline[-length(baseline)]*100,0)),"%",")",sep="");
			
			abline(v=as.numeric(time(baseline)));
			title(paste(BU,"-",.info[1,1],"-",regressor_name,"    R^2=",round(summary(fit)$r.squared,2),sep=""));
			legend("topleft",legend=c("baseline","adverse","severely adverse","BHC"),pch=1,col=c(Color_green,Color_teal,Color_bronze,Color_red));			
			text(.x[-1],max(forecast_ts_month[,1],na.rm=TRUE),paste(.level,.grwothrate)[-1],col=Color_blue);
			dev.off();
		}
		
		if (j == 1) {
			Aggregated_Results2 <- .forecast_matrix;
		} else {
			Aggregated_Results2 <- Aggregated_Results2 + .forecast_matrix;
		}
	}
	colnames(Aggregated_Results2) <- c("actuals","backtesting","baseline","adverse","severe","bhc");
	
	if (output) {
		file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-Aggregate",".csv",sep="");
		write.csv(Aggregated_Results2,file_name);
	}
	
	Aggregated_Results2;
}


for (bu in 1:length(BUS)) {
	# bu <- 1;
	#restart from 18
	BU <- BUS[bu];
	print(BU);
	#set up file hierarchy
	Output_file <- paste(Output_root,BU,sep="");
	if (!dir.exists(Output_file)) {
		dir.create(Output_file);
	}
	Output_file <- paste(Output_root,BU,"/MAQRegression-Summary",sep="");
	if (!dir.exists(Output_file)) {
		dir.create(Output_file);
	}
	
	###transform original revenue data file to formated revenue masterfile
	revenue_lob_raw <- read.xlsx(expense_file_version,sheetName=BU);
	revenue_lob <- data.frame(revenue_lob_raw[Time_periods,]);
	revenue_lob <- adjust_expense_for_risk(revenue_lob); #exclude operational risk losses
	revenue_lob <- adjust_expense_for_MNINEW(revenue_lob); #exclude operational risk losses
	
	variables_raw <- read.xlsx(variable_file_maq_version,sheetName=BU);
	
	#Validity Check
#	i <- 1;
	
	#Aggregate results
	for (i in 1:nrow(variables_raw)) {
#		i<-1
		print(i);
		modelset <- variables_raw[i,];
		modelset <- modelset[,!is.na(modelset)];
		.info <- modelset[1];
		segment <- colnames(modelset)[-1];
		if (!is.null(segment)) {
			#run regression and get forecasts
			forecast1 <- Aggregate_Forecast(segment);
			
			file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-Aggregate",".png",sep="");
			png(file_name,width=1000,height=600);
			forecast_ts_month <- ts(forecast1,frequency=determine_frequency_and_start(as.Date(time(forecast1[,1])))$frequency,start=determine_frequency_and_start(as.Date(time(forecast1[,1])))$start);
			ts.plot(forecast_ts_month,col=c(Color_black,Color_blue,Color_green,Color_teal,Color_bronze,Color_red),type="b",ylab="Expense")
			
			#add growth rate
			forecast_ts_annual <- aggregate(forecast_ts_month,nfrequency=1)
			baseline <- forecast_ts_annual[,1];
			baseline[is.na(baseline)] <- forecast_ts_annual[,3][is.na(baseline)];
			
			.x <- as.numeric(time(baseline)+0.5);
			.level <- format_level_value(baseline);
			.grwothrate <- paste("(",c(NA,round(diff(baseline)/baseline[-length(baseline)]*100,0)),"%",")",sep="");
			
			abline(v=as.numeric(time(baseline)));
			title(paste(BU,"-",.info[1,1],"-",paste(segment,collapse="/"),collapse="/"));
			legend("topleft",legend=c("baseline","adverse","severely adverse","BHC"),pch=1,col=c(Color_green,Color_teal,Color_bronze,Color_red));			
			text(.x[-1],max(forecast_ts_month[,1],na.rm=TRUE),paste(.level,.grwothrate)[-1],col=Color_blue);
			dev.off();	
			#write regression info
			modelset_t <- data.frame(t(modelset));
			file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-Info",".png",sep="");
			png(file_name,height=25*nrow(modelset_t));
			grid.table(modelset_t,col=NULL);
			dev.off();	
		}
	}
}





