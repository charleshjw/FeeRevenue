# 
# 
# Author: XBBKKL3
###############################################################################

if (!"Macro"%in%ls()) {
	Macro <- read.xlsx(Macro_file,sheetName="Monthly-baseline");
}
if (!"Macro_2"%in%ls()) {
	Macro_2 <- read.xlsx(Macro_file,sheetName="Monthly-adverse");
}
if (!"Macro_3"%in%ls()) {
	Macro_3 <- read.xlsx(Macro_file,sheetName="Monthly-severelyadverse");
}
if (!"Macro_4"%in%ls()) {
	Macro_4 <- read.xlsx(Macro_file,sheetName="Monthly-BHC");
}

if (!"Macro_Q"%in%ls()) {
	Macro_Q <- read.xlsx(Macro_file,sheetName="Quarterly-baseline");
}
if (!"Macro_2_Q"%in%ls()) {
	Macro_2_Q <- read.xlsx(Macro_file,sheetName="Quarterly-adverse");
}
if (!"Macro_3_Q"%in%ls()) {
	Macro_3_Q <- read.xlsx(Macro_file,sheetName="Quarterly-severelyadverse");
}
if (!"Macro_4_Q"%in%ls()) {
	Macro_4_Q <- read.xlsx(Macro_file,sheetName="Quarterly-BHC");
}

if (!"sheets"%in%ls()) {
	wb <- loadWorkbook(revenue_file_version);
	sheets <- getSheets(wb);
}

for (bu in 1:length(BUS)) {
#	bu <- 1;
	#restart from 18
	BU <- BUS[bu];
	print(BU);
	Output_file <- paste(Output_root,BU,sep="");
	if (!dir.exists(Output_file)) {
		dir.create(Output_file);
	}
	
	##Run segment 1
	for (segment in 1:max_segments) {
#		segment <- 1;
		if (paste0(BU,segment)%in%names(sheets)) {
			revenue_lob_raw <- read.xlsx(revenue_file_version,sheetName=paste0(BU,segment));
			Output_file <- paste(Output_root,BU,"/Regression-Summary-Segment-",segment,sep="");
			if (!dir.exists(Output_file)) {
				dir.create(Output_file);
			}
			revenue_lob <- revenue_lob_raw[determine_time_periods(revenue_lob_raw[,1]),];
			revenue_lob <- get_revenues(revenue_lob);
			
			variables_raw <- read.xlsx(variable_file_version,sheetName=paste0(BU,segment));
			
			#Aggregate results
			for (i in 1:nrow(variables_raw)) {
#				i<-1;
				print(i);
				modelset <- variables_raw[i,];
				modelset <- modelset[,!is.na(modelset)];
				.info <- modelset[1];
				if (class(modelset)=="data.frame") {
					if (!"dummy_cs_fundvestassets"%in%colnames(Macro)) {
						source("./Production-AddDummyVariables.R");
					}
					#run regression and get forecasts
					forecasts_raw <- Get_Forecast(revenue_lob, modelset, Output_file, ConvertRevenue=TRUE);##########
					#forecasts_raw <- Get_Forecast(revenue_lob, modelset, Output_file, ConvertRevenue=FALSE);##########
					forecasts <- Aggregate_Forecasts(forecasts_raw,covert_to_quarter=FALSE);
					
					#write original data to output
					file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-Aggregate",".csv",sep="");
					write_model_forecast_to_csv(forecasts, file_name);
					
					#plot regression
					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-Aggregate",".png",sep="");
#					title_name <- paste(BU,"-",.info,"-Aggregated Results",sep="");
					title_name <- paste("Fee Revenue Backtest and Forecast under Scenarios",sep="");
					plot_regression_model(forecasts, plot_name, title_name);
					
					##Plot the annual growth rate table
					Comparison <- plot_comparison_table_regression_model(forecasts);
					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-Aggregate-Table",".png",sep="");
					png(plot_name,width=800,height=30*nrow(Comparison));
					plot_table_with_title(Comparison,paste(BU,"-",.info[1,1]));
					dev.off();
					
					#plot backtest statistic
					Comparison <- actual_backtest_comparison(forecasts);
					Comparison <- Comparison[,3,drop=FALSE];
					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-Aggregate-Backtest",".png",sep="");
					png(plot_name,width=max(150,80*ncol(Comparison)),height=30*nrow(Comparison));
					plot_table_with_title(Comparison,paste(BU,"-",.info[1,1]));
					dev.off();

					#write all info
					modelset_t <- data.frame(t(modelset));
					file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-Info",".png",sep="");
					png(file_name,height=25*nrow(modelset_t));
					grid.table(modelset_t,col=NULL);
					dev.off();
#					rownames(forecasts$actual_union)
					a <- ts.union(forecasts$actual_union);
					b <- ts.union(forecasts$backtesting_union);
					a <- trim_ts(a);
					b <- trim_ts(b);
					a <- aggregate(a,nfrequency=1,sum);
					b <- aggregate(b,nfrequency=1,sum);
					
					Comparison <- (b - a)/a;
					
					Comparison <- data.frame(Comparison,row.names=index(Comparison));
					colnames(Comparison) <- c(colnames(modelset)[2:ncol(modelset)],"Aggregated")
					for (j in 1:ncol(Comparison)) {
						Comparison[,j] <- format_percentage(Comparison[,j]);
					}
					Comparison <- t(Comparison[order(rownames(Comparison),decreasing =TRUE),]);
					
					#aggregate backtest info
					file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-Info2",".png",sep="");
					png(file_name,height=40*nrow(Comparison),width=800);
					plot_table_with_title(Comparison,paste(BU,"-",.info[1,1], " (Model-Actual)/Actual"));
					dev.off();
				}
			}
		}
	}
}
