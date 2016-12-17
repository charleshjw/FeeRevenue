# TODO: Add comment
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

##Start Analysis for each Tabs
for (bu in 1:length(BUS)) {
	# bu <- 1;
	#restart from 18
	BU <- BUS[bu];
	print(BU);
	
	for (segment in 1:max_segments) {
#		segment <- 1;
		if (!paste0(BU,segment)%in%names(sheets)) {
			next;
		}
		#set up file hierarchy
		Output_file <- paste(Output_root,BU,sep="");
		if (!dir.exists(Output_file)) {
			dir.create(Output_file);
		}
		
		###transform original revenue data file to formated revenue masterfile
		revenue_lob_raw <- read.xlsx(revenue_file_version,sheetName=paste0(BU,segment));
		#	revenue_lob_raw[1,]
#		revenue_lob_raw[,1]
		revenue_lob <- revenue_lob_raw[determine_time_periods(revenue_lob_raw[,1]),];
		revenues <- get_revenues(revenue_lob);
		Revenue_Items <- names(revenues);
		
		###regression: monthly external model
		Output_file_ <- paste(Output_file,"/Analysis-ARIMAX-Segment-",segment,sep="");
		if (!dir.exists(Output_file_)) {
			dir.create(Output_file_);
		}
		
		for (i in 1:length(Revenue_Items)) {
			#	i <- 1
			#if no data available, next one
			if(all(is.na(revenues[[i]][,2:3]))) {
				next;
			}
			
			#transform revenue masterfile to revenue format
			revenue <- revenues[[i]][,c(1,2)];
			revenue <- revenue[!is.na(revenue[,2]),];
			
			#Variable Selection
			if (revenue_analysis(revenue,revenue_threshold)) {
				#narrow down macro to limited variables to reduce computational complexity
#				arimax(threshold,Output_file_,revenue,Macro);
				Output_file_arimax <- paste(Output_file_,"/",Revenue_Items[i],sep="");
				if (!file.exists(Output_file_arimax)) {
					dir.create(Output_file_arimax);
				}
				
				revenue_name <- colnames(revenue)[2];
				plot_name <- paste(Output_file_arimax,"/",gsub("[.]","-",revenue_name),"-Diagnostic1",".png",sep="");
				png(plot_name,width=800,height=600);
				par(mfrow=c(2,2));
				acf(revenue[,2],main=paste(revenue_name,": ACF of level value",sep=""));
				pacf(revenue[,2],main=paste(revenue_name,": ACF of level value",sep=""));
				acf(diff(revenue[,2]),main=paste(revenue_name,": ACF of First Order Differencing",sep=""));
				pacf(diff(revenue[,2]),main=paste(revenue_name,": ACF of First Order Differencing",sep=""));
				dev.off();
				
				.all_variables <- colnames(Macro)[2:ncol(Macro)];
				combinations <- t(combn(.all_variables,1));	
				for (j in 1:nrow(combinations)) {
#					j <- 1;
					data <- compile_regression_data(determine_Macro(revenue[,1], Macro, Macro_Q),combinations[j],revenue);
					regression_data <- clean_na_data_frame(data);
					
#					plot_name <- paste(Output_file_arimax,"/forecast-",gsub("[.]","-",revenue_name),"-",combinations[j],".png",sep="");
					plot_name <- paste(Output_file_arimax,"/forecast-",j,".png",sep="");
					png(plot_name,width=800,height=600);
					par(mfrow=c(3,3));
					for (ar in 0:2) {
#						ar <- 1;
						for (ma in 0:2) {
#							ma <- 1;
							tryCatch({							
										fit <- arima(data[,2], order=c(ar,1,ma), xreg=data[,3]);
										variables <- combinations[j];
										forecasts <- get_forecasts_arimax(fit,regression_data,data,revenue,variables);
										
										title_name <- paste(BU,revenue_name,combinations[j],"AR",ar,"D",1,"MA",ma,sep="-")
										ts.plot(forecasts$actual,forecasts$backtesting, forecasts$baseline, forecasts$adverse, forecasts$severe, forecasts$bhc,type="b",col=c(Color_history,Color_backtest,Color_baseline,Color_adverse,Color_severe,Color_bhc), ylab="", main=title_name, sub=forecasts$formula);
										abline(v=forecasts$start_forecast);
									},error=function(e){
										title_name <- paste(BU,revenue_name,combinations[j],"AR",ar,"D",1,"MA",ma,sep="-")
										plot(0, main=title_name);
									},finally={})

						}
					}
					dev.off();
				}
			}
		}
	}
}

