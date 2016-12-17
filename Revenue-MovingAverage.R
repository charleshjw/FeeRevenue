# TODO: Add comment
# 
# Author: XBBKKL3
###############################################################################


if (!"sheets"%in%ls()) {
	wb <- loadWorkbook(revenue_file_version);
	sheets <- getSheets(wb);
}

###Configuration
#parameter: number of lookback periods
#if quarterly observation
LookBackPeriods_Q <- c(2,4,6,8);
#if monthly observation
LookBackPeriods <- c(6,12,18,24);

for (bu in 1:length(BUS)) {
	# bu <- 1;
	#restart from 18
	BU <- BUS[bu];
	print(BU);
	
	for (segment in 1:max_segments) {
#		segment <- 3;
		if (!paste0(BU,segment)%in%names(sheets)) {
			next;
		}
		
		revenue_lob_raw <- read.xlsx(revenue_file_version,sheetName=paste0(BU,segment));
		#	revenue_lob_raw[1,];
		#	revenue_lob_raw[,1];
		
		revenue_lob <- revenue_lob_raw[determine_time_periods(revenue_lob_raw[,1]),];
		revenue_lob <- get_revenues(revenue_lob);
		Revenue_Items <- names(revenue_lob);
		
		Output_file <- paste(Output_root,BU,sep="");
		if (!file.exists(Output_file)) {
			dir.create(Output_file);
		}
		
		##Plot the CCF between expense items with external variables
		Output_file <- paste(Output_root,BU,"/Analysis-MovingAverage-Segment-",segment,sep="");
		if (!file.exists(Output_file)) {
			dir.create(Output_file);
		}
		
		for (i in 1:length(revenue_lob)) {
			#		i <- 1;
			revenue <- revenue_lob[[i]];
			if (all(is.na(revenue[,2]))) {
				next;
			}
			revenue <- revenue[!is.na(revenue[,2]),1:2];
			
			if (!revenue_analysis_seasonalmodel(revenue)) next;
			
			Output_file_item <- paste(Output_file,"/",names(revenue_lob)[i],sep="");
			if (!file.exists(Output_file_item)) {
				dir.create(Output_file_item);
			}
			
			#MV function
			#revenue, LookBackPeriods, LookBackPeriods_Q
			start_and_frequency <- determine_frequency_and_start(revenue[,1]);
			.frequency <- start_and_frequency$frequency;
			.start <- start_and_frequency$start;

			historical_data <- ts(revenue[,2],start=.start,frequency=.frequency);
			
			if (.frequency==12) {
				.lookback <- LookBackPeriods;
				.forecastperiod <- 36;
			} else if(.frequency==4) {
				.lookback <- LookBackPeriods_Q;
				.forecastperiod <- 12;
			} else {
				stop("Error: Look Back Periods");
			}
			
			model <- list();
			start_forecast_date <- list();
			for (j in 1:length(.lookback)) {
#				j <- 1;
				if (length(historical_data) > .lookback[j]*1.5) {
					.tmp <- forecast_ts_mv(revenue,.lookback[j],.forecastperiod);
					model[[j]] <- .tmp$backtest_forecast;
					start_forecast_date[[j]] <- .tmp$start_forecast;
				}
			}
			
			model_union <- model[[1]];
			if (j > 2) {
				for (j in 2:length(model)) {
					model_union <- ts.union(model_union, model[[j]])
				}
			}
			
			#Test 1: visualization
			plot_name <- paste(Output_file_item,"/MovingAverageCombinedPlot.png",sep="");
			png(plot_name,width=800,height=600);
			ts.plot(historical_data,model_union,type="b",col=1:(1+length(model)), ylab="", main=paste(Revenue_Items[i], "Moving Average Model (",paste(.lookback,collapse ="/"),")",sep=""));
			for (j in 1:length(model)) {
				abline(v=start_forecast_date[j]);
			}
			legend("topleft",legend=c("history",paste0("MV",.lookback)),pch=1,col=1:(1+length(model)));
			dev.off();
			
			file_name <- paste(Output_file_item,"/OriginalComparison.csv",sep="");
			OriginalComparison <- data.frame(ts.union(historical_data,model_union),row.names=index(model_union));
			write.csv(OriginalComparison,file_name);
			QuarterComparison <- ts.union(aggregate(historical_data,nfrequency=4,FUN=sum),aggregate(model_union,nfrequency=4,FUN=sum))
			QuarterComparison <- data.frame(QuarterComparison,row.names=index(QuarterComparison));
			file_name <- paste(Output_file_item,"/QuarterlyComparison.csv",sep="");
			write.csv(QuarterComparison,file_name);
			
			
			#Test 2: statistical comparison
#			historical_quarter <- aggregate(historical_data,nfrequency=4,FUN=sum)
			historical_year <- aggregate(historical_data,nfrequency=1,FUN=sum)
			
#			model_quarter <- aggregate(model_union,nfrequency=4,FUN=sum)
			model_year <- aggregate(model_union,nfrequency=1,FUN=sum)
			
#			data.frame(ts.union(historical_quarter,model_quarter),row.names=index(model_quarter));
			AnnualComparison <- data.frame(ts.union(historical_year,model_year),row.names=index(model_year));
			colnames(AnnualComparison) <- c("History",paste0("MV",.lookback[1:length(model)]));
			AnnualComparison <- round(AnnualComparison,digits=0);
			for (j in 1:ncol(AnnualComparison)) {
				AnnualComparison[,j] <- format_level_value(AnnualComparison[,j])
			}
			plot_name <- paste(Output_file_item,"/MovingAverageCombinedTable.png",sep="");
			png(plot_name,width=800,height=30*nrow(AnnualComparison));
			grid.table(AnnualComparison);
			dev.off();
			
		}
	}
}



