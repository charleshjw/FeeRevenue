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

#if monthly observation
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
		Output_file <- paste(Output_root,BU,"/Analysis-SeaonalDecomposition-Segment-",segment,sep="");
		if (!file.exists(Output_file)) {
			dir.create(Output_file);
		}
		
		for (i in 1:length(revenue_lob)) {
			#		i <- 2;
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

			m1 <- detect_seasonality_method1(historical_data);
			m2 <- detect_seasonality_method2(historical_data);
			
			Results <- data.frame(ETSResult=m1$Result,ETSStatistic=m1$Statistic,TBATSResult=m2$Result,TBATStatistic=m2$Statistic);
			file_name <- paste(Output_file_item,"/SeasonalDecompositionTestResults.csv",sep="");
			write.csv(Results,file_name);
			
			plot_name <- paste(Output_file_item,"/SeasonalDecompositionTestResults.png",sep="");
			png(plot_name,width=800,height=35*nrow(Results));
			grid.table(Results);
			dev.off();
			
			
			#####Model1: Seasonal Model STL
			#STL Model
			model <- stl(historical_data,"periodic");
			
			plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel1PlotTime0.png",sep="");
			png(plot_name,width=800,height=600);
			plot(model, main=paste(Revenue_Items[i],"Seasonality Decomposition Model - STL Model"));
			title(sub=model$call);
			dev.off();
			
			backtest <- ts(apply(model$time.series[,1:2],1,sum), start=start(model$time.series[,1]), frequency=frequency(model$time.series[,1]));
			plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel1BackTestTime0.png",sep="");
			png(plot_name,width=800,height=600);
			ts.plot(historical_data,backtest,col=1:2,type="b",main=paste(Revenue_Items[i],"Seasonality Decomposition Model - STL Model Backtest"));
			dev.off()
			
			comp.tmp <- compare_time_series(historical_data, backtest, 1);
			Comparison <- cbind(comp.tmp$Comp_Present,format_percentage((comp.tmp$Comp_Orig[,2]-comp.tmp$Comp_Orig[,1])/comp.tmp$Comp_Orig[,1]));
			colnames(Comparison) <- c("Actual","Backtest","(B-A)/A");
			
			plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel1TableTime0.png",sep="");
			png(plot_name,width=800,height=30*nrow(Comparison));
			grid.table(Comparison);
			dev.off();
			
			if (min(time(historical_data)) < 2009) {
				model <- stl(window(historical_data,c(2009,1)),"periodic");
				
				plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel1PlotTime1.png",sep="");
				png(plot_name,width=800,height=600);
				plot(model, main=paste(Revenue_Items[i],"Seasonality Decomposition Model - STL Model (2009-Current)"));
				title(sub=model$call);
				dev.off();
				
				backtest <- ts(apply(model$time.series[,1:2],1,sum), start=start(model$time.series[,1]), frequency=frequency(model$time.series[,1]));
				plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel1BackTestTime1.png",sep="");
				png(plot_name,width=800,height=600);
				ts.plot(window(historical_data,c(2009,1)),backtest,col=1:2,type="b",main=paste(Revenue_Items[i],"Seasonality Decomposition Model - STL Model Backtest  (2009-Current)"));
				dev.off()
				
				comp.tmp <- compare_time_series(window(historical_data,c(2009,1)), backtest, 1);
				Comparison <- cbind(comp.tmp$Comp_Present,format_percentage((comp.tmp$Comp_Orig[,2]-comp.tmp$Comp_Orig[,1])/comp.tmp$Comp_Orig[,1]));
				colnames(Comparison) <- c("Actual","Backtest","(B-A)/A");
				
				plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel1TableTime1.png",sep="");
				png(plot_name,width=800,height=30*nrow(Comparison));
				grid.table(Comparison);
				dev.off();
			}
			
			if (min(time(historical_data)) < 2012) {
				model <- stl(window(historical_data,c(2012,1)),"periodic");
				
				plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel1PlotTime2.png",sep="");
				png(plot_name,width=800,height=600);
				plot(model, main=paste(Revenue_Items[i],"Seasonality Decomposition Model - STL Model (2012-Current)"));
				title(sub=model$call);
				dev.off();
				
				backtest <- ts(apply(model$time.series[,1:2],1,sum), start=start(model$time.series[,1]), frequency=frequency(model$time.series[,1]));
				plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel1BackTestTime2.png",sep="");
				png(plot_name,width=800,height=600);
				ts.plot(window(historical_data,c(2012,1)),backtest,col=1:2,type="b",main=paste(Revenue_Items[i],"Seasonality Decomposition Model - STL Model Backtest  (2012-Current)"));
				dev.off()
				
				comp.tmp <- compare_time_series(window(historical_data,c(2012,1)), backtest, 1);
				Comparison <- cbind(comp.tmp$Comp_Present,format_percentage((comp.tmp$Comp_Orig[,2]-comp.tmp$Comp_Orig[,1])/comp.tmp$Comp_Orig[,1]));
				colnames(Comparison) <- c("Actual","Backtest","(B-A)/A");
				
				plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel1TableTime2.png",sep="");
				png(plot_name,width=800,height=30*nrow(Comparison));
				grid.table(Comparison);
				dev.off();
			}
			
			#####Model2: ets model
			model <- ets(historical_data);
			if (model$components[3]!="N") {
				season <- model$states[,"s1"];
				level <- model$states[,"l"];
				
				plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel2PlotTime0.png",sep="");
				png(plot_name,width=800,height=600);
				plot(model);
				title(sub=paste(Revenue_Items[i],""));
				dev.off();
				
				backtest <- season + level;
				
				plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel2BackTestTime0.png",sep="");
				png(plot_name,width=800,height=600);
				ts.plot(historical_data,backtest,type="b",col=1:2,main=paste(Revenue_Items[i],"Seasonality Decomposition Model - ETS Model Backtest"));
				dev.off();

				comp.tmp <- compare_time_series(historical_data, backtest, 1);
				Comparison <- cbind(comp.tmp$Comp_Present,format_percentage((comp.tmp$Comp_Orig[,2]-comp.tmp$Comp_Orig[,1])/comp.tmp$Comp_Orig[,1]));
				colnames(Comparison) <- c("Actual","Backtest","(B-A)/A");
				
				plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel2TableTime0.png",sep="");
				png(plot_name,width=800,height=30*nrow(Comparison));
				grid.table(Comparison);
				dev.off();
			}
			
			if (min(time(historical_data)) < 2009) {
				model <- ets(window(historical_data,c(2009,1)));
				if (model$components[3]!="N") {
					season <- model$states[,"s1"];
					level <- model$states[,"l"];
					
					plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel2PlotTime1.png",sep="");
					png(plot_name,width=800,height=600);
					plot(model);
					title(sub=paste(Revenue_Items[i],"(2009-Current)"));
					dev.off();
					
					backtest <- season + level;
					
					plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel2BackTestTime1.png",sep="");
					png(plot_name,width=800,height=600);
					ts.plot(historical_data,backtest,type="b",col=1:2,main=paste(Revenue_Items[i],"Seasonality Decomposition Model - ETS Model Backtest (2009-Current)"));
					dev.off();
					
					comp.tmp <- compare_time_series(historical_data, backtest, 1);
					Comparison <- cbind(comp.tmp$Comp_Present,format_percentage((comp.tmp$Comp_Orig[,2]-comp.tmp$Comp_Orig[,1])/comp.tmp$Comp_Orig[,1]));
					colnames(Comparison) <- c("Actual","Backtest","(B-A)/A");
					
					plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel2TableTime1.png",sep="");
					png(plot_name,width=800,height=30*nrow(Comparison));
					grid.table(Comparison);
					dev.off();				
				}
			}
			
			if (min(time(historical_data)) < 2012) {
				model <- ets(window(historical_data,c(2012,1)));
				if (model$components[3]!="N") {
					season <- model$states[,"s1"];
					level <- model$states[,"l"];
					
					plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel2PlotTime2.png",sep="");
					png(plot_name,width=800,height=600);
					plot(model);
					title(sub=paste(Revenue_Items[i],"(2012-Current)"));
					dev.off();
					
					backtest <- season + level;
					
					plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel2BackTestTime2.png",sep="");
					png(plot_name,width=800,height=600);
					ts.plot(historical_data,backtest,type="b",col=1:2,main=paste(Revenue_Items[i],"Seasonality Decomposition Model - ETS Model Backtest (2012-Current)"));
					dev.off();
					
					comp.tmp <- compare_time_series(historical_data, backtest, 1);
					Comparison <- cbind(comp.tmp$Comp_Present,format_percentage((comp.tmp$Comp_Orig[,2]-comp.tmp$Comp_Orig[,1])/comp.tmp$Comp_Orig[,1]));
					colnames(Comparison) <- c("Actual","Backtest","(B-A)/A");
					
					plot_name <- paste(Output_file_item,"/SeasonalDecompositionModel2TableTime2.png",sep="");
					png(plot_name,width=800,height=30*nrow(Comparison));
					grid.table(Comparison);
					dev.off();
				}
			}
		}
	}
}



