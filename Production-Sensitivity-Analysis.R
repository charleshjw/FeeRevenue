# 
# 
# Author: XBBKKL3
###############################################################################

#rm(list=ls());

#BUS_ALL <- c("CT2");

if (!"Macro"%in%ls()) {
	Macro_Original <- Macro <- read.xlsx(Macro_file,sheetName="Monthly-baseline");
}
if (!"Macro_2"%in%ls()) {
	Macro_2_Original <- Macro_2 <- read.xlsx(Macro_file,sheetName="Monthly-adverse");
}
if (!"Macro_3"%in%ls()) {
	Macro_3_Original <- Macro_3 <- read.xlsx(Macro_file,sheetName="Monthly-severelyadverse");
}
if (!"Macro_4"%in%ls()) {
	Macro_4_Original <- Macro_4 <- read.xlsx(Macro_file,sheetName="Monthly-BHC");
}

if (!"Macro_Q"%in%ls()) {
	Macro_Q_Original <- Macro_Q <- read.xlsx(Macro_file,sheetName="Quarterly-baseline");
}
if (!"Macro_2_Q"%in%ls()) {
	Macro_2_Q_Original <- Macro_2_Q <- read.xlsx(Macro_file,sheetName="Quarterly-adverse");
}
if (!"Macro_3_Q"%in%ls()) {
	Macro_3_Q_Original <- Macro_3_Q <- read.xlsx(Macro_file,sheetName="Quarterly-severelyadverse");
}
if (!"Macro_4_Q"%in%ls()) {
	Macro_4_Q_Original <- Macro_4_Q <- read.xlsx(Macro_file,sheetName="Quarterly-BHC");
}

if (!"sheets"%in%ls()) {
	wb <- loadWorkbook(revenue_file_version);
	sheets <- getSheets(wb);
}

source("./Production-AddDummyVariables.R");

BU_ALL <- "BK";
Output_file <- paste(Output_root,BU_ALL,sep="");
if (!dir.exists(Output_file)) {
	dir.create(Output_file);
}

Output_file <- paste(Output_root,BU_ALL,"/Regression-Summary-Segment-1",sep="");
if (!dir.exists(Output_file)) {
	dir.create(Output_file);
}

#read files from excel
BUS_ALL <- c("AS1","Clearing1","CT2","CashMgmt2", "IM1", "BDS2", "FX1", "SL2", "DR1");

if (!"revenue_lob_raw_list"%in%ls()) {
	revenue_lob_raw_list <- list();
	variables_raw_list <- list();
	for (bu in 1:length(BUS_ALL)) {
#	bu <- 1;
		#restart from 18
		BU <- BUS_ALL[bu];
		print(BU);
		
		if (BU%in%names(sheets)) {
			revenue_lob_raw <- read.xlsx(revenue_file_version,sheetName=BU);
			revenue_lob_raw_list[[bu]] <- revenue_lob_raw;
			names(revenue_lob_raw_list)[[bu]] <- BU;
			
			variables_raw <- read.xlsx(variable_file_version,sheetName=BU);
			variables_raw_list[[bu]] <- variables_raw;
			names(variables_raw_list)[[bu]] <- BU;
		} else {
			stop("Wrong Sheet Name Specifications");
		}
	}
}


#####PCA Analysis and Scenario Generation
#Macro <- read.xlsx(Macro_file,sheetName="Monthly-baseline");
pca <- prcomp(na.omit(Macro_Original[,2:ncol(Macro_Original)]), scale. = TRUE);
shifts <- pca$scale*pca$rotation[,1];
shifts <- shifts/shifts[1]*21;
file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-PCAResults",".csv",sep="");
write.csv(shifts,file_name);

forecasts_sensitivity <- list();
plot_index <- 1;
for (size_shift in c(0,-10,-20,-30,-40,-50)) {
#	size_shift <- -20;
	pca_shift <- shifts*size_shift;
	for (j in 2:ncol(Macro_Original)) {
#	j <- 76
		Macro[seq(max(Time_periods),nrow(Macro_Original)),j] <- Macro_Original[seq(max(Time_periods),nrow(Macro_Original)),j]+pca_shift[j-1];
		Macro_2[seq(max(Time_periods),nrow(Macro_Original)),j] <- Macro_2_Original[seq(max(Time_periods),nrow(Macro_Original)),j]+pca_shift[j-1];
		Macro_3[seq(max(Time_periods),nrow(Macro_Original)),j] <- Macro_3_Original[seq(max(Time_periods),nrow(Macro_Original)),j]+pca_shift[j-1];
		Macro_4[seq(max(Time_periods),nrow(Macro_Original)),j] <- Macro_4_Original[seq(max(Time_periods),nrow(Macro_Original)),j]+pca_shift[j-1];
	}
	for (j in 2:ncol(Macro_Original)) {
#	j <- 76
		Macro_Q_Original[seq(max(Time_periods_Q),nrow(Macro_Q_Original)),j] <- Macro_Q_Original[seq(max(Time_periods_Q),nrow(Macro_Q_Original)),j]+pca_shift[j-1];
		Macro_2_Q_Original[seq(max(Time_periods_Q),nrow(Macro_Q_Original)),j] <- Macro_2_Q_Original[seq(max(Time_periods_Q),nrow(Macro_Q_Original)),j]+pca_shift[j-1];
		Macro_3_Q_Original[seq(max(Time_periods_Q),nrow(Macro_Q_Original)),j] <- Macro_3_Q_Original[seq(max(Time_periods_Q),nrow(Macro_Q_Original)),j]+pca_shift[j-1];
		Macro_4_Q_Original[seq(max(Time_periods_Q),nrow(Macro_Q_Original)),j] <- Macro_4_Q_Original[seq(max(Time_periods_Q),nrow(Macro_Q_Original)),j]+pca_shift[j-1];
	}
	
	forecasts_list <- list();
	for (bu in 1:length(BUS_ALL)) {
#	bu <- 3;
		#restart from 18
		BU <- BUS_ALL[bu];
		print(BU);
		
		if (BU%in%names(sheets)) {
#		revenue_lob_raw <- read.xlsx(revenue_file_version,sheetName=BU);
			revenue_lob_raw <- revenue_lob_raw_list[[BU]];
			revenue_lob <- revenue_lob_raw[determine_time_periods(revenue_lob_raw[,1]),];
			revenue_lob <- get_revenues(revenue_lob);
			
#		variables_raw <- read.xlsx(variable_file_version,sheetName=BU);
			variables_raw <- variables_raw_list[[BU]];
			
			modelset <- variables_raw[1,];
			modelset <- modelset[,!is.na(modelset)];
			
			#run regression and get forecasts
			forecasts_raw <- Get_Forecast(revenue_lob, modelset, Output_file, output=FALSE, Macro=Macro,Macro_2=Macro_2,Macro_3=Macro_3,Macro_4=Macro_4,Macro_Q=Macro_Q,Macro_2_Q=Macro_2_Q,Macro_3_Q=Macro_3_Q,Macro_4_Q=Macro_4_Q);
			forecasts_list[[length(forecasts_list)+1]] <- Aggregate_Forecasts(forecasts_raw,covert_to_quarter=TRUE);
		} else {
			stop("Wrong Sheet Name Specifications");
		}
	}

	for (i in 1:length(forecasts_list)) {
		if (i == 1) {
			forecasts <- forecasts_list[[1]];
		} else {
			forecasts <- Add_Aggreated_Forecasts(forecasts,forecasts_list[[i]]);
		}
	}
	
#	BU <- "BK";
##write original data to output
##	file_name <- paste(Output_file,"/forecast-",BU,"-Model1-Aggregate",".csv",sep="");
##	write_model_forecast_to_csv(forecasts, file_name);
#	
#plot regression
	plot_name <- paste(Output_file,"/forecast-Aggregate-Shift-",plot_index,".png",sep="");
	title_name <- paste("SP500 down by ",pca_shift[1], " points",sep="");
	plot_regression_model(forecasts, plot_name, title_name);
	
	##Plot the annual growth rate table
	Comparison <- plot_comparison_table_regression_model(forecasts);
	plot_name <- paste(Output_file,"/forecast-Aggregate-Shift-",plot_index,"-Table.png",sep="");
	png(plot_name,width=800,height=30*nrow(Comparison));
	plot_table_with_title(Comparison,title_name);
	dev.off();
#	
##plot backtest statistic
#	Comparison <- actual_backtest_comparison(forecasts);
#	plot_name <- paste(Output_file,"/forecast-",BU,"-Model1-Aggregate-Backtest",".png",sep="");
#	png(plot_name,width=800,height=30*nrow(Comparison));
#	plot_table_with_title(Comparison,paste(BU,"-",.info[1,1]));
#	dev.off();
	plot_index <- plot_index + 1;
	forecasts_sensitivity[[length(forecasts_sensitivity)+1]] <- forecasts;
}

#names(forecasts_sensitivity[[1]])
for (i in 1:length(forecasts_sensitivity)) {
	if (i == 1) {
		forecast_baseline <- forecasts_sensitivity[[1]]$baseline;
		forecast_adverse <- forecasts_sensitivity[[1]]$adverse;
		forecast_severe<- forecasts_sensitivity[[1]]$severe;
		forecast_bhc <- forecasts_sensitivity[[1]]$bhc;
	}
	forecast_baseline <- ts.union(forecast_baseline,forecasts_sensitivity[[i]]$baseline);
	forecast_adverse <- ts.union(forecast_adverse,forecasts_sensitivity[[i]]$adverse);
	forecast_severe <- ts.union(forecast_severe,forecasts_sensitivity[[i]]$severe);
	forecast_bhc <- ts.union(forecast_bhc,forecasts_sensitivity[[i]]$bhc);
}

plot_name <- paste(Output_file,"/forecast-Aggregate-Sensitivity.png",sep="");
png(plot_name,width=1000,height=600);

par(mfrow=c(2,2))

title_name <- paste(BU,"-",.info,"-Baseline Sensitivity",sep="");
ts.plot(forecasts$actual,forecasts$backtesting, forecast_baseline,type="b",col=c(Color_history,Color_backtest,rep(Color_baseline,length(forecast_baseline))), ylab="", main=title_name, sub=forecasts$formula);
abline(v=forecasts$start_forecast);
legend("topleft",legend=c("history","backtest","baseline","adverse","severely adverse","BHC"),pch=1,col=c(Color_history,Color_backtest,Color_baseline,Color_adverse,Color_severe,Color_bhc));

title_name <- paste(BU,"-",.info,"-Adverse Sensitivity",sep="");
ts.plot(forecasts$actual,forecasts$backtesting, forecast_adverse,type="b",col=c(Color_history,Color_backtest,rep(Color_adverse,length(forecast_baseline))), ylab="", main=title_name, sub=forecasts$formula);
abline(v=forecasts$start_forecast);
legend("topleft",legend=c("history","backtest","baseline","adverse","severely adverse","BHC"),pch=1,col=c(Color_history,Color_backtest,Color_baseline,Color_adverse,Color_severe,Color_bhc));

title_name <- paste(BU,"-",.info,"-Severe Adverse Sensitivity",sep="");
ts.plot(forecasts$actual,forecasts$backtesting, forecast_severe,type="b",col=c(Color_history,Color_backtest,rep(Color_severe,length(forecast_baseline))), ylab="", main=title_name, sub=forecasts$formula);
abline(v=forecasts$start_forecast);
legend("topleft",legend=c("history","backtest","baseline","adverse","severely adverse","BHC"),pch=1,col=c(Color_history,Color_backtest,Color_baseline,Color_adverse,Color_severe,Color_bhc));

title_name <- paste(BU,"-",.info,"-BHC Sensitivity",sep="");
ts.plot(forecasts$actual,forecasts$backtesting, forecast_bhc,type="b",col=c(Color_history,Color_backtest,rep(Color_bhc,length(forecast_baseline))), ylab="", main=title_name, sub=forecasts$formula);
abline(v=forecasts$start_forecast);
legend("topleft",legend=c("history","backtest","baseline","adverse","severely adverse","BHC"),pch=1,col=c(Color_history,Color_backtest,Color_baseline,Color_adverse,Color_severe,Color_bhc));

dev.off();


#names(pca)
#pca$center
#pca$scale
#
#pca$rotation[,1:3]
#
#plot_name <- paste(Output_file,"/forecast-",BU,"-Model1-BK-Diagnostic1",".png",sep="");
#png(plot_name,width=1000,height=800);
#biplot(pca, scale = 0);
#dev.off();
#
#pca_variance <- pca$sdev^2;
#pca_var <- pca_variance/sum(pca_variance);
#
#plot_name <- paste(Output_file,"/forecast-",BU,"-Model1-BK-Diagnostic2",".png",sep="");
#png(plot_name,width=800,height=600);
#plot(pca_var, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b");
#dev.off();
#
#pca$center[1];
#cbind(pca$center,(pca$center + pca$scale*pca$rotation[,1]));
#Macro[1,];
#dim(Macro);
#dim(pca$rotation);
#plot(pca$rotation[,1],type="b");
#abline(h=0);
#text(1:length(pca$rotation),-0.15,"aa")
#plot(pca$rotation[,2],type="b");
#plot(pca$rotation[,3],type="b");
#plot(pca$scale)




