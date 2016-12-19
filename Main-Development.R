# Charles edit it.
# 
# Author: XBBKKL3
###############################################################################
#rm(list=ls())
setwd("/Users/charleshjw/Workspace/FeeRevenue");
source("./Library/Functions.R");
source("./Library/Functions-Revenue.R");
source("./Library/Functions-MovingAverage.R");
source("./Library/Functions-SeasonalDecomposition.R");
source("./Library/Functions-TimeSeriesModel.R");
source("./Library/Functions-RegressionModel.R");
source("./Library/Functions-Production.R")

TEST_ENVIRONMENT <- TRUE;
EXECUTE_DATA_EXAM <- TRUE;
EXECUTE_COMPONENT_ANALYSIS_LOB <- TRUE;
EXECUTE_SEGMENTATION_MATERIALITY_ANALYSIS <- TRUE;
EXECUTE_TimeSeries_ANALYSIS <- TRUE;
EXECUTE_CORR_ANALYSIS <- TRUE;
EXECUTE_REG_ANALYSIS <- FALSE;
EXECUTE_DIFF_REG_ANALYSIS <- FALSE;
EXECUTE_DIFF_REG_ANALYSIS_3V <- FALSE;
EXECUTE_MOVINGAVERAGE <- TRUE;
EXECUTE_SEASONALMODEL <- TRUE;
EXECUTE_REG_SUMMARY <- FALSE;
EXECUTE_ARIMAXMODEL <- TRUE;
####Config and Parameter Setting
num_models_to_examine <- 20; #regression models to plot and exam
revenue_threshold <- 1000000 #if the sum last 12 month revenue < threshold, it will not be analyzed.
threshold <- 0.05; #threshold to pass or fail statistical test
max_backtesing_files <- 10; #backtesting chart to be included in the reports

##Colors
Color_gold <- "#AB8433";
Color_silver<- "#8D9091";
Color_bronze<- "#9E8F6C";
Color_gray <- "#4B4B4B";

Color_black <- "#000000";
Color_blue<- "#0057B8";
Color_green <- "#43B02A";
Color_red <- "#E35205";
Color_teal <- "#008C95";

Color_baseline <- "#22DD26";
#Color_adverse <- "#FFDE0F";
Color_severe <- "#EE2724";
Color_adverse <- Color_severe;
Color_bhc <- Color_severe;
#Color_bhc <- "#661DE8";
Color_history <- "#0094FF";
Color_backtest <- Color_black;

#Remote data file
revenue_file_version <- "./Data/RevenuebyLOBv0.3.xlsx";

Macro_file <- "./Library/Data/Masterfile-Data Process-11-17-16.xlsx";#Newest Version

###Local Files
Output_root <- "../Revenue-Results/Output-Q22016-CCAR11172016/";
if (!dir.exists(Output_root)) {
	dir.create(Output_root);
}

Time_periods <- 1:114;
Time_periods_Q <- 1:38;

max_segments <- 3;

if (TEST_ENVIRONMENT) {
	BUS <- c("AS");
#	BUS <- c("BDS");
#	BUS <- c("DR");
#	BUS <- c("Clearing");
#	BUS <- c("CT");
#	BUS <- c("CashMgmt");
#	BUS <- c("FX");
#	BUS <- c("WM");
#	BUS <- c("IM","SL","DR");
#	BUS <- c("IM");
#	BUS <- c("SL");
#	BUS <- c("DR");
#	BUS <- c("BDS","CashMgmt","WM","Clearing","DR");
#	BUS <- c("Clearing","IM","FX");
	Enable_Narrowed_List <- TRUE;
}  else {
	#	BUS <- c("AS");
	#	BUS <- c("Clearing");
	#	BUS <- c("CT");
	#	BUS <- c("CashMgmt");
	#	BUS <- c("SL");
	#	BUS <- c("Clearing","BDS","FX);
	BUS <- c("CT", "AS","Clearing","CashMgmt", "IM", "BDS", "FX", "SL", "DR","WM");

#	BUS <- c("CT", "AS","Clearing","CashMgmt", "BDS", "DR", "WM");
#	BUS <- c("Clearing","CT","BDS", "FX", "SL");
	
	# sec lending, global markets, dr
#	BUS <- c("BK","IM","AM","WM","IS","AS","CashMgmt","BDS","Clearing","CSD","CT","DR","GCS","OtherSec","FX","TOTECH","TRESRY","TSHSER");
	Enable_Narrowed_List <- TRUE;
#	Expense_Items_Report_Items <- Expense_Items;
}

if (EXECUTE_DATA_EXAM) {
	source("./Revenue-DataExamination.R");
}

#Move to Production
if (EXECUTE_SEGMENTATION_MATERIALITY_ANALYSIS) {
	print("--Execute EXECUTE_SEGMENTATION_MATERIALITY_ANALYSIS\n");
	source("./Revenue-Segmentation.R");
}

#TimeSeries Analysis
if (EXECUTE_TimeSeries_ANALYSIS) {
	source("./Revenue-TimeSeriesAnalysis.R");
}

#Move to Production
if (EXECUTE_COMPONENT_ANALYSIS_LOB) {
	print("--Execute Revenue Pie Chart for LOB Data\n");
#	segment_peroid_start <- c(2015,7);
	segment_peroid_start <- c(2015,1);
#	segment_peroid_end <- c(2016,6);
	segment_peroid_end <- c(2015,12);

	segment_peroid_start_q <- c(2015,1);
	segment_peroid_end_q <- c(2015,4);
	for (bu in 1:length(BUS)) {
#		bu<-10
		BU <- BUS[bu];
		file_name <- paste0("./Revenue-ComponentAnalysis-",BU,".R");
		if (file.exists(file_name)) {
			source(file_name);
		}
	}
}

###Correlation
if (EXECUTE_CORR_ANALYSIS) {
	print("--Execute Revenue Correlation Analysis\n");
	source("./Revenue-CorrelationAnalysis.R");
}

####Run algorithmic regression analysis
###Reg Diff
if (EXECUTE_DIFF_REG_ANALYSIS) {
	print("--Execute DIFF Regression Analysis\n");
	External_variable_selection_list <- c("SP500_END","NASDAQTrades_AVE","SPVIX_END","RealGDPGrowth_END");
	EXECUTE_ALGO_1 <- TRUE;
	EXECUTE_ALGO_2 <- TRUE;
	source("./Revenue-AlgoRegressionDiff.R");
}

#External_variable_selection_list <- c("SP500","NASDAQ","DJGlobalIndex","DJCompositeAverage","MutualFundTotalNetAssetAll",
#		"AGG","FTSE100","RealGDPGrowth","TY5Y","BBBCorporateYield");
if (EXECUTE_REG_ANALYSIS) {
	print("--Execute Regression Analysis\n");
	External_variable_selection_list <- c("SP500_END","NASDAQTrades_AVE","SPVIX_END","RealGDPGrowth_END");
	EXECUTE_ALGO_1 <- TRUE;
	EXECUTE_ALGO_2 <- TRUE;
	source("./Revenue-AlgoRegression.R");
}

if (EXECUTE_DIFF_REG_ANALYSIS_3V) {
	print("--Execute DIFF Regression Analysis with 3 variables! Time consuming.\n");
	External_variable_selection_list <- c("SP500_END","NASDAQTrades_AVE","SPVIX_END","RealGDPGrowth_END","DJGlobal_END","MSCIEmerging_AVE");
	EXECUTE_ALGO_1 <- TRUE;
	EXECUTE_ALGO_2 <- TRUE;
	Seleted_Revenues <- c("AUCRelatedFee","BDS_adjusted");
	source("./Revenue-AlgoRegressionDiff3V.R");
}

###Moving Average Model
if (EXECUTE_MOVINGAVERAGE) {
	print("--Summary of Moving Average Model\n");
	source("./Revenue-MovingAverage.R");
}

###Seasonal Decomposition Model
if (EXECUTE_SEASONALMODEL) {
	print("--Summary of Seaonal Decomposition Model\n");
	source("./Revenue-SeasonalDecomposition.R");
}

###ARIMAX Model
if (EXECUTE_ARIMAXMODEL) {
	print("--Summary of ARIMAX Model Class\n");
	source("./Revenue-ARIMAX.R");
}
