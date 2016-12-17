#
# 
# Author: XBBKKL3
###############################################################################
#rm(list=ls())
setwd("C:/workspace2016/Revenue");
source("../Library/Functions.R");
source("../Library/Functions-Revenue.R");
source("../Library/Functions-MovingAverage.R");
source("../Library/Functions-SeasonalDecomposition.R");
source("../Library/Functions-TimeSeriesModel.R");
source("../Library/Functions-RegressionModel.R");
source("../Library/Functions-Production.R")


EXECUTE_DATA_EXAM <- FALSE;
EXECUTE_SEGMENTATION_MATERIALITY_ANALYSIS <- FALSE;
EXECUTE_TimeSeries_ANALYSIS <- FALSE;
EXECUTE_CORR_ANALYSIS <- FALSE;
EXECUTE_REG_ANALYSIS <- FALSE;
EXECUTE_DIFF_REG_ANALYSIS <- FALSE;
EXECUTE_MOVINGAVERAGE <- FALSE;
EXECUTE_SEASONALMODEL <- FALSE;
EXECUTE_ARIMAXMODEL <- FALSE;
EXECUTE_PRODUCTION <- TRUE;

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
revenue_file_version <- "H:/workspace2016/Revenue/Data/SLSpreadTest.xlsx";

Macro_file <- "../Library/Data/Masterfile-Data Process-11-02-16.xlsx";#Newest Version

###Local Files
variable_file_version <- "./Data/VariableSelection-Revenue-SLTest.xlsx";
#variable_file_maq_version <- "./Data/VariableSelection-MAQ.xlsx";

Output_root <- "../Revenue-Results/Output-Q22016-SLTest/";

Time_periods <- 1:114;
Time_periods_Q <- 1:38;

#Output_root <- "./Output-Q22016-SLTestShort/";
#
#Time_periods <- 37:114;
#Time_periods_Q <- 13:38;


if (!dir.exists(Output_root)) {
	dir.create(Output_root);
}

#max_segments <- 1;
max_segments <- 2;

BUS <- c("SL");
Enable_Narrowed_List <- FALSE;

#Move to Production
if (EXECUTE_SEGMENTATION_MATERIALITY_ANALYSIS) {
	print("--Execute EXECUTE_SEGMENTATION_MATERIALITY_ANALYSIS\n");
	source("./Revenue-Segmentation.R");
}

#TimeSeries Analysis
if (EXECUTE_TimeSeries_ANALYSIS) {
	source("./Revenue-TimeSeriesAnalysis.R")
}

###Correlation
if (EXECUTE_CORR_ANALYSIS) {
	print("--Execute Revenue Correlation Analysis\n");
	source("./Revenue-CorrelationAnalysis.R");
}

####Run algorithmic regression analysis
#External_variable_selection_list <- c("SP500","NASDAQ","DJGlobalIndex","DJCompositeAverage","MutualFundTotalNetAssetAll",
#		"AGG","FTSE100","RealGDPGrowth","TY5Y","BBBCorporateYield");
if (EXECUTE_REG_ANALYSIS) {
	print("--Execute Regression Analysis\n");
#	External_variable_selection_list <- c("SP500","NASDAQ","MutualFundTotalNetAssetAll","AGG","FTSE100","RealGDPGrowth","TY5Y","BBBCorporateYield");
	EXECUTE_ALGO_1 <- TRUE;
	EXECUTE_ALGO_2 <- TRUE;
	source("./Revenue-AlgoRegression.R");
}

###Reg Diff
if (EXECUTE_DIFF_REG_ANALYSIS) {
	print("--Execute DIFF Regression Analysis\n");
#	Internal_ratevariable_selection_list <- c("TOTREV","NININC");
#	External_variable_selection_list <- c("SP500","NASDAQ","MutualFundTotalNetAssetAll","AGG","FTSE100","RealGDPGrowth","TY5Y","BBBCorporateYield");
	EXECUTE_ALGO_1 <- TRUE;
	EXECUTE_ALGO_2 <- TRUE;
	source("./Revenue-AlgoRegressionDiff.R");
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

if (EXECUTE_PRODUCTION) {
	print("--Production\n");
	source("./BUTest/Production-Regression-Summary.R");
}

