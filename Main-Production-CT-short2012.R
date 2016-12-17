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
source("../Library/Functions-RegressionModel.R");
source("../Library/Functions-Production.R");
source("../Library/Functions-TimeSeriesModel.R");

####Config and Parameter Setting
EXECUTE_PRODUCTION <- TRUE;
revenue_threshold <- 1000000 #if the sum last 12 month revenue < threshold, it will not be analyzed.
threshold <- 0.05; #threshold to pass or fail statistical test

##Colors
#Color_gold <- "#AB8433";
#Color_silver<- "#8D9091";
#Color_bronze<- "#9E8F6C";
#Color_gray <- "#4B4B4B";

Color_black <- "#000000";
#Color_blue<- "#0057B8";
#Color_green <- "#43B02A";
#Color_red <- "#E35205";
#Color_teal <- "#008C95";

Color_baseline <- "#22DD26";
#Color_adverse <- "#FFDE0F";
Color_severe <- "#EE2724";
Color_adverse <- Color_severe;
Color_bhc <- Color_severe;
#Color_bhc <- "#661DE8";
Color_history <- "#0094FF";

Color_backtest <- Color_black;

max_segments <- 3;
########################################################
BUS <- c("CT");
revenue_file_version <- "H:/workspace2016/Revenue/Data/RevenuebyLOBv0.3.xlsx";
Macro_file <- "../Library/Data/Masterfile-Data Process-11-17-16.xlsx";#Newest Version
variable_file_version <- "./Data/VariableSelection-Revenue-CCAR.xlsx";
Output_root <- "./Output-Q22016-Short2011/";
if (!dir.exists(Output_root)) {
	dir.create(Output_root);
}

Time_periods <- 49:114;
Time_periods_Q <- 17:38;

if (EXECUTE_PRODUCTION) {
	print("--Production\n");
	source("./Production-Regression-Summary.R");
}
#
#Output_root <- "./Output-Q22016-CCAR11172016/";
#if (!dir.exists(Output_root)) {
#	dir.create(Output_root);
#}
#
#Time_periods <- 1:114;
#Time_periods_Q <- 1:38;
#
#if (EXECUTE_PRODUCTION) {
#	print("--Production\n");
#	source("./Production-Regression-Summary.R");
#}