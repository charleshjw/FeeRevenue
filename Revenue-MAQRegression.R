print("good try");
write(c(1:3),"C:/workspace2016/Revenue/test.csv")
## 
## 
## Author: XBBKKL3
################################################################################
##setInternet2()
##install.packages("xlsx");
##setwd("H:/workspace2016/Expense");
##source("../Revenue/Functions.R");
#
##load macro economic variable universe
#
#if (!"Macro"%in%ls()) {
#	Macro <- read.xlsx("../Expense/Data/Masterfile-Data Process.xlsx",sheetName="Monthly-baseline");
#}
#if (!"Macro_2"%in%ls()) {
#	Macro_2 <- read.xlsx("../Expense/Data/Masterfile-Data Process.xlsx",sheetName="Monthly-adverse");
#}
#if (!"Macro_3"%in%ls()) {
#	Macro_3 <- read.xlsx("../Expense/Data/Masterfile-Data Process.xlsx",sheetName="Monthly-severelyadverse");
#}
#if (!"Macro_4"%in%ls()) {
#	Macro_4 <- read.xlsx("../Expense/Data/Masterfile-Data Process.xlsx",sheetName="Monthly-BHC");
#}
#
#
#if (!"Macro_Q"%in%ls()) {
#	Macro_Q <- read.xlsx("../Expense/Data/Masterfile-Data Process.xlsx",sheetName="Quarterly-baseline");
#}
#if (!"Macro_2_Q"%in%ls()) {
#	Macro_2_Q <- read.xlsx("../Expense/Data/Masterfile-Data Process.xlsx",sheetName="Quarterly-adverse");
#}
#if (!"Macro_3_Q"%in%ls()) {
#	Macro_3_Q <- read.xlsx("../Expense/Data/Masterfile-Data Process.xlsx",sheetName="Quarterly-severelyadverse");
#}
#if (!"Macro_4_Q"%in%ls()) {
#	Macro_4_Q <- read.xlsx("../Expense/Data/Masterfile-Data Process.xlsx",sheetName="Quarterly-BHC");
#}
#
#####Config
##num_models_to_examine <- 20;
##revenue_threshold <- 100#if the sum last 12 month revenue < threshold, it will not be analyzed.
##threshold <- 0.1;
#
#####load internal data
##expense_file_version <- "./Data/ExpensebyLOBv3.xlsx";
##expense_file_version <- "./Data/ExpensebyAccountv0.xlsx";
##get all tab names
##wb <- loadWorkbook(expense_file_version);
##sheets <- getSheets(wb);
#
###Start Analysis for each Tabs
#for (bu in 1:length(BUS)) {
#	# bu <- 1;
#	#restart from 18
#	BU <- BUS[bu];
#	print(BU);
#	#set up file hierarchy
#	Output_file <- paste(Output_root,BU,sep="");
#	if (!dir.exists(Output_file)) {
#		dir.create(Output_file);
#	}
#	
#	###transform original revenue data file to formated revenue masterfile
#	revenue_lob_raw <- read.xlsx(expense_file_version,sheetName=BU);
##	revenue_lob_raw[1,]
##	revenue_lob_raw[,1]
#	
#	revenue_lob <- data.frame(revenue_lob_raw[Time_periods,]);
#	revenue_lob <- adjust_expense_for_risk(revenue_lob); #exclude operational risk losses
#	revenue_lob <- adjust_expense_for_MNINEW(revenue_lob); #exclude operational risk losses
#	revenue_lob <- adjust_expense_for_glsnew(revenue_lob,plot=TRUE,Output_file); #exclude operational risk losses
#	revenue_lob <- adjust_expense_for_extraordinaryevents(revenue_lob,plot=TRUE,Output_file); #exclude operational risk losses
#	
##	revenue_lob[1,];
##	revenue_lob[,1];
#	
#	###regression: monthly external
#	Output_file_regression <- paste(Output_file,"/Analysis-MAQRegression",sep="");
#	if (!dir.exists(Output_file_regression)) {
#		dir.create(Output_file_regression);
#	}
#	for (j in 1:length(Revenue_Items)) {
#		#	j <- 1
#		#set file directory
#		Output_file_regression_item <- paste(Output_file_regression,"/",Revenue_Items[j],sep="");
#		if (!file.exists(Output_file_regression_item)) {
#			dir.create(Output_file_regression_item);
#		}
#		
#		#transform revenue masterfile to revenue format
#		revenue <- get_revenue(revenue_lob,Revenue_Items[j]);
#		
#		#Variable Selection
#		if (revenue_analysis(revenue,revenue_threshold)) {
#			#narrow down macro to limited variables to reduce computational complexity
#			if (Enable_Narrowed_List) {
#				.Macro <- Macro[,colnames(Macro)%in%External_variable_selection_list];	
#				.Macro <- data.frame(date=Macro[,1],.Macro);
#			} else {
#				.Macro <- Macro
#			}
##			dim(.Macro)
#			tryCatch(
#					{
#						Variable_Selection_2(threshold,Output_file_regression_item,revenue,.Macro);
#						Variable_Selection_2variable_Test1(num_models_to_examine,Output_file_regression_item,revenue,.Macro,Macro_2,Macro_3,Macro_4,BU);
#					}, error = function(e) {
#						print(e);
#						print(BU);
#						print(Revenue_Items[j]);
#					}, finally = {}
#			)
#		}
#		#	Variable_Selection_3(threshold,Output_file,revenue,Macro);
#	}
#	
#	##regression: quarterly external
#	Output_file_regressionQ <- paste(Output_file,"/Analysis-MAQRegression-Quarter",sep="");
#	if (!file.exists(Output_file_regressionQ)) {
#		dir.create(Output_file_regressionQ);
#	}
#	for (j in 1:length(Revenue_Items)) {	
#		#	j <- 1
#		#set file directory
#		Output_file_regressionQ_item <- paste(Output_file_regressionQ,"/",Revenue_Items[j],sep="");
#		if (!file.exists(Output_file_regressionQ_item)) {
#			dir.create(Output_file_regressionQ_item);
#		}
#		
#		#transform revenue masterfile to revenue format
#		revenue <- get_revenue(revenue_lob,Revenue_Items[j]);
#		revenue_Q <- monthly_to_quarterly(revenue);
#		
#		#Variable Selection
#		if (revenue_analysis(revenue_Q,revenue_threshold)) {
#			#narrow down macro to limited variables to reduce computational complexity
#			if (Enable_Narrowed_List) {
#				.Macro <- Macro_Q[,colnames(Macro_Q)%in%External_variable_selection_list];	
#				.Macro <- data.frame(date=Macro_Q[,1],.Macro);
#			} else {
#				.Macro <- Macro
#			}
##			dim(.Macro)
#			tryCatch(
#					{
#						Variable_Selection_2(threshold,Output_file_regressionQ_item,revenue_Q,Macro_Q);
#						Variable_Selection_2variable_Test1(num_models_to_examine,Output_file_regressionQ_item,revenue_Q,Macro_Q,Macro_2_Q,Macro_3_Q,Macro_4_Q,BU);
#					}, error = function(e) {
#						print(e);
#						print(BU);
#						print(colnames(revenue)[2]);
#					}, finally = {}
#			)
#		}
#		#	Variable_Selection_3(threshold,Output_file,revenue,Macro);
#	}
#	
#	###regression - monthly internal
#	Output_file_regression_revenue <- paste(Output_file,"/Analysis-MAQRegression-Internal",sep="");
#	if (!file.exists(Output_file_regression_revenue)) {
#		dir.create(Output_file_regression_revenue);
#	}
#	
#	for (j in 1:length(Revenue_Items)) {
##		j <- 1;
#		#set up file hierarchy
#		Output_file_regression_revenue_item <- paste(Output_file_regression_revenue,"/",Revenue_Items[j],sep="");
#		if (!file.exists(Output_file_regression_revenue_item)) {
#			dir.create(Output_file_regression_revenue_item);
#		}
#		revenue <- get_revenue(revenue_lob,Revenue_Items[j]);
#		
#		if (revenue_analysis(revenue,revenue_threshold)) {
#			.Macro <- revenue_lob[,2:16];  #regress on fee items from TOTREV to AIIOTH
#			.Macro <- data.frame(revenue_lob[,1],.Macro[,apply(.Macro,2,sd)>10]);
#			tryCatch(
#					{
#						Variable_Selection_2(threshold,Output_file_regression_revenue_item,revenue,.Macro);
#						Variable_Selection_2variable_Test2(num_models_to_examine,Output_file_regression_revenue_item,revenue,.Macro,BU);
#					}, error = function(e) {
#						print(e);
#						print(BU);
#						print(Revenue_Items[j]);
#					}, finally = {}
#			)
#		} else {
#			warning(paste(BU,"revenue less than threshold, thus not analyzed."));
#		}
#	}
#	
#	###regression: quarterly internal
#	Output_file_regressionQ_revenue <- paste(Output_file,"/Analysis-MAQRegression-Quarterly-Internal",sep="");
#	if (!file.exists(Output_file_regressionQ_revenue)) {
#		dir.create(Output_file_regressionQ_revenue);
#	}
#	
#	for (j in 1:length(Revenue_Items)) {
##		j <- 1;
#		#set up file hierarchy
#		Output_file_regressionQ_revenue_item <- paste(Output_file_regressionQ_revenue,"/",Revenue_Items[j],sep="");
#		if (!file.exists(Output_file_regressionQ_revenue_item)) {
#			dir.create(Output_file_regressionQ_revenue_item);
#		}
#		
#		#get revenue data
#		revenue <- get_revenue(revenue_lob,Revenue_Items[j]);
#		revenue_Q <- monthly_to_quarterly(revenue);
#		
#		if (revenue_analysis(revenue,revenue_threshold)) {
#			.Macro <- revenue_lob[,2:16];  #regress on fee items from TOTREV to AIIOTH
#			.Macro <- data.frame(revenue_lob[,1],.Macro[,apply(.Macro,2,sd)>10]);
#			
#			##convert monthly expense to quarterly expenses
#			k <- 2;
#			.Macro_Q <- monthly_to_quarterly(.Macro[,c(1,k)]);
#			for (k in 3:ncol(.Macro)) {			
#				
#				.Macro_Q <- data.frame(.Macro_Q,monthly_to_quarterly(.Macro[,c(1,k)])[,2]);
#				
#			}
#			colnames(.Macro_Q) <- colnames(.Macro);
#			
#			tryCatch(
#					{
#						Variable_Selection_2(threshold,Output_file_regressionQ_revenue_item,revenue_Q,.Macro_Q);
#						Variable_Selection_2variable_Test2(num_models_to_examine,Output_file_regressionQ_revenue_item,revenue_Q,.Macro_Q,BU);
#					}, error = function(e) {
#						print(e);
#						print(BU);
#						print(Revenue_Items[j]);
#					}, finally = {}
#			)
#		} else {
#			warning(paste(BU,"revenue less than threshold, thus not analyzed."));
#		}
#	}
#	
##	###Run regression monthly revenue growth rate
##	Output_file_regression_revenue_rate <- paste(Output_file,"/Analysis-Regression-Internal-GrowthRate",sep="");
##	if (!file.exists(Output_file_regression_revenue_rate)) {
##		dir.create(Output_file_regression_revenue_rate);
##	}
##	
##	for (j in 1:length(Revenue_Items)) {
#	##		j <- 1;
##		#set up file hierarchy
##		Output_file_regression_revenue_item <- paste(Output_file_regression_revenue_rate,"/",Revenue_Items[j],sep="");
##		if (!file.exists(Output_file_regression_revenue_item)) {
##			dir.create(Output_file_regression_revenue_item);
##		}
##		revenue <- get_revenue(revenue_lob,Revenue_Items[j]);		
##		revenue_gr <- level_to_gr(revenue);
##		
##		revenue_gr_analysis <- function(revenue,revenue_gr) {
##			abs(sum(revenue[(nrow(revenue)-11):nrow(revenue),2]))>1000000;
##			if (is.null(dim(revenue_gr))) {
##				#if revenue grow rate has indefinite numbers, return and write error note
##				file_name <- paste(Output_file_regression_revenue_item,"/","revenue-error.csv",sep="");
##				write.csv(revenue,file_name);
##				next;
##			}
##		}
##		
##		if (revenue_analysis(revenue,revenue_threshold)&revenue_gr_analysis(revenue,revenue_gr)) {
##			
##			.Macro <- revenue_lob[,2:16];  #regress on fee items from TOTREV to AIIOTH
##			.Macro <- data.frame(revenue_lob[,1],.Macro[,apply(.Macro,2,sd)>10]);
##			.Macro <- level_to_gr(.Macro);
##			
##			tryCatch(
##					{
##						Variable_Selection_2(threshold,Output_file_regression_revenue_item,revenue,.Macro);
##						Variable_Selection_2variable_Test2(num_models_to_examine,Output_file_regression_revenue_item,revenue,.Macro,BU);
##					}, error = function(e) {
##						print(e);
##						print(BU);
##						print(Revenue_Items[j]);
##					}, finally = {}
##			)
##		} else {
##			warning(paste(BU,"revenue less than threshold, thus not analyzed."));
##		}
##	}
#}
#
