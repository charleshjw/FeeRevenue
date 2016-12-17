# 
# 
# Author: XBBKKL3
###############################################################################
#setInternet2()
#install.packages("xlsx");
#setwd("H:/workspace2016/Expense");
#source("../Revenue/Functions.R");

#load macro economic variable universe
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
		Output_file_regression <- paste(Output_file,"/Analysis-Regression-Segment-",segment,sep="");
		if (!dir.exists(Output_file_regression)) {
			dir.create(Output_file_regression);
		}
		###regression: monthly external model - Revenue
		for (j in 1:length(Revenue_Items)) {
			#	j <- 1
			#if no data available, next one
			if(all(is.na(revenues[[j]][,2]))) {
				next;
			}
			if(determine_frequency_and_start(revenues[[j]][,1])$frequency==4) {
				print(Revenue_Items[j]);
				print("frequency is quarterly. So montly algo-Revenue is skipped.")
				next;
			}
			
			#set file directory
			Output_file_regression_item1 <- paste(Output_file_regression,"/V1-Revenue-",Revenue_Items[j],sep="");
			if (!file.exists(Output_file_regression_item1)) {
				dir.create(Output_file_regression_item1);
			}
			Output_file_regression_item2 <- paste(Output_file_regression,"/V2-Revenue-",Revenue_Items[j],sep="");
			if (!file.exists(Output_file_regression_item2)) {
				dir.create(Output_file_regression_item2);
			}
			
			#transform revenue masterfile to revenue format
			revenue <- revenues[[j]][,c(1,2)];
			revenue <- revenue[!is.na(revenue[,2]),];
			
			#Variable Selection
			if (revenue_analysis(revenue,revenue_threshold)) {
				#narrow down macro to limited variables to reduce computational complexity
				if (Enable_Narrowed_List) {
					.Macro <- Macro[,colnames(Macro)%in%External_variable_selection_list];
					.Macro <- data.frame(date=Macro[,1],.Macro);
				} else {
					.Macro <- Macro
				}
				
				tryCatch(
						{
							if (EXECUTE_ALGO_1) {
								Variable_Selection_1(threshold,Output_file_regression_item1,revenue,Macro);
								Variable_Selection_2(threshold,Output_file_regression_item2,revenue,.Macro);							
							}
							if (EXECUTE_ALGO_2) {
								Variable_Selection_1variable_Test1(num_models_to_examine,Output_file_regression_item1,revenue,Macro,Macro_2,Macro_3,Macro_4,BU);
								Variable_Selection_2variable_Test1(num_models_to_examine,Output_file_regression_item2,revenue,.Macro,Macro_2,Macro_3,Macro_4,BU);								
							}
						}, error = function(e) {
							print(e);
							print(BU);
							print(Revenue_Items[j]);
						}, finally = {}
				)
			}
			#	Variable_Selection_3(threshold,Output_file,revenue,Macro);
		}
		
		###regression: monthly external model - Metric
		for (j in 1:length(Revenue_Items)) {
#			j <- 16
			#Metric
			revenue <- revenues[[j]][,c(1,3)];
			revenue <- revenue[!is.na(revenue[,2]),];
			
			if (all(is.na(revenue[,2]))) {
				next;
			}
			if(determine_frequency_and_start(revenues[[j]][,1])$frequency==4) {
				print(Revenue_Items[j]);
				print("frequency is quarterly. So montly algo-Metric is skipped.")
				next;
			}
			
			Output_file_regression_item1 <- paste(Output_file_regression,"/V1-Metric-",Revenue_Items[j],sep="");
			if (!file.exists(Output_file_regression_item1)) {
				dir.create(Output_file_regression_item1);
			}
			Output_file_regression_item2 <- paste(Output_file_regression,"/V2-Metric-",Revenue_Items[j],sep="");
			if (!file.exists(Output_file_regression_item2)) {
				dir.create(Output_file_regression_item2);
			}
			
			#Variable Selection
			if (metric_analysis(revenue,revenue_threshold)) {
				#narrow down macro to limited variables to reduce computational complexity
				if (Enable_Narrowed_List) {
					.Macro <- Macro[,colnames(Macro)%in%External_variable_selection_list];
					.Macro <- data.frame(date=Macro[,1],.Macro);
				} else {
					.Macro <- Macro
				}
		#			dim(.Macro)
				tryCatch(
						{
							if (EXECUTE_ALGO_1) {
								Variable_Selection_1(threshold,Output_file_regression_item1,revenue,Macro);
								Variable_Selection_2(threshold,Output_file_regression_item2,revenue,.Macro);
							}
							if (EXECUTE_ALGO_2) {
								Variable_Selection_1variable_Test1(num_models_to_examine,Output_file_regression_item1,revenue,Macro,Macro_2,Macro_3,Macro_4,BU);
								Variable_Selection_2variable_Test1(num_models_to_examine,Output_file_regression_item2,revenue,.Macro,Macro_2,Macro_3,Macro_4,BU);
							}
						}, error = function(e) {
							print(e);
							print(BU);
							print(Revenue_Items[j]);
						}, finally = {}
				)
			}
		}
		
		#####regression: quarterly external model
		Output_file_regressionQ <- paste(Output_file,"/Analysis-Regression-Quarter-Segment-",segment,sep="");
		if (!file.exists(Output_file_regressionQ)) {
			dir.create(Output_file_regressionQ);
		}
		##regression: quarterly external model - Revenue
		for (j in 1:length(Revenue_Items)) {
			#	j <- 2
			#if no data available, next one
			if(all(is.na(revenues[[j]][,2]))) {
				next;
			}
			
			#set file directory
			Output_file_regressionQ_item1 <- paste(Output_file_regressionQ,"/V1-Revenue-",Revenue_Items[j],sep="");
			if (!file.exists(Output_file_regressionQ_item1)) {
				dir.create(Output_file_regressionQ_item1);
			}
			
			Output_file_regressionQ_item2 <- paste(Output_file_regressionQ,"/V2-Revenue-",Revenue_Items[j],sep="");
			if (!file.exists(Output_file_regressionQ_item2)) {
				dir.create(Output_file_regressionQ_item2);
			}
			
			#transform revenue masterfile to revenue format
			revenue <- revenues[[j]][,c(1,2)];
			revenue <- revenue[!is.na(revenue[,2]),];
			
			revenue <- monthly_to_quarterly(revenue);
			
			#Variable Selection
			if (revenue_analysis(revenue,revenue_threshold)) {
				#narrow down macro to limited variables to reduce computational complexity
				if (Enable_Narrowed_List) {
					.Macro <- Macro_Q[,colnames(Macro_Q)%in%External_variable_selection_list];
					.Macro <- data.frame(date=Macro_Q[,1],.Macro);
				} else {
					.Macro <- Macro_Q;
				}
		#			dim(.Macro)
				tryCatch(
						{
							if (EXECUTE_ALGO_1) {
								Variable_Selection_1(threshold,Output_file_regressionQ_item1,revenue,Macro_Q);
								Variable_Selection_2(threshold,Output_file_regressionQ_item2,revenue,.Macro);
							}
							
							if (EXECUTE_ALGO_2) {
								Variable_Selection_1variable_Test1(num_models_to_examine,Output_file_regressionQ_item1,revenue,Macro_Q,Macro_2_Q,Macro_3_Q,Macro_4_Q,BU);
								Variable_Selection_2variable_Test1(num_models_to_examine,Output_file_regressionQ_item2,revenue,.Macro,Macro_2_Q,Macro_3_Q,Macro_4_Q,BU);
							}
						}, error = function(e) {
							print(e);
							print(BU);
							print(colnames(revenue)[2]);
						}, finally = {}
				)
			}
		}

		##regression: quarterly external model - Metric
		for (j in 1:length(Revenue_Items)) {
			#Metric
			revenue <- revenues[[j]][,c(1,3)];
			revenue <- revenue[!is.na(revenue[,2]),];
			
			if (all(is.na(revenue[,2]))) {
				next;
			}
			revenue_Q <- monthly_to_quarterly(revenue);
			
			Output_file_regressionQ_item1 <- paste(Output_file_regressionQ,"/V1-Metric-",Revenue_Items[j],sep="");
			if (!file.exists(Output_file_regressionQ_item1)) {
				dir.create(Output_file_regressionQ_item1);
			}
			Output_file_regressionQ_item2 <- paste(Output_file_regressionQ,"/V2-Metric-",Revenue_Items[j],sep="");
			if (!file.exists(Output_file_regressionQ_item2)) {
				dir.create(Output_file_regressionQ_item2);
			}
			
			#Variable Selection
			if (metric_analysis(revenue,revenue_threshold)) {
				#narrow down macro to limited variables to reduce computational complexity
				if (Enable_Narrowed_List) {
					.Macro <- Macro_Q[,colnames(Macro_Q)%in%External_variable_selection_list];
					.Macro <- data.frame(date=Macro_Q[,1],.Macro);
				} else {
					.Macro <- Macro_Q
				}

				tryCatch(
						{
							if (EXECUTE_ALGO_1) {
								Variable_Selection_1(threshold,Output_file_regressionQ_item1,revenue_Q,Macro_Q);
								Variable_Selection_2(threshold,Output_file_regressionQ_item2,revenue_Q,.Macro);
							}
	
							if (EXECUTE_ALGO_2) {
								Variable_Selection_1variable_Test1(num_models_to_examine,Output_file_regressionQ_item1,revenue_Q,Macro_Q,Macro_2_Q,Macro_3_Q,Macro_4_Q,BU);					
								Variable_Selection_2variable_Test1(num_models_to_examine,Output_file_regressionQ_item2,revenue_Q,.Macro,Macro_2_Q,Macro_3_Q,Macro_4_Q,BU);
							}
						}, error = function(e) {
							print(e);
							print(BU);
							print(Revenue_Items[j]);
						}, finally = {}
				)
			}
		}
	}
}

