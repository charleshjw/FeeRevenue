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
		if (!segment%in%determine_champion_segment(BU)) {
			next;
		}
		
		if (!paste0(BU,segment)%in%names(sheets)) {
			next;
		}
		###transform original revenue data file to formated revenue masterfile
		revenue_lob_raw <- read.xlsx(revenue_file_version,sheetName=paste0(BU,segment));
	#	revenue_lob_raw[1,]
	#	revenue_lob_raw[,1]
		
		revenue_lob <- revenue_lob_raw[determine_time_periods(revenue_lob_raw[,1]),];
		revenues <- get_revenues(revenue_lob);
		
		#set up file hierarchy
		Output_file <- paste(Output_root,BU,sep="");
		if (!dir.exists(Output_file)) {
			dir.create(Output_file);
		}
		Revenue_Items <- names(revenues);
		
		###regression: monthly external model
		Output_file_regression <- paste(Output_file,"/Analysis-DiffRegression3V-Segment-",segment,sep="");
		if (!dir.exists(Output_file_regression)) {
			dir.create(Output_file_regression);
		}
		
		for (j in 1:length(Revenue_Items)) {
			#	j <- 2
			if (!Revenue_Items[j]%in%Seleted_Revenues) {
				next;
			}
			#if no data available, next one
			if(all(is.na(revenues[[j]][,2:3]))) {
				next;
			}
			
			Output_file_regression_item3 <- paste(Output_file_regression,"/V3-Revenue-",Revenue_Items[j],sep="");
			if (!file.exists(Output_file_regression_item3)) {
				dir.create(Output_file_regression_item3);
			}
			
			##Revenue
			#Monthly Version
			#Quarterly Version
			
			##Metric
			#Monthly Version
			#Quarterly Version
	

			#transform revenue masterfile to revenue format
			revenue <- revenues[[j]][,c(1,2)];
			revenue <- revenue[!is.na(revenue[,2]),];
			
			if(determine_frequency_and_start(revenues[[j]][,1])$frequency==12) {
				#Variable Selection
				if (revenue_analysis(revenue,revenue_threshold)) {
					#narrow down macro to limited variables to reduce computational complexity
					if (Enable_Narrowed_List) {
						.Macro <- Macro[,colnames(Macro)%in%External_variable_selection_list];
						.Macro <- data.frame(date=Macro[,1],.Macro);
					} else {
						.Macro <- Macro;
					}
					
					tryCatch(
							{
								if (EXECUTE_ALGO_1) {
									Variable_Selection_Diff_3variable(threshold,Output_file_regression_item3,revenue,.Macro);
								}
								
								if (EXECUTE_ALGO_2) {
									Variable_Selection_Diff_3variable_Test1(num_models_to_examine,Output_file_regression_item3,revenue,Macro,Macro_2,Macro_3,Macro_4,BU);
								}
							}, error = function(e) {
								print(e);
								print(BU);
								print(Revenue_Items[j]);
							}, finally = {}
					)
				}
			} else {
				if (revenue_analysis(revenue,revenue_threshold)) {
					#narrow down macro to limited variables to reduce computational complexity
					if (Enable_Narrowed_List) {
						.Macro <- Macro_Q[,colnames(Macro_Q)%in%External_variable_selection_list];
						.Macro <- data.frame(date=Macro_Q[,1],.Macro);
					} else {
						.Macro <- Macro_Q;
					}
					
					tryCatch(
							{				
								if (EXECUTE_ALGO_1) {
									Variable_Selection_Diff_3variable(threshold,Output_file_regression_item3,revenue,.Macro);
								}
								
								if (EXECUTE_ALGO_2) {
									Variable_Selection_Diff_3variable_Test1(num_models_to_examine,Output_file_regression_item3,revenue,Macro_Q,Macro_2_Q,Macro_3_Q,Macro_4_Q,BU);
								}
							}, error = function(e) {
								print(e);
								print(BU);
								print(Revenue_Items[j]);
							}, finally = {}
					)
				}
			}
			
			#Metric
			revenue <- revenues[[j]][,c(1,3)];
			revenue <- revenue[!is.na(revenue[,2]),];
			if (all(is.na(revenue[,2]))) {
				next;
			}
			
			Output_file_regression_item3 <- paste(Output_file_regression,"/V3-Metric-",Revenue_Items[j],sep="");
			if (!file.exists(Output_file_regression_item3)) {
				dir.create(Output_file_regression_item3);
			}
			
			if(determine_frequency_and_start(revenues[[j]][,1])$frequency==12) {
				#Variable Selection
				if (metric_analysis(revenue,revenue_threshold)) {
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
									Variable_Selection_Diff_3variable(threshold,Output_file_regression_item3,revenue,.Macro);
								}
								
								if (EXECUTE_ALGO_2) {
									Variable_Selection_Diff_3variable_Test1(num_models_to_examine,Output_file_regression_item3,revenue,Macro,Macro_2,Macro_3,Macro_4,BU);
								}
							}, error = function(e) {
								print(e);
								print(BU);
								print(Revenue_Items[j]);
							}, finally = {}
					)
				}
			} else {
				if (metric_analysis(revenue,revenue_threshold)) {
					#narrow down macro to limited variables to reduce computational complexity
					if (Enable_Narrowed_List) {
						.Macro <- Macro_Q[,colnames(Macro_Q)%in%External_variable_selection_list];
						.Macro <- data.frame(date=Macro_Q[,1],.Macro);
					} else {
						.Macro <- Macro_Q;
					}
					
					tryCatch(
							{
								if (EXECUTE_ALGO_1) {
									Variable_Selection_Diff_3variable(threshold,Output_file_regression_item3,revenue,.Macro);
								}
								
								if (EXECUTE_ALGO_2) {
									Variable_Selection_Diff_3variable_Test1(num_models_to_examine,Output_file_regression_item3,revenue,Macro_Q,Macro_2_Q,Macro_3_Q,Macro_4_Q,BU);
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
}

