#Clearing
if (!"dummy_cs_fundvestassets"%in%colnames(Macro)) {
	dates <- c("2008-10-31","2008-11-30");
	dummy_name <- "dummy_cs_fundvestassets";
	Macro <- add_dummy(Macro,dates,dummy_name);
	Macro_2 <- add_dummy(Macro_2,dates,dummy_name);
	Macro_3 <- add_dummy(Macro_3,dates,dummy_name);
	Macro_4 <- add_dummy(Macro_4,dates,dummy_name);
}

if (!"dummy_cs_totalother"%in%colnames(Macro)) {	
	dates <- c("2011-02-28","2011-03-31");
	dummy_name <- "dummy_cs_totalother";
	Macro <- add_dummy(Macro,dates,dummy_name);
	Macro_2 <- add_dummy(Macro_2,dates,dummy_name);
	Macro_3 <- add_dummy(Macro_3,dates,dummy_name);
	Macro_4 <- add_dummy(Macro_4,dates,dummy_name);
}

#BDS
if (!"dummy_bds_bdsexccf"%in%colnames(Macro_Q)) {
	dates <- c("2008-12-31","2009-03-31");
	dummy_name <- "dummy_bds_bdsexccf";
	Macro_Q <- add_dummy(Macro_Q,dates,dummy_name);
	Macro_2_Q <- add_dummy(Macro_2_Q,dates,dummy_name);
	Macro_3_Q <- add_dummy(Macro_3_Q,dates,dummy_name);
	Macro_4_Q <- add_dummy(Macro_4_Q,dates,dummy_name);
}

if (!"dummy_bds_bdsexccf"%in%colnames(Macro)) {
	dates <- c("2008-10-31","2008-11-30","2008-12-31","2009-01-31","2009-02-28","2009-03-31");
	dummy_name <- "dummy_bds_bdsexccf";
	Macro <- add_dummy(Macro,dates,dummy_name);
	Macro_2 <- add_dummy(Macro_2,dates,dummy_name);
	Macro_3 <- add_dummy(Macro_3,dates,dummy_name);
	Macro_4 <- add_dummy(Macro_4,dates,dummy_name);
}

#IM
if (!"dummy_im_globalfi2009"%in%colnames(Macro_Q)) {
	dates <- c("2009-12-31");
	dummy_name <- "dummy_im_globalfi2009";
	Macro_Q <- add_dummy(Macro_Q,dates,dummy_name);
	Macro_2_Q <- add_dummy(Macro_2_Q,dates,dummy_name);
	Macro_3_Q <- add_dummy(Macro_3_Q,dates,dummy_name);
	Macro_4_Q <- add_dummy(Macro_4_Q,dates,dummy_name);
}

if (!"dummy_im_globalfi2011"%in%colnames(Macro_Q)) {
	dates <- c("2011-12-31");
	dummy_name <- "dummy_im_globalfi2011";
	Macro_Q <- add_dummy(Macro_Q,dates,dummy_name);
	Macro_2_Q <- add_dummy(Macro_2_Q,dates,dummy_name);
	Macro_3_Q <- add_dummy(Macro_3_Q,dates,dummy_name);
	Macro_4_Q <- add_dummy(Macro_4_Q,dates,dummy_name);
}


#FX
if (!"AS_AUCA"%in%colnames(Macro)) {
	tmp_revenue_lob_raw <- read.xlsx(revenue_file_version,sheetName=paste0("AS",3));
	tmp_revenue_lob <- tmp_revenue_lob_raw[determine_time_periods(tmp_revenue_lob_raw[,1]),];
	tmp_revenue_lob <- get_revenues(tmp_revenue_lob);
	tmp_revenue <- extract_data_from_revenues(tmp_revenue_lob,"Metric.AUCRelatedFee");
	tmp_revenue <- clean_na_data_frame(tmp_revenue);
	.variables <- c("SP500_END","BarclaysAGG_AVE");
	.data <- compile_regression_data(determine_Macro(tmp_revenue[,1], Macro, Macro_Q),.variables,tmp_revenue);
	.regression_data <- clean_na_data_frame(.data);
	
	fit <- fit_regression(.regression_data);
	
	forecasts <- get_forecasts(fit,.regression_data,.data,tmp_revenue,.variables);
	
	.baseline <- merge_successive_ts(forecasts$actual,forecasts$baseline);
	.adverse <- merge_successive_ts(forecasts$actual,forecasts$adverse);
	.severe <- merge_successive_ts(forecasts$actual,forecasts$severe);
	.bhc <- merge_successive_ts(forecasts$actual,forecasts$bhc);
	
	date <- as.Date(as.yearmon(time(.baseline))+1/12)-1;
	variable_name <- "AS_AUCA";
	
	baseline_values <- data.frame(date,.baseline);
	Macro <- add_variable(determine_Macro(tmp_revenue[,1], Macro, Macro_Q), baseline_values, variable_name);
	
	adverse_values <- data.frame(date,.adverse);
	Macro_2 <- add_variable(determine_Macro(tmp_revenue[,1], Macro_2, Macro_2_Q), adverse_values, variable_name);
	
	severe_values <- data.frame(date,.severe);
	Macro_3 <- add_variable(determine_Macro(tmp_revenue[,1], Macro_3, Macro_3_Q), severe_values, variable_name);
	
	bhc_values <- data.frame(date,.bhc);
	Macro_4 <- add_variable(determine_Macro(tmp_revenue[,1], Macro_4, Macro_4_Q), bhc_values, variable_name);
}

if (!"NumBizDay"%in%colnames(Macro)) {
	require(timeDate)
	start <- as.Date("2007-01-01");
	NumBizDay <- c();
	NumBizDay <- c(sum(isWeekday(timeSequence(start, as.Date(Macro[1,1])))),NumBizDay);
	for (i in 2:nrow(Macro)) {
		NumBizDay <- c(sum(isWeekday((timeSequence(as.Date(Macro[i-1,1])+1, as.Date(Macro[i,1]))))),NumBizDay);
	}
	NumBizDay <- data.frame(Macro[,1],NumBizDay)
	
	Macro <- add_variable(Macro, NumBizDay, "NumBizDay");
	Macro_2 <- add_variable(Macro_2, NumBizDay, "NumBizDay");
	Macro_3 <- add_variable(Macro_3, NumBizDay, "NumBizDay");
	Macro_4 <- add_variable(Macro_4, NumBizDay, "NumBizDay");
}

if (!"NumBizDay"%in%colnames(Macro_Q)) {
	start <- as.Date("2007-01-01");
	NumBizDay <- c();
	NumBizDay <- c(sum(isWeekday(timeSequence(start, as.Date(Macro_Q[1,1])))),NumBizDay);
	for (i in 2:nrow(Macro_Q)) {
		NumBizDay <- c(sum(isWeekday((timeSequence(as.Date(Macro_Q[i-1,1])+1, as.Date(Macro_Q[i,1]))))),NumBizDay);
	}
	NumBizDay <- data.frame(Macro_Q[,1],NumBizDay)
	
	Macro_Q <- add_variable(Macro_Q, NumBizDay, "NumBizDay");
	Macro_2_Q <- add_variable(Macro_2_Q, NumBizDay, "NumBizDay");
	Macro_3_Q <- add_variable(Macro_3_Q, NumBizDay, "NumBizDay");
	Macro_4_Q <- add_variable(Macro_4_Q, NumBizDay, "NumBizDay");
}
