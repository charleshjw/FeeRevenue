# 
# 
# Author: XBBKKL3
###############################################################################
library(car) #vif, influencePlot
library(reshape2)
options(java.parameters = "-Xmx2000m");
#setInternet2();
library(xlsx);
library(gap) #chow test
library(urca) #ca.jo
library(lmtest) #bptest
library(tseries);
library(corrplot);
library(xtable);
library(ggplot2);
library(gridExtra);
library(grid)
library(gtable)
library(forecast);
library(xts);
require(timeDate)
library(cvTools)
library(sandwich)
library(TTR)

# install.packages("gridExtra")
# install.packages("forecast")
# install.packages("xts")
# install.packages("timeDate")
# install.packages("sandwich")
# install.packages("cvTools")

jgc <- function(){
	gc()
	.jcall("java/lang/System", method = "gc")
}    
#independent functions

get_revenue <- function(revenue_lob,expense_item) {
	#revenue_lob is a data frame
	.col <- which(colnames(revenue_lob)==expense_item);
	dates <- revenue_lob[,1];
	y <- as.numeric(as.character(revenue_lob[,.col]));
	revenue <- data.frame(dates,y);
	colnames(revenue)[2] <- expense_item;	
	revenue;
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

trim_ts <- function(forecast_ts_month) {
	.start <- start(forecast_ts_month);
	if(.start[2]!=1) {
		return(window(forecast_ts_month,start=c(ceiling(min(time(forecast_ts_month))),1)));
	} else {
		return(forecast_ts_month);
	}
}

revenue_to_ts <- function(revenue, freq = NULL) {
#	revenue <- revenue_series;
	.tmp <- determine_frequency_and_start(revenue[,1]);
	time_series <- ts(revenue[,2],start=.tmp$start,frequency=.tmp$frequency);
	if (!is.null(freq)) {
		time_series <- aggregate(time_series,freq);
	}
	time_series;
}

ggplot_standard <- function(regressor, threshold=NULL) {
#	regressor <- iy;
	if (is.null(threshold)) {
		ggplot(data=regressor, aes_string(x=colnames(regressor)[1],y=colnames(regressor)[2])) + 
				geom_point() + 
				geom_line() + 
				ylab("") + 
				xlab("") + 
				ggtitle(colnames(regressor)[2]) + 
				scale_y_continuous(labels = format_level_value) + 
				theme(plot.title = element_text(size = 30, face = "bold")) +
				theme(text=element_text(size=30));
	} else {
		ggplot(data=regressor, aes_string(x=colnames(regressor)[1],y=colnames(regressor)[2])) + 
				geom_point() + 
				geom_line() + 
				ylab("") + 
				xlab("") + 
				ggtitle(colnames(regressor)[2]) + 
				scale_y_continuous(labels = format_level_value) +
				theme(plot.title = element_text(size = 30, face = "bold")) +
				theme(text=element_text(size=30)) +
				geom_hline(yintercept=c(threshold,-threshold))
#		+geom_errorbar(aes(ymax = regressor[,2]+threshold, ymin = regressor[,2]-threshold), width=.2, color=Color_baseline);
	}
}

plot_outlier <- function(regressor,plot_name=NULL) {
	regressor_diff <- level_to_diff(regressor,TRUE);
	
	significance_level <- 2;
	threshold <- significance_level*sd(regressor_diff[,2],na.rm=TRUE);
	note <- na.omit(regressor_diff[abs(regressor_diff[,2])>threshold,])
	
	p1 <- ggplot_standard(regressor);
	p2 <- ggplot_standard(regressor_diff,threshold) +
			annotate("text",x=note[,1],y=note[,2],label=note[,1],size=5,fontface="italic",colour="red")	+ 
			ggtitle(paste(colnames(regressor_diff)[2]," - ",significance_level,"*sigma", sep=""))
	if (is.null(plot_name)) {
		grid.arrange(p1,p2,ncol=1);
	} else {
		png(plot_name,width=800,height=600);
		grid.arrange(p1,p2,ncol=1);
		dev.off();
	}
}


level_to_gr <- function(revenue) {
	revenue_gr <- revenue;
	revenue_gr[2:nrow(revenue_gr),2:ncol(revenue_gr)] <- (revenue_gr[-1,2:ncol(revenue_gr)] - revenue_gr[-nrow(revenue_gr),2:ncol(revenue_gr)])/revenue_gr[-nrow(revenue_gr),2:ncol(revenue_gr)]
	revenue_gr <- revenue_gr[-1,];
	###If the growth rate is indefinite when value = 0 sometimes, it is excluded from the regression.
	.error_columns <- c();
	for (j in 2:ncol(revenue_gr)) {
		if (!all(is.finite(revenue_gr[,j]))) {
			.error_columns <- c(.error_columns,j);
		}
	}
	
	if (length(.error_columns)>0) {
		revenue_gr <- revenue_gr[,-.error_columns];
	}
	revenue_gr;
}

level_to_growthrate <- function(revenue) {
	revenue_gr <- revenue;
	revenue_gr[2:nrow(revenue_gr),2:ncol(revenue_gr)] <- (revenue_gr[-1,2:ncol(revenue_gr)] - revenue_gr[-nrow(revenue_gr),2:ncol(revenue_gr)])/revenue_gr[-nrow(revenue_gr),2:ncol(revenue_gr)]
	revenue_gr <- revenue_gr[-1,];
	###If the growth rate is indefinite when value = 0 sometimes, it is excluded from the regression.
	.error_rows <- c();
	for (j in 1:nrow(revenue_gr)) {
		if (!is.finite(revenue_gr[j,2])) {
			.error_rows  <- c(.error_rows ,j);
		}
	}
	
	if (length(.error_rows)>0) {
		revenue_gr[.error_rows,2] <- 0;
	}
	revenue_gr;
}


level_to_diff <- function(revenue,With_NA=FALSE) {
	if (With_NA) {
		revenue_gr <- revenue;
		revenue_gr[2:nrow(revenue_gr),2:ncol(revenue_gr)] <- revenue_gr[-1,2:ncol(revenue_gr)] - revenue_gr[-nrow(revenue_gr),2:ncol(revenue_gr)];
		revenue_gr[1,2] <- NA;
	} else {
		revenue_gr <- revenue;
		revenue_gr[2:nrow(revenue_gr),2:ncol(revenue_gr)] <- revenue_gr[-1,2:ncol(revenue_gr)] - revenue_gr[-nrow(revenue_gr),2:ncol(revenue_gr)];
		revenue_gr <- revenue_gr[-1,];		
	}
	revenue_gr;
}

diff_to_level <- function(start_value,increments,.start,.frequency,remove_first=FALSE) {
	values <- c(start_value,increments)
	for (i in 2:length(values)) {
		#			i<-3
		values[i] <- values[i]+values[i-1]
	}
	if (remove_first) {
		values <- values[-1];
	}
	level_value <- ts(values,start=.start,frequency=.frequency);
	level_value;
}

adjust_expense_for_glsnew <- function(revenue_lob,plot=FALSE,Output_file_risk="") {
	.data <- revenue_lob;
	.glsnew <- .data[,colnames(.data)=="NININC"]-.data[colnames(.data)=="AWMNEW"]-.data[colnames(.data)=="SSFNEW"]-.data[colnames(.data)=="PFENEW"]-.data[colnames(.data)=="DSFNEW"]-
			.data[colnames(.data)=="FRFNEW"]-.data[colnames(.data)=="FXONEW"]-.data[colnames(.data)=="AIIOTH"]
	
	revenue_lob[,colnames(revenue_lob)=="NININC"] <- revenue_lob[,colnames(revenue_lob)=="NININC"] -.glsnew;
	revenue_lob[,colnames(revenue_lob)=="TOTREV"] <- revenue_lob[,colnames(revenue_lob)=="TOTREV"] -.glsnew;
	
	oploss <- data.frame(revenue_lob[,1],.glsnew);
	if (plot) {
		start_and_frequency <- determine_frequency_and_start(oploss[,1]);
		.start <- start_and_frequency$start;
		.frequency <- start_and_frequency$frequency;
		.oploss <- ts(oploss[,2],start=.start,frequency=.frequency);
		.oploss_annual <- aggregate(.oploss,nfrequency=1,sum);
#		Output_file_risk <- Output_file;
		plot_name <- paste(Output_file_risk,"/",BU,"-GLSNEW.png",sep="");
		png(plot_name,width=800,height=600);
		ts.plot(.oploss,type="b",col=1:2);
		title(paste(BU,"-Securities Gains/Losses",sep=""));
		
		abline(v=as.numeric(time(.oploss_annual)));			
		.x <- as.numeric(time(.oploss_annual)+0.5);
		.level_revenue <- format_level_value(.oploss_annual);
		.grwothrate_revenue <- paste("(",c(NA,round(diff(.oploss_annual)/.oploss_annual[-length(.oploss_annual)]*100,0)),"%",")",sep="");
		text(.x,max(.oploss,na.rm=TRUE),paste(.level_revenue,.grwothrate_revenue),col=Color_history);	
		dev.off();		
	}
	revenue_lob;
}

#Two events in other sector are adjusted for BK and other Sector
adjust_expense_for_extraordinaryevents <- function(revenue_lob,plot=FALSE,Output_file_risk="") {
	adjust_for_singleentry <- function(entry,revenue_lob) {
		date <- revenue_lob[,1]==entry$Date
		account <- colnames(revenue_lob)==entry$Account
		if (sum(date)!=1|sum(account)!=1) {
			print(entry);
			stop("extraordinary event adjustment fail!");
		}
		revenue_lob[date,account] <- revenue_lob[date,account]-entry$Amount;
		revenue_lob;
	}
	revenue_lob_tmp <- revenue_lob
	if (colnames(revenue_lob_tmp)[1]=="CBXVIE"|colnames(revenue_lob_tmp)[1]=="OTHSEC") {
		entry<-list(Date="2014-09-30",Account="AIIOTH",Amount=346376352);
		revenue_lob_tmp <- adjust_for_singleentry(entry,revenue_lob_tmp);
		entry<-list(Date="2014-09-30",Account="NININC",Amount=346376352);
		revenue_lob_tmp <- adjust_for_singleentry(entry,revenue_lob_tmp);
		entry<-list(Date="2014-09-30",Account="TOTREV",Amount=346376352);
		revenue_lob_tmp <- adjust_for_singleentry(entry,revenue_lob_tmp);
#		revenue_lob_tmp[93,]-revenue_lob[93,];
		
		entry<-list(Date="2014-07-31",Account="AIIOTH",Amount=490016399);
		revenue_lob_tmp <- adjust_for_singleentry(entry,revenue_lob_tmp);
		entry<-list(Date="2014-07-31",Account="NININC",Amount=490016399);
		revenue_lob_tmp <- adjust_for_singleentry(entry,revenue_lob_tmp);
		entry<-list(Date="2014-07-31",Account="TOTREV",Amount=490016399);
		revenue_lob_tmp <- adjust_for_singleentry(entry,revenue_lob_tmp);
#		revenue_lob_tmp[91,]-revenue_lob[91,];		
	}
	
	revenue_lob_tmp;
}

adjust_expense_for_risk <- function(revenue_lob,plot=FALSE,Output_file_risk="") {
	oploss <- data.frame(revenue_lob[,1],revenue_lob[,colnames(revenue_lob)=="OPLNEW"]);
	revenue_lob[,colnames(revenue_lob)=="RSKNEW"] <- revenue_lob[,colnames(revenue_lob)=="RSKNEW"]-revenue_lob[,colnames(revenue_lob)=="OPLNEW"];
	revenue_lob[,colnames(revenue_lob)=="GENNEW"] <- revenue_lob[,colnames(revenue_lob)=="GENNEW"]-revenue_lob[,colnames(revenue_lob)=="OPLNEW"];
	revenue_lob[,colnames(revenue_lob)=="TOTDIR"] <- revenue_lob[,colnames(revenue_lob)=="TOTDIR"]-revenue_lob[,colnames(revenue_lob)=="OPLNEW"];
	revenue_lob[,colnames(revenue_lob)=="OPLNEW"] <- revenue_lob[,colnames(revenue_lob)=="OPLNEW"]-revenue_lob[,colnames(revenue_lob)=="OPLNEW"];
	
	if (plot) {
		start_and_frequency <- determine_frequency_and_start(oploss[,1]);
		.start <- start_and_frequency$start;
		.frequency <- start_and_frequency$frequency;
		.oploss <- ts(oploss[,2],start=.start,frequency=.frequency);
		.oploss_annual <- aggregate(.oploss,nfrequency=1,sum);
#		Output_file_risk <- Output_file;
		plot_name <- paste(Output_file_risk,"/",BU,"-OPLNEW.png",sep="");
		png(plot_name,width=800,height=600);
		ts.plot(.oploss,type="b",col=1:2);
		title(paste(BU,"-Operational Losses",sep=""));
		
		abline(v=as.numeric(time(.oploss_annual)));			
		.x <- as.numeric(time(.oploss_annual)+0.5);
		.level_revenue <- format_level_value(.oploss_annual);
		.grwothrate_revenue <- paste("(",c(NA,round(diff(.oploss_annual)/.oploss_annual[-length(.oploss_annual)]*100,0)),"%",")",sep="");
		text(.x,max(.oploss,na.rm=TRUE),paste(.level_revenue,.grwothrate_revenue),col=Color_history);	
		dev.off();		
	}
	revenue_lob;
}

adjust_expense_for_MNINEW <- function(revenue_lob,plot=FALSE,Output_file_risk="") {
	
	oploss <- data.frame(revenue_lob[,1],revenue_lob[,colnames(revenue_lob)=="MNINEW"]);
	revenue_lob[,colnames(revenue_lob)=="OGNNEW"] <- revenue_lob[,colnames(revenue_lob)=="OGNNEW"]-revenue_lob[,colnames(revenue_lob)=="MNINEW"];
	revenue_lob[,colnames(revenue_lob)=="GENNEW"] <- revenue_lob[,colnames(revenue_lob)=="GENNEW"]-revenue_lob[,colnames(revenue_lob)=="MNINEW"];
	revenue_lob[,colnames(revenue_lob)=="TOTDIR"] <- revenue_lob[,colnames(revenue_lob)=="TOTDIR"]-revenue_lob[,colnames(revenue_lob)=="MNINEW"];
	revenue_lob[,colnames(revenue_lob)=="MNINEW"] <- revenue_lob[,colnames(revenue_lob)=="MNINEW"]-revenue_lob[,colnames(revenue_lob)=="MNINEW"];
	
	if (plot) {
		start_and_frequency <- determine_frequency_and_start(oploss[,1]);
		.start <- start_and_frequency$start;
		.frequency <- start_and_frequency$frequency;
		.oploss <- ts(oploss[,2],start=.start,frequency=.frequency);
		.oploss_annual <- aggregate(.oploss,nfrequency=1,sum);
#		Output_file_risk <- Output_file;
		plot_name <- paste(Output_file_risk,"/",BU,"-MNINEW.png",sep="");
		png(plot_name,width=800,height=600);
		ts.plot(.oploss,type="b",col=1:2);
		title(paste(BU,"-Customer Suppl. Aggrements",sep=""));
		
		abline(v=as.numeric(time(.oploss_annual)));			
		.x <- as.numeric(time(.oploss_annual)+0.5);
		.level_revenue <- format_level_value(.oploss_annual);
		.grwothrate_revenue <- paste("(",c(NA,round(diff(.oploss_annual)/.oploss_annual[-length(.oploss_annual)]*100,0)),"%",")",sep="");
		text(.x,max(.oploss,na.rm=TRUE),paste(.level_revenue,.grwothrate_revenue),col=Color_history);	
		dev.off();		
	}
	revenue_lob;
}

compile_regression_data <- function(Macro,.variables,revenue) {
	#Macro is the historical Macro data;
	#.variables is the list of characters indicate variable names
	#revenue is historical revenue/expense stream with standard format
#	.variables <- variables
	.select_row <- Macro[,1]%in%revenue[,1];
	.select_colume <- colnames(Macro)%in%.variables;
	data <- cbind(revenue,Macro[.select_row,.select_colume]);
	
	isnull <- !is.na(data[,2])&!is.na(data[,3]);
	if (ncol(data)>3) {
		for (i in 4:ncol(data)) {
			isnull <- isnull&!is.na(data[,i]);
		}
	}
	data <- data[isnull,];
	
	if (length(.variables)==1) {
		colnames(data)[3] <- .variables;
	}
	
	data;
}

clean_na_data_frame <- function(data,clean_idential_column=TRUE) {
#	data <- revenue;
	if (class(data) != "data.frame") {
		warning("Not a data frame");
		stop();
	}
	.missingvalueflag <- !is.na(data[,1]);
	if (ncol(data)>1) {
		for (.i in 2:ncol(data)) {
			.missingvalueflag <- .missingvalueflag&!is.na(data[,.i]);
		}
	}
	data_clean <- data[.missingvalueflag,];
	
	if (clean_idential_column&ncol(data_clean) > 2) {
		identical_column <- apply(data_clean[,2:ncol(data_clean)],2,sd)==0;
		if (sum(identical_column) > 0) {
			data_clean <- data_clean[,-(which(identical_column)+1)];
		}
	}
	data_clean;
}

compile_forecast_data <- function(Macro,.variables,revenue) {
	#Macro is the historical Macro data;
	#.variables is the list of characters indicate variable names
	#revenue is historical revenue/expense stream with standard format
	.select_rows <- (max(which(Macro[,1]%in%revenue[,1]))+1):nrow(Macro);
	.select_colume <- colnames(Macro)%in%.variables;
#	data <- cbind(date = Macro[.select_rows,1], Macro[.select_rows,.select_colume]);
	data <- data.frame(date = Macro[.select_rows,1], Macro[.select_rows,.select_colume]);
	if (length(.variables)==1) {
		colnames(data)[2] <- .variables;
	}
	data;
}


compile_forecast_data_diff <- function(Macro,.variables,revenue) {
	#Macro is the historical Macro data;
	#.variables is the list of characters indicate variable names
	#revenue is historical revenue/expense stream with standard format
	#.variables <- regressor_name
	#.variables <- c("AGG","SP500");
	.select_rows <- (max(which(Macro[,1]%in%revenue[,1]))):nrow(Macro);
	.select_colume <- colnames(Macro)%in%.variables;
	data <- data.frame(date = Macro[.select_rows,1], Macro[.select_rows,.select_colume]);
	if (length(.variables)==1) {
		colnames(data)[2] <- .variables;
	}
	level_to_diff(data);
}


merge_successive_ts <- function(ts1, ts2) {
#	ts1 <- Historical_metric;
#	ts2 <- Forecast_baseline;
	if (frequency(ts1)!=frequency(ts2)) {
		return(NULL);
	}
	
	if (max(time(ts1))<min(time(ts2))) {
		return(ts(c(as.vector(ts1), as.vector(ts2)),frequency=frequency(ts1), start=start(ts1)));
	} else if(min(time(ts1))>max(time(ts2))) {
		return(ts(c(as.vector(ts2), as.vector(ts1)),frequency=frequency(ts1), start=start(ts2)));
	} else {
		return(NULL);
	}
}

formu_display <- function(revenue,fit) {
#	revenue <- data
	fit_display <- as.data.frame(fit$coefficients);
#	fit_display[,1] <- round(fit_display[,1],digits=2);
	fit_display[,1] <- format_level_value(fit_display[,1]);
#	names(summary(fit))
	.pvalues <- summary(fit)$coefficients[,4];
#	.pvalues[1]
	.formula <- "";
	if(length(.pvalues)==2) {
		.formula <- paste(colnames(revenue)[2], "~",fit_display[1,1])
		if (.pvalues[1]<0.05) {
			.formula <- paste0(.formula,"*")
		}
		.formula <- paste(.formula,"+",fit_display[2,1],rownames(fit_display)[2],sep=" ");
		if (.pvalues[2]<0.05) {
			.formula <- paste0(.formula,"*")
		}
		
	} else if(length(.pvalues)==3) {
		.formula <- paste(colnames(revenue)[2], "~",fit_display[1,1])
		if (.pvalues[1]<0.05) {
			.formula <- paste0(.formula,"*")
		}
		.formula <- paste(.formula,"+",fit_display[2,1],rownames(fit_display)[2],sep=" ");
		if (.pvalues[2]<0.05) {
			.formula <- paste0(.formula,"*")
		}
		.formula <- paste(.formula,"+",fit_display[3,1],rownames(fit_display)[3],sep=" ");
		if (.pvalues[3]<0.05) {
			.formula <- paste0(.formula,"*")
		}
#		paste(colnames(revenue)[2], "~",fit_display[1,1],"+",fit_display[2,1],rownames(fit_display)[2],"+",fit_display[3,1],rownames(fit_display)[3],sep=" ");	
	} else if (length(.pvalues)==4) {
		.formula <- paste(colnames(revenue)[2], "~",fit_display[1,1])
		if (.pvalues[1]<0.05) {
			.formula <- paste0(.formula,"*")
		}
		.formula <- paste(.formula,"+",fit_display[2,1],rownames(fit_display)[2],sep=" ");
		if (.pvalues[2]<0.05) {
			.formula <- paste0(.formula,"*")
		}
		.formula <- paste(.formula,"+",fit_display[3,1],rownames(fit_display)[3],sep=" ");
		if (.pvalues[3]<0.05) {
			.formula <- paste0(.formula,"*")
		}
		.formula <- paste(.formula,"+",fit_display[4,1],rownames(fit_display)[4],sep=" ");
		if (.pvalues[4]<0.05) {
			.formula <- paste0(.formula,"*")
		}
	} else if (length(.pvalues)==5) {
		.formula <- paste(colnames(revenue)[2], "~",fit_display[1,1])
		if (.pvalues[1]<0.05) {
			.formula <- paste0(.formula,"*")
		}
		.formula <- paste(.formula,"+",fit_display[2,1],rownames(fit_display)[2],sep=" ");
		if (.pvalues[2]<0.05) {
			.formula <- paste0(.formula,"*")
		}
		.formula <- paste(.formula,"+",fit_display[3,1],rownames(fit_display)[3],sep=" ");
		if (.pvalues[3]<0.05) {
			.formula <- paste0(.formula,"*")
		}
		.formula <- paste(.formula,"+",fit_display[4,1],rownames(fit_display)[4],sep=" ");
		if (.pvalues[4]<0.05) {
			.formula <- paste0(.formula,"*")
		}
		.formula <- paste(.formula,"+",fit_display[5,1],rownames(fit_display)[5],sep=" ");
		if (.pvalues[5]<0.05) {
			.formula <- paste0(.formula,"*")
		}
	}
	.formula
}

format_percentage <- function(.percentage) {
	paste(round(as.numeric(.percentage)*100,digits=1),"%",sep="");
}

#format_level_value <- function(.revenue_aggregate) {
##	.revenue_aggregate <- round(as.data.frame(fit$coefficients)[,1],digits=2)
#	if (abs(mean(.revenue_aggregate))>1000000000000) {
#		return(paste(formatC(round(as.numeric(.revenue_aggregate)/1000000000000,digits=1),format="fg",big.mark=","),"T",sep=""));
#	} else if (abs(mean(.revenue_aggregate))>1000000000) {
#		return(paste(formatC(round(as.numeric(.revenue_aggregate)/1000000000,digits=1),format="fg",big.mark=","),"B",sep=""));
#	} else if (abs(mean(.revenue_aggregate))>1000000) {
#		return(paste(formatC(round(as.numeric(.revenue_aggregate)/1000000,digits=1),format="d",big.mark=","),"M",sep=""));
#	} else {
#		return(paste(formatC(round(as.numeric(.revenue_aggregate)/1000,digits=1),format="d",big.mark=","),"K",sep=""));
#	}
#}

format_level_value <- function(.revenue_aggregate) {
#	.revenue_aggregate <- fit_display[,1]
	.revenue_aggregate_formated <- .revenue_aggregate;
	for (i in 1:length(.revenue_aggregate)) {
#		i <- 1;
#		if (is.na(.revenue_aggregate[i])) {
#			.revenue_aggregate_formated[i] <- NA;
#		} else if (abs(.revenue_aggregate[i])>1000000000000) {
#			.revenue_aggregate_formated[i] <- paste(formatC(round(as.numeric(.revenue_aggregate[i])/1000000000000,digits=3),format="fg",big.mark=","),"T",sep="");
#		} else if (abs(.revenue_aggregate[i])>1000000000) {
#			.revenue_aggregate_formated[i] <- paste(formatC(round(as.numeric(.revenue_aggregate[i])/1000000000,digits=3),format="fg",big.mark=","),"B",sep="");
#		} else if (abs(.revenue_aggregate[i])>1000000) {
#			.revenue_aggregate_formated[i] <- paste(formatC(round(as.numeric(.revenue_aggregate[i])/1000000,digits=3),format="fg",big.mark=","),"M",sep="");
#		} else if (abs(.revenue_aggregate[i])>1000){
#			.revenue_aggregate_formated[i] <- paste(formatC(round(as.numeric(.revenue_aggregate[i])/1000,digits=3),format="fg",big.mark=","),"K",sep="");
#		} else{
#			.revenue_aggregate_formated[i] <- round(as.numeric(.revenue_aggregate_formated[i]), digits=3);
#		}
		if (is.na(.revenue_aggregate[i])) {
			.revenue_aggregate_formated[i] <- NA;
		} else if (abs(.revenue_aggregate[i])>1000000000000) {
			.revenue_aggregate_formated[i] <- paste(round(as.numeric(.revenue_aggregate[i])/1000000000000,digits=1),"T",sep="");
		} else if (abs(.revenue_aggregate[i])>1000000000) {
			.revenue_aggregate_formated[i] <- paste(round(as.numeric(.revenue_aggregate[i])/1000000000,digits=1),"B",sep="");
		} else if (abs(.revenue_aggregate[i])>1000000) {
			.revenue_aggregate_formated[i] <- paste(round(as.numeric(.revenue_aggregate[i])/1000000,digits=1),"M",sep="");
		} else if (abs(.revenue_aggregate[i])>1000){
			.revenue_aggregate_formated[i] <- paste(round(as.numeric(.revenue_aggregate[i])/1000,digits=1),"K",sep="");
		} else{
			.revenue_aggregate_formated[i] <- round(as.numeric(.revenue_aggregate_formated[i]), digits=2);
		}
	}
	.revenue_aggregate_formated;
}

reconcile <- function (totdir,segmentation) {
	return(all(abs(apply(segmentation,1,sum)-totdir)<10));
}

revenue_analysis <- function(revenue,threshold=revenue_threshold) {
	
	if (nrow(revenue)==0) {
		return(FALSE);
	}
	
	#if last year's revenue is more than threshold
	#if revenue period is more than 12 periods
	if (determine_frequency_and_start(revenue[,1])$frequency==12) {
		criteria1 <- abs(sum(revenue[(nrow(revenue)-11):nrow(revenue),2], na.rm=TRUE)) > threshold&sd(revenue[,2],na.rm = TRUE)!=0;	
		criteria2 <- length(na.omit(revenue[,2])) > 24;
	} else {
		criteria1 <- abs(sum(revenue[(nrow(revenue)-3):nrow(revenue),2], na.rm=TRUE)) > threshold&sd(revenue[,2],na.rm = TRUE)!=0;
		criteria2 <- length(na.omit(revenue[,2])) > 8;
	}
	
	if (criteria1 & criteria2) {
		return(TRUE);
	} else {
		return(FALSE);
	}
#	return TRUE if pass threshold
}

metric_analysis <- function(revenue,revenue_threshold) {
	
	#if revenue period is more than 12 periods
	criteria2 <- length(na.omit(revenue[,2])) > 8
	
	if (criteria2) {
		return(TRUE);
	} else {
		return(FALSE);
	}
#	return TRUE if pass threshold
}


monthly_to_quarterly <- function(revenue) {
	if (determine_frequency_and_start(revenue[,1])$frequency == 4) {
		return(revenue);
	} else {
		revenue.ts <- aggregate(ts(revenue[,2],frequency=12,start=c(as.numeric(substr(revenue[1,1],1,4)),as.numeric(substr(revenue[1,1],6,7)))),nfrequency=4,FUN=sum);
		revenue_Q <- data.frame(revenue.ts);
		date <- as.Date(as.yearmon(time(revenue.ts))+0.25)-1
		#old wrong one: date <- as.Date(as.yearmon(2007 + seq(1, length(revenue.ts))/4),frac=0)-1;
		date <- date[(length(date)-nrow(revenue_Q)+1):length(date)]
		revenue_Q <- data.frame(date,revenue_Q)
		colnames(revenue_Q)[2] <- colnames(revenue)[2];
		return(revenue_Q);
	}
}

determine_frequency_and_start <- function(.dates) {
	#.dates should be a vector of dates
	#.dates <- forecast_ts_month
	.dates_num_1 <- as.numeric(as.yearmon(.dates[1]));
	.dates_num_n <- as.numeric(as.yearmon(.dates[length(.dates)]));
	n <- length(.dates)
	
	if((.dates_num_n-.dates_num_1)/n < 0.15) {
		.frequency <- 12;
	} else {
		.frequency <- 4;
	}
	
	.year <- as.numeric(substr(.dates[1],1,4));
	.month <- as.numeric(substr(.dates[1],6,7));
	if (.frequency == 12) {
		return(list(frequency=.frequency,start=c(.year,.month)));
	} else {
		if (.month < 4) {
			.quarter <- 1;
		} else if (.month < 7) {
			.quarter <- 2;
		} else if (.month < 10) {
			.quarter <- 3;
		} else {
			.quarter <- 4;
		}
		return(list(frequency=.frequency,start=c(.year,.quarter)));
	}
}

determine_frequency_and_start_DiffBacktest <-  function(revenue,data) {
	.frequency <- determine_frequency_and_start(revenue[,1])$frequency;
	
	.tmp <- revenue[min(which(revenue[,1]%in%clean_na_data_frame(data)[,1]))-1,];
	.year <- as.numeric(substr(.tmp[1,1],1,4));
	.month <- as.numeric(substr(.tmp[1,1],6,7));
	
	if (.frequency == 12) {
		return(list(value=.tmp[1,2],start=c(.year,.month),frequency=.frequency));
	} else {
		if (.month < 4) {
			.quarter <- 1;
		} else if (.month < 7) {
			.quarter <- 2;
		} else if (.month < 10) {
			.quarter <- 3;
		} else {
			.quarter <- 4;
		}
		return(list(value=.tmp[1,2],start=c(.year,.quarter),frequency=.frequency));
	}
}

determine_time_periods <- function(time_stamp) {
#			time_stamp <- Macro_Q[,1]
	if (determine_frequency_and_start(time_stamp)$frequency==4) {
		Time_periods_Q
	} else {		
		Time_periods;
	}
}

determine_Macro <- function(time_stamp, Macro, Macro_Q) {
	if (determine_frequency_and_start(time_stamp)$frequency==4) {
		Macro_Q
	} else {		
		Macro;
	}
}

determine_yield_periods <- function(time_stamp) {
	#last 6 month average
	if (determine_frequency_and_start(time_stamp)$frequency==4) {
		2;
	} else {		
		6;
	}
}

get_lm_formula <- function(data) {
	formu <- paste(colnames(data)[2], "~", colnames(data)[3]);
	if (ncol(data)>3) {
		for (j in 4:ncol(data)) {
			formu <- paste(formu,"+",colnames(data)[j]);
		}
	}
	formu
}

fit_regression <- function(data) {
#	data <- regression_data
	formu <- paste(colnames(data)[2], "~", colnames(data)[3]);
	if (ncol(data)>3) {
		for (j in 4:ncol(data)) {
			formu <- paste(formu,"+",colnames(data)[j]);
		}
	}
	fit <- lm(as.formula(formu),data=data);
#	paste("lm(", formu,",data=data)",sep="")
	fit;
}

fit_regression_no_intercept <- function(data) {
	formu <- paste(colnames(data)[2], "~ 0+", colnames(data)[3]);
	if (ncol(data)>3) {
		for (j in 4:ncol(data)) {
			formu <- paste(formu,"+",colnames(data)[j]);
		}
	}
	fit <- lm(as.formula(formu),data=data);
	fit;
}

Out_of_Sample_Test <- function(Output_file_outsample,data) {
	#require output directory and formated data to do out of sample testing
	start_and_frequency <- determine_frequency_and_start(data[,1]);
	.start <- start_and_frequency$start;
	.frequency <- start_and_frequency$frequency;
	Historical_metric <- ts(data[,2],start=.start,frequency=.frequency);
	
	if (.frequency==12) {
		outsampleperiod <- c(6,12,18,24);
	} else {
		outsampleperiod <- c(3,6,9);
	}
	for (i in outsampleperiod) {
		#			i<-outsampleperiod[1]
		#get in sample observations and refit the regression
		.data <- data[1:(nrow(data)-i),];
		tmp.fit <- fit_regression(.data);
		
		#back test in sample data
		start_and_frequency <- determine_frequency_and_start(.data[,1]);
		.start <- start_and_frequency$start;
		Backtesting <- ts(predict(tmp.fit,newdata=.data),start=.start,frequency=.frequency);
		
		#test out of sample data
		.newdata <- data[(nrow(data)-i+1):nrow(data),];
		start_and_frequency <- determine_frequency_and_start(.newdata[,1]);
		.start <- start_and_frequency$start;
		Outofsample <- ts(predict(tmp.fit,newdata=.newdata),start=.start,frequency=.frequency);
		
		#plot
		plot_name <- paste(Output_file_outsample,"/Out-of-sample-test-ExcludeRecent",i,".png",sep="");
		png(plot_name,width=800,height=600);
		ts.plot(Historical_metric,Backtesting,Outofsample,col=c("black","blue","red"),type="b",ylab="Metric",sub=formu_display(data,tmp.fit));
		abline(v=as.numeric(as.yearmon(.data[nrow(.data),1])));
		title(main=paste("out-of-sample test /",BU,"/",colnames(data)[2],"/ Exclude Recent",i,"Period"));
		dev.off();
		
		out <- capture.output(summary(tmp.fit));
		file_name <- paste(Output_file_outsample,"/Out-of-sample-test-ExcludeRecent",i,".txt",sep="");
		cat(out, file=file_name, sep="\n");
	}
	
	for (i in outsampleperiod) {
		#			i<-1
		.data <- data[(1+i):nrow(data),];
		tmp.fit <- fit_regression(.data);
		
		#back test in sample data
		start_and_frequency <- determine_frequency_and_start(.data[,1]);
		.start <- start_and_frequency$start;
		Backtesting <- ts(predict(tmp.fit,newdata=.data),start=.start,frequency=.frequency);
		
		#test out of sample data
		.newdata <- data[1:i,];
		start_and_frequency <- determine_frequency_and_start(.newdata[,1]);
		.start <- start_and_frequency$start;
		Outofsample <- ts(predict(tmp.fit,newdata=.newdata),start=.start,frequency=.frequency);
		
		plot_name <- paste(Output_file_outsample,"/Out-of-sample-test-ExcludeAcient",i,".png",sep="");
		png(plot_name,width=800,height=600);
		ts.plot(Historical_metric,Backtesting,Outofsample,col=c("black","blue","red"),type="b",ylab="Metric",sub=formu_display(data,tmp.fit));
		abline(v=as.numeric(as.yearmon(.data[1,1])));
		title(main=paste("out-of-sample test /",BU,"/",colnames(data)[2],"/ Exclude Acient",i,"Period"));
		dev.off();
		
		out <- capture.output(summary(tmp.fit));
		file_name <- paste(Output_file_outsample,"/Out-of-sample-test-ExcludeAcient",i,".txt",sep="");
		cat(out, file=file_name, sep="\n");
	}
}

####advanced functions depending on other functions
Variable_Selection_1 <- function(threshold=0.1,Output_file,revenue,Macro) {
	#threshold is a number. 0.05 or 0.1
	#output folder to write results to
	#revenue has two columes. first column is the date. second are revenue numbers.
	#Macro is the combination of all independent variables
#	Output_file <- Output_file_regression_item1
#	Macro <- .Macro
	#	i <- 1;
#	Output_file <- Output_file_regressionQ_item1
#	Macro <- Macro_Q
	
	Model_Fitting_Results <- data.frame(matrix(NA,ncol=19));
	
	.all_variables <- colnames(Macro)[2:ncol(Macro)];
	combinations <- t(combn(.all_variables,1));	
	.select_row <- Macro[,1]%in%revenue[,1];
	for (j in 1:nrow(combinations)) {
		# j <- 1;
		.model_fitting_results <- rep(NA,19);
		
		.variables <- combinations[j,];
		.select_colume <- colnames(Macro)%in%.variables;
		data <- cbind(revenue,Macro[.select_row,.select_colume]);
		data <- data[!is.na(data[,2])&!is.na(data[,3]),];#changes made on 5/2/16
		colnames(data)[3] <- .variables;
		if (sd(data[,3])==0) {
			next();
		}
		#regression
		formu <- paste(colnames(data)[2], "~", colnames(data)[3]);
		if (ncol(data)>3) {
			for (k in 4:ncol(data)) {
				formu <- paste(formu,"+",colnames(data)[k]);
			}
		}
		fit <- lm(as.formula(formu),data=data);
		
		.coef <- summary(fit)$coefficients[,1];
		.model_fitting_results[1:4] <- c(.coef,rep(NA,4-length(.coef)));
		.model_fitting_results[5] <- as.character(data[1,1]);
		.model_fitting_results[6] <- as.character(data[nrow(data),1]);
		.model_fitting_results[7] <- summary(fit)$adj.r.squared;
		.model_fitting_results[8] <- all(summary(fit)$coefficients[-1,4]<0.05);
		
		#Test3: residuals
		#Stationary Test
#		unit_root_test <- c();
#		for (k in 2:ncol(data)) {
#			unit_root_test <- c(unit_root_test, adf.test(data[,k])$p.value);
#		}
#
#		if (all(unit_root_test<=threshold)) {
#			.model_fitting_results[9] <- 1;
#		} else if (all(unit_root_test>threshold)) {
#			unit_root_residual <- adf.test(fit$residuals)$p.value;
#			unit_root_test <- c(unit_root_test, unit_root_residual);
#			if (unit_root_residual<= threshold) {
#				.model_fitting_results[9] <- 0.5;
#			} else {
#				.model_fitting_results[9] <- 0;
#			}
#		} else {
#			.model_fitting_results[9] <- 0;
#		}
		
#		unit_root_test <- c();
#		for (k in 2:ncol(data)) {
#			unit_root_test <- c(unit_root_test, Stationary_test(data[,k]));
#		}
#		
#		if (all(unit_root_test<=threshold)) {
#			.model_fitting_results[9] <- 1;
#		} else if (all(unit_root_test>threshold)) {
#			unit_root_residual <- Stationary_test(fit$residuals);
#			unit_root_test <- c(unit_root_test, unit_root_residual);
#			if (unit_root_residual<= threshold) {
#				.model_fitting_results[9] <- 0.5;
#			} else {
#				.model_fitting_results[9] <- 0;
#			}
#		} else {
#			.model_fitting_results[9] <- 0;
#		}
		unit_root_residual <- Stationary_test(fit$residuals);
		if (unit_root_residual<= threshold) {
			.model_fitting_results[9] <- 1;
		} else {
			.model_fitting_results[9] <- 0;
		}

#		tryCatch(
#				{
#					coint <- ca.jo(data[,3:ncol(data)],type="trace",K=2,ecdet="none",spec="longrun");
#					.model_fitting_results[10] <- any(coint@teststat[2] > coint@cval[2,]);
#				}, error = function(e) {
#					print(e);
#				}, finally = {}
#		)
		.model_fitting_results[10] <- FALSE;
		
		#Test2: collinearity
		#VIF test
#		.tmp.vif <- vif(fit);
		.model_fitting_results[11] <- 1;
		
		.tmp.bp <- bptest(fit);
		.model_fitting_results[12] <- .tmp.bp$p.value;
		
		.tmp.dw <- dwtest(fit);
		.model_fitting_results[13] <- .tmp.dw$p.value;
		
		.tmp.bg <- bgtest(fit,order=2);
		.model_fitting_results[14] <- .tmp.bg$p.value;
		
		#Normality of residuals
		#Wilk-Shapiro test
		.tmp.ws <- shapiro.test(fit$residuals);
		.model_fitting_results[15] <- .tmp.ws$p.value;
		.tmp.ks <- ks.test(fit$residuals,"pnorm",mean(fit$residuals), sd(fit$residuals));
		.model_fitting_results[16] <- .tmp.ks$p.value;
		
		#write variable names
		.model_fitting_results[17:18] <- colnames(data)[3:4];
		
		#write test results to Model_Fitting_Results data frame;
		Model_Fitting_Results <- rbind(Model_Fitting_Results,.model_fitting_results);
	}
	Model_Fitting_Results <- Model_Fitting_Results[!is.na(Model_Fitting_Results[,1]),];
	colnames(Model_Fitting_Results) <-c("Intercept","Variables1","Variable2","Variable3","start_history","end_history","R^2","Significance of regressors","Engle-Granger stationarity test", "Johansen Stationary Test","max(VIF)","Breusch-Pagan","Durbin-Whatson","Breusch-Godfrey","Shapiro-Wilk","Kolmogorov-Smirnov");
	file.name <- paste(Output_file,"/Model_Fitting_Results_2Varaibles.csv",sep="");
	write.csv(Model_Fitting_Results,file.name);
	
	Model_Fitting_Results[,11] <- as.numeric(Model_Fitting_Results[,11])<10;
	for (j in c(12:16)) {
		Model_Fitting_Results[,j] <- as.numeric(Model_Fitting_Results[,j])>0.05;
	}
	
	Model_Fitting_Results_Good <- Model_Fitting_Results;
	.stationary <- rep(FALSE,nrow(Model_Fitting_Results_Good));
	.stationary[Model_Fitting_Results_Good[,9]==0.5|Model_Fitting_Results_Good[,9]==1] <- TRUE;
	.stationary[!is.na(Model_Fitting_Results_Good[,10])&Model_Fitting_Results_Good[,10]==TRUE] <- TRUE;
	
	Model_Fitting_Results_Good <- Model_Fitting_Results_Good[Model_Fitting_Results_Good[,8]=="TRUE"&.stationary,];
	Model_Fitting_Results_Good <- Model_Fitting_Results_Good[order(Model_Fitting_Results_Good[,7],decreasing=TRUE),];
	
	file.name <- paste(Output_file,"/Model_Fitting_Results_Good_2Varaibles.csv",sep="")
	write.csv(Model_Fitting_Results_Good,file.name);
	
	Model_Fitting_Results_Best <- Model_Fitting_Results_Good;
	select <- apply(Model_Fitting_Results_Best[,11:16],1,sum);		
	Model_Fitting_Results_Best <- Model_Fitting_Results_Best[select==max(select),];
	
	file.name <- paste(Output_file,"/Model_Fitting_Results_Best_2Varaibles.csv",sep="")
	write.csv(Model_Fitting_Results_Best,file.name);
	
}


#Difference Model with 1 variable
Variable_Selection_Diff_1variable <- function(threshold=0.1,Output_file,revenue,.Macro) {
	#threshold is a number. 0.05 or 0.1
	#output folder to write results to
	#revenue has two columes. first column is the date. second are revenue numbers.
	#Macro is the combination of all independent variables
#	Output_file <- Output_file_regression_item1
#	.Macro <- Macro
	#	i <- 1;
	
	Model_Fitting_Results <- data.frame(matrix(NA,ncol=19));
	
	revenue_ts <- revenue_to_ts(revenue);
	regressee_name <- colnames(revenue)[2];
	for (i in 2:ncol(.Macro)) {
#		i <- 2;
		.model_fitting_results <- rep(NA,19);
		
		regressor_name <- colnames(.Macro)[i]
		##do plot of trend and growth rates
		regressor <- .Macro[,c(1,i)];
		regressor <- regressor[regressor[,1]%in%revenue[,1],];
		regressor_ts <- revenue_to_ts(regressor);
		
		####run regression on difference
		#transform data difference
		revenue_diff <- level_to_diff(revenue);
		revenue_ts_diff <- revenue_to_ts(revenue_diff);
		regressor_diff <- level_to_diff(regressor);
		regressor_ts_diff <- revenue_to_ts(regressor_diff)
		data <- cbind(revenue_diff,regressor_diff)[,-3];
		data <- data[!is.na(data[,2])&!is.na(data[,3]),];#changes made on 5/2/16
		
#		adf1 <- round(adf.test(na.omit(data[,2]),k=0)$p.value,digit=2);
#		adf2 <- round(adf.test(na.omit(data[,3]),k=0)$p.value,digit=2);
		fit <- fit_regression(data);
		#		fit_0 <- fit_regression_no_intercept(data);
		
		.coef <- summary(fit)$coefficients[,1];
		.model_fitting_results[1:4] <- c(.coef,rep(NA,4-length(.coef)));
		.model_fitting_results[5] <- as.character(data[1,1]);
		.model_fitting_results[6] <- as.character(data[nrow(data),1]);
		.model_fitting_results[7] <- summary(fit)$adj.r.squared;
		.model_fitting_results[8] <- all(summary(fit)$coefficients[-1,4]<0.05);
		
		#ADD Stationary test
		unit_root_test <- c();
		for (k in 2:ncol(data)) {
			unit_root_test <- c(unit_root_test, Stationary_test(data[,k]));
		}
		unit_root_test <- c(unit_root_test, Stationary_test(fit$residuals));
		#if all p values less than threshold, pass.
		if (all(unit_root_test<=threshold)) {
			.model_fitting_results[9] <- 1;
		} else {
			.model_fitting_results[9] <- 0;
		}
		
		.model_fitting_results[17:18] <- colnames(data)[3:4];
		Model_Fitting_Results <- rbind(Model_Fitting_Results,.model_fitting_results);		
	}
	
	Model_Fitting_Results <- Model_Fitting_Results[!is.na(Model_Fitting_Results[,1]),];
	colnames(Model_Fitting_Results) <-c("Intercept","Variables1","Variable2","Variable3","start_history","end_history","R^2","Significance of regressors","Engle-Granger stationarity test", "Johansen Stationary Test","max(VIF)","Breusch-Pagan","Durbin-Whatson","Breusch-Godfrey","Shapiro-Wilk","Kolmogorov-Smirnov");
	file.name <- paste(Output_file,"/Model_Fitting_Results_1Varaibles.csv",sep="");
	write.csv(Model_Fitting_Results,file.name);
	
	Model_Fitting_Results_Good <- Model_Fitting_Results;
	.stationary <- rep(FALSE,nrow(Model_Fitting_Results_Good));
	.stationary[Model_Fitting_Results_Good[,9]==0.5|Model_Fitting_Results_Good[,9]==1] <- TRUE;
	
	Model_Fitting_Results_Good <- Model_Fitting_Results_Good[Model_Fitting_Results_Good[,8]=="TRUE"&.stationary,];
	Model_Fitting_Results_Good <- Model_Fitting_Results_Good[order(Model_Fitting_Results_Good[,7],decreasing=TRUE),];
	
	file.name <- paste(Output_file,"/Model_Fitting_Results_Good_1Varaibles.csv",sep="")
	write.csv(Model_Fitting_Results_Good,file.name);
}

Variable_Selection_Diff_1variable_Test1 <- function(num_models_to_examine,Output_file,revenue,Macro,Macro_2,Macro_3,Macro_4,BU) {
#Output_file <- Output_file_regression_item1;
#Macro <- Macro_Q
#Macro_2 <- Macro_2_Q
#Macro_3 <- Macro_3_Q
#Macro_4 <- Macro_4_Q
	
	file_name <- paste(Output_file,"/Model_Fitting_Results_Good_1Varaibles.csv",sep="")
	best_file <- read.csv(file_name);
	if (nrow(best_file) == 0) {
		file_name <- paste(Output_file,"/Model_Fitting_Results_Good_2Varaibles.png",sep="");
		best_file_tex <- data.frame(Warning=c("No Model Pass Pivot Statistical Test",Output_file));
		png(file_name,height=35*nrow(best_file_tex),width=800);
		grid.table(best_file_tex);
		dev.off();
		
		return();
	}
	best_file <- best_file[,-1];
	
	if (nrow(best_file)>num_models_to_examine) {
		best_max <- num_models_to_examine;
	} else {
		best_max <- nrow(best_file);
	}
	revenue_name <- colnames(revenue)[2];
	
	regressee_name <- colnames(revenue)[2];
	for (i in 1:best_max) {
		#		i <- 1;
		regressor_name <- as.character(best_file[i,17]);
		##do plot of trend and growth rates
		regressor <- Macro[,c(1,which(colnames(Macro)==regressor_name))];
		regressor <- regressor[regressor[,1]%in%revenue[,1],];
		
		####run regression on difference		
		#transform data difference
		revenue_diff <- level_to_diff(revenue);
		revenue_ts_diff <- revenue_to_ts(revenue_diff);
		regressor_diff <- level_to_diff(regressor);
		regressor_ts_diff <- revenue_to_ts(regressor_diff);
		data <- cbind(revenue_diff,regressor_diff)[,-3];
		data <- data[!is.na(data[,2])&!is.na(data[,3]),];#changes made on 5/2/16
		
#		adf1 <- round(adf.test(na.omit(data[,2]),k=0)$p.value,digit=2);
#		adf2 <- round(adf.test(na.omit(data[,3]),k=0)$p.value,digit=2);
		fit <- fit_regression(data);
		#		fit_0 <- fit_regression_no_intercept(data);
		
		start_and_frequency <- determine_frequency_and_start(data[,1]);
		.frequency <- start_and_frequency$frequency;
		.start <- start_and_frequency$start;

		Historical_metric <- ts(data[,2],start=.start,frequency=.frequency);
		Backtesting <- ts(predict(fit,newdata=data),start=.start,frequency=.frequency);
		
		#########
		#Forecast baseline
		.forecast_data_baseline <- compile_forecast_data_diff(Macro,regressor_name,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data_baseline[,1]);
		Forecast_baseline <- ts(predict(fit,.forecast_data_baseline),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		#Forecast advser
		.forecast_data_adverse <- compile_forecast_data_diff(Macro_2,regressor_name,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data_adverse[,1]);
		Forecast_adverse <- ts(predict(fit,.forecast_data_adverse),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		#Forecast severe
		.forecast_data_severe <- compile_forecast_data_diff(Macro_3,regressor_name,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data_severe[,1]);
		Forecast_severe <- ts(predict(fit,.forecast_data_severe),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		#Forecast bhc
		.forecast_data_bhc <- compile_forecast_data_diff(Macro_4,regressor_name,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data_bhc[,1]);
		Forecast_bhc <- ts(predict(fit,.forecast_data_bhc),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		#backtesting
		plot_name <- paste(Output_file,"/Diff-Model-backtesting-",BU,"-",i,".png",sep="");
		png(plot_name,width=800,height=600);
		ts.plot(Historical_metric,Backtesting,Forecast_baseline,Forecast_adverse,Forecast_severe,Forecast_bhc,col=c(Color_history,Color_backtest,Color_baseline,Color_adverse,Color_severe,Color_bhc),type="b",ylab="Metric",sub=formu_display(data,fit));
		title(paste("Backtest and Forecast ",BU,"/",regressee_name," V.S. ",regressor_name," /R^2=",round(summary(fit)$r.squared,2),sep=""));
		legend("topleft",legend=c("Hisotry","Backtesting"),pch=1,col=c(Color_history, Color_backtest));
		dev.off();
		
		#Convert back to levels
		actual_level <- revenue_to_ts(revenue);

		tmp <- determine_frequency_and_start_DiffBacktest(revenue,data);
		start_value <- tmp$value;		
		.start <- tmp$start;
		increments <- na.omit(as.vector(Backtesting));
		Backtesting_level <- diff_to_level(start_value,increments,.start,.frequency);
		
		start_value <- revenue[nrow(revenue),2];
		increments <- as.vector(Forecast_baseline);
		.start <- start(Forecast_baseline);
		.frequency <- frequency(Forecast_baseline);
		Forecast_baseline_level <- diff_to_level(start_value,increments,.start,.frequency,remove_first=TRUE);
		
		start_value <- revenue[nrow(revenue),2];
		increments <- as.vector(Forecast_adverse);
		.start <- start(Forecast_adverse);
		.frequency <- frequency(Forecast_adverse);
		Forecast_adverse_level <- diff_to_level(start_value,increments,.start,.frequency,remove_first=TRUE);
		
		start_value <- revenue[nrow(revenue),2];
		increments <- as.vector(Forecast_severe);
		.start <- start(Forecast_severe);
		.frequency <- frequency(Forecast_severe);
		Forecast_severe_level <- diff_to_level(start_value,increments,.start,.frequency,remove_first=TRUE);
		
		start_value <- revenue[nrow(revenue),2];
		increments <- as.vector(Forecast_bhc);
		.start <- start(Forecast_bhc);
		.frequency <- frequency(Forecast_bhc);
		Forecast_bhc_level <- diff_to_level(start_value,increments,.start,.frequency,remove_first=TRUE);
		
		plot_name <- paste(Output_file,"/Diff-Model-backtesting-level-",BU,"-",i,".png",sep="");
		png(plot_name,width=800,height=600);
		ts.plot(actual_level,Backtesting_level,Forecast_baseline_level,Forecast_adverse_level,Forecast_severe_level,Forecast_bhc_level,col=c(Color_history,Color_backtest,Color_baseline,Color_adverse,Color_severe,Color_bhc),type="b",sub=formu_display(revenue,fit))
		title(paste("Backtest Level Value ",BU,"/",regressee_name," V.S. ",regressor_name," /R^2=",round(summary(fit)$r.squared,2),sep=""));
		legend("topleft",legend=c("Hisotry","Backtesting"),pch=1,col=c(Color_history, Color_backtest));
		dev.off();
	}
	
	#output to picture to be included in Latex
	best_file_tex <- best_file[1:best_max,];
	for (j in 1:2) {
		best_file_tex[,j] <- format_level_value(best_file_tex[,j]);
	}
	best_file_tex$R.2 <- round(best_file_tex$R.2,digits=2);
	best_file_tex <- best_file_tex[,c(1,2,7,17)];
	colnames(best_file_tex) <- c("Intercept", "Variables1", "R-square", "V1-Name");
	file_name <- paste(Output_file,"/Model_Fitting_Results_Good_2Varaibles.png",sep="");
	png(file_name,height=30*nrow(best_file_tex),width=800);
	grid.table(best_file_tex);
	dev.off();
}

##Difference Model with 1 variable
#Variable_Selection_Diff_1 <- function(threshold=0.1,Output_file,revenue,.Macro) {
#	#threshold is a number. 0.05 or 0.1
#	#output folder to write results to
#	#revenue has two columes. first column is the date. second are revenue numbers.
#	#Macro is the combination of all independent variables
##	Output_file <- Output_file_regression_item1
##	.Macro <- Macro
#	#	i <- 1;
#	if (sd(revenue[,2])==0) {
#		return();
#	}
#	revenue_ts <- revenue_to_ts(revenue);
#	regressee_name <- colnames(revenue)[2];
#	for (i in 2:ncol(.Macro)) {
##		i <- 40;
#		regressor_name <- colnames(.Macro)[i]
#		##do plot of trend and growth rates
#		regressor <- .Macro[,c(1,i)];
#		regressor <- regressor[regressor[,1]%in%revenue[,1],];
#		regressor_ts <- revenue_to_ts(regressor);
#		
#		####run regression on difference		
#		#transform data difference
#		revenue_diff <- level_to_diff(revenue);
#		revenue_ts_diff <- revenue_to_ts(revenue_diff);
#		regressor_diff <- level_to_diff(regressor);
#		regressor_ts_diff <- revenue_to_ts(regressor_diff)
#		data <- cbind(revenue_diff,regressor_diff)[,-3];
#		adf1 <- round(adf.test(na.omit(data[,2]),k=0)$p.value,digit=2);
#		adf2 <- round(adf.test(na.omit(data[,3]),k=0)$p.value,digit=2);
#		fit <- fit_regression(data);
#		#		fit_0 <- fit_regression_no_intercept(data);
#		
#		start_and_frequency <- determine_frequency_and_start(data[,1]);
#		.frequency <- start_and_frequency$frequency;
#		.start <- start_and_frequency$start;
#		Backtesting <- ts(predict(fit,newdata=data),start=.start,frequency=.frequency);
#		
#		Historical_metric <- ts(data[,2],start=.start,frequency=.frequency);
#		
#		#backtesting
#		plot_name <- paste(Output_file,"/Diff-Model-backtesting-",BU,"-",regressee_name,"-",regressor_name,".png",sep="");
#		png(plot_name,width=800,height=600);
#		ts.plot(Historical_metric,Backtesting,col=c("black","blue"),type="b",ylab="Metric",sub=formu_display(revenue,fit));
#		title(paste("Backtest and Forecast ",BU,"/",regressee_name," V.S. ",regressor_name," /R^2=",round(summary(fit)$r.squared,2),sep=""));
#		legend("topleft",legend=c("Actual","Backtesting"),pch=1,col=c("black","blue"));
#		dev.off();
#		
#		#backting to level value
#		fitted_gr <- predict(fit,newdata=data);
#		fitted_gr <- c(0,fitted_gr);
#		fitted_value <- revenue;
#		for(k in 2:nrow(revenue)) {
#			if (!is.na(fitted_gr[k])) {
#				fitted_value[k,2] <- fitted_value[k-1,2]+fitted_gr[k];			
#			}
#		}
#		
#		fitted_value_ts <- revenue_to_ts(fitted_value);
#		plot_name <- paste(Output_file,"/Diff-Model-backtesting-level-",BU,"-",regressee_name,"-",regressor_name,".png",sep="");
#		png(plot_name,width=800,height=600);
#		ts.plot(revenue_ts,fitted_value_ts,col=c(Color_black,Color_blue),type="b",sub=formu_display(revenue,fit))
#		title(paste("Backtest Level Value ",BU,"/",regressee_name," V.S. ",regressor_name," /R^2=",round(summary(fit)$r.squared,2),sep=""));
#		legend("topleft",legend=c("Actual","Backtesting"),pch=1,col=c("black","blue"));
#		dev.off();
#		
##		#out sample testing
##		Output_file_outsample <- paste(Output_file,"/Diff-",regressee_name,"-",regressor_name,sep="");
##		if (!file.exists(Output_file_outsample)) {
##			dir.create(Output_file_outsample);
##		}
##		tryCatch(
##				{
##					Out_of_Sample_Test(Output_file_outsample,data);
##				}, error = function(e) {
##					print(e);
##					print(BU);
##					print(Expense_Items_rate_regression[j]);
##				}, finally = {}
##		)
#	}
#}

#Difference Model with 2 variables
Variable_Selection_Diff_2variable <- function(threshold=0.1,Output_file,revenue,.Macro) {
	#threshold is a number. 0.05 or 0.1
	#output folder to write results to
	#revenue has two columes. first column is the date. second are revenue numbers.
	#Macro is the combination of all independent variables
#	Output_file <- Output_file_regression_item2
#	.Macro <- Macro
	#	i <- 1;
	
	Model_Fitting_Results <- data.frame(matrix(NA,ncol=19));
	
	revenue_ts <- revenue_to_ts(revenue);
	regressee_name <- colnames(revenue)[2];
	
	.all_variables <- colnames(.Macro)[2:ncol(.Macro)];
	combinations <- t(combn(.all_variables,2));	
	
	for (i in 1:nrow(combinations)) {
#		i <- 1;
		.model_fitting_results <- rep(NA,19);
		
		regressor_name <- combinations[i,];
		##do plot of trend and growth rates
		regressor <- .Macro[.Macro[,1]%in%revenue[,1],c(1,which(colnames(.Macro)%in%regressor_name))];
		
		####run regression on difference		
		#transform data difference
		revenue_diff <- level_to_diff(revenue);
#		revenue_ts_diff <- revenue_to_ts(revenue_diff);
		regressor_diff <- level_to_diff(regressor);
		
		data <- cbind(revenue_diff,regressor_diff)[,-3];
		data <- clean_na_data_frame(data)
#		adf1 <- round(adf.test(na.omit(data[,2]),k=0)$p.value,digit=2);
#		adf2 <- round(adf.test(na.omit(data[,3]),k=0)$p.value,digit=2);
		fit <- fit_regression(data);
		#		fit_0 <- fit_regression_no_intercept(data);
		
		.coef <- summary(fit)$coefficients[,1];
		.model_fitting_results[1:4] <- c(.coef,rep(NA,4-length(.coef)));
		.model_fitting_results[5] <- as.character(data[1,1]);
		.model_fitting_results[6] <- as.character(data[nrow(data),1]);
		.model_fitting_results[7] <- summary(fit)$adj.r.squared;
		.model_fitting_results[8] <- all(summary(fit)$coefficients[-1,4]<0.05);
		
		#ADD Stationary test
		unit_root_test <- c();
		for (k in 2:ncol(data)) {
			unit_root_test <- c(unit_root_test, Stationary_test(data[,k]));
		}
		unit_root_test <- c(unit_root_test, Stationary_test(fit$residuals));
		#if all p values less than threshold, pass.
		if (all(unit_root_test<=threshold)) {
			.model_fitting_results[9] <- 1;
		} else {
			.model_fitting_results[9] <- 0;
		}
		
		.model_fitting_results[17:18] <- colnames(data)[3:4];
		Model_Fitting_Results <- rbind(Model_Fitting_Results,.model_fitting_results);	
	}
	
	Model_Fitting_Results <- Model_Fitting_Results[!is.na(Model_Fitting_Results[,1]),];
	colnames(Model_Fitting_Results) <-c("Intercept","Variables1","Variable2","Variable3","start_history","end_history","R^2","Significance of regressors","Engle-Granger stationarity test", "Johansen Stationary Test","max(VIF)","Breusch-Pagan","Durbin-Whatson","Breusch-Godfrey","Shapiro-Wilk","Kolmogorov-Smirnov");
	file.name <- paste(Output_file,"/Model_Fitting_Results_2Varaibles.csv",sep="");
	write.csv(Model_Fitting_Results,file.name);
	
	Model_Fitting_Results_Good <- Model_Fitting_Results;
	.stationary <- rep(FALSE,nrow(Model_Fitting_Results_Good));
	.stationary[Model_Fitting_Results_Good[,9]==0.5|Model_Fitting_Results_Good[,9]==1] <- TRUE;
	
	Model_Fitting_Results_Good <- Model_Fitting_Results_Good[Model_Fitting_Results_Good[,8]=="TRUE"&.stationary,];
	Model_Fitting_Results_Good <- Model_Fitting_Results_Good[order(Model_Fitting_Results_Good[,7],decreasing=TRUE),];
	
	file.name <- paste(Output_file,"/Model_Fitting_Results_Good_2Varaibles.csv",sep="")
	write.csv(Model_Fitting_Results_Good,file.name);
}

#Difference Model with 2 variables
Variable_Selection_Diff_2variable_Test1 <- function(num_models_to_examine,Output_file,revenue,Macro,Macro_2,Macro_3,Macro_4,BU) {
	#threshold is a number. 0.05 or 0.1
	#output folder to write results to
	#revenue has two columes. first column is the date. second are revenue numbers.
	#Macro is the combination of all independent variables
#	Output_file <- Output_file_regression_item2
#	.Macro <- Macro
	#	i <- 1;
	file_name <- paste(Output_file,"/Model_Fitting_Results_Good_2Varaibles.csv",sep="")
	best_file <- read.csv(file_name);
	if (nrow(best_file) == 0) {
		file_name <- paste(Output_file,"/Model_Fitting_Results_Good_2Varaibles.png",sep="");
		best_file_tex <- data.frame(Warning=c("No Model Pass Pivot Statistical Test",Output_file));
		png(file_name,height=35*nrow(best_file_tex),width=800);
		grid.table(best_file_tex);
		dev.off();
		return();
	}
	best_file <- best_file[,-1];
	
	if (nrow(best_file)>num_models_to_examine) {
		best_max <- num_models_to_examine;
	} else {
		best_max <- nrow(best_file);
	}
	revenue_name <- colnames(revenue)[2];
	
	revenue_ts <- revenue_to_ts(revenue);
	regressee_name <- colnames(revenue)[2];
	
	for (i in 1:best_max) {
#		i <- 19;
		regressor_name <- as.character(unlist(best_file[i,17:18]));
		##do plot of trend and growth rates
		regressor <- Macro[,c(1,which(colnames(Macro)%in%regressor_name))];
		regressor <- regressor[regressor[,1]%in%revenue[,1],];
		
		####run regression on difference		
		#transform data difference
		revenue_diff <- level_to_diff(revenue);
		revenue_ts_diff <- revenue_to_ts(revenue_diff);
		regressor_diff <- level_to_diff(regressor);
		
		data <- cbind(revenue_diff,regressor_diff)[,-3];
#		adf1 <- round(adf.test(na.omit(data[,2]),k=0)$p.value,digit=2);
#		adf2 <- round(adf.test(na.omit(data[,3]),k=0)$p.value,digit=2);
		fit <- fit_regression(data);
		#		fit_0 <- fit_regression_no_intercept(data);
		start_and_frequency <- determine_frequency_and_start(data[,1]);
		.frequency <- start_and_frequency$frequency;
		.start <- start_and_frequency$start;
		Backtesting <- ts(predict(fit,newdata=data),start=.start,frequency=.frequency);
		
		Historical_metric <- ts(data[,2],start=.start,frequency=.frequency);
		
		#########
		#Forecast baseline
		.forecast_data_baseline <- compile_forecast_data_diff(Macro,regressor_name,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data_baseline[,1]);
		Forecast_baseline <- ts(predict(fit,.forecast_data_baseline),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		#Forecast advser
		.forecast_data_adverse <- compile_forecast_data_diff(Macro_2,regressor_name,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data_adverse[,1]);
		Forecast_adverse <- ts(predict(fit,.forecast_data_adverse),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		#Forecast severe
		.forecast_data_severe <- compile_forecast_data_diff(Macro_3,regressor_name,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data_severe[,1]);
		Forecast_severe <- ts(predict(fit,.forecast_data_severe),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		#Forecast bhc
		.forecast_data_bhc <- compile_forecast_data_diff(Macro_4,regressor_name,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data_bhc[,1]);
		Forecast_bhc <- ts(predict(fit,.forecast_data_bhc),start=start_and_frequency$start,frequency=start_and_frequency$frequency);

		#backtesting
#		plot_name <- paste(Output_file,"/Diff-Model-backtesting-",BU,"-",regressee_name,"-",paste(regressor_name,collapse="-"),".png",sep="");
		plot_name <- paste(Output_file,"/Diff-Model-backtesting-",BU,"-",i,".png",sep="");
		png(plot_name,width=800,height=600);
		ts.plot(Historical_metric,Backtesting,Forecast_baseline,Forecast_adverse,Forecast_severe,Forecast_bhc,col=c(Color_history,Color_backtest,Color_baseline,Color_adverse,Color_severe,Color_bhc),type="b",ylab="Metric",sub=formu_display(data,fit));
		title(paste("Backtest and Forecast ",BU,"/",regressee_name," V.S. ",paste(regressor_name,collapse="-")," /R^2=",round(summary(fit)$r.squared,2),sep=""));
		legend("topleft",legend=c("Actual","Backtesting"),pch=1,col=c(Color_history, Color_backtest));
		dev.off();
		
		#backting to level value
		#Convert back to levels
		actual_level <- revenue_to_ts(revenue);
		
		###change
		tmp <- determine_frequency_and_start_DiffBacktest(revenue,data);
		start_value <- tmp$value;		
		.start <- tmp$start;
		increments <- na.omit(as.vector(Backtesting));
		Backtesting_level <- diff_to_level(start_value,increments,.start,.frequency);
		
		start_value <- revenue[nrow(revenue),2];
		increments <- as.vector(Forecast_baseline);
		.start <- start(Forecast_baseline);
		.frequency <- frequency(Forecast_baseline);
		Forecast_baseline_level <- diff_to_level(start_value,increments,.start,.frequency,remove_first=TRUE);
		
		start_value <- revenue[nrow(revenue),2];
		increments <- as.vector(Forecast_adverse);
		.start <- start(Forecast_adverse);
		.frequency <- frequency(Forecast_adverse);
		Forecast_adverse_level <- diff_to_level(start_value,increments,.start,.frequency,remove_first=TRUE);
		
		start_value <- revenue[nrow(revenue),2];
		increments <- as.vector(Forecast_severe);
		.start <- start(Forecast_severe);
		.frequency <- frequency(Forecast_severe);
		Forecast_severe_level <- diff_to_level(start_value,increments,.start,.frequency,remove_first=TRUE);
		
		start_value <- revenue[nrow(revenue),2];
		increments <- as.vector(Forecast_bhc);
		.start <- start(Forecast_bhc);
		.frequency <- frequency(Forecast_bhc);
		Forecast_bhc_level <- diff_to_level(start_value,increments,.start,.frequency,remove_first=TRUE);
		
		plot_name <- paste(Output_file,"/Diff-Model-backtesting-level-",BU,"-",i,".png",sep="");
		png(plot_name,width=800,height=600);
		ts.plot(actual_level,Backtesting_level,Forecast_baseline_level,Forecast_adverse_level,Forecast_severe_level,Forecast_bhc_level,col=c(Color_history,Color_backtest,Color_baseline,Color_adverse,Color_severe,Color_bhc),type="b",sub=formu_display(revenue,fit))
		title(paste("Backtest Level Value ",BU,"/",regressee_name," V.S. ",paste(regressor_name,collapse="/")," /R^2=",round(summary(fit)$r.squared,2),sep=""));
		legend("topleft",legend=c("Hisotry","Backtesting"),pch=1,col=c(Color_history, Color_backtest));
		dev.off();

		
#		#out sample testing
#		Output_file_outsample <- paste(Output_file,"/Diff-",regressee_name,"-",paste(regressor_name,collapse="-"),sep="");
#		if (!file.exists(Output_file_outsample)) {
#			dir.create(Output_file_outsample);
#		}
#		tryCatch(
#				{
#					Out_of_Sample_Test(Output_file_outsample,data);
#				}, error = function(e) {
#					print(e);
#					print(BU);
#					print(Expense_Items_rate_regression[j]);
#				}, finally = {}
#		)
	}
	
	#output to picture to be included in Latex
	best_file_tex <- best_file[1:best_max,];
	for (j in 1:3) {
		best_file_tex[,j] <- format_level_value(best_file_tex[,j]);
	}
	best_file_tex$R.2 <- round(best_file_tex$R.2,digits=2);
	best_file_tex <- best_file_tex[,c(1,2,3,7,17,18)];
	colnames(best_file_tex) <- c("Intercept", "Variables1", "Variable2", "R-square", "V1-Name","V2-Name")
	file_name <- paste(Output_file,"/Model_Fitting_Results_Good_2Varaibles.png",sep="");
	png(file_name,height=30*nrow(best_file_tex),width=800);
	grid.table(best_file_tex);
	dev.off();
	
}

Variable_Selection_Diff_3variable <- function(threshold=0.1,Output_file,revenue,.Macro) {
	#threshold is a number. 0.05 or 0.1
	#output folder to write results to
	#revenue has two columes. first column is the date. second are revenue numbers.
	#Macro is the combination of all independent variables
#	threshold,Output_file_regression_item1,revenue,Macro_Q
#	Output_file <- Output_file_regression_item3
#	.Macro <- .Macro
	#	i <- 1;
	
	Model_Fitting_Results <- data.frame(matrix(NA,ncol=19));
	
	revenue_ts <- revenue_to_ts(revenue);
	regressee_name <- colnames(revenue)[2];
	
	.all_variables <- colnames(.Macro)[2:ncol(.Macro)];
	combinations <- t(combn(.all_variables,3));	
	
	for (i in 1:nrow(combinations)) {
#		i <- 1;
		.model_fitting_results <- rep(NA,19);
		
		regressor_name <- combinations[i,];
		##do plot of trend and growth rates
		regressor <- .Macro[.Macro[,1]%in%revenue[,1],c(1,which(colnames(.Macro)%in%regressor_name))];
		
		####run regression on difference		
		#transform data difference
		revenue_diff <- level_to_diff(revenue);
#		revenue_ts_diff <- revenue_to_ts(revenue_diff);
		regressor_diff <- level_to_diff(regressor);
		
		data <- cbind(revenue_diff,regressor_diff)[,-3];
		data <- clean_na_data_frame(data)
#		adf1 <- round(adf.test(na.omit(data[,2]),k=0)$p.value,digit=2);
#		adf2 <- round(adf.test(na.omit(data[,3]),k=0)$p.value,digit=2);
		fit <- fit_regression(data);
		#		fit_0 <- fit_regression_no_intercept(data);
		
		.coef <- summary(fit)$coefficients[,1];
		.model_fitting_results[1:4] <- c(.coef,rep(NA,4-length(.coef)));
		.model_fitting_results[5] <- as.character(data[1,1]);
		.model_fitting_results[6] <- as.character(data[nrow(data),1]);
		.model_fitting_results[7] <- summary(fit)$adj.r.squared;
		.model_fitting_results[8] <- all(summary(fit)$coefficients[-1,4]<0.05);
		if (.model_fitting_results[8]) {
			#ADD Stationary test
			unit_root_test <- c();
			for (k in 2:ncol(data)) {
				unit_root_test <- c(unit_root_test, Stationary_test(data[,k]));
			}
			unit_root_test <- c(unit_root_test, Stationary_test(fit$residuals));
			#if all p values less than threshold, pass.
			if (all(unit_root_test<=threshold)) {
				.model_fitting_results[9] <- 1;
			} else {
				.model_fitting_results[9] <- 0;
			}
		} else {
			.model_fitting_results[9] <- 0;
		}
		
		.model_fitting_results[17:19] <- colnames(data)[3:5];
		Model_Fitting_Results <- rbind(Model_Fitting_Results,.model_fitting_results);	
	}
	
	Model_Fitting_Results <- Model_Fitting_Results[!is.na(Model_Fitting_Results[,1]),];
	colnames(Model_Fitting_Results) <-c("Intercept","Variables1","Variable2","Variable3","start_history","end_history","R^2","Significance of regressors","Engle-Granger stationarity test", "Johansen Stationary Test","max(VIF)","Breusch-Pagan","Durbin-Whatson","Breusch-Godfrey","Shapiro-Wilk","Kolmogorov-Smirnov");
	file.name <- paste(Output_file,"/Model_Fitting_Results_3Varaibles.csv",sep="");
	write.csv(Model_Fitting_Results,file.name);
	
	Model_Fitting_Results_Good <- Model_Fitting_Results;
	.stationary <- rep(FALSE,nrow(Model_Fitting_Results_Good));
	.stationary[Model_Fitting_Results_Good[,9]==0.5|Model_Fitting_Results_Good[,9]==1] <- TRUE;
	
	Model_Fitting_Results_Good <- Model_Fitting_Results_Good[Model_Fitting_Results_Good[,8]=="TRUE"&.stationary,];
	Model_Fitting_Results_Good <- Model_Fitting_Results_Good[order(Model_Fitting_Results_Good[,7],decreasing=TRUE),];
	
	file.name <- paste(Output_file,"/Model_Fitting_Results_Good_3Varaibles.csv",sep="")
	write.csv(Model_Fitting_Results_Good,file.name);
}

#Difference Model with 2 variables
Variable_Selection_Diff_3variable_Test1 <- function(num_models_to_examine,Output_file,revenue,Macro,Macro_2,Macro_3,Macro_4,BU) {
	#threshold is a number. 0.05 or 0.1
	#output folder to write results to
	#revenue has two columes. first column is the date. second are revenue numbers.
	#Macro is the combination of all independent variables
#	Output_file <- Output_file_regression_item3
#	.Macro <- Macro
	#	i <- 1;
	file_name <- paste(Output_file,"/Model_Fitting_Results_Good_3Varaibles.csv",sep="")
	best_file <- read.csv(file_name);
	if (nrow(best_file) == 0) {
		file_name <- paste(Output_file,"/Model_Fitting_Results_Good_3Varaibles.png",sep="");
		best_file_tex <- data.frame(Warning=c("No Model Pass Pivot Statistical Test",Output_file));
		png(file_name,height=35*nrow(best_file_tex),width=800);
		grid.table(best_file_tex);
		dev.off();
		return();
	}
	best_file <- best_file[,-1];
	
	if (nrow(best_file)>num_models_to_examine) {
		best_max <- num_models_to_examine;
	} else {
		best_max <- nrow(best_file);
	}
	revenue_name <- colnames(revenue)[2];
	
	revenue_ts <- revenue_to_ts(revenue);
	regressee_name <- colnames(revenue)[2];
	
	for (i in 1:best_max) {
#		i <- 1;
		regressor_name <- as.character(unlist(best_file[i,17:19]));
		##do plot of trend and growth rates
		regressor <- Macro[,c(1,which(colnames(Macro)%in%regressor_name))];
		regressor <- regressor[regressor[,1]%in%revenue[,1],];
		
		####run regression on difference		
		#transform data difference
		revenue_diff <- level_to_diff(revenue);
		revenue_ts_diff <- revenue_to_ts(revenue_diff);
		regressor_diff <- level_to_diff(regressor);
		
		data <- cbind(revenue_diff,regressor_diff)[,-3];
#		adf1 <- round(adf.test(na.omit(data[,2]),k=0)$p.value,digit=2);
#		adf2 <- round(adf.test(na.omit(data[,3]),k=0)$p.value,digit=2);
		fit <- fit_regression(data);
		#		fit_0 <- fit_regression_no_intercept(data);
		start_and_frequency <- determine_frequency_and_start(data[,1]);
		.frequency <- start_and_frequency$frequency;
		.start <- start_and_frequency$start;
		Backtesting <- ts(predict(fit,newdata=data),start=.start,frequency=.frequency);
		
		Historical_metric <- ts(data[,2],start=.start,frequency=.frequency);
		
		#########
		#Forecast baseline
		.forecast_data_baseline <- compile_forecast_data_diff(Macro,regressor_name,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data_baseline[,1]);
		Forecast_baseline <- ts(predict(fit,.forecast_data_baseline),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		#Forecast advser
		.forecast_data_adverse <- compile_forecast_data_diff(Macro_2,regressor_name,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data_adverse[,1]);
		Forecast_adverse <- ts(predict(fit,.forecast_data_adverse),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		#Forecast severe
		.forecast_data_severe <- compile_forecast_data_diff(Macro_3,regressor_name,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data_severe[,1]);
		Forecast_severe <- ts(predict(fit,.forecast_data_severe),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		#Forecast bhc
		.forecast_data_bhc <- compile_forecast_data_diff(Macro_4,regressor_name,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data_bhc[,1]);
		Forecast_bhc <- ts(predict(fit,.forecast_data_bhc),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		#backtesting
#		plot_name <- paste(Output_file,"/Diff-Model-backtesting-",BU,"-",regressee_name,"-",paste(regressor_name,collapse="-"),".png",sep="");
		plot_name <- paste(Output_file,"/Diff-Model-backtesting-",BU,"-",i,".png",sep="");
		png(plot_name,width=800,height=600);
		ts.plot(Historical_metric,Backtesting,Forecast_baseline,Forecast_adverse,Forecast_severe,Forecast_bhc,col=c(Color_history,Color_backtest,Color_baseline,Color_adverse,Color_severe,Color_bhc),type="b",ylab="Metric",sub=formu_display(data,fit));
		title(paste("Backtest and Forecast ",BU,"/",regressee_name," V.S. ",paste(regressor_name,collapse="-")," /R^2=",round(summary(fit)$r.squared,2),sep=""));
		legend("topleft",legend=c("Actual","Backtesting"),pch=1,col=c(Color_history, Color_backtest));
		dev.off();
		
		#backting to level value
		#Convert back to levels
		actual_level <- revenue_to_ts(revenue);
		
		###change
		tmp <- determine_frequency_and_start_DiffBacktest(revenue,data);
		start_value <- tmp$value;		
		.start <- tmp$start;
		increments <- na.omit(as.vector(Backtesting));
		Backtesting_level <- diff_to_level(start_value,increments,.start,.frequency);
		
		start_value <- revenue[nrow(revenue),2];
		increments <- as.vector(Forecast_baseline);
		.start <- start(Forecast_baseline);
		.frequency <- frequency(Forecast_baseline);
		Forecast_baseline_level <- diff_to_level(start_value,increments,.start,.frequency,remove_first=TRUE);
		
		start_value <- revenue[nrow(revenue),2];
		increments <- as.vector(Forecast_adverse);
		.start <- start(Forecast_adverse);
		.frequency <- frequency(Forecast_adverse);
		Forecast_adverse_level <- diff_to_level(start_value,increments,.start,.frequency,remove_first=TRUE);
		
		start_value <- revenue[nrow(revenue),2];
		increments <- as.vector(Forecast_severe);
		.start <- start(Forecast_severe);
		.frequency <- frequency(Forecast_severe);
		Forecast_severe_level <- diff_to_level(start_value,increments,.start,.frequency,remove_first=TRUE);
		
		start_value <- revenue[nrow(revenue),2];
		increments <- as.vector(Forecast_bhc);
		.start <- start(Forecast_bhc);
		.frequency <- frequency(Forecast_bhc);
		Forecast_bhc_level <- diff_to_level(start_value,increments,.start,.frequency,remove_first=TRUE);
		
		plot_name <- paste(Output_file,"/Diff-Model-backtesting-level-",BU,"-",i,".png",sep="");
		png(plot_name,width=800,height=600);
		ts.plot(actual_level,Backtesting_level,Forecast_baseline_level,Forecast_adverse_level,Forecast_severe_level,Forecast_bhc_level,col=c(Color_history,Color_backtest,Color_baseline,Color_adverse,Color_severe,Color_bhc),type="b",sub=formu_display(revenue,fit))
		title(paste("Backtest Level Value ",BU,"/",regressee_name," V.S. ",paste(regressor_name,collapse="/")," /R^2=",round(summary(fit)$r.squared,2),sep=""));
		legend("topleft",legend=c("Hisotry","Backtesting"),pch=1,col=c(Color_history, Color_backtest));
		dev.off();
		
		
#		#out sample testing
#		Output_file_outsample <- paste(Output_file,"/Diff-",regressee_name,"-",paste(regressor_name,collapse="-"),sep="");
#		if (!file.exists(Output_file_outsample)) {
#			dir.create(Output_file_outsample);
#		}
#		tryCatch(
#				{
#					Out_of_Sample_Test(Output_file_outsample,data);
#				}, error = function(e) {
#					print(e);
#					print(BU);
#					print(Expense_Items_rate_regression[j]);
#				}, finally = {}
#		)
	}
	
	#output to picture to be included in Latex
	best_file_tex <- best_file[1:best_max,];
	for (j in 1:3) {
		best_file_tex[,j] <- format_level_value(best_file_tex[,j]);
	}
	best_file_tex$R.2 <- round(best_file_tex$R.2,digits=2);
	best_file_tex <- best_file_tex[,c(1,2,3,7,17,18,19)];
	colnames(best_file_tex) <- c("Intercept", "Variables1", "Variable2", "R-square", "V1-Name","V2-Name","V3-Name")
	file_name <- paste(Output_file,"/Model_Fitting_Results_Good_2Varaibles.png",sep="");
	png(file_name,height=30*nrow(best_file_tex),width=1000);
	grid.table(best_file_tex);
	dev.off();
	
}



####advanced functions depending on other functions
Variable_Selection_2 <- function(threshold=0.1,Output_file,revenue,Macro) {
	#threshold is a number. 0.05 or 0.1
	#output folder to write results to
	#revenue has two columes. first column is the date. second are revenue numbers.
	#Macro is the combination of all independent variables
#	revenue <- revenue_Q
#	Macro <- Macro_Q
	#	i <- 1;
	
	Model_Fitting_Results <- data.frame(matrix(NA,ncol=19));
	
	.all_variables <- colnames(Macro)[2:ncol(Macro)];
	combinations <- t(combn(.all_variables,2));	
	.select_row <- Macro[,1]%in%revenue[,1];
	for (j in 1:nrow(combinations)) {
		# j <- 1;
		.model_fitting_results <- rep(NA,19);
		
		.variables <- combinations[j,];
		.select_colume <- colnames(Macro)%in%.variables;
		data <- cbind(revenue,Macro[.select_row,.select_colume]);
		data <- data[!is.na(data[,2])&!is.na(data[,3])&!is.na(data[,4]),];#changes made on 5/2/16
		if (sd(data[,3])==0|sd(data[,4])==0) {
			next();
		}
		#regression
		formu <- paste(colnames(data)[2], "~", colnames(data)[3]);
		if (ncol(data)>3) {
			for (k in 4:ncol(data)) {
				formu <- paste(formu,"+",colnames(data)[k]);
			}
		}
		fit <- lm(as.formula(formu),data=data);
		
		.coef <- summary(fit)$coefficients[,1]
		.model_fitting_results[1:4] <- c(.coef,rep(NA,4-length(.coef)));
		.model_fitting_results[5] <- as.character(data[1,1]);
		.model_fitting_results[6] <- as.character(data[nrow(data),1]);
		.model_fitting_results[7] <- summary(fit)$adj.r.squared;
		.model_fitting_results[8] <- all(summary(fit)$coefficients[-1,4]<0.05);
		if (.model_fitting_results[8]) {
		#Test3: residuals
		#Stationary Test
			unit_root_residual <- Stationary_test(fit$residuals);
			if (unit_root_residual<= threshold) {
				.model_fitting_results[9] <- 1;
			} else {
				.model_fitting_results[9] <- 0;
			}
	
			tryCatch(
					{
						coint <- ca.jo(data[,2:ncol(data)],type="trace",K=2,ecdet="none",spec="longrun");
						.model_fitting_results[10] <- any(coint@teststat[2] > coint@cval[2,]);
					}, error = function(e) {
						print(e);
					}, finally = {}
			)
			
			#Test2: collinearity
			#VIF test
			.tmp.vif <- vif(fit);
			.model_fitting_results[11] <- max(.tmp.vif);
			
			.tmp.bp <- bptest(fit);
			.model_fitting_results[12] <- .tmp.bp$p.value;
			
			.tmp.dw <- dwtest(fit);
			.model_fitting_results[13] <- .tmp.dw$p.value;
			
			.tmp.bg <- bgtest(fit,order=2);
			.model_fitting_results[14] <- .tmp.bg$p.value;
			
			#Normality of residuals
			#Wilk-Shapiro test
			.tmp.ws <- shapiro.test(fit$residuals);
			.model_fitting_results[15] <- .tmp.ws$p.value;
			.tmp.ks <- ks.test(fit$residuals,"pnorm",mean(fit$residuals), sd(fit$residuals));
			.model_fitting_results[16] <- .tmp.ks$p.value;
		} else {
			.model_fitting_results[9:16] <- -1;
		}
		#write variable names
		.model_fitting_results[17:18] <- colnames(data)[3:4];
		
		#write test results to Model_Fitting_Results data frame;
		Model_Fitting_Results <- rbind(Model_Fitting_Results,.model_fitting_results);
	}
	Model_Fitting_Results <- Model_Fitting_Results[!is.na(Model_Fitting_Results[,1]),];
	colnames(Model_Fitting_Results) <-c("Intercept","Variables1","Variable2","Variable3","start_history","end_history","R^2","Significance of regressors","Engle-Granger stationarity test", "Johansen Stationary Test","max(VIF)","Breusch-Pagan","Durbin-Whatson","Breusch-Godfrey","Shapiro-Wilk","Kolmogorov-Smirnov");
	file.name <- paste(Output_file,"/Model_Fitting_Results_2Varaibles.csv",sep="");
	write.csv(Model_Fitting_Results,file.name);
	
	Model_Fitting_Results[,11] <- as.numeric(Model_Fitting_Results[,11])<10;
	for (j in c(12:16)) {
		Model_Fitting_Results[,j] <- as.numeric(Model_Fitting_Results[,j])>0.05;
	}
	
	Model_Fitting_Results_Good <- Model_Fitting_Results;
	.stationary <- rep(FALSE,nrow(Model_Fitting_Results_Good));
	.stationary[Model_Fitting_Results_Good[,9]==0.5|Model_Fitting_Results_Good[,9]==1] <- TRUE;
	.stationary[!is.na(Model_Fitting_Results_Good[,10])&Model_Fitting_Results_Good[,10]==TRUE] <- TRUE;
	
	Model_Fitting_Results_Good <- Model_Fitting_Results_Good[Model_Fitting_Results_Good[,8]=="TRUE"&.stationary,];
	Model_Fitting_Results_Good <- Model_Fitting_Results_Good[order(Model_Fitting_Results_Good[,7],decreasing=TRUE),];
	
	file.name <- paste(Output_file,"/Model_Fitting_Results_Good_2Varaibles.csv",sep="")
	write.csv(Model_Fitting_Results_Good,file.name);
	
	Model_Fitting_Results_Best <- Model_Fitting_Results_Good;
	select <- apply(Model_Fitting_Results_Best[,11:16],1,sum);		
	Model_Fitting_Results_Best <- Model_Fitting_Results_Best[select==max(select),];
	
	file.name <- paste(Output_file,"/Model_Fitting_Results_Best_2Varaibles.csv",sep="")
	write.csv(Model_Fitting_Results_Best,file.name);
	
}

Variable_Selection_3 <- function(threshold=0.1,Output_file,revenue,Macro) {
	#threshold is a number. 0.05 or 0.1
	#output folder to write results to
	#revenue has two columes. first column is the date. second are revenue numbers.
	#Macro is the combination of all independent variables
	
	Model_Fitting_Results <- data.frame(matrix(NA,ncol=19));
	
	.all_variables <- colnames(Macro)[2:ncol(Macro)];
	combinations <- t(combn(.all_variables,3));	
	.select_row <- Macro[,1]%in%revenue[,1];
	for (j in 1:nrow(combinations)) {
		
		.model_fitting_results <- rep(NA,19);
		
		.variables <- combinations[j,]
		.select_colume <- colnames(Macro)%in%.variables;
		data <- cbind(revenue,Macro[.select_row,.select_colume]);
		data <- data[!is.na(data[,2])&!is.na(data[,3])&!is.na(data[,4]),]; #changes made on 5/2/16
		
		#regression
		formu <- paste(colnames(data)[2], "~", colnames(data)[3]);
		if (ncol(data)>3) {
			for (k in 4:ncol(data)) {
				formu <- paste(formu,"+",colnames(data)[k]);
			}
		}
		fit <- lm(as.formula(formu),data=data);
		
		.coef <- summary(fit)$coefficients[,1]
		.model_fitting_results[1:4] <- c(.coef,rep(NA,4-length(.coef)));
		.model_fitting_results[5] <- as.character(data[1,1]);
		.model_fitting_results[6] <- as.character(data[nrow(data),1]);
		.model_fitting_results[7] <- summary(fit)$adj.r.squared;
		.model_fitting_results[8] <- all(summary(fit)$coefficients[-1,4]<0.05);
		
		#Test3: residuals
		#Stationary Test
#		unit_root_test <- c();
#		for (k in 2:ncol(data)) {
#			unit_root_test <- c(unit_root_test, adf.test(data[,k])$p.value);
#		}
#		
#		if (all(unit_root_test<=threshold)) {
#			.model_fitting_results[9] <- 1;
#		} else if (all(unit_root_test>threshold)) {
#			unit_root_residual <- adf.test(fit$residuals)$p.value;
#			unit_root_test <- c(unit_root_test, unit_root_residual);
#			if (unit_root_residual<= threshold) {
#				.model_fitting_results[9] <- 0.5;
#			} else {
#				.model_fitting_results[9] <- 0;
#			}
#		} else {
#			.model_fitting_results[9] <- 0;
#		}

		unit_root_residual <- Stationary_test(fit$residuals);
		if (unit_root_residual<= threshold) {
			.model_fitting_results[9] <- 1;
		} else {
			.model_fitting_results[9] <- 0;
		}
		
		tryCatch(
				{
					coint <- ca.jo(data[,3:ncol(data)],type="trace",K=2,ecdet="none",spec="longrun");
					.model_fitting_results[10] <- any(coint@teststat[2] > coint@cval[2,]);
				}, error = function(e) {
					print(e);
				}, finally = {}
		)
		
		#Test2: collinearity
		#VIF test
		.tmp.vif <- vif(fit);
		.model_fitting_results[11] <- max(.tmp.vif);
		
		.tmp.bp <- bptest(fit);
		.model_fitting_results[12] <- .tmp.bp$p.value;
		
		.tmp.dw <- dwtest(fit);
		.model_fitting_results[13] <- .tmp.dw$p.value;
		
		.tmp.bg <- bgtest(fit,order=2);
		.model_fitting_results[14] <- .tmp.bg$p.value;
		
		#Normality of residuals
		#Wilk-Shapiro test
		.tmp.ws <- shapiro.test(fit$residuals);
		.model_fitting_results[15] <- .tmp.ws$p.value;
		.tmp.ks <- ks.test(fit$residuals,"pnorm",mean(fit$residuals), sd(fit$residuals));
		.model_fitting_results[16] <- .tmp.ks$p.value;
		
		#write variable names
		.model_fitting_results[17:19] <- colnames(data)[3:5];
		
		#write test results to Model_Fitting_Results data frame;
		Model_Fitting_Results <- rbind(Model_Fitting_Results,.model_fitting_results);
	}
	Model_Fitting_Results <- Model_Fitting_Results[!is.na(Model_Fitting_Results[,1]),];
	colnames(Model_Fitting_Results) <-c("Intercept","Variables1","Variable2","Variable3","start_history","end_history","R^2","Significance of regressors","Engle-Granger stationarity test", "Johansen Stationary Test","max(VIF)","Breusch-Pagan","Durbin-Whatson","Breusch-Godfrey","Shapiro-Wilk","Kolmogorov-Smirnov");
	file.name <- paste(Output_file,"/Model_Fitting_Results_3Varaibles.csv",sep="");
	write.csv(Model_Fitting_Results,file.name);
	
	Model_Fitting_Results[,11] <- as.numeric(Model_Fitting_Results[,11])<10;
	for (j in c(12:16)) {
		Model_Fitting_Results[,j] <- as.numeric(Model_Fitting_Results[,j])>0.05;
	}
	
	Model_Fitting_Results_Good <- Model_Fitting_Results;
	.stationary <- rep(FALSE,nrow(Model_Fitting_Results_Good));
	.stationary[Model_Fitting_Results_Good[,9]==0.5|Model_Fitting_Results_Good[,9]==1] <- TRUE;
	.stationary[!is.na(Model_Fitting_Results_Good[,10])&Model_Fitting_Results_Good[,10]==TRUE] <- TRUE;
	
	Model_Fitting_Results_Good <- Model_Fitting_Results_Good[Model_Fitting_Results_Good[,8]=="TRUE"&.stationary,];
	Model_Fitting_Results_Good <- Model_Fitting_Results_Good[order(Model_Fitting_Results_Good[,7],decreasing=TRUE),];
	
	file.name <- paste(Output_file,"/Model_Fitting_Results_Good_3Varaibles.csv",sep="")
	write.csv(Model_Fitting_Results_Good,file.name);
	
	Model_Fitting_Results_Best <- Model_Fitting_Results_Good;
	select <- apply(Model_Fitting_Results_Best[,11:16],1,sum);		
	Model_Fitting_Results_Best <- Model_Fitting_Results_Best[select==max(select),];
	
	file.name <- paste(Output_file,"/Model_Fitting_Results_Best_3Varaibles.csv",sep="")
	write.csv(Model_Fitting_Results_Best,file.name);	
}

Variable_Selection_1variable_Test1 <- function(num_models_to_examine,Output_file,revenue,Macro,Macro_2,Macro_3,Macro_4,BU) {
	
#	revenue <- revenue_Q;
#	Macro <- Macro_Q;
#	Macro_2 <- Macro_2_Q;
#	Macro_3 <- Macro_3_Q;
#	Macro_4 <- Macro_4_Q;
#	Output_file <- Output_file_regression_item1
#	num_models_to_examine,Output_file_regression_item1,revenue,Macro,Macro_2,Macro_3,Macro_4,BU
	file_name <- paste(Output_file,"/Model_Fitting_Results_Good_2Varaibles.csv",sep="");
	best_file <- read.csv(file_name);
	if (nrow(best_file) == 0) {
		file_name <- paste(Output_file,"/Model_Fitting_Results_Good_2Varaibles.png",sep="");
		best_file_tex <- data.frame(Warning=c("No Model Pass Pivot Statistical Test",Output_file));
		png(file_name,height=35*nrow(best_file_tex),width=800);
		grid.table(best_file_tex);
		dev.off();
		return();
	}
	best_file <- best_file[,-1];
	
	if (nrow(best_file)>num_models_to_examine) {
		best_max <- num_models_to_examine;
	} else {
		best_max <- nrow(best_file);
	}
	revenue_name <- colnames(revenue)[2];
	revenue_name <- gsub("[.]","-",revenue_name); ###To replace . with -.
	
	for (best in 1:best_max) {
		#	best <- 6;	
		.variables <- as.character(unlist(best_file[best,17]));
		data <- compile_regression_data(Macro,.variables,revenue);
		
		#do the regression of primary model
		fit <- fit_regression(data);

		##do plotting
		start_and_frequency <- determine_frequency_and_start(data[,1]);
		.frequency <- start_and_frequency$frequency;
		.start <- start_and_frequency$start;
		
		#historical actual
		Historical_metric <- ts(data[,2],start=.start,frequency=.frequency);
		
		#historical backtesting
		Backtesting <- ts(fit$fitted.values,start=.start,frequency=.frequency);
		
		#Forecast baseline
		.forecast_data <- compile_forecast_data(Macro,.variables,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data[,1]);
		Forecast_baseline <- ts(predict(fit,.forecast_data),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		#Forecast advser
		.forecast_data <- compile_forecast_data(Macro_2,.variables,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data[,1]);
		Forecast_adverse <- ts(predict(fit,.forecast_data),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		#Forecast severe
		.forecast_data <- compile_forecast_data(Macro_3,.variables,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data[,1]);
		Forecast_severe <- ts(predict(fit,.forecast_data),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		#Forecast bhc
		.forecast_data <- compile_forecast_data(Macro_4,.variables,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data[,1]);
		Forecast_bhc <- ts(predict(fit,.forecast_data),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		plot_name <- paste(Output_file,"/Test-backtesting-and-forecast-",BU,"-",revenue_name,best,".png",sep="");
		png(plot_name,width=800,height=600);
		ts.plot(Historical_metric,Backtesting,Forecast_baseline,Forecast_adverse,Forecast_severe,Forecast_bhc,col=c(Color_history,Color_backtest,Color_baseline,Color_adverse,Color_severe,Color_bhc),type="b",ylab="Metric",sub=formu_display(data,fit));
		abline(v=as.numeric(as.yearmon(data[nrow(data),1])));
		title(paste("Backtest and Forecast ",BU,"/",revenue_name,"    R^2=",round(summary(fit)$r.squared,2),sep=""));
		legend("topleft",legend=c("history","backtest","baseline","adverse","severely adverse","BHC"),pch=1,col=c(Color_history,Color_backtest,Color_baseline,Color_adverse,Color_severe,Color_bhc));
		dev.off();
		
		Comparison <- compare_time_series(Historical_metric,Backtesting)$Comp_Percent
		plot_name <- paste(Output_file,"/RegressionBacktestTable",best,".png",sep="");
		png(plot_name,width=800,height=30*nrow(Comparison));
		grid.table(Comparison);
		dev.off();
		
		##Plot the Table for regreesion
		notation_baseline <- get_notation(merge_successive_ts(Historical_metric,Forecast_baseline),revenue_name)$Table;
		rownames(notation_baseline)[1] <- "Baseline";
		notation_adverse <- get_notation(merge_successive_ts(Historical_metric,Forecast_adverse),revenue_name)$Table;
		rownames(notation_adverse)[1] <- "Adverse";
		notation_severe <- get_notation(merge_successive_ts(Historical_metric,Forecast_severe),revenue_name)$Table;
		rownames(notation_severe)[1] <- "Severe";
		notation_bhc <- get_notation(merge_successive_ts(Historical_metric,Forecast_bhc),revenue_name)$Table;
		rownames(notation_bhc)[1] <- "BHC";
		
		Comparison <- rbind(notation_baseline,notation_adverse,notation_severe,notation_bhc);
		plot_name <- paste(Output_file,"/RegressionForecastTable",best,".png",sep="");
		png(plot_name,width=800,height=30*nrow(Comparison));
		grid.table(Comparison);
		dev.off();
		
#		Output_file_outsample <- paste(Output_file,"/",best,sep="");
#		if (!file.exists(Output_file_outsample)) {
#			dir.create(Output_file_outsample);
#		}
#		
##		file_name <- paste(Output_file_forecast_option,"/Test-backtesting and forecast-",BU,"-",revenue_name,best,".csv",sep="");
##		data_forecast <- ts.union(historical_metric,fitted_metric,fitted_metric_2,fitted_metric_3,fitted_metric_4);
##		write.csv(data_forecast,file_name);
#		tryCatch(
#		{
##		Out_of_Sample_Test(Output_file_outsample,data);
#			}, error = function(e) {
#				print(e);
#				print(BU);
#				print(Revenue_Items[j]);
#			}, finally = {}
#		)
	}
	
	#output to picture to be included in Latex
	best_file_tex <- best_file[1:best_max,];
	for (j in 1:2) {
		best_file_tex[,j] <- format_level_value(best_file_tex[,j]);
	}
	best_file_tex$R.2 <- round(best_file_tex$R.2,digits=2);
	best_file_tex <- best_file_tex[,c(1,2,7,17)];
	colnames(best_file_tex) <- c("Intercept", "Variables1", "R-square", "V1-Name")
	file_name <- paste(Output_file,"/Model_Fitting_Results_Good_2Varaibles.png",sep="");
	png(file_name,height=30*nrow(best_file_tex),width=800);
	grid.table(best_file_tex);
	dev.off();
}

Variable_Selection_2variable_Test1 <- function(num_models_to_examine,Output_file,revenue,Macro,Macro_2,Macro_3,Macro_4,BU) {
#	revenue <- revenue_Q;
#	Macro <- .Macro;
#	Macro_2 <- Macro_2_Q;
#	Macro_3 <- Macro_3_Q;
#	Macro_4 <- Macro_4_Q;
#	Output_file <- Output_file_regression_item2
#	num_models_to_examine,Output_file_regressionQ_item2,revenue_Q,.Macro,Macro_2,Macro_3,Macro_4,BU
	
	file_name <- paste(Output_file,"/Model_Fitting_Results_Good_2Varaibles.csv",sep="")
	best_file <- read.csv(file_name);
	if (nrow(best_file) == 0) {
		file_name <- paste(Output_file,"/Model_Fitting_Results_Good_2Varaibles.png",sep="");
		best_file_tex <- data.frame(Warning=c("No Model Pass Pivot Statistical Test",Output_file));
		png(file_name,height=35*nrow(best_file_tex),width=800);
		grid.table(best_file_tex);
		dev.off();
		
		return();
	}
	best_file <- best_file[,-1];
	
	if (nrow(best_file)>num_models_to_examine) {
		best_max <- num_models_to_examine;
	} else {
		best_max <- nrow(best_file);
	}
	revenue_name <- colnames(revenue)[2];
	revenue_name <- gsub("[.]","-",revenue_name); ###To replace . with -.
	
	for (best in 1:best_max) {
		#	best <- 1;	
		.variables <- as.character(unlist(best_file[best,17:18]));
		data <- compile_regression_data(Macro,.variables,revenue);
		
		#do the regression of primary model
		fit <- fit_regression(data);
		
		##do plotting
		start_and_frequency <- determine_frequency_and_start(data[,1]);
		.frequency <- start_and_frequency$frequency;
		.start <- start_and_frequency$start;
		
		#historical actual
		Historical_metric <- ts(data[,2],start=.start,frequency=.frequency);
		
		#historical backtesting
		Backtesting <- ts(fit$fitted.values,start=.start,frequency=.frequency);
		
		#Forecast baseline
		.forecast_data <- compile_forecast_data(Macro,.variables,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data[,1]);
		Forecast_baseline <- ts(predict(fit,.forecast_data),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		#Forecast advser
		.forecast_data <- compile_forecast_data(Macro_2,.variables,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data[,1]);
		Forecast_adverse <- ts(predict(fit,.forecast_data),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		#Forecast severe
		.forecast_data <- compile_forecast_data(Macro_3,.variables,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data[,1]);
		Forecast_severe <- ts(predict(fit,.forecast_data),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		#Forecast bhc
		.forecast_data <- compile_forecast_data(Macro_4,.variables,revenue);
		start_and_frequency <- determine_frequency_and_start(.forecast_data[,1]);
		Forecast_bhc <- ts(predict(fit,.forecast_data),start=start_and_frequency$start,frequency=start_and_frequency$frequency);
		
		plot_name <- paste(Output_file,"/Test-backtesting-and-forecast-",BU,"-",revenue_name,best,".png",sep="");
		png(plot_name,width=800,height=600);
		ts.plot(Historical_metric,Backtesting,Forecast_baseline,Forecast_adverse,Forecast_severe,Forecast_bhc,col=c(Color_history,Color_backtest,Color_baseline,Color_adverse,Color_severe,Color_bhc),type="b",ylab="Metric",sub=formu_display(data,fit));
		abline(v=as.numeric(as.yearmon(data[nrow(data),1])));
		title(paste("Backtest and Forecast ",BU,"/",revenue_name,"    R^2=",round(summary(fit)$r.squared,2),sep=""));
		legend("topleft",legend=c("history","backtest","baseline","adverse","severely adverse","BHC"),pch=1,col=c(Color_history,Color_backtest,Color_baseline,Color_adverse,Color_severe,Color_bhc));
		dev.off();
		
		Comparison <- compare_time_series(Historical_metric, Backtesting)$Comp_Percent
		plot_name <- paste(Output_file,"/RegressionBacktestTable",best,".png",sep="");
		png(plot_name,width=800,height=30*nrow(Comparison));
		grid.table(Comparison);
		dev.off();
		
		##Plot the Table for regreesion
		notation_baseline <- get_notation(merge_successive_ts(Historical_metric,Forecast_baseline),revenue_name)$Table;
		rownames(notation_baseline)[1] <- "Baseline";
		notation_adverse <- get_notation(merge_successive_ts(Historical_metric,Forecast_adverse),revenue_name)$Table;
		rownames(notation_adverse)[1] <- "Adverse";
		notation_severe <- get_notation(merge_successive_ts(Historical_metric,Forecast_severe),revenue_name)$Table;
		rownames(notation_severe)[1] <- "Severe";
		notation_bhc <- get_notation(merge_successive_ts(Historical_metric,Forecast_bhc),revenue_name)$Table;
		rownames(notation_bhc)[1] <- "BHC";
		
		Comparison <- rbind(notation_baseline,notation_adverse,notation_severe,notation_bhc);
		plot_name <- paste(Output_file,"/RegressionForecastTable",best,".png",sep="");
		png(plot_name,width=800,height=30*nrow(Comparison));
		grid.table(Comparison);
		dev.off();
		
#		Output_file_outsample <- paste(Output_file,"/",best,sep="");
#		if (!file.exists(Output_file_outsample)) {
#			dir.create(Output_file_outsample);
#		}
		
#		file_name <- paste(Output_file_forecast_option,"/Test-backtesting and forecast-",BU,"-",revenue_name,best,".csv",sep="");
#		data_forecast <- ts.union(historical_metric,fitted_metric,fitted_metric_2,fitted_metric_3,fitted_metric_4);
#		write.csv(data_forecast,file_name);
		
#		Out_of_Sample_Test(Output_file_outsample,data);
	}
	
	#output to picture to be included in Latex
	best_file_tex <- best_file[1:best_max,];
	for (j in 1:3) {
		best_file_tex[,j] <- format_level_value(best_file_tex[,j]);
	}
	best_file_tex$R.2 <- round(best_file_tex$R.2,digits=2);
	best_file_tex <- best_file_tex[,c(1,2,3,7,17,18)];
	colnames(best_file_tex) <- c("Intercept", "Variables1", "Variable2", "R-square", "V1-Name","V2-Name")
	file_name <- paste(Output_file,"/Model_Fitting_Results_Good_2Varaibles.png",sep="");
	png(file_name,height=30*nrow(best_file_tex),width=800);
	grid.table(best_file_tex);
	dev.off();
}

#test without forecast
Variable_Selection_2variable_Test2 <- function(num_models_to_examine,Output_file,revenue,Macro,BU) {
	
#	revenue <- revenue_Q
#	Macro <- .Macro
#	Output_file <- Output_file_regression_item
	
	file_name <- paste(Output_file,"/Model_Fitting_Results_Good_2Varaibles.csv",sep="")
	best_file <- read.csv(file_name);
	if (nrow(best_file) == 0) {
		return();
	}
	best_file <- best_file[,-1];
	
	if (nrow(best_file)>num_models_to_examine) {
		best_max <- num_models_to_examine;
	} else {
		best_max <- nrow(best_file);
	}
	
	revenue_name <- colnames(revenue)[2];
	revenue_name <- gsub("[.]","-",revenue_name); ###To replace . with -.
	
	for (best in 1:best_max) {
		#	best <- 2;	
		.variables <- as.character(unlist(best_file[best,17:18]));
		data <- compile_regression_data(Macro,.variables,revenue);
		
		#do the regression of primary model
		fit <- fit_regression(data);
		
		start_and_frequency <- determine_frequency_and_start(data[,1]);
		.frequency <- start_and_frequency$frequency;
		.start <- start_and_frequency$start;
		Backtesting <- ts(predict(fit,newdata=data),start=.start,frequency=.frequency);
		
		Historical_metric <- ts(data[,2],start=.start,frequency=.frequency);
		
		#backtesting
		plot_name <- paste(Output_file,"/Test-backtesting-and-forecast-",BU,"-",revenue_name,best,".png",sep="");
		png(plot_name,width=800,height=600);
		ts.plot(Historical_metric,Backtesting,col=c(Color_history, Color_backtest),type="b",ylab="Metric",sub=formu_display(revenue,fit));
#		abline(v=nrow(data));
		title(paste("Backtest and Forecast ",BU,"/",revenue_name,"    R^2=",round(summary(fit)$r.squared,2),sep=""));
		legend("topleft",legend=c("History","Backtesting"),pch=1,col=c(Color_history, Color_backtest));
		dev.off();
		
		Output_file_outsample <- paste(Output_file,"/",best,sep="");
		if (!file.exists(Output_file_outsample)) {
			dir.create(Output_file_outsample);
		}
		Out_of_Sample_Test(Output_file_outsample,data);
		
	}
}


