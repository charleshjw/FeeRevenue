# 
# 
# Author: XBBKKL3
###############################################################################



ggplot_piechart <- function(Output_file,revenue_lob,segment1,segment_peroid_start,segment_peroid_end,segment_peroid_start_q,segment_peroid_end_q,level) {
	start_and_frequency <- determine_frequency_and_start(revenue_lob[,1]);
	.start <- start_and_frequency$start;
	.frequency <- start_and_frequency$frequency;
	
	length(segment1) == sum(colnames(revenue_lob)%in%segment1);
	segmentation <- revenue_lob[,colnames(revenue_lob)%in%segment1];
	if (.frequency==12) {	
		segmentation_sum <- apply(window(ts(segmentation,frequency=.frequency,start=.start),start=segment_peroid_start,end=segment_peroid_end),2,sum);
	} else {
		segmentation_sum <- apply(window(ts(segmentation,frequency=.frequency,start=.start),start=segment_peroid_start_q,end=segment_peroid_end_q),2,sum);
	}
	
	ggplot_pie(segmentation_sum,Output_file,level);
}


get_revenues <- function(revenue_lob) {
	revenue_list <- list();
	tmp <- 1;
	for(j in seq(2,ncol(revenue_lob),by=2)) {
#		j <- 2;
		revenue_list[[tmp]] <- revenue_lob[,c(1,j:(j+1))];
		names(revenue_list)[tmp] <- get_revenue_name(colnames(revenue_list[[tmp]])[2]);
		tmp <- tmp+1;
	}
	revenue_list;
	#Standardized data for regression
}

determine_champion_segment <- function(BU) {
	if (BU == "AS") {
		return(1);
	} 
	if (BU=="BDS") {
		return(c(1,2));
	}
}


extract_data_from_revenues <- function(revenue_lob,regressor_name) {
	#revenue_lob is the list of revenues
	#regressor_name is like "Revenue.Total_Other"
#	regressor_name <- colnames(modelset)[j]
	revenue_lob_1 <- revenue_lob[[get_revenue_name(regressor_name)]];
	revenue <- revenue_lob_1[,c(1,which(colnames(revenue_lob_1)==regressor_name))];
	revenue;
}

get_revenue_name <- function(text) {
	text <- gsub("Revenue", "", text);
	text <- gsub("Metric", "", text);
	text <- gsub("Expense", "", text);
	text <- gsub("[.]", "", text);
	text;
}

get_variable <- function(Macro, Macro_Q, revenue, variable) {
	.Macro <- determine_Macro(revenue[,1], Macro, Macro_Q);
	.select_rows <- (max(which(.Macro[,1]%in%revenue[,1]))+1):nrow(.Macro);
	.select_colume <- colnames(.Macro)%in%variable;
	data <- data.frame(date = .Macro[.select_rows,1], .Macro[.select_rows,.select_colume]);
	colnames(data)[2] <- variable;
	revenue_to_ts(data);
}


get_notation <- function(forecast_ts_month, segment_name ,GR=FALSE) {
	#get notation when plot historical revenue/metric
	#forecast_ts_month <- forecasts$actual_forecast
#	segment_name <- segment[j];
#	if (grepl("Metric",segment_name)) {
#		forecast_ts_annual <- aggregate(forecast_ts_month,nfreq=1,sum);	
#	} else if (grepl("Revenue",segment_name)) {
#		forecast_ts_annual <- aggregate(forecast_ts_month,nfreq=1,sum);
#	} else {
#		stop("Name convention does not right");
#	}
	
	forecast_ts_month <- trim_ts(forecast_ts_month);

	forecast_ts_annual <- aggregate(forecast_ts_month,nfreq=1,sum);
	year <- as.numeric(time(forecast_ts_annual)); #verticle line
	x <- as.numeric(time(forecast_ts_annual)+0.5); #mid year
	y <- max(forecast_ts_month,na.rm=TRUE); #top of the chart
	
	.level <- format_level_value(forecast_ts_annual);
	gr <- diff(forecast_ts_annual)/forecast_ts_annual[-length(forecast_ts_annual)];
	.grwothrate <- paste("(",c(NA,format_percentage(gr)),")",sep="");

	if (GR) {			
		content <- paste(.level,.grwothrate);
	} else {			
		content <- paste(.level);
	}
	table <- t(data.frame(Amount=.level,AnnualGrowth=c(NA,format_percentage(gr)),row.names=index(.level)));

	list(Year=year, X=x, Y=y, Content=content, Table=table);
}

revenue_analysis_segmentation <- function(revenue) {
	
	if (nrow(revenue)==0) {
		return(FALSE);
	}
	
	#if last year's revenue is more than threshold
	if (determine_frequency_and_start(revenue[,1])$frequency==12) {
		criteria2 <- length(na.omit(revenue[,2])) > 24;
	} else {
		criteria2 <- length(na.omit(revenue[,2])) > 8;
	}
	
	#if revenue period is more than 12 periods
	
	if (criteria2) {
		return(TRUE);
	} else {
		return(FALSE);
	}
#	return TRUE if pass threshold
}

ggplot_pie <- function(data,Output_file,name) {
	#format of data should be like below.
#	Revenue.Total_Balance Revenue.Total_positions        Revenue.Fundvest 
#	76245368               228725707               147726760 
#	Revenue.IRA_Account  Revenue.Clearance_Fees     Revenue.Total_Other 
#	73452862               300054689               632214085 
#	data <- segmentation_sum;
	data<-melt(data)
	data <- cbind(LOB =rownames(data),data);
	rownames(data) <- NULL;
	
	y.breaks <- cumsum(data$value) - data$value/2
	label=paste0(round(data$value/sum(data$value)*100,digit=1),"%")
	p <- ggplot(data, aes(x="", y=value, fill=LOB)) + 
			geom_bar(width = 5, stat = "identity", color="black") +
			coord_polar("y", start=0) +
			scale_fill_brewer(palette="Set3") +
			theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank()) +
			ggtitle(paste(BU," - Breakdown",sep="")) +
			theme(axis.text.x=element_text(color='black')) +
			scale_y_continuous(breaks=y.breaks, labels=gsub("Revenue.","",data$LOB)) +
			annotate(geom = "text", y = y.breaks, x = 1.4, label = label);
	plot_name <- paste(Output_file,"/Pie-",name,"-",BU,".png",sep="");
	ggsave(plot=p, filename=plot_name);	
	
	data_with_weight <- cbind(data,weight=paste0(round(data$value/sum(data$value),digits=3)*100,"%"));
	file_name <- paste(Output_file,"/Table-",name,"-",BU,".png",sep="");
	png(file_name,height=30*nrow(data_with_weight));
	grid.table(data_with_weight);
	dev.off();
}

plot_table_with_title <- function(table,title="") {
#	table <- Comparison;
#	title <- colnames(modelset)[j]
	table <- tableGrob(table);
	title <- textGrob(title,gp=gpar(fontsize=12));
	padding <- unit(5,"mm");
	table <- gtable_add_rows(table, heights = grobHeight(title) + padding,pos = 0)
	table <- gtable_add_grob(table, title, 1, 1, 1, ncol(table));
	missed <- convertWidth(sum(table$widths), "in", valueOnly = TRUE) -	convertWidth(grobWidth(title), "in", valueOnly = TRUE)
	if(missed < 0 ) # need to do something about it
		table$widths <- table$widths + unit(abs(missed)/ncol(table), "in")	
	grid.draw(table);
}

calculate_statistics <- function(revenue_lob,statistics_items,filename,filename_tex,filename_png,title="") {
	start_and_frequency <- determine_frequency_and_start(revenue_lob[,1]);
	.start <- start_and_frequency$start;
	.frequency <- start_and_frequency$frequency;
#	revenue_lob <- segmentation
	for (i in 1:length(statistics_items)) {
		#	i <- 6
		.totdir <- ts(get_revenue(revenue_lob,statistics_items[1])[,2],start=.start,frequency=.frequency);
		.revenue <- ts(get_revenue(revenue_lob,statistics_items[i])[,2],start=.start,frequency=.frequency);
		if (statistics_items[i]!="TT9990") {	
			size_percentages <- aggregate(.revenue,nfrequency=1)/aggregate(.totdir,nfrequency=1);
			size_percentage <- size_percentages[length(size_percentages)];
			size_percentage_ave <- mean(size_percentages,na.rm=TRUE);
			size_volatility <- sd(size_percentages,na.rm=TRUE);
			relative_volatility <- size_volatility/size_percentage_ave;
#				relative_volatility <- format_percentage(relative_volatility);
			size_percentage <- format_percentage(size_percentage);
			size_percentage_ave <- format_percentage(size_percentage_ave);
			size_volatility <- format_percentage(size_volatility);
		} else {
			size_percentage <- "";
			size_percentage_ave <- "";
			size_volatility <- "";
			relative_volatility <- "";
		}
		size <- aggregate(.revenue,nfrequency=1);
		cagr5 <- (size[length(size)]/size[length(size)-5])^(1/5)-1;
		cagr3 <- (size[length(size)]/size[length(size)-3])^(1/3)-1;
		cagr1 <- (size[length(size)]/size[length(size)-1])-1;
		
		cagr1 <- format_percentage(cagr1);
		cagr3 <- format_percentage(cagr3);
		cagr5 <- format_percentage(cagr5);
		
		if (i == 1) {
			table_expense_component <- c(statistics_items[i],size_percentage,size_percentage_ave,size_volatility,round(relative_volatility,digits=2),cagr1,cagr3,cagr5);
		} else {
			table_expense_component <- rbind(table_expense_component,c(statistics_items[i],size_percentage,size_percentage_ave,size_volatility,round(relative_volatility,digits=2),cagr1,cagr3,cagr5));			
		}
	}
	colnames(table_expense_component) <- c("","Weight (last year)","Weight (average)","sd","Relative sd","1Y CAGR", "3Y CAGR", "5Y CAGR")
	write.csv(table_expense_component,filename,row.names=FALSE);
	
	rownames(table_expense_component) <- NULL
	table_expense_componentx <- xtable(table_expense_component,auto=TRUE)
	large <- function(x){
		paste0('{\\Large{\\bfseries ', x, '}}')
	}
	align(table_expense_componentx) <- "l|rrrrrrrr";
	
	print(table_expense_componentx,
			sanitize.colnames.function = large,
			type="latex", file=filename_tex);
	rownames(table_expense_component) <- NULL;
	png(filename_png,width=ncol(table_expense_component)*100,height=nrow(table_expense_component)*25);
	plot_table_with_title(table_expense_component,title)
	dev.off();
}

compare_time_series <- function(data1, data2, freq=1) {
	#Inputs are two time series
	#Outputs are comparison table
	# data1 <- .actual
	# data2 <- .backtesting
	# freq <- 4
	#first segment data to years
	data_intersect <- ts.intersect(data1,data2)
	agg_intersect <- aggregate(data_intersect,nfrequency=freq,FUN=sum);###Problem!!!
	if (freq == 1) {
		.row_names <- index(agg_intersect)
	} else if(freq == 4) {
		.row_names <- as.yearqtr(time(agg_intersect));
	} else if(freq == 12) {
		.row_names <- as.yearmon(time(agg_intersect));
	}
	comparison <- data.frame(agg_intersect,row.names=.row_names);
	comparison_present <- comparison;
	for (j in 1:ncol(comparison)) {
		comparison_present[,j] <- format_level_value(comparison[,j]);
	}
		
	comparison_percent <- cbind(comparison_present,as.character(format_percentage((comparison[,2]-comparison[,1])/comparison[,1])));
	colnames(comparison_percent) <- c("Actual","Model","(M-A)/A");
	
	comparison_percent <- comparison_percent[nrow(comparison_percent):1,];
	comparison_percent <- na.omit(comparison_percent);
	list(Comp_Orig = comparison, Comp_Present = comparison_present, Comp_Percent=comparison_percent);
}

write_model_forecast_to_csv <- function(forecasts, file_name) {
	.forecast_matrix <- ts.union(forecasts$actual,forecasts$backtesting,forecasts$baseline,forecasts$adverse,forecasts$severe,forecasts$bhc);
	write.csv(data.frame(.forecast_matrix,row.names=as.yearmon(time(.forecast_matrix))),file_name);
}

actual_backtest_comparison <- function(forecasts) {
	.actual <- trim_ts(forecasts$actual);
	.backtesting <- trim_ts(forecasts$backtesting);
	historical_year <- aggregate(.actual,nfrequency=1,FUN=sum);
	model_year <- aggregate(.backtesting,nfrequency=1,FUN=sum);
	Comparison <- compare_time_series(historical_year, model_year)$Comp_Percent;
	Comparison_Q <- compare_time_series(.actual, .backtesting,4)$Comp_Percent;
	return_result <- rbind(Comparison_Q[1:6,],Comparison);
	return_result[,3] <- as.character(return_result[,3]);
	#customized
	tmp <- return_result[1,];
	.a <- sum(window(.actual,2016));
	.b <- sum(window(.backtesting,2016));
	tmp[1,1] <- format_level_value(.a);
	tmp[1,2] <- format_level_value(.b);
	tmp[1,3] <- format_percentage((.b-.a)/.a);
	rownames(tmp) <- "2016 1H";
	#
	rbind(tmp,return_result);
	#if no customization
	#	return_result
}