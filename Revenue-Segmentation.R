# Structured approach for expense segmentation
# 
# Author: XBBKKL3
###############################################################################

if (!"sheets"%in%ls()) {
	wb <- loadWorkbook(revenue_file_version);
	sheets <- getSheets(wb);
}

for (bu in 1:length(BUS)) {
#	bu <- 1;
	#restart from 18
	BU <- BUS[bu];
	print(BU);

	Output_file <- paste(Output_root,BU,sep="");
	if (!file.exists(Output_file)) {
		dir.create(Output_file);
	}
	
	#segment1: by revenue
	#segment2: by management unit
	###transform original revenue data file to formated revenue masterfile
	for (i in 1:max_segments) {
	#	i <- 1;
		if (!paste0(BU,i)%in%names(sheets)) {
			next;
		}
		
		Output_file_Materiality <- paste(Output_root,BU,"/Analysis-Materiality-Segment-",i,sep="");
		if (!file.exists(Output_file_Materiality)) {
			dir.create(Output_file_Materiality);
		}
		Output_file_HistoryTrend <- paste(Output_root,BU,"/Analysis-History-Segment-",i,sep="");
		if (!file.exists(Output_file_HistoryTrend)) {
			dir.create(Output_file_HistoryTrend);
		}
		
		###transform original revenue data file to formated revenue masterfile
		revenue_lob_raw <- read.xlsx(revenue_file_version,sheetName=paste0(BU,i));
		#	revenue_lob_raw[1,]
		#	revenue_lob_raw[,1]
		revenue_lob <- data.frame(revenue_lob_raw[determine_time_periods(revenue_lob_raw[,1]),]);

		##Plot history for revenue and metric
		segment <- colnames(revenue_lob)[2:ncol(revenue_lob)];
		for (j in 1:length(segment)) {
			#	j <- 1;
			.revenue <- get_revenue(revenue_lob,segment[j]);
			if (all(is.na(.revenue[,2]))) {
				next;
			}

			.revenue <- .revenue[!is.na(.revenue[,2]),];
			
			forecast_ts_month <- ts(.revenue[,2],frequency=determine_frequency_and_start(.revenue[,1])$frequency,start=determine_frequency_and_start(.revenue[,1])$start);
			
			if (revenue_analysis_segmentation(.revenue)) {
				notation <- get_notation(forecast_ts_month,segment[j]);
				
				file_name <- paste(Output_file_HistoryTrend,"/history-",BU,"-",gsub("[.]","-",segment[j]),".png",sep="");
				png(file_name,width=800,height=600);
#				ts.plot(forecast_ts_month,col=c(Color_history),type="b",ylab="");
#				abline(v=notation$Year);
#				title(paste(BU,"-",segment[j],sep=""));
#				text(notation$X,notation$Y,notation$Content,col=Color_history);
				p1 <- ggplot_standard(.revenue);
				grid.arrange(p1);
				dev.off();
				
				file_name <- paste(Output_file_HistoryTrend,"/history-",BU,"-",gsub("[.]","-",segment[j]),"-Table.png",sep="");
				png(file_name,height=60*nrow(notation$Table),width=600);
				table <- tableGrob(notation$Table);
				title <- textGrob(segment[j],gp=gpar(fontsize=12));
				padding <- unit(5,"mm");
				table <- gtable_add_rows(table, heights = grobHeight(title) + padding,pos = 0)
				table <- gtable_add_grob(table, title, 1, 1, 1, ncol(table))			
				grid.draw(table);
				dev.off();
			} else {
				file_name <- paste(Output_file_HistoryTrend,"/history-",BU,"-",gsub("[.]","-",segment[j]),".png",sep="");
				png(file_name,width=800,height=600);
				ts.plot(forecast_ts_month,col=c(Color_history),type="b",ylab="");
				title(paste(BU,"-",segment[j],sep=""));
				dev.off();
			}
		}
		
		###Materiality of Revenue. Metrics are filtered out.
		if (length(segment)>2) {
			revenue_lob <- revenue_lob[,c(1,c(grep("Revenue[.]",colnames(revenue_lob)),grep("Expense[.]",colnames(revenue_lob))))];
			##Table
			#updated 5/16/2016
			statistics_items <- colnames(revenue_lob)[-1];#the first item should be total revenue
			filename <- paste(Output_file_Materiality,"/Statistics.csv",sep="");
			filename_tex <- paste(Output_file_Materiality,"/Statistics.tex",sep="");
			filename_png <- paste(Output_file_Materiality,"/Statistics.png",sep="");
			calculate_statistics(revenue_lob,statistics_items,filename,filename_tex,filename_png);			
		}
	}
}
