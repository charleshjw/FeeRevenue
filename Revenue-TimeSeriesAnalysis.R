# TODO: Add comment
# 
# Author: XBBKKL3
###############################################################################


##############
#Outliers analysis

#BUS <- c("AS","Clearing","CT","CashMgmt", "IM", "BDS", "FX", "SL", "DR");

#Output_root <- "./Output-Q22016-v2/";

#if (!dir.exists(Output_root)) {
#	dir.create(Output_root);
#}

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
	if (!dir.exists(Output_file)) {
		dir.create(Output_file);
	}
	
	##Run segment 1
	for (segment in 1:max_segments) {
#		segment <- 1;
		if (paste0(BU,segment)%in%names(sheets)) {
			Output_file <- paste(Output_root,BU,"/Analysis-TimeSeries-",segment,sep="");
			if (!dir.exists(Output_file)) {
				dir.create(Output_file);
			}
			
			revenue_lob_raw <- read.xlsx(revenue_file_version,sheetName=paste0(BU,segment));
			revenue_lob <- revenue_lob_raw[determine_time_periods(revenue_lob_raw[,1]),];
			revenue_lob <- get_revenues(revenue_lob);
			
			#individual
			for (i in 1:length(revenue_lob)) {
#				i <- 1;
				for (j in 2:3) {
#					j <- 2;
					regressor <- revenue_lob[[i]][,c(1,j)];
					regressor <- clean_na_data_frame(regressor);
					colnames(regressor)[1] <- "Date";
					
					###TS test1: outliers
					if (nrow(regressor)!=0) {
						plot_name <- paste(Output_file,"/",BU,"-",segment,"-",gsub("[.]","-",colnames(regressor)[2]),"-Outliers.png",sep="");
						plot_outlier(regressor,plot_name);
#						plot_outlier(regressor);
					}
				}
			}
			
			#implied yield
			for (i in 1:length(revenue_lob)) {
#				i <- 2;
				if (all(is.na(revenue_lob[[i]][,3]))) {
					next;
				}
				if (all(is.na(revenue_lob[[i]][,2]))) {
					next;
				}
				
				revenue <- revenue_lob[[i]][,c(1,2)];
				metric <- revenue_lob[[i]][,c(1,3)];
				iy <- data.frame(revenue_lob[[i]][,1],ImpliedYield=revenue_lob[[i]][,2]/revenue_lob[[i]][,3]);
				p1 <- ggplot_standard(revenue);
				p2 <- ggplot_standard(metric);
				p3 <- ggplot_standard(iy);
				
				plot_name <- paste(Output_file,"/",BU,"-",segment,"-",gsub("[.]","-",colnames(revenue_lob[[i]])[3]),"-ImpliedYield.png",sep="");
				
				png(plot_name,width=800,height=600);
				grid.arrange(p1,p2,p3,ncol=1);
				dev.off();
				
					
				}
			}
			
		}
	}
