# 
# 
# Author: XBBKKL3
###############################################################################


if (!"sheets"%in%ls()) {
	wb <- loadWorkbook(revenue_file_version);
	sheets <- getSheets(wb);
}
if (!"Macro"%in%ls()) {
	Macro <- read.xlsx(Macro_file,sheetName="Monthly-baseline");
}

if (!"Macro_Q"%in%ls()) {
	Macro_Q <- read.xlsx(Macro_file,sheetName="Quarterly-baseline");
}


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

		revenue_lob_raw <- read.xlsx(revenue_file_version,sheetName=paste0(BU,segment));
	#	revenue_lob_raw[1,];
	#	revenue_lob_raw[,1];
		
		revenue_lob <- revenue_lob_raw[determine_time_periods(revenue_lob_raw[,1]),];
		revenue_lob <- get_revenues(revenue_lob);
		.Macro <- determine_Macro(revenue_lob_raw[,1], Macro, Macro_Q);
		
		Output_file <- paste(Output_root,BU,sep="");
		if (!file.exists(Output_file)) {
			dir.create(Output_file);
		}
		
		##Plot the CCF between expense items with external variables
		print("Calculate CCF External");
		Output_file <- paste(Output_root,BU,"/Analysis-CrossCorrelation-External-Segment-",segment,sep="");
		if (!file.exists(Output_file)) {
			dir.create(Output_file);
		}
		#Revenue Correlation
		for (i in 1:length(revenue_lob)) {
	#		i <- 1;
			revenue <- revenue_lob[[i]];
			if (all(is.na(revenue[,2]))) {
				next;
			}
			
			Output_file_item <- paste(Output_file,"/",names(revenue_lob)[i],sep="");
			if (!file.exists(Output_file_item)) {
				dir.create(Output_file_item);
			}
			
			.select_row <- .Macro[,1]%in%revenue[,1];
			time_range <- paste(.Macro[range(which(.select_row)),1],collapse=":");
			
			###level correlation
			for (j in 2:ncol(.Macro)) {
				#		j <- 2
				.ccf <- ccf(.Macro[.select_row ,j],revenue[,2],plot=FALSE,na.action=na.pass);
				
				if (j == 2) {
					CCF <- data.frame(as.vector(.ccf$acf));
				} else {
					CCF <- data.frame(CCF,as.vector(.ccf$acf));
				}
			}
			colnames(CCF) <- colnames(.Macro)[2:ncol(.Macro)];
			rownames(CCF) <- .ccf$lag;
			
			CCF2 <- data.frame(row=rownames(CCF),CCF);
			rownames(CCF2) <- NULL;
			CCF2 <- melt(CCF2)
	
			dat1 = data.frame(y=factor(CCF2[,1],levels=-17:17), 
					x=factor(CCF2[,2]), 
					z=round(CCF2[,3],1))
			
			p1 <- ggplot(dat1, aes(x=x, y=y, fill=z)) +
					theme_bw() +
					geom_tile() +
					ylab("Time Lag") +
					xlab("External Variables") +
					geom_text(aes(label=paste(z))) + 
					theme(axis.text.x = element_text(angle = 45, hjust = 1,size=15)) +
					scale_fill_gradient2(midpoint=0, low="#B2182B", high="#2166AC", breaks=c(-1,1)) + 
					ggtitle(paste("Correlation between ",names(revenue_lob)[i]," Fee and External Variables/ History ",time_range,sep=""));
			file_name <- paste(Output_file_item,"/CrossCorrelationWithFactor-",names(revenue_lob)[i],"-Fee.png",sep="");
			ggsave(plot=p1, filename=file_name, height=15, width=25);
			
			###diff correlation
			for (j in 2:ncol(.Macro)) {
				#		j <- 2
				.ccf_diff <- ccf(diff(.Macro[.select_row ,j]),diff(revenue[,2]),plot=FALSE,na.action=na.pass);
				
				if (j == 2) {
					CCF_diff <- data.frame(as.vector(.ccf_diff$acf));
				} else {
					CCF_diff <- data.frame(CCF_diff,as.vector(.ccf_diff$acf));
				}
			}
			colnames(CCF_diff) <- colnames(.Macro)[2:ncol(.Macro)];
			rownames(CCF_diff) <- .ccf_diff$lag;
			
			CCF2 <- data.frame(row=rownames(CCF_diff),CCF_diff);
			rownames(CCF2) <- NULL;
			CCF2 <- melt(CCF2);
			
			dat1 = data.frame(y=factor(CCF2[,1],levels=-17:17), 
					x=factor(CCF2[,2]), 
					z=round(CCF2[,3],1))
			
			p1 <- ggplot(dat1, aes(x=x, y=y, fill=z)) +
					theme_bw() +
					geom_tile() +
					ylab("Time Lag") +
					xlab("External Variables") +
					geom_text(aes(label=paste(z))) + 
					theme(axis.text.x = element_text(angle = 45, hjust = 1,size=15)) +
					scale_fill_gradient2(midpoint=0, low="#B2182B", high="#2166AC", breaks=c(-1,1)) + 
					ggtitle(paste("Correlation between Differenced ",names(revenue_lob)[i]," Fee and External Variables/ History ",time_range,sep=""));
			file_name <- paste(Output_file_item,"/CrossCorrelationWithFactor-",names(revenue_lob)[i],"-Fee-Diff.png",sep="");
			ggsave(plot=p1, filename=file_name, height=15, width=25);
		}
		#Metric Correlation
		for (i in 1:length(revenue_lob)) {
			#	i <- 2;
			revenue <- revenue_lob[[i]];
			#Plot correlation with Metric
			if (all(is.na(revenue[,3]))) {
				next;
			}
			Output_file_item <- paste(Output_file,"/",names(revenue_lob)[i],sep="");
			if (!file.exists(Output_file_item)) {
				dir.create(Output_file_item);
			}
			.select_row <- .Macro[,1]%in%revenue[,1];
			time_range <- paste(.Macro[range(which(.select_row)),1],collapse=":");
			
			###level correlation
			for (j in 2:ncol(.Macro)) {
				.ccf <- ccf(.Macro[.select_row ,j],revenue[,3],plot=FALSE,na.action=na.pass);
				if (j == 2) {
					CCF <- data.frame(as.vector(.ccf$acf))
				} else {
					CCF <- data.frame(CCF,as.vector(.ccf$acf))
				}
			}
			colnames(CCF) <- colnames(.Macro)[2:ncol(.Macro)];
			rownames(CCF) <- .ccf$lag;
			
			CCF2 <- data.frame(row=rownames(CCF),CCF);
			rownames(CCF2) <- NULL;
			CCF2 <- melt(CCF2)
	# Create test data.
			dat1 = data.frame(y=factor(CCF2[,1],levels=-17:17), 
					x=factor(CCF2[,2]), 
					z=round(CCF2[,3],1))
			
			p1 <- ggplot(dat1, aes(x=x, y=y, fill=z)) +
					theme_bw() +
					geom_tile() +
					ylab("Time Lag") +
					xlab("External Variables") +
					geom_text(aes(label=paste(z))) + 
					theme(axis.text.x = element_text(angle = 45, hjust = 1,size=15)) +
					scale_fill_gradient2(midpoint=0, low="#B2182B", high="#2166AC", breaks=c(-1,1)) + 
					ggtitle(paste("Correlation between ",names(revenue_lob)[i]," Metric and External Variables/ History ",time_range,sep=""));
			file_name <- paste(Output_file_item,"/CrossCorrelationWithFactor-",names(revenue_lob)[i],"-Metric.png",sep="");
			ggsave(plot=p1, filename=file_name, height=15, width=25);
			
			###diff correlation
			for (j in 2:ncol(.Macro)) {
				.ccf <- ccf(diff(.Macro[.select_row ,j]),diff(revenue[,3]),plot=FALSE,na.action=na.pass);
				if (j == 2) {
					CCF <- data.frame(as.vector(.ccf$acf))
				} else {
					CCF <- data.frame(CCF,as.vector(.ccf$acf))
				}
			}
			colnames(CCF) <- colnames(.Macro)[2:ncol(.Macro)];
			rownames(CCF) <- .ccf$lag;
			
			CCF2 <- data.frame(row=rownames(CCF),CCF);
			rownames(CCF2) <- NULL;
			CCF2 <- melt(CCF2)
			# Create test data.
			dat1 = data.frame(y=factor(CCF2[,1],levels=-17:17), 
					x=factor(CCF2[,2]), 
					z=round(CCF2[,3],1))
			
			p1 <- ggplot(dat1, aes(x=x, y=y, fill=z)) +
					theme_bw() +
					geom_tile() +
					ylab("Time Lag") +
					xlab("External Variables") +
					geom_text(aes(label=paste(z))) + 
					theme(axis.text.x = element_text(angle = 45, hjust = 1,size=15)) +
					scale_fill_gradient2(midpoint=0, low="#B2182B", high="#2166AC", breaks=c(-1,1)) + 
					ggtitle(paste("Correlation between Differenced ",names(revenue_lob)[i]," Metric and External Variables/ History ",time_range,sep=""));
			file_name <- paste(Output_file_item,"/CrossCorrelationWithFactor-",names(revenue_lob)[i],"-Metric-Diff.png",sep="");
			ggsave(plot=p1, filename=file_name, height=15, width=25);
		}
	}
}

