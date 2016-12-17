# TODO: Add comment
# 
# Author: XBBKKL3
###############################################################################

Output_file <- paste(Output_root,BU,sep="");
if (!file.exists(Output_file)) {
	dir.create(Output_file);
}

#
#i <- 1;
#print(BU);
#Output_file <- paste(Output_root,BU,"/Analysis-ComponentAnalysis-Segment-",i,sep="");
#if (!file.exists(Output_file)) {
#	dir.create(Output_file);
#}
#revenue_lob_raw <- read.xlsx(revenue_file_version,sheetName=paste0(BU,i));
#revenue_lob <- revenue_lob_raw[determine_time_periods(revenue_lob_raw[,1]),];
##	revenue_lob_raw[1,];
##	revenue_lob_raw[,1];
#
#start_and_frequency <- determine_frequency_and_start(revenue_lob[,1]);
#.start <- start_and_frequency$start;
#.frequency <- start_and_frequency$frequency;
#
#segment1 <- c("InvoicedFee","MoneyMarketTrust","CMNDeprct","Misc","ClientReim","Total_Other");
#segment1 <- paste0("Revenue.",segment1)
#length(segment1) == sum(colnames(revenue_lob)%in%segment1);
#segmentation <- revenue_lob[,colnames(revenue_lob)%in%segment1];
#segmentation_sum <- apply(window(ts(segmentation,frequency=.frequency,start=.start),start=segment_peroid_start,end=segment_peroid_end),2,sum);
#
#ggplot_pie(segmentation_sum,Output_file,"N");


#i <- 2;
#print(BU);
#Output_file <- paste(Output_root,BU,"/Analysis-ComponentAnalysis-Segment-",i,sep="");
#if (!file.exists(Output_file)) {
#	dir.create(Output_file);
#}
#revenue_lob_raw <- read.xlsx(revenue_file_version,sheetName=paste0(BU,i));
#revenue_lob <- revenue_lob_raw[determine_time_periods(revenue_lob_raw[,1]),];
##	revenue_lob_raw[1,];
##	revenue_lob_raw[,1];
#
#start_and_frequency <- determine_frequency_and_start(revenue_lob[,1]);
#.start <- start_and_frequency$start;
#.frequency <- start_and_frequency$frequency;
#
#segment1 <- c("LATAMandCanada","Asia","EMEA","US");
#segment1 <- paste0("Revenue.",segment1);
#length(segment1) == sum(colnames(revenue_lob)%in%segment1);
#segmentation <- revenue_lob[,colnames(revenue_lob)%in%segment1];
#segmentation_sum <- apply(window(ts(segmentation,frequency=.frequency,start=.start),start=segment_peroid_start,end=segment_peroid_end),2,sum);
#
#ggplot_pie(segmentation_sum,Output_file,"N+1");
#
#segment2 <- c("LATAMandCanada","Asia","EMEA","FinancialInst","PublicFinance","CorpAndInsurance","Total_Other");
#segment2 <- paste0("Revenue.",segment2);
#length(segment2) == sum(colnames(revenue_lob)%in%segment2);
#segmentation <- revenue_lob[,colnames(revenue_lob)%in%segment2];
#segmentation_sum <- apply(window(ts(segmentation,frequency=.frequency,start=.start),start=segment_peroid_start,end=segment_peroid_end),2,sum);
#
#ggplot_pie(segmentation_sum,Output_file,"N");


i <- 1;
print(BU);
Output_file <- paste(Output_root,BU,"/Analysis-ComponentAnalysis-Segment-",i,sep="");
if (!file.exists(Output_file)) {
	dir.create(Output_file);
}
revenue_lob_raw <- read.xlsx(revenue_file_version,sheetName=paste0(BU,i));
revenue_lob <- revenue_lob_raw[determine_time_periods(revenue_lob_raw[,1]),];

start_and_frequency <- determine_frequency_and_start(revenue_lob[,1]);
.start <- start_and_frequency$start;
.frequency <- start_and_frequency$frequency;

segment1 <- c("APAC","EMEA","US","DocCust","Other","Recon_Error");
segment1 <- paste0("Revenue.",segment1);
ggplot_piechart(Output_file,revenue_lob,segment1,segment_peroid_start,segment_peroid_end,segment_peroid_start_q,segment_peroid_end_q,"N+1");

segment1 <- c("APAC","EMEA","USFI","USMuni","USGovernment","USCorp","Insurance","USOther","DocCust","Other","Recon_Error");
segment1 <- paste0("Revenue.",segment1);
ggplot_piechart(Output_file,revenue_lob,segment1,segment_peroid_start,segment_peroid_end,segment_peroid_start_q,segment_peroid_end_q,"N");
