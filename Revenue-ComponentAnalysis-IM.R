# TODO: Add comment
# 
# Author: XBBKKL3
###############################################################################

Output_file <- paste(Output_root,BU,sep="");
if (!file.exists(Output_file)) {
	dir.create(Output_file);
}

i <- 1;
print(BU);
Output_file <- paste(Output_root,BU,"/Analysis-ComponentAnalysis-Segment-",i,sep="");
if (!file.exists(Output_file)) {
	dir.create(Output_file);
}
revenue_lob_raw <- read.xlsx(revenue_file_version,sheetName=paste0(BU,i));
#	revenue_lob_raw[1,];
#	revenue_lob_raw[,1];
revenue_lob <- revenue_lob_raw[determine_time_periods(revenue_lob_raw[,1]),];

start_and_frequency <- determine_frequency_and_start(revenue_lob[,1]);
.start <- start_and_frequency$start;
.frequency <- start_and_frequency$frequency;

segment1 <- c("EquityIndex","FIIndex","GlobalEquity","USEquity","USFI","GlobalFI","AltAndAbs","OverlayAndCurrency","LDI","ShortTerm","WM","AM_Adjustment","WM_Adjustment");
segment1 <- paste0("Revenue.",segment1)
length(segment1) == sum(colnames(revenue_lob)%in%segment1);
ggplot_piechart(Output_file,revenue_lob,segment1,segment_peroid_start,segment_peroid_end,segment_peroid_start_q,segment_peroid_end_q,"N")


segment1 <- c("TotalIndex","TotalEquity","TotalFI","TotalAlts","LDI","ShortTerm","WM","AM_Adjustment","WM_Adjustment");
segment1 <- paste0("Revenue.",segment1)
length(segment1) == sum(colnames(revenue_lob)%in%segment1);
ggplot_piechart(Output_file,revenue_lob,segment1,segment_peroid_start,segment_peroid_end,segment_peroid_start_q,segment_peroid_end_q,"N+1")


segment1 <- c("LongTerm","ShortTerm","WM","AM_Adjustment","WM_Adjustment");
segment1 <- paste0("Revenue.",segment1)
length(segment1) == sum(colnames(revenue_lob)%in%segment1);
ggplot_piechart(Output_file,revenue_lob,segment1,segment_peroid_start,segment_peroid_end,segment_peroid_start_q,segment_peroid_end_q,"N+2")


segment1 <- c("AM","WM","AM_Adjustment","WM_Adjustment");
segment1 <- paste0("Revenue.",segment1)
length(segment1) == sum(colnames(revenue_lob)%in%segment1);
ggplot_piechart(Output_file,revenue_lob,segment1,segment_peroid_start,segment_peroid_end,segment_peroid_start_q,segment_peroid_end_q,"N+3")


i <- 2;
print(BU);
Output_file <- paste(Output_root,BU,"/Analysis-ComponentAnalysis-Segment-",i,sep="");
if (!file.exists(Output_file)) {
	dir.create(Output_file);
}
revenue_lob_raw <- read.xlsx(revenue_file_version,sheetName=paste0(BU,i));
#	revenue_lob_raw[1,];
#	revenue_lob_raw[,1];
revenue_lob <- revenue_lob_raw[determine_time_periods(revenue_lob_raw[,1]),];

start_and_frequency <- determine_frequency_and_start(revenue_lob[,1]);
.start <- start_and_frequency$start;
.frequency <- start_and_frequency$frequency;

segment1 <- c("PrivateWealth","Distribution","MGMT","Amherst","EACM","Insight","StrategySolution","TBC","ARX","NEWTON","SingulerGuff","CenterSquare","Standish","WalterScott","MCM","Alcentra");
segment1 <- paste0("Revenue.",segment1)
length(segment1) == sum(colnames(revenue_lob)%in%segment1);
ggplot_piechart(Output_file,revenue_lob,segment1,segment_peroid_start,segment_peroid_end,segment_peroid_start_q,segment_peroid_end_q,"N")


segment1 <- c("PrivateWealth","Distribution","MGMT","Boutiques");
segment1 <- paste0("Revenue.",segment1)
length(segment1) == sum(colnames(revenue_lob)%in%segment1);
ggplot_piechart(Output_file,revenue_lob,segment1,segment_peroid_start,segment_peroid_end,segment_peroid_start_q,segment_peroid_end_q,"N+1")

segment1 <- c("PrivateWealth","AssetManagment");
segment1 <- paste0("Revenue.",segment1)
length(segment1) == sum(colnames(revenue_lob)%in%segment1);
ggplot_piechart(Output_file,revenue_lob,segment1,segment_peroid_start,segment_peroid_end,segment_peroid_start_q,segment_peroid_end_q,"N+2")


