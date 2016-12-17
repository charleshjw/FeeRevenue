# 
# 
# Author: XBBKKL3
###############################################################################

Output_file <- paste(Output_root,BU,sep="");
if (!file.exists(Output_file)) {
	dir.create(Output_file);
}

#Segment 1
i <- 1;
print(BU);
Output_file <- paste(Output_root,BU,"/Analysis-ComponentAnalysis-Segment-",i,sep="");
if (!file.exists(Output_file)) {
	dir.create(Output_file);
}
revenue_lob_raw <- read.xlsx(revenue_file_version,sheetName=paste0(BU,i));
revenue_lob <- revenue_lob_raw[determine_time_periods(revenue_lob_raw[,1]),];
#colnames(revenue_lob)

start_and_frequency <- determine_frequency_and_start(revenue_lob[,1]);
.start <- start_and_frequency$start;
.frequency <- start_and_frequency$frequency;

segment1 <- c("MutualFund","UIT","DomesticCustody","InternationalCustody","LiquidityService","SoftwareService","TrustCust","SpecialService","DCTrust","ServicingFee","FoundationEndowment","OtherAUCRelatedFees","OtherInvestmentIncome");
segment1 <- paste0("Revenue.",segment1)
ggplot_piechart(Output_file,revenue_lob,segment1,segment_peroid_start,segment_peroid_end,segment_peroid_start_q,segment_peroid_end_q,"N");

segment1 <- c("AUCRelatedFee","OtherInvestmentIncome");
segment1 <- paste0("Revenue.",segment1)
ggplot_piechart(Output_file,revenue_lob,segment1,segment_peroid_start,segment_peroid_end,segment_peroid_start_q,segment_peroid_end_q,"N+1");


####Treemap
#segmentation_sum<-melt(segmentation_sum);
#segmentation_sum <- cbind(LOB =rownames(segmentation_sum),segmentation_sum);
#rownames(segmentation_sum) <- NULL;
#colnames(segmentation_sum);
#treemap(segmentation_sum, index="LOB",vSize="value",title=BU);

#Segment 2
i <- 2;
print(BU);
Output_file <- paste(Output_root,BU,"/Analysis-ComponentAnalysis-Segment-",i,sep="");
if (!file.exists(Output_file)) {
	dir.create(Output_file);
}
revenue_lob_raw <- read.xlsx(revenue_file_version,sheetName=paste0(BU,i));
revenue_lob <- revenue_lob_raw[determine_time_periods(revenue_lob_raw[,1]),];
#colnames(revenue_lob)

start_and_frequency <- determine_frequency_and_start(revenue_lob[,1]);
.start <- start_and_frequency$start;
.frequency <- start_and_frequency$frequency;

segment1 <- c("USMiddleMarket","CGNP","Platinum","FI","LATAM","OtherAmericas","EMEA","ASIA","AIS","Structured_Products","MiddleMarket","Eagle","OtherAUCRelatedFees","OtherInvestmentIncome");
segment1 <- paste0("Revenue.",segment1)
ggplot_piechart(Output_file,revenue_lob,segment1,segment_peroid_start,segment_peroid_end,segment_peroid_start_q,segment_peroid_end_q,"N");

segment1 <- c("USAS","EMEA","ASIA","AIS","Structured_Products","MiddleMarket","Eagle","Other");
segment1 <- paste0("Revenue.",segment1)
length(segment1) == sum(colnames(revenue_lob)%in%segment1);
ggplot_piechart(Output_file,revenue_lob,segment1,segment_peroid_start,segment_peroid_end,segment_peroid_start_q,segment_peroid_end_q,"N+1");

#segment 3
i <- 3;
segment3 <- c("AS");
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

segment1 <- c("MutualFund","UIT","DomesticCustody","InternationalCustody","LiquidityService","SoftwareService","TrustCust","SpecialService","DCTrust","ServicingFee","FoundationEndowment","OtherAUCRelatedFees","OtherInvestmentIncome");
segment1 <- paste0("Revenue.",segment1)
length(segment1) == sum(colnames(revenue_lob)%in%segment1);
ggplot_piechart(Output_file,revenue_lob,segment1,segment_peroid_start,segment_peroid_end,segment_peroid_start_q,segment_peroid_end_q,"N");
