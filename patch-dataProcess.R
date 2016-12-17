# TODO: Add comment
# 
# Author: XBBKKL3
###############################################################################


source("../Library/Functions.R");
setwd("C:/workspace2016/Revenue");
library(xlsx);
Time_periods <- 1:114;
Time_periods_Q <- 1:38;

#rm(list=ls())
Macro_file <- "../Library/Data/Masterfile-Data Process-6-30-16.xlsx";
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

Macro_file_new <- "../Library/Data/Masterfile-Data Process-6-30-16vCCAR.xlsx";
Macro_new <- read.xlsx(Macro_file_new,sheetName="Monthly-baseline");
Macro_new_Q <- read.xlsx(Macro_file_new,sheetName="Quarterly-baseline");

macro_shift <- function(.Macro_old,.Macro_new) {
#	.Macro_old <- Macro_Q;
#	.Macro_new <- Macro_new_Q;
#	Macro_new[,2]
	.Time_periods <- determine_time_periods(.Macro_old[,1]);
	for (j in 2:ncol(.Macro_old)) {
#		j <- 2
		curve <- .Macro_old[max(.Time_periods):nrow(.Macro_old),j];
		curve <- curve/curve[1];
		.Macro_new[,j] <- c(.Macro_new[.Time_periods,j],.Macro_new[max(.Time_periods),j]*curve[-1]);
	}
	flag <- c(FALSE);
	for (j in 2:ncol(.Macro_new)) {
		flag <- c(flag, all(.Macro_new[.Time_periods,j]==0));
	}
	.Macro_new[,!flag]
}

baseline <- macro_shift(Macro,Macro_new);
adverse <- macro_shift(Macro_2,Macro_new);
severe <- macro_shift(Macro_3,Macro_new);
bhc <- macro_shift(Macro_4,Macro_new);
#j <- 71
#baseline[,j]
#adverse[,j]
#severe[,j]
#bhc[,j]

baseline_Q <- macro_shift(Macro_Q,Macro_new_Q)
adverse_Q <- macro_shift(Macro_2_Q,Macro_new_Q)
severe_Q <- macro_shift(Macro_3_Q,Macro_new_Q)
bhc_Q <- macro_shift(Macro_4_Q,Macro_new_Q)
#j <- 2
#baseline_Q[,j]
#adverse_Q[,j]
#severe_Q[,j]
#bhc_Q[,j]

if (file.exists("../Library/Data/Masterfile-Data Process-6-30-16vCCAR2.xlsx")) {
	file.remove("../Library/Data/Masterfile-Data Process-6-30-16vCCAR2.xlsx")
}
 
write.xlsx(baseline, file="../Library/Data/Masterfile-Data Process-6-30-16vCCAR2.xlsx",sheetName="Monthly-baseline",row.names=FALSE,append=TRUE,showNA=FALSE);
write.xlsx(adverse, file="../Library/Data/Masterfile-Data Process-6-30-16vCCAR2.xlsx", sheetName="Monthly-adverse",row.names=FALSE,append=TRUE,showNA=FALSE);
write.xlsx(severe, file="../Library/Data/Masterfile-Data Process-6-30-16vCCAR2.xlsx", sheetName="Monthly-severelyadverse",row.names=FALSE,append=TRUE);
write.xlsx(bhc, file="../Library/Data/Masterfile-Data Process-6-30-16vCCAR2.xlsx", sheetName="Monthly-BHC",row.names=FALSE,append=TRUE);
write.xlsx(baseline_Q, file="../Library/Data/Masterfile-Data Process-6-30-16vCCAR2.xlsx",sheetName="Quarterly-baseline",row.names=FALSE,append=TRUE,showNA=FALSE);
write.xlsx(adverse_Q, file="../Library/Data/Masterfile-Data Process-6-30-16vCCAR2.xlsx", sheetName="Quarterly-adverse",row.names=FALSE,append=TRUE,showNA=FALSE);
write.xlsx(severe_Q, file="../Library/Data/Masterfile-Data Process-6-30-16vCCAR2.xlsx", sheetName="Quarterly-severelyadverse",row.names=FALSE,append=TRUE);
write.xlsx(bhc_Q, file="../Library/Data/Masterfile-Data Process-6-30-16vCCAR2.xlsx", sheetName="Quarterly-BHC",row.names=FALSE,append=TRUE);

print("Finish Data Process");




