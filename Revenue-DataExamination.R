# 
# 
# Author: XBBKKL3
###############################################################################


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

Output_file <- paste(Output_root,"MacroExamination",sep="");
if (!dir.exists(Output_file)) {
	dir.create(Output_file);
}

for (i in 2:ncol(Macro)) {
#	i <- 51;
	ts_0 <- revenue_to_ts(Macro[determine_time_periods(Macro[,1]),c(1,i)]);
	ts_1 <- revenue_to_ts(Macro[,c(1,i)]);
	ts_2 <- revenue_to_ts(Macro_2[,c(1,i)]);
	ts_3 <- revenue_to_ts(Macro_3[,c(1,i)]);
	ts_4 <- revenue_to_ts(Macro_4[,c(1,i)]);
	
	file_name <- paste(Output_file,"/",colnames(Macro)[i],"-Monthly.png",sep="");
	png(file_name,width=800,height=600);
	ts.plot(ts_1,ts_2,ts_3,ts_4,ts_0,col=c(Color_baseline,Color_adverse,Color_severe,Color_bhc,Color_history),type="b",main=colnames(Macro)[i],gpars=list(cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex.sub=2.5))
	legend("topleft",legend=c("history","baseline","adverse","severely adverse","BHC"),pch=1,col=c(Color_history,Color_baseline,Color_adverse,Color_severe,Color_bhc));
	dev.off();
}
#colnames(Macro)
#colnames(Macro_Q)
for (i in 2:ncol(Macro_Q)) {
#	i <- 67;
	ts_0 <- revenue_to_ts(Macro_Q[determine_time_periods(Macro_Q[,1]),c(1,i)]);
	ts_1 <- revenue_to_ts(Macro_Q[,c(1,i)]);
	ts_2 <- revenue_to_ts(Macro_2_Q[,c(1,i)]);
	ts_3 <- revenue_to_ts(Macro_3_Q[,c(1,i)]);
	ts_4 <- revenue_to_ts(Macro_4_Q[,c(1,i)]);
	
	file_name <- paste(Output_file,"/",colnames(Macro_Q)[i],"-Quarterly.png",sep="");
	png(file_name,width=800,height=600);
	ts.plot(ts_1,ts_2,ts_3,ts_4,ts_0,col=c(Color_baseline,Color_adverse,Color_severe,Color_bhc,Color_history),type="b",main=colnames(Macro_Q)[i],gpars=list(cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex.sub=2.5))
	legend("topleft",legend=c("history","baseline","adverse","severely adverse","BHC"),pch=1,col=c(Color_history,Color_baseline,Color_adverse,Color_severe,Color_bhc));
	dev.off();
}

