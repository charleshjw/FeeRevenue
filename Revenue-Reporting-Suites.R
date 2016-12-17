# 
# 
# Author: XBBKKL3
###############################################################################

report<-createWorkbook(type="xlsx");
cs_gold <- CellStyle(report,fill=Fill(foregroundColor=Color_gold));
cs_silver <- CellStyle(report,fill=Fill(foregroundColor=Color_silver));
cs_bronze <- CellStyle(report,fill=Fill(foregroundColor=Color_bronze)); 
cs_gray <- CellStyle(report,fill=Fill(foregroundColor=Color_gray));

source("../Library/Reporting-Templates.R");
#set up file hierarchy

Output_file_report <- paste(Output_root,"Reports/",sep="");
if (!dir.exists(Output_file_report)) {
	dir.create(Output_file_report);
}

.startRow <- 2;
sheet <- createSheet(report, sheetName = paste("BK Segmentation",sep=""));
xlsx.addTitle(sheet, rowIndex=1, title=paste("BK Segmentation",sep=""),titleStyle = TITLE_STYLE)
for (BU in c("BK","IS","IM","OtherSec")) {
	#	BU <- "BK";
	print(BU);
	
	Output_file <- paste(Output_root,BU,sep="");
	if (!dir.exists(Output_file)) {
		dir.create(Output_file);
	}
	##sheet 1: BU Level Revenue VS Expense
	Input_file <- paste(Output_file,"/Analysis-ComponentAnalysis","/Pie1-",BU,".png",sep="");
	addPicture(Input_file, sheet, scale = 1, startRow = .startRow,startColumn = 1);
	.startRow <- .startRow+30;
}

.startRow <- 2;
sheet <- createSheet(report, sheetName = paste("IM Segmentation",sep=""));
xlsx.addTitle(sheet, rowIndex=1, title=paste("IM Segmentation",sep=""),titleStyle = TITLE_STYLE)
for (BU in c("IM","AM","WM")) {
	#	BU <- "BK";
	print(BU);
	
	Output_file <- paste(Output_root,BU,sep="");
	if (!dir.exists(Output_file)) {
		dir.create(Output_file);
	}
	##sheet 1: BU Level Revenue VS Expense
	Input_file <- paste(Output_file,"/Analysis-ComponentAnalysis","/Pie1-",BU,".png",sep="");
	addPicture(Input_file, sheet, scale = 1, startRow = .startRow,startColumn = 1);
	.startRow <- .startRow+30;
}

.startRow <- 2;
sheet <- createSheet(report, sheetName = paste("IS Segmentation",sep=""));
xlsx.addTitle(sheet, rowIndex=1, title=paste("IS Revenue Segmentation",sep=""),titleStyle = TITLE_STYLE)
for (BU in c("IS","AS","CashMgmt","BDS","Clearing","CSD","CT","DR","GCS")) {
	#	BU <- "BK";
	print(BU);
	
	Output_file <- paste(Output_root,BU,sep="");
	if (!dir.exists(Output_file)) {
		dir.create(Output_file);
	}
	##sheet 1: BU Level Revenue VS Expense
	Input_file <- paste(Output_file,"/Analysis-ComponentAnalysis","/Pie1-",BU,".png",sep="");
	addPicture(Input_file, sheet, scale = 1, startRow = .startRow,startColumn = 1);
	.startRow <- .startRow+30;
}


.startRow <- 2;
sheet <- createSheet(report, sheetName = paste("Other Segmentation",sep=""));
xlsx.addTitle(sheet, rowIndex=1, title=paste("Other Segmentation",sep=""),titleStyle = TITLE_STYLE)
for (BU in c("OtherSec","CM","FX","TOTECH","TRESRY","TSHSER")) {
	#	BU <- "BK";
	print(BU);
	
	Output_file <- paste(Output_root,BU,sep="");
	if (!dir.exists(Output_file)) {
		dir.create(Output_file);
	}
	##sheet 1: BU Level Revenue VS Expense
	Input_file <- paste(Output_file,"/Analysis-ComponentAnalysis","/Pie1-",BU,".png",sep="");
	addPicture(Input_file, sheet, scale = 1, startRow = .startRow,startColumn = 1);
	.startRow <- .startRow+30;
}

file_name <- paste(Output_file_report,"Segmentation-Presentation",as.Date(Sys.time()),".xlsx",sep="");
saveWorkbook(report, file_name);
