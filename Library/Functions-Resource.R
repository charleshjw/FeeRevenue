# TODO: Add comment
# 
# Author: XBBKKL3
###############################################################################

Output_mapping_table <- function(resource_all, Output, date) {
	mapping1 <- unique(resource_all[,colnames(resource_all)%in%c("Segment","Sector","Bud","Bud.1","Bud.2")]);
	file_name <- paste0(Output,paste0("mapping1",date,".csv"));
	write.csv(mapping1,file_name);	
}

Adjust_Currency<- function(resource_all,Output,date) {
	fx <- unique(resource_all[,colnames(resource_all)%in%c("Currency.Code")]);
	fx <- as.vector(unlist(fx));
	file_name <- paste0(Output,paste0("fx",date,".csv"));
	write.csv(fx,file_name);
	
	
	fx_ticker <- paste(fx,"USD",sep="/");
	getFX(fx_ticker,from="2016-05-31",to="2016-05-31");
	
	Adjusted_Salary <- resource_all$Comp.Rate;
	FXRate <- rep(-1,nrow(resource_all));
	for (i in 1:length(fx)) {
#	i <- 1;
		Adjusted_Salary[resource_all$Currency.Code==fx[i]]<-resource_all[resource_all$Currency.Code==fx[i],]$Comp.Rate*as.numeric(get(paste0(fx[i],"USD")));
		FXRate[resource_all$Currency.Code==fx[i]]<-as.numeric(get(paste0(fx[i],"USD")));
	}
	Adjusted_Annual_Salary <- data.frame(resource_all$Currency.Code,resource_all$Comp.Rate,Adjusted_Salary,FXRate)
	file_name <- paste0(Output_ComponentAnalysisHR,paste0("Adjusted_Annual_Salary",date,".csv"));
	write.csv(Adjusted_Annual_Salary,file_name);
	
	Adjusted_Salary;
}


Adjust_Incentive <- function(resource_all,Output,date) {
	Adjusted_Salary <- resource_all$Annual.Incentive;
	FXRate <- rep(-1,nrow(resource_all));
	for (i in 1:length(fx)) {
#	i <- 1;
		Adjusted_Salary[resource_all$Currency.Code==fx[i]]<-resource_all[resource_all$Currency.Code==fx[i],]$Annual.Incentive*as.numeric(get(paste0(fx[i],"USD")));
		FXRate[resource_all$Currency.Code==fx[i]]<-as.numeric(get(paste0(fx[i],"USD")));
	}
	Adjusted_Annual_Salary <- data.frame(resource_all$Currency.Code,resource_all$Annual.Incentive,Adjusted_Salary,FXRate)
	file_name <- paste0(Output_ComponentAnalysisHR,paste0("Adjusted_Annual_Incentive",date,".csv"));
	write.csv(Adjusted_Annual_Salary,file_name);
	
	Adjusted_Salary;
}


Reassign_LOB <- function(resource_all, Output, date) {
	LOB <- rep("",nrow(resource_all));
	
	LOB[resource_all$Sector=="Asset Mgt"] <- "Asset Mgt";
	LOB[resource_all$Sector=="Wealth Mgt"] <- "Wealth Mgt";
	LOB[resource_all$Sector=="Asset Servicing"] <- "Asset Servicing";
	LOB[resource_all$Sector=="Broker Dealer Services"] <- "Broker Dealer Services";
	LOB[resource_all$Sector=="Corporate Trust"] <- "Corporate Trust";
	LOB[resource_all$Sector=="Depositary Receipts"] <- "Depositary Receipts";
	LOB[resource_all$Sector=="Pershing."] <- "Pershing.";
	LOB[resource_all$Sector=="Treasury Services"] <- "Treasury Services";
	LOB[resource_all$Bud=="Capital Markets"] <- "Capital Markets";
	LOB[resource_all$Sector=="Global Markets"&resource_all$Bud!="Capital Markets"] <- "Global Markets";
	
	LOB[resource_all$Segment=="Investment Services"&(LOB=="")] <- "Investment Services-Other"
	
	LOB[resource_all$Segment=="Client Service Delivery"] <- "Client Service Delivery";
	
	LOB[resource_all$Segment=="Client Technology Solutions"] <- "Client Technology Solutions";
	
	LOB[resource_all$Segment=="Business Partners"] <- "Business Partners";
	
	LOB[LOB==""] <- "Other";
	
	LOBAssignment <- data.frame(resource_all$Segment,resource_all$Sector,resource_all$Bud,LOB);
	file_name <- paste0(Output,paste0("LOBAssignment",date,".csv"));
	write.csv(LOBAssignment,file_name);
	LOB;
}

