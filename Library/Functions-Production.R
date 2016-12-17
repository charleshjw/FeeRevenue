# 
# Author: XBBKKL3
###############################################################################

Stationary_test <- function(data, .threshold = threshold, passthreshold=2) {
#	data <- fit$residuals
	if (.threshold<=0.01) {
		percentile <- 1;
	} else if (.threshold <=0.05) {
		percentile <- 2;
	} else {
		percentile <- 3;
	} 
#	data <- regression_data[,2]
	
	test <- ur.df(data, type = "none");
	flag1 <- test@teststat<test@cval[percentile];
	
	test <- ur.pp(data,type="Z-tau",model="constant");
	flag2 <- test@teststat<test@cval[percentile];
	
	test <- kpss.test(data, null="Level");
	flag3 <- test[["p.value"]]>=.threshold;
	
	if (sum(flag1,flag2,flag3)>=passthreshold) {
		return(0);#p value less than threshold means passing
	} else {
		return(1);
	}
}

ADF_test_plot <- function(data) {
#	data <- regression_data[,2]
#	test <- adf.test(data,k=0);
#	if (test$p.value<.threshold) {
#		flag <- "Stationary";		
#	} else {
#		flag <- "Non-stationary";
#	}
#	paste("DF Stat:", round(test$statistic,digits=2),"; p:", round(test$p.value,digits=2),";",flag,sep="");
	test <- ur.df(data, type = "none");
	statistic <- test@teststat;
	cvalue <- test@cval[2];
	if (statistic<cvalue) {
		flag <- "Stationary";		
	} else {
		flag <- "Non-stationary";
	}
	paste("ADF Stat:", round(statistic,digits=2),"; Cval:", round(cvalue,digits=2),";",flag,sep="");
}

PP_test_plot  <- function(data) {
#	data <- regression_data[,2]
#	test <- pp.test(data);
#	if (test$p.value<.threshold) {
#		flag <- "Stationary";		
#	} else {
#		flag <- "Non-stationary";
#	}
#	paste("PP Stat:", round(test$statistic,digits=2),"; p:", round(test$p.value,digits=2),";",flag,sep="");
	test <- ur.pp(data,type="Z-tau",model="constant");
	statistic <- test@teststat;
	cvalue <- test@cval[2];
	if (statistic<cvalue) {
		flag <- "Stationary";		
	} else {
		flag <- "Non-stationary";
	}
	paste("PP Stat:", round(statistic,digits=2),"; Cval:", round(cvalue,digits=2),";",flag,sep="");
}

KPSS_test_plot  <- function(data, .threshold = threshold) {
	test <- kpss.test(data, null="Level");
	statistic <- test[["statistic"]];
	pvalue <- test[["p.value"]]
	if (pvalue>=.threshold) {
		flag <- "Stationary";		
	} else {
		flag <- "Non-stationary";
	}
	paste("KPSS Stat:", round(statistic,digits=2),"; p:", round(pvalue,digits=2),";",flag,sep="");
}

BG_test_plot <- function(fit, .threshold = threshold) {
	test <- bgtest(fit,order=2);
	stat <- round(test$statistic,digits=2);
	pval <- round(test$p.value,digits=2);
	if (pval>=.threshold) {
		flag <- "No Auto-Correlation";		
	} else {
		flag <- "Auto Correlated";
	}
	paste("BG test Stat: ", stat,"; pvalue: ", pval,";", flag, sep="");
}

CointegrationTest <- function(data) {
	simdata<- data[,2:ncol(data)]
	colnames(simdata)<-c("xx","yy","zz")
	fit1<- lm(xx~., data=simdata)
	res<-fit1$residual
	Lag<- function(x,l) embed(c(rep(NA,l), x), l+1) 
	lrez<-Lag(res,2)  # lag
	dres<-diff(res) #  diff
	ldrez<-Lag(dres,3) # lag diff
	
	ecmO<-as.data.frame(cbind(dres,lrez[-1,2]))
	ecm11<-as.data.frame(cbind(dres,lrez[-1,2],ldrez[,2]))  ## add lag1
	ecm22<-as.data.frame(cbind(dres,lrez[-1,2],ldrez[,2:3]))  ## add lag 1 an2
	fit2<-lm(dres~ . ,  data=na.omit(ecmO)) 
	fit11<-lm(dres~ . ,  data=na.omit(ecm11))
	fit22<-lm(dres~ . ,  data=na.omit(ecm22)) 
	Po.test<-summary(fit2)$coefficients[2,1]/summary(fit2)$coefficients[2,2]    # lag 0
	Po.test11<-summary(fit11)$coefficients[2,1]/summary(fit11)$coefficients[2,2]    # lag 1
	Po.test22<-summary(fit22)$coefficients[2,1]/summary(fit22)$coefficients[2,2]     # lag 2
	
	bgresults<-c(bgtest(fit2)$p.value, bgtest(fit11)$p.value, bgtest(fit22)$p.value)  
	POmatrix<-matrix(c(Po.test,Po.test11, Po.test22, bgresults), nrow=2, byrow=TRUE)
	colnames(POmatrix)<-c("O LAG", "1 LAG", "2 LAG")
	rownames(POmatrix)<-c("ADF test on Residuals", "White noise evidence (>0.05)")
	
	CVTable1<-c(-4.29374,-3.74066,-3.45218)
	CVTable2<-c(-4.64332,-4.09600,-3.81020)
	CVTable <- t(data.frame(CVTable1,CVTable2));
	colnames(CVTable)<-c("1%", "5%", "10%")
	
	list(POmatrix=POmatrix,CVTable=CVTable)
}


Diagnostic_test_regression <- function(regression_data,file_prefix) {
	rownames(regression_data) <- regression_data[,1]
	#calculate stationarity of original series
	#plot1: vision the trend
	plot_name <- paste(file_prefix,"-Diagnostic1.png",sep="");
	png(plot_name,width=800,height=600);
	par(mfrow=c((ncol(regression_data)-1),3));
	for (i in 2:ncol(regression_data)) {
		plot(regression_data[,i],main=colnames(regression_data)[i],ylab="",xlab="");
#		legend("topleft",c(ADF_test_plot(regression_data[,i]),PP_test_plot(regression_data[,i]),KPSS_test_plot(regression_data[,i])))
		title(sub=paste(ADF_test_plot(regression_data[,i]),PP_test_plot(regression_data[,i]),KPSS_test_plot(regression_data[,i]),sep="\n"))
		acf(regression_data[,i],main=colnames(regression_data)[i]);
		pacf(regression_data[,i],main=colnames(regression_data)[i]);
	}
	dev.off();
#	Stationary_test(regression_data[,i])
	#plot2: vision the trend of first differencing values
	#calculate stationarity of first differencing series
#	regression_date_diff <- level_to_diff(regression_data);
#	plot_name <- paste(file_prefix,"-Diagnostic2.png",sep="");
#	png(plot_name,width=800,height=600);
#	par(mfrow=c(3,(ncol(regression_date_diff)-1)));
#	for (i in 2:ncol(regression_data)) {
#		plot(regression_date_diff[,i],main=paste(colnames(regression_date_diff)[i],":",ADF_test_plot(regression_date_diff[,i])));
#		legend("topleft",c(ADF_test_plot(regression_date_diff[,i]),PP_test_plot(regression_date_diff[,i])))
#		acf(regression_date_diff[,i],main=colnames(regression_date_diff)[i]);
#		pacf(regression_date_diff[,i],main=colnames(regression_date_diff)[i]);
#	}
#	dev.off();
	
	#Plot 2: residual
	plot_name <- paste(file_prefix,"-Diagnostic2.png",sep="");
	png(plot_name,width=800,height=600);
	layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
	fit <- fit_regression(regression_data);
	resid <- fit$residuals;
	plot(resid, main=paste(colnames(regression_data)[2],"Residuals"),xlab="");
	abline(h=0);
	#add shapiro-wilk and jarque-bera test
	ws_test <- shapiro.test(resid);
	if (ws_test$p.value<=threshold) {
		ws_result <- paste("Wilk Shapiro Test stat ", round(ws_test$statistic,digits=2),"; p value",round(ws_test$p.value,digits=2),"; Not Normally Distributed");
	} else {
		ws_result <- paste("Wilk Shapiro Test stat ", round(ws_test$statistic,digits=2),"; p value",round(ws_test$p.value,digits=2),"; Normally Distributed");
	}
	
	jb_test <- jarque.bera.test(resid);
	if (jb_test$p.value<=threshold) {
		jb_result <- paste("Jarque-Bera Test stat ", round(jb_test$statistic,digits=2),"; p value",round(jb_test$p.value,digits=2),"; Not Normally Distributed");
	} else {
		jb_result <- paste("Jarque-Bera Test stat ", round(jb_test$statistic,digits=2),"; p value",round(jb_test$p.value,digits=2),"; Normally Distributed");
	}
#	legend("topright",c(ws_result,jb_result))
	title(sub=paste(ws_result,jb_result,sep="\n"))
	acf(resid,main=paste("ACF test of the regression residuals"),xlab="");
	#add stationary test
#	stationary_residuals <- c();
#	lags <- trunc((length(resid) - 1)^(1/3));
#	for (i in 0:lags) {
#		if (adf.test(resid,k=i)$p.value<=threshold) {
#			adf_result <- paste("ADF with lag ",i,": p value ",round(adf.test(resid,k=i)$p.value,digits=2),"; Stationary",sep="");
#		} else {
#			adf_result <- paste("ADF with lag ",i,": p value ",round(adf.test(resid,k=i)$p.value,digits=2),"; Not Stationary",sep="");		
#		}
#		stationary_residuals <- c(stationary_residuals,adf_result);
#	}
#	legend("topright",stationary_residuals);
#	legend("topright",c(ADF_test_plot(resid),PP_test_plot(resid),KPSS_test_plot(resid)));
	title(sub=paste(ADF_test_plot(resid),PP_test_plot(resid),KPSS_test_plot(resid),sep="\n"))
	
	#add kpss test
#	kpss_residuals <- kpss.test(resid, null="Trend");	
#	if (kpss_residuals$p.value<=threshold) {
#		kpss_result <- paste("KPSS statistic:",round(kpss_residuals$statistic,digits=2),"; p value:", round(kpss_residuals$p.value,digits=2),"; Not stationary",sep="");
#	} else {
#		kpss_result <- paste("KPSS statistic:",round(kpss_residuals$statistic,digits=2),"; p value:", round(kpss_residuals$p.value,digits=2),"; Stationary",sep="");
#	}

	pacf(resid,main=paste("PACF test of the regression residuals"),xlab="");
	if (ncol(regression_data)<=3) {
		vif_result <- "No VIF test is performed"
		title(sub=paste(BG_test_plot(fit),sep="\n"))
	} else {
		vif_result <- paste("VIF test", round(max(vif(fit)),digits=3));
		title(sub=paste(vif_result,BG_test_plot(fit),sep="\n"))
	}
	dev.off();
	
	#Plot 3: regression plot
	plot_name <- paste(file_prefix,"-Diagnostic3.png",sep="");
	png(plot_name,width=800,height=600);
	par(mfrow=c(2,2));
	plot(fit,main=colnames(regression_data)[2]);
	#add Breusch-Pagan test
	bp_test <- bptest(fit);
	if (bp_test$p.value <= threshold) {
		bp_result <- paste("Breusch-Pagan stat ", round(bp_test$statistic,digits=2),"; p value ",round(bp_test$p.value,digits=2),"; Heteroscedasticity",sep="");
	} else {
		bp_result <- paste("Breusch-Pagan stat ", round(bp_test$statistic,digits=2),"; p value",round(bp_test$p.value,digits=2),"; Homoscedasticity",sep="");
	}
#	legend("topright",bp_result);
	title(sub=bp_result);
	dev.off();
	
#	##Plot 4 Stationary Test
	if (ncol(regression_data)>=4) {
		coint_results <- CointegrationTest(regression_data);
		plot_name <- paste(file_prefix,"-Diagnostic5.png",sep="");
		png(plot_name,width=600,height=200);
		grid.arrange(tableGrob(format_level_value(coint_results$POmatrix)),tableGrob(format_level_value(coint_results$CVTable)),nrow=2);
		dev.off();		
	}
	
#	tryCatch({
#		coint <- ca.jo(regression_data[,2:ncol(regression_data)],type="trace",K=2,ecdet="none",spec="longrun");
#		coint_table <- data.frame(test=coint@teststat,coint@cval)
#		plot_name <- paste(file_prefix,"-Diagnostic4.png",sep="");
#		png(plot_name,width=800,height=40*nrow(coint_table));
#		plot_table_with_title(round(coint_table,digits=2),"Johansen Test");
#		dev.off();
#	}, error = function(e) {
#		plot_name <- paste(file_prefix,"-Diagnostic4.png",sep="");
#		png(plot_name,width=800,height=80);
#		plot_table_with_title("Singular Matrix","Johansen Test");
#		dev.off();
##		FALSE
##		print(e);
#	}, finally = {})
#
#	#PO Diagnostic Test
#	coint <- po.test(regression_data[,2:ncol(regression_data)]);
#	if (coint$p.value <= threshold) {
#		po_test_result <- paste("Phillips-Ouliaris Test p value ", round(coint$p.value,digits=2), "; PASS",sep="");
#	} else {
#		po_test_result <- paste("Phillips-Ouliaris Test p value ", round(coint$p.value,digits=2), "; FAIL",sep="");
#	}
#
#	#ECM test
#	if (any(grepl("dummy",colnames(regression_data)))) {
#		ecm_data <- data.frame(diff=diff(resid),residual=resid[-length(resid)],regression_data[-1,grepl("dummy",colnames(regression_data))])
#	} else {
#		ecm_data <- data.frame(diff=diff(resid),residual=resid[-length(resid)])
#	}
#	
#	ecm_fit <- lm(diff~residual, data=na.omit(ecm_data));
#	ecm_statistic <- round(summary(ecm_fit)$coefficients[2,3],digits=3);
#	
#	if (ecm_statistic < -3.73525) {
#		ecm_result <- paste("ECM statistic:", ecm_statistic,"<-3.735, PASS", sep="");
#	} else {
#		ecm_result <- paste("ECM statistic:", ecm_statistic,">-3.735, FAIL", sep="");
#	}
#	plot_name <- paste(file_prefix,"-Diagnostic5.png",sep="");
#	png(plot_name,width=800,height=80);
#	plot_table_with_title(rbind(po_test_result,ecm_result));
#	dev.off();
	
	#Influential Point plot
	plot_name <- paste(file_prefix,"-Diagnostic6.png",sep="");
	png(plot_name,width=800,height=600);
	influencePlot(fit,main=paste(colnames(regression_data)[2],"Influence Plot"),sub="Circle size is proportial to Cook's Distance");
	dev.off();

#	influencePlot(fit,id.method="identify",main=paste(colnames(regression_data)[2],"Influence Plot"),sub="Circle size is proportial to Cook's Distance");

	#Cross validation
	orig_rmspe <- rmspe(regression_data[,2], predict(fit)) # rmspe means root mean squared prediction error. This line computes the rmspe for the linear regression model 
	# fitted with all the data points.
	orig_mape <- mape(regression_data[,2], predict(fit)) # mape means mean absolute prediction error. This line computes the mape for the linear regression model fitted 
	# with all the data points
	fold <- cvFolds(nrow(regression_data), K = 5, type = "consecutive")  # assign folds, folds number=5
	
	formu <- paste(colnames(regression_data)[2], "~", colnames(regression_data)[3]);
	if (ncol(regression_data)>3) {
		for (j in 4:ncol(regression_data)) {
			formu <- paste(formu,"+",colnames(regression_data)[j]);
		}
	}
	fit <- lm(as.formula(formu),data=regression_data);
	
	cv_rmspe <- repCV(fit, cost = rmspe, folds = fold) # obtain cross validation rmspe
	cv_mape <- repCV(fit, cost = mape, folds = fold) # obtain cross validation mape
	cv_results <- data.frame(matrix(c(orig_rmspe,orig_mape,cv_rmspe$cv,cv_mape$cv,0,0),ncol=2,byrow=TRUE),row.names=c("Original","CV","CV to Orig Ratio"))
	colnames(cv_results) <- c("RMSPE","MAPE");
	cv_results[3,] <- cv_results[2,]/cv_results[1,]
	cv_results <- round(cv_results,digits=2);
	plot_name <- paste(file_prefix,"-Diagnostic7.png",sep="");
	png(plot_name,width=300,height=120);
	plot_table_with_title(cv_results,"Cross Validation Results");
	dev.off();
	
	#Plot: residual vs independet variables
	plot_name <- paste(file_prefix,"-Diagnostic8.png",sep="");
	png(plot_name,width=400*(ncol(regression_data)-2),height=600);
	par(mfrow=c(1,ncol(regression_data)-2));
	for (i in 3:ncol(regression_data)) {
		plot(regression_data[,i],resid,xlab=colnames(regression_data)[i],main="Residual V.S. Independent Variables")
	}
	dev.off();
	
	#output the original regression results
	out <- capture.output(summary(fit));
	file_name <- paste(file_prefix,"-fit.txt",sep="");
	cat(out, file=file_name, sep="\n");
	cov <- vcovHAC(fit)
	out <- capture.output(coeftest(fit, vcov = cov));
	file_name <- paste(file_prefix,"-fit-HAC.txt",sep="");
	cat(out, file=file_name, sep="\n");
	
	rsquared <- paste("R-squared:",round(summary(fit)$r.squared,digits=2));
	plot_name <- paste(file_prefix,"-Diagnostic9.png",sep="");
	png(plot_name,width=500,height=250);
	plot_table_with_title(c(rsquared, ws_result, jb_result, vif_result, bp_result, BG_test_plot(fit)),paste(colnames(regression_data)[2]," - Summary of Diagnostic Results"));
	dev.off();
}

##local function
Get_Forecast <- function(revenue_lob, modelset, Output_file, output=TRUE, ConvertRevenue=TRUE) {
#	output <- FALSE
#	ConvertRevenue <- FALSE;
#	revenue_lob, modelset, Output_file
	Forecasts <- list();
	for (j in 2:ncol(modelset)) {
#		j <-3;
		#get revenue
		revenue <- extract_data_from_revenues(revenue_lob,colnames(modelset)[j])
		revenue <- clean_na_data_frame(revenue);
		
		#get variable names
		.variables_string <- as.character(unlist(modelset[,j]));
		print(.variables_string)
		variables <- unlist(strsplit(.variables_string,","));
		variables <- trim(variables);
		
		if (variables[1]=="Movingaverage") {
			#MV function
			#revenue, LookBackPeriods, LookBackPeriods_Q
			start_and_frequency <- determine_frequency_and_start(revenue[,1]);
			.frequency <- start_and_frequency$frequency;
			.start <- start_and_frequency$start;
			
			historical_data <- ts(revenue[,2],start=.start,frequency=.frequency);
			
			.lookback <- as.numeric(variables[2]);
			
			if (.frequency==12) {
				.forecastperiod <- 60;
			} else if(.frequency==4) {
				.forecastperiod <- 20;
			} else {
				stop("Error: Look Back Periods");
			}
			
			forecasts <- forecast_ts_mv(revenue,.lookback,.forecastperiod);
			
			if (output) {
				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".png",sep="");			
				title_name <- paste(colnames(modelset)[j], "Moving Average Model (",paste(.lookback,collapse ="/"),")",sep="");
				
				plot_mv_model(forecasts, plot_name, title_name, .lookback);
				
				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".csv",sep="");
				write_model_forecast_to_csv(forecasts, file_name);
				
				##Plot the annual growth rate table
				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Table.png",sep="");
				png(plot_name,width=800,height=100);
				plot_table_with_title(get_notation(forecasts$actual_forecast)$Table,colnames(modelset)[j]);
				dev.off();
				
				#plot backtest statistic
				Comparison <- actual_backtest_comparison(forecasts);
				Comparison <- Comparison[,3,drop=FALSE];
				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest.png",sep="");
				png(plot_name,width=max(150,80*ncol(Comparison)),height=30*nrow(Comparison));
				plot_table_with_title(Comparison,colnames(modelset)[j]);
				dev.off();
			}
			#add to info session
			Forecasts[[j]] <- forecasts;
		} else if(variables[1]=="SeasonalDecomposition") {
			#MV function
			#revenue, LookBackPeriods, LookBackPeriods_Q
			start_and_frequency <- determine_frequency_and_start(revenue[,1]);
			.frequency <- start_and_frequency$frequency;
			.start <- start_and_frequency$start;
			
			historical_data <- ts(revenue[,2],start=.start,frequency=.frequency);
			revenue_name <- colnames(revenue)[2];
			
			if (length(variables)>1) {
				historical_data <- window(historical_data,as.numeric(c(variables[2],variables[3])));
			}
			forecasts <- forecast_ts_seasonalmodel(historical_data);
			
			if (output) {
				#plot regression
				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".png",sep="");
				title_name <- paste(BU,"-",.info[1,1],"-",colnames(modelset)[j],sep="");
				plot_regression_model(forecasts, plot_name, title_name);
				
				##Plot the annual growth rate table
				Comparison <- plot_comparison_table_regression_model(forecasts);
				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Table.png",sep="");
				png(plot_name,width=800,height=30*nrow(Comparison));
				plot_table_with_title(Comparison,colnames(modelset)[j]);
				dev.off();
				
				#backtest table
				Comparison <- actual_backtest_comparison(forecasts);
				Comparison <- Comparison[,3,drop=FALSE];
				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest.png",sep="");
				png(plot_name,width=max(150,80*ncol(Comparison)),height=30*nrow(Comparison));
				plot_table_with_title(Comparison,colnames(modelset)[j]);
				dev.off();
						
				#plot Diagnostic 1
				model <- stl(historical_data,"periodic");
				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Diagnostic1.png",sep="");
				png(plot_name,width=800,height=600);
				plot(model, main=paste(revenue_name,"Seasonality Decomposition Model - STL Model"));
				title(sub=model$call);
				dev.off();
				
				#plot Diagnostic 2
				m1 <- detect_seasonality_method1(historical_data);
				m2 <- detect_seasonality_method2(historical_data);
				
				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-SeasonalTestResults.csv",sep="");
				Results <- data.frame(ETSResult=m1$Result,ETSStatistic=m1$Statistic,TBATSResult=m2$Result,TBATStatistic=m2$Statistic);
				write.csv(Results,file_name);
				
				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Diagnostic2.png",sep="");
				png(plot_name,width=800,height=35*nrow(Results));
				grid.table(Results);
				dev.off();
			}
			Forecasts[[j]] <- forecasts;
			
		} else if (variables[1]=="DiffRegression") {
			variables <- variables[-1];
			regressor_name <- variables;
			.Macro <- determine_Macro(revenue[,1], Macro, Macro_Q);
			
			regressor <- .Macro[,c(1,which(colnames(.Macro)%in%variables))];
			regressor <- regressor[regressor[,1]%in%revenue[,1],];
			
			#Output original regression data for auditing purpose
			if (output) {
				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-regressiondata.csv",sep="");
				write.csv(compile_regression_data(.Macro,variables,revenue), file_name);
			}
			
			####run regression on difference
			#transform data difference
			revenue_diff <- level_to_diff(revenue);
			revenue_ts_diff <- revenue_to_ts(revenue_diff);
			regressor_diff <- level_to_diff(regressor);
			
			data <- cbind(revenue_diff,regressor_diff)[,-3];
			
			regression_data <- clean_na_data_frame(data);
			if (length(variables)==1) {
				colnames(regression_data)[3] <- variables;
			}
			
			#Output differenced regression data for auditing purpose
			if (output) {
				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-regressiondiffdata.csv",sep="");
				write.csv(regression_data, file_name);
			}
			
			file_prefix <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),sep="");
			Diagnostic_test_regression(regression_data,file_prefix);
			
#		adf1 <- round(adf.test(na.omit(data[,2]),k=0)$p.value,digit=2);
#		adf2 <- round(adf.test(na.omit(data[,3]),k=0)$p.value,digit=2);
			fit <- fit_regression(regression_data);
#			fit$coefficients[1]<-0
#			fit$coefficients[2]<-fit2$coefficients[1]
#			fit$coefficients[3]<-fit2$coefficients[2]
#			fit2 <- lm(Revenue.FX_Adjusted~NASDAQVol_SUM+SRI100_AVE-1,regression_data)


			forecasts <- get_forecasts_diff(fit,regression_data,data,revenue,variables);
			
			if (output) {
				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".csv",sep="");
				write_model_forecast_to_csv(forecasts, file_name);
				
				#write original data to output
				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-forecastdata.csv",sep="");
				write.csv(forecasts$forecast_data,file_name);
				
				#backtesting of differenced value
				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".png",sep="");
				title_name <- paste(BU,"-",colnames(modelset)[j],"    R^2=",round(summary(fit)$r.squared,2),sep="");
				plot_regression_model(forecasts, plot_name, title_name);				
			}
			forecasts <- convert_forecasts_diff_to_level(forecasts, revenue, data)
			if (output) {
				#backtesting of level value
				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Diagnostic0.png",sep="");
				title_name <- paste("Backtest Level Value ",BU,"/",colnames(revenue)[2]," V.S. ",paste(regressor_name,collapse="/")," /R^2=",round(summary(fit)$r.squared,2),sep="")
				plot_regression_model(forecasts, plot_name, title_name, TRUE);
#				plot_regression_model(forecasts, plot_name, title_name, FALSE);
				
				##Plot the annual growth rate table
				Comparison <- plot_comparison_table_regression_model(forecasts);
				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Table.png",sep="");
				png(plot_name,width=800,height=30*nrow(Comparison));
				plot_table_with_title(Comparison,colnames(modelset)[j]);
				dev.off();
				
				#plot backtest statistic
				Comparison <- actual_backtest_comparison(forecasts);
				Comparison <- Comparison[,3,drop=FALSE];
				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest.png",sep="");
				png(plot_name,width=max(150,80*ncol(Comparison)),height=30*nrow(Comparison));
				plot_table_with_title(Comparison,colnames(modelset)[j]);
				dev.off();
				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest.csv",sep="");
				write.csv(Comparison,file_name)
				
				Output_file_outsample <- paste(Output_file,"/",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),sep="");
				if (!file.exists(Output_file_outsample)) {
					dir.create(Output_file_outsample);
				}
				
				Out_of_Sample_Test(Output_file_outsample,regression_data);
			}
			#add to info session
			metric_model <- grepl("Metric",colnames(modelset)[j]);
			
			if(metric_model&ConvertRevenue) {
				implied_yield <- calculate_implied_yield(revenue_lob,colnames(modelset)[j],forecasts);
				
				forecasts$backtest_forecast <- forecasts$backtest_forecast*implied_yield
				forecasts$actual_forecast <- forecasts$actual_forecast*implied_yield
				forecasts$actual <- forecasts$actual*implied_yield
				forecasts$backtesting <- forecasts$backtesting*implied_yield
				forecasts$baseline <- forecasts$baseline*implied_yield
				forecasts$adverse <- forecasts$adverse*implied_yield
				forecasts$severe <- forecasts$severe*implied_yield
				forecasts$bhc <- forecasts$bhc*implied_yield
				
				if (output) {
					file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-ConvertedRevenue.csv",sep="");
					write_model_forecast_to_csv(forecasts, file_name);
					
					#write original data to output
	#				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-data-ConvertedRevenue.csv",sep="");
	#				write.csv(forecasts$forecast_data,file_name);
					
					#plot regression
					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-ConvertedRevenue.png",sep="");
					title_name <- paste(BU,"-",gsub("Metric","Revenue",colnames(modelset)[j]),"    R^2=",round(summary(fit)$r.squared,2),sep="");
					plot_regression_model(forecasts, plot_name, title_name);
					
					##Plot the annual growth rate table
					Comparison <- plot_comparison_table_regression_model(forecasts);
					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Table-ConvertedRevenue.png",sep="");
					png(plot_name,width=800,height=30*nrow(Comparison));
					plot_table_with_title(Comparison,gsub("Metric","Revenue",colnames(modelset)[j]));
					dev.off();
					
					#plot backtest statistic
					Comparison <- actual_backtest_comparison(forecasts);
					Comparison <- Comparison[,3,drop=FALSE];
					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest-ConvertedRevenue.png",sep="");
					png(plot_name,width=max(150,80*ncol(Comparison)),height=30*nrow(Comparison));
					plot_table_with_title(Comparison,gsub("Metric","Revenue",colnames(modelset)[j]));
					dev.off();
				}
			}
			Forecasts[[j]] <- forecasts;
		} else if (variables[1]=="ARIMAX") {
			ar <- as.numeric(variables[2]);
			d <- as.numeric(variables[3]);
			ma <- as.numeric(variables[4]);
			variable <- variables[5];
			
			revenue_name <- colnames(revenue)[2];
			if (output) {
				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Diagnostic1.png",sep="");
				png(plot_name,width=800,height=600);
				par(mfrow=c(2,2));
				acf(revenue[,2],main=paste(revenue_name,": ACF of level value",sep=""));
				pacf(revenue[,2],main=paste(revenue_name,": ACF of level value",sep=""));
				acf(diff(revenue[,2]),main=paste(revenue_name,": ACF of First Order Differencing",sep=""));
				pacf(diff(revenue[,2]),main=paste(revenue_name,": ACF of First Order Differencing",sep=""));
				dev.off();
			}
			data <- compile_regression_data(determine_Macro(revenue[,1], Macro, Macro_Q),variable,revenue);
			regression_data <- clean_na_data_frame(data);

			tryCatch({
				fit <- arima(data[,2], order=c(ar,d,ma), xreg=data[,3]);
				forecasts <- get_forecasts_arimax(fit,regression_data,data,revenue,variable);
				if (output) {
					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".png",sep="");
					title_name <- paste(BU,revenue_name,variable,"AR",ar,"D",d,"MA",ma,sep="-")
					plot_regression_model(forecasts, plot_name, title_name);
					
					##Plot the annual growth rate table
					Comparison <- plot_comparison_table_regression_model(forecasts);
					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Table.png",sep="");
					png(plot_name,width=800,height=30*nrow(Comparison));
					plot_table_with_title(Comparison,colnames(modelset)[j]);
					dev.off();
					
					#plot backtest statistic
					Comparison <- actual_backtest_comparison(forecasts);
					Comparison <- Comparison[,3,drop=FALSE];
					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest.png",sep="");
					png(plot_name,width=max(150,80*ncol(Comparison)),height=30*nrow(Comparison));
					plot_table_with_title(Comparison,colnames(modelset)[j]);
					dev.off();
					
					file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest.csv",sep="");
					write.csv(Comparison,file_name)
					
				}
				Forecasts[[j]] <- forecasts;
			},error=function(e) {
				title_name <- paste(BU,revenue_name,variables,"AR",ar,"D",d,"MA",ma,sep="-")
				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".png",sep="");
				png(plot_name,width=800,height=600);
				plot(0, main=title_name);
				dev.off();
			},finally={})
		} else {
			#combine data and delete missing values
			data <- compile_regression_data(determine_Macro(revenue[,1], Macro, Macro_Q),variables,revenue);
			if (output) {
				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-regressiondata.csv",sep="");
				write.csv(data, file_name);
			}
			regression_data <- clean_na_data_frame(data);
			if (length(variables)==1) {
				colnames(regression_data)[3] <- variables;
			}
			
			#run regression
			fit <- fit_regression(regression_data);
			forecasts <- get_forecasts(fit,regression_data,data,revenue,variables);

			if (output) {
				##############
				#Plot Factors
				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Diagnostic0.png",sep="");
				png(file_name, width=800,height=600);
				par(mfrow=c(length(variables),1));
				for (variable in variables) {
					.Macro <- determine_Macro(revenue[,1], Macro, Macro_Q);
					history <- revenue_to_ts(.Macro[determine_time_periods(.Macro[,1]),c(1,which(colnames(.Macro)==variable))]);
					
					baseline <- get_variable(Macro, Macro_Q, revenue, variable);
					adverse <- get_variable(Macro_2, Macro_2_Q, revenue, variable);
					severe <- get_variable(Macro_3, Macro_3_Q, revenue, variable);
					bhc <- get_variable(Macro_4, Macro_4_Q, revenue, variable);
					ts.plot(history,baseline,adverse,severe,bhc,type="b",col=c(Color_history,Color_baseline,Color_adverse,Color_severe,Color_bhc),main=variable);
				}
				dev.off();
				
				##############
				file_prefix <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),sep="");
				Diagnostic_test_regression(regression_data,file_prefix);
				
				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".csv",sep="");
				write_model_forecast_to_csv(forecasts, file_name);
				
				#write original data to output
				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-forecastdata.csv",sep="");
				write.csv(forecasts$forecast_data,file_name);
				
				#plot regression
				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".png",sep="");
				title_name <- paste(BU,"-",colnames(modelset)[j],"    R^2=",round(summary(fit)$r.squared,2),sep="");
				plot_regression_model(forecasts, plot_name, title_name);
				
				##Plot the annual growth rate table
				Comparison <- plot_comparison_table_regression_model(forecasts);
				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Table.png",sep="");
				png(plot_name,width=800,height=30*nrow(Comparison));
				plot_table_with_title(Comparison,colnames(modelset)[j]);
				dev.off();
				
				#plot backtest statistic
				Comparison <- actual_backtest_comparison(forecasts);
				Comparison <- Comparison[,3,drop=FALSE];
				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest.png",sep="");
				png(plot_name,width=max(150,80*ncol(Comparison)),height=30*nrow(Comparison));
				plot_table_with_title(Comparison,colnames(modelset)[j]);
				dev.off();
				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest.csv",sep="");
				write.csv(Comparison,file_name)
				
				Output_file_outsample <- paste(Output_file,"/",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),sep="");
				if (!file.exists(Output_file_outsample)) {
					dir.create(Output_file_outsample);
				}
				
				Out_of_Sample_Test(Output_file_outsample,regression_data);
			}
			
			#add to info session
			metric_model <- grepl("Metric",colnames(modelset)[j]);
			
			if(metric_model&ConvertRevenue) {
#				revenue <- extract_data_from_revenues(revenue_lob,colnames(modelset)[j])
#				revenue <- clean_na_data_frame(revenue);
				
				implied_yield <- calculate_implied_yield(revenue_lob,colnames(modelset)[j],forecasts);
				
				revenue_lob_1 <- revenue_lob[[get_revenue_name(colnames(modelset)[j])]];
				revenue_lob_1 <- clean_na_data_frame(revenue_lob_1);
				
#				forecasts$backtest_forecast <- forecasts$backtest_forecast*implied_yield
#				forecasts$actual_forecast <- forecasts$actual_forecast*implied_yield
				forecasts$actual <- revenue_to_ts(revenue_lob_1[,c(1,2)]);
				forecasts$backtesting <- forecasts$backtesting*implied_yield;
				forecasts$baseline <- forecasts$baseline*implied_yield;
				forecasts$adverse <- forecasts$adverse*implied_yield;
				forecasts$severe <- forecasts$severe*implied_yield;
				forecasts$bhc <- forecasts$bhc*implied_yield;
				
				if (output) {
					file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-ConvertedRevenue.csv",sep="");
					write_model_forecast_to_csv(forecasts, file_name);
					
					#write original data to output
	#				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-data-ConvertedRevenue.csv",sep="");
	#				write.csv(forecasts$forecast_data,file_name);
	
					#plot regression
					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-ConvertedRevenue.png",sep="");
					title_name <- paste(BU,"-",gsub("Metric","Revenue",colnames(modelset)[j]),"    R^2=",round(summary(fit)$r.squared,2),sep="");
					plot_regression_model(forecasts, plot_name, title_name);
					
					##Plot the annual growth rate table
					Comparison <- plot_comparison_table_regression_model(forecasts);
					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Table-ConvertedRevenue.png",sep="");
					png(plot_name,width=800,height=30*nrow(Comparison));
					plot_table_with_title(Comparison,gsub("Metric","Revenue",colnames(modelset)[j]));
					dev.off();
					
					#plot backtest statistic
					Comparison <- actual_backtest_comparison(forecasts);
					Comparison <- Comparison[,3,drop=FALSE];
					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest-ConvertedRevenue.png",sep="");
					png(plot_name,width=max(150,80*ncol(Comparison)),height=30*nrow(Comparison));
					plot_table_with_title(Comparison,gsub("Metric","Revenue",colnames(modelset)[j]));
					dev.off();
					
					file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest-ConvertedRevenue.csv",sep="");
					write.csv(Comparison, file_name);
				}
			}
			Forecasts[[j]] <- forecasts;
		}
	}
	Forecasts;
}

Aggregate_Forecasts <- function(forecasts,covert_to_quarter=FALSE) {
#	forecasts <- forecasts_raw
	j <- 1;
	for (i in 1:length(forecasts)) {
#		i <- 2;
		if(!is.null(forecasts[[i]])) {
			if (j == 1) {
#				forecasts[[2]]
				.backtest_forecast <- forecasts[[i]]$backtest_forecast;
				.actual_forecast <- forecasts[[i]]$actual_forecast;
				.actual <- forecasts[[i]]$actual;
				.backtesting <- forecasts[[i]]$backtesting;
				.baseline <- forecasts[[i]]$baseline;
				.adverse <- forecasts[[i]]$adverse;
				.severe <- forecasts[[i]]$severe;
				.bhc <- forecasts[[i]]$bhc;
				.start_forecast <- forecasts[[i]]$start_forecast;
				.actual_union <- forecasts[[i]]$actual;
				.backtesting_union <- forecasts[[i]]$backtesting;
			} else {
				.backtest_forecast <- .backtest_forecast + forecasts[[i]]$backtest_forecast;
				.actual_forecast <- .actual_forecast + forecasts[[i]]$actual_forecast;
				.actual <- .actual + forecasts[[i]]$actual;
				.backtesting <- .backtesting + forecasts[[i]]$backtesting;
				.baseline <- .baseline + forecasts[[i]]$baseline;
				.adverse <- .adverse + forecasts[[i]]$adverse;
				.severe <- .severe + forecasts[[i]]$severe;
				.bhc <- .bhc + forecasts[[i]]$bhc;
				.actual_union <- ts.union(.actual_union,forecasts[[i]]$actual);
				.backtesting_union <- ts.union(.backtesting_union,forecasts[[i]]$backtesting);
			}
			j <- j + 1;
		}
	}
	.actual_union <- ts.union(.actual_union,.actual);
	.backtesting_union <- ts.union(.backtesting_union,.backtesting);

	if (covert_to_quarter) {
		.agg_q <- list(backtest_forecast=aggregate(trim_ts(.backtest_forecast),nfreq=4), actual_forecast=aggregate(trim_ts(.actual_forecast),nfreq=4),
				actual=aggregate(trim_ts(.actual),nfreq=4), backtesting=aggregate(trim_ts(.backtesting),nfreq=4), baseline=aggregate(.baseline,nfreq=4), adverse=aggregate(.adverse,nfreq=4), severe=aggregate(.severe,nfreq=4), bhc=aggregate(.bhc,nfreq=4),
				start_forecast=.start_forecast,
				actual_union=aggregate(.actual_union,nfreq=4), backtesting_union=aggregate(.backtesting_union,nfreq=4));
		
		return(.agg_q);
	} else {
		.agg <- list(backtest_forecast=.backtest_forecast, actual_forecast=.actual_forecast,
				actual=.actual, backtesting=.backtesting, baseline=.baseline, adverse=.adverse, severe=.severe, bhc=.bhc,
				start_forecast=.start_forecast,
				actual_union=.actual_union, backtesting_union=.backtesting_union);
		
		return(.agg);
	}
}

Add_Aggreated_Forecasts <- function(forecast1,forecast2) {
#	forecast1 <- forecasts;
#	forecast2 <- Aggregate_Forecasts(forecasts_raw,TRUE);
#	for (i in c(1:8,10:11)) {
	for (i in c(1:8)) {
		#		i <- 10;
		forecast1[[i]] <- forecast1[[i]] + forecast2[[i]];
	}
	forecast1;
}

#create dummy variables
add_dummy <- function(.Macro, dates, dummy_name) {
#	.Macro <- Macro;
	if(dummy_name%in%colnames(.Macro)) {
		return(.Macro);
	}
	dummy <- .Macro[,1:2];
	dummy[,2] <- 0;
	dummy[as.character(dummy[,1])%in%dates,2]<-1;
	.Macro <- cbind(.Macro,dummy[,2]);
	colnames(.Macro)[ncol(.Macro)] <- dummy_name;
	.Macro;
}


#add variables
add_variable <- function(.Macro, values, variable_name) {
#	.Macro <- determine_Macro(tmp_revenue[,1], Macro, Macro_Q);
#	values <- baseline_values;
	if(variable_name%in%colnames(.Macro)) {
		return(.Macro);
	}
	.start <- which(.Macro[,1]==values[1,1]);
	.tmp <- .Macro[,1:2];
	.tmp[,2] <- NA;
	
	.tmp[.start:(.start+nrow(values)-1),2] <- values[,2]
	.Macro <- cbind(.Macro,.tmp[,2]);
	colnames(.Macro)[ncol(.Macro)] <- variable_name;
	.Macro;
}


###local function archived function
#Get_Forecast <- function(revenue_lob, modelset, Output_file, output=TRUE, Macro=Macro,Macro_2=Macro_2,Macro_3=Macro_3,Macro_4=Macro_4,Macro_Q=Macro_Q,Macro_2_Q=Macro_2_Q,Macro_3_Q=Macro_3_Q,Macro_4_Q=Macro_4_Q) {
##	output <- FALSE
#	Forecasts <- list();
#	for (j in 2:ncol(modelset)) {
##		j <-2;
#		#get revenue
#		revenue <- extract_data_from_revenues(revenue_lob,colnames(modelset)[j])
#		revenue <- clean_na_data_frame(revenue);
#		
#		#get variable names
#		.variables_string <- as.character(unlist(modelset[,j]));
#		print(.variables_string)
#		variables <- unlist(strsplit(.variables_string,","));
#		variables <- trim(variables);
#		
#		if (variables[1]=="Movingaverage") {
#			#MV function
#			#revenue, LookBackPeriods, LookBackPeriods_Q
#			start_and_frequency <- determine_frequency_and_start(revenue[,1]);
#			.frequency <- start_and_frequency$frequency;
#			.start <- start_and_frequency$start;
#			
#			historical_data <- ts(revenue[,2],start=.start,frequency=.frequency);
#			
#			.lookback <- as.numeric(variables[2]);
#			
#			if (.frequency==12) {
#				.forecastperiod <- 60;
#			} else if(.frequency==4) {
#				.forecastperiod <- 20;
#			} else {
#				stop("Error: Look Back Periods");
#			}
#			
#			forecasts <- forecast_ts_mv(revenue,.lookback,.forecastperiod);
#			
#			if (output) {
#				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".png",sep="");			
#				title_name <- paste(colnames(modelset)[j], "Moving Average Model (",paste(.lookback,collapse ="/"),")",sep="");
#				
#				plot_mv_model(forecasts, plot_name, title_name, .lookback);
#				
#				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".csv",sep="");
#				write_model_forecast_to_csv(forecasts, file_name);
#				
#				##Plot the annual growth rate table
#				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Table.png",sep="");
#				png(plot_name,width=800,height=100);
#				plot_table_with_title(get_notation(forecasts$actual_forecast)$Table,colnames(modelset)[j]);
#				dev.off();
#				
#				#plot backtest statistic
#				Comparison <- actual_backtest_comparison(forecasts)
#				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest.png",sep="");
#				png(plot_name,width=800,height=30*nrow(Comparison));
#				plot_table_with_title(Comparison,colnames(modelset)[j]);
#				dev.off();
#			}
#			#add to info session
#			Forecasts[[j]] <- forecasts;
#		} else if(variables[1]=="SeasonalDecomposition") {
#			#MV function
#			#revenue, LookBackPeriods, LookBackPeriods_Q
#			start_and_frequency <- determine_frequency_and_start(revenue[,1]);
#			.frequency <- start_and_frequency$frequency;
#			.start <- start_and_frequency$start;
#			
#			historical_data <- ts(revenue[,2],start=.start,frequency=.frequency);
#			revenue_name <- colnames(revenue)[2];
#			
#			if (length(variables)>1) {
#				historical_data <- window(historical_data,as.numeric(c(variables[2],variables[3])));
#			}
#			forecasts <- forecast_ts_seasonalmodel(historical_data);
#			
#			if (output) {
#				#plot regression
#				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".png",sep="");
#				title_name <- paste(BU,"-",.info[1,1],"-",colnames(modelset)[j],sep="");
#				plot_regression_model(forecasts, plot_name, title_name);
#				
#				##Plot the annual growth rate table
#				Comparison <- plot_comparison_table_regression_model(forecasts);
#				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Table.png",sep="");
#				png(plot_name,width=800,height=30*nrow(Comparison));
#				plot_table_with_title(Comparison,colnames(modelset)[j]);
#				dev.off();
#				
#				#backtest table
#				comp.tmp <- compare_time_series(historical_data, forecasts$backtesting, 1);
#				Comparison <- cbind(comp.tmp$Comp_Present,format_percentage((comp.tmp$Comp_Orig[,2]-comp.tmp$Comp_Orig[,1])/comp.tmp$Comp_Orig[,1]));
#				colnames(Comparison) <- c("Actual","Backtest","(B-A)/A");
#				
#				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest.png",sep="");
#				png(plot_name,width=800,height=30*nrow(Comparison));
#				grid.table(Comparison);
#				dev.off();
#				
#				#plot Diagnostic 1
#				model <- stl(historical_data,"periodic");
#				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Diagnostic1.png",sep="");
#				png(plot_name,width=800,height=600);
#				plot(model, main=paste(revenue_name,"Seasonality Decomposition Model - STL Model"));
#				title(sub=model$call);
#				dev.off();
#				
#				#plot Diagnostic 2
#				m1 <- detect_seasonality_method1(historical_data);
#				m2 <- detect_seasonality_method2(historical_data);
#				
#				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-SeasonalTestResults.csv",sep="");
#				Results <- data.frame(ETSResult=m1$Result,ETSStatistic=m1$Statistic,TBATSResult=m2$Result,TBATStatistic=m2$Statistic);
#				write.csv(Results,file_name);
#				
#				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Diagnostic2.png",sep="");
#				png(plot_name,width=800,height=35*nrow(Results));
#				grid.table(Results);
#				dev.off();
#			}
#			Forecasts[[j]] <- forecasts;
#			
#		} else if (variables[1]=="DiffRegression") {
#			variables <- variables[-1]
#			regressor_name <- variables[2:length(variables)];
#			regressor <- Macro[,c(1,which(colnames(Macro)%in%variables))];
#			regressor <- regressor[regressor[,1]%in%revenue[,1],];
#			
#			####run regression on difference		
#			#transform data difference
#			revenue_diff <- level_to_diff(revenue);
#			revenue_ts_diff <- revenue_to_ts(revenue_diff);
#			regressor_diff <- level_to_diff(regressor);
#			
#			data <- cbind(revenue_diff,regressor_diff)[,-3];
#			
#			regression_data <- clean_na_data_frame(data);
#			if (length(variables)==1) {
#				colnames(regression_data)[3] <- variables;
#			}
#			
##		adf1 <- round(adf.test(na.omit(data[,2]),k=0)$p.value,digit=2);
##		adf2 <- round(adf.test(na.omit(data[,3]),k=0)$p.value,digit=2);
#			fit <- fit_regression(data);
#			forecasts <- get_forecasts_diff(fit,regression_data,data,revenue,variables);
#			
#			if (output) {
#				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".csv",sep="");
#				write_model_forecast_to_csv(forecasts, file_name);
#				
#				#write original data to output
#				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-forecastdata.csv",sep="");
#				write.csv(forecasts$forecast_data,file_name);
#				
#				#backtesting of differenced value
#				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".png",sep="");
#				title_name <- paste("Backtest and Forecast ",BU,"/",colnames(revenue)[2]," V.S. ",paste(regressor_name,collapse="-")," /R^2=",round(summary(fit)$r.squared,2),sep="");
#				plot_regression_model(forecasts, plot_name, title_name);				
#			}
#			forecasts <- convert_forecasts_diff_to_level(forecasts, revenue, data)
#			if (output) {
#				#backtesting of level value
#				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Diagnostic1.png",sep="");
#				title_name <- paste("Backtest Level Value ",BU,"/",colnames(revenue)[2]," V.S. ",paste(regressor_name,collapse="/")," /R^2=",round(summary(fit)$r.squared,2),sep="")
#				plot_regression_model(forecasts, plot_name, title_name, TRUE);
#				
#				##Plot the annual growth rate table
#				Comparison <- plot_comparison_table_regression_model(forecasts);
#				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Table.png",sep="");
#				png(plot_name,width=800,height=30*nrow(Comparison));
#				plot_table_with_title(Comparison,colnames(modelset)[j]);
#				dev.off();
#				
#				#plot backtest statistic
#				Comparison <- actual_backtest_comparison(forecasts);
#				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest.png",sep="");
#				png(plot_name,width=800,height=30*nrow(Comparison));
#				plot_table_with_title(Comparison,colnames(modelset)[j]);
#				dev.off();
#			}
#			#add to info session
#			metric_model <- grepl("Metric",colnames(modelset)[j]);
#			
#			if(metric_model) {
#				implied_yield <- calculate_implied_yield(revenue_lob,colnames(modelset)[j],forecasts);
#				
#				forecasts$backtest_forecast <- forecasts$backtest_forecast*implied_yield
#				forecasts$actual_forecast <- forecasts$actual_forecast*implied_yield
#				forecasts$actual <- forecasts$actual*implied_yield
#				forecasts$backtesting <- forecasts$backtesting*implied_yield
#				forecasts$baseline <- forecasts$baseline*implied_yield
#				forecasts$adverse <- forecasts$adverse*implied_yield
#				forecasts$severe <- forecasts$severe*implied_yield
#				forecasts$bhc <- forecasts$bhc*implied_yield
#				
#				if (output) {
#					file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-ConvertedRevenue.csv",sep="");
#					write_model_forecast_to_csv(forecasts, file_name);
#					
#					#write original data to output
#					#				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-data-ConvertedRevenue.csv",sep="");
#					#				write.csv(forecasts$forecast_data,file_name);
#					
#					#plot regression
#					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-ConvertedRevenue.png",sep="");
#					title_name <- paste(BU,"-",.info[1,1],"-",colnames(modelset)[j],"    R^2=",round(summary(fit)$r.squared,2), "-ConvertedRevenue",sep="");
#					plot_regression_model(forecasts, plot_name, title_name);
#					
#					##Plot the annual growth rate table
#					Comparison <- plot_comparison_table_regression_model(forecasts);
#					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Table-ConvertedRevenue.png",sep="");
#					png(plot_name,width=800,height=30*nrow(Comparison));
#					plot_table_with_title(Comparison,paste(colnames(modelset)[j], "- Converted Revenue"));
#					dev.off();
#					
#					#plot backtest statistic
#					Comparison <- actual_backtest_comparison(forecasts);
#					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest-ConvertedRevenue.png",sep="");
#					png(plot_name,width=800,height=30*nrow(Comparison));
#					plot_table_with_title(Comparison,paste(colnames(modelset)[j],"- Converted Revenue"));
#					dev.off();
#				}
#			}
#			Forecasts[[j]] <- forecasts;
#		} else if (variables[1]=="ARIMAX") {
#			ar <- as.numeric(variables[2]);
#			d <- as.numeric(variables[3]);
#			ma <- as.numeric(variables[4]);
#			variables <- variables[5];
#			
#			revenue_name <- colnames(revenue)[2];
#			if (output) {
#				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Diagnostic1.png",sep="");
#				png(plot_name,width=800,height=600);
#				par(mfrow=c(2,2));
#				acf(revenue[,2],main=paste(revenue_name,": ACF of level value",sep=""));
#				pacf(revenue[,2],main=paste(revenue_name,": ACF of level value",sep=""));
#				acf(diff(revenue[,2]),main=paste(revenue_name,": ACF of First Order Differencing",sep=""));
#				pacf(diff(revenue[,2]),main=paste(revenue_name,": ACF of First Order Differencing",sep=""));
#				dev.off();
#			}
#			data <- compile_regression_data(determine_Macro(revenue[,1], Macro, Macro_Q),variables,revenue);
#			regression_data <- clean_na_data_frame(data);
#			
#			tryCatch({
#						fit <- arima(data[,2], order=c(ar,d,ma), xreg=data[,3]);
#						forecasts <- get_forecasts_arimax(fit,regression_data,data,revenue,variables);
#						if (output) {
#							plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".png",sep="");
#							title_name <- paste(BU,revenue_name,variables,"AR",ar,"D",d,"MA",ma,sep="-")
#							plot_regression_model(forecasts, plot_name, title_name);
#							
#							##Plot the annual growth rate table
#							Comparison <- plot_comparison_table_regression_model(forecasts);
#							plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Table.png",sep="");
#							png(plot_name,width=800,height=30*nrow(Comparison));
#							plot_table_with_title(Comparison,colnames(modelset)[j]);
#							dev.off();
#							
#							#plot backtest statistic
#							Comparison <- actual_backtest_comparison(forecasts);
#							plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest.png",sep="");
#							png(plot_name,width=800,height=30*nrow(Comparison));
#							plot_table_with_title(Comparison,colnames(modelset)[j]);
#							dev.off();
#						}
#						Forecasts[[j]] <- forecasts;
#					},error=function(e) {
#						title_name <- paste(BU,revenue_name,variables,"AR",ar,"D",d,"MA",ma,sep="-")
#						plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".png",sep="");
#						png(plot_name,width=800,height=600);
#						plot(0, main=title_name);
#						dev.off();
#					},finally={})
#		} else {
#			#combine data and delete missing values
#			data <- compile_regression_data(determine_Macro(revenue[,1], Macro, Macro_Q),variables,revenue);
#			if (output) {
#				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-regressiondata.csv",sep="");
#				write.csv(data, file_name);
#			}
#			regression_data <- clean_na_data_frame(data);
#			if (length(variables)==1) {
#				colnames(regression_data)[3] <- variables;
#			}
#			
#			#run regression
#			fit <- fit_regression(regression_data);
#			forecasts <- get_forecasts(fit,regression_data,data,revenue,variables);
#			
#			if (output) {
#				file_prefix <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),sep="");
#				Diagnostic_test_regression(regression_data,file_prefix);
#				
#				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".csv",sep="");
#				write_model_forecast_to_csv(forecasts, file_name);
#				
#				#write original data to output
#				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-forecastdata.csv",sep="");
#				write.csv(forecasts$forecast_data,file_name);
#				
#				#plot regression
#				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),".png",sep="");
#				title_name <- paste(BU,"-",.info[1,1],"-",colnames(modelset)[j],"    R^2=",round(summary(fit)$r.squared,2),sep="");
#				plot_regression_model(forecasts, plot_name, title_name);
#				
#				##Plot the annual growth rate table
#				Comparison <- plot_comparison_table_regression_model(forecasts);
#				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Table.png",sep="");
#				png(plot_name,width=800,height=30*nrow(Comparison));
#				plot_table_with_title(Comparison,colnames(modelset)[j]);
#				dev.off();
#				
#				#plot backtest statistic
#				Comparison <- actual_backtest_comparison(forecasts);
#				plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest.png",sep="");
#				png(plot_name,width=800,height=30*nrow(Comparison));
#				plot_table_with_title(Comparison,colnames(modelset)[j]);
#				dev.off();
#				
#				Output_file_outsample <- paste(Output_file,"/",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),sep="");
#				if (!file.exists(Output_file_outsample)) {
#					dir.create(Output_file_outsample);
#				}
#				
#				Out_of_Sample_Test(Output_file_outsample,regression_data);
#			}
#			
#			#add to info session
#			metric_model <- grepl("Metric",colnames(modelset)[j]);
#			
#			if(metric_model) {
##				revenue <- extract_data_from_revenues(revenue_lob,colnames(modelset)[j])
##				revenue <- clean_na_data_frame(revenue);
#				
#				implied_yield <- calculate_implied_yield(revenue_lob,colnames(modelset)[j],forecasts);
#				
#				forecasts$backtest_forecast <- forecasts$backtest_forecast*implied_yield
#				forecasts$actual_forecast <- forecasts$actual_forecast*implied_yield
#				forecasts$actual <- forecasts$actual*implied_yield
#				forecasts$backtesting <- forecasts$backtesting*implied_yield
#				forecasts$baseline <- forecasts$baseline*implied_yield
#				forecasts$adverse <- forecasts$adverse*implied_yield
#				forecasts$severe <- forecasts$severe*implied_yield
#				forecasts$bhc <- forecasts$bhc*implied_yield
#				
#				if (output) {
#					file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-ConvertedRevenue.csv",sep="");
#					write_model_forecast_to_csv(forecasts, file_name);
#					
#					#write original data to output
#					#				file_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-data-ConvertedRevenue.csv",sep="");
#					#				write.csv(forecasts$forecast_data,file_name);
#					
#					#plot regression
#					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-ConvertedRevenue.png",sep="");
#					title_name <- paste(BU,"-",.info[1,1],"-",colnames(modelset)[j],"    R^2=",round(summary(fit)$r.squared,2), "-ConvertedRevenue",sep="");
#					plot_regression_model(forecasts, plot_name, title_name);
#					
#					##Plot the annual growth rate table
#					Comparison <- plot_comparison_table_regression_model(forecasts);
#					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Table-ConvertedRevenue.png",sep="");
#					png(plot_name,width=800,height=30*nrow(Comparison));
#					plot_table_with_title(Comparison,paste(colnames(modelset)[j], "- Converted Revenue"));
#					dev.off();
#					
#					#plot backtest statistic
#					Comparison <- actual_backtest_comparison(forecasts);
#					plot_name <- paste(Output_file,"/forecast-",BU,"-",.info[1,1],"-",gsub("[.]","-",colnames(modelset)[j]),"-Backtest-ConvertedRevenue.png",sep="");
#					png(plot_name,width=800,height=30*nrow(Comparison));
#					plot_table_with_title(Comparison,paste(colnames(modelset)[j],"- Converted Revenue"));
#					dev.off();
#				}
#			}
#			Forecasts[[j]] <- forecasts;
#		}
#	}
#	Forecasts;
#}


