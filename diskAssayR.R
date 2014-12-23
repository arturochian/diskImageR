library(diversitree)
library(tcltk)
library(zoo)
library(Hmisc)
options(warn=-1)
se <- function(x) sqrt(var(x)/(length(x) - 1))
is.letter <- function(x) grepl("[[:alpha:]]", x)
is.number <- function(x) grepl("[[:digit:]]", x)

mydir <- function(){
	setwd(tk_choose.dir(default = "", caption = "Select main project directory"))}

#Base functions to read in the pictures and perform the imageJ analysis (background)
 load.data <- function(filename) {
	d <- read.csv(filename, header=TRUE, sep="\t")
   names(d) <- c("count", "distance","x")
   d
 }

readIn <- function(directoryPath, newList = list(), numDig=30) {
	currDir <- getwd()
	getData <- function(i, newList, names) {
		if (i > length(dir())){
			names(newList) <- names
			print(names(newList))
			setwd(currDir)
			return (newList)
			}
		else {
			allLines <-  aggregate(load.data(dir()[i])$x,  load.data(dir()[i])["distance"], mean)
			newList[[length(newList)+1L]] <-  data.frame(distance = allLines[,1]*40/length(allLines[,1]), x= allLines[,2])
			temp <- paste(substr(basename(dir()[i]),1,numDig), "", sep="")
			names[i] <- strsplit(temp,".txt")[[1]][1]
			getData(i+1, newList, names)
		}
	}
	setwd(directoryPath)
	i <-1
	names <- c()
	findMin <- c()
	getData(i, newList, names)
}

ReadIn_DirCreate <- function(workingDir, folderLoc, experAbbr){
    setwd(workingDir)
	tList <- list()
	tList <- readIn(folderLoc, tList, 30)
	len <- c()
		for (i in 1:length(tList)){
		len[i] <- length(tList[[i]][,1])
		}
	temp <- data.frame(names = names(tList), len)
	redo <- subset(temp, len==1, names)	
	newdir1 <- paste(workingDir, "/figures/", sep="")
#	newdir2 <- paste(workingDir, "/figures/", experAbbr, sep="")
	newdir3 <- paste(workingDir, "/parameter_files/", sep="")		
	if (!file.exists(newdir1)){
		dir.create(newdir1, showWarnings = FALSE)
		cat(paste("new directory: ", newdir1), sep="")
		}
	# if (!file.exists(newdir2)){		
		# dir.create(newdir2, showWarnings = FALSE)
		# print(paste("new directory: ", newdir2), sep="")
		# }
	if (!file.exists(newdir3)){		
		dir.create(newdir3, showWarnings = FALSE)
		cat(paste("new directory: ", newdir3), sep="")
		}
	tList
	}

#Run the imageJ analysis using tcltk interface
runImageJAnalysis <- function(projectName, discDiam = 6, imageJLoc="default"){
	projectFolder <- tk_choose.dir(caption = "Select main project folder") 
	inputFolder <- tk_choose.dir(caption = "Select location of photos")
	script <- tk_choose.files(caption = "Select the imageJ disc assay script")
	fileFolder <- paste(Sys.Date(), projectName, sep="_")
	outputFolder <- paste(projectFolder, "/imageJ-out/", fileFolder, "/", sep="")
	inputFolder2 <- paste(inputFolder, "/", sep="")
	IJarguments <- paste(inputFolder2, outputFolder, discDiam, sep="*")
	fileFolder <- paste(Sys.Date(), projectName, sep="_")

	if(length(dir(outputFolder)) > 0){
		stop("Output files already exist in specified folder.  Please delete existing files or change project name before continuing.")
	}
	
	dir.create(paste(projectFolder, "/imageJ-out/", sep=""), showWarnings=FALSE)
	dir.create(paste(outputFolder, sep=""), showWarnings= FALSE)
	dir.create(paste(projectFolder, "/figures/", sep=""), showWarnings= FALSE)
	dir.create(paste(projectFolder, "/parameter_files/", sep=""), showWarnings=FALSE)
			
	if (imageJLoc=="default" | imageJLoc=="loc2" ){
		if (imageJLoc=="loc2"){
			call <- paste("/Applications/ImageJ/ImageJ.app/Contents/MacOS/JavaApplicationStub -batch", script, IJarguments, sep=" ")}
		if (imageJLoc=="default"){
			call <- paste("/Applications/ImageJ.app/Contents/MacOS/JavaApplicationStub -batch", script, IJarguments, sep=" ")}
	}
	else {call <- paste(imageJLoc,  "-batch", script, IJarguments, sep=" ")
		}
		
	system(call)

	cat(paste("\nOutput saved in folder: ", fileFolder, "\n", sep=""))
	cat(paste("\nElements in dataframe ", projectName, ": \n", sep=""))	
	temp <- ReadIn_DirCreate(projectFolder, outputFolder, projectName)
	cat("\a")
	assign(projectName, temp, envir=globalenv())
	}

#To manually input the locations of projectFolder, inputFolder and imageJ script
runIJAnalysisManual <- function(projectName, projectFolder, inputFolder, script, discDiam = 6, imageJLoc="default"){
	fileFolder <- paste(Sys.Date(), projectName, sep="_")
	outputFolder <- paste(projectFolder, "imageJ-out/", fileFolder, "/", sep="")
	IJarguments <- paste(inputFolder, outputFolder, discDiam, sep="*")
	
	if(length(dir(outputFolder)) > 0){
		stop("Output files already exist in specified folder.  Please delete existing files or change project name before continuing.")
	}
	
	dir.create(paste(projectFolder, "/imageJ-out/", sep=""), showWarnings=FALSE)
	dir.create(paste(outputFolder, sep=""), showWarnings= FALSE)
	dir.create(paste(projectFolder, "/Figures/", sep=""), showWarnings= FALSE)
		
	if (imageJLoc=="default" | imageJLoc=="loc2" ){
		if (imageJLoc=="loc2"){
			call <- paste("/Applications/ImageJ/ImageJ.app/Contents/MacOS/JavaApplicationStub -batch", script, IJarguments, sep=" ")}
		if (imageJLoc=="default"){
			call <- paste("/Applications/ImageJ.app/Contents/MacOS/JavaApplicationStub -batch", script, IJarguments, sep=" ")}
	}
	else {call <- paste(imageJLoc,  "-batch", script, IJarguments, sep=" ")
		}

	system(call)

	fileFolder <- paste(Sys.Date(), projectName, sep="_")
	cat(paste("\nOutput of imageJ analyses saved in folder: ", fileFolder, "\n", sep=""))
	cat(paste("\nElements in dataframe ", projectName, ": \n", sep=""))	
	temp <- ReadIn_DirCreate(projectFolder, outputFolder, projectName)
	cat("\a")
	assign(projectName, temp, envir=globalenv())
	}

#Read in existing IJ analysis
readInExisting <- function(newList = list(), numDig=30) {
	curDir <- getwd()
	projectFolder <- tk_choose.dir(caption = "Select main project folder") 
	directoryPath <- tk_choose.dir(default = "", caption = "Select directory with ImageJ output")
	setwd(directoryPath)
	getData <- function(i, newList, names) {
		if (i > length(dir())){
			names(newList) <- names
			cat(paste("\nElements in dataframe:  \n", sep=""))	
			print(names(newList))
			setwd(projectFolder)
			cat("\a")	
			return (newList)
			}
		else {
			allLines <-  aggregate(load.data(dir()[i])$x,  load.data(dir()[i])["distance"], mean)
			newList[[length(newList)+1L]] <-  data.frame(distance = allLines[,1]*40/length(allLines[,1]), x= allLines[,2])
			temp <- paste(substr(basename(dir()[i]),1,numDig), "", sep="")
			names[i] <- strsplit(temp,".txt")[[1]][1]
			getData(i+1, newList, names)
		}
	}
	i <-1
	names <- c()
	findMin <- c()
	getData(i, newList, names)
}


############################################
#FUNCTIONS REQUIRED FOR plotRaw
#[discplot1rep] A single plot with 1 overlay
############################################
discplotNoRep <- function(rep1,  label=label, ymin=0, ymax=250, xmin=0, maxDist=40, standardLoc = 2.5, cexPt = 0.6, xaxt="n", yaxt="n", col1="black", stand=0, plotStandardLoc =FALSE){
	plot(rep1[,1], rep1[,2]+stand, ylim=c(ymin, ymax), xlim=c(xmin, maxDist), xaxt=xaxt, yaxt="n", cex=cexPt, col=col1)
	if (yaxt=="s"){
		axis(2, las=2)}
	axis(1, labels=FALSE, at=c(0, 10, 20, 30, 40))
	axis(2, labels=FALSE)
	mtext(label, side=3, cex=0.6)
	if(plotStandardLoc){
		abline(v= standardLoc, lty=2, col="red")
		}
	}
	
plotRaw <-function(projectName,  standardLoc = 2.5, ymin = 0, ymax=200, xmin = 0, maxDist = 40, dotedge = 3.4, xplots = 6, height =8, width = 8, cexX = 0.8, cexPt = 0.6, cexY = 0.8, nameVector = TRUE , plotDot = TRUE, plotStandardLoc=TRUE, showNum=FALSE, popUp = TRUE, overwrite=TRUE, stand=TRUE){
	fileName <- paste(Sys.Date(), "_", projectName,"/", sep="")
	dir.create(paste("figures/", fileName,  sep=""), showWarnings = TRUE)
	t <- paste("figures/", fileName, projectName, "_raw.pdf", sep="")
	if (!overwrite){
		if (file.exists(t)){
			t <- paste("figures/", fileName, projectName, "_raw_2_sL=", standardLoc, "_dE=", dotedge, ".pdf", sep="")
			if (file.exists(t)){
				k <- 2
				while(file.exists(t)){
					k <- k+1
					t <- paste("figures/", fileName, projectName, "_raw_", k, "sL=", standardLoc, "_dE=", dotedge, ".pdf", sep="")
					}
				}
			}
		}
	data <- eval(parse(text=projectName))
	dotMax <- max(sapply(data, function(x) {x[which(x[,1] > standardLoc)[1], 2]})) 		
	standards <-c( sapply(data, function(x) {dotMax-x[which(x[,1] > standardLoc)[1], 2]}))	
	convert <- unlist(lapply(data, function(x) 40/length(x[,1])))
	if (is.logical(nameVector)){
		if (nameVector){label <- names(data)}		
		else {label <- rep("", length(data))}
		}
	else {label <- nameVector}

	if (xplots > length(data)){
		xplots <- length(data)
		}
	if (ceiling(length(data)/xplots) < 6) {
		yplots<- ceiling(length(data)/xplots)}
	else {yplots<- 6}
	numpages <- ceiling(length(data)/(xplots*yplots))
	pdf(t, width=width, height=height)
	par(mfrow=c(yplots , xplots), mar=c(1,1,1,1), oma=c(4,5,1,1))
		for (i in 1:length(data)){
			if(stand){
				discplotNoRep(data[[i]], label[i], ymin=ymin, ymax=ymax, xmin=xmin, maxDist=maxDist, stand=standards[i], standardLoc = standardLoc, cexPt = cexPt, plotStandardLoc = plotStandardLoc)
				}
			if(!stand){
				discplotNoRep(data[[i]], label[i], ymin=ymin, ymax=ymax, xmin=xmin, maxDist=maxDist, standardLoc = standardLoc, cexPt = cexPt, plotStandardLoc = plotStandardLoc)
				}
			if(numpages == 1){
				if (i >= xplots*yplots-xplots+1){
					axis(1, cex.axis=cexX, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
					}
				}
			if(numpages == 2){
				if (i >= xplots*yplots-xplots+1 & i < xplots*yplots+1){
					axis(1, cex.axis=cexX, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
					}
				if (i >= 2*xplots*yplots-xplots+1){
					axis(1, cex.axis=cexX, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
					}
				}				
			if(numpages == 3){
				if (i >= xplots*yplots-xplots+1 & i < xplots*yplots+1){
					axis(1, cex.axis=cexX, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
					}
				if (i >= 2*xplots*yplots-xplots+1 & i < 2*xplots*yplots+1){
					axis(1, cex.axis=cexX, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
					}
				if (i >= (length(data)-xplots)){
					axis(1, cex.axis=cexX, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
					}
				}				

			k <- 1
			while (k <= numpages){
				if (i %in% seq(1, k*yplots*xplots, by=xplots)) {axis(2, cex.axis=cexY, las=2)}
					k <- k+1}


			if (plotDot) {abline(v=dotedge+0.5, lty=2)}
			if(showNum){
				text(maxDist*0.95, maxDist*0.95, i)
			}
		}
	
	mtext("Distance from disc center (mm)", side= 1, outer=TRUE, line=2)
	mtext("Pixel intensity", side=2, outer=TRUE, line=2)
	dev.off()
	cat(paste("\nSaving figure: ", t, sep=""))
	if(popUp){
	tt <- paste("open ",t)
	system(tt)
	}
	}

############################################
#FUNCTIONS REQUIRED FOR plotRaw
#[discplot1rep] A single plot with 1 overlay
#plot the raw data of a subset of the data overtop each other (different colours)
############################################
discplotRep <- function(subList,  standardLoc = 2.5, spline = TRUE,  spar = 0.4, label = "", label2 = "", ymin = 0, ymax = 250, dotedge = 3.4, maxDist = 35,  xaxt = TRUE, yaxt = TRUE, yaxt.lab = TRUE, xaxt.lab = TRUE, colours = "black",  legend = FALSE, legendLoc = "bottomright"){
	if (length(colours) == 1){
			colours <- rep(colours, length(subList))
			}
	if (length(colours) == length(subList)){
		colours <- colours
		}
	if (length(colours) %/% length(subList) == 0){
		colours <- rep(colours, length(colours) %% length(subList))
		}
	if (length(colours) %% length(subList) != 0) {
		cat("The length of the given colours vector is not the same length or divisible by the length of the sublist.  All lines will be plotted in black")
		colours <- rep("black", length(subList))
		}
	if (!is.logical(standardLoc)){
		dotIntensity <- c()
		startX <- c()
		stopX <- c()
		if (length(subList)>1){
			for (i in 1:length(subList)){
				dotIntensity[i] <- subList[[i]][,2][which(subList[[i]][,1] > standardLoc)[1]]
				startX[i] <- which(subList[[i]][,1] > dotedge)[1]
				stopX[i] <- which(subList[[i]][,1] > maxDist)[1]
			}
			stand <- dotIntensity-min(dotIntensity)
		}
		else {
			dotIntensity <- subList[[1]][,2][which(subList[[1]][,1] > standardLoc)[1]]
			startX <- which(subList[[1]][,1] > dotedge)[1]
			stopX <- which(subList[[1]][,1] > maxDist)[1]
			stand <- 0
			}	
	}
	else {
		dotIntensity <- c()
		startX <- c()
		stopX <- c()
		for (i in 1:length(subList)){
			dotIntensity[i] <- subList[[i]][,2][which(subList[[i]][,1] > standardLoc)[1]]
			startX[i] <- which(subList[[i]][,1] > dotedge)[1]
			stopX[i] <- which(subList[[i]][,1] > maxDist)[1]
		}
		stand <-rep(0, length(subList))
	}
 	
 	if (spline){
 		yvals <- subList[[1]][startX[1]:stopX[1],2]-stand[1]
 		yvals[yvals < 0] <- 0
	 	plot(smooth.spline(subList[[1]][startX[1]:stopX[1],1], yvals , spar=spar), ylim=c(ymin, ymax), xlim=c(dotedge, maxDist), xaxt="n", yaxt="n", cex=0.6, col=colours[1],  type="l", lwd=3, xlab="", ylab="")
		if(length(subList) > 1){
			for (i in 2:length(subList)){
				yvals2 <- subList[[i]][startX[i]:stopX[i], 2]-stand[i]
				yvals2[yvals2 < 0] <- 0
				points(smooth.spline(subList[[i]][startX[i]:stopX[i],1], yvals2,spar=spar), col=colours[i], cex=0.6, type="l", lwd=3)
			}
		}
	}	
 	else{
 		plot(subList[[1]][,1], subList[[1]][,2]-stand[1], ylim=c(ymin, ymax), xlim=c(dotedge, maxDist), xaxt="n", yaxt="n", cex=0.6, col=colours[1], type="l", lwd=3, xlab="", ylab="")
	 	for (i in 2:length(subList)){
			points(subList[[i]][,1], subList[[i]][,2]-stand[i], col=colours[i], cex=0.6, type="l", lwd=3)
		}
	}
 	
 	if (yaxt){
 		axis(2, las=2, cex.axis=1.2)
 		if (yaxt.lab){
	 		mtext("pixel intensity", side=2, line=3, outer=FALSE, cex=1.2)
		}
	}
	else{axis(2, labels=FALSE)}
 	if (xaxt){
 		axis(1, cex.axis=1.2)
		if (xaxt.lab){
	 		mtext("distance from center of disc (mm)", side=1, line=2.5, outer=FALSE, cex=1.2)
	 	}
	 }
	else{ 	axis(1, labels=FALSE)}

 	text(dotedge+dotedge*0.05, ymax*0.95, label, cex=1.2, font=2, pos=4)
 	text(dotedge+dotedge*0.05, ymax*0.85, label2, cex=1.2, pos=4)
	if (legend == TRUE) {
		legend(legendLoc, legend = names(subList), lwd = 3, col = colours)
		}
}


##################################################################
#FUNCTIONS REQUIRED FOR maxLik (background)
# [getstats] fits a logistic equation to a list (=data) using diversitree/Otto methics
# [curve] is the function that underlies the ML fit
# [plotRawMLCurve] plots the raw data with the ML curve on top
# [maxLik] applies getstats, plots the raw data and fits the ML curve to it
##################################################################
getstatsLog <- function(i, data, stand, dotedge=dotedge, maxDist=maxDist, maxSlope=20){
	cat(".")
	startX <- which(data[[i]][,1] > dotedge+0.5)[1]
	stopX <- which(data[[i]][,1] > maxDist - 0.5)[1]
	data[[i]] <- data[[i]][startX:stopX, 1:2]
	data[[i]] <- subset(data[[i]], x != "NA")
	data[[i]]$x <- data[[i]]$x+ stand[i] -min(data[[i]]$x+stand[i])  #the micel only fits when it goes down to 0
	data[[i]]$distance <- data[[i]]$distance - (dotedge+0.5)
	data[[i]]$distance <- log(data[[i]]$distance)
	sumsquares.fit <- function(theta){
		asym<-theta[[1]]
		ic50<-theta[[2]]
		scal<-theta[[3]]
		sigma<-theta[[4]]
		y<-data[[i]]$x
		x<-data[[i]]$distance
		res <- dnorm(y, (asym*exp(scal*(x-ic50))/(1+exp(scal*(x-ic50)))), sigma, log= T) 
		sum(res)
	}
	#asyms <- lapply(data, function(x) min(x$x))
	lowIC <- min(data[[i]]$x)
	highIC <- quantile(data[[i]]$x, 0.99)
#	highIC <- max(data[[i]]$x)
	lower <- c(highIC*0.8, 0, 0,0)
	upper <- c(highIC, max(data[[i]]$distance), maxSlope,maxSlope)
		
	par.tryA <-c(asym = 0.9*highIC, ic50 = log(maxDist)/4, scal = maxSlope*0.01, sigma =  0.2)
	par.tryB<-c(asym = 0.9*highIC, ic50 = log(maxDist)/4, scal = maxSlope*0.1, sigma = 0.2)
	par.tryC<-c(asym = 0.9*highIC,ic50 = log(maxDist)/2, scal =  maxSlope*0.01, sigma = 0.1)
	par.tryD<-c(asym = 0.9*highIC,ic50 = log(maxDist)/2, scal = maxSlope*0.1, sigma = 0.1)		

	mlpoint<-c()
	mlpointA<-find.mle(sumsquares.fit,par.tryA, method="subplex",upper=upper,lower=lower)
	mlpointB<-find.mle(sumsquares.fit,par.tryB,method="subplex",upper=upper,lower=lower)
	mlpointC<-find.mle(sumsquares.fit,par.tryC,method="subplex",upper=upper,lower=lower)
	mlpointD<-find.mle(sumsquares.fit,par.tryD,method="subplex",upper=upper,lower=lower)

	mlpoint <- if (mlpointA$lnLik>mlpointB$lnLik) mlpointA else mlpointB
	mlpoint <- if (mlpointC$lnLik>mlpoint$lnLik) mlpointC else mlpoint
	mlpoint <- if (mlpointD$lnLik>mlpoint$lnLik) mlpointD else mlpoint
	mlpoint
}

getstats2curve <- function(i, data, stand, dotedge=dotedge, maxDist=maxDist, maxSlope=20){
	cat(".")
	startX <- which(data[[i]][,1] > dotedge+0.5)[1]
	stopX <- which(data[[i]][,1] > maxDist - 0.5)[1]
	data[[i]] <- data[[i]][startX:stopX, 1:2]
	data[[i]] <- subset(data[[i]], x != "NA")
	data[[i]]$x <- data[[i]]$x+ stand[i] -min(data[[i]]$x+stand[i])  #the micel only fits when it goes down to 0
	data[[i]]$distance <- data[[i]]$distance - (dotedge+0.5)
	data[[i]]$distance <- log(data[[i]]$distance)
	sumsquares.fit <- function(theta){
		asym<-theta[[1]]
		od50<-theta[[2]]
		scal<-theta[[3]]
		sigma<-theta[[4]]
		asymB<-theta[[5]]
		od50B<-theta[[6]]
		scalB<-theta[[7]]
		y<-data[[i]]$x
		x<-data[[i]]$distance
		res <- dnorm(y, (asym*exp(scal*(x-od50))/(1+exp(scal*(x-od50)))+asymB*exp(scalB*(x-od50B))/(1+exp(scalB*(x-od50B)))), sigma, log= T) 
		sum(res)
	}
    #asyms <- lapply(data, function(x) min(x$x))
	lowOD <- min(data[[i]]$x)
	highOD <- quantile(data[[i]]$x, 0.99)
	lower <- c(0, 0, 0,0, 0, 0, 0)
	upper <- c(highOD, log(maxDist), maxSlope, 10, highOD,  log(maxDist), maxSlope)

#	upper <- c(highOD,  log(maxDist), maxSlope, 10, highOD,  log(maxDist), maxSlope)
	
	#This is conservative, keeping them symmetric 	
	par.tryA <-c(asym = 0.9*highOD, od50 = log(maxDist)/4, scal = maxSlope*0.01, sigma =  0.2, asymB = 0.9*highOD, od50B = log(maxDist)/4, scalB = maxSlope*0.01)
	par.tryB <-c(asym = 0.9*highOD, od50 = log(maxDist)/4, scal = maxSlope*0.1, sigma =  0.2, asymB = 0.9*highOD, od50B = log(maxDist)/4, scalB = maxSlope*0.1)
	par.tryC<-c(asym = 0.9*highOD,od50 = log(maxDist)/2, scal =  maxSlope*0.01, sigma = 0.1, asymB = 0.9*highOD,od50B = log(maxDist)/2, scal =  maxSlope*0.01)
	par.tryD<-c(asym = 0.9*highOD,od50 = log(maxDist)/2, scal =  maxSlope*0.1, sigma = 0.1, asymB = 0.9*highOD,od50B = log(maxDist)/2, scalB =  maxSlope*0.1)
	#Change asym and od50 
	par.tryE <-c(asym = 0.5*highOD, od50 =  log(maxDist)/4, scal = maxSlope*0.01, sigma =  0.2, asymB = 0.7*highOD, od50B =  log(maxDist)/2, scalB = maxSlope*0.01)
	par.tryF <-c(asym = 0.5*highOD, od50 =  log(maxDist)/4, scal = maxSlope*0.1, sigma =  0.2, asymB = 0.7*highOD, od50B =  log(maxDist)/2, scalB = maxSlope*0.01)
	par.tryG <-c(asym = 0.5*highOD, od50 =  log(maxDist)/4, scal = maxSlope*0.1, sigma =  0.2, asymB = 0.7*highOD, od50B =  log(maxDist)/2, scalB = maxSlope*0.1)
	par.tryH <-c(asym = 0.5*highOD, od50 =  log(maxDist)/4, scal = maxSlope*0.01, sigma =  0.2, asymB = 0.7*highOD, od50B =  log(maxDist)/2, scalB = maxSlope*0.01)
	
	mlpoint<-c()
	mlpointA<-find.mle(sumsquares.fit,par.tryA, method="subplex",upper=upper,lower=lower)
	mlpointB<-find.mle(sumsquares.fit,par.tryB,method="subplex",upper=upper,lower=lower)
	mlpointC<-find.mle(sumsquares.fit,par.tryC,method="subplex",upper=upper,lower=lower)
	mlpointD<-find.mle(sumsquares.fit,par.tryD,method="subplex",upper=upper,lower=lower)
	mlpointE<-find.mle(sumsquares.fit,par.tryE,method="subplex",upper=upper,lower=lower)
	mlpointF<-find.mle(sumsquares.fit,par.tryF,method="subplex",upper=upper,lower=lower)
	mlpointG<-find.mle(sumsquares.fit,par.tryG,method="subplex",upper=upper,lower=lower)	
	mlpointH<-find.mle(sumsquares.fit,par.tryH,method="subplex",upper=upper,lower=lower)	

	mlpoint <- if (mlpointA$lnLik>mlpointB$lnLik) mlpointA else mlpointB
	mlpoint <- if (mlpointC$lnLik>mlpoint$lnLik) mlpointC else mlpoint
	mlpoint <- if (mlpointD$lnLik>mlpoint$lnLik) mlpointD else mlpoint
	mlpoint <- if (mlpointE$lnLik>mlpoint$lnLik) mlpointE else mlpoint
	mlpoint <- if (mlpointF$lnLik>mlpoint$lnLik) mlpointF else mlpoint
	mlpoint <- if (mlpointG$lnLik>mlpoint$lnLik) mlpointG else mlpoint			
	mlpoint <- if (mlpointH$lnLik>mlpoint$lnLik) mlpointH else mlpoint			
	mlpoint
}


curve <-  function(asym, ic50,scal, x) {asym*exp(scal*(x-ic50))/(1+exp(scal*(x-ic50)))}
curve2 <- function(asym, od50, scal, asymB, od50B, scalB, x) { asym*exp(scal*(x-od50))/(1+exp(scal*(x-od50)))+asymB*exp(scalB*(x-od50B))/(1+exp(scalB*(x-od50B)))} 


getZOIheight <- function(data, i,  ML, ML2, stand= stand, clearHaloStand, ZOIcor = 1, dotedge = 3.4, maxDist = 35){
	cat(".")
	startX <- which(data[[i]][,1] > dotedge+0.5)[1]
	stopX <- which(data[[i]][,1] > maxDist - 0.5)[1]
	data[[i]] <- data[[i]][startX:stopX, 1:2]
	data[[i]]$x <- data[[i]]$x + stand[i] - clearHaloStand  
	data[[i]]$distance <- data[[i]]$distance - (dotedge+0.5)
	xx <- seq(log(data[[i]]$distance[1]), log(max(data[[i]][,1])), length=200) 
	yy <- curve2(ML2[[i]]$par[1], ML2[[i]]$par[2], ML2[[i]]$par[3], ML2[[i]]$par[5], ML2[[i]]$par[6], ML2[[i]]$par[7], xx) 
	return(yy[which.max(yy> yy[length(yy)]*0.95)]+min(data[[i]]$x))
	}

plotModelFit2 <- function(data, i, ML, ML2, stand,  clearHaloStand, ZOIcor = 1, ymax=150, dotedge = 3.4, maxDist= 35, label="", both=FALSE, plotCompon = FALSE, addZOIcor = FALSE){ 
	print(i)
	startX <- which(data[[i]][,1] > dotedge+0.5)[1]
	stopX <- which(data[[i]][,1] > maxDist - 0.5)[1]
	data[[i]] <- data[[i]][startX:stopX, 1:2]
	data[[i]]$x <- data[[i]]$x + stand[i] - clearHaloStand 
	data[[i]]$distance <- data[[i]]$distance - (dotedge+0.5)
	xx <- seq(log(data[[i]]$distance[1]), log(max(data[[i]][,1])), length=200) 
	yy2.1<- curve(ML2[[i]]$par[1], ML2[[i]]$par[2], ML2[[i]]$par[3],xx)
	yy2.2<- curve(ML2[[i]]$par[5], ML2[[i]]$par[6], ML2[[i]]$par[7],xx)	
	yy2<- curve2(ML2[[i]]$par[1], ML2[[i]]$par[2], ML2[[i]]$par[3], ML2[[i]]$par[5], ML2[[i]]$par[6], ML2[[i]]$par[7],xx)
	ploty <- data[[i]]$x*ZOIcor[i]
	ploty[ploty<0] <- 0
	yyplot <- (yy2+min(data[[i]]$x))*ZOIcor[i]
	slope <- ML[[i]]['par'][1]$par[3]

	if(slope > 1){
		plot(data[[i]]$distance, ploty, cex=0.7, col=grey(0.6), type="p", ylim=c(0, ymax), xlim=c(0, maxDist-dotedge), xaxt="n", yaxt="n", xlab="", ylab="")
		yyplot <- (yy2+min(data[[i]]$x))*ZOIcor[i]
		yyplot[yyplot < 0 ] <- 0
		points(exp(xx), yyplot, type="l", col="red", lwd=2, lty=2)	
		yhalo <- (yy2[which.max(yy2> yy2[length(yy2)] * 0.8)]+min(data[[i]]$x))*ZOIcor[i]
		if(addZOIcor){
			abline(h=yhalo, col="black", lwd=1, lty=2) 	
		}
		xhalo <- exp(xx[which.max(yy2> yy2[length(yy2)] * 0.8)])
		abline(v=xhalo, col="blue", lwd=1.5)  	

	}	
	
	if(slope < 1){
	 plot(data[[i]]$distance, ploty, cex=0.7, col=grey(0.6), type="p", ylim=c(0, ymax), xlim=c(0, maxDist-dotedge), xaxt="n", yaxt="n", xlab="", ylab="")

		yhalo <- (yy2[which.max(yy2> yy2[length(yy2)] * 0.8)]+min(data[[i]]$x))*ZOIcor[i]
		if(addZOIcor){
			abline(h=yhalo, col="black", lwd=1, lty=2) 	
		}
		abline(h=yhalo, col="red", lwd=2, lty=2) 		
	}
	print(plotCompon)
	if(plotCompon){
		print("here")
		yy1plot <- (yy2.1 +min(data[[i]]$x))*ZOIcor[i]
		yy1plot[yy1plot <0] <-0
		yy2plot <- (yy2.2 +min(data[[i]]$x))*ZOIcor[i]
		yy2plot[yy2plot <0] <-0
		points(exp(xx), yy1plot , type="l", col="orange", lwd=2, lty=2)	
		points(exp(xx), yy2plot, type="l", col="orange", lwd=2, lty=2)	
		}		

    axis(2, las=2, at=c(0, ymax*0.25, ymax*0.5, ymax*0.75, ymax), labels=FALSE)
    axis(1, labels=FALSE, at=c(0, 10, 20, 30, 40))
    mtext(label, side=3, cex=0.6)
}


plotML <- function(projectName, ML, ML2, stand, clearHaloStand, ZOIcor, dotedge=3.4, maxDist=35, ymax=150, plotCompon = FALSE, xplots=6, width= 7, height = 10, label = label, overwrite = TRUE, popUp = TRUE, legend=TRUE, addZOIcor = FALSE){

t <- paste("figures/", Sys.Date(), "_", projectName , "/", projectName, "_MLfit.pdf", sep="")

	if (!overwrite){
		if (file.exists(t)){
			t <- paste("figures/", Sys.Date(), "_", projectName , "/", projectName, "_MLfit_2_sL=", standardLoc, "_dE=", dotedge, ".pdf", sep="")
			if (file.exists(t)){
				k <- 2
				while(file.exists(t)){
					k <- k+1
					t <- paste("figures/", Sys.Date(), "_", projectName , "/", projectName, "_MLfit_", k, "_sL=", standardLoc, "_dE=", dotedge, ".pdf", sep="")
					}
				}
			}
		}

	data <- eval(parse(text=projectName))
	if(xplots > length(data)){
		xplots <- length(data)
	}
	
	if (ceiling(length(data)/xplots) < 6) {
		yplots<- ceiling(length(data)/xplots)}
	else {yplots<- 6}
	numpages <- ceiling(length(data)/(xplots*yplots))
	
	pdf(t, width=width, height=height)
	par(mfrow=c(yplots , xplots), mar=c(1,1,1,1), oma=c(4,5,1,1))
	for (k in 1:length(data)){
		plotModelFit2(data, ML=ML, ML2=ML2, stand=stand, clearHaloStand = clearHaloStand, ZOIcor = ZOIcor, i =k, dotedge=dotedge, maxDist=maxDist, ymax=ymax, label=label[k], plotCompon = plotCompon, addZOIcor = addZOIcor)			
		if(numpages == 1){
			if (k >= xplots*yplots-xplots+1){
				axis(1, cex.axis=0.8, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
				}
			}
		if(numpages == 2){
			if (k >= xplots*yplots-xplots+1 & k < xplots*yplots+1){
				axis(1, cex.axis=0.8, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
				}
			if (k >= 2*xplots*yplots-xplots+1){
				axis(1, cex.axis=0.8, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
				}
			}				
		if(numpages == 3){
			if (k >= xplots*yplots-xplots+1 & k < xplots*yplots+1){
				axis(1, cex.axis=0.8, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
				}
			if (k >= 2*xplots*yplots-xplots+1 & k < 2*xplots*yplots+1){
				axis(1, cex.axis=0.8, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
				}
			if (k >= (length(data)-xplots)){
				axis(1, cex.axis=0.8, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
				}
			}				

		j <- 1
		while (j <= numpages){
			if (k %in% seq(1, j*yplots*xplots, by=xplots)) {axis(2, cex.axis=0.8, at=c(0, ymax*0.25, ymax*0.5, ymax*0.75, ymax), las=2)}
			j <- j+1
			}
		}
	
	mtext("Distance from edge of disk (mm)", outer=TRUE, side=1, line=2, cex=1.2)
	mtext("Pixel density", outer=TRUE, side=2, line=1.5, cex=1.2)
	if(legend){
		if (plotCompon){
			legend("topleft", legend = c("data", "model", "zoi", "components"), col = c("grey", "red", "blue", "orange"), pch = c(19, NA, NA, NA), lwd= c(NA, 2, 2, 2), lty= c(NA, 1, 1, 2), bg="white", cex=0.8)
			}
		else {
			legend("topleft", legend = c("data", "model", "ZOI 50%", "ZOI 99%"), col = c("grey", "red", "red", "blue"), pch = c(19, NA, NA), lwd= c(NA, 1, 1, 2), lty= c(NA, 1, 1, 1), bg="white", cex=0.8)
			}
		}
	cat(paste("\n\tSaving figure: ", t, sep=""))
	dev.off()

	if(popUp){
		tt <- paste("open", t)
		system(tt)
	}
	}

singleAUC <- function(data, ML, ML2, stand, clearHaloStand, dotedge = 3.4, maxDist = 40, ymax = 200, percentileLow = 0.2,  ZOIcor = 0,  AUC=10, ZOI=10, i, label, showAUC = TRUE, showIC = TRUE, plotCompon=FALSE){
	cat(".")
	startX <- which(data[[i]][,1] > dotedge+0.5)[1]
	stopX <- which(data[[i]][,1] > maxDist - 0.5)[1]
	data[[i]] <- data[[i]][startX:stopX, 1:2]
	data[[i]]$x <- data[[i]]$x + stand[i] - clearHaloStand 
	data[[i]]$distance <- data[[i]]$distance - (dotedge+0.5)
	xx <- seq(log(data[[i]]$distance[1]), log(max(data[[i]][,1])), length=200) 	
	yy2.1<- curve(ML2[[i]]$par[1], ML2[[i]]$par[2], ML2[[i]]$par[3],xx)
	yy2.2<- curve(ML2[[i]]$par[5], ML2[[i]]$par[6], ML2[[i]]$par[7],xx)		
	yy<- curve2(ML2[[i]]$par[1], ML2[[i]]$par[2], ML2[[i]]$par[3], ML2[[i]]$par[5], ML2[[i]]$par[6], ML2[[i]]$par[7], xx) 
	#ZOI
	ploty <- data[[i]]$x
	ploty[ploty < 0] <-0
	slope <- ML[[i]]$par[3]
	ic50 <- ML[[i]]$par[2]	
#	asym <- (ML2[[i]]$par[1]+ML2[[i]]$par[5]+min(data[[i]]$x))*ZOIcor[i]
	asym <- (ML[[i]]$par[1]+min(data[[i]]$x))
	plot(data[[i]]$distance, ploty, cex=0.7, col=grey(0.4), type="p", ylim=c(0, ymax), xlim=c(0, maxDist -dotedge), xaxt="n", yaxt="n", xlab="", ylab="")
	yyplot <- (yy+min(data[[i]]$x))
	yyplot[yyplot < 0] <- 0
	points(exp(xx), yyplot, type="l", col="black", lwd=3)			

	###new 14.09.08###
	# yy80halo <- yyplot[which.max(yyplot> yyplot[length(yyplot)] * 0.2)]
	# yy50halo <- yyplot[which.max(yyplot> yyplot[length(yyplot)] * 0.5)]
	# yy10halo <- yyplot[which.max(yyplot> yyplot[length(yyplot)] * 0.9)]
	useAsym <- "TRUE"
	yy80halo <- yyplot[which.max(yyplot> asym * 0.2)]
	yy50halo <- yyplot[which.max(yyplot> asym * 0.5)]
	yy10halo <- yyplot[which.max(yyplot> asym * 0.9)]
	if(yy10halo < yy50halo){
		 yy10halo <- yyplot[which.max(yyplot> yyplot[length(yyplot)] * 0.9)]
		useAsym <- "FALSE"
	}

	xx <- seq(log(data[[i]]$distance[1]), log(max(data[[i]][,1])), length=200) 	
	
	###new 14.10.15###
	xx80 <- exp(xx[which.max(yyplot> asym * 0.2)])
	xx50 <- exp(xx[which.max(yyplot> asym * 0.5)])
	xx10 <- exp(xx[which.max(yyplot> asym * 0.9)])
	if(useAsym == "FALSE"){
		 xx10 <- exp(xx[which.max(yyplot> yyplot[length(yyplot)] * 0.9)])

	}

	if(AUC==50){
		xx <- exp(xx[1:which.max(exp(xx) > xx50)-1])	
		}
	if(AUC==80){
		xx <- exp(xx[1:which.max(exp(xx) > xx80)-1])	
		}
	if(AUC==10){
		xx <- exp(xx[1:which.max(exp(xx) > xx10)-1])	
		}

	if(length(xx)<1){
		xx <- seq(log(data[[i]]$distance[1]), log(max(data[[i]][,1])), length=200)
	}		
	
	yy<- curve2(ML2[[i]]$par[1], ML2[[i]]$par[2], ML2[[i]]$par[3], ML2[[i]]$par[5], ML2[[i]]$par[6], ML2[[i]]$par[7], log(xx)) 
	yy <- (yy+min(data[[i]]$x))
	yy[yy < 0] <- 0
	if (slope >1){
		xx2 <- c(xx[1], xx, xx[length(xx)])
		yy2 <- c(0, yy, 0)
		if(showAUC){
			polygon(xx2, yy2, density=15, col="red")
			}
		if(ZOI == 80){
			points(xx80, yy80halo, col="blue", cex=1.5, pch=19)
			}
		if(ZOI ==50){
			points(xx50, yy50halo, col="blue", cex=1.5, pch=19)
			}
		if(ZOI ==10){
			points(xx10, yy10halo, col="blue", cex=1.5, pch=19)
			}
		if(ZOI=="all"){
			points(xx80, yy80halo, col="yellow", cex=1.5, pch=19)
			points(xx50, yy50halo, col="purple", cex=1.5, pch=19)
			points(xx10, yy10halo, col="blue", cex=1.5, pch=19)
			}			
		}
		if(plotCompon){
		yy1plot <- (yy2.1 +min(data[[i]]$x))
		yy1plot[yy1plot <0] <-0
		yy2plot <- (yy2.2 +min(data[[i]]$x))
		yy2plot[yy2plot <0] <-0
		points(exp(xx), yy1plot , type="l", col="orange", lwd=2, lty=2)	
		points(exp(xx), yy2plot, type="l", col="orange", lwd=2, lty=2)	
		}			
	mtext(label, side=3, cex=0.6)
}

plotAUC <- function(projectName, ML , ML2, stand,  clearHaloStand, ZOIcor, standardLoc = 2.5, ymax=200, dotedge = 3.4, maxDist= 40, xplots = 4, percentileLow = 0.2, height = 10, width=7,  AUC=10, ZOI=10, overwrite = TRUE, popUp = TRUE, showAUC = TRUE, showIC = TRUE, label=label, plotCompon=FALSE){

	data <- eval(parse(text=projectName))

	fileFolder <- paste(Sys.Date(), projectName, sep="_")
	dir.create(paste(getwd(), "/figures/", sep=""), showWarnings= FALSE)
	dir.create(paste(getwd(), "/figures/", fileFolder, sep=""), showWarnings= FALSE)
	t <- paste("figures/", Sys.Date(), "_", projectName , "/", projectName, "_AUC.pdf", sep="")
	if (!overwrite){
		if (file.exists(t)){
			t <- paste("figures/", Sys.Date(), "_", projectName , "/", projectName, "_AUC_2_AUC", AUC, "_ZOI", ZOI, ".pdf", sep="")
			if (file.exists(t)){
				k <- 2
				while(file.exists(t)){
					k <- k+1
					t <- paste("figures/", Sys.Date(), "_", projectName , "/", projectName, "_AUC_", k, "_AUC", AUC, "_ZOI", ZOI, ".pdf", sep="")
					}
				}
			}
		}

	if(xplots > length(data)){
		xplots <- length(data)
	}
	if (ceiling(length(data)/xplots) < 6) {
		yplots<- ceiling(length(data)/xplots)}
	else {yplots<- 6}
	numpages <- ceiling(length(data)/(xplots*yplots))
	pdf(t, width=width, height=height)
	par(mfrow=c(yplots , xplots), mar=c(1,1,1,1), oma=c(4,5,1,1))
	for (k in 1:length(data)){
		singleAUC(data = data, ML = ML, ML2 = ML2, dotedge = dotedge, maxDist = maxDist, ymax = ymax, stand = stand, percentileLow = percentileLow, i = k, ZOIcor = ZOIcor, AUC=AUC, ZOI = ZOI, clearHaloStand = clearHaloStand, label=label[k], showAUC = showAUC, showIC = showIC, plotCompon=plotCompon)

		if(numpages == 1){
			if (k >= xplots*yplots-xplots+1){
				axis(1, cex.axis=0.8)
				}
			else {axis(1, cex.axis=0.8, labels= FALSE)}
			}
		if(numpages == 2){
			if (k >= xplots*yplots-xplots+1 & k < xplots*yplots+1){
				axis(1, cex.axis=0.8)
				}
			if (k >= 2*xplots*yplots-xplots+1){
				axis(1, cex.axis=0.8)
				}
			else {axis(1, cex.axis=0.8, labels= FALSE)}	
			}				
		if(numpages == 3){
			if (k >= xplots*yplots-xplots+1 & k < xplots*yplots+1 | k >= 2*xplots*yplots-xplots+1 & k < 2*xplots*yplots+1 | k >= 3*xplots*yplots-xplots+1){
				axis(1, cex.axis=0.8)
				}
			else{axis(1, labels=FALSE)}
			}				
		axis(1, labels=FALSE)
		j <- 1
		while (j <= numpages){
			if (k %in% seq(1, j*yplots*xplots, by=xplots)) {axis(2, cex.axis=0.8, las=2)}
			j <- j+1
		}
	}
	
	mtext("Distance from edge of disk (mm)", outer=TRUE, side=1, line=2, cex=1.2)
	mtext("Pixel intensity", outer=TRUE, side=2, line=1.5, cex=1.2)
	dev.off()	
	cat(paste("\nSaving figure: ", t, sep=""))

	if(popUp){
		tt <- paste("open", t)
		system(tt)
	}

}

		
maxLik <- function(projectName, clearHalo, standardLoc = 2.5, dotedge = 3.4, maxDist=30, ymax=200, xplots = 6, percentileLow = 0.2, width = 7, height = 10,  AUC=10, ZOI=10, popUp = TRUE, nameVector=TRUE, needML = TRUE, overwrite = TRUE, plotCompon = FALSE, legend = TRUE, addZOIcor = FALSE, saveMLParam = FALSE, showAUC = TRUE, showIC = TRUE, plotAUC = TRUE){
	if(!(hasArg(clearHalo))){
		stop("No picture with clear halo specified.")
	}
	if(AUC %nin% c(10, 50, 80)){
		stop("Current suppported AUC values = 10, 50, 80")
		}
	if(ZOI %nin% c(10, 50, 80, "all")){
		stop("Current suppported ZOI values = 10, 50, 80")
		}
	fileFolder <- paste(Sys.Date(), projectName, sep="_")
	dir.create(paste(getwd(), "/figures/", sep=""), showWarnings= FALSE)
	dir.create(paste(getwd(), "/figures/", fileFolder, sep=""), showWarnings= FALSE)
	# dir.create(paste(getwd(), "/parameter_files/", projectName, sep=""), showWarnings=FALSE)

	data <- eval(parse(text=projectName))
	
	if (is.logical(nameVector)){
		if (nameVector){label <- names(data)}		
		else {label <- rep("", length(data))}
		}
	else {label <- nameVector}	

	if (!is.logical(standardLoc)){
		dotMax <- max(sapply(data, function(x) {x[which(x[,1] > standardLoc)[1], 2]})) 		
		stand <-c( sapply(data, function(x) {dotMax-x[which(x[,1] > standardLoc)[1], 2]}))
		}
	else{
		stand <- rep(0, length(data))
		}

	if(needML){		
		ML <-lapply(c(1:length(data)), getstatsLog, data=data, dotedge=dotedge, maxDist=maxDist, stand=stand, maxSlope=20)
		ML2 <- lapply(c(1:length(data)), getstats2curve, data=data, dotedge=dotedge, maxDist=maxDist, stand=stand, maxSlope=20)
		assign(paste(projectName, ".ML", sep=""), ML, envir=globalenv())
		cat(paste("\n", projectName, ".ML has been written to the global environment", sep=""))
		assign(paste(projectName, ".ML2", sep=""), ML2, envir=globalenv())
		cat(paste("\n", projectName, ".ML2 has been written to the global environment", sep=""))
		if(saveMLParam){
			saveMLParam(projectName)
			}
		}
	
	if(!needML){		
		MLt <- paste(projectName, ".ML", sep="") 
		MLt2 <- paste(projectName, ".ML2", sep="") 
		ML <- eval(parse(text=MLt))
		ML2 <- eval(parse(text=MLt2))
		cat(paste("\n Using existing ML results ", MLt, " & ", MLt2, sep=""))		
		}

	if(plotAUC){
		clearHaloData <- data[[clearHalo]]
		startX <- which(clearHaloData[,1] > dotedge+0.5)[1]
		stopX <- which(clearHaloData[,1] > maxDist - 0.5)[1]
		clearHaloData <- clearHaloData[startX:stopX, 1:2]
		clearHaloData$x <- clearHaloData$x + stand[clearHalo] 
		clearHaloData$distance <- clearHaloData$distance - (dotedge+0.5)
		clearHaloStand <- clearHaloData[1,2]
		 
		cat("\n Plotting ML results: ")				
		 ZOIheight <- unlist(lapply(c(1:length(data)), getZOIheight, data = data, ML = ML, ML2 = ML2, dotedge=dotedge, maxDist=maxDist, stand = stand, clearHaloStand = clearHaloStand))
		ZOIcor <- max(ZOIheight)/ZOIheight	
	
		plotAUC(projectName, ML=ML, ML2=ML2, dotedge = dotedge, stand = stand, standardLoc = standardLoc, maxDist = maxDist, ymax = ymax, clearHaloStand = clearHaloStand, ZOIcor = ZOIcor,  AUC=AUC, ZOI=ZOI, height = height, width=width, xplots = xplots,label=label, overwrite = overwrite, popUp = popUp, showAUC = showAUC, showIC = showIC, plotCompon=plotCompon)
	}
	cat("\a")
}

#################################################################################
#FUNCTIONS REQUIRED FOR createDataframe
#[haloParam] uses the results of MLparam to fit the minHalo and MIC
#[CI.generalNest] is used twice, once for IC50 and once for slope to find confidence intervals
#The dataframe is written onto the hard drive
#################################################################################

relativeAUC.df <- function(data, ML, ML2, stand,  clearHalo, dotedge = 3.4, maxDist = 40, ymax = 200, percentileLow = 0.2){
	dotMax <- max(sapply(data, function(x) {x[which(x[,1] > standardLoc)[1], 2]})) 		
	stand <-c( sapply(data, function(x) {dotMax-x[which(x[,1] > standardLoc)[1], 2]}))

	clearHaloData <- data[[clearHalo]]
	startX <- which(clearHaloData[,1] > dotedge+0.5)[1]
	stopX <- which(clearHaloData[,1] > maxDist - 0.5)[1]
	clearHaloData <- clearHaloData[startX:stopX, 1:2]
	clearHaloData$x <- clearHaloData$x + stand[clearHalo] 
	clearHaloData$distance <- clearHaloData$distance - (dotedge+0.5)
	clearHaloStand <- clearHaloData[1,2]
	
	 ZOIheight <- unlist(lapply(c(1:length(data)), getZOIheight, data = data, ML = ML, ML2 = ML2, dotedge=dotedge, maxDist=maxDist, stand = stand, clearHaloStand = clearHaloStand))
	ZOIcor <- max(ZOIheight)/ZOIheight
	
	
	AUC.df <-  sapply(c(1:length(data)),  relativeAUC, data=data, ML=ML, ML2 = ML2, stand = stand, ZOIcor = ZOIcor, dotedge = dotedge,  maxDist = maxDist, ymax=ymax,clearHaloStand = clearHaloStand, standardLoc = standardLoc, percentileLow = percentileLow)
}

relativeAUC <- function(data, ML, ML2 , compareTo, i, clearHalo, dotedge = 3.4,  maxDist = 35, ymax=200, standardLoc = 2.5, percentileLow = 0.1,  AUC1name = "AUC1", AUC2name = "AUC2"){
	k <- compareTo

	dotMax <- max(sapply(data, function(x) {x[which(x[,1] > standardLoc)[1], 2]})) 		
	stand <-c( sapply(data, function(x) {dotMax-x[which(x[,1] > standardLoc)[1], 2]}))

	clearHaloData <- data[[clearHalo]]
	startX <- which(clearHaloData[,1] > dotedge+0.5)[1]
	stopX <- which(clearHaloData[,1] > maxDist - 0.5)[1]
	clearHaloData <- clearHaloData[startX:stopX, 1:2]
	clearHaloData$x <- clearHaloData$x + stand[clearHalo] 
	clearHaloData$distance <- clearHaloData$distance - (dotedge+0.5)
	clearHaloStand <- clearHaloData[1,2]

	 ZOIheight <- unlist(lapply(c(1:length(data)), getZOIheight, data = data, ML = ML, ML2 = ML2, dotedge=dotedge, maxDist=maxDist, stand = stand, clearHaloStand = clearHaloStand))
	ZOIcor <- max(ZOIheight)/ZOIheight

	startX <- which(data[[k]][,1] > dotedge+0.5)[1]
	stopX <- which(data[[k]][,1] > maxDist - 0.5)[1]
	data[[k]] <- data[[k]][startX:stopX, 1:2]
	data[[k]]$x <- data[[k]]$x + stand[k] - clearHaloStand 
	data[[k]]$distance <- data[[k]]$distance - (dotedge+0.5)
	xx <- seq(log(data[[k]]$distance[1]), log(max(data[[k]][,1])), length=200) 
	yy<- curve2(ML2[[k]]$par[1], ML2[[k]]$par[2], ML2[[k]]$par[3], ML2[[k]]$par[5], ML2[[k]]$par[6], ML2[[k]]$par[7], xx) 
	slope <- max(ML2[[k]]$par[3], ML2[[k]]$par[7])
	ploty <- data[[k]]$x*ZOIcor[k]
	ploty[ploty<0] <-0
	yyn <- (yy+min(data[[k]]$x))*ZOIcor[k]
	yyn[yyn<0] <-0

	yhalo <- yyn[which.max(yyn>yyn[length(yyn)] * 0.8)]
	xhalo <- exp(xx[which.max(yyn> yyn[length(yyn)] * 0.8)])		
	
		plot(data[[k]]$distance, ploty, cex=0.7, col=grey(0.4), type="p", ylim=c(0, ymax), xlim=c(0, maxDist -dotedge), xaxt="n", yaxt="n", xlab="", ylab="")
	points(exp(xx), yyn, type="l", col="red", lwd=2)		
	axis(2, las=2)

	xx <- exp(xx[1:which.max(exp(xx) > xhalo)-1])
	if(length(xx)<1){
		xx <- seq(log(data[[k]]$distance[1]), log(max(data[[k]][,1])), length=200)}	
	yy<- curve2(ML2[[k]]$par[1], ML2[[k]]$par[2], ML2[[k]]$par[3], ML2[[k]]$par[5], ML2[[k]]$par[6], ML2[[k]]$par[7], log(xx)) 
	
	yyn <- (yy+min(data[[k]]$x))*ZOIcor[k]
	yyn[yyn<0] <-0
	abline(v=xhalo, col="purple", lwd=2) 
	abline(h=yhalo, col="purple", lwd=1, lty=2) 		
	points(xx, yyn, type="l")
	xx2 <- c(xx[1], xx, xx[length(xx)])
	yy2 <- c(0, yyn, 0)
	yy2[yy2<0] <- 0
	polygon(xx2, yy2, density=15, col="red")
	id <- order(xx2)
	AUC1 <- sum(diff(xx2[id])*rollmean(yy2[id], 2))

	startX <- which(data[[i]][,1] > dotedge+0.5)[1]
	stopX <- which(data[[i]][,1] > maxDist - 0.5)[1]
	data[[i]] <- data[[i]][startX:stopX, 1:2]
	data[[i]]$x <- data[[i]]$x + stand[i] - clearHaloStand 
	data[[i]]$distance <- data[[i]]$distance - (dotedge+0.5)
	xx <- seq(log(data[[i]]$distance[1]), log(max(data[[i]][,1])), length=200) 
	
	yy<- curve2(ML2[[i]]$par[1], ML2[[i]]$par[2], ML2[[i]]$par[3], ML2[[i]]$par[5], ML2[[i]]$par[6], ML2[[i]]$par[7], xx) 
	slope <- max(ML2[[i]]$par[3], ML2[[i]]$par[7])
	
	ploty <- data[[i]]$x*ZOIcor[i]
	ploty[ploty<0] <-0
	yyn <- (yy+min(data[[i]]$x))*ZOIcor[i]
	yyn[yyn<0] <-0

	yhalo2 <- (yyn[which.max(yyn> yyn[length(yyn)] * 0.8)]+min(data[[i]]$x))*ZOIcor[i]
	xhalo2 <- exp(xx[which.max(yyn> yyn[length(yyn)] * 0.8)])		

	plot(data[[i]]$distance, ploty, cex=0.7, col=grey(0.4), type="p", ylim=c(0, ymax), xlim=c(0, maxDist -dotedge), xaxt="n", yaxt="n", xlab="", ylab="")
	points(exp(xx), yyn, type="l", col="red", lwd=2)		
	axis(2, las=2, labels=FALSE)
	xx <- exp(xx[1:which.max(exp(xx) > xhalo)-1])
	if(length(xx)<1){
		xx <- seq(log(data[[i]]$distance[1]), log(max(data[[i]][,1])), length=200)}	
	yy<- curve2(ML2[[i]]$par[1], ML2[[i]]$par[2], ML2[[i]]$par[3], ML2[[i]]$par[5], ML2[[i]]$par[6], ML2[[i]]$par[7], log(xx)) 
	
	yyn <- (yy+min(data[[i]]$x))*ZOIcor[i]
	yyn[yyn<0] <-0

	abline(v=xhalo, col="purple", lwd=2) 
	abline(h=yhalo, col="purple", lwd=1, lty=2) 		
	abline(v=xhalo2, col="blue", lwd=2) 
	abline(h=yhalo2, col="red", lwd=1, lty=2) 		
	
	xx2 <- c(xx[1], xx, xx[length(xx)])
	yy2 <- c(0, yyn, 0)
	polygon(xx2, yy2, density=15, col="red")
	id <- order(xx)
	AUC2 <- sum(diff(xx2[id])*rollmean(yy2[id], 2))
	return(c(AUC1 = AUC1, AUC2 = AUC2, diff = AUC1-AUC2, ZOI1 =xhalo, ZOI2 = xhalo2, diff.Z = xhalo-xhalo2 ))
}

MLparam <- function(projectName){
	data <- eval(parse(text=projectName))
	ML <- eval(parse(text=paste(projectName, ".ML", sep="")))
	asym <- unlist(lapply(ML, function(x) x$par[1]))
	od50 <- unlist(lapply(ML, function(x) x$par[2]))
	scal <- unlist(lapply(ML, function(x) x$par[3]))
	sigma <- unlist(lapply(ML, function(x) x$par[4]))
	ML.df <- data.frame(line = names(data), asym, od50, scal, sigma)
	return(ML.df)
	}

ML2param <- function(projectName){
	data <- eval(parse(text=projectName))
	ML2 <- eval(parse(text=paste(projectName, ".ML2", sep="")))
	asymA <- unlist(lapply(ML2, function(x) x$par[1]))
	od50A <- unlist(lapply(ML2, function(x) x$par[2]))
	scalA <- unlist(lapply(ML2, function(x) x$par[3]))
	sigma <- unlist(lapply(ML2, function(x) x$par[4]))
	asymB <- unlist(lapply(ML2, function(x) x$par[5]))
	od50B <- unlist(lapply(ML2, function(x) x$par[6]))
	scalB <- unlist(lapply(ML2, function(x) x$par[7]))
	ML2.df <- data.frame(line = names(data), asymA, od50A, scalA, sigma, asymB, od50B, scalB)
	return(ML2.df)
	}


saveMLParam <- function(projectName){
	fileFolder <- paste(Sys.Date(), projectName, sep="_")
	newdir <- paste(getwd(), "/parameter_files/", sep="")
	newdir2 <- paste(getwd(), "/parameter_files/", Sys.Date(), "_", projectName, "/", sep="")
	if (!file.exists(newdir)){		
		dir.create(newdir, showWarnings = FALSE)
		cat(paste("\n\tCreating new directory: ", newdir), sep="")
		}
	if (!file.exists(newdir2)){		
		dir.create(newdir2, showWarnings = FALSE)
		cat(paste("\n\tCreating new directory: ", newdir2), sep="")
		}

	ML.df <- MLparam(projectName)
	ML2.df <- ML2param(projectName)

	MLdf <- paste(projectName, "_ML.df", sep="")
	ML2df <- paste(projectName, "_ML2.df", sep="")

	filename1 <- paste(getwd(), "/parameter_files/", Sys.Date(), "_", projectName, "/",  projectName, "_ML.csv", sep="")
	filename2 <- paste(getwd(), "/parameter_files/", Sys.Date(), "_", projectName, "/",  projectName, "_ML2.csv", sep="")

	cat("\n")
	cat(paste("\n\t", MLdf, " has been written to the global environment", sep=""))
	assign(MLdf, ML.df, envir=globalenv())
	cat(paste("\n\t", ML2df, " has been written to the global environment", sep=""))
	assign(ML2df, ML2.df, envir=globalenv())

	cat(paste("\n\tSaving files: ", MLdf, ", ", ML2df, sep=""))

	write.csv(ML.df, file=filename1, row.names=FALSE)	
	write.csv(ML2.df, file=filename2, row.names=FALSE)	
}

############
#Functions required for createDataframe
############

findSlope <- function(data, ML, i, stand, ZOIcor, clearHaloStand, dotedge = 3.4,  maxDist = 35, showPlot = FALSE){
	startX <- which(data[[i]][,1] > dotedge+0.5)[1]
	stopX <- which(data[[i]][,1] > maxDist - 0.5)[1]
	data[[i]] <- data[[i]][startX:stopX, 1:2]
	data[[i]]$x <- data[[i]]$x + stand[i] - clearHaloStand 
	data[[i]]$distance <- data[[i]]$distance - (dotedge+0.5)
	xx <- seq(log(data[[i]]$distance[1]), log(max(data[[i]][,1])), length=200)
	yy<- curve(ML[[i]]['par'][1]$par[1], ML[[i]]['par'][1]$par[2], ML[[i]]['par'][1]$par[3],xx)
	yycor <- (yy+min(data[[i]]$x))
	xcross <- exp(ML[[i]]['par'][1]$par[2])
	xxmid <- which.max(exp(xx) > xcross)
	if ((xxmid-10) > 1){
		xxSlope <- xx[(xxmid-10):(xxmid+10)]
		yySlope <- yy[(xxmid-10):(xxmid+10)]
		}
	else {
		xxSlope <- xx[1:(xxmid+10)]
		yySlope <- yy[1:(xxmid+10)]
	}
	slope <- lm(yySlope ~ xxSlope)$coefficients[2]
	cat(".")
	
	if(showPlot){
		ploty <- data[[i]]$x*ZOIcor[i]
		plot(data[[i]]$distance, ploty, cex=0.7, col=grey(0.4), type="p", ylim=c(0, 200), xlim=c(0, maxDist -dotedge), xaxt="n", yaxt="n", xlab="", ylab="")
		points(exp(xx), yycor, type="l", col="red", lwd=1)				
		axis(2, las=2)
		axis(1)
		abline(v=xcross)		
		points(xxSlope, (yySlope+min(data[[i]]$x)), pch=19)
		model <- lm(yySlope ~ xxSlope)
		abline(model)
	}
	return(slope)
}

findAUC <- function(data, ML, ML2, stand, clearHaloStand, ZOIcor, dotedge = 3.4, maxDist = 35, ymax = 200, standardLoc = 2.5, percentileLow = 0.2,  i){	
	startX <- which(data[[i]][,1] > dotedge+0.5)[1]
	stopX <- which(data[[i]][,1] > maxDist - 0.5)[1]
	data[[i]] <- data[[i]][startX:stopX, 1:2]
	data[[i]]$x <- data[[i]]$x + stand[i] - clearHaloStand 
	data[[i]]$distance <- data[[i]]$distance - (dotedge+0.5)
	xx <- seq(log(data[[i]]$distance[1]), log(max(data[[i]][,1])), length=200) 
	yy<- curve2(ML2[[i]]$par[1], ML2[[i]]$par[2], ML2[[i]]$par[3], ML2[[i]]$par[5], ML2[[i]]$par[6], ML2[[i]]$par[7], xx) 
	ic50 <- ML[[i]]$par[2]	
	ploty <- data[[i]]$x
	ploty[ploty < 0] <-0
	slope <- ML[[i]]$par[3]
	asym <- (ML[[i]]$par[1]+min(data[[i]]$x))

	xx <- seq(log(data[[i]]$distance[1]), log(max(data[[i]][,1])), length=200) 				
	yy <- (yy+min(data[[i]]$x))
	yy[yy < 0] <- 0		
	x80 <- xx[which.max(yy> asym * 0.2)]
	x50 <- xx[which.max(yy> asym * 0.5)]
	x10 <- xx[which.max(yy> asym * 0.9)]
	if (x10 < x50){
		x10 <- xx[which.max(yy> yy[length(yy)] * 0.9)]
		}

	if(exp(x80)>1){
		 xx80 <- seq(log(data[[i]]$distance[1]), log(round(exp(x80))), length=200)

		}
	else{
		 xx80 <- seq(log(data[[i]]$distance[1]), log(data[[i]]$distance[2]), length=200)}


	if(exp(x50)>1){
		 xx50 <- seq(log(data[[i]]$distance[1]), log(round(exp(x50))), length=200)		

		}
	else{
		 xx50 <- seq(log(data[[i]]$distance[1]), log(data[[i]]$distance[2]), length=200)

		}

	if(exp(x10)>1){
		 xx10 <- seq(log(data[[i]]$distance[1]), log(round(exp(x10))), length=200)
		
		}
	else{
		 xx10 <- seq(log(data[[i]]$distance[1]), log(data[[i]]$distance[2]), length=200)}	

	yy <- curve2(ML2[[i]]$par[1], ML2[[i]]$par[2], ML2[[i]]$par[3], ML2[[i]]$par[5], ML2[[i]]$par[6], ML2[[i]]$par[7], xx)		
	yy80 <- curve2(ML2[[i]]$par[1], ML2[[i]]$par[2], ML2[[i]]$par[3], ML2[[i]]$par[5], ML2[[i]]$par[6], ML2[[i]]$par[7], xx80)	
	yy50<- curve2(ML2[[i]]$par[1], ML2[[i]]$par[2], ML2[[i]]$par[3], ML2[[i]]$par[5], ML2[[i]]$par[6], ML2[[i]]$par[7], xx50)
	yy10<- curve2(ML2[[i]]$par[1], ML2[[i]]$par[2], ML2[[i]]$par[3], ML2[[i]]$par[5], ML2[[i]]$par[6], ML2[[i]]$par[7], xx10) 
		
	yy <- (yy+min(data[[i]]$x))	
	yy[yy < 0] <- 0.1
	yy80 <- (yy80+min(data[[i]]$x))
	yy80[yy80 < 0] <- 0.1
	yy50 <- (yy50+min(data[[i]]$x))
	yy50[yy50 < 0] <- 0.1
	yy10 <- (yy10+min(data[[i]]$x))
	yy10[yy10 < 0] <- 0.1

			
	id <- order(xx)
	id80 <- order(xx80)
	id50 <- order(xx50)
	id10 <- order(xx10)		

	maxAUC <- sum(diff(xx[id])*rollmean(yy[id], 2))	
	maxAUC80 <- exp(x80)*max(yy80)
	maxAUC50 <- exp(x50)*max(yy50)	
	maxAUC10 <- exp(x10)*max(yy10)	
		
	AUC80 <- sum(diff(exp(xx80[id80]))*rollmean(yy80[id80], 2))		
	AUC50 <- sum(diff(exp(xx50[id50]))*rollmean(yy50[id50], 2))		
	AUC10 <- sum(diff(exp(xx10[id10]))*rollmean(yy10[id10], 2))		
	
	# if(exp(x80) < 1){
		# maxAUC80 <- AUC80}
	# if(exp(x50) < 1){
		# maxAUC50 <- AUC50}
	# if(exp(x10) < 1){
		# maxAUC10 <- AUC10}
	
	 param <- data.frame(x80 = round(exp(x80), digits=0), x50 = round(exp(x50), digits=2), x10 = round(exp(x10), digits=0) , AUC80 = round(AUC80, digits=0), AUC50= round(AUC50, digits=0), AUC10= round(AUC10, digits=0), maxAUC = round(maxAUC, digits=0), maxAUC80 = round(maxAUC80, digits=0), maxAUC50 = round(maxAUC50, digits=0), maxAUC10 = round(maxAUC10, digits=0))		
	 
	 if (exp(param$x80)<1){
	 	param$x80 <- 1}
	 if (exp(param$x50)<1){
	 	param$x50 <- 1}	 
	if (exp(param$x10)<1){
		param$x10 <- 1}	  
		return(param)	
	}


createDataframe <- function(projectName, clearHalo, lowDens = c(), typePlace=2, dotedge = 3.4, maxDist = 30, standardLoc = 2.5, xplots = 6,  percentileLow = 0.2, ymax = 200, disk = 6, nameVector=TRUE, showNum=FALSE, popUp=FALSE, typeVector=TRUE,  typeType = "_", order="default"){
	if(!(hasArg(clearHalo))){
		stop("Need to specify a picture with a clear halo.")
	}

	cat("\nDoing all the things: ")
	data <- eval(parse(text=projectName))
	df <- data.frame()

	newdir <- paste(getwd(), "/parameter_files/", sep="")
	newdir2 <- paste(getwd(), "/parameter_files/", Sys.Date(), "_", projectName, "/", sep="")
	newdir3 <- paste(getwd(), "/figures/", Sys.Date(), "_", projectName, "/", sep="")
	
	filename <- paste(getwd(), "/parameter_files/", Sys.Date(), "_", projectName, "/",  projectName, "_df.csv", sep="")

	if (!file.exists(newdir)){		
		dir.create(newdir, showWarnings = FALSE)
		cat(paste("\n\tCreating new directory: ", newdir), sep="")
		}
	if (!file.exists(newdir2)){		
		dir.create(newdir2, showWarnings = FALSE)
		cat(paste("\n\tCreating new directory: ", newdir2), sep="")
		}
	if (!file.exists(newdir3)){		
		dir.create(newdir3, showWarnings = FALSE)
		cat(paste("\n\tCreating new directory: ", newdir3), sep="")
		}
	df <- data.frame(row.names = seq(1, length(data)))

	ML <- paste(projectName, ".ML", sep="")	
	ML2 <- paste(projectName, ".ML2", sep="")
	ML <- eval(parse(text=ML))
	ML2 <- eval(parse(text=ML2))	
		
	cat("\n\tObtaining MIC80 and MIC10 \n\t ")
	dotMax <- max(sapply(data, function(x) {x[which(x[,1] > standardLoc)[1], 2]})) 		
	stand <-c( sapply(data, function(x) {dotMax-x[which(x[,1] > standardLoc)[1], 2]}))

	clearHaloData <- data[[clearHalo]]
	startX <- which(clearHaloData[,1] > dotedge+0.5)[1]
	stopX <- which(clearHaloData[,1] > maxDist - 0.5)[1]
	clearHaloData <- clearHaloData[startX:stopX, 1:2]
	clearHaloData$x <- clearHaloData$x + stand[clearHalo] 
	clearHaloData$distance <- clearHaloData$distance - (dotedge+0.5)
	clearHaloStand <- clearHaloData[1,2]
	
	cat("  ")
		 ZOIheight <- unlist(lapply(c(1:length(data)), getZOIheight, data = data, ML = ML, ML2 = ML2, dotedge=dotedge, maxDist=maxDist, stand = stand, clearHaloStand = clearHaloStand))
		ZOIdf <- data.frame(names = names(data), ZOIheight)
		ZOIdfsub <- subset(ZOIdf, names %nin% lowDens)
		ZOIcor <- max(ZOIdfsub$ZOIheight)/ZOIdf$ZOIheight
	
	cat("\n\tObtaining MIC50 from ML parameters\n\t")
	ic50 <- round(c(exp(unlist(lapply(ML, function(x) as.numeric(as.character(unlist(x)[2])))))), digits=0)

	 cat("\n\tFitting slope \n\t")
	slope <- sapply(c(1:length(data)),  findSlope, data=data, ML=ML, stand = stand, ZOIcor = ZOIcor, dotedge = dotedge,  maxDist = maxDist, clearHaloStand = clearHaloStand)
	
	cat("\n\tCalculating average area under the curve\n\t ")
	AUC.df <-  sapply(c(1:length(data)),  findAUC, data=data, ML=ML, ML2 = ML2, stand = stand, ZOIcor = ZOIcor, dotedge = dotedge,  maxDist = maxDist, ymax=ymax, clearHaloStand = clearHaloStand, standardLoc = standardLoc, percentileLow = percentileLow)

	x80 <- unlist(AUC.df[1,])	
	x50 <- unlist(AUC.df[2,])	
	x10 <- unlist(AUC.df[3,])
	AUC80 <- unlist(AUC.df[4,])	
	AUC50 <- unlist(AUC.df[5,])	
	AUC10 <- unlist(AUC.df[6,])	
	maxAUC <- unlist(AUC.df[7,])		
	maxAUC80 <- unlist(AUC.df[8,])		
	maxAUC50 <- unlist(AUC.df[9,])		
	maxAUC10 <- unlist(AUC.df[10,])				
	
	AUC80[slope < 5] <- NA
	AUC50[slope < 5] <- NA
	AUC10[slope < 5] <- NA
	x80[slope < 5] <- 1
	x50[slope < 5] <- 1
	x10[slope < 5] <- 1

	aveAUC80 <- AUC80/x80
	aveAUC50 <- AUC50/x50
	aveAUC10 <- AUC10/x10	

	param <- data.frame(ZOI80 =round(x80, digits=0), ZOI50 = round(x50, digits=0), ZOI10 = round(x10, digits=0), fracAUC80 = round(AUC80/maxAUC80, digits=2), fracAUC50 = round(AUC50/maxAUC50, digits=2), fracAUC10 = round(AUC10/maxAUC10, digits=2), slope=round(slope, digits=1))
	if (is.logical(nameVector)){
		if (nameVector){
			lines <- unlist(lapply(names(data), function(x) strsplit(x, typeType)[[1]][1]))
			df <- data.frame(name = names(data), lines)
			}
			
		if (!nameVector){
			lines <- seq(1, length(data))
			cat("\n\tAssigning line names based on photo number")
			df <- data.frame(name = names(data), lines, df)	
		}
	}
	if (!is.logical(nameVector)){
		cat("\n\tAssigning line names based on user-input")
		lines <- nameVector
		names <- unlist(lapply(names(data), function(x) strsplit(x, "_")[[1]][1]))
		df <- data.frame(names=names, lines=lines, df)	
		}

	if (is.logical(typeVector)){
		if (typeVector){	
				cat(paste("\n\tAssigning photo type based on the element in the ", typePlace, " position"))
				type <- unlist(lapply(names(data), function(x) strsplit(x, typeType)[[1]][typePlace]))
				cat(paste("\n\tThere were ", length(unique(type)), " unique types found in dataset:", paste(unique(type), collapse=", ")))
				df <- data.frame(df, type, param)
			}
		else {
				cat("\n\tNo type assigned")
				df$type <- 1
				df <- data.frame(df, param)
			}
		}
	if(!is.logical(typeVector)){
		cat("\n\tPhoto type based on user-input")
		type <- typeVector
		df <- data.frame(df, type, param)
		}

	df <- df[order(df$lines),] 
	df$fracAUC80[df$fracAUC80 >1] <- 1
	df$fracAUC50[df$fracAUC50 >1] <- 1
	df$fracAUC10[df$fracAUC10 >1] <- 1	
	df$fracAUC80[df$ZOI80 == 1] <- NA
	df$fracAUC50[df$ZOI50 == 1] <- NA
	df$fracAUC10[df$ZOI10 == 1] <- NA

	#nb 6 here is the width of the disk in mm
	df$dOI80 <- df$ZOI80*2+disk
	df$dOI50 <- df$ZOI50*2+disk
	df$dOI10 <- df$ZOI10*2+disk
	
	
	write.csv(df, file=filename, row.names=FALSE)	
	
	dfName <- paste(projectName, ".df", sep="")
	cat("\n")
	cat(paste("\n\t", dfName, " has been written to the global environment", sep=""))
	cat(paste("\n\tSaving file: ", filename,  sep=""))
	cat(paste("\n\t", projectName, "_df.csv can be opened in MS Excel.  Save as .xls file if desired.",  sep=""))
	assign(dfName, df, envir=globalenv())
	}


discplotNoRepParam <- function(rep1,  label=label, ymin=0, ymax=250, xmin=0, maxDist=40, standardLoc = 2.5, cexPt = 0.6, xaxt="n", yaxt="n", col1="black", stand=0, plotStandardLoc =FALSE){
	plot(rep1[,1], rep1[,2]+stand, ylim=c(ymin, ymax), xlim=c(xmin, maxDist), xaxt=xaxt, yaxt="n", cex=cexPt, col=col1)
	if (yaxt=="s"){
		axis(2, las=2)}
	axis(1, labels=FALSE, at=c(0, 10, 20, 30, 40))
	axis(2, labels=FALSE)
	mtext(label, side=3, cex=0.6)
	if(plotStandardLoc){
		abline(v= standardLoc, lty=2, col="red")
		}
	}
	
plotRawParam <-function(projectName,  df, standardLoc = 2.5, ymin = 0, ymax=200, xmin = 0, maxDist = 40, dotedge = 3.4, xplots = 6, height =8, width = 8, cexX = 0.8, cexPt = 0.6, cexY = 0.8, nameVector = TRUE , plotDot = TRUE, plotStandardLoc=TRUE, showNum=FALSE, popUp = TRUE, overwrite=TRUE, stand=TRUE){
	fileName <- paste(Sys.Date(), "_", projectName,"/", sep="")
	dir.create(paste("figures/", fileName,  sep=""), showWarnings = TRUE)
	t <- paste("figures/", fileName, projectName, "_raw.pdf", sep="")
	if (!overwrite){
		if (file.exists(t)){
			t <- paste("figures/", fileName, projectName, "_raw_2_sL=", standardLoc, "_dE=", dotedge, ".pdf", sep="")
			if (file.exists(t)){
				k <- 2
				while(file.exists(t)){
					k <- k+1
					t <- paste("figures/", fileName, projectName, "_raw_", k, "sL=", standardLoc, "_dE=", dotedge, ".pdf", sep="")
					}
				}
			}
		}
	data <- eval(parse(text=projectName))
	df <- eval(parse(text=paste(projectName, ".df", sep="")))	
	dotMax <- max(sapply(data, function(x) {x[which(x[,1] > standardLoc)[1], 2]})) 		
	standards <-c( sapply(data, function(x) {dotMax-x[which(x[,1] > standardLoc)[1], 2]}))	
	convert <- unlist(lapply(data, function(x) 40/length(x[,1])))
	if (is.logical(nameVector)){
		if (nameVector){label <- names(data)}		
		else {label <- rep("", length(data))}
		}
	else {label <- nameVector}

	if (xplots > length(data)){
		xplots <- length(data)
		}
	if (ceiling(length(data)/xplots) < 6) {
		yplots<- ceiling(length(data)/xplots)}
	else {yplots<- 6}
	numpages <- ceiling(length(data)/(xplots*yplots))
	pdf(t, width=width, height=height)
	par(mfrow=c(yplots , xplots), mar=c(1,1,1,1), oma=c(4,5,1,1))
		for (i in 1:length(data)){
			if(stand){
				discplotNoRepParam(data[[i]], df=df, label[i], ymin=ymin, ymax=ymax, xmin=xmin, maxDist=maxDist, stand=standards[i], standardLoc = standardLoc, cexPt = cexPt, plotStandardLoc = plotStandardLoc)
				}
			if(!stand){
				discplotNoRepParam(data[[i]], df=df, label[i], ymin=ymin, ymax=ymax, xmin=xmin, maxDist=maxDist, standardLoc = standardLoc, cexPt = cexPt, plotStandardLoc = plotStandardLoc, df=df)
				}
			if(numpages == 1){
				if (i >= xplots*yplots-xplots+1){
					axis(1, cex.axis=cexX, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
					}
				}
			if(numpages == 2){
				if (i >= xplots*yplots-xplots+1 & i < xplots*yplots+1){
					axis(1, cex.axis=cexX, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
					}
				if (i >= 2*xplots*yplots-xplots+1){
					axis(1, cex.axis=cexX, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
					}
				}				
			if(numpages == 3){
				if (i >= xplots*yplots-xplots+1 & i < xplots*yplots+1){
					axis(1, cex.axis=cexX, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
					}
				if (i >= 2*xplots*yplots-xplots+1 & i < 2*xplots*yplots+1){
					axis(1, cex.axis=cexX, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
					}
				if (i >= (length(data)-xplots)){
					axis(1, cex.axis=cexX, at=c(0, 10, 20, 30, 40), labels=c(0, 10, 20, 30, 40))
					}
				}				

			k <- 1
			while (k <= numpages){
				if (i %in% seq(1, k*yplots*xplots, by=xplots)) {axis(2, cex.axis=cexY, las=2)}
					k <- k+1}


			if (plotDot) {abline(v=dotedge+0.5, lty=2)}
			if(showNum){
				text(maxDist*0.95, maxDist*0.95, i)
			}
		}
	
	mtext("Distance from disc center (mm)", side= 1, outer=TRUE, line=2)
	mtext("Pixel intensity", side=2, outer=TRUE, line=2)
	dev.off()
	cat(paste("\nSaving figure: ", t, sep=""))
	if(popUp){
	tt <- paste("open ",t)
	system(tt)
	}
	}


############################################
#FUNCTIONS REQUIRED FOR aggregateData
############################################
 aggregateDataOrd <- function(projectName, replicate = c("lines", "type"), identifier="default", order="default", linesOrder = "default"){
	dataframe <- eval(parse(text=paste(projectName, ".df", sep="")))
	temp <- aggregate(c(dataframe[c("order24", "order72", "MIC90", "MIC50", "MIC20", "slope", "aveAUC50", "aveAUC90", "fracAUC20","fracAUC50", "fracAUC90")]), dataframe[replicate], mean)
	temp$sd.MIC90 <- aggregate(c(dataframe[c("MIC90")]), dataframe[replicate], sd)$MIC90
	temp$sd.MIC50 <- aggregate(c(dataframe[c("MIC50")]), dataframe[replicate], sd)$MIC50
	temp$sd.MIC20 <- aggregate(c(dataframe[c("MIC20")]), dataframe[replicate], sd)$MIC20
	temp$sd.slope <- aggregate(c(dataframe[c("slope")]), dataframe[replicate], sd)$slope
	temp$sd.aveAUC20 <- aggregate(c(dataframe[c("aveAUC20")]), dataframe[replicate], sd)$aveAUC20
	temp$sd.aveAUC50 <- aggregate(c(dataframe[c("aveAUC50")]), dataframe[replicate], sd)$aveAUC50
	temp$sd.aveAUC90 <- aggregate(c(dataframe[c("aveAUC90")]), dataframe[replicate], sd)$aveAUC90	
	temp$sd.fracAUC20 <- aggregate(c(dataframe[c("fracAUC20")]), dataframe[replicate], sd)$fracAUC20
	temp$sd.fracAUC50 <- aggregate(c(dataframe[c("fracAUC50")]), dataframe[replicate], sd)$fracAUC50
	temp$sd.fracAUC90 <- aggregate(c(dataframe[c("fracAUC90")]), dataframe[replicate], sd)$fracAUC90	

	temp <- temp[order(temp$lines),] 

	filename <- paste(getwd(), "/parameter_files/", Sys.Date(), "_", projectName, "/",projectName, "_ag.csv", sep="")
	newdir2 <- paste(getwd(), "/parameter_files/", sep="")		
	newdir3 <- paste(getwd(), "/parameter_files/", Sys.Date(), "_", projectName, "/", sep="")	

	dir.create(newdir2, showWarnings = FALSE)
	dir.create(newdir3, showWarnings = FALSE)

	write.csv(temp, file=filename, row.names=FALSE)	
	agName <- paste(projectName, ".ag", sep="")
	cat(paste("\n", agName, " has been written to the global environment", sep=""))
	cat(paste("\n\nSaving file: ", filename, sep=""))
	cat(paste("\n",  projectName, "_ag.csv can be opened in MS Excel (save as .xls file if desired)",  sep=""))
	assign(agName, temp, envir=globalenv())
	}

 aggregateData <- function(projectName, replicate = c("lines", "type"), identifier="default", order="default", linesOrder = "default"){
	dataframe <- eval(parse(text=paste(projectName, ".df", sep="")))

	temp <- aggregate(c(dataframe[c( "ZOI80", "ZOI50", "ZOI10", "slope", "fracAUC80","fracAUC50", "fracAUC10")]), dataframe[replicate], mean, na.rm=TRUE)
	temp$se.ZOI80 <- aggregate(c(dataframe[c("ZOI80")]), dataframe[replicate], se)$ZOI80
	temp$se.ZOI50 <- aggregate(c(dataframe[c("ZOI50")]), dataframe[replicate], se)$ZOI50
	temp$se.ZOI10 <- aggregate(c(dataframe[c("ZOI10")]), dataframe[replicate], se)$ZOI10
	temp$se.slope <- aggregate(c(dataframe[c("slope")]), dataframe[replicate], se)$slope
	temp$se.fracAUC80 <- aggregate(c(dataframe[c("fracAUC80")]), dataframe[replicate], se)$fracAUC80
	temp$se.fracAUC50 <- aggregate(c(dataframe[c("fracAUC50")]), dataframe[replicate], se)$fracAUC50
	temp$se.fracAUC10 <- aggregate(c(dataframe[c("fracAUC10")]), dataframe[replicate], se)$fracAUC10	
	
	temp <- temp[order(temp$lines),] 

	filename <- paste(getwd(), "/parameter_files/", Sys.Date(), "_", projectName, "/",projectName, "_ag.csv", sep="")
	newdir2 <- paste(getwd(), "/parameter_files/", sep="")		
	newdir3 <- paste(getwd(), "/parameter_files/", Sys.Date(), "_", projectName, "/", sep="")	

	dir.create(newdir2, showWarnings = FALSE)
	dir.create(newdir3, showWarnings = FALSE)

	write.csv(temp, file=filename, row.names=FALSE)	
	agName <- paste(projectName, ".ag", sep="")
	cat(paste("\n", agName, " has been written to the global environment", sep=""))
	cat(paste("\n\nSaving file: ", filename, sep=""))
	cat(paste("\n",  projectName, "_ag.csv can be opened in MS Excel (save as .xls file if desired)",  sep=""))
	assign(agName, temp, envir=globalenv())
	}

aggregateData.sd <- function(projectName, replicate = c("lines", "type"), identifier="default", order="default", linesOrder = "default"){
	dataframe <- eval(parse(text=paste(projectName, ".df", sep="")))

	temp <- aggregate(c(dataframe[c( "ZOI80", "ZOI50", "ZOI10", "slope", "aveAUC80", "aveAUC50", "aveAUC10", "fracAUC80","fracAUC50", "fracAUC10")]), dataframe[replicate], mean)
	temp$sd.ZOI80 <- aggregate(c(dataframe[c("MIC80")]), dataframe[replicate], sd)$ZOI80
	temp$sd.ZOI50 <- aggregate(c(dataframe[c("MIC50")]), dataframe[replicate], sd)$ZOI50
	temp$sd.ZOI10 <- aggregate(c(dataframe[c("MIC10")]), dataframe[replicate], sd)$ZOI10
	temp$sd.slope <- aggregate(c(dataframe[c("slope")]), dataframe[replicate], sd)$slope
	temp$sd.aveAUC80 <- aggregate(c(dataframe[c("aveAUC80")]), dataframe[replicate], sd)$aveAUC80
	temp$sd.aveAUC50 <- aggregate(c(dataframe[c("aveAUC50")]), dataframe[replicate], sd)$aveAUC50
	temp$sd.aveAUC10 <- aggregate(c(dataframe[c("aveAUC10")]), dataframe[replicate], sd)$aveAUC10	
	temp$sd.fracAUC80 <- aggregate(c(dataframe[c("fracAUC80")]), dataframe[replicate], sd)$fracAUC80
	temp$sd.fracAUC50 <- aggregate(c(dataframe[c("fracAUC50")]), dataframe[replicate], sd)$fracAUC50
	temp$sd.fracAUC10 <- aggregate(c(dataframe[c("fracAUC10")]), dataframe[replicate], sd)$fracAUC10	

	temp <- temp[order(temp$lines),] 

	filename <- paste(getwd(), "/parameter_files/", Sys.Date(), "_", projectName, "/",projectName, "_ag.csv", sep="")
	newdir2 <- paste(getwd(), "/parameter_files/", sep="")		
	newdir3 <- paste(getwd(), "/parameter_files/", Sys.Date(), "_", projectName, "/", sep="")	

	dir.create(newdir2, showWarnings = FALSE)
	dir.create(newdir3, showWarnings = FALSE)

	write.csv(temp, file=filename, row.names=FALSE)	
	agName <- paste(projectName, ".ag", sep="")
	cat(paste("\n", agName, " has been written to the global environment", sep=""))
	cat(paste("\n\nSaving file: ", filename, sep=""))
	cat(paste("\n",  projectName, "_ag.csv can be opened in MS Excel (save as .xls file if desired)",  sep=""))
	assign(agName, temp, envir=globalenv())
	}


testing <- function(df, line, param){
		sub <- subset(df, lines == line)		
		if(names(sub)[3] == "type"){
			names(sub)[3] <- "col"
			}
		obj<-try(t.test(subset(sub, col=="C0")[,param], subset(sub, col=="C8")[,param]), silent=TRUE)
   if (is(obj, "try-error")){
   	return("fail") 
   }
   else{
   	return(t.test(subset(sub, col=="C0")[,param], subset(sub, col=="C8")[,param]))
  	}
  }

allttest <- function(df, param){
	statistic <- c()
	degf <- c()
	p <- c()
	for (i in 1:length(unique(df$line))){
			t<- testing(df, unique(df$line)[i], param)
			if (t == "fail"){
				statistic[i] <- NA
				degf[i] <- NA
				p[i] <- 1
				}
			else{
				statistic[i] <- round(t$statistic, 3)
				degf[i] <- round(t$parameter, 3)
				p[i] <- round(t$p.value,4)
			}
	}
	return(data.frame(line=unique(df$line), t = statistic, degf = degf, p = p))
	}

############################################
#FUNCTIONS REQUIRED FOR readExistingDF
############################################
readExistingDF <- function(projectName, mean=FALSE, projectFolder=FALSE, dataframeFolder = FALSE){
	if (projectFolder==FALSE){
		projectFolder <- tk_choose.dir(caption = "Select location of project folder")
	}
	setwd(projectFolder)
	if (dataframeFolder==FALSE){
		dataframeFolder <- tk_choose.dir(caption = "Select location of folder with dataframe")
	}
	else {projectFolder <- projectFolder}
	if (mean==FALSE){
		dfName <- paste(projectName, "_df.csv", sep="")
		t <- read.csv(paste(dataframeFolder,"/", dfName, sep=""))
		df <- paste(projectName, ".df", sep="")	
	}
	else {
		dfName <- paste(projectName, "_ag.csv", sep="")
		t <- read.csv(paste(dataframeFolder,"/", dfName, sep=""))
		df <- paste(projectName, ".ag", sep="")	
	}
	assign(df, t, envir=globalenv())	
	fileFolder <- paste(Sys.Date(), projectName, sep="_")
	dir.create(paste(projectFolder, "/figures/", fileFolder, sep=""), showWarnings= FALSE)
	cat(paste(df, "now present in working directory"))
}


basic2panel <- function(dat.ag, order, figureName, ZOI = "ZOI10", AUC = "fracAUC10", ZOImin = 60, tolMax = 100, xlabels=mp[1,], plotLegend=FALSE, legendText="", addLines=FALSE){
	tols <- dat.ag[, AUC][order]	
	mp <- barplot(t(tols), beside=TRUE, plot=FALSE)	
	pdf(figureName, width=5, height=6)
	par(mfrow=c(2, 1), oma=c(4, 4, 1, 1), mar=c(1, 1, 1, 1))
	plot(mp[1,], dat.ag[, ZOI][order], ylim=c(ZOImin, 0), yaxt="n", xaxt="n", yaxs="i", xaxs="i", pch=19, xlab="", ylab="", col=grey(0.3), xlim=c(0, max(mp)+1), cex=1.4)
	axis(2, las=2, cex.axis=0.8)
	arrows(mp[1,], dat.ag[, ZOI][order]-dat.ag[,paste("se.", ZOI, sep="")][order], mp[1,], dat.ag[, ZOI][order]+dat.ag[,paste("se.", ZOI, sep="")][order], length=0)
	if(plotLegend){
		legend(legendText)
		}
	if(addLines){
		abline(h=14, lty=2)
		abline(h=19, lty=2)
		}
	mtext("Distance from disk (mm)", side=2, line=2.5)
	mtext(expression(paste(bold(A), " Resistance", sep="")), side=3, adj=0.01)
	axis(1, at=xlabels, labels=FALSE)
	
	mp <- barplot(t(tols*100), ann=FALSE, beside=TRUE, yaxs="i", xaxs="i", ylim=c(0, tolMax), xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(0, max(mp)+1))
	box()
	axis(1, at=xlabels, labels=FALSE)
	text(xlabels, -5, paste("A", order, sep=""), srt=-45, xpd=NA, adj=0, cex=0.8)
	axis(2, las=2, at=c(0, 20, 40, 60, 80, 100), cex.axis=0.8)
	mtext("Growth above ZOI (%)",  side=2, line=2.5)
	arrows(mp[1,], dat.ag[,AUC][order]*100-dat.ag[,paste("se.", AUC, sep="")][order]*100, mp[1,], dat.ag[,AUC][order]*100+dat.ag[,paste("se.", AUC, sep="")][order]*100, length=0)
	mtext(expression(paste(bold(B), " Tolerance", sep="")), side=3, adj=0.01)
	dev.off()	
	system(paste("open ", figureName, sep=""))
}

basic2panel2means <- function(dat.ag, dat.ag2, order, figureName, ZOI = "ZOI10", AUC = "fracAUC10", ZOImin = 60, tolMax = 100, xlabels=mp[1,], plotLegend=FALSE, legendText="", addLines=FALSE){
	tols <- cbind(dat.ag[, AUC][order], dat.ag2[,AUC][order])
	mp <- barplot(t(tols), beside=TRUE, plot=FALSE)	
	pdf(figureName, width=6.5, height=6)
	par(mfrow=c(2, 1), oma=c(4, 4, 1, 1), mar=c(1, 1, 1, 1))
	plot(mp[1,], dat.ag[, ZOI][order], ylim=c(ZOImin, 0), yaxt="n", xaxt="n", yaxs="i", xaxs="i", pch=19, xlab="", ylab="", col=grey(0.3), xlim=c(0, max(mp)+1), cex=1.4)
	points(mp[2,], dat.ag2[,ZOI][order], col=grey(0.7), pch=19, cex=1.4)
	axis(2, las=2, cex.axis=0.8)
	arrows(mp[1,], dat.ag[, ZOI][order]-dat.ag[,paste("se.", ZOI, sep="")][order], mp[1,], dat.ag[, ZOI][order]+dat.ag[,paste("se.", ZOI, sep="")][order], length=0)
	arrows(mp[2,], dat.ag2[, ZOI][order]-dat.ag2[,paste("se.", ZOI, sep="")][order], mp[2,], dat.ag2[, ZOI][order]+dat.ag2[,paste("se.", ZOI, sep="")][order], length=0)
	if(plotLegend){
		legend(legendText)
		}
	if(addLines){
		abline(h=14, lty=2)
		abline(h=19, lty=2)
		}
	mtext("Distance from disk (mm)", side=2, line=2.5)
	mtext(expression(paste(bold(A), " Resistance", sep="")), side=3, adj=0.01)
	axis(1, at=xlabels, labels=FALSE)
	
	mp <- barplot(t(tols*100), ann=FALSE, beside=TRUE, yaxs="i", xaxs="i", ylim=c(0, tolMax), xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(0, max(mp)+1))
	box()
	axis(1, at=xlabels, labels=FALSE)
	text(xlabels, -5, paste("A", order, sep=""), srt=-45, xpd=NA, adj=0, cex=0.8)
	axis(2, las=2, at=c(0, 20, 40, 60, 80, 100), cex.axis=0.8)
	mtext("Growth within ZOI (%)",  side=2, line=2.5)
	arrows(mp[1,], dat.ag[,AUC][order]*100-dat.ag[,paste("se.", AUC, sep="")][order]*100, mp[1,], dat.ag[,AUC][order]*100+dat.ag[,paste("se.", AUC, sep="")][order]*100, length=0)
	arrows(mp[2,], dat.ag2[,AUC][order]*100-dat.ag2[,paste("se.", AUC, sep="")][order]*100, mp[2,], dat.ag2[,AUC][order]*100+dat.ag2[,paste("se.", AUC, sep="")][order]*100, length=0)
	mtext(expression(paste(bold(B), " Tolerance", sep="")), side=3, adj=0.01)
	dev.off()	
	system(paste("open ", figureName, sep=""))
}

