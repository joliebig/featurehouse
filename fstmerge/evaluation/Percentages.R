###################################
# Input data
###################################

#load tables
#csharp
autowikibrowserSM <- read.table("./AutoWikiBrowser/result-semiStructured.csv", sep="\t", header=TRUE)
autowikibrowserUM <- read.table("./AutoWikiBrowser/result-unStructured.csv", sep="\t", header=TRUE)

ccnetSM <- read.table("./CruiseControl_NET/result-semiStructured.csv", sep="\t", header=TRUE)
ccnetUM <- read.table("./CruiseControl_NET/result-unStructured.csv", sep="\t", header=TRUE)

eraserSM <- read.table("./Eraser/result-semiStructured.csv", sep="\t", header=TRUE)
eraserUM <- read.table("./Eraser/result-unStructured.csv", sep="\t", header=TRUE)

fireircclientSM <- read.table("./FireIRC_IRC_Client/result-semiStructured.csv", sep="\t", header=TRUE)
fireircclientUM <- read.table("./FireIRC_IRC_Client/result-unStructured.csv", sep="\t", header=TRUE)

ifolderSM <- read.table("./iFolder/result-semiStructured.csv", sep="\t", header=TRUE)
ifolderUM <- read.table("./iFolder/result-unStructured.csv", sep="\t", header=TRUE)

processhackerSM <- read.table("./Process_Hacker/result-semiStructured.csv", sep="\t", header=TRUE)
processhackerUM <- read.table("./Process_Hacker/result-unStructured.csv", sep="\t", header=TRUE)

rssbanditSM <- read.table("./RSS_Bandit/result-semiStructured.csv", sep="\t", header=TRUE)
rssbanditUM <- read.table("./RSS_Bandit/result-unStructured.csv", sep="\t", header=TRUE)

worldwindSM <- read.table("./NASA_worldwind/result-semiStructured.csv", sep="\t", header=TRUE)
worldwindUM <- read.table("./NASA_worldwind/result-unStructured.csv", sep="\t", header=TRUE)

#python
bitpimSM <- read.table("./BitPim/result-semiStructured.csv", sep="\t", header=TRUE)
bitpimUM <- read.table("./BitPim/result-unStructured.csv", sep="\t", header=TRUE)

emeseneSM <- read.table("./emesene/result-semiStructured.csv", sep="\t", header=TRUE)
emeseneUM <- read.table("./emesene/result-unStructured.csv", sep="\t", header=TRUE)

exeSM <- read.table("./eXe/result-semiStructured.csv", sep="\t", header=TRUE)
exeUM <- read.table("./eXe/result-unStructured.csv", sep="\t", header=TRUE)

matplotlibSM <- read.table("./matplotlib/result-semiStructured.csv", sep="\t", header=TRUE)
matplotlibUM <- read.table("./matplotlib/result-unStructured.csv", sep="\t", header=TRUE)

spamBayesSM <- read.table("./SpamBayes/result-semiStructured.csv", sep="\t", header=TRUE)
spamBayesUM <- read.table("./SpamBayes/result-unStructured.csv", sep="\t", header=TRUE)

wicdSM <- read.table("./Wicd/result-semiStructured.csv", sep="\t", header=TRUE)
wicdUM <- read.table("./Wicd/result-unStructured.csv", sep="\t", header=TRUE)

#java
drjavaSM <- read.table("./DrJava/result-semiStructured.csv", sep="\t", header=TRUE)
drjavaUM <- read.table("./DrJava/result-unStructured.csv", sep="\t", header=TRUE)

freecolSM <- read.table("./Freecol/result-semiStructured.csv", sep="\t", header=TRUE)
freecolUM <- read.table("./Freecol/result-unStructured.csv", sep="\t", header=TRUE)

genjSM <- read.table("./GenealogyJ/result-semiStructured.csv", sep="\t", header=TRUE)
genjUM <- read.table("./GenealogyJ/result-unStructured.csv", sep="\t", header=TRUE)

itextSM <- read.table("./iText/result-semiStructured.csv", sep="\t", header=TRUE)
itextUM <- read.table("./iText/result-unStructured.csv", sep="\t", header=TRUE)

jabrefSM <- read.table("./JabRef/result-semiStructured.csv", sep="\t", header=TRUE)
jabrefUM <- read.table("./JabRef/result-unStructured.csv", sep="\t", header=TRUE)

jeditSM <- read.table("./jEdit/result-semiStructured.csv", sep="\t", header=TRUE)
jeditUM <- read.table("./jEdit/result-unStructured.csv", sep="\t", header=TRUE)

jfreechartSM <- read.table("./JFreeChart/result-semiStructured.csv", sep="\t", header=TRUE)
jfreechartUM <- read.table("./JFreeChart/result-unStructured.csv", sep="\t", header=TRUE)

jmolSM <- read.table("./Jmol/result-semiStructured.csv", sep="\t", header=TRUE)
jmolUM <- read.table("./Jmol/result-unStructured.csv", sep="\t", header=TRUE)

pmdSM <- read.table("./PMD/result-semiStructured.csv", sep="\t", header=TRUE)
pmdUM <- read.table("./PMD/result-unStructured.csv", sep="\t", header=TRUE)

squirrelSM <- read.table("./SQuirrel_SQL/result-semiStructured.csv", sep="\t", header=TRUE)
squirrelUM <- read.table("./SQuirrel_SQL/result-unStructured.csv", sep="\t", header=TRUE)

###################################
# Global variables
###################################

SM <- "semistructured"
UM <- "unstructured"

###################################
# Functions
###################################

#builds dataframe from csv-input files
buildDataFrame <- function(project, semistructuredTable, unstructuredTable)
{
	SMRevisions <- semistructuredTable$Revisions
	UMRevisions <- unstructuredTable$Revisions
	
	SMconflicts <- semistructuredTable$Syntactic.Conflicts
	UMconflicts <- unstructuredTable$Syntactic.Conflicts	

	SMConfLines <- semistructuredTable$Confilicting.Lines.1 + semistructuredTable$Conflicting.Lines.2
	UMConfLines <- unstructuredTable$Confilicting.Lines.1   + unstructuredTable$Conflicting.Lines.2	

	SMfiles <- semistructuredTable$Number.of.Files
	UMfiles <- unstructuredTable$Number.of.Files
	
	SMsemConf <- semistructuredTable$Semantic.Conflicts
	UMsemConf <- unstructuredTable$Semantic.Conflicts

	dataFrameSM <- data.frame(as.factor(project), as.character(SMRevisions), 
	                          as.numeric(SMconflicts), as.numeric(SMConfLines), 
	                          as.numeric(SMfiles), as.factor(SM),
	                          as.numeric(SMsemConf))
	colnames(dataFrameSM) <- c("Programm", "Revisions", 
	                           "Conf", "ConfLines", 
	                           "ConfFiles", "Merge",
	                           "SemConf")

	dataFrameUM <- data.frame(as.factor(project), as.character(UMRevisions),
	                          as.numeric(UMconflicts), as.numeric(UMConfLines), 
	                          as.numeric(UMfiles),  as.factor(UM),
	                          as.numeric(UMsemConf))

	colnames(dataFrameUM) <- c("Programm", "Revisions", 
	                           "Conf", "ConfLines", 
	                           "ConfFiles", "Merge",
	                           "SemConf")

	dataFrame <- rbind(dataFrameSM, dataFrameUM)

	return(dataFrame)
}

countProjects <- function(language, dataFrame)
{
	print(language)
	programms <- subset(dataFrame,select=Programm)
	programms <- programms[which(!duplicated(programms)), ]
	print(paste("Number of projects: ",length(programms)))

	revisions <- subset(dataFrame,select=Revisions)
	revisions <- revisions[which(!duplicated(revisions)), ]
	print(paste("Number of merge scenarios: ",length(revisions)))

	conflictsSM <- subset(dataFrame,dataFrame$Merge==SM,select=c(Revisions,Conf))
	conflictsUM <- subset(dataFrame,dataFrame$Merge==UM,select=c(Revisions,Conf))
	conflictsSM <- sum(conflictsSM$Conf)
	conflictsUM <- sum(conflictsUM$Conf)
	print(paste("Total - conflictsSM: ",conflictsSM,"; conflictsUM: ",conflictsUM, percent(conflictsSM, conflictsUM)))

	conflictsSM <- subset(dataFrame,dataFrame$Merge==SM,select=c(Revisions,ConfLines))
	conflictsUM <- subset(dataFrame,dataFrame$Merge==UM,select=c(Revisions,ConfLines))
	conflictsSM <- sum(conflictsSM$Conf)
	conflictsUM <- sum(conflictsUM$Conf)
	print(paste("Total - confLinesSM: ",conflictsSM,"; confLinesUM: ",conflictsUM, percent(conflictsSM, conflictsUM)))


	conflictsSM <- subset(dataFrame,dataFrame$Merge==SM,select=c(Revisions,ConfFiles))
	conflictsUM <- subset(dataFrame,dataFrame$Merge==UM,select=c(Revisions,ConfFiles))
	conflictsSM <- sum(conflictsSM$Conf)
	conflictsUM <- sum(conflictsUM$Conf)
	print(paste("Total - confFilesSM: ",conflictsSM,"; confFilesUM: ",conflictsUM, percent(conflictsSM, conflictsUM)))

}

countConflicts <- function(dataFrame)
{

	cat("\n","Count conflicts total per project", "\n")
	ratio <- data.frame(factor(), numeric(), numeric(), numeric())
	colnames(ratio) <- c("Programm", "ConfSM", "ConfUM", "SMUMRatio")
	
	for (prog in levels(dataFrame$Programm))
    	{ 
		revisions <- subset(dataFrame, dataFrame$Programm==prog)

		conflictsSM <- subset(revisions,revisions$Merge==SM,select=c(Revisions,Conf))
		conflictsUM <- subset(revisions,revisions$Merge==UM,select=c(Revisions,Conf))
		conflictsSM <- sum(conflictsSM$Conf)
		conflictsUM <- sum(conflictsUM$Conf)
		output <- paste(prog," - conflictsSM: ",conflictsSM,"; conflictsUM: ",conflictsUM)
		
		output <- paste(output, percent(conflictsSM, conflictsUM))		
		print(output)		

		conflictratio <- conflictsSM/conflictsUM		

		newline <- data.frame(prog, conflictsSM, conflictsUM, conflictratio)
		colnames(newline) <- colnames(ratio) 
		ratio <- rbind(ratio, newline)
	}
	ratio <- ratio[!is.na(ratio[,"SMUMRatio"]),]
	cat("Mean ratio: ", mean(ratio$SMUMRatio),"\t=>",evalpercent(mean(ratio$SMUMRatio)),"\n")
	cat("SD: ", sd((1-ratio$SMUMRatio)*100),"%\n")
	cat("Min ratio: ",min(ratio$SMUMRatio),"\t=>",evalpercent(min(ratio$SMUMRatio)),"\n")
	
	minline <- apply(ratio[, "SMUMRatio",drop=FALSE], 2, which.min)
	print(ratio[minline, ])
	
	cat("Max ratio: ",max(ratio$SMUMRatio),"\t=>",evalpercent(max(ratio$SMUMRatio)),"\n")

	maxline <- apply(ratio[, "SMUMRatio",drop=FALSE], 2, which.max)
	print(ratio[maxline, ])
}

countConfLines <- function(dataFrame)
{

	cat("\n","Count conflicting lines total per project", "\n")
	ratio <- data.frame(factor(), numeric(), numeric(), numeric())
	colnames(ratio) <- c("Programm", "ConfLinesSM", "ConfLinesUM", "SMUMRatio")
	
	for (prog in levels(dataFrame$Programm))
    	{ 
		revisions <- subset(dataFrame, dataFrame$Programm==prog)

		conflictsSM <- subset(revisions,revisions$Merge==SM,select=c(Revisions,ConfLines))
		conflictsUM <- subset(revisions,revisions$Merge==UM,select=c(Revisions,ConfLines))
		conflictsSM <- sum(conflictsSM$Conf)
		conflictsUM <- sum(conflictsUM$Conf)
		output <- paste(prog," - conflictsSM: ",conflictsSM,"; conflictsUM: ",conflictsUM)
		
		output <- paste(output, percent(conflictsSM, conflictsUM))		
		print(output)		

		conflictratio <- conflictsSM/conflictsUM		

		newline <- data.frame(prog, conflictsSM, conflictsUM, conflictratio)
		colnames(newline) <- colnames(ratio) 
		ratio <- rbind(ratio, newline)
	}
	ratio <- ratio[!is.na(ratio[,"SMUMRatio"]),]
	cat("Mean ratio: ", mean(ratio$SMUMRatio),"\t=>",evalpercent(mean(ratio$SMUMRatio)),"\n")
	cat("SD: ", sd((1-ratio$SMUMRatio)*100),"%\n")
	cat("Min ratio: ",min(ratio$SMUMRatio),"\t=>",evalpercent(min(ratio$SMUMRatio)),"\n")

	minline <- apply(ratio[, "SMUMRatio",drop=FALSE], 2, which.min)
	print(ratio[minline, ])
	
	cat("Max ratio: ",max(ratio$SMUMRatio),"\t=>",evalpercent(max(ratio$SMUMRatio)),"\n")

	maxline <- apply(ratio[, "SMUMRatio",drop=FALSE], 2, which.max)
	print(ratio[maxline, ])
	

}

countConfFiles <- function(dataFrame)
{

	cat("\n","Count conflicting files total per project", "\n")
	ratio <- data.frame(factor(), numeric(), numeric(), numeric())
	colnames(ratio) <- c("Programm", "ConfFilesSM", "ConfFilesUM", "SMUMRatio")
	
	for (prog in levels(dataFrame$Programm))
    	{ 
		revisions <- subset(dataFrame, dataFrame$Programm==prog)

		conflictsSM <- subset(revisions,revisions$Merge==SM,select=c(Revisions,ConfFiles))
		conflictsUM <- subset(revisions,revisions$Merge==UM,select=c(Revisions,ConfFiles))
		conflictsSM <- sum(conflictsSM$Conf)
		conflictsUM <- sum(conflictsUM$Conf)
		output <- paste(prog," - conflictsSM: ",conflictsSM,"; conflictsUM: ",conflictsUM)
		
		output <- paste(output, percent(conflictsSM, conflictsUM))		
		print(output)		

		conflictratio <- conflictsSM/conflictsUM		

		newline <- data.frame(prog, conflictsSM, conflictsUM, conflictratio)
		colnames(newline) <- colnames(ratio) 
		ratio <- rbind(ratio, newline)
	}
	ratio <- ratio[!is.na(ratio[,"SMUMRatio"]),]
	cat("Mean ratio: ", mean(ratio$SMUMRatio),"\t=>",evalpercent(mean(ratio$SMUMRatio)),"\n")
	cat("SD: ", sd((1-ratio$SMUMRatio)*100),"%\n")
	cat("Min ratio: ",min(ratio$SMUMRatio),"\t=>",evalpercent(min(ratio$SMUMRatio)),"\n")

	minline <- apply(ratio[, "SMUMRatio",drop=FALSE], 2, which.min)
	print(ratio[minline, ])
	
	cat("Max ratio: ",max(ratio$SMUMRatio),"\t=>",evalpercent(max(ratio$SMUMRatio)),"\n")

	maxline <- apply(ratio[, "SMUMRatio",drop=FALSE], 2, which.max)
	print(ratio[maxline, ])
	

}


percent <- function(value1, value2)
{
	if(value1 < value2)		
	{
		percent <- 1-(value1/value2)
		percent <- percent * 100
		percent <- round(percent, digits=2)
		output <- paste("; reduction by ", percent, "%")
	}
	else
	{
		percent <- (value1/value2)-1
		percent <- percent * 100
		percent <- round(percent, digits=2)
		output <- paste("; increase by ", percent, "%")
	}
	return(output)
}

evalpercent <- function(value)
{
	if(value < 1)		
	{
		percent <- 1-value
		percent <- percent * 100
		percent <- round(percent, digits=2)
		output <- paste("reduction by ", percent, "%")
	}
	else
	{
		percent <- value-1
		percent <- percent * 100
		percent <- round(percent, digits=2)
		output <- paste("increase by ", percent, "%")
	}
	return(output)
}


minRatioConflicts <- function(dataFrame)
{

	cat("\n","Best and worst revision: Conflicts:", "\n")

	conflictsSM <- subset(dataFrame, dataFrame$Merge==SM, select=c(Programm, Revisions, Conf))
	names(conflictsSM)[names(conflictsSM)=="Conf"] <- "ConfSM"
	conflictsUM <- subset(dataFrame, dataFrame$Merge==UM, select=c(Conf))
	names(conflictsUM)[names(conflictsUM)=="Conf"] <- "ConfUM"

	conflicts <- cbind(conflictsSM, conflictsUM)
	conflicts <- cbind(conflicts, "SMUMRatio"=(conflicts$ConfSM/conflicts$ConfUM))

	conflictsNA <- conflicts[!is.na(conflicts[,"SMUMRatio"]),]
	
	cat("Min ratio: ", min(conflictsNA$SMUMRatio),"\t=>", evalpercent(min(conflictsNA$SMUMRatio)),"\n")
	minline <- apply(conflictsNA[, "SMUMRatio",drop=FALSE], 2, which.min)
	print(conflicts[minline, ])

	cat("Max ratio: ",max(conflictsNA$SMUMRatio),"\t=>", evalpercent(max(conflictsNA$SMUMRatio)),"\n")
	maxline <- apply(conflictsNA[, "SMUMRatio",drop=FALSE], 2, which.max)
	print(conflicts[maxline, ])
}

minRatioConfLines <- function(dataFrame)
{

	cat("\n","Best and worst revision: Conflicting lines: ", "\n")

	conflictsSM <- subset(dataFrame, dataFrame$Merge==SM, select=c(Programm, Revisions, ConfLines))
	names(conflictsSM)[names(conflictsSM)=="ConfLines"] <- "ConfLinesSM"
	conflictsUM <- subset(dataFrame, dataFrame$Merge==UM, select=c(ConfLines))
	names(conflictsUM)[names(conflictsUM)=="ConfLines"] <- "ConfLinesUM"

	conflicts <- cbind(conflictsSM, conflictsUM)
	conflicts <- cbind(conflicts, "SMUMRatio"=(conflicts$ConfLinesSM/conflicts$ConfLinesUM))

	conflictsNA <- conflicts[!is.na(conflicts[,"SMUMRatio"]),]

	cat("Min ratio: ", min(conflictsNA$SMUMRatio),"\t=>", evalpercent(min(conflictsNA$SMUMRatio)),"\n")
	minline <- apply(conflictsNA[, "SMUMRatio",drop=FALSE], 2, which.min)
	print(conflicts[minline, ])	

	cat("Max ratio: ",max(conflictsNA$SMUMRatio),"\t=>", evalpercent(max(conflictsNA$SMUMRatio)),"\n")
	maxline <- apply(conflictsNA[, "SMUMRatio",drop=FALSE], 2, which.max)
	print(conflicts[maxline, ])
}

minRatioConfFiles <- function(dataFrame)
{
	cat("\n","Best and worst revision: Conflicting files: ", "\n")

	conflictsSM <- subset(dataFrame, dataFrame$Merge==SM, select=c(Programm, Revisions, ConfFiles))
	names(conflictsSM)[names(conflictsSM)=="ConfFiles"] <- "ConfFilesSM"
	conflictsUM <- subset(dataFrame, dataFrame$Merge==UM, select=c(ConfFiles))
	names(conflictsUM)[names(conflictsUM)=="ConfFiles"] <- "ConfFilesUM"

	conflicts <- cbind(conflictsSM, conflictsUM)
	conflicts <- cbind(conflicts, "SMUMRatio"=(conflicts$ConfFilesSM/conflicts$ConfFilesUM))

	conflictsNA <- conflicts[!is.na(conflicts[,"SMUMRatio"]),]

	cat("Min ratio: ", min(conflictsNA$SMUMRatio),"\t=>", evalpercent(min(conflictsNA$SMUMRatio)),"\n")
	minline <- apply(conflictsNA[, "SMUMRatio",drop=FALSE], 2, which.min)
	print(conflicts[minline, ])

	cat("Max ratio: ",max(conflictsNA$SMUMRatio),"\t=>", evalpercent(max(conflictsNA$SMUMRatio)),"\n")
	maxline <- apply(conflictsNA[, "SMUMRatio",drop=FALSE], 2, which.max)
	print(conflicts[maxline, ])
}

evaluteResults <- function(dataFrame)
{
	cat("\n","Evaluate Results", "\n")
	result <- data.frame(factor(), character(), 
			numeric(), numeric(), numeric(),
			numeric(), numeric(), numeric(),
			numeric(), numeric(), numeric())
	colnames(result) <- c("Programm", "Revision", 
			"ConfSMbesser", "Confgleich", "ConfUMbesser",
			"ConfLinesSMbesser", "ConfLinesgleich", "ConfLinesUMbesser",
			"ConfFilesSMbesser", "ConfFilesgleich", "ConfFilesUMbesser")

	for (prog in levels(dataFrame$Programm))
    	{ 
		conflictsSM <- subset(dataFrame,dataFrame$Programm==prog&dataFrame$Merge==SM,
			select=c(Programm, Revisions,Conf,ConfLines,ConfFiles))
		names(conflictsSM)[names(conflictsSM)=="Conf"] <- "ConfSM"
		names(conflictsSM)[names(conflictsSM)=="ConfLines"] <- "ConfLinesSM"
		names(conflictsSM)[names(conflictsSM)=="ConfFiles"] <- "ConfFilesSM"

		conflictsUM <- subset(dataFrame,dataFrame$Programm==prog&dataFrame$Merge==UM,
			select=c(Conf,ConfLines,ConfFiles))
		names(conflictsUM)[names(conflictsUM)=="Conf"] <- "ConfUM"
		names(conflictsUM)[names(conflictsUM)=="ConfLines"] <- "ConfLinesUM"
		names(conflictsUM)[names(conflictsUM)=="ConfFiles"] <- "ConfFilesUM"
	
		conflicts <- cbind(conflictsSM, conflictsUM)

		for (rev in conflicts$Revision)
		{
			confsmbesser <- 0;
			confgleich <- 0;
			confumbesser <- 0;			
			rev = subset(conflicts,conflicts$Revision==rev)
			if(rev$ConfSM > rev$ConfUM)
			{
				confumbesser <- 1
			}
			if(rev$ConfSM == rev$ConfUM)
			{
				confgleich <- 1
			}
			if(rev$ConfSM < rev$ConfUM)
			{
				confsmbesser <- 1
			}

			conflinessmbesser <- 0
			conflinesgleich <- 0
			conflinesumbesser <- 0
			if(rev$ConfLinesSM > rev$ConfLinesUM)
			{
				conflinesumbesser <- 1
			}			
			if(rev$ConfLinesUM == rev$ConfLinesSM)
			{
				conflinesgleich <- 1
			}
			if(rev$ConfLinesSM < rev$ConfLinesUM)
			{
				conflinessmbesser <- 1
			}

			conffilessmbesser <- 0
			conffilesgleich <- 0
			conffilesumbesser <- 0
			if(rev$ConfFilesSM > rev$ConfFilesUM)
			{
				conffilesumbesser <- 1
			}
			if(rev$ConfFilesSM == rev$ConfFilesUM)
			{
				conffilesgleich <- 1
			}
			if(rev$ConfFilesSM < rev$ConfFilesUM)
			{
				conffilessmbesser <- 1
			}

			newline <- data.frame(prog, rev$Revision, 
				confsmbesser, confgleich, confumbesser,
				conflinessmbesser, conflinesgleich, conflinesumbesser,
				conffilessmbesser, conffilesgleich, conffilesumbesser)
			colnames(newline) <- colnames(result)
			result <- rbind(result, newline)
		}


	}
	colnames(result) <- c("Programm", "Revision", 
			"ConfSMbesser", "Confgleich", "ConfUMbesser",
			"ConfLinesSMbesser", "ConfLinesgleich", "ConfLinesUMbesser",
			"ConfFilesSMbesser", "ConfFilesgleich", "ConfFilesUMbesser")
	for (prog in levels(result$Programm))
	{
		resultsub <- subset(result, result$Programm==prog)
		confsmbesser <- sum(resultsub$ConfSMbesser)
		confgleich <- sum(resultsub$Confgleich)
		confumbesser <- sum(resultsub$ConfUMbesser)
	
		conflinessmbesser <- sum(resultsub$ConfLinesSMbesser)
		conflinesgleich <- sum(resultsub$ConfLinesgleich)
		conflinesumbesser <- sum(resultsub$ConfLinesUMbesser)

		conffilessmbesser <- sum(resultsub$ConfFilesSMbesser)
		conffilesgleich <- sum(resultsub$ConfFilesgleich)
		conffilesumbesser <- sum(resultsub$ConfFilesUMbesser)		
	}
	confsmbesser <- sum(result$ConfSMbesser)
	confgleich <- sum(result$Confgleich)
	confumbesser <- sum(result$ConfUMbesser)
	
	conflinessmbesser <- sum(result$ConfLinesSMbesser)
	conflinesgleich <- sum(result$ConfLinesgleich)
	conflinesumbesser <- sum(result$ConfLinesUMbesser)

	conffilessmbesser <- sum(result$ConfFilesSMbesser)
	conffilesgleich <- sum(result$ConfFilesgleich)
	conffilesumbesser <- sum(result$ConfFilesUMbesser)

	revisions <- subset(dataFrame,select=Revisions)
	revisions <- revisions[which(!duplicated(revisions)), ]

	cat("\nNumber of merge scenarios: ",length(revisions))

	cat("\nTotal - ","ConfSM:",confsmbesser,"\tConfGleich:", confgleich, "\tConfUM:", confumbesser)
	cat("\nTotal - ","ConfLinesSM:",conflinessmbesser,"\tConfLinesGleich:", conflinesgleich, "\tConfLinesUM:", conflinesumbesser)
	cat("\nTotal - ","ConfFilesSM:",conffilessmbesser,"\tConfFilesGleich:", conffilesgleich, "\tConfFilesUM:", conffilesumbesser, "\n")


	##conflicts
	cat("\n","\n","Semistructed Merge at conflicts better")
	confsmbesser <- subset(result,result$ConfSMbesser==1,select=Revision)
	confsmbesser <- merge(dataFrame, confsmbesser, by.x="Revisions", by.y="Revision")
	countConflicts(confsmbesser)	
	minRatioConflicts(confsmbesser)

	cat("\n","\n","Semistructed Merge and Unstructured Merge same conflict numbers")
	confgleich <- subset(result,result$Confgleich==1,select=Revision)
	confgleich <- merge(dataFrame, confgleich, by.x="Revisions", by.y="Revision")
	countConflicts(confgleich)		
	minRatioConflicts(confgleich)

	cat("\n","\n","Unstructured Merge at conflicts better")
	confumbesser <- subset(result,result$ConfUMbesser==1,select=Revision)
	confumbesser <- merge(dataFrame, confumbesser, by.x="Revisions", by.y="Revision")
	countConflicts(confumbesser)		
	minRatioConflicts(confumbesser)

	##lines
	cat("\n","\n","Semistructed Merge at conflicting lines better")
	confsmbesser <- subset(result,result$ConfLinesSMbesser==1,select=Revision)
	confsmbesser <- merge(dataFrame, confsmbesser, by.x="Revisions", by.y="Revision")
	countConfLines(confsmbesser)
	minRatioConfLines(confsmbesser)

	cat("\n","\n","Semistructed Merge and Unstructured Merge same conflicting lines numbers")
	confgleich <- subset(result,result$ConfLinesgleich==1,select=Revision)
	confgleich <- merge(dataFrame, confgleich, by.x="Revisions", by.y="Revision")
	countConfLines(confgleich)	
	minRatioConfLines(confgleich)

	cat("\n","\n","Unstructured Merge at conflicting lines better")
	confumbesser <- subset(result,result$ConfLinesUMbesser==1,select=Revision)
	confumbesser <- merge(dataFrame, confumbesser, by.x="Revisions", by.y="Revision")
	countConfLines(confumbesser)	
	minRatioConfLines(confumbesser)

	##files
	cat("\n","\n","Semistructed Merge at conflicting files better")
	confsmbesser <- subset(result,result$ConfFilesSMbesser==1,select=Revision)
	confsmbesser <- merge(dataFrame, confsmbesser, by.x="Revisions", by.y="Revision")
	countConfFiles(confsmbesser)	
	minRatioConfFiles(confsmbesser)

	cat("\n","\n","Semistructed Merge and Unstructured Merge same conflicting files numbers")
	confgleich <- subset(result,result$ConfFilesgleich==1,select=Revision)
	confgleich <- merge(dataFrame, confgleich, by.x="Revisions", by.y="Revision")
	countConfFiles(confgleich)	
	minRatioConfFiles(confgleich)

	cat("\n","\n","Unstructured Merge at conflicting files better")
	confumbesser <- subset(result,result$ConfFilesUMbesser==1,select=Revision)
	confumbesser <- merge(dataFrame, confumbesser, by.x="Revisions", by.y="Revision")
	countConfFiles(confumbesser)
	minRatioConfFiles(confumbesser)
}	



###################################
# Operations
###################################

#build dataframes from input tables
#java
#single tables
df1 <- buildDataFrame("DrJava", drjavaSM, drjavaUM)
df2 <- buildDataFrame("FreeCol", freecolSM, freecolUM)
df3 <- buildDataFrame("GenealogyJ", genjSM, genjUM)
df4 <- buildDataFrame("iText", itextSM, itextUM)
df5 <- buildDataFrame("Jabref", jabrefSM, jabrefUM)
df6 <- buildDataFrame("jEdit", jeditSM, jeditUM)
df7 <- buildDataFrame("JFreechart", jfreechartSM, jfreechartUM)
df8 <- buildDataFrame("Jmol", jmolSM, jmolUM)
df9 <- buildDataFrame("PMD", pmdSM, pmdUM)
df10 <- buildDataFrame("SQuirrelSQL", squirrelSM, squirrelUM)

#unite tables
allJava <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10)

#csharp
dfcs1 <- buildDataFrame("AutoWikiBrowser", autowikibrowserSM, autowikibrowserUM)
dfcs2 <- buildDataFrame("CruiseControl.NET", ccnetSM, ccnetUM)
dfcs3 <- buildDataFrame("Eraser", eraserSM, eraserUM)
dfcs4 <- buildDataFrame("FireIRC IRC Client", fireircclientSM, fireircclientUM)
dfcs5 <- buildDataFrame("iFolder", ifolderSM, ifolderUM)
dfcs6 <- buildDataFrame("Process Hacker", processhackerSM, processhackerUM)
dfcs7 <- buildDataFrame("RSS Bandit", rssbanditSM, rssbanditUM)
dfcs8 <- buildDataFrame("NASA WorldWind", worldwindSM, worldwindUM)

#unite tables
allCSharp <- rbind(dfcs1, dfcs2, dfcs3, dfcs4, dfcs5, dfcs6, dfcs7, dfcs8)

#python
dfpy1 <- buildDataFrame("BitPim", bitpimSM, bitpimUM)
dfpy2 <- buildDataFrame("emesene", emeseneSM, emeseneUM)
dfpy3 <- buildDataFrame("eXe", exeSM, exeUM)
dfpy4 <- buildDataFrame("matplotlib", matplotlibSM, matplotlibUM)
dfpy5 <- buildDataFrame("SpamBayes", spamBayesSM, spamBayesUM)
dfpy6 <- buildDataFrame("Wicd", wicdSM, wicdUM)

#unite tables
allPython <- rbind(dfpy1, dfpy2, dfpy3, dfpy4, dfpy5, dfpy6)

#all project dataframe
all <- rbind(allJava, allCSharp, allPython)

#evalute and print results
evaluteResults(all)

