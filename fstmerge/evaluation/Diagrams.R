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

JabrefSM <- read.table("./JabRef/result-semiStructured.csv", sep="\t", header=TRUE)
JabrefUM <- read.table("./JabRef/result-unStructured.csv", sep="\t", header=TRUE)

jeditSM <- read.table("./jEdit/result-semiStructured.csv", sep="\t", header=TRUE)
jeditUM <- read.table("./jEdit/result-unStructured.csv", sep="\t", header=TRUE)

JFreechartSM <- read.table("./JFreeChart/result-semiStructured.csv", sep="\t", header=TRUE)
JFreechartUM <- read.table("./JFreeChart/result-unStructured.csv", sep="\t", header=TRUE)

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

# color of semistructured lines
COLSM <- "green"
#COLSM <- "gray95"

# color of unstructured lines
COLUM <- "red"
#COLUM <- "gray25"

# Pointtype
POINTTYPE <- "o"
POINTCH <- 20

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

# draw a conflict bar plot
drawConflictsBarPlot <- function(projectName, dataFrame, legendposition)
{	
	barplotFrameSM <- subset(dataFrame,dataFrame$Programm==projectName& dataFrame$Merge==SM,select=Conf)
	names(barplotFrameSM)[names(barplotFrameSM)=="Conf"] <- "Conf-SM"
	
	barplotFrameUM <- subset(dataFrame,dataFrame$Programm==projectName& dataFrame$Merge==UM,select=Conf)
	names(barplotFrameUM)[names(barplotFrameUM)=="Conf"] <- "Conf-UM"
		
	barplotFrame <- cbind(barplotFrameSM, barplotFrameUM)

	barplotMatrix <- as.matrix(barplotFrame)
	barplotMatrix <- t(barplotMatrix)
	revs <- subset(dataFrame,dataFrame$Programm==projectName,select=Revisions)

	mp <- barplot(barplotMatrix, beside = TRUE, axisnames=FALSE, col=c(COLSM,COLUM))

	#draw axis
	axis(1, at=mp[1, ], labels=FALSE, tick=FALSE)
	labels <- paste(revs$Revisions)
	text(mp[1, ], par("usr")[3]-0.05, srt=-60, adj=c(-0.15,0), labels=labels, xpd=TRUE, cex=1) 
	
	#draw grid
	grid(lty="dotdash", col="darkgrey")	

	
	# draw legend
	drawLegendBarPlot(legendposition)

	# draw title
	title(ylab="Number of conflicts",cex.lab=1)
}

drawConflictLinesBarPlot <- function(projectName, dataFrame, legendposition)
{
	
	barplotFrameSM <- subset(dataFrame,dataFrame$Programm==projectName& dataFrame$Merge==SM,select=ConfLines)
	names(barplotFrameSM)[names(barplotFrameSM)=="ConfLines"] <- "ConfLines-SM"
	
	barplotFrameUM <- subset(dataFrame,dataFrame$Programm==projectName& dataFrame$Merge==UM,select=ConfLines)
	names(barplotFrameUM)[names(barplotFrameUM)=="ConfLines"] <- "ConfLines-UM"
	
	barplotFrame <- cbind(barplotFrameSM, barplotFrameUM)
	
	barplotMatrix <- as.matrix(barplotFrame)
	barplotMatrix <- t(barplotMatrix)
	revs <- subset(dataFrame,dataFrame$Programm==projectName,select=Revisions)
	
	mp <- barplot(barplotMatrix, beside = TRUE, axisnames=FALSE, col=c(COLSM,COLUM))

	#draw axis
	axis(1, at=mp[1, ], labels=FALSE, tick=FALSE)
	labels <- paste(revs$Revisions)
	text(mp[1, ], par("usr")[3]-0.05, srt=-60, adj=c(-0.15,0), labels=labels, xpd=TRUE, cex=1) 
	
	#draw grid
	grid(lty="dotdash", col="darkgrey")	
	
	# draw legend
	drawLegendBarPlot(legendposition)

	# draw title
	title(ylab="Number of conflicting lines of code",cex.lab=1)	
}

drawConflictFilesBarPlot <- function(projectName, dataFrame, legendposition)
{
	
	barplotFrameSM <- subset(dataFrame,dataFrame$Programm==projectName& dataFrame$Merge==SM,select=ConfFiles)
	names(barplotFrameSM)[names(barplotFrameSM)=="ConfFiles"] <- "ConfFiles-SM"
	
	barplotFrameUM <- subset(dataFrame,dataFrame$Programm==projectName& dataFrame$Merge==UM,select=ConfFiles)
	names(barplotFrameUM)[names(barplotFrameUM)=="ConfFiles"] <- "ConfFiles-UM"
	
	barplotFrame <- cbind(barplotFrameSM, barplotFrameUM)
	
	barplotMatrix <- as.matrix(barplotFrame)
	barplotMatrix <- t(barplotMatrix)
	revs <- subset(dataFrame,dataFrame$Programm==projectName,select=Revisions)
	
	mp <- barplot(barplotMatrix, beside = TRUE, axisnames=FALSE, col=c(COLSM,COLUM))

	#draw axis
	axis(1, at=mp[1, ], labels=FALSE, tick=FALSE)
	labels <- paste(revs$Revisions)
	text(mp[1, ], par("usr")[3]-0.05, srt=-60, adj=c(-0.15,0), labels=labels, xpd=TRUE, cex=1) 

	
	#draw grid
	grid(lty="dotdash", col="darkgrey")	

	
	# draw legend
	drawLegendBarPlot(legendposition)

	# draw title
	title(ylab="Number of conflicting files",cex.lab=1)
}


drawConflictSemanticBarPlot <- function(projectName, dataFrame, legendposition)
{
	
	barplotFrameSM <- subset(dataFrame,dataFrame$Programm==projectName& dataFrame$Merge==SM,select=SemConf)
	names(barplotFrameSM)[names(barplotFrameSM)=="SemConf"] <- "SemConf-SM"
	
	barplotFrameUM <- subset(dataFrame,dataFrame$Programm==projectName& dataFrame$Merge==UM,select=SemConf)
	names(barplotFrameUM)[names(barplotFrameUM)=="SemConf"] <- "SemConf-UM"
	
	barplotFrame <- cbind(barplotFrameSM, barplotFrameUM)
	
	barplotMatrix <- as.matrix(barplotFrame)
	barplotMatrix <- t(barplotMatrix)
	revs <- subset(dataFrame,dataFrame$Programm==projectName,select=Revisions)
	
	mp <- barplot(barplotMatrix, beside = TRUE, axisnames=FALSE, col=c(COLSM,COLUM))

	#draw axis
	axis(1, at=mp[1, ], labels=FALSE, tick=FALSE)
	labels <- paste(revs$Revisions)
	text(mp[1, ], par("usr")[3]-0.05, srt=-60, adj=c(-0.15,0), labels=labels, xpd=TRUE, cex=1) 

	
	#draw grid
	grid(lty="dotdash", col="darkgrey")	

	
	# draw legend
	drawLegendBarPlot(legendposition)

	# draw title
	title(ylab="Number of semantic conflicts",cex.lab=1)
}


# function for axis and grid
drawCaption <- function(table)
{
	# draw revision axis
	axis(	1, 
		at = table$Revisions, 
		labels = FALSE, 
		tick = TRUE
	)
	
	# labels for xaxis
	labels <- paste(table$Revisions)
	
	# draw labels on xaxis
	text(	
		table$Revisions, 
		par("usr")[3], 
		srt = -60, 
		labels = labels, 
		adj = c(-0.1,1), 
		xpd=TRUE,
		cex=1
	)
	
	#draw grid
	grid(lty="dotdash", col="darkgrey")	
}


drawLegendBarPlot <- function(position)
{
 legend(position,                             
         legend=c("Semistructured Merge","Unstructured Merge"),
         cex=0.7,            
         fill=c(COLSM,COLUM),
         bg="white")  
}



drawBarplotPNGs <- function()
{

WIDTH <- 400
HEIGHT <- 300

#csharp
#autowikibrowser
png(filename="./autowikibrowserc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictsBarPlot("AutoWikiBrowser", autowikibrowserdf, "topleft")
dev.off()

png(filename="./autowikibrowsercl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictLinesBarPlot("AutoWikiBrowser", autowikibrowserdf, "topleft")
dev.off()

png(filename="./autowikibrowsercf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictFilesBarPlot("AutoWikiBrowser", autowikibrowserdf, "topleft")
dev.off()

png(filename="./autowikibrowsersc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictSemanticBarPlot("AutoWikiBrowser", autowikibrowserdf, "topright")
dev.off()


#ccnet
png(filename="./ccnetc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictsBarPlot("CruiseControl.NET", ccnetdf, "topright")
dev.off()

png(filename="./ccnetcl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictLinesBarPlot("CruiseControl.NET", ccnetdf, "topright")
dev.off()

png(filename="./ccnetcf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictFilesBarPlot("CruiseControl.NET", ccnetdf, "topright")
dev.off()

png(filename="./ccnetsc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictSemanticBarPlot("CruiseControl.NET", ccnetdf, "topright")
dev.off()


#eraser
png(filename="./eraserc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictsBarPlot("Eraser", eraserdf, "topleft")
dev.off()

png(filename="./erasercl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictLinesBarPlot("Eraser", eraserdf, "topleft")
dev.off()

png(filename="./erasercf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictFilesBarPlot("Eraser", eraserdf, "topleft")
dev.off()

png(filename="./erasersc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictSemanticBarPlot("Eraser", eraserdf, "topleft")
dev.off()

#fireircclient
png(filename="./fireircclientc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictsBarPlot("FireIRC IRC Client", fireircclientdf, "topleft")
dev.off()

png(filename="./fireircclientcl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictLinesBarPlot("FireIRC IRC Client", fireircclientdf, "topleft")
dev.off()

png(filename="./fireircclientcf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictFilesBarPlot("FireIRC IRC Client", fireircclientdf, "topleft")
dev.off()

png(filename="./fireircclientsc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictSemanticBarPlot("FireIRC IRC Client", fireircclientdf, "topleft")
dev.off()

#iFolder
png(filename="./ifolderc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictsBarPlot("iFolder", ifolderdf, "topright")
dev.off()

png(filename="./ifoldercl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictLinesBarPlot("iFolder", ifolderdf, "topleft")
dev.off()

png(filename="./ifoldercf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictFilesBarPlot("iFolder", ifolderdf, "topleft")
dev.off()

png(filename="./ifoldersc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictSemanticBarPlot("iFolder", ifolderdf, "topright")
dev.off()

#processhacker
png(filename="./processhackerc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictsBarPlot("Process Hacker", processhackerdf, "topleft")
dev.off()

png(filename="./processhackercl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictLinesBarPlot("Process Hacker", processhackerdf, "topleft")
dev.off()

png(filename="./processhackercf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictFilesBarPlot("Process Hacker", processhackerdf, "topleft")
dev.off()

png(filename="./processhackersc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictSemanticBarPlot("Process Hacker", processhackerdf, "topleft")
dev.off()

#rssbandit
png(filename="./rssbanditc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictsBarPlot("RSS Bandit", rssbanditdf, "topleft")
dev.off()

png(filename="./rssbanditcl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictLinesBarPlot("RSS Bandit", rssbanditdf, "topleft")
dev.off()

png(filename="./rssbanditcf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictFilesBarPlot("RSS Bandit", rssbanditdf, "topleft")
dev.off()

png(filename="./rssbanditsc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictSemanticBarPlot("RSS Bandit", rssbanditdf, "topleft")
dev.off()

#worldwind
png(filename="./worldwindc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictsBarPlot("NASA WorldWind", worldwinddf, "topleft")
dev.off()

png(filename="./worldwindcl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictLinesBarPlot("NASA WorldWind", worldwinddf, "topright")
dev.off()

png(filename="./worldwindcf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictFilesBarPlot("NASA WorldWind", worldwinddf, "topright")
dev.off()

png(filename="./worldwindsc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictSemanticBarPlot("NASA WorldWind", worldwinddf, "topleft")
dev.off()

#python
#BitPim
png(filename="./bitpimc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictsBarPlot("BitPim", bitpimdf, "topleft")
dev.off()

png(filename="./bitpimcl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictLinesBarPlot("BitPim", bitpimdf, "topleft")
dev.off()

png(filename="./bitpimcf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictFilesBarPlot("BitPim", bitpimdf, "topleft")
dev.off()

png(filename="./bitpimsc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictSemanticBarPlot("BitPim", bitpimdf, "topleft")
dev.off()

#emesene
png(filename="./emesenec-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictsBarPlot("emesene", emesenedf, "topleft")
dev.off()

png(filename="./emesenecl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictLinesBarPlot("emesene", emesenedf, "topleft")
dev.off()

png(filename="./emesenecf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictFilesBarPlot("emesene", emesenedf, "topright")
dev.off()

png(filename="./emesenesc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictSemanticBarPlot("emesene", emesenedf, "topright")
dev.off()

#exe
png(filename="./exec-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictsBarPlot("eXe", exedf, "topright")
dev.off()

png(filename="./execl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictLinesBarPlot("eXe", exedf, "topright")
dev.off()

png(filename="./execf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictFilesBarPlot("eXe", exedf, "topright")
dev.off()

png(filename="./exesc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictSemanticBarPlot("eXe", exedf, "topright")
dev.off()

#matplotlib
png(filename="./matplotlibc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictsBarPlot("matplotlib", matplotlibdf, "topleft")
dev.off()

png(filename="./matplotlibcl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictLinesBarPlot("matplotlib", matplotlibdf, "topleft")
dev.off()

png(filename="./matplotlibcf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictFilesBarPlot("matplotlib", matplotlibdf, "topleft")
dev.off()

png(filename="./matplotlibsc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictSemanticBarPlot("matplotlib", matplotlibdf, "topleft")
dev.off()

#SpamBayes
png(filename="./spambayesc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictsBarPlot("SpamBayes", spambayesdf, "topleft")
dev.off()

png(filename="./spambayescl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictLinesBarPlot("SpamBayes", spambayesdf, "topleft")
dev.off()

png(filename="./spambayescf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictFilesBarPlot("SpamBayes", spambayesdf, "topleft")
dev.off()

png(filename="./spambayessc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictSemanticBarPlot("SpamBayes", spambayesdf, "topleft")
dev.off()

#Wicd
png(filename="./wicdc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictsBarPlot("Wicd", wicddf, "topleft")
dev.off()

png(filename="./wicdcl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictLinesBarPlot("Wicd", wicddf, "topleft")
dev.off()

png(filename="./wicdcf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictFilesBarPlot("Wicd", wicddf, "topleft")
dev.off()

png(filename="./wicdsc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictSemanticBarPlot("Wicd", wicddf, "topleft")
dev.off()

#java
#drjava
png(filename="./drjavac-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictsBarPlot("DrJava", drjavadf, "topright")
dev.off()

png(filename="./drjavacl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictLinesBarPlot("DrJava", drjavadf, "topright")
dev.off()

png(filename="./drjavacf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictFilesBarPlot("DrJava", drjavadf, "topright")
dev.off()

png(filename="./drjavasc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictSemanticBarPlot("DrJava", drjavadf, "topright")
dev.off()

#freecol
png(filename="./freecolc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictsBarPlot("FreeCol", freecoldf, "topleft")
dev.off()

png(filename="./freecolcl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictLinesBarPlot("FreeCol", freecoldf, "topleft")
dev.off()

png(filename="./freecolcf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2)) 
drawConflictFilesBarPlot("FreeCol", freecoldf, "topleft")
dev.off()

png(filename="./freecolsc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictSemanticBarPlot("FreeCol", freecoldf, "topleft")
dev.off()

#genj
png(filename="./genjc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictsBarPlot("GenealogyJ", genjdf, "topright")
dev.off()

png(filename="./genjcl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictLinesBarPlot("GenealogyJ", genjdf, "topright")
dev.off()

png(filename="./genjcf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictFilesBarPlot("GenealogyJ", genjdf, "topright")
dev.off()

png(filename="./genjsc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictSemanticBarPlot("GenealogyJ", genjdf, "topright")
dev.off()

#iText
png(filename="./itextc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictsBarPlot("iText", itextdf, "topleft")
dev.off()

png(filename="./itextcl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictLinesBarPlot("iText", itextdf, "topleft")
dev.off()

png(filename="./itextcf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictFilesBarPlot("iText", itextdf, "topleft")
dev.off()

png(filename="./itextsc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictSemanticBarPlot("iText", itextdf, "topleft")
dev.off()

#Jabref
png(filename="./jabrefc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictsBarPlot("JabRef", jabrefdf, "topleft")

png(filename="./jabrefcl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictLinesBarPlot("JabRef", jabrefdf, "topleft")
dev.off()

png(filename="./jabrefcf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictFilesBarPlot("JabRef", jabrefdf, "topleft")
dev.off()

png(filename="./jabrefsc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictSemanticBarPlot("JabRef", jabrefdf, "topleft")
dev.off()

#jedit
png(filename="./jeditc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(7,5,1,3))
drawConflictsBarPlot("jEdit", jeditdf, "topleft")
dev.off()

png(filename="./jeditcl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(7,5,1,3))
drawConflictLinesBarPlot("jEdit", jeditdf, "topright")
dev.off()

png(filename="./jeditcf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(7,5,1,3))
drawConflictFilesBarPlot("jEdit", jeditdf, "topleft")
dev.off()

png(filename="./jeditsc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(7,5,1,3))
drawConflictSemanticBarPlot("jEdit", jeditdf, "topleft")
dev.off()

#jfreechart
png(filename="./jfreechartc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictsBarPlot("JFreechart", jfreechartdf, "topleft")
dev.off()

png(filename="./jfreechartcl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictLinesBarPlot("JFreechart", jfreechartdf, "topleft")
dev.off()

png(filename="./jfreechartcf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictFilesBarPlot("JFreechart", jfreechartdf, "topleft")
dev.off()

png(filename="./jfreechartsc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictSemanticBarPlot("JFreechart", jfreechartdf, "topleft")
dev.off()

#jmol
png(filename="./jmolc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(7,5,1,3))
drawConflictsBarPlot("Jmol", jmoldf,"topleft")
dev.off()

png(filename="./jmolcl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(7,5,1,3))
drawConflictLinesBarPlot("Jmol", jmoldf, "topleft")
dev.off()

png(filename="./jmolcf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(7,5,1,3))
drawConflictFilesBarPlot("Jmol", jmoldf,"topleft")
dev.off()

png(filename="./jmolsc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(7,5,1,3))
drawConflictSemanticBarPlot("Jmol", jmoldf, "topleft")
dev.off()

#pmd
png(filename="./pmdc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictsBarPlot("PMD", pmddf,"topleft")
dev.off()

png(filename="./pmdcl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictLinesBarPlot("PMD", pmddf, "topleft")
dev.off()

png(filename="./pmdcf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictFilesBarPlot("PMD", pmddf,"topleft")
dev.off()

png(filename="./pmdsc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictSemanticBarPlot("PMD", pmddf, "topleft")
dev.off()

#squirrel sql
png(filename="./squirrelsqlc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictsBarPlot("SQuirrelSQL", squirreldf,"topleft")
dev.off()

png(filename="./squirrelsqlcl-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictLinesBarPlot("SQuirrelSQL", squirreldf, "topleft")
dev.off()

png(filename="./squirrelsqlcf-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictFilesBarPlot("SQuirrelSQL", squirreldf,"topleft")
dev.off()

png(filename="./squirrelsqlsc-Barplot.png",width=WIDTH,height=HEIGHT)
par(mar=c(6,5,1,2))
drawConflictSemanticBarPlot("SQuirrelSQL", squirreldf, "topleft")
dev.off()
}

###################################
# Operations
###################################

### Build DataFrames Start

#build dataframes from input tables
#single tables
drjavadf <- buildDataFrame("DrJava", drjavaSM, drjavaUM)
freecoldf <- buildDataFrame("FreeCol", freecolSM, freecolUM)
genjdf <- buildDataFrame("GenealogyJ", genjSM, genjUM)
itextdf <- buildDataFrame("iText", itextSM, itextUM)
jabrefdf <- buildDataFrame("JabRef", JabrefSM, JabrefUM)
jeditdf <- buildDataFrame("jEdit", jeditSM, jeditUM)
jfreechartdf <- buildDataFrame("JFreechart", JFreechartSM, JFreechartUM)
jmoldf <- buildDataFrame("Jmol", jmolSM, jmolUM)
pmddf <- buildDataFrame("PMD", pmdSM, pmdUM)
squirreldf <- buildDataFrame("SQuirrelSQL", squirrelSM, squirrelUM)

#csharp
autowikibrowserdf <- buildDataFrame("AutoWikiBrowser", autowikibrowserSM, autowikibrowserUM)
ccnetdf <- buildDataFrame("CruiseControl.NET", ccnetSM, ccnetUM)
eraserdf <- buildDataFrame("Eraser", eraserSM, eraserUM)
fireircclientdf <- buildDataFrame("FireIRC IRC Client", fireircclientSM, fireircclientUM)
ifolderdf <- buildDataFrame("iFolder", ifolderSM, ifolderUM)
processhackerdf <- buildDataFrame("Process Hacker", processhackerSM, processhackerUM)
rssbanditdf <- buildDataFrame("RSS Bandit", rssbanditSM, rssbanditUM)
worldwinddf <- buildDataFrame("NASA WorldWind", worldwindSM, worldwindUM)

#python
bitpimdf <- buildDataFrame("BitPim", bitpimSM, bitpimUM)
emesenedf <- buildDataFrame("emesene", emeseneSM, emeseneUM)
exedf <- buildDataFrame("eXe", exeSM, exeUM)
matplotlibdf <- buildDataFrame("matplotlib", matplotlibSM, matplotlibUM)
spambayesdf <- buildDataFrame("SpamBayes", spamBayesSM, spamBayesUM)
wicddf <- buildDataFrame("Wicd", wicdSM, wicdUM)

### Build DataFrame End
drawBarplotPNGs()

