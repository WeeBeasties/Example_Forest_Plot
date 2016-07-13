######################################################################################################################
#  summarize.R                                                                                                       #
#  A script to summarize the assessment data based upon various meta-data fields.                                    #
#  Created: April 2015                                                                                               #
#  Last modified: June 2016                                                                                          #
#                                                                                                                    #
#  MIT License (MIT)                                                                                                 #
#  Copyright (c) 2015 Dr. Clifton Franklund                                                                          #
######################################################################################################################

library(dplyr)
library(ggplot2)
library(forestplot)

weighted.var.se <- function(x, w, na.rm=FALSE)
	#  Computes the variance of a weighted mean following Cochran 1977 definition
{
	if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
	n = length(w)
	xWbar = weighted.mean(x,w,na.rm=na.rm)
	wbar = mean(w)
	out = n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar*sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2))
	low = xWbar-(out*1.96)
	high = xWbar+(out*1.96)
	myOutput <- c(mean=round(xWbar,2),low=round(low,2),high=round(high,2))
	return(myOutput)
}

#myData <- readRDS("./processed/cleanData.rds")
myData <- read.csv("./processed/cleanData.csv")
myData$SCI1 <- as.numeric(levels(myData$SCI1))[myData$SCI1]

dataTable <- myData %>%
	group_by(file) %>%
	summarise(n = length(SCI1), mean = mean(SCI1), lower = mean(SCI1)-(1.96*sd(SCI1)/sqrt(length(SCI1))), upper = mean(SCI1)+(1.96*sd(SCI1)/sqrt(length(SCI1))))
dataTable$file <- NULL
myWeighted <- weighted.var.se(dataTable$mean,dataTable$n)
dataTable$n <- NULL
dataTable <- rbind(dataTable,myWeighted)
nullHeadings <- c(NA,NA,NA)
dataTable <- rbind(nullHeadings,dataTable)

textTable <- myData %>%
	group_by(file) %>%
	summarize(Semester = Semester[1], Prefix = Prefix[1], Level = Level[1], Outcome = "SCI1", N = length(SCI1), Mean = round(mean(SCI1),2))
textTable$file <- NULL
textTable$Semester <- as.character(textTable$Semester)
textTable$Prefix <- as.character(textTable$Prefix)
textTable$Level <- as.character(textTable$Level)
headings <- c("Semester","Prefix","Level","Outcome","N","Mean")
textTable <- rbind(headings,textTable)
theSummary <- c("Weighted average",NA,NA,NA,NA,myWeighted)
textTable <- rbind(textTable,theSummary)

align <- c("c","c","c","c","c","c")

pdf("figs/forest.pdf", width=8.0, height=3.5)
forestplot(textTable, dataTable,
	   new_page = FALSE,                             # Image on one page
	   is.summary=c(TRUE,rep(FALSE,12),TRUE),         # Bold for heading and summary lines
	   boxsize = .2,                                # Set symbol size
	   xlog=FALSE,                                   # Linear scale
	   xticks = c(0,1,2,3,4),                        # Ticks at the rubric values
	   zero = 2.6,                                   # Set threshold value
	   grid = gpar(lty=3, col="#333333", lwd=1.25),  # Make vertical lines gray dots
	   xlab = "\nMean rubric score ± 95% CI",        # Label x-axis
	   title = "Performance on Scientific Understanding Outcome #1 Based Upon Lecture Exam 1",
	   align = align,                                # Center all text columns in table
	   colgap = unit(4, 'mm'),                       # Tighten up the columns
	   graphwidth = unit(80, 'mm'),                  # Make the plot 80mm wide
	   graph.pos=ncol(textTable),                    # Move average values after the plot
	   hrzl_lines = TRUE,                            # Add horizontal lines
	   txt_gp = fpTxtGp(label=gpar(cex=.5), xlab = gpar(cex=0.5), ticks = gpar(cex=0.5)),
	   col=fpColors(box="maroon",line="black", summary="maroon", zero="gray50"))
dev.off()
