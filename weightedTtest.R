library(weights)
newData <- read.csv("textTable.csv", row.names = NULL, stringsAsFactors = FALSE)
newData <- newData[3:length(newData$N)-1,]
newData$N <- as.numeric(newData$N)
newData$Mean <- as.numeric(newData$Mean)

myWeight <- wtd.t.test(newData$Mean, 2.6, newData$N, alternative = "greater")
paste("Comparison to 2.6 cutoff made using a",myWeight$test)
paste("t value =",myWeight$coefficients[1])
paste("df =", myWeight$coefficients[2])
paste("p value =", myWeight$coefficients[3])
dVal <- myWeight$coefficients[1]*2/sqrt(myWeight$coefficients[2])
paste("Cohen's D =",dVal)
