library (readxl)
library(ggplot2)

#Function to build a column setting compliance level
Compfun <- function (relFU) {
  if (!is.na(relFU)) {
    if (relFU >= 1.1) {"noncompliant"}
    else if (relFU <= 1) {"compliant"}
    else {"suspicious"}
  } else {"unknnown"}
}

filedir <- c('/Users/jasper/Documents/Rfiles/FCSulphur/FilesFCSulphur')
Filenames <- list.files(path = filedir)
##Result: 148 files with FC

FCData <- data.frame(matrix(NA, ncol = 10, nrow = length(Filenames)))  
colnames(FCData) <- c("IMO", "SoundingEntrySECA", "SoundingInsp", "LSFOBunkered", "AdminFuelUse" , "CalcFuelUse", "relFuelUse", "NextportFuelUse", "FileName", "FuelSample")

for (i in 1: length(Filenames)) {
  filename <- paste0(filedir,'/',Filenames[i])
  rawData <- read_excel(filename, sheet = 'Calculations')
  #Only extract data if the data column exist (something with different layouts of FC)
  if ("X__3" %in% colnames(rawData)) {
    FCData$SoundingEntrySECA[i] <- rawData$X__2[1]
    FCData$SoundingInsp[i] <- rawData$X__2[4]
    FCData$LSFOBunkered[i] <- rawData$X__2[2]
    FCData$CalcFuelUse[i] <- rawData$X__3[17]
    FCData$NextportFuelUse[i] <- rawData$X__3[27]
  }
  rawData <- read_excel(filename, sheet = 'Input')
  FCData$IMO[i] <- if (!is.na(rawData[9,1]) & as.character(rawData[9,1]) == "IMO number") {as.character(rawData[9,2])
                   } else if (!is.na(rawData[7,4]) & as.character(rawData[7,4]) == "IMO number") {as.character(rawData[7,6]) 
                   } else if (!is.na(rawData[13,1]) & as.character(rawData[13,1]) == "IMO number") {as.character(rawData[13,6]) 
                   } else {NA}
  completeInspectionData <-InspectionData[!is.na(InspectionData$IMO) & !is.na(InspectionData$Sample),]
  FCData$FuelSample[i] <-  if (length(which(completeInspectionData$IMO == FCData$IMO[i])) == 1) {completeInspectionData$Sample[completeInspectionData$IMO == FCData$IMO[i]]} else {NA}
  FCData$FileName[i] <- Filenames[i]
}

FCData$SoundingEntrySECA <- as.numeric(FCData$SoundingEntrySEC)
FCData$SoundingInsp <- as.numeric(FCData$SoundingInsp)
FCData$LSFOBunkered<- as.numeric(FCData$LSFOBunkered)
FCData$CalcFuelUse<- as.numeric(FCData$CalcFuelUse)
FCData$NextportFuelUse<- as.numeric(FCData$NextportFuelUse)
FCData$AdminFuelUse <- as.numeric(FCData$SoundingEntrySECA)-(as.numeric(FCData$SoundingInsp)-as.numeric(FCData$LSFOBunkered))
FCData$relFuelUse <- as.numeric(FCData$CalcFuelUse/FCData$AdminFuelUse)
FCData$ComplianceLevel <-as.factor(as.character((lapply (FCData$relFuelUse,Compfun))))

##Result: 148 files read succesfully

#quick plot of raw results
plot(FCData$AdminFuelUse,FCData$CalcFuelUse)
x <- seq(from = 0, to = 200, by = 50)
y<- x
lines(x,y)

#clean data for missing values, negatives and zeros
FCData <- FCData[FCData$SoundingEntrySECA != 0,]
FCData <- FCData[!is.na(FCData$SoundingEntrySECA),]
##Result: 5 lines removed with very low values for AdminFuelUse or 'NA' Sounding data at Entry SECA, 143 lines remaining
FCData <- FCData[FCData$CalcFuelUse > 5,]
FCData <- FCData[!is.na(FCData$CalcFuelUse),]
##Result: 12 lines removed with '0' or 'NA' calculated fuel use, 127 lines remaining
FCData <- FCData[FCData$AdminFuelUse > 0,]
##Result: 2 lines removed with negatice fuel use from bunkerlogs, 125 lines remaing

#Some bunker data seems incorrectly processed. SoundingEntrySECA is larder then SoundingInsp even though bunkering has taken place
  #this could be date issues with BDNs, correcting only the extreme cases where fraction is > 2.5, at lower values ambigious results
ind <- which(FCData$SoundingEntrySECA>FCData$SoundingInsp & FCData$LSFOBunkered>0 & FCData$relFuelUse > 2.5)
FCData$AdminFuelUse[ind] <- FCData$SoundingEntrySECA[ind]-FCData$SoundingInsp[ind]
FCData$relFuelUse[ind] <- FCData$AdminFuelUse[ind]/FCData$CalcFuelUse[ind]
##Result 6 lines corrected AdminFuelUse

#plot of cleaned results
plot(FCData$AdminFuelUse,FCData$CalcFuelUse)
lines(x,y)
hist (FCData$relFuelUse, breaks = 20)

#Plot distribution of relFuelUse
ggplot(data = FCData, aes(x=relFuelUse)) +
  geom_histogram(binwidth = 0.05, colour = "#FF6666", fill = "#FF6666", alpha = "0.5") +
  geom_vline(aes(xintercept = mean(FCData$relFuelUse)), color = "#FF6666", linetype = "dashed", size = 1)

write.csv2(FCData,file = "FCData.csv")
