library (readxl)
library (dplyr)
library (tidyr)

filename <- c('Inspecties2017Jeanette (11-01-2018).xlsx')
rawData <- read_excel(filename, sheet = ' Annex VI PSC en FSC', skip = 5)

InspectionData <- data.frame(matrix(NA, ncol = 9, nrow = length(rawData$IMO.nr.)))  
colnames(InspectionData) <- c("Name", "IMO", "Date", "ILTSample" , "SGSSample", "Sample", "SampleCompliant", "SampleTaken", "FCUsed")

InspectionData$Name <- rawData$Naam
InspectionData$IMO <- rawData$IMO.nr.
InspectionData$Date <- rawData$`datum monstername`
InspectionData$ILTSample <- as.numeric(rawData$ILT)
InspectionData$SGSSample <- as.numeric(rawData$SGS)


for (i in 1:length (InspectionData$IMO)) {
  InspectionData$Sample[i] <- ifelse (is.na(InspectionData$SGS[i]), InspectionData$ILTSample[i], InspectionData$SGSSample[i])
  InspectionData$SampleCompliant[i] <- InspectionData$Sample[i] < 0.12
  InspectionData$SampleTaken[i] <- !is.na(InspectionData$Sample[i])
  InspectionData$FCUsed[i] <- ifelse (rawData$`Fuel Calculator`[i] == 1, TRUE, FALSE)
}

table(InspectionData$SampleTaken, InspectionData$FCUsed)
table(InspectionData$SampleCompliant, InspectionData$FCUsed)

length(which(InspectionData$SampleCompliant == FALSE))

write.csv2(InspectionData,file = "InspectionData.csv")

InspectionData %>%
  group_by (FCUsed, SampleTaken, SampleCompliant) %>%
  summarise (n=n()) %>%
  spread (SampleCompliant,n) %>%
  kable() %>%
  kable_styling() %>%
  add_header_above(c(" " =1, " " =1, "Sample Compliant" =3))

  