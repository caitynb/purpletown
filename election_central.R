setwd("C:/Users/jlsumner/Downloads")

caity <- read.csv("caitydata.csv")

# sum for each Town/City for votes for Biden/Harris and votes for Trump/Pence

library(dplyr)
summarize(.data=caity,group="Municipality.Name")

caity2 <- caity %>%
  group_by(Municipality.Name) %>%
  summarise(BidenHarris = sum(Joseph.R..Biden...Kamala.D..Harris,na.rm=T), 
            TrumpPence = sum(Donald.J..Trump...Michael.R..Pence,na.rm=T),
            nwards = n())
