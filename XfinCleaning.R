library(tidyverse)
library(purrr)
library(curl)
library(httr)
library(rvest)


#Load the Three Xfinity Files
Xfinity0710 = read.csv("Xfinity0710.csv",sep = ";")
Xfinity11 = read.csv("Xfinity11.csv",sep = ";")
Xfinity1220 = read.csv("Xfinity1220.csv",sep = ";")

#Bind Three Dataframes Together
Xfinity1 = rbind.data.frame(Xfinity0710, Xfinity11)
Xfinity2 = rbind.data.frame(Xfinity1, Xfinity1220)

#Exclude blank row, Header row, and Loop data row)
Xfinity <- Xfinity2[!(Xfinity2$Finish == "Finish" |Xfinity2$Finish == "" |Xfinity2$Finish == "Loop data for this race:"), ]

#Save CSV
write.csv2(Xfinity,"Xfinity2007_2020.csv")