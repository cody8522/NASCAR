library(tidyverse)
library(purrr)
library(curl)
library(httr)
library(rvest)


#Load the Three Trucks Files
trucks1214 = read.csv("Trucks1214.csv",sep = ";")
trucks0711 = read.csv("Trucks0711.csv",sep = ";")
trucks1520 = read.csv("Trucks1520.csv",sep = ";")

#Bind Three Dataframes Together
trucks1 = rbind.data.frame(trucks0711, trucks1214)
trucks2 = rbind.data.frame(trucks1, trucks1520)

#Exclude blank row, Header row, and Loop data row)
trucks <- trucks2[!(trucks2$Finish == "Finish" |trucks2$Finish == "" |trucks2$Finish == "Loop data for this race:"), ]

#Save CSV
write.csv2(trucks,"Trucks2007_2020.csv")