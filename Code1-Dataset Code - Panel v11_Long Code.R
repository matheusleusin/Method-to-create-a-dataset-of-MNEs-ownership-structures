######################################### Establishing a global dataset of AI patents by MNEs #########################################

### Prepare R ####
#clear your global environment
rm(list=ls())

#Increase Memory
options(java.parameters = "- Xmx1024m")
memory.limit(size=70000)

#set working directory:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#let's  create the invisible folder, where all files created using this code will be saved:
dir.create("files_created_code1")

#load libraries we need
library(writexl)
library(readxl) #for reading the xlsx files
library(magrittr) # For extra-piping operators (eg. %<>%)
library(tidyverse) # Collection of all the good stuff like dplyr, ggplot2 ect.
library(tidyr)
library(stringr) #for picking the year from the Priority date information
library(expss) #for Vlookup
library(xlsx) #for Excel files
library(plyr) #For data analysis
library(dplyr) #For data analysis
library(naniar) #For NAs
library(zoo)
library(data.table)
library(lattice)
library(psych)
library(stringr)


### Part I: Creating a dataset of corporate MNEs and their subsidiaries as of 2020 ####

#   1. Creating a dataset of ownership structures of all MNEs from Orbis (bottom-up) ####

##Create dataset of subsidiaries with foreign shareholders

#Read data of foreign subsidiaries (1:Name,2:Company-BvDID,3:GUOBvDID,4:Parent-BvDID):
GUO_files<-list.files(path="Dataset/GUO",pattern ='.xlsx', full.names = T)
GUO_data <- sapply(GUO_files, read_excel, sheet=2,simplify=FALSE) %>% 
   bind_rows(.id = "id")

#Remove Row of Numbers and Company Names to have 3 columns with BvDIDs + Rename/Order Columns
GUO_data[1:3] <- NULL
names(GUO_data) <- c("Company", "GUO","Parent")
col_order <- c("GUO", "Parent", "Company")
mydata1<-GUO_data[, col_order]

#Define all as character
mydata1$GUO<-as.character(mydata1$GUO)
mydata1$Parent<-as.character(mydata1$Parent)
mydata1$Company<-as.character(mydata1$Company)


##Establish ownership connections between companies and their subsidiaries

#Look for subsidiaries in the Parent Column and return subsidiaries in column Sub1-6:
mydata1$Sub1<-expss::vlookup_df(mydata1$Company,mydata1,result_column = 3, lookup_column = 2)
mydata1$Sub1<-unlist(mydata1$Sub1)
mydata1$Sub1<-ifelse(mydata1$Sub1==mydata1$Parent,NA,mydata1$Sub1)
mydata1$Sub1<-ifelse(mydata1$Sub1==mydata1$Company,NA,mydata1$Sub1)
mydata1$Sub1<-ifelse(mydata1$Sub1==mydata1$GUO,NA,mydata1$Sub1)

mydata1$Sub2<-vlookup_df(mydata1$Sub1,mydata1,result_column = 4, lookup_column = 2)
mydata1$Sub2<-unlist(mydata1$Sub2)
mydata1$Sub2<-ifelse(mydata1$Sub2==mydata1$Parent,NA,mydata1$Sub2)
mydata1$Sub2<-ifelse(mydata1$Sub2==mydata1$Company,NA,mydata1$Sub2)
mydata1$Sub2<-ifelse(mydata1$Sub2==mydata1$Sub1,NA,mydata1$Sub2)
mydata1$Sub2<-ifelse(mydata1$Sub2==mydata1$GUO,NA,mydata1$Sub2)

mydata1$Sub3<-vlookup_df(mydata1$Sub2,mydata1,result_column = 5, lookup_column = 2)
mydata1$Sub3<-unlist(mydata1$Sub3)
mydata1$Sub3<-ifelse(mydata1$Sub3==mydata1$Parent,NA,mydata1$Sub3)
mydata1$Sub3<-ifelse(mydata1$Sub3==mydata1$Company,NA,mydata1$Sub3)
mydata1$Sub3<-ifelse(mydata1$Sub3==mydata1$Sub2,NA,mydata1$Sub3)
mydata1$Sub3<-ifelse(mydata1$Sub3==mydata1$Sub1,NA,mydata1$Sub3)
mydata1$Sub3<-ifelse(mydata1$Sub3==mydata1$GUO,NA,mydata1$Sub3)

mydata1$Sub4<-vlookup_df(mydata1$Sub3,mydata1,result_column = 6, lookup_column = 2)
mydata1$Sub4<-unlist(mydata1$Sub4)
mydata1$Sub4<-ifelse(mydata1$Sub4==mydata1$Parent,NA,mydata1$Sub4)
mydata1$Sub4<-ifelse(mydata1$Sub4==mydata1$Company,NA,mydata1$Sub4)
mydata1$Sub4<-ifelse(mydata1$Sub4==mydata1$Sub3,NA,mydata1$Sub4)
mydata1$Sub4<-ifelse(mydata1$Sub4==mydata1$Sub2,NA,mydata1$Sub4)
mydata1$Sub4<-ifelse(mydata1$Sub4==mydata1$Sub1,NA,mydata1$Sub4)
mydata1$Sub4<-ifelse(mydata1$Sub4==mydata1$GUO,NA,mydata1$Sub4)

mydata1$Sub5<-vlookup_df(mydata1$Sub4,mydata1,result_column = 7, lookup_column = 2)
mydata1$Sub5<-unlist(mydata1$Sub5)
mydata1$Sub5<-ifelse(mydata1$Sub5==mydata1$Parent,NA,mydata1$Sub5)
mydata1$Sub5<-ifelse(mydata1$Sub5==mydata1$Company,NA,mydata1$Sub5)
mydata1$Sub5<-ifelse(mydata1$Sub5==mydata1$Sub4,NA,mydata1$Sub5)
mydata1$Sub5<-ifelse(mydata1$Sub5==mydata1$Sub3,NA,mydata1$Sub5)
mydata1$Sub5<-ifelse(mydata1$Sub5==mydata1$Sub2,NA,mydata1$Sub5)
mydata1$Sub5<-ifelse(mydata1$Sub5==mydata1$Sub1,NA,mydata1$Sub5)
mydata1$Sub5<-ifelse(mydata1$Sub5==mydata1$GUO,NA,mydata1$Sub5)

mydata1$Sub6<-vlookup_df(mydata1$Sub5,mydata1,result_column = 8, lookup_column = 2)
mydata1$Sub6<-unlist(mydata1$Sub6)
mydata1$Sub6<-ifelse(mydata1$Sub6==mydata1$Parent,NA,mydata1$Sub6)
mydata1$Sub6<-ifelse(mydata1$Sub6==mydata1$Company,NA,mydata1$Sub6)
mydata1$Sub6<-ifelse(mydata1$Sub6==mydata1$Sub5,NA,mydata1$Sub6)
mydata1$Sub6<-ifelse(mydata1$Sub6==mydata1$Sub4,NA,mydata1$Sub6)
mydata1$Sub6<-ifelse(mydata1$Sub6==mydata1$Sub3,NA,mydata1$Sub6)
mydata1$Sub6<-ifelse(mydata1$Sub6==mydata1$Sub2,NA,mydata1$Sub6)
mydata1$Sub6<-ifelse(mydata1$Sub6==mydata1$Sub1,NA,mydata1$Sub6)
mydata1$Sub6<-ifelse(mydata1$Sub6==mydata1$GUO,NA,mydata1$Sub6)

#Check if any parents/subsidiaries are left
length(which(!is.na(mydata1$Sub4))) 


##Establish ownership connections between companies and their parents

#Look for Parents in the Subsidiary  Column and return Parents in column Own1-8:
mydata1$Own1<-vlookup_df(mydata1$Parent,mydata1,result_column = 2, lookup_column = 3)
mydata1$Own1<-unlist(mydata1$Own1)
mydata1$Own1<-ifelse(mydata1$Own1==mydata1$Parent,NA,mydata1$Own1)
mydata1$Own1<-ifelse(mydata1$Own1==mydata1$Company,NA,mydata1$Own1)
mydata1$Own1<-ifelse(mydata1$Own1==mydata1$GUO,NA,mydata1$Own1)

mydata1$Own2<-vlookup_df(mydata1$Own1,mydata1,result_column = 2, lookup_column = 3)
mydata1$Own2<-unlist(mydata1$Own2)
mydata1$Own2<-ifelse(mydata1$Own2==mydata1$Parent,NA,mydata1$Own2)
mydata1$Own2<-ifelse(mydata1$Own2==mydata1$Company,NA,mydata1$Own2)
mydata1$Own2<-ifelse(mydata1$Own2==mydata1$Own1,NA,mydata1$Own2)
mydata1$Own2<-ifelse(mydata1$Own2==mydata1$GUO,NA,mydata1$Own2)

mydata1$Own3<-vlookup_df(mydata1$Own2,mydata1,result_column = 2, lookup_column = 3)
mydata1$Own3<-unlist(mydata1$Own3)
mydata1$Own3<-ifelse(mydata1$Own3==mydata1$Parent,NA,mydata1$Own3)
mydata1$Own3<-ifelse(mydata1$Own3==mydata1$Company,NA,mydata1$Own3)
mydata1$Own3<-ifelse(mydata1$Own3==mydata1$Own2,NA,mydata1$Own3)
mydata1$Own3<-ifelse(mydata1$Own3==mydata1$Own1,NA,mydata1$Own3)
mydata1$Own3<-ifelse(mydata1$Own3==mydata1$GUO,NA,mydata1$Own3)

mydata1$Own4<-vlookup_df(mydata1$Own3,mydata1,result_column = 2, lookup_column = 3)
mydata1$Own4<-unlist(mydata1$Own4)
mydata1$Own4<-ifelse(mydata1$Own4==mydata1$Parent,NA,mydata1$Own4)
mydata1$Own4<-ifelse(mydata1$Own4==mydata1$Company,NA,mydata1$Own4)
mydata1$Own4<-ifelse(mydata1$Own4==mydata1$Own3,NA,mydata1$Own4)
mydata1$Own4<-ifelse(mydata1$Own4==mydata1$Own2,NA,mydata1$Own4)
mydata1$Own4<-ifelse(mydata1$Own4==mydata1$Own1,NA,mydata1$Own4)
mydata1$Own4<-ifelse(mydata1$Own4==mydata1$GUO,NA,mydata1$Own4)

mydata1$Own5<-vlookup_df(mydata1$Own4,mydata1,result_column = 2, lookup_column = 3)
mydata1$Own5<-unlist(mydata1$Own5)
mydata1$Own5<-ifelse(mydata1$Own5==mydata1$Parent,NA,mydata1$Own5)
mydata1$Own5<-ifelse(mydata1$Own5==mydata1$Company,NA,mydata1$Own5)
mydata1$Own5<-ifelse(mydata1$Own5==mydata1$Own4,NA,mydata1$Own5)
mydata1$Own5<-ifelse(mydata1$Own5==mydata1$Own3,NA,mydata1$Own5)
mydata1$Own5<-ifelse(mydata1$Own5==mydata1$Own2,NA,mydata1$Own5)
mydata1$Own5<-ifelse(mydata1$Own5==mydata1$Own1,NA,mydata1$Own5)
mydata1$Own5<-ifelse(mydata1$Own5==mydata1$GUO,NA,mydata1$Own5)

mydata1$Own6<-vlookup_df(mydata1$Own5,mydata1,result_column = 2, lookup_column = 3)
mydata1$Own6<-unlist(mydata1$Own6)
mydata1$Own6<-ifelse(mydata1$Own6==mydata1$Parent,NA,mydata1$Own6)
mydata1$Own6<-ifelse(mydata1$Own6==mydata1$Company,NA,mydata1$Own6)
mydata1$Own6<-ifelse(mydata1$Own6==mydata1$Own5,NA,mydata1$Own6)
mydata1$Own6<-ifelse(mydata1$Own6==mydata1$Own4,NA,mydata1$Own6)
mydata1$Own6<-ifelse(mydata1$Own6==mydata1$Own3,NA,mydata1$Own6)
mydata1$Own6<-ifelse(mydata1$Own6==mydata1$Own2,NA,mydata1$Own6)
mydata1$Own6<-ifelse(mydata1$Own6==mydata1$Own1,NA,mydata1$Own6)
mydata1$Own6<-ifelse(mydata1$Own6==mydata1$GUO,NA,mydata1$Own6)

mydata1$Own7<-vlookup_df(mydata1$Own6,mydata1,result_column = 2, lookup_column = 3)
mydata1$Own7<-unlist(mydata1$Own7)
mydata1$Own7<-ifelse(mydata1$Own7==mydata1$Parent,NA,mydata1$Own7)
mydata1$Own7<-ifelse(mydata1$Own7==mydata1$Company,NA,mydata1$Own7)
mydata1$Own7<-ifelse(mydata1$Own7==mydata1$Own6,NA,mydata1$Own7)
mydata1$Own7<-ifelse(mydata1$Own7==mydata1$Own5,NA,mydata1$Own7)
mydata1$Own7<-ifelse(mydata1$Own7==mydata1$Own4,NA,mydata1$Own7)
mydata1$Own7<-ifelse(mydata1$Own7==mydata1$Own3,NA,mydata1$Own7)
mydata1$Own7<-ifelse(mydata1$Own7==mydata1$Own2,NA,mydata1$Own7)
mydata1$Own7<-ifelse(mydata1$Own7==mydata1$Own1,NA,mydata1$Own7)
mydata1$Own7<-ifelse(mydata1$Own7==mydata1$GUO,NA,mydata1$Own7)

mydata1$Own8<-vlookup_df(mydata1$Own7,mydata1,result_column = 2, lookup_column = 3)
mydata1$Own8<-unlist(mydata1$Own8)
mydata1$Own8<-ifelse(mydata1$Own8==mydata1$Parent,NA,mydata1$Own8)
mydata1$Own8<-ifelse(mydata1$Own8==mydata1$Company,NA,mydata1$Own8)
mydata1$Own8<-ifelse(mydata1$Own8==mydata1$Own7,NA,mydata1$Own8)
mydata1$Own8<-ifelse(mydata1$Own8==mydata1$Own6,NA,mydata1$Own8)
mydata1$Own8<-ifelse(mydata1$Own8==mydata1$Own5,NA,mydata1$Own8)
mydata1$Own8<-ifelse(mydata1$Own8==mydata1$Own4,NA,mydata1$Own8)
mydata1$Own8<-ifelse(mydata1$Own8==mydata1$Own3,NA,mydata1$Own8)
mydata1$Own8<-ifelse(mydata1$Own8==mydata1$Own2,NA,mydata1$Own8)
mydata1$Own8<-ifelse(mydata1$Own8==mydata1$Own1,NA,mydata1$Own8)
mydata1$Own8<-ifelse(mydata1$Own8==mydata1$GUO,NA,mydata1$Own8)

mydata1$Own9<-vlookup_df(mydata1$Own8,mydata1,result_column = 2, lookup_column = 3)
mydata1$Own9<-unlist(mydata1$Own9)
mydata1$Own9<-ifelse(mydata1$Own9==mydata1$Parent,NA,mydata1$Own9)
mydata1$Own9<-ifelse(mydata1$Own9==mydata1$Company,NA,mydata1$Own9)
mydata1$Own9<-ifelse(mydata1$Own9==mydata1$Own8,NA,mydata1$Own9)
mydata1$Own9<-ifelse(mydata1$Own9==mydata1$Own7,NA,mydata1$Own9)
mydata1$Own9<-ifelse(mydata1$Own9==mydata1$Own6,NA,mydata1$Own9)
mydata1$Own9<-ifelse(mydata1$Own9==mydata1$Own5,NA,mydata1$Own9)
mydata1$Own9<-ifelse(mydata1$Own9==mydata1$Own4,NA,mydata1$Own9)
mydata1$Own9<-ifelse(mydata1$Own9==mydata1$Own3,NA,mydata1$Own9)
mydata1$Own9<-ifelse(mydata1$Own9==mydata1$Own2,NA,mydata1$Own9)
mydata1$Own9<-ifelse(mydata1$Own9==mydata1$Own1,NA,mydata1$Own9)
mydata1$Own9<-ifelse(mydata1$Own9==mydata1$GUO,NA,mydata1$Own9)

mydata1$Own10<-vlookup_df(mydata1$Own9,mydata1,result_column = 2, lookup_column = 3)
mydata1$Own10<-unlist(mydata1$Own10)
mydata1$Own10<-ifelse(mydata1$Own10==mydata1$Parent,NA,mydata1$Own10)
mydata1$Own10<-ifelse(mydata1$Own10==mydata1$Company,NA,mydata1$Own10)
mydata1$Own10<-ifelse(mydata1$Own10==mydata1$Own9,NA,mydata1$Own10)
mydata1$Own10<-ifelse(mydata1$Own10==mydata1$Own8,NA,mydata1$Own10)
mydata1$Own10<-ifelse(mydata1$Own10==mydata1$Own7,NA,mydata1$Own10)
mydata1$Own10<-ifelse(mydata1$Own10==mydata1$Own6,NA,mydata1$Own10)
mydata1$Own10<-ifelse(mydata1$Own10==mydata1$Own5,NA,mydata1$Own10)
mydata1$Own10<-ifelse(mydata1$Own10==mydata1$Own4,NA,mydata1$Own10)
mydata1$Own10<-ifelse(mydata1$Own10==mydata1$Own3,NA,mydata1$Own10)
mydata1$Own10<-ifelse(mydata1$Own10==mydata1$Own2,NA,mydata1$Own10)
mydata1$Own10<-ifelse(mydata1$Own10==mydata1$Own1,NA,mydata1$Own10)
mydata1$Own10<-ifelse(mydata1$Own10==mydata1$GUO,NA,mydata1$Own10)

mydata1$Own11<-vlookup_df(mydata1$Own10,mydata1,result_column = 2, lookup_column = 3)
mydata1$Own11<-unlist(mydata1$Own11)
mydata1$Own11<-ifelse(mydata1$Own11==mydata1$Parent,NA,mydata1$Own11)
mydata1$Own11<-ifelse(mydata1$Own11==mydata1$Company,NA,mydata1$Own11)
mydata1$Own11<-ifelse(mydata1$Own11==mydata1$Own10,NA,mydata1$Own11)
mydata1$Own11<-ifelse(mydata1$Own11==mydata1$Own9,NA,mydata1$Own11)
mydata1$Own11<-ifelse(mydata1$Own11==mydata1$Own8,NA,mydata1$Own11)
mydata1$Own11<-ifelse(mydata1$Own11==mydata1$Own7,NA,mydata1$Own11)
mydata1$Own11<-ifelse(mydata1$Own11==mydata1$Own6,NA,mydata1$Own11)
mydata1$Own11<-ifelse(mydata1$Own11==mydata1$Own5,NA,mydata1$Own11)
mydata1$Own11<-ifelse(mydata1$Own11==mydata1$Own4,NA,mydata1$Own11)
mydata1$Own11<-ifelse(mydata1$Own11==mydata1$Own3,NA,mydata1$Own11)
mydata1$Own11<-ifelse(mydata1$Own11==mydata1$Own2,NA,mydata1$Own11)
mydata1$Own11<-ifelse(mydata1$Own11==mydata1$Own1,NA,mydata1$Own11)
mydata1$Own11<-ifelse(mydata1$Own11==mydata1$GUO,NA,mydata1$Own11)

mydata1$Own12<-vlookup_df(mydata1$Own11,mydata1,result_column = 2, lookup_column = 3)
mydata1$Own12<-unlist(mydata1$Own12)
mydata1$Own12<-ifelse(mydata1$Own12==mydata1$Parent,NA,mydata1$Own12)
mydata1$Own12<-ifelse(mydata1$Own12==mydata1$Company,NA,mydata1$Own12)
mydata1$Own12<-ifelse(mydata1$Own12==mydata1$Own11,NA,mydata1$Own12)
mydata1$Own12<-ifelse(mydata1$Own12==mydata1$Own10,NA,mydata1$Own12)
mydata1$Own12<-ifelse(mydata1$Own12==mydata1$Own9,NA,mydata1$Own12)
mydata1$Own12<-ifelse(mydata1$Own12==mydata1$Own8,NA,mydata1$Own12)
mydata1$Own12<-ifelse(mydata1$Own12==mydata1$Own7,NA,mydata1$Own12)
mydata1$Own12<-ifelse(mydata1$Own12==mydata1$Own6,NA,mydata1$Own12)
mydata1$Own12<-ifelse(mydata1$Own12==mydata1$Own5,NA,mydata1$Own12)
mydata1$Own12<-ifelse(mydata1$Own12==mydata1$Own4,NA,mydata1$Own12)
mydata1$Own12<-ifelse(mydata1$Own12==mydata1$Own3,NA,mydata1$Own12)
mydata1$Own12<-ifelse(mydata1$Own12==mydata1$Own2,NA,mydata1$Own12)
mydata1$Own12<-ifelse(mydata1$Own12==mydata1$Own1,NA,mydata1$Own12)
mydata1$Own12<-ifelse(mydata1$Own12==mydata1$GUO,NA,mydata1$Own12)

mydata1$Own13<-vlookup_df(mydata1$Own12,mydata1,result_column = 2, lookup_column = 3)
mydata1$Own13<-unlist(mydata1$Own13)
mydata1$Own13<-ifelse(mydata1$Own13==mydata1$Parent,NA,mydata1$Own13)
mydata1$Own13<-ifelse(mydata1$Own13==mydata1$Company,NA,mydata1$Own13)
mydata1$Own13<-ifelse(mydata1$Own13==mydata1$Own12,NA,mydata1$Own13)
mydata1$Own13<-ifelse(mydata1$Own13==mydata1$Own11,NA,mydata1$Own13)
mydata1$Own13<-ifelse(mydata1$Own13==mydata1$Own10,NA,mydata1$Own13)
mydata1$Own13<-ifelse(mydata1$Own13==mydata1$Own9,NA,mydata1$Own13)
mydata1$Own13<-ifelse(mydata1$Own13==mydata1$Own8,NA,mydata1$Own13)
mydata1$Own13<-ifelse(mydata1$Own13==mydata1$Own7,NA,mydata1$Own13)
mydata1$Own13<-ifelse(mydata1$Own13==mydata1$Own6,NA,mydata1$Own13)
mydata1$Own13<-ifelse(mydata1$Own13==mydata1$Own5,NA,mydata1$Own13)
mydata1$Own13<-ifelse(mydata1$Own13==mydata1$Own4,NA,mydata1$Own13)
mydata1$Own13<-ifelse(mydata1$Own13==mydata1$Own3,NA,mydata1$Own13)
mydata1$Own13<-ifelse(mydata1$Own13==mydata1$Own2,NA,mydata1$Own13)
mydata1$Own13<-ifelse(mydata1$Own13==mydata1$Own1,NA,mydata1$Own13)
mydata1$Own13<-ifelse(mydata1$Own13==mydata1$GUO,NA,mydata1$Own13)

mydata1$Own14<-vlookup_df(mydata1$Own13,mydata1,result_column = 2, lookup_column = 3)
mydata1$Own14<-unlist(mydata1$Own14)
mydata1$Own14<-ifelse(mydata1$Own14==mydata1$Parent,NA,mydata1$Own14)
mydata1$Own14<-ifelse(mydata1$Own14==mydata1$Company,NA,mydata1$Own14)
mydata1$Own14<-ifelse(mydata1$Own14==mydata1$Own13,NA,mydata1$Own14)
mydata1$Own14<-ifelse(mydata1$Own14==mydata1$Own12,NA,mydata1$Own14)
mydata1$Own14<-ifelse(mydata1$Own14==mydata1$Own11,NA,mydata1$Own14)
mydata1$Own14<-ifelse(mydata1$Own14==mydata1$Own10,NA,mydata1$Own14)
mydata1$Own14<-ifelse(mydata1$Own14==mydata1$Own9,NA,mydata1$Own14)
mydata1$Own14<-ifelse(mydata1$Own14==mydata1$Own8,NA,mydata1$Own14)
mydata1$Own14<-ifelse(mydata1$Own14==mydata1$Own7,NA,mydata1$Own14)
mydata1$Own14<-ifelse(mydata1$Own14==mydata1$Own6,NA,mydata1$Own14)
mydata1$Own14<-ifelse(mydata1$Own14==mydata1$Own5,NA,mydata1$Own14)
mydata1$Own14<-ifelse(mydata1$Own14==mydata1$Own4,NA,mydata1$Own14)
mydata1$Own14<-ifelse(mydata1$Own14==mydata1$Own3,NA,mydata1$Own14)
mydata1$Own14<-ifelse(mydata1$Own14==mydata1$Own2,NA,mydata1$Own14)
mydata1$Own14<-ifelse(mydata1$Own14==mydata1$Own1,NA,mydata1$Own14)
mydata1$Own14<-ifelse(mydata1$Own14==mydata1$GUO,NA,mydata1$Own14)

mydata1$Own15<-vlookup_df(mydata1$Own14,mydata1,result_column = 2, lookup_column = 3)
mydata1$Own15<-unlist(mydata1$Own15)
mydata1$Own15<-ifelse(mydata1$Own15==mydata1$Parent,NA,mydata1$Own15)
mydata1$Own15<-ifelse(mydata1$Own15==mydata1$Company,NA,mydata1$Own15)
mydata1$Own15<-ifelse(mydata1$Own15==mydata1$Own14,NA,mydata1$Own15)
mydata1$Own15<-ifelse(mydata1$Own15==mydata1$Own13,NA,mydata1$Own15)
mydata1$Own15<-ifelse(mydata1$Own15==mydata1$Own12,NA,mydata1$Own15)
mydata1$Own15<-ifelse(mydata1$Own15==mydata1$Own11,NA,mydata1$Own15)
mydata1$Own15<-ifelse(mydata1$Own15==mydata1$Own10,NA,mydata1$Own15)
mydata1$Own15<-ifelse(mydata1$Own15==mydata1$Own9,NA,mydata1$Own15)
mydata1$Own15<-ifelse(mydata1$Own15==mydata1$Own8,NA,mydata1$Own15)
mydata1$Own15<-ifelse(mydata1$Own15==mydata1$Own7,NA,mydata1$Own15)
mydata1$Own15<-ifelse(mydata1$Own15==mydata1$Own6,NA,mydata1$Own15)
mydata1$Own15<-ifelse(mydata1$Own15==mydata1$Own5,NA,mydata1$Own15)
mydata1$Own15<-ifelse(mydata1$Own15==mydata1$Own4,NA,mydata1$Own15)
mydata1$Own15<-ifelse(mydata1$Own15==mydata1$Own3,NA,mydata1$Own15)
mydata1$Own15<-ifelse(mydata1$Own15==mydata1$Own2,NA,mydata1$Own15)
mydata1$Own15<-ifelse(mydata1$Own15==mydata1$Own1,NA,mydata1$Own15)
mydata1$Own15<-ifelse(mydata1$Own15==mydata1$GUO,NA,mydata1$Own15)

mydata1$Own16<-vlookup_df(mydata1$Own15,mydata1,result_column = 2, lookup_column = 3)
mydata1$Own16<-unlist(mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$Parent,NA,mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$Company,NA,mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$Own15,NA,mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$Own14,NA,mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$Own13,NA,mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$Own12,NA,mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$Own11,NA,mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$Own10,NA,mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$Own9,NA,mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$Own8,NA,mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$Own7,NA,mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$Own6,NA,mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$Own5,NA,mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$Own4,NA,mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$Own3,NA,mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$Own2,NA,mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$Own1,NA,mydata1$Own16)
mydata1$Own16<-ifelse(mydata1$Own16==mydata1$GUO,NA,mydata1$Own16)

#Check if any subsidiaries/parents are left
length(which(!is.na(mydata1$Own16)))


#Reorder the data to receive a row of Ownership relations
col_order<-c("GUO","Own16","Own15","Own14","Own13","Own12","Own11","Own10","Own9",
             "Own8","Own7","Own6","Own5","Own4","Own3","Own2","Own1",
             "Parent","Company","Sub1","Sub2","Sub3","Sub4","Sub5","Sub6")
Network1<-mydata1[, col_order]

#Remove duplicate values in each column
Duplicate1<-t(apply(Network1, 1, function(x) { x[duplicated(x)] <- NA; x }))
Network1<-as.data.frame(Duplicate1)

#Make a dataset with column values moved to the left to right - creates ownership levels
Sort1<- as.data.frame(t(apply(Network1, 1, function(x) x[order(is.na(x))])))     #Takes some time

#Remove empty columns
emptycols <- sapply(Sort1, function (k) all(is.na(k)))
Sorted1 <- Sort1[!emptycols]

names(Sorted1) <- c("Own1","Own2","Own3","Own4","Own5","Own6","Own7",
                    "Own8","Own9","Own10","Own11","Own12","Own13","Own14",
                    "Own15","Own16","Own17","Own18","Own19","Own20","Own21")

#Remove Duplicates and Sort Alphabetically
Duplicates1<-unique(Sorted1,)
Interim1<-Duplicates1[order(Duplicates1[,1],Duplicates1[,2],Duplicates1[,3],Duplicates1[,4],
                            Duplicates1[,5],Duplicates1[,6],Duplicates1[,7],Duplicates1[,8],
                            Duplicates1[,9],Duplicates1[,10],Duplicates1[,11],Duplicates1[,12],
                            Duplicates1[,13],Duplicates1[,14],Duplicates1[,15],Duplicates1[,16],
                            Duplicates1[,17],Duplicates1[,18],Duplicates1[,19],Duplicates1[,20],
                            Duplicates1[,21]),]





#   2. Attach additional subsidiaries based on shareholder search with foreign subsidiaries top-down) ####

## Create dataset of shareholders with foreign subsidiaries

#Read shareholder data (1:Name,2:Shareholder-BvDID,3:GUOBvDID,4:Subsidiaries-BvDID):
Share_files<-list.files(path="Dataset/Share",pattern ='.xlsx', full.names = T)
Share_data <- sapply(Share_files, read_excel, sheet=2,simplify=FALSE) %>% 
   bind_rows(.id = "id")

##Restructure dataset
#Remove Row of Numbers and Company Names + Rename/Order Columns
Share_data[1:3] <- NULL
names(Share_data) <- c("SubBvDID","GUOBvDID","BvDID","ParentBvDID","Share")
sapply(Share_data, class)

#Indicate missing values
Share<-na_if(Share_data,"n.a.")

##Create a country Owner+subsidiaries dataframe for binding
Share$GUOBvDID<-na.locf(Share$GUOBvDID)
Share$BvDID<-na.locf(Share$BvDID)
Share$ParentBvDID<-na.locf(Share$ParentBvDID)

#Only retain direct ownership share with 25.01 percent (not unknown)
Share$Share<-lapply(Share$Share,gsub, pattern='WO', replacement='50')
Share$Share<-lapply(Share$Share,gsub, pattern='MO', replacement='50') # not clear if majority or minority owned
Share$Share<-lapply(Share$Share,gsub, pattern='>', replacement='')
Share$Share<-as.numeric(Share$Share)
Share$Delete<-ifelse(Share$Share>25,1,0)
Share<-subset(Share,Delete==1)

#Remove unnecessary variables
Share[4:6] <- NULL

#Rename data
names(Share) <- c("Company", "GUO","Parent")
col_order <- c("GUO", "Parent", "Company")
mydata2<-Share[, col_order]

#Define all as character
mydata2$GUO<-as.character(mydata2$GUO)
mydata2$Parent<-as.character(mydata2$Parent)
mydata2$Company<-as.character(mydata2$Company)

#Extract Company and GUO Column for later
OriginalCompany<-mydata2[,3]
OriginalGUO<-mydata2[,1]

##Establish ownership connections between companies and their subsidiaries
#Look for subsidiaries in the Parent Column and return subsidiaries in column Sub1-6:
mydata2$Sub1<-vlookup_df(mydata2$Company,mydata2,result_column = 3, lookup_column = 2)
mydata2$Sub1<-unlist(mydata2$Sub1)
mydata2$Sub1<-ifelse(mydata2$Sub1==mydata2$Parent,NA,mydata2$Sub1)
mydata2$Sub1<-ifelse(mydata2$Sub1==mydata2$Company,NA,mydata2$Sub1)
mydata2$Sub1<-ifelse(mydata2$Sub1==mydata2$GUO,NA,mydata2$Sub1)

mydata2$Sub2<-vlookup_df(mydata2$Sub1,mydata2,result_column = 4, lookup_column = 2)
mydata2$Sub2<-unlist(mydata2$Sub2)
mydata2$Sub2<-ifelse(mydata2$Sub2==mydata2$Parent,NA,mydata2$Sub2)
mydata2$Sub2<-ifelse(mydata2$Sub2==mydata2$Company,NA,mydata2$Sub2)
mydata2$Sub2<-ifelse(mydata2$Sub2==mydata2$Sub1,NA,mydata2$Sub2)
mydata2$Sub2<-ifelse(mydata2$Sub2==mydata2$GUO,NA,mydata2$Sub2)

mydata2$Sub3<-vlookup_df(mydata2$Sub2,mydata2,result_column = 5, lookup_column = 2)
mydata2$Sub3<-unlist(mydata2$Sub3)
mydata2$Sub3<-ifelse(mydata2$Sub3==mydata2$Parent,NA,mydata2$Sub3)
mydata2$Sub3<-ifelse(mydata2$Sub3==mydata2$Company,NA,mydata2$Sub3)
mydata2$Sub3<-ifelse(mydata2$Sub3==mydata2$Sub2,NA,mydata2$Sub3)
mydata2$Sub3<-ifelse(mydata2$Sub3==mydata2$Sub1,NA,mydata2$Sub3)
mydata2$Sub3<-ifelse(mydata2$Sub3==mydata2$GUO,NA,mydata2$Sub3)

mydata2$Sub4<-vlookup_df(mydata2$Sub3,mydata2,result_column = 6, lookup_column = 2)
mydata2$Sub4<-unlist(mydata2$Sub4)
mydata2$Sub4<-ifelse(mydata2$Sub4==mydata2$Parent,NA,mydata2$Sub4)
mydata2$Sub4<-ifelse(mydata2$Sub4==mydata2$Company,NA,mydata2$Sub4)
mydata2$Sub4<-ifelse(mydata2$Sub4==mydata2$Sub3,NA,mydata2$Sub4)
mydata2$Sub4<-ifelse(mydata2$Sub4==mydata2$Sub2,NA,mydata2$Sub4)
mydata2$Sub4<-ifelse(mydata2$Sub4==mydata2$Sub1,NA,mydata2$Sub4)
mydata2$Sub4<-ifelse(mydata2$Sub4==mydata2$GUO,NA,mydata2$Sub4)

mydata2$Sub5<-vlookup_df(mydata2$Sub4,mydata2,result_column = 7, lookup_column = 2)
mydata2$Sub5<-unlist(mydata2$Sub5)
mydata2$Sub5<-ifelse(mydata2$Sub5==mydata2$Parent,NA,mydata2$Sub5)
mydata2$Sub5<-ifelse(mydata2$Sub5==mydata2$Company,NA,mydata2$Sub5)
mydata2$Sub5<-ifelse(mydata2$Sub5==mydata2$Sub4,NA,mydata2$Sub5)
mydata2$Sub5<-ifelse(mydata2$Sub5==mydata2$Sub3,NA,mydata2$Sub5)
mydata2$Sub5<-ifelse(mydata2$Sub5==mydata2$Sub2,NA,mydata2$Sub5)
mydata2$Sub5<-ifelse(mydata2$Sub5==mydata2$Sub1,NA,mydata2$Sub5)
mydata2$Sub5<-ifelse(mydata2$Sub5==mydata2$GUO,NA,mydata2$Sub5)

mydata2$Sub6<-vlookup_df(mydata2$Sub5,mydata2,result_column = 8, lookup_column = 2)
mydata2$Sub6<-unlist(mydata2$Sub6)
mydata2$Sub6<-ifelse(mydata2$Sub6==mydata2$Parent,NA,mydata2$Sub6)
mydata2$Sub6<-ifelse(mydata2$Sub6==mydata2$Company,NA,mydata2$Sub6)
mydata2$Sub6<-ifelse(mydata2$Sub6==mydata2$Sub5,NA,mydata2$Sub6)
mydata2$Sub6<-ifelse(mydata2$Sub6==mydata2$Sub4,NA,mydata2$Sub6)
mydata2$Sub6<-ifelse(mydata2$Sub6==mydata2$Sub3,NA,mydata2$Sub6)
mydata2$Sub6<-ifelse(mydata2$Sub6==mydata2$Sub2,NA,mydata2$Sub6)
mydata2$Sub6<-ifelse(mydata2$Sub6==mydata2$Sub1,NA,mydata2$Sub6)
mydata2$Sub6<-ifelse(mydata2$Sub6==mydata2$GUO,NA,mydata2$Sub6)

#Check if any parents/subsidiaries are left
length(which(!is.na(mydata2$Sub4))) 

##Establish ownership connections between companies and their parents
#Look for Parents in the Subsidiary  Column and return Parents in column Own1-8:
mydata2$Own1<-vlookup_df(mydata2$Parent,mydata2,result_column = 2, lookup_column = 3)
mydata2$Own1<-unlist(mydata2$Own1)
mydata2$Own1<-ifelse(mydata2$Own1==mydata2$Parent,NA,mydata2$Own1)
mydata2$Own1<-ifelse(mydata2$Own1==mydata2$Company,NA,mydata2$Own1)
mydata2$Own1<-ifelse(mydata2$Own1==mydata2$GUO,NA,mydata2$Own1)

mydata2$Own2<-vlookup_df(mydata2$Own1,mydata2,result_column = 2, lookup_column = 3)
mydata2$Own2<-unlist(mydata2$Own2)
mydata2$Own2<-ifelse(mydata2$Own2==mydata2$Parent,NA,mydata2$Own2)
mydata2$Own2<-ifelse(mydata2$Own2==mydata2$Company,NA,mydata2$Own2)
mydata2$Own2<-ifelse(mydata2$Own2==mydata2$Own1,NA,mydata2$Own2)
mydata2$Own2<-ifelse(mydata2$Own2==mydata2$GUO,NA,mydata2$Own2)

mydata2$Own3<-vlookup_df(mydata2$Own2,mydata2,result_column = 2, lookup_column = 3)
mydata2$Own3<-unlist(mydata2$Own3)
mydata2$Own3<-ifelse(mydata2$Own3==mydata2$Parent,NA,mydata2$Own3)
mydata2$Own3<-ifelse(mydata2$Own3==mydata2$Company,NA,mydata2$Own3)
mydata2$Own3<-ifelse(mydata2$Own3==mydata2$Own2,NA,mydata2$Own3)
mydata2$Own3<-ifelse(mydata2$Own3==mydata2$Own1,NA,mydata2$Own3)
mydata2$Own3<-ifelse(mydata2$Own3==mydata2$GUO,NA,mydata2$Own3)

mydata2$Own4<-vlookup_df(mydata2$Own3,mydata2,result_column = 2, lookup_column = 3)
mydata2$Own4<-unlist(mydata2$Own4)
mydata2$Own4<-ifelse(mydata2$Own4==mydata2$Parent,NA,mydata2$Own4)
mydata2$Own4<-ifelse(mydata2$Own4==mydata2$Company,NA,mydata2$Own4)
mydata2$Own4<-ifelse(mydata2$Own4==mydata2$Own3,NA,mydata2$Own4)
mydata2$Own4<-ifelse(mydata2$Own4==mydata2$Own2,NA,mydata2$Own4)
mydata2$Own4<-ifelse(mydata2$Own4==mydata2$Own1,NA,mydata2$Own4)
mydata2$Own4<-ifelse(mydata2$Own4==mydata2$GUO,NA,mydata2$Own4)

mydata2$Own5<-vlookup_df(mydata2$Own4,mydata2,result_column = 2, lookup_column = 3)
mydata2$Own5<-unlist(mydata2$Own5)
mydata2$Own5<-ifelse(mydata2$Own5==mydata2$Parent,NA,mydata2$Own5)
mydata2$Own5<-ifelse(mydata2$Own5==mydata2$Company,NA,mydata2$Own5)
mydata2$Own5<-ifelse(mydata2$Own5==mydata2$Own4,NA,mydata2$Own5)
mydata2$Own5<-ifelse(mydata2$Own5==mydata2$Own3,NA,mydata2$Own5)
mydata2$Own5<-ifelse(mydata2$Own5==mydata2$Own2,NA,mydata2$Own5)
mydata2$Own5<-ifelse(mydata2$Own5==mydata2$Own1,NA,mydata2$Own5)
mydata2$Own5<-ifelse(mydata2$Own5==mydata2$GUO,NA,mydata2$Own5)

mydata2$Own6<-vlookup_df(mydata2$Own5,mydata2,result_column = 2, lookup_column = 3)
mydata2$Own6<-unlist(mydata2$Own6)
mydata2$Own6<-ifelse(mydata2$Own6==mydata2$Parent,NA,mydata2$Own6)
mydata2$Own6<-ifelse(mydata2$Own6==mydata2$Company,NA,mydata2$Own6)
mydata2$Own6<-ifelse(mydata2$Own6==mydata2$Own5,NA,mydata2$Own6)
mydata2$Own6<-ifelse(mydata2$Own6==mydata2$Own4,NA,mydata2$Own6)
mydata2$Own6<-ifelse(mydata2$Own6==mydata2$Own3,NA,mydata2$Own6)
mydata2$Own6<-ifelse(mydata2$Own6==mydata2$Own2,NA,mydata2$Own6)
mydata2$Own6<-ifelse(mydata2$Own6==mydata2$Own1,NA,mydata2$Own6)
mydata2$Own6<-ifelse(mydata2$Own6==mydata2$GUO,NA,mydata2$Own6)

mydata2$Own7<-vlookup_df(mydata2$Own6,mydata2,result_column = 2, lookup_column = 3)
mydata2$Own7<-unlist(mydata2$Own7)
mydata2$Own7<-ifelse(mydata2$Own7==mydata2$Parent,NA,mydata2$Own7)
mydata2$Own7<-ifelse(mydata2$Own7==mydata2$Company,NA,mydata2$Own7)
mydata2$Own7<-ifelse(mydata2$Own7==mydata2$Own6,NA,mydata2$Own7)
mydata2$Own7<-ifelse(mydata2$Own7==mydata2$Own5,NA,mydata2$Own7)
mydata2$Own7<-ifelse(mydata2$Own7==mydata2$Own4,NA,mydata2$Own7)
mydata2$Own7<-ifelse(mydata2$Own7==mydata2$Own3,NA,mydata2$Own7)
mydata2$Own7<-ifelse(mydata2$Own7==mydata2$Own2,NA,mydata2$Own7)
mydata2$Own7<-ifelse(mydata2$Own7==mydata2$Own1,NA,mydata2$Own7)
mydata2$Own7<-ifelse(mydata2$Own7==mydata2$GUO,NA,mydata2$Own7)

mydata2$Own8<-vlookup_df(mydata2$Own7,mydata2,result_column = 2, lookup_column = 3)
mydata2$Own8<-unlist(mydata2$Own6)
mydata2$Own8<-ifelse(mydata2$Own8==mydata2$Parent,NA,mydata2$Own8)
mydata2$Own8<-ifelse(mydata2$Own8==mydata2$Company,NA,mydata2$Own8)
mydata2$Own8<-ifelse(mydata2$Own8==mydata2$Own7,NA,mydata2$Own8)
mydata2$Own8<-ifelse(mydata2$Own8==mydata2$Own6,NA,mydata2$Own8)
mydata2$Own8<-ifelse(mydata2$Own8==mydata2$Own5,NA,mydata2$Own8)
mydata2$Own8<-ifelse(mydata2$Own8==mydata2$Own4,NA,mydata2$Own8)
mydata2$Own8<-ifelse(mydata2$Own8==mydata2$Own3,NA,mydata2$Own8)
mydata2$Own8<-ifelse(mydata2$Own8==mydata2$Own2,NA,mydata2$Own8)
mydata2$Own8<-ifelse(mydata2$Own8==mydata2$Own1,NA,mydata2$Own8)
mydata2$Own8<-ifelse(mydata2$Own8==mydata2$GUO,NA,mydata2$Own8)

#Check if any subsidiaries/parents are left
length(which(!is.na(mydata2$Own8)))

##Restructure dataset
#Reorder the data to receive a row of Ownership relations
col_order<-c("GUO","Own8","Own7","Own6","Own5","Own4","Own3","Own2","Own1",
             "Parent","Company","Sub1","Sub2","Sub3","Sub4","Sub5","Sub6")
Network2<-mydata2[, col_order]

#Remove duplicate values in each column
Duplicate2<-t(apply(Network2, 1, function(x) { x[duplicated(x)] <- NA; x }))
Network2<-as.data.frame(Duplicate2)

#Make a dataset with column values moved to the left to right - creates ownership levels
Sort2<- as.data.frame(t(apply(Network2, 1, function(x) x[order(is.na(x))])))     #Takes some time

#Remove empty columns
emptycols <- sapply(Sort2, function (k) all(is.na(k)))
Sorted2 <- Sort2[!emptycols]

names(Sorted2) <- c("Own1","Own2","Own3","Own4","Own5","Own6","Own7",
                    "Own8","Own9","Own10","Own11","Own12")

#Remove Duplicates and sort Alphabetically
Duplicates2<-unique(Sorted2,)
Interim2<-Duplicates2[order(Duplicates2[,1],Duplicates2[,2],Duplicates2[,3],Duplicates2[,4],
                            Duplicates2[,5],Duplicates2[,6],Duplicates2[,7],Duplicates2[,8],
                            Duplicates2[,9],Duplicates2[,10],Duplicates2[,11],Duplicates2[,12]),]





#   3. Complement bottom-up data with top-down shareholder data ####

#Produce files of ultimate owners with all subsidiaries in one column
Interim211<-Interim2[,c(1,2)]
names(Interim211)<-c("Own","Subs")
Interim211<-subset(Interim211,!is.na(Interim211$Subs))
Interim212<-Interim2[,c(1,3)]
names(Interim212)<-c("Own","Subs")
Interim212<-subset(Interim212,!is.na(Interim212$Subs))
Interim213<-Interim2[,c(1,4)]
names(Interim213)<-c("Own","Subs")
Interim213<-subset(Interim213,!is.na(Interim213$Subs))
Interim214<-Interim2[,c(1,5)]
names(Interim214)<-c("Own","Subs")
Interim214<-subset(Interim214,!is.na(Interim214$Subs))
Interim215<-Interim2[,c(1,6)]
names(Interim215)<-c("Own","Subs")
Interim215<-subset(Interim215,!is.na(Interim215$Subs))
Interim216<-Interim2[,c(1,7)]
names(Interim216)<-c("Own","Subs")
Interim216<-subset(Interim216,!is.na(Interim216$Subs))
Interim217<-Interim2[,c(1,8)]
names(Interim217)<-c("Own","Subs")
Interim217<-subset(Interim217,!is.na(Interim217$Subs))
Interim218<-Interim2[,c(1,9)]
names(Interim218)<-c("Own","Subs")
Interim218<-subset(Interim218,!is.na(Interim218$Subs))
Interim219<-Interim2[,c(1,10)]
names(Interim219)<-c("Own","Subs")
Interim219<-subset(Interim219,!is.na(Interim219$Subs))
Interim220<-Interim2[,c(1,11)]
names(Interim220)<-c("Own","Subs")
Interim220<-subset(Interim220,!is.na(Interim220$Subs))
Interim221<-Interim2[,c(1,12)]
names(Interim221)<-c("Own","Subs")
Interim221<-subset(Interim221,!is.na(Interim221$Subs))
Interim223<-Interim2[,c(2,3)]
names(Interim223)<-c("Own","Subs")
Interim223<-subset(Interim223,!is.na(Interim223$Subs))
Interim224<-Interim2[,c(2,4)]
names(Interim224)<-c("Own","Subs")
Interim224<-subset(Interim224,!is.na(Interim224$Subs))
Interim225<-Interim2[,c(2,5)]
names(Interim225)<-c("Own","Subs")
Interim225<-subset(Interim225,!is.na(Interim225$Subs))
Interim226<-Interim2[,c(2,6)]
names(Interim226)<-c("Own","Subs")
Interim226<-subset(Interim226,!is.na(Interim226$Subs))
Interim227<-Interim2[,c(2,7)]
names(Interim227)<-c("Own","Subs")
Interim227<-subset(Interim227,!is.na(Interim227$Subs))
Interim228<-Interim2[,c(2,8)]
names(Interim228)<-c("Own","Subs")
Interim228<-subset(Interim228,!is.na(Interim228$Subs))
Interim229<-Interim2[,c(2,9)]
names(Interim229)<-c("Own","Subs")
Interim229<-subset(Interim229,!is.na(Interim229$Subs))
Interim230<-Interim2[,c(2,10)]
names(Interim230)<-c("Own","Subs")
Interim230<-subset(Interim230,!is.na(Interim230$Subs))
Interim231<-Interim2[,c(2,11)]
names(Interim231)<-c("Own","Subs")
Interim231<-subset(Interim231,!is.na(Interim231$Subs))
Interim232<-Interim2[,c(2,12)]
names(Interim232)<-c("Own","Subs")
Interim232<-subset(Interim232,!is.na(Interim232$Subs))
Interim234<-Interim2[,c(3,4)]
names(Interim234)<-c("Own","Subs")
Interim234<-subset(Interim234,!is.na(Interim234$Subs))
Interim235<-Interim2[,c(3,5)]
names(Interim235)<-c("Own","Subs")
Interim235<-subset(Interim235,!is.na(Interim235$Subs))
Interim236<-Interim2[,c(3,6)]
names(Interim236)<-c("Own","Subs")
Interim236<-subset(Interim236,!is.na(Interim236$Subs))
Interim237<-Interim2[,c(3,7)]
names(Interim237)<-c("Own","Subs")
Interim237<-subset(Interim237,!is.na(Interim237$Subs))
Interim238<-Interim2[,c(3,8)]
names(Interim238)<-c("Own","Subs")
Interim238<-subset(Interim238,!is.na(Interim238$Subs))
Interim239<-Interim2[,c(3,9)]
names(Interim239)<-c("Own","Subs")
Interim239<-subset(Interim239,!is.na(Interim239$Subs))
Interim240<-Interim2[,c(3,10)]
names(Interim240)<-c("Own","Subs")
Interim240<-subset(Interim240,!is.na(Interim240$Subs))
Interim241<-Interim2[,c(3,11)]
names(Interim241)<-c("Own","Subs")
Interim241<-subset(Interim241,!is.na(Interim241$Subs))
Interim242<-Interim2[,c(3,12)]
names(Interim242)<-c("Own","Subs")
Interim242<-subset(Interim242,!is.na(Interim242$Subs))
Interim244<-Interim2[,c(4,5)]
names(Interim244)<-c("Own","Subs")
Interim244<-subset(Interim244,!is.na(Interim244$Subs))
Interim245<-Interim2[,c(4,6)]
names(Interim245)<-c("Own","Subs")
Interim245<-subset(Interim245,!is.na(Interim245$Subs))
Interim246<-Interim2[,c(4,7)]
names(Interim246)<-c("Own","Subs")
Interim246<-subset(Interim246,!is.na(Interim246$Subs))
Interim247<-Interim2[,c(4,8)]
names(Interim247)<-c("Own","Subs")
Interim247<-subset(Interim247,!is.na(Interim247$Subs))
Interim248<-Interim2[,c(4,9)]
names(Interim248)<-c("Own","Subs")
Interim248<-subset(Interim248,!is.na(Interim248$Subs))
Interim249<-Interim2[,c(4,10)]
names(Interim249)<-c("Own","Subs")
Interim249<-subset(Interim249,!is.na(Interim249$Subs))
Interim250<-Interim2[,c(4,11)]
names(Interim250)<-c("Own","Subs")
Interim250<-subset(Interim250,!is.na(Interim250$Subs))
Interim251<-Interim2[,c(4,12)]
names(Interim251)<-c("Own","Subs")
Interim251<-subset(Interim251,!is.na(Interim251$Subs))
Interim253<-Interim2[,c(5,6)]
names(Interim253)<-c("Own","Subs")
Interim253<-subset(Interim253,!is.na(Interim253$Subs))
Interim254<-Interim2[,c(5,7)]
names(Interim254)<-c("Own","Subs")
Interim254<-subset(Interim254,!is.na(Interim254$Subs))
Interim255<-Interim2[,c(5,8)]
names(Interim255)<-c("Own","Subs")
Interim255<-subset(Interim255,!is.na(Interim255$Subs))
Interim256<-Interim2[,c(5,9)]
names(Interim256)<-c("Own","Subs")
Interim256<-subset(Interim256,!is.na(Interim256$Subs))
Interim257<-Interim2[,c(5,10)]
names(Interim257)<-c("Own","Subs")
Interim257<-subset(Interim257,!is.na(Interim257$Subs))
Interim258<-Interim2[,c(5,11)]
names(Interim258)<-c("Own","Subs")
Interim258<-subset(Interim258,!is.na(Interim258$Subs))
Interim259<-Interim2[,c(5,12)]
names(Interim259)<-c("Own","Subs")
Interim259<-subset(Interim259,!is.na(Interim259$Subs))
Interim261<-Interim2[,c(6,7)]
names(Interim261)<-c("Own","Subs")
Interim261<-subset(Interim261,!is.na(Interim261$Subs))
Interim262<-Interim2[,c(6,8)]
names(Interim262)<-c("Own","Subs")
Interim262<-subset(Interim262,!is.na(Interim262$Subs))
Interim263<-Interim2[,c(6,9)]
names(Interim263)<-c("Own","Subs")
Interim263<-subset(Interim263,!is.na(Interim263$Subs))
Interim264<-Interim2[,c(6,10)]
names(Interim264)<-c("Own","Subs")
Interim264<-subset(Interim264,!is.na(Interim264$Subs))
Interim265<-Interim2[,c(6,11)]
names(Interim265)<-c("Own","Subs")
Interim265<-subset(Interim265,!is.na(Interim265$Subs))
Interim266<-Interim2[,c(6,12)]
names(Interim266)<-c("Own","Subs")
Interim266<-subset(Interim266,!is.na(Interim266$Subs))
Interim268<-Interim2[,c(7,8)]
names(Interim268)<-c("Own","Subs")
Interim268<-subset(Interim268,!is.na(Interim268$Subs))
Interim269<-Interim2[,c(7,9)]
names(Interim269)<-c("Own","Subs")
Interim269<-subset(Interim269,!is.na(Interim269$Subs))
Interim270<-Interim2[,c(7,10)]
names(Interim270)<-c("Own","Subs")
Interim270<-subset(Interim270,!is.na(Interim270$Subs))
Interim271<-Interim2[,c(7,11)]
names(Interim271)<-c("Own","Subs")
Interim271<-subset(Interim271,!is.na(Interim271$Subs))
Interim272<-Interim2[,c(7,12)]
names(Interim272)<-c("Own","Subs")
Interim272<-subset(Interim272,!is.na(Interim272$Subs))
Interim274<-Interim2[,c(8,9)]
names(Interim274)<-c("Own","Subs")
Interim274<-subset(Interim274,!is.na(Interim274$Subs))
Interim275<-Interim2[,c(8,10)]
names(Interim275)<-c("Own","Subs")
Interim275<-subset(Interim275,!is.na(Interim275$Subs))
Interim276<-Interim2[,c(8,11)]
names(Interim276)<-c("Own","Subs")
Interim276<-subset(Interim276,!is.na(Interim276$Subs))
Interim277<-Interim2[,c(8,12)]
names(Interim277)<-c("Own","Subs")
Interim277<-subset(Interim277,!is.na(Interim277$Subs))
Interim279<-Interim2[,c(9,10)]
names(Interim279)<-c("Own","Subs")
Interim279<-subset(Interim279,!is.na(Interim279$Subs))
Interim280<-Interim2[,c(9,11)]
names(Interim280)<-c("Own","Subs")
Interim280<-subset(Interim280,!is.na(Interim280$Subs))
Interim281<-Interim2[,c(9,12)]
names(Interim281)<-c("Own","Subs")
Interim281<-subset(Interim281,!is.na(Interim281$Subs))
Interim283<-Interim2[,c(10,11)]
names(Interim283)<-c("Own","Subs")
Interim283<-subset(Interim283,!is.na(Interim283$Subs))
Interim284<-Interim2[,c(10,12)]
names(Interim284)<-c("Own","Subs")
Interim284<-subset(Interim284,!is.na(Interim284$Subs))
Interim286<-Interim2[,c(11,12)]
names(Interim286)<-c("Own","Subs")
Interim286<-subset(Interim286,!is.na(Interim286$Subs))

Add<-bind_rows(Interim211,Interim212,Interim213, Interim214,Interim215,Interim216,Interim217,Interim218,Interim219,Interim220,Interim221,Interim223,Interim224,
               Interim225,Interim226,Interim227,Interim228,Interim229,Interim230,Interim231,Interim232,Interim234,Interim235,Interim236,Interim237,Interim238,Interim239,
               Interim240,Interim241,Interim242,Interim244,Interim245,Interim246,Interim247,Interim248,Interim249,Interim250,Interim251,Interim253,Interim254,
               Interim255,Interim256,Interim257,Interim258,Interim259,Interim261,Interim262,Interim263,Interim264,Interim265,Interim266,Interim268,Interim269,
               Interim270,Interim271,Interim272,Interim274,Interim275,Interim276,Interim277,Interim279,Interim280,Interim281,Interim283,Interim284,Interim286)

rm(Interim211,Interim212,Interim213, Interim214,Interim215,Interim216,Interim217,Interim218,Interim219,Interim220,Interim221,Interim222,Interim223,Interim224,
   Interim225,Interim226,Interim227,Interim228,Interim229,Interim230,Interim231,Interim232,Interim233,Interim234,Interim235,Interim236,Interim237,Interim238,Interim239,
   Interim240,Interim241,Interim242,Interim243,Interim244,Interim245,Interim246,Interim247,Interim248,Interim249,Interim250,Interim251,Interim252,Interim253,Interim254,
   Interim255,Interim256,Interim257,Interim258,Interim259, Interim260,Interim261,Interim262,Interim263,Interim264,Interim265,Interim266,Interim267,Interim268,Interim269,
   Interim270,Interim271,Interim272,Interim273,Interim274,Interim275,Interim276,Interim277,Interim278,Interim279,Interim280,Interim281,Interim282,Interim283,Interim284,
   Interim285,Interim286,GUO_data,col_order,mydata1,mydata2,Company,Share_data,Sort1,Sort2,Sorted1,Sorted2,Share,OriginalGUO, OriginalCompany, Network1,
   Network2,mydata1,mydata2,Duplicates1,Duplicates2,Duplicate1,Duplicate2,emptycols,GUO_files,Share_files,emptycols,col_order,Interim2)

#Remove duplicates
Add$test<-paste(Add$Check2,Add$Subs,sep=" ")
Add<-Add[!duplicated(Add$test),]
Add[3]<-NULL

Add$Own<-as.factor(Add$Own)
Add$Subs<-as.factor(Add$Subs)
names(Add)<-c("Own","Subs")

#Establish ownership level 2 from shareholder data
Add1<-Add
Add1$Check1<-ifelse(Add1$Own%in%Interim1$Own1,1,0)
Add1$Check2<-ifelse(Add1$Subs%in%Interim1$Own1|Add1$Subs%in%Interim1$Own2|Add1$Subs%in%Interim1$Own3|Add1$Subs%in%Interim1$Own4|
                       Add1$Subs%in%Interim1$Own5|Add1$Subs%in%Interim1$Own6|Add1$Subs%in%Interim1$Own7|Add1$Subs%in%Interim1$Own8|
                       Add1$Subs%in%Interim1$Own9|Add1$Subs%in%Interim1$Own10|Add1$Subs%in%Interim1$Own11|Add1$Subs%in%Interim1$Own12|
                       Add1$Subs%in%Interim1$Own13|Add1$Subs%in%Interim1$Own14|Add1$Subs%in%Interim1$Own15|Add1$Subs%in%Interim1$Own16|
                       Add1$Subs%in%Interim1$Own17|Add1$Subs%in%Interim1$Own18|Add1$Subs%in%Interim1$Own19|Add1$Subs%in%Interim1$Own20|
                       Add1$Subs%in%Interim1$Own21,0,1)
Add1$Check<-ifelse(Add1$Check1==1 & Add1$Check2==1,1,0)
Add1<-subset(Add1,Add1$Check==1)
Add1[3:5]<-NULL
names(Add1)<-c("Own1","Own2")
mydata1<-Interim1[1:2]
mydata1<-bind_rows(mydata1,Add1)
mydata1$test<-paste(mydata1$Own1,mydata1$Own2,sep=" ")
mydata1<-mydata1[!duplicated(mydata1$test),]
mydata1[3]<-NULL

#Establish ownership level 3 from shareholder data
Add2<-Add
Add2$Check1<-ifelse(Add2$Own%in%mydata1$Own2,1,0)
Add2$Check2<-ifelse(Add2$Subs%in%Interim1$Own1|Add2$Subs%in%Interim1$Own2|Add2$Subs%in%Interim1$Own3|Add2$Subs%in%Interim1$Own4|
                       Add2$Subs%in%Interim1$Own5|Add2$Subs%in%Interim1$Own6|Add2$Subs%in%Interim1$Own7|Add2$Subs%in%Interim1$Own8|
                       Add2$Subs%in%Interim1$Own9|Add2$Subs%in%Interim1$Own10|Add2$Subs%in%Interim1$Own11|Add2$Subs%in%Interim1$Own12|
                       Add2$Subs%in%Interim1$Own13|Add2$Subs%in%Interim1$Own14|Add2$Subs%in%Interim1$Own15|Add2$Subs%in%Interim1$Own16|
                       Add2$Subs%in%Interim1$Own17|Add2$Subs%in%Interim1$Own18|Add2$Subs%in%Interim1$Own19|Add2$Subs%in%Interim1$Own20|
                       Add2$Subs%in%Interim1$Own21,0,1)
Add2$Check<-ifelse(Add2$Check1==1 & Add2$Check2==1,1,0)
Add2<-subset(Add2,Add2$Check==1)
Add2[3:5]<-NULL
names(Add2)<-c("Own2","Own3")
mydata21<-Interim1[1:3]
mydata22<-left_join(mydata1,Add2,by="Own2",na_matches="never")
mydata2<-bind_rows(mydata21,mydata22)
mydata2$test<-paste(mydata2$Own1,mydata2$Own2,mydata2$Own3,sep=" ")
mydata2<-mydata2[!duplicated(mydata2$test),]
mydata2[4]<-NULL

#Establish ownership level 4 from shareholder data
Add3<-Add
Add3$Check1<-ifelse(Add3$Own%in%mydata2$Own3,1,0)
Add3$Check2<-ifelse(Add3$Subs%in%Interim1$Own1|Add3$Subs%in%Interim1$Own2|Add3$Subs%in%Interim1$Own3|Add3$Subs%in%Interim1$Own4|
                       Add3$Subs%in%Interim1$Own5|Add3$Subs%in%Interim1$Own6|Add3$Subs%in%Interim1$Own7|Add3$Subs%in%Interim1$Own8|
                       Add3$Subs%in%Interim1$Own9|Add3$Subs%in%Interim1$Own10|Add3$Subs%in%Interim1$Own11|Add3$Subs%in%Interim1$Own12|
                       Add3$Subs%in%Interim1$Own13|Add3$Subs%in%Interim1$Own14|Add3$Subs%in%Interim1$Own15|Add3$Subs%in%Interim1$Own16|
                       Add3$Subs%in%Interim1$Own17|Add3$Subs%in%Interim1$Own18|Add3$Subs%in%Interim1$Own19|Add3$Subs%in%Interim1$Own20|
                       Add3$Subs%in%Interim1$Own21,0,1)
Add3$Check<-ifelse(Add3$Check1==1 & Add3$Check2==1,1,0)
Add3<-subset(Add3,Add3$Check==1)
Add3[3:5]<-NULL
names(Add3)<-c("Own3","Own4")
mydata31<-Interim1[1:4]
mydata32<-left_join(mydata2,Add3,by="Own3",na_matches="never")
mydata3<-bind_rows(mydata31,mydata32)
mydata3$test<-paste(mydata3$Own1,mydata3$Own2,mydata3$Own3,mydata3$Own4,sep=" ")
mydata3<-mydata3[!duplicated(mydata3$test),]
mydata3[5]<-NULL

#Establish ownership level 5 from shareholder data
Add4<-Add
Add4$Check1<-ifelse(Add4$Own%in%mydata3$Own4,1,0)
Add4$Check2<-ifelse(Add4$Subs%in%Interim1$Own1|Add4$Subs%in%Interim1$Own2|Add4$Subs%in%Interim1$Own3|Add4$Subs%in%Interim1$Own4|
                       Add4$Subs%in%Interim1$Own5|Add4$Subs%in%Interim1$Own6|Add4$Subs%in%Interim1$Own7|Add4$Subs%in%Interim1$Own8|
                       Add4$Subs%in%Interim1$Own9|Add4$Subs%in%Interim1$Own10|Add4$Subs%in%Interim1$Own11|Add4$Subs%in%Interim1$Own12|
                       Add4$Subs%in%Interim1$Own13|Add4$Subs%in%Interim1$Own14|Add4$Subs%in%Interim1$Own15|Add4$Subs%in%Interim1$Own16|
                       Add4$Subs%in%Interim1$Own17|Add4$Subs%in%Interim1$Own18|Add4$Subs%in%Interim1$Own19|Add4$Subs%in%Interim1$Own20|
                       Add4$Subs%in%Interim1$Own21,0,1)
Add4$Check<-ifelse(Add4$Check1==1 & Add4$Check2==1,1,0)
Add4<-subset(Add4,Add4$Check==1)
Add4[3:5]<-NULL
names(Add4)<-c("Own4","Own5")
mydata41<-Interim1[1:5]
mydata42<-left_join(mydata3,Add4,by="Own4",na_matches="never")
mydata4<-bind_rows(mydata41,mydata42)
mydata4$test<-paste(mydata4$Own1,mydata4$Own2,mydata4$Own3,mydata4$Own4,mydata4$Own5,sep=" ")
mydata4<-mydata4[!duplicated(mydata4$test),]
mydata4[6]<-NULL

#Establish ownership level 6 from shareholder data
Add5<-Add
Add5$Check1<-ifelse(Add5$Own%in%mydata4$Own5,1,0)
Add5$Check2<-ifelse(Add5$Subs%in%Interim1$Own1|Add5$Subs%in%Interim1$Own2|Add5$Subs%in%Interim1$Own3|Add5$Subs%in%Interim1$Own4|
                       Add5$Subs%in%Interim1$Own5|Add5$Subs%in%Interim1$Own6|Add5$Subs%in%Interim1$Own7|Add5$Subs%in%Interim1$Own8|
                       Add5$Subs%in%Interim1$Own9|Add5$Subs%in%Interim1$Own10|Add5$Subs%in%Interim1$Own11|Add5$Subs%in%Interim1$Own12|
                       Add5$Subs%in%Interim1$Own13|Add5$Subs%in%Interim1$Own14|Add5$Subs%in%Interim1$Own15|Add5$Subs%in%Interim1$Own16|
                       Add5$Subs%in%Interim1$Own17|Add5$Subs%in%Interim1$Own18|Add5$Subs%in%Interim1$Own19|Add5$Subs%in%Interim1$Own20|
                       Add5$Subs%in%Interim1$Own21,0,1)
Add5$Check<-ifelse(Add5$Check1==1 & Add5$Check2==1,1,0)
Add5<-subset(Add5,Add5$Check==1)
Add5[3:5]<-NULL
names(Add5)<-c("Own5","Own6")
mydata51<-Interim1[1:6]
mydata52<-left_join(mydata4,Add5,by="Own5",na_matches="never")
mydata5<-bind_rows(mydata51,mydata52)
mydata5$test<-paste(mydata5$Own1,mydata5$Own2,mydata5$Own3,mydata5$Own4,mydata5$Own5,mydata5$Own6,sep=" ")
mydata5<-mydata5[!duplicated(mydata5$test),]
mydata5[7]<-NULL

#Establish ownership level 7 from shareholder data
Add6<-Add
Add6$Check1<-ifelse(Add6$Own%in%mydata5$Own6,1,0)
Add6$Check2<-ifelse(Add6$Subs%in%Interim1$Own1|Add6$Subs%in%Interim1$Own2|Add6$Subs%in%Interim1$Own3|Add6$Subs%in%Interim1$Own4|
                       Add6$Subs%in%Interim1$Own5|Add6$Subs%in%Interim1$Own6|Add6$Subs%in%Interim1$Own7|Add6$Subs%in%Interim1$Own8|
                       Add6$Subs%in%Interim1$Own9|Add6$Subs%in%Interim1$Own10|Add6$Subs%in%Interim1$Own11|Add6$Subs%in%Interim1$Own12|
                       Add6$Subs%in%Interim1$Own13|Add6$Subs%in%Interim1$Own14|Add6$Subs%in%Interim1$Own15|Add6$Subs%in%Interim1$Own16|
                       Add6$Subs%in%Interim1$Own17|Add6$Subs%in%Interim1$Own18|Add6$Subs%in%Interim1$Own19|Add6$Subs%in%Interim1$Own20|
                       Add6$Subs%in%Interim1$Own21,0,1)
Add6$Check<-ifelse(Add6$Check1==1 & Add6$Check2==1,1,0)
Add6<-subset(Add6,Add6$Check==1)
Add6[3:5]<-NULL
names(Add6)<-c("Own6","Own7")
mydata61<-Interim1[1:7]
mydata62<-left_join(mydata5,Add6,by="Own6",na_matches="never")
mydata6<-bind_rows(mydata61,mydata62)
mydata6$test<-paste(mydata6$Own1,mydata6$Own2,mydata6$Own3,mydata6$Own4,mydata6$Own5,mydata6$Own6,mydata6$Own7,sep=" ")
mydata6<-mydata6[!duplicated(mydata6$test),]
mydata6[8]<-NULL

#Establish ownership level 8 from shareholder data
Add7<-Add
Add7$Check1<-ifelse(Add7$Own%in%mydata6$Own7,1,0)
Add7$Check2<-ifelse(Add7$Subs%in%Interim1$Own1|Add7$Subs%in%Interim1$Own2|Add7$Subs%in%Interim1$Own3|Add7$Subs%in%Interim1$Own4|
                       Add7$Subs%in%Interim1$Own5|Add7$Subs%in%Interim1$Own6|Add7$Subs%in%Interim1$Own7|Add7$Subs%in%Interim1$Own8|
                       Add7$Subs%in%Interim1$Own9|Add7$Subs%in%Interim1$Own10|Add7$Subs%in%Interim1$Own11|Add7$Subs%in%Interim1$Own12|
                       Add7$Subs%in%Interim1$Own13|Add7$Subs%in%Interim1$Own14|Add7$Subs%in%Interim1$Own15|Add7$Subs%in%Interim1$Own16|
                       Add7$Subs%in%Interim1$Own17|Add7$Subs%in%Interim1$Own18|Add7$Subs%in%Interim1$Own19|Add7$Subs%in%Interim1$Own20|
                       Add7$Subs%in%Interim1$Own21,0,1)
Add7$Check<-ifelse(Add7$Check1==1 & Add7$Check2==1,1,0)
Add7<-subset(Add7,Add7$Check==1)
Add7[3:5]<-NULL
names(Add7)<-c("Own7","Own8")
mydata71<-Interim1[1:8]
mydata72<-left_join(mydata6,Add7,by="Own7",na_matches="never")
mydata7<-bind_rows(mydata71,mydata72)
mydata7$test<-paste(mydata7$Own1,mydata7$Own2,mydata7$Own3,mydata7$Own4,mydata7$Own5,mydata7$Own6,mydata7$Own7,
                    mydata7$Own8,sep=" ")
mydata7<-mydata7[!duplicated(mydata7$test),]
mydata7[9]<-NULL

#Establish ownership level 9 from shareholder data
Add8<-Add
Add8$Check1<-ifelse(Add8$Own%in%mydata7$Own8,1,0)
Add8$Check2<-ifelse(Add8$Subs%in%Interim1$Own1|Add8$Subs%in%Interim1$Own2|Add8$Subs%in%Interim1$Own3|Add8$Subs%in%Interim1$Own4|
                       Add8$Subs%in%Interim1$Own5|Add8$Subs%in%Interim1$Own6|Add8$Subs%in%Interim1$Own7|Add8$Subs%in%Interim1$Own8|
                       Add8$Subs%in%Interim1$Own9|Add8$Subs%in%Interim1$Own10|Add8$Subs%in%Interim1$Own11|Add8$Subs%in%Interim1$Own12|
                       Add8$Subs%in%Interim1$Own13|Add8$Subs%in%Interim1$Own14|Add8$Subs%in%Interim1$Own15|Add8$Subs%in%Interim1$Own16|
                       Add8$Subs%in%Interim1$Own17|Add8$Subs%in%Interim1$Own18|Add8$Subs%in%Interim1$Own19|Add8$Subs%in%Interim1$Own20|
                       Add8$Subs%in%Interim1$Own21,0,1)
Add8$Check<-ifelse(Add8$Check1==1 & Add8$Check2==1,1,0)
Add8<-subset(Add8,Add8$Check==1)
Add8[3:5]<-NULL
names(Add8)<-c("Own8","Own9")
mydata81<-Interim1[1:9]
mydata82<-left_join(mydata7,Add8,by="Own8",na_matches="never")
mydata8<-bind_rows(mydata81,mydata82)
mydata8$test<-paste(mydata8$Own1,mydata8$Own2,mydata8$Own3,mydata8$Own4,mydata8$Own5,mydata8$Own6,mydata8$Own7,
                    mydata8$Own8,mydata8$Own9,sep=" ")
mydata8<-mydata8[!duplicated(mydata8$test),]
mydata8[10]<-NULL

#Establish ownership level 10 from shareholder data
Add9<-Add
Add9$Check1<-ifelse(Add9$Own%in%mydata8$Own9,1,0)
Add9$Check2<-ifelse(Add9$Subs%in%Interim1$Own1|Add9$Subs%in%Interim1$Own2|Add9$Subs%in%Interim1$Own3|Add9$Subs%in%Interim1$Own4|
                       Add9$Subs%in%Interim1$Own5|Add9$Subs%in%Interim1$Own6|Add9$Subs%in%Interim1$Own7|Add9$Subs%in%Interim1$Own8|
                       Add9$Subs%in%Interim1$Own9|Add9$Subs%in%Interim1$Own10|Add9$Subs%in%Interim1$Own11|Add9$Subs%in%Interim1$Own12|
                       Add9$Subs%in%Interim1$Own13|Add9$Subs%in%Interim1$Own14|Add9$Subs%in%Interim1$Own15|Add9$Subs%in%Interim1$Own16|
                       Add9$Subs%in%Interim1$Own17|Add9$Subs%in%Interim1$Own18|Add9$Subs%in%Interim1$Own19|Add9$Subs%in%Interim1$Own20|
                       Add9$Subs%in%Interim1$Own21,0,1)
Add9$Check<-ifelse(Add9$Check1==1 & Add9$Check2==1,1,0)
Add9<-subset(Add9,Add9$Check==1)
Add9[3:5]<-NULL
names(Add9)<-c("Own9","Own10")
mydata91<-Interim1[1:10]
mydata92<-left_join(mydata8,Add9,by="Own9",na_matches="never")
mydata9<-bind_rows(mydata91,mydata92)
mydata9$test<-paste(mydata9$Own1,mydata9$Own2,mydata9$Own3,mydata9$Own4,mydata9$Own5,mydata9$Own6,mydata9$Own7,
                    mydata9$Own8,mydata9$Own9,mydata9$Own10,sep=" ")
mydata9<-mydata9[!duplicated(mydata9$test),]
mydata9[11]<-NULL

#Establish ownership level 11 from shareholder data
Add10<-Add
Add10$Check1<-ifelse(Add10$Own%in%mydata9$Own10,1,0)
Add10$Check2<-ifelse(Add10$Subs%in%Interim1$Own1|Add10$Subs%in%Interim1$Own2|Add10$Subs%in%Interim1$Own3|Add10$Subs%in%Interim1$Own4|
                        Add10$Subs%in%Interim1$Own5|Add10$Subs%in%Interim1$Own6|Add10$Subs%in%Interim1$Own7|Add10$Subs%in%Interim1$Own8|
                        Add10$Subs%in%Interim1$Own9|Add10$Subs%in%Interim1$Own10|Add10$Subs%in%Interim1$Own11|Add10$Subs%in%Interim1$Own12|
                        Add10$Subs%in%Interim1$Own13|Add10$Subs%in%Interim1$Own14|Add10$Subs%in%Interim1$Own15|Add10$Subs%in%Interim1$Own16|
                        Add10$Subs%in%Interim1$Own17|Add10$Subs%in%Interim1$Own18|Add10$Subs%in%Interim1$Own19|Add10$Subs%in%Interim1$Own20|
                        Add10$Subs%in%Interim1$Own21,0,1)
Add10$Check<-ifelse(Add10$Check1==1 & Add10$Check2==1,1,0)
Add10<-subset(Add10,Add10$Check==1)
Add10[3:5]<-NULL
names(Add10)<-c("Own10","Own11")
mydata101<-Interim1[1:11]
mydata102<-left_join(mydata9,Add10,by="Own10",na_matches="never")
mydata10<-bind_rows(mydata101,mydata102)
mydata10$test<-paste(mydata10$Own1,mydata10$Own2,mydata10$Own3,mydata10$Own4,mydata10$Own5,mydata10$Own6,mydata10$Own7,
                     mydata10$Own8,mydata10$Own9,mydata10$Own10,mydata10$Own11,sep=" ")
mydata10<-mydata10[!duplicated(mydata10$test),]
mydata10[12]<-NULL

#Establish ownership level 12 from shareholder data
Add11<-Add
Add11$Check1<-ifelse(Add11$Own%in%mydata10$Own11,1,0)
Add11$Check2<-ifelse(Add11$Subs%in%Interim1$Own1|Add11$Subs%in%Interim1$Own2|Add11$Subs%in%Interim1$Own3|Add11$Subs%in%Interim1$Own4|
                        Add11$Subs%in%Interim1$Own5|Add11$Subs%in%Interim1$Own6|Add11$Subs%in%Interim1$Own7|Add11$Subs%in%Interim1$Own8|
                        Add11$Subs%in%Interim1$Own9|Add11$Subs%in%Interim1$Own10|Add11$Subs%in%Interim1$Own11|Add11$Subs%in%Interim1$Own12|
                        Add11$Subs%in%Interim1$Own13|Add11$Subs%in%Interim1$Own14|Add11$Subs%in%Interim1$Own15|Add11$Subs%in%Interim1$Own16|
                        Add11$Subs%in%Interim1$Own17|Add11$Subs%in%Interim1$Own18|Add11$Subs%in%Interim1$Own19|Add11$Subs%in%Interim1$Own20|
                        Add11$Subs%in%Interim1$Own21,0,1)
Add11$Check<-ifelse(Add11$Check1==1 & Add11$Check2==1,1,0)
Add11<-subset(Add11,Add11$Check==1)
Add11[3:5]<-NULL
names(Add11)<-c("Own11","Own12")
mydata111<-Interim1[1:12]
mydata112<-left_join(mydata10,Add11,by="Own11",na_matches="never")
mydata11<-bind_rows(mydata111,mydata112)
mydata11$test<-paste(mydata11$Own1,mydata11$Own2,mydata11$Own3,mydata11$Own4,mydata11$Own5,mydata11$Own6,mydata11$Own7,
                     mydata11$Own8,mydata11$Own9,mydata11$Own10,mydata11$Own11,mydata11$Own12,sep=" ")
mydata11<-mydata11[!duplicated(mydata11$test),]
mydata11[13]<-NULL

#Establish ownership level 13 from shareholder data
Add12<-Add
Add12$Check1<-ifelse(Add12$Own%in%mydata11$Own12,1,0)
Add12$Check2<-ifelse(Add12$Subs%in%Interim1$Own1|Add12$Subs%in%Interim1$Own2|Add12$Subs%in%Interim1$Own3|Add12$Subs%in%Interim1$Own4|
                        Add12$Subs%in%Interim1$Own5|Add12$Subs%in%Interim1$Own6|Add12$Subs%in%Interim1$Own7|Add12$Subs%in%Interim1$Own8|
                        Add12$Subs%in%Interim1$Own9|Add12$Subs%in%Interim1$Own10|Add12$Subs%in%Interim1$Own11|Add12$Subs%in%Interim1$Own12|
                        Add12$Subs%in%Interim1$Own13|Add12$Subs%in%Interim1$Own14|Add12$Subs%in%Interim1$Own15|Add12$Subs%in%Interim1$Own16|
                        Add12$Subs%in%Interim1$Own17|Add12$Subs%in%Interim1$Own18|Add12$Subs%in%Interim1$Own19|Add12$Subs%in%Interim1$Own20|
                        Add12$Subs%in%Interim1$Own21,0,1)
Add12$Check<-ifelse(Add12$Check1==1 & Add12$Check2==1,1,0)
Add12<-subset(Add12,Add12$Check==1)
Add12[3:5]<-NULL
names(Add12)<-c("Own12","Own13")
mydata121<-Interim1[1:13]
mydata122<-left_join(mydata11,Add12,by="Own12",na_matches="never")
mydata12<-bind_rows(mydata121,mydata122)
mydata12$test<-paste(mydata12$Own1,mydata12$Own2,mydata12$Own3,mydata12$Own4,mydata12$Own5,mydata12$Own6,mydata12$Own7,
                     mydata12$Own8,mydata12$Own9,mydata12$Own10,mydata12$Own11,mydata12$Own12,mydata12$Own13,
                     sep=" ")
mydata12<-mydata12[!duplicated(mydata12$test),]
mydata12[14]<-NULL

#Establish ownership level 14 from shareholder data
Add13<-Add
Add13$Check1<-ifelse(Add13$Own%in%mydata12$Own13,1,0)
Add13$Check2<-ifelse(Add13$Subs%in%Interim1$Own1|Add13$Subs%in%Interim1$Own2|Add13$Subs%in%Interim1$Own3|Add13$Subs%in%Interim1$Own4|
                        Add13$Subs%in%Interim1$Own5|Add13$Subs%in%Interim1$Own6|Add13$Subs%in%Interim1$Own7|Add13$Subs%in%Interim1$Own8|
                        Add13$Subs%in%Interim1$Own9|Add13$Subs%in%Interim1$Own10|Add13$Subs%in%Interim1$Own11|Add13$Subs%in%Interim1$Own12|
                        Add13$Subs%in%Interim1$Own13|Add13$Subs%in%Interim1$Own14|Add13$Subs%in%Interim1$Own15|Add13$Subs%in%Interim1$Own16|
                        Add13$Subs%in%Interim1$Own17|Add13$Subs%in%Interim1$Own18|Add13$Subs%in%Interim1$Own19|Add13$Subs%in%Interim1$Own20|
                        Add13$Subs%in%Interim1$Own21,0,1)
Add13$Check<-ifelse(Add13$Check1==1 & Add13$Check2==1,1,0)
Add13<-subset(Add13,Add13$Check==1)
Add13[3:5]<-NULL
names(Add13)<-c("Own13","Own14")
mydata131<-Interim1[1:14]
mydata132<-left_join(mydata12,Add13,by="Own13",na_matches="never")
mydata13<-bind_rows(mydata131,mydata132)
mydata13$test<-paste(mydata13$Own1,mydata13$Own2,mydata13$Own3,mydata13$Own4,mydata13$Own5,mydata13$Own6,mydata13$Own7,
                     mydata13$Own8,mydata13$Own9,mydata13$Own10,mydata13$Own11,mydata13$Own12,mydata13$Own13,
                     mydata13$Own14,sep=" ")
mydata13<-mydata13[!duplicated(mydata13$test),]
mydata13[15]<-NULL

#Establish ownership level 15 from shareholder data
Add14<-Add
Add14$Check1<-ifelse(Add14$Own%in%mydata13$Own14,1,0)
Add14$Check2<-ifelse(Add14$Subs%in%Interim1$Own1|Add14$Subs%in%Interim1$Own2|Add14$Subs%in%Interim1$Own3|Add14$Subs%in%Interim1$Own4|
                        Add14$Subs%in%Interim1$Own5|Add14$Subs%in%Interim1$Own6|Add14$Subs%in%Interim1$Own7|Add14$Subs%in%Interim1$Own8|
                        Add14$Subs%in%Interim1$Own9|Add14$Subs%in%Interim1$Own10|Add14$Subs%in%Interim1$Own11|Add14$Subs%in%Interim1$Own12|
                        Add14$Subs%in%Interim1$Own13|Add14$Subs%in%Interim1$Own14|Add14$Subs%in%Interim1$Own15|Add14$Subs%in%Interim1$Own16|
                        Add14$Subs%in%Interim1$Own17|Add14$Subs%in%Interim1$Own18|Add14$Subs%in%Interim1$Own19|Add14$Subs%in%Interim1$Own20|
                        Add14$Subs%in%Interim1$Own21,0,1)
Add14$Check<-ifelse(Add14$Check1==1 & Add14$Check2==1,1,0)
Add14<-subset(Add14,Add14$Check==1)
Add14[3:5]<-NULL
names(Add14)<-c("Own14","Own15")
mydata141<-Interim1[1:15]
mydata142<-left_join(mydata13,Add14,by="Own14",na_matches="never")
mydata14<-bind_rows(mydata141,mydata142)
mydata14$test<-paste(mydata14$Own1,mydata14$Own2,mydata14$Own3,mydata14$Own4,mydata14$Own5,mydata14$Own6,mydata14$Own7,
                     mydata14$Own8,mydata14$Own9,mydata14$Own10,mydata14$Own11,mydata14$Own12,mydata14$Own13,
                     mydata14$Own14,mydata14$Own15,sep=" ")
mydata14<-mydata14[!duplicated(mydata14$test),]
mydata14[16]<-NULL

#Establish ownership level 16 from shareholder data
Add15<-Add
Add15$Check1<-ifelse(Add15$Own%in%mydata14$Own15,1,0)
Add15$Check2<-ifelse(Add15$Subs%in%Interim1$Own1|Add15$Subs%in%Interim1$Own2|Add15$Subs%in%Interim1$Own3|Add15$Subs%in%Interim1$Own4|
                        Add15$Subs%in%Interim1$Own5|Add15$Subs%in%Interim1$Own6|Add15$Subs%in%Interim1$Own7|Add15$Subs%in%Interim1$Own8|
                        Add15$Subs%in%Interim1$Own9|Add15$Subs%in%Interim1$Own10|Add15$Subs%in%Interim1$Own11|Add15$Subs%in%Interim1$Own12|
                        Add15$Subs%in%Interim1$Own13|Add15$Subs%in%Interim1$Own14|Add15$Subs%in%Interim1$Own15|Add15$Subs%in%Interim1$Own16|
                        Add15$Subs%in%Interim1$Own17|Add15$Subs%in%Interim1$Own18|Add15$Subs%in%Interim1$Own19|Add15$Subs%in%Interim1$Own20|
                        Add15$Subs%in%Interim1$Own21,0,1)
Add15$Check<-ifelse(Add15$Check1==1 & Add15$Check2==1,1,0)
Add15<-subset(Add15,Add15$Check==1)
Add15[3:5]<-NULL
names(Add15)<-c("Own15","Own16")
mydata151<-Interim1[1:16]
mydata152<-left_join(mydata14,Add15,by="Own15",na_matches="never")
mydata15<-bind_rows(mydata151,mydata152)
mydata15$test<-paste(mydata15$Own1,mydata15$Own2,mydata15$Own3,mydata15$Own4,mydata15$Own5,mydata15$Own6,mydata15$Own7,
                     mydata15$Own8,mydata15$Own9,mydata15$Own10,mydata15$Own11,mydata15$Own12,mydata15$Own13,
                     mydata15$Own14,mydata15$Own15,mydata15$Own16,sep=" ")
mydata15<-mydata15[!duplicated(mydata15$test),]
mydata15[17]<-NULL

#Establish ownership level 17 from shareholder data
Add16<-Add
Add16$Check1<-ifelse(Add16$Own%in%mydata15$Own16,1,0)
Add16$Check2<-ifelse(Add16$Subs%in%Interim1$Own1|Add16$Subs%in%Interim1$Own2|Add16$Subs%in%Interim1$Own3|Add16$Subs%in%Interim1$Own4|
                        Add16$Subs%in%Interim1$Own5|Add16$Subs%in%Interim1$Own6|Add16$Subs%in%Interim1$Own7|Add16$Subs%in%Interim1$Own8|
                        Add16$Subs%in%Interim1$Own9|Add16$Subs%in%Interim1$Own10|Add16$Subs%in%Interim1$Own11|Add16$Subs%in%Interim1$Own12|
                        Add16$Subs%in%Interim1$Own13|Add16$Subs%in%Interim1$Own14|Add16$Subs%in%Interim1$Own15|Add16$Subs%in%Interim1$Own16|
                        Add16$Subs%in%Interim1$Own17|Add16$Subs%in%Interim1$Own18|Add16$Subs%in%Interim1$Own19|Add16$Subs%in%Interim1$Own20|
                        Add16$Subs%in%Interim1$Own21,0,1)
Add16$Check<-ifelse(Add16$Check1==1 & Add16$Check2==1,1,0)
Add16<-subset(Add16,Add16$Check==1)
Add16[3:5]<-NULL
names(Add16)<-c("Own16","Own17")
mydata161<-Interim1[1:17]
mydata162<-left_join(mydata15,Add16,by="Own16",na_matches="never")
mydata16<-bind_rows(mydata161,mydata162)
mydata16$test<-paste(mydata16$Own1,mydata16$Own2,mydata16$Own3,mydata16$Own4,mydata16$Own5,mydata16$Own6,mydata16$Own7,
                     mydata16$Own8,mydata16$Own9,mydata16$Own10,mydata16$Own11,mydata16$Own12,mydata16$Own13,
                     mydata16$Own14,mydata16$Own15,mydata16$Own16,mydata16$Own17,sep=" ")
mydata16<-mydata16[!duplicated(mydata16$test),]
mydata16[18]<-NULL

#Establish ownership level 18 from shareholder data
Add17<-Add
Add17$Check1<-ifelse(Add17$Own%in%mydata16$Own17,1,0)
Add17$Check2<-ifelse(Add17$Subs%in%Interim1$Own1|Add17$Subs%in%Interim1$Own2|Add17$Subs%in%Interim1$Own3|Add17$Subs%in%Interim1$Own4|
                        Add17$Subs%in%Interim1$Own5|Add17$Subs%in%Interim1$Own6|Add17$Subs%in%Interim1$Own7|Add17$Subs%in%Interim1$Own8|
                        Add17$Subs%in%Interim1$Own9|Add17$Subs%in%Interim1$Own10|Add17$Subs%in%Interim1$Own11|Add17$Subs%in%Interim1$Own12|
                        Add17$Subs%in%Interim1$Own13|Add17$Subs%in%Interim1$Own14|Add17$Subs%in%Interim1$Own15|Add17$Subs%in%Interim1$Own16|
                        Add17$Subs%in%Interim1$Own17|Add17$Subs%in%Interim1$Own18|Add17$Subs%in%Interim1$Own19|Add17$Subs%in%Interim1$Own20|
                        Add17$Subs%in%Interim1$Own21,0,1)
Add17$Check<-ifelse(Add17$Check1==1 & Add17$Check2==1,1,0)
Add17<-subset(Add17,Add17$Check==1)
Add17[3:5]<-NULL
names(Add17)<-c("Own17","Own18")
mydata171<-Interim1[1:18]
mydata172<-left_join(mydata16,Add17,by="Own17",na_matches="never")
mydata17<-bind_rows(mydata171,mydata172)
mydata17$test<-paste(mydata17$Own1,mydata17$Own2,mydata17$Own3,mydata17$Own4,mydata17$Own5,mydata17$Own6,mydata17$Own7,
                     mydata17$Own8,mydata17$Own9,mydata17$Own10,mydata17$Own11,mydata17$Own12,mydata17$Own13,
                     mydata17$Own14,mydata17$Own15,mydata17$Own16,mydata17$Own17,mydata17$Own18,sep=" ")
mydata17<-mydata17[!duplicated(mydata17$test),]
mydata17[19]<-NULL

#Establish ownership level 19 from shareholder data
Add18<-Add
Add18$Check1<-ifelse(Add18$Own%in%mydata17$Own18,1,0)
Add18$Check2<-ifelse(Add18$Subs%in%Interim1$Own1|Add18$Subs%in%Interim1$Own2|Add18$Subs%in%Interim1$Own3|Add18$Subs%in%Interim1$Own4|
                        Add18$Subs%in%Interim1$Own5|Add18$Subs%in%Interim1$Own6|Add18$Subs%in%Interim1$Own7|Add18$Subs%in%Interim1$Own8|
                        Add18$Subs%in%Interim1$Own9|Add18$Subs%in%Interim1$Own10|Add18$Subs%in%Interim1$Own11|Add18$Subs%in%Interim1$Own12|
                        Add18$Subs%in%Interim1$Own13|Add18$Subs%in%Interim1$Own14|Add18$Subs%in%Interim1$Own15|Add18$Subs%in%Interim1$Own16|
                        Add18$Subs%in%Interim1$Own17|Add18$Subs%in%Interim1$Own18|Add18$Subs%in%Interim1$Own19|Add18$Subs%in%Interim1$Own20|
                        Add18$Subs%in%Interim1$Own21,0,1)
Add18$Check<-ifelse(Add18$Check1==1 & Add18$Check2==1,1,0)
Add18<-subset(Add18,Add18$Check==1)
Add18[3:5]<-NULL
names(Add18)<-c("Own18","Own19")
mydata181<-Interim1[1:19]
mydata182<-left_join(mydata17,Add18,by="Own18",na_matches="never")
mydata18<-bind_rows(mydata181,mydata182)
mydata18$test<-paste(mydata18$Own1,mydata18$Own2,mydata18$Own3,mydata18$Own4,mydata18$Own5,mydata18$Own6,mydata18$Own7,
                     mydata18$Own8,mydata18$Own9,mydata18$Own10,mydata18$Own11,mydata18$Own12,mydata18$Own13,
                     mydata18$Own14,mydata18$Own15,mydata18$Own16,mydata18$Own17,mydata18$Own18,mydata18$Own19,
                     sep=" ")
mydata18<-mydata18[!duplicated(mydata18$test),]
mydata18[20]<-NULL

#Establish ownership level 20 from shareholder data
Add19<-Add
Add19$Check1<-ifelse(Add19$Own%in%mydata18$Own19,1,0)
Add19$Check2<-ifelse(Add19$Subs%in%Interim1$Own1|Add19$Subs%in%Interim1$Own2|Add19$Subs%in%Interim1$Own3|Add19$Subs%in%Interim1$Own4|
                        Add19$Subs%in%Interim1$Own5|Add19$Subs%in%Interim1$Own6|Add19$Subs%in%Interim1$Own7|Add19$Subs%in%Interim1$Own8|
                        Add19$Subs%in%Interim1$Own9|Add19$Subs%in%Interim1$Own10|Add19$Subs%in%Interim1$Own11|Add19$Subs%in%Interim1$Own12|
                        Add19$Subs%in%Interim1$Own13|Add19$Subs%in%Interim1$Own14|Add19$Subs%in%Interim1$Own15|Add19$Subs%in%Interim1$Own16|
                        Add19$Subs%in%Interim1$Own17|Add19$Subs%in%Interim1$Own18|Add19$Subs%in%Interim1$Own19|Add19$Subs%in%Interim1$Own20|
                        Add19$Subs%in%Interim1$Own21,0,1)
Add19$Check<-ifelse(Add19$Check1==1 & Add19$Check2==1,1,0)
Add19<-subset(Add19,Add19$Check==1)
Add19[3:5]<-NULL
names(Add19)<-c("Own19","Own20")
mydata191<-Interim1[1:20]
mydata192<-left_join(mydata18,Add19,by="Own19",na_matches="never")
mydata19<-bind_rows(mydata191,mydata192)
mydata19$test<-paste(mydata19$Own1,mydata19$Own2,mydata19$Own3,mydata19$Own4,mydata19$Own5,mydata19$Own6,mydata19$Own7,
                     mydata19$Own8,mydata19$Own9,mydata19$Own10,mydata19$Own11,mydata19$Own12,mydata19$Own13,
                     mydata19$Own14,mydata19$Own15,mydata19$Own16,mydata19$Own17,mydata19$Own18,mydata19$Own19,
                     mydata19$Own20,sep=" ")
mydata19<-mydata19[!duplicated(mydata19$test),]
mydata19[21]<-NULL

#Establish ownership level 21 from shareholder data
Add20<-Add
Add20$Check1<-ifelse(Add20$Own%in%mydata19$Own20,1,0)
Add20$Check2<-ifelse(Add20$Subs%in%Interim1$Own1|Add20$Subs%in%Interim1$Own2|Add20$Subs%in%Interim1$Own3|Add20$Subs%in%Interim1$Own4|
                        Add20$Subs%in%Interim1$Own5|Add20$Subs%in%Interim1$Own6|Add20$Subs%in%Interim1$Own7|Add20$Subs%in%Interim1$Own8|
                        Add20$Subs%in%Interim1$Own9|Add20$Subs%in%Interim1$Own10|Add20$Subs%in%Interim1$Own11|Add20$Subs%in%Interim1$Own12|
                        Add20$Subs%in%Interim1$Own13|Add20$Subs%in%Interim1$Own14|Add20$Subs%in%Interim1$Own15|Add20$Subs%in%Interim1$Own16|
                        Add20$Subs%in%Interim1$Own17|Add20$Subs%in%Interim1$Own18|Add20$Subs%in%Interim1$Own19|Add20$Subs%in%Interim1$Own20|
                        Add20$Subs%in%Interim1$Own21,0,1)
Add20$Check<-ifelse(Add20$Check1==1 & Add20$Check2==1,1,0)
Add20<-subset(Add20,Add20$Check==1)
Add20[3:5]<-NULL
names(Add20)<-c("Own20","Own21")
mydata201<-Interim1[1:21]
mydata202<-left_join(mydata19,Add20,by="Own20",na_matches="never")
mydata20<-bind_rows(mydata201,mydata202)
mydata20$test<-paste(mydata20$Own1,mydata20$Own2,mydata20$Own3,mydata20$Own4,mydata20$Own5,mydata20$Own6,mydata20$Own7,
                     mydata20$Own8,mydata20$Own9,mydata20$Own10,mydata20$Own11,mydata20$Own12,mydata20$Own13,
                     mydata20$Own14,mydata20$Own15,mydata20$Own16,mydata20$Own17,mydata20$Own18,mydata20$Own19,
                     mydata20$Own20,sep=" ")
mydata20<-mydata20[!duplicated(mydata20$test),]
mydata20[22]<-NULL

#Remove Duplicates
mydata<-unique(mydata20,)
mydata<-mydata[complete.cases(mydata[ , 1:2]),]

#Remove unnecessary files
rm(Add1,Add2,Add3,Add4,Add5,Add6,Add7,Add8,Add9,Add10,Add11,Add12,Add13,Add14,Add15,Add16,Add17,Add18,Add19,Add20,
   mydata2,mydata3,mydata4,mydata5,mydata6,mydata7,mydata8,mydata9,mydata10,mydata11,mydata12,mydata13,
   mydata14,mydata15,mydata16,mydata17,mydata18,mydata19,mydata21,mydata22,mydata21,mydata22,mydata31,mydata32,
   mydata41,mydata42,mydata51,mydata52,mydata61,mydata62,mydata71,mydata72,mydata81,mydata82,mydata91,mydata92,
   mydata101,mydata102,mydata111,mydata112,mydata121,mydata122,mydata131,mydata132,mydata141,mydata142,mydata151,
   mydata152,mydata161,mydata162,mydata171,mydata172,mydata181,mydata182,mydata191,mydata192,mydata201,mydata202,
   Add,mydata20,Interim1)






#   4. Clarify the type of GUOs and remove non-corporate MNEs (or move to the next nod) ####

##Remove Ultimate Owners that are Banks,Insurance Companies,PEF,HedgeFunds,VC,
##Mutual & Pension Funds,Research Institutes,Public Authorities,Individuals,Employees 

##Create dataset of shareholders with foreign subsidiaries
#Enter the data (same order as above:Company-BvDID,GUO - BvDID)
Banks1<-read_excel("Dataset/Type/Banks1.xlsx", sheet = "Results")
Banks2<-read_excel("Dataset/Type/Banks2.xlsx", sheet = "Results")
Banks3<-read_excel("Dataset/Type/Banks3.xlsx", sheet = "Results")
Insurance1<-read_excel("Dataset/Type/Insurance1.xlsx", sheet = "Results")
PEFs1<-read_excel("Dataset/Type/PEFs1.xlsx", sheet = "Results")
HFs1<-read_excel("Dataset/Type/HFs1.xlsx", sheet = "Results")
MPFs1<-read_excel("Dataset/Type/MPFs1.xlsx", sheet = "Results")
MPFs2<-read_excel("Dataset/Type/MPFs2.xlsx", sheet = "Results")
RIs1<-read_excel("Dataset/Type/RIs1.xlsx", sheet = "Results")
PAs1<-read_excel("Dataset/Type/PAs1.xlsx", sheet = "Results")
PAs2<-read_excel("Dataset/Type/PAs2.xlsx", sheet = "Results")
EMP1<-read_excel("Dataset/Type/EMP1.xlsx", sheet = "Results")

IND_files<-list.files(path="Dataset/Type/IND",pattern ='.xlsx', full.names = T)
IND <- sapply(IND_files, read_excel, sheet=2,simplify=FALSE) %>% 
   bind_rows(.id = "id")

Corporate_files<-list.files(path="Dataset/Type/Corporate",pattern ='.xlsx', full.names = T)
Corporate <- sapply(Corporate_files, read_excel, sheet=2,simplify=FALSE) %>% 
   bind_rows(.id = "id")

#Establish a "Type" Variable and match the data
Bank<-rbind(Banks1,Banks2)
Bank$Type<-"Bank"
Insurance<-Insurance1
Insurance$Type<-"Insurance"
PEF<-PEFs1
PEF$Type<-"PEF"
HF<-HFs1
HF$Type<-"HF"
MPF<-rbind(MPFs1,MPFs2)
MPF$Type<-"MPF"
RI<-RIs1
RI$Type<-"RI"
PA<-rbind(PAs1,PAs2)
PA$Type<-"PA"
EMP<-EMP1
EMP$Type<-"EMP"
IND[1]<-NULL
IND$Type<-"IND"
Corporate[1]<-NULL
Corporate$Type<-"Corporate"

Type<-rbind(Bank,Insurance,PEF,HF,MPF,RI,PA,IND,EMP,Corporate)

#Remove unnecessary data
rm(Banks1,Banks2,Banks3,Insurance1,PEFs1,HFs1,MPFs1,MPFs2,RIs1,PAs1,PAs2,Banks,Bank,Insurance,PEF,HF,MPF,RI,PA,IND,EMP,Corporate,
   Corporate_files,IND_files,EMP1)

##Restructure dataset
#Remove Company Name and Immediate Parent ID
Type[1:3] <- NULL
Type[2] <- NULL
names(Type)<-c("Own1","Type")

BU_Type<-Type

#Save the type file for later
names(BU_Type)<-c("BvDID","Type")
write.csv2(BU_Type, file = "files_created_code1/Type.csv",row.names = F)
names(BU_Type)<-c("Own1","Type")


#Extract GUO data for match
GUO<-Type[,1]
GUO2<-Type[,1]
mydata3<-cbind(GUO,GUO2)
mydata2<-unique(mydata3,)
names(mydata2)<-c("Own1","GUO2")

#Reduce size of Type data
mydata1<-unique(Type,)

#Remove unnecessary data
rm(GUO2,GUO,mydata3)

##Match the ownership data of both datasets to establish a type variabl
#Match Original Data with GUOs that are not corporate - BY Company Name
Medium<-left_join(mydata,mydata1,by="Own1",na_matches="never")

#Remove unnecessary data
rm(mydata1)

#Match Original Data with GUOs that are not corporate - BY GUO Name
Large<-left_join(Medium,mydata2,by="Own1",na_matches="never")

#Remove unnecessary data
rm(Interim,Medium,mydata2,Type)

#Define NAs as character
Large$GUO2<-as.character(Large$GUO2)
Large$Type<-as.character(Large$Type)

#Check for number of GUO matches
length(which(!is.na(Large$GUO2))) 
Large$Check<-ifelse(Large$GUO==Large$GUO2,1,NA)
length(which(!is.na(Large$Check))) 
Large$Check<-NULL

##Create the type variable and look for conflicts between both datasets
#Combine the matching cases to a column of GUOs that are not corporate
Large$Corporate<-case_when(is.na(Large$GUO2) & is.na(Large$Type) ~ 1,
                           Large$Type == "Corporate"~ 1,
                           Large$Type != "Corporate"~ 0)

#Identify GUOs have a subsidiary
Large$Double1<-ifelse(Large$Own1 == Large$Own1[c(NA,1:(nrow(Large)-1))] & Large$Type != Large$Type[c(NA,1:(nrow(Large)-1))], 1, 0)
Large$Double2<-ifelse(lead(Large$Double)==1,1,0)
Large$Double<-ifelse(Large$Double1==1 | Large$Double2==1,1,NA)
length(which(!is.na(Large$Double)))

#Check length of corporate companies
length(which(Large$Corporate==1)) 

#Subset GUOs that are defined as corporate and other simultaneously
Conflicts<-subset(Large,Large$Double==1)
Corporate<-subset(Conflicts,Conflicts$Type=="Corporate")
Clear<-subset(Large,is.na(Large$Double))
Structure<-bind_rows(Clear,Corporate)

#Remove unnecessary columns + data
length(which(!is.na(Structure$Own21)))
Structure[25:27]<-NULL
rm(Clear,Conflicts,Corporate)

#Make all character
cols.num <- c("Own1","Own2","Own3","Own4","Own5","Own6","Own7",
              "Own8","Own9","Own10","Own11","Own12","Own13","Own14",
              "Own15","Own16","Own17","Own18","Own19","Own20","Own21","Type","GUO2","Corporate")
Structure[cols.num] <- sapply(Structure[cols.num],as.character)
sapply(Structure, class)

#Final Dataset with all multinational enterprises + structures worldwide + Column indicating non-corporate GUOs
#View(Structure)





#   5. Simplify the dataset (Structure) for match with Shareholder and AI data ####

##Remove unnecessary data
#Remove Duplicates and NA (Should not be there)
Dup<-unique(Structure,)
length(which(is.na(Dup$Own1)))
Comp<-Dup[complete.cases(Dup[,1]), ]
length(which(is.na(Comp$Own1)))
Nas<-Comp[!is.na(Comp$Own1),]   
length(which(is.na(Nas$Own1)))

#Remove Companies that are not corporate and have only one subsidiary
Nas$Dup1<-duplicated(Nas$Own1) | duplicated(Nas$Own1, fromLast = TRUE)
Nas$Not<-ifelse(Nas$Type!="Corporate" | !is.na(Nas$Type),1,0)
Nas$Dup2<-if_else(Nas$Not==1 & is.na(Nas$Own3) & Nas$Dup1==FALSE,1,0)
Final<-Nas[Nas$Dup2!=1|is.na(Nas$Dup2), ]

#Remove unnecessary data
rm(Comp,Dup,Nas,Structure)
Final[25:27]<-NULL 
length(which(!is.na(Final$Own21))) #Remove Own13 if only NAs


##Organize dataset
#Make all variables characters
cols.num <- c("Own1","Own2","Own3","Own4","Own5","Own6","Own7",
              "Own8","Own9","Own10","Own11","Own12","Own13","Own14",
              "Own15","Own16","Own17","Own18","Own19","Own20","Own21","Type","GUO2","Corporate")
Final[cols.num] <- sapply(Final[cols.num],as.character)
sapply(Final, class)

#Check the amount of non-corporate - non-explained Owners
length(which(is.na(Final$Type)))
length(which(is.na(Final$Corporate)))
length(which(is.na(Final$Own1)))
length(which(is.na(Final$Own2)))

##Find the GUOs that are corporate
#Move down the ownership structure if Original GUO matches non-corporate column
Final$Match1<-ifelse(Final$Corporate==1,Final$Own1,NA)
length(which(!is.na(Final$Match1)))
Final$Match2<-ifelse(Final$Corporate!=1 & Final$Own1==Final$GUO2,Final$Own2,NA)
length(which(!is.na(Final$Match2)))
Final$Match3<-ifelse(Final$Corporate!=1 & Final$Own2==Final$GUO2,Final$Own3,NA)
length(which(!is.na(Final$Match3)))

#Check if the new GUOs are still not corporate
Type<-subset(BU_Type,BU_Type$Type!="Corporate")
names(Type)<-c("Match2","Type")
Final1<-left_join(Final,Type,by="Match2",na_matches="never")
length(which(!is.na(Final1$Type.y))) #107
rm(Final1)


#   6. Produce final data strings for the match with deals data ####

#Create all ownership levels in one column to later delete the subsidiaries
#backup<- Final
Subs1<-Final
Subs1[22:27]<-NULL

## Produce files with all companies in one and all of their subsidiaries in the other column
Subs101<-Subs1[,c(1,2)]
names(Subs101)<-c("Check2","Subs")
Subs101<-subset(Subs101,!is.na(Subs101$Subs))
Subs102<-Subs1[,c(1,3)]
names(Subs102)<-c("Check2","Subs")
Subs102<-subset(Subs102,!is.na(Subs102$Subs))
Subs103<-Subs1[,c(1,4)]
names(Subs103)<-c("Check2","Subs")
Subs103<-subset(Subs103,!is.na(Subs103$Subs))
Subs104<-Subs1[,c(1,5)]
names(Subs104)<-c("Check2","Subs")
Subs104<-subset(Subs104,!is.na(Subs104$Subs))
Subs105<-Subs1[,c(1,6)]
names(Subs105)<-c("Check2","Subs")
Subs105<-subset(Subs105,!is.na(Subs105$Subs))
Subs106<-Subs1[,c(1,7)]
names(Subs106)<-c("Check2","Subs")
Subs106<-subset(Subs106,!is.na(Subs106$Subs))
Subs107<-Subs1[,c(1,8)]
names(Subs107)<-c("Check2","Subs")
Subs107<-subset(Subs107,!is.na(Subs107$Subs))
Subs108<-Subs1[,c(1,9)]
names(Subs108)<-c("Check2","Subs")
Subs108<-subset(Subs108,!is.na(Subs108$Subs))
Subs109<-Subs1[,c(1,10)]
names(Subs109)<-c("Check2","Subs")
Subs109<-subset(Subs109,!is.na(Subs109$Subs))
Subs110<-Subs1[,c(1,11)]
names(Subs110)<-c("Check2","Subs")
Subs110<-subset(Subs110,!is.na(Subs110$Subs))
Subs111<-Subs1[,c(1,12)]
names(Subs111)<-c("Check2","Subs")
Subs111<-subset(Subs111,!is.na(Subs111$Subs))
Subs112<-Subs1[,c(1,13)]
names(Subs112)<-c("Check2","Subs")
Subs112<-subset(Subs112,!is.na(Subs112$Subs))
Subs113<-Subs1[,c(1,14)]
names(Subs113)<-c("Check2","Subs")
Subs113<-subset(Subs113,!is.na(Subs113$Subs))
Subs114<-Subs1[,c(1,15)]
names(Subs114)<-c("Check2","Subs")
Subs114<-subset(Subs114,!is.na(Subs114$Subs))
Subs115<-Subs1[,c(1,16)]
names(Subs115)<-c("Check2","Subs")
Subs115<-subset(Subs115,!is.na(Subs115$Subs))
Subs116<-Subs1[,c(1,17)]
names(Subs116)<-c("Check2","Subs")
Subs116<-subset(Subs116,!is.na(Subs116$Subs))
Subs117<-Subs1[,c(1,18)]
names(Subs117)<-c("Check2","Subs")
Subs117<-subset(Subs117,!is.na(Subs117$Subs))
Subs118<-Subs1[,c(1,19)]
names(Subs118)<-c("Check2","Subs")
Subs118<-subset(Subs118,!is.na(Subs118$Subs))
Subs119<-Subs1[,c(1,20)]
names(Subs119)<-c("Check2","Subs")
Subs119<-subset(Subs119,!is.na(Subs119$Subs))
Subs120<-Subs1[,c(1,21)]
names(Subs120)<-c("Check2","Subs")
Subs120<-subset(Subs120,!is.na(Subs120$Subs))
Subs121<-Subs1[,c(2,3)]
names(Subs121)<-c("Check2","Subs")
Subs121<-subset(Subs121,!is.na(Subs121$Subs))
Subs122<-Subs1[,c(2,4)]
names(Subs122)<-c("Check2","Subs")
Subs122<-subset(Subs122,!is.na(Subs122$Subs))
Subs123<-Subs1[,c(2,5)]
names(Subs123)<-c("Check2","Subs")
Subs123<-subset(Subs123,!is.na(Subs123$Subs))
Subs124<-Subs1[,c(2,6)]
names(Subs124)<-c("Check2","Subs")
Subs124<-subset(Subs124,!is.na(Subs124$Subs))
Subs125<-Subs1[,c(2,7)]
names(Subs125)<-c("Check2","Subs")
Subs125<-subset(Subs125,!is.na(Subs125$Subs))
Subs126<-Subs1[,c(2,8)]
names(Subs126)<-c("Check2","Subs")
Subs126<-subset(Subs126,!is.na(Subs126$Subs))
Subs127<-Subs1[,c(2,9)]
names(Subs127)<-c("Check2","Subs")
Subs127<-subset(Subs127,!is.na(Subs127$Subs))
Subs128<-Subs1[,c(2,10)]
names(Subs128)<-c("Check2","Subs")
Subs128<-subset(Subs128,!is.na(Subs128$Subs))
Subs129<-Subs1[,c(2,11)]
names(Subs129)<-c("Check2","Subs")
Subs129<-subset(Subs129,!is.na(Subs129$Subs))
Subs130<-Subs1[,c(2,12)]
names(Subs130)<-c("Check2","Subs")
Subs130<-subset(Subs130,!is.na(Subs130$Subs))
Subs131<-Subs1[,c(2,13)]
names(Subs131)<-c("Check2","Subs")
Subs131<-subset(Subs131,!is.na(Subs131$Subs))
Subs132<-Subs1[,c(2,14)]
names(Subs132)<-c("Check2","Subs")
Subs132<-subset(Subs132,!is.na(Subs132$Subs))
Subs133<-Subs1[,c(2,15)]
names(Subs133)<-c("Check2","Subs")
Subs133<-subset(Subs133,!is.na(Subs133$Subs))
Subs134<-Subs1[,c(2,16)]
names(Subs134)<-c("Check2","Subs")
Subs134<-subset(Subs134,!is.na(Subs134$Subs))
Subs135<-Subs1[,c(2,17)]
names(Subs135)<-c("Check2","Subs")
Subs135<-subset(Subs135,!is.na(Subs135$Subs))
Subs136<-Subs1[,c(2,18)]
names(Subs136)<-c("Check2","Subs")
Subs136<-subset(Subs136,!is.na(Subs136$Subs))
Subs137<-Subs1[,c(2,19)]
names(Subs137)<-c("Check2","Subs")
Subs137<-subset(Subs137,!is.na(Subs137$Subs))
Subs138<-Subs1[,c(2,20)]
names(Subs138)<-c("Check2","Subs")
Subs138<-subset(Subs138,!is.na(Subs138$Subs))
Subs139<-Subs1[,c(2,21)]
names(Subs139)<-c("Check2","Subs")
Subs139<-subset(Subs139,!is.na(Subs139$Subs))
Subs140<-Subs1[,c(3,4)]
names(Subs140)<-c("Check2","Subs")
Subs140<-subset(Subs140,!is.na(Subs140$Subs))
Subs141<-Subs1[,c(3,5)]
names(Subs141)<-c("Check2","Subs")
Subs141<-subset(Subs141,!is.na(Subs141$Subs))
Subs142<-Subs1[,c(3,6)]
names(Subs142)<-c("Check2","Subs")
Subs142<-subset(Subs142,!is.na(Subs142$Subs))
Subs143<-Subs1[,c(3,7)]
names(Subs143)<-c("Check2","Subs")
Subs143<-subset(Subs143,!is.na(Subs143$Subs))
Subs144<-Subs1[,c(3,8)]
names(Subs144)<-c("Check2","Subs")
Subs144<-subset(Subs144,!is.na(Subs144$Subs))
Subs145<-Subs1[,c(3,9)]
names(Subs145)<-c("Check2","Subs")
Subs145<-subset(Subs145,!is.na(Subs145$Subs))
Subs146<-Subs1[,c(3,10)]
names(Subs146)<-c("Check2","Subs")
Subs146<-subset(Subs146,!is.na(Subs146$Subs))
Subs147<-Subs1[,c(3,11)]
names(Subs147)<-c("Check2","Subs")
Subs147<-subset(Subs147,!is.na(Subs147$Subs))
Subs148<-Subs1[,c(3,12)]
names(Subs148)<-c("Check2","Subs")
Subs148<-subset(Subs148,!is.na(Subs148$Subs))
Subs149<-Subs1[,c(3,13)]
names(Subs149)<-c("Check2","Subs")
Subs149<-subset(Subs149,!is.na(Subs149$Subs))
Subs150<-Subs1[,c(3,14)]
names(Subs150)<-c("Check2","Subs")
Subs150<-subset(Subs150,!is.na(Subs150$Subs))
Subs151<-Subs1[,c(3,15)]
names(Subs151)<-c("Check2","Subs")
Subs151<-subset(Subs151,!is.na(Subs151$Subs))
Subs152<-Subs1[,c(3,16)]
names(Subs152)<-c("Check2","Subs")
Subs152<-subset(Subs152,!is.na(Subs152$Subs))
Subs153<-Subs1[,c(3,17)]
names(Subs153)<-c("Check2","Subs")
Subs153<-subset(Subs153,!is.na(Subs153$Subs))
Subs154<-Subs1[,c(3,18)]
names(Subs154)<-c("Check2","Subs")
Subs154<-subset(Subs154,!is.na(Subs154$Subs))
Subs155<-Subs1[,c(3,19)]
names(Subs155)<-c("Check2","Subs")
Subs155<-subset(Subs155,!is.na(Subs155$Subs))
Subs156<-Subs1[,c(3,20)]
names(Subs156)<-c("Check2","Subs")
Subs156<-subset(Subs156,!is.na(Subs156$Subs))
Subs157<-Subs1[,c(3,21)]
names(Subs157)<-c("Check2","Subs")
Subs157<-subset(Subs157,!is.na(Subs157$Subs))
Subs158<-Subs1[,c(4,5)]
names(Subs158)<-c("Check2","Subs")
Subs158<-subset(Subs158,!is.na(Subs158$Subs))
Subs159<-Subs1[,c(4,6)]
names(Subs159)<-c("Check2","Subs")
Subs159<-subset(Subs159,!is.na(Subs159$Subs))
Subs160<-Subs1[,c(4,7)]
names(Subs160)<-c("Check2","Subs")
Subs160<-subset(Subs160,!is.na(Subs160$Subs))
Subs161<-Subs1[,c(4,8)]
names(Subs161)<-c("Check2","Subs")
Subs161<-subset(Subs161,!is.na(Subs161$Subs))
Subs162<-Subs1[,c(4,9)]
names(Subs162)<-c("Check2","Subs")
Subs162<-subset(Subs162,!is.na(Subs162$Subs))
Subs163<-Subs1[,c(4,10)]
names(Subs163)<-c("Check2","Subs")
Subs163<-subset(Subs163,!is.na(Subs163$Subs))
Subs164<-Subs1[,c(4,11)]
names(Subs164)<-c("Check2","Subs")
Subs164<-subset(Subs164,!is.na(Subs164$Subs))
Subs165<-Subs1[,c(4,12)]
names(Subs165)<-c("Check2","Subs")
Subs165<-subset(Subs165,!is.na(Subs165$Subs))
Subs166<-Subs1[,c(4,13)]
names(Subs166)<-c("Check2","Subs")
Subs166<-subset(Subs166,!is.na(Subs166$Subs))
Subs167<-Subs1[,c(4,14)]
names(Subs167)<-c("Check2","Subs")
Subs167<-subset(Subs167,!is.na(Subs167$Subs))
Subs168<-Subs1[,c(4,15)]
names(Subs168)<-c("Check2","Subs")
Subs168<-subset(Subs168,!is.na(Subs168$Subs))
Subs169<-Subs1[,c(4,16)]
names(Subs169)<-c("Check2","Subs")
Subs169<-subset(Subs169,!is.na(Subs169$Subs))
Subs170<-Subs1[,c(4,17)]
names(Subs170)<-c("Check2","Subs")
Subs170<-subset(Subs170,!is.na(Subs170$Subs))
Subs171<-Subs1[,c(4,18)]
names(Subs171)<-c("Check2","Subs")
Subs171<-subset(Subs171,!is.na(Subs171$Subs))
Subs172<-Subs1[,c(4,19)]
names(Subs172)<-c("Check2","Subs")
Subs172<-subset(Subs172,!is.na(Subs172$Subs))
Subs173<-Subs1[,c(4,20)]
names(Subs173)<-c("Check2","Subs")
Subs173<-subset(Subs173,!is.na(Subs173$Subs))
Subs174<-Subs1[,c(4,21)]
names(Subs174)<-c("Check2","Subs")
Subs174<-subset(Subs174,!is.na(Subs174$Subs))
Subs175<-Subs1[,c(5,6)]
names(Subs175)<-c("Check2","Subs")
Subs175<-subset(Subs175,!is.na(Subs175$Subs))
Subs176<-Subs1[,c(5,7)]
names(Subs176)<-c("Check2","Subs")
Subs176<-subset(Subs176,!is.na(Subs176$Subs))
Subs177<-Subs1[,c(5,8)]
names(Subs177)<-c("Check2","Subs")
Subs177<-subset(Subs177,!is.na(Subs177$Subs))
Subs178<-Subs1[,c(5,9)]
names(Subs178)<-c("Check2","Subs")
Subs178<-subset(Subs178,!is.na(Subs178$Subs))
Subs179<-Subs1[,c(5,10)]
names(Subs179)<-c("Check2","Subs")
Subs179<-subset(Subs179,!is.na(Subs179$Subs))
Subs180<-Subs1[,c(5,11)]
names(Subs180)<-c("Check2","Subs")
Subs180<-subset(Subs180,!is.na(Subs180$Subs))
Subs181<-Subs1[,c(5,12)]
names(Subs181)<-c("Check2","Subs")
Subs181<-subset(Subs181,!is.na(Subs181$Subs))
Subs182<-Subs1[,c(5,13)]
names(Subs182)<-c("Check2","Subs")
Subs182<-subset(Subs182,!is.na(Subs182$Subs))
Subs183<-Subs1[,c(5,14)]
names(Subs183)<-c("Check2","Subs")
Subs183<-subset(Subs183,!is.na(Subs183$Subs))
Subs184<-Subs1[,c(5,15)]
names(Subs184)<-c("Check2","Subs")
Subs184<-subset(Subs184,!is.na(Subs184$Subs))
Subs185<-Subs1[,c(5,16)]
names(Subs185)<-c("Check2","Subs")
Subs185<-subset(Subs185,!is.na(Subs185$Subs))
Subs186<-Subs1[,c(5,17)]
names(Subs186)<-c("Check2","Subs")
Subs186<-subset(Subs186,!is.na(Subs186$Subs))
Subs187<-Subs1[,c(5,18)]
names(Subs187)<-c("Check2","Subs")
Subs187<-subset(Subs187,!is.na(Subs187$Subs))
Subs188<-Subs1[,c(5,19)]
names(Subs188)<-c("Check2","Subs")
Subs188<-subset(Subs188,!is.na(Subs188$Subs))
Subs189<-Subs1[,c(5,20)]
names(Subs189)<-c("Check2","Subs")
Subs189<-subset(Subs189,!is.na(Subs189$Subs))
Subs190<-Subs1[,c(5,21)]
names(Subs190)<-c("Check2","Subs")
Subs190<-subset(Subs190,!is.na(Subs190$Subs))
Subs191<-Subs1[,c(6,7)]
names(Subs191)<-c("Check2","Subs")
Subs191<-subset(Subs191,!is.na(Subs191$Subs))
Subs192<-Subs1[,c(6,8)]
names(Subs192)<-c("Check2","Subs")
Subs192<-subset(Subs192,!is.na(Subs192$Subs))
Subs193<-Subs1[,c(6,9)]
names(Subs193)<-c("Check2","Subs")
Subs193<-subset(Subs193,!is.na(Subs193$Subs))
Subs194<-Subs1[,c(6,10)]
names(Subs194)<-c("Check2","Subs")
Subs194<-subset(Subs194,!is.na(Subs194$Subs))
Subs195<-Subs1[,c(6,11)]
names(Subs195)<-c("Check2","Subs")
Subs195<-subset(Subs195,!is.na(Subs195$Subs))
Subs196<-Subs1[,c(6,12)]
names(Subs196)<-c("Check2","Subs")
Subs196<-subset(Subs196,!is.na(Subs196$Subs))
Subs197<-Subs1[,c(6,13)]
names(Subs197)<-c("Check2","Subs")
Subs197<-subset(Subs197,!is.na(Subs197$Subs))
Subs198<-Subs1[,c(6,14)]
names(Subs198)<-c("Check2","Subs")
Subs198<-subset(Subs198,!is.na(Subs198$Subs))
Subs199<-Subs1[,c(6,15)]
names(Subs199)<-c("Check2","Subs")
Subs199<-subset(Subs199,!is.na(Subs199$Subs))
Subs200<-Subs1[,c(6,16)]
names(Subs200)<-c("Check2","Subs")
Subs200<-subset(Subs200,!is.na(Subs200$Subs))
Subs201<-Subs1[,c(6,17)]
names(Subs201)<-c("Check2","Subs")
Subs201<-subset(Subs201,!is.na(Subs201$Subs))
Subs202<-Subs1[,c(6,18)]
names(Subs202)<-c("Check2","Subs")
Subs202<-subset(Subs202,!is.na(Subs202$Subs))
Subs203<-Subs1[,c(6,19)]
names(Subs203)<-c("Check2","Subs")
Subs203<-subset(Subs203,!is.na(Subs203$Subs))
Subs204<-Subs1[,c(6,20)]
names(Subs204)<-c("Check2","Subs")
Subs204<-subset(Subs204,!is.na(Subs204$Subs))
Subs205<-Subs1[,c(6,21)]
names(Subs205)<-c("Check2","Subs")
Subs205<-subset(Subs205,!is.na(Subs205$Subs))
Subs206<-Subs1[,c(7,8)]
names(Subs206)<-c("Check2","Subs")
Subs206<-subset(Subs206,!is.na(Subs206$Subs))
Subs207<-Subs1[,c(7,9)]
names(Subs207)<-c("Check2","Subs")
Subs207<-subset(Subs207,!is.na(Subs207$Subs))
Subs208<-Subs1[,c(7,10)]
names(Subs208)<-c("Check2","Subs")
Subs208<-subset(Subs208,!is.na(Subs208$Subs))
Subs209<-Subs1[,c(7,11)]
names(Subs209)<-c("Check2","Subs")
Subs209<-subset(Subs209,!is.na(Subs209$Subs))
Subs210<-Subs1[,c(7,12)]
names(Subs210)<-c("Check2","Subs")
Subs210<-subset(Subs210,!is.na(Subs210$Subs))
Subs211<-Subs1[,c(7,13)]
names(Subs211)<-c("Check2","Subs")
Subs211<-subset(Subs211,!is.na(Subs211$Subs))
Subs212<-Subs1[,c(7,14)]
names(Subs212)<-c("Check2","Subs")
Subs212<-subset(Subs212,!is.na(Subs212$Subs))
Subs213<-Subs1[,c(7,15)]
names(Subs213)<-c("Check2","Subs")
Subs213<-subset(Subs213,!is.na(Subs213$Subs))
Subs214<-Subs1[,c(7,16)]
names(Subs214)<-c("Check2","Subs")
Subs214<-subset(Subs214,!is.na(Subs214$Subs))
Subs215<-Subs1[,c(7,17)]
names(Subs215)<-c("Check2","Subs")
Subs215<-subset(Subs215,!is.na(Subs215$Subs))
Subs216<-Subs1[,c(7,18)]
names(Subs216)<-c("Check2","Subs")
Subs216<-subset(Subs216,!is.na(Subs216$Subs))
Subs217<-Subs1[,c(7,19)]
names(Subs217)<-c("Check2","Subs")
Subs217<-subset(Subs217,!is.na(Subs217$Subs))
Subs218<-Subs1[,c(7,20)]
names(Subs218)<-c("Check2","Subs")
Subs218<-subset(Subs218,!is.na(Subs218$Subs))
Subs219<-Subs1[,c(7,21)]
names(Subs219)<-c("Check2","Subs")
Subs219<-subset(Subs219,!is.na(Subs219$Subs))
Subs220<-Subs1[,c(8,9)]
names(Subs220)<-c("Check2","Subs")
Subs220<-subset(Subs220,!is.na(Subs220$Subs))
Subs221<-Subs1[,c(8,10)]
names(Subs221)<-c("Check2","Subs")
Subs221<-subset(Subs221,!is.na(Subs221$Subs))
Subs222<-Subs1[,c(8,11)]
names(Subs222)<-c("Check2","Subs")
Subs222<-subset(Subs222,!is.na(Subs222$Subs))
Subs223<-Subs1[,c(8,12)]
names(Subs223)<-c("Check2","Subs")
Subs223<-subset(Subs223,!is.na(Subs223$Subs))
Subs224<-Subs1[,c(8,13)]
names(Subs224)<-c("Check2","Subs")
Subs224<-subset(Subs224,!is.na(Subs224$Subs))
Subs225<-Subs1[,c(8,14)]
names(Subs225)<-c("Check2","Subs")
Subs225<-subset(Subs225,!is.na(Subs225$Subs))
Subs226<-Subs1[,c(8,15)]
names(Subs226)<-c("Check2","Subs")
Subs226<-subset(Subs226,!is.na(Subs226$Subs))
Subs227<-Subs1[,c(8,16)]
names(Subs227)<-c("Check2","Subs")
Subs227<-subset(Subs227,!is.na(Subs227$Subs))
Subs228<-Subs1[,c(8,17)]
names(Subs228)<-c("Check2","Subs")
Subs228<-subset(Subs228,!is.na(Subs228$Subs))
Subs229<-Subs1[,c(8,18)]
names(Subs229)<-c("Check2","Subs")
Subs229<-subset(Subs229,!is.na(Subs229$Subs))
Subs230<-Subs1[,c(8,19)]
names(Subs230)<-c("Check2","Subs")
Subs230<-subset(Subs230,!is.na(Subs230$Subs))
Subs231<-Subs1[,c(8,20)]
names(Subs231)<-c("Check2","Subs")
Subs231<-subset(Subs231,!is.na(Subs231$Subs))
Subs232<-Subs1[,c(8,21)]
names(Subs232)<-c("Check2","Subs")
Subs232<-subset(Subs232,!is.na(Subs232$Subs))
Subs233<-Subs1[,c(9,10)]
names(Subs233)<-c("Check2","Subs")
Subs233<-subset(Subs233,!is.na(Subs233$Subs))
Subs234<-Subs1[,c(9,11)]
names(Subs234)<-c("Check2","Subs")
Subs234<-subset(Subs234,!is.na(Subs234$Subs))
Subs235<-Subs1[,c(9,12)]
names(Subs235)<-c("Check2","Subs")
Subs235<-subset(Subs235,!is.na(Subs235$Subs))
Subs236<-Subs1[,c(9,13)]
names(Subs236)<-c("Check2","Subs")
Subs236<-subset(Subs236,!is.na(Subs236$Subs))
Subs237<-Subs1[,c(9,14)]
names(Subs237)<-c("Check2","Subs")
Subs237<-subset(Subs237,!is.na(Subs237$Subs))
Subs238<-Subs1[,c(9,15)]
names(Subs238)<-c("Check2","Subs")
Subs238<-subset(Subs238,!is.na(Subs238$Subs))
Subs239<-Subs1[,c(9,16)]
names(Subs239)<-c("Check2","Subs")
Subs239<-subset(Subs239,!is.na(Subs239$Subs))
Subs240<-Subs1[,c(9,17)]
names(Subs240)<-c("Check2","Subs")
Subs240<-subset(Subs240,!is.na(Subs240$Subs))
Subs241<-Subs1[,c(9,18)]
names(Subs241)<-c("Check2","Subs")
Subs241<-subset(Subs241,!is.na(Subs241$Subs))
Subs242<-Subs1[,c(9,19)]
names(Subs242)<-c("Check2","Subs")
Subs242<-subset(Subs242,!is.na(Subs242$Subs))
Subs243<-Subs1[,c(9,20)]
names(Subs243)<-c("Check2","Subs")
Subs243<-subset(Subs243,!is.na(Subs243$Subs))
Subs244<-Subs1[,c(9,21)]
names(Subs244)<-c("Check2","Subs")
Subs244<-subset(Subs244,!is.na(Subs244$Subs))
Subs245<-Subs1[,c(10,11)]
names(Subs245)<-c("Check2","Subs")
Subs245<-subset(Subs245,!is.na(Subs245$Subs))
Subs246<-Subs1[,c(10,12)]
names(Subs246)<-c("Check2","Subs")
Subs246<-subset(Subs246,!is.na(Subs246$Subs))
Subs247<-Subs1[,c(10,13)]
names(Subs247)<-c("Check2","Subs")
Subs247<-subset(Subs247,!is.na(Subs247$Subs))
Subs248<-Subs1[,c(10,14)]
names(Subs248)<-c("Check2","Subs")
Subs248<-subset(Subs248,!is.na(Subs248$Subs))
Subs249<-Subs1[,c(10,15)]
names(Subs249)<-c("Check2","Subs")
Subs249<-subset(Subs249,!is.na(Subs249$Subs))
Subs250<-Subs1[,c(10,16)]
names(Subs250)<-c("Check2","Subs")
Subs250<-subset(Subs250,!is.na(Subs250$Subs))
Subs251<-Subs1[,c(10,17)]
names(Subs251)<-c("Check2","Subs")
Subs251<-subset(Subs251,!is.na(Subs251$Subs))
Subs252<-Subs1[,c(10,18)]
names(Subs252)<-c("Check2","Subs")
Subs252<-subset(Subs252,!is.na(Subs252$Subs))
Subs253<-Subs1[,c(10,19)]
names(Subs253)<-c("Check2","Subs")
Subs253<-subset(Subs253,!is.na(Subs253$Subs))
Subs254<-Subs1[,c(10,20)]
names(Subs254)<-c("Check2","Subs")
Subs254<-subset(Subs254,!is.na(Subs254$Subs))
Subs255<-Subs1[,c(10,21)]
names(Subs255)<-c("Check2","Subs")
Subs255<-subset(Subs255,!is.na(Subs255$Subs))
Subs256<-Subs1[,c(11,12)]
names(Subs256)<-c("Check2","Subs")
Subs256<-subset(Subs256,!is.na(Subs256$Subs))
Subs257<-Subs1[,c(11,13)]
names(Subs257)<-c("Check2","Subs")
Subs257<-subset(Subs257,!is.na(Subs257$Subs))
Subs258<-Subs1[,c(11,14)]
names(Subs258)<-c("Check2","Subs")
Subs258<-subset(Subs258,!is.na(Subs258$Subs))
Subs259<-Subs1[,c(11,15)]
names(Subs259)<-c("Check2","Subs")
Subs259<-subset(Subs259,!is.na(Subs259$Subs))
Subs260<-Subs1[,c(11,16)]
names(Subs260)<-c("Check2","Subs")
Subs260<-subset(Subs260,!is.na(Subs260$Subs))
Subs261<-Subs1[,c(11,17)]
names(Subs261)<-c("Check2","Subs")
Subs261<-subset(Subs261,!is.na(Subs261$Subs))
Subs262<-Subs1[,c(11,18)]
names(Subs262)<-c("Check2","Subs")
Subs262<-subset(Subs262,!is.na(Subs262$Subs))
Subs263<-Subs1[,c(11,19)]
names(Subs263)<-c("Check2","Subs")
Subs263<-subset(Subs263,!is.na(Subs263$Subs))
Subs264<-Subs1[,c(11,20)]
names(Subs264)<-c("Check2","Subs")
Subs264<-subset(Subs264,!is.na(Subs264$Subs))
Subs265<-Subs1[,c(11,21)]
names(Subs265)<-c("Check2","Subs")
Subs265<-subset(Subs265,!is.na(Subs265$Subs))
Subs266<-Subs1[,c(12,13)]
names(Subs266)<-c("Check2","Subs")
Subs266<-subset(Subs266,!is.na(Subs266$Subs))
Subs267<-Subs1[,c(12,14)]
names(Subs267)<-c("Check2","Subs")
Subs267<-subset(Subs267,!is.na(Subs267$Subs))
Subs268<-Subs1[,c(12,15)]
names(Subs268)<-c("Check2","Subs")
Subs268<-subset(Subs268,!is.na(Subs268$Subs))
Subs269<-Subs1[,c(12,16)]
names(Subs269)<-c("Check2","Subs")
Subs269<-subset(Subs269,!is.na(Subs269$Subs))
Subs270<-Subs1[,c(12,17)]
names(Subs270)<-c("Check2","Subs")
Subs270<-subset(Subs270,!is.na(Subs270$Subs))
Subs271<-Subs1[,c(12,18)]
names(Subs271)<-c("Check2","Subs")
Subs271<-subset(Subs271,!is.na(Subs271$Subs))
Subs272<-Subs1[,c(12,19)]
names(Subs272)<-c("Check2","Subs")
Subs272<-subset(Subs272,!is.na(Subs272$Subs))
Subs273<-Subs1[,c(12,20)]
names(Subs273)<-c("Check2","Subs")
Subs273<-subset(Subs273,!is.na(Subs273$Subs))
Subs274<-Subs1[,c(12,21)]
names(Subs274)<-c("Check2","Subs")
Subs274<-subset(Subs274,!is.na(Subs274$Subs))
Subs275<-Subs1[,c(13,14)]
names(Subs275)<-c("Check2","Subs")
Subs275<-subset(Subs275,!is.na(Subs275$Subs))
Subs276<-Subs1[,c(13,15)]
names(Subs276)<-c("Check2","Subs")
Subs276<-subset(Subs276,!is.na(Subs276$Subs))
Subs277<-Subs1[,c(13,16)]
names(Subs277)<-c("Check2","Subs")
Subs277<-subset(Subs277,!is.na(Subs277$Subs))
Subs278<-Subs1[,c(13,17)]
names(Subs278)<-c("Check2","Subs")
Subs278<-subset(Subs278,!is.na(Subs278$Subs))
Subs279<-Subs1[,c(13,18)]
names(Subs279)<-c("Check2","Subs")
Subs279<-subset(Subs279,!is.na(Subs279$Subs))
Subs280<-Subs1[,c(13,19)]
names(Subs280)<-c("Check2","Subs")
Subs280<-subset(Subs280,!is.na(Subs280$Subs))
Subs281<-Subs1[,c(13,20)]
names(Subs281)<-c("Check2","Subs")
Subs281<-subset(Subs281,!is.na(Subs281$Subs))
Subs282<-Subs1[,c(13,21)]
names(Subs282)<-c("Check2","Subs")
Subs282<-subset(Subs282,!is.na(Subs282$Subs))
Subs283<-Subs1[,c(14,15)]
names(Subs283)<-c("Check2","Subs")
Subs283<-subset(Subs283,!is.na(Subs283$Subs))
Subs284<-Subs1[,c(14,16)]
names(Subs284)<-c("Check2","Subs")
Subs284<-subset(Subs284,!is.na(Subs284$Subs))
Subs285<-Subs1[,c(14,17)]
names(Subs285)<-c("Check2","Subs")
Subs285<-subset(Subs285,!is.na(Subs285$Subs))
Subs286<-Subs1[,c(14,18)]
names(Subs286)<-c("Check2","Subs")
Subs286<-subset(Subs286,!is.na(Subs286$Subs))
Subs287<-Subs1[,c(14,19)]
names(Subs287)<-c("Check2","Subs")
Subs287<-subset(Subs287,!is.na(Subs287$Subs))
Subs288<-Subs1[,c(14,20)]
names(Subs288)<-c("Check2","Subs")
Subs288<-subset(Subs288,!is.na(Subs288$Subs))
Subs289<-Subs1[,c(14,21)]
names(Subs289)<-c("Check2","Subs")
Subs289<-subset(Subs289,!is.na(Subs289$Subs))
Subs290<-Subs1[,c(15,16)]
names(Subs290)<-c("Check2","Subs")
Subs290<-subset(Subs290,!is.na(Subs290$Subs))
Subs291<-Subs1[,c(15,17)]
names(Subs291)<-c("Check2","Subs")
Subs291<-subset(Subs291,!is.na(Subs291$Subs))
Subs292<-Subs1[,c(15,18)]
names(Subs292)<-c("Check2","Subs")
Subs292<-subset(Subs292,!is.na(Subs292$Subs))
Subs293<-Subs1[,c(15,19)]
names(Subs293)<-c("Check2","Subs")
Subs293<-subset(Subs293,!is.na(Subs293$Subs))
Subs294<-Subs1[,c(15,20)]
names(Subs294)<-c("Check2","Subs")
Subs294<-subset(Subs294,!is.na(Subs294$Subs))
Subs295<-Subs1[,c(15,21)]
names(Subs295)<-c("Check2","Subs")
Subs295<-subset(Subs295,!is.na(Subs295$Subs))
Subs296<-Subs1[,c(16,17)]
names(Subs296)<-c("Check2","Subs")
Subs296<-subset(Subs296,!is.na(Subs296$Subs))
Subs297<-Subs1[,c(16,18)]
names(Subs297)<-c("Check2","Subs")
Subs297<-subset(Subs297,!is.na(Subs297$Subs))
Subs298<-Subs1[,c(16,19)]
names(Subs298)<-c("Check2","Subs")
Subs298<-subset(Subs298,!is.na(Subs298$Subs))
Subs299<-Subs1[,c(16,20)]
names(Subs299)<-c("Check2","Subs")
Subs299<-subset(Subs299,!is.na(Subs299$Subs))
Subs300<-Subs1[,c(16,21)]
names(Subs300)<-c("Check2","Subs")
Subs300<-subset(Subs300,!is.na(Subs300$Subs))
Subs301<-Subs1[,c(17,18)]
names(Subs301)<-c("Check2","Subs")
Subs301<-subset(Subs301,!is.na(Subs301$Subs))
Subs302<-Subs1[,c(17,19)]
names(Subs302)<-c("Check2","Subs")
Subs302<-subset(Subs302,!is.na(Subs302$Subs))
Subs303<-Subs1[,c(17,20)]
names(Subs303)<-c("Check2","Subs")
Subs303<-subset(Subs303,!is.na(Subs303$Subs))
Subs304<-Subs1[,c(17,21)]
names(Subs304)<-c("Check2","Subs")
Subs304<-subset(Subs304,!is.na(Subs304$Subs))
Subs305<-Subs1[,c(18,19)]
names(Subs305)<-c("Check2","Subs")
Subs305<-subset(Subs305,!is.na(Subs305$Subs))
Subs306<-Subs1[,c(18,20)]
names(Subs306)<-c("Check2","Subs")
Subs306<-subset(Subs306,!is.na(Subs306$Subs))
Subs307<-Subs1[,c(18,21)]
names(Subs307)<-c("Check2","Subs")
Subs307<-subset(Subs307,!is.na(Subs307$Subs))
Subs308<-Subs1[,c(19,20)]
names(Subs308)<-c("Check2","Subs")
Subs308<-subset(Subs308,!is.na(Subs308$Subs))
Subs309<-Subs1[,c(19,21)]
names(Subs309)<-c("Check2","Subs")
Subs309<-subset(Subs309,!is.na(Subs309$Subs))
Subs310<-Subs1[,c(20,21)]
names(Subs310)<-c("Check2","Subs")
Subs310<-subset(Subs310,!is.na(Subs310$Subs))

Subs<-rbind(Subs101,Subs102,Subs103,Subs104,Subs105,Subs106,Subs107,Subs108,Subs109,Subs110,Subs111,Subs112,
            Subs113,Subs114,Subs115,Subs116,Subs117,Subs118,Subs119,Subs120,Subs121,Subs122,Subs123,Subs124,
            Subs125,Subs126,Subs127,Subs128,Subs129,Subs130,Subs131,Subs132,Subs133,Subs134,Subs135,Subs136,
            Subs137,Subs138,Subs139,Subs140,Subs141,Subs142,Subs143,Subs144,Subs145,Subs146,Subs147,Subs148,
            Subs149,Subs150,Subs151,Subs152,Subs153,Subs154,Subs155,Subs156,Subs157,Subs158,Subs159,Subs160,
            Subs161,Subs162,Subs163,Subs164,Subs165,Subs166,Subs167,Subs168,Subs169,Subs170,Subs171,Subs172,
            Subs173,Subs174,Subs175,Subs176,Subs177,Subs178,Subs179,Subs180,Subs181,Subs182,Subs183,Subs184,
            Subs185,Subs186,Subs187,Subs188,Subs189,Subs190,Subs191,Subs192,Subs193,Subs194,Subs195,Subs196,
            Subs197,Subs198,Subs199,Subs200,Subs201,Subs202,Subs203,Subs204,Subs205,Subs206,Subs207,Subs208,
            Subs209,Subs210,Subs211,Subs212,Subs213,Subs214,Subs215,Subs216,Subs217,Subs218,Subs219,Subs220,
            Subs221,Subs222,Subs223,Subs224,Subs225,Subs226,Subs227,Subs228,Subs229,Subs230,Subs231,Subs232,
            Subs233,Subs234,Subs235,Subs236,Subs237,Subs238,Subs239,Subs240,Subs241,Subs242,Subs243,Subs244,
            Subs245,Subs246,Subs247,Subs248,Subs249,Subs250,Subs251,Subs252,Subs253,Subs254,Subs255,Subs256,
            Subs257,Subs258,Subs259,Subs260,Subs261,Subs262,Subs263,Subs264,Subs265,Subs266,Subs267,Subs268,
            Subs269,Subs270,Subs271,Subs272,Subs273,Subs274,Subs275,Subs276,Subs277,Subs278,Subs279,Subs280,
            Subs281,Subs282,Subs283,Subs284,Subs285,Subs286,Subs287,Subs288,Subs289,Subs290,Subs291,Subs292,
            Subs293,Subs294,Subs295,Subs296,Subs297,Subs298,Subs299,Subs300,Subs301,Subs302,Subs303,Subs304,
            Subs305,Subs306,Subs307,Subs308,Subs309,Subs310)

rm(Subs101,Subs102,Subs103,Subs104,Subs105,Subs106,Subs107,Subs108,Subs109,Subs110,Subs111,Subs112,
   Subs113,Subs114,Subs115,Subs116,Subs117,Subs118,Subs119,Subs120,Subs121,Subs122,Subs123,Subs124,
   Subs125,Subs126,Subs127,Subs128,Subs129,Subs130,Subs131,Subs132,Subs133,Subs134,Subs135,Subs136,
   Subs137,Subs138,Subs139,Subs140,Subs141,Subs142,Subs143,Subs144,Subs145,Subs146,Subs147,Subs148,
   Subs149,Subs150,Subs151,Subs152,Subs153,Subs154,Subs155,Subs156,Subs157,Subs158,Subs159,Subs160,
   Subs161,Subs162,Subs163,Subs164,Subs165,Subs166,Subs167,Subs168,Subs169,Subs170,Subs171,Subs172,
   Subs173,Subs174,Subs175,Subs176,Subs177,Subs178,Subs179,Subs180,Subs181,Subs182,Subs183,Subs184,
   Subs185,Subs186,Subs187,Subs188,Subs189,Subs190,Subs191,Subs192,Subs193,Subs194,Subs195,Subs196,
   Subs197,Subs198,Subs199,Subs200,Subs201,Subs202,Subs203,Subs204,Subs205,Subs206,Subs207,Subs208,
   Subs209,Subs210,Subs211,Subs212,Subs213,Subs214,Subs215,Subs216,Subs217,Subs218,Subs219,Subs220,
   Subs221,Subs222,Subs223,Subs224,Subs225,Subs226,Subs227,Subs228,Subs229,Subs230,Subs231,Subs232,
   Subs233,Subs234,Subs235,Subs236,Subs237,Subs238,Subs239,Subs240,Subs241,Subs242,Subs243,Subs244,
   Subs245,Subs246,Subs247,Subs248,Subs249,Subs250,Subs251,Subs252,Subs253,Subs254,Subs255,Subs256,
   Subs257,Subs258,Subs259,Subs260,Subs261,Subs262,Subs263,Subs264,Subs265,Subs266,Subs267,Subs268,
   Subs269,Subs270,Subs271,Subs272,Subs273,Subs274,Subs275,Subs276,Subs277,Subs278,Subs279,Subs280,
   Subs281,Subs282,Subs283,Subs284,Subs285,Subs286,Subs287,Subs288,Subs289,Subs290,Subs291,Subs292,
   Subs293,Subs294,Subs295,Subs296,Subs297,Subs298,Subs299,Subs300,Subs301,Subs302,Subs303,Subs304,
   Subs305,Subs306,Subs307,Subs308,Subs309,Subs310)

#Remove duplicates
names(Subs)<-c("Check2","Subs")
Subs$test<-paste(Subs$Check2,Subs$Subs,sep=" ")
Subs<-Subs[!duplicated(Subs$test),]
Subs[3]<-NULL


##Produce files of ultimate owners with all subsidiaries in one column
Own1<-Final[!is.na(Final$Match1),]
Own1[22:27]<-NULL

Own111<-Own1
Subsidiaries1<-Own1[,1]
Own11<-cbind(Own111,Subsidiaries1)
Own11[2:21]<-NULL
names(Own11) <- c("Company","Subsidiaries")

Own12<-Own1
Own12[3:21]<-NULL
names(Own12) <- c("Company","Subsidiaries")

Own13<-Own1
Own13[2]<-NULL
Own13[3:20]<-NULL
names(Own13) <- c("Company","Subsidiaries")

Own14<-Own1
Own14[2:3]<-NULL
Own14[3:19]<-NULL
names(Own14) <- c("Company","Subsidiaries")

Own15<-Own1
Own15[2:4]<-NULL
Own15[3:18]<-NULL
names(Own15) <- c("Company","Subsidiaries")

Own16<-Own1
Own16[2:5]<-NULL
Own16[3:17]<-NULL
names(Own16) <- c("Company","Subsidiaries")

Own17<-Own1
Own17[2:6]<-NULL
Own17[3:16]<-NULL
names(Own17) <- c("Company","Subsidiaries")

Own18<-Own1
Own18[2:7]<-NULL
Own18[3:15]<-NULL
names(Own18) <- c("Company","Subsidiaries")

Own19<-Own1
Own19[2:8]<-NULL
Own19[3:14]<-NULL
names(Own19) <- c("Company","Subsidiaries")

Own20<-Own1
Own20[2:9]<-NULL
Own20[3:13]<-NULL
names(Own20) <- c("Company","Subsidiaries")

Own21<-Own1
Own21[2:10]<-NULL
Own21[3:12]<-NULL
names(Own21) <- c("Company","Subsidiaries")

Own22<-Own1
Own22[2:11]<-NULL
Own22[3:11]<-NULL
names(Own22) <- c("Company","Subsidiaries")

Own23<-Own1
Own23[2:12]<-NULL
Own23[3:10]<-NULL
names(Own23) <- c("Company","Subsidiaries")

Own24<-Own1
Own24[2:13]<-NULL
Own24[3:9]<-NULL
names(Own24) <- c("Company","Subsidiaries")

Own25<-Own1
Own25[2:14]<-NULL
Own25[3:8]<-NULL
names(Own25) <- c("Company","Subsidiaries")

Own26<-Own1
Own26[2:15]<-NULL
Own26[3:7]<-NULL
names(Own26) <- c("Company","Subsidiaries")

Own27<-Own1
Own27[2:16]<-NULL
Own27[3:6]<-NULL
names(Own27) <- c("Company","Subsidiaries")

Own28<-Own1
Own28[2:17]<-NULL
Own28[3:5]<-NULL
names(Own28) <- c("Company","Subsidiaries")

Own29<-Own1
Own29[2:18]<-NULL
Own29[3:4]<-NULL
names(Own29) <- c("Company","Subsidiaries")

Own30<-Own1
Own30[2:19]<-NULL
Own30[3]<-NULL
names(Own30) <- c("Company","Subsidiaries")

Own31<-Own1
Own31[2:20]<-NULL
names(Own31) <- c("Company","Subsidiaries")


#Second level
Own4<-Final[!is.na(Final$Match2),]
Own4[22:27]<-NULL
Own4[1]<-NULL

Own411<-Own4
Subsidiaries3<-Own4[,1]
Own41<-cbind(Own411,Subsidiaries3)
Own41[2:20]<-NULL
names(Own41) <- c("Company","Subsidiaries")

Own42<-Own4
Own42[3:20]<-NULL
names(Own42) <- c("Company","Subsidiaries")

Own43<-Own4
Own43[2]<-NULL
Own43[3:19]<-NULL
names(Own43) <- c("Company","Subsidiaries")

Own44<-Own4
Own44[2:3]<-NULL
Own44[3:18]<-NULL
names(Own44) <- c("Company","Subsidiaries")

Own45<-Own4
Own45[2:4]<-NULL
Own45[3:17]<-NULL
names(Own45) <- c("Company","Subsidiaries")

Own46<-Own4
Own46[2:5]<-NULL
Own46[3:16]<-NULL
names(Own46) <- c("Company","Subsidiaries")

Own47<-Own4
Own47[2:6]<-NULL
Own47[3:15]<-NULL
names(Own47) <- c("Company","Subsidiaries")

Own48<-Own4
Own48[2:7]<-NULL
Own48[3:14]<-NULL
names(Own48) <- c("Company","Subsidiaries")

Own49<-Own4
Own49[2:8]<-NULL
Own49[3:13]<-NULL
names(Own49) <- c("Company","Subsidiaries")

Own50<-Own4
Own50[2:9]<-NULL
Own50[3:12]<-NULL
names(Own50) <- c("Company","Subsidiaries")

Own51<-Own4
Own51[2:10]<-NULL
Own51[3:11]<-NULL
names(Own51) <- c("Company","Subsidiaries")

Own52<-Own4
Own52[2:11]<-NULL
Own52[3:10]<-NULL
names(Own52) <- c("Company","Subsidiaries")

Own53<-Own4
Own53[2:12]<-NULL
Own53[3:9]<-NULL
names(Own53) <- c("Company","Subsidiaries")

Own54<-Own4
Own54[2:13]<-NULL
Own54[3:8]<-NULL
names(Own54) <- c("Company","Subsidiaries")

Own55<-Own4
Own55[2:14]<-NULL
Own55[3:7]<-NULL
names(Own55) <- c("Company","Subsidiaries")

Own56<-Own4
Own56[2:15]<-NULL
Own56[3:6]<-NULL
names(Own56) <- c("Company","Subsidiaries")

Own57<-Own4
Own57[2:16]<-NULL
Own57[3:5]<-NULL
names(Own57) <- c("Company","Subsidiaries")

Own58<-Own4
Own58[2:17]<-NULL
Own58[3:4]<-NULL
names(Own58) <- c("Company","Subsidiaries")

Own59<-Own4
Own59[2:18]<-NULL
Own59[3]<-NULL
names(Own59) <- c("Company","Subsidiaries")

Own60<-Own4
Own60[2:19]<-NULL
names(Own60) <- c("Company","Subsidiaries")


#Bind all Own-datasets with two columns
Own1<-bind_rows(Own11,Own12,Own13,Own14,Own15,Own16,Own17,Own18,Own19,Own20,Own21,Own22,Own23,Own24,
                Own25,Own26,Own27,Own28,Own29,Own30,Own31)
Own2<-bind_rows(Own41,Own42,Own43,Own44,Own45,Own46,Own47,Own48,Own49,Own50,Own51,Own52,Own53,Own54,
                Own55,Own56,Own57,Own58,Own59,Own60)

#Combine the three dataset with different levels of owners
Own3<-bind_rows(Own1,Own2)

#Remove duplicates
Own3$test<-paste(Own3$Company,Own3$Subsidiaries,sep=" ")
Own<-Own3[!duplicated(Own3$test),]
Own[3]<-NULL

Own<-subset(Own,!is.na(Own$Subsidiaries))
Own<-subset(Own,!is.na(Own$Company))
Own<-Own[complete.cases(Own[,1:2]), ]

Own<-Own[order(Own[,1],Own[,2]),]

#Remove Companies that are not corporate and have only one subsidiary
Own$test<-duplicated(Own$Company) | duplicated(Own$Company, fromLast = TRUE)
Own<-subset(Own,Own$test!="FALSE")
Own[3]<-NULL

#Remove unnecessary data
rm(Own3,Own4,Own11,Own12,Own13,Own14,Own15,Own16,Own17,Own18,Own19,Own20,Own21,Own22,Own23,Own24,
   Own25,Own26,Own27,Own28,Own29,Own30,Own31,Own41,Own42,Own43,Own44,Own45,Own46,Own47,Own111,Own411,
   Own48,Own49,Own50,Own51,Own52,Own53,Own54,Own55,Own56,Own57,Own58,Own59,Own60,Own1,Own2,
   col_order,cols.num,Large,mydata,Subsidiaries1,Subsidiaries3,Subs1,Type)

# Establishment of global ownership structures in 2020 completed #




#### . ####


###Part II: Recreating ownership structures of the past 10 years by adding M&A data and dates of incorporation ####

#   1. Complement today's ownership structure with past M&A deals ####

## Prepare the data
#Read the data
Deals1<-read_excel("Dataset/Deals/Deals1.xlsx",sheet = "Results")
Deals2<-read_excel("Dataset/Deals/Deals2.xlsx",sheet = "Results")
Deals3<-read_excel("Dataset/Deals/Deals3.xlsx",sheet = "Results")
Deals4<-read_excel("Dataset/Deals/Deals4.xlsx",sheet = "Results")
Deals5<-read_excel("Dataset/Deals/Deals5.xlsx",sheet = "Results")

names(Deals5) <- names(Deals1)
names(Deals4) <- names(Deals1) 
names(Deals3) <- names(Deals1) 
names(Deals2) <- names(Deals1) 

identical(names(Deals5),names(Deals4))
identical(names(Deals3),names(Deals4))
identical(names(Deals2),names(Deals3))
identical(names(Deals2),names(Deals1))

#Merge data
Deals_data<-rbind(Deals1,Deals2,Deals3,Deals4,Deals5)

Deals_data1<-Deals_data

#Remove unnecessary files
rm(Deals1,Deals2,Deals3,Deals4,Deals5)

##Restructure dataset
#Remove Row of Numbers and Company Names + Rename/Order Columns
Deals_data[1:2] <- NULL
names(Deals_data) <- c("TargetName","TargetBvDID","AcquirorName","AcqBvDID","VendorName","VendBvDID",
                       "DealType","DealStatus","ExpCompDate","AssCompDate","CompDate","DealValue")
sapply(Deals_data, class)

#Indicate missing values
Deals_data<-na_if(Deals_data,"n.a.")

#Fill empty target cells
Deals_data[1,2]<-"DE5050202813"
Deals_data<-Deals_data[complete.cases(Deals_data[,1:4]), ]
Deals_data<-Deals_data[complete.cases(Deals_data[,7]), ]
#Deals_data$TargetBvDID<-na.locf(Deals_data$TargetBvDID)
#Deals_data$DealType<-na.locf(Deals_data$DealType)

#Decide on one specific date
Deals_data$Date1<-case_when(!is.na(Deals_data$CompDate)~Deals_data$CompDate,
                            is.na(Deals_data$CompDate)~Deals_data$AssCompDate)

Deals_data$Date<-case_when(!is.na(Deals_data$Date1)~Deals_data$Date1,
                           is.na(Deals_data$Date1)~Deals_data$DealStatus)

Deals_data$Date<-substr(Deals_data$Date,7,10)
Deals_data$Date<-na.locf(Deals_data$Date)

#Delete unnecessary columns
Deals_data<-Deals_data[,c(-1,-3,-5,-9,-10,-11,-12,-13)]

#Clear the data of text to retain deals that are relevant (acquisition of >25.01% or the creation of new companies)
Deals_data<-subset(Deals_data,Deals_data$DealType!="Minority stake unknown %")
Deals_data<-subset(Deals_data,substr(Deals_data$DealType,1,21)!="Acquisition increased")
Deals_data<-subset(Deals_data,Deals_data$DealType!="Capital Increase unknown stake %")
Deals_data<-subset(Deals_data,substr(Deals_data$DealType,1,21)!="Institutional buy-out")
Deals_data<-subset(Deals_data,substr(Deals_data$DealType,1,39)!="Capital Increase unknown minority stake")
Deals_data<-subset(Deals_data,substr(Deals_data$DealType,1,27)!="Share buyback unknown stake")
Deals_data<-subset(Deals_data,substr(Deals_data$DealType,1,22)!="unknown majority stake")
Deals_data<-subset(Deals_data,substr(Deals_data$DealType,1,23)!="unknown remaining stake")

Deals_data$DealType<- lapply(Deals_data$DealType,gsub, pattern=c('%'), replacement='')
Deals_data$DealType<- lapply(Deals_data$DealType,gsub, pattern='Acquisition', replacement='')
Deals_data$DealType<- lapply(Deals_data$DealType,gsub, pattern='Minority stake', replacement='')
Deals_data$DealType<- lapply(Deals_data$DealType,gsub, pattern='Share buyback', replacement='')
Deals_data$DealType<- lapply(Deals_data$DealType,gsub, pattern='Capital Increase', replacement='')
Deals_data$DealType<- lapply(Deals_data$DealType,gsub, pattern='Capital increase', replacement='')
Deals_data$DealType<- lapply(Deals_data$DealType,gsub, pattern='Joint Venture', replacement='')
Deals_data$DealType<- lapply(Deals_data$DealType,gsub, pattern='Joint venture', replacement='')
Deals_data$DealType<- lapply(Deals_data$DealType,gsub, pattern='Capital increase', replacement='')
Deals_data$DealType<- lapply(Deals_data$DealType,gsub, pattern='acquired', replacement='')
Deals_data$DealType<- lapply(Deals_data$DealType,gsub, pattern='Majority stake', replacement='')
Deals_data$DealType<- lapply(Deals_data$DealType,gsub, pattern='Demerger', replacement='')
Deals_data$DealType<- lapply(Deals_data$DealType,gsub, pattern='Merger', replacement='')
Deals_data$DealType<- lapply(Deals_data$DealType,gsub, pattern='Management buy-out', replacement='')
Deals_data$DealType<- lapply(Deals_data$DealType,gsub, pattern='Management buy-in', replacement='')
Deals_data$DealType1<-str_extract(Deals_data$DealType,"(?i)(?<=to\\D)\\d+")
Deals_data$DealType2<-str_extract(Deals_data$DealType,"(?i)(?<=hold\\D)\\d+")
Deals_data$DealType3<-ifelse(is.na(Deals_data$DealType1),Deals_data$DealType2,Deals_data$DealType1)
Deals_data$DealType<-ifelse(!is.na(Deals_data$DealType3),Deals_data$DealType3,Deals_data$DealType)

#Extract the numbers from the dataset
Deals_data$DealType<-substr(Deals_data$DealType,1,4)

#Make numeric
Deals_data$DealType<-as.numeric(Deals_data$DealType)
Deals_data$Date<-na.locf(Deals_data$Date)
Deals_data$Date<-as.numeric(Deals_data$Date)

#Delete data that does not fit the criteria 
Deals_data$Delete<-ifelse(Deals_data$DealType<25,1,0)
Deals_data<-subset(Deals_data,Deals_data$Delete!=1)

#Extract necessary variables
Deals_data<-Deals_data[1:6]


##Add subsidiary data for the company-subsidiary string
DealsSubs_Files<-list.files(path="Dataset/DealsSubs",pattern ='.xlsx', full.names = T)
DealsSubs <- sapply(DealsSubs_Files, read_excel, sheet=2,simplify=FALSE) %>% 
   bind_rows(.id = "id")

#Restructure data
#Remove Row of Numbers and Company Names + Rename/Order Columns
DealsSubs[1:3] <- NULL
names(DealsSubs) <- c("BvDID","SubBvDID","Share")
sapply(DealsSubs, class)

#Indicate missing values
DealsSubs<-na_if(DealsSubs,"n.a.")

##Create a country Owner+subsidiaries data frame for binding
DealsSubs$BvDID<-na.locf(DealsSubs$BvDID)
DealsSubs$SubBvDID<-na.locf(DealsSubs$SubBvDID)

#Only retain direct ownership share with 25.01 percent (not unknown)
DealsSubs$Share<-lapply(DealsSubs$Share,gsub, pattern='WO', replacement='50')
DealsSubs$Share<-lapply(DealsSubs$Share,gsub, pattern='MO', replacement='50') # not clear if majority or minority owned
DealsSubs$Share<-lapply(DealsSubs$Share,gsub, pattern='>', replacement='')
DealsSubs$Share<-as.numeric(DealsSubs$Share)
DealsSubs$Delete<-ifelse(DealsSubs$Share>25,1,0)
DealsSubs<-subset(DealsSubs,Delete==1)

#Add deals subsidiaries to subsidiary string
Deals<-DealsSubs[1:2]
names(Deals)<-c("Check2","Subs")
Subs<-bind_rows(Subs,Deals)


## Join the deal data with the ownership dataset   -  by year!
# Divide deals dataset by year
Deals2020<-subset(Deals_data,Deals_data$Date==2020)
Deals2019<-subset(Deals_data,Deals_data$Date==2019)
Deals2018<-subset(Deals_data,Deals_data$Date==2018)
Deals2017<-subset(Deals_data,Deals_data$Date==2017)
Deals2016<-subset(Deals_data,Deals_data$Date==2016)
Deals2015<-subset(Deals_data,Deals_data$Date==2015)
Deals2014<-subset(Deals_data,Deals_data$Date==2014)
Deals2013<-subset(Deals_data,Deals_data$Date==2013)
Deals2012<-subset(Deals_data,Deals_data$Date==2012)
Deals2011<-subset(Deals_data,Deals_data$Date==2011)
Deals2010<-subset(Deals_data,Deals_data$Date==2010)

#Enter a dummy variables to later identify subsidiaries that were added in this year
Deals2020$Added<-1
Deals2019$Added<-1
Deals2018$Added<-1
Deals2017$Added<-1
Deals2016$Added<-1
Deals2015$Added<-1
Deals2014$Added<-1
Deals2013$Added<-1
Deals2012$Added<-1
Deals2011$Added<-1
Deals2010$Added<-1

#Remove duplicates
names(Subs)<-c("Check2","Subs")
Subs$test<-paste(Subs$Check2,Subs$Subs,sep=" ")
Subs<-Subs[!duplicated(Subs$test),]
Subs[3]<-NULL
Subs<-Subs[complete.cases(Subs[,1:2]), ]

###Merge ownership and deals data for each year and move back through the years
names(Subs)<-c("TargetBvDID193","Subs")
###Merge ownership and deals data for each year and move back through the years
##Ownership 2019
#Merge by vendor and if the GUO sold targets in 2020, they are added to the ownership structure of 2019
Deals20193<-Deals2020[,c(3,1,2,3,7)]
names(Deals20193)<-c("Subsidiaries","TargetBvDID193","AcqBvDID193","VendBvDID193","Added")
Full193x<-left_join(Own,Deals20193,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full193x$TargetBvDID193)))
names(Subs)<-c("TargetBvDID193","Subs")
Full193<-left_join(Full193x,Subs,by="TargetBvDID193",na_matches="never")
length(which(!is.na(Full193$Subs)))
Full193x<-subset(Full193,!is.na(Full193$VendBvDID193))
Full193y<-Full193x[,c(5,3,6)]
names(Full193y)<-c("Company","Subsidiaries","Added")
Full193z<-Full193x[,c(5,7,6)]
names(Full193z)<-c("Company","Subsidiaries","Added")
Full193a<-bind_rows(Full193y,Full193z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full193<-left_join(Own,Full193a,by="Subsidiaries",na_matches="never")
Full19A<-subset(Full193,is.na(Full193$Added))
Full19A<-Full19A[,c(1,2,4)]
Full19B<-subset(Full193,!is.na(Full193$Added))
Full19B<-Full19B[,c(3,2,4)]
names(Full19A)<-c("Company","Subsidiaries","Added")
names(Full19B)<-c("Company","Subsidiaries","Added")
Full19<-bind_rows(Full19A,Full19B)

#Remove duplicate GUO-subsidiary combinations
Full19$test<-paste(Full19$Company,Full19$Subsidiaries,sep=" ")
Full19<-Full19[!duplicated(Full19$test),]
Full19<-subset(Full19,!is.na(Full19$Subsidiaries))
Full19[4]<-NULL
Full19<-Full19[complete.cases(Full19[,1:2]), ]
Full19B<-Full19[1:2]

#Add to the subsidiary data
Full193a[3]<-NULL
names(Subs)<-c("Company","Subsidiaries")
Subs19<-left_join(Subs,Full193a,by="Subsidiaries",na_matches="never")
Subs19<-subset(Subs19,is.na(Subs19$Company.y))
Subs19[3]<-NULL
names(Full193a)<-c("Companies","Subs")
names(Subs19)<-c("Companies","Subs")
Subs19<-bind_rows(Subs19,Full193a)

#Remove duplicates
Subs19$test<-paste(Subs19$Companies,Subs19$Subs,sep=" ")
Subs19<-Subs19[!duplicated(Subs19$test),]
Subs19[3]<-NULL
Subs19<-Subs19[complete.cases(Subs19[,1:2]), ]

#Remove unnecessary files
rm(Full193,Deals20193,Full193a,Full193x,Full193y,Full193z,Full19A,Deals_data,Deals_data1)


##Ownership 2018
names(Subs19) <- c("TargetBvDID183","Subs")

#Merge by vendor and if the GUO sold targets in 2020, they are added to the ownership structure of 2018
Deals20183<-Deals2019[,c(3,1,2,3,7)]
names(Deals20183)<-c("Subsidiaries","TargetBvDID183","AcqBvDID183","VendBvDID183","Added")
Full183x<-left_join(Full19B,Deals20183,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full183x$TargetBvDID183)))
names(Subs)<-c("TargetBvDID183","Subs")
Full183<-left_join(Full183x,Subs19,by="TargetBvDID183",na_matches="never")
length(which(!is.na(Full183$Subs)))
Full183x<-subset(Full183,!is.na(Full183$VendBvDID183))
Full183y<-Full183x[,c(5,3,6)]
names(Full183y)<-c("Company","Subsidiaries","Added")
Full183z<-Full183x[,c(5,7,6)]
names(Full183z)<-c("Company","Subsidiaries","Added")
Full183a<-bind_rows(Full183y,Full183z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full183<-left_join(Full19B,Full183a,by="Subsidiaries",na_matches="never")
Full18A<-subset(Full183,is.na(Full183$Added))
Full18A<-Full18A[,c(1,2,4)]
Full18B<-subset(Full183,!is.na(Full183$Added))
Full18B<-Full18B[,c(3,2,4)]
names(Full18A)<-c("Company","Subsidiaries","Added")
names(Full18B)<-c("Company","Subsidiaries","Added")
Full18<-bind_rows(Full18A,Full18B)

#Remove duplicate GUO-subsidiary combinations
Full18$test<-paste(Full18$Company,Full18$Subsidiaries,sep=" ")
Full18<-Full18[!duplicated(Full18$test),]
Full18<-subset(Full18,!is.na(Full18$Subsidiaries))
Full18[4]<-NULL
Full18<-Full18[complete.cases(Full18[,1:2]), ]
Full18B<-Full18[1:2]

#Add to the subsidiary data
Full183a[3]<-NULL
names(Subs19)<-c("Company","Subsidiaries")
Subs18<-left_join(Subs19,Full183a,by="Subsidiaries",na_matches="never")
Subs18<-subset(Subs18,is.na(Subs18$Company.y))
Subs18[3]<-NULL
names(Full183a)<-c("Companies","Subs")
names(Subs18)<-c("Companies","Subs")
Subs18<-bind_rows(Subs18,Full183a)

#Remove duplicates
Subs18$test<-paste(Subs18$Companies,Subs18$Subs,sep=" ")
Subs18<-Subs18[!duplicated(Subs18$test),]
Subs18[3]<-NULL
Subs18<-Subs18[complete.cases(Subs18[,1:2]), ]

#Remove unnecessary files
rm(Full183,Deals20183,Full183a,Full183x,Full183y,Full183z,Full18A,Deals_data,Deals_data1)


##Ownership 2017
names(Subs18) <- c("TargetBvDID173","Subs")

#Merge by vendor and if the GUO sold targets in 2020, they are added to the ownership structure of 2017
Deals20173<-Deals2018[,c(3,1,2,3,7)]
names(Deals20173)<-c("Subsidiaries","TargetBvDID173","AcqBvDID173","VendBvDID173","Added")
Full173x<-left_join(Full18B,Deals20173,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full173x$TargetBvDID173)))
names(Subs)<-c("TargetBvDID173","Subs")
Full173<-left_join(Full173x,Subs18,by="TargetBvDID173",na_matches="never")
length(which(!is.na(Full173$Subs)))
Full173x<-subset(Full173,!is.na(Full173$VendBvDID173))
Full173y<-Full173x[,c(5,3,6)]
names(Full173y)<-c("Company","Subsidiaries","Added")
Full173z<-Full173x[,c(5,7,6)]
names(Full173z)<-c("Company","Subsidiaries","Added")
Full173a<-bind_rows(Full173y,Full173z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full173<-left_join(Full18B,Full173a,by="Subsidiaries",na_matches="never")
Full17A<-subset(Full173,is.na(Full173$Added))
Full17A<-Full17A[,c(1,2,4)]
Full17B<-subset(Full173,!is.na(Full173$Added))
Full17B<-Full17B[,c(3,2,4)]
names(Full17A)<-c("Company","Subsidiaries","Added")
names(Full17B)<-c("Company","Subsidiaries","Added")
Full17<-bind_rows(Full17A,Full17B)

#Remove duplicate GUO-subsidiary combinations
Full17$test<-paste(Full17$Company,Full17$Subsidiaries,sep=" ")
Full17<-Full17[!duplicated(Full17$test),]
Full17<-subset(Full17,!is.na(Full17$Subsidiaries))
Full17[4]<-NULL
Full17<-Full17[complete.cases(Full17[,1:2]), ]
Full17B<-Full17[1:2]

#Add to the subsidiary data
Full173a[3]<-NULL
names(Subs18)<-c("Company","Subsidiaries")
Subs17<-left_join(Subs18,Full173a,by="Subsidiaries",na_matches="never")
Subs17<-subset(Subs17,is.na(Subs17$Company.y))
Subs17[3]<-NULL
names(Full173a)<-c("Companies","Subs")
names(Subs17)<-c("Companies","Subs")
Subs17<-bind_rows(Subs17,Full173a)

#Remove duplicates
Subs17$test<-paste(Subs17$Companies,Subs17$Subs,sep=" ")
Subs17<-Subs17[!duplicated(Subs17$test),]
Subs17[3]<-NULL
Subs17<-Subs17[complete.cases(Subs17[,1:2]), ]

#Remove unnecessary files
rm(Full173,Deals20173,Full173a,Full173x,Full173y,Full173z,Full17A,Deals_data,Deals_data1)


##Ownership 2016
names(Subs17) <- c("TargetBvDID163","Subs")

#Merge by vendor and if the GUO sold targets in 2020, they are added to the ownership structure of 2016
Deals20163<-Deals2017[,c(3,1,2,3,7)]
names(Deals20163)<-c("Subsidiaries","TargetBvDID163","AcqBvDID163","VendBvDID163","Added")
Full163x<-left_join(Full17B,Deals20163,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full163x$TargetBvDID163)))
names(Subs)<-c("TargetBvDID163","Subs")
Full163<-left_join(Full163x,Subs17,by="TargetBvDID163",na_matches="never")
length(which(!is.na(Full163$Subs)))
Full163x<-subset(Full163,!is.na(Full163$VendBvDID163))
Full163y<-Full163x[,c(5,3,6)]
names(Full163y)<-c("Company","Subsidiaries","Added")
Full163z<-Full163x[,c(5,7,6)]
names(Full163z)<-c("Company","Subsidiaries","Added")
Full163a<-bind_rows(Full163y,Full163z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full163<-left_join(Full17B,Full163a,by="Subsidiaries",na_matches="never")
Full16A<-subset(Full163,is.na(Full163$Added))
Full16A<-Full16A[,c(1,2,4)]
Full16B<-subset(Full163,!is.na(Full163$Added))
Full16B<-Full16B[,c(3,2,4)]
names(Full16A)<-c("Company","Subsidiaries","Added")
names(Full16B)<-c("Company","Subsidiaries","Added")
Full16<-bind_rows(Full16A,Full16B)

#Remove duplicate GUO-subsidiary combinations
Full16$test<-paste(Full16$Company,Full16$Subsidiaries,sep=" ")
Full16<-Full16[!duplicated(Full16$test),]
Full16<-subset(Full16,!is.na(Full16$Subsidiaries))
Full16[4]<-NULL
Full16<-Full16[complete.cases(Full16[,1:2]), ]
Full16B<-Full16[1:2]

#Add to the subsidiary data
Full163a[3]<-NULL
names(Subs17)<-c("Company","Subsidiaries")
Subs16<-left_join(Subs17,Full163a,by="Subsidiaries",na_matches="never")
Subs16<-subset(Subs16,is.na(Subs16$Company.y))
Subs16[3]<-NULL
names(Full163a)<-c("Companies","Subs")
names(Subs16)<-c("Companies","Subs")
Subs16<-bind_rows(Subs16,Full163a)

#Remove duplicates
Subs16$test<-paste(Subs16$Companies,Subs16$Subs,sep=" ")
Subs16<-Subs16[!duplicated(Subs16$test),]
Subs16[3]<-NULL
Subs16<-Subs16[complete.cases(Subs16[,1:2]), ]

#Remove unnecessary files
rm(Full163,Deals20163,Full163a,Full163x,Full163y,Full163z,Full16A,Deals_data,Deals_data1)


##Ownership 2015
names(Subs16) <- c("TargetBvDID153","Subs")

#Merge by vendor and if the GUO sold targets in 2020, they are added to the ownership structure of 2015
Deals20153<-Deals2016[,c(3,1,2,3,7)]
names(Deals20153)<-c("Subsidiaries","TargetBvDID153","AcqBvDID153","VendBvDID153","Added")
Full153x<-left_join(Full16B,Deals20153,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full153x$TargetBvDID153)))
names(Subs)<-c("TargetBvDID153","Subs")
Full153<-left_join(Full153x,Subs16,by="TargetBvDID153",na_matches="never")
length(which(!is.na(Full153$Subs)))
Full153x<-subset(Full153,!is.na(Full153$VendBvDID153))
Full153y<-Full153x[,c(5,3,6)]
names(Full153y)<-c("Company","Subsidiaries","Added")
Full153z<-Full153x[,c(5,7,6)]
names(Full153z)<-c("Company","Subsidiaries","Added")
Full153a<-bind_rows(Full153y,Full153z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full153<-left_join(Full16B,Full153a,by="Subsidiaries",na_matches="never")
Full15A<-subset(Full153,is.na(Full153$Added))
Full15A<-Full15A[,c(1,2,4)]
Full15B<-subset(Full153,!is.na(Full153$Added))
Full15B<-Full15B[,c(3,2,4)]
names(Full15A)<-c("Company","Subsidiaries","Added")
names(Full15B)<-c("Company","Subsidiaries","Added")
Full15<-bind_rows(Full15A,Full15B)

#Remove duplicate GUO-subsidiary combinations
Full15$test<-paste(Full15$Company,Full15$Subsidiaries,sep=" ")
Full15<-Full15[!duplicated(Full15$test),]
Full15<-subset(Full15,!is.na(Full15$Subsidiaries))
Full15[4]<-NULL
Full15<-Full15[complete.cases(Full15[,1:2]), ]
Full15B<-Full15[1:2]

#Add to the subsidiary data
Full153a[3]<-NULL
names(Subs16)<-c("Company","Subsidiaries")
Subs15<-left_join(Subs16,Full153a,by="Subsidiaries",na_matches="never")
Subs15<-subset(Subs15,is.na(Subs15$Company.y))
Subs15[3]<-NULL
names(Full153a)<-c("Companies","Subs")
names(Subs15)<-c("Companies","Subs")
Subs15<-bind_rows(Subs15,Full153a)

#Remove duplicates
Subs15$test<-paste(Subs15$Companies,Subs15$Subs,sep=" ")
Subs15<-Subs15[!duplicated(Subs15$test),]
Subs15[3]<-NULL
Subs15<-Subs15[complete.cases(Subs15[,1:2]), ]

#Remove unnecessary files
rm(Full153,Deals20153,Full153a,Full153x,Full153y,Full153z,Full15A,Deals_data,Deals_data1)


##Ownership 2014
names(Subs15) <- c("TargetBvDID143","Subs")

#Merge by vendor and if the GUO sold targets in 2020, they are added to the ownership structure of 2014
Deals20143<-Deals2015[,c(3,1,2,3,7)]
names(Deals20143)<-c("Subsidiaries","TargetBvDID143","AcqBvDID143","VendBvDID143","Added")
Full143x<-left_join(Full15B,Deals20143,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full143x$TargetBvDID143)))
names(Subs)<-c("TargetBvDID143","Subs")
Full143<-left_join(Full143x,Subs15,by="TargetBvDID143",na_matches="never")
length(which(!is.na(Full143$Subs)))
Full143x<-subset(Full143,!is.na(Full143$VendBvDID143))
Full143y<-Full143x[,c(5,3,6)]
names(Full143y)<-c("Company","Subsidiaries","Added")
Full143z<-Full143x[,c(5,7,6)]
names(Full143z)<-c("Company","Subsidiaries","Added")
Full143a<-bind_rows(Full143y,Full143z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full143<-left_join(Full15B,Full143a,by="Subsidiaries",na_matches="never")
Full14A<-subset(Full143,is.na(Full143$Added))
Full14A<-Full14A[,c(1,2,4)]
Full14B<-subset(Full143,!is.na(Full143$Added))
Full14B<-Full14B[,c(3,2,4)]
names(Full14A)<-c("Company","Subsidiaries","Added")
names(Full14B)<-c("Company","Subsidiaries","Added")
Full14<-bind_rows(Full14A,Full14B)

#Remove duplicate GUO-subsidiary combinations
Full14$test<-paste(Full14$Company,Full14$Subsidiaries,sep=" ")
Full14<-Full14[!duplicated(Full14$test),]
Full14<-subset(Full14,!is.na(Full14$Subsidiaries))
Full14[4]<-NULL
Full14<-Full14[complete.cases(Full14[,1:2]), ]
Full14B<-Full14[1:2]

#Add to the subsidiary data
Full143a[3]<-NULL
names(Subs15)<-c("Company","Subsidiaries")
Subs14<-left_join(Subs15,Full143a,by="Subsidiaries",na_matches="never")
Subs14<-subset(Subs14,is.na(Subs14$Company.y))
Subs14[3]<-NULL
names(Full143a)<-c("Companies","Subs")
names(Subs14)<-c("Companies","Subs")
Subs14<-bind_rows(Subs14,Full143a)

#Remove duplicates
Subs14$test<-paste(Subs14$Companies,Subs14$Subs,sep=" ")
Subs14<-Subs14[!duplicated(Subs14$test),]
Subs14[3]<-NULL
Subs14<-Subs14[complete.cases(Subs14[,1:2]), ]

#Remove unnecessary files
rm(Full143,Deals20143,Full143a,Full143x,Full143y,Full143z,Full14A,Deals_data,Deals_data1)


##Ownership 2013
names(Subs14) <- c("TargetBvDID133","Subs")

#Merge by vendor and if the GUO sold targets in 2020, they are added to the ownership structure of 2013
Deals20133<-Deals2014[,c(3,1,2,3,7)]
names(Deals20133)<-c("Subsidiaries","TargetBvDID133","AcqBvDID133","VendBvDID133","Added")
Full133x<-left_join(Full14B,Deals20133,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full133x$TargetBvDID133)))
names(Subs)<-c("TargetBvDID133","Subs")
Full133<-left_join(Full133x,Subs14,by="TargetBvDID133",na_matches="never")
length(which(!is.na(Full133$Subs)))
Full133x<-subset(Full133,!is.na(Full133$VendBvDID133))
Full133y<-Full133x[,c(5,3,6)]
names(Full133y)<-c("Company","Subsidiaries","Added")
Full133z<-Full133x[,c(5,7,6)]
names(Full133z)<-c("Company","Subsidiaries","Added")
Full133a<-bind_rows(Full133y,Full133z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full133<-left_join(Full14B,Full133a,by="Subsidiaries",na_matches="never")
Full13A<-subset(Full133,is.na(Full133$Added))
Full13A<-Full13A[,c(1,2,4)]
Full13B<-subset(Full133,!is.na(Full133$Added))
Full13B<-Full13B[,c(3,2,4)]
names(Full13A)<-c("Company","Subsidiaries","Added")
names(Full13B)<-c("Company","Subsidiaries","Added")
Full13<-bind_rows(Full13A,Full13B)

#Remove duplicate GUO-subsidiary combinations
Full13$test<-paste(Full13$Company,Full13$Subsidiaries,sep=" ")
Full13<-Full13[!duplicated(Full13$test),]
Full13<-subset(Full13,!is.na(Full13$Subsidiaries))
Full13[4]<-NULL
Full13<-Full13[complete.cases(Full13[,1:2]), ]
Full13B<-Full13[1:2]

#Add to the subsidiary data
Full133a[3]<-NULL
names(Subs14)<-c("Company","Subsidiaries")
Subs13<-left_join(Subs14,Full133a,by="Subsidiaries",na_matches="never")
Subs13<-subset(Subs13,is.na(Subs13$Company.y))
Subs13[3]<-NULL
names(Full133a)<-c("Companies","Subs")
names(Subs13)<-c("Companies","Subs")
Subs13<-bind_rows(Subs13,Full133a)

#Remove duplicates
Subs13$test<-paste(Subs13$Companies,Subs13$Subs,sep=" ")
Subs13<-Subs13[!duplicated(Subs13$test),]
Subs13[3]<-NULL
Subs13<-Subs13[complete.cases(Subs13[,1:2]), ]

#Remove unnecessary files
rm(Full133,Deals20133,Full133a,Full133x,Full133y,Full133z,Full13A,Deals_data,Deals_data1)


##Ownership 2012
names(Subs13) <- c("TargetBvDID123","Subs")

#Merge by vendor and if the GUO sold targets in 2020, they are added to the ownership structure of 2012
Deals20123<-Deals2013[,c(3,1,2,3,7)]
names(Deals20123)<-c("Subsidiaries","TargetBvDID123","AcqBvDID123","VendBvDID123","Added")
Full123x<-left_join(Full13B,Deals20123,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full123x$TargetBvDID123)))
names(Subs)<-c("TargetBvDID123","Subs")
Full123<-left_join(Full123x,Subs13,by="TargetBvDID123",na_matches="never")
length(which(!is.na(Full123$Subs)))
Full123x<-subset(Full123,!is.na(Full123$VendBvDID123))
Full123y<-Full123x[,c(5,3,6)]
names(Full123y)<-c("Company","Subsidiaries","Added")
Full123z<-Full123x[,c(5,7,6)]
names(Full123z)<-c("Company","Subsidiaries","Added")
Full123a<-bind_rows(Full123y,Full123z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full123<-left_join(Full13B,Full123a,by="Subsidiaries",na_matches="never")
Full12A<-subset(Full123,is.na(Full123$Added))
Full12A<-Full12A[,c(1,2,4)]
Full12B<-subset(Full123,!is.na(Full123$Added))
Full12B<-Full12B[,c(3,2,4)]
names(Full12A)<-c("Company","Subsidiaries","Added")
names(Full12B)<-c("Company","Subsidiaries","Added")
Full12<-bind_rows(Full12A,Full12B)

#Remove duplicate GUO-subsidiary combinations
Full12$test<-paste(Full12$Company,Full12$Subsidiaries,sep=" ")
Full12<-Full12[!duplicated(Full12$test),]
Full12<-subset(Full12,!is.na(Full12$Subsidiaries))
Full12[4]<-NULL
Full12<-Full12[complete.cases(Full12[,1:2]), ]
Full12B<-Full12[1:2]

#Add to the subsidiary data
Full123a[3]<-NULL
names(Subs13)<-c("Company","Subsidiaries")
Subs12<-left_join(Subs13,Full123a,by="Subsidiaries",na_matches="never")
Subs12<-subset(Subs12,is.na(Subs12$Company.y))
Subs12[3]<-NULL
names(Full123a)<-c("Companies","Subs")
names(Subs12)<-c("Companies","Subs")
Subs12<-bind_rows(Subs12,Full123a)

#Remove duplicates
Subs12$test<-paste(Subs12$Companies,Subs12$Subs,sep=" ")
Subs12<-Subs12[!duplicated(Subs12$test),]
Subs12[3]<-NULL
Subs12<-Subs12[complete.cases(Subs12[,1:2]), ]

#Remove unnecessary files
rm(Full123,Deals20123,Full123a,Full123x,Full123y,Full123z,Full12A,Deals_data,Deals_data1)


##Ownership 2011
names(Subs12) <- c("TargetBvDID113","Subs")

#Merge by vendor and if the GUO sold targets in 2020, they are added to the ownership structure of 2011
Deals20113<-Deals2012[,c(3,1,2,3,7)]
names(Deals20113)<-c("Subsidiaries","TargetBvDID113","AcqBvDID113","VendBvDID113","Added")
Full113x<-left_join(Full12B,Deals20113,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full113x$TargetBvDID113)))
names(Subs)<-c("TargetBvDID113","Subs")
Full113<-left_join(Full113x,Subs12,by="TargetBvDID113",na_matches="never")
length(which(!is.na(Full113$Subs)))
Full113x<-subset(Full113,!is.na(Full113$VendBvDID113))
Full113y<-Full113x[,c(5,3,6)]
names(Full113y)<-c("Company","Subsidiaries","Added")
Full113z<-Full113x[,c(5,7,6)]
names(Full113z)<-c("Company","Subsidiaries","Added")
Full113a<-bind_rows(Full113y,Full113z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full113<-left_join(Full12B,Full113a,by="Subsidiaries",na_matches="never")
Full11A<-subset(Full113,is.na(Full113$Added))
Full11A<-Full11A[,c(1,2,4)]
Full11B<-subset(Full113,!is.na(Full113$Added))
Full11B<-Full11B[,c(3,2,4)]
names(Full11A)<-c("Company","Subsidiaries","Added")
names(Full11B)<-c("Company","Subsidiaries","Added")
Full11<-bind_rows(Full11A,Full11B)

#Remove duplicate GUO-subsidiary combinations
Full11$test<-paste(Full11$Company,Full11$Subsidiaries,sep=" ")
Full11<-Full11[!duplicated(Full11$test),]
Full11<-subset(Full11,!is.na(Full11$Subsidiaries))
Full11[4]<-NULL
Full11<-Full11[complete.cases(Full11[,1:2]), ]
Full11B<-Full11[1:2]

#Add to the subsidiary data
Full113a[3]<-NULL
names(Subs12)<-c("Company","Subsidiaries")
Subs11<-left_join(Subs12,Full113a,by="Subsidiaries",na_matches="never")
Subs11<-subset(Subs11,is.na(Subs11$Company.y))
Subs11[3]<-NULL
names(Full113a)<-c("Companies","Subs")
names(Subs11)<-c("Companies","Subs")
Subs11<-bind_rows(Subs11,Full113a)

#Remove duplicates
Subs11$test<-paste(Subs11$Companies,Subs11$Subs,sep=" ")
Subs11<-Subs11[!duplicated(Subs11$test),]
Subs11[3]<-NULL
Subs11<-Subs11[complete.cases(Subs11[,1:2]), ]

#Remove unnecessary files
rm(Full113,Deals20113,Full113a,Full113x,Full113y,Full113z,Full11A,Deals_data,Deals_data1)


##Ownership 2010
names(Subs11) <- c("TargetBvDID103","Subs")

#Merge by vendor and if the GUO sold targets in 2020, they are added to the ownership structure of 2010
Deals20103<-Deals2011[,c(3,1,2,3,7)]
names(Deals20103)<-c("Subsidiaries","TargetBvDID103","AcqBvDID103","VendBvDID103","Added")
Full103x<-left_join(Full11B,Deals20103,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full103x$TargetBvDID103)))
names(Subs)<-c("TargetBvDID103","Subs")
Full103<-left_join(Full103x,Subs11,by="TargetBvDID103",na_matches="never")
length(which(!is.na(Full103$Subs)))
Full103x<-subset(Full103,!is.na(Full103$VendBvDID103))
Full103y<-Full103x[,c(5,3,6)]
names(Full103y)<-c("Company","Subsidiaries","Added")
Full103z<-Full103x[,c(5,7,6)]
names(Full103z)<-c("Company","Subsidiaries","Added")
Full103a<-bind_rows(Full103y,Full103z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full103<-left_join(Full11B,Full103a,by="Subsidiaries",na_matches="never")
Full10A<-subset(Full103,is.na(Full103$Added))
Full10A<-Full10A[,c(1,2,4)]
Full10B<-subset(Full103,!is.na(Full103$Added))
Full10B<-Full10B[,c(3,2,4)]
names(Full10A)<-c("Company","Subsidiaries","Added")
names(Full10B)<-c("Company","Subsidiaries","Added")
Full10<-bind_rows(Full10A,Full10B)

#Remove duplicate GUO-subsidiary combinations
Full10$test<-paste(Full10$Company,Full10$Subsidiaries,sep=" ")
Full10<-Full10[!duplicated(Full10$test),]
Full10<-subset(Full10,!is.na(Full10$Subsidiaries))
Full10[4]<-NULL
Full10<-Full10[complete.cases(Full10[,1:2]), ]
Full10B<-Full10[1:2]

#Add to the subsidiary data
Full103a[3]<-NULL
names(Subs11)<-c("Company","Subsidiaries")
Subs10<-left_join(Subs11,Full103a,by="Subsidiaries",na_matches="never")
Subs10<-subset(Subs10,is.na(Subs10$Company.y))
Subs10[3]<-NULL
names(Full103a)<-c("Companies","Subs")
names(Subs10)<-c("Companies","Subs")
Subs10<-bind_rows(Subs10,Full103a)

#Remove duplicates
Subs10$test<-paste(Subs10$Companies,Subs10$Subs,sep=" ")
Subs10<-Subs10[!duplicated(Subs10$test),]
Subs10[3]<-NULL
Subs10<-Subs10[complete.cases(Subs10[,1:2]), ]

#Remove unnecessary files
rm(Full103,Deals20103,Full103a,Full103x,Full103y,Full103z,Full10A,Deals_data,Deals_data1)








### Use the 2010 ownership structure as a basis for a reverse ownership connection (to account for deal that were not included before)

##Ownership 2011
names(Subs10) <- c("Check2","Subs")
##Merge by subsidiaries
Deals20111<-Deals2011[,c(1,1,2,3,7)]
names(Deals20111)<-c("Subsidiaries","TargetBvDID111","AcqBvDID111","VendBvDID111","Added")
Full111<-left_join(Full10B,Deals20111,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full111$TargetBvDID111)))

#Check if the acquiror of the target may still be in the ownership group - if not, delete from the GUO
Full111$AcqBvDID111[is.na(Full111$AcqBvDID111)] <- 0
Full111$Check<-vlookup_df(Full111$AcqBvDID111,Full111,result_column=1,lookup_column=2)
sapply(Full111, class)
Full111$Check<-unlist(Full111$Check)

#If the acquiror is not in the company - exclusion - but what about their subsidiaries? Therefore, add target BvDID for match
Full111$Check2<-ifelse(!is.na(Full111$Check) & Full111$Company!=Full111$Check,Full111$TargetBvDID111,NA)

#Match all subsidiaries of the acquired companies
Full111<-left_join(Full111,Subs10,by="Check2",na_matches="never")
length(which(!is.na(Full111$Subs)))

#Look for the matched subsidiaries in the subsidiaries column and add the acquiror BvDID
Full111$Check3<-vlookup_df(Full111$Subsidiaries,Full111,result_column=4,lookup_column=9)
Full111$Check3<-unlist(Full111$Check3)
sapply(Full111, class)

#Re-check if the matched acquiror of the matched target may still be in the ownership group - if not, delete from the GUO
Full111$Check4<-vlookup_df(Full111$Check3,Full111,result_column=1,lookup_column=2)
Full111$Check4<-unlist(Full111$Check4)

#Check if the new matched acquiror is not part of the GUO structure
Full111$Check5<-case_when(!is.na(Full111$Check) & Full111$Company!=Full111$Check~1,
                          !is.na(Full111$Check4) & Full111$Company!=Full111$Check4~1)
Full111$Check5[is.na(Full111$Check5)] <- 0

#Check if GUO was acquired - exclude all companies
Full111$Check6<-case_when(Full111$TargetBvDID111==Full111$Company~1,
                          Full111$TargetBvDID111!=Full111$Company~0,
                          is.na(Full111$TargetBvDID111)~0)
Full111$Check7<-with (Full111,ave(Full111$Check6,Full111$Company, FUN=sum))

#Exclusion of all acquired GUOs and related companies (Check6 & 7)
Full111$Check8<-ifelse(Full111$Check7==0,Full111$Check5,Full111$Check7)
Full111$Check8[is.na(Full111$Check8)] <- 0
Full111w<-subset(Full111,Full111$Check8>1.1)
Full111x<-subset(Full111,Full111$Check8==1)
Full111<-subset(Full111,Full111$Check8<0.9)

#Add the subsidiaries
Full111w1<-Full111w[c(4,3,6)]
names(Full111w1)<-c("Subsidiaries","Subs","Added")
Full111x1<-Full111x[c(4,3,6)]
names(Full111x1)<-c("Subsidiaries","Subs","Added")
Full111x2<-Full111x[c(4,9,6)]
names(Full111x2)<-c("Subsidiaries","Subs","Added")
Full111a<-bind_rows(Full111w1,Full111x1,Full111x2)
Full111[3:5]<-NULL
Full111[4:12]<-NULL
Full111<-left_join(Full111,Full111a,by="Subsidiaries",na_matches="never")
Full111a<-subset(Full111a,!is.na(Full111a$Added))
Full111a<-Full111[c(1,4,5)]
Full111[4:5]<-NULL
names(Full111)<-c("Company","Subsidiaries","Added")
names(Full111a)<-c("Company","Subsidiaries","Added")
Full111<-bind_rows(Full111,Full111a)
Full111<-Full111[complete.cases(Full111[,1:2]), ]

##Change the list of subsidiaries for the next match
#Delete
Full111y<-Full111w[,c(2,12)]
Full111y[2]<-1
names(Full111y)<-c("Subs","Delete")
Subs11<-left_join(Subs10,Full111y,by="Subs",na_matches="never")
Subs11<-Subs11[is.na(Subs11$Delete),]
Subs11[3]<-NULL
Full111y<-Full111x[,c(2,12)]
Full111y[2]<-1
names(Full111y)<-c("Subs","Delete")
Subs11<-left_join(Subs11,Full111y,by="Subs",na_matches="never")
Subs11<-Subs11[is.na(Subs11$Delete),]

#Add
Full111yy<-Full111w[,c(10,2,12)]
names(Full111yy)<-c("Check2","Subs","Delete")
Full111y<-Full111w[,c(4,2,12)]
names(Full111y)<-c("Check2","Subs","Delete")
Full111z<-Full111x[,c(4,2,12)]
names(Full111z)<-c("Check2","Subs","Delete")
names(Subs11)<-c("Check2","Subs","Delete")
Subs11<-bind_rows(Subs11,Full111yy,Full111y,Full111z)

#Remove duplicates
Subs11$test<-paste(Subs11$Check2,Subs11$Subs,sep=" ")
Subs11<-Subs11[!duplicated(Subs11$test),]
Subs11[3:4]<-NULL


##Merge by Acquiror
Deals20112<-Deals2011[,c(2,1,2,3,7)]
names(Deals20112)<-c("Subsidiaries","TargetBvDID112","AcqBvDID112","VendBvDID112","Added")
Full112<-left_join(Full10B,Deals20112,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full112$TargetBvDID112)))

#Add the subsidiaries of acquired targets
names(Subs11)<-c("TargetBvDID112","Subs")
Full112<-left_join(Full112,Subs11,by="TargetBvDID112",na_matches="never")
length(which(!is.na(Full112$Subs)))

#Lookup the GUOs of target companies
Full112$Check<-vlookup_df(Full112$TargetBvDID112,Full112,result_column=1,lookup_column=2)
Full112$Check<-unlist(Full112$Check)

#If GUO of acquired companies is not the same as acquring GUO - add
Full112$Check2<-ifelse(Full112$Check!=Full112$Company | is.na(Full112$Check),Full112$TargetBvDID112,NA)

#Lookup the GUOs of subsidiaries
Full112$Check3<-vlookup_df(Full112$Subsidiaries,Full112,result_column=1,lookup_column=7)
Full112$Check3<-unlist(Full112$Check3)
Full112$Check4<-vlookup_df(Full112$Subsidiaries,Full112,result_column=4,lookup_column=7) # column 4 for acquiror
Full112$Check4<-unlist(Full112$Check4)
Full112$Check5<-ifelse(Full112$Check3!=Full112$Company,1,NA)
Full112$Check5<-unlist(Full112$Check5)
Full112$Delete1<-ifelse(!is.na(Full112$Check5)|!is.na(Full112$Check2),1,NA)
Full112$Delete2<-ifelse(!is.na(Full112$Check3) & Full112$Company==Full112$Subsidiaries,2,NA)
Full112$Delete2[is.na(Full112$Delete2)] <- 0
Full112$Delete3<-with (Full112,ave(Full112$Delete2,Full112$Company, FUN=sum))
Full112$Delete<-ifelse(Full112$Delete3>1.1,Full112$Delete3,Full112$Delete1)

#Delete File
Full112w<-subset(Full112,!is.na(Full112$Delete))
Full112w1<-Full112w[,c(2,2)]
names(Full112w1)<-c("Subsidiaries","Deletex")
Full112w2<-Full112w[,c(7,7)]
names(Full112w2)<-c("Subsidiaries","Deletex")
Full112w<-bind_rows(Full112w1,Full112w2)
Full112w<-Full112w[complete.cases(Full112w[,1:2]), ]

#Add File
Full112x<-subset(Full112,Full112$Delete>0.9)
Full112x1<-Full112x[,c(11,2,16)]                          #Acquiror
Full112x1<-Full112x1[complete.cases(Full112x1[,1:2]), ]
Full112x2<-Full112x[,c(10,2,16)]
Full112x2<-Full112x2[complete.cases(Full112x2[,1:2]), ]
Full112x3<-Full112x[,c(4,3,16)]
Full112x3<-Full112x3[complete.cases(Full112x3[,1:2]), ]
names(Full112x1)<-c("Company","Subsidiaries","Added")
names(Full112x2)<-c("Company","Subsidiaries","Added")
names(Full112x3)<-c("Company","Subsidiaries","Added")
Full112x1$Added<-1
Full112x2$Added<-1
Full112x3$Added<-1
Full112x<-bind_rows(Full112x2,Full112x3)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full112<-left_join(Full111,Full112x,by="Subsidiaries",na_matches="never")
Full112<-subset(Full112,!is.na(Full112$Added.y))
Full112[3:4]<-NULL
names(Full112)<-c("Company","Subsidiaries","Added")
Full112$test<-paste(Full112$Companies,Full112$Subsidiaries,sep=" ")
Full112<-Full112[!duplicated(Full112$test),]
Full112[4]<-NULL
Full11<-bind_rows(Full111,Full112)
Full11$Added<-ifelse(!is.na(Full11$Added),3,NA)
Full11B<-Full11[1:2]

##Change the list of subsidiaries for the next match
#Remove
Full112w1[3]<-NULL
names(Full112w1)<-c("Subs","Delete")
names(Subs11)<-c("Companies","Subs")
Subs11<-left_join(Subs11,Full112w1,by="Subs",na_matches="never")
Subs11<-Subs11[is.na(Subs11$Delete),]
Subs11[3]<-NULL
Full112w2[3]<-NULL
names(Full112w2)<-c("Subs","Delete")
names(Subs11)<-c("Companies","Subs")
Subs11<-left_join(Subs11,Full112w2,by="Subs",na_matches="never")
Subs11<-Subs11[is.na(Subs11$Delete),]
Subs11[3]<-NULL

#Add
Full112x1[3]<-NULL
names(Full112x1)<-c("Companies","Subs")
Full112x2[3]<-NULL
names(Full112x2)<-c("Companies","Subs")
Full112x3[3]<-NULL
names(Full112x3)<-c("Companies","Subs")
Subs11<-bind_rows(Subs11,Full112x1,Full112x2,Full112x3)

#Remove duplicates
Subs11$test<-paste(Subs11$Companies,Subs11$Subs,sep=" ")
Subs11<-Subs11[!duplicated(Subs11$test),]
Subs11[3]<-NULL


##Merge by vendor and if the GUO sold targets in 2012, they are added to the ownership structure of 2011
Deals20113<-Deals2012[,c(3,1,2,3,7)]
names(Deals20113)<-c("Subsidiaries","TargetBvDID113","AcqBvDID113","VendBvDID113","Added")
Full113x<-left_join(Full11B,Deals20113,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full113x$TargetBvDID113)))
names(Subs11)<-c("TargetBvDID113","Subs")
Full113<-left_join(Full113x,Subs11,by="TargetBvDID113",na_matches="never")
length(which(!is.na(Full113$Subs)))
Full113x<-subset(Full113,!is.na(Full113$VendBvDID113))
Full113y<-Full113x[,c(5,3,6)]
names(Full113y)<-c("Company","Subsidiaries","Added")
Full113z<-Full113x[,c(5,7,6)]
names(Full113z)<-c("Company","Subsidiaries","Added")
Full113<-bind_rows(Full113y,Full113z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full113<-left_join(Full11,Full113,by="Subsidiaries",na_matches="never")
Full113<-subset(Full113,!is.na(Full113$Added.y))
Full113<-Full113[,c(1,2,5)]
names(Full113)<-c("Company","Subsidiaries","Added")
Full113$Added<-0
Full11<-bind_rows(Full11,Full113)

#Remove duplicate GUO-subsidiary combinations
Full11$test<-paste(Full11$Company,Full11$Subsidiaries,sep=" ")
Full11<-Full11[!duplicated(Full11$test),]
Full11<-subset(Full11,!is.na(Full11$Subsidiaries))
Full11[4]<-NULL
Full11<-Full11[complete.cases(Full11[,1:2]), ]
Full11B<-Full11[1:2]

#Add to the subsidiary data
Full113y[3]<-NULL
names(Full113y)<-c("Companies","Subs")
Full113z[3]<-NULL
names(Full113z)<-c("Companies","Subs")
names(Subs11)<-c("Companies","Subs")
Subs11<-bind_rows(Subs11,Full113y,Full113z)

#Remove duplicates
Subs11$test<-paste(Subs11$Companies,Subs11$Subs,sep=" ")
Subs11<-Subs11[!duplicated(Subs11$test),]
Subs11[3]<-NULL
Subs11<-Subs11[complete.cases(Subs11[,1:2]), ]

#Remove unnecessary files
rm(Full111,Full111x,Full111y,Full112,Full112x,Full112y,Full112z,Deals20111,Deals20112,Deals20113,Full113,
   Full113x,Full113y,Full113z,Full11A,Deals_data1,DealsSubs,Full111a,Full111w,Full111w1,Full111x1,Full111x2,
   Full111w2,Full111yy,Full111z,Full111w,Full112w,Full112w1,Full112w2,Full112x1,Full112x2,Full112x3)


##Ownership 2012
names(Subs11) <- c("Check2","Subs")
##Merge by subsidiaries
Deals20121<-Deals2012[,c(1,1,2,3,7)]
names(Deals20121)<-c("Subsidiaries","TargetBvDID121","AcqBvDID121","VendBvDID121","Added")
Full121<-left_join(Full11B,Deals20121,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full121$TargetBvDID121)))

#Check if the acquiror of the target may still be in the ownership group - if not, delete from the GUO
Full121$AcqBvDID121[is.na(Full121$AcqBvDID121)] <- 0
Full121$Check<-vlookup_df(Full121$AcqBvDID121,Full121,result_column=1,lookup_column=2)
sapply(Full121, class)
Full121$Check<-unlist(Full121$Check)

#If the acquiror is not in the company - exclusion - but what about their subsidiaries? Therefore, add target BvDID for match
Full121$Check2<-ifelse(!is.na(Full121$Check) & Full121$Company!=Full121$Check,Full121$TargetBvDID121,NA)

#Match all subsidiaries of the acquired companies
Full121<-left_join(Full121,Subs11,by="Check2",na_matches="never")
length(which(!is.na(Full121$Subs)))

#Look for the matched subsidiaries in the subsidiaries column and add the acquiror BvDID
Full121$Check3<-vlookup_df(Full121$Subsidiaries,Full121,result_column=4,lookup_column=9)
Full121$Check3<-unlist(Full121$Check3)
sapply(Full121, class)

#Re-check if the matched acquiror of the matched target may still be in the ownership group - if not, delete from the GUO
Full121$Check4<-vlookup_df(Full121$Check3,Full121,result_column=1,lookup_column=2)
Full121$Check4<-unlist(Full121$Check4)

#Check if the new matched acquiror is not part of the GUO structure
Full121$Check5<-case_when(!is.na(Full121$Check) & Full121$Company!=Full121$Check~1,
                          !is.na(Full121$Check4) & Full121$Company!=Full121$Check4~1)
Full121$Check5[is.na(Full121$Check5)] <- 0

#Check if GUO was acquired - exclude all companies
Full121$Check6<-case_when(Full121$TargetBvDID121==Full121$Company~1,
                          Full121$TargetBvDID121!=Full121$Company~0,
                          is.na(Full121$TargetBvDID121)~0)
Full121$Check7<-with (Full121,ave(Full121$Check6,Full121$Company, FUN=sum))

#Exclusion of all acquired GUOs and related companies (Check6 & 7)
Full121$Check8<-ifelse(Full121$Check7==0,Full121$Check5,Full121$Check7)
Full121$Check8[is.na(Full121$Check8)] <- 0
Full121w<-subset(Full121,Full121$Check8>1.1)
Full121x<-subset(Full121,Full121$Check8==1)
Full121<-subset(Full121,Full121$Check8<0.9)

#Add the subsidiaries
Full121w1<-Full121w[c(4,3,6)]
names(Full121w1)<-c("Subsidiaries","Subs","Added")
Full121x1<-Full121x[c(4,3,6)]
names(Full121x1)<-c("Subsidiaries","Subs","Added")
Full121x2<-Full121x[c(4,9,6)]
names(Full121x2)<-c("Subsidiaries","Subs","Added")
Full121a<-bind_rows(Full121w1,Full121x1,Full121x2)
Full121[3:5]<-NULL
Full121[4:12]<-NULL
Full121<-left_join(Full121,Full121a,by="Subsidiaries",na_matches="never")
Full121a<-subset(Full121a,!is.na(Full121a$Added))
Full121a<-Full121[c(1,4,5)]
Full121[4:5]<-NULL
names(Full121)<-c("Company","Subsidiaries","Added")
names(Full121a)<-c("Company","Subsidiaries","Added")
Full121<-bind_rows(Full121,Full121a)
Full121<-Full121[complete.cases(Full121[,1:2]), ]

##Change the list of subsidiaries for the next match
#Delete
Full121y<-Full121w[,c(2,12)]
Full121y[2]<-1
names(Full121y)<-c("Subs","Delete")
Subs12<-left_join(Subs11,Full121y,by="Subs",na_matches="never")
Subs12<-Subs12[is.na(Subs12$Delete),]
Subs12[3]<-NULL
Full121y<-Full121x[,c(2,12)]
Full121y[2]<-1
names(Full121y)<-c("Subs","Delete")
Subs12<-left_join(Subs12,Full121y,by="Subs",na_matches="never")
Subs12<-Subs12[is.na(Subs12$Delete),]

#Add
Full121yy<-Full121w[,c(10,2,12)]
names(Full121yy)<-c("Check2","Subs","Delete")
Full121y<-Full121w[,c(4,2,12)]
names(Full121y)<-c("Check2","Subs","Delete")
Full121z<-Full121x[,c(4,2,12)]
names(Full121z)<-c("Check2","Subs","Delete")
names(Subs12)<-c("Check2","Subs","Delete")
Subs12<-bind_rows(Subs12,Full121yy,Full121y,Full121z)

#Remove duplicates
Subs12$test<-paste(Subs12$Check2,Subs12$Subs,sep=" ")
Subs12<-Subs12[!duplicated(Subs12$test),]
Subs12[3:4]<-NULL


##Merge by Acquiror
Deals20122<-Deals2012[,c(2,1,2,3,7)]
names(Deals20122)<-c("Subsidiaries","TargetBvDID122","AcqBvDID122","VendBvDID122","Added")
Full122<-left_join(Full11B,Deals20122,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full122$TargetBvDID122)))

#Add the subsidiaries of acquired targets
names(Subs12)<-c("TargetBvDID122","Subs")
Full122<-left_join(Full122,Subs12,by="TargetBvDID122",na_matches="never")
length(which(!is.na(Full122$Subs)))

#Lookup the GUOs of target companies
Full122$Check<-vlookup_df(Full122$TargetBvDID122,Full122,result_column=1,lookup_column=2)
Full122$Check<-unlist(Full122$Check)

#If GUO of acquired companies is not the same as acquring GUO - add
Full122$Check2<-ifelse(Full122$Check!=Full122$Company | is.na(Full122$Check),Full122$TargetBvDID122,NA)

#Lookup the GUOs of subsidiaries
Full122$Check3<-vlookup_df(Full122$Subsidiaries,Full122,result_column=1,lookup_column=7)
Full122$Check3<-unlist(Full122$Check3)
Full122$Check4<-vlookup_df(Full122$Subsidiaries,Full122,result_column=4,lookup_column=7) # column 4 for acquiror
Full122$Check4<-unlist(Full122$Check4)
Full122$Check5<-ifelse(Full122$Check3!=Full122$Company,1,NA)
Full122$Check5<-unlist(Full122$Check5)
Full122$Delete1<-ifelse(!is.na(Full122$Check5)|!is.na(Full122$Check2),1,NA)
Full122$Delete2<-ifelse(!is.na(Full122$Check3) & Full122$Company==Full122$Subsidiaries,2,NA)
Full122$Delete2[is.na(Full122$Delete2)] <- 0
Full122$Delete3<-with (Full122,ave(Full122$Delete2,Full122$Company, FUN=sum))
Full122$Delete<-ifelse(Full122$Delete3>1.1,Full122$Delete3,Full122$Delete1)

#Delete File
Full122w<-subset(Full122,!is.na(Full122$Delete))
Full122w1<-Full122w[,c(2,2)]
names(Full122w1)<-c("Subsidiaries","Deletex")
Full122w2<-Full122w[,c(7,7)]
names(Full122w2)<-c("Subsidiaries","Deletex")
Full122w<-bind_rows(Full122w1,Full122w2)
Full122w<-Full122w[complete.cases(Full122w[,1:2]), ]

#Add File
Full122x<-subset(Full122,Full122$Delete>0.9)
Full122x1<-Full122x[,c(11,2,16)]                          #Acquiror
Full122x1<-Full122x1[complete.cases(Full122x1[,1:2]), ]
Full122x2<-Full122x[,c(10,2,16)]
Full122x2<-Full122x2[complete.cases(Full122x2[,1:2]), ]
Full122x3<-Full122x[,c(4,3,16)]
Full122x3<-Full122x3[complete.cases(Full122x3[,1:2]), ]
names(Full122x1)<-c("Company","Subsidiaries","Added")
names(Full122x2)<-c("Company","Subsidiaries","Added")
names(Full122x3)<-c("Company","Subsidiaries","Added")
Full122x1$Added<-1
Full122x2$Added<-1
Full122x3$Added<-1
Full122x<-bind_rows(Full122x2,Full122x3)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full122<-left_join(Full121,Full122x,by="Subsidiaries",na_matches="never")
Full122<-subset(Full122,!is.na(Full122$Added.y))
Full122[3:4]<-NULL
names(Full122)<-c("Company","Subsidiaries","Added")
Full122$test<-paste(Full122$Companies,Full122$Subsidiaries,sep=" ")
Full122<-Full122[!duplicated(Full122$test),]
Full122[4]<-NULL
Full12<-bind_rows(Full121,Full122)
Full12$Added<-ifelse(!is.na(Full12$Added),3,NA)
Full12B<-Full12[1:2]

##Change the list of subsidiaries for the next match
#Remove
Full122w1[3]<-NULL
names(Full122w1)<-c("Subs","Delete")
names(Subs12)<-c("Companies","Subs")
Subs12<-left_join(Subs12,Full122w1,by="Subs",na_matches="never")
Subs12<-Subs12[is.na(Subs12$Delete),]
Subs12[3]<-NULL
Full122w2[3]<-NULL
names(Full122w2)<-c("Subs","Delete")
names(Subs12)<-c("Companies","Subs")
Subs12<-left_join(Subs12,Full122w2,by="Subs",na_matches="never")
Subs12<-Subs12[is.na(Subs12$Delete),]
Subs12[3]<-NULL

#Add
Full122x1[3]<-NULL
names(Full122x1)<-c("Companies","Subs")
Full122x2[3]<-NULL
names(Full122x2)<-c("Companies","Subs")
Full122x3[3]<-NULL
names(Full122x3)<-c("Companies","Subs")
Subs12<-bind_rows(Subs12,Full122x1,Full122x2,Full122x3)

#Remove duplicates
Subs12$test<-paste(Subs12$Companies,Subs12$Subs,sep=" ")
Subs12<-Subs12[!duplicated(Subs12$test),]
Subs12[3]<-NULL


#Merge by vendor and if the GUO sold targets in 2011, they are added to the ownership structure of 2012
Deals20123<-Deals2013[,c(3,1,2,3,7)]
names(Deals20123)<-c("Subsidiaries","TargetBvDID123","AcqBvDID123","VendBvDID123","Added")
Full123x<-left_join(Full12B,Deals20123,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full123x$TargetBvDID123)))
names(Subs12)<-c("TargetBvDID123","Subs")
Full123<-left_join(Full123x,Subs12,by="TargetBvDID123",na_matches="never")
length(which(!is.na(Full123$Subs)))
Full123x<-subset(Full123,!is.na(Full123$VendBvDID123))
Full123y<-Full123x[,c(5,3,6)]
names(Full123y)<-c("Company","Subsidiaries","Added")
Full123z<-Full123x[,c(5,7,6)]
names(Full123z)<-c("Company","Subsidiaries","Added")
Full123<-bind_rows(Full123y,Full123z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full123<-left_join(Full12,Full123,by="Subsidiaries",na_matches="never")
Full123<-subset(Full123,!is.na(Full123$Added.y))
Full123<-Full123[,c(1,2,5)]
names(Full123)<-c("Company","Subsidiaries","Added")
Full12<-bind_rows(Full12,Full123)

#Remove duplicate GUO-subsidiary combinations
Full12$test<-paste(Full12$Company,Full12$Subsidiaries,sep=" ")
Full12<-Full12[!duplicated(Full12$test),]
Full12<-subset(Full12,!is.na(Full12$Subsidiaries))
Full12[4]<-NULL
Full12<-Full12[complete.cases(Full12[,1:2]), ]
Full12B<-Full12[1:2]

#Add to the subsidiary data
Full123y[3]<-NULL
names(Full123y)<-c("Companies","Subs")
Full123z[3]<-NULL
names(Full123z)<-c("Companies","Subs")
names(Subs12)<-c("Companies","Subs")
Subs12<-bind_rows(Subs12,Full123y,Full123z)

#Remove duplicates
Subs12$test<-paste(Subs12$Companies,Subs12$Subs,sep=" ")
Subs12<-Subs12[!duplicated(Subs12$test),]
Subs12[3]<-NULL
Subs12<-Subs12[complete.cases(Subs12[,1:2]), ]

#Remove unnecessary files
rm(Full121,Full121x,Full121y,Full122,Full122x,Full122y,Full122z,Deals20121,Deals20122,Deals20123,Full123,
   Full123x,Full123y,Full123z,Full12A,Deals_data1,DealsSubs,Full121a,Full121w,Full121w1,Full121x1,Full121x2,
   Full121w2,Full121yy,Full121z,Full121w,Full122w,Full122w1,Full122w2,Full122x1,Full122x2,Full122x3)


##Ownership 2013
names(Subs12) <- c("Check2","Subs")
##Merge by subsidiaries
Deals20131<-Deals2013[,c(1,1,2,3,7)]
names(Deals20131)<-c("Subsidiaries","TargetBvDID131","AcqBvDID131","VendBvDID131","Added")
Full131<-left_join(Full12B,Deals20131,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full131$TargetBvDID131)))

#Check if the acquiror of the target may still be in the ownership group - if not, delete from the GUO
Full131$AcqBvDID131[is.na(Full131$AcqBvDID131)] <- 0
Full131$Check<-vlookup_df(Full131$AcqBvDID131,Full131,result_column=1,lookup_column=2)
sapply(Full131, class)
Full131$Check<-unlist(Full131$Check)

#If the acquiror is not in the company - exclusion - but what about their subsidiaries? Therefore, add target BvDID for match
Full131$Check2<-ifelse(!is.na(Full131$Check) & Full131$Company!=Full131$Check,Full131$TargetBvDID131,NA)

#Match all subsidiaries of the acquired companies
Full131<-left_join(Full131,Subs12,by="Check2",na_matches="never")
length(which(!is.na(Full131$Subs)))

#Look for the matched subsidiaries in the subsidiaries column and add the acquiror BvDID
Full131$Check3<-vlookup_df(Full131$Subsidiaries,Full131,result_column=4,lookup_column=9)
Full131$Check3<-unlist(Full131$Check3)
sapply(Full131, class)

#Re-check if the matched acquiror of the matched target may still be in the ownership group - if not, delete from the GUO
Full131$Check4<-vlookup_df(Full131$Check3,Full131,result_column=1,lookup_column=2)
Full131$Check4<-unlist(Full131$Check4)

#Check if the new matched acquiror is not part of the GUO structure
Full131$Check5<-case_when(!is.na(Full131$Check) & Full131$Company!=Full131$Check~1,
                          !is.na(Full131$Check4) & Full131$Company!=Full131$Check4~1)
Full131$Check5[is.na(Full131$Check5)] <- 0

#Check if GUO was acquired - exclude all companies
Full131$Check6<-case_when(Full131$TargetBvDID131==Full131$Company~1,
                          Full131$TargetBvDID131!=Full131$Company~0,
                          is.na(Full131$TargetBvDID131)~0)
Full131$Check7<-with (Full131,ave(Full131$Check6,Full131$Company, FUN=sum))

#Exclusion of all acquired GUOs and related companies (Check6 & 7)
Full131$Check8<-ifelse(Full131$Check7==0,Full131$Check5,Full131$Check7)
Full131$Check8[is.na(Full131$Check8)] <- 0
Full131w<-subset(Full131,Full131$Check8>1.1)
Full131x<-subset(Full131,Full131$Check8==1)
Full131<-subset(Full131,Full131$Check8<0.9)

#Add the subsidiaries
Full131w1<-Full131w[c(4,3,6)]
names(Full131w1)<-c("Subsidiaries","Subs","Added")
Full131x1<-Full131x[c(4,3,6)]
names(Full131x1)<-c("Subsidiaries","Subs","Added")
Full131x2<-Full131x[c(4,9,6)]
names(Full131x2)<-c("Subsidiaries","Subs","Added")
Full131a<-bind_rows(Full131w1,Full131x1,Full131x2)
Full131[3:5]<-NULL
Full131[4:12]<-NULL
Full131<-left_join(Full131,Full131a,by="Subsidiaries",na_matches="never")
Full131a<-subset(Full131a,!is.na(Full131a$Added))
Full131a<-Full131[c(1,4,5)]
Full131[4:5]<-NULL
names(Full131)<-c("Company","Subsidiaries","Added")
names(Full131a)<-c("Company","Subsidiaries","Added")
Full131<-bind_rows(Full131,Full131a)
Full131<-Full131[complete.cases(Full131[,1:2]), ]

##Change the list of subsidiaries for the next match
#Delete
Full131y<-Full131w[,c(2,12)]
Full131y[2]<-1
names(Full131y)<-c("Subs","Delete")
Subs13<-left_join(Subs12,Full131y,by="Subs",na_matches="never")
Subs13<-Subs13[is.na(Subs13$Delete),]
Subs13[3]<-NULL
Full131y<-Full131x[,c(2,12)]
Full131y[2]<-1
names(Full131y)<-c("Subs","Delete")
Subs13<-left_join(Subs13,Full131y,by="Subs",na_matches="never")
Subs13<-Subs13[is.na(Subs13$Delete),]

#Add
Full131yy<-Full131w[,c(10,2,12)]
names(Full131yy)<-c("Check2","Subs","Delete")
Full131y<-Full131w[,c(4,2,12)]
names(Full131y)<-c("Check2","Subs","Delete")
Full131z<-Full131x[,c(4,2,12)]
names(Full131z)<-c("Check2","Subs","Delete")
names(Subs13)<-c("Check2","Subs","Delete")
Subs13<-bind_rows(Subs13,Full131yy,Full131y,Full131z)

#Remove duplicates
Subs13$test<-paste(Subs13$Check2,Subs13$Subs,sep=" ")
Subs13<-Subs13[!duplicated(Subs13$test),]
Subs13[3:4]<-NULL


##Merge by Acquiror
Deals20132<-Deals2013[,c(2,1,2,3,7)]
names(Deals20132)<-c("Subsidiaries","TargetBvDID132","AcqBvDID132","VendBvDID132","Added")
Full132<-left_join(Full12B,Deals20132,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full132$TargetBvDID132)))

#Add the subsidiaries of acquired targets
names(Subs13)<-c("TargetBvDID132","Subs")
Full132<-left_join(Full132,Subs13,by="TargetBvDID132",na_matches="never")
length(which(!is.na(Full132$Subs)))

#Lookup the GUOs of target companies
Full132$Check<-vlookup_df(Full132$TargetBvDID132,Full132,result_column=1,lookup_column=2)
Full132$Check<-unlist(Full132$Check)

#If GUO of acquired companies is not the same as acquring GUO - add
Full132$Check2<-ifelse(Full132$Check!=Full132$Company | is.na(Full132$Check),Full132$TargetBvDID132,NA)

#Lookup the GUOs of subsidiaries
Full132$Check3<-vlookup_df(Full132$Subsidiaries,Full132,result_column=1,lookup_column=7)
Full132$Check3<-unlist(Full132$Check3)
Full132$Check4<-vlookup_df(Full132$Subsidiaries,Full132,result_column=4,lookup_column=7) # column 4 for acquiror
Full132$Check4<-unlist(Full132$Check4)
Full132$Check5<-ifelse(Full132$Check3!=Full132$Company,1,NA)
Full132$Check5<-unlist(Full132$Check5)
Full132$Delete1<-ifelse(!is.na(Full132$Check5)|!is.na(Full132$Check2),1,NA)
Full132$Delete2<-ifelse(!is.na(Full132$Check3) & Full132$Company==Full132$Subsidiaries,2,NA)
Full132$Delete2[is.na(Full132$Delete2)] <- 0
Full132$Delete3<-with (Full132,ave(Full132$Delete2,Full132$Company, FUN=sum))
Full132$Delete<-ifelse(Full132$Delete3>1.1,Full132$Delete3,Full132$Delete1)

#Delete File
Full132w<-subset(Full132,!is.na(Full132$Delete))
Full132w1<-Full132w[,c(2,2)]
names(Full132w1)<-c("Subsidiaries","Deletex")
Full132w2<-Full132w[,c(7,7)]
names(Full132w2)<-c("Subsidiaries","Deletex")
Full132w<-bind_rows(Full132w1,Full132w2)
Full132w<-Full132w[complete.cases(Full132w[,1:2]), ]

#Add File
Full132x<-subset(Full132,Full132$Delete>0.9)
Full132x1<-Full132x[,c(11,2,16)]                          #Acquiror
Full132x1<-Full132x1[complete.cases(Full132x1[,1:2]), ]
Full132x2<-Full132x[,c(10,2,16)]
Full132x2<-Full132x2[complete.cases(Full132x2[,1:2]), ]
Full132x3<-Full132x[,c(4,3,16)]
Full132x3<-Full132x3[complete.cases(Full132x3[,1:2]), ]
names(Full132x1)<-c("Company","Subsidiaries","Added")
names(Full132x2)<-c("Company","Subsidiaries","Added")
names(Full132x3)<-c("Company","Subsidiaries","Added")
Full132x1$Added<-1
Full132x2$Added<-1
Full132x3$Added<-1
Full132x<-bind_rows(Full132x2,Full132x3)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full132<-left_join(Full131,Full132x,by="Subsidiaries",na_matches="never")
Full132<-subset(Full132,!is.na(Full132$Added.y))
Full132[3:4]<-NULL
names(Full132)<-c("Company","Subsidiaries","Added")
Full132$test<-paste(Full132$Companies,Full132$Subsidiaries,sep=" ")
Full132<-Full132[!duplicated(Full132$test),]
Full132[4]<-NULL
Full13<-bind_rows(Full131,Full132)
Full13$Added<-ifelse(!is.na(Full13$Added),3,NA)
Full13B<-Full13[1:2]

##Change the list of subsidiaries for the next match
#Remove
Full132w1[3]<-NULL
names(Full132w1)<-c("Subs","Delete")
names(Subs13)<-c("Companies","Subs")
Subs13<-left_join(Subs13,Full132w1,by="Subs",na_matches="never")
Subs13<-Subs13[is.na(Subs13$Delete),]
Subs13[3]<-NULL
Full132w2[3]<-NULL
names(Full132w2)<-c("Subs","Delete")
names(Subs13)<-c("Companies","Subs")
Subs13<-left_join(Subs13,Full132w2,by="Subs",na_matches="never")
Subs13<-Subs13[is.na(Subs13$Delete),]
Subs13[3]<-NULL

#Add
Full132x1[3]<-NULL
names(Full132x1)<-c("Companies","Subs")
Full132x2[3]<-NULL
names(Full132x2)<-c("Companies","Subs")
Full132x3[3]<-NULL
names(Full132x3)<-c("Companies","Subs")
Subs13<-bind_rows(Subs13,Full132x1,Full132x2,Full132x3)

#Remove duplicates
Subs13$test<-paste(Subs13$Companies,Subs13$Subs,sep=" ")
Subs13<-Subs13[!duplicated(Subs13$test),]
Subs13[3]<-NULL


#Merge by vendor and if the GUO sold targets in 2012, they are added to the ownership structure of 2013
Deals20133<-Deals2014[,c(3,1,2,3,7)]
names(Deals20133)<-c("Subsidiaries","TargetBvDID133","AcqBvDID133","VendBvDID133","Added")
Full133x<-left_join(Full13B,Deals20133,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full133x$TargetBvDID133)))
names(Subs13)<-c("TargetBvDID133","Subs")
Full133<-left_join(Full133x,Subs13,by="TargetBvDID133",na_matches="never")
length(which(!is.na(Full133$Subs)))
Full133x<-subset(Full133,!is.na(Full133$VendBvDID133))
Full133y<-Full133x[,c(5,3,6)]
names(Full133y)<-c("Company","Subsidiaries","Added")
Full133z<-Full133x[,c(5,7,6)]
names(Full133z)<-c("Company","Subsidiaries","Added")
Full133<-bind_rows(Full133y,Full133z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full133<-left_join(Full13,Full133,by="Subsidiaries",na_matches="never")
Full133<-subset(Full133,!is.na(Full133$Added.y))
Full133<-Full133[,c(1,2,5)]
names(Full133)<-c("Company","Subsidiaries","Added")
Full13<-bind_rows(Full13,Full133)

#Remove duplicate GUO-subsidiary combinations
Full13$test<-paste(Full13$Company,Full13$Subsidiaries,sep=" ")
Full13<-Full13[!duplicated(Full13$test),]
Full13<-subset(Full13,!is.na(Full13$Subsidiaries))
Full13[4]<-NULL
Full13<-Full13[complete.cases(Full13[,1:2]), ]
Full13B<-Full13[1:2]

#Add to the subsidiary data
Full133y[3]<-NULL
names(Full133y)<-c("Companies","Subs")
Full133z[3]<-NULL
names(Full133z)<-c("Companies","Subs")
names(Subs13)<-c("Companies","Subs")
Subs13<-bind_rows(Subs13,Full133y,Full133z)

#Remove duplicates
Subs13$test<-paste(Subs13$Companies,Subs13$Subs,sep=" ")
Subs13<-Subs13[!duplicated(Subs13$test),]
Subs13[3]<-NULL
Subs13<-Subs13[complete.cases(Subs13[,1:2]), ]

#Remove unnecessary files
rm(Full131,Full131x,Full131y,Full132,Full132x,Full132y,Full132z,Deals20131,Deals20132,Deals20133,Full133,
   Full133x,Full133y,Full133z,Full13A,Deals_data1,DealsSubs,Full131a,Full131w,Full131w1,Full131x1,Full131x2,
   Full131w2,Full131yy,Full131z,Full131w,Full132w,Full132w1,Full132w2,Full132x1,Full132x2,Full132x3)


##Ownership 2014
names(Subs13) <- c("Check2","Subs")
##Merge by subsidiaries
Deals20141<-Deals2014[,c(1,1,2,3,7)]
names(Deals20141)<-c("Subsidiaries","TargetBvDID141","AcqBvDID141","VendBvDID141","Added")
Full141<-left_join(Full13B,Deals20141,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full141$TargetBvDID141)))

#Check if the acquiror of the target may still be in the ownership group - if not, delete from the GUO
Full141$AcqBvDID141[is.na(Full141$AcqBvDID141)] <- 0
Full141$Check<-vlookup_df(Full141$AcqBvDID141,Full141,result_column=1,lookup_column=2)
sapply(Full141, class)
Full141$Check<-unlist(Full141$Check)

#If the acquiror is not in the company - exclusion - but what about their subsidiaries? Therefore, add target BvDID for match
Full141$Check2<-ifelse(!is.na(Full141$Check) & Full141$Company!=Full141$Check,Full141$TargetBvDID141,NA)

#Match all subsidiaries of the acquired companies
Full141<-left_join(Full141,Subs13,by="Check2",na_matches="never")
length(which(!is.na(Full141$Subs)))

#Look for the matched subsidiaries in the subsidiaries column and add the acquiror BvDID
Full141$Check3<-vlookup_df(Full141$Subsidiaries,Full141,result_column=4,lookup_column=9)
Full141$Check3<-unlist(Full141$Check3)
sapply(Full141, class)

#Re-check if the matched acquiror of the matched target may still be in the ownership group - if not, delete from the GUO
Full141$Check4<-vlookup_df(Full141$Check3,Full141,result_column=1,lookup_column=2)
Full141$Check4<-unlist(Full141$Check4)

#Check if the new matched acquiror is not part of the GUO structure
Full141$Check5<-case_when(!is.na(Full141$Check) & Full141$Company!=Full141$Check~1,
                          !is.na(Full141$Check4) & Full141$Company!=Full141$Check4~1)
Full141$Check5[is.na(Full141$Check5)] <- 0

#Check if GUO was acquired - exclude all companies
Full141$Check6<-case_when(Full141$TargetBvDID141==Full141$Company~1,
                          Full141$TargetBvDID141!=Full141$Company~0,
                          is.na(Full141$TargetBvDID141)~0)
Full141$Check7<-with (Full141,ave(Full141$Check6,Full141$Company, FUN=sum))

#Exclusion of all acquired GUOs and related companies (Check6 & 7)
Full141$Check8<-ifelse(Full141$Check7==0,Full141$Check5,Full141$Check7)
Full141$Check8[is.na(Full141$Check8)] <- 0
Full141w<-subset(Full141,Full141$Check8>1.1)
Full141x<-subset(Full141,Full141$Check8==1)
Full141<-subset(Full141,Full141$Check8<0.9)

#Add the subsidiaries
Full141w1<-Full141w[c(4,3,6)]
names(Full141w1)<-c("Subsidiaries","Subs","Added")
Full141x1<-Full141x[c(4,3,6)]
names(Full141x1)<-c("Subsidiaries","Subs","Added")
Full141x2<-Full141x[c(4,9,6)]
names(Full141x2)<-c("Subsidiaries","Subs","Added")
Full141a<-bind_rows(Full141w1,Full141x1,Full141x2)
Full141[3:5]<-NULL
Full141[4:12]<-NULL
Full141<-left_join(Full141,Full141a,by="Subsidiaries",na_matches="never")
Full141a<-subset(Full141a,!is.na(Full141a$Added))
Full141a<-Full141[c(1,4,5)]
Full141[4:5]<-NULL
names(Full141)<-c("Company","Subsidiaries","Added")
names(Full141a)<-c("Company","Subsidiaries","Added")
Full141<-bind_rows(Full141,Full141a)
Full141<-Full141[complete.cases(Full141[,1:2]), ]

##Change the list of subsidiaries for the next match
#Delete
Full141y<-Full141w[,c(2,12)]
Full141y[2]<-1
names(Full141y)<-c("Subs","Delete")
Subs14<-left_join(Subs13,Full141y,by="Subs",na_matches="never")
Subs14<-Subs14[is.na(Subs14$Delete),]
Subs14[3]<-NULL
Full141y<-Full141x[,c(2,12)]
Full141y[2]<-1
names(Full141y)<-c("Subs","Delete")
Subs14<-left_join(Subs14,Full141y,by="Subs",na_matches="never")
Subs14<-Subs14[is.na(Subs14$Delete),]

#Add
Full141yy<-Full141w[,c(10,2,12)]
names(Full141yy)<-c("Check2","Subs","Delete")
Full141y<-Full141w[,c(4,2,12)]
names(Full141y)<-c("Check2","Subs","Delete")
Full141z<-Full141x[,c(4,2,12)]
names(Full141z)<-c("Check2","Subs","Delete")
names(Subs14)<-c("Check2","Subs","Delete")
Subs14<-bind_rows(Subs14,Full141yy,Full141y,Full141z)

#Remove duplicates
Subs14$test<-paste(Subs14$Check2,Subs14$Subs,sep=" ")
Subs14<-Subs14[!duplicated(Subs14$test),]
Subs14[3:4]<-NULL


##Merge by Acquiror
Deals20142<-Deals2014[,c(2,1,2,3,7)]
names(Deals20142)<-c("Subsidiaries","TargetBvDID142","AcqBvDID142","VendBvDID142","Added")
Full142<-left_join(Full13B,Deals20142,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full142$TargetBvDID142)))

#Add the subsidiaries of acquired targets
names(Subs14)<-c("TargetBvDID142","Subs")
Full142<-left_join(Full142,Subs14,by="TargetBvDID142",na_matches="never")
length(which(!is.na(Full142$Subs)))

#Lookup the GUOs of target companies
Full142$Check<-vlookup_df(Full142$TargetBvDID142,Full142,result_column=1,lookup_column=2)
Full142$Check<-unlist(Full142$Check)

#If GUO of acquired companies is not the same as acquring GUO - add
Full142$Check2<-ifelse(Full142$Check!=Full142$Company | is.na(Full142$Check),Full142$TargetBvDID142,NA)

#Lookup the GUOs of subsidiaries
Full142$Check3<-vlookup_df(Full142$Subsidiaries,Full142,result_column=1,lookup_column=7)
Full142$Check3<-unlist(Full142$Check3)
Full142$Check4<-vlookup_df(Full142$Subsidiaries,Full142,result_column=4,lookup_column=7) # column 4 for acquiror
Full142$Check4<-unlist(Full142$Check4)
Full142$Check5<-ifelse(Full142$Check3!=Full142$Company,1,NA)
Full142$Check5<-unlist(Full142$Check5)
Full142$Delete1<-ifelse(!is.na(Full142$Check5)|!is.na(Full142$Check2),1,NA)
Full142$Delete2<-ifelse(!is.na(Full142$Check3) & Full142$Company==Full142$Subsidiaries,2,NA)
Full142$Delete2[is.na(Full142$Delete2)] <- 0
Full142$Delete3<-with (Full142,ave(Full142$Delete2,Full142$Company, FUN=sum))
Full142$Delete<-ifelse(Full142$Delete3>1.1,Full142$Delete3,Full142$Delete1)

#Delete File
Full142w<-subset(Full142,!is.na(Full142$Delete))
Full142w1<-Full142w[,c(2,2)]
names(Full142w1)<-c("Subsidiaries","Deletex")
Full142w2<-Full142w[,c(7,7)]
names(Full142w2)<-c("Subsidiaries","Deletex")
Full142w<-bind_rows(Full142w1,Full142w2)
Full142w<-Full142w[complete.cases(Full142w[,1:2]),]

#Add File
Full142x<-subset(Full142,Full142$Delete>0.9)
Full142x1<-Full142x[,c(11,2,16)]                          #Acquiror
Full142x1<-Full142x1[complete.cases(Full142x1[,1:2]),]
Full142x2<-Full142x[,c(10,2,16)]
Full142x2<-Full142x2[complete.cases(Full142x2[,1:2]),]
Full142x3<-Full142x[,c(4,3,16)]
Full142x3<-Full142x3[complete.cases(Full142x3[,1:2]),]
names(Full142x1)<-c("Company","Subsidiaries","Added")
names(Full142x2)<-c("Company","Subsidiaries","Added")
names(Full142x3)<-c("Company","Subsidiaries","Added")
Full142x1$Added<-1
Full142x2$Added<-1
Full142x3$Added<-1
Full142x<-bind_rows(Full142x2,Full142x3)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full142<-left_join(Full141,Full142x,by="Subsidiaries",na_matches="never")
Full142<-subset(Full142,!is.na(Full142$Added.y))
Full142[3:4]<-NULL
names(Full142)<-c("Company","Subsidiaries","Added")
Full142$test<-paste(Full142$Companies,Full142$Subsidiaries,sep=" ")
Full142<-Full142[!duplicated(Full142$test),]
Full142[4]<-NULL
Full14<-bind_rows(Full141,Full142)
Full14$Added<-ifelse(!is.na(Full14$Added),3,NA)
Full14B<-Full14[1:2]

##Change the list of subsidiaries for the next match
#Remove
Full142w1[3]<-NULL
names(Full142w1)<-c("Subs","Delete")
names(Subs14)<-c("Companies","Subs")
Subs14<-left_join(Subs14,Full142w1,by="Subs",na_matches="never")
Subs14<-Subs14[is.na(Subs14$Delete),]
Subs14[3]<-NULL
Full142w2[3]<-NULL
names(Full142w2)<-c("Subs","Delete")
names(Subs14)<-c("Companies","Subs")
Subs14<-left_join(Subs14,Full142w2,by="Subs",na_matches="never")
Subs14<-Subs14[is.na(Subs14$Delete),]
Subs14[3]<-NULL

#Add
Full142x1[3]<-NULL
names(Full142x1)<-c("Companies","Subs")
Full142x2[3]<-NULL
names(Full142x2)<-c("Companies","Subs")
Full142x3[3]<-NULL
names(Full142x3)<-c("Companies","Subs")
Subs14<-bind_rows(Subs14,Full142x1,Full142x2,Full142x3)

#Remove duplicates
Subs14$test<-paste(Subs14$Companies,Subs14$Subs,sep=" ")
Subs14<-Subs14[!duplicated(Subs14$test),]
Subs14[3]<-NULL


#Merge by vendor and if the GUO sold targets in 2013, they are added to the ownership structure of 2014
Deals20143<-Deals2015[,c(3,1,2,3,7)]
names(Deals20143)<-c("Subsidiaries","TargetBvDID143","AcqBvDID143","VendBvDID143","Added")
Full143x<-left_join(Full14B,Deals20143,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full143x$TargetBvDID143)))
names(Subs14)<-c("TargetBvDID143","Subs")
Full143<-left_join(Full143x,Subs14,by="TargetBvDID143",na_matches="never")
length(which(!is.na(Full143$Subs)))
Full143x<-subset(Full143,!is.na(Full143$VendBvDID143))
Full143y<-Full143x[,c(5,3,6)]
names(Full143y)<-c("Company","Subsidiaries","Added")
Full143z<-Full143x[,c(5,7,6)]
names(Full143z)<-c("Company","Subsidiaries","Added")
Full143<-bind_rows(Full143y,Full143z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full143<-left_join(Full14,Full143,by="Subsidiaries",na_matches="never")
Full143<-subset(Full143,!is.na(Full143$Added.y))
Full143<-Full143[,c(1,2,5)]
names(Full143)<-c("Company","Subsidiaries","Added")
Full14<-bind_rows(Full14,Full143)

#Remove duplicate GUO-subsidiary combinations
Full14$test<-paste(Full14$Company,Full14$Subsidiaries,sep=" ")
Full14<-Full14[!duplicated(Full14$test),]
Full14<-subset(Full14,!is.na(Full14$Subsidiaries))
Full14[4]<-NULL
Full14<-Full14[complete.cases(Full14[,1:2]), ]
Full14B<-Full14[1:2]

#Add to the subsidiary data
Full143y[3]<-NULL
names(Full143y)<-c("Companies","Subs")
Full143z[3]<-NULL
names(Full143z)<-c("Companies","Subs")
names(Subs14)<-c("Companies","Subs")
Subs14<-bind_rows(Subs14,Full143y,Full143z)

#Remove duplicates
Subs14$test<-paste(Subs14$Companies,Subs14$Subs,sep=" ")
Subs14<-Subs14[!duplicated(Subs14$test),]
Subs14[3]<-NULL
Subs14<-Subs14[complete.cases(Subs14[,1:2]), ]

#Remove unnecessary files
rm(Full141,Full141x,Full141y,Full142,Full142x,Full142y,Full142z,Deals20141,Deals20142,Deals20143,Full143,
   Full143x,Full143y,Full143z,Full14A,Deals_data1,DealsSubs,Full141a,Full141w,Full141w1,Full141x1,Full141x2,
   Full141w2,Full141yy,Full141z,Full141w,Full142w,Full142w1,Full142w2,Full142x1,Full142x2,Full142x3)



##Ownership 2015
names(Subs14) <- c("Check2","Subs")
##Merge by subsidiaries
Deals20151<-Deals2015[,c(1,1,2,3,7)]
names(Deals20151)<-c("Subsidiaries","TargetBvDID151","AcqBvDID151","VendBvDID151","Added")
Full151<-left_join(Full14B,Deals20151,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full151$TargetBvDID151)))

#Check if the acquiror of the target may still be in the ownership group - if not, delete from the GUO
Full151$AcqBvDID151[is.na(Full151$AcqBvDID151)] <- 0
Full151$Check<-vlookup_df(Full151$AcqBvDID151,Full151,result_column=1,lookup_column=2)
sapply(Full151, class)
Full151$Check<-unlist(Full151$Check)

#If the acquiror is not in the company - exclusion - but what about their subsidiaries? Therefore, add target BvDID for match
Full151$Check2<-ifelse(!is.na(Full151$Check) & Full151$Company!=Full151$Check,Full151$TargetBvDID151,NA)

#Match all subsidiaries of the acquired companies
Full151<-left_join(Full151,Subs14,by="Check2",na_matches="never")
length(which(!is.na(Full151$Subs)))

#Look for the matched subsidiaries in the subsidiaries column and add the acquiror BvDID
Full151$Check3<-vlookup_df(Full151$Subsidiaries,Full151,result_column=4,lookup_column=9)
Full151$Check3<-unlist(Full151$Check3)
sapply(Full151, class)

#Re-check if the matched acquiror of the matched target may still be in the ownership group - if not, delete from the GUO
Full151$Check4<-vlookup_df(Full151$Check3,Full151,result_column=1,lookup_column=2)
Full151$Check4<-unlist(Full151$Check4)

#Check if the new matched acquiror is not part of the GUO structure
Full151$Check5<-case_when(!is.na(Full151$Check) & Full151$Company!=Full151$Check~1,
                          !is.na(Full151$Check4) & Full151$Company!=Full151$Check4~1)
Full151$Check5[is.na(Full151$Check5)] <- 0

#Check if GUO was acquired - exclude all companies
Full151$Check6<-case_when(Full151$TargetBvDID151==Full151$Company~1,
                          Full151$TargetBvDID151!=Full151$Company~0,
                          is.na(Full151$TargetBvDID151)~0)
Full151$Check7<-with (Full151,ave(Full151$Check6,Full151$Company, FUN=sum))

#Exclusion of all acquired GUOs and related companies (Check6 & 7)
Full151$Check8<-ifelse(Full151$Check7==0,Full151$Check5,Full151$Check7)
Full151$Check8[is.na(Full151$Check8)] <- 0
Full151w<-subset(Full151,Full151$Check8>1.1)
Full151x<-subset(Full151,Full151$Check8==1)
Full151<-subset(Full151,Full151$Check8<0.9)

#Add the subsidiaries
Full151w1<-Full151w[c(4,3,6)]
names(Full151w1)<-c("Subsidiaries","Subs","Added")
Full151x1<-Full151x[c(4,3,6)]
names(Full151x1)<-c("Subsidiaries","Subs","Added")
Full151x2<-Full151x[c(4,9,6)]
names(Full151x2)<-c("Subsidiaries","Subs","Added")
Full151a<-bind_rows(Full151w1,Full151x1,Full151x2)
Full151[3:5]<-NULL
Full151[4:12]<-NULL
Full151<-left_join(Full151,Full151a,by="Subsidiaries",na_matches="never")
Full151a<-subset(Full151a,!is.na(Full151a$Added))
Full151a<-Full151[c(1,4,5)]
Full151[4:5]<-NULL
names(Full151)<-c("Company","Subsidiaries","Added")
names(Full151a)<-c("Company","Subsidiaries","Added")
Full151<-bind_rows(Full151,Full151a)
Full151<-Full151[complete.cases(Full151[,1:2]), ]

##Change the list of subsidiaries for the next match
#Delete
Full151y<-Full151w[,c(2,12)]
Full151y[2]<-1
names(Full151y)<-c("Subs","Delete")
Subs15<-left_join(Subs14,Full151y,by="Subs",na_matches="never")
Subs15<-Subs15[is.na(Subs15$Delete),]
Subs15[3]<-NULL
Full151y<-Full151x[,c(2,12)]
Full151y[2]<-1
names(Full151y)<-c("Subs","Delete")
Subs15<-left_join(Subs15,Full151y,by="Subs",na_matches="never")
Subs15<-Subs15[is.na(Subs15$Delete),]

#Add
Full151yy<-Full151w[,c(10,2,12)]
names(Full151yy)<-c("Check2","Subs","Delete")
Full151y<-Full151w[,c(4,2,12)]
names(Full151y)<-c("Check2","Subs","Delete")
Full151z<-Full151x[,c(4,2,12)]
names(Full151z)<-c("Check2","Subs","Delete")
names(Subs15)<-c("Check2","Subs","Delete")
Subs15<-bind_rows(Subs15,Full151yy,Full151y,Full151z)

#Remove duplicates
Subs15$test<-paste(Subs15$Check2,Subs15$Subs,sep=" ")
Subs15<-Subs15[!duplicated(Subs15$test),]
Subs15[3:4]<-NULL


##Merge by Acquiror
Deals20152<-Deals2015[,c(2,1,2,3,7)]
names(Deals20152)<-c("Subsidiaries","TargetBvDID152","AcqBvDID152","VendBvDID152","Added")
Full152<-left_join(Full14B,Deals20152,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full152$TargetBvDID152)))

#Add the subsidiaries of acquired targets
names(Subs15)<-c("TargetBvDID152","Subs")
Full152<-left_join(Full152,Subs15,by="TargetBvDID152",na_matches="never")
length(which(!is.na(Full152$Subs)))

#Lookup the GUOs of target companies
Full152$Check<-vlookup_df(Full152$TargetBvDID152,Full152,result_column=1,lookup_column=2)
Full152$Check<-unlist(Full152$Check)

#If GUO of acquired companies is not the same as acquring GUO - add
Full152$Check2<-ifelse(Full152$Check!=Full152$Company | is.na(Full152$Check),Full152$TargetBvDID152,NA)

#Lookup the GUOs of subsidiaries
Full152$Check3<-vlookup_df(Full152$Subsidiaries,Full152,result_column=1,lookup_column=7)
Full152$Check3<-unlist(Full152$Check3)
Full152$Check4<-vlookup_df(Full152$Subsidiaries,Full152,result_column=4,lookup_column=7) # column 4 for acquiror
Full152$Check4<-unlist(Full152$Check4)
Full152$Check5<-ifelse(Full152$Check3!=Full152$Company,1,NA)
Full152$Check5<-unlist(Full152$Check5)
Full152$Delete1<-ifelse(!is.na(Full152$Check5)|!is.na(Full152$Check2),1,NA)
Full152$Delete2<-ifelse(!is.na(Full152$Check3) & Full152$Company==Full152$Subsidiaries,2,NA)
Full152$Delete2[is.na(Full152$Delete2)] <- 0
Full152$Delete3<-with (Full152,ave(Full152$Delete2,Full152$Company, FUN=sum))
Full152$Delete<-ifelse(Full152$Delete3>1.1,Full152$Delete3,Full152$Delete1)

#Delete File
Full152w<-subset(Full152,!is.na(Full152$Delete))
Full152w1<-Full152w[,c(2,2)]
names(Full152w1)<-c("Subsidiaries","Deletex")
Full152w2<-Full152w[,c(7,7)]
names(Full152w2)<-c("Subsidiaries","Deletex")
Full152w<-bind_rows(Full152w1,Full152w2)
Full152w<-Full152w[complete.cases(Full152w[,1:2]), ]

#Add File
Full152x<-subset(Full152,Full152$Delete>0.9)
Full152x1<-Full152x[,c(11,2,16)]                          #Acquiror
Full152x1<-Full152x1[complete.cases(Full152x1[,1:2]), ]
Full152x2<-Full152x[,c(10,2,16)]
Full152x2<-Full152x2[complete.cases(Full152x2[,1:2]), ]
Full152x3<-Full152x[,c(4,3,16)]
Full152x3<-Full152x3[complete.cases(Full152x3[,1:2]), ]
names(Full152x1)<-c("Company","Subsidiaries","Added")
names(Full152x2)<-c("Company","Subsidiaries","Added")
names(Full152x3)<-c("Company","Subsidiaries","Added")
Full152x1$Added<-1
Full152x2$Added<-1
Full152x3$Added<-1
Full152x<-bind_rows(Full152x2,Full152x3)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full152<-left_join(Full151,Full152x,by="Subsidiaries",na_matches="never")
Full152<-subset(Full152,!is.na(Full152$Added.y))
Full152[3:4]<-NULL
names(Full152)<-c("Company","Subsidiaries","Added")
Full152$test<-paste(Full152$Companies,Full152$Subsidiaries,sep=" ")
Full152<-Full152[!duplicated(Full152$test),]
Full152[4]<-NULL
Full15<-bind_rows(Full151,Full152)
Full15$Added<-ifelse(!is.na(Full15$Added),3,NA)
Full15B<-Full15[1:2]

##Change the list of subsidiaries for the next match
#Remove
Full152w1[3]<-NULL
names(Full152w1)<-c("Subs","Delete")
names(Subs15)<-c("Companies","Subs")
Subs15<-left_join(Subs15,Full152w1,by="Subs",na_matches="never")
Subs15<-Subs15[is.na(Subs15$Delete),]
Subs15[3]<-NULL
Full152w2[3]<-NULL
names(Full152w2)<-c("Subs","Delete")
names(Subs15)<-c("Companies","Subs")
Subs15<-left_join(Subs15,Full152w2,by="Subs",na_matches="never")
Subs15<-Subs15[is.na(Subs15$Delete),]
Subs15[3]<-NULL

#Add
Full152x1[3]<-NULL
names(Full152x1)<-c("Companies","Subs")
Full152x2[3]<-NULL
names(Full152x2)<-c("Companies","Subs")
Full152x3[3]<-NULL
names(Full152x3)<-c("Companies","Subs")
Subs15<-bind_rows(Subs15,Full152x1,Full152x2,Full152x3)

#Remove duplicates
Subs15$test<-paste(Subs15$Companies,Subs15$Subs,sep=" ")
Subs15<-Subs15[!duplicated(Subs15$test),]
Subs15[3]<-NULL


#Merge by vendor and if the GUO sold targets in 2014, they are added to the ownership structure of 2015
Deals20153<-Deals2016[,c(3,1,2,3,7)]
names(Deals20153)<-c("Subsidiaries","TargetBvDID153","AcqBvDID153","VendBvDID153","Added")
Full153x<-left_join(Full15B,Deals20153,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full153x$TargetBvDID153)))
names(Subs15)<-c("TargetBvDID153","Subs")
Full153<-left_join(Full153x,Subs15,by="TargetBvDID153",na_matches="never")
length(which(!is.na(Full153$Subs)))
Full153x<-subset(Full153,!is.na(Full153$VendBvDID153))
Full153y<-Full153x[,c(5,3,6)]
names(Full153y)<-c("Company","Subsidiaries","Added")
Full153z<-Full153x[,c(5,7,6)]
names(Full153z)<-c("Company","Subsidiaries","Added")
Full153<-bind_rows(Full153y,Full153z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full153<-left_join(Full15,Full153,by="Subsidiaries",na_matches="never")
Full153<-subset(Full153,!is.na(Full153$Added.y))
Full153<-Full153[,c(1,2,5)]
names(Full153)<-c("Company","Subsidiaries","Added")
Full15<-bind_rows(Full15,Full153)

#Remove duplicate GUO-subsidiary combinations
Full15$test<-paste(Full15$Company,Full15$Subsidiaries,sep=" ")
Full15<-Full15[!duplicated(Full15$test),]
Full15<-subset(Full15,!is.na(Full15$Subsidiaries))
Full15[4]<-NULL
Full15<-Full15[complete.cases(Full15[,1:2]), ]
Full15B<-Full15[1:2]

#Add to the subsidiary data
Full153y[3]<-NULL
names(Full153y)<-c("Companies","Subs")
Full153z[3]<-NULL
names(Full153z)<-c("Companies","Subs")
names(Subs15)<-c("Companies","Subs")
Subs15<-bind_rows(Subs15,Full153y,Full153z)

#Remove duplicates
Subs15$test<-paste(Subs15$Companies,Subs15$Subs,sep=" ")
Subs15<-Subs15[!duplicated(Subs15$test),]
Subs15[3]<-NULL
Subs15<-Subs15[complete.cases(Subs15[,1:2]), ]

#Remove unnecessary files
rm(Full151,Full151x,Full151y,Full152,Full152x,Full152y,Full152z,Deals20151,Deals20152,Deals20153,Full153,
   Full153x,Full153y,Full153z,Full15A,Deals_data1,DealsSubs,Full151a,Full151w,Full151w1,Full151x1,Full151x2,
   Full151w2,Full151yy,Full151z,Full151w,Full152w,Full152w1,Full152w2,Full152x1,Full152x2,Full152x3)


##Ownership 2016
names(Subs15) <- c("Check2","Subs")
##Merge by subsidiaries
Deals20161<-Deals2016[,c(1,1,2,3,7)]
names(Deals20161)<-c("Subsidiaries","TargetBvDID161","AcqBvDID161","VendBvDID161","Added")
Full161<-left_join(Full15B,Deals20161,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full161$TargetBvDID161)))

#Check if the acquiror of the target may still be in the ownership group - if not, delete from the GUO
Full161$AcqBvDID161[is.na(Full161$AcqBvDID161)] <- 0
Full161$Check<-vlookup_df(Full161$AcqBvDID161,Full161,result_column=1,lookup_column=2)
sapply(Full161, class)
Full161$Check<-unlist(Full161$Check)

#If the acquiror is not in the company - exclusion - but what about their subsidiaries? Therefore, add target BvDID for match
Full161$Check2<-ifelse(!is.na(Full161$Check) & Full161$Company!=Full161$Check,Full161$TargetBvDID161,NA)

#Match all subsidiaries of the acquired companies
Full161<-left_join(Full161,Subs15,by="Check2",na_matches="never")
length(which(!is.na(Full161$Subs)))

#Look for the matched subsidiaries in the subsidiaries column and add the acquiror BvDID
Full161$Check3<-vlookup_df(Full161$Subsidiaries,Full161,result_column=4,lookup_column=9)
Full161$Check3<-unlist(Full161$Check3)
sapply(Full161, class)

#Re-check if the matched acquiror of the matched target may still be in the ownership group - if not, delete from the GUO
Full161$Check4<-vlookup_df(Full161$Check3,Full161,result_column=1,lookup_column=2)
Full161$Check4<-unlist(Full161$Check4)

#Check if the new matched acquiror is not part of the GUO structure
Full161$Check5<-case_when(!is.na(Full161$Check) & Full161$Company!=Full161$Check~1,
                          !is.na(Full161$Check4) & Full161$Company!=Full161$Check4~1)
Full161$Check5[is.na(Full161$Check5)] <- 0

#Check if GUO was acquired - exclude all companies
Full161$Check6<-case_when(Full161$TargetBvDID161==Full161$Company~1,
                          Full161$TargetBvDID161!=Full161$Company~0,
                          is.na(Full161$TargetBvDID161)~0)
Full161$Check7<-with (Full161,ave(Full161$Check6,Full161$Company, FUN=sum))

#Exclusion of all acquired GUOs and related companies (Check6 & 7)
Full161$Check8<-ifelse(Full161$Check7==0,Full161$Check5,Full161$Check7)
Full161$Check8[is.na(Full161$Check8)] <- 0
Full161w<-subset(Full161,Full161$Check8>1.1)
Full161x<-subset(Full161,Full161$Check8==1)
Full161<-subset(Full161,Full161$Check8<0.9)

#Add the subsidiaries
Full161w1<-Full161w[c(4,3,6)]
names(Full161w1)<-c("Subsidiaries","Subs","Added")
Full161x1<-Full161x[c(4,3,6)]
names(Full161x1)<-c("Subsidiaries","Subs","Added")
Full161x2<-Full161x[c(4,9,6)]
names(Full161x2)<-c("Subsidiaries","Subs","Added")
Full161a<-bind_rows(Full161w1,Full161x1,Full161x2)
Full161[3:5]<-NULL
Full161[4:12]<-NULL
Full161<-left_join(Full161,Full161a,by="Subsidiaries",na_matches="never")
Full161a<-subset(Full161a,!is.na(Full161a$Added))
Full161a<-Full161[c(1,4,5)]
Full161[4:5]<-NULL
names(Full161)<-c("Company","Subsidiaries","Added")
names(Full161a)<-c("Company","Subsidiaries","Added")
Full161<-bind_rows(Full161,Full161a)
Full161<-Full161[complete.cases(Full161[,1:2]), ]

##Change the list of subsidiaries for the next match
#Delete
Full161y<-Full161w[,c(2,12)]
Full161y[2]<-1
names(Full161y)<-c("Subs","Delete")
Subs16<-left_join(Subs15,Full161y,by="Subs",na_matches="never")
Subs16<-Subs16[is.na(Subs16$Delete),]
Subs16[3]<-NULL
Full161y<-Full161x[,c(2,12)]
Full161y[2]<-1
names(Full161y)<-c("Subs","Delete")
Subs16<-left_join(Subs16,Full161y,by="Subs",na_matches="never")
Subs16<-Subs16[is.na(Subs16$Delete),]

#Add
Full161yy<-Full161w[,c(10,2,12)]
names(Full161yy)<-c("Check2","Subs","Delete")
Full161y<-Full161w[,c(4,2,12)]
names(Full161y)<-c("Check2","Subs","Delete")
Full161z<-Full161x[,c(4,2,12)]
names(Full161z)<-c("Check2","Subs","Delete")
names(Subs16)<-c("Check2","Subs","Delete")
Subs16<-bind_rows(Subs16,Full161yy,Full161y,Full161z)

#Remove duplicates
Subs16$test<-paste(Subs16$Check2,Subs16$Subs,sep=" ")
Subs16<-Subs16[!duplicated(Subs16$test),]
Subs16[3:4]<-NULL


##Merge by Acquiror
Deals20162<-Deals2016[,c(2,1,2,3,7)]
names(Deals20162)<-c("Subsidiaries","TargetBvDID162","AcqBvDID162","VendBvDID162","Added")
Full162<-left_join(Full15B,Deals20162,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full162$TargetBvDID162)))

#Add the subsidiaries of acquired targets
names(Subs16)<-c("TargetBvDID162","Subs")
Full162<-left_join(Full162,Subs16,by="TargetBvDID162",na_matches="never")
length(which(!is.na(Full162$Subs)))

#Lookup the GUOs of target companies
Full162$Check<-vlookup_df(Full162$TargetBvDID162,Full162,result_column=1,lookup_column=2)
Full162$Check<-unlist(Full162$Check)

#If GUO of acquired companies is not the same as acquring GUO - add
Full162$Check2<-ifelse(Full162$Check!=Full162$Company | is.na(Full162$Check),Full162$TargetBvDID162,NA)

#Lookup the GUOs of subsidiaries
Full162$Check3<-vlookup_df(Full162$Subsidiaries,Full162,result_column=1,lookup_column=7)
Full162$Check3<-unlist(Full162$Check3)
Full162$Check4<-vlookup_df(Full162$Subsidiaries,Full162,result_column=4,lookup_column=7) # column 4 for acquiror
Full162$Check4<-unlist(Full162$Check4)
Full162$Check5<-ifelse(Full162$Check3!=Full162$Company,1,NA)
Full162$Check5<-unlist(Full162$Check5)
Full162$Delete1<-ifelse(!is.na(Full162$Check5)|!is.na(Full162$Check2),1,NA)
Full162$Delete2<-ifelse(!is.na(Full162$Check3) & Full162$Company==Full162$Subsidiaries,2,NA)
Full162$Delete2[is.na(Full162$Delete2)] <- 0
Full162$Delete3<-with (Full162,ave(Full162$Delete2,Full162$Company, FUN=sum))
Full162$Delete<-ifelse(Full162$Delete3>1.1,Full162$Delete3,Full162$Delete1)

#Delete File
Full162w<-subset(Full162,!is.na(Full162$Delete))
Full162w1<-Full162w[,c(2,2)]
names(Full162w1)<-c("Subsidiaries","Deletex")
Full162w2<-Full162w[,c(7,7)]
names(Full162w2)<-c("Subsidiaries","Deletex")
Full162w<-bind_rows(Full162w1,Full162w2)
Full162w<-Full162w[complete.cases(Full162w[,1:2]), ]

#Add File
Full162x<-subset(Full162,Full162$Delete>0.9)
Full162x1<-Full162x[,c(11,2,16)]                          #Acquiror
Full162x1<-Full162x1[complete.cases(Full162x1[,1:2]), ]
Full162x2<-Full162x[,c(10,2,16)]
Full162x2<-Full162x2[complete.cases(Full162x2[,1:2]), ]
Full162x3<-Full162x[,c(4,3,16)]
Full162x3<-Full162x3[complete.cases(Full162x3[,1:2]), ]
names(Full162x1)<-c("Company","Subsidiaries","Added")
names(Full162x2)<-c("Company","Subsidiaries","Added")
names(Full162x3)<-c("Company","Subsidiaries","Added")
Full162x1$Added<-1
Full162x2$Added<-1
Full162x3$Added<-1
Full162x<-bind_rows(Full162x2,Full162x3)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full162<-left_join(Full161,Full162x,by="Subsidiaries",na_matches="never")
Full162<-subset(Full162,!is.na(Full162$Added.y))
Full162[3:4]<-NULL
names(Full162)<-c("Company","Subsidiaries","Added")
Full162$test<-paste(Full162$Companies,Full162$Subsidiaries,sep=" ")
Full162<-Full162[!duplicated(Full162$test),]
Full162[4]<-NULL
Full16<-bind_rows(Full161,Full162)
Full16$Added<-ifelse(!is.na(Full16$Added),3,NA)
Full16B<-Full16[1:2]

##Change the list of subsidiaries for the next match
#Remove
Full162w1[3]<-NULL
names(Full162w1)<-c("Subs","Delete")
names(Subs16)<-c("Companies","Subs")
Subs16<-left_join(Subs16,Full162w1,by="Subs",na_matches="never")
Subs16<-Subs16[is.na(Subs16$Delete),]
Subs16[3]<-NULL
Full162w2[3]<-NULL
names(Full162w2)<-c("Subs","Delete")
names(Subs16)<-c("Companies","Subs")
Subs16<-left_join(Subs16,Full162w2,by="Subs",na_matches="never")
Subs16<-Subs16[is.na(Subs16$Delete),]
Subs16[3]<-NULL

#Add
Full162x1[3]<-NULL
names(Full162x1)<-c("Companies","Subs")
Full162x2[3]<-NULL
names(Full162x2)<-c("Companies","Subs")
Full162x3[3]<-NULL
names(Full162x3)<-c("Companies","Subs")
Subs16<-bind_rows(Subs16,Full162x1,Full162x2,Full162x3)

#Remove duplicates
Subs16$test<-paste(Subs16$Companies,Subs16$Subs,sep=" ")
Subs16<-Subs16[!duplicated(Subs16$test),]
Subs16[3]<-NULL


#Merge by vendor and if the GUO sold targets in 2015, they are added to the ownership structure of 2016
Deals20163<-Deals2017[,c(3,1,2,3,7)]
names(Deals20163)<-c("Subsidiaries","TargetBvDID163","AcqBvDID163","VendBvDID163","Added")
Full163x<-left_join(Full16B,Deals20163,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full163x$TargetBvDID163)))
names(Subs16)<-c("TargetBvDID163","Subs")
Full163<-left_join(Full163x,Subs16,by="TargetBvDID163",na_matches="never")
length(which(!is.na(Full163$Subs)))
Full163x<-subset(Full163,!is.na(Full163$VendBvDID163))
Full163y<-Full163x[,c(5,3,6)]
names(Full163y)<-c("Company","Subsidiaries","Added")
Full163z<-Full163x[,c(5,7,6)]
names(Full163z)<-c("Company","Subsidiaries","Added")
Full163<-bind_rows(Full163y,Full163z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full163<-left_join(Full16,Full163,by="Subsidiaries",na_matches="never")
Full163<-subset(Full163,!is.na(Full163$Added.y))
Full163<-Full163[,c(1,2,5)]
names(Full163)<-c("Company","Subsidiaries","Added")
Full16<-bind_rows(Full16,Full163)

#Remove duplicate GUO-subsidiary combinations
Full16$test<-paste(Full16$Company,Full16$Subsidiaries,sep=" ")
Full16<-Full16[!duplicated(Full16$test),]
Full16<-subset(Full16,!is.na(Full16$Subsidiaries))
Full16[4]<-NULL
Full16<-Full16[complete.cases(Full16[,1:2]), ]
Full16B<-Full16[1:2]

#Add to the subsidiary data
Full163y[3]<-NULL
names(Full163y)<-c("Companies","Subs")
Full163z[3]<-NULL
names(Full163z)<-c("Companies","Subs")
names(Subs16)<-c("Companies","Subs")
Subs16<-bind_rows(Subs16,Full163y,Full163z)

#Remove duplicates
Subs16$test<-paste(Subs16$Companies,Subs16$Subs,sep=" ")
Subs16<-Subs16[!duplicated(Subs16$test),]
Subs16[3]<-NULL
Subs16<-Subs16[complete.cases(Subs16[,1:2]), ]

#Remove unnecessary files
rm(Full161,Full161x,Full161y,Full162,Full162x,Full162y,Full162z,Deals20161,Deals20162,Deals20163,Full163,
   Full163x,Full163y,Full163z,Full16A,Deals_data1,DealsSubs,Full161a,Full161w,Full161w1,Full161x1,Full161x2,
   Full161w2,Full161yy,Full161z,Full161w,Full162w,Full162w1,Full162w2,Full162x1,Full162x2,Full162x3)



##Ownership 2017
names(Subs16) <- c("Check2","Subs")
##Merge by subsidiaries
Deals20171<-Deals2017[,c(1,1,2,3,7)]
names(Deals20171)<-c("Subsidiaries","TargetBvDID171","AcqBvDID171","VendBvDID171","Added")
Full171<-left_join(Full16B,Deals20171,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full171$TargetBvDID171)))

#Check if the acquiror of the target may still be in the ownership group - if not, delete from the GUO
Full171$AcqBvDID171[is.na(Full171$AcqBvDID171)] <- 0
Full171$Check<-vlookup_df(Full171$AcqBvDID171,Full171,result_column=1,lookup_column=2)
sapply(Full171, class)
Full171$Check<-unlist(Full171$Check)

#If the acquiror is not in the company - exclusion - but what about their subsidiaries? Therefore, add target BvDID for match
Full171$Check2<-ifelse(!is.na(Full171$Check) & Full171$Company!=Full171$Check,Full171$TargetBvDID171,NA)

#Match all subsidiaries of the acquired companies
Full171<-left_join(Full171,Subs16,by="Check2",na_matches="never")
length(which(!is.na(Full171$Subs)))

#Look for the matched subsidiaries in the subsidiaries column and add the acquiror BvDID
Full171$Check3<-vlookup_df(Full171$Subsidiaries,Full171,result_column=4,lookup_column=9)
Full171$Check3<-unlist(Full171$Check3)
sapply(Full171, class)

#Re-check if the matched acquiror of the matched target may still be in the ownership group - if not, delete from the GUO
Full171$Check4<-vlookup_df(Full171$Check3,Full171,result_column=1,lookup_column=2)
Full171$Check4<-unlist(Full171$Check4)

#Check if the new matched acquiror is not part of the GUO structure
Full171$Check5<-case_when(!is.na(Full171$Check) & Full171$Company!=Full171$Check~1,
                          !is.na(Full171$Check4) & Full171$Company!=Full171$Check4~1)
Full171$Check5[is.na(Full171$Check5)] <- 0

#Check if GUO was acquired - exclude all companies
Full171$Check6<-case_when(Full171$TargetBvDID171==Full171$Company~1,
                          Full171$TargetBvDID171!=Full171$Company~0,
                          is.na(Full171$TargetBvDID171)~0)
Full171$Check7<-with (Full171,ave(Full171$Check6,Full171$Company, FUN=sum))

#Exclusion of all acquired GUOs and related companies (Check6 & 7)
Full171$Check8<-ifelse(Full171$Check7==0,Full171$Check5,Full171$Check7)
Full171$Check8[is.na(Full171$Check8)] <- 0
Full171w<-subset(Full171,Full171$Check8>1.1)
Full171x<-subset(Full171,Full171$Check8==1)
Full171<-subset(Full171,Full171$Check8<0.9)

#Add the subsidiaries
Full171w1<-Full171w[c(4,3,6)]
names(Full171w1)<-c("Subsidiaries","Subs","Added")
Full171x1<-Full171x[c(4,3,6)]
names(Full171x1)<-c("Subsidiaries","Subs","Added")
Full171x2<-Full171x[c(4,9,6)]
names(Full171x2)<-c("Subsidiaries","Subs","Added")
Full171a<-bind_rows(Full171w1,Full171x1,Full171x2)
Full171[3:5]<-NULL
Full171[4:12]<-NULL
Full171<-left_join(Full171,Full171a,by="Subsidiaries",na_matches="never")
Full171a<-subset(Full171a,!is.na(Full171a$Added))
Full171a<-Full171[c(1,4,5)]
Full171[4:5]<-NULL
names(Full171)<-c("Company","Subsidiaries","Added")
names(Full171a)<-c("Company","Subsidiaries","Added")
Full171<-bind_rows(Full171,Full171a)
Full171<-Full171[complete.cases(Full171[,1:2]), ]

##Change the list of subsidiaries for the next match
#Delete
Full171y<-Full171w[,c(2,12)]
Full171y[2]<-1
names(Full171y)<-c("Subs","Delete")
Subs17<-left_join(Subs16,Full171y,by="Subs",na_matches="never")
Subs17<-Subs17[is.na(Subs17$Delete),]
Subs17[3]<-NULL
Full171y<-Full171x[,c(2,12)]
Full171y[2]<-1
names(Full171y)<-c("Subs","Delete")
Subs17<-left_join(Subs17,Full171y,by="Subs",na_matches="never")
Subs17<-Subs17[is.na(Subs17$Delete),]

#Add
Full171yy<-Full171w[,c(10,2,12)]
names(Full171yy)<-c("Check2","Subs","Delete")
Full171y<-Full171w[,c(4,2,12)]
names(Full171y)<-c("Check2","Subs","Delete")
Full171z<-Full171x[,c(4,2,12)]
names(Full171z)<-c("Check2","Subs","Delete")
names(Subs17)<-c("Check2","Subs","Delete")
Subs17<-bind_rows(Subs17,Full171yy,Full171y,Full171z)

#Remove duplicates
Subs17$test<-paste(Subs17$Check2,Subs17$Subs,sep=" ")
Subs17<-Subs17[!duplicated(Subs17$test),]
Subs17[3:4]<-NULL


##Merge by Acquiror
Deals20172<-Deals2017[,c(2,1,2,3,7)]
names(Deals20172)<-c("Subsidiaries","TargetBvDID172","AcqBvDID172","VendBvDID172","Added")
Full172<-left_join(Full16B,Deals20172,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full172$TargetBvDID172)))

#Add the subsidiaries of acquired targets
names(Subs17)<-c("TargetBvDID172","Subs")
Full172<-left_join(Full172,Subs17,by="TargetBvDID172",na_matches="never")
length(which(!is.na(Full172$Subs)))

#Lookup the GUOs of target companies
Full172$Check<-vlookup_df(Full172$TargetBvDID172,Full172,result_column=1,lookup_column=2)
Full172$Check<-unlist(Full172$Check)

#If GUO of acquired companies is not the same as acquring GUO - add
Full172$Check2<-ifelse(Full172$Check!=Full172$Company | is.na(Full172$Check),Full172$TargetBvDID172,NA)

#Lookup the GUOs of subsidiaries
Full172$Check3<-vlookup_df(Full172$Subsidiaries,Full172,result_column=1,lookup_column=7)
Full172$Check3<-unlist(Full172$Check3)
Full172$Check4<-vlookup_df(Full172$Subsidiaries,Full172,result_column=4,lookup_column=7) # column 4 for acquiror
Full172$Check4<-unlist(Full172$Check4)
Full172$Check5<-ifelse(Full172$Check3!=Full172$Company,1,NA)
Full172$Check5<-unlist(Full172$Check5)
Full172$Delete1<-ifelse(!is.na(Full172$Check5)|!is.na(Full172$Check2),1,NA)
Full172$Delete2<-ifelse(!is.na(Full172$Check3) & Full172$Company==Full172$Subsidiaries,2,NA)
Full172$Delete2[is.na(Full172$Delete2)] <- 0
Full172$Delete3<-with (Full172,ave(Full172$Delete2,Full172$Company, FUN=sum))
Full172$Delete<-ifelse(Full172$Delete3>1.1,Full172$Delete3,Full172$Delete1)

#Delete File
Full172w<-subset(Full172,!is.na(Full172$Delete))
Full172w1<-Full172w[,c(2,2)]
names(Full172w1)<-c("Subsidiaries","Deletex")
Full172w2<-Full172w[,c(7,7)]
names(Full172w2)<-c("Subsidiaries","Deletex")
Full172w<-bind_rows(Full172w1,Full172w2)
Full172w<-Full172w[complete.cases(Full172w[,1:2]), ]

#Add File
Full172x<-subset(Full172,Full172$Delete>0.9)
Full172x1<-Full172x[,c(11,2,16)]                          #Acquiror
Full172x1<-Full172x1[complete.cases(Full172x1[,1:2]), ]
Full172x2<-Full172x[,c(10,2,16)]
Full172x2<-Full172x2[complete.cases(Full172x2[,1:2]), ]
Full172x3<-Full172x[,c(4,3,16)]
Full172x3<-Full172x3[complete.cases(Full172x3[,1:2]), ]
names(Full172x1)<-c("Company","Subsidiaries","Added")
names(Full172x2)<-c("Company","Subsidiaries","Added")
names(Full172x3)<-c("Company","Subsidiaries","Added")
Full172x1$Added<-1
Full172x2$Added<-1
Full172x3$Added<-1
Full172x<-bind_rows(Full172x2,Full172x3)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full172<-left_join(Full171,Full172x,by="Subsidiaries",na_matches="never")
Full172<-subset(Full172,!is.na(Full172$Added.y))
Full172[3:4]<-NULL
names(Full172)<-c("Company","Subsidiaries","Added")
Full172$test<-paste(Full172$Companies,Full172$Subsidiaries,sep=" ")
Full172<-Full172[!duplicated(Full172$test),]
Full172[4]<-NULL
Full17<-bind_rows(Full171,Full172)
Full17$Added<-ifelse(!is.na(Full17$Added),3,NA)
Full17B<-Full17[1:2]

##Change the list of subsidiaries for the next match
#Remove
Full172w1[3]<-NULL
names(Full172w1)<-c("Subs","Delete")
names(Subs17)<-c("Companies","Subs")
Subs17<-left_join(Subs17,Full172w1,by="Subs",na_matches="never")
Subs17<-Subs17[is.na(Subs17$Delete),]
Subs17[3]<-NULL
Full172w2[3]<-NULL
names(Full172w2)<-c("Subs","Delete")
names(Subs17)<-c("Companies","Subs")
Subs17<-left_join(Subs17,Full172w2,by="Subs",na_matches="never")
Subs17<-Subs17[is.na(Subs17$Delete),]
Subs17[3]<-NULL

#Add
Full172x1[3]<-NULL
names(Full172x1)<-c("Companies","Subs")
Full172x2[3]<-NULL
names(Full172x2)<-c("Companies","Subs")
Full172x3[3]<-NULL
names(Full172x3)<-c("Companies","Subs")
Subs17<-bind_rows(Subs17,Full172x1,Full172x2,Full172x3)

#Remove duplicates
Subs17$test<-paste(Subs17$Companies,Subs17$Subs,sep=" ")
Subs17<-Subs17[!duplicated(Subs17$test),]
Subs17[3]<-NULL


#Merge by vendor and if the GUO sold targets in 2016, they are added to the ownership structure of 2017
Deals20173<-Deals2018[,c(3,1,2,3,7)]
names(Deals20173)<-c("Subsidiaries","TargetBvDID173","AcqBvDID173","VendBvDID173","Added")
Full173x<-left_join(Full17B,Deals20173,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full173x$TargetBvDID173)))
names(Subs17)<-c("TargetBvDID173","Subs")
Full173<-left_join(Full173x,Subs17,by="TargetBvDID173",na_matches="never")
length(which(!is.na(Full173$Subs)))
Full173x<-subset(Full173,!is.na(Full173$VendBvDID173))
Full173y<-Full173x[,c(5,3,6)]
names(Full173y)<-c("Company","Subsidiaries","Added")
Full173z<-Full173x[,c(5,7,6)]
names(Full173z)<-c("Company","Subsidiaries","Added")
Full173<-bind_rows(Full173y,Full173z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full173<-left_join(Full17,Full173,by="Subsidiaries",na_matches="never")
Full173<-subset(Full173,!is.na(Full173$Added.y))
Full173<-Full173[,c(1,2,5)]
names(Full173)<-c("Company","Subsidiaries","Added")
Full17<-bind_rows(Full17,Full173)

#Remove duplicate GUO-subsidiary combinations
Full17$test<-paste(Full17$Company,Full17$Subsidiaries,sep=" ")
Full17<-Full17[!duplicated(Full17$test),]
Full17<-subset(Full17,!is.na(Full17$Subsidiaries))
Full17[4]<-NULL
Full17<-Full17[complete.cases(Full17[,1:2]), ]
Full17B<-Full17[1:2]

#Add to the subsidiary data
Full173y[3]<-NULL
names(Full173y)<-c("Companies","Subs")
Full173z[3]<-NULL
names(Full173z)<-c("Companies","Subs")
names(Subs17)<-c("Companies","Subs")
Subs17<-bind_rows(Subs17,Full173y,Full173z)

#Remove duplicates
Subs17$test<-paste(Subs17$Companies,Subs17$Subs,sep=" ")
Subs17<-Subs17[!duplicated(Subs17$test),]
Subs17[3]<-NULL
Subs17<-Subs17[complete.cases(Subs17[,1:2]), ]

#Remove unnecessary files
rm(Full171,Full171x,Full171y,Full172,Full172x,Full172y,Full172z,Deals20171,Deals20172,Deals20173,Full173,
   Full173x,Full173y,Full173z,Full17A,Deals_data1,DealsSubs,Full171a,Full171w,Full171w1,Full171x1,Full171x2,
   Full171w2,Full171yy,Full171z,Full171w,Full172w,Full172w1,Full172w2,Full172x1,Full172x2,Full172x3)



##Ownership 2018
names(Subs17) <- c("Check2","Subs")
##Merge by subsidiaries
Deals20181<-Deals2018[,c(1,1,2,3,7)]
names(Deals20181)<-c("Subsidiaries","TargetBvDID181","AcqBvDID181","VendBvDID181","Added")
Full181<-left_join(Full17B,Deals20181,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full181$TargetBvDID181)))

#Check if the acquiror of the target may still be in the ownership group - if not, delete from the GUO
Full181$AcqBvDID181[is.na(Full181$AcqBvDID181)] <- 0
Full181$Check<-vlookup_df(Full181$AcqBvDID181,Full181,result_column=1,lookup_column=2)
sapply(Full181, class)
Full181$Check<-unlist(Full181$Check)

#If the acquiror is not in the company - exclusion - but what about their subsidiaries? Therefore, add target BvDID for match
Full181$Check2<-ifelse(!is.na(Full181$Check) & Full181$Company!=Full181$Check,Full181$TargetBvDID181,NA)

#Match all subsidiaries of the acquired companies
Full181<-left_join(Full181,Subs17,by="Check2",na_matches="never")
length(which(!is.na(Full181$Subs)))

#Look for the matched subsidiaries in the subsidiaries column and add the acquiror BvDID
Full181$Check3<-vlookup_df(Full181$Subsidiaries,Full181,result_column=4,lookup_column=9)
Full181$Check3<-unlist(Full181$Check3)
sapply(Full181, class)

#Re-check if the matched acquiror of the matched target may still be in the ownership group - if not, delete from the GUO
Full181$Check4<-vlookup_df(Full181$Check3,Full181,result_column=1,lookup_column=2)
Full181$Check4<-unlist(Full181$Check4)

#Check if the new matched acquiror is not part of the GUO structure
Full181$Check5<-case_when(!is.na(Full181$Check) & Full181$Company!=Full181$Check~1,
                          !is.na(Full181$Check4) & Full181$Company!=Full181$Check4~1)
Full181$Check5[is.na(Full181$Check5)] <- 0

#Check if GUO was acquired - exclude all companies
Full181$Check6<-case_when(Full181$TargetBvDID181==Full181$Company~1,
                          Full181$TargetBvDID181!=Full181$Company~0,
                          is.na(Full181$TargetBvDID181)~0)
Full181$Check7<-with (Full181,ave(Full181$Check6,Full181$Company, FUN=sum))

#Exclusion of all acquired GUOs and related companies (Check6 & 7)
Full181$Check8<-ifelse(Full181$Check7==0,Full181$Check5,Full181$Check7)
Full181$Check8[is.na(Full181$Check8)] <- 0
Full181w<-subset(Full181,Full181$Check8>1.1)
Full181x<-subset(Full181,Full181$Check8==1)
Full181<-subset(Full181,Full181$Check8<0.9)

#Add the subsidiaries
Full181w1<-Full181w[c(4,3,6)]
names(Full181w1)<-c("Subsidiaries","Subs","Added")
Full181x1<-Full181x[c(4,3,6)]
names(Full181x1)<-c("Subsidiaries","Subs","Added")
Full181x2<-Full181x[c(4,9,6)]
names(Full181x2)<-c("Subsidiaries","Subs","Added")
Full181a<-bind_rows(Full181w1,Full181x1,Full181x2)
Full181[3:5]<-NULL
Full181[4:12]<-NULL
Full181<-left_join(Full181,Full181a,by="Subsidiaries",na_matches="never")
Full181a<-subset(Full181a,!is.na(Full181a$Added))
Full181a<-Full181[c(1,4,5)]
Full181[4:5]<-NULL
names(Full181)<-c("Company","Subsidiaries","Added")
names(Full181a)<-c("Company","Subsidiaries","Added")
Full181<-bind_rows(Full181,Full181a)
Full181<-Full181[complete.cases(Full181[,1:2]), ]

##Change the list of subsidiaries for the next match
#Delete
Full181y<-Full181w[,c(2,12)]
Full181y[2]<-1
names(Full181y)<-c("Subs","Delete")
Subs18<-left_join(Subs17,Full181y,by="Subs",na_matches="never")
Subs18<-Subs18[is.na(Subs18$Delete),]
Subs18[3]<-NULL
Full181y<-Full181x[,c(2,12)]
Full181y[2]<-1
names(Full181y)<-c("Subs","Delete")
Subs18<-left_join(Subs18,Full181y,by="Subs",na_matches="never")
Subs18<-Subs18[is.na(Subs18$Delete),]

#Add
Full181yy<-Full181w[,c(10,2,12)]
names(Full181yy)<-c("Check2","Subs","Delete")
Full181y<-Full181w[,c(4,2,12)]
names(Full181y)<-c("Check2","Subs","Delete")
Full181z<-Full181x[,c(4,2,12)]
names(Full181z)<-c("Check2","Subs","Delete")
names(Subs18)<-c("Check2","Subs","Delete")
Subs18<-bind_rows(Subs18,Full181yy,Full181y,Full181z)

#Remove duplicates
Subs18$test<-paste(Subs18$Check2,Subs18$Subs,sep=" ")
Subs18<-Subs18[!duplicated(Subs18$test),]
Subs18[3:4]<-NULL


##Merge by Acquiror
Deals20182<-Deals2018[,c(2,1,2,3,7)]
names(Deals20182)<-c("Subsidiaries","TargetBvDID182","AcqBvDID182","VendBvDID182","Added")
Full182<-left_join(Full17B,Deals20182,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full182$TargetBvDID182)))

#Add the subsidiaries of acquired targets
names(Subs18)<-c("TargetBvDID182","Subs")
Full182<-left_join(Full182,Subs18,by="TargetBvDID182",na_matches="never")
length(which(!is.na(Full182$Subs)))

#Lookup the GUOs of target companies
Full182$Check<-vlookup_df(Full182$TargetBvDID182,Full182,result_column=1,lookup_column=2)
Full182$Check<-unlist(Full182$Check)

#If GUO of acquired companies is not the same as acquring GUO - add
Full182$Check2<-ifelse(Full182$Check!=Full182$Company | is.na(Full182$Check),Full182$TargetBvDID182,NA)

#Lookup the GUOs of subsidiaries
Full182$Check3<-vlookup_df(Full182$Subsidiaries,Full182,result_column=1,lookup_column=7)
Full182$Check3<-unlist(Full182$Check3)
Full182$Check4<-vlookup_df(Full182$Subsidiaries,Full182,result_column=4,lookup_column=7) # column 4 for acquiror
Full182$Check4<-unlist(Full182$Check4)
Full182$Check5<-ifelse(Full182$Check3!=Full182$Company,1,NA)
Full182$Check5<-unlist(Full182$Check5)
Full182$Delete1<-ifelse(!is.na(Full182$Check5)|!is.na(Full182$Check2),1,NA)
Full182$Delete2<-ifelse(!is.na(Full182$Check3) & Full182$Company==Full182$Subsidiaries,2,NA)
Full182$Delete2[is.na(Full182$Delete2)] <- 0
Full182$Delete3<-with (Full182,ave(Full182$Delete2,Full182$Company, FUN=sum))
Full182$Delete<-ifelse(Full182$Delete3>1.1,Full182$Delete3,Full182$Delete1)

#Delete File
Full182w<-subset(Full182,!is.na(Full182$Delete))
Full182w1<-Full182w[,c(2,2)]
names(Full182w1)<-c("Subsidiaries","Deletex")
Full182w2<-Full182w[,c(7,7)]
names(Full182w2)<-c("Subsidiaries","Deletex")
Full182w<-bind_rows(Full182w1,Full182w2)
Full182w<-Full182w[complete.cases(Full182w[,1:2]), ]

#Add File
Full182x<-subset(Full182,Full182$Delete>0.9)
Full182x1<-Full182x[,c(11,2,16)]                          #Acquiror
Full182x1<-Full182x1[complete.cases(Full182x1[,1:2]), ]
Full182x2<-Full182x[,c(10,2,16)]
Full182x2<-Full182x2[complete.cases(Full182x2[,1:2]), ]
Full182x3<-Full182x[,c(4,3,16)]
Full182x3<-Full182x3[complete.cases(Full182x3[,1:2]), ]
names(Full182x1)<-c("Company","Subsidiaries","Added")
names(Full182x2)<-c("Company","Subsidiaries","Added")
names(Full182x3)<-c("Company","Subsidiaries","Added")
Full182x1$Added<-1
Full182x2$Added<-1
Full182x3$Added<-1
Full182x<-bind_rows(Full182x2,Full182x3)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full182<-left_join(Full181,Full182x,by="Subsidiaries",na_matches="never")
Full182<-subset(Full182,!is.na(Full182$Added.y))
Full182[3:4]<-NULL
names(Full182)<-c("Company","Subsidiaries","Added")
Full182$test<-paste(Full182$Companies,Full182$Subsidiaries,sep=" ")
Full182<-Full182[!duplicated(Full182$test),]
Full182[4]<-NULL
Full18<-bind_rows(Full181,Full182)
Full18$Added<-ifelse(!is.na(Full18$Added),3,NA)
Full18B<-Full18[1:2]

##Change the list of subsidiaries for the next match
#Remove
Full182w1[3]<-NULL
names(Full182w1)<-c("Subs","Delete")
names(Subs18)<-c("Companies","Subs")
Subs18<-left_join(Subs18,Full182w1,by="Subs",na_matches="never")
Subs18<-Subs18[is.na(Subs18$Delete),]
Subs18[3]<-NULL
Full182w2[3]<-NULL
names(Full182w2)<-c("Subs","Delete")
names(Subs18)<-c("Companies","Subs")
Subs18<-left_join(Subs18,Full182w2,by="Subs",na_matches="never")
Subs18<-Subs18[is.na(Subs18$Delete),]
Subs18[3]<-NULL

#Add
Full182x1[3]<-NULL
names(Full182x1)<-c("Companies","Subs")
Full182x2[3]<-NULL
names(Full182x2)<-c("Companies","Subs")
Full182x3[3]<-NULL
names(Full182x3)<-c("Companies","Subs")
Subs18<-bind_rows(Subs18,Full182x1,Full182x2,Full182x3)

#Remove duplicates
Subs18$test<-paste(Subs18$Companies,Subs18$Subs,sep=" ")
Subs18<-Subs18[!duplicated(Subs18$test),]
Subs18[3]<-NULL


#Merge by vendor and if the GUO sold targets in 2017, they are added to the ownership structure of 2018
Deals20183<-Deals2019[,c(3,1,2,3,7)]
names(Deals20183)<-c("Subsidiaries","TargetBvDID183","AcqBvDID183","VendBvDID183","Added")
Full183x<-left_join(Full18B,Deals20183,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full183x$TargetBvDID183)))
names(Subs18)<-c("TargetBvDID183","Subs")
Full183<-left_join(Full183x,Subs18,by="TargetBvDID183",na_matches="never")
length(which(!is.na(Full183$Subs)))
Full183x<-subset(Full183,!is.na(Full183$VendBvDID183))
Full183y<-Full183x[,c(5,3,6)]
names(Full183y)<-c("Company","Subsidiaries","Added")
Full183z<-Full183x[,c(5,7,6)]
names(Full183z)<-c("Company","Subsidiaries","Added")
Full183<-bind_rows(Full183y,Full183z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full183<-left_join(Full18,Full183,by="Subsidiaries",na_matches="never")
Full183<-subset(Full183,!is.na(Full183$Added.y))
Full183<-Full183[,c(1,2,5)]
names(Full183)<-c("Company","Subsidiaries","Added")
Full18<-bind_rows(Full18,Full183)

#Remove duplicate GUO-subsidiary combinations
Full18$test<-paste(Full18$Company,Full18$Subsidiaries,sep=" ")
Full18<-Full18[!duplicated(Full18$test),]
Full18<-subset(Full18,!is.na(Full18$Subsidiaries))
Full18[4]<-NULL
Full18<-Full18[complete.cases(Full18[,1:2]), ]
Full18B<-Full18[1:2]

#Add to the subsidiary data
Full183y[3]<-NULL
names(Full183y)<-c("Companies","Subs")
Full183z[3]<-NULL
names(Full183z)<-c("Companies","Subs")
names(Subs18)<-c("Companies","Subs")
Subs18<-bind_rows(Subs18,Full183y,Full183z)

#Remove duplicates
Subs18$test<-paste(Subs18$Companies,Subs18$Subs,sep=" ")
Subs18<-Subs18[!duplicated(Subs18$test),]
Subs18[3]<-NULL
Subs18<-Subs18[complete.cases(Subs18[,1:2]), ]

#Remove unnecessary files
rm(Full181,Full181x,Full181y,Full182,Full182x,Full182y,Full182z,Deals20181,Deals20182,Deals20183,Full183,
   Full183x,Full183y,Full183z,Full18A,Deals_data1,DealsSubs,Full181a,Full181w,Full181w1,Full181x1,Full181x2,
   Full181w2,Full181yy,Full181z,Full181w,Full182w,Full182w1,Full182w2,Full182x1,Full182x2,Full182x3)


##Ownership 2019
names(Subs18) <- c("Check2","Subs")
##Merge by subsidiaries
Deals20191<-Deals2019[,c(1,1,2,3,7)]
names(Deals20191)<-c("Subsidiaries","TargetBvDID191","AcqBvDID191","VendBvDID191","Added")
Full191<-left_join(Full18B,Deals20191,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full191$TargetBvDID191)))

#Check if the acquiror of the target may still be in the ownership group - if not, delete from the GUO
Full191$AcqBvDID191[is.na(Full191$AcqBvDID191)] <- 0
Full191$Check<-vlookup_df(Full191$AcqBvDID191,Full191,result_column=1,lookup_column=2)
sapply(Full191, class)
Full191$Check<-unlist(Full191$Check)

#If the acquiror is not in the company - exclusion - but what about their subsidiaries? Therefore, add target BvDID for match
Full191$Check2<-ifelse(!is.na(Full191$Check) & Full191$Company!=Full191$Check,Full191$TargetBvDID191,NA)

#Match all subsidiaries of the acquired companies
Full191<-left_join(Full191,Subs18,by="Check2",na_matches="never")
length(which(!is.na(Full191$Subs)))

#Look for the matched subsidiaries in the subsidiaries column and add the acquiror BvDID
Full191$Check3<-vlookup_df(Full191$Subsidiaries,Full191,result_column=4,lookup_column=9)
Full191$Check3<-unlist(Full191$Check3)
sapply(Full191, class)

#Re-check if the matched acquiror of the matched target may still be in the ownership group - if not, delete from the GUO
Full191$Check4<-vlookup_df(Full191$Check3,Full191,result_column=1,lookup_column=2)
Full191$Check4<-unlist(Full191$Check4)

#Check if the new matched acquiror is not part of the GUO structure
Full191$Check5<-case_when(!is.na(Full191$Check) & Full191$Company!=Full191$Check~1,
                          !is.na(Full191$Check4) & Full191$Company!=Full191$Check4~1)
Full191$Check5[is.na(Full191$Check5)] <- 0

#Check if GUO was acquired - exclude all companies
Full191$Check6<-case_when(Full191$TargetBvDID191==Full191$Company~1,
                          Full191$TargetBvDID191!=Full191$Company~0,
                          is.na(Full191$TargetBvDID191)~0)
Full191$Check7<-with (Full191,ave(Full191$Check6,Full191$Company, FUN=sum))

#Exclusion of all acquired GUOs and related companies (Check6 & 7)
Full191$Check8<-ifelse(Full191$Check7==0,Full191$Check5,Full191$Check7)
Full191$Check8[is.na(Full191$Check8)] <- 0
Full191w<-subset(Full191,Full191$Check8>1.1)
Full191x<-subset(Full191,Full191$Check8==1)
Full191<-subset(Full191,Full191$Check8<0.9)

#Add the subsidiaries
Full191w1<-Full191w[c(4,3,6)]
names(Full191w1)<-c("Subsidiaries","Subs","Added")
Full191x1<-Full191x[c(4,3,6)]
names(Full191x1)<-c("Subsidiaries","Subs","Added")
Full191x2<-Full191x[c(4,9,6)]
names(Full191x2)<-c("Subsidiaries","Subs","Added")
Full191a<-bind_rows(Full191w1,Full191x1,Full191x2)
Full191[3:5]<-NULL
Full191[4:12]<-NULL
Full191<-left_join(Full191,Full191a,by="Subsidiaries",na_matches="never")
Full191a<-subset(Full191a,!is.na(Full191a$Added))
Full191a<-Full191[c(1,4,5)]
Full191[4:5]<-NULL
names(Full191)<-c("Company","Subsidiaries","Added")
names(Full191a)<-c("Company","Subsidiaries","Added")
Full191<-bind_rows(Full191,Full191a)
Full191<-Full191[complete.cases(Full191[,1:2]), ]

##Change the list of subsidiaries for the next match
#Delete
Full191y<-Full191w[,c(2,12)]
Full191y[2]<-1
names(Full191y)<-c("Subs","Delete")
Subs19<-left_join(Subs18,Full191y,by="Subs",na_matches="never")
Subs19<-Subs19[is.na(Subs19$Delete),]
Subs19[3]<-NULL
Full191y<-Full191x[,c(2,12)]
Full191y[2]<-1
names(Full191y)<-c("Subs","Delete")
Subs19<-left_join(Subs19,Full191y,by="Subs",na_matches="never")
Subs19<-Subs19[is.na(Subs19$Delete),]

#Add
Full191yy<-Full191w[,c(10,2,12)]
names(Full191yy)<-c("Check2","Subs","Delete")
Full191y<-Full191w[,c(4,2,12)]
names(Full191y)<-c("Check2","Subs","Delete")
Full191z<-Full191x[,c(4,2,12)]
names(Full191z)<-c("Check2","Subs","Delete")
names(Subs19)<-c("Check2","Subs","Delete")
Subs19<-bind_rows(Subs19,Full191yy,Full191y,Full191z)

#Remove duplicates
Subs19$test<-paste(Subs19$Check2,Subs19$Subs,sep=" ")
Subs19<-Subs19[!duplicated(Subs19$test),]
Subs19[3:4]<-NULL


##Merge by Acquiror
Deals20192<-Deals2019[,c(2,1,2,3,7)]
names(Deals20192)<-c("Subsidiaries","TargetBvDID192","AcqBvDID192","VendBvDID192","Added")
Full192<-left_join(Full18B,Deals20192,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full192$TargetBvDID192)))

#Add the subsidiaries of acquired targets
names(Subs19)<-c("TargetBvDID192","Subs")
Full192<-left_join(Full192,Subs19,by="TargetBvDID192",na_matches="never")
length(which(!is.na(Full192$Subs)))

#Lookup the GUOs of target companies
Full192$Check<-vlookup_df(Full192$TargetBvDID192,Full192,result_column=1,lookup_column=2)
Full192$Check<-unlist(Full192$Check)

#If GUO of acquired companies is not the same as acquring GUO - add
Full192$Check2<-ifelse(Full192$Check!=Full192$Company | is.na(Full192$Check),Full192$TargetBvDID192,NA)

#Lookup the GUOs of subsidiaries
Full192$Check3<-vlookup_df(Full192$Subsidiaries,Full192,result_column=1,lookup_column=7)
Full192$Check3<-unlist(Full192$Check3)
Full192$Check4<-vlookup_df(Full192$Subsidiaries,Full192,result_column=4,lookup_column=7) # column 4 for acquiror
Full192$Check4<-unlist(Full192$Check4)
Full192$Check5<-ifelse(Full192$Check3!=Full192$Company,1,NA)
Full192$Check5<-unlist(Full192$Check5)
Full192$Delete1<-ifelse(!is.na(Full192$Check5)|!is.na(Full192$Check2),1,NA)
Full192$Delete2<-ifelse(!is.na(Full192$Check3) & Full192$Company==Full192$Subsidiaries,2,NA)
Full192$Delete2[is.na(Full192$Delete2)] <- 0
Full192$Delete3<-with (Full192,ave(Full192$Delete2,Full192$Company, FUN=sum))
Full192$Delete<-ifelse(Full192$Delete3>1.1,Full192$Delete3,Full192$Delete1)

#Delete File
Full192w<-subset(Full192,!is.na(Full192$Delete))
Full192w1<-Full192w[,c(2,2)]
names(Full192w1)<-c("Subsidiaries","Deletex")
Full192w2<-Full192w[,c(7,7)]
names(Full192w2)<-c("Subsidiaries","Deletex")
Full192w<-bind_rows(Full192w1,Full192w2)
Full192w<-Full192w[complete.cases(Full192w[,1:2]), ]

#Add File
Full192x<-subset(Full192,Full192$Delete>0.9)
Full192x1<-Full192x[,c(11,2,16)]                          #Acquiror
Full192x1<-Full192x1[complete.cases(Full192x1[,1:2]), ]
Full192x2<-Full192x[,c(10,2,16)]
Full192x2<-Full192x2[complete.cases(Full192x2[,1:2]), ]
Full192x3<-Full192x[,c(4,3,16)]
Full192x3<-Full192x3[complete.cases(Full192x3[,1:2]), ]
names(Full192x1)<-c("Company","Subsidiaries","Added")
names(Full192x2)<-c("Company","Subsidiaries","Added")
names(Full192x3)<-c("Company","Subsidiaries","Added")
Full192x1$Added<-1
Full192x2$Added<-1
Full192x3$Added<-1
Full192x<-bind_rows(Full192x2,Full192x3)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full192<-left_join(Full191,Full192x,by="Subsidiaries",na_matches="never")
Full192<-subset(Full192,!is.na(Full192$Added.y))
Full192[3:4]<-NULL
names(Full192)<-c("Company","Subsidiaries","Added")
Full192$test<-paste(Full192$Companies,Full192$Subsidiaries,sep=" ")
Full192<-Full192[!duplicated(Full192$test),]
Full192[4]<-NULL
Full19<-bind_rows(Full191,Full192)
Full19$Added<-ifelse(!is.na(Full19$Added),3,NA)
Full19B<-Full19[1:2]

##Change the list of subsidiaries for the next match
#Remove
Full192w1[3]<-NULL
names(Full192w1)<-c("Subs","Delete")
names(Subs19)<-c("Companies","Subs")
Subs19<-left_join(Subs19,Full192w1,by="Subs",na_matches="never")
Subs19<-Subs19[is.na(Subs19$Delete),]
Subs19[3]<-NULL
Full192w2[3]<-NULL
names(Full192w2)<-c("Subs","Delete")
names(Subs19)<-c("Companies","Subs")
Subs19<-left_join(Subs19,Full192w2,by="Subs",na_matches="never")
Subs19<-Subs19[is.na(Subs19$Delete),]
Subs19[3]<-NULL

#Add
Full192x1[3]<-NULL
names(Full192x1)<-c("Companies","Subs")
Full192x2[3]<-NULL
names(Full192x2)<-c("Companies","Subs")
Full192x3[3]<-NULL
names(Full192x3)<-c("Companies","Subs")
Subs19<-bind_rows(Subs19,Full192x1,Full192x2,Full192x3)

#Remove duplicates
Subs19$test<-paste(Subs19$Companies,Subs19$Subs,sep=" ")
Subs19<-Subs19[!duplicated(Subs19$test),]
Subs19[3]<-NULL


#Merge by vendor and if the GUO sold targets in 2018, they are added to the ownership structure of 2019
Deals20193<-Deals2020[,c(3,1,2,3,7)]
names(Deals20193)<-c("Subsidiaries","TargetBvDID193","AcqBvDID193","VendBvDID193","Added")
Full193x<-left_join(Full19B,Deals20193,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full193x$TargetBvDID193)))
names(Subs19)<-c("TargetBvDID193","Subs")
Full193<-left_join(Full193x,Subs19,by="TargetBvDID193",na_matches="never")
length(which(!is.na(Full193$Subs)))
Full193x<-subset(Full193,!is.na(Full193$VendBvDID193))
Full193y<-Full193x[,c(5,3,6)]
names(Full193y)<-c("Company","Subsidiaries","Added")
Full193z<-Full193x[,c(5,7,6)]
names(Full193z)<-c("Company","Subsidiaries","Added")
Full193<-bind_rows(Full193y,Full193z)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full193<-left_join(Full19,Full193,by="Subsidiaries",na_matches="never")
Full193<-subset(Full193,!is.na(Full193$Added.y))
Full193<-Full193[,c(1,2,5)]
names(Full193)<-c("Company","Subsidiaries","Added")
Full19<-bind_rows(Full19,Full193)

#Remove duplicate GUO-subsidiary combinations
Full19$test<-paste(Full19$Company,Full19$Subsidiaries,sep=" ")
Full19<-Full19[!duplicated(Full19$test),]
Full19<-subset(Full19,!is.na(Full19$Subsidiaries))
Full19[4]<-NULL
Full19<-Full19[complete.cases(Full19[,1:2]), ]
Full19B<-Full19[1:2]

#Add to the subsidiary data
Full193y[3]<-NULL
names(Full193y)<-c("Companies","Subs")
Full193z[3]<-NULL
names(Full193z)<-c("Companies","Subs")
names(Subs19)<-c("Companies","Subs")
Subs19<-bind_rows(Subs19,Full193y,Full193z)

#Remove duplicates
Subs19$test<-paste(Subs19$Companies,Subs19$Subs,sep=" ")
Subs19<-Subs19[!duplicated(Subs19$test),]
Subs19[3]<-NULL
Subs19<-Subs19[complete.cases(Subs19[,1:2]), ]

#Remove unnecessary files
rm(Full191,Full191x,Full191y,Full192,Full192x,Full192y,Full192z,Deals20191,Deals20192,Deals20193,Full193,
   Full193x,Full193y,Full193z,Full19A,Deals_data1,DealsSubs,Full191a,Full191w,Full191w1,Full191x1,Full191x2,
   Full191w2,Full191yy,Full191z,Full191w,Full192w,Full192w1,Full192w2,Full192x1,Full192x2,Full192x3)


##Ownership 2020
names(Subs19) <- c("Check2","Subs")
##Merge by subsidiaries
Deals20201<-Deals2020[,c(1,1,2,3,7)]
names(Deals20201)<-c("Subsidiaries","TargetBvDID201","AcqBvDID201","VendBvDID201","Added")
Full201<-left_join(Full19B,Deals20201,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full201$TargetBvDID201)))

#Check if the acquiror of the target may still be in the ownership group - if not, delete from the GUO
Full201$AcqBvDID201[is.na(Full201$AcqBvDID201)] <- 0
Full201$Check<-vlookup_df(Full201$AcqBvDID201,Full201,result_column=1,lookup_column=2)
sapply(Full201, class)
Full201$Check<-unlist(Full201$Check)

#If the acquiror is not in the company - exclusion - but what about their subsidiaries? Therefore, add target BvDID for match
Full201$Check2<-ifelse(!is.na(Full201$Check) & Full201$Company!=Full201$Check,Full201$TargetBvDID201,NA)

#Match all subsidiaries of the acquired companies
Full201<-left_join(Full201,Subs19,by="Check2",na_matches="never")
length(which(!is.na(Full201$Subs)))

#Look for the matched subsidiaries in the subsidiaries column and add the acquiror BvDID
Full201$Check3<-vlookup_df(Full201$Subsidiaries,Full201,result_column=4,lookup_column=9)
Full201$Check3<-unlist(Full201$Check3)
sapply(Full201, class)

#Re-check if the matched acquiror of the matched target may still be in the ownership group - if not, delete from the GUO
Full201$Check4<-vlookup_df(Full201$Check3,Full201,result_column=1,lookup_column=2)
Full201$Check4<-unlist(Full201$Check4)

#Check if the new matched acquiror is not part of the GUO structure
Full201$Check5<-case_when(!is.na(Full201$Check) & Full201$Company!=Full201$Check~1,
                          !is.na(Full201$Check4) & Full201$Company!=Full201$Check4~1)
Full201$Check5[is.na(Full201$Check5)] <- 0

#Check if GUO was acquired - exclude all companies
Full201$Check6<-case_when(Full201$TargetBvDID201==Full201$Company~1,
                          Full201$TargetBvDID201!=Full201$Company~0,
                          is.na(Full201$TargetBvDID201)~0)
Full201$Check7<-with (Full201,ave(Full201$Check6,Full201$Company, FUN=sum))

#Exclusion of all acquired GUOs and related companies (Check6 & 7)
Full201$Check8<-ifelse(Full201$Check7==0,Full201$Check5,Full201$Check7)
Full201$Check8[is.na(Full201$Check8)] <- 0
Full201w<-subset(Full201,Full201$Check8>1.1)
Full201x<-subset(Full201,Full201$Check8==1)
Full201<-subset(Full201,Full201$Check8<0.9)

#Add the subsidiaries
Full201w1<-Full201w[c(4,3,6)]
names(Full201w1)<-c("Subsidiaries","Subs","Added")
Full201x1<-Full201x[c(4,3,6)]
names(Full201x1)<-c("Subsidiaries","Subs","Added")
Full201x2<-Full201x[c(4,9,6)]
names(Full201x2)<-c("Subsidiaries","Subs","Added")
Full201a<-bind_rows(Full201w1,Full201x1,Full201x2)
Full201[3:5]<-NULL
Full201[4:12]<-NULL
Full201<-left_join(Full201,Full201a,by="Subsidiaries",na_matches="never")
Full201a<-subset(Full201a,!is.na(Full201a$Added))
Full201a<-Full201[c(1,4,5)]
Full201[4:5]<-NULL
names(Full201)<-c("Company","Subsidiaries","Added")
names(Full201a)<-c("Company","Subsidiaries","Added")
Full201<-bind_rows(Full201,Full201a)
Full201<-Full201[complete.cases(Full201[,1:2]), ]

##Change the list of subsidiaries for the next match
#Delete
Full201y<-Full201w[,c(2,12)]
Full201y[2]<-1
names(Full201y)<-c("Subs","Delete")
Subs20<-left_join(Subs19,Full201y,by="Subs",na_matches="never")
Subs20<-Subs20[is.na(Subs20$Delete),]
Subs20[3]<-NULL
Full201y<-Full201x[,c(2,12)]
Full201y[2]<-1
names(Full201y)<-c("Subs","Delete")
Subs20<-left_join(Subs20,Full201y,by="Subs",na_matches="never")
Subs20<-Subs20[is.na(Subs20$Delete),]

#Add
Full201yy<-Full201w[,c(10,2,12)]
names(Full201yy)<-c("Check2","Subs","Delete")
Full201y<-Full201w[,c(4,2,12)]
names(Full201y)<-c("Check2","Subs","Delete")
Full201z<-Full201x[,c(4,2,12)]
names(Full201z)<-c("Check2","Subs","Delete")
names(Subs20)<-c("Check2","Subs","Delete")
Subs20<-bind_rows(Subs20,Full201yy,Full201y,Full201z)

#Remove duplicates
Subs20$test<-paste(Subs20$Check2,Subs20$Subs,sep=" ")
Subs20<-Subs20[!duplicated(Subs20$test),]
Subs20[3:4]<-NULL


##Merge by Acquiror
Deals20202<-Deals2020[,c(2,1,2,3,7)]
names(Deals20202)<-c("Subsidiaries","TargetBvDID202","AcqBvDID202","VendBvDID202","Added")
Full202<-left_join(Full19B,Deals20202,by="Subsidiaries",na_matches="never")
length(which(!is.na(Full202$TargetBvDID202)))

#Add the subsidiaries of acquired targets
names(Subs20)<-c("TargetBvDID202","Subs")
Full202<-left_join(Full202,Subs20,by="TargetBvDID202",na_matches="never")
length(which(!is.na(Full202$Subs)))

#Lookup the GUOs of target companies
Full202$Check<-vlookup_df(Full202$TargetBvDID202,Full202,result_column=1,lookup_column=2)
Full202$Check<-unlist(Full202$Check)

#If GUO of acquired companies is not the same as acquring GUO - add
Full202$Check2<-ifelse(Full202$Check!=Full202$Company | is.na(Full202$Check),Full202$TargetBvDID202,NA)

#Lookup the GUOs of subsidiaries
Full202$Check3<-vlookup_df(Full202$Subsidiaries,Full202,result_column=1,lookup_column=7)
Full202$Check3<-unlist(Full202$Check3)
Full202$Check4<-vlookup_df(Full202$Subsidiaries,Full202,result_column=4,lookup_column=7) # column 4 for acquiror
Full202$Check4<-unlist(Full202$Check4)
Full202$Check5<-ifelse(Full202$Check3!=Full202$Company,1,NA)
Full202$Check5<-unlist(Full202$Check5)
Full202$Delete1<-ifelse(!is.na(Full202$Check5)|!is.na(Full202$Check2),1,NA)
Full202$Delete2<-ifelse(!is.na(Full202$Check3) & Full202$Company==Full202$Subsidiaries,2,NA)
Full202$Delete2[is.na(Full202$Delete2)] <- 0
Full202$Delete3<-with (Full202,ave(Full202$Delete2,Full202$Company, FUN=sum))
Full202$Delete<-ifelse(Full202$Delete3>1.1,Full202$Delete3,Full202$Delete1)

#Delete File
Full202w<-subset(Full202,!is.na(Full202$Delete))
Full202w1<-Full202w[,c(2,2)]
names(Full202w1)<-c("Subsidiaries","Deletex")
Full202w2<-Full202w[,c(7,7)]
names(Full202w2)<-c("Subsidiaries","Deletex")
Full202w<-bind_rows(Full202w1,Full202w2)
Full202w<-Full202w[complete.cases(Full202w[,1:2]), ]

#Add File
Full202x<-subset(Full202,Full202$Delete>0.9)
Full202x1<-Full202x[,c(11,2,16)]                          #Acquiror
Full202x1<-Full202x1[complete.cases(Full202x1[,1:2]), ]
Full202x2<-Full202x[,c(10,2,16)]
Full202x2<-Full202x2[complete.cases(Full202x2[,1:2]), ]
Full202x3<-Full202x[,c(4,3,16)]
Full202x3<-Full202x3[complete.cases(Full202x3[,1:2]), ]
names(Full202x1)<-c("Company","Subsidiaries","Added")
names(Full202x2)<-c("Company","Subsidiaries","Added")
names(Full202x3)<-c("Company","Subsidiaries","Added")
Full202x1$Added<-1
Full202x2$Added<-1
Full202x3$Added<-1
Full202x<-bind_rows(Full202x2,Full202x3)

#Add and remove the targets of the GUO as the acquiror to the ownership structure
Full202<-left_join(Full201,Full202x,by="Subsidiaries",na_matches="never")
Full202<-subset(Full202,!is.na(Full202$Added.y))
Full202[3:4]<-NULL
names(Full202)<-c("Company","Subsidiaries","Added")
Full202$test<-paste(Full202$Companies,Full202$Subsidiaries,sep=" ")
Full202<-Full202[!duplicated(Full202$test),]
Full202[4]<-NULL
Full20<-bind_rows(Full201,Full202)
Full20$Added<-ifelse(!is.na(Full20$Added),3,NA)
Full20B<-Full20[1:2]

##Change the list of subsidiaries for the next match
#Remove
Full202w1[3]<-NULL
names(Full202w1)<-c("Subs","Delete")
names(Subs20)<-c("Companies","Subs")
Subs20<-left_join(Subs20,Full202w1,by="Subs",na_matches="never")
Subs20<-Subs20[is.na(Subs20$Delete),]
Subs20[3]<-NULL
Full202w2[3]<-NULL
names(Full202w2)<-c("Subs","Delete")
names(Subs20)<-c("Companies","Subs")
Subs20<-left_join(Subs20,Full202w2,by="Subs",na_matches="never")
Subs20<-Subs20[is.na(Subs20$Delete),]
Subs20[3]<-NULL

#Add
Full202x1[3]<-NULL
names(Full202x1)<-c("Companies","Subs")
Full202x2[3]<-NULL
names(Full202x2)<-c("Companies","Subs")
Full202x3[3]<-NULL
names(Full202x3)<-c("Companies","Subs")
Subs20<-bind_rows(Subs20,Full202x1,Full202x2,Full202x3)

#Remove duplicates
Subs20$test<-paste(Subs20$Companies,Subs20$Subs,sep=" ")
Subs20<-Subs20[!duplicated(Subs20$test),]
Subs20[3]<-NULL

#Remove unnecessary files
rm(Full201,Full201x,Full201y,Full202,Full202x,Full202y,Full202z,Deals20201,Deals20202,Deals20203,Full203,
   Full203x,Full203y,Full203z,Full20A,Deals_data1,DealsSubs,Full201a,Full201w,Full201w1,Full201x1,Full201x2,
   Full201w2,Full201yy,Full201z,Full201w,Full202w,Full202w1,Full202w2,Full202x1,Full202x2,Full202x3)

#Only complete cases
Full20<-Full20[complete.cases(Full20[,1:2]), ]
Own<-Own[complete.cases(Own[,1:2]), ]

#Remove unnecessary files
rm(Full201,Full201x,Full201y,Full202,Full202x,Full202y,Full202z,Deals20201,Deals20202,Deals20203,
   Full203,Full203x,Full203y,Full203z,Full20A,Full201,Full201x,Full201y,Full202,Full202x,Full202y,
   Full202z,Deals20201,Deals20202,Deals20203,Full203,Full203x,Full203y,Full203z,Full20A,Full10B,
   Full11B,Full12B,Full13B,Full14B,Full15B,Full16B,Full17B,Full18B,Full19B,Full20B,Full10A,Full11A,
   Full12A,Full13A,Full14A,Full15A,Full16A,Full17A,Full18A,Full19A,Full20A,Deals2010,Deals2011,
   Deals2012,Deals2013,Deals2014,Deals2015,Deals2016,Deals2017,Deals2018,Deals2019,Deals2020,
   Subs10,Subs11,Subs12,Subs13,Subs14,Subs15,Subs16,Subs17,Subs18,Subs19,Subs20,DealsSubs_Files,DealsSubs_Files,Deals)

##Check for consistency of original and calculated 2020 ownership data
#Own: 3018023 - Full20 - 3.017.324; Difference of 700
Own$Check<-paste(Own$Company,Own$Subsidiaries,sep=" ")
Full20$Check<-paste(Full20$Company,Full20$Subsidiaries,sep=" ")
Full20$Check1<-ifelse(Full20$Subsidiaries %in% Own$Subsidiaries,1,NA)
length(which(is.na(Full20$Check1)))                                     # 676 subsidiaries
Own$Check1<-ifelse(Own$Subsidiaries %in% Full20$Subsidiaries,1,NA)
length(which(is.na(Own$Check1)))                                        # 48501 subsidiaries
Full20$Check2<-ifelse(Full20$Check %in% Own$Check,1,NA)
length(which(is.na(Full20$Check2)))                                     # 33116 subsidiaries
Own$Check2<-ifelse(Own$Check %in% Full20$Check,1,NA)
length(which(is.na(Own$Check2)))                                        # 77421 subsidiaries; number checked on 18/08/2021
Full20[4:6]<-NULL







#   2. Account for the date of incorporation ####

#Attach date of incorporation dataset
Date_files<-list.files(path="Dataset/Date",pattern ='.xlsx', full.names = T)
Date_data <- sapply(Date_files, read_excel, sheet=2,simplify=FALSE) %>% 
   bind_rows(.id = "id")

##Restructure dataset
#Remove Row of Numbers and Company Names + Rename/Order Columns
Date_data[1:3] <- NULL
names(Date_data)<-c("Subsidiaries","Date")

#Indicate missing values
Date_data<-na_if(Date_data,"n.a.")
sapply(Date_data, class)

#Extract year of incorporation
Date_data$Date<-str_sub(Date_data$Date,-4,-1)

#Indicate numeric variables
Date_data$Date<-as.numeric(Date_data$Date)
sapply(Date_data, class)

#Merge the yearly ownership files and remove subsidiaries founded after year of interest
Full10<-left_join(Full10,Date_data,by="Subsidiaries",na_matches="never")
length(which(is.na(Full10$Date)))
Full10<-subset(Full10,Full10$Date<2011 | is.na(Full10$Date))

Full11<-left_join(Full11,Date_data,by="Subsidiaries",na_matches="never")
length(which(is.na(Full11$Date)))
Full11<-subset(Full11,Full11$Date<2012 | is.na(Full11$Date))

Full12<-left_join(Full12,Date_data,by="Subsidiaries",na_matches="never")
length(which(is.na(Full12$Date)))
Full12<-subset(Full12,Full12$Date<2013 | is.na(Full12$Date))

Full13<-left_join(Full13,Date_data,by="Subsidiaries",na_matches="never")
length(which(is.na(Full13$Date)))
Full13<-subset(Full13,Full13$Date<2014 | is.na(Full13$Date))

Full14<-left_join(Full14,Date_data,by="Subsidiaries",na_matches="never")
length(which(is.na(Full14$Date)))
Full14<-subset(Full14,Full14$Date<2015 | is.na(Full14$Date))

Full15<-left_join(Full15,Date_data,by="Subsidiaries",na_matches="never")
length(which(is.na(Full15$Date)))
Full15<-subset(Full15,Full15$Date<2016 | is.na(Full15$Date))

Full16<-left_join(Full16,Date_data,by="Subsidiaries",na_matches="never")
length(which(is.na(Full16$Date)))
Full16<-subset(Full16,Full16$Date<2017 | is.na(Full16$Date))

Full17<-left_join(Full17,Date_data,by="Subsidiaries",na_matches="never")
length(which(is.na(Full17$Date)))
Full17<-subset(Full17,Full17$Date<2018 | is.na(Full17$Date))

Full18<-left_join(Full18,Date_data,by="Subsidiaries",na_matches="never")
length(which(is.na(Full18$Date)))
Full18<-subset(Full18,Full18$Date<2019| is.na(Full18$Date))

Full19<-left_join(Full19,Date_data,by="Subsidiaries",na_matches="never")
length(which(is.na(Full19$Date)))
Full19<-subset(Full19,Full19$Date<=2020 | is.na(Full19$Date))

Full20<-left_join(Full20,Date_data,by="Subsidiaries",na_matches="never")
length(which(is.na(Full20$Date)))
Full20<-subset(Full20,Full20$Date<=2021 | is.na(Full20$Date))

#Create a year variable
Full10$Year<-2010
Full11$Year<-2011
Full12$Year<-2012
Full13$Year<-2013
Full14$Year<-2014
Full15$Year<-2015
Full16$Year<-2016
Full17$Year<-2017
Full18$Year<-2018
Full19$Year<-2019
Full20$Year<-2020

#Change columns order
col_order <- c("Company","Subsidiaries","Date","Year","Added")

Full10<-Full10[, col_order]
Full11<-Full11[, col_order]
Full12<-Full12[, col_order]
Full13<-Full13[, col_order]
Full14<-Full14[, col_order]
Full15<-Full15[, col_order]
Full16<-Full16[, col_order]
Full17<-Full17[, col_order]
Full18<-Full18[, col_order]
Full19<-Full19[, col_order]
Full20<-Full20[, col_order]

#Create new dummy for greenfield and brownfield investment (1=brownfield based on deals data, 2=Greenfield based on inc. data)
Full10$Added<-case_when(Full10$Added == 3~"3",
                        Full10$Date == 2010~"2")
Full11$Added<-case_when(Full11$Added == 3~"3",
                        Full11$Date == 2011~"2")
Full12$Added<-case_when(Full12$Added == 3~"3",
                        Full12$Date == 2012~"2")
Full13$Added<-case_when(Full13$Added == 3~"3",
                        Full13$Date == 2013~"2")
Full14$Added<-case_when(Full14$Added == 3~"3",
                        Full14$Date == 2014~"2")
Full15$Added<-case_when(Full15$Added == 3~"3",
                        Full15$Date == 2015~"2")
Full16$Added<-case_when(Full16$Added == 3~"3",
                        Full16$Date == 2016~"2")
Full17$Added<-case_when(Full17$Added == 3~"3",
                        Full17$Date == 2017~"2")
Full18$Added<-case_when(Full18$Added == 3~"3",
                        Full18$Date == 2018~"2")
Full19$Added<-case_when(Full19$Added == 3~"3",
                        Full19$Date == 2019~"2")
Full20$Added<-case_when(Full20$Added == 3~"3",
                        Full20$Date == 2020~"2")

## Write interim files with GUO-Subsidiary-Year-Added
write.csv2(Full11, file = "files_created_code1/MNE11.csv",row.names = F) #error in the original code (18/08): Full1 written instead of Full11
write.csv2(Full12, file = "files_created_code1/MNE12.csv",row.names = F) #error in the original code (18/08): Full2 written instead of Full12
write.csv2(Full13, file = "files_created_code1/MNE13.csv",row.names = F) #error in the original code (18/08): Full3 written instead of Full13
write.csv2(Full14, file = "files_created_code1/MNE14.csv",row.names = F) #error in the original code (18/08): Full4 written instead of Full14
write.csv2(Full15, file = "files_created_code1/MNE15.csv",row.names = F) #error in the original code (18/08): Full5 written instead of Full15
write.csv2(Full16, file = "files_created_code1/MNE16.csv",row.names = F) #error in the original code (18/08): Full6 written instead of Full16
write.csv2(Full17, file = "files_created_code1/MNE17.csv",row.names = F) #error in the original code (18/08): Full7 written instead of Full17
write.csv2(Full18, file = "files_created_code1/MNE18.csv",row.names = F) #error in the original code (18/08): Full8 written instead of Full18
write.csv2(Full19, file = "files_created_code1/MNE19.csv",row.names = F) #error in the original code (18/08): Full9 written instead of Full19

#new lines added: ----
write.csv2(Full10, file = "files_created_code1/MNE00.csv",row.names = F) 
write.csv2(Full20, file = "files_created_code1/MNE20.csv",row.names = F) 

##End of main MNE code

#In the next part, some additional files will be created (Full1.csv, Full2.csv, ...Full9.csv, PatentStock.csv, Stock2011.csv,
#Stock2012.csv, ...Stock2019.csv,Patents2011.csv, Patents2012.csv, Patents2019.csv). These files are used
#in Code3 to create a descriptive file named Full.csv.
#### <  ####




### Part III: Creating variables based on the GUO-subsidiary data ####
#clean environment
#rm(list=ls())

#Please pay attention to the following: depending on your system, dec="," has to be adjusted for dec=".". You
#can see which one works best for you by reading the Full files (e.g.Full1.csv, Full2.csv,...). Read them an look
#at the GDP variable; it should be in a numeric format like 3.45e+09, instead of a character format like 3449688452,87021


#read the files created before:
#Full11 <- fread("files_created_code1/MNE11.csv", dec=",")
#Full12 <- fread("files_created_code1/MNE12.csv", dec=",")
#Full13 <- fread("files_created_code1/MNE13.csv", dec=",")
#Full14 <- fread("files_created_code1/MNE14.csv", dec=",")
#Full15 <- fread("files_created_code1/MNE15.csv", dec=",")
#Full16 <- fread("files_created_code1/MNE16.csv", dec=",")
#Full17 <- fread("files_created_code1/MNE17.csv", dec=",")
#Full18 <- fread("files_created_code1/MNE18.csv", dec=",")
#Full19 <- fread("files_created_code1/MNE19.csv", dec=",")

# 1. Create variables like number of subsidiaries and number of countries operating in - by year! ####
##Subsidiary data 2011
#Create variable for total number of subsidiaries
Full11$NoSub2<-ifelse(Full11$Company!=Full11$Subsidiaries,1,0)
Full11$NoSub2011<-with (Full11,ave(Full11$NoSub2,Full11$Company, FUN=sum))
Full11[,3]<-NULL

#Create variable for country of Subsidiaries
Full11$Country1<-as.character(Full11$Company)
Full11$Country2<-as.character(Full11$Subsidiaries)
Full11$Country1<-substr(Full11$Company, 1, 2)
Full11$Country2<-substr(Full11$Subsidiaries,1,2)

#Create a variable of foreign subsidiaries
Full11$ForeignSub1<-ifelse(Full11$Country2!=Full11$Country1,1,0)
Full11$ForeignSub1<-as.numeric(Full11$ForeignSub1)
Full11$ForSub2011<- with(Full11, ave(Full11$ForeignSub1,Full11$Company, FUN=sum))

#Share of foreign subsidiaries
Full11$MultiSub2011<-Full11$ForSub2011 / Full11$NoSub2011

#Variable of number of countries
Full11$Countries1<-ifelse(Full11$Country1!=Full11$Country2 & Full11$Country2!=lag(Full11$Country2),1,0)
Full11$Countries1<-as.numeric(Full11$Countries1)
Full11$Countries2011<- with(Full11, ave(Full11$Countries1,Full11$Company, FUN=sum))

#Average number of subsidiaries per host country
Full11$AvSub4<-case_when(Full11$Country1!=Full11$Country2 & Full11$Country2!=lag(Full11$Country2)~seq(1,1649863, by=1),  # Increase from 1649863
                         Full11$Country1==Full11$Country2~0)
Full11$AvSub4<-na.locf(Full11$AvSub4)
Full11$AvSub3<- with(Full11, ave(Full11$ForeignSub1,Full11$AvSub4, FUN=sum))
Full11$AvSub2<- ifelse(Full11$Country1!=Full11$Country2 & Full11$Country2!=lag(Full11$Country2),Full11$AvSub3,0)
Full11$AvSub1<-with(Full11, ave(Full11$AvSub2,Full11$Company, FUN=sum))
Full11$AvSub2011<-Full11$AvSub1/Full11$Countries2011


##Subsidiary data 2012
#Create variable for total number of subsidiaries
Full12$NoSub2<-ifelse(Full12$Company!=Full12$Subsidiaries,1,0)
Full12$NoSub2012<-with (Full12,ave(Full12$NoSub2,Full12$Company, FUN=sum))
Full12[,3]<-NULL

#Create variable for country of Subsidiaries
Full12$Country1<-as.character(Full12$Company)
Full12$Country2<-as.character(Full12$Subsidiaries)
Full12$Country1<-substr(Full12$Company, 1, 2)
Full12$Country2<-substr(Full12$Subsidiaries,1,2)

#Create a variable of foreign subsidiaries
Full12$ForeignSub1<-ifelse(Full12$Country2!=Full12$Country1,1,0)
Full12$ForeignSub1<-as.numeric(Full12$ForeignSub1)
Full12$ForSub2012<- with(Full12, ave(Full12$ForeignSub1,Full12$Company, FUN=sum))

#Share of foreign subsidiaries
Full12$MultiSub2012<-Full12$ForSub2012 / Full12$NoSub2012

#Variable of number of countries
Full12$Countries1<-ifelse(Full12$Country1!=Full12$Country2 & Full12$Country2!=lag(Full12$Country2),1,0)
Full12$Countries1<-as.numeric(Full12$Countries1)
Full12$Countries2012<- with(Full12, ave(Full12$Countries1,Full12$Company, FUN=sum))

#Average number of subsidiaries per host country
Full12$AvSub4<-case_when(Full12$Country1!=Full12$Country2 & Full12$Country2!=lag(Full12$Country2)~seq(1,1696713, by=1),  # Increase from 2654903
                         Full12$Country1==Full12$Country2~0)
Full12$AvSub4<-na.locf(Full12$AvSub4)
Full12$AvSub3<- with(Full12, ave(Full12$ForeignSub1,Full12$AvSub4, FUN=sum))
Full12$AvSub2<- ifelse(Full12$Country1!=Full12$Country2 & Full12$Country2!=lag(Full12$Country2),Full12$AvSub3,0)
Full12$AvSub1<-with(Full12, ave(Full12$AvSub2,Full12$Company, FUN=sum))
Full12$AvSub2012<-Full12$AvSub1/Full12$Countries2012


##Subsidiary data 2013
#Create variable for total number of subsidiaries
Full13$NoSub2<-ifelse(Full13$Company!=Full13$Subsidiaries,1,0)
Full13$NoSub2013<-with (Full13,ave(Full13$NoSub2,Full13$Company, FUN=sum))
Full13[,3]<-NULL

#Create variable for country of Subsidiaries
Full13$Country1<-as.character(Full13$Company)
Full13$Country2<-as.character(Full13$Subsidiaries)
Full13$Country1<-substr(Full13$Company, 1, 2)
Full13$Country2<-substr(Full13$Subsidiaries,1,2)

#Create a variable of foreign subsidiaries
Full13$ForeignSub1<-ifelse(Full13$Country2!=Full13$Country1,1,0)
Full13$ForeignSub1<-as.numeric(Full13$ForeignSub1)
Full13$ForSub2013<- with(Full13, ave(Full13$ForeignSub1,Full13$Company, FUN=sum))

#Share of foreign subsidiaries
Full13$MultiSub2013<-Full13$ForSub2013 / Full13$NoSub2013

#Variable of number of countries
Full13$Countries1<-ifelse(Full13$Country1!=Full13$Country2 & Full13$Country2!=lag(Full13$Country2),1,0)
Full13$Countries1<-as.numeric(Full13$Countries1)
Full13$Countries2013<- with(Full13, ave(Full13$Countries1,Full13$Company, FUN=sum))

#Average number of subsidiaries per host country
Full13$AvSub4<-case_when(Full13$Country1!=Full13$Country2 & Full13$Country2!=lag(Full13$Country2)~seq(1,1747034, by=1),  # Increase from 2654903
                         Full13$Country1==Full13$Country2~0)
Full13$AvSub4<-na.locf(Full13$AvSub4)
Full13$AvSub3<- with(Full13, ave(Full13$ForeignSub1,Full13$AvSub4, FUN=sum))
Full13$AvSub2<- ifelse(Full13$Country1!=Full13$Country2 & Full13$Country2!=lag(Full13$Country2),Full13$AvSub3,0)
Full13$AvSub1<-with(Full13, ave(Full13$AvSub2,Full13$Company, FUN=sum))
Full13$AvSub2013<-Full13$AvSub1/Full13$Countries2013


##Subsidiary data 2014
#Create variable for total number of subsidiaries
Full14$NoSub2<-ifelse(Full14$Company!=Full14$Subsidiaries,1,0)
Full14$NoSub2014<-with (Full14,ave(Full14$NoSub2,Full14$Company, FUN=sum))
Full14[,3]<-NULL

#Create variable for country of Subsidiaries
Full14$Country1<-as.character(Full14$Company)
Full14$Country2<-as.character(Full14$Subsidiaries)
Full14$Country1<-substr(Full14$Company, 1, 2)
Full14$Country2<-substr(Full14$Subsidiaries,1,2)

#Create a variable of foreign subsidiaries
Full14$ForeignSub1<-ifelse(Full14$Country2!=Full14$Country1,1,0)
Full14$ForeignSub1<-as.numeric(Full14$ForeignSub1)
Full14$ForSub2014<- with(Full14, ave(Full14$ForeignSub1,Full14$Company, FUN=sum))

#Share of foreign subsidiaries
Full14$MultiSub2014<-Full14$ForSub2014 / Full14$NoSub2014

#Variable of number of countries
Full14$Countries1<-ifelse(Full14$Country1!=Full14$Country2 & Full14$Country2!=lag(Full14$Country2),1,0)
Full14$Countries1<-as.numeric(Full14$Countries1)
Full14$Countries2014<- with(Full14, ave(Full14$Countries1,Full14$Company, FUN=sum))

#Average number of subsidiaries per host country
Full14$AvSub4<-case_when(Full14$Country1!=Full14$Country2 & Full14$Country2!=lag(Full14$Country2)~seq(1,1798762, by=1),  # Increase from 2654903
                         Full14$Country1==Full14$Country2~0)
Full14$AvSub4<-na.locf(Full14$AvSub4)
Full14$AvSub3<- with(Full14, ave(Full14$ForeignSub1,Full14$AvSub4, FUN=sum))
Full14$AvSub2<- ifelse(Full14$Country1!=Full14$Country2 & Full14$Country2!=lag(Full14$Country2),Full14$AvSub3,0)
Full14$AvSub1<-with(Full14, ave(Full14$AvSub2,Full14$Company, FUN=sum))
Full14$AvSub2014<-Full14$AvSub1/Full14$Countries2014


##Subsidiary data 2015
#Create variable for total number of subsidiaries
Full15$NoSub2<-ifelse(Full15$Company!=Full15$Subsidiaries,1,0)
Full15$NoSub2015<-with (Full15,ave(Full15$NoSub2,Full15$Company, FUN=sum))
Full15[,3]<-NULL

#Create variable for country of Subsidiaries
Full15$Country1<-as.character(Full15$Company)
Full15$Country2<-as.character(Full15$Subsidiaries)
Full15$Country1<-substr(Full15$Company, 1, 2)
Full15$Country2<-substr(Full15$Subsidiaries,1,2)

#Create a variable of foreign subsidiaries
Full15$ForeignSub1<-ifelse(Full15$Country2!=Full15$Country1,1,0)
Full15$ForeignSub1<-as.numeric(Full15$ForeignSub1)
Full15$ForSub2015<- with(Full15, ave(Full15$ForeignSub1,Full15$Company, FUN=sum))

#Share of foreign subsidiaries
Full15$MultiSub2015<-Full15$ForSub2015 / Full15$NoSub2015

#Variable of number of countries
Full15$Countries1<-ifelse(Full15$Country1!=Full15$Country2 & Full15$Country2!=lag(Full15$Country2),1,0)
Full15$Countries1<-as.numeric(Full15$Countries1)
Full15$Countries2015<- with(Full15, ave(Full15$Countries1,Full15$Company, FUN=sum))

#Average number of subsidiaries per host country
Full15$AvSub4<-case_when(Full15$Country1!=Full15$Country2 & Full15$Country2!=lag(Full15$Country2)~seq(1,1852586, by=1),  # Increase from 2654903
                         Full15$Country1==Full15$Country2~0)
Full15$AvSub4<-na.locf(Full15$AvSub4)
Full15$AvSub3<- with(Full15, ave(Full15$ForeignSub1,Full15$AvSub4, FUN=sum))
Full15$AvSub2<- ifelse(Full15$Country1!=Full15$Country2 & Full15$Country2!=lag(Full15$Country2),Full15$AvSub3,0)
Full15$AvSub1<-with(Full15, ave(Full15$AvSub2,Full15$Company, FUN=sum))
Full15$AvSub2015<-Full15$AvSub1/Full15$Countries2015


##Subsidiary data 2016
#Create variable for total number of subsidiaries
Full16$NoSub2<-ifelse(Full16$Company!=Full16$Subsidiaries,1,0)
Full16$NoSub2016<-with (Full16,ave(Full16$NoSub2,Full16$Company, FUN=sum))
Full16[,3]<-NULL

#Create variable for country of Subsidiaries
Full16$Country1<-as.character(Full16$Company)
Full16$Country2<-as.character(Full16$Subsidiaries)
Full16$Country1<-substr(Full16$Company, 1, 2)
Full16$Country2<-substr(Full16$Subsidiaries,1,2)

#Create a variable of foreign subsidiaries
Full16$ForeignSub1<-ifelse(Full16$Country2!=Full16$Country1,1,0)
Full16$ForeignSub1<-as.numeric(Full16$ForeignSub1)
Full16$ForSub2016<- with(Full16, ave(Full16$ForeignSub1,Full16$Company, FUN=sum))

#Share of foreign subsidiaries
Full16$MultiSub2016<-Full16$ForSub2016 / Full16$NoSub2016

#Variable of number of countries
Full16$Countries1<-ifelse(Full16$Country1!=Full16$Country2 & Full16$Country2!=lag(Full16$Country2),1,0)
Full16$Countries1<-as.numeric(Full16$Countries1)
Full16$Countries2016<- with(Full16, ave(Full16$Countries1,Full16$Company, FUN=sum))

#Average number of subsidiaries per host country
Full16$AvSub4<-case_when(Full16$Country1!=Full16$Country2 & Full16$Country2!=lag(Full16$Country2)~seq(1,1907127, by=1),  # Increase from 2654903
                         Full16$Country1==Full16$Country2~0)
Full16$AvSub4<-na.locf(Full16$AvSub4)
Full16$AvSub3<- with(Full16, ave(Full16$ForeignSub1,Full16$AvSub4, FUN=sum))
Full16$AvSub2<- ifelse(Full16$Country1!=Full16$Country2 & Full16$Country2!=lag(Full16$Country2),Full16$AvSub3,0)
Full16$AvSub1<-with(Full16, ave(Full16$AvSub2,Full16$Company, FUN=sum))
Full16$AvSub2016<-Full16$AvSub1/Full16$Countries2016


##Subsidiary data 2017
#Create variable for total number of subsidiaries
Full17$NoSub2<-ifelse(Full17$Company!=Full17$Subsidiaries,1,0)
Full17$NoSub2017<-with (Full17,ave(Full17$NoSub2,Full17$Company, FUN=sum))
Full17[,3]<-NULL

#Create variable for country of Subsidiaries
Full17$Country1<-as.character(Full17$Company)
Full17$Country2<-as.character(Full17$Subsidiaries)
Full17$Country1<-substr(Full17$Company, 1, 2)
Full17$Country2<-substr(Full17$Subsidiaries,1,2)

#Create a variable of foreign subsidiaries
Full17$ForeignSub1<-ifelse(Full17$Country2!=Full17$Country1,1,0)
Full17$ForeignSub1<-as.numeric(Full17$ForeignSub1)
Full17$ForSub2017<- with(Full17, ave(Full17$ForeignSub1,Full17$Company, FUN=sum))

#Share of foreign subsidiaries
Full17$MultiSub2017<-Full17$ForSub2017 / Full17$NoSub2017

#Variable of number of countries
Full17$Countries1<-ifelse(Full17$Country1!=Full17$Country2 & Full17$Country2!=lag(Full17$Country2),1,0)
Full17$Countries1<-as.numeric(Full17$Countries1)
Full17$Countries2017<- with(Full17, ave(Full17$Countries1,Full17$Company, FUN=sum))

#Average number of subsidiaries per host country
Full17$AvSub4<-case_when(Full17$Country1!=Full17$Country2 & Full17$Country2!=lag(Full17$Country2)~seq(1,1958278, by=1),  # Increase from 2654903
                         Full17$Country1==Full17$Country2~0)
Full17$AvSub4<-na.locf(Full17$AvSub4)
Full17$AvSub3<- with(Full17, ave(Full17$ForeignSub1,Full17$AvSub4, FUN=sum))
Full17$AvSub2<- ifelse(Full17$Country1!=Full17$Country2 & Full17$Country2!=lag(Full17$Country2),Full17$AvSub3,0)
Full17$AvSub1<-with(Full17, ave(Full17$AvSub2,Full17$Company, FUN=sum))
Full17$AvSub2017<-Full17$AvSub1/Full17$Countries2017


##Subsidiary data 2018
#Create variable for total number of subsidiaries
Full18$NoSub2<-ifelse(Full18$Company!=Full18$Subsidiaries,1,0)
Full18$NoSub2018<-with (Full18,ave(Full18$NoSub2,Full18$Company, FUN=sum))
Full18[,3]<-NULL

#Create variable for country of Subsidiaries
Full18$Country1<-as.character(Full18$Company)
Full18$Country2<-as.character(Full18$Subsidiaries)
Full18$Country1<-substr(Full18$Company, 1, 2)
Full18$Country2<-substr(Full18$Subsidiaries,1,2)

#Create a variable of foreign subsidiaries
Full18$ForeignSub1<-ifelse(Full18$Country2!=Full18$Country1,1,0)
Full18$ForeignSub1<-as.numeric(Full18$ForeignSub1)
Full18$ForSub2018<- with(Full18, ave(Full18$ForeignSub1,Full18$Company, FUN=sum))

#Share of foreign subsidiaries
Full18$MultiSub2018<-Full18$ForSub2018 / Full18$NoSub2018

#Variable of number of countries
Full18$Countries1<-ifelse(Full18$Country1!=Full18$Country2 & Full18$Country2!=lag(Full18$Country2),1,0)
Full18$Countries1<-as.numeric(Full18$Countries1)
Full18$Countries2018<- with(Full18, ave(Full18$Countries1,Full18$Company, FUN=sum))

#Average number of subsidiaries per host country
Full18$AvSub4<-case_when(Full18$Country1!=Full18$Country2 & Full18$Country2!=lag(Full18$Country2)~seq(1,2006336, by=1),  # Increase from 2654903
                         Full18$Country1==Full18$Country2~0)
Full18$AvSub4<-na.locf(Full18$AvSub4)
Full18$AvSub3<- with(Full18, ave(Full18$ForeignSub1,Full18$AvSub4, FUN=sum))
Full18$AvSub2<- ifelse(Full18$Country1!=Full18$Country2 & Full18$Country2!=lag(Full18$Country2),Full18$AvSub3,0)
Full18$AvSub1<-with(Full18, ave(Full18$AvSub2,Full18$Company, FUN=sum))
Full18$AvSub2018<-Full18$AvSub1/Full18$Countries2018


##Subsidiary data 2019
#Create variable for total number of subsidiaries
#Full19<-fread("Output_MNE_Code/MNE19.csv",sep=";") #used to try to fix the error from the lines below
Full19$NoSub2<-ifelse(Full19$Company!=Full19$Subsidiaries,1,0)
Full19$NoSub2019<-with (Full19,ave(Full19$NoSub2,Full19$Company, FUN=sum))
Full19[,3]<-NULL

#Create variable for country of Subsidiaries
Full19$Country1<-as.character(Full19$Company)
Full19$Country2<-as.character(Full19$Subsidiaries)
Full19$Country1<-substr(Full19$Company, 1, 2)
Full19$Country2<-substr(Full19$Subsidiaries,1,2)

#Create a variable of foreign subsidiaries
Full19$ForeignSub1<-ifelse(Full19$Country2!=Full19$Country1,1,0)
Full19$ForeignSub1<-as.numeric(Full19$ForeignSub1)
Full19$ForSub2019<- with(Full19, ave(Full19$ForeignSub1,Full19$Company, FUN=sum))

#Share of foreign subsidiaries
Full19$MultiSub2019<-Full19$ForSub2019 / Full19$NoSub2019

#Variable of number of countries
Full19$Countries1<-ifelse(Full19$Country1!=Full19$Country2 & Full19$Country2!=lag(Full19$Country2),1,0)
Full19$Countries1<-as.numeric(Full19$Countries1)
Full19$Countries2019<- with(Full19, ave(Full19$Countries1,Full19$Company, FUN=sum))

#Average number of subsidiaries per host country
Full19$AvSub4<-case_when(Full19$Country1!=Full19$Country2 & Full19$Country2!=lag(Full19$Country2)~seq(1,2055048, by=1),  # Increase from 2654903
                         Full19$Country1==Full19$Country2~0) 
#error----
#error message: 'seq(1, 2044415, by = 1)` must be length 2055048 or one, not 2044415.'

#if I read the Full19 file from scratch on line 6004, it suddenly works without error. This proves that the file Full9
#generated here is not exactly the same as the one used by felix to create his Full.csv file.
#I tried to fix it, replacing the wrong value of 2044415 by 2055048, but the related file (Full9) will be slightly different

Full19$AvSub4<-na.locf(Full19$AvSub4)
Full19$AvSub3<- with(Full19, ave(Full19$ForeignSub1,Full19$AvSub4, FUN=sum))
Full19$AvSub2<- ifelse(Full19$Country1!=Full19$Country2 & Full19$Country2!=lag(Full19$Country2),Full19$AvSub3,0)
Full19$AvSub1<-with(Full19, ave(Full19$AvSub2,Full19$Company, FUN=sum))
Full19$AvSub2019<-Full19$AvSub1/Full19$Countries2019

#Reduce data by the number of unnecessary variables
Full11 <- Full11[,c(-5,-9,-12,-14,-15,-16,-17)]
Full12 <- Full12[,c(-5,-9,-12,-14,-15,-16,-17)]
Full13 <- Full13[,c(-5,-9,-12,-14,-15,-16,-17)]
Full14 <- Full14[,c(-5,-9,-12,-14,-15,-16,-17)]
Full15 <- Full15[,c(-5,-9,-12,-14,-15,-16,-17)]
Full16 <- Full16[,c(-5,-9,-12,-14,-15,-16,-17)]
Full17 <- Full17[,c(-5,-9,-12,-14,-15,-16,-17)]
Full18 <- Full18[,c(-5,-9,-12,-14,-15,-16,-17)]
Full19 <- Full19[,c(-5,-9,-12,-14,-15,-16,-17)]

#Rename the datasets to have the same variable names
names(Full11)<-c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries","AvSub")
names(Full12)<-c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries","AvSub")
names(Full13)<-c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries","AvSub")
names(Full14)<-c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries","AvSub")
names(Full15)<-c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries","AvSub")
names(Full16)<-c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries","AvSub")
names(Full17)<-c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries","AvSub")
names(Full18)<-c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries","AvSub")
names(Full19)<-c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries","AvSub")




# 3. Create more variables based on country data ####

#Load a country dataset
Country<-read_excel("Dataset/CountryPanel.xlsx",sheet = "Sheet1")

##Prepare dataset
#Rename data
names(Country) <- c("Country2","IPR11","IPR12","IPR13","IPR14","IPR15","IPR16","IPR17","IPR18","IPR19","TP11","TP12","TP13",
                    "TP14","TP15","TP16","TP17","TP18","TP19","GDP11","GDP12","GDP13","GDP14","GDP15","GDP16",
                    "GDP17","GDP18","GDP19","GDPpc11","GDPpc12","GDPpc13","GDPpc14","GDPpc15","GDPpc16",
                    "GDPpc17","GDPpc18","GDPpc19","RD11","RD12","RD13","RD14","RD15","RD16","RD17","RD18","RD19",
                    "LR11","LR12","LR13","LR14","LR15","LR16","LR17","LR18","LR19","TR11","TR12","TR13","TR14","TR15","TR16",
                    "TR17","TR18","TR19","Inno11","Inno12","Inno13","Inno14","Inno15","Inno16","Inno17","Inno18","Inno19")

sapply(Country, class)

#Indicate missing values
Country<-na_if(Country,"NA")

#Indicate numeric and character variables
cols.num <- c("IPR11","IPR12","IPR13","IPR14","IPR15","IPR16","IPR17","IPR18","IPR19","TP11","TP12","TP13",
              "TP14","TP15","TP16","TP17","TP18","TP19","GDP11","GDP12","GDP13","GDP14","GDP15","GDP16",
              "GDP17","GDP18","GDP19","GDPpc11","GDPpc12","GDPpc13","GDPpc14","GDPpc15","GDPpc16",
              "GDPpc17","GDPpc18","GDPpc19","RD11","RD12","RD13","RD14","RD15","RD16","RD17","RD18","RD19",
              "LR11","LR12","LR13","LR14","LR15","LR16","LR17","LR18","LR19","TR11","TR12","TR13","TR14","TR15","TR16",
              "TR17","TR18","TR19","Inno11","Inno12","Inno13","Inno14","Inno15","Inno16","Inno17","Inno18","Inno19")
Country[cols.num] <- sapply(Country[cols.num],as.numeric)
sapply(Country, class)

#Extract the yearly data from the wide-format country data
Country2011<-Country[,c(1,2,11,20,29,38,47,56,65)]
Country2012<-Country[,c(1,3,12,21,30,39,48,57,66)]
Country2013<-Country[,c(1,4,13,22,31,40,49,58,67)]
Country2014<-Country[,c(1,5,14,23,32,41,50,59,68)]
Country2015<-Country[,c(1,6,15,24,33,42,51,60,69)]
Country2016<-Country[,c(1,7,16,25,34,43,52,61,70)]
Country2017<-Country[,c(1,8,17,26,35,44,53,62,71)]
Country2018<-Country[,c(1,9,18,27,36,45,54,63,72)]
Country2019<-Country[,c(1,10,19,28,37,46,55,64,73)]


## Match datasets for 2011
Full11$Country2<-as.character(Full11$Country2)

Full1<-left_join(Full11,Country2011,by="Country2",na_matches="never")

names(Full1) <- c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries",
                  "AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR","Inno")


##Create more variables
#Differences in intellectual property rights between home and (average) host (based on EFI)
Full1$IPR0000<-case_when(Full1$Country1!=Full1$Country2~Full1$IPR,
                         Full1$Country1==Full1$Country2~0)
Full1$IPR000<- ifelse(Full1$Country1!=Full1$Country2 & Full1$Country2!=lag(Full1$Country2),Full1$IPR0000,0)
Full1$IPR000[is.na(Full1$IPR000)] <- 0
Full1$IPR00<-with(Full1,ave(Full1$IPR000,Full1$Company, FUN=sum))
Full1$IPR0<-Full1$IPR00/Full1$Countries
Full1$IPRDist<-Full1$IPR-Full1$IPR0
Full1$IPRDist<-abs(Full1$IPRDist)

#Sum of IPR differences (Song and Shin 2008)
Full1$IPR1<-vlookup_df(Full1$Country1,Full1,result_column=10,lookup_column=5)

#warning1----
#The line above gives an error: 'Error in `[.data.table`(subset_dataframe(dict, ind, drop = FALSE), , result_column,  : 
#j (the 2nd argument inside [...]) is a single symbol but column name 'result_column' is not found. Perhaps you intended DT[, ..result_column]. This difference to data.frame is deliberate and explained in FAQ 1.1.'

#The same error happens for all Full files (e.g., FUll2, Full3, ... Full9).Therefore, the variables related 
#to IPR (including IPR1, IPR2, IPR3, IPRDist2) should be carefully considered.

Full1$IPR1<-as.numeric(unlist((Full1$IPR1)))
Full1$IPR2<-Full1$IPR1-Full1$IPR
Full1$IPR3<-ifelse(Full1$Country1==Full1$Country2,0,Full1$IPR2)
Full1$IPR3[is.na(Full1$IPR3)] <- 0
Full1$IPR3<-abs(Full1$IPR3)
Full1$IPRDist2<-with(Full1,ave(Full1$IPR3,Full1$Company, FUN=sum))

#Number of low IPR countries
Full1$IPRCountries1<-ifelse(Full1$Country1!=Full1$Country2 & Full1$Country2!=lag(Full1$Country2) & Full1$IPR<75,1,0)
Full1$IPRCountries1<-as.numeric(Full1$IPRCountries1)
Full1$IPRCountries1[is.na(Full1$IPRCountries1)] <- 0
Full1$IPRCountries<- with(Full1, ave(Full1$IPRCountries1,Full1$Company, FUN=sum))

#Number of subsidiaries in low IPR locations
Full1$IPRSub1<-ifelse(Full1$Country1!=Full1$Country2 & Full1$IPR<75,1,0)
Full1$IPRSub1<-as.numeric(Full1$IPRSub1)
Full1$IPRSub<- with(Full1, ave(Full1$IPRSub1,Full1$Company, FUN=sum))

##Average patent applications per host country
Full1$TP000<-case_when(Full1$Country1!=Full1$Country2~Full1$TP,
                       Full1$Country1==Full1$Country2~0)
Full1$TP00<- ifelse(Full1$Country1!=Full1$Country2 & Full1$Country2!=lag(Full1$Country2),Full1$TP000,0)
Full1$TP00[is.na(Full1$TP00)] <- 0
Full1$TP00<-as.numeric(Full1$TP00)
Full1$TP0<-with(Full1, ave(Full1$TP00,Full1$Company, FUN=sum))
Full1$CPat<-Full1$TP0/Full1$Countries
Full1$CPatRat<-Full1$CPat/Full1$TP         #Ratio of home and host country patents

#Total GDP of host countries
Full1$CGDP00<-case_when(Full1$Country1!=Full1$Country2~Full1$GDP,
                        Full1$Country1==Full1$Country2~0)
Full1$CGDP0<- ifelse(Full1$Country1!=Full1$Country2 & Full1$Country2!=lag(Full1$Country2),Full1$CGDP00,0)
Full1$CGDP0[is.na(Full1$CGDP0)] <- 0
Full1$CGDP0<-as.numeric(Full1$CGDP0)
Full1$CGDP<-with(Full1, ave(Full1$CGDP0,Full1$Company, FUN=sum))

#Total GDP served
Full1$TotalGDP<-Full1$GDP+Full1$CGDP

#Average GDP per capita per host country
Full1$CGDPpc000<-case_when(Full1$Country1!=Full1$Country2~Full1$GDPpc,
                           Full1$Country1==Full1$Country2~0)
Full1$CGDPpc00<- ifelse(Full1$Country1!=Full1$Country2 & Full1$Country2!=lag(Full1$Country2),Full1$CGDPpc000,0)
Full1$CGDPpc00[is.na(Full1$CGDPpc00)] <- 0
Full1$CGDPpc00<-as.numeric(Full1$CGDPpc00)
Full1$CGDPpc0<-with(Full1, ave(Full1$CGDPpc00,Full1$Company, FUN=function(x) mean(x, na.rm=T)))
Full1$CGDPpc<-Full1$CGDPpc0/Full1$Countries

#Average share of high-tech exports per host country
Full1$CRD000<-case_when(Full1$Country1!=Full1$Country2~Full1$RD,
                        Full1$Country1==Full1$Country2~0)
Full1$CRD00<- ifelse(Full1$Country1!=Full1$Country2 & Full1$Country2!=lag(Full1$Country2),Full1$CRD000,0)
Full1$CRD00[is.na(Full1$CRD00)] <- 0
Full1$CRD00<-as.numeric(Full1$CRD00)
Full1$CRD0<-with(Full1, ave(Full1$CRD00,Full1$Company, FUN=sum))
Full1$CRD<-Full1$CRD0/Full1$Countries

#Differences in legal rights between home and (average) host
Full1$LR0000<-case_when(Full1$Country1!=Full1$Country2~Full1$LR,
                        Full1$Country1==Full1$Country2~0)
Full1$LR000<- ifelse(Full1$Country1!=Full1$Country2 & Full1$Country2!=lag(Full1$Country2),Full1$LR0000,0)
Full1$LR000[is.na(Full1$LR000)] <- 0
Full1$LR00<-with(Full1,ave(Full1$LR000,Full1$Company, FUN=sum))
Full1$LR0<-Full1$LR00/Full1$Countries
Full1$LRDist<-Full1$LR-Full1$LR0

#Technological Distance
Full1$TR4<-case_when(Full1$Country1!=Full1$Country2~Full1$TR,
                     Full1$Country1==Full1$Country2~0)
Full1$TR3<- ifelse(Full1$Country1!=Full1$Country2 & Full1$Country2!=lag(Full1$Country2),Full1$TR4,0)
Full1$TR3[is.na(Full1$TR3)] <- 0
Full1$TR2<-with(Full1,ave(Full1$TR3,Full1$Company, FUN=sum))
Full1$TR1<-Full1$TR2/Full1$Countries
Full1$TRDist<-Full1$TR-Full1$TR1

##Differences in Innovativeness between home and (average) hostcountry (based on GCI)
Full1$Inno0000<-case_when(Full1$Country1!=Full1$Country2~Full1$Inno,
                          Full1$Country1==Full1$Country2~0)
Full1$Inno000<- ifelse(Full1$Country1!=Full1$Country2 & Full1$Country2!=lag(Full1$Country2),Full1$Inno0000,0)
Full1$Inno000[is.na(Full1$Inno000)] <- 0
Full1$Inno00<-with(Full1,ave(Full1$Inno000,Full1$Company, FUN=sum))
Full1$Inno0<-Full1$Inno00/Full1$Countries
Full1$InnoDist<-Full1$Inno-Full1$Inno0

#Sum of Innovation differences (Song and Shin 2008)
Full1$Inno1<-vlookup_df(Full1$Country1,Full1,result_column=10,lookup_column=5)

#warning2----
#The line above gives an error: 'Error in `[.data.table`(subset_dataframe(dict, ind, drop = FALSE), , result_column,  : 
#j (the 2nd argument inside [...]) is a single symbol but column name 'result_column' is not found. Perhaps you intended DT[, ..result_column]. This difference to data.frame is deliberate and explained in FAQ 1.1.'

#The same error happens for all Full files (e.g., FUll2, Full3, ... Full9).Therefore, the variables related to 
#Inno1 (i.e., Inno1, Inno2, Inno3, and InnoDist2) should be carefully considered.

Full1$Inno1<-as.numeric(unlist((Full1$Inno1)))
Full1$Inno2<-Full1$Inno1-Full1$Inno
Full1$Inno3<-ifelse(Full1$Country1==Full1$Country2,0,Full1$Inno2)
Full1$Inno3[is.na(Full1$Inno3)] <- 0
Full1$Inno3<-abs(Full1$Inno3)
Full1$InnoDist2<-with(Full1,ave(Full1$Inno3,Full1$Company, FUN=sum))

#Number of high-tech countries
Full1$InnoCountries1<-ifelse(Full1$Country1!=Full1$Country2 & Full1$Country2!=lag(Full1$Country2) & Full1$Inno>5,1,0)
Full1$InnoCountries1<-as.numeric(Full1$InnoCountries1)
Full1$InnoCountries<- with(Full1, ave(Full1$InnoCountries1,Full1$Company, FUN=sum))

#Number of subsidiaries in high-tech locations
Full1$InnoSub1<-ifelse(Full1$Country1!=Full1$Country2 & Full1$Inno>5,1,0)
Full1$InnoSub1<-as.numeric(Full1$InnoSub1)
Full1$InnoSub<- with(Full1, ave(Full1$InnoSub1,Full1$Company, FUN=sum))

#Remove unnecessary columns
Full1 <- Full1[,c(-6,-7,-20,-21,-22,-23,-25,-26,-27,-29,-31,-33,-34,-35,-38,-39,-42,-43,-44,-46,-47,-48,-50,-51,-52,-53,-55,-56,-57,-58,
                  -60,-61,-62,-63,-65,-66,-67,-69,-71)]


##Match datasets for 2012
Full12$Country2<-as.character(Full12$Country2)

Full2<-left_join(Full12,Country2012,by="Country2",na_matches="never")

names(Full2) <- c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries",
                  "AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR","Inno")


##Create more variables
#Differences in intellectual property rights between home and (average) host (based on EFI)
Full2$IPR0000<-case_when(Full2$Country1!=Full2$Country2~Full2$IPR,
                         Full2$Country1==Full2$Country2~0)
Full2$IPR000<- ifelse(Full2$Country1!=Full2$Country2 & Full2$Country2!=lag(Full2$Country2),Full2$IPR0000,0)
Full2$IPR000[is.na(Full2$IPR000)] <- 0
Full2$IPR00<-with(Full2,ave(Full2$IPR000,Full2$Company, FUN=sum))
Full2$IPR0<-Full2$IPR00/Full2$Countries
Full2$IPRDist<-Full2$IPR-Full2$IPR0

#Sum of IPR differences (Song and Shin 2008)
Full2$IPR1<-vlookup_df(Full2$Country1,Full2,result_column=10,lookup_column=5)
Full2$IPR1<-as.numeric(unlist((Full2$IPR1)))
Full2$IPR2<-Full2$IPR1-Full2$IPR
Full2$IPR3<-ifelse(Full2$Country1==Full2$Country2,0,Full2$IPR2)
Full2$IPR3[is.na(Full2$IPR3)] <- 0
Full2$IPR3<-abs(Full2$IPR3)
Full2$IPRDist2<-with(Full2,ave(Full2$IPR3,Full2$Company, FUN=sum))

#Number of low IPR countries
Full2$IPRCountries1<-ifelse(Full2$Country1!=Full2$Country2 & Full2$Country2!=lag(Full2$Country2) & Full2$IPR<75,1,0)
Full2$IPRCountries1<-as.numeric(Full2$IPRCountries1)
Full2$IPRCountries1[is.na(Full2$IPRCountries1)] <- 0
Full2$IPRCountries<- with(Full2, ave(Full2$IPRCountries1,Full2$Company, FUN=sum))

#Number of subsidiaries in low IPR locations
Full2$IPRSub1<-ifelse(Full2$Country1!=Full2$Country2 & Full2$IPR<75,1,0)
Full2$IPRSub1<-as.numeric(Full2$IPRSub1)
Full2$IPRSub<- with(Full2, ave(Full2$IPRSub1,Full2$Company, FUN=sum))

##Average patent applications per host country
Full2$TP000<-case_when(Full2$Country1!=Full2$Country2~Full2$TP,
                       Full2$Country1==Full2$Country2~0)
Full2$TP00<- ifelse(Full2$Country1!=Full2$Country2 & Full2$Country2!=lag(Full2$Country2),Full2$TP000,0)
Full2$TP00[is.na(Full2$TP00)] <- 0
Full2$TP00<-as.numeric(Full2$TP00)
Full2$TP0<-with(Full2, ave(Full2$TP00,Full2$Company, FUN=sum))
Full2$CPat<-Full2$TP0/Full2$Countries
Full2$CPatRat<-Full2$CPat/Full2$TP         #Ratio of home and host country patents

#Total GDP of host countries
Full2$CGDP00<-case_when(Full2$Country1!=Full2$Country2~Full2$GDP,
                        Full2$Country1==Full2$Country2~0)
Full2$CGDP0<- ifelse(Full2$Country1!=Full2$Country2 & Full2$Country2!=lag(Full2$Country2),Full2$CGDP00,0)
Full2$CGDP0[is.na(Full2$CGDP0)] <- 0
Full2$CGDP0<-as.numeric(Full2$CGDP0)
Full2$CGDP<-with(Full2, ave(Full2$CGDP0,Full2$Company, FUN=sum))

#Total GDP served
Full2$TotalGDP<-Full2$GDP+Full2$CGDP

#Average GDP per capita per host country
Full2$CGDPpc000<-case_when(Full2$Country1!=Full2$Country2~Full2$GDPpc,
                           Full2$Country1==Full2$Country2~0)
Full2$CGDPpc00<- ifelse(Full2$Country1!=Full2$Country2 & Full2$Country2!=lag(Full2$Country2),Full2$CGDPpc000,0)
Full2$CGDPpc00[is.na(Full2$CGDPpc00)] <- 0
Full2$CGDPpc00<-as.numeric(Full2$CGDPpc00)
Full2$CGDPpc0<-with(Full2, ave(Full2$CGDPpc00,Full2$Company, FUN=function(x) mean(x, na.rm=T)))
Full2$CGDPpc<-Full2$CGDPpc0/Full2$Countries

#Average share of high-tech exports per host country
Full2$CRD000<-case_when(Full2$Country1!=Full2$Country2~Full2$RD,
                        Full2$Country1==Full2$Country2~0)
Full2$CRD00<- ifelse(Full2$Country1!=Full2$Country2 & Full2$Country2!=lag(Full2$Country2),Full2$CRD000,0)
Full2$CRD00[is.na(Full2$CRD00)] <- 0
Full2$CRD00<-as.numeric(Full2$CRD00)
Full2$CRD0<-with(Full2, ave(Full2$CRD00,Full2$Company, FUN=sum))
Full2$CRD<-Full2$CRD0/Full2$Countries

#Differences in legal rights between home and (average) host
Full2$LR0000<-case_when(Full2$Country1!=Full2$Country2~Full2$LR,
                        Full2$Country1==Full2$Country2~0)
Full2$LR000<- ifelse(Full2$Country1!=Full2$Country2 & Full2$Country2!=lag(Full2$Country2),Full2$LR0000,0)
Full2$LR000[is.na(Full2$LR000)] <- 0
Full2$LR00<-with(Full2,ave(Full2$LR000,Full2$Company, FUN=sum))
Full2$LR0<-Full2$LR00/Full2$Countries
Full2$LRDist<-Full2$LR-Full2$LR0

#Technological Distance
Full2$TR4<-case_when(Full2$Country1!=Full2$Country2~Full2$TR,
                     Full2$Country1==Full2$Country2~0)
Full2$TR3<- ifelse(Full2$Country1!=Full2$Country2 & Full2$Country2!=lag(Full2$Country2),Full2$TR4,0)
Full2$TR3[is.na(Full2$TR3)] <- 0
Full2$TR2<-with(Full2,ave(Full2$TR3,Full2$Company, FUN=sum))
Full2$TR1<-Full2$TR2/Full2$Countries
Full2$TRDist<-Full2$TR-Full2$TR1

##Differences in Innovativeness between home and (average) hostcountry (based on GCI)
Full2$Inno0000<-case_when(Full2$Country1!=Full2$Country2~Full2$Inno,
                          Full2$Country1==Full2$Country2~0)
Full2$Inno000<- ifelse(Full2$Country1!=Full2$Country2 & Full2$Country2!=lag(Full2$Country2),Full2$Inno0000,0)
Full2$Inno000[is.na(Full2$Inno000)] <- 0
Full2$Inno00<-with(Full2,ave(Full2$Inno000,Full2$Company, FUN=sum))
Full2$Inno0<-Full2$Inno00/Full2$Countries
Full2$InnoDist<-Full2$Inno-Full2$Inno0

#Sum of Innovation differences (Song and Shin 2008)
Full2$Inno1<-vlookup_df(Full2$Country1,Full2,result_column=10,lookup_column=5)
Full2$Inno1<-as.numeric(unlist((Full2$Inno1)))
Full2$Inno2<-Full2$Inno1-Full2$Inno
Full2$Inno3<-ifelse(Full2$Country1==Full2$Country2,0,Full2$Inno2)
Full2$Inno3[is.na(Full2$Inno3)] <- 0
Full2$Inno3<-abs(Full2$Inno3)
Full2$InnoDist2<-with(Full2,ave(Full2$Inno3,Full2$Company, FUN=sum))

#Number of high-tech countries
Full2$InnoCountries1<-ifelse(Full2$Country1!=Full2$Country2 & Full2$Country2!=lag(Full2$Country2) & Full2$Inno>5,1,0)
Full2$InnoCountries1<-as.numeric(Full2$InnoCountries1)
Full2$InnoCountries<- with(Full2, ave(Full2$InnoCountries1,Full2$Company, FUN=sum))

#Number of subsidiaries in high-tech locations
Full2$InnoSub1<-ifelse(Full2$Country1!=Full2$Country2 & Full2$Inno>5,1,0)
Full2$InnoSub1<-as.numeric(Full2$InnoSub1)
Full2$InnoSub<- with(Full2, ave(Full2$InnoSub1,Full2$Company, FUN=sum))

#Remove unnecessary columns
Full2 <- Full2[,c(-6,-7,-20,-21,-22,-23,-25,-26,-27,-29,-31,-33,-34,-35,-38,-39,-42,-43,-44,-46,-47,-48,-50,-51,-52,-53,-55,-56,-57,-58,
                  -60,-61,-62,-63,-65,-66,-67,-69,-71)]


##Match datasets for 2013
Full13$Country2<-as.character(Full13$Country2)

Full3<-left_join(Full13,Country2013,by="Country2",na_matches="never")

names(Full3) <- c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries",
                  "AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR","Inno")


##Create more variables
#Differences in intellectual property rights between home and (average) host (based on EFI)
Full3$IPR0000<-case_when(Full3$Country1!=Full3$Country2~Full3$IPR,
                         Full3$Country1==Full3$Country2~0)
Full3$IPR000<- ifelse(Full3$Country1!=Full3$Country2 & Full3$Country2!=lag(Full3$Country2),Full3$IPR0000,0)
Full3$IPR000[is.na(Full3$IPR000)] <- 0
Full3$IPR00<-with(Full3,ave(Full3$IPR000,Full3$Company, FUN=sum))
Full3$IPR0<-Full3$IPR00/Full3$Countries
Full3$IPRDist<-Full3$IPR-Full3$IPR0

#Sum of IPR differences (Song and Shin 2008)
Full3$IPR1<-vlookup_df(Full3$Country1,Full3,result_column=10,lookup_column=5)
Full3$IPR1<-as.numeric(unlist((Full3$IPR1)))
Full3$IPR2<-Full3$IPR1-Full3$IPR
Full3$IPR3<-ifelse(Full3$Country1==Full3$Country2,0,Full3$IPR2)
Full3$IPR3[is.na(Full3$IPR3)] <- 0
Full3$IPR3<-abs(Full3$IPR3)
Full3$IPRDist2<-with(Full3,ave(Full3$IPR3,Full3$Company, FUN=sum))

#Number of low IPR countries
Full3$IPRCountries1<-ifelse(Full3$Country1!=Full3$Country2 & Full3$Country2!=lag(Full3$Country2) & Full3$IPR<75,1,0)
Full3$IPRCountries1<-as.numeric(Full3$IPRCountries1)
Full3$IPRCountries1[is.na(Full3$IPRCountries1)] <- 0
Full3$IPRCountries<- with(Full3, ave(Full3$IPRCountries1,Full3$Company, FUN=sum))

#Number of subsidiaries in low IPR locations
Full3$IPRSub1<-ifelse(Full3$Country1!=Full3$Country2 & Full3$IPR<75,1,0)
Full3$IPRSub1<-as.numeric(Full3$IPRSub1)
Full3$IPRSub<- with(Full3, ave(Full3$IPRSub1,Full3$Company, FUN=sum))

##Average patent applications per host country
Full3$TP000<-case_when(Full3$Country1!=Full3$Country2~Full3$TP,
                       Full3$Country1==Full3$Country2~0)
Full3$TP00<- ifelse(Full3$Country1!=Full3$Country2 & Full3$Country2!=lag(Full3$Country2),Full3$TP000,0)
Full3$TP00[is.na(Full3$TP00)] <- 0
Full3$TP00<-as.numeric(Full3$TP00)
Full3$TP0<-with(Full3, ave(Full3$TP00,Full3$Company, FUN=sum))
Full3$CPat<-Full3$TP0/Full3$Countries
Full3$CPatRat<-Full3$CPat/Full3$TP         #Ratio of home and host country patents

#Total GDP of host countries
Full3$CGDP00<-case_when(Full3$Country1!=Full3$Country2~Full3$GDP,
                        Full3$Country1==Full3$Country2~0)
Full3$CGDP0<- ifelse(Full3$Country1!=Full3$Country2 & Full3$Country2!=lag(Full3$Country2),Full3$CGDP00,0)
Full3$CGDP0[is.na(Full3$CGDP0)] <- 0
Full3$CGDP0<-as.numeric(Full3$CGDP0)
Full3$CGDP<-with(Full3, ave(Full3$CGDP0,Full3$Company, FUN=sum))

#Total GDP served
Full3$TotalGDP<-Full3$GDP+Full3$CGDP

#Average GDP per capita per host country
Full3$CGDPpc000<-case_when(Full3$Country1!=Full3$Country2~Full3$GDPpc,
                           Full3$Country1==Full3$Country2~0)
Full3$CGDPpc00<- ifelse(Full3$Country1!=Full3$Country2 & Full3$Country2!=lag(Full3$Country2),Full3$CGDPpc000,0)
Full3$CGDPpc00[is.na(Full3$CGDPpc00)] <- 0
Full3$CGDPpc00<-as.numeric(Full3$CGDPpc00)
Full3$CGDPpc0<-with(Full3, ave(Full3$CGDPpc00,Full3$Company, FUN=function(x) mean(x, na.rm=T)))
Full3$CGDPpc<-Full3$CGDPpc0/Full3$Countries

#Average share of high-tech exports per host country
Full3$CRD000<-case_when(Full3$Country1!=Full3$Country2~Full3$RD,
                        Full3$Country1==Full3$Country2~0)
Full3$CRD00<- ifelse(Full3$Country1!=Full3$Country2 & Full3$Country2!=lag(Full3$Country2),Full3$CRD000,0)
Full3$CRD00[is.na(Full3$CRD00)] <- 0
Full3$CRD00<-as.numeric(Full3$CRD00)
Full3$CRD0<-with(Full3, ave(Full3$CRD00,Full3$Company, FUN=sum))
Full3$CRD<-Full3$CRD0/Full3$Countries

#Differences in legal rights between home and (average) host
Full3$LR0000<-case_when(Full3$Country1!=Full3$Country2~Full3$LR,
                        Full3$Country1==Full3$Country2~0)
Full3$LR000<- ifelse(Full3$Country1!=Full3$Country2 & Full3$Country2!=lag(Full3$Country2),Full3$LR0000,0)
Full3$LR000[is.na(Full3$LR000)] <- 0
Full3$LR00<-with(Full3,ave(Full3$LR000,Full3$Company, FUN=sum))
Full3$LR0<-Full3$LR00/Full3$Countries
Full3$LRDist<-Full3$LR-Full3$LR0

#Technological Distance
Full3$TR4<-case_when(Full3$Country1!=Full3$Country2~Full3$TR,
                     Full3$Country1==Full3$Country2~0)
Full3$TR3<- ifelse(Full3$Country1!=Full3$Country2 & Full3$Country2!=lag(Full3$Country2),Full3$TR4,0)
Full3$TR3[is.na(Full3$TR3)] <- 0
Full3$TR2<-with(Full3,ave(Full3$TR3,Full3$Company, FUN=sum))
Full3$TR1<-Full3$TR2/Full3$Countries
Full3$TRDist<-Full3$TR-Full3$TR1

##Differences in Innovativeness between home and (average) hostcountry (based on GCI)
Full3$Inno0000<-case_when(Full3$Country1!=Full3$Country2~Full3$Inno,
                          Full3$Country1==Full3$Country2~0)
Full3$Inno000<- ifelse(Full3$Country1!=Full3$Country2 & Full3$Country2!=lag(Full3$Country2),Full3$Inno0000,0)
Full3$Inno000[is.na(Full3$Inno000)] <- 0
Full3$Inno00<-with(Full3,ave(Full3$Inno000,Full3$Company, FUN=sum))
Full3$Inno0<-Full3$Inno00/Full3$Countries
Full3$InnoDist<-Full3$Inno-Full3$Inno0

#Sum of Innovation differences (Song and Shin 2008)
Full3$Inno1<-vlookup_df(Full3$Country1,Full3,result_column=10,lookup_column=5)
Full3$Inno1<-as.numeric(unlist((Full3$Inno1)))
Full3$Inno2<-Full3$Inno1-Full3$Inno
Full3$Inno3<-ifelse(Full3$Country1==Full3$Country2,0,Full3$Inno2)
Full3$Inno3[is.na(Full3$Inno3)] <- 0
Full3$Inno3<-abs(Full3$Inno3)
Full3$InnoDist2<-with(Full3,ave(Full3$Inno3,Full3$Company, FUN=sum))

#Number of high-tech countries
Full3$InnoCountries1<-ifelse(Full3$Country1!=Full3$Country2 & Full3$Country2!=lag(Full3$Country2) & Full3$Inno>5,1,0)
Full3$InnoCountries1<-as.numeric(Full3$InnoCountries1)
Full3$InnoCountries<- with(Full3, ave(Full3$InnoCountries1,Full3$Company, FUN=sum))

#Number of subsidiaries in high-tech locations
Full3$InnoSub1<-ifelse(Full3$Country1!=Full3$Country2 & Full3$Inno>5,1,0)
Full3$InnoSub1<-as.numeric(Full3$InnoSub1)
Full3$InnoSub<- with(Full3, ave(Full3$InnoSub1,Full3$Company, FUN=sum))

#Remove unnecessary columns
Full3 <- Full3[,c(-6,-7,-20,-21,-22,-23,-25,-26,-27,-29,-31,-33,-34,-35,-38,-39,-42,-43,-44,-46,-47,-48,-50,-51,-52,-53,-55,-56,-57,-58,
                  -60,-61,-62,-63,-65,-66,-67,-69,-71)]


##Match datasets for 2014
Full14$Country2<-as.character(Full14$Country2)

Full4<-left_join(Full14,Country2014,by="Country2",na_matches="never")

names(Full4) <- c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries",
                  "AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR","Inno")


##Create more variables
#Differences in intellectual property rights between home and (average) host (based on EFI)
Full4$IPR0000<-case_when(Full4$Country1!=Full4$Country2~Full4$IPR,
                         Full4$Country1==Full4$Country2~0)
Full4$IPR000<- ifelse(Full4$Country1!=Full4$Country2 & Full4$Country2!=lag(Full4$Country2),Full4$IPR0000,0)
Full4$IPR000[is.na(Full4$IPR000)] <- 0
Full4$IPR00<-with(Full4,ave(Full4$IPR000,Full4$Company, FUN=sum))
Full4$IPR0<-Full4$IPR00/Full4$Countries
Full4$IPRDist<-Full4$IPR-Full4$IPR0

#Sum of IPR differences (Song and Shin 2008)
Full4$IPR1<-vlookup_df(Full4$Country1,Full4,result_column=10,lookup_column=5)
Full4$IPR1<-as.numeric(unlist((Full4$IPR1)))
Full4$IPR2<-Full4$IPR1-Full4$IPR
Full4$IPR3<-ifelse(Full4$Country1==Full4$Country2,0,Full4$IPR2)
Full4$IPR3[is.na(Full4$IPR3)] <- 0
Full4$IPR3<-abs(Full4$IPR3)
Full4$IPRDist2<-with(Full4,ave(Full4$IPR3,Full4$Company, FUN=sum))

#Number of low IPR countries
Full4$IPRCountries1<-ifelse(Full4$Country1!=Full4$Country2 & Full4$Country2!=lag(Full4$Country2) & Full4$IPR<75,1,0)
Full4$IPRCountries1<-as.numeric(Full4$IPRCountries1)
Full4$IPRCountries1[is.na(Full4$IPRCountries1)] <- 0
Full4$IPRCountries<- with(Full4, ave(Full4$IPRCountries1,Full4$Company, FUN=sum))

#Number of subsidiaries in low IPR locations
Full4$IPRSub1<-ifelse(Full4$Country1!=Full4$Country2 & Full4$IPR<75,1,0)
Full4$IPRSub1<-as.numeric(Full4$IPRSub1)
Full4$IPRSub<- with(Full4, ave(Full4$IPRSub1,Full4$Company, FUN=sum))

##Average patent applications per host country
Full4$TP000<-case_when(Full4$Country1!=Full4$Country2~Full4$TP,
                       Full4$Country1==Full4$Country2~0)
Full4$TP00<- ifelse(Full4$Country1!=Full4$Country2 & Full4$Country2!=lag(Full4$Country2),Full4$TP000,0)
Full4$TP00[is.na(Full4$TP00)] <- 0
Full4$TP00<-as.numeric(Full4$TP00)
Full4$TP0<-with(Full4, ave(Full4$TP00,Full4$Company, FUN=sum))
Full4$CPat<-Full4$TP0/Full4$Countries
Full4$CPatRat<-Full4$CPat/Full4$TP         #Ratio of home and host country patents

#Total GDP of host countries
Full4$CGDP00<-case_when(Full4$Country1!=Full4$Country2~Full4$GDP,
                        Full4$Country1==Full4$Country2~0)
Full4$CGDP0<- ifelse(Full4$Country1!=Full4$Country2 & Full4$Country2!=lag(Full4$Country2),Full4$CGDP00,0)
Full4$CGDP0[is.na(Full4$CGDP0)] <- 0
Full4$CGDP0<-as.numeric(Full4$CGDP0)
Full4$CGDP<-with(Full4, ave(Full4$CGDP0,Full4$Company, FUN=sum))

#Total GDP served
Full4$TotalGDP<-Full4$GDP+Full4$CGDP

#Average GDP per capita per host country
Full4$CGDPpc000<-case_when(Full4$Country1!=Full4$Country2~Full4$GDPpc,
                           Full4$Country1==Full4$Country2~0)
Full4$CGDPpc00<- ifelse(Full4$Country1!=Full4$Country2 & Full4$Country2!=lag(Full4$Country2),Full4$CGDPpc000,0)
Full4$CGDPpc00[is.na(Full4$CGDPpc00)] <- 0
Full4$CGDPpc00<-as.numeric(Full4$CGDPpc00)
Full4$CGDPpc0<-with(Full4, ave(Full4$CGDPpc00,Full4$Company, FUN=function(x) mean(x, na.rm=T)))
Full4$CGDPpc<-Full4$CGDPpc0/Full4$Countries

#Average share of high-tech exports per host country
Full4$CRD000<-case_when(Full4$Country1!=Full4$Country2~Full4$RD,
                        Full4$Country1==Full4$Country2~0)
Full4$CRD00<- ifelse(Full4$Country1!=Full4$Country2 & Full4$Country2!=lag(Full4$Country2),Full4$CRD000,0)
Full4$CRD00[is.na(Full4$CRD00)] <- 0
Full4$CRD00<-as.numeric(Full4$CRD00)
Full4$CRD0<-with(Full4, ave(Full4$CRD00,Full4$Company, FUN=sum))
Full4$CRD<-Full4$CRD0/Full4$Countries

#Differences in legal rights between home and (average) host
Full4$LR0000<-case_when(Full4$Country1!=Full4$Country2~Full4$LR,
                        Full4$Country1==Full4$Country2~0)
Full4$LR000<- ifelse(Full4$Country1!=Full4$Country2 & Full4$Country2!=lag(Full4$Country2),Full4$LR0000,0)
Full4$LR000[is.na(Full4$LR000)] <- 0
Full4$LR00<-with(Full4,ave(Full4$LR000,Full4$Company, FUN=sum))
Full4$LR0<-Full4$LR00/Full4$Countries
Full4$LRDist<-Full4$LR-Full4$LR0

#Technological Distance
Full4$TR4<-case_when(Full4$Country1!=Full4$Country2~Full4$TR,
                     Full4$Country1==Full4$Country2~0)
Full4$TR3<- ifelse(Full4$Country1!=Full4$Country2 & Full4$Country2!=lag(Full4$Country2),Full4$TR4,0)
Full4$TR3[is.na(Full4$TR3)] <- 0
Full4$TR2<-with(Full4,ave(Full4$TR3,Full4$Company, FUN=sum))
Full4$TR1<-Full4$TR2/Full4$Countries
Full4$TRDist<-Full4$TR-Full4$TR1

##Differences in Innovativeness between home and (average) hostcountry (based on GCI)
Full4$Inno0000<-case_when(Full4$Country1!=Full4$Country2~Full4$Inno,
                          Full4$Country1==Full4$Country2~0)
Full4$Inno000<- ifelse(Full4$Country1!=Full4$Country2 & Full4$Country2!=lag(Full4$Country2),Full4$Inno0000,0)
Full4$Inno000[is.na(Full4$Inno000)] <- 0
Full4$Inno00<-with(Full4,ave(Full4$Inno000,Full4$Company, FUN=sum))
Full4$Inno0<-Full4$Inno00/Full4$Countries
Full4$InnoDist<-Full4$Inno-Full4$Inno0

#Sum of Innovation differences (Song and Shin 2008)
Full4$Inno1<-vlookup_df(Full4$Country1,Full4,result_column=10,lookup_column=5)
Full4$Inno1<-as.numeric(unlist((Full4$Inno1)))
Full4$Inno2<-Full4$Inno1-Full4$Inno
Full4$Inno3<-ifelse(Full4$Country1==Full4$Country2,0,Full4$Inno2)
Full4$Inno3[is.na(Full4$Inno3)] <- 0
Full4$Inno3<-abs(Full4$Inno3)
Full4$InnoDist2<-with(Full4,ave(Full4$Inno3,Full4$Company, FUN=sum))

#Number of high-tech countries
Full4$InnoCountries1<-ifelse(Full4$Country1!=Full4$Country2 & Full4$Country2!=lag(Full4$Country2) & Full4$Inno>5,1,0)
Full4$InnoCountries1<-as.numeric(Full4$InnoCountries1)
Full4$InnoCountries<- with(Full4, ave(Full4$InnoCountries1,Full4$Company, FUN=sum))

#Number of subsidiaries in high-tech locations
Full4$InnoSub1<-ifelse(Full4$Country1!=Full4$Country2 & Full4$Inno>5,1,0)
Full4$InnoSub1<-as.numeric(Full4$InnoSub1)
Full4$InnoSub<- with(Full4, ave(Full4$InnoSub1,Full4$Company, FUN=sum))

#Remove unnecessary columns
Full4 <- Full4[,c(-6,-7,-20,-21,-22,-23,-25,-26,-27,-29,-31,-33,-34,-35,-38,-39,-42,-43,-44,-46,-47,-48,-50,-51,-52,-53,-55,-56,-57,-58,
                  -60,-61,-62,-63,-65,-66,-67,-69,-71)]


##Match datasets for 2015
Full15$Country2<-as.character(Full15$Country2)

Full5<-left_join(Full15,Country2015,by="Country2",na_matches="never")

names(Full5) <- c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries",
                  "AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR","Inno")


##Create more variables
#Differences in intellectual property rights between home and (average) host (based on EFI)
Full5$IPR0000<-case_when(Full5$Country1!=Full5$Country2~Full5$IPR,
                         Full5$Country1==Full5$Country2~0)
Full5$IPR000<- ifelse(Full5$Country1!=Full5$Country2 & Full5$Country2!=lag(Full5$Country2),Full5$IPR0000,0)
Full5$IPR000[is.na(Full5$IPR000)] <- 0
Full5$IPR00<-with(Full5,ave(Full5$IPR000,Full5$Company, FUN=sum))
Full5$IPR0<-Full5$IPR00/Full5$Countries
Full5$IPRDist<-Full5$IPR-Full5$IPR0

#Sum of IPR differences (Song and Shin 2008)
Full5$IPR1<-vlookup_df(Full5$Country1,Full5,result_column=10,lookup_column=5)
Full5$IPR1<-as.numeric(unlist((Full5$IPR1)))
Full5$IPR2<-Full5$IPR1-Full5$IPR
Full5$IPR3<-ifelse(Full5$Country1==Full5$Country2,0,Full5$IPR2)
Full5$IPR3[is.na(Full5$IPR3)] <- 0
Full5$IPR3<-abs(Full5$IPR3)
Full5$IPRDist2<-with(Full5,ave(Full5$IPR3,Full5$Company, FUN=sum))

#Number of low IPR countries
Full5$IPRCountries1<-ifelse(Full5$Country1!=Full5$Country2 & Full5$Country2!=lag(Full5$Country2) & Full5$IPR<75,1,0)
Full5$IPRCountries1<-as.numeric(Full5$IPRCountries1)
Full5$IPRCountries1[is.na(Full5$IPRCountries1)] <- 0
Full5$IPRCountries<- with(Full5, ave(Full5$IPRCountries1,Full5$Company, FUN=sum))

#Number of subsidiaries in low IPR locations
Full5$IPRSub1<-ifelse(Full5$Country1!=Full5$Country2 & Full5$IPR<75,1,0)
Full5$IPRSub1<-as.numeric(Full5$IPRSub1)
Full5$IPRSub<- with(Full5, ave(Full5$IPRSub1,Full5$Company, FUN=sum))

##Average patent applications per host country
Full5$TP000<-case_when(Full5$Country1!=Full5$Country2~Full5$TP,
                       Full5$Country1==Full5$Country2~0)
Full5$TP00<- ifelse(Full5$Country1!=Full5$Country2 & Full5$Country2!=lag(Full5$Country2),Full5$TP000,0)
Full5$TP00[is.na(Full5$TP00)] <- 0
Full5$TP00<-as.numeric(Full5$TP00)
Full5$TP0<-with(Full5, ave(Full5$TP00,Full5$Company, FUN=sum))
Full5$CPat<-Full5$TP0/Full5$Countries
Full5$CPatRat<-Full5$CPat/Full5$TP         #Ratio of home and host country patents

#Total GDP of host countries
Full5$CGDP00<-case_when(Full5$Country1!=Full5$Country2~Full5$GDP,
                        Full5$Country1==Full5$Country2~0)
Full5$CGDP0<- ifelse(Full5$Country1!=Full5$Country2 & Full5$Country2!=lag(Full5$Country2),Full5$CGDP00,0)
Full5$CGDP0[is.na(Full5$CGDP0)] <- 0
Full5$CGDP0<-as.numeric(Full5$CGDP0)
Full5$CGDP<-with(Full5, ave(Full5$CGDP0,Full5$Company, FUN=sum))

#Total GDP served
Full5$TotalGDP<-Full5$GDP+Full5$CGDP

#Average GDP per capita per host country
Full5$CGDPpc000<-case_when(Full5$Country1!=Full5$Country2~Full5$GDPpc,
                           Full5$Country1==Full5$Country2~0)
Full5$CGDPpc00<- ifelse(Full5$Country1!=Full5$Country2 & Full5$Country2!=lag(Full5$Country2),Full5$CGDPpc000,0)
Full5$CGDPpc00[is.na(Full5$CGDPpc00)] <- 0
Full5$CGDPpc00<-as.numeric(Full5$CGDPpc00)
Full5$CGDPpc0<-with(Full5, ave(Full5$CGDPpc00,Full5$Company, FUN=function(x) mean(x, na.rm=T)))
Full5$CGDPpc<-Full5$CGDPpc0/Full5$Countries

#Average share of high-tech exports per host country
Full5$CRD000<-case_when(Full5$Country1!=Full5$Country2~Full5$RD,
                        Full5$Country1==Full5$Country2~0)
Full5$CRD00<- ifelse(Full5$Country1!=Full5$Country2 & Full5$Country2!=lag(Full5$Country2),Full5$CRD000,0)
Full5$CRD00[is.na(Full5$CRD00)] <- 0
Full5$CRD00<-as.numeric(Full5$CRD00)
Full5$CRD0<-with(Full5, ave(Full5$CRD00,Full5$Company, FUN=sum))
Full5$CRD<-Full5$CRD0/Full5$Countries

#Differences in legal rights between home and (average) host
Full5$LR0000<-case_when(Full5$Country1!=Full5$Country2~Full5$LR,
                        Full5$Country1==Full5$Country2~0)
Full5$LR000<- ifelse(Full5$Country1!=Full5$Country2 & Full5$Country2!=lag(Full5$Country2),Full5$LR0000,0)
Full5$LR000[is.na(Full5$LR000)] <- 0
Full5$LR00<-with(Full5,ave(Full5$LR000,Full5$Company, FUN=sum))
Full5$LR0<-Full5$LR00/Full5$Countries
Full5$LRDist<-Full5$LR-Full5$LR0

#Technological Distance
Full5$TR4<-case_when(Full5$Country1!=Full5$Country2~Full5$TR,
                     Full5$Country1==Full5$Country2~0)
Full5$TR3<- ifelse(Full5$Country1!=Full5$Country2 & Full5$Country2!=lag(Full5$Country2),Full5$TR4,0)
Full5$TR3[is.na(Full5$TR3)] <- 0
Full5$TR2<-with(Full5,ave(Full5$TR3,Full5$Company, FUN=sum))
Full5$TR1<-Full5$TR2/Full5$Countries
Full5$TRDist<-Full5$TR-Full5$TR1

##Differences in Innovativeness between home and (average) hostcountry (based on GCI)
Full5$Inno0000<-case_when(Full5$Country1!=Full5$Country2~Full5$Inno,
                          Full5$Country1==Full5$Country2~0)
Full5$Inno000<- ifelse(Full5$Country1!=Full5$Country2 & Full5$Country2!=lag(Full5$Country2),Full5$Inno0000,0)
Full5$Inno000[is.na(Full5$Inno000)] <- 0
Full5$Inno00<-with(Full5,ave(Full5$Inno000,Full5$Company, FUN=sum))
Full5$Inno0<-Full5$Inno00/Full5$Countries
Full5$InnoDist<-Full5$Inno-Full5$Inno0

#Sum of Innovation differences (Song and Shin 2008)
Full5$Inno1<-vlookup_df(Full5$Country1,Full5,result_column=10,lookup_column=5)
Full5$Inno1<-as.numeric(unlist((Full5$Inno1)))
Full5$Inno2<-Full5$Inno1-Full5$Inno
Full5$Inno3<-ifelse(Full5$Country1==Full5$Country2,0,Full5$Inno2)
Full5$Inno3[is.na(Full5$Inno3)] <- 0
Full5$Inno3<-abs(Full5$Inno3)
Full5$InnoDist2<-with(Full5,ave(Full5$Inno3,Full5$Company, FUN=sum))

#Number of high-tech countries
Full5$InnoCountries1<-ifelse(Full5$Country1!=Full5$Country2 & Full5$Country2!=lag(Full5$Country2) & Full5$Inno>5,1,0)
Full5$InnoCountries1<-as.numeric(Full5$InnoCountries1)
Full5$InnoCountries<- with(Full5, ave(Full5$InnoCountries1,Full5$Company, FUN=sum))

#Number of subsidiaries in high-tech locations
Full5$InnoSub1<-ifelse(Full5$Country1!=Full5$Country2 & Full5$Inno>5,1,0)
Full5$InnoSub1<-as.numeric(Full5$InnoSub1)
Full5$InnoSub<- with(Full5, ave(Full5$InnoSub1,Full5$Company, FUN=sum))

#Remove unnecessary columns
Full5 <- Full5[,c(-6,-7,-20,-21,-22,-23,-25,-26,-27,-29,-31,-33,-34,-35,-38,-39,-42,-43,-44,-46,-47,-48,-50,-51,-52,-53,-55,-56,-57,-58,
                  -60,-61,-62,-63,-65,-66,-67,-69,-71)]


##Match datasets for 2016
Full16$Country2<-as.character(Full16$Country2)

Full6<-left_join(Full16,Country2016,by="Country2",na_matches="never")

names(Full6) <- c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries",
                  "AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR","Inno")

##Create more variables
#Differences in intellectual property rights between home and (average) host (based on EFI)
Full6$IPR0000<-case_when(Full6$Country1!=Full6$Country2~Full6$IPR,
                         Full6$Country1==Full6$Country2~0)
Full6$IPR000<- ifelse(Full6$Country1!=Full6$Country2 & Full6$Country2!=lag(Full6$Country2),Full6$IPR0000,0)
Full6$IPR000[is.na(Full6$IPR000)] <- 0
Full6$IPR00<-with(Full6,ave(Full6$IPR000,Full6$Company, FUN=sum))
Full6$IPR0<-Full6$IPR00/Full6$Countries
Full6$IPRDist<-Full6$IPR-Full6$IPR0

#Sum of IPR differences (Song and Shin 2008)
Full6$IPR1<-vlookup_df(Full6$Country1,Full6,result_column=10,lookup_column=5)
Full6$IPR1<-as.numeric(unlist((Full6$IPR1)))
Full6$IPR2<-Full6$IPR1-Full6$IPR
Full6$IPR3<-ifelse(Full6$Country1==Full6$Country2,0,Full6$IPR2)
Full6$IPR3[is.na(Full6$IPR3)] <- 0
Full6$IPR3<-abs(Full6$IPR3)
Full6$IPRDist2<-with(Full6,ave(Full6$IPR3,Full6$Company, FUN=sum))

#Number of low IPR countries
Full6$IPRCountries1<-ifelse(Full6$Country1!=Full6$Country2 & Full6$Country2!=lag(Full6$Country2) & Full6$IPR<75,1,0)
Full6$IPRCountries1<-as.numeric(Full6$IPRCountries1)
Full6$IPRCountries1[is.na(Full6$IPRCountries1)] <- 0
Full6$IPRCountries<- with(Full6, ave(Full6$IPRCountries1,Full6$Company, FUN=sum))

#Number of subsidiaries in low IPR locations
Full6$IPRSub1<-ifelse(Full6$Country1!=Full6$Country2 & Full6$IPR<75,1,0)
Full6$IPRSub1<-as.numeric(Full6$IPRSub1)
Full6$IPRSub<- with(Full6, ave(Full6$IPRSub1,Full6$Company, FUN=sum))

##Average patent applications per host country
Full6$TP000<-case_when(Full6$Country1!=Full6$Country2~Full6$TP,
                       Full6$Country1==Full6$Country2~0)
Full6$TP00<- ifelse(Full6$Country1!=Full6$Country2 & Full6$Country2!=lag(Full6$Country2),Full6$TP000,0)
Full6$TP00[is.na(Full6$TP00)] <- 0
Full6$TP00<-as.numeric(Full6$TP00)
Full6$TP0<-with(Full6, ave(Full6$TP00,Full6$Company, FUN=sum))
Full6$CPat<-Full6$TP0/Full6$Countries
Full6$CPatRat<-Full6$CPat/Full6$TP         #Ratio of home and host country patents

#Total GDP of host countries
Full6$CGDP00<-case_when(Full6$Country1!=Full6$Country2~Full6$GDP,
                        Full6$Country1==Full6$Country2~0)
Full6$CGDP0<- ifelse(Full6$Country1!=Full6$Country2 & Full6$Country2!=lag(Full6$Country2),Full6$CGDP00,0)
Full6$CGDP0[is.na(Full6$CGDP0)] <- 0
Full6$CGDP0<-as.numeric(Full6$CGDP0)
Full6$CGDP<-with(Full6, ave(Full6$CGDP0,Full6$Company, FUN=sum))

#Total GDP served
Full6$TotalGDP<-Full6$GDP+Full6$CGDP

#Average GDP per capita per host country
Full6$CGDPpc000<-case_when(Full6$Country1!=Full6$Country2~Full6$GDPpc,
                           Full6$Country1==Full6$Country2~0)
Full6$CGDPpc00<- ifelse(Full6$Country1!=Full6$Country2 & Full6$Country2!=lag(Full6$Country2),Full6$CGDPpc000,0)
Full6$CGDPpc00[is.na(Full6$CGDPpc00)] <- 0
Full6$CGDPpc00<-as.numeric(Full6$CGDPpc00)
Full6$CGDPpc0<-with(Full6, ave(Full6$CGDPpc00,Full6$Company, FUN=function(x) mean(x, na.rm=T)))
Full6$CGDPpc<-Full6$CGDPpc0/Full6$Countries

#Average share of high-tech exports per host country
Full6$CRD000<-case_when(Full6$Country1!=Full6$Country2~Full6$RD,
                        Full6$Country1==Full6$Country2~0)
Full6$CRD00<- ifelse(Full6$Country1!=Full6$Country2 & Full6$Country2!=lag(Full6$Country2),Full6$CRD000,0)
Full6$CRD00[is.na(Full6$CRD00)] <- 0
Full6$CRD00<-as.numeric(Full6$CRD00)
Full6$CRD0<-with(Full6, ave(Full6$CRD00,Full6$Company, FUN=sum))
Full6$CRD<-Full6$CRD0/Full6$Countries

#Differences in legal rights between home and (average) host
Full6$LR0000<-case_when(Full6$Country1!=Full6$Country2~Full6$LR,
                        Full6$Country1==Full6$Country2~0)
Full6$LR000<- ifelse(Full6$Country1!=Full6$Country2 & Full6$Country2!=lag(Full6$Country2),Full6$LR0000,0)
Full6$LR000[is.na(Full6$LR000)] <- 0
Full6$LR00<-with(Full6,ave(Full6$LR000,Full6$Company, FUN=sum))
Full6$LR0<-Full6$LR00/Full6$Countries
Full6$LRDist<-Full6$LR-Full6$LR0

#Technological Distance
Full6$TR4<-case_when(Full6$Country1!=Full6$Country2~Full6$TR,
                     Full6$Country1==Full6$Country2~0)
Full6$TR3<- ifelse(Full6$Country1!=Full6$Country2 & Full6$Country2!=lag(Full6$Country2),Full6$TR4,0)
Full6$TR3[is.na(Full6$TR3)] <- 0
Full6$TR2<-with(Full6,ave(Full6$TR3,Full6$Company, FUN=sum))
Full6$TR1<-Full6$TR2/Full6$Countries
Full6$TRDist<-Full6$TR-Full6$TR1

##Differences in Innovativeness between home and (average) hostcountry (based on GCI)
Full6$Inno0000<-case_when(Full6$Country1!=Full6$Country2~Full6$Inno,
                          Full6$Country1==Full6$Country2~0)
Full6$Inno000<- ifelse(Full6$Country1!=Full6$Country2 & Full6$Country2!=lag(Full6$Country2),Full6$Inno0000,0)
Full6$Inno000[is.na(Full6$Inno000)] <- 0
Full6$Inno00<-with(Full6,ave(Full6$Inno000,Full6$Company, FUN=sum))
Full6$Inno0<-Full6$Inno00/Full6$Countries
Full6$InnoDist<-Full6$Inno-Full6$Inno0

#Sum of Innovation differences (Song and Shin 2008)
Full6$Inno1<-vlookup_df(Full6$Country1,Full6,result_column=10,lookup_column=5)
Full6$Inno1<-as.numeric(unlist((Full6$Inno1)))
Full6$Inno2<-Full6$Inno1-Full6$Inno
Full6$Inno3<-ifelse(Full6$Country1==Full6$Country2,0,Full6$Inno2)
Full6$Inno3[is.na(Full6$Inno3)] <- 0
Full6$Inno3<-abs(Full6$Inno3)
Full6$InnoDist2<-with(Full6,ave(Full6$Inno3,Full6$Company, FUN=sum))

#Number of high-tech countries
Full6$InnoCountries1<-ifelse(Full6$Country1!=Full6$Country2 & Full6$Country2!=lag(Full6$Country2) & Full6$Inno>5,1,0)
Full6$InnoCountries1<-as.numeric(Full6$InnoCountries1)
Full6$InnoCountries<- with(Full6, ave(Full6$InnoCountries1,Full6$Company, FUN=sum))

#Number of subsidiaries in high-tech locations
Full6$InnoSub1<-ifelse(Full6$Country1!=Full6$Country2 & Full6$Inno>5,1,0)
Full6$InnoSub1<-as.numeric(Full6$InnoSub1)
Full6$InnoSub<- with(Full6, ave(Full6$InnoSub1,Full6$Company, FUN=sum))

#Remove unnecessary columns
Full6 <- Full6[,c(-6,-7,-20,-21,-22,-23,-25,-26,-27,-29,-31,-33,-34,-35,-38,-39,-42,-43,-44,-46,-47,-48,-50,-51,-52,-53,-55,-56,-57,-58,
                  -60,-61,-62,-63,-65,-66,-67,-69,-71)]


##Match datasets for 2017
Full17$Country2<-as.character(Full17$Country2)

Full7<-left_join(Full17,Country2017,by="Country2",na_matches="never")

names(Full7) <- c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries",
                  "AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR","Inno")


##Create more variables
#Differences in intellectual property rights between home and (average) host (based on EFI)
Full7$IPR0000<-case_when(Full7$Country1!=Full7$Country2~Full7$IPR,
                         Full7$Country1==Full7$Country2~0)
Full7$IPR000<- ifelse(Full7$Country1!=Full7$Country2 & Full7$Country2!=lag(Full7$Country2),Full7$IPR0000,0)
Full7$IPR000[is.na(Full7$IPR000)] <- 0
Full7$IPR00<-with(Full7,ave(Full7$IPR000,Full7$Company, FUN=sum))
Full7$IPR0<-Full7$IPR00/Full7$Countries
Full7$IPRDist<-Full7$IPR-Full7$IPR0

#Sum of IPR differences (Song and Shin 2008)
Full7$IPR1<-vlookup_df(Full7$Country1,Full7,result_column=10,lookup_column=5)
Full7$IPR1<-as.numeric(unlist((Full7$IPR1)))
Full7$IPR2<-Full7$IPR1-Full7$IPR
Full7$IPR3<-ifelse(Full7$Country1==Full7$Country2,0,Full7$IPR2)
Full7$IPR3[is.na(Full7$IPR3)] <- 0
Full7$IPR3<-abs(Full7$IPR3)
Full7$IPRDist2<-with(Full7,ave(Full7$IPR3,Full7$Company, FUN=sum))

#Number of low IPR countries
Full7$IPRCountries1<-ifelse(Full7$Country1!=Full7$Country2 & Full7$Country2!=lag(Full7$Country2) & Full7$IPR<75,1,0)
Full7$IPRCountries1<-as.numeric(Full7$IPRCountries1)
Full7$IPRCountries1[is.na(Full7$IPRCountries1)] <- 0
Full7$IPRCountries<- with(Full7, ave(Full7$IPRCountries1,Full7$Company, FUN=sum))

#Number of subsidiaries in low IPR locations
Full7$IPRSub1<-ifelse(Full7$Country1!=Full7$Country2 & Full7$IPR<75,1,0)
Full7$IPRSub1<-as.numeric(Full7$IPRSub1)
Full7$IPRSub<- with(Full7, ave(Full7$IPRSub1,Full7$Company, FUN=sum))

##Average patent applications per host country
Full7$TP000<-case_when(Full7$Country1!=Full7$Country2~Full7$TP,
                       Full7$Country1==Full7$Country2~0)
Full7$TP00<- ifelse(Full7$Country1!=Full7$Country2 & Full7$Country2!=lag(Full7$Country2),Full7$TP000,0)
Full7$TP00[is.na(Full7$TP00)] <- 0
Full7$TP00<-as.numeric(Full7$TP00)
Full7$TP0<-with(Full7, ave(Full7$TP00,Full7$Company, FUN=sum))
Full7$CPat<-Full7$TP0/Full7$Countries
Full7$CPatRat<-Full7$CPat/Full7$TP         #Ratio of home and host country patents

#Total GDP of host countries
Full7$CGDP00<-case_when(Full7$Country1!=Full7$Country2~Full7$GDP,
                        Full7$Country1==Full7$Country2~0)
Full7$CGDP0<- ifelse(Full7$Country1!=Full7$Country2 & Full7$Country2!=lag(Full7$Country2),Full7$CGDP00,0)
Full7$CGDP0[is.na(Full7$CGDP0)] <- 0
Full7$CGDP0<-as.numeric(Full7$CGDP0)
Full7$CGDP<-with(Full7, ave(Full7$CGDP0,Full7$Company, FUN=sum))

#Total GDP served
Full7$TotalGDP<-Full7$GDP+Full7$CGDP

#Average GDP per capita per host country
Full7$CGDPpc000<-case_when(Full7$Country1!=Full7$Country2~Full7$GDPpc,
                           Full7$Country1==Full7$Country2~0)
Full7$CGDPpc00<- ifelse(Full7$Country1!=Full7$Country2 & Full7$Country2!=lag(Full7$Country2),Full7$CGDPpc000,0)
Full7$CGDPpc00[is.na(Full7$CGDPpc00)] <- 0
Full7$CGDPpc00<-as.numeric(Full7$CGDPpc00)
Full7$CGDPpc0<-with(Full7, ave(Full7$CGDPpc00,Full7$Company, FUN=function(x) mean(x, na.rm=T)))
Full7$CGDPpc<-Full7$CGDPpc0/Full7$Countries

#Average share of high-tech exports per host country
Full7$CRD000<-case_when(Full7$Country1!=Full7$Country2~Full7$RD,
                        Full7$Country1==Full7$Country2~0)
Full7$CRD00<- ifelse(Full7$Country1!=Full7$Country2 & Full7$Country2!=lag(Full7$Country2),Full7$CRD000,0)
Full7$CRD00[is.na(Full7$CRD00)] <- 0
Full7$CRD00<-as.numeric(Full7$CRD00)
Full7$CRD0<-with(Full7, ave(Full7$CRD00,Full7$Company, FUN=sum))
Full7$CRD<-Full7$CRD0/Full7$Countries

#Differences in legal rights between home and (average) host
Full7$LR0000<-case_when(Full7$Country1!=Full7$Country2~Full7$LR,
                        Full7$Country1==Full7$Country2~0)
Full7$LR000<- ifelse(Full7$Country1!=Full7$Country2 & Full7$Country2!=lag(Full7$Country2),Full7$LR0000,0)
Full7$LR000[is.na(Full7$LR000)] <- 0
Full7$LR00<-with(Full7,ave(Full7$LR000,Full7$Company, FUN=sum))
Full7$LR0<-Full7$LR00/Full7$Countries
Full7$LRDist<-Full7$LR-Full7$LR0

#Technological Distance
Full7$TR4<-case_when(Full7$Country1!=Full7$Country2~Full7$TR,
                     Full7$Country1==Full7$Country2~0)
Full7$TR3<- ifelse(Full7$Country1!=Full7$Country2 & Full7$Country2!=lag(Full7$Country2),Full7$TR4,0)
Full7$TR3[is.na(Full7$TR3)] <- 0
Full7$TR2<-with(Full7,ave(Full7$TR3,Full7$Company, FUN=sum))
Full7$TR1<-Full7$TR2/Full7$Countries
Full7$TRDist<-Full7$TR-Full7$TR1

##Differences in Innovativeness between home and (average) hostcountry (based on GCI)
Full7$Inno0000<-case_when(Full7$Country1!=Full7$Country2~Full7$Inno,
                          Full7$Country1==Full7$Country2~0)
Full7$Inno000<- ifelse(Full7$Country1!=Full7$Country2 & Full7$Country2!=lag(Full7$Country2),Full7$Inno0000,0)
Full7$Inno000[is.na(Full7$Inno000)] <- 0
Full7$Inno00<-with(Full7,ave(Full7$Inno000,Full7$Company, FUN=sum))
Full7$Inno0<-Full7$Inno00/Full7$Countries
Full7$InnoDist<-Full7$Inno-Full7$Inno0

#Sum of Innovation differences (Song and Shin 2008)
Full7$Inno1<-vlookup_df(Full7$Country1,Full7,result_column=10,lookup_column=5)
Full7$Inno1<-as.numeric(unlist((Full7$Inno1)))
Full7$Inno2<-Full7$Inno1-Full7$Inno
Full7$Inno3<-ifelse(Full7$Country1==Full7$Country2,0,Full7$Inno2)
Full7$Inno3[is.na(Full7$Inno3)] <- 0
Full7$Inno3<-abs(Full7$Inno3)
Full7$InnoDist2<-with(Full7,ave(Full7$Inno3,Full7$Company, FUN=sum))

#Number of high-tech countries
Full7$InnoCountries1<-ifelse(Full7$Country1!=Full7$Country2 & Full7$Country2!=lag(Full7$Country2) & Full7$Inno>5,1,0)
Full7$InnoCountries1<-as.numeric(Full7$InnoCountries1)
Full7$InnoCountries<- with(Full7, ave(Full7$InnoCountries1,Full7$Company, FUN=sum))

#Number of subsidiaries in high-tech locations
Full7$InnoSub1<-ifelse(Full7$Country1!=Full7$Country2 & Full7$Inno>5,1,0)
Full7$InnoSub1<-as.numeric(Full7$InnoSub1)
Full7$InnoSub<- with(Full7, ave(Full7$InnoSub1,Full7$Company, FUN=sum))

#Remove unnecessary columns
Full7 <- Full7[,c(-6,-7,-20,-21,-22,-23,-25,-26,-27,-29,-31,-33,-34,-35,-38,-39,-42,-43,-44,-46,-47,-48,-50,-51,-52,-53,-55,-56,-57,-58,
                  -60,-61,-62,-63,-65,-66,-67,-69,-71)]



##Match datasets for 2018
Full18$Country2<-as.character(Full18$Country2)

Full8<-left_join(Full18,Country2018,by="Country2",na_matches="never")

names(Full8) <- c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries",
                  "AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR","Inno")


##Create more variables
#Differences in intellectual property rights between home and (average) host (based on EFI)
Full8$IPR0000<-case_when(Full8$Country1!=Full8$Country2~Full8$IPR,
                         Full8$Country1==Full8$Country2~0)
Full8$IPR000<- ifelse(Full8$Country1!=Full8$Country2 & Full8$Country2!=lag(Full8$Country2),Full8$IPR0000,0)
Full8$IPR000[is.na(Full8$IPR000)] <- 0
Full8$IPR00<-with(Full8,ave(Full8$IPR000,Full8$Company, FUN=sum))
Full8$IPR0<-Full8$IPR00/Full8$Countries
Full8$IPRDist<-Full8$IPR-Full8$IPR0

#Sum of IPR differences (Song and Shin 2008)
Full8$IPR1<-vlookup_df(Full8$Country1,Full8,result_column=10,lookup_column=5)
Full8$IPR1<-as.numeric(unlist((Full8$IPR1)))
Full8$IPR2<-Full8$IPR1-Full8$IPR
Full8$IPR3<-ifelse(Full8$Country1==Full8$Country2,0,Full8$IPR2)
Full8$IPR3[is.na(Full8$IPR3)] <- 0
Full8$IPR3<-abs(Full8$IPR3)
Full8$IPRDist2<-with(Full8,ave(Full8$IPR3,Full8$Company, FUN=sum))

#Number of low IPR countries
Full8$IPRCountries1<-ifelse(Full8$Country1!=Full8$Country2 & Full8$Country2!=lag(Full8$Country2) & Full8$IPR<75,1,0)
Full8$IPRCountries1<-as.numeric(Full8$IPRCountries1)
Full8$IPRCountries1[is.na(Full8$IPRCountries1)] <- 0
Full8$IPRCountries<- with(Full8, ave(Full8$IPRCountries1,Full8$Company, FUN=sum))

#Number of subsidiaries in low IPR locations
Full8$IPRSub1<-ifelse(Full8$Country1!=Full8$Country2 & Full8$IPR<75,1,0)
Full8$IPRSub1<-as.numeric(Full8$IPRSub1)
Full8$IPRSub<- with(Full8, ave(Full8$IPRSub1,Full8$Company, FUN=sum))

##Average patent applications per host country
Full8$TP000<-case_when(Full8$Country1!=Full8$Country2~Full8$TP,
                       Full8$Country1==Full8$Country2~0)
Full8$TP00<- ifelse(Full8$Country1!=Full8$Country2 & Full8$Country2!=lag(Full8$Country2),Full8$TP000,0)
Full8$TP00[is.na(Full8$TP00)] <- 0
Full8$TP00<-as.numeric(Full8$TP00)
Full8$TP0<-with(Full8, ave(Full8$TP00,Full8$Company, FUN=sum))
Full8$CPat<-Full8$TP0/Full8$Countries
Full8$CPatRat<-Full8$CPat/Full8$TP         #Ratio of home and host country patents

#Total GDP of host countries
Full8$CGDP00<-case_when(Full8$Country1!=Full8$Country2~Full8$GDP,
                        Full8$Country1==Full8$Country2~0)
Full8$CGDP0<- ifelse(Full8$Country1!=Full8$Country2 & Full8$Country2!=lag(Full8$Country2),Full8$CGDP00,0)
Full8$CGDP0[is.na(Full8$CGDP0)] <- 0
Full8$CGDP0<-as.numeric(Full8$CGDP0)
Full8$CGDP<-with(Full8, ave(Full8$CGDP0,Full8$Company, FUN=sum))

#Total GDP served
Full8$TotalGDP<-Full8$GDP+Full8$CGDP

#Average GDP per capita per host country
Full8$CGDPpc000<-case_when(Full8$Country1!=Full8$Country2~Full8$GDPpc,
                           Full8$Country1==Full8$Country2~0)
Full8$CGDPpc00<- ifelse(Full8$Country1!=Full8$Country2 & Full8$Country2!=lag(Full8$Country2),Full8$CGDPpc000,0)
Full8$CGDPpc00[is.na(Full8$CGDPpc00)] <- 0
Full8$CGDPpc00<-as.numeric(Full8$CGDPpc00)
Full8$CGDPpc0<-with(Full8, ave(Full8$CGDPpc00,Full8$Company, FUN=function(x) mean(x, na.rm=T)))
Full8$CGDPpc<-Full8$CGDPpc0/Full8$Countries

#Average share of high-tech exports per host country
Full8$CRD000<-case_when(Full8$Country1!=Full8$Country2~Full8$RD,
                        Full8$Country1==Full8$Country2~0)
Full8$CRD00<- ifelse(Full8$Country1!=Full8$Country2 & Full8$Country2!=lag(Full8$Country2),Full8$CRD000,0)
Full8$CRD00[is.na(Full8$CRD00)] <- 0
Full8$CRD00<-as.numeric(Full8$CRD00)
Full8$CRD0<-with(Full8, ave(Full8$CRD00,Full8$Company, FUN=sum))
Full8$CRD<-Full8$CRD0/Full8$Countries

#Differences in legal rights between home and (average) host
Full8$LR0000<-case_when(Full8$Country1!=Full8$Country2~Full8$LR,
                        Full8$Country1==Full8$Country2~0)
Full8$LR000<- ifelse(Full8$Country1!=Full8$Country2 & Full8$Country2!=lag(Full8$Country2),Full8$LR0000,0)
Full8$LR000[is.na(Full8$LR000)] <- 0
Full8$LR00<-with(Full8,ave(Full8$LR000,Full8$Company, FUN=sum))
Full8$LR0<-Full8$LR00/Full8$Countries
Full8$LRDist<-Full8$LR-Full8$LR0

#Technological Distance
Full8$TR4<-case_when(Full8$Country1!=Full8$Country2~Full8$TR,
                     Full8$Country1==Full8$Country2~0)
Full8$TR3<- ifelse(Full8$Country1!=Full8$Country2 & Full8$Country2!=lag(Full8$Country2),Full8$TR4,0)
Full8$TR3[is.na(Full8$TR3)] <- 0
Full8$TR2<-with(Full8,ave(Full8$TR3,Full8$Company, FUN=sum))
Full8$TR1<-Full8$TR2/Full8$Countries
Full8$TRDist<-Full8$TR-Full8$TR1

##Differences in Innovativeness between home and (average) hostcountry (based on GCI)
Full8$Inno0000<-case_when(Full8$Country1!=Full8$Country2~Full8$Inno,
                          Full8$Country1==Full8$Country2~0)
Full8$Inno000<- ifelse(Full8$Country1!=Full8$Country2 & Full8$Country2!=lag(Full8$Country2),Full8$Inno0000,0)
Full8$Inno000[is.na(Full8$Inno000)] <- 0
Full8$Inno00<-with(Full8,ave(Full8$Inno000,Full8$Company, FUN=sum))
Full8$Inno0<-Full8$Inno00/Full8$Countries
Full8$InnoDist<-Full8$Inno-Full8$Inno0

#Sum of Innovation differences (Song and Shin 2008)
Full8$Inno1<-vlookup_df(Full8$Country1,Full8,result_column=10,lookup_column=5)
Full8$Inno1<-as.numeric(unlist((Full8$Inno1)))
Full8$Inno2<-Full8$Inno1-Full8$Inno
Full8$Inno3<-ifelse(Full8$Country1==Full8$Country2,0,Full8$Inno2)
Full8$Inno3[is.na(Full8$Inno3)] <- 0
Full8$Inno3<-abs(Full8$Inno3)
Full8$InnoDist2<-with(Full8,ave(Full8$Inno3,Full8$Company, FUN=sum))

#Number of high-tech countries
Full8$InnoCountries1<-ifelse(Full8$Country1!=Full8$Country2 & Full8$Country2!=lag(Full8$Country2) & Full8$Inno>5,1,0)
Full8$InnoCountries1<-as.numeric(Full8$InnoCountries1)
Full8$InnoCountries<- with(Full8, ave(Full8$InnoCountries1,Full8$Company, FUN=sum))

#Number of subsidiaries in high-tech locations
Full8$InnoSub1<-ifelse(Full8$Country1!=Full8$Country2 & Full8$Inno>5,1,0)
Full8$InnoSub1<-as.numeric(Full8$InnoSub1)
Full8$InnoSub<- with(Full8, ave(Full8$InnoSub1,Full8$Company, FUN=sum))

#Remove unnecessary columns
Full8 <- Full8[,c(-6,-7,-20,-21,-22,-23,-25,-26,-27,-29,-31,-33,-34,-35,-38,-39,-42,-43,-44,-46,-47,-48,-50,-51,-52,-53,-55,-56,-57,-58,
                  -60,-61,-62,-63,-65,-66,-67,-69,-71)]



##Match datasets for 2019
Full19$Country2<-as.character(Full19$Country2)

Full9<-left_join(Full19,Country2019,by="Country2",na_matches="never")

names(Full9) <- c("Company","Subsidiaries","Year","Added","NoSub","Country1","Country2","ForSub","MultiSub","Countries",
                  "AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR","Inno")


##Create more variables
#Differences in intellectual property rights between home and (average) host (based on EFI)
Full9$IPR0000<-case_when(Full9$Country1!=Full9$Country2~Full9$IPR,
                         Full9$Country1==Full9$Country2~0)
Full9$IPR000<- ifelse(Full9$Country1!=Full9$Country2 & Full9$Country2!=lag(Full9$Country2),Full9$IPR0000,0)
Full9$IPR000[is.na(Full9$IPR000)] <- 0
Full9$IPR00<-with(Full9,ave(Full9$IPR000,Full9$Company, FUN=sum))
Full9$IPR0<-Full9$IPR00/Full9$Countries
Full9$IPRDist<-Full9$IPR-Full9$IPR0

#Sum of IPR differences (Song and Shin 2008)
Full9$IPR1<-vlookup_df(Full9$Country1,Full9,result_column=10,lookup_column=5)
Full9$IPR1<-as.numeric(unlist((Full9$IPR1)))
Full9$IPR2<-Full9$IPR1-Full9$IPR
Full9$IPR3<-ifelse(Full9$Country1==Full9$Country2,0,Full9$IPR2)
Full9$IPR3[is.na(Full9$IPR3)] <- 0
Full9$IPR3<-abs(Full9$IPR3)
Full9$IPRDist2<-with(Full9,ave(Full9$IPR3,Full9$Company, FUN=sum))

#Number of low IPR countries
Full9$IPRCountries1<-ifelse(Full9$Country1!=Full9$Country2 & Full9$Country2!=lag(Full9$Country2) & Full9$IPR<75,1,0)
Full9$IPRCountries1<-as.numeric(Full9$IPRCountries1)
Full9$IPRCountries1[is.na(Full9$IPRCountries1)] <- 0
Full9$IPRCountries<- with(Full9, ave(Full9$IPRCountries1,Full9$Company, FUN=sum))

#Number of subsidiaries in low IPR locations
Full9$IPRSub1<-ifelse(Full9$Country1!=Full9$Country2 & Full9$IPR<75,1,0)
Full9$IPRSub1<-as.numeric(Full9$IPRSub1)
Full9$IPRSub<- with(Full9, ave(Full9$IPRSub1,Full9$Company, FUN=sum))

##Average patent applications per host country
Full9$TP000<-case_when(Full9$Country1!=Full9$Country2~Full9$TP,
                       Full9$Country1==Full9$Country2~0)
Full9$TP00<- ifelse(Full9$Country1!=Full9$Country2 & Full9$Country2!=lag(Full9$Country2),Full9$TP000,0)
Full9$TP00[is.na(Full9$TP00)] <- 0
Full9$TP00<-as.numeric(Full9$TP00)
Full9$TP0<-with(Full9, ave(Full9$TP00,Full9$Company, FUN=sum))
Full9$CPat<-Full9$TP0/Full9$Countries
Full9$CPatRat<-Full9$CPat/Full9$TP         #Ratio of home and host country patents

#Total GDP of host countries
Full9$CGDP00<-case_when(Full9$Country1!=Full9$Country2~Full9$GDP,
                        Full9$Country1==Full9$Country2~0)
Full9$CGDP0<- ifelse(Full9$Country1!=Full9$Country2 & Full9$Country2!=lag(Full9$Country2),Full9$CGDP00,0)
Full9$CGDP0[is.na(Full9$CGDP0)] <- 0
Full9$CGDP0<-as.numeric(Full9$CGDP0)
Full9$CGDP<-with(Full9, ave(Full9$CGDP0,Full9$Company, FUN=sum))

#Total GDP served
Full9$TotalGDP<-Full9$GDP+Full9$CGDP

#Average GDP per capita per host country
Full9$CGDPpc000<-case_when(Full9$Country1!=Full9$Country2~Full9$GDPpc,
                           Full9$Country1==Full9$Country2~0)
Full9$CGDPpc00<- ifelse(Full9$Country1!=Full9$Country2 & Full9$Country2!=lag(Full9$Country2),Full9$CGDPpc000,0)
Full9$CGDPpc00[is.na(Full9$CGDPpc00)] <- 0
Full9$CGDPpc00<-as.numeric(Full9$CGDPpc00)
Full9$CGDPpc0<-with(Full9, ave(Full9$CGDPpc00,Full9$Company, FUN=function(x) mean(x, na.rm=T)))
Full9$CGDPpc<-Full9$CGDPpc0/Full9$Countries

#Average share of high-tech exports per host country
Full9$CRD000<-case_when(Full9$Country1!=Full9$Country2~Full9$RD,
                        Full9$Country1==Full9$Country2~0)
Full9$CRD00<- ifelse(Full9$Country1!=Full9$Country2 & Full9$Country2!=lag(Full9$Country2),Full9$CRD000,0)
Full9$CRD00[is.na(Full9$CRD00)] <- 0
Full9$CRD00<-as.numeric(Full9$CRD00)
Full9$CRD0<-with(Full9, ave(Full9$CRD00,Full9$Company, FUN=sum))
Full9$CRD<-Full9$CRD0/Full9$Countries

#Differences in legal rights between home and (average) host
Full9$LR0000<-case_when(Full9$Country1!=Full9$Country2~Full9$LR,
                        Full9$Country1==Full9$Country2~0)
Full9$LR000<- ifelse(Full9$Country1!=Full9$Country2 & Full9$Country2!=lag(Full9$Country2),Full9$LR0000,0)
Full9$LR000[is.na(Full9$LR000)] <- 0
Full9$LR00<-with(Full9,ave(Full9$LR000,Full9$Company, FUN=sum))
Full9$LR0<-Full9$LR00/Full9$Countries
Full9$LRDist<-Full9$LR-Full9$LR0

#Technological Distance
Full9$TR4<-case_when(Full9$Country1!=Full9$Country2~Full9$TR,
                     Full9$Country1==Full9$Country2~0)
Full9$TR3<- ifelse(Full9$Country1!=Full9$Country2 & Full9$Country2!=lag(Full9$Country2),Full9$TR4,0)
Full9$TR3[is.na(Full9$TR3)] <- 0
Full9$TR2<-with(Full9,ave(Full9$TR3,Full9$Company, FUN=sum))
Full9$TR1<-Full9$TR2/Full9$Countries
Full9$TRDist<-Full9$TR-Full9$TR1

##Differences in Innovativeness between home and (average) hostcountry (based on GCI)
Full9$Inno0000<-case_when(Full9$Country1!=Full9$Country2~Full9$Inno,
                          Full9$Country1==Full9$Country2~0)
Full9$Inno000<- ifelse(Full9$Country1!=Full9$Country2 & Full9$Country2!=lag(Full9$Country2),Full9$Inno0000,0)
Full9$Inno000[is.na(Full9$Inno000)] <- 0
Full9$Inno00<-with(Full9,ave(Full9$Inno000,Full9$Company, FUN=sum))
Full9$Inno0<-Full9$Inno00/Full9$Countries
Full9$InnoDist<-Full9$Inno-Full9$Inno0

#Sum of Innovation differences (Song and Shin 2008)
Full9$Inno1<-vlookup_df(Full9$Country1,Full9,result_column=10,lookup_column=5)
Full9$Inno1<-as.numeric(unlist((Full9$Inno1)))
Full9$Inno2<-Full9$Inno1-Full9$Inno
Full9$Inno3<-ifelse(Full9$Country1==Full9$Country2,0,Full9$Inno2)
Full9$Inno3[is.na(Full9$Inno3)] <- 0
Full9$Inno3<-abs(Full9$Inno3)
Full9$InnoDist2<-with(Full9,ave(Full9$Inno3,Full9$Company, FUN=sum))

#Number of high-tech countries
Full9$InnoCountries1<-ifelse(Full9$Country1!=Full9$Country2 & Full9$Country2!=lag(Full9$Country2) & Full9$Inno>5,1,0)
Full9$InnoCountries1<-as.numeric(Full9$InnoCountries1)
Full9$InnoCountries<- with(Full9, ave(Full9$InnoCountries1,Full9$Company, FUN=sum))

#Number of subsidiaries in high-tech locations
Full9$InnoSub1<-ifelse(Full9$Country1!=Full9$Country2 & Full9$Inno>5,1,0)
Full9$InnoSub1<-as.numeric(Full9$InnoSub1)
Full9$InnoSub<- with(Full9, ave(Full9$InnoSub1,Full9$Company, FUN=sum))

#Remove unnecessary columns
Full9 <- Full9[,c(-6,-7,-20,-21,-22,-23,-25,-26,-27,-29,-31,-33,-34,-35,-38,-39,-42,-43,-44,-46,-47,-48,-50,-51,-52,-53,-55,-56,-57,-58,
                  -60,-61,-62,-63,-65,-66,-67,-69,-71)]

#Remove unnecessary files
rm(col_order,cols.num,Date_data,Deals_data,Full10,Full11,Full12,Full13,Full14,Full15,Full16,Full17,Full18,Full19,Full20,
   Country2011,Country2012,Country2013,Country2014,Country2015,Country2016,Country2017,Country2018,Country2019,Date_files,
   Country,Final,Subs,Own)

write.csv2(Full1, file = "files_created_code1/Full1.csv",row.names = F)
write.csv2(Full2, file = "files_created_code1/Full2.csv",row.names = F)
write.csv2(Full3, file = "files_created_code1/Full3.csv",row.names = F)
write.csv2(Full4, file = "files_created_code1/Full4.csv",row.names = F)
write.csv2(Full5, file = "files_created_code1/Full5.csv",row.names = F)
write.csv2(Full6, file = "files_created_code1/Full6.csv",row.names = F)
write.csv2(Full7, file = "files_created_code1/Full7.csv",row.names = F)
write.csv2(Full8, file = "files_created_code1/Full8.csv",row.names = F)
write.csv2(Full9, file = "files_created_code1/Full9.csv",row.names = F)


#### > ####

### Part IV: Attach Patent Data ####
#   1. Add and clean patent data from Orbis IP dataset ####

##Create global AI dataset based on Orbis IP data
#Read data of foreign subsidiaries (1:Name,2:Company-BvDID,3:GUOBvDID,4:Parent-BvDID):
Stock1<-read_excel("Dataset/Patents/Stock1.xlsx",sheet = "Results")
Stock2<-read_excel("Dataset/Patents/Stock2.xlsx",sheet = "Results")
Stock3<-read_excel("Dataset/Patents/Stock3.xlsx",sheet = "Results")
Stock4<-read_excel("Dataset/Patents/Stock4.xlsx",sheet = "Results")
Stock5<-read_excel("Dataset/Patents/Stock5.xlsx",sheet = "Results")
Stock6<-read_excel("Dataset/Patents/Stock6.xlsx",sheet = "Results")
Stock7<-read_excel("Dataset/Patents/Stock7.xlsx",sheet = "Results")
Stock8<-read_excel("Dataset/Patents/Stock8.xlsx",sheet = "Results")
Stock9<-read_excel("Dataset/Patents/Stock9.xlsx",sheet = "Results")
Stock10<-read_excel("Dataset/Patents/Stock10.xlsx",sheet = "Results")
Stock11<-read_excel("Dataset/Patents/Stock11.xlsx",sheet = "Results")
Stock12<-read_excel("Dataset/Patents/Stock12.xlsx",sheet = "Results")
Stock13<-read_excel("Dataset/Patents/Stock13.xlsx",sheet = "Results")
Stock14<-read_excel("Dataset/Patents/Stock14.xlsx",sheet = "Results")
Stock15<-read_excel("Dataset/Patents/Stock15.xlsx",sheet = "Results")
Stock16<-read_excel("Dataset/Patents/Stock16.xlsx",sheet = "Results")
Stock17<-read_excel("Dataset/Patents/Stock17.xlsx",sheet = "Results")
Stock18<-read_excel("Dataset/Patents/Stock18.xlsx",sheet = "Results")
Stock19<-read_excel("Dataset/Patents/Stock19.xlsx",sheet = "Results")
Stock20<-read_excel("Dataset/Patents/Stock20.xlsx",sheet = "Results")
Stock21<-read_excel("Dataset/Patents/Stock21.xlsx",sheet = "Results")
Stock22<-read_excel("Dataset/Patents/Stock22.xlsx",sheet = "Results")
Stock23<-read_excel("Dataset/Patents/Stock23.xlsx",sheet = "Results")
Stock24<-read_excel("Dataset/Patents/Stock24.xlsx",sheet = "Results")
Stock25<-read_excel("Dataset/Patents/Stock25.xlsx",sheet = "Results")
Stock26<-read_excel("Dataset/Patents/Stock26.xlsx",sheet = "Results")
Stock27<-read_excel("Dataset/Patents/Stock27.xlsx",sheet = "Results")
Stock28<-read_excel("Dataset/Patents/Stock28.xlsx",sheet = "Results")
Stock29<-read_excel("Dataset/Patents/Stock29.xlsx",sheet = "Results")
Stock30<-read_excel("Dataset/Patents/Stock30.xlsx",sheet = "Results")
Stock31<-read_excel("Dataset/Patents/Stock31.xlsx",sheet = "Results")
Stock32<-read_excel("Dataset/Patents/Stock32.xlsx",sheet = "Results")
Stock33<-read_excel("Dataset/Patents/Stock33.xlsx",sheet = "Results")
Stock34<-read_excel("Dataset/Patents/Stock34.xlsx",sheet = "Results")
Stock35<-read_excel("Dataset/Patents/Stock35.xlsx",sheet = "Results")
Stock36<-read_excel("Dataset/Patents/Stock36.xlsx",sheet = "Results")
Stock37<-read_excel("Dataset/Patents/Stock37.xlsx",sheet = "Results")
Stock38<-read_excel("Dataset/Patents/Stock38.xlsx",sheet = "Results")
Stock39<-read_excel("Dataset/Patents/Stock39.xlsx",sheet = "Results")
Stock40<-read_excel("Dataset/Patents/Stock40.xlsx",sheet = "Results")
Stock41<-read_excel("Dataset/Patents/Stock41.xlsx",sheet = "Results")
Stock42<-read_excel("Dataset/Patents/Stock42.xlsx",sheet = "Results")
Stock43<-read_excel("Dataset/Patents/Stock43.xlsx",sheet = "Results")
Stock44<-read_excel("Dataset/Patents/Stock44.xlsx",sheet = "Results")
Stock45<-read_excel("Dataset/Patents/Stock45.xlsx",sheet = "Results")
Stock46<-read_excel("Dataset/Patents/Stock46.xlsx",sheet = "Results")
Stock47<-read_excel("Dataset/Patents/Stock47.xlsx",sheet = "Results")
Stock48<-read_excel("Dataset/Patents/Stock48.xlsx",sheet = "Results")
Stock49<-read_excel("Dataset/Patents/Stock49.xlsx",sheet = "Results")
Stock50<-read_excel("Dataset/Patents/Stock50.xlsx",sheet = "Results")
Stock51<-read_excel("Dataset/Patents/Stock51.xlsx",sheet = "Results")
Stock52<-read_excel("Dataset/Patents/Stock52.xlsx",sheet = "Results")
Stock53<-read_excel("Dataset/Patents/Stock53.xlsx",sheet = "Results")
Stock54<-read_excel("Dataset/Patents/Stock54.xlsx",sheet = "Results")
Stock55<-read_excel("Dataset/Patents/Stock55.xlsx",sheet = "Results")
Stock56<-read_excel("Dataset/Patents/Stock56.xlsx",sheet = "Results")
Stock57<-read_excel("Dataset/Patents/Stock57.xlsx",sheet = "Results")
Stock58<-read_excel("Dataset/Patents/Stock58.xlsx",sheet = "Results")
Stock59<-read_excel("Dataset/Patents/Stock59.xlsx",sheet = "Results")
Stock60<-read_excel("Dataset/Patents/Stock60.xlsx",sheet = "Results")
Stock61<-read_excel("Dataset/Patents/Stock61.xlsx",sheet = "Results")
Stock62<-read_excel("Dataset/Patents/Stock62.xlsx",sheet = "Results")
Stock63<-read_excel("Dataset/Patents/Stock63.xlsx",sheet = "Results")
Stock64<-read_excel("Dataset/Patents/Stock64.xlsx",sheet = "Results")
Stock65<-read_excel("Dataset/Patents/Stock65.xlsx",sheet = "Results")
Stock66<-read_excel("Dataset/Patents/Stock66.xlsx",sheet = "Results")
Stock67<-read_excel("Dataset/Patents/Stock67.xlsx",sheet = "Results")
Stock68<-read_excel("Dataset/Patents/Stock68.xlsx",sheet = "Results")
Stock69<-read_excel("Dataset/Patents/Stock69.xlsx",sheet = "Results")
Stock70<-read_excel("Dataset/Patents/Stock70.xlsx",sheet = "Results")
Stock71<-read_excel("Dataset/Patents/Stock71.xlsx",sheet = "Results")
Stock72<-read_excel("Dataset/Patents/Stock72.xlsx",sheet = "Results")
Stock73<-read_excel("Dataset/Patents/Stock73.xlsx",sheet = "Results")
Stock74<-read_excel("Dataset/Patents/Stock74.xlsx",sheet = "Results")
Stock75<-read_excel("Dataset/Patents/Stock75.xlsx",sheet = "Results")
Stock76<-read_excel("Dataset/Patents/Stock76.xlsx",sheet = "Results")
Stock77<-read_excel("Dataset/Patents/Stock77.xlsx",sheet = "Results")
Stock78<-read_excel("Dataset/Patents/Stock78.xlsx",sheet = "Results")
Stock79<-read_excel("Dataset/Patents/Stock79.xlsx",sheet = "Results")
Stock80<-read_excel("Dataset/Patents/Stock80.xlsx",sheet = "Results")
Stock81<-read_excel("Dataset/Patents/Stock81.xlsx",sheet = "Results")
Stock82<-read_excel("Dataset/Patents/Stock82.xlsx",sheet = "Results")
Stock83<-read_excel("Dataset/Patents/Stock83.xlsx",sheet = "Results")
Stock84<-read_excel("Dataset/Patents/Stock84.xlsx",sheet = "Results")
Stock85<-read_excel("Dataset/Patents/Stock85.xlsx",sheet = "Results")
Stock86<-read_excel("Dataset/Patents/Stock86.xlsx",sheet = "Results")
Stock87<-read_excel("Dataset/Patents/Stock87.xlsx",sheet = "Results")
Stock88<-read_excel("Dataset/Patents/Stock88.xlsx",sheet = "Results")
Stock89<-read_excel("Dataset/Patents/Stock89.xlsx",sheet = "Results")
Stock90<-read_excel("Dataset/Patents/Stock90.xlsx",sheet = "Results")
Stock91<-read_excel("Dataset/Patents/Stock91.xlsx",sheet = "Results")
Stock92<-read_excel("Dataset/Patents/Stock92.xlsx",sheet = "Results")
Stock93<-read_excel("Dataset/Patents/Stock93.xlsx",sheet = "Results")
Stock94<-read_excel("Dataset/Patents/Stock94.xlsx",sheet = "Results")
Stock95<-read_excel("Dataset/Patents/Stock95.xlsx",sheet = "Results")
Stock96<-read_excel("Dataset/Patents/Stock96.xlsx",sheet = "Results")
Stock97<-read_excel("Dataset/Patents/Stock97.xlsx",sheet = "Results")
Stock98<-read_excel("Dataset/Patents/Stock98.xlsx",sheet = "Results")
Stock99<-read_excel("Dataset/Patents/Stock99.xlsx",sheet = "Results")
Stock100<-read_excel("Dataset/Patents/Stock100.xlsx",sheet = "Results")
Stock101<-read_excel("Dataset/Patents/Stock101.xlsx",sheet = "Results")
Stock102<-read_excel("Dataset/Patents/Stock102.xlsx",sheet = "Results")
Stock103<-read_excel("Dataset/Patents/Stock103.xlsx",sheet = "Results")
Stock104<-read_excel("Dataset/Patents/Stock104.xlsx",sheet = "Results")
Stock105<-read_excel("Dataset/Patents/Stock105.xlsx",sheet = "Results")
Stock106<-read_excel("Dataset/Patents/Stock106.xlsx",sheet = "Results")
Stock107<-read_excel("Dataset/Patents/Stock107.xlsx",sheet = "Results")
Stock108<-read_excel("Dataset/Patents/Stock108.xlsx",sheet = "Results")
Stock109<-read_excel("Dataset/Patents/Stock109.xlsx",sheet = "Results")
Stock110<-read_excel("Dataset/Patents/Stock110.xlsx",sheet = "Results")
Stock111<-read_excel("Dataset/Patents/Stock111.xlsx",sheet = "Results")
Stock112<-read_excel("Dataset/Patents/Stock112.xlsx",sheet = "Results")
Stock113<-read_excel("Dataset/Patents/Stock113.xlsx",sheet = "Results")
Stock114<-read_excel("Dataset/Patents/Stock114.xlsx",sheet = "Results")
Stock115<-read_excel("Dataset/Patents/Stock115.xlsx",sheet = "Results")
Stock116<-read_excel("Dataset/Patents/Stock116.xlsx",sheet = "Results")
Stock117<-read_excel("Dataset/Patents/Stock117.xlsx",sheet = "Results")
Stock118<-read_excel("Dataset/Patents/Stock118.xlsx",sheet = "Results")
Stock119<-read_excel("Dataset/Patents/Stock119.xlsx",sheet = "Results")
Stock120<-read_excel("Dataset/Patents/Stock120.xlsx",sheet = "Results")
Stock121<-read_excel("Dataset/Patents/Stock121.xlsx",sheet = "Results")
Stock122<-read_excel("Dataset/Patents/Stock122.xlsx",sheet = "Results")
Stock123<-read_excel("Dataset/Patents/Stock123.xlsx",sheet = "Results")
Stock124<-read_excel("Dataset/Patents/Stock124.xlsx",sheet = "Results")
Stock125<-read_excel("Dataset/Patents/Stock125.xlsx",sheet = "Results")
Stock126<-read_excel("Dataset/Patents/Stock126.xlsx",sheet = "Results")
Stock127<-read_excel("Dataset/Patents/Stock127.xlsx",sheet = "Results")
Stock128<-read_excel("Dataset/Patents/Stock128.xlsx",sheet = "Results")
Stock129<-read_excel("Dataset/Patents/Stock129.xlsx",sheet = "Results")
Stock130<-read_excel("Dataset/Patents/Stock130.xlsx",sheet = "Results")
Stock131<-read_excel("Dataset/Patents/Stock131.xlsx",sheet = "Results")
Stock132<-read_excel("Dataset/Patents/Stock132.xlsx",sheet = "Results")
Stock133<-read_excel("Dataset/Patents/Stock133.xlsx",sheet = "Results")
Stock134<-read_excel("Dataset/Patents/Stock134.xlsx",sheet = "Results")
Stock135<-read_excel("Dataset/Patents/Stock135.xlsx",sheet = "Results")
Stock136<-read_excel("Dataset/Patents/Stock136.xlsx",sheet = "Results")
Stock137<-read_excel("Dataset/Patents/Stock137.xlsx",sheet = "Results")
Stock138<-read_excel("Dataset/Patents/Stock138.xlsx",sheet = "Results")
Stock139<-read_excel("Dataset/Patents/Stock139.xlsx",sheet = "Results")
Stock140<-read_excel("Dataset/Patents/Stock140.xlsx",sheet = "Results")
Stock141<-read_excel("Dataset/Patents/Stock141.xlsx",sheet = "Results")
Stock142<-read_excel("Dataset/Patents/Stock142.xlsx",sheet = "Results")
Stock143<-read_excel("Dataset/Patents/Stock143.xlsx",sheet = "Results")
Stock144<-read_excel("Dataset/Patents/Stock144.xlsx",sheet = "Results")
Stock145<-read_excel("Dataset/Patents/Stock145.xlsx",sheet = "Results")
Stock146<-read_excel("Dataset/Patents/Stock146.xlsx",sheet = "Results")
Stock147<-read_excel("Dataset/Patents/Stock147.xlsx",sheet = "Results")
Stock148<-read_excel("Dataset/Patents/Stock148.xlsx",sheet = "Results")
Stock149<-read_excel("Dataset/Patents/Stock149.xlsx",sheet = "Results")
Stock150<-read_excel("Dataset/Patents/Stock150.xlsx",sheet = "Results")
Stock151<-read_excel("Dataset/Patents/Stock151.xlsx",sheet = "Results")
Stock152<-read_excel("Dataset/Patents/Stock152.xlsx",sheet = "Results")
Stock153<-read_excel("Dataset/Patents/Stock153.xlsx",sheet = "Results")
Stock154<-read_excel("Dataset/Patents/Stock154.xlsx",sheet = "Results")
Stock155<-read_excel("Dataset/Patents/Stock155.xlsx",sheet = "Results")
Stock156<-read_excel("Dataset/Patents/Stock156.xlsx",sheet = "Results")
Stock157<-read_excel("Dataset/Patents/Stock157.xlsx",sheet = "Results")
Stock158<-read_excel("Dataset/Patents/Stock158.xlsx",sheet = "Results")
Stock159<-read_excel("Dataset/Patents/Stock159.xlsx",sheet = "Results")
Stock160<-read_excel("Dataset/Patents/Stock160.xlsx",sheet = "Results")
Stock161<-read_excel("Dataset/Patents/Stock161.xlsx",sheet = "Results")
Stock162<-read_excel("Dataset/Patents/Stock162.xlsx",sheet = "Results")
Stock163<-read_excel("Dataset/Patents/Stock163.xlsx",sheet = "Results")
Stock164<-read_excel("Dataset/Patents/Stock164.xlsx",sheet = "Results")
Stock165<-read_excel("Dataset/Patents/Stock165.xlsx",sheet = "Results")
Stock166<-read_excel("Dataset/Patents/Stock166.xlsx",sheet = "Results")
Stock167<-read_excel("Dataset/Patents/Stock167.xlsx",sheet = "Results")
Stock168<-read_excel("Dataset/Patents/Stock168.xlsx",sheet = "Results")
Stock169<-read_excel("Dataset/Patents/Stock169.xlsx",sheet = "Results")
Stock170<-read_excel("Dataset/Patents/Stock170.xlsx",sheet = "Results")
Stock171<-read_excel("Dataset/Patents/Stock171.xlsx",sheet = "Results")
Stock172<-read_excel("Dataset/Patents/Stock172.xlsx",sheet = "Results")
Stock173<-read_excel("Dataset/Patents/Stock173.xlsx",sheet = "Results")
Stock174<-read_excel("Dataset/Patents/Stock174.xlsx",sheet = "Results")
Stock175<-read_excel("Dataset/Patents/Stock175.xlsx",sheet = "Results")
Stock176<-read_excel("Dataset/Patents/Stock176.xlsx",sheet = "Results")
Stock177<-read_excel("Dataset/Patents/Stock177.xlsx",sheet = "Results")
Stock178<-read_excel("Dataset/Patents/Stock178.xlsx",sheet = "Results")
Stock179<-read_excel("Dataset/Patents/Stock179.xlsx",sheet = "Results")
Stock180<-read_excel("Dataset/Patents/Stock180.xlsx",sheet = "Results")
Stock181<-read_excel("Dataset/Patents/Stock181.xlsx",sheet = "Results")
Stock182<-read_excel("Dataset/Patents/Stock182.xlsx",sheet = "Results")
Stock183<-read_excel("Dataset/Patents/Stock183.xlsx",sheet = "Results")
Stock184<-read_excel("Dataset/Patents/Stock184.xlsx",sheet = "Results")
Stock185<-read_excel("Dataset/Patents/Stock185.xlsx",sheet = "Results")
Stock186<-read_excel("Dataset/Patents/Stock186.xlsx",sheet = "Results")
Stock187<-read_excel("Dataset/Patents/Stock187.xlsx",sheet = "Results")
Stock188<-read_excel("Dataset/Patents/Stock188.xlsx",sheet = "Results")
Stock189<-read_excel("Dataset/Patents/Stock189.xlsx",sheet = "Results")
Stock190<-read_excel("Dataset/Patents/Stock190.xlsx",sheet = "Results")
Stock191<-read_excel("Dataset/Patents/Stock191.xlsx",sheet = "Results")
Stock192<-read_excel("Dataset/Patents/Stock192.xlsx",sheet = "Results")
Stock193<-read_excel("Dataset/Patents/Stock193.xlsx",sheet = "Results")
Stock194<-read_excel("Dataset/Patents/Stock194.xlsx",sheet = "Results")
Stock195<-read_excel("Dataset/Patents/Stock195.xlsx",sheet = "Results")
Stock196<-read_excel("Dataset/Patents/Stock196.xlsx",sheet = "Results")
Stock197<-read_excel("Dataset/Patents/Stock197.xlsx",sheet = "Results")
Stock198<-read_excel("Dataset/Patents/Stock198.xlsx",sheet = "Results")
Stock199<-read_excel("Dataset/Patents/Stock199.xlsx",sheet = "Results")
Stock200<-read_excel("Dataset/Patents/Stock200.xlsx",sheet = "Results")
Stock201<-read_excel("Dataset/Patents/Stock201.xlsx",sheet = "Results")
Stock202<-read_excel("Dataset/Patents/Stock202.xlsx",sheet = "Results")
Stock203<-read_excel("Dataset/Patents/Stock203.xlsx",sheet = "Results")
Stock204<-read_excel("Dataset/Patents/Stock204.xlsx",sheet = "Results")
Stock205<-read_excel("Dataset/Patents/Stock205.xlsx",sheet = "Results")
Stock206<-read_excel("Dataset/Patents/Stock206.xlsx",sheet = "Results")
Stock207<-read_excel("Dataset/Patents/Stock207.xlsx",sheet = "Results")
Stock208<-read_excel("Dataset/Patents/Stock208.xlsx",sheet = "Results")
Stock209<-read_excel("Dataset/Patents/Stock209.xlsx",sheet = "Results")
Stock210<-read_excel("Dataset/Patents/Stock210.xlsx",sheet = "Results")
Stock211<-read_excel("Dataset/Patents/Stock211.xlsx",sheet = "Results")
Stock212<-read_excel("Dataset/Patents/Stock212.xlsx",sheet = "Results")
Stock213<-read_excel("Dataset/Patents/Stock213.xlsx",sheet = "Results")
Stock214<-read_excel("Dataset/Patents/Stock214.xlsx",sheet = "Results")
Stock215<-read_excel("Dataset/Patents/Stock215.xlsx",sheet = "Results")
Stock216<-read_excel("Dataset/Patents/Stock216.xlsx",sheet = "Results")
Stock217<-read_excel("Dataset/Patents/Stock217.xlsx",sheet = "Results")
Stock218<-read_excel("Dataset/Patents/Stock218.xlsx",sheet = "Results")
Stock219<-read_excel("Dataset/Patents/Stock219.xlsx",sheet = "Results")
Stock220<-read_excel("Dataset/Patents/Stock220.xlsx",sheet = "Results")
Stock221<-read_excel("Dataset/Patents/Stock221.xlsx",sheet = "Results")
Stock222<-read_excel("Dataset/Patents/Stock222.xlsx",sheet = "Results")
Stock223<-read_excel("Dataset/Patents/Stock223.xlsx",sheet = "Results")
Stock224<-read_excel("Dataset/Patents/Stock224.xlsx",sheet = "Results")
Stock225<-read_excel("Dataset/Patents/Stock225.xlsx",sheet = "Results")
Stock226<-read_excel("Dataset/Patents/Stock226.xlsx",sheet = "Results")
Stock227<-read_excel("Dataset/Patents/Stock227.xlsx",sheet = "Results")
Stock228<-read_excel("Dataset/Patents/Stock228.xlsx",sheet = "Results")
Stock229<-read_excel("Dataset/Patents/Stock229.xlsx",sheet = "Results")
Stock230<-read_excel("Dataset/Patents/Stock230.xlsx",sheet = "Results")
Stock231<-read_excel("Dataset/Patents/Stock231.xlsx",sheet = "Results")
Stock232<-read_excel("Dataset/Patents/Stock232.xlsx",sheet = "Results")
Stock233<-read_excel("Dataset/Patents/Stock233.xlsx",sheet = "Results")
Stock234<-read_excel("Dataset/Patents/Stock234.xlsx",sheet = "Results")
Stock235<-read_excel("Dataset/Patents/Stock235.xlsx",sheet = "Results")
Stock236<-read_excel("Dataset/Patents/Stock236.xlsx",sheet = "Results")
Stock237<-read_excel("Dataset/Patents/Stock237.xlsx",sheet = "Results")
Stock238<-read_excel("Dataset/Patents/Stock238.xlsx",sheet = "Results")
Stock239<-read_excel("Dataset/Patents/Stock239.xlsx",sheet = "Results")
Stock240<-read_excel("Dataset/Patents/Stock240.xlsx",sheet = "Results")
Stock241<-read_excel("Dataset/Patents/Stock241.xlsx",sheet = "Results")
Stock242<-read_excel("Dataset/Patents/Stock242.xlsx",sheet = "Results")
Stock243<-read_excel("Dataset/Patents/Stock243.xlsx",sheet = "Results")
Stock244<-read_excel("Dataset/Patents/Stock244.xlsx",sheet = "Results")
Stock245<-read_excel("Dataset/Patents/Stock245.xlsx",sheet = "Results")
Stock246<-read_excel("Dataset/Patents/Stock246.xlsx",sheet = "Results")
Stock247<-read_excel("Dataset/Patents/Stock247.xlsx",sheet = "Results")
Stock248<-read_excel("Dataset/Patents/Stock248.xlsx",sheet = "Results")
Stock249<-read_excel("Dataset/Patents/Stock249.xlsx",sheet = "Results")
Stock250<-read_excel("Dataset/Patents/Stock250.xlsx",sheet = "Results")
Stock251<-read_excel("Dataset/Patents/Stock251.xlsx",sheet = "Results")
Stock252<-read_excel("Dataset/Patents/Stock252.xlsx",sheet = "Results")
Stock253<-read_excel("Dataset/Patents/Stock253.xlsx",sheet = "Results")
Stock254<-read_excel("Dataset/Patents/Stock254.xlsx",sheet = "Results")
Stock255<-read_excel("Dataset/Patents/Stock255.xlsx",sheet = "Results")
Stock256<-read_excel("Dataset/Patents/Stock256.xlsx",sheet = "Results")
Stock257<-read_excel("Dataset/Patents/Stock257.xlsx",sheet = "Results")
Stock258<-read_excel("Dataset/Patents/Stock258.xlsx",sheet = "Results")
Stock259<-read_excel("Dataset/Patents/Stock259.xlsx",sheet = "Results")
Stock260<-read_excel("Dataset/Patents/Stock260.xlsx",sheet = "Results")
Stock261<-read_excel("Dataset/Patents/Stock261.xlsx",sheet = "Results")
Stock262<-read_excel("Dataset/Patents/Stock262.xlsx",sheet = "Results")
Stock263<-read_excel("Dataset/Patents/Stock263.xlsx",sheet = "Results")
Stock264<-read_excel("Dataset/Patents/Stock264.xlsx",sheet = "Results")
Stock265<-read_excel("Dataset/Patents/Stock265.xlsx",sheet = "Results")
Stock266<-read_excel("Dataset/Patents/Stock266.xlsx",sheet = "Results")
Stock267<-read_excel("Dataset/Patents/Stock267.xlsx",sheet = "Results")
Stock268<-read_excel("Dataset/Patents/Stock268.xlsx",sheet = "Results")
Stock269<-read_excel("Dataset/Patents/Stock269.xlsx",sheet = "Results")
Stock270<-read_excel("Dataset/Patents/Stock270.xlsx",sheet = "Results")
Stock271<-read_excel("Dataset/Patents/Stock271.xlsx",sheet = "Results")
Stock272<-read_excel("Dataset/Patents/Stock272.xlsx",sheet = "Results")
Stock273<-read_excel("Dataset/Patents/Stock273.xlsx",sheet = "Results")
Stock274<-read_excel("Dataset/Patents/Stock274.xlsx",sheet = "Results")
Stock275<-read_excel("Dataset/Patents/Stock275.xlsx",sheet = "Results")
Stock276<-read_excel("Dataset/Patents/Stock276.xlsx",sheet = "Results")
Stock277<-read_excel("Dataset/Patents/Stock277.xlsx",sheet = "Results")
Stock278<-read_excel("Dataset/Patents/Stock278.xlsx",sheet = "Results")
Stock279<-read_excel("Dataset/Patents/Stock279.xlsx",sheet = "Results")
Stock280<-read_excel("Dataset/Patents/Stock280.xlsx",sheet = "Results")
Stock281<-read_excel("Dataset/Patents/Stock281.xlsx",sheet = "Results")
Stock282<-read_excel("Dataset/Patents/Stock282.xlsx",sheet = "Results")
Stock283<-read_excel("Dataset/Patents/Stock283.xlsx",sheet = "Results")
Stock284<-read_excel("Dataset/Patents/Stock284.xlsx",sheet = "Results")
Stock285<-read_excel("Dataset/Patents/Stock285.xlsx",sheet = "Results")
Stock286<-read_excel("Dataset/Patents/Stock286.xlsx",sheet = "Results")
Stock287<-read_excel("Dataset/Patents/Stock287.xlsx",sheet = "Results")
Stock288<-read_excel("Dataset/Patents/Stock288.xlsx",sheet = "Results")
Stock289<-read_excel("Dataset/Patents/Stock289.xlsx",sheet = "Results")
Stock290<-read_excel("Dataset/Patents/Stock290.xlsx",sheet = "Results")
Stock291<-read_excel("Dataset/Patents/Stock291.xlsx",sheet = "Results")
Stock292<-read_excel("Dataset/Patents/Stock292.xlsx",sheet = "Results")
Stock293<-read_excel("Dataset/Patents/Stock293.xlsx",sheet = "Results")
Stock294<-read_excel("Dataset/Patents/Stock294.xlsx",sheet = "Results")
Stock295<-read_excel("Dataset/Patents/Stock295.xlsx",sheet = "Results")
Stock296<-read_excel("Dataset/Patents/Stock296.xlsx",sheet = "Results")
Stock297<-read_excel("Dataset/Patents/Stock297.xlsx",sheet = "Results")
Stock298<-read_excel("Dataset/Patents/Stock298.xlsx",sheet = "Results")
Stock299<-read_excel("Dataset/Patents/Stock299.xlsx",sheet = "Results")
Stock300<-read_excel("Dataset/Patents/Stock300.xlsx",sheet = "Results")
Stock301<-read_excel("Dataset/Patents/Stock301.xlsx",sheet = "Results")
Stock302<-read_excel("Dataset/Patents/Stock302.xlsx",sheet = "Results")
Stock303<-read_excel("Dataset/Patents/Stock303.xlsx",sheet = "Results")
Stock304<-read_excel("Dataset/Patents/Stock304.xlsx",sheet = "Results")
Stock305<-read_excel("Dataset/Patents/Stock305.xlsx",sheet = "Results")
Stock306<-read_excel("Dataset/Patents/Stock306.xlsx",sheet = "Results")
Stock307<-read_excel("Dataset/Patents/Stock307.xlsx",sheet = "Results")
Stock308<-read_excel("Dataset/Patents/Stock308.xlsx",sheet = "Results")
Stock309<-read_excel("Dataset/Patents/Stock309.xlsx",sheet = "Results")
Stock310<-read_excel("Dataset/Patents/Stock310.xlsx",sheet = "Results")
Stock311<-read_excel("Dataset/Patents/Stock311.xlsx",sheet = "Results")
Stock312<-read_excel("Dataset/Patents/Stock312.xlsx",sheet = "Results")
Stock313<-read_excel("Dataset/Patents/Stock313.xlsx",sheet = "Results")
Stock314<-read_excel("Dataset/Patents/Stock314.xlsx",sheet = "Results")
Stock315<-read_excel("Dataset/Patents/Stock315.xlsx",sheet = "Results")
Stock316<-read_excel("Dataset/Patents/Stock316.xlsx",sheet = "Results")
Stock317<-read_excel("Dataset/Patents/Stock317.xlsx",sheet = "Results")
Stock318<-read_excel("Dataset/Patents/Stock318.xlsx",sheet = "Results")
Stock319<-read_excel("Dataset/Patents/Stock319.xlsx",sheet = "Results")
Stock320<-read_excel("Dataset/Patents/Stock320.xlsx",sheet = "Results")
Stock321<-read_excel("Dataset/Patents/Stock321.xlsx",sheet = "Results")
Stock322<-read_excel("Dataset/Patents/Stock322.xlsx",sheet = "Results")
Stock323<-read_excel("Dataset/Patents/Stock323.xlsx",sheet = "Results")
Stock324<-read_excel("Dataset/Patents/Stock324.xlsx",sheet = "Results")
Stock325<-read_excel("Dataset/Patents/Stock325.xlsx",sheet = "Results")
Stock326<-read_excel("Dataset/Patents/Stock326.xlsx",sheet = "Results")
Stock327<-read_excel("Dataset/Patents/Stock327.xlsx",sheet = "Results")
Stock328<-read_excel("Dataset/Patents/Stock328.xlsx",sheet = "Results")
Stock329<-read_excel("Dataset/Patents/Stock329.xlsx",sheet = "Results")
Stock330<-read_excel("Dataset/Patents/Stock330.xlsx",sheet = "Results")
Stock331<-read_excel("Dataset/Patents/Stock331.xlsx",sheet = "Results")
Stock332<-read_excel("Dataset/Patents/Stock332.xlsx",sheet = "Results")
Stock333<-read_excel("Dataset/Patents/Stock333.xlsx",sheet = "Results")
Stock334<-read_excel("Dataset/Patents/Stock334.xlsx",sheet = "Results")
Stock335<-read_excel("Dataset/Patents/Stock335.xlsx",sheet = "Results")
Stock336<-read_excel("Dataset/Patents/Stock336.xlsx",sheet = "Results")
Stock337<-read_excel("Dataset/Patents/Stock337.xlsx",sheet = "Results")
Stock338<-read_excel("Dataset/Patents/Stock338.xlsx",sheet = "Results")
Stock339<-read_excel("Dataset/Patents/Stock339.xlsx",sheet = "Results")
Stock340<-read_excel("Dataset/Patents/Stock340.xlsx",sheet = "Results")
Stock341<-read_excel("Dataset/Patents/Stock341.xlsx",sheet = "Results")
Stock342<-read_excel("Dataset/Patents/Stock342.xlsx",sheet = "Results")
Stock343<-read_excel("Dataset/Patents/Stock343.xlsx",sheet = "Results")
Stock344<-read_excel("Dataset/Patents/Stock344.xlsx",sheet = "Results")
Stock345<-read_excel("Dataset/Patents/Stock345.xlsx",sheet = "Results")
Stock346<-read_excel("Dataset/Patents/Stock346.xlsx",sheet = "Results")
Stock347<-read_excel("Dataset/Patents/Stock347.xlsx",sheet = "Results")
Stock348<-read_excel("Dataset/Patents/Stock348.xlsx",sheet = "Results")
Stock349<-read_excel("Dataset/Patents/Stock349.xlsx",sheet = "Results")
Stock350<-read_excel("Dataset/Patents/Stock350.xlsx",sheet = "Results")
Stock351<-read_excel("Dataset/Patents/Stock351.xlsx",sheet = "Results")
Stock352<-read_excel("Dataset/Patents/Stock352.xlsx",sheet = "Results")
Stock353<-read_excel("Dataset/Patents/Stock353.xlsx",sheet = "Results")
Stock354<-read_excel("Dataset/Patents/Stock354.xlsx",sheet = "Results")
Stock355<-read_excel("Dataset/Patents/Stock355.xlsx",sheet = "Results")
Stock356<-read_excel("Dataset/Patents/Stock356.xlsx",sheet = "Results")
Stock357<-read_excel("Dataset/Patents/Stock357.xlsx",sheet = "Results")
Stock358<-read_excel("Dataset/Patents/Stock358.xlsx",sheet = "Results")
Stock359<-read_excel("Dataset/Patents/Stock359.xlsx",sheet = "Results")
Stock360<-read_excel("Dataset/Patents/Stock360.xlsx",sheet = "Results")
Stock361<-read_excel("Dataset/Patents/Stock361.xlsx",sheet = "Results")
Stock362<-read_excel("Dataset/Patents/Stock362.xlsx",sheet = "Results")
Stock363<-read_excel("Dataset/Patents/Stock363.xlsx",sheet = "Results")
Stock364<-read_excel("Dataset/Patents/Stock364.xlsx",sheet = "Results")
Stock365<-read_excel("Dataset/Patents/Stock365.xlsx",sheet = "Results")
Stock366<-read_excel("Dataset/Patents/Stock366.xlsx",sheet = "Results")
Stock367<-read_excel("Dataset/Patents/Stock367.xlsx",sheet = "Results")
Stock368<-read_excel("Dataset/Patents/Stock368.xlsx",sheet = "Results")
Stock369<-read_excel("Dataset/Patents/Stock369.xlsx",sheet = "Results")
Stock370<-read_excel("Dataset/Patents/Stock370.xlsx",sheet = "Results")
Stock371<-read_excel("Dataset/Patents/Stock371.xlsx",sheet = "Results")
Stock372<-read_excel("Dataset/Patents/Stock372.xlsx",sheet = "Results")
Stock373<-read_excel("Dataset/Patents/Stock373.xlsx",sheet = "Results")
Stock374<-read_excel("Dataset/Patents/Stock374.xlsx",sheet = "Results")
Stock375<-read_excel("Dataset/Patents/Stock375.xlsx",sheet = "Results")
Stock376<-read_excel("Dataset/Patents/Stock376.xlsx",sheet = "Results")
Stock377<-read_excel("Dataset/Patents/Stock377.xlsx",sheet = "Results")
Stock378<-read_excel("Dataset/Patents/Stock378.xlsx",sheet = "Results")
Stock379<-read_excel("Dataset/Patents/Stock379.xlsx",sheet = "Results")
Stock380<-read_excel("Dataset/Patents/Stock380.xlsx",sheet = "Results")
Stock381<-read_excel("Dataset/Patents/Stock381.xlsx",sheet = "Results")
Stock382<-read_excel("Dataset/Patents/Stock382.xlsx",sheet = "Results")
Stock383<-read_excel("Dataset/Patents/Stock383.xlsx",sheet = "Results")
Stock384<-read_excel("Dataset/Patents/Stock384.xlsx",sheet = "Results")
Stock385<-read_excel("Dataset/Patents/Stock385.xlsx",sheet = "Results")
Stock386<-read_excel("Dataset/Patents/Stock386.xlsx",sheet = "Results")
Stock387<-read_excel("Dataset/Patents/Stock387.xlsx",sheet = "Results")
Stock388<-read_excel("Dataset/Patents/Stock388.xlsx",sheet = "Results")
Stock389<-read_excel("Dataset/Patents/Stock389.xlsx",sheet = "Results")
Stock390<-read_excel("Dataset/Patents/Stock390.xlsx",sheet = "Results")
Stock391<-read_excel("Dataset/Patents/Stock391.xlsx",sheet = "Results")
Stock392<-read_excel("Dataset/Patents/Stock392.xlsx",sheet = "Results")
Stock393<-read_excel("Dataset/Patents/Stock393.xlsx",sheet = "Results")
Stock394<-read_excel("Dataset/Patents/Stock394.xlsx",sheet = "Results")
Stock395<-read_excel("Dataset/Patents/Stock395.xlsx",sheet = "Results")
Stock396<-read_excel("Dataset/Patents/Stock396.xlsx",sheet = "Results")
Stock397<-read_excel("Dataset/Patents/Stock397.xlsx",sheet = "Results")
Stock398<-read_excel("Dataset/Patents/Stock398.xlsx",sheet = "Results")
Stock399<-read_excel("Dataset/Patents/Stock399.xlsx",sheet = "Results")
Stock400<-read_excel("Dataset/Patents/Stock400.xlsx",sheet = "Results")
Stock401<-read_excel("Dataset/Patents/Stock401.xlsx",sheet = "Results")
Stock402<-read_excel("Dataset/Patents/Stock402.xlsx",sheet = "Results")
Stock403<-read_excel("Dataset/Patents/Stock403.xlsx",sheet = "Results")
Stock404<-read_excel("Dataset/Patents/Stock404.xlsx",sheet = "Results")
Stock405<-read_excel("Dataset/Patents/Stock405.xlsx",sheet = "Results")
Stock406<-read_excel("Dataset/Patents/Stock406.xlsx",sheet = "Results")
Stock407<-read_excel("Dataset/Patents/Stock407.xlsx",sheet = "Results")
Stock408<-read_excel("Dataset/Patents/Stock408.xlsx",sheet = "Results")
Stock409<-read_excel("Dataset/Patents/Stock409.xlsx",sheet = "Results")
Stock410<-read_excel("Dataset/Patents/Stock410.xlsx",sheet = "Results")
Stock411<-read_excel("Dataset/Patents/Stock411.xlsx",sheet = "Results")
Stock412<-read_excel("Dataset/Patents/Stock412.xlsx",sheet = "Results")
Stock413<-read_excel("Dataset/Patents/Stock413.xlsx",sheet = "Results")
Stock414<-read_excel("Dataset/Patents/Stock414.xlsx",sheet = "Results")
Stock415<-read_excel("Dataset/Patents/Stock415.xlsx",sheet = "Results")
Stock416<-read_excel("Dataset/Patents/Stock416.xlsx",sheet = "Results")
Stock417<-read_excel("Dataset/Patents/Stock417.xlsx",sheet = "Results")
Stock418<-read_excel("Dataset/Patents/Stock418.xlsx",sheet = "Results")
Stock419<-read_excel("Dataset/Patents/Stock419.xlsx",sheet = "Results")
Stock420<-read_excel("Dataset/Patents/Stock420.xlsx",sheet = "Results")
Stock421<-read_excel("Dataset/Patents/Stock421.xlsx",sheet = "Results")
Stock422<-read_excel("Dataset/Patents/Stock422.xlsx",sheet = "Results")
Stock423<-read_excel("Dataset/Patents/Stock423.xlsx",sheet = "Results")
Stock424<-read_excel("Dataset/Patents/Stock424.xlsx",sheet = "Results")
Stock425<-read_excel("Dataset/Patents/Stock425.xlsx",sheet = "Results")
Stock426<-read_excel("Dataset/Patents/Stock426.xlsx",sheet = "Results")
Stock427<-read_excel("Dataset/Patents/Stock427.xlsx",sheet = "Results")
Stock428<-read_excel("Dataset/Patents/Stock428.xlsx",sheet = "Results")
Stock429<-read_excel("Dataset/Patents/Stock429.xlsx",sheet = "Results")
Stock430<-read_excel("Dataset/Patents/Stock430.xlsx",sheet = "Results")
Stock431<-read_excel("Dataset/Patents/Stock431.xlsx",sheet = "Results")
Stock432<-read_excel("Dataset/Patents/Stock432.xlsx",sheet = "Results")
Stock433<-read_excel("Dataset/Patents/Stock433.xlsx",sheet = "Results")
Stock434<-read_excel("Dataset/Patents/Stock434.xlsx",sheet = "Results")
Stock435<-read_excel("Dataset/Patents/Stock435.xlsx",sheet = "Results")
Stock436<-read_excel("Dataset/Patents/Stock436.xlsx",sheet = "Results")
Stock437<-read_excel("Dataset/Patents/Stock437.xlsx",sheet = "Results")
Stock438<-read_excel("Dataset/Patents/Stock438.xlsx",sheet = "Results")
Stock439<-read_excel("Dataset/Patents/Stock439.xlsx",sheet = "Results")
Stock440<-read_excel("Dataset/Patents/Stock440.xlsx",sheet = "Results")
Stock441<-read_excel("Dataset/Patents/Stock441.xlsx",sheet = "Results")
Stock442<-read_excel("Dataset/Patents/Stock442.xlsx",sheet = "Results")
Stock443<-read_excel("Dataset/Patents/Stock443.xlsx",sheet = "Results")
Stock444<-read_excel("Dataset/Patents/Stock444.xlsx",sheet = "Results")
Stock445<-read_excel("Dataset/Patents/Stock445.xlsx",sheet = "Results")
Stock446<-read_excel("Dataset/Patents/Stock446.xlsx",sheet = "Results")
Stock447<-read_excel("Dataset/Patents/Stock447.xlsx",sheet = "Results")
Stock448<-read_excel("Dataset/Patents/Stock448.xlsx",sheet = "Results")
Stock449<-read_excel("Dataset/Patents/Stock449.xlsx",sheet = "Results")
Stock450<-read_excel("Dataset/Patents/Stock450.xlsx",sheet = "Results")
Stock451<-read_excel("Dataset/Patents/Stock451.xlsx",sheet = "Results")
Stock452<-read_excel("Dataset/Patents/Stock452.xlsx",sheet = "Results")
Stock453<-read_excel("Dataset/Patents/Stock453.xlsx",sheet = "Results")
Stock454<-read_excel("Dataset/Patents/Stock454.xlsx",sheet = "Results")
Stock455<-read_excel("Dataset/Patents/Stock455.xlsx",sheet = "Results")
Stock456<-read_excel("Dataset/Patents/Stock456.xlsx",sheet = "Results")
Stock457<-read_excel("Dataset/Patents/Stock457.xlsx",sheet = "Results")
Stock458<-read_excel("Dataset/Patents/Stock458.xlsx",sheet = "Results")
Stock459<-read_excel("Dataset/Patents/Stock459.xlsx",sheet = "Results")
Stock460<-read_excel("Dataset/Patents/Stock460.xlsx",sheet = "Results")
Stock461<-read_excel("Dataset/Patents/Stock461.xlsx",sheet = "Results")
Stock462<-read_excel("Dataset/Patents/Stock462.xlsx",sheet = "Results")
Stock463<-read_excel("Dataset/Patents/Stock463.xlsx",sheet = "Results")
Stock464<-read_excel("Dataset/Patents/Stock464.xlsx",sheet = "Results")
Stock465<-read_excel("Dataset/Patents/Stock465.xlsx",sheet = "Results")
Stock466<-read_excel("Dataset/Patents/Stock466.xlsx",sheet = "Results")
Stock467<-read_excel("Dataset/Patents/Stock467.xlsx",sheet = "Results")
Stock468<-read_excel("Dataset/Patents/Stock468.xlsx",sheet = "Results")
Stock469<-read_excel("Dataset/Patents/Stock469.xlsx",sheet = "Results")
Stock470<-read_excel("Dataset/Patents/Stock470.xlsx",sheet = "Results")
Stock471<-read_excel("Dataset/Patents/Stock471.xlsx",sheet = "Results")
Stock472<-read_excel("Dataset/Patents/Stock472.xlsx",sheet = "Results")
Stock473<-read_excel("Dataset/Patents/Stock473.xlsx",sheet = "Results")
Stock474<-read_excel("Dataset/Patents/Stock474.xlsx",sheet = "Results")
Stock475<-read_excel("Dataset/Patents/Stock475.xlsx",sheet = "Results")
Stock476<-read_excel("Dataset/Patents/Stock476.xlsx",sheet = "Results")
Stock477<-read_excel("Dataset/Patents/Stock477.xlsx",sheet = "Results")
Stock478<-read_excel("Dataset/Patents/Stock478.xlsx",sheet = "Results")
Stock479<-read_excel("Dataset/Patents/Stock479.xlsx",sheet = "Results")
Stock480<-read_excel("Dataset/Patents/Stock480.xlsx",sheet = "Results")
Stock481<-read_excel("Dataset/Patents/Stock481.xlsx",sheet = "Results")
Stock482<-read_excel("Dataset/Patents/Stock482.xlsx",sheet = "Results")
Stock483<-read_excel("Dataset/Patents/Stock483.xlsx",sheet = "Results")
Stock484<-read_excel("Dataset/Patents/Stock484.xlsx",sheet = "Results")
Stock485<-read_excel("Dataset/Patents/Stock485.xlsx",sheet = "Results")
Stock486<-read_excel("Dataset/Patents/Stock486.xlsx",sheet = "Results")
Stock487<-read_excel("Dataset/Patents/Stock487.xlsx",sheet = "Results")
Stock488<-read_excel("Dataset/Patents/Stock488.xlsx",sheet = "Results")
Stock489<-read_excel("Dataset/Patents/Stock489.xlsx",sheet = "Results")
Stock490<-read_excel("Dataset/Patents/Stock490.xlsx",sheet = "Results")
Stock491<-read_excel("Dataset/Patents/Stock491.xlsx",sheet = "Results")
Stock492<-read_excel("Dataset/Patents/Stock492.xlsx",sheet = "Results")
Stock493<-read_excel("Dataset/Patents/Stock493.xlsx",sheet = "Results")
Stock494<-read_excel("Dataset/Patents/Stock494.xlsx",sheet = "Results")
Stock495<-read_excel("Dataset/Patents/Stock495.xlsx",sheet = "Results")
Stock496<-read_excel("Dataset/Patents/Stock496.xlsx",sheet = "Results")
Stock497<-read_excel("Dataset/Patents/Stock497.xlsx",sheet = "Results")
Stock498<-read_excel("Dataset/Patents/Stock498.xlsx",sheet = "Results")
Stock499<-read_excel("Dataset/Patents/Stock499.xlsx",sheet = "Results")
Stock500<-read_excel("Dataset/Patents/Stock500.xlsx",sheet = "Results")
Stock501<-read_excel("Dataset/Patents/Stock501.xlsx",sheet = "Results")
Stock502<-read_excel("Dataset/Patents/Stock502.xlsx",sheet = "Results")
Stock503<-read_excel("Dataset/Patents/Stock503.xlsx",sheet = "Results")
Stock504<-read_excel("Dataset/Patents/Stock504.xlsx",sheet = "Results")
Stock505<-read_excel("Dataset/Patents/Stock505.xlsx",sheet = "Results")
Stock506<-read_excel("Dataset/Patents/Stock506.xlsx",sheet = "Results")
Stock507<-read_excel("Dataset/Patents/Stock507.xlsx",sheet = "Results")
Stock508<-read_excel("Dataset/Patents/Stock508.xlsx",sheet = "Results")
Stock509<-read_excel("Dataset/Patents/Stock509.xlsx",sheet = "Results")
Stock510<-read_excel("Dataset/Patents/Stock510.xlsx",sheet = "Results")
Stock511<-read_excel("Dataset/Patents/Stock511.xlsx",sheet = "Results")
Stock512<-read_excel("Dataset/Patents/Stock512.xlsx",sheet = "Results")
Stock513<-read_excel("Dataset/Patents/Stock513.xlsx",sheet = "Results")
Stock514<-read_excel("Dataset/Patents/Stock514.xlsx",sheet = "Results")
Stock515<-read_excel("Dataset/Patents/Stock515.xlsx",sheet = "Results")
Stock516<-read_excel("Dataset/Patents/Stock516.xlsx",sheet = "Results")
Stock517<-read_excel("Dataset/Patents/Stock517.xlsx",sheet = "Results")
Stock518<-read_excel("Dataset/Patents/Stock518.xlsx",sheet = "Results")
Stock519<-read_excel("Dataset/Patents/Stock519.xlsx",sheet = "Results")

Stock1$`Priority date`<-as.character(Stock1$`Priority date`)
Stock2$`Priority date`<-as.character(Stock2$`Priority date`)
Stock3$`Priority date`<-as.character(Stock3$`Priority date`)
Stock4$`Priority date`<-as.character(Stock4$`Priority date`)
Stock5$`Priority date`<-as.character(Stock5$`Priority date`)
Stock6$`Priority date`<-as.character(Stock6$`Priority date`)
Stock7$`Priority date`<-as.character(Stock7$`Priority date`)
Stock8$`Priority date`<-as.character(Stock8$`Priority date`)
Stock9$`Priority date`<-as.character(Stock9$`Priority date`)
Stock10$`Priority date`<-as.character(Stock10$`Priority date`)
Stock11$`Priority date`<-as.character(Stock11$`Priority date`)
Stock12$`Priority date`<-as.character(Stock12$`Priority date`)
Stock13$`Priority date`<-as.character(Stock13$`Priority date`)
Stock14$`Priority date`<-as.character(Stock14$`Priority date`)
Stock15$`Priority date`<-as.character(Stock15$`Priority date`)
Stock16$`Priority date`<-as.character(Stock16$`Priority date`)
Stock17$`Priority date`<-as.character(Stock17$`Priority date`)
Stock18$`Priority date`<-as.character(Stock18$`Priority date`)
Stock19$`Priority date`<-as.character(Stock19$`Priority date`)
Stock20$`Priority date`<-as.character(Stock20$`Priority date`)
Stock21$`Priority date`<-as.character(Stock21$`Priority date`)
Stock22$`Priority date`<-as.character(Stock22$`Priority date`)
Stock23$`Priority date`<-as.character(Stock23$`Priority date`)
Stock24$`Priority date`<-as.character(Stock24$`Priority date`)
Stock25$`Priority date`<-as.character(Stock25$`Priority date`)
Stock26$`Priority date`<-as.character(Stock26$`Priority date`)
Stock27$`Priority date`<-as.character(Stock27$`Priority date`)
Stock28$`Priority date`<-as.character(Stock28$`Priority date`)
Stock29$`Priority date`<-as.character(Stock29$`Priority date`)
Stock30$`Priority date`<-as.character(Stock30$`Priority date`)
Stock31$`Priority date`<-as.character(Stock31$`Priority date`)
Stock32$`Priority date`<-as.character(Stock32$`Priority date`)
Stock33$`Priority date`<-as.character(Stock33$`Priority date`)
Stock34$`Priority date`<-as.character(Stock34$`Priority date`)
Stock35$`Priority date`<-as.character(Stock35$`Priority date`)
Stock36$`Priority date`<-as.character(Stock36$`Priority date`)
Stock37$`Priority date`<-as.character(Stock37$`Priority date`)
Stock38$`Priority date`<-as.character(Stock38$`Priority date`)
Stock39$`Priority date`<-as.character(Stock39$`Priority date`)
Stock40$`Priority date`<-as.character(Stock40$`Priority date`)
Stock41$`Priority date`<-as.character(Stock41$`Priority date`)
Stock42$`Priority date`<-as.character(Stock42$`Priority date`)
Stock43$`Priority date`<-as.character(Stock43$`Priority date`)
Stock44$`Priority date`<-as.character(Stock44$`Priority date`)
Stock45$`Priority date`<-as.character(Stock45$`Priority date`)
Stock46$`Priority date`<-as.character(Stock46$`Priority date`)
Stock47$`Priority date`<-as.character(Stock47$`Priority date`)
Stock48$`Priority date`<-as.character(Stock48$`Priority date`)
Stock49$`Priority date`<-as.character(Stock49$`Priority date`)
Stock50$`Priority date`<-as.character(Stock50$`Priority date`)
Stock51$`Priority date`<-as.character(Stock51$`Priority date`)
Stock52$`Priority date`<-as.character(Stock52$`Priority date`)
Stock53$`Priority date`<-as.character(Stock53$`Priority date`)
Stock54$`Priority date`<-as.character(Stock54$`Priority date`)
Stock55$`Priority date`<-as.character(Stock55$`Priority date`)
Stock56$`Priority date`<-as.character(Stock56$`Priority date`)
Stock57$`Priority date`<-as.character(Stock57$`Priority date`)
Stock58$`Priority date`<-as.character(Stock58$`Priority date`)
Stock59$`Priority date`<-as.character(Stock59$`Priority date`)
Stock60$`Priority date`<-as.character(Stock60$`Priority date`)
Stock61$`Priority date`<-as.character(Stock61$`Priority date`)
Stock62$`Priority date`<-as.character(Stock62$`Priority date`)
Stock63$`Priority date`<-as.character(Stock63$`Priority date`)
Stock64$`Priority date`<-as.character(Stock64$`Priority date`)
Stock65$`Priority date`<-as.character(Stock65$`Priority date`)
Stock66$`Priority date`<-as.character(Stock66$`Priority date`)
Stock67$`Priority date`<-as.character(Stock67$`Priority date`)
Stock68$`Priority date`<-as.character(Stock68$`Priority date`)
Stock69$`Priority date`<-as.character(Stock69$`Priority date`)
Stock70$`Priority date`<-as.character(Stock70$`Priority date`)
Stock71$`Priority date`<-as.character(Stock71$`Priority date`)
Stock72$`Priority date`<-as.character(Stock72$`Priority date`)
Stock73$`Priority date`<-as.character(Stock73$`Priority date`)
Stock74$`Priority date`<-as.character(Stock74$`Priority date`)
Stock75$`Priority date`<-as.character(Stock75$`Priority date`)
Stock76$`Priority date`<-as.character(Stock76$`Priority date`)
Stock77$`Priority date`<-as.character(Stock77$`Priority date`)
Stock78$`Priority date`<-as.character(Stock78$`Priority date`)
Stock79$`Priority date`<-as.character(Stock79$`Priority date`)
Stock80$`Priority date`<-as.character(Stock80$`Priority date`)
Stock81$`Priority date`<-as.character(Stock81$`Priority date`)
Stock82$`Priority date`<-as.character(Stock82$`Priority date`)
Stock83$`Priority date`<-as.character(Stock83$`Priority date`)
Stock84$`Priority date`<-as.character(Stock84$`Priority date`)
Stock85$`Priority date`<-as.character(Stock85$`Priority date`)
Stock86$`Priority date`<-as.character(Stock86$`Priority date`)
Stock87$`Priority date`<-as.character(Stock87$`Priority date`)
Stock88$`Priority date`<-as.character(Stock88$`Priority date`)
Stock89$`Priority date`<-as.character(Stock89$`Priority date`)
Stock90$`Priority date`<-as.character(Stock90$`Priority date`)
Stock91$`Priority date`<-as.character(Stock91$`Priority date`)
Stock92$`Priority date`<-as.character(Stock92$`Priority date`)
Stock93$`Priority date`<-as.character(Stock93$`Priority date`)
Stock94$`Priority date`<-as.character(Stock94$`Priority date`)
Stock95$`Priority date`<-as.character(Stock95$`Priority date`)
Stock96$`Priority date`<-as.character(Stock96$`Priority date`)
Stock97$`Priority date`<-as.character(Stock97$`Priority date`)
Stock98$`Priority date`<-as.character(Stock98$`Priority date`)
Stock99$`Priority date`<-as.character(Stock99$`Priority date`)
Stock100$`Priority date`<-as.character(Stock100$`Priority date`)
Stock101$`Priority date`<-as.character(Stock101$`Priority date`)
Stock102$`Priority date`<-as.character(Stock102$`Priority date`)
Stock103$`Priority date`<-as.character(Stock103$`Priority date`)
Stock104$`Priority date`<-as.character(Stock104$`Priority date`)
Stock105$`Priority date`<-as.character(Stock105$`Priority date`)
Stock106$`Priority date`<-as.character(Stock106$`Priority date`)
Stock107$`Priority date`<-as.character(Stock107$`Priority date`)
Stock108$`Priority date`<-as.character(Stock108$`Priority date`)
Stock109$`Priority date`<-as.character(Stock109$`Priority date`)
Stock110$`Priority date`<-as.character(Stock110$`Priority date`)
Stock111$`Priority date`<-as.character(Stock111$`Priority date`)
Stock112$`Priority date`<-as.character(Stock112$`Priority date`)
Stock113$`Priority date`<-as.character(Stock113$`Priority date`)
Stock114$`Priority date`<-as.character(Stock114$`Priority date`)
Stock115$`Priority date`<-as.character(Stock115$`Priority date`)
Stock116$`Priority date`<-as.character(Stock116$`Priority date`)
Stock117$`Priority date`<-as.character(Stock117$`Priority date`)
Stock118$`Priority date`<-as.character(Stock118$`Priority date`)
Stock119$`Priority date`<-as.character(Stock119$`Priority date`)
Stock120$`Priority date`<-as.character(Stock120$`Priority date`)
Stock121$`Priority date`<-as.character(Stock121$`Priority date`)
Stock122$`Priority date`<-as.character(Stock122$`Priority date`)
Stock123$`Priority date`<-as.character(Stock123$`Priority date`)
Stock124$`Priority date`<-as.character(Stock124$`Priority date`)
Stock125$`Priority date`<-as.character(Stock125$`Priority date`)
Stock126$`Priority date`<-as.character(Stock126$`Priority date`)
Stock127$`Priority date`<-as.character(Stock127$`Priority date`)
Stock128$`Priority date`<-as.character(Stock128$`Priority date`)
Stock129$`Priority date`<-as.character(Stock129$`Priority date`)
Stock130$`Priority date`<-as.character(Stock130$`Priority date`)
Stock131$`Priority date`<-as.character(Stock131$`Priority date`)
Stock132$`Priority date`<-as.character(Stock132$`Priority date`)
Stock133$`Priority date`<-as.character(Stock133$`Priority date`)
Stock134$`Priority date`<-as.character(Stock134$`Priority date`)
Stock135$`Priority date`<-as.character(Stock135$`Priority date`)
Stock136$`Priority date`<-as.character(Stock136$`Priority date`)
Stock137$`Priority date`<-as.character(Stock137$`Priority date`)
Stock138$`Priority date`<-as.character(Stock138$`Priority date`)
Stock139$`Priority date`<-as.character(Stock139$`Priority date`)
Stock140$`Priority date`<-as.character(Stock140$`Priority date`)
Stock141$`Priority date`<-as.character(Stock141$`Priority date`)
Stock142$`Priority date`<-as.character(Stock142$`Priority date`)
Stock143$`Priority date`<-as.character(Stock143$`Priority date`)
Stock144$`Priority date`<-as.character(Stock144$`Priority date`)
Stock145$`Priority date`<-as.character(Stock145$`Priority date`)
Stock146$`Priority date`<-as.character(Stock146$`Priority date`)
Stock147$`Priority date`<-as.character(Stock147$`Priority date`)
Stock148$`Priority date`<-as.character(Stock148$`Priority date`)
Stock149$`Priority date`<-as.character(Stock149$`Priority date`)
Stock150$`Priority date`<-as.character(Stock150$`Priority date`)
Stock151$`Priority date`<-as.character(Stock151$`Priority date`)
Stock152$`Priority date`<-as.character(Stock152$`Priority date`)
Stock153$`Priority date`<-as.character(Stock153$`Priority date`)
Stock154$`Priority date`<-as.character(Stock154$`Priority date`)
Stock155$`Priority date`<-as.character(Stock155$`Priority date`)
Stock156$`Priority date`<-as.character(Stock156$`Priority date`)
Stock157$`Priority date`<-as.character(Stock157$`Priority date`)
Stock158$`Priority date`<-as.character(Stock158$`Priority date`)
Stock159$`Priority date`<-as.character(Stock159$`Priority date`)
Stock160$`Priority date`<-as.character(Stock160$`Priority date`)
Stock161$`Priority date`<-as.character(Stock161$`Priority date`)
Stock162$`Priority date`<-as.character(Stock162$`Priority date`)
Stock163$`Priority date`<-as.character(Stock163$`Priority date`)
Stock164$`Priority date`<-as.character(Stock164$`Priority date`)
Stock165$`Priority date`<-as.character(Stock165$`Priority date`)
Stock166$`Priority date`<-as.character(Stock166$`Priority date`)
Stock167$`Priority date`<-as.character(Stock167$`Priority date`)
Stock168$`Priority date`<-as.character(Stock168$`Priority date`)
Stock169$`Priority date`<-as.character(Stock169$`Priority date`)
Stock170$`Priority date`<-as.character(Stock170$`Priority date`)
Stock171$`Priority date`<-as.character(Stock171$`Priority date`)
Stock172$`Priority date`<-as.character(Stock172$`Priority date`)
Stock173$`Priority date`<-as.character(Stock173$`Priority date`)
Stock174$`Priority date`<-as.character(Stock174$`Priority date`)
Stock175$`Priority date`<-as.character(Stock175$`Priority date`)
Stock176$`Priority date`<-as.character(Stock176$`Priority date`)
Stock177$`Priority date`<-as.character(Stock177$`Priority date`)
Stock178$`Priority date`<-as.character(Stock178$`Priority date`)
Stock179$`Priority date`<-as.character(Stock179$`Priority date`)
Stock180$`Priority date`<-as.character(Stock180$`Priority date`)
Stock181$`Priority date`<-as.character(Stock181$`Priority date`)
Stock182$`Priority date`<-as.character(Stock182$`Priority date`)
Stock183$`Priority date`<-as.character(Stock183$`Priority date`)
Stock184$`Priority date`<-as.character(Stock184$`Priority date`)
Stock185$`Priority date`<-as.character(Stock185$`Priority date`)
Stock186$`Priority date`<-as.character(Stock186$`Priority date`)
Stock187$`Priority date`<-as.character(Stock187$`Priority date`)
Stock188$`Priority date`<-as.character(Stock188$`Priority date`)
Stock189$`Priority date`<-as.character(Stock189$`Priority date`)
Stock190$`Priority date`<-as.character(Stock190$`Priority date`)
Stock191$`Priority date`<-as.character(Stock191$`Priority date`)
Stock192$`Priority date`<-as.character(Stock192$`Priority date`)
Stock193$`Priority date`<-as.character(Stock193$`Priority date`)
Stock194$`Priority date`<-as.character(Stock194$`Priority date`)
Stock195$`Priority date`<-as.character(Stock195$`Priority date`)
Stock196$`Priority date`<-as.character(Stock196$`Priority date`)
Stock197$`Priority date`<-as.character(Stock197$`Priority date`)
Stock198$`Priority date`<-as.character(Stock198$`Priority date`)
Stock199$`Priority date`<-as.character(Stock199$`Priority date`)
Stock200$`Priority date`<-as.character(Stock200$`Priority date`)
Stock201$`Priority date`<-as.character(Stock201$`Priority date`)
Stock202$`Priority date`<-as.character(Stock202$`Priority date`)
Stock203$`Priority date`<-as.character(Stock203$`Priority date`)
Stock204$`Priority date`<-as.character(Stock204$`Priority date`)
Stock205$`Priority date`<-as.character(Stock205$`Priority date`)
Stock206$`Priority date`<-as.character(Stock206$`Priority date`)
Stock207$`Priority date`<-as.character(Stock207$`Priority date`)
Stock208$`Priority date`<-as.character(Stock208$`Priority date`)
Stock209$`Priority date`<-as.character(Stock209$`Priority date`)
Stock210$`Priority date`<-as.character(Stock210$`Priority date`)
Stock211$`Priority date`<-as.character(Stock211$`Priority date`)
Stock212$`Priority date`<-as.character(Stock212$`Priority date`)
Stock213$`Priority date`<-as.character(Stock213$`Priority date`)
Stock214$`Priority date`<-as.character(Stock214$`Priority date`)
Stock215$`Priority date`<-as.character(Stock215$`Priority date`)
Stock216$`Priority date`<-as.character(Stock216$`Priority date`)
Stock217$`Priority date`<-as.character(Stock217$`Priority date`)
Stock218$`Priority date`<-as.character(Stock218$`Priority date`)
Stock219$`Priority date`<-as.character(Stock219$`Priority date`)
Stock220$`Priority date`<-as.character(Stock220$`Priority date`)
Stock221$`Priority date`<-as.character(Stock221$`Priority date`)
Stock222$`Priority date`<-as.character(Stock222$`Priority date`)
Stock223$`Priority date`<-as.character(Stock223$`Priority date`)
Stock224$`Priority date`<-as.character(Stock224$`Priority date`)
Stock225$`Priority date`<-as.character(Stock225$`Priority date`)
Stock226$`Priority date`<-as.character(Stock226$`Priority date`)
Stock227$`Priority date`<-as.character(Stock227$`Priority date`)
Stock228$`Priority date`<-as.character(Stock228$`Priority date`)
Stock229$`Priority date`<-as.character(Stock229$`Priority date`)
Stock230$`Priority date`<-as.character(Stock230$`Priority date`)
Stock231$`Priority date`<-as.character(Stock231$`Priority date`)
Stock232$`Priority date`<-as.character(Stock232$`Priority date`)
Stock233$`Priority date`<-as.character(Stock233$`Priority date`)
Stock234$`Priority date`<-as.character(Stock234$`Priority date`)
Stock235$`Priority date`<-as.character(Stock235$`Priority date`)
Stock236$`Priority date`<-as.character(Stock236$`Priority date`)
Stock237$`Priority date`<-as.character(Stock237$`Priority date`)
Stock238$`Priority date`<-as.character(Stock238$`Priority date`)
Stock239$`Priority date`<-as.character(Stock239$`Priority date`)
Stock240$`Priority date`<-as.character(Stock240$`Priority date`)
Stock241$`Priority date`<-as.character(Stock241$`Priority date`)
Stock242$`Priority date`<-as.character(Stock242$`Priority date`)
Stock243$`Priority date`<-as.character(Stock243$`Priority date`)
Stock244$`Priority date`<-as.character(Stock244$`Priority date`)
Stock245$`Priority date`<-as.character(Stock245$`Priority date`)
Stock246$`Priority date`<-as.character(Stock246$`Priority date`)
Stock247$`Priority date`<-as.character(Stock247$`Priority date`)
Stock248$`Priority date`<-as.character(Stock248$`Priority date`)
Stock249$`Priority date`<-as.character(Stock249$`Priority date`)
Stock250$`Priority date`<-as.character(Stock250$`Priority date`)
Stock251$`Priority date`<-as.character(Stock251$`Priority date`)
Stock252$`Priority date`<-as.character(Stock252$`Priority date`)
Stock253$`Priority date`<-as.character(Stock253$`Priority date`)
Stock254$`Priority date`<-as.character(Stock254$`Priority date`)
Stock255$`Priority date`<-as.character(Stock255$`Priority date`)
Stock256$`Priority date`<-as.character(Stock256$`Priority date`)
Stock257$`Priority date`<-as.character(Stock257$`Priority date`)
Stock258$`Priority date`<-as.character(Stock258$`Priority date`)
Stock259$`Priority date`<-as.character(Stock259$`Priority date`)
Stock260$`Priority date`<-as.character(Stock260$`Priority date`)
Stock261$`Priority date`<-as.character(Stock261$`Priority date`)
Stock262$`Priority date`<-as.character(Stock262$`Priority date`)
Stock263$`Priority date`<-as.character(Stock263$`Priority date`)
Stock264$`Priority date`<-as.character(Stock264$`Priority date`)
Stock265$`Priority date`<-as.character(Stock265$`Priority date`)
Stock266$`Priority date`<-as.character(Stock266$`Priority date`)
Stock267$`Priority date`<-as.character(Stock267$`Priority date`)
Stock268$`Priority date`<-as.character(Stock268$`Priority date`)
Stock269$`Priority date`<-as.character(Stock269$`Priority date`)
Stock270$`Priority date`<-as.character(Stock270$`Priority date`)
Stock271$`Priority date`<-as.character(Stock271$`Priority date`)
Stock272$`Priority date`<-as.character(Stock272$`Priority date`)
Stock273$`Priority date`<-as.character(Stock273$`Priority date`)
Stock274$`Priority date`<-as.character(Stock274$`Priority date`)
Stock275$`Priority date`<-as.character(Stock275$`Priority date`)
Stock276$`Priority date`<-as.character(Stock276$`Priority date`)
Stock277$`Priority date`<-as.character(Stock277$`Priority date`)
Stock278$`Priority date`<-as.character(Stock278$`Priority date`)
Stock279$`Priority date`<-as.character(Stock279$`Priority date`)
Stock280$`Priority date`<-as.character(Stock280$`Priority date`)
Stock281$`Priority date`<-as.character(Stock281$`Priority date`)
Stock282$`Priority date`<-as.character(Stock282$`Priority date`)
Stock283$`Priority date`<-as.character(Stock283$`Priority date`)
Stock284$`Priority date`<-as.character(Stock284$`Priority date`)
Stock285$`Priority date`<-as.character(Stock285$`Priority date`)
Stock286$`Priority date`<-as.character(Stock286$`Priority date`)
Stock287$`Priority date`<-as.character(Stock287$`Priority date`)
Stock288$`Priority date`<-as.character(Stock288$`Priority date`)
Stock289$`Priority date`<-as.character(Stock289$`Priority date`)
Stock290$`Priority date`<-as.character(Stock290$`Priority date`)
Stock291$`Priority date`<-as.character(Stock291$`Priority date`)
Stock292$`Priority date`<-as.character(Stock292$`Priority date`)
Stock293$`Priority date`<-as.character(Stock293$`Priority date`)
Stock294$`Priority date`<-as.character(Stock294$`Priority date`)
Stock295$`Priority date`<-as.character(Stock295$`Priority date`)
Stock296$`Priority date`<-as.character(Stock296$`Priority date`)
Stock297$`Priority date`<-as.character(Stock297$`Priority date`)
Stock298$`Priority date`<-as.character(Stock298$`Priority date`)
Stock299$`Priority date`<-as.character(Stock299$`Priority date`)
Stock300$`Priority date`<-as.character(Stock300$`Priority date`)
Stock301$`Priority date`<-as.character(Stock301$`Priority date`)
Stock302$`Priority date`<-as.character(Stock302$`Priority date`)
Stock303$`Priority date`<-as.character(Stock303$`Priority date`)
Stock304$`Priority date`<-as.character(Stock304$`Priority date`)
Stock305$`Priority date`<-as.character(Stock305$`Priority date`)
Stock306$`Priority date`<-as.character(Stock306$`Priority date`)
Stock307$`Priority date`<-as.character(Stock307$`Priority date`)
Stock308$`Priority date`<-as.character(Stock308$`Priority date`)
Stock309$`Priority date`<-as.character(Stock309$`Priority date`)
Stock310$`Priority date`<-as.character(Stock310$`Priority date`)
Stock311$`Priority date`<-as.character(Stock311$`Priority date`)
Stock312$`Priority date`<-as.character(Stock312$`Priority date`)
Stock313$`Priority date`<-as.character(Stock313$`Priority date`)
Stock314$`Priority date`<-as.character(Stock314$`Priority date`)
Stock315$`Priority date`<-as.character(Stock315$`Priority date`)
Stock316$`Priority date`<-as.character(Stock316$`Priority date`)
Stock317$`Priority date`<-as.character(Stock317$`Priority date`)
Stock318$`Priority date`<-as.character(Stock318$`Priority date`)
Stock319$`Priority date`<-as.character(Stock319$`Priority date`)
Stock320$`Priority date`<-as.character(Stock320$`Priority date`)
Stock321$`Priority date`<-as.character(Stock321$`Priority date`)
Stock322$`Priority date`<-as.character(Stock322$`Priority date`)
Stock323$`Priority date`<-as.character(Stock323$`Priority date`)
Stock324$`Priority date`<-as.character(Stock324$`Priority date`)
Stock325$`Priority date`<-as.character(Stock325$`Priority date`)
Stock326$`Priority date`<-as.character(Stock326$`Priority date`)
Stock327$`Priority date`<-as.character(Stock327$`Priority date`)
Stock328$`Priority date`<-as.character(Stock328$`Priority date`)
Stock329$`Priority date`<-as.character(Stock329$`Priority date`)
Stock330$`Priority date`<-as.character(Stock330$`Priority date`)
Stock331$`Priority date`<-as.character(Stock331$`Priority date`)
Stock332$`Priority date`<-as.character(Stock332$`Priority date`)
Stock333$`Priority date`<-as.character(Stock333$`Priority date`)
Stock334$`Priority date`<-as.character(Stock334$`Priority date`)
Stock335$`Priority date`<-as.character(Stock335$`Priority date`)
Stock336$`Priority date`<-as.character(Stock336$`Priority date`)
Stock337$`Priority date`<-as.character(Stock337$`Priority date`)
Stock338$`Priority date`<-as.character(Stock338$`Priority date`)
Stock339$`Priority date`<-as.character(Stock339$`Priority date`)
Stock340$`Priority date`<-as.character(Stock340$`Priority date`)
Stock341$`Priority date`<-as.character(Stock341$`Priority date`)
Stock342$`Priority date`<-as.character(Stock342$`Priority date`)
Stock343$`Priority date`<-as.character(Stock343$`Priority date`)
Stock344$`Priority date`<-as.character(Stock344$`Priority date`)
Stock345$`Priority date`<-as.character(Stock345$`Priority date`)
Stock346$`Priority date`<-as.character(Stock346$`Priority date`)
Stock347$`Priority date`<-as.character(Stock347$`Priority date`)
Stock348$`Priority date`<-as.character(Stock348$`Priority date`)
Stock349$`Priority date`<-as.character(Stock349$`Priority date`)
Stock350$`Priority date`<-as.character(Stock350$`Priority date`)
Stock351$`Priority date`<-as.character(Stock351$`Priority date`)
Stock352$`Priority date`<-as.character(Stock352$`Priority date`)
Stock353$`Priority date`<-as.character(Stock353$`Priority date`)
Stock354$`Priority date`<-as.character(Stock354$`Priority date`)
Stock355$`Priority date`<-as.character(Stock355$`Priority date`)
Stock356$`Priority date`<-as.character(Stock356$`Priority date`)
Stock357$`Priority date`<-as.character(Stock357$`Priority date`)
Stock358$`Priority date`<-as.character(Stock358$`Priority date`)
Stock359$`Priority date`<-as.character(Stock359$`Priority date`)
Stock360$`Priority date`<-as.character(Stock360$`Priority date`)
Stock361$`Priority date`<-as.character(Stock361$`Priority date`)
Stock362$`Priority date`<-as.character(Stock362$`Priority date`)
Stock363$`Priority date`<-as.character(Stock363$`Priority date`)
Stock364$`Priority date`<-as.character(Stock364$`Priority date`)
Stock365$`Priority date`<-as.character(Stock365$`Priority date`)
Stock366$`Priority date`<-as.character(Stock366$`Priority date`)
Stock367$`Priority date`<-as.character(Stock367$`Priority date`)
Stock368$`Priority date`<-as.character(Stock368$`Priority date`)
Stock369$`Priority date`<-as.character(Stock369$`Priority date`)
Stock370$`Priority date`<-as.character(Stock370$`Priority date`)
Stock371$`Priority date`<-as.character(Stock371$`Priority date`)
Stock372$`Priority date`<-as.character(Stock372$`Priority date`)
Stock373$`Priority date`<-as.character(Stock373$`Priority date`)
Stock374$`Priority date`<-as.character(Stock374$`Priority date`)
Stock375$`Priority date`<-as.character(Stock375$`Priority date`)
Stock376$`Priority date`<-as.character(Stock376$`Priority date`)
Stock377$`Priority date`<-as.character(Stock377$`Priority date`)
Stock378$`Priority date`<-as.character(Stock378$`Priority date`)
Stock379$`Priority date`<-as.character(Stock379$`Priority date`)
Stock380$`Priority date`<-as.character(Stock380$`Priority date`)
Stock381$`Priority date`<-as.character(Stock381$`Priority date`)
Stock382$`Priority date`<-as.character(Stock382$`Priority date`)
Stock383$`Priority date`<-as.character(Stock383$`Priority date`)
Stock384$`Priority date`<-as.character(Stock384$`Priority date`)
Stock385$`Priority date`<-as.character(Stock385$`Priority date`)
Stock386$`Priority date`<-as.character(Stock386$`Priority date`)
Stock387$`Priority date`<-as.character(Stock387$`Priority date`)
Stock388$`Priority date`<-as.character(Stock388$`Priority date`)
Stock389$`Priority date`<-as.character(Stock389$`Priority date`)
Stock390$`Priority date`<-as.character(Stock390$`Priority date`)
Stock391$`Priority date`<-as.character(Stock391$`Priority date`)
Stock392$`Priority date`<-as.character(Stock392$`Priority date`)
Stock393$`Priority date`<-as.character(Stock393$`Priority date`)
Stock394$`Priority date`<-as.character(Stock394$`Priority date`)
Stock395$`Priority date`<-as.character(Stock395$`Priority date`)
Stock396$`Priority date`<-as.character(Stock396$`Priority date`)
Stock397$`Priority date`<-as.character(Stock397$`Priority date`)
Stock398$`Priority date`<-as.character(Stock398$`Priority date`)
Stock399$`Priority date`<-as.character(Stock399$`Priority date`)
Stock400$`Priority date`<-as.character(Stock400$`Priority date`)
Stock401$`Priority date`<-as.character(Stock401$`Priority date`)
Stock402$`Priority date`<-as.character(Stock402$`Priority date`)
Stock403$`Priority date`<-as.character(Stock403$`Priority date`)
Stock404$`Priority date`<-as.character(Stock404$`Priority date`)
Stock405$`Priority date`<-as.character(Stock405$`Priority date`)
Stock406$`Priority date`<-as.character(Stock406$`Priority date`)
Stock407$`Priority date`<-as.character(Stock407$`Priority date`)
Stock408$`Priority date`<-as.character(Stock408$`Priority date`)
Stock409$`Priority date`<-as.character(Stock409$`Priority date`)
Stock410$`Priority date`<-as.character(Stock410$`Priority date`)
Stock411$`Priority date`<-as.character(Stock411$`Priority date`)
Stock412$`Priority date`<-as.character(Stock412$`Priority date`)
Stock413$`Priority date`<-as.character(Stock413$`Priority date`)
Stock414$`Priority date`<-as.character(Stock414$`Priority date`)
Stock415$`Priority date`<-as.character(Stock415$`Priority date`)
Stock416$`Priority date`<-as.character(Stock416$`Priority date`)
Stock417$`Priority date`<-as.character(Stock417$`Priority date`)
Stock418$`Priority date`<-as.character(Stock418$`Priority date`)
Stock419$`Priority date`<-as.character(Stock419$`Priority date`)
Stock420$`Priority date`<-as.character(Stock420$`Priority date`)
Stock421$`Priority date`<-as.character(Stock421$`Priority date`)
Stock422$`Priority date`<-as.character(Stock422$`Priority date`)
Stock423$`Priority date`<-as.character(Stock423$`Priority date`)
Stock424$`Priority date`<-as.character(Stock424$`Priority date`)
Stock425$`Priority date`<-as.character(Stock425$`Priority date`)
Stock426$`Priority date`<-as.character(Stock426$`Priority date`)
Stock427$`Priority date`<-as.character(Stock427$`Priority date`)
Stock428$`Priority date`<-as.character(Stock428$`Priority date`)
Stock429$`Priority date`<-as.character(Stock429$`Priority date`)
Stock430$`Priority date`<-as.character(Stock430$`Priority date`)
Stock431$`Priority date`<-as.character(Stock431$`Priority date`)
Stock432$`Priority date`<-as.character(Stock432$`Priority date`)
Stock433$`Priority date`<-as.character(Stock433$`Priority date`)
Stock434$`Priority date`<-as.character(Stock434$`Priority date`)
Stock435$`Priority date`<-as.character(Stock435$`Priority date`)
Stock436$`Priority date`<-as.character(Stock436$`Priority date`)
Stock437$`Priority date`<-as.character(Stock437$`Priority date`)
Stock438$`Priority date`<-as.character(Stock438$`Priority date`)
Stock439$`Priority date`<-as.character(Stock439$`Priority date`)
Stock440$`Priority date`<-as.character(Stock440$`Priority date`)
Stock441$`Priority date`<-as.character(Stock441$`Priority date`)
Stock442$`Priority date`<-as.character(Stock442$`Priority date`)
Stock443$`Priority date`<-as.character(Stock443$`Priority date`)
Stock444$`Priority date`<-as.character(Stock444$`Priority date`)
Stock445$`Priority date`<-as.character(Stock445$`Priority date`)
Stock446$`Priority date`<-as.character(Stock446$`Priority date`)
Stock447$`Priority date`<-as.character(Stock447$`Priority date`)
Stock448$`Priority date`<-as.character(Stock448$`Priority date`)
Stock449$`Priority date`<-as.character(Stock449$`Priority date`)
Stock450$`Priority date`<-as.character(Stock450$`Priority date`)
Stock451$`Priority date`<-as.character(Stock451$`Priority date`)
Stock452$`Priority date`<-as.character(Stock452$`Priority date`)
Stock453$`Priority date`<-as.character(Stock453$`Priority date`)
Stock454$`Priority date`<-as.character(Stock454$`Priority date`)
Stock455$`Priority date`<-as.character(Stock455$`Priority date`)
Stock456$`Priority date`<-as.character(Stock456$`Priority date`)
Stock457$`Priority date`<-as.character(Stock457$`Priority date`)
Stock458$`Priority date`<-as.character(Stock458$`Priority date`)
Stock459$`Priority date`<-as.character(Stock459$`Priority date`)
Stock460$`Priority date`<-as.character(Stock460$`Priority date`)
Stock461$`Priority date`<-as.character(Stock461$`Priority date`)
Stock462$`Priority date`<-as.character(Stock462$`Priority date`)
Stock463$`Priority date`<-as.character(Stock463$`Priority date`)
Stock464$`Priority date`<-as.character(Stock464$`Priority date`)
Stock465$`Priority date`<-as.character(Stock465$`Priority date`)
Stock466$`Priority date`<-as.character(Stock466$`Priority date`)
Stock467$`Priority date`<-as.character(Stock467$`Priority date`)
Stock468$`Priority date`<-as.character(Stock468$`Priority date`)
Stock469$`Priority date`<-as.character(Stock469$`Priority date`)
Stock470$`Priority date`<-as.character(Stock470$`Priority date`)
Stock471$`Priority date`<-as.character(Stock471$`Priority date`)
Stock472$`Priority date`<-as.character(Stock472$`Priority date`)
Stock473$`Priority date`<-as.character(Stock473$`Priority date`)
Stock474$`Priority date`<-as.character(Stock474$`Priority date`)
Stock475$`Priority date`<-as.character(Stock475$`Priority date`)
Stock476$`Priority date`<-as.character(Stock476$`Priority date`)
Stock477$`Priority date`<-as.character(Stock477$`Priority date`)
Stock478$`Priority date`<-as.character(Stock478$`Priority date`)
Stock479$`Priority date`<-as.character(Stock479$`Priority date`)
Stock480$`Priority date`<-as.character(Stock480$`Priority date`)
Stock481$`Priority date`<-as.character(Stock481$`Priority date`)
Stock482$`Priority date`<-as.character(Stock482$`Priority date`)
Stock483$`Priority date`<-as.character(Stock483$`Priority date`)
Stock484$`Priority date`<-as.character(Stock484$`Priority date`)
Stock485$`Priority date`<-as.character(Stock485$`Priority date`)
Stock486$`Priority date`<-as.character(Stock486$`Priority date`)
Stock487$`Priority date`<-as.character(Stock487$`Priority date`)
Stock488$`Priority date`<-as.character(Stock488$`Priority date`)
Stock489$`Priority date`<-as.character(Stock489$`Priority date`)
Stock490$`Priority date`<-as.character(Stock490$`Priority date`)
Stock491$`Priority date`<-as.character(Stock491$`Priority date`)
Stock492$`Priority date`<-as.character(Stock492$`Priority date`)
Stock493$`Priority date`<-as.character(Stock493$`Priority date`)
Stock494$`Priority date`<-as.character(Stock494$`Priority date`)
Stock495$`Priority date`<-as.character(Stock495$`Priority date`)
Stock496$`Priority date`<-as.character(Stock496$`Priority date`)
Stock497$`Priority date`<-as.character(Stock497$`Priority date`)
Stock498$`Priority date`<-as.character(Stock498$`Priority date`)
Stock499$`Priority date`<-as.character(Stock499$`Priority date`)
Stock500$`Priority date`<-as.character(Stock500$`Priority date`)
Stock501$`Priority date`<-as.character(Stock501$`Priority date`)
Stock502$`Priority date`<-as.character(Stock502$`Priority date`)
Stock503$`Priority date`<-as.character(Stock503$`Priority date`)
Stock504$`Priority date`<-as.character(Stock504$`Priority date`)
Stock505$`Priority date`<-as.character(Stock505$`Priority date`)
Stock506$`Priority date`<-as.character(Stock506$`Priority date`)
Stock507$`Priority date`<-as.character(Stock507$`Priority date`)
Stock508$`Priority date`<-as.character(Stock508$`Priority date`)
Stock509$`Priority date`<-as.character(Stock509$`Priority date`)
Stock510$`Priority date`<-as.character(Stock510$`Priority date`)
Stock511$`Priority date`<-as.character(Stock511$`Priority date`)
Stock512$`Priority date`<-as.character(Stock512$`Priority date`)
Stock513$`Priority date`<-as.character(Stock513$`Priority date`)
Stock514$`Priority date`<-as.character(Stock514$`Priority date`)
Stock515$`Priority date`<-as.character(Stock515$`Priority date`)
Stock516$`Priority date`<-as.character(Stock516$`Priority date`)
Stock517$`Priority date`<-as.character(Stock517$`Priority date`)
Stock518$`Priority date`<-as.character(Stock518$`Priority date`)
Stock519$`Priority date`<-as.character(Stock519$`Priority date`)

Stock1$`Expiration date`<-as.character(Stock1$`Expiration date`)
Stock2$`Expiration date`<-as.character(Stock2$`Expiration date`)
Stock3$`Expiration date`<-as.character(Stock3$`Expiration date`)
Stock4$`Expiration date`<-as.character(Stock4$`Expiration date`)
Stock5$`Expiration date`<-as.character(Stock5$`Expiration date`)
Stock6$`Expiration date`<-as.character(Stock6$`Expiration date`)
Stock7$`Expiration date`<-as.character(Stock7$`Expiration date`)
Stock8$`Expiration date`<-as.character(Stock8$`Expiration date`)
Stock9$`Expiration date`<-as.character(Stock9$`Expiration date`)
Stock10$`Expiration date`<-as.character(Stock10$`Expiration date`)
Stock11$`Expiration date`<-as.character(Stock11$`Expiration date`)
Stock12$`Expiration date`<-as.character(Stock12$`Expiration date`)
Stock13$`Expiration date`<-as.character(Stock13$`Expiration date`)
Stock14$`Expiration date`<-as.character(Stock14$`Expiration date`)
Stock15$`Expiration date`<-as.character(Stock15$`Expiration date`)
Stock16$`Expiration date`<-as.character(Stock16$`Expiration date`)
Stock17$`Expiration date`<-as.character(Stock17$`Expiration date`)
Stock18$`Expiration date`<-as.character(Stock18$`Expiration date`)
Stock19$`Expiration date`<-as.character(Stock19$`Expiration date`)
Stock20$`Expiration date`<-as.character(Stock20$`Expiration date`)
Stock21$`Expiration date`<-as.character(Stock21$`Expiration date`)
Stock22$`Expiration date`<-as.character(Stock22$`Expiration date`)
Stock23$`Expiration date`<-as.character(Stock23$`Expiration date`)
Stock24$`Expiration date`<-as.character(Stock24$`Expiration date`)
Stock25$`Expiration date`<-as.character(Stock25$`Expiration date`)
Stock26$`Expiration date`<-as.character(Stock26$`Expiration date`)
Stock27$`Expiration date`<-as.character(Stock27$`Expiration date`)
Stock28$`Expiration date`<-as.character(Stock28$`Expiration date`)
Stock29$`Expiration date`<-as.character(Stock29$`Expiration date`)
Stock30$`Expiration date`<-as.character(Stock30$`Expiration date`)
Stock31$`Expiration date`<-as.character(Stock31$`Expiration date`)
Stock32$`Expiration date`<-as.character(Stock32$`Expiration date`)
Stock33$`Expiration date`<-as.character(Stock33$`Expiration date`)
Stock34$`Expiration date`<-as.character(Stock34$`Expiration date`)
Stock35$`Expiration date`<-as.character(Stock35$`Expiration date`)
Stock36$`Expiration date`<-as.character(Stock36$`Expiration date`)
Stock37$`Expiration date`<-as.character(Stock37$`Expiration date`)
Stock38$`Expiration date`<-as.character(Stock38$`Expiration date`)
Stock39$`Expiration date`<-as.character(Stock39$`Expiration date`)
Stock40$`Expiration date`<-as.character(Stock40$`Expiration date`)
Stock41$`Expiration date`<-as.character(Stock41$`Expiration date`)
Stock42$`Expiration date`<-as.character(Stock42$`Expiration date`)
Stock43$`Expiration date`<-as.character(Stock43$`Expiration date`)
Stock44$`Expiration date`<-as.character(Stock44$`Expiration date`)
Stock45$`Expiration date`<-as.character(Stock45$`Expiration date`)
Stock46$`Expiration date`<-as.character(Stock46$`Expiration date`)
Stock47$`Expiration date`<-as.character(Stock47$`Expiration date`)
Stock48$`Expiration date`<-as.character(Stock48$`Expiration date`)
Stock49$`Expiration date`<-as.character(Stock49$`Expiration date`)
Stock50$`Expiration date`<-as.character(Stock50$`Expiration date`)
Stock51$`Expiration date`<-as.character(Stock51$`Expiration date`)
Stock52$`Expiration date`<-as.character(Stock52$`Expiration date`)
Stock53$`Expiration date`<-as.character(Stock53$`Expiration date`)
Stock54$`Expiration date`<-as.character(Stock54$`Expiration date`)
Stock55$`Expiration date`<-as.character(Stock55$`Expiration date`)
Stock56$`Expiration date`<-as.character(Stock56$`Expiration date`)
Stock57$`Expiration date`<-as.character(Stock57$`Expiration date`)
Stock58$`Expiration date`<-as.character(Stock58$`Expiration date`)
Stock59$`Expiration date`<-as.character(Stock59$`Expiration date`)
Stock60$`Expiration date`<-as.character(Stock60$`Expiration date`)
Stock61$`Expiration date`<-as.character(Stock61$`Expiration date`)
Stock62$`Expiration date`<-as.character(Stock62$`Expiration date`)
Stock63$`Expiration date`<-as.character(Stock63$`Expiration date`)
Stock64$`Expiration date`<-as.character(Stock64$`Expiration date`)
Stock65$`Expiration date`<-as.character(Stock65$`Expiration date`)
Stock66$`Expiration date`<-as.character(Stock66$`Expiration date`)
Stock67$`Expiration date`<-as.character(Stock67$`Expiration date`)
Stock68$`Expiration date`<-as.character(Stock68$`Expiration date`)
Stock69$`Expiration date`<-as.character(Stock69$`Expiration date`)
Stock70$`Expiration date`<-as.character(Stock70$`Expiration date`)
Stock71$`Expiration date`<-as.character(Stock71$`Expiration date`)
Stock72$`Expiration date`<-as.character(Stock72$`Expiration date`)
Stock73$`Expiration date`<-as.character(Stock73$`Expiration date`)
Stock74$`Expiration date`<-as.character(Stock74$`Expiration date`)
Stock75$`Expiration date`<-as.character(Stock75$`Expiration date`)
Stock76$`Expiration date`<-as.character(Stock76$`Expiration date`)
Stock77$`Expiration date`<-as.character(Stock77$`Expiration date`)
Stock78$`Expiration date`<-as.character(Stock78$`Expiration date`)
Stock79$`Expiration date`<-as.character(Stock79$`Expiration date`)
Stock80$`Expiration date`<-as.character(Stock80$`Expiration date`)
Stock81$`Expiration date`<-as.character(Stock81$`Expiration date`)
Stock82$`Expiration date`<-as.character(Stock82$`Expiration date`)
Stock83$`Expiration date`<-as.character(Stock83$`Expiration date`)
Stock84$`Expiration date`<-as.character(Stock84$`Expiration date`)
Stock85$`Expiration date`<-as.character(Stock85$`Expiration date`)
Stock86$`Expiration date`<-as.character(Stock86$`Expiration date`)
Stock87$`Expiration date`<-as.character(Stock87$`Expiration date`)
Stock88$`Expiration date`<-as.character(Stock88$`Expiration date`)
Stock89$`Expiration date`<-as.character(Stock89$`Expiration date`)
Stock90$`Expiration date`<-as.character(Stock90$`Expiration date`)
Stock91$`Expiration date`<-as.character(Stock91$`Expiration date`)
Stock92$`Expiration date`<-as.character(Stock92$`Expiration date`)
Stock93$`Expiration date`<-as.character(Stock93$`Expiration date`)
Stock94$`Expiration date`<-as.character(Stock94$`Expiration date`)
Stock95$`Expiration date`<-as.character(Stock95$`Expiration date`)
Stock96$`Expiration date`<-as.character(Stock96$`Expiration date`)
Stock97$`Expiration date`<-as.character(Stock97$`Expiration date`)
Stock98$`Expiration date`<-as.character(Stock98$`Expiration date`)
Stock99$`Expiration date`<-as.character(Stock99$`Expiration date`)
Stock100$`Expiration date`<-as.character(Stock100$`Expiration date`)
Stock101$`Expiration date`<-as.character(Stock101$`Expiration date`)
Stock102$`Expiration date`<-as.character(Stock102$`Expiration date`)
Stock103$`Expiration date`<-as.character(Stock103$`Expiration date`)
Stock104$`Expiration date`<-as.character(Stock104$`Expiration date`)
Stock105$`Expiration date`<-as.character(Stock105$`Expiration date`)
Stock106$`Expiration date`<-as.character(Stock106$`Expiration date`)
Stock107$`Expiration date`<-as.character(Stock107$`Expiration date`)
Stock108$`Expiration date`<-as.character(Stock108$`Expiration date`)
Stock109$`Expiration date`<-as.character(Stock109$`Expiration date`)
Stock110$`Expiration date`<-as.character(Stock110$`Expiration date`)
Stock111$`Expiration date`<-as.character(Stock111$`Expiration date`)
Stock112$`Expiration date`<-as.character(Stock112$`Expiration date`)
Stock113$`Expiration date`<-as.character(Stock113$`Expiration date`)
Stock114$`Expiration date`<-as.character(Stock114$`Expiration date`)
Stock115$`Expiration date`<-as.character(Stock115$`Expiration date`)
Stock116$`Expiration date`<-as.character(Stock116$`Expiration date`)
Stock117$`Expiration date`<-as.character(Stock117$`Expiration date`)
Stock118$`Expiration date`<-as.character(Stock118$`Expiration date`)
Stock119$`Expiration date`<-as.character(Stock119$`Expiration date`)
Stock120$`Expiration date`<-as.character(Stock120$`Expiration date`)
Stock121$`Expiration date`<-as.character(Stock121$`Expiration date`)
Stock122$`Expiration date`<-as.character(Stock122$`Expiration date`)
Stock123$`Expiration date`<-as.character(Stock123$`Expiration date`)
Stock124$`Expiration date`<-as.character(Stock124$`Expiration date`)
Stock125$`Expiration date`<-as.character(Stock125$`Expiration date`)
Stock126$`Expiration date`<-as.character(Stock126$`Expiration date`)
Stock127$`Expiration date`<-as.character(Stock127$`Expiration date`)
Stock128$`Expiration date`<-as.character(Stock128$`Expiration date`)
Stock129$`Expiration date`<-as.character(Stock129$`Expiration date`)
Stock130$`Expiration date`<-as.character(Stock130$`Expiration date`)
Stock131$`Expiration date`<-as.character(Stock131$`Expiration date`)
Stock132$`Expiration date`<-as.character(Stock132$`Expiration date`)
Stock133$`Expiration date`<-as.character(Stock133$`Expiration date`)
Stock134$`Expiration date`<-as.character(Stock134$`Expiration date`)
Stock135$`Expiration date`<-as.character(Stock135$`Expiration date`)
Stock136$`Expiration date`<-as.character(Stock136$`Expiration date`)
Stock137$`Expiration date`<-as.character(Stock137$`Expiration date`)
Stock138$`Expiration date`<-as.character(Stock138$`Expiration date`)
Stock139$`Expiration date`<-as.character(Stock139$`Expiration date`)
Stock140$`Expiration date`<-as.character(Stock140$`Expiration date`)
Stock141$`Expiration date`<-as.character(Stock141$`Expiration date`)
Stock142$`Expiration date`<-as.character(Stock142$`Expiration date`)
Stock143$`Expiration date`<-as.character(Stock143$`Expiration date`)
Stock144$`Expiration date`<-as.character(Stock144$`Expiration date`)
Stock145$`Expiration date`<-as.character(Stock145$`Expiration date`)
Stock146$`Expiration date`<-as.character(Stock146$`Expiration date`)
Stock147$`Expiration date`<-as.character(Stock147$`Expiration date`)
Stock148$`Expiration date`<-as.character(Stock148$`Expiration date`)
Stock149$`Expiration date`<-as.character(Stock149$`Expiration date`)
Stock150$`Expiration date`<-as.character(Stock150$`Expiration date`)
Stock151$`Expiration date`<-as.character(Stock151$`Expiration date`)
Stock152$`Expiration date`<-as.character(Stock152$`Expiration date`)
Stock153$`Expiration date`<-as.character(Stock153$`Expiration date`)
Stock154$`Expiration date`<-as.character(Stock154$`Expiration date`)
Stock155$`Expiration date`<-as.character(Stock155$`Expiration date`)
Stock156$`Expiration date`<-as.character(Stock156$`Expiration date`)
Stock157$`Expiration date`<-as.character(Stock157$`Expiration date`)
Stock158$`Expiration date`<-as.character(Stock158$`Expiration date`)
Stock159$`Expiration date`<-as.character(Stock159$`Expiration date`)
Stock160$`Expiration date`<-as.character(Stock160$`Expiration date`)
Stock161$`Expiration date`<-as.character(Stock161$`Expiration date`)
Stock162$`Expiration date`<-as.character(Stock162$`Expiration date`)
Stock163$`Expiration date`<-as.character(Stock163$`Expiration date`)
Stock164$`Expiration date`<-as.character(Stock164$`Expiration date`)
Stock165$`Expiration date`<-as.character(Stock165$`Expiration date`)
Stock166$`Expiration date`<-as.character(Stock166$`Expiration date`)
Stock167$`Expiration date`<-as.character(Stock167$`Expiration date`)
Stock168$`Expiration date`<-as.character(Stock168$`Expiration date`)
Stock169$`Expiration date`<-as.character(Stock169$`Expiration date`)
Stock170$`Expiration date`<-as.character(Stock170$`Expiration date`)
Stock171$`Expiration date`<-as.character(Stock171$`Expiration date`)
Stock172$`Expiration date`<-as.character(Stock172$`Expiration date`)
Stock173$`Expiration date`<-as.character(Stock173$`Expiration date`)
Stock174$`Expiration date`<-as.character(Stock174$`Expiration date`)
Stock175$`Expiration date`<-as.character(Stock175$`Expiration date`)
Stock176$`Expiration date`<-as.character(Stock176$`Expiration date`)
Stock177$`Expiration date`<-as.character(Stock177$`Expiration date`)
Stock178$`Expiration date`<-as.character(Stock178$`Expiration date`)
Stock179$`Expiration date`<-as.character(Stock179$`Expiration date`)
Stock180$`Expiration date`<-as.character(Stock180$`Expiration date`)
Stock181$`Expiration date`<-as.character(Stock181$`Expiration date`)
Stock182$`Expiration date`<-as.character(Stock182$`Expiration date`)
Stock183$`Expiration date`<-as.character(Stock183$`Expiration date`)
Stock184$`Expiration date`<-as.character(Stock184$`Expiration date`)
Stock185$`Expiration date`<-as.character(Stock185$`Expiration date`)
Stock186$`Expiration date`<-as.character(Stock186$`Expiration date`)
Stock187$`Expiration date`<-as.character(Stock187$`Expiration date`)
Stock188$`Expiration date`<-as.character(Stock188$`Expiration date`)
Stock189$`Expiration date`<-as.character(Stock189$`Expiration date`)
Stock190$`Expiration date`<-as.character(Stock190$`Expiration date`)
Stock191$`Expiration date`<-as.character(Stock191$`Expiration date`)
Stock192$`Expiration date`<-as.character(Stock192$`Expiration date`)
Stock193$`Expiration date`<-as.character(Stock193$`Expiration date`)
Stock194$`Expiration date`<-as.character(Stock194$`Expiration date`)
Stock195$`Expiration date`<-as.character(Stock195$`Expiration date`)
Stock196$`Expiration date`<-as.character(Stock196$`Expiration date`)
Stock197$`Expiration date`<-as.character(Stock197$`Expiration date`)
Stock198$`Expiration date`<-as.character(Stock198$`Expiration date`)
Stock199$`Expiration date`<-as.character(Stock199$`Expiration date`)
Stock200$`Expiration date`<-as.character(Stock200$`Expiration date`)
Stock201$`Expiration date`<-as.character(Stock201$`Expiration date`)
Stock202$`Expiration date`<-as.character(Stock202$`Expiration date`)
Stock203$`Expiration date`<-as.character(Stock203$`Expiration date`)
Stock204$`Expiration date`<-as.character(Stock204$`Expiration date`)
Stock205$`Expiration date`<-as.character(Stock205$`Expiration date`)
Stock206$`Expiration date`<-as.character(Stock206$`Expiration date`)
Stock207$`Expiration date`<-as.character(Stock207$`Expiration date`)
Stock208$`Expiration date`<-as.character(Stock208$`Expiration date`)
Stock209$`Expiration date`<-as.character(Stock209$`Expiration date`)
Stock210$`Expiration date`<-as.character(Stock210$`Expiration date`)
Stock211$`Expiration date`<-as.character(Stock211$`Expiration date`)
Stock212$`Expiration date`<-as.character(Stock212$`Expiration date`)
Stock213$`Expiration date`<-as.character(Stock213$`Expiration date`)
Stock214$`Expiration date`<-as.character(Stock214$`Expiration date`)
Stock215$`Expiration date`<-as.character(Stock215$`Expiration date`)
Stock216$`Expiration date`<-as.character(Stock216$`Expiration date`)
Stock217$`Expiration date`<-as.character(Stock217$`Expiration date`)
Stock218$`Expiration date`<-as.character(Stock218$`Expiration date`)
Stock219$`Expiration date`<-as.character(Stock219$`Expiration date`)
Stock220$`Expiration date`<-as.character(Stock220$`Expiration date`)
Stock221$`Expiration date`<-as.character(Stock221$`Expiration date`)
Stock222$`Expiration date`<-as.character(Stock222$`Expiration date`)
Stock223$`Expiration date`<-as.character(Stock223$`Expiration date`)
Stock224$`Expiration date`<-as.character(Stock224$`Expiration date`)
Stock225$`Expiration date`<-as.character(Stock225$`Expiration date`)
Stock226$`Expiration date`<-as.character(Stock226$`Expiration date`)
Stock227$`Expiration date`<-as.character(Stock227$`Expiration date`)
Stock228$`Expiration date`<-as.character(Stock228$`Expiration date`)
Stock229$`Expiration date`<-as.character(Stock229$`Expiration date`)
Stock230$`Expiration date`<-as.character(Stock230$`Expiration date`)
Stock231$`Expiration date`<-as.character(Stock231$`Expiration date`)
Stock232$`Expiration date`<-as.character(Stock232$`Expiration date`)
Stock233$`Expiration date`<-as.character(Stock233$`Expiration date`)
Stock234$`Expiration date`<-as.character(Stock234$`Expiration date`)
Stock235$`Expiration date`<-as.character(Stock235$`Expiration date`)
Stock236$`Expiration date`<-as.character(Stock236$`Expiration date`)
Stock237$`Expiration date`<-as.character(Stock237$`Expiration date`)
Stock238$`Expiration date`<-as.character(Stock238$`Expiration date`)
Stock239$`Expiration date`<-as.character(Stock239$`Expiration date`)
Stock240$`Expiration date`<-as.character(Stock240$`Expiration date`)
Stock241$`Expiration date`<-as.character(Stock241$`Expiration date`)
Stock242$`Expiration date`<-as.character(Stock242$`Expiration date`)
Stock243$`Expiration date`<-as.character(Stock243$`Expiration date`)
Stock244$`Expiration date`<-as.character(Stock244$`Expiration date`)
Stock245$`Expiration date`<-as.character(Stock245$`Expiration date`)
Stock246$`Expiration date`<-as.character(Stock246$`Expiration date`)
Stock247$`Expiration date`<-as.character(Stock247$`Expiration date`)
Stock248$`Expiration date`<-as.character(Stock248$`Expiration date`)
Stock249$`Expiration date`<-as.character(Stock249$`Expiration date`)
Stock250$`Expiration date`<-as.character(Stock250$`Expiration date`)
Stock251$`Expiration date`<-as.character(Stock251$`Expiration date`)
Stock252$`Expiration date`<-as.character(Stock252$`Expiration date`)
Stock253$`Expiration date`<-as.character(Stock253$`Expiration date`)
Stock254$`Expiration date`<-as.character(Stock254$`Expiration date`)
Stock255$`Expiration date`<-as.character(Stock255$`Expiration date`)
Stock256$`Expiration date`<-as.character(Stock256$`Expiration date`)
Stock257$`Expiration date`<-as.character(Stock257$`Expiration date`)
Stock258$`Expiration date`<-as.character(Stock258$`Expiration date`)
Stock259$`Expiration date`<-as.character(Stock259$`Expiration date`)
Stock260$`Expiration date`<-as.character(Stock260$`Expiration date`)
Stock261$`Expiration date`<-as.character(Stock261$`Expiration date`)
Stock262$`Expiration date`<-as.character(Stock262$`Expiration date`)
Stock263$`Expiration date`<-as.character(Stock263$`Expiration date`)
Stock264$`Expiration date`<-as.character(Stock264$`Expiration date`)
Stock265$`Expiration date`<-as.character(Stock265$`Expiration date`)
Stock266$`Expiration date`<-as.character(Stock266$`Expiration date`)
Stock267$`Expiration date`<-as.character(Stock267$`Expiration date`)
Stock268$`Expiration date`<-as.character(Stock268$`Expiration date`)
Stock269$`Expiration date`<-as.character(Stock269$`Expiration date`)
Stock270$`Expiration date`<-as.character(Stock270$`Expiration date`)
Stock271$`Expiration date`<-as.character(Stock271$`Expiration date`)
Stock272$`Expiration date`<-as.character(Stock272$`Expiration date`)
Stock273$`Expiration date`<-as.character(Stock273$`Expiration date`)
Stock274$`Expiration date`<-as.character(Stock274$`Expiration date`)
Stock275$`Expiration date`<-as.character(Stock275$`Expiration date`)
Stock276$`Expiration date`<-as.character(Stock276$`Expiration date`)
Stock277$`Expiration date`<-as.character(Stock277$`Expiration date`)
Stock278$`Expiration date`<-as.character(Stock278$`Expiration date`)
Stock279$`Expiration date`<-as.character(Stock279$`Expiration date`)
Stock280$`Expiration date`<-as.character(Stock280$`Expiration date`)
Stock281$`Expiration date`<-as.character(Stock281$`Expiration date`)
Stock282$`Expiration date`<-as.character(Stock282$`Expiration date`)
Stock283$`Expiration date`<-as.character(Stock283$`Expiration date`)
Stock284$`Expiration date`<-as.character(Stock284$`Expiration date`)
Stock285$`Expiration date`<-as.character(Stock285$`Expiration date`)
Stock286$`Expiration date`<-as.character(Stock286$`Expiration date`)
Stock287$`Expiration date`<-as.character(Stock287$`Expiration date`)
Stock288$`Expiration date`<-as.character(Stock288$`Expiration date`)
Stock289$`Expiration date`<-as.character(Stock289$`Expiration date`)
Stock290$`Expiration date`<-as.character(Stock290$`Expiration date`)
Stock291$`Expiration date`<-as.character(Stock291$`Expiration date`)
Stock292$`Expiration date`<-as.character(Stock292$`Expiration date`)
Stock293$`Expiration date`<-as.character(Stock293$`Expiration date`)
Stock294$`Expiration date`<-as.character(Stock294$`Expiration date`)
Stock295$`Expiration date`<-as.character(Stock295$`Expiration date`)
Stock296$`Expiration date`<-as.character(Stock296$`Expiration date`)
Stock297$`Expiration date`<-as.character(Stock297$`Expiration date`)
Stock298$`Expiration date`<-as.character(Stock298$`Expiration date`)
Stock299$`Expiration date`<-as.character(Stock299$`Expiration date`)
Stock300$`Expiration date`<-as.character(Stock300$`Expiration date`)
Stock301$`Expiration date`<-as.character(Stock301$`Expiration date`)
Stock302$`Expiration date`<-as.character(Stock302$`Expiration date`)
Stock303$`Expiration date`<-as.character(Stock303$`Expiration date`)
Stock304$`Expiration date`<-as.character(Stock304$`Expiration date`)
Stock305$`Expiration date`<-as.character(Stock305$`Expiration date`)
Stock306$`Expiration date`<-as.character(Stock306$`Expiration date`)
Stock307$`Expiration date`<-as.character(Stock307$`Expiration date`)
Stock308$`Expiration date`<-as.character(Stock308$`Expiration date`)
Stock309$`Expiration date`<-as.character(Stock309$`Expiration date`)
Stock310$`Expiration date`<-as.character(Stock310$`Expiration date`)
Stock311$`Expiration date`<-as.character(Stock311$`Expiration date`)
Stock312$`Expiration date`<-as.character(Stock312$`Expiration date`)
Stock313$`Expiration date`<-as.character(Stock313$`Expiration date`)
Stock314$`Expiration date`<-as.character(Stock314$`Expiration date`)
Stock315$`Expiration date`<-as.character(Stock315$`Expiration date`)
Stock316$`Expiration date`<-as.character(Stock316$`Expiration date`)
Stock317$`Expiration date`<-as.character(Stock317$`Expiration date`)
Stock318$`Expiration date`<-as.character(Stock318$`Expiration date`)
Stock319$`Expiration date`<-as.character(Stock319$`Expiration date`)
Stock320$`Expiration date`<-as.character(Stock320$`Expiration date`)
Stock321$`Expiration date`<-as.character(Stock321$`Expiration date`)
Stock322$`Expiration date`<-as.character(Stock322$`Expiration date`)
Stock323$`Expiration date`<-as.character(Stock323$`Expiration date`)
Stock324$`Expiration date`<-as.character(Stock324$`Expiration date`)
Stock325$`Expiration date`<-as.character(Stock325$`Expiration date`)
Stock326$`Expiration date`<-as.character(Stock326$`Expiration date`)
Stock327$`Expiration date`<-as.character(Stock327$`Expiration date`)
Stock328$`Expiration date`<-as.character(Stock328$`Expiration date`)
Stock329$`Expiration date`<-as.character(Stock329$`Expiration date`)
Stock330$`Expiration date`<-as.character(Stock330$`Expiration date`)
Stock331$`Expiration date`<-as.character(Stock331$`Expiration date`)
Stock332$`Expiration date`<-as.character(Stock332$`Expiration date`)
Stock333$`Expiration date`<-as.character(Stock333$`Expiration date`)
Stock334$`Expiration date`<-as.character(Stock334$`Expiration date`)
Stock335$`Expiration date`<-as.character(Stock335$`Expiration date`)
Stock336$`Expiration date`<-as.character(Stock336$`Expiration date`)
Stock337$`Expiration date`<-as.character(Stock337$`Expiration date`)
Stock338$`Expiration date`<-as.character(Stock338$`Expiration date`)
Stock339$`Expiration date`<-as.character(Stock339$`Expiration date`)
Stock340$`Expiration date`<-as.character(Stock340$`Expiration date`)
Stock341$`Expiration date`<-as.character(Stock341$`Expiration date`)
Stock342$`Expiration date`<-as.character(Stock342$`Expiration date`)
Stock343$`Expiration date`<-as.character(Stock343$`Expiration date`)
Stock344$`Expiration date`<-as.character(Stock344$`Expiration date`)
Stock345$`Expiration date`<-as.character(Stock345$`Expiration date`)
Stock346$`Expiration date`<-as.character(Stock346$`Expiration date`)
Stock347$`Expiration date`<-as.character(Stock347$`Expiration date`)
Stock348$`Expiration date`<-as.character(Stock348$`Expiration date`)
Stock349$`Expiration date`<-as.character(Stock349$`Expiration date`)
Stock350$`Expiration date`<-as.character(Stock350$`Expiration date`)
Stock351$`Expiration date`<-as.character(Stock351$`Expiration date`)
Stock352$`Expiration date`<-as.character(Stock352$`Expiration date`)
Stock353$`Expiration date`<-as.character(Stock353$`Expiration date`)
Stock354$`Expiration date`<-as.character(Stock354$`Expiration date`)
Stock355$`Expiration date`<-as.character(Stock355$`Expiration date`)
Stock356$`Expiration date`<-as.character(Stock356$`Expiration date`)
Stock357$`Expiration date`<-as.character(Stock357$`Expiration date`)
Stock358$`Expiration date`<-as.character(Stock358$`Expiration date`)
Stock359$`Expiration date`<-as.character(Stock359$`Expiration date`)
Stock360$`Expiration date`<-as.character(Stock360$`Expiration date`)
Stock361$`Expiration date`<-as.character(Stock361$`Expiration date`)
Stock362$`Expiration date`<-as.character(Stock362$`Expiration date`)
Stock363$`Expiration date`<-as.character(Stock363$`Expiration date`)
Stock364$`Expiration date`<-as.character(Stock364$`Expiration date`)
Stock365$`Expiration date`<-as.character(Stock365$`Expiration date`)
Stock366$`Expiration date`<-as.character(Stock366$`Expiration date`)
Stock367$`Expiration date`<-as.character(Stock367$`Expiration date`)
Stock368$`Expiration date`<-as.character(Stock368$`Expiration date`)
Stock369$`Expiration date`<-as.character(Stock369$`Expiration date`)
Stock370$`Expiration date`<-as.character(Stock370$`Expiration date`)
Stock371$`Expiration date`<-as.character(Stock371$`Expiration date`)
Stock372$`Expiration date`<-as.character(Stock372$`Expiration date`)
Stock373$`Expiration date`<-as.character(Stock373$`Expiration date`)
Stock374$`Expiration date`<-as.character(Stock374$`Expiration date`)
Stock375$`Expiration date`<-as.character(Stock375$`Expiration date`)
Stock376$`Expiration date`<-as.character(Stock376$`Expiration date`)
Stock377$`Expiration date`<-as.character(Stock377$`Expiration date`)
Stock378$`Expiration date`<-as.character(Stock378$`Expiration date`)
Stock379$`Expiration date`<-as.character(Stock379$`Expiration date`)
Stock380$`Expiration date`<-as.character(Stock380$`Expiration date`)
Stock381$`Expiration date`<-as.character(Stock381$`Expiration date`)
Stock382$`Expiration date`<-as.character(Stock382$`Expiration date`)
Stock383$`Expiration date`<-as.character(Stock383$`Expiration date`)
Stock384$`Expiration date`<-as.character(Stock384$`Expiration date`)
Stock385$`Expiration date`<-as.character(Stock385$`Expiration date`)
Stock386$`Expiration date`<-as.character(Stock386$`Expiration date`)
Stock387$`Expiration date`<-as.character(Stock387$`Expiration date`)
Stock388$`Expiration date`<-as.character(Stock388$`Expiration date`)
Stock389$`Expiration date`<-as.character(Stock389$`Expiration date`)
Stock390$`Expiration date`<-as.character(Stock390$`Expiration date`)
Stock391$`Expiration date`<-as.character(Stock391$`Expiration date`)
Stock392$`Expiration date`<-as.character(Stock392$`Expiration date`)
Stock393$`Expiration date`<-as.character(Stock393$`Expiration date`)
Stock394$`Expiration date`<-as.character(Stock394$`Expiration date`)
Stock395$`Expiration date`<-as.character(Stock395$`Expiration date`)
Stock396$`Expiration date`<-as.character(Stock396$`Expiration date`)
Stock397$`Expiration date`<-as.character(Stock397$`Expiration date`)
Stock398$`Expiration date`<-as.character(Stock398$`Expiration date`)
Stock399$`Expiration date`<-as.character(Stock399$`Expiration date`)
Stock400$`Expiration date`<-as.character(Stock400$`Expiration date`)
Stock401$`Expiration date`<-as.character(Stock401$`Expiration date`)
Stock402$`Expiration date`<-as.character(Stock402$`Expiration date`)
Stock403$`Expiration date`<-as.character(Stock403$`Expiration date`)
Stock404$`Expiration date`<-as.character(Stock404$`Expiration date`)
Stock405$`Expiration date`<-as.character(Stock405$`Expiration date`)
Stock406$`Expiration date`<-as.character(Stock406$`Expiration date`)
Stock407$`Expiration date`<-as.character(Stock407$`Expiration date`)
Stock408$`Expiration date`<-as.character(Stock408$`Expiration date`)
Stock409$`Expiration date`<-as.character(Stock409$`Expiration date`)
Stock410$`Expiration date`<-as.character(Stock410$`Expiration date`)
Stock411$`Expiration date`<-as.character(Stock411$`Expiration date`)
Stock412$`Expiration date`<-as.character(Stock412$`Expiration date`)
Stock413$`Expiration date`<-as.character(Stock413$`Expiration date`)
Stock414$`Expiration date`<-as.character(Stock414$`Expiration date`)
Stock415$`Expiration date`<-as.character(Stock415$`Expiration date`)
Stock416$`Expiration date`<-as.character(Stock416$`Expiration date`)
Stock417$`Expiration date`<-as.character(Stock417$`Expiration date`)
Stock418$`Expiration date`<-as.character(Stock418$`Expiration date`)
Stock419$`Expiration date`<-as.character(Stock419$`Expiration date`)
Stock420$`Expiration date`<-as.character(Stock420$`Expiration date`)
Stock421$`Expiration date`<-as.character(Stock421$`Expiration date`)
Stock422$`Expiration date`<-as.character(Stock422$`Expiration date`)
Stock423$`Expiration date`<-as.character(Stock423$`Expiration date`)
Stock424$`Expiration date`<-as.character(Stock424$`Expiration date`)
Stock425$`Expiration date`<-as.character(Stock425$`Expiration date`)
Stock426$`Expiration date`<-as.character(Stock426$`Expiration date`)
Stock427$`Expiration date`<-as.character(Stock427$`Expiration date`)
Stock428$`Expiration date`<-as.character(Stock428$`Expiration date`)
Stock429$`Expiration date`<-as.character(Stock429$`Expiration date`)
Stock430$`Expiration date`<-as.character(Stock430$`Expiration date`)
Stock431$`Expiration date`<-as.character(Stock431$`Expiration date`)
Stock432$`Expiration date`<-as.character(Stock432$`Expiration date`)
Stock433$`Expiration date`<-as.character(Stock433$`Expiration date`)
Stock434$`Expiration date`<-as.character(Stock434$`Expiration date`)
Stock435$`Expiration date`<-as.character(Stock435$`Expiration date`)
Stock436$`Expiration date`<-as.character(Stock436$`Expiration date`)
Stock437$`Expiration date`<-as.character(Stock437$`Expiration date`)
Stock438$`Expiration date`<-as.character(Stock438$`Expiration date`)
Stock439$`Expiration date`<-as.character(Stock439$`Expiration date`)
Stock440$`Expiration date`<-as.character(Stock440$`Expiration date`)
Stock441$`Expiration date`<-as.character(Stock441$`Expiration date`)
Stock442$`Expiration date`<-as.character(Stock442$`Expiration date`)
Stock443$`Expiration date`<-as.character(Stock443$`Expiration date`)
Stock444$`Expiration date`<-as.character(Stock444$`Expiration date`)
Stock445$`Expiration date`<-as.character(Stock445$`Expiration date`)
Stock446$`Expiration date`<-as.character(Stock446$`Expiration date`)
Stock447$`Expiration date`<-as.character(Stock447$`Expiration date`)
Stock448$`Expiration date`<-as.character(Stock448$`Expiration date`)
Stock449$`Expiration date`<-as.character(Stock449$`Expiration date`)
Stock450$`Expiration date`<-as.character(Stock450$`Expiration date`)
Stock451$`Expiration date`<-as.character(Stock451$`Expiration date`)
Stock452$`Expiration date`<-as.character(Stock452$`Expiration date`)
Stock453$`Expiration date`<-as.character(Stock453$`Expiration date`)
Stock454$`Expiration date`<-as.character(Stock454$`Expiration date`)
Stock455$`Expiration date`<-as.character(Stock455$`Expiration date`)
Stock456$`Expiration date`<-as.character(Stock456$`Expiration date`)
Stock457$`Expiration date`<-as.character(Stock457$`Expiration date`)
Stock458$`Expiration date`<-as.character(Stock458$`Expiration date`)
Stock459$`Expiration date`<-as.character(Stock459$`Expiration date`)
Stock460$`Expiration date`<-as.character(Stock460$`Expiration date`)
Stock461$`Expiration date`<-as.character(Stock461$`Expiration date`)
Stock462$`Expiration date`<-as.character(Stock462$`Expiration date`)
Stock463$`Expiration date`<-as.character(Stock463$`Expiration date`)
Stock464$`Expiration date`<-as.character(Stock464$`Expiration date`)
Stock465$`Expiration date`<-as.character(Stock465$`Expiration date`)
Stock466$`Expiration date`<-as.character(Stock466$`Expiration date`)
Stock467$`Expiration date`<-as.character(Stock467$`Expiration date`)
Stock468$`Expiration date`<-as.character(Stock468$`Expiration date`)
Stock469$`Expiration date`<-as.character(Stock469$`Expiration date`)
Stock470$`Expiration date`<-as.character(Stock470$`Expiration date`)
Stock471$`Expiration date`<-as.character(Stock471$`Expiration date`)
Stock472$`Expiration date`<-as.character(Stock472$`Expiration date`)
Stock473$`Expiration date`<-as.character(Stock473$`Expiration date`)
Stock474$`Expiration date`<-as.character(Stock474$`Expiration date`)
Stock475$`Expiration date`<-as.character(Stock475$`Expiration date`)
Stock476$`Expiration date`<-as.character(Stock476$`Expiration date`)
Stock477$`Expiration date`<-as.character(Stock477$`Expiration date`)
Stock478$`Expiration date`<-as.character(Stock478$`Expiration date`)
Stock479$`Expiration date`<-as.character(Stock479$`Expiration date`)
Stock480$`Expiration date`<-as.character(Stock480$`Expiration date`)
Stock481$`Expiration date`<-as.character(Stock481$`Expiration date`)
Stock482$`Expiration date`<-as.character(Stock482$`Expiration date`)
Stock483$`Expiration date`<-as.character(Stock483$`Expiration date`)
Stock484$`Expiration date`<-as.character(Stock484$`Expiration date`)
Stock485$`Expiration date`<-as.character(Stock485$`Expiration date`)
Stock486$`Expiration date`<-as.character(Stock486$`Expiration date`)
Stock487$`Expiration date`<-as.character(Stock487$`Expiration date`)
Stock488$`Expiration date`<-as.character(Stock488$`Expiration date`)
Stock489$`Expiration date`<-as.character(Stock489$`Expiration date`)
Stock490$`Expiration date`<-as.character(Stock490$`Expiration date`)
Stock491$`Expiration date`<-as.character(Stock491$`Expiration date`)
Stock492$`Expiration date`<-as.character(Stock492$`Expiration date`)
Stock493$`Expiration date`<-as.character(Stock493$`Expiration date`)
Stock494$`Expiration date`<-as.character(Stock494$`Expiration date`)
Stock495$`Expiration date`<-as.character(Stock495$`Expiration date`)
Stock496$`Expiration date`<-as.character(Stock496$`Expiration date`)
Stock497$`Expiration date`<-as.character(Stock497$`Expiration date`)
Stock498$`Expiration date`<-as.character(Stock498$`Expiration date`)
Stock499$`Expiration date`<-as.character(Stock499$`Expiration date`)
Stock500$`Expiration date`<-as.character(Stock500$`Expiration date`)
Stock501$`Expiration date`<-as.character(Stock501$`Expiration date`)
Stock502$`Expiration date`<-as.character(Stock502$`Expiration date`)
Stock503$`Expiration date`<-as.character(Stock503$`Expiration date`)
Stock504$`Expiration date`<-as.character(Stock504$`Expiration date`)
Stock505$`Expiration date`<-as.character(Stock505$`Expiration date`)
Stock506$`Expiration date`<-as.character(Stock506$`Expiration date`)
Stock507$`Expiration date`<-as.character(Stock507$`Expiration date`)
Stock508$`Expiration date`<-as.character(Stock508$`Expiration date`)
Stock509$`Expiration date`<-as.character(Stock509$`Expiration date`)
Stock510$`Expiration date`<-as.character(Stock510$`Expiration date`)
Stock511$`Expiration date`<-as.character(Stock511$`Expiration date`)
Stock512$`Expiration date`<-as.character(Stock512$`Expiration date`)
Stock513$`Expiration date`<-as.character(Stock513$`Expiration date`)
Stock514$`Expiration date`<-as.character(Stock514$`Expiration date`)
Stock515$`Expiration date`<-as.character(Stock515$`Expiration date`)
Stock516$`Expiration date`<-as.character(Stock516$`Expiration date`)
Stock517$`Expiration date`<-as.character(Stock517$`Expiration date`)
Stock518$`Expiration date`<-as.character(Stock518$`Expiration date`)
Stock519$`Expiration date`<-as.character(Stock519$`Expiration date`)

PatentStock1<-bind_rows(Stock1,Stock2,Stock3,Stock4,Stock5,Stock6,Stock7,Stock8,Stock9,Stock10,Stock11,Stock12,
                        Stock13,Stock14,Stock15,Stock16,Stock17,Stock18,Stock19,Stock20,Stock21,Stock22,Stock23,
                        Stock24,Stock25,Stock26,Stock27,Stock28,Stock29,Stock30,Stock31,Stock32,Stock33,Stock34,
                        Stock35,Stock36,Stock37,Stock38,Stock39,Stock40,Stock41,Stock42,Stock43,Stock44,Stock45,
                        Stock46,Stock47,Stock48,Stock49,Stock50,Stock51,Stock52,Stock53,Stock54,Stock55,Stock56,
                        Stock57,Stock58,Stock59,Stock60,Stock61,Stock62,Stock63,Stock64,Stock65,Stock66,Stock67,
                        Stock68,Stock69,Stock70,Stock71,Stock72,Stock73,Stock74,Stock75,Stock76,Stock77,Stock78,
                        Stock79,Stock80,Stock81,Stock82,Stock83,Stock84,Stock85,Stock86,Stock87,Stock88,Stock89,
                        Stock90,Stock91,Stock92,Stock93,Stock94,Stock95,Stock96,Stock97,Stock98,Stock99,Stock100,
                        Stock101,Stock102,Stock103,Stock104,Stock105,Stock106,Stock107,Stock108,Stock109,Stock110,
                        Stock111,Stock112,Stock113,Stock114,Stock115,Stock116,Stock117,Stock118,Stock119,Stock120,
                        Stock121,Stock122,Stock123,Stock124,Stock125,Stock126,Stock127,Stock128,Stock129,Stock130,
                        Stock131,Stock132,Stock133,Stock134,Stock135,Stock136,Stock137,Stock138,Stock139,Stock140,
                        Stock141,Stock142,Stock143,Stock144,Stock145,Stock146,Stock147,Stock148,Stock149,Stock150,
                        Stock151,Stock152,Stock153,Stock154,Stock155,Stock156,Stock157,Stock158,Stock159,Stock160,
                        Stock161,Stock162,Stock163,Stock164,Stock165,Stock166,Stock167,Stock168,Stock169,Stock170,
                        Stock171,Stock172,Stock173,Stock174,Stock175,Stock176,Stock177,Stock178,Stock179,Stock180,
                        Stock181,Stock182,Stock183,Stock184,Stock185,Stock186,Stock187,Stock188,Stock189,Stock190,
                        Stock191,Stock192,Stock193,Stock194,Stock195,Stock196,Stock197,Stock198,Stock199,Stock200,
                        Stock201,Stock202,Stock203,Stock204,Stock205,Stock206,Stock207,Stock208,Stock209,Stock210,
                        Stock211,Stock212,Stock213,Stock214,Stock215,Stock216,Stock217,Stock218,Stock219,Stock220,
                        Stock221,Stock222,Stock223,Stock224,Stock225,Stock226,Stock227,Stock228,Stock229,Stock230,
                        Stock231,Stock232,Stock233,Stock234,Stock235,Stock236,Stock237,Stock238,Stock239,Stock240,
                        Stock241,Stock242,Stock243,Stock244,Stock245,Stock246,Stock247,Stock248,Stock249,Stock250,
                        Stock251,Stock252,Stock253,Stock254,Stock255,Stock256,Stock257,Stock258,Stock259,Stock260,
                        Stock261,Stock262,Stock263,Stock264,Stock265,Stock266,Stock267,Stock268,Stock269,Stock270,
                        Stock271,Stock272,Stock273,Stock274,Stock275,Stock276,Stock277,Stock278,Stock279,Stock280,
                        Stock281,Stock282,Stock283,Stock284,Stock285,Stock286,Stock287,Stock288,Stock289,Stock290,
                        Stock291,Stock292,Stock293,Stock294,Stock295,Stock296,Stock297,Stock298,Stock299,Stock300,
                        Stock301,Stock302,Stock303,Stock304,Stock305,Stock306,Stock307,Stock308,Stock309,Stock310,
                        Stock311,Stock312,Stock313,Stock314,Stock315,Stock316,Stock317,Stock318,Stock319,Stock320,
                        Stock321,Stock322,Stock323,Stock324,Stock325,Stock326,Stock327,Stock328,Stock329,Stock330,
                        Stock331,Stock332,Stock333,Stock334,Stock335,Stock336,Stock337,Stock338,Stock339,Stock340,
                        Stock341,Stock342,Stock343,Stock344,Stock345,Stock346,Stock347,Stock348,Stock349,Stock350,
                        Stock351,Stock352,Stock353,Stock354,Stock355,Stock356,Stock357,Stock358,Stock359,Stock360,
                        Stock361,Stock362,Stock363,Stock364,Stock365,Stock366,Stock367,Stock368,Stock369,Stock370,
                        Stock371,Stock372,Stock373,Stock374,Stock375,Stock376,Stock377,Stock378,Stock379,Stock380,
                        Stock381,Stock382,Stock383,Stock384,Stock385,Stock386,Stock387,Stock388,Stock389,Stock390,
                        Stock391,Stock392,Stock393,Stock394,Stock395,Stock396,Stock397,Stock398,Stock399,Stock400,
                        Stock401,Stock402,Stock403,Stock404,Stock405,Stock406,Stock407,Stock408,Stock409,Stock410,
                        Stock411,Stock412,Stock413,Stock414,Stock415,Stock416,Stock417,Stock418,Stock419,Stock420,
                        Stock421,Stock422,Stock423,Stock424,Stock425,Stock426,Stock427,Stock428,Stock429,Stock430,
                        Stock431,Stock432,Stock433,Stock434,Stock435,Stock436,Stock437,Stock438,Stock439,Stock440,
                        Stock441,Stock442,Stock443,Stock444,Stock445,Stock446,Stock447,Stock448,Stock449,Stock450,
                        Stock451,Stock452,Stock453,Stock454,Stock455,Stock456,Stock457,Stock458,Stock459,Stock460,
                        Stock461,Stock462,Stock463,Stock464,Stock465,Stock466,Stock467,Stock468,Stock469,Stock470,
                        Stock471,Stock472,Stock473,Stock474,Stock475,Stock476,Stock477,Stock478,Stock479,Stock480,
                        Stock481,Stock482,Stock483,Stock484,Stock485,Stock486,Stock487,Stock488,Stock489,Stock490,
                        Stock491,Stock492,Stock493,Stock494,Stock495,Stock496,Stock497,Stock498,Stock499,Stock500,
                        Stock501,Stock502,Stock503,Stock504,Stock505,Stock506,Stock507,Stock508,Stock509,Stock510,
                        Stock511,Stock512,Stock513,Stock514,Stock515,Stock516,Stock517,Stock518,Stock519)

rm(Stock1,Stock2,Stock3,Stock4,Stock5,Stock6,Stock7,Stock8,Stock9,Stock10,Stock11,Stock12,
   Stock13,Stock14,Stock15,Stock16,Stock17,Stock18,Stock19,Stock20,Stock21,Stock22,Stock23,
   Stock24,Stock25,Stock26,Stock27,Stock28,Stock29,Stock30,Stock31,Stock32,Stock33,Stock34,
   Stock35,Stock36,Stock37,Stock38,Stock39,Stock40,Stock41,Stock42,Stock43,Stock44,Stock45,
   Stock46,Stock47,Stock48,Stock49,Stock50,Stock51,Stock52,Stock53,Stock54,Stock55,Stock56,
   Stock57,Stock58,Stock59,Stock60,Stock61,Stock62,Stock63,Stock64,Stock65,Stock66,Stock67,
   Stock68,Stock69,Stock70,Stock71,Stock72,Stock73,Stock74,Stock75,Stock76,Stock77,Stock78,
   Stock79,Stock80,Stock81,Stock82,Stock83,Stock84,Stock85,Stock86,Stock87,Stock88,Stock89,
   Stock90,Stock91,Stock92,Stock93,Stock94,Stock95,Stock96,Stock97,Stock98,Stock99,Stock100,
   Stock101,Stock102,Stock103,Stock104,Stock105,Stock106,Stock107,Stock108,Stock109,Stock110,
   Stock111,Stock112,Stock113,Stock114,Stock115,Stock116,Stock117,Stock118,Stock119,Stock120,
   Stock121,Stock122,Stock123,Stock124,Stock125,Stock126,Stock127,Stock128,Stock129,Stock130,
   Stock131,Stock132,Stock133,Stock134,Stock135,Stock136,Stock137,Stock138,Stock139,Stock140,
   Stock141,Stock142,Stock143,Stock144,Stock145,Stock146,Stock147,Stock148,Stock149,Stock150,
   Stock151,Stock152,Stock153,Stock154,Stock155,Stock156,Stock157,Stock158,Stock159,Stock160,
   Stock161,Stock162,Stock163,Stock164,Stock165,Stock166,Stock167,Stock168,Stock169,Stock170,
   Stock171,Stock172,Stock173,Stock174,Stock175,Stock176,Stock177,Stock178,Stock179,Stock180,
   Stock181,Stock182,Stock183,Stock184,Stock185,Stock186,Stock187,Stock188,Stock189,Stock190,
   Stock191,Stock192,Stock193,Stock194,Stock195,Stock196,Stock197,Stock198,Stock199,Stock200,
   Stock201,Stock202,Stock203,Stock204,Stock205,Stock206,Stock207,Stock208,Stock209,Stock210,
   Stock211,Stock212,Stock213,Stock214,Stock215,Stock216,Stock217,Stock218,Stock219,Stock220,
   Stock221,Stock222,Stock223,Stock224,Stock225,Stock226,Stock227,Stock228,Stock229,Stock230,
   Stock231,Stock232,Stock233,Stock234,Stock235,Stock236,Stock237,Stock238,Stock239,Stock240,
   Stock241,Stock242,Stock243,Stock244,Stock245,Stock246,Stock247,Stock248,Stock249,Stock250,
   Stock251,Stock252,Stock253,Stock254,Stock255,Stock256,Stock257,Stock258,Stock259,Stock260,
   Stock261,Stock262,Stock263,Stock264,Stock265,Stock266,Stock267,Stock268,Stock269,Stock270,
   Stock271,Stock272,Stock273,Stock274,Stock275,Stock276,Stock277,Stock278,Stock279,Stock280,
   Stock281,Stock282,Stock283,Stock284,Stock285,Stock286,Stock287,Stock288,Stock289,Stock290,
   Stock291,Stock292,Stock293,Stock294,Stock295,Stock296,Stock297,Stock298,Stock299,Stock300,
   Stock301,Stock302,Stock303,Stock304,Stock305,Stock306,Stock307,Stock308,Stock309,Stock310,
   Stock311,Stock312,Stock313,Stock314,Stock315,Stock316,Stock317,Stock318,Stock319,Stock320,
   Stock321,Stock322,Stock323,Stock324,Stock325,Stock326,Stock327,Stock328,Stock329,Stock330,
   Stock331,Stock332,Stock333,Stock334,Stock335,Stock336,Stock337,Stock338,Stock339,Stock340,
   Stock341,Stock342,Stock343,Stock344,Stock345,Stock346,Stock347,Stock348,Stock349,Stock350,
   Stock351,Stock352,Stock353,Stock354,Stock355,Stock356,Stock357,Stock358,Stock359,Stock360,
   Stock361,Stock362,Stock363,Stock364,Stock365,Stock366,Stock367,Stock368,Stock369,Stock370,
   Stock371,Stock372,Stock373,Stock374,Stock375,Stock376,Stock377,Stock378,Stock379,Stock380,
   Stock381,Stock382,Stock383,Stock384,Stock385,Stock386,Stock387,Stock388,Stock389,Stock390,
   Stock391,Stock392,Stock393,Stock394,Stock395,Stock396,Stock397,Stock398,Stock399,Stock400,
   Stock401,Stock402,Stock403,Stock404,Stock405,Stock406,Stock407,Stock408,Stock409,Stock410,
   Stock411,Stock412,Stock413,Stock414,Stock415,Stock416,Stock417,Stock418,Stock419,Stock420,
   Stock421,Stock422,Stock423,Stock424,Stock425,Stock426,Stock427,Stock428,Stock429,Stock430,
   Stock431,Stock432,Stock433,Stock434,Stock435,Stock436,Stock437,Stock438,Stock439,Stock440,
   Stock441,Stock442,Stock443,Stock444,Stock445,Stock446,Stock447,Stock448,Stock449,Stock450,
   Stock451,Stock452,Stock453,Stock454,Stock455,Stock456,Stock457,Stock458,Stock459,Stock460,
   Stock461,Stock462,Stock463,Stock464,Stock465,Stock466,Stock467,Stock468,Stock469,Stock470,
   Stock471,Stock472,Stock473,Stock474,Stock475,Stock476,Stock477,Stock478,Stock479,Stock480,
   Stock481,Stock482,Stock483,Stock484,Stock485,Stock486,Stock487,Stock488,Stock489,Stock490,
   Stock491,Stock492,Stock493,Stock494,Stock495,Stock496,Stock497,Stock498,Stock499,Stock500,
   Stock501,Stock502,Stock503,Stock504,Stock505,Stock506,Stock507,Stock508,Stock509,Stock510,
   Stock511,Stock512,Stock513,Stock514,Stock515,Stock516,Stock517,Stock518,Stock519,Stock520,
   Stock521,Stock522,Stock523)

PatentStock1<-PatentStock1[,c(-1)]
names(PatentStock1) <- c("PubNo","CurrOwner","PrDate","Grant","IPCmain","IPCother","ExpDate")

#Indicate missing values
PatentStock1<-na_if(PatentStock1,"n.a.")

##Read the data
Change1<-read_excel("Dataset/Patents/Change1.xlsx",sheet = "Results")
Change2<-read_excel("Dataset/Patents/Change2.xlsx",sheet = "Results")
Change3<-read_excel("Dataset/Patents/Change3.xlsx",sheet = "Results")
Change4<-read_excel("Dataset/Patents/Change4.xlsx",sheet = "Results")
Change5<-read_excel("Dataset/Patents/Change5.xlsx",sheet = "Results")
Change6<-read_excel("Dataset/Patents/Change6.xlsx",sheet = "Results")
Change7<-read_excel("Dataset/Patents/Change7.xlsx",sheet = "Results")
Change8<-read_excel("Dataset/Patents/Change8.xlsx",sheet = "Results")
Change9<-read_excel("Dataset/Patents/Change9.xlsx",sheet = "Results")
Change10<-read_excel("Dataset/Patents/Change10.xlsx",sheet = "Results")
Change11<-read_excel("Dataset/Patents/Change11.xlsx",sheet = "Results")
Change12<-read_excel("Dataset/Patents/Change12.xlsx",sheet = "Results")
Change13<-read_excel("Dataset/Patents/Change13.xlsx",sheet = "Results")
Change14<-read_excel("Dataset/Patents/Change14.xlsx",sheet = "Results")
Change15<-read_excel("Dataset/Patents/Change15.xlsx",sheet = "Results")
Change16<-read_excel("Dataset/Patents/Change16.xlsx",sheet = "Results")
Change17<-read_excel("Dataset/Patents/Change17.xlsx",sheet = "Results")
Change18<-read_excel("Dataset/Patents/Change18.xlsx",sheet = "Results")
Change19<-read_excel("Dataset/Patents/Change19.xlsx",sheet = "Results")
Change20<-read_excel("Dataset/Patents/Change20.xlsx",sheet = "Results")
Change21<-read_excel("Dataset/Patents/Change21.xlsx",sheet = "Results")
Change22<-read_excel("Dataset/Patents/Change22.xlsx",sheet = "Results")
Change23<-read_excel("Dataset/Patents/Change23.xlsx",sheet = "Results")
Change24<-read_excel("Dataset/Patents/Change24.xlsx",sheet = "Results")
Change25<-read_excel("Dataset/Patents/Change25.xlsx",sheet = "Results")
Change26<-read_excel("Dataset/Patents/Change26.xlsx",sheet = "Results")

Change1$`Transaction date`<-as.character(Change1$`Transaction date`)
Change2$`Transaction date`<-as.character(Change2$`Transaction date`)
Change3$`Transaction date`<-as.character(Change3$`Transaction date`)
Change4$`Transaction date`<-as.character(Change4$`Transaction date`)
Change5$`Transaction date`<-as.character(Change5$`Transaction date`)
Change6$`Transaction date`<-as.character(Change6$`Transaction date`)
Change7$`Transaction date`<-as.character(Change7$`Transaction date`)
Change8$`Transaction date`<-as.character(Change8$`Transaction date`)
Change9$`Transaction date`<-as.character(Change9$`Transaction date`)
Change10$`Transaction date`<-as.character(Change10$`Transaction date`)
Change11$`Transaction date`<-as.character(Change11$`Transaction date`)
Change12$`Transaction date`<-as.character(Change12$`Transaction date`)
Change13$`Transaction date`<-as.character(Change13$`Transaction date`)
Change14$`Transaction date`<-as.character(Change14$`Transaction date`)
Change15$`Transaction date`<-as.character(Change15$`Transaction date`)
Change16$`Transaction date`<-as.character(Change16$`Transaction date`)
Change17$`Transaction date`<-as.character(Change17$`Transaction date`)
Change18$`Transaction date`<-as.character(Change18$`Transaction date`)
Change19$`Transaction date`<-as.character(Change19$`Transaction date`)
Change20$`Transaction date`<-as.character(Change20$`Transaction date`)
Change21$`Transaction date`<-as.character(Change21$`Transaction date`)
Change22$`Transaction date`<-as.character(Change22$`Transaction date`)
Change23$`Transaction date`<-as.character(Change23$`Transaction date`)
Change24$`Transaction date`<-as.character(Change24$`Transaction date`)
Change25$`Transaction date`<-as.character(Change25$`Transaction date`)
Change26$`Transaction date`<-as.character(Change26$`Transaction date`)

PatentStock2<-rbind(Change1,Change2,Change3,Change4,Change5,Change6,Change7,Change8,Change9,Change10,
                    Change11,Change12,Change13,Change14,Change15, Change16, Change17,Change18,Change19,
                    Change20,Change21,Change22,Change23,Change24,Change25,Change26)

rm(Change1,Change2,Change3,Change4,Change5,Change6,Change7,Change8,Change9,Change10,
   Change11,Change12,Change13,Change14,Change15, Change16, Change17,Change18,Change19,
   Change20,Change21,Change22,Change23,Change24,Change25,Change26)

#Remove Row of Numbers and Company Names + Rename/Order Columns
PatentStock2<-PatentStock2[,c(-1)]

#Indicate missing values
PatentStock2<-na_if(PatentStock2,"n.a.")

names(PatentStock2) <- c("PubNo","PrevOwner","TransType","TransDate")
PatentStock2$PubNo<-na.locf(PatentStock2$PubNo)
PatentStock2$PrevOwner<-na.locf(PatentStock2$PrevOwner)

#Remove corporate acquisitions as this is covered through ownership changes
#Clear the data of text to retain deals that are relevant (acquisition of >25.01% or the creation of new companies)
PatentStock2<-subset(PatentStock2,PatentStock2$TransType!="Corporate acquisition")
#PatentStock2<-subset(PatentStock2,PatentStock2$TransType!="Intra-company")
PatentStock2<-subset(PatentStock2,!is.na(PatentStock2$TransDate))

PatentStock<-left_join(PatentStock1,PatentStock2,by="PubNo",na_matches="never")
rm(PatentStock2,PatentStock1)

PatentStock$Test<-ifelse(!is.na(PatentStock$CurrOwner) | !is.na(PatentStock$PrevOwner),1,0)
PatentStock<-subset(PatentStock,PatentStock$Test==1)
PatentStock$Test<-NULL

PatentStock$ExpDate<-substr(PatentStock$ExpDate,1,4)
PatentStock$PrDate<-substr(PatentStock$PrDate,1,4)
PatentStock$TransDate<-substr(PatentStock$TransDate,1,4)

PatentStock$ExpDate<-as.numeric(PatentStock$ExpDate)
PatentStock$PrDate<-as.numeric(PatentStock$PrDate)
PatentStock$TransDate<-as.numeric(PatentStock$TransDate)

#Filter for patents expired after 2011 or unknown
PatentStock$Test<-ifelse(PatentStock$ExpDate>2011 | is.na(PatentStock$ExpDate),1,0)
PatentStock<-subset(PatentStock,PatentStock$Test==1)
PatentStock$Test<-NULL

#Make IPC Codes comparable to IPC lists
PatentStock$IPCmain<-lapply(PatentStock$IPCmain,gsub, pattern=c('/'), replacement='')
PatentStock$IPCmain<-unlist(PatentStock$IPCmain)

write.csv2(PatentStock, file = "files_created_code1/PatentStock.csv",row.names = F)

## This creates a stock file of all patents in the Orbis database that were "active" between 2010 and 2020
## This can be complemented with keyword-based searches for ANY technological area

#remove unnecessary files:
rm(Full1, Full2, Full3,Full4,Full5,Full6,Full7,Full8,Full9)

## In the following a AI-search was used to establish patent ownership per year and an AI dummy
#   2. Add AI patent data and create stock files per year ####

#PatentStock<- fread("Dataset/files_created_code2/PatentStock.csv")

##Make AI variable
AI1<-read_excel("Dataset/AI/AI1.xlsx",sheet = "Results")
AI2<-read_excel("Dataset/AI/AI2.xlsx",sheet = "Results")
AI3<-read_excel("Dataset/AI/AI3.xlsx",sheet = "Results")
AI4<-read_excel("Dataset/AI/AI4.xlsx",sheet = "Results")
AI5<-read_excel("Dataset/AI/AI5.xlsx",sheet = "Results")
AI6<-read_excel("Dataset/AI/AI6.xlsx",sheet = "Results")
AI7<-read_excel("Dataset/AI/AI7.xlsx",sheet = "Results")
AI8<-read_excel("Dataset/AI/AI8.xlsx",sheet = "Results")
AI9<-read_excel("Dataset/AI/AI9.xlsx",sheet = "Results")
AI10<-read_excel("Dataset/AI/AI10.xlsx",sheet = "Results")
AI11<-read_excel("Dataset/AI/AI11.xlsx",sheet = "Results")

AI<-rbind(AI1,AI2,AI3,AI4,AI5,AI6,AI7,AI8,AI9,AI10,AI11)

rm(AI1,AI2,AI3,AI4,AI5,AI6,AI7,AI8,AI9,AI10,AI11)

#Remove Row of Numbers and Company Names + Rename/Order Columns
AI<-AI[,c(-1)]

#Indicate missing values
AI<-na_if(AI,"n.a.")

names(AI) <- c("PubNo")
AI$AI2<-AI$PubNo

PatentStock<-left_join(PatentStock,AI,by="PubNo",na_matches="never")

#Make dummy
PatentStock$AI<-ifelse(!is.na(PatentStock$AI2),1,0)
PatentStock$AI[is.na(PatentStock$AI)] <- 0
PatentStock$AI2<-NULL

##making Functional variable
Funct1<-read_excel("Dataset/AI/Funct1.xlsx",sheet = "Results")
Funct2<-read_excel("Dataset/AI/Funct2.xlsx",sheet = "Results")

Funct<-rbind(Funct1,Funct2)

rm(Funct1,Funct2)

#Remove Row of Numbers and Company Names + Rename/Order Columns
Funct<-Funct[,c(-1)]

#Indicate missing values
Funct<-na_if(Funct,"n.a.")

names(Funct) <- c("PubNo")
Funct$Funct2<-Funct$PubNo

PatentStock<-left_join(PatentStock,Funct,by="PubNo",na_matches="never")

#Make dummy
PatentStock$Funct<-ifelse(!is.na(PatentStock$Funct2),1,0)
PatentStock$Funct[is.na(PatentStock$Funct)] <- 0
PatentStock$Funct2<-NULL

rm(AI,Funct)

#write.csv2(PatentStock, file = "PatentStock_AI.csv",row.names = F)
#End of preparation

##Work with patent stock
#PatentStock<- fread("files_created_code2/PatentStock_AI.csv")

#Number of all AI patents
PatentStock<-subset(PatentStock,Funct==1)
length(which(!duplicated(PatentStock$PubNo))) #963462

#Filter for year
Stock2011<-subset(PatentStock,PatentStock$PrDate<=2011)
Stock2012<-subset(PatentStock,PatentStock$PrDate<=2012)
Stock2013<-subset(PatentStock,PatentStock$PrDate<=2013)
Stock2014<-subset(PatentStock,PatentStock$PrDate<=2014)
Stock2015<-subset(PatentStock,PatentStock$PrDate<=2015)
Stock2016<-subset(PatentStock,PatentStock$PrDate<=2016)
Stock2017<-subset(PatentStock,PatentStock$PrDate<=2017)
Stock2018<-subset(PatentStock,PatentStock$PrDate<=2018)
Stock2019<-subset(PatentStock,PatentStock$PrDate<=2019)
rm(PatentStock)

##2011
#Make as dataframe
Stock2011<-as.data.frame(Stock2011)

#Remove expired patents
Stock2011$Test<-ifelse(Stock2011$ExpDate>=2011 | is.na(Stock2011$ExpDate),1,0)
Stock2011<-subset(Stock2011,Stock2011$Test==1)
Stock2011$ExpDate<-NULL
Stock2011$Test<-NULL

#Split files
Stock2011A<-subset(Stock2011,is.na(Stock2011$TransDate))
Stock2011B<-subset(Stock2011,!is.na(Stock2011$TransDate))

#Remove duplicate publication numbers
Stock2011A$Test<-ifelse(Stock2011A$PubNo==lag(Stock2011A$PubNo) & Stock2011A$CurrOwner==lag(Stock2011A$CurrOwner),1,0)
length(which(duplicated(Stock2011B$PubNo)))  
length(which(Stock2011B$Test==1))  
Stock2011A$Test<-NULL

Stock2011A[7:9]<-NULL
Stock2011A[1]<-NULL
Stock2011A$Year<-2011
Stock2011A$Added<-0

names(Stock2011A)<-c("BvDID","PRYear","Granted","IPCmain","IPCother","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2011A<-Stock2011A[, col_order]


#Transacted patents
Stock2011B<-subset(Stock2011B,Stock2011B$TransDate>=2011)

#Remove duplicates
Stock2011B$Test<-paste(Stock2011B$PubNo,Stock2011B$CurrOwner,Stock2011B$PrevOwner,sep=" ")
Stock2011B<-Stock2011B[!duplicated(Stock2011B$Test),]

#Check for the last transaction date before this year
Stock2011B$Test<-with(Stock2011B,ave(Stock2011B$TransDate,Stock2011B$PubNo, FUN=min))
Stock2011B$Test2<-ifelse(Stock2011B$Test==Stock2011B$TransDate,1,0)
Stock2011B<-subset(Stock2011B,Stock2011B$Test2==1)
Stock2011B$Test<-ifelse(Stock2011B$PubNo==lag(Stock2011B$PubNo) & Stock2011B$TransDate!=lag(Stock2011B$TransDate),1,0)
length(which(duplicated(Stock2011B$PubNo)))  
length(which(Stock2011B$Test==1))  

Stock2011B$Test<-NULL
Stock2011B$Test2<-NULL
Stock2011B[,1:2]<-NULL
Stock2011B$TransType<-NULL
Stock2011B$TransDate<-NULL
Stock2011B$Year<-2011
Stock2011B$Added<-0

names(Stock2011B)<-c("PRYear","Granted","IPCmain","IPCother","BvDID","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2011B<-Stock2011B[,col_order]

Stock2011<-bind_rows(Stock2011A,Stock2011B)
rm(Stock2011A,Stock2011B)
length(which(Stock2011$Year>2011))

write.csv2(Stock2011, file = "files_created_code1/Stock2011.csv",row.names = F)
rm(Stock2011)


##2012
#Make as dataframe
Stock2012<-as.data.frame(Stock2012)

#Remove expired patents
Stock2012$Test<-ifelse(Stock2012$ExpDate>=2012 | is.na(Stock2012$ExpDate),1,0)
Stock2012<-subset(Stock2012,Stock2012$Test==1)
Stock2012$ExpDate<-NULL
Stock2012$Test<-NULL

#Split files
Stock2012A<-subset(Stock2012,is.na(Stock2012$TransDate))
Stock2012B<-subset(Stock2012,!is.na(Stock2012$TransDate))

#Remove duplicate publication numbers
Stock2012A$Test<-ifelse(Stock2012A$PubNo==lag(Stock2012A$PubNo) & Stock2012A$CurrOwner==lag(Stock2012A$CurrOwner),1,0)
length(which(duplicated(Stock2012B$PubNo)))  
length(which(Stock2012B$Test==1))  
Stock2012A$Test<-NULL

Stock2012A[7:9]<-NULL
Stock2012A[1]<-NULL
Stock2012A$Year<-2012
Stock2012A$Added<-0

names(Stock2012A)<-c("BvDID","PRYear","Granted","IPCmain","IPCother","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2012A<-Stock2012A[, col_order]


#Transacted patents
Stock2012B<-subset(Stock2012B,Stock2012B$TransDate>=2012)

#Remove duplicates
Stock2012B$Test<-paste(Stock2012B$PubNo,Stock2012B$CurrOwner,Stock2012B$PrevOwner,sep=" ")
Stock2012B<-Stock2012B[!duplicated(Stock2012B$Test),]

#Check for the last transaction date before this year
Stock2012B$Test<-with(Stock2012B,ave(Stock2012B$TransDate,Stock2012B$PubNo, FUN=min))
Stock2012B$Test2<-ifelse(Stock2012B$Test==Stock2012B$TransDate,1,0)
Stock2012B<-subset(Stock2012B,Stock2012B$Test2==1)
Stock2012B$Test<-ifelse(Stock2012B$PubNo==lag(Stock2012B$PubNo) & Stock2012B$TransDate!=lag(Stock2012B$TransDate),1,0)
length(which(duplicated(Stock2012B$PubNo)))  
length(which(Stock2012B$Test==1))  

Stock2012B$Test<-NULL
Stock2012B$Test2<-NULL
Stock2012B[,1:2]<-NULL
Stock2012B$TransType<-NULL
Stock2012B$TransDate<-NULL
Stock2012B$Year<-2012
Stock2012B$Added<-0

names(Stock2012B)<-c("PRYear","Granted","IPCmain","IPCother","BvDID","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2012B<-Stock2012B[,col_order]

Stock2012<-bind_rows(Stock2012A,Stock2012B)
rm(Stock2012A,Stock2012B)
length(which(Stock2012$Year>2012))

write.csv2(Stock2012, file = "files_created_code1/Stock2012.csv",row.names = F)
rm(Stock2012)

##2013
#Make as dataframe
Stock2013<-as.data.frame(Stock2013)

#Remove expired patents
Stock2013$Test<-ifelse(Stock2013$ExpDate>=2013 | is.na(Stock2013$ExpDate),1,0)
Stock2013<-subset(Stock2013,Stock2013$Test==1)
Stock2013$ExpDate<-NULL
Stock2013$Test<-NULL

#Split files
Stock2013A<-subset(Stock2013,is.na(Stock2013$TransDate))
Stock2013B<-subset(Stock2013,!is.na(Stock2013$TransDate))

#Remove duplicate publication numbers
Stock2013A$Test<-ifelse(Stock2013A$PubNo==lag(Stock2013A$PubNo) & Stock2013A$CurrOwner==lag(Stock2013A$CurrOwner),1,0)
length(which(duplicated(Stock2013B$PubNo)))  
length(which(Stock2013B$Test==1))  
Stock2013A$Test<-NULL

Stock2013A[7:9]<-NULL
Stock2013A[1]<-NULL
Stock2013A$Year<-2013
Stock2013A$Added<-0

names(Stock2013A)<-c("BvDID","PRYear","Granted","IPCmain","IPCother","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2013A<-Stock2013A[, col_order]


#Transacted patents
Stock2013B<-subset(Stock2013B,Stock2013B$TransDate>=2013)

#Remove duplicates
Stock2013B$Test<-paste(Stock2013B$PubNo,Stock2013B$CurrOwner,Stock2013B$PrevOwner,sep=" ")
Stock2013B<-Stock2013B[!duplicated(Stock2013B$Test),]

#Check for the last transaction date before this year
Stock2013B$Test<-with(Stock2013B,ave(Stock2013B$TransDate,Stock2013B$PubNo, FUN=min))
Stock2013B$Test2<-ifelse(Stock2013B$Test==Stock2013B$TransDate,1,0)
Stock2013B<-subset(Stock2013B,Stock2013B$Test2==1)
Stock2013B$Test<-ifelse(Stock2013B$PubNo==lag(Stock2013B$PubNo) & Stock2013B$TransDate!=lag(Stock2013B$TransDate),1,0)
length(which(duplicated(Stock2013B$PubNo)))  
length(which(Stock2013B$Test==1))  

Stock2013B$Test<-NULL
Stock2013B$Test2<-NULL
Stock2013B[,1:2]<-NULL
Stock2013B$TransType<-NULL
Stock2013B$TransDate<-NULL
Stock2013B$Year<-2013
Stock2013B$Added<-0

names(Stock2013B)<-c("PRYear","Granted","IPCmain","IPCother","BvDID","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2013B<-Stock2013B[,col_order]

Stock2013<-bind_rows(Stock2013A,Stock2013B)
rm(Stock2013A,Stock2013B)
length(which(Stock2013$Year>2013))

write.csv2(Stock2013, file = "files_created_code1/Stock2013.csv",row.names = F)
rm(Stock2013)

##2014
#Make as dataframe
Stock2014<-as.data.frame(Stock2014)

#Remove expired patents
Stock2014$Test<-ifelse(Stock2014$ExpDate>=2014 | is.na(Stock2014$ExpDate),1,0)
Stock2014<-subset(Stock2014,Stock2014$Test==1)
Stock2014$ExpDate<-NULL
Stock2014$Test<-NULL

#Split files
Stock2014A<-subset(Stock2014,is.na(Stock2014$TransDate))
Stock2014B<-subset(Stock2014,!is.na(Stock2014$TransDate))

#Remove duplicate publication numbers
Stock2014A$Test<-ifelse(Stock2014A$PubNo==lag(Stock2014A$PubNo) & Stock2014A$CurrOwner==lag(Stock2014A$CurrOwner),1,0)
length(which(duplicated(Stock2014B$PubNo)))  
length(which(Stock2014B$Test==1))  
Stock2014A$Test<-NULL

Stock2014A[7:9]<-NULL
Stock2014A[1]<-NULL
Stock2014A$Year<-2014
Stock2014A$Added<-0

names(Stock2014A)<-c("BvDID","PRYear","Granted","IPCmain","IPCother","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2014A<-Stock2014A[, col_order]

#Transacted patents
Stock2014B<-subset(Stock2014B,Stock2014B$TransDate>=2014)

#Remove duplicates
Stock2014B$Test<-paste(Stock2014B$PubNo,Stock2014B$CurrOwner,Stock2014B$PrevOwner,sep=" ")
Stock2014B<-Stock2014B[!duplicated(Stock2014B$Test),]

#Check for the last transaction date before this year
Stock2014B$Test<-with(Stock2014B,ave(Stock2014B$TransDate,Stock2014B$PubNo, FUN=min))
Stock2014B$Test2<-ifelse(Stock2014B$Test==Stock2014B$TransDate,1,0)
Stock2014B<-subset(Stock2014B,Stock2014B$Test2==1)
Stock2014B$Test<-ifelse(Stock2014B$PubNo==lag(Stock2014B$PubNo) & Stock2014B$TransDate!=lag(Stock2014B$TransDate),1,0)
length(which(duplicated(Stock2014B$PubNo)))  
length(which(Stock2014B$Test==1))  

Stock2014B$Test<-NULL
Stock2014B$Test2<-NULL
Stock2014B[,1:2]<-NULL
Stock2014B$TransType<-NULL
Stock2014B$TransDate<-NULL
Stock2014B$Year<-2014
Stock2014B$Added<-0

names(Stock2014B)<-c("PRYear","Granted","IPCmain","IPCother","BvDID","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2014B<-Stock2014B[,col_order]

Stock2014<-bind_rows(Stock2014A,Stock2014B)
rm(Stock2014A,Stock2014B)
length(which(Stock2014$Year>2014))

write.csv2(Stock2014, file = "files_created_code1/Stock2014.csv",row.names = F)
rm(Stock2014)

##2015
#Make as dataframe
Stock2015<-as.data.frame(Stock2015)

#Remove expired patents
Stock2015$Test<-ifelse(Stock2015$ExpDate>=2015 | is.na(Stock2015$ExpDate),1,0)
Stock2015<-subset(Stock2015,Stock2015$Test==1)
Stock2015$ExpDate<-NULL
Stock2015$Test<-NULL

#Split files
Stock2015A<-subset(Stock2015,is.na(Stock2015$TransDate))
Stock2015B<-subset(Stock2015,!is.na(Stock2015$TransDate))

#Remove duplicate publication numbers
Stock2015A$Test<-ifelse(Stock2015A$PubNo==lag(Stock2015A$PubNo) & Stock2015A$CurrOwner==lag(Stock2015A$CurrOwner),1,0)
length(which(duplicated(Stock2015B$PubNo)))  
length(which(Stock2015B$Test==1))  
Stock2015A$Test<-NULL

Stock2015A[7:9]<-NULL
Stock2015A[1]<-NULL
Stock2015A$Year<-2015
Stock2015A$Added<-0

names(Stock2015A)<-c("BvDID","PRYear","Granted","IPCmain","IPCother","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2015A<-Stock2015A[, col_order]

#Transacted patents
Stock2015B<-subset(Stock2015B,Stock2015B$TransDate>=2015)

#Remove duplicates
Stock2015B$Test<-paste(Stock2015B$PubNo,Stock2015B$CurrOwner,Stock2015B$PrevOwner,sep=" ")
Stock2015B<-Stock2015B[!duplicated(Stock2015B$Test),]

#Check for the last transaction date before this year
Stock2015B$Test<-with(Stock2015B,ave(Stock2015B$TransDate,Stock2015B$PubNo, FUN=min))
Stock2015B$Test2<-ifelse(Stock2015B$Test==Stock2015B$TransDate,1,0)
Stock2015B<-subset(Stock2015B,Stock2015B$Test2==1)
Stock2015B$Test<-ifelse(Stock2015B$PubNo==lag(Stock2015B$PubNo) & Stock2015B$TransDate!=lag(Stock2015B$TransDate),1,0)
length(which(duplicated(Stock2015B$PubNo)))  
length(which(Stock2015B$Test==1))  

Stock2015B$Test<-NULL
Stock2015B$Test2<-NULL
Stock2015B[,1:2]<-NULL
Stock2015B$TransType<-NULL
Stock2015B$TransDate<-NULL
Stock2015B$Year<-2015
Stock2015B$Added<-0

names(Stock2015B)<-c("PRYear","Granted","IPCmain","IPCother","BvDID","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2015B<-Stock2015B[,col_order]

Stock2015<-bind_rows(Stock2015A,Stock2015B)
rm(Stock2015A,Stock2015B)
length(which(Stock2015$Year>2015))

write.csv2(Stock2015, file = "files_created_code1/Stock2015.csv",row.names = F)
rm(Stock2015)

##2016
#Make as dataframe
Stock2016<-as.data.frame(Stock2016)

#Remove expired patents
Stock2016$Test<-ifelse(Stock2016$ExpDate>=2016 | is.na(Stock2016$ExpDate),1,0)
Stock2016<-subset(Stock2016,Stock2016$Test==1)
Stock2016$ExpDate<-NULL
Stock2016$Test<-NULL

#Split files
Stock2016A<-subset(Stock2016,is.na(Stock2016$TransDate))
Stock2016B<-subset(Stock2016,!is.na(Stock2016$TransDate))

#Remove duplicate publication numbers
Stock2016A$Test<-ifelse(Stock2016A$PubNo==lag(Stock2016A$PubNo) & Stock2016A$CurrOwner==lag(Stock2016A$CurrOwner),1,0)
length(which(duplicated(Stock2016B$PubNo)))  
length(which(Stock2016B$Test==1))  
Stock2016A$Test<-NULL

Stock2016A[7:9]<-NULL
Stock2016A[1]<-NULL
Stock2016A$Year<-2016
Stock2016A$Added<-0

names(Stock2016A)<-c("BvDID","PRYear","Granted","IPCmain","IPCother","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2016A<-Stock2016A[, col_order]

#Transacted patents
Stock2016B<-subset(Stock2016B,Stock2016B$TransDate>=2016)

#Remove duplicates
Stock2016B$Test<-paste(Stock2016B$PubNo,Stock2016B$CurrOwner,Stock2016B$PrevOwner,sep=" ")
Stock2016B<-Stock2016B[!duplicated(Stock2016B$Test),]

#Check for the last transaction date before this year
Stock2016B$Test<-with(Stock2016B,ave(Stock2016B$TransDate,Stock2016B$PubNo, FUN=min))
Stock2016B$Test2<-ifelse(Stock2016B$Test==Stock2016B$TransDate,1,0)
Stock2016B<-subset(Stock2016B,Stock2016B$Test2==1)
Stock2016B$Test<-ifelse(Stock2016B$PubNo==lag(Stock2016B$PubNo) & Stock2016B$TransDate!=lag(Stock2016B$TransDate),1,0)
length(which(duplicated(Stock2016B$PubNo)))  
length(which(Stock2016B$Test==1))  

Stock2016B$Test<-NULL
Stock2016B$Test2<-NULL
Stock2016B[,1:2]<-NULL
Stock2016B$TransType<-NULL
Stock2016B$TransDate<-NULL
Stock2016B$Year<-2016
Stock2016B$Added<-0

names(Stock2016B)<-c("PRYear","Granted","IPCmain","IPCother","BvDID","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2016B<-Stock2016B[,col_order]

Stock2016<-bind_rows(Stock2016A,Stock2016B)
rm(Stock2016A,Stock2016B)
length(which(Stock2016$Year>2016))

write.csv2(Stock2016, file = "files_created_code1/Stock2016.csv",row.names = F)
rm(Stock2016)

##2017
#Make as dataframe
Stock2017<-as.data.frame(Stock2017)

#Remove expired patents
Stock2017$Test<-ifelse(Stock2017$ExpDate>=2017 | is.na(Stock2017$ExpDate),1,0)
Stock2017<-subset(Stock2017,Stock2017$Test==1)
Stock2017$ExpDate<-NULL
Stock2017$Test<-NULL

#Split files
Stock2017A<-subset(Stock2017,is.na(Stock2017$TransDate))
Stock2017B<-subset(Stock2017,!is.na(Stock2017$TransDate))

#Remove duplicate publication numbers
Stock2017A$Test<-ifelse(Stock2017A$PubNo==lag(Stock2017A$PubNo) & Stock2017A$CurrOwner==lag(Stock2017A$CurrOwner),1,0)
length(which(duplicated(Stock2017B$PubNo)))  
length(which(Stock2017B$Test==1))  
Stock2017A$Test<-NULL

Stock2017A[7:9]<-NULL
Stock2017A[1]<-NULL
Stock2017A$Year<-2017
Stock2017A$Added<-0

names(Stock2017A)<-c("BvDID","PRYear","Granted","IPCmain","IPCother","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2017A<-Stock2017A[, col_order]

#Transacted patents
Stock2017B<-subset(Stock2017B,Stock2017B$TransDate>=2017)

#Remove duplicates
Stock2017B$Test<-paste(Stock2017B$PubNo,Stock2017B$CurrOwner,Stock2017B$PrevOwner,sep=" ")
Stock2017B<-Stock2017B[!duplicated(Stock2017B$Test),]

#Check for the last transaction date before this year
Stock2017B$Test<-with(Stock2017B,ave(Stock2017B$TransDate,Stock2017B$PubNo, FUN=min))
Stock2017B$Test2<-ifelse(Stock2017B$Test==Stock2017B$TransDate,1,0)
Stock2017B<-subset(Stock2017B,Stock2017B$Test2==1)
Stock2017B$Test<-ifelse(Stock2017B$PubNo==lag(Stock2017B$PubNo) & Stock2017B$TransDate!=lag(Stock2017B$TransDate),1,0)
length(which(duplicated(Stock2017B$PubNo)))  
length(which(Stock2017B$Test==1))  

Stock2017B$Test<-NULL
Stock2017B$Test2<-NULL
Stock2017B[,1:2]<-NULL
Stock2017B$TransType<-NULL
Stock2017B$TransDate<-NULL
Stock2017B$Year<-2017
Stock2017B$Added<-0

names(Stock2017B)<-c("PRYear","Granted","IPCmain","IPCother","BvDID","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2017B<-Stock2017B[,col_order]

Stock2017<-bind_rows(Stock2017A,Stock2017B)
rm(Stock2017A,Stock2017B)
length(which(Stock2017$Year>2017))

write.csv2(Stock2017, file = "files_created_code1/Stock2017.csv",row.names = F)
rm(Stock2017)

##2018
#Make as dataframe
Stock2018<-as.data.frame(Stock2018)

#Remove expired patents
Stock2018$Test<-ifelse(Stock2018$ExpDate>=2018 | is.na(Stock2018$ExpDate),1,0)
Stock2018<-subset(Stock2018,Stock2018$Test==1)
Stock2018$ExpDate<-NULL
Stock2018$Test<-NULL

#Split files
Stock2018A<-subset(Stock2018,is.na(Stock2018$TransDate))
Stock2018B<-subset(Stock2018,!is.na(Stock2018$TransDate))

#Remove duplicate publication numbers
Stock2018A$Test<-ifelse(Stock2018A$PubNo==lag(Stock2018A$PubNo) & Stock2018A$CurrOwner==lag(Stock2018A$CurrOwner),1,0)
length(which(duplicated(Stock2018B$PubNo)))  
length(which(Stock2018B$Test==1))  
Stock2018A$Test<-NULL

Stock2018A[7:9]<-NULL
Stock2018A[1]<-NULL
Stock2018A$Year<-2018
Stock2018A$Added<-0

names(Stock2018A)<-c("BvDID","PRYear","Granted","IPCmain","IPCother","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2018A<-Stock2018A[, col_order]

#Transacted patents
Stock2018B<-subset(Stock2018B,Stock2018B$TransDate>=2018)

#Remove duplicates
Stock2018B$Test<-paste(Stock2018B$PubNo,Stock2018B$CurrOwner,Stock2018B$PrevOwner,sep=" ")
Stock2018B<-Stock2018B[!duplicated(Stock2018B$Test),]

#Check for the last transaction date before this year
Stock2018B$Test<-with(Stock2018B,ave(Stock2018B$TransDate,Stock2018B$PubNo, FUN=min))
Stock2018B$Test2<-ifelse(Stock2018B$Test==Stock2018B$TransDate,1,0)
Stock2018B<-subset(Stock2018B,Stock2018B$Test2==1)
Stock2018B$Test<-ifelse(Stock2018B$PubNo==lag(Stock2018B$PubNo) & Stock2018B$TransDate!=lag(Stock2018B$TransDate),1,0)
length(which(duplicated(Stock2018B$PubNo)))  
length(which(Stock2018B$Test==1))  

Stock2018B$Test<-NULL
Stock2018B$Test2<-NULL
Stock2018B[,1:2]<-NULL
Stock2018B$TransType<-NULL
Stock2018B$TransDate<-NULL
Stock2018B$Year<-2018
Stock2018B$Added<-0

names(Stock2018B)<-c("PRYear","Granted","IPCmain","IPCother","BvDID","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2018B<-Stock2018B[,col_order]

Stock2018<-bind_rows(Stock2018A,Stock2018B)
rm(Stock2018A,Stock2018B)
length(which(Stock2018$Year>2018))

write.csv2(Stock2018, file = "files_created_code1/Stock2018.csv",row.names = F)
rm(Stock2018)

##2019
#Make as dataframe
Stock2019<-as.data.frame(Stock2019)

#Remove expired patents
Stock2019$Test<-ifelse(Stock2019$ExpDate>=2019 | is.na(Stock2019$ExpDate),1,0)
Stock2019<-subset(Stock2019,Stock2019$Test==1)
Stock2019$ExpDate<-NULL
Stock2019$Test<-NULL

#Split files
Stock2019A<-subset(Stock2019,is.na(Stock2019$TransDate))
Stock2019B<-subset(Stock2019,!is.na(Stock2019$TransDate))

#Remove duplicate publication numbers
Stock2019A$Test<-ifelse(Stock2019A$PubNo==lag(Stock2019A$PubNo) & Stock2019A$CurrOwner==lag(Stock2019A$CurrOwner),1,0)
length(which(duplicated(Stock2019B$PubNo)))  
length(which(Stock2019B$Test==1))  
Stock2019A$Test<-NULL

Stock2019A[7:9]<-NULL
Stock2019A[1]<-NULL
Stock2019A$Year<-2019
Stock2019A$Added<-0

names(Stock2019A)<-c("BvDID","PRYear","Granted","IPCmain","IPCother","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2019A<-Stock2019A[, col_order]

#Transacted patents
Stock2019B<-subset(Stock2019B,Stock2019B$TransDate>=2019)

#Remove duplicates
Stock2019B$Test<-paste(Stock2019B$PubNo,Stock2019B$CurrOwner,Stock2019B$PrevOwner,sep=" ")
Stock2019B<-Stock2019B[!duplicated(Stock2019B$Test),]

#Check for the last transaction date before this year
Stock2019B$Test<-with(Stock2019B,ave(Stock2019B$TransDate,Stock2019B$PubNo, FUN=min))
Stock2019B$Test2<-ifelse(Stock2019B$Test==Stock2019B$TransDate,1,0)
Stock2019B<-subset(Stock2019B,Stock2019B$Test2==1)
Stock2019B$Test<-ifelse(Stock2019B$PubNo==lag(Stock2019B$PubNo) & Stock2019B$TransDate!=lag(Stock2019B$TransDate),1,0)
length(which(duplicated(Stock2019B$PubNo)))  
length(which(Stock2019B$Test==1))  

Stock2019B$Test<-NULL
Stock2019B$Test2<-NULL
Stock2019B[,1:2]<-NULL
Stock2019B$TransType<-NULL
Stock2019B$TransDate<-NULL
Stock2019B$Year<-2019
Stock2019B$Added<-0

names(Stock2019B)<-c("PRYear","Granted","IPCmain","IPCother","BvDID","AI","Funct","Year","Added")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")
Stock2019B<-Stock2019B[,col_order]

Stock2019<-bind_rows(Stock2019A,Stock2019B)
rm(Stock2019A,Stock2019B)
length(which(Stock2019$Year>2019))

write.csv2(Stock2019, file = "files_created_code1/Stock2019.csv",row.names = F)
rm(Stock2019)


## The stock files attach each patent to the owning company for the given year
## Patents are NOT aggregated by company yet!
## An added variable was established if the patent was added in this specific year This can be complemented with keyword-based searches for ANY technological area
## In the following a patents are aggregated by company with respect to different patenting characteristics




#   3. Start creating patent variables for each year ####
IPC<-read_excel("Dataset/AI.xlsx",sheet = "Sheet1") #deleted extra 'Dataset'

## Patent variables for 2011
Patents2011<- fread("files_created_code1/Stock2011.csv")

#Create necessary variables
Patents2011$Company<-Patents2011$BvDID

##Create a variable of the number of Patents patents per company
Patents2011$Pat<-ifelse(Patents2011$BvDID==Patents2011$Company,1,0)
Patents2011$Pat<-as.numeric(Patents2011$Pat)
Patents2011$Pat[is.na(Patents2011$Pat)] <- 0
Patents2011$NoPat<- with(Patents2011, ave(Patents2011$Pat,Patents2011$Company, FUN=sum))

#Sum number of applications per firm
Patents2011$App1<-ifelse(Patents2011$Year==Patents2011$PRYear,1,0)
Patents2011$App1[is.na(Patents2011$App1)] <- 0
Patents2011$App<-with(Patents2011,ave(Patents2011$App1,Patents2011$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2011$GF1<-ifelse(Patents2011$Added==2,Patents2011$Pat,0)
Patents2011$GF1[is.na(Patents2011$GF1)] <- 0
Patents2011$GF<-with(Patents2011,ave(Patents2011$GF1,Patents2011$Company, FUN=sum,na.rm=TRUE))
Patents2011$BF1<-ifelse(Patents2011$Added==3,Patents2011$Pat,0)
Patents2011$BF1[is.na(Patents2011$BF1)] <- 0
Patents2011$BF<-with(Patents2011,ave(Patents2011$BF1,Patents2011$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2011$SDPat<-Patents2011$App-Patents2011$GF-Patents2011$BF

##Create a variable of the number of granted Patents 2011 patents per company
Patents2011$Grant1<-ifelse(Patents2011$Granted=="Yes",1,0)
Patents2011$Grant1<-as.numeric(Patents2011$Grant1)
Patents2011$Grant1[is.na(Patents2011$Grant1)] <- 0
Patents2011$GrantPat<- with(Patents2011, ave(Patents2011$Grant1,Patents2011$Company, FUN=sum))

#Filter for technological area of invention
Patents2011$IPC<-substr(Patents2011$IPCmain,1,4)

#HighTech (IPC Code XX)
Patents2011$HighTech1<-ifelse(Patents2011$IPC=="B41J"|Patents2011$IPC=="G06C"|Patents2011$IPC=="G06D"|
                                 Patents2011$IPC=="G06E"|Patents2011$IPC=="G11C"|Patents2011$IPC=="G06Q"|
                                 Patents2011$IPC=="G06G"|Patents2011$IPC=="G06J"|Patents2011$IPC=="G06F"|
                                 Patents2011$IPC=="G06M"|Patents2011$IPC=="B64B"|Patents2011$IPC=="B64C"|
                                 Patents2011$IPC=="B64D"|Patents2011$IPC=="B64F"|Patents2011$IPC=="B64G"|
                                 Patents2011$IPC=="C40B"|Patents2011$IPC=="C40B"|Patents2011$IPC=="C12P"|
                                 Patents2011$IPC=="C12Q"|Patents2011$IPC=="H01S"|Patents2011$IPC=="H01L"|
                                 Patents2011$IPC=="H04B"|Patents2011$IPC=="H04H"|Patents2011$IPC=="H04J"|
                                 Patents2011$IPC=="H04K"|Patents2011$IPC=="H04L"|Patents2011$IPC=="H04M"|
                                 Patents2011$IPC=="H04N"|Patents2011$IPC=="H04Q"|Patents2011$IPC=="H04R"|
                                 Patents2011$IPC=="H04S",1,0)
Patents2011$HighTech1[is.na(Patents2011$HighTech1)] <- 0
Patents2011$HighTech1<-as.numeric(Patents2011$HighTech1)
Patents2011$HighTech<- with(Patents2011, ave(Patents2011$HighTech1,Patents2011$Company, FUN=sum))


##AI Patents
##Create a variable of the number of Patents patents per company
Patents2011$AIPat<-ifelse(Patents2011$AI==1,1,0)
Patents2011$AIPat<-as.numeric(Patents2011$AIPat)
Patents2011$AIPat[is.na(Patents2011$AIPat)] <- 0
Patents2011$AINoPat<- with(Patents2011, ave(Patents2011$AIPat,Patents2011$Company, FUN=sum))

#Sum number of AI applications per firm
Patents2011$AIApp1<-ifelse(Patents2011$Year==Patents2011$PRYear & Patents2011$AI==1,1,0)
Patents2011$AIApp1[is.na(Patents2011$AIApp1)] <- 0
Patents2011$AIApp<-with(Patents2011,ave(Patents2011$AIApp1,Patents2011$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2011$AIGF1<-ifelse(Patents2011$Added==2,Patents2011$AIPat,0)
Patents2011$AIGF1[is.na(Patents2011$AIGF1)] <- 0
Patents2011$AIGF<-with(Patents2011,ave(Patents2011$AIGF1,Patents2011$Company, FUN=sum,na.rm=TRUE))
Patents2011$AIBF1<-ifelse(Patents2011$Added==3,Patents2011$AIPat,0)
Patents2011$AIBF1[is.na(Patents2011$AIBF1)] <- 0
Patents2011$AIBF<-with(Patents2011,ave(Patents2011$AIBF1,Patents2011$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2011$AISDPat<-Patents2011$AIApp-Patents2011$AIGF-Patents2011$AIBF

##Create a variable of the number of granted Patents 2011 patents per company
Patents2011$AIGrant1<-ifelse(Patents2011$Granted=="Yes" & Patents2011$AI==1,1,0)
Patents2011$AIGrant1<-as.numeric(Patents2011$AIGrant1)
Patents2011$AIGrant1[is.na(Patents2011$AIGrant1)] <- 0
Patents2011$AIGrantPat<- with(Patents2011, ave(Patents2011$AIGrant1,Patents2011$Company, FUN=sum))

#Technical area (CPC code G means physics - describes rather basic research
Patents2011$Tech<-ifelse(Patents2011$AI==1 & Patents2011$Funct!=1,1,0)
Patents2011$Tech[is.na(Patents2011$Tech)] <- 0
Patents2011$Tech<-as.numeric(Patents2011$Tech)
Patents2011$TechPat<- with(Patents2011, ave(Patents2011$Tech,Patents2011$Company, FUN=sum))

#Technical Patent Applications
Patents2011$AppTech1<-ifelse(Patents2011$AI==1&Patents2011$Funct!=1&Patents2011$Year==Patents2011$PRYear,1,0)
Patents2011$AppTech1[is.na(Patents2011$AppTech1)] <- 0
Patents2011$AppTech<-with(Patents2011,ave(Patents2011$AppTech1,Patents2011$Company, FUN=sum,na.rm=TRUE))

#Functional area (CPC code not G means the application of Patents2011 technologies in other technical areas)
Patents2011$Funct1<-ifelse(Patents2011$AI==1 & Patents2011$Funct==1,1,0)
Patents2011$Funct1[is.na(Patents2011$Funct1)] <- 0
Patents2011$Funct1<-as.numeric(Patents2011$Funct)
Patents2011$FunctPat<- with(Patents2011, ave(Patents2011$Funct,Patents2011$Company, FUN=sum))

#Functional Patent Applications
Patents2011$AppFunct1<-ifelse(Patents2011$AI==1 & Patents2011$Funct==1&Patents2011$Year==Patents2011$PRYear,1,0)
Patents2011$AppFunct1[is.na(Patents2011$AppFunct1)] <- 0
Patents2011$AppFunct<-with(Patents2011,ave(Patents2011$AppFunct1,Patents2011$Company, FUN=sum,na.rm=TRUE))

#Remove duplicate companies
Patents2011<-Patents2011[!duplicated(Patents2011$BvDID),]

#Remove unnecessary data
Patents2011 <- Patents2011[,c(-3,-4,-5,-6,-7,-8,-9,-10,-11,-13,-15,-17,-20,-22,-23,-25,-27,-29,-31,-34,-36,-38,-40,-42)]

#Reorder dataset
names(Patents2011)<-c("Subsidiaries","Year","NoPat","App","GF","BF","SDPat","GrantPat","HighTech","AINoPat","AIApp","AIGF","AIBF",
                      "AISDPat","AIGrantPat","TechPat","AppTech","FunctPat","AppFunct")

##Patent data descriptives
sum(Patents2011$NoPat,na.rm = TRUE)
sum(Patents2011$App,na.rm = TRUE)
sum(Patents2011$GF,na.rm = TRUE)
sum(Patents2011$BF,na.rm = TRUE)
sum(Patents2011$SDPat,na.rm = TRUE)
sum(Patents2011$GrantPat,na.rm = TRUE)
sum(Patents2011$HighTech,na.rm = TRUE)
sum(Patents2011$AINoPat,na.rm = TRUE)
sum(Patents2011$AIApp,na.rm = TRUE)
sum(Patents2011$AIGF,na.rm = TRUE)
sum(Patents2011$AIBF,na.rm = TRUE)
sum(Patents2011$AISDPat,na.rm = TRUE)
sum(Patents2011$AIGrantPat,na.rm = TRUE)
sum(Patents2011$TechPat,na.rm = TRUE)
sum(Patents2011$AppTech,na.rm = TRUE)
sum(Patents2011$FunctPat,na.rm = TRUE)
sum(Patents2011$AppFunct,na.rm = TRUE)

write.csv2(Patents2011, file = "files_created_code1/Patents2011.csv",row.names = F)
rm(Patents2011)


## Patent variables for 2012
Patents2012<- fread("files_created_code1/Stock2012.csv")

#Create necessary variables
Patents2012$Company<-Patents2012$BvDID

##Create a variable of the number of Patents patents per company
Patents2012$Pat<-ifelse(Patents2012$BvDID==Patents2012$Company,1,0)
Patents2012$Pat<-as.numeric(Patents2012$Pat)
Patents2012$Pat[is.na(Patents2012$Pat)] <- 0
Patents2012$NoPat<- with(Patents2012, ave(Patents2012$Pat,Patents2012$Company, FUN=sum))

#Sum number of applications per firm
Patents2012$App1<-ifelse(Patents2012$Year==Patents2012$PRYear,1,0)
Patents2012$App1[is.na(Patents2012$App1)] <- 0
Patents2012$App<-with(Patents2012,ave(Patents2012$App1,Patents2012$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2012$GF1<-ifelse(Patents2012$Added==2,Patents2012$Pat,0)
Patents2012$GF1[is.na(Patents2012$GF1)] <- 0
Patents2012$GF<-with(Patents2012,ave(Patents2012$GF1,Patents2012$Company, FUN=sum,na.rm=TRUE))
Patents2012$BF1<-ifelse(Patents2012$Added==3,Patents2012$Pat,0)
Patents2012$BF1[is.na(Patents2012$BF1)] <- 0
Patents2012$BF<-with(Patents2012,ave(Patents2012$BF1,Patents2012$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2012$SDPat<-Patents2012$App-Patents2012$GF-Patents2012$BF

##Create a variable of the number of granted Patents 2012 patents per company
Patents2012$Grant1<-ifelse(Patents2012$Granted=="Yes",1,0)
Patents2012$Grant1<-as.numeric(Patents2012$Grant1)
Patents2012$Grant1[is.na(Patents2012$Grant1)] <- 0
Patents2012$GrantPat<- with(Patents2012, ave(Patents2012$Grant1,Patents2012$Company, FUN=sum))

#Filter for technological area of invention
Patents2012$IPC<-substr(Patents2012$IPCmain,1,4)

#HighTech (IPC Code XX)
Patents2012$HighTech1<-ifelse(Patents2012$IPC=="B41J"|Patents2012$IPC=="G06C"|Patents2012$IPC=="G06D"|
                                 Patents2012$IPC=="G06E"|Patents2012$IPC=="G11C"|Patents2012$IPC=="G06Q"|
                                 Patents2012$IPC=="G06G"|Patents2012$IPC=="G06J"|Patents2012$IPC=="G06F"|
                                 Patents2012$IPC=="G06M"|Patents2012$IPC=="B64B"|Patents2012$IPC=="B64C"|
                                 Patents2012$IPC=="B64D"|Patents2012$IPC=="B64F"|Patents2012$IPC=="B64G"|
                                 Patents2012$IPC=="C40B"|Patents2012$IPC=="C40B"|Patents2012$IPC=="C12P"|
                                 Patents2012$IPC=="C12Q"|Patents2012$IPC=="H01S"|Patents2012$IPC=="H01L"|
                                 Patents2012$IPC=="H04B"|Patents2012$IPC=="H04H"|Patents2012$IPC=="H04J"|
                                 Patents2012$IPC=="H04K"|Patents2012$IPC=="H04L"|Patents2012$IPC=="H04M"|
                                 Patents2012$IPC=="H04N"|Patents2012$IPC=="H04Q"|Patents2012$IPC=="H04R"|
                                 Patents2012$IPC=="H04S",1,0)
Patents2012$HighTech1[is.na(Patents2012$HighTech1)] <- 0
Patents2012$HighTech1<-as.numeric(Patents2012$HighTech1)
Patents2012$HighTech<- with(Patents2012, ave(Patents2012$HighTech1,Patents2012$Company, FUN=sum))

##AI Patents
##Create a variable of the number of Patents patents per company
Patents2012$AIPat<-ifelse(Patents2012$AI==1,1,0)
Patents2012$AIPat<-as.numeric(Patents2012$AIPat)
Patents2012$AIPat[is.na(Patents2012$AIPat)] <- 0
Patents2012$AINoPat<- with(Patents2012, ave(Patents2012$AIPat,Patents2012$Company, FUN=sum))

#Sum number of AI applications per firm
Patents2012$AIApp1<-ifelse(Patents2012$Year==Patents2012$PRYear & Patents2012$AI==1,1,0)
Patents2012$AIApp1[is.na(Patents2012$AIApp1)] <- 0
Patents2012$AIApp<-with(Patents2012,ave(Patents2012$AIApp1,Patents2012$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2012$AIGF1<-ifelse(Patents2012$Added==2,Patents2012$AIPat,0)
Patents2012$AIGF1[is.na(Patents2012$AIGF1)] <- 0
Patents2012$AIGF<-with(Patents2012,ave(Patents2012$AIGF1,Patents2012$Company, FUN=sum,na.rm=TRUE))
Patents2012$AIBF1<-ifelse(Patents2012$Added==3,Patents2012$AIPat,0)
Patents2012$AIBF1[is.na(Patents2012$AIBF1)] <- 0
Patents2012$AIBF<-with(Patents2012,ave(Patents2012$AIBF1,Patents2012$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2012$AISDPat<-Patents2012$AIApp-Patents2012$AIGF-Patents2012$AIBF

##Create a variable of the number of granted Patents 2012 patents per company
Patents2012$AIGrant1<-ifelse(Patents2012$Granted=="Yes" & Patents2012$AI==1,1,0)
Patents2012$AIGrant1<-as.numeric(Patents2012$AIGrant1)
Patents2012$AIGrant1[is.na(Patents2012$AIGrant1)] <- 0
Patents2012$AIGrantPat<- with(Patents2012, ave(Patents2012$AIGrant1,Patents2012$Company, FUN=sum))

#Technical area (CPC code G means physics - describes rather basic research
Patents2012$Tech<-ifelse(Patents2012$AI==1 & Patents2012$Funct!=1,1,0)
Patents2012$Tech[is.na(Patents2012$Tech)] <- 0
Patents2012$Tech<-as.numeric(Patents2012$Tech)
Patents2012$TechPat<- with(Patents2012, ave(Patents2012$Tech,Patents2012$Company, FUN=sum))

#Technical Patent Applications
Patents2012$AppTech1<-ifelse(Patents2012$AI==1&Patents2012$Funct!=1&Patents2012$Year==Patents2012$PRYear,1,0)
Patents2012$AppTech1[is.na(Patents2012$AppTech1)] <- 0
Patents2012$AppTech<-with(Patents2012,ave(Patents2012$AppTech1,Patents2012$Company, FUN=sum,na.rm=TRUE))

#Functional area (CPC code not G means the application of Patents2012 technologies in other technical areas)
Patents2012$Funct1<-ifelse(Patents2012$AI==1 & Patents2012$Funct==1,1,0)
Patents2012$Funct1[is.na(Patents2012$Funct1)] <- 0
Patents2012$Funct1<-as.numeric(Patents2012$Funct)
Patents2012$FunctPat<- with(Patents2012, ave(Patents2012$Funct,Patents2012$Company, FUN=sum))

#Functional Patent Applications
Patents2012$AppFunct1<-ifelse(Patents2012$AI==1 & Patents2012$Funct==1&Patents2012$Year==Patents2012$PRYear,1,0)
Patents2012$AppFunct1[is.na(Patents2012$AppFunct1)] <- 0
Patents2012$AppFunct<-with(Patents2012,ave(Patents2012$AppFunct1,Patents2012$Company, FUN=sum,na.rm=TRUE))

#Remove duplicate companies
Patents2012<-Patents2012[!duplicated(Patents2012$BvDID),]

#Remove unnecessary data
Patents2012 <- Patents2012[,c(-3,-4,-5,-6,-7,-8,-9,-10,-11,-13,-15,-17,-20,-22,-23,-25,-27,-29,-31,-34,-36,-38,-40,-42)]

#Reorder dataset
names(Patents2012)<-c("Subsidiaries","Year","NoPat","App","GF","BF","SDPat","GrantPat","HighTech","AINoPat","AIApp","AIGF","AIBF",
                      "AISDPat","AIGrantPat","TechPat","AppTech","FunctPat","AppFunct")

##Patent data descriptives
sum(Patents2012$NoPat,na.rm = TRUE)
sum(Patents2012$App,na.rm = TRUE)
sum(Patents2012$GF,na.rm = TRUE)
sum(Patents2012$BF,na.rm = TRUE)
sum(Patents2012$SDPat,na.rm = TRUE)
sum(Patents2012$GrantPat,na.rm = TRUE)
sum(Patents2012$HighTech,na.rm = TRUE)
sum(Patents2012$AINoPat,na.rm = TRUE)
sum(Patents2012$AIApp,na.rm = TRUE)
sum(Patents2012$AIGF,na.rm = TRUE)
sum(Patents2012$AIBF,na.rm = TRUE)
sum(Patents2012$AISDPat,na.rm = TRUE)
sum(Patents2012$AIGrantPat,na.rm = TRUE)
sum(Patents2012$TechPat,na.rm = TRUE)
sum(Patents2012$AppTech,na.rm = TRUE)
sum(Patents2012$FunctPat,na.rm = TRUE)
sum(Patents2012$AppFunct,na.rm = TRUE)

write.csv2(Patents2012, file = "files_created_code1/Patents2012.csv",row.names = F)
rm(Patents2012)


## Patent variables for 2013
Patents2013<- fread("files_created_code1/Stock2013.csv")

#Create necessary variables
Patents2013$Company<-Patents2013$BvDID

##Create a variable of the number of Patents patents per company
Patents2013$Pat<-ifelse(Patents2013$BvDID==Patents2013$Company,1,0)
Patents2013$Pat<-as.numeric(Patents2013$Pat)
Patents2013$Pat[is.na(Patents2013$Pat)] <- 0
Patents2013$NoPat<- with(Patents2013, ave(Patents2013$Pat,Patents2013$Company, FUN=sum))

#Sum number of applications per firm
Patents2013$App1<-ifelse(Patents2013$Year==Patents2013$PRYear,1,0)
Patents2013$App1[is.na(Patents2013$App1)] <- 0
Patents2013$App<-with(Patents2013,ave(Patents2013$App1,Patents2013$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2013$GF1<-ifelse(Patents2013$Added==2,Patents2013$Pat,0)
Patents2013$GF1[is.na(Patents2013$GF1)] <- 0
Patents2013$GF<-with(Patents2013,ave(Patents2013$GF1,Patents2013$Company, FUN=sum,na.rm=TRUE))
Patents2013$BF1<-ifelse(Patents2013$Added==3,Patents2013$Pat,0)
Patents2013$BF1[is.na(Patents2013$BF1)] <- 0
Patents2013$BF<-with(Patents2013,ave(Patents2013$BF1,Patents2013$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2013$SDPat<-Patents2013$App-Patents2013$GF-Patents2013$BF

##Create a variable of the number of granted Patents 2013 patents per company
Patents2013$Grant1<-ifelse(Patents2013$Granted=="Yes",1,0)
Patents2013$Grant1<-as.numeric(Patents2013$Grant1)
Patents2013$Grant1[is.na(Patents2013$Grant1)] <- 0
Patents2013$GrantPat<- with(Patents2013, ave(Patents2013$Grant1,Patents2013$Company, FUN=sum))

#Filter for technological area of invention
Patents2013$IPC<-substr(Patents2013$IPCmain,1,4)

#HighTech (IPC Code XX)
Patents2013$HighTech1<-ifelse(Patents2013$IPC=="B41J"|Patents2013$IPC=="G06C"|Patents2013$IPC=="G06D"|
                                 Patents2013$IPC=="G06E"|Patents2013$IPC=="G11C"|Patents2013$IPC=="G06Q"|
                                 Patents2013$IPC=="G06G"|Patents2013$IPC=="G06J"|Patents2013$IPC=="G06F"|
                                 Patents2013$IPC=="G06M"|Patents2013$IPC=="B64B"|Patents2013$IPC=="B64C"|
                                 Patents2013$IPC=="B64D"|Patents2013$IPC=="B64F"|Patents2013$IPC=="B64G"|
                                 Patents2013$IPC=="C40B"|Patents2013$IPC=="C40B"|Patents2013$IPC=="C12P"|
                                 Patents2013$IPC=="C12Q"|Patents2013$IPC=="H01S"|Patents2013$IPC=="H01L"|
                                 Patents2013$IPC=="H04B"|Patents2013$IPC=="H04H"|Patents2013$IPC=="H04J"|
                                 Patents2013$IPC=="H04K"|Patents2013$IPC=="H04L"|Patents2013$IPC=="H04M"|
                                 Patents2013$IPC=="H04N"|Patents2013$IPC=="H04Q"|Patents2013$IPC=="H04R"|
                                 Patents2013$IPC=="H04S",1,0)
Patents2013$HighTech1[is.na(Patents2013$HighTech1)] <- 0
Patents2013$HighTech1<-as.numeric(Patents2013$HighTech1)
Patents2013$HighTech<- with(Patents2013, ave(Patents2013$HighTech1,Patents2013$Company, FUN=sum))

##AI Patents
##Create a variable of the number of Patents patents per company
Patents2013$AIPat<-ifelse(Patents2013$AI==1,1,0)
Patents2013$AIPat<-as.numeric(Patents2013$AIPat)
Patents2013$AIPat[is.na(Patents2013$AIPat)] <- 0
Patents2013$AINoPat<- with(Patents2013, ave(Patents2013$AIPat,Patents2013$Company, FUN=sum))

#Sum number of AI applications per firm
Patents2013$AIApp1<-ifelse(Patents2013$Year==Patents2013$PRYear & Patents2013$AI==1,1,0)
Patents2013$AIApp1[is.na(Patents2013$AIApp1)] <- 0
Patents2013$AIApp<-with(Patents2013,ave(Patents2013$AIApp1,Patents2013$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2013$AIGF1<-ifelse(Patents2013$Added==2,Patents2013$AIPat,0)
Patents2013$AIGF1[is.na(Patents2013$AIGF1)] <- 0
Patents2013$AIGF<-with(Patents2013,ave(Patents2013$AIGF1,Patents2013$Company, FUN=sum,na.rm=TRUE))
Patents2013$AIBF1<-ifelse(Patents2013$Added==3,Patents2013$AIPat,0)
Patents2013$AIBF1[is.na(Patents2013$AIBF1)] <- 0
Patents2013$AIBF<-with(Patents2013,ave(Patents2013$AIBF1,Patents2013$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2013$AISDPat<-Patents2013$AIApp-Patents2013$AIGF-Patents2013$AIBF

##Create a variable of the number of granted Patents 2013 patents per company
Patents2013$AIGrant1<-ifelse(Patents2013$Granted=="Yes" & Patents2013$AI==1,1,0)
Patents2013$AIGrant1<-as.numeric(Patents2013$AIGrant1)
Patents2013$AIGrant1[is.na(Patents2013$AIGrant1)] <- 0
Patents2013$AIGrantPat<- with(Patents2013, ave(Patents2013$AIGrant1,Patents2013$Company, FUN=sum))

#Technical area (CPC code G means physics - describes rather basic research
Patents2013$Tech<-ifelse(Patents2013$AI==1 & Patents2013$Funct!=1,1,0)
Patents2013$Tech[is.na(Patents2013$Tech)] <- 0
Patents2013$Tech<-as.numeric(Patents2013$Tech)
Patents2013$TechPat<- with(Patents2013, ave(Patents2013$Tech,Patents2013$Company, FUN=sum))

#Technical Patent Applications
Patents2013$AppTech1<-ifelse(Patents2013$AI==1&Patents2013$Funct!=1&Patents2013$Year==Patents2013$PRYear,1,0)
Patents2013$AppTech1[is.na(Patents2013$AppTech1)] <- 0
Patents2013$AppTech<-with(Patents2013,ave(Patents2013$AppTech1,Patents2013$Company, FUN=sum,na.rm=TRUE))

#Functional area (CPC code not G means the application of Patents2013 technologies in other technical areas)
Patents2013$Funct1<-ifelse(Patents2013$AI==1 & Patents2013$Funct==1,1,0)
Patents2013$Funct1[is.na(Patents2013$Funct1)] <- 0
Patents2013$Funct1<-as.numeric(Patents2013$Funct)
Patents2013$FunctPat<- with(Patents2013, ave(Patents2013$Funct,Patents2013$Company, FUN=sum))

#Functional Patent Applications
Patents2013$AppFunct1<-ifelse(Patents2013$AI==1 & Patents2013$Funct==1&Patents2013$Year==Patents2013$PRYear,1,0)
Patents2013$AppFunct1[is.na(Patents2013$AppFunct1)] <- 0
Patents2013$AppFunct<-with(Patents2013,ave(Patents2013$AppFunct1,Patents2013$Company, FUN=sum,na.rm=TRUE))

#Remove duplicate companies
Patents2013<-Patents2013[!duplicated(Patents2013$BvDID),]

#Remove unnecessary data
Patents2013 <- Patents2013[,c(-3,-4,-5,-6,-7,-8,-9,-10,-11,-13,-15,-17,-20,-22,-23,-25,-27,-29,-31,-34,-36,-38,-40,-42)]

#Reorder dataset
names(Patents2013)<-c("Subsidiaries","Year","NoPat","App","GF","BF","SDPat","GrantPat","HighTech","AINoPat","AIApp","AIGF","AIBF",
                      "AISDPat","AIGrantPat","TechPat","AppTech","FunctPat","AppFunct")

##Patent data descriptives
sum(Patents2013$NoPat,na.rm = TRUE)
sum(Patents2013$App,na.rm = TRUE)
sum(Patents2013$GF,na.rm = TRUE)
sum(Patents2013$BF,na.rm = TRUE)
sum(Patents2013$SDPat,na.rm = TRUE)
sum(Patents2013$GrantPat,na.rm = TRUE)
sum(Patents2013$HighTech,na.rm = TRUE)
sum(Patents2013$AINoPat,na.rm = TRUE)
sum(Patents2013$AIApp,na.rm = TRUE)
sum(Patents2013$AIGF,na.rm = TRUE)
sum(Patents2013$AIBF,na.rm = TRUE)
sum(Patents2013$AISDPat,na.rm = TRUE)
sum(Patents2013$AIGrantPat,na.rm = TRUE)
sum(Patents2013$TechPat,na.rm = TRUE)
sum(Patents2013$AppTech,na.rm = TRUE)
sum(Patents2013$FunctPat,na.rm = TRUE)
sum(Patents2013$AppFunct,na.rm = TRUE)

write.csv2(Patents2013, file = "files_created_code1/Patents2013.csv",row.names = F)
rm(Patents2013)


## Patent variables for 2014
Patents2014<- fread("files_created_code1/Stock2014.csv")

#Create necessary variables
Patents2014$Company<-Patents2014$BvDID

##Create a variable of the number of Patents patents per company
Patents2014$Pat<-ifelse(Patents2014$BvDID==Patents2014$Company,1,0)
Patents2014$Pat<-as.numeric(Patents2014$Pat)
Patents2014$Pat[is.na(Patents2014$Pat)] <- 0
Patents2014$NoPat<- with(Patents2014, ave(Patents2014$Pat,Patents2014$Company, FUN=sum))

#Sum number of applications per firm
Patents2014$App1<-ifelse(Patents2014$Year==Patents2014$PRYear,1,0)
Patents2014$App1[is.na(Patents2014$App1)] <- 0
Patents2014$App<-with(Patents2014,ave(Patents2014$App1,Patents2014$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2014$GF1<-ifelse(Patents2014$Added==2,Patents2014$Pat,0)
Patents2014$GF1[is.na(Patents2014$GF1)] <- 0
Patents2014$GF<-with(Patents2014,ave(Patents2014$GF1,Patents2014$Company, FUN=sum,na.rm=TRUE))
Patents2014$BF1<-ifelse(Patents2014$Added==3,Patents2014$Pat,0)
Patents2014$BF1[is.na(Patents2014$BF1)] <- 0
Patents2014$BF<-with(Patents2014,ave(Patents2014$BF1,Patents2014$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2014$SDPat<-Patents2014$App-Patents2014$GF-Patents2014$BF

##Create a variable of the number of granted Patents 2014 patents per company
Patents2014$Grant1<-ifelse(Patents2014$Granted=="Yes",1,0)
Patents2014$Grant1<-as.numeric(Patents2014$Grant1)
Patents2014$Grant1[is.na(Patents2014$Grant1)] <- 0
Patents2014$GrantPat<- with(Patents2014, ave(Patents2014$Grant1,Patents2014$Company, FUN=sum))

#Filter for technological area of invention
Patents2014$IPC<-substr(Patents2014$IPCmain,1,4)

#HighTech (IPC Code XX)
Patents2014$HighTech1<-ifelse(Patents2014$IPC=="B41J"|Patents2014$IPC=="G06C"|Patents2014$IPC=="G06D"|
                                 Patents2014$IPC=="G06E"|Patents2014$IPC=="G11C"|Patents2014$IPC=="G06Q"|
                                 Patents2014$IPC=="G06G"|Patents2014$IPC=="G06J"|Patents2014$IPC=="G06F"|
                                 Patents2014$IPC=="G06M"|Patents2014$IPC=="B64B"|Patents2014$IPC=="B64C"|
                                 Patents2014$IPC=="B64D"|Patents2014$IPC=="B64F"|Patents2014$IPC=="B64G"|
                                 Patents2014$IPC=="C40B"|Patents2014$IPC=="C40B"|Patents2014$IPC=="C12P"|
                                 Patents2014$IPC=="C12Q"|Patents2014$IPC=="H01S"|Patents2014$IPC=="H01L"|
                                 Patents2014$IPC=="H04B"|Patents2014$IPC=="H04H"|Patents2014$IPC=="H04J"|
                                 Patents2014$IPC=="H04K"|Patents2014$IPC=="H04L"|Patents2014$IPC=="H04M"|
                                 Patents2014$IPC=="H04N"|Patents2014$IPC=="H04Q"|Patents2014$IPC=="H04R"|
                                 Patents2014$IPC=="H04S",1,0)
Patents2014$HighTech1[is.na(Patents2014$HighTech1)] <- 0
Patents2014$HighTech1<-as.numeric(Patents2014$HighTech1)
Patents2014$HighTech<- with(Patents2014, ave(Patents2014$HighTech1,Patents2014$Company, FUN=sum))

##AI Patents
##Create a variable of the number of Patents patents per company
Patents2014$AIPat<-ifelse(Patents2014$AI==1,1,0)
Patents2014$AIPat<-as.numeric(Patents2014$AIPat)
Patents2014$AIPat[is.na(Patents2014$AIPat)] <- 0
Patents2014$AINoPat<- with(Patents2014, ave(Patents2014$AIPat,Patents2014$Company, FUN=sum))

#Sum number of AI applications per firm
Patents2014$AIApp1<-ifelse(Patents2014$Year==Patents2014$PRYear & Patents2014$AI==1,1,0)
Patents2014$AIApp1[is.na(Patents2014$AIApp1)] <- 0
Patents2014$AIApp<-with(Patents2014,ave(Patents2014$AIApp1,Patents2014$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2014$AIGF1<-ifelse(Patents2014$Added==2,Patents2014$AIPat,0)
Patents2014$AIGF1[is.na(Patents2014$AIGF1)] <- 0
Patents2014$AIGF<-with(Patents2014,ave(Patents2014$AIGF1,Patents2014$Company, FUN=sum,na.rm=TRUE))
Patents2014$AIBF1<-ifelse(Patents2014$Added==3,Patents2014$AIPat,0)
Patents2014$AIBF1[is.na(Patents2014$AIBF1)] <- 0
Patents2014$AIBF<-with(Patents2014,ave(Patents2014$AIBF1,Patents2014$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2014$AISDPat<-Patents2014$AIApp-Patents2014$AIGF-Patents2014$AIBF

##Create a variable of the number of granted Patents 2014 patents per company
Patents2014$AIGrant1<-ifelse(Patents2014$Granted=="Yes" & Patents2014$AI==1,1,0)
Patents2014$AIGrant1<-as.numeric(Patents2014$AIGrant1)
Patents2014$AIGrant1[is.na(Patents2014$AIGrant1)] <- 0
Patents2014$AIGrantPat<- with(Patents2014, ave(Patents2014$AIGrant1,Patents2014$Company, FUN=sum))

#Technical area (CPC code G means physics - describes rather basic research
Patents2014$Tech<-ifelse(Patents2014$AI==1 & Patents2014$Funct!=1,1,0)
Patents2014$Tech[is.na(Patents2014$Tech)] <- 0
Patents2014$Tech<-as.numeric(Patents2014$Tech)
Patents2014$TechPat<- with(Patents2014, ave(Patents2014$Tech,Patents2014$Company, FUN=sum))

#Technical Patent Applications
Patents2014$AppTech1<-ifelse(Patents2014$AI==1&Patents2014$Funct!=1&Patents2014$Year==Patents2014$PRYear,1,0)
Patents2014$AppTech1[is.na(Patents2014$AppTech1)] <- 0
Patents2014$AppTech<-with(Patents2014,ave(Patents2014$AppTech1,Patents2014$Company, FUN=sum,na.rm=TRUE))

#Functional area (CPC code not G means the application of Patents2014 technologies in other technical areas)
Patents2014$Funct1<-ifelse(Patents2014$AI==1 & Patents2014$Funct==1,1,0)
Patents2014$Funct1[is.na(Patents2014$Funct1)] <- 0
Patents2014$Funct1<-as.numeric(Patents2014$Funct)
Patents2014$FunctPat<- with(Patents2014, ave(Patents2014$Funct,Patents2014$Company, FUN=sum))

#Functional Patent Applications
Patents2014$AppFunct1<-ifelse(Patents2014$AI==1 & Patents2014$Funct==1&Patents2014$Year==Patents2014$PRYear,1,0)
Patents2014$AppFunct1[is.na(Patents2014$AppFunct1)] <- 0
Patents2014$AppFunct<-with(Patents2014,ave(Patents2014$AppFunct1,Patents2014$Company, FUN=sum,na.rm=TRUE))

#Remove duplicate companies
Patents2014<-Patents2014[!duplicated(Patents2014$BvDID),]

#Remove unnecessary data
Patents2014 <- Patents2014[,c(-3,-4,-5,-6,-7,-8,-9,-10,-11,-13,-15,-17,-20,-22,-23,-25,-27,-29,-31,-34,-36,-38,-40,-42)]

#Reorder dataset
names(Patents2014)<-c("Subsidiaries","Year","NoPat","App","GF","BF","SDPat","GrantPat","HighTech","AINoPat","AIApp","AIGF","AIBF",
                      "AISDPat","AIGrantPat","TechPat","AppTech","FunctPat","AppFunct")

##Patent data descriptives
sum(Patents2014$NoPat,na.rm = TRUE)
sum(Patents2014$App,na.rm = TRUE)
sum(Patents2014$GF,na.rm = TRUE)
sum(Patents2014$BF,na.rm = TRUE)
sum(Patents2014$SDPat,na.rm = TRUE)
sum(Patents2014$GrantPat,na.rm = TRUE)
sum(Patents2014$HighTech,na.rm = TRUE)
sum(Patents2014$AINoPat,na.rm = TRUE)
sum(Patents2014$AIApp,na.rm = TRUE)
sum(Patents2014$AIGF,na.rm = TRUE)
sum(Patents2014$AIBF,na.rm = TRUE)
sum(Patents2014$AISDPat,na.rm = TRUE)
sum(Patents2014$AIGrantPat,na.rm = TRUE)
sum(Patents2014$TechPat,na.rm = TRUE)
sum(Patents2014$AppTech,na.rm = TRUE)
sum(Patents2014$FunctPat,na.rm = TRUE)
sum(Patents2014$AppFunct,na.rm = TRUE)

write.csv2(Patents2014, file = "files_created_code1/Patents2014.csv",row.names = F)
rm(Patents2014)


## Patent variables for 2015
Patents2015<- fread("files_created_code1/Stock2015.csv")

#Create necessary variables
Patents2015$Company<-Patents2015$BvDID

##Create a variable of the number of Patents patents per company
Patents2015$Pat<-ifelse(Patents2015$BvDID==Patents2015$Company,1,0)
Patents2015$Pat<-as.numeric(Patents2015$Pat)
Patents2015$Pat[is.na(Patents2015$Pat)] <- 0
Patents2015$NoPat<- with(Patents2015, ave(Patents2015$Pat,Patents2015$Company, FUN=sum))

#Sum number of applications per firm
Patents2015$App1<-ifelse(Patents2015$Year==Patents2015$PRYear,1,0)
Patents2015$App1[is.na(Patents2015$App1)] <- 0
Patents2015$App<-with(Patents2015,ave(Patents2015$App1,Patents2015$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2015$GF1<-ifelse(Patents2015$Added==2,Patents2015$Pat,0)
Patents2015$GF1[is.na(Patents2015$GF1)] <- 0
Patents2015$GF<-with(Patents2015,ave(Patents2015$GF1,Patents2015$Company, FUN=sum,na.rm=TRUE))
Patents2015$BF1<-ifelse(Patents2015$Added==3,Patents2015$Pat,0)
Patents2015$BF1[is.na(Patents2015$BF1)] <- 0
Patents2015$BF<-with(Patents2015,ave(Patents2015$BF1,Patents2015$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2015$SDPat<-Patents2015$App-Patents2015$GF-Patents2015$BF

##Create a variable of the number of granted Patents 2015 patents per company
Patents2015$Grant1<-ifelse(Patents2015$Granted=="Yes",1,0)
Patents2015$Grant1<-as.numeric(Patents2015$Grant1)
Patents2015$Grant1[is.na(Patents2015$Grant1)] <- 0
Patents2015$GrantPat<- with(Patents2015, ave(Patents2015$Grant1,Patents2015$Company, FUN=sum))

#Filter for technological area of invention
Patents2015$IPC<-substr(Patents2015$IPCmain,1,4)

#HighTech (IPC Code XX)
Patents2015$HighTech1<-ifelse(Patents2015$IPC=="B41J"|Patents2015$IPC=="G06C"|Patents2015$IPC=="G06D"|
                                 Patents2015$IPC=="G06E"|Patents2015$IPC=="G11C"|Patents2015$IPC=="G06Q"|
                                 Patents2015$IPC=="G06G"|Patents2015$IPC=="G06J"|Patents2015$IPC=="G06F"|
                                 Patents2015$IPC=="G06M"|Patents2015$IPC=="B64B"|Patents2015$IPC=="B64C"|
                                 Patents2015$IPC=="B64D"|Patents2015$IPC=="B64F"|Patents2015$IPC=="B64G"|
                                 Patents2015$IPC=="C40B"|Patents2015$IPC=="C40B"|Patents2015$IPC=="C12P"|
                                 Patents2015$IPC=="C12Q"|Patents2015$IPC=="H01S"|Patents2015$IPC=="H01L"|
                                 Patents2015$IPC=="H04B"|Patents2015$IPC=="H04H"|Patents2015$IPC=="H04J"|
                                 Patents2015$IPC=="H04K"|Patents2015$IPC=="H04L"|Patents2015$IPC=="H04M"|
                                 Patents2015$IPC=="H04N"|Patents2015$IPC=="H04Q"|Patents2015$IPC=="H04R"|
                                 Patents2015$IPC=="H04S",1,0)
Patents2015$HighTech1[is.na(Patents2015$HighTech1)] <- 0
Patents2015$HighTech1<-as.numeric(Patents2015$HighTech1)
Patents2015$HighTech<- with(Patents2015, ave(Patents2015$HighTech1,Patents2015$Company, FUN=sum))

##AI Patents
##Create a variable of the number of Patents patents per company
Patents2015$AIPat<-ifelse(Patents2015$AI==1,1,0)
Patents2015$AIPat<-as.numeric(Patents2015$AIPat)
Patents2015$AIPat[is.na(Patents2015$AIPat)] <- 0
Patents2015$AINoPat<- with(Patents2015, ave(Patents2015$AIPat,Patents2015$Company, FUN=sum))

#Sum number of AI applications per firm
Patents2015$AIApp1<-ifelse(Patents2015$Year==Patents2015$PRYear & Patents2015$AI==1,1,0)
Patents2015$AIApp1[is.na(Patents2015$AIApp1)] <- 0
Patents2015$AIApp<-with(Patents2015,ave(Patents2015$AIApp1,Patents2015$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2015$AIGF1<-ifelse(Patents2015$Added==2,Patents2015$AIPat,0)
Patents2015$AIGF1[is.na(Patents2015$AIGF1)] <- 0
Patents2015$AIGF<-with(Patents2015,ave(Patents2015$AIGF1,Patents2015$Company, FUN=sum,na.rm=TRUE))
Patents2015$AIBF1<-ifelse(Patents2015$Added==3,Patents2015$AIPat,0)
Patents2015$AIBF1[is.na(Patents2015$AIBF1)] <- 0
Patents2015$AIBF<-with(Patents2015,ave(Patents2015$AIBF1,Patents2015$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2015$AISDPat<-Patents2015$AIApp-Patents2015$AIGF-Patents2015$AIBF

##Create a variable of the number of granted Patents 2015 patents per company
Patents2015$AIGrant1<-ifelse(Patents2015$Granted=="Yes" & Patents2015$AI==1,1,0)
Patents2015$AIGrant1<-as.numeric(Patents2015$AIGrant1)
Patents2015$AIGrant1[is.na(Patents2015$AIGrant1)] <- 0
Patents2015$AIGrantPat<- with(Patents2015, ave(Patents2015$AIGrant1,Patents2015$Company, FUN=sum))

#Technical area (CPC code G means physics - describes rather basic research
Patents2015$Tech<-ifelse(Patents2015$AI==1 & Patents2015$Funct!=1,1,0)
Patents2015$Tech[is.na(Patents2015$Tech)] <- 0
Patents2015$Tech<-as.numeric(Patents2015$Tech)
Patents2015$TechPat<- with(Patents2015, ave(Patents2015$Tech,Patents2015$Company, FUN=sum))

#Technical Patent Applications
Patents2015$AppTech1<-ifelse(Patents2015$AI==1&Patents2015$Funct!=1&Patents2015$Year==Patents2015$PRYear,1,0)
Patents2015$AppTech1[is.na(Patents2015$AppTech1)] <- 0
Patents2015$AppTech<-with(Patents2015,ave(Patents2015$AppTech1,Patents2015$Company, FUN=sum,na.rm=TRUE))

#Functional area (CPC code not G means the application of Patents2015 technologies in other technical areas)
Patents2015$Funct1<-ifelse(Patents2015$AI==1 & Patents2015$Funct==1,1,0)
Patents2015$Funct1[is.na(Patents2015$Funct1)] <- 0
Patents2015$Funct1<-as.numeric(Patents2015$Funct)
Patents2015$FunctPat<- with(Patents2015, ave(Patents2015$Funct,Patents2015$Company, FUN=sum))

#Functional Patent Applications
Patents2015$AppFunct1<-ifelse(Patents2015$AI==1 & Patents2015$Funct==1&Patents2015$Year==Patents2015$PRYear,1,0)
Patents2015$AppFunct1[is.na(Patents2015$AppFunct1)] <- 0
Patents2015$AppFunct<-with(Patents2015,ave(Patents2015$AppFunct1,Patents2015$Company, FUN=sum,na.rm=TRUE))

#Remove duplicate companies
Patents2015<-Patents2015[!duplicated(Patents2015$BvDID),]

#Remove unnecessary data
Patents2015 <- Patents2015[,c(-3,-4,-5,-6,-7,-8,-9,-10,-11,-13,-15,-17,-20,-22,-23,-25,-27,-29,-31,-34,-36,-38,-40,-42)]

#Reorder dataset
names(Patents2015)<-c("Subsidiaries","Year","NoPat","App","GF","BF","SDPat","GrantPat","HighTech","AINoPat","AIApp","AIGF","AIBF",
                      "AISDPat","AIGrantPat","TechPat","AppTech","FunctPat","AppFunct")

##Patent data descriptives
sum(Patents2015$NoPat,na.rm = TRUE)
sum(Patents2015$App,na.rm = TRUE)
sum(Patents2015$GF,na.rm = TRUE)
sum(Patents2015$BF,na.rm = TRUE)
sum(Patents2015$SDPat,na.rm = TRUE)
sum(Patents2015$GrantPat,na.rm = TRUE)
sum(Patents2015$HighTech,na.rm = TRUE)
sum(Patents2015$AINoPat,na.rm = TRUE)
sum(Patents2015$AIApp,na.rm = TRUE)
sum(Patents2015$AIGF,na.rm = TRUE)
sum(Patents2015$AIBF,na.rm = TRUE)
sum(Patents2015$AISDPat,na.rm = TRUE)
sum(Patents2015$AIGrantPat,na.rm = TRUE)
sum(Patents2015$TechPat,na.rm = TRUE)
sum(Patents2015$AppTech,na.rm = TRUE)
sum(Patents2015$FunctPat,na.rm = TRUE)
sum(Patents2015$AppFunct,na.rm = TRUE)

write.csv2(Patents2015, file = "files_created_code1/Patents2015.csv",row.names = F)
rm(Patents2015)


## Patent variables for 2016
Patents2016<- fread("files_created_code1/Stock2016.csv")

#Create necessary variables
Patents2016$Company<-Patents2016$BvDID

##Create a variable of the number of Patents patents per company
Patents2016$Pat<-ifelse(Patents2016$BvDID==Patents2016$Company,1,0)
Patents2016$Pat<-as.numeric(Patents2016$Pat)
Patents2016$Pat[is.na(Patents2016$Pat)] <- 0
Patents2016$NoPat<- with(Patents2016, ave(Patents2016$Pat,Patents2016$Company, FUN=sum))

#Sum number of applications per firm
Patents2016$App1<-ifelse(Patents2016$Year==Patents2016$PRYear,1,0)
Patents2016$App1[is.na(Patents2016$App1)] <- 0
Patents2016$App<-with(Patents2016,ave(Patents2016$App1,Patents2016$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2016$GF1<-ifelse(Patents2016$Added==2,Patents2016$Pat,0)
Patents2016$GF1[is.na(Patents2016$GF1)] <- 0
Patents2016$GF<-with(Patents2016,ave(Patents2016$GF1,Patents2016$Company, FUN=sum,na.rm=TRUE))
Patents2016$BF1<-ifelse(Patents2016$Added==3,Patents2016$Pat,0)
Patents2016$BF1[is.na(Patents2016$BF1)] <- 0
Patents2016$BF<-with(Patents2016,ave(Patents2016$BF1,Patents2016$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2016$SDPat<-Patents2016$App-Patents2016$GF-Patents2016$BF

##Create a variable of the number of granted Patents 2016 patents per company
Patents2016$Grant1<-ifelse(Patents2016$Granted=="Yes",1,0)
Patents2016$Grant1<-as.numeric(Patents2016$Grant1)
Patents2016$Grant1[is.na(Patents2016$Grant1)] <- 0
Patents2016$GrantPat<- with(Patents2016, ave(Patents2016$Grant1,Patents2016$Company, FUN=sum))

#Filter for technological area of invention
Patents2016$IPC<-substr(Patents2016$IPCmain,1,4)

#HighTech (IPC Code XX)
Patents2016$HighTech1<-ifelse(Patents2016$IPC=="B41J"|Patents2016$IPC=="G06C"|Patents2016$IPC=="G06D"|
                                 Patents2016$IPC=="G06E"|Patents2016$IPC=="G11C"|Patents2016$IPC=="G06Q"|
                                 Patents2016$IPC=="G06G"|Patents2016$IPC=="G06J"|Patents2016$IPC=="G06F"|
                                 Patents2016$IPC=="G06M"|Patents2016$IPC=="B64B"|Patents2016$IPC=="B64C"|
                                 Patents2016$IPC=="B64D"|Patents2016$IPC=="B64F"|Patents2016$IPC=="B64G"|
                                 Patents2016$IPC=="C40B"|Patents2016$IPC=="C40B"|Patents2016$IPC=="C12P"|
                                 Patents2016$IPC=="C12Q"|Patents2016$IPC=="H01S"|Patents2016$IPC=="H01L"|
                                 Patents2016$IPC=="H04B"|Patents2016$IPC=="H04H"|Patents2016$IPC=="H04J"|
                                 Patents2016$IPC=="H04K"|Patents2016$IPC=="H04L"|Patents2016$IPC=="H04M"|
                                 Patents2016$IPC=="H04N"|Patents2016$IPC=="H04Q"|Patents2016$IPC=="H04R"|
                                 Patents2016$IPC=="H04S",1,0)
Patents2016$HighTech1[is.na(Patents2016$HighTech1)] <- 0
Patents2016$HighTech1<-as.numeric(Patents2016$HighTech1)
Patents2016$HighTech<- with(Patents2016, ave(Patents2016$HighTech1,Patents2016$Company, FUN=sum))

##AI Patents
##Create a variable of the number of Patents patents per company
Patents2016$AIPat<-ifelse(Patents2016$AI==1,1,0)
Patents2016$AIPat<-as.numeric(Patents2016$AIPat)
Patents2016$AIPat[is.na(Patents2016$AIPat)] <- 0
Patents2016$AINoPat<- with(Patents2016, ave(Patents2016$AIPat,Patents2016$Company, FUN=sum))

#Sum number of AI applications per firm
Patents2016$AIApp1<-ifelse(Patents2016$Year==Patents2016$PRYear & Patents2016$AI==1,1,0)
Patents2016$AIApp1[is.na(Patents2016$AIApp1)] <- 0
Patents2016$AIApp<-with(Patents2016,ave(Patents2016$AIApp1,Patents2016$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2016$AIGF1<-ifelse(Patents2016$Added==2,Patents2016$AIPat,0)
Patents2016$AIGF1[is.na(Patents2016$AIGF1)] <- 0
Patents2016$AIGF<-with(Patents2016,ave(Patents2016$AIGF1,Patents2016$Company, FUN=sum,na.rm=TRUE))
Patents2016$AIBF1<-ifelse(Patents2016$Added==3,Patents2016$AIPat,0)
Patents2016$AIBF1[is.na(Patents2016$AIBF1)] <- 0
Patents2016$AIBF<-with(Patents2016,ave(Patents2016$AIBF1,Patents2016$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2016$AISDPat<-Patents2016$AIApp-Patents2016$AIGF-Patents2016$AIBF

##Create a variable of the number of granted Patents 2016 patents per company
Patents2016$AIGrant1<-ifelse(Patents2016$Granted=="Yes" & Patents2016$AI==1,1,0)
Patents2016$AIGrant1<-as.numeric(Patents2016$AIGrant1)
Patents2016$AIGrant1[is.na(Patents2016$AIGrant1)] <- 0
Patents2016$AIGrantPat<- with(Patents2016, ave(Patents2016$AIGrant1,Patents2016$Company, FUN=sum))

#Technical area (CPC code G means physics - describes rather basic research
Patents2016$Tech<-ifelse(Patents2016$AI==1 & Patents2016$Funct!=1,1,0)
Patents2016$Tech[is.na(Patents2016$Tech)] <- 0
Patents2016$Tech<-as.numeric(Patents2016$Tech)
Patents2016$TechPat<- with(Patents2016, ave(Patents2016$Tech,Patents2016$Company, FUN=sum))

#Technical Patent Applications
Patents2016$AppTech1<-ifelse(Patents2016$AI==1&Patents2016$Funct!=1&Patents2016$Year==Patents2016$PRYear,1,0)
Patents2016$AppTech1[is.na(Patents2016$AppTech1)] <- 0
Patents2016$AppTech<-with(Patents2016,ave(Patents2016$AppTech1,Patents2016$Company, FUN=sum,na.rm=TRUE))

#Functional area (CPC code not G means the application of Patents2016 technologies in other technical areas)
Patents2016$Funct1<-ifelse(Patents2016$AI==1 & Patents2016$Funct==1,1,0)
Patents2016$Funct1[is.na(Patents2016$Funct1)] <- 0
Patents2016$Funct1<-as.numeric(Patents2016$Funct)
Patents2016$FunctPat<- with(Patents2016, ave(Patents2016$Funct,Patents2016$Company, FUN=sum))

#Functional Patent Applications
Patents2016$AppFunct1<-ifelse(Patents2016$AI==1 & Patents2016$Funct==1&Patents2016$Year==Patents2016$PRYear,1,0)
Patents2016$AppFunct1[is.na(Patents2016$AppFunct1)] <- 0
Patents2016$AppFunct<-with(Patents2016,ave(Patents2016$AppFunct1,Patents2016$Company, FUN=sum,na.rm=TRUE))

#Remove duplicate companies
Patents2016<-Patents2016[!duplicated(Patents2016$BvDID),]

#Remove unnecessary data
Patents2016 <- Patents2016[,c(-3,-4,-5,-6,-7,-8,-9,-10,-11,-13,-15,-17,-20,-22,-23,-25,-27,-29,-31,-34,-36,-38,-40,-42)]

#Reorder dataset
names(Patents2016)<-c("Subsidiaries","Year","NoPat","App","GF","BF","SDPat","GrantPat","HighTech","AINoPat","AIApp","AIGF","AIBF",
                      "AISDPat","AIGrantPat","TechPat","AppTech","FunctPat","AppFunct")

##Patent data descriptives
sum(Patents2016$NoPat,na.rm = TRUE)
sum(Patents2016$App,na.rm = TRUE)
sum(Patents2016$GF,na.rm = TRUE)
sum(Patents2016$BF,na.rm = TRUE)
sum(Patents2016$SDPat,na.rm = TRUE)
sum(Patents2016$GrantPat,na.rm = TRUE)
sum(Patents2016$HighTech,na.rm = TRUE)
sum(Patents2016$AINoPat,na.rm = TRUE)
sum(Patents2016$AIApp,na.rm = TRUE)
sum(Patents2016$AIGF,na.rm = TRUE)
sum(Patents2016$AIBF,na.rm = TRUE)
sum(Patents2016$AISDPat,na.rm = TRUE)
sum(Patents2016$AIGrantPat,na.rm = TRUE)
sum(Patents2016$TechPat,na.rm = TRUE)
sum(Patents2016$AppTech,na.rm = TRUE)
sum(Patents2016$FunctPat,na.rm = TRUE)
sum(Patents2016$AppFunct,na.rm = TRUE)

write.csv2(Patents2016, file = "files_created_code1/Patents2016.csv",row.names = F)
rm(Patents2016)


## Patent variables for 2017
Patents2017<- fread("files_created_code1/Stock2017.csv")

#Create necessary variables
Patents2017$Company<-Patents2017$BvDID

##Create a variable of the number of Patents patents per company
Patents2017$Pat<-ifelse(Patents2017$BvDID==Patents2017$Company,1,0)
Patents2017$Pat<-as.numeric(Patents2017$Pat)
Patents2017$Pat[is.na(Patents2017$Pat)] <- 0
Patents2017$NoPat<- with(Patents2017, ave(Patents2017$Pat,Patents2017$Company, FUN=sum))

#Sum number of applications per firm
Patents2017$App1<-ifelse(Patents2017$Year==Patents2017$PRYear,1,0)
Patents2017$App1[is.na(Patents2017$App1)] <- 0
Patents2017$App<-with(Patents2017,ave(Patents2017$App1,Patents2017$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2017$GF1<-ifelse(Patents2017$Added==2,Patents2017$Pat,0)
Patents2017$GF1[is.na(Patents2017$GF1)] <- 0
Patents2017$GF<-with(Patents2017,ave(Patents2017$GF1,Patents2017$Company, FUN=sum,na.rm=TRUE))
Patents2017$BF1<-ifelse(Patents2017$Added==3,Patents2017$Pat,0)
Patents2017$BF1[is.na(Patents2017$BF1)] <- 0
Patents2017$BF<-with(Patents2017,ave(Patents2017$BF1,Patents2017$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2017$SDPat<-Patents2017$App-Patents2017$GF-Patents2017$BF

##Create a variable of the number of granted Patents 2017 patents per company
Patents2017$Grant1<-ifelse(Patents2017$Granted=="Yes",1,0)
Patents2017$Grant1<-as.numeric(Patents2017$Grant1)
Patents2017$Grant1[is.na(Patents2017$Grant1)] <- 0
Patents2017$GrantPat<- with(Patents2017, ave(Patents2017$Grant1,Patents2017$Company, FUN=sum))

#Filter for technological area of invention
Patents2017$IPC<-substr(Patents2017$IPCmain,1,4)

#HighTech (IPC Code XX)
Patents2017$HighTech1<-ifelse(Patents2017$IPC=="B41J"|Patents2017$IPC=="G06C"|Patents2017$IPC=="G06D"|
                                 Patents2017$IPC=="G06E"|Patents2017$IPC=="G11C"|Patents2017$IPC=="G06Q"|
                                 Patents2017$IPC=="G06G"|Patents2017$IPC=="G06J"|Patents2017$IPC=="G06F"|
                                 Patents2017$IPC=="G06M"|Patents2017$IPC=="B64B"|Patents2017$IPC=="B64C"|
                                 Patents2017$IPC=="B64D"|Patents2017$IPC=="B64F"|Patents2017$IPC=="B64G"|
                                 Patents2017$IPC=="C40B"|Patents2017$IPC=="C40B"|Patents2017$IPC=="C12P"|
                                 Patents2017$IPC=="C12Q"|Patents2017$IPC=="H01S"|Patents2017$IPC=="H01L"|
                                 Patents2017$IPC=="H04B"|Patents2017$IPC=="H04H"|Patents2017$IPC=="H04J"|
                                 Patents2017$IPC=="H04K"|Patents2017$IPC=="H04L"|Patents2017$IPC=="H04M"|
                                 Patents2017$IPC=="H04N"|Patents2017$IPC=="H04Q"|Patents2017$IPC=="H04R"|
                                 Patents2017$IPC=="H04S",1,0)
Patents2017$HighTech1[is.na(Patents2017$HighTech1)] <- 0
Patents2017$HighTech1<-as.numeric(Patents2017$HighTech1)
Patents2017$HighTech<- with(Patents2017, ave(Patents2017$HighTech1,Patents2017$Company, FUN=sum))

##AI Patents
##Create a variable of the number of Patents patents per company
Patents2017$AIPat<-ifelse(Patents2017$AI==1,1,0)
Patents2017$AIPat<-as.numeric(Patents2017$AIPat)
Patents2017$AIPat[is.na(Patents2017$AIPat)] <- 0
Patents2017$AINoPat<- with(Patents2017, ave(Patents2017$AIPat,Patents2017$Company, FUN=sum))

#Sum number of AI applications per firm
Patents2017$AIApp1<-ifelse(Patents2017$Year==Patents2017$PRYear & Patents2017$AI==1,1,0)
Patents2017$AIApp1[is.na(Patents2017$AIApp1)] <- 0
Patents2017$AIApp<-with(Patents2017,ave(Patents2017$AIApp1,Patents2017$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2017$AIGF1<-ifelse(Patents2017$Added==2,Patents2017$AIPat,0)
Patents2017$AIGF1[is.na(Patents2017$AIGF1)] <- 0
Patents2017$AIGF<-with(Patents2017,ave(Patents2017$AIGF1,Patents2017$Company, FUN=sum,na.rm=TRUE))
Patents2017$AIBF1<-ifelse(Patents2017$Added==3,Patents2017$AIPat,0)
Patents2017$AIBF1[is.na(Patents2017$AIBF1)] <- 0
Patents2017$AIBF<-with(Patents2017,ave(Patents2017$AIBF1,Patents2017$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2017$AISDPat<-Patents2017$AIApp-Patents2017$AIGF-Patents2017$AIBF

##Create a variable of the number of granted Patents 2017 patents per company
Patents2017$AIGrant1<-ifelse(Patents2017$Granted=="Yes" & Patents2017$AI==1,1,0)
Patents2017$AIGrant1<-as.numeric(Patents2017$AIGrant1)
Patents2017$AIGrant1[is.na(Patents2017$AIGrant1)] <- 0
Patents2017$AIGrantPat<- with(Patents2017, ave(Patents2017$AIGrant1,Patents2017$Company, FUN=sum))

#Technical area (CPC code G means physics - describes rather basic research
Patents2017$Tech<-ifelse(Patents2017$AI==1 & Patents2017$Funct!=1,1,0)
Patents2017$Tech[is.na(Patents2017$Tech)] <- 0
Patents2017$Tech<-as.numeric(Patents2017$Tech)
Patents2017$TechPat<- with(Patents2017, ave(Patents2017$Tech,Patents2017$Company, FUN=sum))

#Technical Patent Applications
Patents2017$AppTech1<-ifelse(Patents2017$AI==1&Patents2017$Funct!=1&Patents2017$Year==Patents2017$PRYear,1,0)
Patents2017$AppTech1[is.na(Patents2017$AppTech1)] <- 0
Patents2017$AppTech<-with(Patents2017,ave(Patents2017$AppTech1,Patents2017$Company, FUN=sum,na.rm=TRUE))

#Functional area (CPC code not G means the application of Patents2017 technologies in other technical areas)
Patents2017$Funct1<-ifelse(Patents2017$AI==1 & Patents2017$Funct==1,1,0)
Patents2017$Funct1[is.na(Patents2017$Funct1)] <- 0
Patents2017$Funct1<-as.numeric(Patents2017$Funct)
Patents2017$FunctPat<- with(Patents2017, ave(Patents2017$Funct,Patents2017$Company, FUN=sum))

#Functional Patent Applications
Patents2017$AppFunct1<-ifelse(Patents2017$AI==1 & Patents2017$Funct==1&Patents2017$Year==Patents2017$PRYear,1,0)
Patents2017$AppFunct1[is.na(Patents2017$AppFunct1)] <- 0
Patents2017$AppFunct<-with(Patents2017,ave(Patents2017$AppFunct1,Patents2017$Company, FUN=sum,na.rm=TRUE))

#Remove duplicate companies
Patents2017<-Patents2017[!duplicated(Patents2017$BvDID),]

#Remove unnecessary data
Patents2017 <- Patents2017[,c(-3,-4,-5,-6,-7,-8,-9,-10,-11,-13,-15,-17,-20,-22,-23,-25,-27,-29,-31,-34,-36,-38,-40,-42)]

#Reorder dataset
names(Patents2017)<-c("Subsidiaries","Year","NoPat","App","GF","BF","SDPat","GrantPat","HighTech","AINoPat","AIApp","AIGF","AIBF",
                      "AISDPat","AIGrantPat","TechPat","AppTech","FunctPat","AppFunct")

##Patent data descriptives
sum(Patents2017$NoPat,na.rm = TRUE)
sum(Patents2017$App,na.rm = TRUE)
sum(Patents2017$GF,na.rm = TRUE)
sum(Patents2017$BF,na.rm = TRUE)
sum(Patents2017$SDPat,na.rm = TRUE)
sum(Patents2017$GrantPat,na.rm = TRUE)
sum(Patents2017$HighTech,na.rm = TRUE)
sum(Patents2017$AINoPat,na.rm = TRUE)
sum(Patents2017$AIApp,na.rm = TRUE)
sum(Patents2017$AIGF,na.rm = TRUE)
sum(Patents2017$AIBF,na.rm = TRUE)
sum(Patents2017$AISDPat,na.rm = TRUE)
sum(Patents2017$AIGrantPat,na.rm = TRUE)
sum(Patents2017$TechPat,na.rm = TRUE)
sum(Patents2017$AppTech,na.rm = TRUE)
sum(Patents2017$FunctPat,na.rm = TRUE)
sum(Patents2017$AppFunct,na.rm = TRUE)

write.csv2(Patents2017, file = "files_created_code1/Patents2017.csv",row.names = F)
rm(Patents2017)


## Patent variables for 2018
Patents2018<- fread("files_created_code1/Stock2018.csv")

#Create necessary variables
Patents2018$Company<-Patents2018$BvDID

##Create a variable of the number of Patents patents per company
Patents2018$Pat<-ifelse(Patents2018$BvDID==Patents2018$Company,1,0)
Patents2018$Pat<-as.numeric(Patents2018$Pat)
Patents2018$Pat[is.na(Patents2018$Pat)] <- 0
Patents2018$NoPat<- with(Patents2018, ave(Patents2018$Pat,Patents2018$Company, FUN=sum))

#Sum number of applications per firm
Patents2018$App1<-ifelse(Patents2018$Year==Patents2018$PRYear,1,0)
Patents2018$App1[is.na(Patents2018$App1)] <- 0
Patents2018$App<-with(Patents2018,ave(Patents2018$App1,Patents2018$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2018$GF1<-ifelse(Patents2018$Added==2,Patents2018$Pat,0)
Patents2018$GF1[is.na(Patents2018$GF1)] <- 0
Patents2018$GF<-with(Patents2018,ave(Patents2018$GF1,Patents2018$Company, FUN=sum,na.rm=TRUE))
Patents2018$BF1<-ifelse(Patents2018$Added==3,Patents2018$Pat,0)
Patents2018$BF1[is.na(Patents2018$BF1)] <- 0
Patents2018$BF<-with(Patents2018,ave(Patents2018$BF1,Patents2018$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2018$SDPat<-Patents2018$App-Patents2018$GF-Patents2018$BF

##Create a variable of the number of granted Patents 2018 patents per company
Patents2018$Grant1<-ifelse(Patents2018$Granted=="Yes",1,0)
Patents2018$Grant1<-as.numeric(Patents2018$Grant1)
Patents2018$Grant1[is.na(Patents2018$Grant1)] <- 0
Patents2018$GrantPat<- with(Patents2018, ave(Patents2018$Grant1,Patents2018$Company, FUN=sum))

#Filter for technological area of invention
Patents2018$IPC<-substr(Patents2018$IPCmain,1,4)

#HighTech (IPC Code XX)
Patents2018$HighTech1<-ifelse(Patents2018$IPC=="B41J"|Patents2018$IPC=="G06C"|Patents2018$IPC=="G06D"|
                                 Patents2018$IPC=="G06E"|Patents2018$IPC=="G11C"|Patents2018$IPC=="G06Q"|
                                 Patents2018$IPC=="G06G"|Patents2018$IPC=="G06J"|Patents2018$IPC=="G06F"|
                                 Patents2018$IPC=="G06M"|Patents2018$IPC=="B64B"|Patents2018$IPC=="B64C"|
                                 Patents2018$IPC=="B64D"|Patents2018$IPC=="B64F"|Patents2018$IPC=="B64G"|
                                 Patents2018$IPC=="C40B"|Patents2018$IPC=="C40B"|Patents2018$IPC=="C12P"|
                                 Patents2018$IPC=="C12Q"|Patents2018$IPC=="H01S"|Patents2018$IPC=="H01L"|
                                 Patents2018$IPC=="H04B"|Patents2018$IPC=="H04H"|Patents2018$IPC=="H04J"|
                                 Patents2018$IPC=="H04K"|Patents2018$IPC=="H04L"|Patents2018$IPC=="H04M"|
                                 Patents2018$IPC=="H04N"|Patents2018$IPC=="H04Q"|Patents2018$IPC=="H04R"|
                                 Patents2018$IPC=="H04S",1,0)
Patents2018$HighTech1[is.na(Patents2018$HighTech1)] <- 0
Patents2018$HighTech1<-as.numeric(Patents2018$HighTech1)
Patents2018$HighTech<- with(Patents2018, ave(Patents2018$HighTech1,Patents2018$Company, FUN=sum))

##AI Patents
##Create a variable of the number of Patents patents per company
Patents2018$AIPat<-ifelse(Patents2018$AI==1,1,0)
Patents2018$AIPat<-as.numeric(Patents2018$AIPat)
Patents2018$AIPat[is.na(Patents2018$AIPat)] <- 0
Patents2018$AINoPat<- with(Patents2018, ave(Patents2018$AIPat,Patents2018$Company, FUN=sum))

#Sum number of AI applications per firm
Patents2018$AIApp1<-ifelse(Patents2018$Year==Patents2018$PRYear & Patents2018$AI==1,1,0)
Patents2018$AIApp1[is.na(Patents2018$AIApp1)] <- 0
Patents2018$AIApp<-with(Patents2018,ave(Patents2018$AIApp1,Patents2018$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2018$AIGF1<-ifelse(Patents2018$Added==2,Patents2018$AIPat,0)
Patents2018$AIGF1[is.na(Patents2018$AIGF1)] <- 0
Patents2018$AIGF<-with(Patents2018,ave(Patents2018$AIGF1,Patents2018$Company, FUN=sum,na.rm=TRUE))
Patents2018$AIBF1<-ifelse(Patents2018$Added==3,Patents2018$AIPat,0)
Patents2018$AIBF1[is.na(Patents2018$AIBF1)] <- 0
Patents2018$AIBF<-with(Patents2018,ave(Patents2018$AIBF1,Patents2018$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2018$AISDPat<-Patents2018$AIApp-Patents2018$AIGF-Patents2018$AIBF

##Create a variable of the number of granted Patents 2018 patents per company
Patents2018$AIGrant1<-ifelse(Patents2018$Granted=="Yes" & Patents2018$AI==1,1,0)
Patents2018$AIGrant1<-as.numeric(Patents2018$AIGrant1)
Patents2018$AIGrant1[is.na(Patents2018$AIGrant1)] <- 0
Patents2018$AIGrantPat<- with(Patents2018, ave(Patents2018$AIGrant1,Patents2018$Company, FUN=sum))

#Technical area (CPC code G means physics - describes rather basic research
Patents2018$Tech<-ifelse(Patents2018$AI==1 & Patents2018$Funct!=1,1,0)
Patents2018$Tech[is.na(Patents2018$Tech)] <- 0
Patents2018$Tech<-as.numeric(Patents2018$Tech)
Patents2018$TechPat<- with(Patents2018, ave(Patents2018$Tech,Patents2018$Company, FUN=sum))

#Technical Patent Applications
Patents2018$AppTech1<-ifelse(Patents2018$AI==1&Patents2018$Funct!=1&Patents2018$Year==Patents2018$PRYear,1,0)
Patents2018$AppTech1[is.na(Patents2018$AppTech1)] <- 0
Patents2018$AppTech<-with(Patents2018,ave(Patents2018$AppTech1,Patents2018$Company, FUN=sum,na.rm=TRUE))

#Functional area (CPC code not G means the application of Patents2018 technologies in other technical areas)
Patents2018$Funct1<-ifelse(Patents2018$AI==1 & Patents2018$Funct==1,1,0)
Patents2018$Funct1[is.na(Patents2018$Funct1)] <- 0
Patents2018$Funct1<-as.numeric(Patents2018$Funct)
Patents2018$FunctPat<- with(Patents2018, ave(Patents2018$Funct,Patents2018$Company, FUN=sum))

#Functional Patent Applications
Patents2018$AppFunct1<-ifelse(Patents2018$AI==1 & Patents2018$Funct==1&Patents2018$Year==Patents2018$PRYear,1,0)
Patents2018$AppFunct1[is.na(Patents2018$AppFunct1)] <- 0
Patents2018$AppFunct<-with(Patents2018,ave(Patents2018$AppFunct1,Patents2018$Company, FUN=sum,na.rm=TRUE))

#Remove duplicate companies
Patents2018<-Patents2018[!duplicated(Patents2018$BvDID),]

#Remove unnecessary data
Patents2018 <- Patents2018[,c(-3,-4,-5,-6,-7,-8,-9,-10,-11,-13,-15,-17,-20,-22,-23,-25,-27,-29,-31,-34,-36,-38,-40,-42)]

#Reorder dataset
names(Patents2018)<-c("Subsidiaries","Year","NoPat","App","GF","BF","SDPat","GrantPat","HighTech","AINoPat","AIApp","AIGF","AIBF",
                      "AISDPat","AIGrantPat","TechPat","AppTech","FunctPat","AppFunct")

##Patent data descriptives
sum(Patents2018$NoPat,na.rm = TRUE)
sum(Patents2018$App,na.rm = TRUE)
sum(Patents2018$GF,na.rm = TRUE)
sum(Patents2018$BF,na.rm = TRUE)
sum(Patents2018$SDPat,na.rm = TRUE)
sum(Patents2018$GrantPat,na.rm = TRUE)
sum(Patents2018$HighTech,na.rm = TRUE)
sum(Patents2018$AINoPat,na.rm = TRUE)
sum(Patents2018$AIApp,na.rm = TRUE)
sum(Patents2018$AIGF,na.rm = TRUE)
sum(Patents2018$AIBF,na.rm = TRUE)
sum(Patents2018$AISDPat,na.rm = TRUE)
sum(Patents2018$AIGrantPat,na.rm = TRUE)
sum(Patents2018$TechPat,na.rm = TRUE)
sum(Patents2018$AppTech,na.rm = TRUE)
sum(Patents2018$FunctPat,na.rm = TRUE)
sum(Patents2018$AppFunct,na.rm = TRUE)

write.csv2(Patents2018, file = "files_created_code1/Patents2018.csv",row.names = F)
rm(Patents2018)


## Patent variables for 2019
Patents2019<- fread("files_created_code1/Stock2019.csv")

#Create necessary variables
Patents2019$Company<-Patents2019$BvDID

##Create a variable of the number of Patents patents per company
Patents2019$Pat<-ifelse(Patents2019$BvDID==Patents2019$Company,1,0)
Patents2019$Pat<-as.numeric(Patents2019$Pat)
Patents2019$Pat[is.na(Patents2019$Pat)] <- 0
Patents2019$NoPat<- with(Patents2019, ave(Patents2019$Pat,Patents2019$Company, FUN=sum))

#Sum number of applications per firm
Patents2019$App1<-ifelse(Patents2019$Year==Patents2019$PRYear,1,0)
Patents2019$App1[is.na(Patents2019$App1)] <- 0
Patents2019$App<-with(Patents2019,ave(Patents2019$App1,Patents2019$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2019$GF1<-ifelse(Patents2019$Added==2,Patents2019$Pat,0)
Patents2019$GF1[is.na(Patents2019$GF1)] <- 0
Patents2019$GF<-with(Patents2019,ave(Patents2019$GF1,Patents2019$Company, FUN=sum,na.rm=TRUE))
Patents2019$BF1<-ifelse(Patents2019$Added==3,Patents2019$Pat,0)
Patents2019$BF1[is.na(Patents2019$BF1)] <- 0
Patents2019$BF<-with(Patents2019,ave(Patents2019$BF1,Patents2019$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2019$SDPat<-Patents2019$App-Patents2019$GF-Patents2019$BF

##Create a variable of the number of granted Patents 2019 patents per company
Patents2019$Grant1<-ifelse(Patents2019$Granted=="Yes",1,0)
Patents2019$Grant1<-as.numeric(Patents2019$Grant1)
Patents2019$Grant1[is.na(Patents2019$Grant1)] <- 0
Patents2019$GrantPat<- with(Patents2019, ave(Patents2019$Grant1,Patents2019$Company, FUN=sum))

#Filter for technological area of invention
Patents2019$IPC<-substr(Patents2019$IPCmain,1,4)

#HighTech (IPC Code XX)
Patents2019$HighTech1<-ifelse(Patents2019$IPC=="B41J"|Patents2019$IPC=="G06C"|Patents2019$IPC=="G06D"|
                                 Patents2019$IPC=="G06E"|Patents2019$IPC=="G11C"|Patents2019$IPC=="G06Q"|
                                 Patents2019$IPC=="G06G"|Patents2019$IPC=="G06J"|Patents2019$IPC=="G06F"|
                                 Patents2019$IPC=="G06M"|Patents2019$IPC=="B64B"|Patents2019$IPC=="B64C"|
                                 Patents2019$IPC=="B64D"|Patents2019$IPC=="B64F"|Patents2019$IPC=="B64G"|
                                 Patents2019$IPC=="C40B"|Patents2019$IPC=="C40B"|Patents2019$IPC=="C12P"|
                                 Patents2019$IPC=="C12Q"|Patents2019$IPC=="H01S"|Patents2019$IPC=="H01L"|
                                 Patents2019$IPC=="H04B"|Patents2019$IPC=="H04H"|Patents2019$IPC=="H04J"|
                                 Patents2019$IPC=="H04K"|Patents2019$IPC=="H04L"|Patents2019$IPC=="H04M"|
                                 Patents2019$IPC=="H04N"|Patents2019$IPC=="H04Q"|Patents2019$IPC=="H04R"|
                                 Patents2019$IPC=="H04S",1,0)
Patents2019$HighTech1[is.na(Patents2019$HighTech1)] <- 0
Patents2019$HighTech1<-as.numeric(Patents2019$HighTech1)
Patents2019$HighTech<- with(Patents2019, ave(Patents2019$HighTech1,Patents2019$Company, FUN=sum))

##AI Patents
##Create a variable of the number of Patents patents per company
Patents2019$AIPat<-ifelse(Patents2019$AI==1,1,0)
Patents2019$AIPat<-as.numeric(Patents2019$AIPat)
Patents2019$AIPat[is.na(Patents2019$AIPat)] <- 0
Patents2019$AINoPat<- with(Patents2019, ave(Patents2019$AIPat,Patents2019$Company, FUN=sum))

#Sum number of AI applications per firm
Patents2019$AIApp1<-ifelse(Patents2019$Year==Patents2019$PRYear & Patents2019$AI==1,1,0)
Patents2019$AIApp1[is.na(Patents2019$AIApp1)] <- 0
Patents2019$AIApp<-with(Patents2019,ave(Patents2019$AIApp1,Patents2019$Company, FUN=sum,na.rm=TRUE))

#Sum number of acquired patents per firm
Patents2019$AIGF1<-ifelse(Patents2019$Added==2,Patents2019$AIPat,0)
Patents2019$AIGF1[is.na(Patents2019$AIGF1)] <- 0
Patents2019$AIGF<-with(Patents2019,ave(Patents2019$AIGF1,Patents2019$Company, FUN=sum,na.rm=TRUE))
Patents2019$AIBF1<-ifelse(Patents2019$Added==3,Patents2019$AIPat,0)
Patents2019$AIBF1[is.na(Patents2019$AIBF1)] <- 0
Patents2019$AIBF<-with(Patents2019,ave(Patents2019$AIBF1,Patents2019$Company, FUN=sum,na.rm=TRUE))

#Sum of self-developed Patents
Patents2019$AISDPat<-Patents2019$AIApp-Patents2019$AIGF-Patents2019$AIBF

##Create a variable of the number of granted Patents 2019 patents per company
Patents2019$AIGrant1<-ifelse(Patents2019$Granted=="Yes" & Patents2019$AI==1,1,0)
Patents2019$AIGrant1<-as.numeric(Patents2019$AIGrant1)
Patents2019$AIGrant1[is.na(Patents2019$AIGrant1)] <- 0
Patents2019$AIGrantPat<- with(Patents2019, ave(Patents2019$AIGrant1,Patents2019$Company, FUN=sum))

#Technical area (CPC code G means physics - describes rather basic research
Patents2019$Tech<-ifelse(Patents2019$AI==1 & Patents2019$Funct!=1,1,0)
Patents2019$Tech[is.na(Patents2019$Tech)] <- 0
Patents2019$Tech<-as.numeric(Patents2019$Tech)
Patents2019$TechPat<- with(Patents2019, ave(Patents2019$Tech,Patents2019$Company, FUN=sum))

#Technical Patent Applications
Patents2019$AppTech1<-ifelse(Patents2019$AI==1&Patents2019$Funct!=1&Patents2019$Year==Patents2019$PRYear,1,0)
Patents2019$AppTech1[is.na(Patents2019$AppTech1)] <- 0
Patents2019$AppTech<-with(Patents2019,ave(Patents2019$AppTech1,Patents2019$Company, FUN=sum,na.rm=TRUE))

#Functional area (CPC code not G means the application of Patents2019 technologies in other technical areas)
Patents2019$Funct1<-ifelse(Patents2019$AI==1 & Patents2019$Funct==1,1,0)
Patents2019$Funct1[is.na(Patents2019$Funct1)] <- 0
Patents2019$Funct1<-as.numeric(Patents2019$Funct)
Patents2019$FunctPat<- with(Patents2019, ave(Patents2019$Funct,Patents2019$Company, FUN=sum))

#Functional Patent Applications
Patents2019$AppFunct1<-ifelse(Patents2019$AI==1 & Patents2019$Funct==1&Patents2019$Year==Patents2019$PRYear,1,0)
Patents2019$AppFunct1[is.na(Patents2019$AppFunct1)] <- 0
Patents2019$AppFunct<-with(Patents2019,ave(Patents2019$AppFunct1,Patents2019$Company, FUN=sum,na.rm=TRUE))

#Remove duplicate companies
Patents2019<-Patents2019[!duplicated(Patents2019$BvDID),]

#Remove unnecessary data
Patents2019 <- Patents2019[,c(-3,-4,-5,-6,-7,-8,-9,-10,-11,-13,-15,-17,-20,-22,-23,-25,-27,-29,-31,-34,-36,-38,-40,-42)]

#Reorder dataset
names(Patents2019)<-c("Subsidiaries","Year","NoPat","App","GF","BF","SDPat","GrantPat","HighTech","AINoPat","AIApp","AIGF","AIBF",
                      "AISDPat","AIGrantPat","TechPat","AppTech","FunctPat","AppFunct")

##Patent data descriptives
sum(Patents2019$NoPat,na.rm = TRUE)
sum(Patents2019$App,na.rm = TRUE)
sum(Patents2019$GF,na.rm = TRUE)
sum(Patents2019$BF,na.rm = TRUE)
sum(Patents2019$SDPat,na.rm = TRUE)
sum(Patents2019$GrantPat,na.rm = TRUE)
sum(Patents2019$HighTech,na.rm = TRUE)
sum(Patents2019$AINoPat,na.rm = TRUE)
sum(Patents2019$AIApp,na.rm = TRUE)
sum(Patents2019$AIGF,na.rm = TRUE)
sum(Patents2019$AIBF,na.rm = TRUE)
sum(Patents2019$AISDPat,na.rm = TRUE)
sum(Patents2019$AIGrantPat,na.rm = TRUE)
sum(Patents2019$TechPat,na.rm = TRUE)
sum(Patents2019$AppTech,na.rm = TRUE)
sum(Patents2019$FunctPat,na.rm = TRUE)
sum(Patents2019$AppFunct,na.rm = TRUE)

write.csv2(Patents2019, file = "files_created_code1/Patents2019.csv",row.names = F)
rm(Patents2019)

## End of Patent2 Code

#I've runned the code until here on 18/08/2021; the next part was pushed to Code3-Dataset Code - Additional analysis,
#and it needs adaptations since files like Patents2011 were  not loaded until yet
