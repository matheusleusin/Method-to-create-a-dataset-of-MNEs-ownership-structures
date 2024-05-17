######################################### Establishing a global dataset of AI patents by MNEs #########################################

### Prepare R ####
#clear your global environment
rm(list=ls())

#Please pay attention to the following: 
#1st: Depending on your system, dec="," has to be adjusted for dec="." when reading files
#using the command 'fread'. You can see which one works best for you by reading the Full files created in Code1 (e.g.Full1.csv, 
#Full2.csv,...). Read them an look at the GDP variable; it should be in a numeric format like 3.45e+09, 
#instead of a character format like 3449688452,87021

#2nd: This code uses an alternative identification strategy for identifying AI patents, patent owners, and 
#stocks of patents. Code 2 uses the information described in the 'Data description_MNE_AI' file. This code
#3 was used by Felix for his own research, and uses accordingly definitions that were adopted on it.

#Increase Memory
options(java.parameters = "- Xmx1024m")
memory.limit(size=70000)

#set working directory:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#let's  create the invisible folder, where all files created using this code will be saved:
dir.create("files_created_code3")

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


###Part V: Match of company with AI data ####
#This is a continuation from Code1, which stopped on Part IV. It creates one file named 'Full', on the folder
#named 'files_created_code3'. This file is the used in Felix's analysis.

#   1. Add Patent data to the subsidiary data ####

## Add Patent data to the subsidiary data - 2011

Full1<-fread("files_created_code1/Full1.csv",sep=";", dec=",")
Patents2011<-fread("files_created_code1/Patents2011.csv",sep=";", dec=",")
#Patents2011_Alternative<-fread("files_created_code2/FinalDataset_allYears.csv",sep=";", dec=",")

names(Full1) <- c("GUO","Subsidiaries","Year","Added","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                  "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                  "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")

All11<-left_join(Full1,Patents2011,by="Subsidiaries",na_matches="never")

##Check quality of the match
#Check the NAs per rows
length(which(!is.na(All11$NoPat)))
#Check for the number of matches with BvDID Match #1692263 - NOW: 2169248
All11$Check<-ifelse(All11$GUO!=All11$Subsidiaries,1,NA)
length(which(!is.na(All11$Check)))
All11$Check<-NULL

#Sum number of patents per GUO
All11$NoPat<-as.numeric(All11$NoPat)
All11<-All11[order(All11[,1]),]
All11$Patents1<-ifelse(is.na(All11$NoPat),0,All11$NoPat)
All11$Patents1[is.na(All11$Patents1)] <- 0
All11$Patents<-with(All11,ave(All11$Patents1,All11$GUO, FUN=sum,na.rm=TRUE))
length(which(All11$Patents!=0))

#Sum number of patent applications per GUO
All11$App<-as.numeric(All11$App)
All11$Applications1<-ifelse(is.na(All11$App),0,All11$App)
All11$Applications1[is.na(All11$Applications1)] <- 0
All11$Applications<-with(All11,ave(All11$Applications1,All11$GUO, FUN=sum,na.rm=TRUE))
length(which(All11$Applications!=0))

#Sum number of acquired patents per firm
All11$Greenfield2<-ifelse(All11$Added==2,All11$Patents1,0)
All11$Greenfield1<-All11$Greenfield2+All11$GF
All11$Greenfield1[is.na(All11$Greenfield1)] <- 0
All11$Greenfield<-with(All11,ave(All11$Greenfield1,All11$GUO, FUN=sum,na.rm=TRUE))
length(which(All11$Greenfield!=0))
All11$Brownfield2<-ifelse(All11$Added==3,All11$Patents1,0)
All11$Brownfield1<-All11$Brownfield2+All11$BF
All11$Brownfield1[is.na(All11$Brownfield1)] <- 0
All11$Brownfield<-with(All11,ave(All11$Brownfield1,All11$GUO, FUN=sum,na.rm=TRUE))
length(which(All11$Brownfield!=0))

#Sum of self-developed Patents
All11$SDPatents<-All11$Applications/(All11$Applications+All11$Greenfield+All11$Brownfield)
length(which(All11$SDPatents!=0))

#Sum number of technical AI patents per GUO
All11$GrantPat<-as.numeric(All11$GrantPat)
All11$GrantPat1<-ifelse(is.na(All11$GrantPat),0,All11$GrantPat)
All11$GrantPat1[is.na(All11$GrantPat1)] <- 0
All11$GrantedPatents<-with(All11,ave(All11$GrantPat1,All11$GUO, FUN=sum,na.rm=TRUE))
length(which(All11$GrantedPatents!=0))

#Sum number of technical AI patents per GUO
All11$HighTech<-as.numeric(All11$HighTech)
All11$HighTech1<-ifelse(is.na(All11$HighTech),0,All11$HighTech)
All11$HighTech1[is.na(All11$HighTech1)] <- 0
All11$HighTechPatents<-with(All11,ave(All11$HighTech1,All11$GUO, FUN=sum,na.rm=TRUE))
length(which(All11$HighTechPatents!=0))

#Number of patents located in the subsidiaries
All11$SubPat2<-ifelse(All11$GUO!=All11$Subsidiaries,All11$NoPat,NA)
All11$SubPat1<-ifelse(is.na(All11$SubPat2),0,All11$SubPat2)
All11$SubPat1<-as.numeric(All11$SubPat1)
All11$SubPat1[is.na(All11$SubPat1)] <- 0
All11$SubPat<- with(All11,ave(All11$SubPat1,All11$GUO, FUN=sum))
length(which(All11$SubPat!=0))

#Number of patents located in the subsidiaries
All11$SubApp2<-ifelse(All11$GUO!=All11$Subsidiaries,All11$App,NA)
All11$SubApp1<-ifelse(is.na(All11$SubApp2),0,All11$SubApp2)
All11$SubApp1<-as.numeric(All11$SubApp1)
All11$SubApp1[is.na(All11$SubApp1)] <- 0
All11$SubApp<- with(All11,ave(All11$SubApp1,All11$GUO, FUN=sum))
length(which(All11$SubApp!=0))

#Number of patents located in foreign countries
All11$Country<-substr(All11$Subsidiaries, 1, 2)
All11$Country2<-substr(All11$GUO, 1, 2)
All11$ForPat2<-ifelse(All11$Country!=All11$Country2,All11$NoPat,NA)
All11$ForPat1<-ifelse(is.na(All11$ForPat2),0,All11$ForPat2)
All11$ForPat1<-as.numeric(All11$ForPat1)
All11$ForPat1[is.na(All11$ForPat1)] <- 0
All11$ForPat<- with(All11,ave(All11$ForPat1,All11$GUO, FUN=sum))

#Number of patents located in foreign countries
All11$ForApp2<-ifelse(All11$Country!=All11$Country2,All11$App,NA)
All11$ForApp1<-ifelse(is.na(All11$ForApp2),0,All11$ForApp2)
All11$ForApp1<-as.numeric(All11$ForApp1)
All11$ForApp1[is.na(All11$ForApp1)] <- 0
All11$ForApp<- with(All11,ave(All11$ForApp1,All11$GUO, FUN=sum))


##AI Patents
#Sum number of AI patents per GUO
All11$AINoPat<-as.numeric(All11$AINoPat)
All11$AIPatents1<-ifelse(is.na(All11$AINoPat),0,All11$AINoPat)
All11$AIPatents1[is.na(All11$AIPatents1)] <- 0
All11$AIPatents<-with(All11,ave(All11$AIPatents1,All11$GUO, FUN=sum,na.rm=TRUE))
length(which(All11$AIPatents!=0))

#Sum number of patent applications per GUO
All11$AIApp<-as.numeric(All11$AIApp)
All11$AIApplications1<-ifelse(is.na(All11$AIApp),0,All11$AIApp)
All11$AIApplications1[is.na(All11$AIApplications1)] <- 0
All11$AIApplications<-with(All11,ave(All11$AIApplications1,All11$GUO, FUN=sum,na.rm=TRUE))
length(which(All11$AIApplications!=0))

#Sum number of granted AI patents per GUO
All11$AIGrantPat<-as.numeric(All11$AIGrantPat)
All11$AIGrantPat1<-ifelse(is.na(All11$AIGrantPat),0,All11$AIGrantPat)
All11$AIGrantPat1[is.na(All11$AIGrantPat1)] <- 0
All11$AIGrantedPatents<-with(All11,ave(All11$AIGrantPat1,All11$GUO, FUN=sum,na.rm=TRUE))
length(which(All11$AIGrantedPatents!=0))

#Sum number of acquired patents per firm
All11$AIGreenfield2<-ifelse(All11$Added==2,All11$AIPatents1,0)
All11$AIGreenfield1<-All11$AIGreenfield2+All11$AIGF
All11$AIGreenfield1[is.na(All11$AIGreenfield1)] <- 0
All11$AIGreenfield<-with(All11,ave(All11$AIGreenfield1,All11$GUO, FUN=sum,na.rm=TRUE))
length(which(All11$AIGreenfield!=0))
All11$AIBrownfield2<-ifelse(All11$Added==3,All11$AIPatents1,0)
All11$AIBrownfield1<-All11$AIBrownfield2+All11$AIBF
All11$AIBrownfield1[is.na(All11$AIBrownfield1)] <- 0
All11$AIBrownfield<-with(All11,ave(All11$AIBrownfield1,All11$GUO, FUN=sum,na.rm=TRUE))
length(which(All11$AIBrownfield!=0))

#Sum of self-developed Patents
All11$AISDPatents<-All11$AIApplications/(All11$AIApplications+All11$AIGreenfield+All11$AIBrownfield)
length(which(All11$SDPatents!=0))

#Sum number of technical AI patents per GUO
All11$TechPat<-as.numeric(All11$TechPat)
All11$TechPat1<-ifelse(is.na(All11$TechPat),0,All11$TechPat)
All11$TechPat1[is.na(All11$TechPat1)] <- 0
All11$AITechPatents<-with(All11,ave(All11$TechPat1,All11$GUO, FUN=sum,na.rm=TRUE))
length(which(All11$AITechPatents!=0))

#Sum number of technical AI patents per GUO
All11$AppTech<-as.numeric(All11$AppTech)
All11$TechApplications1<-ifelse(is.na(All11$AppTech),0,All11$AppTech)
All11$TechApplications1[is.na(All11$TechApplications1)] <- 0
All11$AITechApplications<-with(All11,ave(All11$TechApplications1,All11$GUO, FUN=sum,na.rm=TRUE))
length(which(All11$TechApplications!=0))

#Sum number of Functional AI patents per GUO
All11$FunctPat<-as.numeric(All11$FunctPat)
All11$FunctPat1<-ifelse(is.na(All11$FunctPat),0,All11$FunctPat)
All11$FunctPat1[is.na(All11$FunctPat1)] <- 0
All11$AIFunctPatents<-with(All11,ave(All11$FunctPat1,All11$GUO, FUN=sum,na.rm=TRUE))
length(which(All11$AIFunctPatents!=0))

#Sum number of Functional AI patents per GUO
All11$AppFunct<-as.numeric(All11$AppFunct)
All11$FunctApplications1<-ifelse(is.na(All11$AppFunct),0,All11$AppFunct)
All11$FunctApplications1[is.na(All11$FunctApplications1)] <- 0
All11$AIFunctApplications<-with(All11,ave(All11$FunctApplications1,All11$GUO, FUN=sum,na.rm=TRUE))
length(which(All11$FunctApplications!=0))

#Number of patents located in the subsidiaries
All11$AISubPat2<-ifelse(All11$GUO!=All11$Subsidiaries,All11$AINoPat,NA)
All11$AISubPat1<-ifelse(is.na(All11$AISubPat2),0,All11$AISubPat2)
All11$AISubPat1<-as.numeric(All11$AISubPat1)
All11$AISubPat1[is.na(All11$AISubPat1)] <- 0
All11$AISubPat<- with(All11,ave(All11$AISubPat1,All11$GUO, FUN=sum))
length(which(All11$AISubPat!=0))

#Number of patents located in the AISubsidiaries
All11$AISubApp2<-ifelse(All11$GUO!=All11$Subsidiaries,All11$AIApp,NA)
All11$AISubApp1<-ifelse(is.na(All11$AISubApp2),0,All11$AISubApp2)
All11$AISubApp1<-as.numeric(All11$AISubApp1)
All11$AISubApp1[is.na(All11$AISubApp1)] <- 0
All11$AISubApp<- with(All11,ave(All11$AISubApp1,All11$GUO, FUN=sum))
length(which(All11$AISubApp!=0))

#Number of patents located in foreign countries
All11$AIForPat2<-ifelse(All11$Country!=All11$Country2,All11$AINoPat,NA)
All11$AIForPat1<-ifelse(is.na(All11$AIForPat2),0,All11$AIForPat2)
All11$AIForPat1<-as.numeric(All11$AIForPat1)
All11$AIForPat1[is.na(All11$AIForPat1)] <- 0
All11$AIForPat<- with(All11,ave(All11$AIForPat1,All11$GUO, FUN=sum))

#Number of patents located in AIForeign countries
All11$AIForApp2<-ifelse(All11$Country!=All11$Country2,All11$AIApp,NA)
All11$AIForApp1<-ifelse(is.na(All11$AIForApp2),0,All11$AIForApp2)
All11$AIForApp1<-as.numeric(All11$AIForApp1)
All11$AIForApp1[is.na(All11$AIForApp1)] <- 0
All11$AIForApp<- with(All11,ave(All11$AIForApp1,All11$GUO, FUN=sum))

#Delete unnecessary variables
All11[,34:52]<-NULL
All11<-All11[,c(-2,-4,-35,-37,-38,-40,-41,-44,-46,-48,-49,-51,-52,-54,-55,-56,-57,-59,-60,-62,-64,-66,-68,-69,-71,-72,-75,-77,-79,-81,-83,-84,-86,-87,-89,-90,-92,-93)]

#Delete duplicate GUOs
All11<-All11[!duplicated(All11$GUO),]

#Rename Dataset
names(All11)<-c("BvDID","Year","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub","Patents","Applications",
                "GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents","SubPat","SubApp","ForPat","ForApp",
                "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
                "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp")

#Patent data descriptives
sum(All11$Patents,na.rm = TRUE)
sum(All11$Applications,na.rm = TRUE)
sum(All11$GreenfieldPatents,na.rm = TRUE)
sum(All11$BrownfieldPatents,na.rm = TRUE)
sum(All11$SDPatents,na.rm = TRUE)
sum(All11$GrantedPatents,na.rm = TRUE)
sum(All11$HighTechPatents,na.rm = TRUE)
sum(All11$SubPat,na.rm = TRUE)
sum(All11$SubApp,na.rm = TRUE)
sum(All11$ForPat,na.rm = TRUE)
sum(All11$ForApp,na.rm = TRUE)
sum(All11$AIPatents,na.rm = TRUE)
sum(All11$AIApplications,na.rm = TRUE)
sum(All11$AIGrantedPatents,na.rm = TRUE)
sum(All11$AIGreenfieldPatents,na.rm = TRUE)
sum(All11$AIBrownfieldPatents,na.rm = TRUE)
sum(All11$AISDPatents,na.rm = TRUE)
sum(All11$AITechPatents,na.rm = TRUE)
sum(All11$AITechApps,na.rm = TRUE)
sum(All11$AIFunctPatents,na.rm = TRUE)
sum(All11$AIFunctApps,na.rm = TRUE)
sum(All11$AISubPat,na.rm = TRUE)
sum(All11$AISubApp,na.rm = TRUE)
sum(All11$AIForPat,na.rm = TRUE)
sum(All11$AIForApp,na.rm = TRUE)



##Add Patent data to the subsidiary data - 2012

Full2<-fread("files_created_code1/Full2.csv",sep=";", dec=",")
Patents2012<-fread("files_created_code1/Patents2012.csv",sep=";", dec=",")

names(Full2) <- c("GUO","Subsidiaries","Year","Added","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                  "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                  "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")

All12<-left_join(Full2,Patents2012,by="Subsidiaries",na_matches="never")

##Check quality of the match
#Check the NAs per rows
length(which(!is.na(All12$NoPat)))
#Check for the number of matches with BvDID Match #1692263 - NOW: 2169248
All12$Check<-ifelse(All12$GUO!=All12$Subsidiaries,1,NA)
length(which(!is.na(All12$Check)))
All12$Check<-NULL

#Sum number of patents per GUO
All12$NoPat<-as.numeric(All12$NoPat)
All12<-All12[order(All12[,1]),]
All12$Patents1<-ifelse(is.na(All12$NoPat),0,All12$NoPat)
All12$Patents1[is.na(All12$Patents1)] <- 0
All12$Patents<-with(All12,ave(All12$Patents1,All12$GUO, FUN=sum,na.rm=TRUE))
length(which(All12$Patents!=0))

#Sum number of patent applications per GUO
All12$App<-as.numeric(All12$App)
All12$Applications1<-ifelse(is.na(All12$App),0,All12$App)
All12$Applications1[is.na(All12$Applications1)] <- 0
All12$Applications<-with(All12,ave(All12$Applications1,All12$GUO, FUN=sum,na.rm=TRUE))
length(which(All12$Applications!=0))

#Sum number of acquired patents per firm
All12$Greenfield2<-ifelse(All12$Added==2,All12$Patents1,0)
All12$Greenfield1<-All12$Greenfield2+All12$GF
All12$Greenfield1[is.na(All12$Greenfield1)] <- 0
All12$Greenfield<-with(All12,ave(All12$Greenfield1,All12$GUO, FUN=sum,na.rm=TRUE))
length(which(All12$Greenfield!=0))
All12$Brownfield2<-ifelse(All12$Added==3,All12$Patents1,0)
All12$Brownfield1<-All12$Brownfield2+All12$BF
All12$Brownfield1[is.na(All12$Brownfield1)] <- 0
All12$Brownfield<-with(All12,ave(All12$Brownfield1,All12$GUO, FUN=sum,na.rm=TRUE))
length(which(All12$Brownfield!=0))

#Sum of self-developed Patents
All12$SDPatents<-All12$Applications/(All12$Applications+All12$Greenfield+All12$Brownfield)
length(which(All12$SDPatents!=0))

#Sum number of technical AI patents per GUO
All12$GrantPat<-as.numeric(All12$GrantPat)
All12$GrantPat1<-ifelse(is.na(All12$GrantPat),0,All12$GrantPat)
All12$GrantPat1[is.na(All12$GrantPat1)] <- 0
All12$GrantedPatents<-with(All12,ave(All12$GrantPat1,All12$GUO, FUN=sum,na.rm=TRUE))
length(which(All12$GrantedPatents!=0))

#Sum number of technical AI patents per GUO
All12$HighTech<-as.numeric(All12$HighTech)
All12$HighTech1<-ifelse(is.na(All12$HighTech),0,All12$HighTech)
All12$HighTech1[is.na(All12$HighTech1)] <- 0
All12$HighTechPatents<-with(All12,ave(All12$HighTech1,All12$GUO, FUN=sum,na.rm=TRUE))
length(which(All12$HighTechPatents!=0))

#Number of patents located in the subsidiaries
All12$SubPat2<-ifelse(All12$GUO!=All12$Subsidiaries,All12$NoPat,NA)
All12$SubPat1<-ifelse(is.na(All12$SubPat2),0,All12$SubPat2)
All12$SubPat1<-as.numeric(All12$SubPat1)
All12$SubPat1[is.na(All12$SubPat1)] <- 0
All12$SubPat<- with(All12,ave(All12$SubPat1,All12$GUO, FUN=sum))
length(which(All12$SubPat!=0))

#Number of patents located in the subsidiaries
All12$SubApp2<-ifelse(All12$GUO!=All12$Subsidiaries,All12$App,NA)
All12$SubApp1<-ifelse(is.na(All12$SubApp2),0,All12$SubApp2)
All12$SubApp1<-as.numeric(All12$SubApp1)
All12$SubApp1[is.na(All12$SubApp1)] <- 0
All12$SubApp<- with(All12,ave(All12$SubApp1,All12$GUO, FUN=sum))
length(which(All12$SubApp!=0))

#Number of patents located in foreign countries
All12$Country<-substr(All12$Subsidiaries, 1, 2)
All12$Country2<-substr(All12$GUO, 1, 2)
All12$ForPat2<-ifelse(All12$Country!=All12$Country2,All12$NoPat,NA)
All12$ForPat1<-ifelse(is.na(All12$ForPat2),0,All12$ForPat2)
All12$ForPat1<-as.numeric(All12$ForPat1)
All12$ForPat1[is.na(All12$ForPat1)] <- 0
All12$ForPat<- with(All12,ave(All12$ForPat1,All12$GUO, FUN=sum))

#Number of patents located in foreign countries
All12$ForApp2<-ifelse(All12$Country!=All12$Country2,All12$App,NA)
All12$ForApp1<-ifelse(is.na(All12$ForApp2),0,All12$ForApp2)
All12$ForApp1<-as.numeric(All12$ForApp1)
All12$ForApp1[is.na(All12$ForApp1)] <- 0
All12$ForApp<- with(All12,ave(All12$ForApp1,All12$GUO, FUN=sum))


##AI Patents
#Sum number of AI patents per GUO
All12$AINoPat<-as.numeric(All12$AINoPat)
All12$AIPatents1<-ifelse(is.na(All12$AINoPat),0,All12$AINoPat)
All12$AIPatents1[is.na(All12$AIPatents1)] <- 0
All12$AIPatents<-with(All12,ave(All12$AIPatents1,All12$GUO, FUN=sum,na.rm=TRUE))
length(which(All12$AIPatents!=0))

#Sum number of patent applications per GUO
All12$AIApp<-as.numeric(All12$AIApp)
All12$AIApplications1<-ifelse(is.na(All12$AIApp),0,All12$AIApp)
All12$AIApplications1[is.na(All12$AIApplications1)] <- 0
All12$AIApplications<-with(All12,ave(All12$AIApplications1,All12$GUO, FUN=sum,na.rm=TRUE))
length(which(All12$AIApplications!=0))

#Sum number of granted AI patents per GUO
All12$AIGrantPat<-as.numeric(All12$AIGrantPat)
All12$AIGrantPat1<-ifelse(is.na(All12$AIGrantPat),0,All12$AIGrantPat)
All12$AIGrantPat1[is.na(All12$AIGrantPat1)] <- 0
All12$AIGrantedPatents<-with(All12,ave(All12$AIGrantPat1,All12$GUO, FUN=sum,na.rm=TRUE))
length(which(All12$AIGrantedPatents!=0))

#Sum number of acquired patents per firm
All12$AIGreenfield2<-ifelse(All12$Added==2,All12$AIPatents1,0)
All12$AIGreenfield1<-All12$AIGreenfield2+All12$AIGF
All12$AIGreenfield1[is.na(All12$AIGreenfield1)] <- 0
All12$AIGreenfield<-with(All12,ave(All12$AIGreenfield1,All12$GUO, FUN=sum,na.rm=TRUE))
length(which(All12$AIGreenfield!=0))
All12$AIBrownfield2<-ifelse(All12$Added==3,All12$AIPatents1,0)
All12$AIBrownfield1<-All12$AIBrownfield2+All12$AIBF
All12$AIBrownfield1[is.na(All12$AIBrownfield1)] <- 0
All12$AIBrownfield<-with(All12,ave(All12$AIBrownfield1,All12$GUO, FUN=sum,na.rm=TRUE))
length(which(All12$AIBrownfield!=0))

#Sum of self-developed Patents
All12$AISDPatents<-All12$AIApplications/(All12$AIApplications+All12$AIGreenfield+All12$AIBrownfield)
length(which(All12$SDPatents!=0))

#Sum number of technical AI patents per GUO
All12$TechPat<-as.numeric(All12$TechPat)
All12$TechPat1<-ifelse(is.na(All12$TechPat),0,All12$TechPat)
All12$TechPat1[is.na(All12$TechPat1)] <- 0
All12$AITechPatents<-with(All12,ave(All12$TechPat1,All12$GUO, FUN=sum,na.rm=TRUE))
length(which(All12$AITechPatents!=0))

#Sum number of technical AI patents per GUO
All12$AppTech<-as.numeric(All12$AppTech)
All12$TechApplications1<-ifelse(is.na(All12$AppTech),0,All12$AppTech)
All12$TechApplications1[is.na(All12$TechApplications1)] <- 0
All12$AITechApplications<-with(All12,ave(All12$TechApplications1,All12$GUO, FUN=sum,na.rm=TRUE))
length(which(All12$TechApplications!=0))

#Sum number of Functional AI patents per GUO
All12$FunctPat<-as.numeric(All12$FunctPat)
All12$FunctPat1<-ifelse(is.na(All12$FunctPat),0,All12$FunctPat)
All12$FunctPat1[is.na(All12$FunctPat1)] <- 0
All12$AIFunctPatents<-with(All12,ave(All12$FunctPat1,All12$GUO, FUN=sum,na.rm=TRUE))
length(which(All12$AIFunctPatents!=0))

#Sum number of Functional AI patents per GUO
All12$AppFunct<-as.numeric(All12$AppFunct)
All12$FunctApplications1<-ifelse(is.na(All12$AppFunct),0,All12$AppFunct)
All12$FunctApplications1[is.na(All12$FunctApplications1)] <- 0
All12$AIFunctApplications<-with(All12,ave(All12$FunctApplications1,All12$GUO, FUN=sum,na.rm=TRUE))
length(which(All12$FunctApplications!=0))

#Number of patents located in the subsidiaries
All12$AISubPat2<-ifelse(All12$GUO!=All12$Subsidiaries,All12$AINoPat,NA)
All12$AISubPat1<-ifelse(is.na(All12$AISubPat2),0,All12$AISubPat2)
All12$AISubPat1<-as.numeric(All12$AISubPat1)
All12$AISubPat1[is.na(All12$AISubPat1)] <- 0
All12$AISubPat<- with(All12,ave(All12$AISubPat1,All12$GUO, FUN=sum))
length(which(All12$AISubPat!=0))

#Number of patents located in the AISubsidiaries
All12$AISubApp2<-ifelse(All12$GUO!=All12$Subsidiaries,All12$AIApp,NA)
All12$AISubApp1<-ifelse(is.na(All12$AISubApp2),0,All12$AISubApp2)
All12$AISubApp1<-as.numeric(All12$AISubApp1)
All12$AISubApp1[is.na(All12$AISubApp1)] <- 0
All12$AISubApp<- with(All12,ave(All12$AISubApp1,All12$GUO, FUN=sum))
length(which(All12$AISubApp!=0))

#Number of patents located in foreign countries
All12$AIForPat2<-ifelse(All12$Country!=All12$Country2,All12$AINoPat,NA)
All12$AIForPat1<-ifelse(is.na(All12$AIForPat2),0,All12$AIForPat2)
All12$AIForPat1<-as.numeric(All12$AIForPat1)
All12$AIForPat1[is.na(All12$AIForPat1)] <- 0
All12$AIForPat<- with(All12,ave(All12$AIForPat1,All12$GUO, FUN=sum))

#Number of patents located in AIForeign countries
All12$AIForApp2<-ifelse(All12$Country!=All12$Country2,All12$AIApp,NA)
All12$AIForApp1<-ifelse(is.na(All12$AIForApp2),0,All12$AIForApp2)
All12$AIForApp1<-as.numeric(All12$AIForApp1)
All12$AIForApp1[is.na(All12$AIForApp1)] <- 0
All12$AIForApp<- with(All12,ave(All12$AIForApp1,All12$GUO, FUN=sum))

#Delete unnecessary variables
All12[,34:52]<-NULL
All12<-All12[,c(-2,-4,-35,-37,-38,-40,-41,-44,-46,-48,-49,-51,-52,-54,-55,-56,-57,-59,-60,-62,-64,-66,-68,-69,-71,-72,-75,-77,-79,-81,-83,-84,-86,-87,-89,-90,-92,-93)]

#Delete duplicate GUOs
All12<-All12[!duplicated(All12$GUO),]

#Rename Dataset
names(All12)<-c("BvDID","Year","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub","Patents","Applications",
                "GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents","SubPat","SubApp","ForPat","ForApp",
                "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
                "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp")

#Patent data descriptives
sum(All12$Patents,na.rm = TRUE)
sum(All12$Applications,na.rm = TRUE)
sum(All12$GreenfieldPatents,na.rm = TRUE)
sum(All12$BrownfieldPatents,na.rm = TRUE)
sum(All12$SDPatents,na.rm = TRUE)
sum(All12$GrantedPatents,na.rm = TRUE)
sum(All12$HighTechPatents,na.rm = TRUE)
sum(All12$SubPat,na.rm = TRUE)
sum(All12$SubApp,na.rm = TRUE)
sum(All12$ForPat,na.rm = TRUE)
sum(All12$ForApp,na.rm = TRUE)
sum(All12$AIPatents,na.rm = TRUE)
sum(All12$AIApplications,na.rm = TRUE)
sum(All12$AIGrantedPatents,na.rm = TRUE)
sum(All12$AIGreenfieldPatents,na.rm = TRUE)
sum(All12$AIBrownfieldPatents,na.rm = TRUE)
sum(All12$AISDPatents,na.rm = TRUE)
sum(All12$AITechPatents,na.rm = TRUE)
sum(All12$AITechApps,na.rm = TRUE)
sum(All12$AIFunctPatents,na.rm = TRUE)
sum(All12$AIFunctApps,na.rm = TRUE)
sum(All12$AISubPat,na.rm = TRUE)
sum(All12$AISubApp,na.rm = TRUE)
sum(All12$AIForPat,na.rm = TRUE)
sum(All12$AIForApp,na.rm = TRUE)


##Add Patent data to the subsidiary data - 2013
Full3<-fread("files_created_code1/Full3.csv",sep=";", dec=",")
Patents2013<-fread("files_created_code1/Patents2013.csv",sep=";", dec=",")

names(Full3) <- c("GUO","Subsidiaries","Year","Added","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                  "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                  "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")

All13<-left_join(Full3,Patents2013,by="Subsidiaries",na_matches="never")

##Check quality of the match
#Check the NAs per rows
length(which(!is.na(All13$NoPat)))
#Check for the number of matches with BvDID Match #1692263 - NOW: 2169248
All13$Check<-ifelse(All13$GUO!=All13$Subsidiaries,1,NA)
length(which(!is.na(All13$Check)))
All13$Check<-NULL

#Sum number of patents per GUO
All13$NoPat<-as.numeric(All13$NoPat)
All13<-All13[order(All13[,1]),]
All13$Patents1<-ifelse(is.na(All13$NoPat),0,All13$NoPat)
All13$Patents1[is.na(All13$Patents1)] <- 0
All13$Patents<-with(All13,ave(All13$Patents1,All13$GUO, FUN=sum,na.rm=TRUE))
length(which(All13$Patents!=0))

#Sum number of patent applications per GUO
All13$App<-as.numeric(All13$App)
All13$Applications1<-ifelse(is.na(All13$App),0,All13$App)
All13$Applications1[is.na(All13$Applications1)] <- 0
All13$Applications<-with(All13,ave(All13$Applications1,All13$GUO, FUN=sum,na.rm=TRUE))
length(which(All13$Applications!=0))

#Sum number of acquired patents per firm
All13$Greenfield2<-ifelse(All13$Added==2,All13$Patents1,0)
All13$Greenfield1<-All13$Greenfield2+All13$GF
All13$Greenfield1[is.na(All13$Greenfield1)] <- 0
All13$Greenfield<-with(All13,ave(All13$Greenfield1,All13$GUO, FUN=sum,na.rm=TRUE))
length(which(All13$Greenfield!=0))
All13$Brownfield2<-ifelse(All13$Added==3,All13$Patents1,0)
All13$Brownfield1<-All13$Brownfield2+All13$BF
All13$Brownfield1[is.na(All13$Brownfield1)] <- 0
All13$Brownfield<-with(All13,ave(All13$Brownfield1,All13$GUO, FUN=sum,na.rm=TRUE))
length(which(All13$Brownfield!=0))

#Sum of self-developed Patents
All13$SDPatents<-All13$Applications/(All13$Applications+All13$Greenfield+All13$Brownfield)
length(which(All13$SDPatents!=0))

#Sum number of technical AI patents per GUO
All13$GrantPat<-as.numeric(All13$GrantPat)
All13$GrantPat1<-ifelse(is.na(All13$GrantPat),0,All13$GrantPat)
All13$GrantPat1[is.na(All13$GrantPat1)] <- 0
All13$GrantedPatents<-with(All13,ave(All13$GrantPat1,All13$GUO, FUN=sum,na.rm=TRUE))
length(which(All13$GrantedPatents!=0))

#Sum number of technical AI patents per GUO
All13$HighTech<-as.numeric(All13$HighTech)
All13$HighTech1<-ifelse(is.na(All13$HighTech),0,All13$HighTech)
All13$HighTech1[is.na(All13$HighTech1)] <- 0
All13$HighTechPatents<-with(All13,ave(All13$HighTech1,All13$GUO, FUN=sum,na.rm=TRUE))
length(which(All13$HighTechPatents!=0))

#Number of patents located in the subsidiaries
All13$SubPat2<-ifelse(All13$GUO!=All13$Subsidiaries,All13$NoPat,NA)
All13$SubPat1<-ifelse(is.na(All13$SubPat2),0,All13$SubPat2)
All13$SubPat1<-as.numeric(All13$SubPat1)
All13$SubPat1[is.na(All13$SubPat1)] <- 0
All13$SubPat<- with(All13,ave(All13$SubPat1,All13$GUO, FUN=sum))
length(which(All13$SubPat!=0))

#Number of patents located in the subsidiaries
All13$SubApp2<-ifelse(All13$GUO!=All13$Subsidiaries,All13$App,NA)
All13$SubApp1<-ifelse(is.na(All13$SubApp2),0,All13$SubApp2)
All13$SubApp1<-as.numeric(All13$SubApp1)
All13$SubApp1[is.na(All13$SubApp1)] <- 0
All13$SubApp<- with(All13,ave(All13$SubApp1,All13$GUO, FUN=sum))
length(which(All13$SubApp!=0))

#Number of patents located in foreign countries
All13$Country<-substr(All13$Subsidiaries, 1, 2)
All13$Country2<-substr(All13$GUO, 1, 2)
All13$ForPat2<-ifelse(All13$Country!=All13$Country2,All13$NoPat,NA)
All13$ForPat1<-ifelse(is.na(All13$ForPat2),0,All13$ForPat2)
All13$ForPat1<-as.numeric(All13$ForPat1)
All13$ForPat1[is.na(All13$ForPat1)] <- 0
All13$ForPat<- with(All13,ave(All13$ForPat1,All13$GUO, FUN=sum))

#Number of patents located in foreign countries
All13$ForApp2<-ifelse(All13$Country!=All13$Country2,All13$App,NA)
All13$ForApp1<-ifelse(is.na(All13$ForApp2),0,All13$ForApp2)
All13$ForApp1<-as.numeric(All13$ForApp1)
All13$ForApp1[is.na(All13$ForApp1)] <- 0
All13$ForApp<- with(All13,ave(All13$ForApp1,All13$GUO, FUN=sum))


##AI Patents
#Sum number of AI patents per GUO
All13$AINoPat<-as.numeric(All13$AINoPat)
All13$AIPatents1<-ifelse(is.na(All13$AINoPat),0,All13$AINoPat)
All13$AIPatents1[is.na(All13$AIPatents1)] <- 0
All13$AIPatents<-with(All13,ave(All13$AIPatents1,All13$GUO, FUN=sum,na.rm=TRUE))
length(which(All13$AIPatents!=0))

#Sum number of patent applications per GUO
All13$AIApp<-as.numeric(All13$AIApp)
All13$AIApplications1<-ifelse(is.na(All13$AIApp),0,All13$AIApp)
All13$AIApplications1[is.na(All13$AIApplications1)] <- 0
All13$AIApplications<-with(All13,ave(All13$AIApplications1,All13$GUO, FUN=sum,na.rm=TRUE))
length(which(All13$AIApplications!=0))

#Sum number of granted AI patents per GUO
All13$AIGrantPat<-as.numeric(All13$AIGrantPat)
All13$AIGrantPat1<-ifelse(is.na(All13$AIGrantPat),0,All13$AIGrantPat)
All13$AIGrantPat1[is.na(All13$AIGrantPat1)] <- 0
All13$AIGrantedPatents<-with(All13,ave(All13$AIGrantPat1,All13$GUO, FUN=sum,na.rm=TRUE))
length(which(All13$AIGrantedPatents!=0))

#Sum number of acquired patents per firm
All13$AIGreenfield2<-ifelse(All13$Added==2,All13$AIPatents1,0)
All13$AIGreenfield1<-All13$AIGreenfield2+All13$AIGF
All13$AIGreenfield1[is.na(All13$AIGreenfield1)] <- 0
All13$AIGreenfield<-with(All13,ave(All13$AIGreenfield1,All13$GUO, FUN=sum,na.rm=TRUE))
length(which(All13$AIGreenfield!=0))
All13$AIBrownfield2<-ifelse(All13$Added==3,All13$AIPatents1,0)
All13$AIBrownfield1<-All13$AIBrownfield2+All13$AIBF
All13$AIBrownfield1[is.na(All13$AIBrownfield1)] <- 0
All13$AIBrownfield<-with(All13,ave(All13$AIBrownfield1,All13$GUO, FUN=sum,na.rm=TRUE))
length(which(All13$AIBrownfield!=0))

#Sum of self-developed Patents
All13$AISDPatents<-All13$AIApplications/(All13$AIApplications+All13$AIGreenfield+All13$AIBrownfield)
length(which(All13$SDPatents!=0))

#Sum number of technical AI patents per GUO
All13$TechPat<-as.numeric(All13$TechPat)
All13$TechPat1<-ifelse(is.na(All13$TechPat),0,All13$TechPat)
All13$TechPat1[is.na(All13$TechPat1)] <- 0
All13$AITechPatents<-with(All13,ave(All13$TechPat1,All13$GUO, FUN=sum,na.rm=TRUE))
length(which(All13$AITechPatents!=0))

#Sum number of technical AI patents per GUO
All13$AppTech<-as.numeric(All13$AppTech)
All13$TechApplications1<-ifelse(is.na(All13$AppTech),0,All13$AppTech)
All13$TechApplications1[is.na(All13$TechApplications1)] <- 0
All13$AITechApplications<-with(All13,ave(All13$TechApplications1,All13$GUO, FUN=sum,na.rm=TRUE))
length(which(All13$TechApplications!=0))

#Sum number of Functional AI patents per GUO
All13$FunctPat<-as.numeric(All13$FunctPat)
All13$FunctPat1<-ifelse(is.na(All13$FunctPat),0,All13$FunctPat)
All13$FunctPat1[is.na(All13$FunctPat1)] <- 0
All13$AIFunctPatents<-with(All13,ave(All13$FunctPat1,All13$GUO, FUN=sum,na.rm=TRUE))
length(which(All13$AIFunctPatents!=0))

#Sum number of Functional AI patents per GUO
All13$AppFunct<-as.numeric(All13$AppFunct)
All13$FunctApplications1<-ifelse(is.na(All13$AppFunct),0,All13$AppFunct)
All13$FunctApplications1[is.na(All13$FunctApplications1)] <- 0
All13$AIFunctApplications<-with(All13,ave(All13$FunctApplications1,All13$GUO, FUN=sum,na.rm=TRUE))
length(which(All13$FunctApplications!=0))

#Number of patents located in the subsidiaries
All13$AISubPat2<-ifelse(All13$GUO!=All13$Subsidiaries,All13$AINoPat,NA)
All13$AISubPat1<-ifelse(is.na(All13$AISubPat2),0,All13$AISubPat2)
All13$AISubPat1<-as.numeric(All13$AISubPat1)
All13$AISubPat1[is.na(All13$AISubPat1)] <- 0
All13$AISubPat<- with(All13,ave(All13$AISubPat1,All13$GUO, FUN=sum))
length(which(All13$AISubPat!=0))

#Number of patents located in the AISubsidiaries
All13$AISubApp2<-ifelse(All13$GUO!=All13$Subsidiaries,All13$AIApp,NA)
All13$AISubApp1<-ifelse(is.na(All13$AISubApp2),0,All13$AISubApp2)
All13$AISubApp1<-as.numeric(All13$AISubApp1)
All13$AISubApp1[is.na(All13$AISubApp1)] <- 0
All13$AISubApp<- with(All13,ave(All13$AISubApp1,All13$GUO, FUN=sum))
length(which(All13$AISubApp!=0))

#Number of patents located in foreign countries
All13$AIForPat2<-ifelse(All13$Country!=All13$Country2,All13$AINoPat,NA)
All13$AIForPat1<-ifelse(is.na(All13$AIForPat2),0,All13$AIForPat2)
All13$AIForPat1<-as.numeric(All13$AIForPat1)
All13$AIForPat1[is.na(All13$AIForPat1)] <- 0
All13$AIForPat<- with(All13,ave(All13$AIForPat1,All13$GUO, FUN=sum))

#Number of patents located in AIForeign countries
All13$AIForApp2<-ifelse(All13$Country!=All13$Country2,All13$AIApp,NA)
All13$AIForApp1<-ifelse(is.na(All13$AIForApp2),0,All13$AIForApp2)
All13$AIForApp1<-as.numeric(All13$AIForApp1)
All13$AIForApp1[is.na(All13$AIForApp1)] <- 0
All13$AIForApp<- with(All13,ave(All13$AIForApp1,All13$GUO, FUN=sum))

#Delete unnecessary variables
All13[,34:52]<-NULL
All13<-All13[,c(-2,-4,-35,-37,-38,-40,-41,-44,-46,-48,-49,-51,-52,-54,-55,-56,-57,-59,-60,-62,-64,-66,-68,-69,-71,-72,-75,-77,-79,-81,-83,-84,-86,-87,-89,-90,-92,-93)]

#Delete duplicate GUOs
All13<-All13[!duplicated(All13$GUO),]

#Rename Dataset
names(All13)<-c("BvDID","Year","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub","Patents","Applications",
                "GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents","SubPat","SubApp","ForPat","ForApp",
                "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
                "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp")

#Patent data descriptives
sum(All13$Patents,na.rm = TRUE)
sum(All13$Applications,na.rm = TRUE)
sum(All13$GreenfieldPatents,na.rm = TRUE)
sum(All13$BrownfieldPatents,na.rm = TRUE)
sum(All13$SDPatents,na.rm = TRUE)
sum(All13$GrantedPatents,na.rm = TRUE)
sum(All13$HighTechPatents,na.rm = TRUE)
sum(All13$SubPat,na.rm = TRUE)
sum(All13$SubApp,na.rm = TRUE)
sum(All13$ForPat,na.rm = TRUE)
sum(All13$ForApp,na.rm = TRUE)
sum(All13$AIPatents,na.rm = TRUE)
sum(All13$AIApplications,na.rm = TRUE)
sum(All13$AIGrantedPatents,na.rm = TRUE)
sum(All13$AIGreenfieldPatents,na.rm = TRUE)
sum(All13$AIBrownfieldPatents,na.rm = TRUE)
sum(All13$AISDPatents,na.rm = TRUE)
sum(All13$AITechPatents,na.rm = TRUE)
sum(All13$AITechApps,na.rm = TRUE)
sum(All13$AIFunctPatents,na.rm = TRUE)
sum(All13$AIFunctApps,na.rm = TRUE)
sum(All13$AISubPat,na.rm = TRUE)
sum(All13$AISubApp,na.rm = TRUE)
sum(All13$AIForPat,na.rm = TRUE)
sum(All13$AIForApp,na.rm = TRUE)



##Add Patent data to the subsidiary data - 2014
Full4<-fread("files_created_code1/Full4.csv",sep=";", dec=",")
Patents2014<-fread("files_created_code1/Patents2014.csv",sep=";", dec=",")

names(Full4) <- c("GUO","Subsidiaries","Year","Added","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                  "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                  "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")

All14<-left_join(Full4,Patents2014,by="Subsidiaries",na_matches="never")

##Check quality of the match
#Check the NAs per rows
length(which(!is.na(All14$NoPat)))
#Check for the number of matches with BvDID Match #1692263 - NOW: 2169248
All14$Check<-ifelse(All14$GUO!=All14$Subsidiaries,1,NA)
length(which(!is.na(All14$Check)))
All14$Check<-NULL

#Sum number of patents per GUO
All14$NoPat<-as.numeric(All14$NoPat)
All14<-All14[order(All14[,1]),]
All14$Patents1<-ifelse(is.na(All14$NoPat),0,All14$NoPat)
All14$Patents1[is.na(All14$Patents1)] <- 0
All14$Patents<-with(All14,ave(All14$Patents1,All14$GUO, FUN=sum,na.rm=TRUE))
length(which(All14$Patents!=0))

#Sum number of patent applications per GUO
All14$App<-as.numeric(All14$App)
All14$Applications1<-ifelse(is.na(All14$App),0,All14$App)
All14$Applications1[is.na(All14$Applications1)] <- 0
All14$Applications<-with(All14,ave(All14$Applications1,All14$GUO, FUN=sum,na.rm=TRUE))
length(which(All14$Applications!=0))

#Sum number of acquired patents per firm
All14$Greenfield2<-ifelse(All14$Added==2,All14$Patents1,0)
All14$Greenfield1<-All14$Greenfield2+All14$GF
All14$Greenfield1[is.na(All14$Greenfield1)] <- 0
All14$Greenfield<-with(All14,ave(All14$Greenfield1,All14$GUO, FUN=sum,na.rm=TRUE))
length(which(All14$Greenfield!=0))
All14$Brownfield2<-ifelse(All14$Added==3,All14$Patents1,0)
All14$Brownfield1<-All14$Brownfield2+All14$BF
All14$Brownfield1[is.na(All14$Brownfield1)] <- 0
All14$Brownfield<-with(All14,ave(All14$Brownfield1,All14$GUO, FUN=sum,na.rm=TRUE))
length(which(All14$Brownfield!=0))

#Sum of self-developed Patents
All14$SDPatents<-All14$Applications/(All14$Applications+All14$Greenfield+All14$Brownfield)
length(which(All14$SDPatents!=0))

#Sum number of technical AI patents per GUO
All14$GrantPat<-as.numeric(All14$GrantPat)
All14$GrantPat1<-ifelse(is.na(All14$GrantPat),0,All14$GrantPat)
All14$GrantPat1[is.na(All14$GrantPat1)] <- 0
All14$GrantedPatents<-with(All14,ave(All14$GrantPat1,All14$GUO, FUN=sum,na.rm=TRUE))
length(which(All14$GrantedPatents!=0))

#Sum number of technical AI patents per GUO
All14$HighTech<-as.numeric(All14$HighTech)
All14$HighTech1<-ifelse(is.na(All14$HighTech),0,All14$HighTech)
All14$HighTech1[is.na(All14$HighTech1)] <- 0
All14$HighTechPatents<-with(All14,ave(All14$HighTech1,All14$GUO, FUN=sum,na.rm=TRUE))
length(which(All14$HighTechPatents!=0))

#Number of patents located in the subsidiaries
All14$SubPat2<-ifelse(All14$GUO!=All14$Subsidiaries,All14$NoPat,NA)
All14$SubPat1<-ifelse(is.na(All14$SubPat2),0,All14$SubPat2)
All14$SubPat1<-as.numeric(All14$SubPat1)
All14$SubPat1[is.na(All14$SubPat1)] <- 0
All14$SubPat<- with(All14,ave(All14$SubPat1,All14$GUO, FUN=sum))
length(which(All14$SubPat!=0))

#Number of patents located in the subsidiaries
All14$SubApp2<-ifelse(All14$GUO!=All14$Subsidiaries,All14$App,NA)
All14$SubApp1<-ifelse(is.na(All14$SubApp2),0,All14$SubApp2)
All14$SubApp1<-as.numeric(All14$SubApp1)
All14$SubApp1[is.na(All14$SubApp1)] <- 0
All14$SubApp<- with(All14,ave(All14$SubApp1,All14$GUO, FUN=sum))
length(which(All14$SubApp!=0))

#Number of patents located in foreign countries
All14$Country<-substr(All14$Subsidiaries, 1, 2)
All14$Country2<-substr(All14$GUO, 1, 2)
All14$ForPat2<-ifelse(All14$Country!=All14$Country2,All14$NoPat,NA)
All14$ForPat1<-ifelse(is.na(All14$ForPat2),0,All14$ForPat2)
All14$ForPat1<-as.numeric(All14$ForPat1)
All14$ForPat1[is.na(All14$ForPat1)] <- 0
All14$ForPat<- with(All14,ave(All14$ForPat1,All14$GUO, FUN=sum))

#Number of patents located in foreign countries
All14$ForApp2<-ifelse(All14$Country!=All14$Country2,All14$App,NA)
All14$ForApp1<-ifelse(is.na(All14$ForApp2),0,All14$ForApp2)
All14$ForApp1<-as.numeric(All14$ForApp1)
All14$ForApp1[is.na(All14$ForApp1)] <- 0
All14$ForApp<- with(All14,ave(All14$ForApp1,All14$GUO, FUN=sum))


##AI Patents
#Sum number of AI patents per GUO
All14$AINoPat<-as.numeric(All14$AINoPat)
All14$AIPatents1<-ifelse(is.na(All14$AINoPat),0,All14$AINoPat)
All14$AIPatents1[is.na(All14$AIPatents1)] <- 0
All14$AIPatents<-with(All14,ave(All14$AIPatents1,All14$GUO, FUN=sum,na.rm=TRUE))
length(which(All14$AIPatents!=0))

#Sum number of patent applications per GUO
All14$AIApp<-as.numeric(All14$AIApp)
All14$AIApplications1<-ifelse(is.na(All14$AIApp),0,All14$AIApp)
All14$AIApplications1[is.na(All14$AIApplications1)] <- 0
All14$AIApplications<-with(All14,ave(All14$AIApplications1,All14$GUO, FUN=sum,na.rm=TRUE))
length(which(All14$AIApplications!=0))

#Sum number of granted AI patents per GUO
All14$AIGrantPat<-as.numeric(All14$AIGrantPat)
All14$AIGrantPat1<-ifelse(is.na(All14$AIGrantPat),0,All14$AIGrantPat)
All14$AIGrantPat1[is.na(All14$AIGrantPat1)] <- 0
All14$AIGrantedPatents<-with(All14,ave(All14$AIGrantPat1,All14$GUO, FUN=sum,na.rm=TRUE))
length(which(All14$AIGrantedPatents!=0))

#Sum number of acquired patents per firm
All14$AIGreenfield2<-ifelse(All14$Added==2,All14$AIPatents1,0)
All14$AIGreenfield1<-All14$AIGreenfield2+All14$AIGF
All14$AIGreenfield1[is.na(All14$AIGreenfield1)] <- 0
All14$AIGreenfield<-with(All14,ave(All14$AIGreenfield1,All14$GUO, FUN=sum,na.rm=TRUE))
length(which(All14$AIGreenfield!=0))
All14$AIBrownfield2<-ifelse(All14$Added==3,All14$AIPatents1,0)
All14$AIBrownfield1<-All14$AIBrownfield2+All14$AIBF
All14$AIBrownfield1[is.na(All14$AIBrownfield1)] <- 0
All14$AIBrownfield<-with(All14,ave(All14$AIBrownfield1,All14$GUO, FUN=sum,na.rm=TRUE))
length(which(All14$AIBrownfield!=0))

#Sum of self-developed Patents
All14$AISDPatents<-All14$AIApplications/(All14$AIApplications+All14$AIGreenfield+All14$AIBrownfield)
length(which(All14$SDPatents!=0))

#Sum number of technical AI patents per GUO
All14$TechPat<-as.numeric(All14$TechPat)
All14$TechPat1<-ifelse(is.na(All14$TechPat),0,All14$TechPat)
All14$TechPat1[is.na(All14$TechPat1)] <- 0
All14$AITechPatents<-with(All14,ave(All14$TechPat1,All14$GUO, FUN=sum,na.rm=TRUE))
length(which(All14$AITechPatents!=0))

#Sum number of technical AI patents per GUO
All14$AppTech<-as.numeric(All14$AppTech)
All14$TechApplications1<-ifelse(is.na(All14$AppTech),0,All14$AppTech)
All14$TechApplications1[is.na(All14$TechApplications1)] <- 0
All14$AITechApplications<-with(All14,ave(All14$TechApplications1,All14$GUO, FUN=sum,na.rm=TRUE))
length(which(All14$TechApplications!=0))

#Sum number of Functional AI patents per GUO
All14$FunctPat<-as.numeric(All14$FunctPat)
All14$FunctPat1<-ifelse(is.na(All14$FunctPat),0,All14$FunctPat)
All14$FunctPat1[is.na(All14$FunctPat1)] <- 0
All14$AIFunctPatents<-with(All14,ave(All14$FunctPat1,All14$GUO, FUN=sum,na.rm=TRUE))
length(which(All14$AIFunctPatents!=0))

#Sum number of Functional AI patents per GUO
All14$AppFunct<-as.numeric(All14$AppFunct)
All14$FunctApplications1<-ifelse(is.na(All14$AppFunct),0,All14$AppFunct)
All14$FunctApplications1[is.na(All14$FunctApplications1)] <- 0
All14$AIFunctApplications<-with(All14,ave(All14$FunctApplications1,All14$GUO, FUN=sum,na.rm=TRUE))
length(which(All14$FunctApplications!=0))

#Number of patents located in the subsidiaries
All14$AISubPat2<-ifelse(All14$GUO!=All14$Subsidiaries,All14$AINoPat,NA)
All14$AISubPat1<-ifelse(is.na(All14$AISubPat2),0,All14$AISubPat2)
All14$AISubPat1<-as.numeric(All14$AISubPat1)
All14$AISubPat1[is.na(All14$AISubPat1)] <- 0
All14$AISubPat<- with(All14,ave(All14$AISubPat1,All14$GUO, FUN=sum))
length(which(All14$AISubPat!=0))

#Number of patents located in the AISubsidiaries
All14$AISubApp2<-ifelse(All14$GUO!=All14$Subsidiaries,All14$AIApp,NA)
All14$AISubApp1<-ifelse(is.na(All14$AISubApp2),0,All14$AISubApp2)
All14$AISubApp1<-as.numeric(All14$AISubApp1)
All14$AISubApp1[is.na(All14$AISubApp1)] <- 0
All14$AISubApp<- with(All14,ave(All14$AISubApp1,All14$GUO, FUN=sum))
length(which(All14$AISubApp!=0))

#Number of patents located in foreign countries
All14$AIForPat2<-ifelse(All14$Country!=All14$Country2,All14$AINoPat,NA)
All14$AIForPat1<-ifelse(is.na(All14$AIForPat2),0,All14$AIForPat2)
All14$AIForPat1<-as.numeric(All14$AIForPat1)
All14$AIForPat1[is.na(All14$AIForPat1)] <- 0
All14$AIForPat<- with(All14,ave(All14$AIForPat1,All14$GUO, FUN=sum))

#Number of patents located in AIForeign countries
All14$AIForApp2<-ifelse(All14$Country!=All14$Country2,All14$AIApp,NA)
All14$AIForApp1<-ifelse(is.na(All14$AIForApp2),0,All14$AIForApp2)
All14$AIForApp1<-as.numeric(All14$AIForApp1)
All14$AIForApp1[is.na(All14$AIForApp1)] <- 0
All14$AIForApp<- with(All14,ave(All14$AIForApp1,All14$GUO, FUN=sum))

#Delete unnecessary variables
All14[,34:52]<-NULL
All14<-All14[,c(-2,-4,-35,-37,-38,-40,-41,-44,-46,-48,-49,-51,-52,-54,-55,-56,-57,-59,-60,-62,-64,-66,-68,-69,-71,-72,-75,-77,-79,-81,-83,-84,-86,-87,-89,-90,-92,-93)]

#Delete duplicate GUOs
All14<-All14[!duplicated(All14$GUO),]

#Rename Dataset
names(All14)<-c("BvDID","Year","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub","Patents","Applications",
                "GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents","SubPat","SubApp","ForPat","ForApp",
                "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
                "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp")

#Patent data descriptives
sum(All14$Patents,na.rm = TRUE)
sum(All14$Applications,na.rm = TRUE)
sum(All14$GreenfieldPatents,na.rm = TRUE)
sum(All14$BrownfieldPatents,na.rm = TRUE)
sum(All14$SDPatents,na.rm = TRUE)
sum(All14$GrantedPatents,na.rm = TRUE)
sum(All14$HighTechPatents,na.rm = TRUE)
sum(All14$SubPat,na.rm = TRUE)
sum(All14$SubApp,na.rm = TRUE)
sum(All14$ForPat,na.rm = TRUE)
sum(All14$ForApp,na.rm = TRUE)
sum(All14$AIPatents,na.rm = TRUE)
sum(All14$AIApplications,na.rm = TRUE)
sum(All14$AIGrantedPatents,na.rm = TRUE)
sum(All14$AIGreenfieldPatents,na.rm = TRUE)
sum(All14$AIBrownfieldPatents,na.rm = TRUE)
sum(All14$AISDPatents,na.rm = TRUE)
sum(All14$AITechPatents,na.rm = TRUE)
sum(All14$AITechApps,na.rm = TRUE)
sum(All14$AIFunctPatents,na.rm = TRUE)
sum(All14$AIFunctApps,na.rm = TRUE)
sum(All14$AISubPat,na.rm = TRUE)
sum(All14$AISubApp,na.rm = TRUE)
sum(All14$AIForPat,na.rm = TRUE)
sum(All14$AIForApp,na.rm = TRUE)


##Add Patent data to the subsidiary data - 2015
Full5<-fread("files_created_code1/Full5.csv",sep=";", dec=",")
Patents2015<-fread("files_created_code1/Patents2015.csv",sep=";", dec=",")

names(Full5) <- c("GUO","Subsidiaries","Year","Added","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                  "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                  "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")

All15<-left_join(Full5,Patents2015,by="Subsidiaries",na_matches="never")

##Check quality of the match
#Check the NAs per rows
length(which(!is.na(All15$NoPat)))
#Check for the number of matches with BvDID Match #1692263 - NOW: 2169248
All15$Check<-ifelse(All15$GUO!=All15$Subsidiaries,1,NA)
length(which(!is.na(All15$Check)))
All15$Check<-NULL

#Sum number of patents per GUO
All15$NoPat<-as.numeric(All15$NoPat)
All15<-All15[order(All15[,1]),]
All15$Patents1<-ifelse(is.na(All15$NoPat),0,All15$NoPat)
All15$Patents1[is.na(All15$Patents1)] <- 0
All15$Patents<-with(All15,ave(All15$Patents1,All15$GUO, FUN=sum,na.rm=TRUE))
length(which(All15$Patents!=0))

#Sum number of patent applications per GUO
All15$App<-as.numeric(All15$App)
All15$Applications1<-ifelse(is.na(All15$App),0,All15$App)
All15$Applications1[is.na(All15$Applications1)] <- 0
All15$Applications<-with(All15,ave(All15$Applications1,All15$GUO, FUN=sum,na.rm=TRUE))
length(which(All15$Applications!=0))

#Sum number of acquired patents per firm
All15$Greenfield2<-ifelse(All15$Added==2,All15$Patents1,0)
All15$Greenfield1<-All15$Greenfield2+All15$GF
All15$Greenfield1[is.na(All15$Greenfield1)] <- 0
All15$Greenfield<-with(All15,ave(All15$Greenfield1,All15$GUO, FUN=sum,na.rm=TRUE))
length(which(All15$Greenfield!=0))
All15$Brownfield2<-ifelse(All15$Added==3,All15$Patents1,0)
All15$Brownfield1<-All15$Brownfield2+All15$BF
All15$Brownfield1[is.na(All15$Brownfield1)] <- 0
All15$Brownfield<-with(All15,ave(All15$Brownfield1,All15$GUO, FUN=sum,na.rm=TRUE))
length(which(All15$Brownfield!=0))

#Sum of self-developed Patents
All15$SDPatents<-All15$Applications/(All15$Applications+All15$Greenfield+All15$Brownfield)
length(which(All15$SDPatents!=0))

#Sum number of technical AI patents per GUO
All15$GrantPat<-as.numeric(All15$GrantPat)
All15$GrantPat1<-ifelse(is.na(All15$GrantPat),0,All15$GrantPat)
All15$GrantPat1[is.na(All15$GrantPat1)] <- 0
All15$GrantedPatents<-with(All15,ave(All15$GrantPat1,All15$GUO, FUN=sum,na.rm=TRUE))
length(which(All15$GrantedPatents!=0))

#Sum number of technical AI patents per GUO
All15$HighTech<-as.numeric(All15$HighTech)
All15$HighTech1<-ifelse(is.na(All15$HighTech),0,All15$HighTech)
All15$HighTech1[is.na(All15$HighTech1)] <- 0
All15$HighTechPatents<-with(All15,ave(All15$HighTech1,All15$GUO, FUN=sum,na.rm=TRUE))
length(which(All15$HighTechPatents!=0))

#Number of patents located in the subsidiaries
All15$SubPat2<-ifelse(All15$GUO!=All15$Subsidiaries,All15$NoPat,NA)
All15$SubPat1<-ifelse(is.na(All15$SubPat2),0,All15$SubPat2)
All15$SubPat1<-as.numeric(All15$SubPat1)
All15$SubPat1[is.na(All15$SubPat1)] <- 0
All15$SubPat<- with(All15,ave(All15$SubPat1,All15$GUO, FUN=sum))
length(which(All15$SubPat!=0))

#Number of patents located in the subsidiaries
All15$SubApp2<-ifelse(All15$GUO!=All15$Subsidiaries,All15$App,NA)
All15$SubApp1<-ifelse(is.na(All15$SubApp2),0,All15$SubApp2)
All15$SubApp1<-as.numeric(All15$SubApp1)
All15$SubApp1[is.na(All15$SubApp1)] <- 0
All15$SubApp<- with(All15,ave(All15$SubApp1,All15$GUO, FUN=sum))
length(which(All15$SubApp!=0))

#Number of patents located in foreign countries
All15$Country<-substr(All15$Subsidiaries, 1, 2)
All15$Country2<-substr(All15$GUO, 1, 2)
All15$ForPat2<-ifelse(All15$Country!=All15$Country2,All15$NoPat,NA)
All15$ForPat1<-ifelse(is.na(All15$ForPat2),0,All15$ForPat2)
All15$ForPat1<-as.numeric(All15$ForPat1)
All15$ForPat1[is.na(All15$ForPat1)] <- 0
All15$ForPat<- with(All15,ave(All15$ForPat1,All15$GUO, FUN=sum))

#Number of patents located in foreign countries
All15$ForApp2<-ifelse(All15$Country!=All15$Country2,All15$App,NA)
All15$ForApp1<-ifelse(is.na(All15$ForApp2),0,All15$ForApp2)
All15$ForApp1<-as.numeric(All15$ForApp1)
All15$ForApp1[is.na(All15$ForApp1)] <- 0
All15$ForApp<- with(All15,ave(All15$ForApp1,All15$GUO, FUN=sum))


##AI Patents
#Sum number of AI patents per GUO
All15$AINoPat<-as.numeric(All15$AINoPat)
All15$AIPatents1<-ifelse(is.na(All15$AINoPat),0,All15$AINoPat)
All15$AIPatents1[is.na(All15$AIPatents1)] <- 0
All15$AIPatents<-with(All15,ave(All15$AIPatents1,All15$GUO, FUN=sum,na.rm=TRUE))
length(which(All15$AIPatents!=0))

#Sum number of patent applications per GUO
All15$AIApp<-as.numeric(All15$AIApp)
All15$AIApplications1<-ifelse(is.na(All15$AIApp),0,All15$AIApp)
All15$AIApplications1[is.na(All15$AIApplications1)] <- 0
All15$AIApplications<-with(All15,ave(All15$AIApplications1,All15$GUO, FUN=sum,na.rm=TRUE))
length(which(All15$AIApplications!=0))

#Sum number of granted AI patents per GUO
All15$AIGrantPat<-as.numeric(All15$AIGrantPat)
All15$AIGrantPat1<-ifelse(is.na(All15$AIGrantPat),0,All15$AIGrantPat)
All15$AIGrantPat1[is.na(All15$AIGrantPat1)] <- 0
All15$AIGrantedPatents<-with(All15,ave(All15$AIGrantPat1,All15$GUO, FUN=sum,na.rm=TRUE))
length(which(All15$AIGrantedPatents!=0))

#Sum number of acquired patents per firm
All15$AIGreenfield2<-ifelse(All15$Added==2,All15$AIPatents1,0)
All15$AIGreenfield1<-All15$AIGreenfield2+All15$AIGF
All15$AIGreenfield1[is.na(All15$AIGreenfield1)] <- 0
All15$AIGreenfield<-with(All15,ave(All15$AIGreenfield1,All15$GUO, FUN=sum,na.rm=TRUE))
length(which(All15$AIGreenfield!=0))
All15$AIBrownfield2<-ifelse(All15$Added==3,All15$AIPatents1,0)
All15$AIBrownfield1<-All15$AIBrownfield2+All15$AIBF
All15$AIBrownfield1[is.na(All15$AIBrownfield1)] <- 0
All15$AIBrownfield<-with(All15,ave(All15$AIBrownfield1,All15$GUO, FUN=sum,na.rm=TRUE))
length(which(All15$AIBrownfield!=0))

#Sum of self-developed Patents
All15$AISDPatents<-All15$AIApplications/(All15$AIApplications+All15$AIGreenfield+All15$AIBrownfield)
length(which(All15$SDPatents!=0))

#Sum number of technical AI patents per GUO
All15$TechPat<-as.numeric(All15$TechPat)
All15$TechPat1<-ifelse(is.na(All15$TechPat),0,All15$TechPat)
All15$TechPat1[is.na(All15$TechPat1)] <- 0
All15$AITechPatents<-with(All15,ave(All15$TechPat1,All15$GUO, FUN=sum,na.rm=TRUE))
length(which(All15$AITechPatents!=0))

#Sum number of technical AI patents per GUO
All15$AppTech<-as.numeric(All15$AppTech)
All15$TechApplications1<-ifelse(is.na(All15$AppTech),0,All15$AppTech)
All15$TechApplications1[is.na(All15$TechApplications1)] <- 0
All15$AITechApplications<-with(All15,ave(All15$TechApplications1,All15$GUO, FUN=sum,na.rm=TRUE))
length(which(All15$TechApplications!=0))

#Sum number of Functional AI patents per GUO
All15$FunctPat<-as.numeric(All15$FunctPat)
All15$FunctPat1<-ifelse(is.na(All15$FunctPat),0,All15$FunctPat)
All15$FunctPat1[is.na(All15$FunctPat1)] <- 0
All15$AIFunctPatents<-with(All15,ave(All15$FunctPat1,All15$GUO, FUN=sum,na.rm=TRUE))
length(which(All15$AIFunctPatents!=0))

#Sum number of Functional AI patents per GUO
All15$AppFunct<-as.numeric(All15$AppFunct)
All15$FunctApplications1<-ifelse(is.na(All15$AppFunct),0,All15$AppFunct)
All15$FunctApplications1[is.na(All15$FunctApplications1)] <- 0
All15$AIFunctApplications<-with(All15,ave(All15$FunctApplications1,All15$GUO, FUN=sum,na.rm=TRUE))
length(which(All15$FunctApplications!=0))

#Number of patents located in the subsidiaries
All15$AISubPat2<-ifelse(All15$GUO!=All15$Subsidiaries,All15$AINoPat,NA)
All15$AISubPat1<-ifelse(is.na(All15$AISubPat2),0,All15$AISubPat2)
All15$AISubPat1<-as.numeric(All15$AISubPat1)
All15$AISubPat1[is.na(All15$AISubPat1)] <- 0
All15$AISubPat<- with(All15,ave(All15$AISubPat1,All15$GUO, FUN=sum))
length(which(All15$AISubPat!=0))

#Number of patents located in the AISubsidiaries
All15$AISubApp2<-ifelse(All15$GUO!=All15$Subsidiaries,All15$AIApp,NA)
All15$AISubApp1<-ifelse(is.na(All15$AISubApp2),0,All15$AISubApp2)
All15$AISubApp1<-as.numeric(All15$AISubApp1)
All15$AISubApp1[is.na(All15$AISubApp1)] <- 0
All15$AISubApp<- with(All15,ave(All15$AISubApp1,All15$GUO, FUN=sum))
length(which(All15$AISubApp!=0))

#Number of patents located in foreign countries
All15$AIForPat2<-ifelse(All15$Country!=All15$Country2,All15$AINoPat,NA)
All15$AIForPat1<-ifelse(is.na(All15$AIForPat2),0,All15$AIForPat2)
All15$AIForPat1<-as.numeric(All15$AIForPat1)
All15$AIForPat1[is.na(All15$AIForPat1)] <- 0
All15$AIForPat<- with(All15,ave(All15$AIForPat1,All15$GUO, FUN=sum))

#Number of patents located in AIForeign countries
All15$AIForApp2<-ifelse(All15$Country!=All15$Country2,All15$AIApp,NA)
All15$AIForApp1<-ifelse(is.na(All15$AIForApp2),0,All15$AIForApp2)
All15$AIForApp1<-as.numeric(All15$AIForApp1)
All15$AIForApp1[is.na(All15$AIForApp1)] <- 0
All15$AIForApp<- with(All15,ave(All15$AIForApp1,All15$GUO, FUN=sum))

#Delete unnecessary variables
All15[,34:52]<-NULL
All15<-All15[,c(-2,-4,-35,-37,-38,-40,-41,-44,-46,-48,-49,-51,-52,-54,-55,-56,-57,-59,-60,-62,-64,-66,-68,-69,-71,-72,-75,-77,-79,-81,-83,-84,-86,-87,-89,-90,-92,-93)]

#Delete duplicate GUOs
All15<-All15[!duplicated(All15$GUO),]

#Rename Dataset
names(All15)<-c("BvDID","Year","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub","Patents","Applications",
                "GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents","SubPat","SubApp","ForPat","ForApp",
                "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
                "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp")

#Patent data descriptives
sum(All15$Patents,na.rm = TRUE)
sum(All15$Applications,na.rm = TRUE)
sum(All15$GreenfieldPatents,na.rm = TRUE)
sum(All15$BrownfieldPatents,na.rm = TRUE)
sum(All15$SDPatents,na.rm = TRUE)
sum(All15$GrantedPatents,na.rm = TRUE)
sum(All15$HighTechPatents,na.rm = TRUE)
sum(All15$SubPat,na.rm = TRUE)
sum(All15$SubApp,na.rm = TRUE)
sum(All15$ForPat,na.rm = TRUE)
sum(All15$ForApp,na.rm = TRUE)
sum(All15$AIPatents,na.rm = TRUE)
sum(All15$AIApplications,na.rm = TRUE)
sum(All15$AIGrantedPatents,na.rm = TRUE)
sum(All15$AIGreenfieldPatents,na.rm = TRUE)
sum(All15$AIBrownfieldPatents,na.rm = TRUE)
sum(All15$AISDPatents,na.rm = TRUE)
sum(All15$AITechPatents,na.rm = TRUE)
sum(All15$AITechApps,na.rm = TRUE)
sum(All15$AIFunctPatents,na.rm = TRUE)
sum(All15$AIFunctApps,na.rm = TRUE)
sum(All15$AISubPat,na.rm = TRUE)
sum(All15$AISubApp,na.rm = TRUE)
sum(All15$AIForPat,na.rm = TRUE)
sum(All15$AIForApp,na.rm = TRUE)



##Add Patent data to the subsidiary data - 2016
Full6<-fread("files_created_code1/Full6.csv",sep=";", dec=",")
Patents2016<-fread("files_created_code1/Patents2016.csv",sep=";", dec=",")

names(Full6) <- c("GUO","Subsidiaries","Year","Added","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                  "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                  "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")

All16<-left_join(Full6,Patents2016,by="Subsidiaries",na_matches="never")

##Check quality of the match
#Check the NAs per rows
length(which(!is.na(All16$NoPat)))
#Check for the number of matches with BvDID Match #1692263 - NOW: 2169248
All16$Check<-ifelse(All16$GUO!=All16$Subsidiaries,1,NA)
length(which(!is.na(All16$Check)))
All16$Check<-NULL

#Sum number of patents per GUO
All16$NoPat<-as.numeric(All16$NoPat)
All16<-All16[order(All16[,1]),]
All16$Patents1<-ifelse(is.na(All16$NoPat),0,All16$NoPat)
All16$Patents1[is.na(All16$Patents1)] <- 0
All16$Patents<-with(All16,ave(All16$Patents1,All16$GUO, FUN=sum,na.rm=TRUE))
length(which(All16$Patents!=0))

#Sum number of patent applications per GUO
All16$App<-as.numeric(All16$App)
All16$Applications1<-ifelse(is.na(All16$App),0,All16$App)
All16$Applications1[is.na(All16$Applications1)] <- 0
All16$Applications<-with(All16,ave(All16$Applications1,All16$GUO, FUN=sum,na.rm=TRUE))
length(which(All16$Applications!=0))

#Sum number of acquired patents per firm
All16$Greenfield2<-ifelse(All16$Added==2,All16$Patents1,0)
All16$Greenfield1<-All16$Greenfield2+All16$GF
All16$Greenfield1[is.na(All16$Greenfield1)] <- 0
All16$Greenfield<-with(All16,ave(All16$Greenfield1,All16$GUO, FUN=sum,na.rm=TRUE))
length(which(All16$Greenfield!=0))
All16$Brownfield2<-ifelse(All16$Added==3,All16$Patents1,0)
All16$Brownfield1<-All16$Brownfield2+All16$BF
All16$Brownfield1[is.na(All16$Brownfield1)] <- 0
All16$Brownfield<-with(All16,ave(All16$Brownfield1,All16$GUO, FUN=sum,na.rm=TRUE))
length(which(All16$Brownfield!=0))

#Sum of self-developed Patents
All16$SDPatents<-All16$Applications/(All16$Applications+All16$Greenfield+All16$Brownfield)
length(which(All16$SDPatents!=0))

#Sum number of technical AI patents per GUO
All16$GrantPat<-as.numeric(All16$GrantPat)
All16$GrantPat1<-ifelse(is.na(All16$GrantPat),0,All16$GrantPat)
All16$GrantPat1[is.na(All16$GrantPat1)] <- 0
All16$GrantedPatents<-with(All16,ave(All16$GrantPat1,All16$GUO, FUN=sum,na.rm=TRUE))
length(which(All16$GrantedPatents!=0))

#Sum number of technical AI patents per GUO
All16$HighTech<-as.numeric(All16$HighTech)
All16$HighTech1<-ifelse(is.na(All16$HighTech),0,All16$HighTech)
All16$HighTech1[is.na(All16$HighTech1)] <- 0
All16$HighTechPatents<-with(All16,ave(All16$HighTech1,All16$GUO, FUN=sum,na.rm=TRUE))
length(which(All16$HighTechPatents!=0))

#Number of patents located in the subsidiaries
All16$SubPat2<-ifelse(All16$GUO!=All16$Subsidiaries,All16$NoPat,NA)
All16$SubPat1<-ifelse(is.na(All16$SubPat2),0,All16$SubPat2)
All16$SubPat1<-as.numeric(All16$SubPat1)
All16$SubPat1[is.na(All16$SubPat1)] <- 0
All16$SubPat<- with(All16,ave(All16$SubPat1,All16$GUO, FUN=sum))
length(which(All16$SubPat!=0))

#Number of patents located in the subsidiaries
All16$SubApp2<-ifelse(All16$GUO!=All16$Subsidiaries,All16$App,NA)
All16$SubApp1<-ifelse(is.na(All16$SubApp2),0,All16$SubApp2)
All16$SubApp1<-as.numeric(All16$SubApp1)
All16$SubApp1[is.na(All16$SubApp1)] <- 0
All16$SubApp<- with(All16,ave(All16$SubApp1,All16$GUO, FUN=sum))
length(which(All16$SubApp!=0))

#Number of patents located in foreign countries
All16$Country<-substr(All16$Subsidiaries, 1, 2)
All16$Country2<-substr(All16$GUO, 1, 2)
All16$ForPat2<-ifelse(All16$Country!=All16$Country2,All16$NoPat,NA)
All16$ForPat1<-ifelse(is.na(All16$ForPat2),0,All16$ForPat2)
All16$ForPat1<-as.numeric(All16$ForPat1)
All16$ForPat1[is.na(All16$ForPat1)] <- 0
All16$ForPat<- with(All16,ave(All16$ForPat1,All16$GUO, FUN=sum))

#Number of patents located in foreign countries
All16$ForApp2<-ifelse(All16$Country!=All16$Country2,All16$App,NA)
All16$ForApp1<-ifelse(is.na(All16$ForApp2),0,All16$ForApp2)
All16$ForApp1<-as.numeric(All16$ForApp1)
All16$ForApp1[is.na(All16$ForApp1)] <- 0
All16$ForApp<- with(All16,ave(All16$ForApp1,All16$GUO, FUN=sum))


##AI Patents
#Sum number of AI patents per GUO
All16$AINoPat<-as.numeric(All16$AINoPat)
All16$AIPatents1<-ifelse(is.na(All16$AINoPat),0,All16$AINoPat)
All16$AIPatents1[is.na(All16$AIPatents1)] <- 0
All16$AIPatents<-with(All16,ave(All16$AIPatents1,All16$GUO, FUN=sum,na.rm=TRUE))
length(which(All16$AIPatents!=0))

#Sum number of patent applications per GUO
All16$AIApp<-as.numeric(All16$AIApp)
All16$AIApplications1<-ifelse(is.na(All16$AIApp),0,All16$AIApp)
All16$AIApplications1[is.na(All16$AIApplications1)] <- 0
All16$AIApplications<-with(All16,ave(All16$AIApplications1,All16$GUO, FUN=sum,na.rm=TRUE))
length(which(All16$AIApplications!=0))

#Sum number of granted AI patents per GUO
All16$AIGrantPat<-as.numeric(All16$AIGrantPat)
All16$AIGrantPat1<-ifelse(is.na(All16$AIGrantPat),0,All16$AIGrantPat)
All16$AIGrantPat1[is.na(All16$AIGrantPat1)] <- 0
All16$AIGrantedPatents<-with(All16,ave(All16$AIGrantPat1,All16$GUO, FUN=sum,na.rm=TRUE))
length(which(All16$AIGrantedPatents!=0))

#Sum number of acquired patents per firm
All16$AIGreenfield2<-ifelse(All16$Added==2,All16$AIPatents1,0)
All16$AIGreenfield1<-All16$AIGreenfield2+All16$AIGF
All16$AIGreenfield1[is.na(All16$AIGreenfield1)] <- 0
All16$AIGreenfield<-with(All16,ave(All16$AIGreenfield1,All16$GUO, FUN=sum,na.rm=TRUE))
length(which(All16$AIGreenfield!=0))
All16$AIBrownfield2<-ifelse(All16$Added==3,All16$AIPatents1,0)
All16$AIBrownfield1<-All16$AIBrownfield2+All16$AIBF
All16$AIBrownfield1[is.na(All16$AIBrownfield1)] <- 0
All16$AIBrownfield<-with(All16,ave(All16$AIBrownfield1,All16$GUO, FUN=sum,na.rm=TRUE))
length(which(All16$AIBrownfield!=0))

#Sum of self-developed Patents
All16$AISDPatents<-All16$AIApplications/(All16$AIApplications+All16$AIGreenfield+All16$AIBrownfield)
length(which(All16$SDPatents!=0))

#Sum number of technical AI patents per GUO
All16$TechPat<-as.numeric(All16$TechPat)
All16$TechPat1<-ifelse(is.na(All16$TechPat),0,All16$TechPat)
All16$TechPat1[is.na(All16$TechPat1)] <- 0
All16$AITechPatents<-with(All16,ave(All16$TechPat1,All16$GUO, FUN=sum,na.rm=TRUE))
length(which(All16$AITechPatents!=0))

#Sum number of technical AI patents per GUO
All16$AppTech<-as.numeric(All16$AppTech)
All16$TechApplications1<-ifelse(is.na(All16$AppTech),0,All16$AppTech)
All16$TechApplications1[is.na(All16$TechApplications1)] <- 0
All16$AITechApplications<-with(All16,ave(All16$TechApplications1,All16$GUO, FUN=sum,na.rm=TRUE))
length(which(All16$TechApplications!=0))

#Sum number of Functional AI patents per GUO
All16$FunctPat<-as.numeric(All16$FunctPat)
All16$FunctPat1<-ifelse(is.na(All16$FunctPat),0,All16$FunctPat)
All16$FunctPat1[is.na(All16$FunctPat1)] <- 0
All16$AIFunctPatents<-with(All16,ave(All16$FunctPat1,All16$GUO, FUN=sum,na.rm=TRUE))
length(which(All16$AIFunctPatents!=0))

#Sum number of Functional AI patents per GUO
All16$AppFunct<-as.numeric(All16$AppFunct)
All16$FunctApplications1<-ifelse(is.na(All16$AppFunct),0,All16$AppFunct)
All16$FunctApplications1[is.na(All16$FunctApplications1)] <- 0
All16$AIFunctApplications<-with(All16,ave(All16$FunctApplications1,All16$GUO, FUN=sum,na.rm=TRUE))
length(which(All16$FunctApplications!=0))

#Number of patents located in the subsidiaries
All16$AISubPat2<-ifelse(All16$GUO!=All16$Subsidiaries,All16$AINoPat,NA)
All16$AISubPat1<-ifelse(is.na(All16$AISubPat2),0,All16$AISubPat2)
All16$AISubPat1<-as.numeric(All16$AISubPat1)
All16$AISubPat1[is.na(All16$AISubPat1)] <- 0
All16$AISubPat<- with(All16,ave(All16$AISubPat1,All16$GUO, FUN=sum))
length(which(All16$AISubPat!=0))

#Number of patents located in the AISubsidiaries
All16$AISubApp2<-ifelse(All16$GUO!=All16$Subsidiaries,All16$AIApp,NA)
All16$AISubApp1<-ifelse(is.na(All16$AISubApp2),0,All16$AISubApp2)
All16$AISubApp1<-as.numeric(All16$AISubApp1)
All16$AISubApp1[is.na(All16$AISubApp1)] <- 0
All16$AISubApp<- with(All16,ave(All16$AISubApp1,All16$GUO, FUN=sum))
length(which(All16$AISubApp!=0))

#Number of patents located in foreign countries
All16$AIForPat2<-ifelse(All16$Country!=All16$Country2,All16$AINoPat,NA)
All16$AIForPat1<-ifelse(is.na(All16$AIForPat2),0,All16$AIForPat2)
All16$AIForPat1<-as.numeric(All16$AIForPat1)
All16$AIForPat1[is.na(All16$AIForPat1)] <- 0
All16$AIForPat<- with(All16,ave(All16$AIForPat1,All16$GUO, FUN=sum))

#Number of patents located in AIForeign countries
All16$AIForApp2<-ifelse(All16$Country!=All16$Country2,All16$AIApp,NA)
All16$AIForApp1<-ifelse(is.na(All16$AIForApp2),0,All16$AIForApp2)
All16$AIForApp1<-as.numeric(All16$AIForApp1)
All16$AIForApp1[is.na(All16$AIForApp1)] <- 0
All16$AIForApp<- with(All16,ave(All16$AIForApp1,All16$GUO, FUN=sum))

#Delete unnecessary variables
All16[,34:52]<-NULL
All16<-All16[,c(-2,-4,-35,-37,-38,-40,-41,-44,-46,-48,-49,-51,-52,-54,-55,-56,-57,-59,-60,-62,-64,-66,-68,-69,-71,-72,-75,-77,-79,-81,-83,-84,-86,-87,-89,-90,-92,-93)]

#Delete duplicate GUOs
All16<-All16[!duplicated(All16$GUO),]

#Rename Dataset
names(All16)<-c("BvDID","Year","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub","Patents","Applications",
                "GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents","SubPat","SubApp","ForPat","ForApp",
                "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
                "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp")

#Patent data descriptives
sum(All16$Patents,na.rm = TRUE)
sum(All16$Applications,na.rm = TRUE)
sum(All16$GreenfieldPatents,na.rm = TRUE)
sum(All16$BrownfieldPatents,na.rm = TRUE)
sum(All16$SDPatents,na.rm = TRUE)
sum(All16$GrantedPatents,na.rm = TRUE)
sum(All16$HighTechPatents,na.rm = TRUE)
sum(All16$SubPat,na.rm = TRUE)
sum(All16$SubApp,na.rm = TRUE)
sum(All16$ForPat,na.rm = TRUE)
sum(All16$ForApp,na.rm = TRUE)
sum(All16$AIPatents,na.rm = TRUE)
sum(All16$AIApplications,na.rm = TRUE)
sum(All16$AIGrantedPatents,na.rm = TRUE)
sum(All16$AIGreenfieldPatents,na.rm = TRUE)
sum(All16$AIBrownfieldPatents,na.rm = TRUE)
sum(All16$AISDPatents,na.rm = TRUE)
sum(All16$AITechPatents,na.rm = TRUE)
sum(All16$AITechApps,na.rm = TRUE)
sum(All16$AIFunctPatents,na.rm = TRUE)
sum(All16$AIFunctApps,na.rm = TRUE)
sum(All16$AISubPat,na.rm = TRUE)
sum(All16$AISubApp,na.rm = TRUE)
sum(All16$AIForPat,na.rm = TRUE)
sum(All16$AIForApp,na.rm = TRUE)


##Add Patent data to the subsidiary data - 2017
Full7<-fread("files_created_code1/Full7.csv",sep=";", dec=",")
Patents2017<-fread("files_created_code1/Patents2017.csv",sep=";", dec=",")

names(Full7) <- c("GUO","Subsidiaries","Year","Added","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                  "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                  "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")

All17<-left_join(Full7,Patents2017,by="Subsidiaries",na_matches="never")

##Check quality of the match
#Check the NAs per rows
length(which(!is.na(All17$NoPat)))
#Check for the number of matches with BvDID Match #1692263 - NOW: 2169248
All17$Check<-ifelse(All17$GUO!=All17$Subsidiaries,1,NA)
length(which(!is.na(All17$Check)))
All17$Check<-NULL

#Sum number of patents per GUO
All17$NoPat<-as.numeric(All17$NoPat)
All17<-All17[order(All17[,1]),]
All17$Patents1<-ifelse(is.na(All17$NoPat),0,All17$NoPat)
All17$Patents1[is.na(All17$Patents1)] <- 0
All17$Patents<-with(All17,ave(All17$Patents1,All17$GUO, FUN=sum,na.rm=TRUE))
length(which(All17$Patents!=0))

#Sum number of patent applications per GUO
All17$App<-as.numeric(All17$App)
All17$Applications1<-ifelse(is.na(All17$App),0,All17$App)
All17$Applications1[is.na(All17$Applications1)] <- 0
All17$Applications<-with(All17,ave(All17$Applications1,All17$GUO, FUN=sum,na.rm=TRUE))
length(which(All17$Applications!=0))

#Sum number of acquired patents per firm
All17$Greenfield2<-ifelse(All17$Added==2,All17$Patents1,0)
All17$Greenfield1<-All17$Greenfield2+All17$GF
All17$Greenfield1[is.na(All17$Greenfield1)] <- 0
All17$Greenfield<-with(All17,ave(All17$Greenfield1,All17$GUO, FUN=sum,na.rm=TRUE))
length(which(All17$Greenfield!=0))
All17$Brownfield2<-ifelse(All17$Added==3,All17$Patents1,0)
All17$Brownfield1<-All17$Brownfield2+All17$BF
All17$Brownfield1[is.na(All17$Brownfield1)] <- 0
All17$Brownfield<-with(All17,ave(All17$Brownfield1,All17$GUO, FUN=sum,na.rm=TRUE))
length(which(All17$Brownfield!=0))

#Sum of self-developed Patents
All17$SDPatents<-All17$Applications/(All17$Applications+All17$Greenfield+All17$Brownfield)
length(which(All17$SDPatents!=0))

#Sum number of technical AI patents per GUO
All17$GrantPat<-as.numeric(All17$GrantPat)
All17$GrantPat1<-ifelse(is.na(All17$GrantPat),0,All17$GrantPat)
All17$GrantPat1[is.na(All17$GrantPat1)] <- 0
All17$GrantedPatents<-with(All17,ave(All17$GrantPat1,All17$GUO, FUN=sum,na.rm=TRUE))
length(which(All17$GrantedPatents!=0))

#Sum number of technical AI patents per GUO
All17$HighTech<-as.numeric(All17$HighTech)
All17$HighTech1<-ifelse(is.na(All17$HighTech),0,All17$HighTech)
All17$HighTech1[is.na(All17$HighTech1)] <- 0
All17$HighTechPatents<-with(All17,ave(All17$HighTech1,All17$GUO, FUN=sum,na.rm=TRUE))
length(which(All17$HighTechPatents!=0))

#Number of patents located in the subsidiaries
All17$SubPat2<-ifelse(All17$GUO!=All17$Subsidiaries,All17$NoPat,NA)
All17$SubPat1<-ifelse(is.na(All17$SubPat2),0,All17$SubPat2)
All17$SubPat1<-as.numeric(All17$SubPat1)
All17$SubPat1[is.na(All17$SubPat1)] <- 0
All17$SubPat<- with(All17,ave(All17$SubPat1,All17$GUO, FUN=sum))
length(which(All17$SubPat!=0))

#Number of patents located in the subsidiaries
All17$SubApp2<-ifelse(All17$GUO!=All17$Subsidiaries,All17$App,NA)
All17$SubApp1<-ifelse(is.na(All17$SubApp2),0,All17$SubApp2)
All17$SubApp1<-as.numeric(All17$SubApp1)
All17$SubApp1[is.na(All17$SubApp1)] <- 0
All17$SubApp<- with(All17,ave(All17$SubApp1,All17$GUO, FUN=sum))
length(which(All17$SubApp!=0))

#Number of patents located in foreign countries
All17$Country<-substr(All17$Subsidiaries, 1, 2)
All17$Country2<-substr(All17$GUO, 1, 2)
All17$ForPat2<-ifelse(All17$Country!=All17$Country2,All17$NoPat,NA)
All17$ForPat1<-ifelse(is.na(All17$ForPat2),0,All17$ForPat2)
All17$ForPat1<-as.numeric(All17$ForPat1)
All17$ForPat1[is.na(All17$ForPat1)] <- 0
All17$ForPat<- with(All17,ave(All17$ForPat1,All17$GUO, FUN=sum))

#Number of patents located in foreign countries
All17$ForApp2<-ifelse(All17$Country!=All17$Country2,All17$App,NA)
All17$ForApp1<-ifelse(is.na(All17$ForApp2),0,All17$ForApp2)
All17$ForApp1<-as.numeric(All17$ForApp1)
All17$ForApp1[is.na(All17$ForApp1)] <- 0
All17$ForApp<- with(All17,ave(All17$ForApp1,All17$GUO, FUN=sum))


##AI Patents
#Sum number of AI patents per GUO
All17$AINoPat<-as.numeric(All17$AINoPat)
All17$AIPatents1<-ifelse(is.na(All17$AINoPat),0,All17$AINoPat)
All17$AIPatents1[is.na(All17$AIPatents1)] <- 0
All17$AIPatents<-with(All17,ave(All17$AIPatents1,All17$GUO, FUN=sum,na.rm=TRUE))
length(which(All17$AIPatents!=0))

#Sum number of patent applications per GUO
All17$AIApp<-as.numeric(All17$AIApp)
All17$AIApplications1<-ifelse(is.na(All17$AIApp),0,All17$AIApp)
All17$AIApplications1[is.na(All17$AIApplications1)] <- 0
All17$AIApplications<-with(All17,ave(All17$AIApplications1,All17$GUO, FUN=sum,na.rm=TRUE))
length(which(All17$AIApplications!=0))

#Sum number of granted AI patents per GUO
All17$AIGrantPat<-as.numeric(All17$AIGrantPat)
All17$AIGrantPat1<-ifelse(is.na(All17$AIGrantPat),0,All17$AIGrantPat)
All17$AIGrantPat1[is.na(All17$AIGrantPat1)] <- 0
All17$AIGrantedPatents<-with(All17,ave(All17$AIGrantPat1,All17$GUO, FUN=sum,na.rm=TRUE))
length(which(All17$AIGrantedPatents!=0))

#Sum number of acquired patents per firm
All17$AIGreenfield2<-ifelse(All17$Added==2,All17$AIPatents1,0)
All17$AIGreenfield1<-All17$AIGreenfield2+All17$AIGF
All17$AIGreenfield1[is.na(All17$AIGreenfield1)] <- 0
All17$AIGreenfield<-with(All17,ave(All17$AIGreenfield1,All17$GUO, FUN=sum,na.rm=TRUE))
length(which(All17$AIGreenfield!=0))
All17$AIBrownfield2<-ifelse(All17$Added==3,All17$AIPatents1,0)
All17$AIBrownfield1<-All17$AIBrownfield2+All17$AIBF
All17$AIBrownfield1[is.na(All17$AIBrownfield1)] <- 0
All17$AIBrownfield<-with(All17,ave(All17$AIBrownfield1,All17$GUO, FUN=sum,na.rm=TRUE))
length(which(All17$AIBrownfield!=0))

#Sum of self-developed Patents
All17$AISDPatents<-All17$AIApplications/(All17$AIApplications+All17$AIGreenfield+All17$AIBrownfield)
length(which(All17$SDPatents!=0))

#Sum number of technical AI patents per GUO
All17$TechPat<-as.numeric(All17$TechPat)
All17$TechPat1<-ifelse(is.na(All17$TechPat),0,All17$TechPat)
All17$TechPat1[is.na(All17$TechPat1)] <- 0
All17$AITechPatents<-with(All17,ave(All17$TechPat1,All17$GUO, FUN=sum,na.rm=TRUE))
length(which(All17$AITechPatents!=0))

#Sum number of technical AI patents per GUO
All17$AppTech<-as.numeric(All17$AppTech)
All17$TechApplications1<-ifelse(is.na(All17$AppTech),0,All17$AppTech)
All17$TechApplications1[is.na(All17$TechApplications1)] <- 0
All17$AITechApplications<-with(All17,ave(All17$TechApplications1,All17$GUO, FUN=sum,na.rm=TRUE))
length(which(All17$TechApplications!=0))

#Sum number of Functional AI patents per GUO
All17$FunctPat<-as.numeric(All17$FunctPat)
All17$FunctPat1<-ifelse(is.na(All17$FunctPat),0,All17$FunctPat)
All17$FunctPat1[is.na(All17$FunctPat1)] <- 0
All17$AIFunctPatents<-with(All17,ave(All17$FunctPat1,All17$GUO, FUN=sum,na.rm=TRUE))
length(which(All17$AIFunctPatents!=0))

#Sum number of Functional AI patents per GUO
All17$AppFunct<-as.numeric(All17$AppFunct)
All17$FunctApplications1<-ifelse(is.na(All17$AppFunct),0,All17$AppFunct)
All17$FunctApplications1[is.na(All17$FunctApplications1)] <- 0
All17$AIFunctApplications<-with(All17,ave(All17$FunctApplications1,All17$GUO, FUN=sum,na.rm=TRUE))
length(which(All17$FunctApplications!=0))

#Number of patents located in the subsidiaries
All17$AISubPat2<-ifelse(All17$GUO!=All17$Subsidiaries,All17$AINoPat,NA)
All17$AISubPat1<-ifelse(is.na(All17$AISubPat2),0,All17$AISubPat2)
All17$AISubPat1<-as.numeric(All17$AISubPat1)
All17$AISubPat1[is.na(All17$AISubPat1)] <- 0
All17$AISubPat<- with(All17,ave(All17$AISubPat1,All17$GUO, FUN=sum))
length(which(All17$AISubPat!=0))

#Number of patents located in the AISubsidiaries
All17$AISubApp2<-ifelse(All17$GUO!=All17$Subsidiaries,All17$AIApp,NA)
All17$AISubApp1<-ifelse(is.na(All17$AISubApp2),0,All17$AISubApp2)
All17$AISubApp1<-as.numeric(All17$AISubApp1)
All17$AISubApp1[is.na(All17$AISubApp1)] <- 0
All17$AISubApp<- with(All17,ave(All17$AISubApp1,All17$GUO, FUN=sum))
length(which(All17$AISubApp!=0))

#Number of patents located in foreign countries
All17$AIForPat2<-ifelse(All17$Country!=All17$Country2,All17$AINoPat,NA)
All17$AIForPat1<-ifelse(is.na(All17$AIForPat2),0,All17$AIForPat2)
All17$AIForPat1<-as.numeric(All17$AIForPat1)
All17$AIForPat1[is.na(All17$AIForPat1)] <- 0
All17$AIForPat<- with(All17,ave(All17$AIForPat1,All17$GUO, FUN=sum))

#Number of patents located in AIForeign countries
All17$AIForApp2<-ifelse(All17$Country!=All17$Country2,All17$AIApp,NA)
All17$AIForApp1<-ifelse(is.na(All17$AIForApp2),0,All17$AIForApp2)
All17$AIForApp1<-as.numeric(All17$AIForApp1)
All17$AIForApp1[is.na(All17$AIForApp1)] <- 0
All17$AIForApp<- with(All17,ave(All17$AIForApp1,All17$GUO, FUN=sum))

#Delete unnecessary variables
All17[,34:52]<-NULL
All17<-All17[,c(-2,-4,-35,-37,-38,-40,-41,-44,-46,-48,-49,-51,-52,-54,-55,-56,-57,-59,-60,-62,-64,-66,-68,-69,-71,-72,-75,-77,-79,-81,-83,-84,-86,-87,-89,-90,-92,-93)]

#Delete duplicate GUOs
All17<-All17[!duplicated(All17$GUO),]

#Rename Dataset
names(All17)<-c("BvDID","Year","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub","Patents","Applications",
                "GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents","SubPat","SubApp","ForPat","ForApp",
                "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
                "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp")

#Patent data descriptives
sum(All17$Patents,na.rm = TRUE)
sum(All17$Applications,na.rm = TRUE)
sum(All17$GreenfieldPatents,na.rm = TRUE)
sum(All17$BrownfieldPatents,na.rm = TRUE)
sum(All17$SDPatents,na.rm = TRUE)
sum(All17$GrantedPatents,na.rm = TRUE)
sum(All17$HighTechPatents,na.rm = TRUE)
sum(All17$SubPat,na.rm = TRUE)
sum(All17$SubApp,na.rm = TRUE)
sum(All17$ForPat,na.rm = TRUE)
sum(All17$ForApp,na.rm = TRUE)
sum(All17$AIPatents,na.rm = TRUE)
sum(All17$AIApplications,na.rm = TRUE)
sum(All17$AIGrantedPatents,na.rm = TRUE)
sum(All17$AIGreenfieldPatents,na.rm = TRUE)
sum(All17$AIBrownfieldPatents,na.rm = TRUE)
sum(All17$AISDPatents,na.rm = TRUE)
sum(All17$AITechPatents,na.rm = TRUE)
sum(All17$AITechApps,na.rm = TRUE)
sum(All17$AIFunctPatents,na.rm = TRUE)
sum(All17$AIFunctApps,na.rm = TRUE)
sum(All17$AISubPat,na.rm = TRUE)
sum(All17$AISubApp,na.rm = TRUE)
sum(All17$AIForPat,na.rm = TRUE)
sum(All17$AIForApp,na.rm = TRUE)


##Add Patent data to the subsidiary data - 2018
Full8<-fread("files_created_code1/Full8.csv",sep=";", dec=",")
Patents2018<-fread("files_created_code1/Patents2018.csv",sep=";", dec=",")

names(Full8) <- c("GUO","Subsidiaries","Year","Added","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                  "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                  "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")

All18<-left_join(Full8,Patents2018,by="Subsidiaries",na_matches="never")

##Check quality of the match
#Check the NAs per rows
length(which(!is.na(All18$NoPat)))
#Check for the number of matches with BvDID Match #1692263 - NOW: 2169248
All18$Check<-ifelse(All18$GUO!=All18$Subsidiaries,1,NA)
length(which(!is.na(All18$Check)))
All18$Check<-NULL

#Sum number of patents per GUO
All18$NoPat<-as.numeric(All18$NoPat)
All18<-All18[order(All18[,1]),]
All18$Patents1<-ifelse(is.na(All18$NoPat),0,All18$NoPat)
All18$Patents1[is.na(All18$Patents1)] <- 0
All18$Patents<-with(All18,ave(All18$Patents1,All18$GUO, FUN=sum,na.rm=TRUE))
length(which(All18$Patents!=0))

#Sum number of patent applications per GUO
All18$App<-as.numeric(All18$App)
All18$Applications1<-ifelse(is.na(All18$App),0,All18$App)
All18$Applications1[is.na(All18$Applications1)] <- 0
All18$Applications<-with(All18,ave(All18$Applications1,All18$GUO, FUN=sum,na.rm=TRUE))
length(which(All18$Applications!=0))

#Sum number of acquired patents per firm
All18$Greenfield2<-ifelse(All18$Added==2,All18$Patents1,0)
All18$Greenfield1<-All18$Greenfield2+All18$GF
All18$Greenfield1[is.na(All18$Greenfield1)] <- 0
All18$Greenfield<-with(All18,ave(All18$Greenfield1,All18$GUO, FUN=sum,na.rm=TRUE))
length(which(All18$Greenfield!=0))
All18$Brownfield2<-ifelse(All18$Added==3,All18$Patents1,0)
All18$Brownfield1<-All18$Brownfield2+All18$BF
All18$Brownfield1[is.na(All18$Brownfield1)] <- 0
All18$Brownfield<-with(All18,ave(All18$Brownfield1,All18$GUO, FUN=sum,na.rm=TRUE))
length(which(All18$Brownfield!=0))

#Sum of self-developed Patents
All18$SDPatents<-All18$Applications/(All18$Applications+All18$Greenfield+All18$Brownfield)
length(which(All18$SDPatents!=0))

#Sum number of technical AI patents per GUO
All18$GrantPat<-as.numeric(All18$GrantPat)
All18$GrantPat1<-ifelse(is.na(All18$GrantPat),0,All18$GrantPat)
All18$GrantPat1[is.na(All18$GrantPat1)] <- 0
All18$GrantedPatents<-with(All18,ave(All18$GrantPat1,All18$GUO, FUN=sum,na.rm=TRUE))
length(which(All18$GrantedPatents!=0))

#Sum number of technical AI patents per GUO
All18$HighTech<-as.numeric(All18$HighTech)
All18$HighTech1<-ifelse(is.na(All18$HighTech),0,All18$HighTech)
All18$HighTech1[is.na(All18$HighTech1)] <- 0
All18$HighTechPatents<-with(All18,ave(All18$HighTech1,All18$GUO, FUN=sum,na.rm=TRUE))
length(which(All18$HighTechPatents!=0))

#Number of patents located in the subsidiaries
All18$SubPat2<-ifelse(All18$GUO!=All18$Subsidiaries,All18$NoPat,NA)
All18$SubPat1<-ifelse(is.na(All18$SubPat2),0,All18$SubPat2)
All18$SubPat1<-as.numeric(All18$SubPat1)
All18$SubPat1[is.na(All18$SubPat1)] <- 0
All18$SubPat<- with(All18,ave(All18$SubPat1,All18$GUO, FUN=sum))
length(which(All18$SubPat!=0))

#Number of patents located in the subsidiaries
All18$SubApp2<-ifelse(All18$GUO!=All18$Subsidiaries,All18$App,NA)
All18$SubApp1<-ifelse(is.na(All18$SubApp2),0,All18$SubApp2)
All18$SubApp1<-as.numeric(All18$SubApp1)
All18$SubApp1[is.na(All18$SubApp1)] <- 0
All18$SubApp<- with(All18,ave(All18$SubApp1,All18$GUO, FUN=sum))
length(which(All18$SubApp!=0))

#Number of patents located in foreign countries
All18$Country<-substr(All18$Subsidiaries, 1, 2)
All18$Country2<-substr(All18$GUO, 1, 2)
All18$ForPat2<-ifelse(All18$Country!=All18$Country2,All18$NoPat,NA)
All18$ForPat1<-ifelse(is.na(All18$ForPat2),0,All18$ForPat2)
All18$ForPat1<-as.numeric(All18$ForPat1)
All18$ForPat1[is.na(All18$ForPat1)] <- 0
All18$ForPat<- with(All18,ave(All18$ForPat1,All18$GUO, FUN=sum))

#Number of patents located in foreign countries
All18$ForApp2<-ifelse(All18$Country!=All18$Country2,All18$App,NA)
All18$ForApp1<-ifelse(is.na(All18$ForApp2),0,All18$ForApp2)
All18$ForApp1<-as.numeric(All18$ForApp1)
All18$ForApp1[is.na(All18$ForApp1)] <- 0
All18$ForApp<- with(All18,ave(All18$ForApp1,All18$GUO, FUN=sum))


##AI Patents
#Sum number of AI patents per GUO
All18$AINoPat<-as.numeric(All18$AINoPat)
All18$AIPatents1<-ifelse(is.na(All18$AINoPat),0,All18$AINoPat)
All18$AIPatents1[is.na(All18$AIPatents1)] <- 0
All18$AIPatents<-with(All18,ave(All18$AIPatents1,All18$GUO, FUN=sum,na.rm=TRUE))
length(which(All18$AIPatents!=0))

#Sum number of patent applications per GUO
All18$AIApp<-as.numeric(All18$AIApp)
All18$AIApplications1<-ifelse(is.na(All18$AIApp),0,All18$AIApp)
All18$AIApplications1[is.na(All18$AIApplications1)] <- 0
All18$AIApplications<-with(All18,ave(All18$AIApplications1,All18$GUO, FUN=sum,na.rm=TRUE))
length(which(All18$AIApplications!=0))

#Sum number of granted AI patents per GUO
All18$AIGrantPat<-as.numeric(All18$AIGrantPat)
All18$AIGrantPat1<-ifelse(is.na(All18$AIGrantPat),0,All18$AIGrantPat)
All18$AIGrantPat1[is.na(All18$AIGrantPat1)] <- 0
All18$AIGrantedPatents<-with(All18,ave(All18$AIGrantPat1,All18$GUO, FUN=sum,na.rm=TRUE))
length(which(All18$AIGrantedPatents!=0))

#Sum number of acquired patents per firm
All18$AIGreenfield2<-ifelse(All18$Added==2,All18$AIPatents1,0)
All18$AIGreenfield1<-All18$AIGreenfield2+All18$AIGF
All18$AIGreenfield1[is.na(All18$AIGreenfield1)] <- 0
All18$AIGreenfield<-with(All18,ave(All18$AIGreenfield1,All18$GUO, FUN=sum,na.rm=TRUE))
length(which(All18$AIGreenfield!=0))
All18$AIBrownfield2<-ifelse(All18$Added==3,All18$AIPatents1,0)
All18$AIBrownfield1<-All18$AIBrownfield2+All18$AIBF
All18$AIBrownfield1[is.na(All18$AIBrownfield1)] <- 0
All18$AIBrownfield<-with(All18,ave(All18$AIBrownfield1,All18$GUO, FUN=sum,na.rm=TRUE))
length(which(All18$AIBrownfield!=0))

#Sum of self-developed Patents
All18$AISDPatents<-All18$AIApplications/(All18$AIApplications+All18$AIGreenfield+All18$AIBrownfield)
length(which(All18$SDPatents!=0))

#Sum number of technical AI patents per GUO
All18$TechPat<-as.numeric(All18$TechPat)
All18$TechPat1<-ifelse(is.na(All18$TechPat),0,All18$TechPat)
All18$TechPat1[is.na(All18$TechPat1)] <- 0
All18$AITechPatents<-with(All18,ave(All18$TechPat1,All18$GUO, FUN=sum,na.rm=TRUE))
length(which(All18$AITechPatents!=0))

#Sum number of technical AI patents per GUO
All18$AppTech<-as.numeric(All18$AppTech)
All18$TechApplications1<-ifelse(is.na(All18$AppTech),0,All18$AppTech)
All18$TechApplications1[is.na(All18$TechApplications1)] <- 0
All18$AITechApplications<-with(All18,ave(All18$TechApplications1,All18$GUO, FUN=sum,na.rm=TRUE))
length(which(All18$TechApplications!=0))

#Sum number of Functional AI patents per GUO
All18$FunctPat<-as.numeric(All18$FunctPat)
All18$FunctPat1<-ifelse(is.na(All18$FunctPat),0,All18$FunctPat)
All18$FunctPat1[is.na(All18$FunctPat1)] <- 0
All18$AIFunctPatents<-with(All18,ave(All18$FunctPat1,All18$GUO, FUN=sum,na.rm=TRUE))
length(which(All18$AIFunctPatents!=0))

#Sum number of Functional AI patents per GUO
All18$AppFunct<-as.numeric(All18$AppFunct)
All18$FunctApplications1<-ifelse(is.na(All18$AppFunct),0,All18$AppFunct)
All18$FunctApplications1[is.na(All18$FunctApplications1)] <- 0
All18$AIFunctApplications<-with(All18,ave(All18$FunctApplications1,All18$GUO, FUN=sum,na.rm=TRUE))
length(which(All18$FunctApplications!=0))

#Number of patents located in the subsidiaries
All18$AISubPat2<-ifelse(All18$GUO!=All18$Subsidiaries,All18$AINoPat,NA)
All18$AISubPat1<-ifelse(is.na(All18$AISubPat2),0,All18$AISubPat2)
All18$AISubPat1<-as.numeric(All18$AISubPat1)
All18$AISubPat1[is.na(All18$AISubPat1)] <- 0
All18$AISubPat<- with(All18,ave(All18$AISubPat1,All18$GUO, FUN=sum))
length(which(All18$AISubPat!=0))

#Number of patents located in the AISubsidiaries
All18$AISubApp2<-ifelse(All18$GUO!=All18$Subsidiaries,All18$AIApp,NA)
All18$AISubApp1<-ifelse(is.na(All18$AISubApp2),0,All18$AISubApp2)
All18$AISubApp1<-as.numeric(All18$AISubApp1)
All18$AISubApp1[is.na(All18$AISubApp1)] <- 0
All18$AISubApp<- with(All18,ave(All18$AISubApp1,All18$GUO, FUN=sum))
length(which(All18$AISubApp!=0))

#Number of patents located in foreign countries
All18$AIForPat2<-ifelse(All18$Country!=All18$Country2,All18$AINoPat,NA)
All18$AIForPat1<-ifelse(is.na(All18$AIForPat2),0,All18$AIForPat2)
All18$AIForPat1<-as.numeric(All18$AIForPat1)
All18$AIForPat1[is.na(All18$AIForPat1)] <- 0
All18$AIForPat<- with(All18,ave(All18$AIForPat1,All18$GUO, FUN=sum))

#Number of patents located in AIForeign countries
All18$AIForApp2<-ifelse(All18$Country!=All18$Country2,All18$AIApp,NA)
All18$AIForApp1<-ifelse(is.na(All18$AIForApp2),0,All18$AIForApp2)
All18$AIForApp1<-as.numeric(All18$AIForApp1)
All18$AIForApp1[is.na(All18$AIForApp1)] <- 0
All18$AIForApp<- with(All18,ave(All18$AIForApp1,All18$GUO, FUN=sum))

#Delete unnecessary variables
All18[,34:52]<-NULL
All18<-All18[,c(-2,-4,-35,-37,-38,-40,-41,-44,-46,-48,-49,-51,-52,-54,-55,-56,-57,-59,-60,-62,-64,-66,-68,-69,-71,-72,-75,-77,-79,-81,-83,-84,-86,-87,-89,-90,-92,-93)]

#Delete duplicate GUOs
All18<-All18[!duplicated(All18$GUO),]

#Rename Dataset
names(All18)<-c("BvDID","Year","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub","Patents","Applications",
                "GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents","SubPat","SubApp","ForPat","ForApp",
                "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
                "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp")

#Patent data descriptives
sum(All18$Patents,na.rm = TRUE)
sum(All18$Applications,na.rm = TRUE)
sum(All18$GreenfieldPatents,na.rm = TRUE)
sum(All18$BrownfieldPatents,na.rm = TRUE)
sum(All18$SDPatents,na.rm = TRUE)
sum(All18$GrantedPatents,na.rm = TRUE)
sum(All18$HighTechPatents,na.rm = TRUE)
sum(All18$SubPat,na.rm = TRUE)
sum(All18$SubApp,na.rm = TRUE)
sum(All18$ForPat,na.rm = TRUE)
sum(All18$ForApp,na.rm = TRUE)
sum(All18$AIPatents,na.rm = TRUE)
sum(All18$AIApplications,na.rm = TRUE)
sum(All18$AIGrantedPatents,na.rm = TRUE)
sum(All18$AIGreenfieldPatents,na.rm = TRUE)
sum(All18$AIBrownfieldPatents,na.rm = TRUE)
sum(All18$AISDPatents,na.rm = TRUE)
sum(All18$AITechPatents,na.rm = TRUE)
sum(All18$AITechApps,na.rm = TRUE)
sum(All18$AIFunctPatents,na.rm = TRUE)
sum(All18$AIFunctApps,na.rm = TRUE)
sum(All18$AISubPat,na.rm = TRUE)
sum(All18$AISubApp,na.rm = TRUE)
sum(All18$AIForPat,na.rm = TRUE)
sum(All18$AIForApp,na.rm = TRUE)


##Add Patent data to the subsidiary data - 2019
Full9<-fread("files_created_code1/Full9.csv",sep=";", dec=",")
Patents2019<-fread("files_created_code1/Patents2019.csv",sep=";", dec=",")

names(Full9) <- c("GUO","Subsidiaries","Year","Added","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                  "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                  "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")

All19<-left_join(Full9,Patents2019,by="Subsidiaries",na_matches="never")

##Check quality of the match
#Check the NAs per rows
length(which(!is.na(All19$NoPat)))
#Check for the number of matches with BvDID Match #1692263 - NOW: 2169248
All19$Check<-ifelse(All19$GUO!=All19$Subsidiaries,1,NA)
length(which(!is.na(All19$Check)))
All19$Check<-NULL

#Sum number of patents per GUO
All19$NoPat<-as.numeric(All19$NoPat)
All19<-All19[order(All19[,1]),]
All19$Patents1<-ifelse(is.na(All19$NoPat),0,All19$NoPat)
All19$Patents1[is.na(All19$Patents1)] <- 0
All19$Patents<-with(All19,ave(All19$Patents1,All19$GUO, FUN=sum,na.rm=TRUE))
length(which(All19$Patents!=0))

#Sum number of patent applications per GUO
All19$App<-as.numeric(All19$App)
All19$Applications1<-ifelse(is.na(All19$App),0,All19$App)
All19$Applications1[is.na(All19$Applications1)] <- 0
All19$Applications<-with(All19,ave(All19$Applications1,All19$GUO, FUN=sum,na.rm=TRUE))
length(which(All19$Applications!=0))

#Sum number of acquired patents per firm
All19$Greenfield2<-ifelse(All19$Added==2,All19$Patents1,0)
All19$Greenfield1<-All19$Greenfield2+All19$GF
All19$Greenfield1[is.na(All19$Greenfield1)] <- 0
All19$Greenfield<-with(All19,ave(All19$Greenfield1,All19$GUO, FUN=sum,na.rm=TRUE))
length(which(All19$Greenfield!=0))
All19$Brownfield2<-ifelse(All19$Added==3,All19$Patents1,0)
All19$Brownfield1<-All19$Brownfield2+All19$BF
All19$Brownfield1[is.na(All19$Brownfield1)] <- 0
All19$Brownfield<-with(All19,ave(All19$Brownfield1,All19$GUO, FUN=sum,na.rm=TRUE))
length(which(All19$Brownfield!=0))

#Sum of self-developed Patents
All19$SDPatents<-All19$Applications/(All19$Applications+All19$Greenfield+All19$Brownfield)
length(which(All19$SDPatents!=0))

#Sum number of technical AI patents per GUO
All19$GrantPat<-as.numeric(All19$GrantPat)
All19$GrantPat1<-ifelse(is.na(All19$GrantPat),0,All19$GrantPat)
All19$GrantPat1[is.na(All19$GrantPat1)] <- 0
All19$GrantedPatents<-with(All19,ave(All19$GrantPat1,All19$GUO, FUN=sum,na.rm=TRUE))
length(which(All19$GrantedPatents!=0))

#Sum number of technical AI patents per GUO
All19$HighTech<-as.numeric(All19$HighTech)
All19$HighTech1<-ifelse(is.na(All19$HighTech),0,All19$HighTech)
All19$HighTech1[is.na(All19$HighTech1)] <- 0
All19$HighTechPatents<-with(All19,ave(All19$HighTech1,All19$GUO, FUN=sum,na.rm=TRUE))
length(which(All19$HighTechPatents!=0))

#Number of patents located in the subsidiaries
All19$SubPat2<-ifelse(All19$GUO!=All19$Subsidiaries,All19$NoPat,NA)
All19$SubPat1<-ifelse(is.na(All19$SubPat2),0,All19$SubPat2)
All19$SubPat1<-as.numeric(All19$SubPat1)
All19$SubPat1[is.na(All19$SubPat1)] <- 0
All19$SubPat<- with(All19,ave(All19$SubPat1,All19$GUO, FUN=sum))
length(which(All19$SubPat!=0))

#Number of patents located in the subsidiaries
All19$SubApp2<-ifelse(All19$GUO!=All19$Subsidiaries,All19$App,NA)
All19$SubApp1<-ifelse(is.na(All19$SubApp2),0,All19$SubApp2)
All19$SubApp1<-as.numeric(All19$SubApp1)
All19$SubApp1[is.na(All19$SubApp1)] <- 0
All19$SubApp<- with(All19,ave(All19$SubApp1,All19$GUO, FUN=sum))
length(which(All19$SubApp!=0))

#Number of patents located in foreign countries
All19$Country<-substr(All19$Subsidiaries, 1, 2)
All19$Country2<-substr(All19$GUO, 1, 2)
All19$ForPat2<-ifelse(All19$Country!=All19$Country2,All19$NoPat,NA)
All19$ForPat1<-ifelse(is.na(All19$ForPat2),0,All19$ForPat2)
All19$ForPat1<-as.numeric(All19$ForPat1)
All19$ForPat1[is.na(All19$ForPat1)] <- 0
All19$ForPat<- with(All19,ave(All19$ForPat1,All19$GUO, FUN=sum))

#Number of patents located in foreign countries
All19$ForApp2<-ifelse(All19$Country!=All19$Country2,All19$App,NA)
All19$ForApp1<-ifelse(is.na(All19$ForApp2),0,All19$ForApp2)
All19$ForApp1<-as.numeric(All19$ForApp1)
All19$ForApp1[is.na(All19$ForApp1)] <- 0
All19$ForApp<- with(All19,ave(All19$ForApp1,All19$GUO, FUN=sum))


##AI Patents
#Sum number of AI patents per GUO
All19$AINoPat<-as.numeric(All19$AINoPat)
All19$AIPatents1<-ifelse(is.na(All19$AINoPat),0,All19$AINoPat)
All19$AIPatents1[is.na(All19$AIPatents1)] <- 0
All19$AIPatents<-with(All19,ave(All19$AIPatents1,All19$GUO, FUN=sum,na.rm=TRUE))
length(which(All19$AIPatents!=0))

#Sum number of patent applications per GUO
All19$AIApp<-as.numeric(All19$AIApp)
All19$AIApplications1<-ifelse(is.na(All19$AIApp),0,All19$AIApp)
All19$AIApplications1[is.na(All19$AIApplications1)] <- 0
All19$AIApplications<-with(All19,ave(All19$AIApplications1,All19$GUO, FUN=sum,na.rm=TRUE))
length(which(All19$AIApplications!=0))

#Sum number of granted AI patents per GUO
All19$AIGrantPat<-as.numeric(All19$AIGrantPat)
All19$AIGrantPat1<-ifelse(is.na(All19$AIGrantPat),0,All19$AIGrantPat)
All19$AIGrantPat1[is.na(All19$AIGrantPat1)] <- 0
All19$AIGrantedPatents<-with(All19,ave(All19$AIGrantPat1,All19$GUO, FUN=sum,na.rm=TRUE))
length(which(All19$AIGrantedPatents!=0))

#Sum number of acquired patents per firm
All19$AIGreenfield2<-ifelse(All19$Added==2,All19$AIPatents1,0)
All19$AIGreenfield1<-All19$AIGreenfield2+All19$AIGF
All19$AIGreenfield1[is.na(All19$AIGreenfield1)] <- 0
All19$AIGreenfield<-with(All19,ave(All19$AIGreenfield1,All19$GUO, FUN=sum,na.rm=TRUE))
length(which(All19$AIGreenfield!=0))
All19$AIBrownfield2<-ifelse(All19$Added==3,All19$AIPatents1,0)
All19$AIBrownfield1<-All19$AIBrownfield2+All19$AIBF
All19$AIBrownfield1[is.na(All19$AIBrownfield1)] <- 0
All19$AIBrownfield<-with(All19,ave(All19$AIBrownfield1,All19$GUO, FUN=sum,na.rm=TRUE))
length(which(All19$AIBrownfield!=0))

#Sum of self-developed Patents
All19$AISDPatents<-All19$AIApplications/(All19$AIApplications+All19$AIGreenfield+All19$AIBrownfield)
length(which(All19$SDPatents!=0))

#Sum number of technical AI patents per GUO
All19$TechPat<-as.numeric(All19$TechPat)
All19$TechPat1<-ifelse(is.na(All19$TechPat),0,All19$TechPat)
All19$TechPat1[is.na(All19$TechPat1)] <- 0
All19$AITechPatents<-with(All19,ave(All19$TechPat1,All19$GUO, FUN=sum,na.rm=TRUE))
length(which(All19$AITechPatents!=0))

#Sum number of technical AI patents per GUO
All19$AppTech<-as.numeric(All19$AppTech)
All19$TechApplications1<-ifelse(is.na(All19$AppTech),0,All19$AppTech)
All19$TechApplications1[is.na(All19$TechApplications1)] <- 0
All19$AITechApplications<-with(All19,ave(All19$TechApplications1,All19$GUO, FUN=sum,na.rm=TRUE))
length(which(All19$TechApplications!=0))

#Sum number of Functional AI patents per GUO
All19$FunctPat<-as.numeric(All19$FunctPat)
All19$FunctPat1<-ifelse(is.na(All19$FunctPat),0,All19$FunctPat)
All19$FunctPat1[is.na(All19$FunctPat1)] <- 0
All19$AIFunctPatents<-with(All19,ave(All19$FunctPat1,All19$GUO, FUN=sum,na.rm=TRUE))
length(which(All19$AIFunctPatents!=0))

#Sum number of Functional AI patents per GUO
All19$AppFunct<-as.numeric(All19$AppFunct)
All19$FunctApplications1<-ifelse(is.na(All19$AppFunct),0,All19$AppFunct)
All19$FunctApplications1[is.na(All19$FunctApplications1)] <- 0
All19$AIFunctApplications<-with(All19,ave(All19$FunctApplications1,All19$GUO, FUN=sum,na.rm=TRUE))
length(which(All19$FunctApplications!=0))

#Number of patents located in the subsidiaries
All19$AISubPat2<-ifelse(All19$GUO!=All19$Subsidiaries,All19$AINoPat,NA)
All19$AISubPat1<-ifelse(is.na(All19$AISubPat2),0,All19$AISubPat2)
All19$AISubPat1<-as.numeric(All19$AISubPat1)
All19$AISubPat1[is.na(All19$AISubPat1)] <- 0
All19$AISubPat<- with(All19,ave(All19$AISubPat1,All19$GUO, FUN=sum))
length(which(All19$AISubPat!=0))

#Number of patents located in the AISubsidiaries
All19$AISubApp2<-ifelse(All19$GUO!=All19$Subsidiaries,All19$AIApp,NA)
All19$AISubApp1<-ifelse(is.na(All19$AISubApp2),0,All19$AISubApp2)
All19$AISubApp1<-as.numeric(All19$AISubApp1)
All19$AISubApp1[is.na(All19$AISubApp1)] <- 0
All19$AISubApp<- with(All19,ave(All19$AISubApp1,All19$GUO, FUN=sum))
length(which(All19$AISubApp!=0))

#Number of patents located in foreign countries
All19$AIForPat2<-ifelse(All19$Country!=All19$Country2,All19$AINoPat,NA)
All19$AIForPat1<-ifelse(is.na(All19$AIForPat2),0,All19$AIForPat2)
All19$AIForPat1<-as.numeric(All19$AIForPat1)
All19$AIForPat1[is.na(All19$AIForPat1)] <- 0
All19$AIForPat<- with(All19,ave(All19$AIForPat1,All19$GUO, FUN=sum))

#Number of patents located in AIForeign countries
All19$AIForApp2<-ifelse(All19$Country!=All19$Country2,All19$AIApp,NA)
All19$AIForApp1<-ifelse(is.na(All19$AIForApp2),0,All19$AIForApp2)
All19$AIForApp1<-as.numeric(All19$AIForApp1)
All19$AIForApp1[is.na(All19$AIForApp1)] <- 0
All19$AIForApp<- with(All19,ave(All19$AIForApp1,All19$GUO, FUN=sum))

#Delete unnecessary variables
All19[,34:52]<-NULL
All19<-All19[,c(-2,-4,-35,-37,-38,-40,-41,-44,-46,-48,-49,-51,-52,-54,-55,-56,-57,-59,-60,-62,-64,-66,-68,-69,-71,-72,-75,-77,-79,-81,-83,-84,-86,-87,-89,-90,-92,-93)]

#Delete duplicate GUOs
All19<-All19[!duplicated(All19$GUO),]

#Rename Dataset
names(All19)<-c("BvDID","Year","NoSub","ForSub","MultiSub","Countries","AvSub","IPR","TP","GDP","GDPpc",
                "RD","LR","TR","Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP",
                "CGDPpc","CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub","Patents","Applications",
                "GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents","SubPat","SubApp","ForPat","ForApp",
                "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
                "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp")

#Patent data descriptives
sum(All19$Patents,na.rm = TRUE)
sum(All19$Applications,na.rm = TRUE)
sum(All19$GreenfieldPatents,na.rm = TRUE)
sum(All19$BrownfieldPatents,na.rm = TRUE)
sum(All19$SDPatents,na.rm = TRUE)
sum(All19$GrantedPatents,na.rm = TRUE)
sum(All19$HighTechPatents,na.rm = TRUE)
sum(All19$SubPat,na.rm = TRUE)
sum(All19$SubApp,na.rm = TRUE)
sum(All19$ForPat,na.rm = TRUE)
sum(All19$ForApp,na.rm = TRUE)
sum(All19$AIPatents,na.rm = TRUE)
sum(All19$AIApplications,na.rm = TRUE)
sum(All19$AIGrantedPatents,na.rm = TRUE)
sum(All19$AIGreenfieldPatents,na.rm = TRUE)
sum(All19$AIBrownfieldPatents,na.rm = TRUE)
sum(All19$AISDPatents,na.rm = TRUE)
sum(All19$AITechPatents,na.rm = TRUE)
sum(All19$AITechApps,na.rm = TRUE)
sum(All19$AIFunctPatents,na.rm = TRUE)
sum(All19$AIFunctApps,na.rm = TRUE)
sum(All19$AISubPat,na.rm = TRUE)
sum(All19$AISubApp,na.rm = TRUE)
sum(All19$AIForPat,na.rm = TRUE)
sum(All19$AIForApp,na.rm = TRUE)


##Only GUOs that are existing during all periods
All11$Check<-ifelse(All11$BvDID %in% All12$BvDID & All11$BvDID %in% All13$BvDID & All11$BvDID %in% All14$BvDID &
                       All11$BvDID %in% All15$BvDID & All11$BvDID %in% All16$BvDID & All11$BvDID %in% All17$BvDID &
                       All11$BvDID %in% All18$BvDID & All11$BvDID %in% All19$BvDID,1,NA)
length(which(is.na(All11$Check)))
All12$Check<-ifelse(All12$BvDID %in% All11$BvDID & All12$BvDID %in% All13$BvDID & All12$BvDID %in% All14$BvDID &
                       All12$BvDID %in% All15$BvDID & All12$BvDID %in% All16$BvDID & All12$BvDID %in% All17$BvDID &
                       All12$BvDID %in% All18$BvDID & All12$BvDID %in% All19$BvDID,1,NA)
length(which(is.na(All12$Check)))
All13$Check<-ifelse(All13$BvDID %in% All11$BvDID & All13$BvDID %in% All12$BvDID & All13$BvDID %in% All14$BvDID &
                       All13$BvDID %in% All15$BvDID & All13$BvDID %in% All16$BvDID & All13$BvDID %in% All17$BvDID &
                       All13$BvDID %in% All18$BvDID & All13$BvDID %in% All19$BvDID,1,NA)
length(which(is.na(All13$Check)))
All14$Check<-ifelse(All14$BvDID %in% All11$BvDID & All14$BvDID %in% All12$BvDID & All14$BvDID %in% All13$BvDID &
                       All14$BvDID %in% All15$BvDID & All14$BvDID %in% All16$BvDID & All14$BvDID %in% All17$BvDID &
                       All14$BvDID %in% All18$BvDID & All14$BvDID %in% All19$BvDID,1,NA)
length(which(is.na(All14$Check)))
All15$Check<-ifelse(All15$BvDID %in% All11$BvDID & All15$BvDID %in% All12$BvDID & All15$BvDID %in% All13$BvDID &
                       All15$BvDID %in% All14$BvDID & All15$BvDID %in% All16$BvDID & All15$BvDID %in% All17$BvDID &
                       All15$BvDID %in% All18$BvDID & All15$BvDID %in% All19$BvDID,1,NA)
length(which(is.na(All15$Check)))
All16$Check<-ifelse(All16$BvDID %in% All11$BvDID & All16$BvDID %in% All12$BvDID & All16$BvDID %in% All13$BvDID &
                       All16$BvDID %in% All14$BvDID & All16$BvDID %in% All15$BvDID & All16$BvDID %in% All17$BvDID &
                       All16$BvDID %in% All18$BvDID & All16$BvDID %in% All19$BvDID,1,NA)
length(which(is.na(All16$Check)))
All17$Check<-ifelse(All17$BvDID %in% All11$BvDID & All17$BvDID %in% All12$BvDID & All17$BvDID %in% All13$BvDID &
                       All17$BvDID %in% All14$BvDID & All17$BvDID %in% All15$BvDID & All17$BvDID %in% All16$BvDID &
                       All17$BvDID %in% All18$BvDID & All17$BvDID %in% All19$BvDID,1,NA)
length(which(is.na(All17$Check)))
All18$Check<-ifelse(All18$BvDID %in% All11$BvDID & All18$BvDID %in% All12$BvDID & All18$BvDID %in% All13$BvDID &
                       All18$BvDID %in% All14$BvDID & All18$BvDID %in% All15$BvDID & All18$BvDID %in% All16$BvDID &
                       All18$BvDID %in% All17$BvDID & All18$BvDID %in% All19$BvDID,1,NA)
length(which(is.na(All18$Check)))
All19$Check<-ifelse(All19$BvDID %in% All12$BvDID & All19$BvDID %in% All13$BvDID & All19$BvDID %in% All14$BvDID &
                       All19$BvDID %in% All15$BvDID & All19$BvDID %in% All16$BvDID & All19$BvDID %in% All17$BvDID &
                       All19$BvDID %in% All18$BvDID & All19$BvDID %in% All11$BvDID,1,NA)
length(which(is.na(All19$Check)))

All11<-subset(All11,!is.na(All11$Check))
All12<-subset(All12,!is.na(All12$Check))
All13<-subset(All13,!is.na(All13$Check))
All14<-subset(All14,!is.na(All14$Check))
All15<-subset(All15,!is.na(All15$Check))
All16<-subset(All16,!is.na(All16$Check))
All17<-subset(All17,!is.na(All17$Check))
All18<-subset(All18,!is.na(All18$Check))
All19<-subset(All19,!is.na(All19$Check))

All11$Check<-NULL
All12$Check<-NULL
All13$Check<-NULL
All14$Check<-NULL
All15$Check<-NULL
All16$Check<-NULL
All17$Check<-NULL
All18$Check<-NULL
All19$Check<-NULL




#   2. Calculate Multinationality Index of technological and institutional breadth and depth ####

#Technological  breadth

#2011
MTB11<-left_join(Full1,Patents2011,by="Subsidiaries",na_matches="never")

MTB11$MTB8<-case_when(MTB11$TR>79.9~5,
                      MTB11$TR<=79.9&MTB11$TR>59.9~4,
                      MTB11$TR<=50.9&MTB11$TR>39.9~3,
                      MTB11$TR<=39.9&MTB11$TR>19.9~2,
                      MTB11$TR<=19.9~1)

#Create variable for country of Subsidiaries
MTB11$Country1<-as.character(MTB11$GUO)
MTB11$Country2<-as.character(MTB11$Subsidiaries)
MTB11$Country1<-substr(MTB11$GUO, 1, 2)
MTB11$Country2<-substr(MTB11$Subsidiaries,1,2)

#Variable of number of countries
MTB11$Countries2<-ifelse(MTB11$Country1!=MTB11$Country2 & MTB11$Country2!=lag(MTB11$Country2),1,0)
MTB11$Countries1<-ifelse(MTB11$GUO==MTB11$Subsidiaries | MTB11$Countries2==1,1,0)
MTB11$Countries1<-as.numeric(MTB11$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MTB11<-subset(MTB11,!is.na(MTB11$MTB8)&MTB11$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MTB11$MTB7<-ave(MTB11$Countries1,MTB11[,c('GUO', 'MTB8')],FUN=sum)

#Number of unique technology contexts
MTB11$MTB6<-ave(MTB11$MTB8,MTB11[,c('GUO')],FUN=function(x) length(unique(x)))

MTB11$MTB5<-MTB11$MTB7/(MTB11$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MTB11$MTB5[is.na(MTB11$MTB5)] <- 0
MTB11$MTB5<-as.numeric(MTB11$MTB5)

#Remove duplicates to sum unique values
MTB11$Test<-paste(MTB11$GUO,MTB11$MTB8,MTB11$MTB5,sep=" ")
MTB11<-MTB11[!duplicated(MTB11$Test),]

MTB11$MTB4<-ave(MTB11$MTB5,MTB11$GUO,FUN=sum)
MTB11$MTB3<-MTB11$MTB4^2

MTB11$MTB<-MTB11$MTB3*(MTB11$MTB6/(MTB11$MTB6+1))

MTB11[,2:62]<-NULL
names(MTB11)<-c("BvDID","MTB")

MTB11<-MTB11[!duplicated(MTB11$BvDID),]


#2012
MTB12<-left_join(Full2,Patents2012,by="Subsidiaries",na_matches="never")

MTB12$MTB8<-case_when(MTB12$TR>79.9~5,
                      MTB12$TR<=79.9&MTB12$TR>59.9~4,
                      MTB12$TR<=59.9&MTB12$TR>39.9~3,
                      MTB12$TR<=39.9&MTB12$TR>19.9~2,
                      MTB12$TR<=19.9~1)

#Create variable for country of Subsidiaries
MTB12$Country1<-as.character(MTB12$GUO)
MTB12$Country2<-as.character(MTB12$Subsidiaries)
MTB12$Country1<-substr(MTB12$GUO, 1, 2)
MTB12$Country2<-substr(MTB12$Subsidiaries,1,2)

#Variable of number of countries
MTB12$Countries2<-ifelse(MTB12$Country1!=MTB12$Country2 & MTB12$Country2!=lag(MTB12$Country2),1,0)
MTB12$Countries1<-ifelse(MTB12$GUO==MTB12$Subsidiaries | MTB12$Countries2==1,1,0)
MTB12$Countries1<-as.numeric(MTB12$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MTB12<-subset(MTB12,!is.na(MTB12$MTB8)&MTB12$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MTB12$MTB7<-ave(MTB12$Countries1,MTB12[,c('GUO', 'MTB8')],FUN=sum)

#Number of unique technology contexts
MTB12$MTB6<-ave(MTB12$MTB8,MTB12[,c('GUO')],FUN=function(x) length(unique(x)))

MTB12$MTB5<-MTB12$MTB7/(MTB12$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MTB12$MTB5[is.na(MTB12$MTB5)] <- 0
MTB12$MTB5<-as.numeric(MTB12$MTB5)

#Remove duplicates to sum unique values
MTB12$Test<-paste(MTB12$GUO,MTB12$MTB8,MTB12$MTB5,sep=" ")
MTB12<-MTB12[!duplicated(MTB12$Test),]

MTB12$MTB4<-ave(MTB12$MTB5,MTB12$GUO,FUN=sum)
MTB12$MTB3<-MTB12$MTB4^2

MTB12$MTB<-MTB12$MTB3*(MTB12$MTB6/(MTB12$MTB6+1))

MTB12[,2:62]<-NULL
names(MTB12)<-c("BvDID","MTB")

MTB12<-MTB12[!duplicated(MTB12$BvDID),]


# 2013
MTB13<-left_join(Full3,Patents2013,by="Subsidiaries",na_matches="never")

MTB13$MTB8<-case_when(MTB13$TR>79.9~5,
                      MTB13$TR<=79.9&MTB13$TR>59.9~4,
                      MTB13$TR<=59.9&MTB13$TR>39.9~3,
                      MTB13$TR<=39.9&MTB13$TR>19.9~2,
                      MTB13$TR<=19.9~1)

#Create variable for country of Subsidiaries
MTB13$Country1<-as.character(MTB13$GUO)
MTB13$Country2<-as.character(MTB13$Subsidiaries)
MTB13$Country1<-substr(MTB13$GUO, 1, 2)
MTB13$Country2<-substr(MTB13$Subsidiaries,1,2)

#Variable of number of countries
MTB13$Countries2<-ifelse(MTB13$Country1!=MTB13$Country2 & MTB13$Country2!=lag(MTB13$Country2),1,0)
MTB13$Countries1<-ifelse(MTB13$GUO==MTB13$Subsidiaries | MTB13$Countries2==1,1,0)
MTB13$Countries1<-as.numeric(MTB13$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MTB13<-subset(MTB13,!is.na(MTB13$MTB8)&MTB13$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MTB13$MTB7<-ave(MTB13$Countries1,MTB13[,c('GUO', 'MTB8')],FUN=sum)

#Number of unique technology contexts
MTB13$MTB6<-ave(MTB13$MTB8,MTB13[,c('GUO')],FUN=function(x) length(unique(x)))

MTB13$MTB5<-MTB13$MTB7/(MTB13$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MTB13$MTB5[is.na(MTB13$MTB5)] <- 0
MTB13$MTB5<-as.numeric(MTB13$MTB5)

#Remove duplicates to sum unique values
MTB13$Test<-paste(MTB13$GUO,MTB13$MTB8,MTB13$MTB5,sep=" ")
MTB13<-MTB13[!duplicated(MTB13$Test),]

MTB13$MTB4<-ave(MTB13$MTB5,MTB13$GUO,FUN=sum)
MTB13$MTB3<-MTB13$MTB4^2

MTB13$MTB<-MTB13$MTB3*(MTB13$MTB6/(MTB13$MTB6+1))

MTB13[,2:62]<-NULL
names(MTB13)<-c("BvDID","MTB")

MTB13<-MTB13[!duplicated(MTB13$BvDID),]


#2014
MTB14<-left_join(Full4,Patents2014,by="Subsidiaries",na_matches="never")

MTB14$MTB8<-case_when(MTB14$TR>79.9~5,
                      MTB14$TR<=79.9&MTB14$TR>59.9~4,
                      MTB14$TR<=59.9&MTB14$TR>39.9~3,
                      MTB14$TR<=39.9&MTB14$TR>19.9~2,
                      MTB14$TR<=19.9~1)

#Create variable for country of Subsidiaries
MTB14$Country1<-as.character(MTB14$GUO)
MTB14$Country2<-as.character(MTB14$Subsidiaries)
MTB14$Country1<-substr(MTB14$GUO, 1, 2)
MTB14$Country2<-substr(MTB14$Subsidiaries,1,2)

#Variable of number of countries
MTB14$Countries2<-ifelse(MTB14$Country1!=MTB14$Country2 & MTB14$Country2!=lag(MTB14$Country2),1,0)
MTB14$Countries1<-ifelse(MTB14$GUO==MTB14$Subsidiaries | MTB14$Countries2==1,1,0)
MTB14$Countries1<-as.numeric(MTB14$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MTB14<-subset(MTB14,!is.na(MTB14$MTB8)&MTB14$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MTB14$MTB7<-ave(MTB14$Countries1,MTB14[,c('GUO', 'MTB8')],FUN=sum)

#Number of unique technology contexts
MTB14$MTB6<-ave(MTB14$MTB8,MTB14[,c('GUO')],FUN=function(x) length(unique(x)))

MTB14$MTB5<-MTB14$MTB7/(MTB14$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MTB14$MTB5[is.na(MTB14$MTB5)] <- 0
MTB14$MTB5<-as.numeric(MTB14$MTB5)

#Remove duplicates to sum unique values
MTB14$Test<-paste(MTB14$GUO,MTB14$MTB8,MTB14$MTB5,sep=" ")
MTB14<-MTB14[!duplicated(MTB14$Test),]

MTB14$MTB4<-ave(MTB14$MTB5,MTB14$GUO,FUN=sum)
MTB14$MTB3<-MTB14$MTB4^2

MTB14$MTB<-MTB14$MTB3*(MTB14$MTB6/(MTB14$MTB6+1))

MTB14[,2:62]<-NULL
names(MTB14)<-c("BvDID","MTB")

MTB14<-MTB14[!duplicated(MTB14$BvDID),]


#2015
MTB15<-left_join(Full5,Patents2015,by="Subsidiaries",na_matches="never")

MTB15$MTB8<-case_when(MTB15$TR>79.9~5,
                      MTB15$TR<=79.9&MTB15$TR>59.9~4,
                      MTB15$TR<=59.9&MTB15$TR>39.9~3,
                      MTB15$TR<=39.9&MTB15$TR>19.9~2,
                      MTB15$TR<=19.9~1)

#Create variable for country of Subsidiaries
MTB15$Country1<-as.character(MTB15$GUO)
MTB15$Country2<-as.character(MTB15$Subsidiaries)
MTB15$Country1<-substr(MTB15$GUO, 1, 2)
MTB15$Country2<-substr(MTB15$Subsidiaries,1,2)

#Variable of number of countries
MTB15$Countries2<-ifelse(MTB15$Country1!=MTB15$Country2 & MTB15$Country2!=lag(MTB15$Country2),1,0)
MTB15$Countries1<-ifelse(MTB15$GUO==MTB15$Subsidiaries | MTB15$Countries2==1,1,0)
MTB15$Countries1<-as.numeric(MTB15$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MTB15<-subset(MTB15,!is.na(MTB15$MTB8)&MTB15$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MTB15$MTB7<-ave(MTB15$Countries1,MTB15[,c('GUO', 'MTB8')],FUN=sum)

#Number of unique technology contexts
MTB15$MTB6<-ave(MTB15$MTB8,MTB15[,c('GUO')],FUN=function(x) length(unique(x)))

MTB15$MTB5<-MTB15$MTB7/(MTB15$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MTB15$MTB5[is.na(MTB15$MTB5)] <- 0
MTB15$MTB5<-as.numeric(MTB15$MTB5)

#Remove duplicates to sum unique values
MTB15$Test<-paste(MTB15$GUO,MTB15$MTB8,MTB15$MTB5,sep=" ")
MTB15<-MTB15[!duplicated(MTB15$Test),]

MTB15$MTB4<-ave(MTB15$MTB5,MTB15$GUO,FUN=sum)
MTB15$MTB3<-MTB15$MTB4^2

MTB15$MTB<-MTB15$MTB3*(MTB15$MTB6/(MTB15$MTB6+1))

MTB15[,2:62]<-NULL
names(MTB15)<-c("BvDID","MTB")

MTB15<-MTB15[!duplicated(MTB15$BvDID),]


#2016
MTB16<-left_join(Full6,Patents2016,by="Subsidiaries",na_matches="never")

MTB16$MTB8<-case_when(MTB16$TR>79.9~5,
                      MTB16$TR<=79.9&MTB16$TR>59.9~4,
                      MTB16$TR<=59.9&MTB16$TR>39.9~3,
                      MTB16$TR<=39.9&MTB16$TR>19.9~2,
                      MTB16$TR<=19.9~1)

#Create variable for country of Subsidiaries
MTB16$Country1<-as.character(MTB16$GUO)
MTB16$Country2<-as.character(MTB16$Subsidiaries)
MTB16$Country1<-substr(MTB16$GUO, 1, 2)
MTB16$Country2<-substr(MTB16$Subsidiaries,1,2)

#Variable of number of countries
MTB16$Countries2<-ifelse(MTB16$Country1!=MTB16$Country2 & MTB16$Country2!=lag(MTB16$Country2),1,0)
MTB16$Countries1<-ifelse(MTB16$GUO==MTB16$Subsidiaries | MTB16$Countries2==1,1,0)
MTB16$Countries1<-as.numeric(MTB16$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MTB16<-subset(MTB16,!is.na(MTB16$MTB8)&MTB16$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MTB16$MTB7<-ave(MTB16$Countries1,MTB16[,c('GUO', 'MTB8')],FUN=sum)

#Number of unique technology contexts
MTB16$MTB6<-ave(MTB16$MTB8,MTB16[,c('GUO')],FUN=function(x) length(unique(x)))

MTB16$MTB5<-MTB16$MTB7/(MTB16$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MTB16$MTB5[is.na(MTB16$MTB5)] <- 0
MTB16$MTB5<-as.numeric(MTB16$MTB5)

#Remove duplicates to sum unique values
MTB16$Test<-paste(MTB16$GUO,MTB16$MTB8,MTB16$MTB5,sep=" ")
MTB16<-MTB16[!duplicated(MTB16$Test),]

MTB16$MTB4<-ave(MTB16$MTB5,MTB16$GUO,FUN=sum)
MTB16$MTB3<-MTB16$MTB4^2

MTB16$MTB<-MTB16$MTB3*(MTB16$MTB6/(MTB16$MTB6+1))

MTB16[,2:62]<-NULL
names(MTB16)<-c("BvDID","MTB")

MTB16<-MTB16[!duplicated(MTB16$BvDID),]


#2017
MTB17<-left_join(Full7,Patents2017,by="Subsidiaries",na_matches="never")

MTB17$MTB8<-case_when(MTB17$TR>79.9~5,
                      MTB17$TR<=79.9&MTB17$TR>59.9~4,
                      MTB17$TR<=59.9&MTB17$TR>39.9~3,
                      MTB17$TR<=39.9&MTB17$TR>19.9~2,
                      MTB17$TR<=19.9~1)

#Create variable for country of Subsidiaries
MTB17$Country1<-as.character(MTB17$GUO)
MTB17$Country2<-as.character(MTB17$Subsidiaries)
MTB17$Country1<-substr(MTB17$GUO, 1, 2)
MTB17$Country2<-substr(MTB17$Subsidiaries,1,2)

#Variable of number of countries
MTB17$Countries2<-ifelse(MTB17$Country1!=MTB17$Country2 & MTB17$Country2!=lag(MTB17$Country2),1,0)
MTB17$Countries1<-ifelse(MTB17$GUO==MTB17$Subsidiaries | MTB17$Countries2==1,1,0)
MTB17$Countries1<-as.numeric(MTB17$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MTB17<-subset(MTB17,!is.na(MTB17$MTB8)&MTB17$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MTB17$MTB7<-ave(MTB17$Countries1,MTB17[,c('GUO', 'MTB8')],FUN=sum)

#Number of unique technology contexts
MTB17$MTB6<-ave(MTB17$MTB8,MTB17[,c('GUO')],FUN=function(x) length(unique(x)))

MTB17$MTB5<-MTB17$MTB7/(MTB17$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MTB17$MTB5[is.na(MTB17$MTB5)] <- 0
MTB17$MTB5<-as.numeric(MTB17$MTB5)

#Remove duplicates to sum unique values
MTB17$Test<-paste(MTB17$GUO,MTB17$MTB8,MTB17$MTB5,sep=" ")
MTB17<-MTB17[!duplicated(MTB17$Test),]

MTB17$MTB4<-ave(MTB17$MTB5,MTB17$GUO,FUN=sum)
MTB17$MTB3<-MTB17$MTB4^2

MTB17$MTB<-MTB17$MTB3*(MTB17$MTB6/(MTB17$MTB6+1))

MTB17[,2:62]<-NULL
names(MTB17)<-c("BvDID","MTB")

MTB17<-MTB17[!duplicated(MTB17$BvDID),]


#2018
MTB18<-left_join(Full8,Patents2018,by="Subsidiaries",na_matches="never")

MTB18$MTB8<-case_when(MTB18$TR>79.9~5,
                      MTB18$TR<=79.9&MTB18$TR>59.9~4,
                      MTB18$TR<=59.9&MTB18$TR>39.9~3,
                      MTB18$TR<=39.9&MTB18$TR>19.9~2,
                      MTB18$TR<=19.9~1)

#Create variable for country of Subsidiaries
MTB18$Country1<-as.character(MTB18$GUO)
MTB18$Country2<-as.character(MTB18$Subsidiaries)
MTB18$Country1<-substr(MTB18$GUO, 1, 2)
MTB18$Country2<-substr(MTB18$Subsidiaries,1,2)

#Variable of number of countries
MTB18$Countries2<-ifelse(MTB18$Country1!=MTB18$Country2 & MTB18$Country2!=lag(MTB18$Country2),1,0)
MTB18$Countries1<-ifelse(MTB18$GUO==MTB18$Subsidiaries | MTB18$Countries2==1,1,0)
MTB18$Countries1<-as.numeric(MTB18$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MTB18<-subset(MTB18,!is.na(MTB18$MTB8)&MTB18$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MTB18$MTB7<-ave(MTB18$Countries1,MTB18[,c('GUO', 'MTB8')],FUN=sum)

#Number of unique technology contexts
MTB18$MTB6<-ave(MTB18$MTB8,MTB18[,c('GUO')],FUN=function(x) length(unique(x)))

MTB18$MTB5<-MTB18$MTB7/(MTB18$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MTB18$MTB5[is.na(MTB18$MTB5)] <- 0
MTB18$MTB5<-as.numeric(MTB18$MTB5)

#Remove duplicates to sum unique values
MTB18$Test<-paste(MTB18$GUO,MTB18$MTB8,MTB18$MTB5,sep=" ")
MTB18<-MTB18[!duplicated(MTB18$Test),]

MTB18$MTB4<-ave(MTB18$MTB5,MTB18$GUO,FUN=sum)
MTB18$MTB3<-MTB18$MTB4^2

MTB18$MTB<-MTB18$MTB3*(MTB18$MTB6/(MTB18$MTB6+1))

MTB18[,2:62]<-NULL
names(MTB18)<-c("BvDID","MTB")

MTB18<-MTB18[!duplicated(MTB18$BvDID),]


#2019
MTB19<-left_join(Full9,Patents2019,by="Subsidiaries",na_matches="never")

MTB19$MTB8<-case_when(MTB19$TR>79.9~5,
                      MTB19$TR<=79.9&MTB19$TR>59.9~4,
                      MTB19$TR<=59.9&MTB19$TR>39.9~3,
                      MTB19$TR<=39.9&MTB19$TR>19.9~2,
                      MTB19$TR<=19.9~1)

#Create variable for country of Subsidiaries
MTB19$Country1<-as.character(MTB19$GUO)
MTB19$Country2<-as.character(MTB19$Subsidiaries)
MTB19$Country1<-substr(MTB19$GUO, 1, 2)
MTB19$Country2<-substr(MTB19$Subsidiaries,1,2)

#Variable of number of countries
MTB19$Countries2<-ifelse(MTB19$Country1!=MTB19$Country2 & MTB19$Country2!=lag(MTB19$Country2),1,0)
MTB19$Countries1<-ifelse(MTB19$GUO==MTB19$Subsidiaries | MTB19$Countries2==1,1,0)
MTB19$Countries1<-as.numeric(MTB19$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MTB19<-subset(MTB19,!is.na(MTB19$MTB8)&MTB19$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MTB19$MTB7<-ave(MTB19$Countries1,MTB19[,c('GUO', 'MTB8')],FUN=sum)

#Number of unique technology contexts
MTB19$MTB6<-ave(MTB19$MTB8,MTB19[,c('GUO')],FUN=function(x) length(unique(x)))

MTB19$MTB5<-MTB19$MTB7/(MTB19$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MTB19$MTB5[is.na(MTB19$MTB5)] <- 0
MTB19$MTB5<-as.numeric(MTB19$MTB5)

#Remove duplicates to sum unique values
MTB19$Test<-paste(MTB19$GUO,MTB19$MTB8,MTB19$MTB5,sep=" ")
MTB19<-MTB19[!duplicated(MTB19$Test),]

MTB19$MTB4<-ave(MTB19$MTB5,MTB19$GUO,FUN=sum)
MTB19$MTB3<-MTB19$MTB4^2

MTB19$MTB<-MTB19$MTB3*(MTB19$MTB6/(MTB19$MTB6+1))

MTB19[,2:62]<-NULL
names(MTB19)<-c("BvDID","MTB")

MTB19<-MTB19[!duplicated(MTB19$BvDID),]


##Technological depth

##Sum of number of subsidiaries divided by countries in tech class multiplied with relevance for AI

#2011
MTD11<-left_join(Full1,Patents2011,by="Subsidiaries",na_matches="never")

MTD11$MTD8<-case_when(MTD11$TR>79.9~5,
                      MTD11$TR<=79.9&MTD11$TR>59.9~4,
                      MTD11$TR<=59.9&MTD11$TR>39.9~3,
                      MTD11$TR<=39.9&MTD11$TR>11.9~2,
                      MTD11$TR<=11.9~1)

#Create variable for country of Subsidiaries
MTD11$MTD7<-1

MTD11$MTD6<-ave(MTD11$MTD7,MTD11[,c('GUO','MTD8')],FUN=sum) #if ave(MTD11$MTD5,MTD11[,c('GUO','MTD8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MTD11$MTD5<-ave(MTD11$MTD7,MTD11[,c('GUO')],FUN=sum)

MTD11$Country1<-as.character(MTD11$GUO)
MTD11$Country2<-as.character(MTD11$Subsidiaries)
MTD11$Country1<-substr(MTD11$GUO, 1, 2)
MTD11$Country2<-substr(MTD11$Subsidiaries,1,2)

MTD11$MTD4<-ave(MTD11$Country2,MTD11[,c('GUO')],FUN=function(x) length(unique(x)))
MTD11$MTD4<-as.numeric(MTD11$MTD4)
MTD11$MTD3<-ave(MTD11$Country2,MTD11[,c('MTD8','GUO')],FUN=function(x) length(unique(x)))
MTD11$MTD3<-as.numeric(MTD11$MTD3)
MTD11$MTD3<-(ifelse(is.na(MTD11$MTD3),0,MTD11$MTD3))

MTD11$MTD2<-(MTD11$MTD6/(MTD11$MTD5+1))
MTD11$MTD1<-1/(MTD11$MTD4/MTD11$MTD3)

#Remove duplicates to sum unique values
MTD11$Test<-paste(MTD11$GUO,MTD11$MTD8,sep=" ")
MTD11<-MTD11[!duplicated(MTD11$Test),]

MTD11<-subset(MTD11,!is.na(MTD11$MTD8))

MTD11$MTDX<-MTD11$MTD2*MTD11$MTD1
MTD11$MTDX<-MTD11$MTDX^2

#Sum number of subsdiaries in technology classs
MTD11$MTDY<-case_when(MTD11$TR>79.9~0.9,
                      MTD11$TR<=79.9&MTD11$TR>59.9~0.7,
                      MTD11$TR<=59.9&MTD11$TR>39.9~0.5,
                      MTD11$TR<=39.9&MTD11$TR>11.9~0.3,
                      MTD11$TR<=11.9~0.1)

MTD11$MTDZ<-MTD11$MTDX*MTD11$MTDY

MTD11$MTD<-ave(MTD11$MTDZ,MTD11[,c('GUO')],FUN=sum)

MTD11[,2:65]<-NULL
names(MTD11)<-c("BvDID","MTD")

MTD11<-MTD11[!duplicated(MTD11$BvDID),]


#2012
MTD12<-left_join(Full2,Patents2012,by="Subsidiaries",na_matches="never")

MTD12$MTD8<-case_when(MTD12$TR>79.9~5,
                      MTD12$TR<=79.9&MTD12$TR>59.9~4,
                      MTD12$TR<=59.9&MTD12$TR>39.9~3,
                      MTD12$TR<=39.9&MTD12$TR>12.9~2,
                      MTD12$TR<=12.9~1)

#Create variable for country of Subsidiaries
MTD12$MTD7<-1

MTD12$MTD6<-ave(MTD12$MTD7,MTD12[,c('GUO','MTD8')],FUN=sum) #if ave(MTD12$MTD5,MTD12[,c('GUO','MTD8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MTD12$MTD5<-ave(MTD12$MTD7,MTD12[,c('GUO')],FUN=sum)

MTD12$Country1<-as.character(MTD12$GUO)
MTD12$Country2<-as.character(MTD12$Subsidiaries)
MTD12$Country1<-substr(MTD12$GUO, 1, 2)
MTD12$Country2<-substr(MTD12$Subsidiaries,1,2)

MTD12$MTD4<-ave(MTD12$Country2,MTD12[,c('GUO')],FUN=function(x) length(unique(x)))
MTD12$MTD4<-as.numeric(MTD12$MTD4)
MTD12$MTD3<-ave(MTD12$Country2,MTD12[,c('MTD8','GUO')],FUN=function(x) length(unique(x)))
MTD12$MTD3<-as.numeric(MTD12$MTD3)
MTD12$MTD3<-(ifelse(is.na(MTD12$MTD3),0,MTD12$MTD3))

MTD12$MTD2<-(MTD12$MTD6/(MTD12$MTD5+1))
MTD12$MTD1<-1/(MTD12$MTD4/MTD12$MTD3)

#Remove duplicates to sum unique values
MTD12$Test<-paste(MTD12$GUO,MTD12$MTD8,sep=" ")
MTD12<-MTD12[!duplicated(MTD12$Test),]

MTD12<-subset(MTD12,!is.na(MTD12$MTD8))

MTD12$MTDX<-MTD12$MTD2*MTD12$MTD1
MTD12$MTDX<-MTD12$MTDX^2

#Sum number of subsdiaries in technology classs
MTD12$MTDY<-case_when(MTD12$TR>79.9~0.9,
                      MTD12$TR<=79.9&MTD12$TR>59.9~0.7,
                      MTD12$TR<=59.9&MTD12$TR>39.9~0.5,
                      MTD12$TR<=39.9&MTD12$TR>12.9~0.3,
                      MTD12$TR<=12.9~0.1)

MTD12$MTDZ<-MTD12$MTDX*MTD12$MTDY

MTD12$MTD<-ave(MTD12$MTDZ,MTD12[,c('GUO')],FUN=sum)

MTD12[,2:65]<-NULL
names(MTD12)<-c("BvDID","MTD")

MTD12<-MTD12[!duplicated(MTD12$BvDID),]


#2013
MTD13<-left_join(Full3,Patents2013,by="Subsidiaries",na_matches="never")

MTD13$MTD8<-case_when(MTD13$TR>79.9~5,
                      MTD13$TR<=79.9&MTD13$TR>59.9~4,
                      MTD13$TR<=59.9&MTD13$TR>39.9~3,
                      MTD13$TR<=39.9&MTD13$TR>13.9~2,
                      MTD13$TR<=13.9~1)

#Create variable for country of Subsidiaries
MTD13$MTD7<-1

MTD13$MTD6<-ave(MTD13$MTD7,MTD13[,c('GUO','MTD8')],FUN=sum) #if ave(MTD13$MTD5,MTD13[,c('GUO','MTD8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MTD13$MTD5<-ave(MTD13$MTD7,MTD13[,c('GUO')],FUN=sum)

MTD13$Country1<-as.character(MTD13$GUO)
MTD13$Country2<-as.character(MTD13$Subsidiaries)
MTD13$Country1<-substr(MTD13$GUO, 1, 2)
MTD13$Country2<-substr(MTD13$Subsidiaries,1,2)

MTD13$MTD4<-ave(MTD13$Country2,MTD13[,c('GUO')],FUN=function(x) length(unique(x)))
MTD13$MTD4<-as.numeric(MTD13$MTD4)
MTD13$MTD3<-ave(MTD13$Country2,MTD13[,c('MTD8','GUO')],FUN=function(x) length(unique(x)))
MTD13$MTD3<-as.numeric(MTD13$MTD3)
MTD13$MTD3<-(ifelse(is.na(MTD13$MTD3),0,MTD13$MTD3))

MTD13$MTD2<-(MTD13$MTD6/(MTD13$MTD5+1))
MTD13$MTD1<-1/(MTD13$MTD4/MTD13$MTD3)

#Remove duplicates to sum unique values
MTD13$Test<-paste(MTD13$GUO,MTD13$MTD8,sep=" ")
MTD13<-MTD13[!duplicated(MTD13$Test),]

MTD13<-subset(MTD13,!is.na(MTD13$MTD8))

MTD13$MTDX<-MTD13$MTD2*MTD13$MTD1
MTD13$MTDX<-MTD13$MTDX^2

#Sum number of subsdiaries in technology classs
MTD13$MTDY<-case_when(MTD13$TR>79.9~0.9,
                      MTD13$TR<=79.9&MTD13$TR>59.9~0.7,
                      MTD13$TR<=59.9&MTD13$TR>39.9~0.5,
                      MTD13$TR<=39.9&MTD13$TR>13.9~0.3,
                      MTD13$TR<=13.9~0.1)

MTD13$MTDZ<-MTD13$MTDX*MTD13$MTDY

MTD13$MTD<-ave(MTD13$MTDZ,MTD13[,c('GUO')],FUN=sum)

MTD13[,2:65]<-NULL
names(MTD13)<-c("BvDID","MTD")

MTD13<-MTD13[!duplicated(MTD13$BvDID),]


#2014
MTD14<-left_join(Full4,Patents2014,by="Subsidiaries",na_matches="never")

MTD14$MTD8<-case_when(MTD14$TR>79.9~5,
                      MTD14$TR<=79.9&MTD14$TR>59.9~4,
                      MTD14$TR<=59.9&MTD14$TR>39.9~3,
                      MTD14$TR<=39.9&MTD14$TR>14.9~2,
                      MTD14$TR<=14.9~1)

#Create variable for country of Subsidiaries
MTD14$MTD7<-1

MTD14$MTD6<-ave(MTD14$MTD7,MTD14[,c('GUO','MTD8')],FUN=sum) #if ave(MTD14$MTD5,MTD14[,c('GUO','MTD8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MTD14$MTD5<-ave(MTD14$MTD7,MTD14[,c('GUO')],FUN=sum)

MTD14$Country1<-as.character(MTD14$GUO)
MTD14$Country2<-as.character(MTD14$Subsidiaries)
MTD14$Country1<-substr(MTD14$GUO, 1, 2)
MTD14$Country2<-substr(MTD14$Subsidiaries,1,2)

MTD14$MTD4<-ave(MTD14$Country2,MTD14[,c('GUO')],FUN=function(x) length(unique(x)))
MTD14$MTD4<-as.numeric(MTD14$MTD4)
MTD14$MTD3<-ave(MTD14$Country2,MTD14[,c('MTD8','GUO')],FUN=function(x) length(unique(x)))
MTD14$MTD3<-as.numeric(MTD14$MTD3)
MTD14$MTD3<-(ifelse(is.na(MTD14$MTD3),0,MTD14$MTD3))

MTD14$MTD2<-(MTD14$MTD6/(MTD14$MTD5+1))
MTD14$MTD1<-1/(MTD14$MTD4/MTD14$MTD3)

#Remove duplicates to sum unique values
MTD14$Test<-paste(MTD14$GUO,MTD14$MTD8,sep=" ")
MTD14<-MTD14[!duplicated(MTD14$Test),]

MTD14<-subset(MTD14,!is.na(MTD14$MTD8))

MTD14$MTDX<-MTD14$MTD2*MTD14$MTD1
MTD14$MTDX<-MTD14$MTDX^2

#Sum number of subsdiaries in technology classs
MTD14$MTDY<-case_when(MTD14$TR>79.9~0.9,
                      MTD14$TR<=79.9&MTD14$TR>59.9~0.7,
                      MTD14$TR<=59.9&MTD14$TR>39.9~0.5,
                      MTD14$TR<=39.9&MTD14$TR>14.9~0.3,
                      MTD14$TR<=14.9~0.1)

MTD14$MTDZ<-MTD14$MTDX*MTD14$MTDY

MTD14$MTD<-ave(MTD14$MTDZ,MTD14[,c('GUO')],FUN=sum)

MTD14[,2:65]<-NULL
names(MTD14)<-c("BvDID","MTD")

MTD14<-MTD14[!duplicated(MTD14$BvDID),]


#2015
MTD15<-left_join(Full5,Patents2015,by="Subsidiaries",na_matches="never")

MTD15$MTD8<-case_when(MTD15$TR>79.9~5,
                      MTD15$TR<=79.9&MTD15$TR>59.9~4,
                      MTD15$TR<=59.9&MTD15$TR>39.9~3,
                      MTD15$TR<=39.9&MTD15$TR>15.9~2,
                      MTD15$TR<=15.9~1)

#Create variable for country of Subsidiaries
MTD15$MTD7<-1

MTD15$MTD6<-ave(MTD15$MTD7,MTD15[,c('GUO','MTD8')],FUN=sum) #if ave(MTD15$MTD5,MTD15[,c('GUO','MTD8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MTD15$MTD5<-ave(MTD15$MTD7,MTD15[,c('GUO')],FUN=sum)

MTD15$Country1<-as.character(MTD15$GUO)
MTD15$Country2<-as.character(MTD15$Subsidiaries)
MTD15$Country1<-substr(MTD15$GUO, 1, 2)
MTD15$Country2<-substr(MTD15$Subsidiaries,1,2)

MTD15$MTD4<-ave(MTD15$Country2,MTD15[,c('GUO')],FUN=function(x) length(unique(x)))
MTD15$MTD4<-as.numeric(MTD15$MTD4)
MTD15$MTD3<-ave(MTD15$Country2,MTD15[,c('MTD8','GUO')],FUN=function(x) length(unique(x)))
MTD15$MTD3<-as.numeric(MTD15$MTD3)
MTD15$MTD3<-(ifelse(is.na(MTD15$MTD3),0,MTD15$MTD3))

MTD15$MTD2<-(MTD15$MTD6/(MTD15$MTD5+1))
MTD15$MTD1<-1/(MTD15$MTD4/MTD15$MTD3)

#Remove duplicates to sum unique values
MTD15$Test<-paste(MTD15$GUO,MTD15$MTD8,sep=" ")
MTD15<-MTD15[!duplicated(MTD15$Test),]

MTD15<-subset(MTD15,!is.na(MTD15$MTD8))

MTD15$MTDX<-MTD15$MTD2*MTD15$MTD1
MTD15$MTDX<-MTD15$MTDX^2

#Sum number of subsdiaries in technology classs
MTD15$MTDY<-case_when(MTD15$TR>79.9~0.9,
                      MTD15$TR<=79.9&MTD15$TR>59.9~0.7,
                      MTD15$TR<=59.9&MTD15$TR>39.9~0.5,
                      MTD15$TR<=39.9&MTD15$TR>15.9~0.3,
                      MTD15$TR<=15.9~0.1)

MTD15$MTDZ<-MTD15$MTDX*MTD15$MTDY

MTD15$MTD<-ave(MTD15$MTDZ,MTD15[,c('GUO')],FUN=sum)

MTD15[,2:65]<-NULL
names(MTD15)<-c("BvDID","MTD")

MTD15<-MTD15[!duplicated(MTD15$BvDID),]


#2016
MTD16<-left_join(Full6,Patents2016,by="Subsidiaries",na_matches="never")

MTD16$MTD8<-case_when(MTD16$TR>79.9~5,
                      MTD16$TR<=79.9&MTD16$TR>59.9~4,
                      MTD16$TR<=59.9&MTD16$TR>39.9~3,
                      MTD16$TR<=39.9&MTD16$TR>16.9~2,
                      MTD16$TR<=16.9~1)

#Create variable for country of Subsidiaries
MTD16$MTD7<-1

MTD16$MTD6<-ave(MTD16$MTD7,MTD16[,c('GUO','MTD8')],FUN=sum) #if ave(MTD16$MTD5,MTD16[,c('GUO','MTD8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MTD16$MTD5<-ave(MTD16$MTD7,MTD16[,c('GUO')],FUN=sum)

MTD16$Country1<-as.character(MTD16$GUO)
MTD16$Country2<-as.character(MTD16$Subsidiaries)
MTD16$Country1<-substr(MTD16$GUO, 1, 2)
MTD16$Country2<-substr(MTD16$Subsidiaries,1,2)

MTD16$MTD4<-ave(MTD16$Country2,MTD16[,c('GUO')],FUN=function(x) length(unique(x)))
MTD16$MTD4<-as.numeric(MTD16$MTD4)
MTD16$MTD3<-ave(MTD16$Country2,MTD16[,c('MTD8','GUO')],FUN=function(x) length(unique(x)))
MTD16$MTD3<-as.numeric(MTD16$MTD3)
MTD16$MTD3<-(ifelse(is.na(MTD16$MTD3),0,MTD16$MTD3))

MTD16$MTD2<-(MTD16$MTD6/(MTD16$MTD5+1))
MTD16$MTD1<-1/(MTD16$MTD4/MTD16$MTD3)

#Remove duplicates to sum unique values
MTD16$Test<-paste(MTD16$GUO,MTD16$MTD8,sep=" ")
MTD16<-MTD16[!duplicated(MTD16$Test),]

MTD16<-subset(MTD16,!is.na(MTD16$MTD8))

MTD16$MTDX<-MTD16$MTD2*MTD16$MTD1
MTD16$MTDX<-MTD16$MTDX^2

#Sum number of subsdiaries in technology classs
MTD16$MTDY<-case_when(MTD16$TR>79.9~0.9,
                      MTD16$TR<=79.9&MTD16$TR>59.9~0.7,
                      MTD16$TR<=59.9&MTD16$TR>39.9~0.5,
                      MTD16$TR<=39.9&MTD16$TR>16.9~0.3,
                      MTD16$TR<=16.9~0.1)

MTD16$MTDZ<-MTD16$MTDX*MTD16$MTDY

MTD16$MTD<-ave(MTD16$MTDZ,MTD16[,c('GUO')],FUN=sum)

MTD16[,2:65]<-NULL
names(MTD16)<-c("BvDID","MTD")

MTD16<-MTD16[!duplicated(MTD16$BvDID),]


#2017
MTD17<-left_join(Full7,Patents2017,by="Subsidiaries",na_matches="never")

MTD17$MTD8<-case_when(MTD17$TR>79.9~5,
                      MTD17$TR<=79.9&MTD17$TR>59.9~4,
                      MTD17$TR<=59.9&MTD17$TR>39.9~3,
                      MTD17$TR<=39.9&MTD17$TR>17.9~2,
                      MTD17$TR<=17.9~1)

#Create variable for country of Subsidiaries
MTD17$MTD7<-1

MTD17$MTD6<-ave(MTD17$MTD7,MTD17[,c('GUO','MTD8')],FUN=sum) #if ave(MTD17$MTD5,MTD17[,c('GUO','MTD8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MTD17$MTD5<-ave(MTD17$MTD7,MTD17[,c('GUO')],FUN=sum)

MTD17$Country1<-as.character(MTD17$GUO)
MTD17$Country2<-as.character(MTD17$Subsidiaries)
MTD17$Country1<-substr(MTD17$GUO, 1, 2)
MTD17$Country2<-substr(MTD17$Subsidiaries,1,2)

MTD17$MTD4<-ave(MTD17$Country2,MTD17[,c('GUO')],FUN=function(x) length(unique(x)))
MTD17$MTD4<-as.numeric(MTD17$MTD4)
MTD17$MTD3<-ave(MTD17$Country2,MTD17[,c('MTD8','GUO')],FUN=function(x) length(unique(x)))
MTD17$MTD3<-as.numeric(MTD17$MTD3)
MTD17$MTD3<-(ifelse(is.na(MTD17$MTD3),0,MTD17$MTD3))

MTD17$MTD2<-(MTD17$MTD6/(MTD17$MTD5+1))
MTD17$MTD1<-1/(MTD17$MTD4/MTD17$MTD3)

#Remove duplicates to sum unique values
MTD17$Test<-paste(MTD17$GUO,MTD17$MTD8,sep=" ")
MTD17<-MTD17[!duplicated(MTD17$Test),]

MTD17<-subset(MTD17,!is.na(MTD17$MTD8))

MTD17$MTDX<-MTD17$MTD2*MTD17$MTD1
MTD17$MTDX<-MTD17$MTDX^2

#Sum number of subsdiaries in technology classs
MTD17$MTDY<-case_when(MTD17$TR>79.9~0.9,
                      MTD17$TR<=79.9&MTD17$TR>59.9~0.7,
                      MTD17$TR<=59.9&MTD17$TR>39.9~0.5,
                      MTD17$TR<=39.9&MTD17$TR>17.9~0.3,
                      MTD17$TR<=17.9~0.1)

MTD17$MTDZ<-MTD17$MTDX*MTD17$MTDY

MTD17$MTD<-ave(MTD17$MTDZ,MTD17[,c('GUO')],FUN=sum)

MTD17[,2:65]<-NULL
names(MTD17)<-c("BvDID","MTD")

MTD17<-MTD17[!duplicated(MTD17$BvDID),]


#2018
MTD18<-left_join(Full8,Patents2018,by="Subsidiaries",na_matches="never")

MTD18$MTD8<-case_when(MTD18$TR>79.9~5,
                      MTD18$TR<=79.9&MTD18$TR>59.9~4,
                      MTD18$TR<=59.9&MTD18$TR>39.9~3,
                      MTD18$TR<=39.9&MTD18$TR>18.9~2,
                      MTD18$TR<=18.9~1)

#Create variable for country of Subsidiaries
MTD18$MTD7<-1

MTD18$MTD6<-ave(MTD18$MTD7,MTD18[,c('GUO','MTD8')],FUN=sum) #if ave(MTD18$MTD5,MTD18[,c('GUO','MTD8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MTD18$MTD5<-ave(MTD18$MTD7,MTD18[,c('GUO')],FUN=sum)

MTD18$Country1<-as.character(MTD18$GUO)
MTD18$Country2<-as.character(MTD18$Subsidiaries)
MTD18$Country1<-substr(MTD18$GUO, 1, 2)
MTD18$Country2<-substr(MTD18$Subsidiaries,1,2)

MTD18$MTD4<-ave(MTD18$Country2,MTD18[,c('GUO')],FUN=function(x) length(unique(x)))
MTD18$MTD4<-as.numeric(MTD18$MTD4)
MTD18$MTD3<-ave(MTD18$Country2,MTD18[,c('MTD8','GUO')],FUN=function(x) length(unique(x)))
MTD18$MTD3<-as.numeric(MTD18$MTD3)
MTD18$MTD3<-(ifelse(is.na(MTD18$MTD3),0,MTD18$MTD3))

MTD18$MTD2<-(MTD18$MTD6/(MTD18$MTD5+1))
MTD18$MTD1<-1/(MTD18$MTD4/MTD18$MTD3)

#Remove duplicates to sum unique values
MTD18$Test<-paste(MTD18$GUO,MTD18$MTD8,sep=" ")
MTD18<-MTD18[!duplicated(MTD18$Test),]

MTD18<-subset(MTD18,!is.na(MTD18$MTD8))

MTD18$MTDX<-MTD18$MTD2*MTD18$MTD1
MTD18$MTDX<-MTD18$MTDX^2

#Sum number of subsdiaries in technology classs
MTD18$MTDY<-case_when(MTD18$TR>79.9~0.9,
                      MTD18$TR<=79.9&MTD18$TR>59.9~0.7,
                      MTD18$TR<=59.9&MTD18$TR>39.9~0.5,
                      MTD18$TR<=39.9&MTD18$TR>18.9~0.3,
                      MTD18$TR<=18.9~0.1)

MTD18$MTDZ<-MTD18$MTDX*MTD18$MTDY

MTD18$MTD<-ave(MTD18$MTDZ,MTD18[,c('GUO')],FUN=sum)

MTD18[,2:65]<-NULL
names(MTD18)<-c("BvDID","MTD")

MTD18<-MTD18[!duplicated(MTD18$BvDID),]



#2019
MTD19<-left_join(Full9,Patents2019,by="Subsidiaries",na_matches="never")

MTD19$MTD8<-case_when(MTD19$TR>79.9~5,
                      MTD19$TR<=79.9&MTD19$TR>59.9~4,
                      MTD19$TR<=59.9&MTD19$TR>39.9~3,
                      MTD19$TR<=39.9&MTD19$TR>19.9~2,
                      MTD19$TR<=19.9~1)

#Create variable for country of Subsidiaries
MTD19$MTD7<-1

MTD19$MTD6<-ave(MTD19$MTD7,MTD19[,c('GUO','MTD8')],FUN=sum) #if ave(MTD19$MTD5,MTD19[,c('GUO','MTD8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MTD19$MTD5<-ave(MTD19$MTD7,MTD19[,c('GUO')],FUN=sum)

MTD19$Country1<-as.character(MTD19$GUO)
MTD19$Country2<-as.character(MTD19$Subsidiaries)
MTD19$Country1<-substr(MTD19$GUO, 1, 2)
MTD19$Country2<-substr(MTD19$Subsidiaries,1,2)

MTD19$MTD4<-ave(MTD19$Country2,MTD19[,c('GUO')],FUN=function(x) length(unique(x)))
MTD19$MTD4<-as.numeric(MTD19$MTD4)
MTD19$MTD3<-ave(MTD19$Country2,MTD19[,c('MTD8','GUO')],FUN=function(x) length(unique(x)))
MTD19$MTD3<-as.numeric(MTD19$MTD3)
MTD19$MTD3<-(ifelse(is.na(MTD19$MTD3),0,MTD19$MTD3))

MTD19$MTD2<-(MTD19$MTD6/(MTD19$MTD5+1))
MTD19$MTD1<-1/(MTD19$MTD4/MTD19$MTD3)

#Remove duplicates to sum unique values
MTD19$Test<-paste(MTD19$GUO,MTD19$MTD8,sep=" ")
MTD19<-MTD19[!duplicated(MTD19$Test),]

MTD19<-subset(MTD19,!is.na(MTD19$MTD8))

MTD19$MTDX<-MTD19$MTD2*MTD19$MTD1
MTD19$MTDX<-MTD19$MTDX^2

#Sum number of subsdiaries in technology classs
MTD19$MTDY<-case_when(MTD19$TR>79.9~0.9,
                      MTD19$TR<=79.9&MTD19$TR>59.9~0.7,
                      MTD19$TR<=59.9&MTD19$TR>39.9~0.5,
                      MTD19$TR<=39.9&MTD19$TR>19.9~0.3,
                      MTD19$TR<=19.9~0.1)

MTD19$MTDZ<-MTD19$MTDX*MTD19$MTDY

MTD19$MTD<-ave(MTD19$MTDZ,MTD19[,c('GUO')],FUN=sum)

MTD19[,2:65]<-NULL
names(MTD19)<-c("BvDID","MTD")

MTD19<-MTD19[!duplicated(MTD19$BvDID),]


#Institutional breadth
#2011
MIB11<-left_join(Full1,Patents2011,by="Subsidiaries",na_matches="never")

MIB11$MIB8<-case_when(MIB11$IPR>79.9~5,
                      MIB11$IPR<=79.9&MIB11$IPR>59.9~4,
                      MIB11$IPR<=50.9&MIB11$IPR>39.9~3,
                      MIB11$IPR<=39.9&MIB11$IPR>19.9~2,
                      MIB11$IPR<=19.9~1)

#Create variable for country of Subsidiaries
MIB11$Country1<-as.character(MIB11$GUO)
MIB11$Country2<-as.character(MIB11$Subsidiaries)
MIB11$Country1<-substr(MIB11$GUO, 1, 2)
MIB11$Country2<-substr(MIB11$Subsidiaries,1,2)

#Variable of number of countries
MIB11$Countries2<-ifelse(MIB11$Country1!=MIB11$Country2 & MIB11$Country2!=lag(MIB11$Country2),1,0)
MIB11$Countries1<-ifelse(MIB11$GUO==MIB11$Subsidiaries | MIB11$Countries2==1,1,0)
MIB11$Countries1<-as.numeric(MIB11$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MIB11<-subset(MIB11,!is.na(MIB11$MIB8)&MIB11$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MIB11$MIB7<-ave(MIB11$Countries1,MIB11[,c('GUO', 'MIB8')],FUN=sum)

#Number of unique technology contexts
MIB11$MIB6<-ave(MIB11$MIB8,MIB11[,c('GUO')],FUN=function(x) length(unique(x)))

MIB11$MIB5<-MIB11$MIB7/(MIB11$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MIB11$MIB5[is.na(MIB11$MIB5)] <- 0
MIB11$MIB5<-as.numeric(MIB11$MIB5)

#Remove duplicates to sum unique values
MIB11$Test<-paste(MIB11$GUO,MIB11$MIB8,MIB11$MIB5,sep=" ")
MIB11<-MIB11[!duplicated(MIB11$Test),]

MIB11$MIB4<-ave(MIB11$MIB5,MIB11$GUO,FUN=sum)
MIB11$MIB3<-MIB11$MIB4^2

MIB11$MIB<-MIB11$MIB3*(MIB11$MIB6/(MIB11$MIB6+1))

MIB11[,2:62]<-NULL
names(MIB11)<-c("BvDID","MIB")

MIB11<-MIB11[!duplicated(MIB11$BvDID),]


#2012
MIB12<-left_join(Full2,Patents2012,by="Subsidiaries",na_matches="never")

MIB12$MIB8<-case_when(MIB12$IPR>79.9~5,
                      MIB12$IPR<=79.9&MIB12$IPR>59.9~4,
                      MIB12$IPR<=59.9&MIB12$IPR>39.9~3,
                      MIB12$IPR<=39.9&MIB12$IPR>19.9~2,
                      MIB12$IPR<=19.9~1)

#Create variable for country of Subsidiaries
MIB12$Country1<-as.character(MIB12$GUO)
MIB12$Country2<-as.character(MIB12$Subsidiaries)
MIB12$Country1<-substr(MIB12$GUO, 1, 2)
MIB12$Country2<-substr(MIB12$Subsidiaries,1,2)

#Variable of number of countries
MIB12$Countries2<-ifelse(MIB12$Country1!=MIB12$Country2 & MIB12$Country2!=lag(MIB12$Country2),1,0)
MIB12$Countries1<-ifelse(MIB12$GUO==MIB12$Subsidiaries | MIB12$Countries2==1,1,0)
MIB12$Countries1<-as.numeric(MIB12$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MIB12<-subset(MIB12,!is.na(MIB12$MIB8)&MIB12$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MIB12$MIB7<-ave(MIB12$Countries1,MIB12[,c('GUO', 'MIB8')],FUN=sum)

#Number of unique technology contexts
MIB12$MIB6<-ave(MIB12$MIB8,MIB12[,c('GUO')],FUN=function(x) length(unique(x)))

MIB12$MIB5<-MIB12$MIB7/(MIB12$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MIB12$MIB5[is.na(MIB12$MIB5)] <- 0
MIB12$MIB5<-as.numeric(MIB12$MIB5)

#Remove duplicates to sum unique values
MIB12$Test<-paste(MIB12$GUO,MIB12$MIB8,MIB12$MIB5,sep=" ")
MIB12<-MIB12[!duplicated(MIB12$Test),]

MIB12$MIB4<-ave(MIB12$MIB5,MIB12$GUO,FUN=sum)
MIB12$MIB3<-MIB12$MIB4^2

MIB12$MIB<-MIB12$MIB3*(MIB12$MIB6/(MIB12$MIB6+1))

MIB12[,2:62]<-NULL
names(MIB12)<-c("BvDID","MIB")

MIB12<-MIB12[!duplicated(MIB12$BvDID),]


# 2013
MIB13<-left_join(Full3,Patents2013,by="Subsidiaries",na_matches="never")

MIB13$MIB8<-case_when(MIB13$IPR>79.9~5,
                      MIB13$IPR<=79.9&MIB13$IPR>59.9~4,
                      MIB13$IPR<=59.9&MIB13$IPR>39.9~3,
                      MIB13$IPR<=39.9&MIB13$IPR>19.9~2,
                      MIB13$IPR<=19.9~1)

#Create variable for country of Subsidiaries
MIB13$Country1<-as.character(MIB13$GUO)
MIB13$Country2<-as.character(MIB13$Subsidiaries)
MIB13$Country1<-substr(MIB13$GUO, 1, 2)
MIB13$Country2<-substr(MIB13$Subsidiaries,1,2)

#Variable of number of countries
MIB13$Countries2<-ifelse(MIB13$Country1!=MIB13$Country2 & MIB13$Country2!=lag(MIB13$Country2),1,0)
MIB13$Countries1<-ifelse(MIB13$GUO==MIB13$Subsidiaries | MIB13$Countries2==1,1,0)
MIB13$Countries1<-as.numeric(MIB13$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MIB13<-subset(MIB13,!is.na(MIB13$MIB8)&MIB13$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MIB13$MIB7<-ave(MIB13$Countries1,MIB13[,c('GUO', 'MIB8')],FUN=sum)

#Number of unique technology contexts
MIB13$MIB6<-ave(MIB13$MIB8,MIB13[,c('GUO')],FUN=function(x) length(unique(x)))

MIB13$MIB5<-MIB13$MIB7/(MIB13$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MIB13$MIB5[is.na(MIB13$MIB5)] <- 0
MIB13$MIB5<-as.numeric(MIB13$MIB5)

#Remove duplicates to sum unique values
MIB13$Test<-paste(MIB13$GUO,MIB13$MIB8,MIB13$MIB5,sep=" ")
MIB13<-MIB13[!duplicated(MIB13$Test),]

MIB13$MIB4<-ave(MIB13$MIB5,MIB13$GUO,FUN=sum)
MIB13$MIB3<-MIB13$MIB4^2

MIB13$MIB<-MIB13$MIB3*(MIB13$MIB6/(MIB13$MIB6+1))

MIB13[,2:62]<-NULL
names(MIB13)<-c("BvDID","MIB")

MIB13<-MIB13[!duplicated(MIB13$BvDID),]


#2014
MIB14<-left_join(Full4,Patents2014,by="Subsidiaries",na_matches="never")

MIB14$MIB8<-case_when(MIB14$IPR>79.9~5,
                      MIB14$IPR<=79.9&MIB14$IPR>59.9~4,
                      MIB14$IPR<=59.9&MIB14$IPR>39.9~3,
                      MIB14$IPR<=39.9&MIB14$IPR>19.9~2,
                      MIB14$IPR<=19.9~1)

#Create variable for country of Subsidiaries
MIB14$Country1<-as.character(MIB14$GUO)
MIB14$Country2<-as.character(MIB14$Subsidiaries)
MIB14$Country1<-substr(MIB14$GUO, 1, 2)
MIB14$Country2<-substr(MIB14$Subsidiaries,1,2)

#Variable of number of countries
MIB14$Countries2<-ifelse(MIB14$Country1!=MIB14$Country2 & MIB14$Country2!=lag(MIB14$Country2),1,0)
MIB14$Countries1<-ifelse(MIB14$GUO==MIB14$Subsidiaries | MIB14$Countries2==1,1,0)
MIB14$Countries1<-as.numeric(MIB14$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MIB14<-subset(MIB14,!is.na(MIB14$MIB8)&MIB14$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MIB14$MIB7<-ave(MIB14$Countries1,MIB14[,c('GUO', 'MIB8')],FUN=sum)

#Number of unique technology contexts
MIB14$MIB6<-ave(MIB14$MIB8,MIB14[,c('GUO')],FUN=function(x) length(unique(x)))

MIB14$MIB5<-MIB14$MIB7/(MIB14$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MIB14$MIB5[is.na(MIB14$MIB5)] <- 0
MIB14$MIB5<-as.numeric(MIB14$MIB5)

#Remove duplicates to sum unique values
MIB14$Test<-paste(MIB14$GUO,MIB14$MIB8,MIB14$MIB5,sep=" ")
MIB14<-MIB14[!duplicated(MIB14$Test),]

MIB14$MIB4<-ave(MIB14$MIB5,MIB14$GUO,FUN=sum)
MIB14$MIB3<-MIB14$MIB4^2

MIB14$MIB<-MIB14$MIB3*(MIB14$MIB6/(MIB14$MIB6+1))

MIB14[,2:62]<-NULL
names(MIB14)<-c("BvDID","MIB")

MIB14<-MIB14[!duplicated(MIB14$BvDID),]


#2015
MIB15<-left_join(Full5,Patents2015,by="Subsidiaries",na_matches="never")

MIB15$MIB8<-case_when(MIB15$IPR>79.9~5,
                      MIB15$IPR<=79.9&MIB15$IPR>59.9~4,
                      MIB15$IPR<=59.9&MIB15$IPR>39.9~3,
                      MIB15$IPR<=39.9&MIB15$IPR>19.9~2,
                      MIB15$IPR<=19.9~1)

#Create variable for country of Subsidiaries
MIB15$Country1<-as.character(MIB15$GUO)
MIB15$Country2<-as.character(MIB15$Subsidiaries)
MIB15$Country1<-substr(MIB15$GUO, 1, 2)
MIB15$Country2<-substr(MIB15$Subsidiaries,1,2)

#Variable of number of countries
MIB15$Countries2<-ifelse(MIB15$Country1!=MIB15$Country2 & MIB15$Country2!=lag(MIB15$Country2),1,0)
MIB15$Countries1<-ifelse(MIB15$GUO==MIB15$Subsidiaries | MIB15$Countries2==1,1,0)
MIB15$Countries1<-as.numeric(MIB15$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MIB15<-subset(MIB15,!is.na(MIB15$MIB8)&MIB15$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MIB15$MIB7<-ave(MIB15$Countries1,MIB15[,c('GUO', 'MIB8')],FUN=sum)

#Number of unique technology contexts
MIB15$MIB6<-ave(MIB15$MIB8,MIB15[,c('GUO')],FUN=function(x) length(unique(x)))

MIB15$MIB5<-MIB15$MIB7/(MIB15$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MIB15$MIB5[is.na(MIB15$MIB5)] <- 0
MIB15$MIB5<-as.numeric(MIB15$MIB5)

#Remove duplicates to sum unique values
MIB15$Test<-paste(MIB15$GUO,MIB15$MIB8,MIB15$MIB5,sep=" ")
MIB15<-MIB15[!duplicated(MIB15$Test),]

MIB15$MIB4<-ave(MIB15$MIB5,MIB15$GUO,FUN=sum)
MIB15$MIB3<-MIB15$MIB4^2

MIB15$MIB<-MIB15$MIB3*(MIB15$MIB6/(MIB15$MIB6+1))

MIB15[,2:62]<-NULL
names(MIB15)<-c("BvDID","MIB")

MIB15<-MIB15[!duplicated(MIB15$BvDID),]


#2016
MIB16<-left_join(Full6,Patents2016,by="Subsidiaries",na_matches="never")

MIB16$MIB8<-case_when(MIB16$IPR>79.9~5,
                      MIB16$IPR<=79.9&MIB16$IPR>59.9~4,
                      MIB16$IPR<=59.9&MIB16$IPR>39.9~3,
                      MIB16$IPR<=39.9&MIB16$IPR>19.9~2,
                      MIB16$IPR<=19.9~1)

#Create variable for country of Subsidiaries
MIB16$Country1<-as.character(MIB16$GUO)
MIB16$Country2<-as.character(MIB16$Subsidiaries)
MIB16$Country1<-substr(MIB16$GUO, 1, 2)
MIB16$Country2<-substr(MIB16$Subsidiaries,1,2)

#Variable of number of countries
MIB16$Countries2<-ifelse(MIB16$Country1!=MIB16$Country2 & MIB16$Country2!=lag(MIB16$Country2),1,0)
MIB16$Countries1<-ifelse(MIB16$GUO==MIB16$Subsidiaries | MIB16$Countries2==1,1,0)
MIB16$Countries1<-as.numeric(MIB16$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MIB16<-subset(MIB16,!is.na(MIB16$MIB8)&MIB16$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MIB16$MIB7<-ave(MIB16$Countries1,MIB16[,c('GUO', 'MIB8')],FUN=sum)

#Number of unique technology contexts
MIB16$MIB6<-ave(MIB16$MIB8,MIB16[,c('GUO')],FUN=function(x) length(unique(x)))

MIB16$MIB5<-MIB16$MIB7/(MIB16$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MIB16$MIB5[is.na(MIB16$MIB5)] <- 0
MIB16$MIB5<-as.numeric(MIB16$MIB5)

#Remove duplicates to sum unique values
MIB16$Test<-paste(MIB16$GUO,MIB16$MIB8,MIB16$MIB5,sep=" ")
MIB16<-MIB16[!duplicated(MIB16$Test),]

MIB16$MIB4<-ave(MIB16$MIB5,MIB16$GUO,FUN=sum)
MIB16$MIB3<-MIB16$MIB4^2

MIB16$MIB<-MIB16$MIB3*(MIB16$MIB6/(MIB16$MIB6+1))

MIB16[,2:62]<-NULL
names(MIB16)<-c("BvDID","MIB")

MIB16<-MIB16[!duplicated(MIB16$BvDID),]


#2017
MIB17<-left_join(Full7,Patents2017,by="Subsidiaries",na_matches="never")

MIB17$MIB8<-case_when(MIB17$IPR>79.9~5,
                      MIB17$IPR<=79.9&MIB17$IPR>59.9~4,
                      MIB17$IPR<=59.9&MIB17$IPR>39.9~3,
                      MIB17$IPR<=39.9&MIB17$IPR>19.9~2,
                      MIB17$IPR<=19.9~1)

#Create variable for country of Subsidiaries
MIB17$Country1<-as.character(MIB17$GUO)
MIB17$Country2<-as.character(MIB17$Subsidiaries)
MIB17$Country1<-substr(MIB17$GUO, 1, 2)
MIB17$Country2<-substr(MIB17$Subsidiaries,1,2)

#Variable of number of countries
MIB17$Countries2<-ifelse(MIB17$Country1!=MIB17$Country2 & MIB17$Country2!=lag(MIB17$Country2),1,0)
MIB17$Countries1<-ifelse(MIB17$GUO==MIB17$Subsidiaries | MIB17$Countries2==1,1,0)
MIB17$Countries1<-as.numeric(MIB17$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MIB17<-subset(MIB17,!is.na(MIB17$MIB8)&MIB17$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MIB17$MIB7<-ave(MIB17$Countries1,MIB17[,c('GUO', 'MIB8')],FUN=sum)

#Number of unique technology contexts
MIB17$MIB6<-ave(MIB17$MIB8,MIB17[,c('GUO')],FUN=function(x) length(unique(x)))

MIB17$MIB5<-MIB17$MIB7/(MIB17$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MIB17$MIB5[is.na(MIB17$MIB5)] <- 0
MIB17$MIB5<-as.numeric(MIB17$MIB5)

#Remove duplicates to sum unique values
MIB17$Test<-paste(MIB17$GUO,MIB17$MIB8,MIB17$MIB5,sep=" ")
MIB17<-MIB17[!duplicated(MIB17$Test),]

MIB17$MIB4<-ave(MIB17$MIB5,MIB17$GUO,FUN=sum)
MIB17$MIB3<-MIB17$MIB4^2

MIB17$MIB<-MIB17$MIB3*(MIB17$MIB6/(MIB17$MIB6+1))

MIB17[,2:62]<-NULL
names(MIB17)<-c("BvDID","MIB")

MIB17<-MIB17[!duplicated(MIB17$BvDID),]


#2018
MIB18<-left_join(Full8,Patents2018,by="Subsidiaries",na_matches="never")

MIB18$MIB8<-case_when(MIB18$IPR>79.9~5,
                      MIB18$IPR<=79.9&MIB18$IPR>59.9~4,
                      MIB18$IPR<=59.9&MIB18$IPR>39.9~3,
                      MIB18$IPR<=39.9&MIB18$IPR>19.9~2,
                      MIB18$IPR<=19.9~1)

#Create variable for country of Subsidiaries
MIB18$Country1<-as.character(MIB18$GUO)
MIB18$Country2<-as.character(MIB18$Subsidiaries)
MIB18$Country1<-substr(MIB18$GUO, 1, 2)
MIB18$Country2<-substr(MIB18$Subsidiaries,1,2)

#Variable of number of countries
MIB18$Countries2<-ifelse(MIB18$Country1!=MIB18$Country2 & MIB18$Country2!=lag(MIB18$Country2),1,0)
MIB18$Countries1<-ifelse(MIB18$GUO==MIB18$Subsidiaries | MIB18$Countries2==1,1,0)
MIB18$Countries1<-as.numeric(MIB18$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MIB18<-subset(MIB18,!is.na(MIB18$MIB8)&MIB18$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MIB18$MIB7<-ave(MIB18$Countries1,MIB18[,c('GUO', 'MIB8')],FUN=sum)

#Number of unique technology contexts
MIB18$MIB6<-ave(MIB18$MIB8,MIB18[,c('GUO')],FUN=function(x) length(unique(x)))

MIB18$MIB5<-MIB18$MIB7/(MIB18$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MIB18$MIB5[is.na(MIB18$MIB5)] <- 0
MIB18$MIB5<-as.numeric(MIB18$MIB5)

#Remove duplicates to sum unique values
MIB18$Test<-paste(MIB18$GUO,MIB18$MIB8,MIB18$MIB5,sep=" ")
MIB18<-MIB18[!duplicated(MIB18$Test),]

MIB18$MIB4<-ave(MIB18$MIB5,MIB18$GUO,FUN=sum)
MIB18$MIB3<-MIB18$MIB4^2

MIB18$MIB<-MIB18$MIB3*(MIB18$MIB6/(MIB18$MIB6+1))

MIB18[,2:62]<-NULL
names(MIB18)<-c("BvDID","MIB")

MIB18<-MIB18[!duplicated(MIB18$BvDID),]


#2019
MIB19<-left_join(Full9,Patents2019,by="Subsidiaries",na_matches="never")

MIB19$MIB8<-case_when(MIB19$IPR>79.9~5,
                      MIB19$IPR<=79.9&MIB19$IPR>59.9~4,
                      MIB19$IPR<=59.9&MIB19$IPR>39.9~3,
                      MIB19$IPR<=39.9&MIB19$IPR>19.9~2,
                      MIB19$IPR<=19.9~1)

#Create variable for country of Subsidiaries
MIB19$Country1<-as.character(MIB19$GUO)
MIB19$Country2<-as.character(MIB19$Subsidiaries)
MIB19$Country1<-substr(MIB19$GUO, 1, 2)
MIB19$Country2<-substr(MIB19$Subsidiaries,1,2)

#Variable of number of countries
MIB19$Countries2<-ifelse(MIB19$Country1!=MIB19$Country2 & MIB19$Country2!=lag(MIB19$Country2),1,0)
MIB19$Countries1<-ifelse(MIB19$GUO==MIB19$Subsidiaries | MIB19$Countries2==1,1,0)
MIB19$Countries1<-as.numeric(MIB19$Countries1)

#Remove not assigend countries due to biased estimates - no index countries are not technology-intensive anyway!
MIB19<-subset(MIB19,!is.na(MIB19$MIB8)&MIB19$Countries1!=0)

#Sum of different IPC Codes within each technical area in which the company is active 
MIB19$MIB7<-ave(MIB19$Countries1,MIB19[,c('GUO', 'MIB8')],FUN=sum)

#Number of unique technology contexts
MIB19$MIB6<-ave(MIB19$MIB8,MIB19[,c('GUO')],FUN=function(x) length(unique(x)))

MIB19$MIB5<-MIB19$MIB7/(MIB19$Countries+1)

#Account for not classified countries here -- increases the final number immensely
MIB19$MIB5[is.na(MIB19$MIB5)] <- 0
MIB19$MIB5<-as.numeric(MIB19$MIB5)

#Remove duplicates to sum unique values
MIB19$Test<-paste(MIB19$GUO,MIB19$MIB8,MIB19$MIB5,sep=" ")
MIB19<-MIB19[!duplicated(MIB19$Test),]

MIB19$MIB4<-ave(MIB19$MIB5,MIB19$GUO,FUN=sum)
MIB19$MIB3<-MIB19$MIB4^2

MIB19$MIB<-MIB19$MIB3*(MIB19$MIB6/(MIB19$MIB6+1))

MIB19[,2:62]<-NULL
names(MIB19)<-c("BvDID","MIB")

MIB19<-MIB19[!duplicated(MIB19$BvDID),]


##Institutional depth

##Sum of number of subsidiaries divided by countries in tech class multiplied with relevance for AI

#2011
MID11<-left_join(Full9,Patents2011,by="Subsidiaries",na_matches="never")

MID11$MID8<-case_when(MID11$IPR>79.9~5,
                      MID11$IPR<=79.9&MID11$IPR>59.9~4,
                      MID11$IPR<=59.9&MID11$IPR>39.9~3,
                      MID11$IPR<=39.9&MID11$IPR>11.9~2,
                      MID11$IPR<=11.9~1)

#Create variable for country of Subsidiaries
MID11$MID7<-1

MID11$MID6<-ave(MID11$MID7,MID11[,c('GUO','MID8')],FUN=sum) #if ave(MID11$MID5,MID11[,c('GUO','MID8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MID11$MID5<-ave(MID11$MID7,MID11[,c('GUO')],FUN=sum)

MID11$Country1<-as.character(MID11$GUO)
MID11$Country2<-as.character(MID11$Subsidiaries)
MID11$Country1<-substr(MID11$GUO, 1, 2)
MID11$Country2<-substr(MID11$Subsidiaries,1,2)

MID11$MID4<-ave(MID11$Country2,MID11[,c('GUO')],FUN=function(x) length(unique(x)))
MID11$MID4<-as.numeric(MID11$MID4)
MID11$MID3<-ave(MID11$Country2,MID11[,c('MID8','GUO')],FUN=function(x) length(unique(x)))
MID11$MID3<-as.numeric(MID11$MID3)
MID11$MID3<-(ifelse(is.na(MID11$MID3),0,MID11$MID3))

MID11$MID2<-(MID11$MID6/(MID11$MID5+1))
MID11$MID1<-1/(MID11$MID4/MID11$MID3)

#Remove duplicates to sum unique values
MID11$Test<-paste(MID11$GUO,MID11$MID8,sep=" ")
MID11<-MID11[!duplicated(MID11$Test),]

MID11<-subset(MID11,!is.na(MID11$MID8))

MID11$MIDX<-MID11$MID2*MID11$MID1
MID11$MIDX<-MID11$MIDX^2

#Sum number of subsdiaries in technology classs
MID11$MIDY<-case_when(MID11$IPR>79.9~0.1,
                      MID11$IPR<=79.9&MID11$IPR>59.9~0.3,
                      MID11$IPR<=59.9&MID11$IPR>39.9~0.5,
                      MID11$IPR<=39.9&MID11$IPR>11.9~0.7,
                      MID11$IPR<=11.9~0.9)

MID11$MIDZ<-MID11$MIDX*MID11$MIDY

MID11$MID<-ave(MID11$MIDZ,MID11[,c('GUO')],FUN=sum)

MID11[,2:65]<-NULL
names(MID11)<-c("BvDID","MID")

MID11<-MID11[!duplicated(MID11$BvDID),]


#2012
MID12<-left_join(Full9,Patents2012,by="Subsidiaries",na_matches="never")

MID12$MID8<-case_when(MID12$IPR>79.9~5,
                      MID12$IPR<=79.9&MID12$IPR>59.9~4,
                      MID12$IPR<=59.9&MID12$IPR>39.9~3,
                      MID12$IPR<=39.9&MID12$IPR>12.9~2,
                      MID12$IPR<=12.9~1)

#Create variable for country of Subsidiaries
MID12$MID7<-1

MID12$MID6<-ave(MID12$MID7,MID12[,c('GUO','MID8')],FUN=sum) #if ave(MID12$MID5,MID12[,c('GUO','MID8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MID12$MID5<-ave(MID12$MID7,MID12[,c('GUO')],FUN=sum)

MID12$Country1<-as.character(MID12$GUO)
MID12$Country2<-as.character(MID12$Subsidiaries)
MID12$Country1<-substr(MID12$GUO, 1, 2)
MID12$Country2<-substr(MID12$Subsidiaries,1,2)

MID12$MID4<-ave(MID12$Country2,MID12[,c('GUO')],FUN=function(x) length(unique(x)))
MID12$MID4<-as.numeric(MID12$MID4)
MID12$MID3<-ave(MID12$Country2,MID12[,c('MID8','GUO')],FUN=function(x) length(unique(x)))
MID12$MID3<-as.numeric(MID12$MID3)
MID12$MID3<-(ifelse(is.na(MID12$MID3),0,MID12$MID3))

MID12$MID2<-(MID12$MID6/(MID12$MID5+1))
MID12$MID1<-1/(MID12$MID4/MID12$MID3)

#Remove duplicates to sum unique values
MID12$Test<-paste(MID12$GUO,MID12$MID8,sep=" ")
MID12<-MID12[!duplicated(MID12$Test),]

MID12<-subset(MID12,!is.na(MID12$MID8))

MID12$MIDX<-MID12$MID2*MID12$MID1
MID12$MIDX<-MID12$MIDX^2

#Sum number of subsdiaries in technology classs
MID12$MIDY<-case_when(MID12$IPR>79.9~0.1,
                      MID12$IPR<=79.9&MID12$IPR>59.9~0.3,
                      MID12$IPR<=59.9&MID12$IPR>39.9~0.5,
                      MID12$IPR<=39.9&MID12$IPR>12.9~0.7,
                      MID12$IPR<=12.9~0.9)

MID12$MIDZ<-MID12$MIDX*MID12$MIDY

MID12$MID<-ave(MID12$MIDZ,MID12[,c('GUO')],FUN=sum)

MID12[,2:65]<-NULL
names(MID12)<-c("BvDID","MID")

MID12<-MID12[!duplicated(MID12$BvDID),]


#2013
MID13<-left_join(Full9,Patents2013,by="Subsidiaries",na_matches="never")

MID13$MID8<-case_when(MID13$IPR>79.9~5,
                      MID13$IPR<=79.9&MID13$IPR>59.9~4,
                      MID13$IPR<=59.9&MID13$IPR>39.9~3,
                      MID13$IPR<=39.9&MID13$IPR>13.9~2,
                      MID13$IPR<=13.9~1)

#Create variable for country of Subsidiaries
MID13$MID7<-1

MID13$MID6<-ave(MID13$MID7,MID13[,c('GUO','MID8')],FUN=sum) #if ave(MID13$MID5,MID13[,c('GUO','MID8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MID13$MID5<-ave(MID13$MID7,MID13[,c('GUO')],FUN=sum)

MID13$Country1<-as.character(MID13$GUO)
MID13$Country2<-as.character(MID13$Subsidiaries)
MID13$Country1<-substr(MID13$GUO, 1, 2)
MID13$Country2<-substr(MID13$Subsidiaries,1,2)

MID13$MID4<-ave(MID13$Country2,MID13[,c('GUO')],FUN=function(x) length(unique(x)))
MID13$MID4<-as.numeric(MID13$MID4)
MID13$MID3<-ave(MID13$Country2,MID13[,c('MID8','GUO')],FUN=function(x) length(unique(x)))
MID13$MID3<-as.numeric(MID13$MID3)
MID13$MID3<-(ifelse(is.na(MID13$MID3),0,MID13$MID3))

MID13$MID2<-(MID13$MID6/(MID13$MID5+1))
MID13$MID1<-1/(MID13$MID4/MID13$MID3)

#Remove duplicates to sum unique values
MID13$Test<-paste(MID13$GUO,MID13$MID8,sep=" ")
MID13<-MID13[!duplicated(MID13$Test),]

MID13<-subset(MID13,!is.na(MID13$MID8))

MID13$MIDX<-MID13$MID2*MID13$MID1
MID13$MIDX<-MID13$MIDX^2

#Sum number of subsdiaries in technology classs
MID13$MIDY<-case_when(MID13$IPR>79.9~0.1,
                      MID13$IPR<=79.9&MID13$IPR>59.9~0.3,
                      MID13$IPR<=59.9&MID13$IPR>39.9~0.5,
                      MID13$IPR<=39.9&MID13$IPR>13.9~0.7,
                      MID13$IPR<=13.9~0.9)

MID13$MIDZ<-MID13$MIDX*MID13$MIDY

MID13$MID<-ave(MID13$MIDZ,MID13[,c('GUO')],FUN=sum)

MID13[,2:65]<-NULL
names(MID13)<-c("BvDID","MID")

MID13<-MID13[!duplicated(MID13$BvDID),]


#2014
MID14<-left_join(Full4,Patents2014,by="Subsidiaries",na_matches="never")

MID14$MID8<-case_when(MID14$IPR>79.9~5,
                      MID14$IPR<=79.9&MID14$IPR>59.9~4,
                      MID14$IPR<=59.9&MID14$IPR>39.9~3,
                      MID14$IPR<=39.9&MID14$IPR>14.9~2,
                      MID14$IPR<=14.9~1)

#Create variable for country of Subsidiaries
MID14$MID7<-1

MID14$MID6<-ave(MID14$MID7,MID14[,c('GUO','MID8')],FUN=sum) #if ave(MID14$MID5,MID14[,c('GUO','MID8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MID14$MID5<-ave(MID14$MID7,MID14[,c('GUO')],FUN=sum)

MID14$Country1<-as.character(MID14$GUO)
MID14$Country2<-as.character(MID14$Subsidiaries)
MID14$Country1<-substr(MID14$GUO, 1, 2)
MID14$Country2<-substr(MID14$Subsidiaries,1,2)

MID14$MID4<-ave(MID14$Country2,MID14[,c('GUO')],FUN=function(x) length(unique(x)))
MID14$MID4<-as.numeric(MID14$MID4)
MID14$MID3<-ave(MID14$Country2,MID14[,c('MID8','GUO')],FUN=function(x) length(unique(x)))
MID14$MID3<-as.numeric(MID14$MID3)
MID14$MID3<-(ifelse(is.na(MID14$MID3),0,MID14$MID3))

MID14$MID2<-(MID14$MID6/(MID14$MID5+1))
MID14$MID1<-1/(MID14$MID4/MID14$MID3)

#Remove duplicates to sum unique values
MID14$Test<-paste(MID14$GUO,MID14$MID8,sep=" ")
MID14<-MID14[!duplicated(MID14$Test),]

MID14<-subset(MID14,!is.na(MID14$MID8))

MID14$MIDX<-MID14$MID2*MID14$MID1
MID14$MIDX<-MID14$MIDX^2

#Sum number of subsdiaries in technology classs
MID14$MIDY<-case_when(MID14$IPR>79.9~0.1,
                      MID14$IPR<=79.9&MID14$IPR>59.9~0.3,
                      MID14$IPR<=59.9&MID14$IPR>39.9~0.5,
                      MID14$IPR<=39.9&MID14$IPR>14.9~0.7,
                      MID14$IPR<=14.9~0.9)

MID14$MIDZ<-MID14$MIDX*MID14$MIDY

MID14$MID<-ave(MID14$MIDZ,MID14[,c('GUO')],FUN=sum)

MID14[,2:65]<-NULL
names(MID14)<-c("BvDID","MID")

MID14<-MID14[!duplicated(MID14$BvDID),]


#2015
MID15<-left_join(Full5,Patents2015,by="Subsidiaries",na_matches="never")

MID15$MID8<-case_when(MID15$IPR>79.9~5,
                      MID15$IPR<=79.9&MID15$IPR>59.9~4,
                      MID15$IPR<=59.9&MID15$IPR>39.9~3,
                      MID15$IPR<=39.9&MID15$IPR>15.9~2,
                      MID15$IPR<=15.9~1)

#Create variable for country of Subsidiaries
MID15$MID7<-1

MID15$MID6<-ave(MID15$MID7,MID15[,c('GUO','MID8')],FUN=sum) #if ave(MID15$MID5,MID15[,c('GUO','MID8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MID15$MID5<-ave(MID15$MID7,MID15[,c('GUO')],FUN=sum)

MID15$Country1<-as.character(MID15$GUO)
MID15$Country2<-as.character(MID15$Subsidiaries)
MID15$Country1<-substr(MID15$GUO, 1, 2)
MID15$Country2<-substr(MID15$Subsidiaries,1,2)

MID15$MID4<-ave(MID15$Country2,MID15[,c('GUO')],FUN=function(x) length(unique(x)))
MID15$MID4<-as.numeric(MID15$MID4)
MID15$MID3<-ave(MID15$Country2,MID15[,c('MID8','GUO')],FUN=function(x) length(unique(x)))
MID15$MID3<-as.numeric(MID15$MID3)
MID15$MID3<-(ifelse(is.na(MID15$MID3),0,MID15$MID3))

MID15$MID2<-(MID15$MID6/(MID15$MID5+1))
MID15$MID1<-1/(MID15$MID4/MID15$MID3)

#Remove duplicates to sum unique values
MID15$Test<-paste(MID15$GUO,MID15$MID8,sep=" ")
MID15<-MID15[!duplicated(MID15$Test),]

MID15<-subset(MID15,!is.na(MID15$MID8))

MID15$MIDX<-MID15$MID2*MID15$MID1
MID15$MIDX<-MID15$MIDX^2

#Sum number of subsdiaries in technology classs
MID15$MIDY<-case_when(MID15$IPR>79.9~0.1,
                      MID15$IPR<=79.9&MID15$IPR>59.9~0.3,
                      MID15$IPR<=59.9&MID15$IPR>39.9~0.5,
                      MID15$IPR<=39.9&MID15$IPR>15.9~0.7,
                      MID15$IPR<=15.9~0.9)

MID15$MIDZ<-MID15$MIDX*MID15$MIDY

MID15$MID<-ave(MID15$MIDZ,MID15[,c('GUO')],FUN=sum)

MID15[,2:65]<-NULL
names(MID15)<-c("BvDID","MID")

MID15<-MID15[!duplicated(MID15$BvDID),]


#2016
MID16<-left_join(Full6,Patents2016,by="Subsidiaries",na_matches="never")

MID16$MID8<-case_when(MID16$IPR>79.9~5,
                      MID16$IPR<=79.9&MID16$IPR>59.9~4,
                      MID16$IPR<=59.9&MID16$IPR>39.9~3,
                      MID16$IPR<=39.9&MID16$IPR>16.9~2,
                      MID16$IPR<=16.9~1)

#Create variable for country of Subsidiaries
MID16$MID7<-1

MID16$MID6<-ave(MID16$MID7,MID16[,c('GUO','MID8')],FUN=sum) #if ave(MID16$MID5,MID16[,c('GUO','MID8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MID16$MID5<-ave(MID16$MID7,MID16[,c('GUO')],FUN=sum)

MID16$Country1<-as.character(MID16$GUO)
MID16$Country2<-as.character(MID16$Subsidiaries)
MID16$Country1<-substr(MID16$GUO, 1, 2)
MID16$Country2<-substr(MID16$Subsidiaries,1,2)

MID16$MID4<-ave(MID16$Country2,MID16[,c('GUO')],FUN=function(x) length(unique(x)))
MID16$MID4<-as.numeric(MID16$MID4)
MID16$MID3<-ave(MID16$Country2,MID16[,c('MID8','GUO')],FUN=function(x) length(unique(x)))
MID16$MID3<-as.numeric(MID16$MID3)
MID16$MID3<-(ifelse(is.na(MID16$MID3),0,MID16$MID3))

MID16$MID2<-(MID16$MID6/(MID16$MID5+1))
MID16$MID1<-1/(MID16$MID4/MID16$MID3)

#Remove duplicates to sum unique values
MID16$Test<-paste(MID16$GUO,MID16$MID8,sep=" ")
MID16<-MID16[!duplicated(MID16$Test),]

MID16<-subset(MID16,!is.na(MID16$MID8))

MID16$MIDX<-MID16$MID2*MID16$MID1
MID16$MIDX<-MID16$MIDX^2

#Sum number of subsdiaries in technology classs
MID16$MIDY<-case_when(MID16$IPR>79.9~0.1,
                      MID16$IPR<=79.9&MID16$IPR>59.9~0.3,
                      MID16$IPR<=59.9&MID16$IPR>39.9~0.5,
                      MID16$IPR<=39.9&MID16$IPR>16.9~0.7,
                      MID16$IPR<=16.9~0.9)

MID16$MIDZ<-MID16$MIDX*MID16$MIDY

MID16$MID<-ave(MID16$MIDZ,MID16[,c('GUO')],FUN=sum)

MID16[,2:65]<-NULL
names(MID16)<-c("BvDID","MID")

MID16<-MID16[!duplicated(MID16$BvDID),]


#2017
MID17<-left_join(Full7,Patents2017,by="Subsidiaries",na_matches="never")

MID17$MID8<-case_when(MID17$IPR>79.9~5,
                      MID17$IPR<=79.9&MID17$IPR>59.9~4,
                      MID17$IPR<=59.9&MID17$IPR>39.9~3,
                      MID17$IPR<=39.9&MID17$IPR>17.9~2,
                      MID17$IPR<=17.9~1)

#Create variable for country of Subsidiaries
MID17$MID7<-1

MID17$MID6<-ave(MID17$MID7,MID17[,c('GUO','MID8')],FUN=sum) #if ave(MID17$MID5,MID17[,c('GUO','MID8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MID17$MID5<-ave(MID17$MID7,MID17[,c('GUO')],FUN=sum)

MID17$Country1<-as.character(MID17$GUO)
MID17$Country2<-as.character(MID17$Subsidiaries)
MID17$Country1<-substr(MID17$GUO, 1, 2)
MID17$Country2<-substr(MID17$Subsidiaries,1,2)

MID17$MID4<-ave(MID17$Country2,MID17[,c('GUO')],FUN=function(x) length(unique(x)))
MID17$MID4<-as.numeric(MID17$MID4)
MID17$MID3<-ave(MID17$Country2,MID17[,c('MID8','GUO')],FUN=function(x) length(unique(x)))
MID17$MID3<-as.numeric(MID17$MID3)
MID17$MID3<-(ifelse(is.na(MID17$MID3),0,MID17$MID3))

MID17$MID2<-(MID17$MID6/(MID17$MID5+1))
MID17$MID1<-1/(MID17$MID4/MID17$MID3)

#Remove duplicates to sum unique values
MID17$Test<-paste(MID17$GUO,MID17$MID8,sep=" ")
MID17<-MID17[!duplicated(MID17$Test),]

MID17<-subset(MID17,!is.na(MID17$MID8))

MID17$MIDX<-MID17$MID2*MID17$MID1
MID17$MIDX<-MID17$MIDX^2

#Sum number of subsdiaries in technology classs
MID17$MIDY<-case_when(MID17$IPR>79.9~0.1,
                      MID17$IPR<=79.9&MID17$IPR>59.9~0.3,
                      MID17$IPR<=59.9&MID17$IPR>39.9~0.5,
                      MID17$IPR<=39.9&MID17$IPR>17.9~0.7,
                      MID17$IPR<=17.9~0.9)

MID17$MIDZ<-MID17$MIDX*MID17$MIDY

MID17$MID<-ave(MID17$MIDZ,MID17[,c('GUO')],FUN=sum)

MID17[,2:65]<-NULL
names(MID17)<-c("BvDID","MID")

MID17<-MID17[!duplicated(MID17$BvDID),]


#2018
MID18<-left_join(Full8,Patents2018,by="Subsidiaries",na_matches="never")

MID18$MID8<-case_when(MID18$IPR>79.9~5,
                      MID18$IPR<=79.9&MID18$IPR>59.9~4,
                      MID18$IPR<=59.9&MID18$IPR>39.9~3,
                      MID18$IPR<=39.9&MID18$IPR>18.9~2,
                      MID18$IPR<=18.9~1)

#Create variable for country of Subsidiaries
MID18$MID7<-1

MID18$MID6<-ave(MID18$MID7,MID18[,c('GUO','MID8')],FUN=sum) #if ave(MID18$MID5,MID18[,c('GUO','MID8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MID18$MID5<-ave(MID18$MID7,MID18[,c('GUO')],FUN=sum)

MID18$Country1<-as.character(MID18$GUO)
MID18$Country2<-as.character(MID18$Subsidiaries)
MID18$Country1<-substr(MID18$GUO, 1, 2)
MID18$Country2<-substr(MID18$Subsidiaries,1,2)

MID18$MID4<-ave(MID18$Country2,MID18[,c('GUO')],FUN=function(x) length(unique(x)))
MID18$MID4<-as.numeric(MID18$MID4)
MID18$MID3<-ave(MID18$Country2,MID18[,c('MID8','GUO')],FUN=function(x) length(unique(x)))
MID18$MID3<-as.numeric(MID18$MID3)
MID18$MID3<-(ifelse(is.na(MID18$MID3),0,MID18$MID3))

MID18$MID2<-(MID18$MID6/(MID18$MID5+1))
MID18$MID1<-1/(MID18$MID4/MID18$MID3)

#Remove duplicates to sum unique values
MID18$Test<-paste(MID18$GUO,MID18$MID8,sep=" ")
MID18<-MID18[!duplicated(MID18$Test),]

MID18<-subset(MID18,!is.na(MID18$MID8))

MID18$MIDX<-MID18$MID2*MID18$MID1
MID18$MIDX<-MID18$MIDX^2

#Sum number of subsdiaries in technology classs
MID18$MIDY<-case_when(MID18$IPR>79.9~0.1,
                      MID18$IPR<=79.9&MID18$IPR>59.9~0.3,
                      MID18$IPR<=59.9&MID18$IPR>39.9~0.5,
                      MID18$IPR<=39.9&MID18$IPR>18.9~0.7,
                      MID18$IPR<=18.9~0.9)

MID18$MIDZ<-MID18$MIDX*MID18$MIDY

MID18$MID<-ave(MID18$MIDZ,MID18[,c('GUO')],FUN=sum)

MID18[,2:65]<-NULL
names(MID18)<-c("BvDID","MID")

MID18<-MID18[!duplicated(MID18$BvDID),]



#2019
MID19<-left_join(Full9,Patents2019,by="Subsidiaries",na_matches="never")

MID19$MID8<-case_when(MID19$TR>79.9~5,
                      MID19$TR<=79.9&MID19$TR>59.9~4,
                      MID19$TR<=59.9&MID19$TR>39.9~3,
                      MID19$TR<=39.9&MID19$TR>19.9~2,
                      MID19$TR<=19.9~1)

#Create variable for country of Subsidiaries
MID19$MID7<-1

MID19$MID6<-ave(MID19$MID7,MID19[,c('GUO','MID8')],FUN=sum) #if ave(MID19$MID5,MID19[,c('GUO','MID8')],FUN=sum) would measure subs in relation to all subs in class, now in relation to all subs
MID19$MID5<-ave(MID19$MID7,MID19[,c('GUO')],FUN=sum)

MID19$Country1<-as.character(MID19$GUO)
MID19$Country2<-as.character(MID19$Subsidiaries)
MID19$Country1<-substr(MID19$GUO, 1, 2)
MID19$Country2<-substr(MID19$Subsidiaries,1,2)

MID19$MID4<-ave(MID19$Country2,MID19[,c('GUO')],FUN=function(x) length(unique(x)))
MID19$MID4<-as.numeric(MID19$MID4)
MID19$MID3<-ave(MID19$Country2,MID19[,c('MID8','GUO')],FUN=function(x) length(unique(x)))
MID19$MID3<-as.numeric(MID19$MID3)
MID19$MID3<-(ifelse(is.na(MID19$MID3),0,MID19$MID3))

MID19$MID2<-(MID19$MID6/(MID19$MID5+1))
MID19$MID1<-1/(MID19$MID4/MID19$MID3)

#Remove duplicates to sum unique values
MID19$Test<-paste(MID19$GUO,MID19$MID8,sep=" ")
MID19<-MID19[!duplicated(MID19$Test),]

MID19<-subset(MID19,!is.na(MID19$MID8))

MID19$MIDX<-MID19$MID2*MID19$MID1
MID19$MIDX<-MID19$MIDX^2

#Sum number of subsdiaries in technology classs
MID19$MIDY<-case_when(MID19$TR>79.9~0.1,
                      MID19$TR<=79.9&MID19$TR>59.9~0.3,
                      MID19$TR<=59.9&MID19$TR>39.9~0.5,
                      MID19$TR<=39.9&MID19$TR>19.9~0.7,
                      MID19$TR<=19.9~0.9)

MID19$MIDZ<-MID19$MIDX*MID19$MIDY

MID19$MID<-ave(MID19$MIDZ,MID19[,c('GUO')],FUN=sum)

MID19[,2:65]<-NULL
names(MID19)<-c("BvDID","MID")

MID19<-MID19[!duplicated(MID19$BvDID),]


#Join the institutional and technological multinationality files
M1A<-left_join(MTB11,MTD11,by="BvDID",na_matches="never")
M1B<-left_join(M1A,MIB11,by="BvDID",na_matches="never")
M1<-left_join(M1B,MID11,by="BvDID",na_matches="never")

M2A<-left_join(MTB12,MTD12,by="BvDID",na_matches="never")
M2B<-left_join(M2A,MIB12,by="BvDID",na_matches="never")
M2<-left_join(M2B,MID12,by="BvDID",na_matches="never")

M3A<-left_join(MTB13,MTD13,by="BvDID",na_matches="never")
M3B<-left_join(M3A,MIB13,by="BvDID",na_matches="never")
M3<-left_join(M3B,MID13,by="BvDID",na_matches="never")

M4A<-left_join(MTB14,MTD14,by="BvDID",na_matches="never")
M4B<-left_join(M4A,MIB14,by="BvDID",na_matches="never")
M4<-left_join(M4B,MID14,by="BvDID",na_matches="never")

M5A<-left_join(MTB15,MTD15,by="BvDID",na_matches="never")
M5B<-left_join(M5A,MIB15,by="BvDID",na_matches="never")
M5<-left_join(M5B,MID15,by="BvDID",na_matches="never")

M6A<-left_join(MTB16,MTD16,by="BvDID",na_matches="never")
M6B<-left_join(M6A,MIB16,by="BvDID",na_matches="never")
M6<-left_join(M6B,MID16,by="BvDID",na_matches="never")

M7A<-left_join(MTB17,MTD17,by="BvDID",na_matches="never")
M7B<-left_join(M7A,MIB17,by="BvDID",na_matches="never")
M7<-left_join(M7B,MID17,by="BvDID",na_matches="never")

M8A<-left_join(MTB18,MTD18,by="BvDID",na_matches="never")
M8B<-left_join(M8A,MIB18,by="BvDID",na_matches="never")
M8<-left_join(M8B,MID18,by="BvDID",na_matches="never")

M9A<-left_join(MTB19,MTD19,by="BvDID",na_matches="never")
M9B<-left_join(M9A,MIB19,by="BvDID",na_matches="never")
M9<-left_join(M9B,MID19,by="BvDID",na_matches="never")

#Save Multinationality Files Files

#write.csv2(M1, file = "Felix/Multinationality11.csv",row.names = F)
#write.csv2(M2, file = "Felix/Multinationality12.csv",row.names = F)
#write.csv2(M3, file = "Felix/Multinationality13.csv",row.names = F)
#write.csv2(M4, file = "Felix/Multinationality14.csv",row.names = F)
#write.csv2(M5, file = "Felix/Multinationality15.csv",row.names = F)
#write.csv2(M6, file = "Felix/Multinationality16.csv",row.names = F)
#write.csv2(M7, file = "Felix/Multinationality17.csv",row.names = F)
#write.csv2(M8, file = "Felix/Multinationality18.csv",row.names = F)
#write.csv2(M9, file = "Felix/Multinationality19.csv",row.names = F)


#Remove unnecessary data
rm(Patents2011,Patents2012,Patents2013,Patents2014,Patents2015,Patents2016,Patents2017,Patents2018,Patents2019,M1A,M2A,M3A,
   M4A,M5A,M6A,M7A,M8A,M9A,M1B,M2B,M3B,M4B,M5B,M6B,M7B,M8B,M9B,MTB11,MTB12,MTB13,MTB14,MTB15,MTB16,MTB17,MTB18,MTB19,
   MTD11,MTD12,MTD13,MTD14,MTD15,MTD16,MTD17,MTD18,MTD19,MIB11,MIB12,MIB13,MIB14,MIB15,MIB16,MIB17,MIB18,MIB19,
   MID11,MID12,MID13,MID14,MID15,MID16,MID17,MID18,MID19,Full1,Full2,Full3,Full4,Full5,Full6,Full7,Full8,Full9)









#   3. Calculate Herfindahl Index of technological diversification ####

#2011
Stock2011<-fread("files_created_code2/Merged_file_WithStock_2011.csv", dec=",")
col_order<- c("BvDID","Year","Added","PRYear","Granted","IPCmain","IPCother","AI","Funct")

Stock2011<-Stock2011[,c(-2,-3,-4,-5,-6,-7,-8)]
Full1<-fread("files_created_code1/Full1.csv", dec=",")
Full1[,3:33]<-NULL
names(Full1) <- c("GUO","Subsidiaries")
names(Stock2011) <- c("Subsidiaries","IPCmain")

H1<-left_join(Full1,Stock2011,by="Subsidiaries",na_matches="never")

H1$IPCmain<-as.character(H1$IPCmain)

#Filter for technological area of invention
H1$Area<-case_when(substr(H1$IPCmain,1,3)=="F21"~"A",
                   substr(H1$IPCmain,1,4)=="H01B"|substr(H1$IPCmain,1,4)=="H01C"|substr(H1$IPCmain,1,4)=="H01F"|
                      substr(H1$IPCmain,1,4)=="H01G"|substr(H1$IPCmain,1,4)=="H01H"|substr(H1$IPCmain,1,4)=="H01J"|
                      substr(H1$IPCmain,1,4)=="H01K"|substr(H1$IPCmain,1,4)=="H01M"|substr(H1$IPCmain,1,4)=="H01R"|
                      substr(H1$IPCmain,1,4)=="H01T"|substr(H1$IPCmain,1,3)=="H02"|substr(H1$IPCmain,1,4)=="H05B"|
                      substr(H1$IPCmain,1,4)=="H05C"|substr(H1$IPCmain,1,4)=="H05F"|substr(H1$IPCmain,1,4)=="H99Z"~"A",
                   substr(H1$IPCmain,1,4)=="G09F"|substr(H1$IPCmain,1,4)=="G09G"|substr(H1$IPCmain,1,4)=="G11B"|
                      substr(H1$IPCmain,1,6)=="H04N003"|substr(H1$IPCmain,1,6)=="H04N003"|substr(H1$IPCmain,1,6)=="H04N009"|
                      substr(H1$IPCmain,1,6)==" H04N013"|substr(H1$IPCmain,1,6)=="H04N015"|substr(H1$IPCmain,1,6)=="H04N017"|
                      substr(H1$IPCmain,1,4)=="H04R"|substr(H1$IPCmain,1,4)=="H04S"|substr(H1$IPCmain,1,4)=="H05K"~"B",
                   substr(H1$IPCmain,1,4)=="G08C"|substr(H1$IPCmain,1,4)=="H01P"|substr(H1$IPCmain,1,4)=="H01Q"|
                      substr(H1$IPCmain,1,4)=="H04B"|substr(H1$IPCmain,1,4)=="H04H"|substr(H1$IPCmain,1,4)==" H04J"|
                      substr(H1$IPCmain,1,4)=="H04K"|substr(H1$IPCmain,1,4)=="H04M"|substr(H1$IPCmain,1,6)=="H04N001"|
                      substr(H1$IPCmain,1,6)=="H04N007"|substr(H1$IPCmain,1,6)=="H04N011"|substr(H1$IPCmain,1,4)=="H04Q"~"C",
                   substr(H1$IPCmain,1,4)=="H04L"~"D",
                   substr(H1$IPCmain,1,3)=="H03"~"E",
                   substr(H1$IPCmain,1,3)=="G06"|substr(H1$IPCmain,1,4)=="G11C"|substr(H1$IPCmain,1,4)=="G10L"~"F",
                   substr(H1$IPCmain,1,4)=="G06Q"~"G",
                   substr(H1$IPCmain,1,4)=="H01L"~"H",
                   substr(H1$IPCmain,1,3)=="G02"|substr(H1$IPCmain,1,4)=="G03B"|substr(H1$IPCmain,1,4)=="G03C"|
                      substr(H1$IPCmain,1,6)=="G03D"|substr(H1$IPCmain,1,4)=="G03F"|substr(H1$IPCmain,1,4)=="G03G"|
                      substr(H1$IPCmain,1,4)=="G03H"|substr(H1$IPCmain,1,4)=="H01S"~"I",
                   substr(H1$IPCmain,1,4)=="G01B"|substr(H1$IPCmain,1,4)=="G01C"|substr(H1$IPCmain,1,4)=="G01D"|
                      substr(H1$IPCmain,1,4)=="G01F"|substr(H1$IPCmain,1,4)=="G01G"|substr(H1$IPCmain,1,4)=="G01H"|
                      substr(H1$IPCmain,1,4)=="G01J"|substr(H1$IPCmain,1,4)=="G01K"|substr(H1$IPCmain,1,4)=="G01L"|
                      substr(H1$IPCmain,1,4)=="G01M"|substr(H1$IPCmain,1,4)=="G01N"|substr(H1$IPCmain,1,4)=="G01P"|
                      substr(H1$IPCmain,1,4)=="G01R"|substr(H1$IPCmain,1,4)=="G01S"|substr(H1$IPCmain,1,4)=="G01V"|
                      substr(H1$IPCmain,1,4)=="G01W"|substr(H1$IPCmain,1,3)=="G04"|substr(H1$IPCmain,1,4)=="G12B"|
                      substr(H1$IPCmain,1,4)=="G99Z"~"J",
                   substr(H1$IPCmain,1,6)=="G01N033"~"K",
                   substr(H1$IPCmain,1,4)=="G05B"|substr(H1$IPCmain,1,4)=="G05D"|substr(H1$IPCmain,1,4)=="G05F"|
                      substr(H1$IPCmain,1,3)=="G07"|substr(H1$IPCmain,1,4)=="G08B"|substr(H1$IPCmain,1,4)=="G08G"|
                      substr(H1$IPCmain,1,4)=="G09B"|substr(H1$IPCmain,1,4)=="G09C"|substr(H1$IPCmain,1,4)=="G09D"~"L",
                   substr(H1$IPCmain,1,4)=="A61B"|substr(H1$IPCmain,1,4)=="A61C"|substr(H1$IPCmain,1,4)=="A61D"|
                      substr(H1$IPCmain,1,4)=="A61F"|substr(H1$IPCmain,1,4)=="A61G"|substr(H1$IPCmain,1,4)=="A61H"|
                      substr(H1$IPCmain,1,4)=="A61J"|substr(H1$IPCmain,1,4)=="A61L"|substr(H1$IPCmain,1,4)=="A61M"|
                      substr(H1$IPCmain,1,4)=="A61N"|substr(H1$IPCmain,1,4)=="H05G"~"M",
                   substr(H1$IPCmain,1,4)=="C07B"|substr(H1$IPCmain,1,4)=="C07C"|substr(H1$IPCmain,1,4)=="C07D"|
                      substr(H1$IPCmain,1,4)=="C07F"|substr(H1$IPCmain,1,4)=="C07H"|substr(H1$IPCmain,1,4)=="C07J"|
                      substr(H1$IPCmain,1,4)=="C40B"|substr(H1$IPCmain,1,7)=="A61K008"|substr(H1$IPCmain,1,4)=="A61Q"~"N",
                   substr(H1$IPCmain,1,4)=="C07G"|substr(H1$IPCmain,1,4)=="C07K"|substr(H1$IPCmain,1,4)=="C12M"|
                      substr(H1$IPCmain,1,4)=="C12N"|substr(H1$IPCmain,1,4)=="C12P"|substr(H1$IPCmain,1,4)=="C12Q"|
                      substr(H1$IPCmain,1,4)=="C12R"|substr(H1$IPCmain,1,4)=="C12S"~"O",
                   substr(H1$IPCmain,1,4)=="A61K"~"P",
                   substr(H1$IPCmain,1,4)=="C08B"|substr(H1$IPCmain,1,4)=="C08C"|substr(H1$IPCmain,1,4)=="C08F"|
                      substr(H1$IPCmain,1,4)=="C08G"|substr(H1$IPCmain,1,4)=="C08H"|substr(H1$IPCmain,1,4)=="C08K"|
                      substr(H1$IPCmain,1,4)=="C08L"~"Q",
                   substr(H1$IPCmain,1,4)=="A01H"|substr(H1$IPCmain,1,4)=="A21D"|substr(H1$IPCmain,1,4)=="A23B"|
                      substr(H1$IPCmain,1,4)=="A23C"|substr(H1$IPCmain,1,4)=="A23D"|substr(H1$IPCmain,1,4)=="A23F"|
                      substr(H1$IPCmain,1,4)=="A23G"|substr(H1$IPCmain,1,4)=="A23J"|substr(H1$IPCmain,1,4)=="A23K"|
                      substr(H1$IPCmain,1,4)=="A23L"|substr(H1$IPCmain,1,4)=="C12C"|substr(H1$IPCmain,1,4)=="C12F"|
                      substr(H1$IPCmain,1,4)=="C12G"|substr(H1$IPCmain,1,4)=="C12H"|substr(H1$IPCmain,1,4)=="C12J"|
                      substr(H1$IPCmain,1,4)=="C13D"|substr(H1$IPCmain,1,4)=="C13F"|substr(H1$IPCmain,1,4)=="C13J"|
                      substr(H1$IPCmain,1,4)=="C13K"~"R",
                   substr(H1$IPCmain,1,4)=="A01N"|substr(H1$IPCmain,1,4)=="A01P"|substr(H1$IPCmain,1,3)=="C05"|
                      substr(H1$IPCmain,1,3)=="C06"|substr(H1$IPCmain,1,4)=="C09B"|substr(H1$IPCmain,1,4)=="C09C"|
                      substr(H1$IPCmain,1,4)=="C09F"|substr(H1$IPCmain,1,4)=="C09G"|substr(H1$IPCmain,1,4)=="C09H"|
                      substr(H1$IPCmain,1,4)=="C09K"|substr(H1$IPCmain,1,4)=="C09D"|substr(H1$IPCmain,1,4)=="C09J"|
                      substr(H1$IPCmain,1,4)=="C10B"|substr(H1$IPCmain,1,4)=="C10C"|substr(H1$IPCmain,1,4)=="C10F"|
                      substr(H1$IPCmain,1,4)=="C10G"|substr(H1$IPCmain,1,4)=="C10H"|substr(H1$IPCmain,1,4)=="C10J"|
                      substr(H1$IPCmain,1,4)=="C10K"|substr(H1$IPCmain,1,4)=="C10L"|substr(H1$IPCmain,1,4)=="C10M"|
                      substr(H1$IPCmain,1,4)=="C10N"|substr(H1$IPCmain,1,4)=="C11B"|substr(H1$IPCmain,1,4)=="C11C"|
                      substr(H1$IPCmain,1,4)=="C11D"|substr(H1$IPCmain,1,4)=="C99Z"~"S",
                   substr(H1$IPCmain,1,3)=="C01"|substr(H1$IPCmain,1,4)=="C03C"|substr(H1$IPCmain,1,3)=="C04"|
                      substr(H1$IPCmain,1,3)=="C21"|substr(H1$IPCmain,1,3)=="C22"|substr(H1$IPCmain,1,3)=="B22"~"T",
                   substr(H1$IPCmain,1,4)=="B05C"|substr(H1$IPCmain,1,4)=="B05D"|substr(H1$IPCmain,1,3)=="B32"|
                      substr(H1$IPCmain,1,3)=="C23"|substr(H1$IPCmain,1,3)=="C25"|substr(H1$IPCmain,1,3)=="C30"~"U",
                   substr(H1$IPCmain,1,3)=="B81"|substr(H1$IPCmain,1,3)=="B82"~"V",
                   substr(H1$IPCmain,1,4)=="B01B"|substr(H1$IPCmain,1,7)=="B01D000"|substr(H1$IPCmain,1,6)=="B01D01"|
                      substr(H1$IPCmain,1,6)=="B01D02"|substr(H1$IPCmain,1,6)=="B01D03"|substr(H1$IPCmain,1,7)=="B01D041"|
                      substr(H1$IPCmain,1,7)=="B01D043"|substr(H1$IPCmain,1,7)=="B01D057"|substr(H1$IPCmain,1,7)=="B01D059"|
                      substr(H1$IPCmain,1,6)=="B01D06"|substr(H1$IPCmain,1,6)=="B01D07"|substr(H1$IPCmain,1,4)=="B01F"|
                      substr(H1$IPCmain,1,4)=="B01J"|substr(H1$IPCmain,1,4)=="B01L"|substr(H1$IPCmain,1,4)=="B02C"|
                      substr(H1$IPCmain,1,3)=="B03"|substr(H1$IPCmain,1,3)=="B04"|substr(H1$IPCmain,1,4)=="B05B"|
                      substr(H1$IPCmain,1,4)=="B06B"|substr(H1$IPCmain,1,3)=="B07"|substr(H1$IPCmain,1,3)=="B08"|
                      substr(H1$IPCmain,1,4)=="D06B"|substr(H1$IPCmain,1,4)=="D06C"|substr(H1$IPCmain,1,4)=="D06L"|
                      substr(H1$IPCmain,1,4)=="F25J"|substr(H1$IPCmain,1,3)=="F26"|substr(H1$IPCmain,1,4)=="C14C"|
                      substr(H1$IPCmain,1,4)=="H05H"~"W",
                   substr(H1$IPCmain,1,4)=="A62D"|substr(H1$IPCmain,1,7)=="B01D045"|substr(H1$IPCmain,1,7)=="B01D046"|
                      substr(H1$IPCmain,1,7)=="B01D047"|substr(H1$IPCmain,1,7)=="B01D049"|substr(H1$IPCmain,1,7)=="B01D050"|
                      substr(H1$IPCmain,1,7)=="B01D051"|substr(H1$IPCmain,1,7)=="B01D052"|substr(H1$IPCmain,1,7)=="B01D053"|
                      substr(H1$IPCmain,1,3)=="B09"|substr(H1$IPCmain,1,4)=="B65F"|substr(H1$IPCmain,1,3)=="C02"|
                      substr(H1$IPCmain,1,4)=="F01N"|substr(H1$IPCmain,1,4)=="F23G"|substr(H1$IPCmain,1,4)=="F23J"|
                      substr(H1$IPCmain,1,4)=="G01T"|substr(H1$IPCmain,1,7)=="E01F008"|substr(H1$IPCmain,1,4)=="A62C"~"X",
                   substr(H1$IPCmain,1,4)=="B25J"|substr(H1$IPCmain,1,4)=="B65B"|substr(H1$IPCmain,1,4)=="B65C"|
                      substr(H1$IPCmain,1,4)=="B65D"|substr(H1$IPCmain,1,4)=="B65G"|substr(H1$IPCmain,1,4)=="B65H"|
                      substr(H1$IPCmain,1,3)=="B66"|substr(H1$IPCmain,1,3)=="B67"~"Y",
                   substr(H1$IPCmain,1,3)=="B21"|substr(H1$IPCmain,1,3)=="B23"|substr(H1$IPCmain,1,3)=="B24"|
                      substr(H1$IPCmain,1,4)=="B26D"|substr(H1$IPCmain,1,4)=="B26F"|substr(H1$IPCmain,1,3)=="B27"|
                      substr(H1$IPCmain,1,3)=="B30"|substr(H1$IPCmain,1,4)=="B25B"|substr(H1$IPCmain,1,4)=="B25C"|
                      substr(H1$IPCmain,1,4)=="B25D"|substr(H1$IPCmain,1,4)=="B25F"|substr(H1$IPCmain,1,4)=="B25G"|
                      substr(H1$IPCmain,1,4)=="B25H"|substr(H1$IPCmain,1,4)=="B26B"~"Z",
                   substr(H1$IPCmain,1,4)=="F01B"|substr(H1$IPCmain,1,4)=="F01C"|substr(H1$IPCmain,1,4)=="F01D"|
                      substr(H1$IPCmain,1,4)=="F01K"|substr(H1$IPCmain,1,4)=="F01L"|substr(H1$IPCmain,1,4)=="F01M"|
                      substr(H1$IPCmain,1,4)=="F01P"|substr(H1$IPCmain,1,3)=="F02"|substr(H1$IPCmain,1,3)=="F03"|
                      substr(H1$IPCmain,1,3)=="F04"|substr(H1$IPCmain,1,4)=="F23R"|substr(H1$IPCmain,1,3)=="G21"|
                      substr(H1$IPCmain,1,4)=="F99Z"~"AA",
                   substr(H1$IPCmain,1,4)=="A41H"|substr(H1$IPCmain,1,4)=="A43D"|substr(H1$IPCmain,1,4)=="A46D"|
                      substr(H1$IPCmain,1,4)=="C14B"|substr(H1$IPCmain,1,3)=="D01"|substr(H1$IPCmain,1,3)=="D02"|
                      substr(H1$IPCmain,1,3)=="D03"|substr(H1$IPCmain,1,4)=="D04B"|substr(H1$IPCmain,1,4)=="D04C"|
                      substr(H1$IPCmain,1,4)=="D04G"|substr(H1$IPCmain,1,4)=="D04H"|substr(H1$IPCmain,1,4)=="D06J"|
                      substr(H1$IPCmain,1,4)=="D06M"|substr(H1$IPCmain,1,4)=="D06P"|substr(H1$IPCmain,1,4)=="D06Q"|
                      substr(H1$IPCmain,1,4)=="D99Z"|substr(H1$IPCmain,1,3)=="B31"|substr(H1$IPCmain,1,3)=="D21"|
                      substr(H1$IPCmain,1,3)=="B41"~"AB",
                   substr(H1$IPCmain,1,4)=="A01B"|substr(H1$IPCmain,1,4)=="A01C"|substr(H1$IPCmain,1,4)=="A01D"|
                      substr(H1$IPCmain,1,4)=="A01F"|substr(H1$IPCmain,1,4)=="A01G"|substr(H1$IPCmain,1,4)=="A01J"|
                      substr(H1$IPCmain,1,4)=="A01K"|substr(H1$IPCmain,1,4)=="A01L"|substr(H1$IPCmain,1,4)=="A01M"|
                      substr(H1$IPCmain,1,4)=="A21B"|substr(H1$IPCmain,1,4)=="A21C"|substr(H1$IPCmain,1,3)=="A22"|
                      substr(H1$IPCmain,1,4)=="A23N"|substr(H1$IPCmain,1,4)=="A23P"|substr(H1$IPCmain,1,4)=="B02B"|
                      substr(H1$IPCmain,1,4)=="C12L"|substr(H1$IPCmain,1,4)=="C13C"|substr(H1$IPCmain,1,4)=="C13G"|
                      substr(H1$IPCmain,1,4)=="C13H"|substr(H1$IPCmain,1,3)=="B28"|substr(H1$IPCmain,1,3)=="B29"|
                      substr(H1$IPCmain,1,4)=="C03B"|substr(H1$IPCmain,1,4)=="C08J"|substr(H1$IPCmain,1,4)=="B99Z"|
                      substr(H1$IPCmain,1,3)=="F41"|substr(H1$IPCmain,1,3)=="F42"~"AC",
                   substr(H1$IPCmain,1,3)=="F22"|substr(H1$IPCmain,1,4)=="F23B"|substr(H1$IPCmain,1,4)=="F23C"|
                      substr(H1$IPCmain,1,4)=="F23D"|substr(H1$IPCmain,1,4)=="F23H"|substr(H1$IPCmain,1,4)=="F23K"|
                      substr(H1$IPCmain,1,4)=="F23L"|substr(H1$IPCmain,1,4)=="F23M"|substr(H1$IPCmain,1,4)=="F23N"|
                      substr(H1$IPCmain,1,4)=="F23Q"|substr(H1$IPCmain,1,3)=="F24"|substr(H1$IPCmain,1,4)=="F25B"|
                      substr(H1$IPCmain,1,4)=="F25C"|substr(H1$IPCmain,1,3)=="F27"|substr(H1$IPCmain,1,3)=="F28"~"AD",
                   substr(H1$IPCmain,1,3)=="F15"|substr(H1$IPCmain,1,3)=="F16"|substr(H1$IPCmain,1,3)=="F17"|
                      substr(H1$IPCmain,1,4)=="G05G"~"AE",
                   substr(H1$IPCmain,1,3)=="B60"|substr(H1$IPCmain,1,3)=="B61"|substr(H1$IPCmain,1,3)=="B62"|
                      substr(H1$IPCmain,1,4)=="B63B"|substr(H1$IPCmain,1,4)=="B63C"|substr(H1$IPCmain,1,4)=="B63G"|
                      substr(H1$IPCmain,1,4)=="B63H"|substr(H1$IPCmain,1,4)=="B63J"|substr(H1$IPCmain,1,3)=="B64"~"AF",
                   substr(H1$IPCmain,1,3)=="A47"|substr(H1$IPCmain,1,3)=="A63"~"AG",
                   substr(H1$IPCmain,1,3)=="A24"|substr(H1$IPCmain,1,4)=="A41B"|substr(H1$IPCmain,1,4)=="A41C"|
                      substr(H1$IPCmain,1,4)=="A41D"|substr(H1$IPCmain,1,4)=="A41F"|substr(H1$IPCmain,1,4)=="A41G"|
                      substr(H1$IPCmain,1,3)=="A42"|substr(H1$IPCmain,1,4)=="A43B"|substr(H1$IPCmain,1,4)=="A43C"|
                      substr(H1$IPCmain,1,3)=="A44"|substr(H1$IPCmain,1,3)=="A45"|substr(H1$IPCmain,1,4)=="A46B"|
                      substr(H1$IPCmain,1,4)=="A62B"|substr(H1$IPCmain,1,3)=="B42"|substr(H1$IPCmain,1,3)=="B43"|
                      substr(H1$IPCmain,1,4)=="D04D"|substr(H1$IPCmain,1,3)=="D07"|substr(H1$IPCmain,1,4)=="G10B"|
                      substr(H1$IPCmain,1,4)=="G10C"|substr(H1$IPCmain,1,4)=="G10D"|substr(H1$IPCmain,1,4)=="G10F"|
                      substr(H1$IPCmain,1,4)=="G10G"|substr(H1$IPCmain,1,4)=="G10H"|substr(H1$IPCmain,1,4)=="G10K"|
                      substr(H1$IPCmain,1,3)=="B44"|substr(H1$IPCmain,1,3)=="B68"|substr(H1$IPCmain,1,4)=="D06F"|
                      substr(H1$IPCmain,1,4)=="D06N"|substr(H1$IPCmain,1,4)=="F25D"|substr(H1$IPCmain,1,4)=="A99Z"~"AH",
                   substr(H1$IPCmain,1,3)=="E02"|substr(H1$IPCmain,1,4)=="E01B"|substr(H1$IPCmain,1,4)=="E01C"|
                      substr(H1$IPCmain,1,4)=="E01C"|substr(H1$IPCmain,1,7)=="E01F001"|substr(H1$IPCmain,1,7)=="E01F003"|
                      substr(H1$IPCmain,1,7)=="E01F005"|substr(H1$IPCmain,1,7)=="E01F007"|substr(H1$IPCmain,1,7)=="E01F009"|
                      substr(H1$IPCmain,1,6)=="E01F01"|substr(H1$IPCmain,1,4)=="E01H"|substr(H1$IPCmain,1,3)=="E03"|
                      substr(H1$IPCmain,1,3)=="E04"|substr(H1$IPCmain,1,3)=="E05"|substr(H1$IPCmain,1,3)=="E06"|
                      substr(H1$IPCmain,1,3)=="E21"|substr(H1$IPCmain,1,4)=="E99Z"~"AI")

#Sum of different IPC Codes by company
H1$DivTech<-ave(H1$IPCmain,H1$GUO, FUN=function(x) length(unique(x))) #now with variety of IPC codes in area, otherwise total number of patents with H1$DivTech2<-ifelse(!is.na(H1$IPCmain),1,0)
H1$DivTech1<-ifelse(is.na(H1$IPCmain),0,H1$DivTech)
H1$DivTech1<-as.numeric(H1$DivTech1)

#Sum of different IPC Codes within each technical area in which the company is active 
H1$DivTech2<-ave(H1$IPCmain,H1[,c('GUO', 'Area')],FUN=function(x) length(unique(x)))
H1$DivTech2<-as.numeric(H1$DivTech2)

#Distribution of area patents among all patents
H1$DivTech3<-H1$DivTech2/H1$DivTech1

#Account for not classified patents here -- increases the final number immensely
H1$DivTech3[is.na(H1$DivTech3)] <- 0

# Test Company: CL869632007, AT9010000310

#Remove duplicates to sum unique values
H1$Test<-paste(H1$GUO,H1$Area,H1$DivTech3,sep=" ")
H1<-H1[!duplicated(H1$Test),]

#Sum the shares, square them and substract from 1 = From concentration to diversification
H1$DivTech4<-ave(H1$DivTech3,H1$GUO,FUN=sum)
H1$DivTech5<-H1$DivTech4^2
H1$DivTech6<-1-H1$DivTech5

#Correct for the total number of patents (the measure is biased for small numbers)
H1$DivTech7<-ave(H1$DivTech1,H1$GUO,FUN=sum)
H1$DivTech8<-ifelse(H1$DivTech1==0 & H1$DivTech7!=0,NA,1)
H1<-subset(H1,!is.na(H1$DivTech8))
H1$DivTech<-H1$DivTech6*(1-1/H1$DivTech1) # different than in Rahko formular but makes more sense

H1$DivTech[H1$DivTech=="-Inf"] <-0

H1<-H1[,c(-2,-3,-4,-6,-7,-8,-9,-10,-11,-12,-13,-14)]
names(H1)<-c("BvDID","DivTech")

H1<-H1[!duplicated(H1$BvDID),]


#2012
Stock2012<-fread("files_created_code1/Stock2012.csv", dec=",")

Stock2012<-Stock2012[,c(-2,-3,-4,-5,-7,-8,-9)]
Full2<-fread("files_created_code1/Full2.csv", dec=",")
Full2[,3:33]<-NULL
names(Full2) <- c("GUO","Subsidiaries")

names(Stock2012) <- c("Subsidiaries","IPCmain")

H2<-left_join(Full2,Stock2012,by="Subsidiaries",na_matches="never")

H2$IPCmain<-as.character(H2$IPCmain)

#Filter for technological area of invention
H2$Area<-case_when(substr(H2$IPCmain,1,3)=="F21"~"A",
                   substr(H2$IPCmain,1,4)=="H01B"|substr(H2$IPCmain,1,4)=="H01C"|substr(H2$IPCmain,1,4)=="H01F"|
                      substr(H2$IPCmain,1,4)=="H01G"|substr(H2$IPCmain,1,4)=="H01H"|substr(H2$IPCmain,1,4)=="H01J"|
                      substr(H2$IPCmain,1,4)=="H01K"|substr(H2$IPCmain,1,4)=="H01M"|substr(H2$IPCmain,1,4)=="H01R"|
                      substr(H2$IPCmain,1,4)=="H01T"|substr(H2$IPCmain,1,3)=="H02"|substr(H2$IPCmain,1,4)=="H05B"|
                      substr(H2$IPCmain,1,4)=="H05C"|substr(H2$IPCmain,1,4)=="H05F"|substr(H2$IPCmain,1,4)=="H99Z"~"A",
                   substr(H2$IPCmain,1,4)=="G09F"|substr(H2$IPCmain,1,4)=="G09G"|substr(H2$IPCmain,1,4)=="G11B"|
                      substr(H2$IPCmain,1,6)=="H04N003"|substr(H2$IPCmain,1,6)=="H04N003"|substr(H2$IPCmain,1,6)=="H04N009"|
                      substr(H2$IPCmain,1,6)==" H04N013"|substr(H2$IPCmain,1,6)=="H04N015"|substr(H2$IPCmain,1,6)=="H04N017"|
                      substr(H2$IPCmain,1,4)=="H04R"|substr(H2$IPCmain,1,4)=="H04S"|substr(H2$IPCmain,1,4)=="H05K"~"B",
                   substr(H2$IPCmain,1,4)=="G08C"|substr(H2$IPCmain,1,4)=="H01P"|substr(H2$IPCmain,1,4)=="H01Q"|
                      substr(H2$IPCmain,1,4)=="H04B"|substr(H2$IPCmain,1,4)=="H04H"|substr(H2$IPCmain,1,4)==" H04J"|
                      substr(H2$IPCmain,1,4)=="H04K"|substr(H2$IPCmain,1,4)=="H04M"|substr(H2$IPCmain,1,6)=="H04N001"|
                      substr(H2$IPCmain,1,6)=="H04N007"|substr(H2$IPCmain,1,6)=="H04N011"|substr(H2$IPCmain,1,4)=="H04Q"~"C",
                   substr(H2$IPCmain,1,4)=="H04L"~"D",
                   substr(H2$IPCmain,1,3)=="H03"~"E",
                   substr(H2$IPCmain,1,3)=="G06"|substr(H2$IPCmain,1,4)=="G11C"|substr(H2$IPCmain,1,4)=="G10L"~"F",
                   substr(H2$IPCmain,1,4)=="G06Q"~"G",
                   substr(H2$IPCmain,1,4)=="H01L"~"H",
                   substr(H2$IPCmain,1,3)=="G02"|substr(H2$IPCmain,1,4)=="G03B"|substr(H2$IPCmain,1,4)=="G03C"|
                      substr(H2$IPCmain,1,6)=="G03D"|substr(H2$IPCmain,1,4)=="G03F"|substr(H2$IPCmain,1,4)=="G03G"|
                      substr(H2$IPCmain,1,4)=="G03H"|substr(H2$IPCmain,1,4)=="H01S"~"I",
                   substr(H2$IPCmain,1,4)=="G01B"|substr(H2$IPCmain,1,4)=="G01C"|substr(H2$IPCmain,1,4)=="G01D"|
                      substr(H2$IPCmain,1,4)=="G01F"|substr(H2$IPCmain,1,4)=="G01G"|substr(H2$IPCmain,1,4)=="G01H"|
                      substr(H2$IPCmain,1,4)=="G01J"|substr(H2$IPCmain,1,4)=="G01K"|substr(H2$IPCmain,1,4)=="G01L"|
                      substr(H2$IPCmain,1,4)=="G01M"|substr(H2$IPCmain,1,4)=="G01N"|substr(H2$IPCmain,1,4)=="G01P"|
                      substr(H2$IPCmain,1,4)=="G01R"|substr(H2$IPCmain,1,4)=="G01S"|substr(H2$IPCmain,1,4)=="G01V"|
                      substr(H2$IPCmain,1,4)=="G01W"|substr(H2$IPCmain,1,3)=="G04"|substr(H2$IPCmain,1,4)=="G12B"|
                      substr(H2$IPCmain,1,4)=="G99Z"~"J",
                   substr(H2$IPCmain,1,6)=="G01N033"~"K",
                   substr(H2$IPCmain,1,4)=="G05B"|substr(H2$IPCmain,1,4)=="G05D"|substr(H2$IPCmain,1,4)=="G05F"|
                      substr(H2$IPCmain,1,3)=="G07"|substr(H2$IPCmain,1,4)=="G08B"|substr(H2$IPCmain,1,4)=="G08G"|
                      substr(H2$IPCmain,1,4)=="G09B"|substr(H2$IPCmain,1,4)=="G09C"|substr(H2$IPCmain,1,4)=="G09D"~"L",
                   substr(H2$IPCmain,1,4)=="A61B"|substr(H2$IPCmain,1,4)=="A61C"|substr(H2$IPCmain,1,4)=="A61D"|
                      substr(H2$IPCmain,1,4)=="A61F"|substr(H2$IPCmain,1,4)=="A61G"|substr(H2$IPCmain,1,4)=="A61H"|
                      substr(H2$IPCmain,1,4)=="A61J"|substr(H2$IPCmain,1,4)=="A61L"|substr(H2$IPCmain,1,4)=="A61M"|
                      substr(H2$IPCmain,1,4)=="A61N"|substr(H2$IPCmain,1,4)=="H05G"~"M",
                   substr(H2$IPCmain,1,4)=="C07B"|substr(H2$IPCmain,1,4)=="C07C"|substr(H2$IPCmain,1,4)=="C07D"|
                      substr(H2$IPCmain,1,4)=="C07F"|substr(H2$IPCmain,1,4)=="C07H"|substr(H2$IPCmain,1,4)=="C07J"|
                      substr(H2$IPCmain,1,4)=="C40B"|substr(H2$IPCmain,1,7)=="A61K008"|substr(H2$IPCmain,1,4)=="A61Q"~"N",
                   substr(H2$IPCmain,1,4)=="C07G"|substr(H2$IPCmain,1,4)=="C07K"|substr(H2$IPCmain,1,4)=="C12M"|
                      substr(H2$IPCmain,1,4)=="C12N"|substr(H2$IPCmain,1,4)=="C12P"|substr(H2$IPCmain,1,4)=="C12Q"|
                      substr(H2$IPCmain,1,4)=="C12R"|substr(H2$IPCmain,1,4)=="C12S"~"O",
                   substr(H2$IPCmain,1,4)=="A61K"~"P",
                   substr(H2$IPCmain,1,4)=="C08B"|substr(H2$IPCmain,1,4)=="C08C"|substr(H2$IPCmain,1,4)=="C08F"|
                      substr(H2$IPCmain,1,4)=="C08G"|substr(H2$IPCmain,1,4)=="C08H"|substr(H2$IPCmain,1,4)=="C08K"|
                      substr(H2$IPCmain,1,4)=="C08L"~"Q",
                   substr(H2$IPCmain,1,4)=="A01H"|substr(H2$IPCmain,1,4)=="A21D"|substr(H2$IPCmain,1,4)=="A23B"|
                      substr(H2$IPCmain,1,4)=="A23C"|substr(H2$IPCmain,1,4)=="A23D"|substr(H2$IPCmain,1,4)=="A23F"|
                      substr(H2$IPCmain,1,4)=="A23G"|substr(H2$IPCmain,1,4)=="A23J"|substr(H2$IPCmain,1,4)=="A23K"|
                      substr(H2$IPCmain,1,4)=="A23L"|substr(H2$IPCmain,1,4)=="C12C"|substr(H2$IPCmain,1,4)=="C12F"|
                      substr(H2$IPCmain,1,4)=="C12G"|substr(H2$IPCmain,1,4)=="C12H"|substr(H2$IPCmain,1,4)=="C12J"|
                      substr(H2$IPCmain,1,4)=="C13D"|substr(H2$IPCmain,1,4)=="C13F"|substr(H2$IPCmain,1,4)=="C13J"|
                      substr(H2$IPCmain,1,4)=="C13K"~"R",
                   substr(H2$IPCmain,1,4)=="A01N"|substr(H2$IPCmain,1,4)=="A01P"|substr(H2$IPCmain,1,3)=="C05"|
                      substr(H2$IPCmain,1,3)=="C06"|substr(H2$IPCmain,1,4)=="C09B"|substr(H2$IPCmain,1,4)=="C09C"|
                      substr(H2$IPCmain,1,4)=="C09F"|substr(H2$IPCmain,1,4)=="C09G"|substr(H2$IPCmain,1,4)=="C09H"|
                      substr(H2$IPCmain,1,4)=="C09K"|substr(H2$IPCmain,1,4)=="C09D"|substr(H2$IPCmain,1,4)=="C09J"|
                      substr(H2$IPCmain,1,4)=="C10B"|substr(H2$IPCmain,1,4)=="C10C"|substr(H2$IPCmain,1,4)=="C10F"|
                      substr(H2$IPCmain,1,4)=="C10G"|substr(H2$IPCmain,1,4)=="C10H"|substr(H2$IPCmain,1,4)=="C10J"|
                      substr(H2$IPCmain,1,4)=="C10K"|substr(H2$IPCmain,1,4)=="C10L"|substr(H2$IPCmain,1,4)=="C10M"|
                      substr(H2$IPCmain,1,4)=="C10N"|substr(H2$IPCmain,1,4)=="C11B"|substr(H2$IPCmain,1,4)=="C11C"|
                      substr(H2$IPCmain,1,4)=="C11D"|substr(H2$IPCmain,1,4)=="C99Z"~"S",
                   substr(H2$IPCmain,1,3)=="C01"|substr(H2$IPCmain,1,4)=="C03C"|substr(H2$IPCmain,1,3)=="C04"|
                      substr(H2$IPCmain,1,3)=="C21"|substr(H2$IPCmain,1,3)=="C22"|substr(H2$IPCmain,1,3)=="B22"~"T",
                   substr(H2$IPCmain,1,4)=="B05C"|substr(H2$IPCmain,1,4)=="B05D"|substr(H2$IPCmain,1,3)=="B32"|
                      substr(H2$IPCmain,1,3)=="C23"|substr(H2$IPCmain,1,3)=="C25"|substr(H2$IPCmain,1,3)=="C30"~"U",
                   substr(H2$IPCmain,1,3)=="B81"|substr(H2$IPCmain,1,3)=="B82"~"V",
                   substr(H2$IPCmain,1,4)=="B01B"|substr(H2$IPCmain,1,7)=="B01D000"|substr(H2$IPCmain,1,6)=="B01D01"|
                      substr(H2$IPCmain,1,6)=="B01D02"|substr(H2$IPCmain,1,6)=="B01D03"|substr(H2$IPCmain,1,7)=="B01D041"|
                      substr(H2$IPCmain,1,7)=="B01D043"|substr(H2$IPCmain,1,7)=="B01D057"|substr(H2$IPCmain,1,7)=="B01D059"|
                      substr(H2$IPCmain,1,6)=="B01D06"|substr(H2$IPCmain,1,6)=="B01D07"|substr(H2$IPCmain,1,4)=="B01F"|
                      substr(H2$IPCmain,1,4)=="B01J"|substr(H2$IPCmain,1,4)=="B01L"|substr(H2$IPCmain,1,4)=="B02C"|
                      substr(H2$IPCmain,1,3)=="B03"|substr(H2$IPCmain,1,3)=="B04"|substr(H2$IPCmain,1,4)=="B05B"|
                      substr(H2$IPCmain,1,4)=="B06B"|substr(H2$IPCmain,1,3)=="B07"|substr(H2$IPCmain,1,3)=="B08"|
                      substr(H2$IPCmain,1,4)=="D06B"|substr(H2$IPCmain,1,4)=="D06C"|substr(H2$IPCmain,1,4)=="D06L"|
                      substr(H2$IPCmain,1,4)=="F25J"|substr(H2$IPCmain,1,3)=="F26"|substr(H2$IPCmain,1,4)=="C14C"|
                      substr(H2$IPCmain,1,4)=="H05H"~"W",
                   substr(H2$IPCmain,1,4)=="A62D"|substr(H2$IPCmain,1,7)=="B01D045"|substr(H2$IPCmain,1,7)=="B01D046"|
                      substr(H2$IPCmain,1,7)=="B01D047"|substr(H2$IPCmain,1,7)=="B01D049"|substr(H2$IPCmain,1,7)=="B01D050"|
                      substr(H2$IPCmain,1,7)=="B01D051"|substr(H2$IPCmain,1,7)=="B01D052"|substr(H2$IPCmain,1,7)=="B01D053"|
                      substr(H2$IPCmain,1,3)=="B09"|substr(H2$IPCmain,1,4)=="B65F"|substr(H2$IPCmain,1,3)=="C02"|
                      substr(H2$IPCmain,1,4)=="F01N"|substr(H2$IPCmain,1,4)=="F23G"|substr(H2$IPCmain,1,4)=="F23J"|
                      substr(H2$IPCmain,1,4)=="G01T"|substr(H2$IPCmain,1,7)=="E01F008"|substr(H2$IPCmain,1,4)=="A62C"~"X",
                   substr(H2$IPCmain,1,4)=="B25J"|substr(H2$IPCmain,1,4)=="B65B"|substr(H2$IPCmain,1,4)=="B65C"|
                      substr(H2$IPCmain,1,4)=="B65D"|substr(H2$IPCmain,1,4)=="B65G"|substr(H2$IPCmain,1,4)=="B65H"|
                      substr(H2$IPCmain,1,3)=="B66"|substr(H2$IPCmain,1,3)=="B67"~"Y",
                   substr(H2$IPCmain,1,3)=="B21"|substr(H2$IPCmain,1,3)=="B23"|substr(H2$IPCmain,1,3)=="B24"|
                      substr(H2$IPCmain,1,4)=="B26D"|substr(H2$IPCmain,1,4)=="B26F"|substr(H2$IPCmain,1,3)=="B27"|
                      substr(H2$IPCmain,1,3)=="B30"|substr(H2$IPCmain,1,4)=="B25B"|substr(H2$IPCmain,1,4)=="B25C"|
                      substr(H2$IPCmain,1,4)=="B25D"|substr(H2$IPCmain,1,4)=="B25F"|substr(H2$IPCmain,1,4)=="B25G"|
                      substr(H2$IPCmain,1,4)=="B25H"|substr(H2$IPCmain,1,4)=="B26B"~"Z",
                   substr(H2$IPCmain,1,4)=="F01B"|substr(H2$IPCmain,1,4)=="F01C"|substr(H2$IPCmain,1,4)=="F01D"|
                      substr(H2$IPCmain,1,4)=="F01K"|substr(H2$IPCmain,1,4)=="F01L"|substr(H2$IPCmain,1,4)=="F01M"|
                      substr(H2$IPCmain,1,4)=="F01P"|substr(H2$IPCmain,1,3)=="F02"|substr(H2$IPCmain,1,3)=="F03"|
                      substr(H2$IPCmain,1,3)=="F04"|substr(H2$IPCmain,1,4)=="F23R"|substr(H2$IPCmain,1,3)=="G21"|
                      substr(H2$IPCmain,1,4)=="F99Z"~"AA",
                   substr(H2$IPCmain,1,4)=="A41H"|substr(H2$IPCmain,1,4)=="A43D"|substr(H2$IPCmain,1,4)=="A46D"|
                      substr(H2$IPCmain,1,4)=="C14B"|substr(H2$IPCmain,1,3)=="D01"|substr(H2$IPCmain,1,3)=="D02"|
                      substr(H2$IPCmain,1,3)=="D03"|substr(H2$IPCmain,1,4)=="D04B"|substr(H2$IPCmain,1,4)=="D04C"|
                      substr(H2$IPCmain,1,4)=="D04G"|substr(H2$IPCmain,1,4)=="D04H"|substr(H2$IPCmain,1,4)=="D06J"|
                      substr(H2$IPCmain,1,4)=="D06M"|substr(H2$IPCmain,1,4)=="D06P"|substr(H2$IPCmain,1,4)=="D06Q"|
                      substr(H2$IPCmain,1,4)=="D99Z"|substr(H2$IPCmain,1,3)=="B31"|substr(H2$IPCmain,1,3)=="D21"|
                      substr(H2$IPCmain,1,3)=="B41"~"AB",
                   substr(H2$IPCmain,1,4)=="A01B"|substr(H2$IPCmain,1,4)=="A01C"|substr(H2$IPCmain,1,4)=="A01D"|
                      substr(H2$IPCmain,1,4)=="A01F"|substr(H2$IPCmain,1,4)=="A01G"|substr(H2$IPCmain,1,4)=="A01J"|
                      substr(H2$IPCmain,1,4)=="A01K"|substr(H2$IPCmain,1,4)=="A01L"|substr(H2$IPCmain,1,4)=="A01M"|
                      substr(H2$IPCmain,1,4)=="A21B"|substr(H2$IPCmain,1,4)=="A21C"|substr(H2$IPCmain,1,3)=="A22"|
                      substr(H2$IPCmain,1,4)=="A23N"|substr(H2$IPCmain,1,4)=="A23P"|substr(H2$IPCmain,1,4)=="B02B"|
                      substr(H2$IPCmain,1,4)=="C12L"|substr(H2$IPCmain,1,4)=="C13C"|substr(H2$IPCmain,1,4)=="C13G"|
                      substr(H2$IPCmain,1,4)=="C13H"|substr(H2$IPCmain,1,3)=="B28"|substr(H2$IPCmain,1,3)=="B29"|
                      substr(H2$IPCmain,1,4)=="C03B"|substr(H2$IPCmain,1,4)=="C08J"|substr(H2$IPCmain,1,4)=="B99Z"|
                      substr(H2$IPCmain,1,3)=="F41"|substr(H2$IPCmain,1,3)=="F42"~"AC",
                   substr(H2$IPCmain,1,3)=="F22"|substr(H2$IPCmain,1,4)=="F23B"|substr(H2$IPCmain,1,4)=="F23C"|
                      substr(H2$IPCmain,1,4)=="F23D"|substr(H2$IPCmain,1,4)=="F23H"|substr(H2$IPCmain,1,4)=="F23K"|
                      substr(H2$IPCmain,1,4)=="F23L"|substr(H2$IPCmain,1,4)=="F23M"|substr(H2$IPCmain,1,4)=="F23N"|
                      substr(H2$IPCmain,1,4)=="F23Q"|substr(H2$IPCmain,1,3)=="F24"|substr(H2$IPCmain,1,4)=="F25B"|
                      substr(H2$IPCmain,1,4)=="F25C"|substr(H2$IPCmain,1,3)=="F27"|substr(H2$IPCmain,1,3)=="F28"~"AD",
                   substr(H2$IPCmain,1,3)=="F15"|substr(H2$IPCmain,1,3)=="F16"|substr(H2$IPCmain,1,3)=="F17"|
                      substr(H2$IPCmain,1,4)=="G05G"~"AE",
                   substr(H2$IPCmain,1,3)=="B60"|substr(H2$IPCmain,1,3)=="B61"|substr(H2$IPCmain,1,3)=="B62"|
                      substr(H2$IPCmain,1,4)=="B63B"|substr(H2$IPCmain,1,4)=="B63C"|substr(H2$IPCmain,1,4)=="B63G"|
                      substr(H2$IPCmain,1,4)=="B63H"|substr(H2$IPCmain,1,4)=="B63J"|substr(H2$IPCmain,1,3)=="B64"~"AF",
                   substr(H2$IPCmain,1,3)=="A47"|substr(H2$IPCmain,1,3)=="A63"~"AG",
                   substr(H2$IPCmain,1,3)=="A24"|substr(H2$IPCmain,1,4)=="A41B"|substr(H2$IPCmain,1,4)=="A41C"|
                      substr(H2$IPCmain,1,4)=="A41D"|substr(H2$IPCmain,1,4)=="A41F"|substr(H2$IPCmain,1,4)=="A41G"|
                      substr(H2$IPCmain,1,3)=="A42"|substr(H2$IPCmain,1,4)=="A43B"|substr(H2$IPCmain,1,4)=="A43C"|
                      substr(H2$IPCmain,1,3)=="A44"|substr(H2$IPCmain,1,3)=="A45"|substr(H2$IPCmain,1,4)=="A46B"|
                      substr(H2$IPCmain,1,4)=="A62B"|substr(H2$IPCmain,1,3)=="B42"|substr(H2$IPCmain,1,3)=="B43"|
                      substr(H2$IPCmain,1,4)=="D04D"|substr(H2$IPCmain,1,3)=="D07"|substr(H2$IPCmain,1,4)=="G10B"|
                      substr(H2$IPCmain,1,4)=="G10C"|substr(H2$IPCmain,1,4)=="G10D"|substr(H2$IPCmain,1,4)=="G10F"|
                      substr(H2$IPCmain,1,4)=="G10G"|substr(H2$IPCmain,1,4)=="G10H"|substr(H2$IPCmain,1,4)=="G10K"|
                      substr(H2$IPCmain,1,3)=="B44"|substr(H2$IPCmain,1,3)=="B68"|substr(H2$IPCmain,1,4)=="D06F"|
                      substr(H2$IPCmain,1,4)=="D06N"|substr(H2$IPCmain,1,4)=="F25D"|substr(H2$IPCmain,1,4)=="A99Z"~"AH",
                   substr(H2$IPCmain,1,3)=="E02"|substr(H2$IPCmain,1,4)=="E01B"|substr(H2$IPCmain,1,4)=="E01C"|
                      substr(H2$IPCmain,1,4)=="E01C"|substr(H2$IPCmain,1,7)=="E01F001"|substr(H2$IPCmain,1,7)=="E01F003"|
                      substr(H2$IPCmain,1,7)=="E01F005"|substr(H2$IPCmain,1,7)=="E01F007"|substr(H2$IPCmain,1,7)=="E01F009"|
                      substr(H2$IPCmain,1,6)=="E01F01"|substr(H2$IPCmain,1,4)=="E01H"|substr(H2$IPCmain,1,3)=="E03"|
                      substr(H2$IPCmain,1,3)=="E04"|substr(H2$IPCmain,1,3)=="E05"|substr(H2$IPCmain,1,3)=="E06"|
                      substr(H2$IPCmain,1,3)=="E21"|substr(H2$IPCmain,1,4)=="E99Z"~"AI")

#Sum of different IPC Codes by company
H2$DivTech<-ave(H2$IPCmain,H2$GUO, FUN=function(x) length(unique(x))) #now with variety of IPC codes in area, otherwise total number of patents with H2$DivTech2<-ifelse(!is.na(H2$IPCmain),1,0)
H2$DivTech1<-ifelse(is.na(H2$IPCmain),0,H2$DivTech)
H2$DivTech1<-as.numeric(H2$DivTech1)

#Sum of different IPC Codes within each technical area in which the company is active 
H2$DivTech2<-ave(H2$IPCmain,H2[,c('GUO', 'Area')],FUN=function(x) length(unique(x)))
H2$DivTech2<-as.numeric(H2$DivTech2)

#Distribution of area patents among all patents
H2$DivTech3<-H2$DivTech2/H2$DivTech1

#Account for not classified patents here -- increases the final number immensely
H2$DivTech3[is.na(H2$DivTech3)] <- 0

#Remove duplicates to sum unique values
H2$Test<-paste(H2$GUO,H2$Area,H2$DivTech3,sep=" ")
H2<-H2[!duplicated(H2$Test),]

#Sum the shares, square them and substract from 1 = From concentration to diversification
H2$DivTech4<-ave(H2$DivTech3,H2$GUO,FUN=sum)
H2$DivTech5<-H2$DivTech4^2
H2$DivTech6<-1-H2$DivTech5

#Correct for the total number of patents (the measure is biased for small numbers)
H2$DivTech7<-ave(H2$DivTech1,H2$GUO,FUN=sum)
H2$DivTech8<-ifelse(H2$DivTech1==0 & H2$DivTech7!=0,NA,1)
H2<-subset(H2,!is.na(H2$DivTech8))
H2$DivTech<-H2$DivTech6*(1-1/H2$DivTech1) # different than in Rahko formular but makes more sense

H2$DivTech[H2$DivTech=="-Inf"] <-0

H2<-H2[,c(-2,-3,-4,-6,-7,-8,-9,-10,-11,-12,-13,-14)]
names(H2)<-c("BvDID","DivTech")

H2<-H2[!duplicated(H2$BvDID),]


#2013
Stock2013<-fread("files_created_code1/Stock2013.csv", dec=",")

Stock2013<-Stock2013[,c(-2,-3,-4,-5,-7,-8,-9)]
Full3<-fread("files_created_code1/Full3.csv", dec=",")
Full3[,3:33]<-NULL
names(Full3) <- c("GUO","Subsidiaries")
names(Stock2013) <- c("Subsidiaries","IPCmain")

H3<-left_join(Full3,Stock2013,by="Subsidiaries",na_matches="never")

H3$IPCmain<-as.character(H3$IPCmain)

#Filter for technological area of invention
H3$Area<-case_when(substr(H3$IPCmain,1,3)=="F21"~"A",
                   substr(H3$IPCmain,1,4)=="H01B"|substr(H3$IPCmain,1,4)=="H01C"|substr(H3$IPCmain,1,4)=="H01F"|
                      substr(H3$IPCmain,1,4)=="H01G"|substr(H3$IPCmain,1,4)=="H01H"|substr(H3$IPCmain,1,4)=="H01J"|
                      substr(H3$IPCmain,1,4)=="H01K"|substr(H3$IPCmain,1,4)=="H01M"|substr(H3$IPCmain,1,4)=="H01R"|
                      substr(H3$IPCmain,1,4)=="H01T"|substr(H3$IPCmain,1,3)=="H02"|substr(H3$IPCmain,1,4)=="H05B"|
                      substr(H3$IPCmain,1,4)=="H05C"|substr(H3$IPCmain,1,4)=="H05F"|substr(H3$IPCmain,1,4)=="H99Z"~"A",
                   substr(H3$IPCmain,1,4)=="G09F"|substr(H3$IPCmain,1,4)=="G09G"|substr(H3$IPCmain,1,4)=="G11B"|
                      substr(H3$IPCmain,1,6)=="H04N003"|substr(H3$IPCmain,1,6)=="H04N003"|substr(H3$IPCmain,1,6)=="H04N009"|
                      substr(H3$IPCmain,1,6)==" H04N013"|substr(H3$IPCmain,1,6)=="H04N015"|substr(H3$IPCmain,1,6)=="H04N017"|
                      substr(H3$IPCmain,1,4)=="H04R"|substr(H3$IPCmain,1,4)=="H04S"|substr(H3$IPCmain,1,4)=="H05K"~"B",
                   substr(H3$IPCmain,1,4)=="G08C"|substr(H3$IPCmain,1,4)=="H01P"|substr(H3$IPCmain,1,4)=="H01Q"|
                      substr(H3$IPCmain,1,4)=="H04B"|substr(H3$IPCmain,1,4)=="H04H"|substr(H3$IPCmain,1,4)==" H04J"|
                      substr(H3$IPCmain,1,4)=="H04K"|substr(H3$IPCmain,1,4)=="H04M"|substr(H3$IPCmain,1,6)=="H04N001"|
                      substr(H3$IPCmain,1,6)=="H04N007"|substr(H3$IPCmain,1,6)=="H04N011"|substr(H3$IPCmain,1,4)=="H04Q"~"C",
                   substr(H3$IPCmain,1,4)=="H04L"~"D",
                   substr(H3$IPCmain,1,3)=="H03"~"E",
                   substr(H3$IPCmain,1,3)=="G06"|substr(H3$IPCmain,1,4)=="G11C"|substr(H3$IPCmain,1,4)=="G10L"~"F",
                   substr(H3$IPCmain,1,4)=="G06Q"~"G",
                   substr(H3$IPCmain,1,4)=="H01L"~"H",
                   substr(H3$IPCmain,1,3)=="G02"|substr(H3$IPCmain,1,4)=="G03B"|substr(H3$IPCmain,1,4)=="G03C"|
                      substr(H3$IPCmain,1,6)=="G03D"|substr(H3$IPCmain,1,4)=="G03F"|substr(H3$IPCmain,1,4)=="G03G"|
                      substr(H3$IPCmain,1,4)=="G03H"|substr(H3$IPCmain,1,4)=="H01S"~"I",
                   substr(H3$IPCmain,1,4)=="G01B"|substr(H3$IPCmain,1,4)=="G01C"|substr(H3$IPCmain,1,4)=="G01D"|
                      substr(H3$IPCmain,1,4)=="G01F"|substr(H3$IPCmain,1,4)=="G01G"|substr(H3$IPCmain,1,4)=="G01H"|
                      substr(H3$IPCmain,1,4)=="G01J"|substr(H3$IPCmain,1,4)=="G01K"|substr(H3$IPCmain,1,4)=="G01L"|
                      substr(H3$IPCmain,1,4)=="G01M"|substr(H3$IPCmain,1,4)=="G01N"|substr(H3$IPCmain,1,4)=="G01P"|
                      substr(H3$IPCmain,1,4)=="G01R"|substr(H3$IPCmain,1,4)=="G01S"|substr(H3$IPCmain,1,4)=="G01V"|
                      substr(H3$IPCmain,1,4)=="G01W"|substr(H3$IPCmain,1,3)=="G04"|substr(H3$IPCmain,1,4)=="G12B"|
                      substr(H3$IPCmain,1,4)=="G99Z"~"J",
                   substr(H3$IPCmain,1,6)=="G01N033"~"K",
                   substr(H3$IPCmain,1,4)=="G05B"|substr(H3$IPCmain,1,4)=="G05D"|substr(H3$IPCmain,1,4)=="G05F"|
                      substr(H3$IPCmain,1,3)=="G07"|substr(H3$IPCmain,1,4)=="G08B"|substr(H3$IPCmain,1,4)=="G08G"|
                      substr(H3$IPCmain,1,4)=="G09B"|substr(H3$IPCmain,1,4)=="G09C"|substr(H3$IPCmain,1,4)=="G09D"~"L",
                   substr(H3$IPCmain,1,4)=="A61B"|substr(H3$IPCmain,1,4)=="A61C"|substr(H3$IPCmain,1,4)=="A61D"|
                      substr(H3$IPCmain,1,4)=="A61F"|substr(H3$IPCmain,1,4)=="A61G"|substr(H3$IPCmain,1,4)=="A61H"|
                      substr(H3$IPCmain,1,4)=="A61J"|substr(H3$IPCmain,1,4)=="A61L"|substr(H3$IPCmain,1,4)=="A61M"|
                      substr(H3$IPCmain,1,4)=="A61N"|substr(H3$IPCmain,1,4)=="H05G"~"M",
                   substr(H3$IPCmain,1,4)=="C07B"|substr(H3$IPCmain,1,4)=="C07C"|substr(H3$IPCmain,1,4)=="C07D"|
                      substr(H3$IPCmain,1,4)=="C07F"|substr(H3$IPCmain,1,4)=="C07H"|substr(H3$IPCmain,1,4)=="C07J"|
                      substr(H3$IPCmain,1,4)=="C40B"|substr(H3$IPCmain,1,7)=="A61K008"|substr(H3$IPCmain,1,4)=="A61Q"~"N",
                   substr(H3$IPCmain,1,4)=="C07G"|substr(H3$IPCmain,1,4)=="C07K"|substr(H3$IPCmain,1,4)=="C12M"|
                      substr(H3$IPCmain,1,4)=="C12N"|substr(H3$IPCmain,1,4)=="C12P"|substr(H3$IPCmain,1,4)=="C12Q"|
                      substr(H3$IPCmain,1,4)=="C12R"|substr(H3$IPCmain,1,4)=="C12S"~"O",
                   substr(H3$IPCmain,1,4)=="A61K"~"P",
                   substr(H3$IPCmain,1,4)=="C08B"|substr(H3$IPCmain,1,4)=="C08C"|substr(H3$IPCmain,1,4)=="C08F"|
                      substr(H3$IPCmain,1,4)=="C08G"|substr(H3$IPCmain,1,4)=="C08H"|substr(H3$IPCmain,1,4)=="C08K"|
                      substr(H3$IPCmain,1,4)=="C08L"~"Q",
                   substr(H3$IPCmain,1,4)=="A01H"|substr(H3$IPCmain,1,4)=="A21D"|substr(H3$IPCmain,1,4)=="A23B"|
                      substr(H3$IPCmain,1,4)=="A23C"|substr(H3$IPCmain,1,4)=="A23D"|substr(H3$IPCmain,1,4)=="A23F"|
                      substr(H3$IPCmain,1,4)=="A23G"|substr(H3$IPCmain,1,4)=="A23J"|substr(H3$IPCmain,1,4)=="A23K"|
                      substr(H3$IPCmain,1,4)=="A23L"|substr(H3$IPCmain,1,4)=="C12C"|substr(H3$IPCmain,1,4)=="C12F"|
                      substr(H3$IPCmain,1,4)=="C12G"|substr(H3$IPCmain,1,4)=="C12H"|substr(H3$IPCmain,1,4)=="C12J"|
                      substr(H3$IPCmain,1,4)=="C13D"|substr(H3$IPCmain,1,4)=="C13F"|substr(H3$IPCmain,1,4)=="C13J"|
                      substr(H3$IPCmain,1,4)=="C13K"~"R",
                   substr(H3$IPCmain,1,4)=="A01N"|substr(H3$IPCmain,1,4)=="A01P"|substr(H3$IPCmain,1,3)=="C05"|
                      substr(H3$IPCmain,1,3)=="C06"|substr(H3$IPCmain,1,4)=="C09B"|substr(H3$IPCmain,1,4)=="C09C"|
                      substr(H3$IPCmain,1,4)=="C09F"|substr(H3$IPCmain,1,4)=="C09G"|substr(H3$IPCmain,1,4)=="C09H"|
                      substr(H3$IPCmain,1,4)=="C09K"|substr(H3$IPCmain,1,4)=="C09D"|substr(H3$IPCmain,1,4)=="C09J"|
                      substr(H3$IPCmain,1,4)=="C10B"|substr(H3$IPCmain,1,4)=="C10C"|substr(H3$IPCmain,1,4)=="C10F"|
                      substr(H3$IPCmain,1,4)=="C10G"|substr(H3$IPCmain,1,4)=="C10H"|substr(H3$IPCmain,1,4)=="C10J"|
                      substr(H3$IPCmain,1,4)=="C10K"|substr(H3$IPCmain,1,4)=="C10L"|substr(H3$IPCmain,1,4)=="C10M"|
                      substr(H3$IPCmain,1,4)=="C10N"|substr(H3$IPCmain,1,4)=="C11B"|substr(H3$IPCmain,1,4)=="C11C"|
                      substr(H3$IPCmain,1,4)=="C11D"|substr(H3$IPCmain,1,4)=="C99Z"~"S",
                   substr(H3$IPCmain,1,3)=="C01"|substr(H3$IPCmain,1,4)=="C03C"|substr(H3$IPCmain,1,3)=="C04"|
                      substr(H3$IPCmain,1,3)=="C21"|substr(H3$IPCmain,1,3)=="C22"|substr(H3$IPCmain,1,3)=="B22"~"T",
                   substr(H3$IPCmain,1,4)=="B05C"|substr(H3$IPCmain,1,4)=="B05D"|substr(H3$IPCmain,1,3)=="B32"|
                      substr(H3$IPCmain,1,3)=="C23"|substr(H3$IPCmain,1,3)=="C25"|substr(H3$IPCmain,1,3)=="C30"~"U",
                   substr(H3$IPCmain,1,3)=="B81"|substr(H3$IPCmain,1,3)=="B82"~"V",
                   substr(H3$IPCmain,1,4)=="B01B"|substr(H3$IPCmain,1,7)=="B01D000"|substr(H3$IPCmain,1,6)=="B01D01"|
                      substr(H3$IPCmain,1,6)=="B01D02"|substr(H3$IPCmain,1,6)=="B01D03"|substr(H3$IPCmain,1,7)=="B01D041"|
                      substr(H3$IPCmain,1,7)=="B01D043"|substr(H3$IPCmain,1,7)=="B01D057"|substr(H3$IPCmain,1,7)=="B01D059"|
                      substr(H3$IPCmain,1,6)=="B01D06"|substr(H3$IPCmain,1,6)=="B01D07"|substr(H3$IPCmain,1,4)=="B01F"|
                      substr(H3$IPCmain,1,4)=="B01J"|substr(H3$IPCmain,1,4)=="B01L"|substr(H3$IPCmain,1,4)=="B02C"|
                      substr(H3$IPCmain,1,3)=="B03"|substr(H3$IPCmain,1,3)=="B04"|substr(H3$IPCmain,1,4)=="B05B"|
                      substr(H3$IPCmain,1,4)=="B06B"|substr(H3$IPCmain,1,3)=="B07"|substr(H3$IPCmain,1,3)=="B08"|
                      substr(H3$IPCmain,1,4)=="D06B"|substr(H3$IPCmain,1,4)=="D06C"|substr(H3$IPCmain,1,4)=="D06L"|
                      substr(H3$IPCmain,1,4)=="F25J"|substr(H3$IPCmain,1,3)=="F26"|substr(H3$IPCmain,1,4)=="C14C"|
                      substr(H3$IPCmain,1,4)=="H05H"~"W",
                   substr(H3$IPCmain,1,4)=="A62D"|substr(H3$IPCmain,1,7)=="B01D045"|substr(H3$IPCmain,1,7)=="B01D046"|
                      substr(H3$IPCmain,1,7)=="B01D047"|substr(H3$IPCmain,1,7)=="B01D049"|substr(H3$IPCmain,1,7)=="B01D050"|
                      substr(H3$IPCmain,1,7)=="B01D051"|substr(H3$IPCmain,1,7)=="B01D052"|substr(H3$IPCmain,1,7)=="B01D053"|
                      substr(H3$IPCmain,1,3)=="B09"|substr(H3$IPCmain,1,4)=="B65F"|substr(H3$IPCmain,1,3)=="C02"|
                      substr(H3$IPCmain,1,4)=="F01N"|substr(H3$IPCmain,1,4)=="F23G"|substr(H3$IPCmain,1,4)=="F23J"|
                      substr(H3$IPCmain,1,4)=="G01T"|substr(H3$IPCmain,1,7)=="E01F008"|substr(H3$IPCmain,1,4)=="A62C"~"X",
                   substr(H3$IPCmain,1,4)=="B25J"|substr(H3$IPCmain,1,4)=="B65B"|substr(H3$IPCmain,1,4)=="B65C"|
                      substr(H3$IPCmain,1,4)=="B65D"|substr(H3$IPCmain,1,4)=="B65G"|substr(H3$IPCmain,1,4)=="B65H"|
                      substr(H3$IPCmain,1,3)=="B66"|substr(H3$IPCmain,1,3)=="B67"~"Y",
                   substr(H3$IPCmain,1,3)=="B21"|substr(H3$IPCmain,1,3)=="B23"|substr(H3$IPCmain,1,3)=="B24"|
                      substr(H3$IPCmain,1,4)=="B26D"|substr(H3$IPCmain,1,4)=="B26F"|substr(H3$IPCmain,1,3)=="B27"|
                      substr(H3$IPCmain,1,3)=="B30"|substr(H3$IPCmain,1,4)=="B25B"|substr(H3$IPCmain,1,4)=="B25C"|
                      substr(H3$IPCmain,1,4)=="B25D"|substr(H3$IPCmain,1,4)=="B25F"|substr(H3$IPCmain,1,4)=="B25G"|
                      substr(H3$IPCmain,1,4)=="B25H"|substr(H3$IPCmain,1,4)=="B26B"~"Z",
                   substr(H3$IPCmain,1,4)=="F01B"|substr(H3$IPCmain,1,4)=="F01C"|substr(H3$IPCmain,1,4)=="F01D"|
                      substr(H3$IPCmain,1,4)=="F01K"|substr(H3$IPCmain,1,4)=="F01L"|substr(H3$IPCmain,1,4)=="F01M"|
                      substr(H3$IPCmain,1,4)=="F01P"|substr(H3$IPCmain,1,3)=="F02"|substr(H3$IPCmain,1,3)=="F03"|
                      substr(H3$IPCmain,1,3)=="F04"|substr(H3$IPCmain,1,4)=="F23R"|substr(H3$IPCmain,1,3)=="G21"|
                      substr(H3$IPCmain,1,4)=="F99Z"~"AA",
                   substr(H3$IPCmain,1,4)=="A41H"|substr(H3$IPCmain,1,4)=="A43D"|substr(H3$IPCmain,1,4)=="A46D"|
                      substr(H3$IPCmain,1,4)=="C14B"|substr(H3$IPCmain,1,3)=="D01"|substr(H3$IPCmain,1,3)=="D02"|
                      substr(H3$IPCmain,1,3)=="D03"|substr(H3$IPCmain,1,4)=="D04B"|substr(H3$IPCmain,1,4)=="D04C"|
                      substr(H3$IPCmain,1,4)=="D04G"|substr(H3$IPCmain,1,4)=="D04H"|substr(H3$IPCmain,1,4)=="D06J"|
                      substr(H3$IPCmain,1,4)=="D06M"|substr(H3$IPCmain,1,4)=="D06P"|substr(H3$IPCmain,1,4)=="D06Q"|
                      substr(H3$IPCmain,1,4)=="D99Z"|substr(H3$IPCmain,1,3)=="B31"|substr(H3$IPCmain,1,3)=="D21"|
                      substr(H3$IPCmain,1,3)=="B41"~"AB",
                   substr(H3$IPCmain,1,4)=="A01B"|substr(H3$IPCmain,1,4)=="A01C"|substr(H3$IPCmain,1,4)=="A01D"|
                      substr(H3$IPCmain,1,4)=="A01F"|substr(H3$IPCmain,1,4)=="A01G"|substr(H3$IPCmain,1,4)=="A01J"|
                      substr(H3$IPCmain,1,4)=="A01K"|substr(H3$IPCmain,1,4)=="A01L"|substr(H3$IPCmain,1,4)=="A01M"|
                      substr(H3$IPCmain,1,4)=="A21B"|substr(H3$IPCmain,1,4)=="A21C"|substr(H3$IPCmain,1,3)=="A22"|
                      substr(H3$IPCmain,1,4)=="A23N"|substr(H3$IPCmain,1,4)=="A23P"|substr(H3$IPCmain,1,4)=="B02B"|
                      substr(H3$IPCmain,1,4)=="C12L"|substr(H3$IPCmain,1,4)=="C13C"|substr(H3$IPCmain,1,4)=="C13G"|
                      substr(H3$IPCmain,1,4)=="C13H"|substr(H3$IPCmain,1,3)=="B28"|substr(H3$IPCmain,1,3)=="B29"|
                      substr(H3$IPCmain,1,4)=="C03B"|substr(H3$IPCmain,1,4)=="C08J"|substr(H3$IPCmain,1,4)=="B99Z"|
                      substr(H3$IPCmain,1,3)=="F41"|substr(H3$IPCmain,1,3)=="F42"~"AC",
                   substr(H3$IPCmain,1,3)=="F22"|substr(H3$IPCmain,1,4)=="F23B"|substr(H3$IPCmain,1,4)=="F23C"|
                      substr(H3$IPCmain,1,4)=="F23D"|substr(H3$IPCmain,1,4)=="F23H"|substr(H3$IPCmain,1,4)=="F23K"|
                      substr(H3$IPCmain,1,4)=="F23L"|substr(H3$IPCmain,1,4)=="F23M"|substr(H3$IPCmain,1,4)=="F23N"|
                      substr(H3$IPCmain,1,4)=="F23Q"|substr(H3$IPCmain,1,3)=="F24"|substr(H3$IPCmain,1,4)=="F25B"|
                      substr(H3$IPCmain,1,4)=="F25C"|substr(H3$IPCmain,1,3)=="F27"|substr(H3$IPCmain,1,3)=="F28"~"AD",
                   substr(H3$IPCmain,1,3)=="F15"|substr(H3$IPCmain,1,3)=="F16"|substr(H3$IPCmain,1,3)=="F17"|
                      substr(H3$IPCmain,1,4)=="G05G"~"AE",
                   substr(H3$IPCmain,1,3)=="B60"|substr(H3$IPCmain,1,3)=="B61"|substr(H3$IPCmain,1,3)=="B62"|
                      substr(H3$IPCmain,1,4)=="B63B"|substr(H3$IPCmain,1,4)=="B63C"|substr(H3$IPCmain,1,4)=="B63G"|
                      substr(H3$IPCmain,1,4)=="B63H"|substr(H3$IPCmain,1,4)=="B63J"|substr(H3$IPCmain,1,3)=="B64"~"AF",
                   substr(H3$IPCmain,1,3)=="A47"|substr(H3$IPCmain,1,3)=="A63"~"AG",
                   substr(H3$IPCmain,1,3)=="A24"|substr(H3$IPCmain,1,4)=="A41B"|substr(H3$IPCmain,1,4)=="A41C"|
                      substr(H3$IPCmain,1,4)=="A41D"|substr(H3$IPCmain,1,4)=="A41F"|substr(H3$IPCmain,1,4)=="A41G"|
                      substr(H3$IPCmain,1,3)=="A42"|substr(H3$IPCmain,1,4)=="A43B"|substr(H3$IPCmain,1,4)=="A43C"|
                      substr(H3$IPCmain,1,3)=="A44"|substr(H3$IPCmain,1,3)=="A45"|substr(H3$IPCmain,1,4)=="A46B"|
                      substr(H3$IPCmain,1,4)=="A62B"|substr(H3$IPCmain,1,3)=="B42"|substr(H3$IPCmain,1,3)=="B43"|
                      substr(H3$IPCmain,1,4)=="D04D"|substr(H3$IPCmain,1,3)=="D07"|substr(H3$IPCmain,1,4)=="G10B"|
                      substr(H3$IPCmain,1,4)=="G10C"|substr(H3$IPCmain,1,4)=="G10D"|substr(H3$IPCmain,1,4)=="G10F"|
                      substr(H3$IPCmain,1,4)=="G10G"|substr(H3$IPCmain,1,4)=="G10H"|substr(H3$IPCmain,1,4)=="G10K"|
                      substr(H3$IPCmain,1,3)=="B44"|substr(H3$IPCmain,1,3)=="B68"|substr(H3$IPCmain,1,4)=="D06F"|
                      substr(H3$IPCmain,1,4)=="D06N"|substr(H3$IPCmain,1,4)=="F25D"|substr(H3$IPCmain,1,4)=="A99Z"~"AH",
                   substr(H3$IPCmain,1,3)=="E02"|substr(H3$IPCmain,1,4)=="E01B"|substr(H3$IPCmain,1,4)=="E01C"|
                      substr(H3$IPCmain,1,4)=="E01C"|substr(H3$IPCmain,1,7)=="E01F001"|substr(H3$IPCmain,1,7)=="E01F003"|
                      substr(H3$IPCmain,1,7)=="E01F005"|substr(H3$IPCmain,1,7)=="E01F007"|substr(H3$IPCmain,1,7)=="E01F009"|
                      substr(H3$IPCmain,1,6)=="E01F01"|substr(H3$IPCmain,1,4)=="E01H"|substr(H3$IPCmain,1,3)=="E03"|
                      substr(H3$IPCmain,1,3)=="E04"|substr(H3$IPCmain,1,3)=="E05"|substr(H3$IPCmain,1,3)=="E06"|
                      substr(H3$IPCmain,1,3)=="E21"|substr(H3$IPCmain,1,4)=="E99Z"~"AI")

#Sum of different IPC Codes by company
H3$DivTech<-ave(H3$IPCmain,H3$GUO, FUN=function(x) length(unique(x))) #now with variety of IPC codes in area, otherwise total number of patents with H3$DivTech2<-ifelse(!is.na(H3$IPCmain),1,0)
H3$DivTech1<-ifelse(is.na(H3$IPCmain),0,H3$DivTech)
H3$DivTech1<-as.numeric(H3$DivTech1)

#Sum of different IPC Codes within each technical area in which the company is active 
H3$DivTech2<-ave(H3$IPCmain,H3[,c('GUO', 'Area')],FUN=function(x) length(unique(x)))
H3$DivTech2<-as.numeric(H3$DivTech2)

#Distribution of area patents among all patents
H3$DivTech3<-H3$DivTech2/H3$DivTech1

#Account for not classified patents here -- increases the final number immensely
H3$DivTech3[is.na(H3$DivTech3)] <- 0

# Test Company: CL869632007

#Remove duplicates to sum unique values
H3$Test<-paste(H3$GUO,H3$Area,H3$DivTech3,sep=" ")
H3<-H3[!duplicated(H3$Test),]

#Sum the shares, square them and substract from 1 = From concentration to diversification
H3$DivTech4<-ave(H3$DivTech3,H3$GUO,FUN=sum)
H3$DivTech5<-H3$DivTech4^2
H3$DivTech6<-1-H3$DivTech5

#Correct for the total number of patents (the measure is biased for small numbers)
H3$DivTech7<-ave(H3$DivTech1,H3$GUO,FUN=sum)
H3$DivTech8<-ifelse(H3$DivTech1==0 & H3$DivTech7!=0,NA,1)
H3<-subset(H3,!is.na(H3$DivTech8))
H3$DivTech<-H3$DivTech6*(1-1/H3$DivTech1) # different than in Rahko formular but makes more sense

H3$DivTech[H3$DivTech=="-Inf"] <-0

H3<-H3[,c(-2,-3,-4,-6,-7,-8,-9,-10,-11,-12,-13,-14)]
names(H3)<-c("BvDID","DivTech")

H3<-H3[!duplicated(H3$BvDID),]


#2014
Stock2014<-fread("files_created_code1/Stock2014.csv", dec=",")

Stock2014<-Stock2014[,c(-2,-3,-4,-5,-7,-8,-9)]
Full4<-fread("files_created_code1/Full4.csv", dec=",")
Full4[,3:33]<-NULL
names(Full4) <- c("GUO","Subsidiaries")
names(Stock2014) <- c("Subsidiaries","IPCmain")

H4<-left_join(Full4,Stock2014,by="Subsidiaries",na_matches="never")

H4$IPCmain<-as.character(H4$IPCmain)

#Filter for technological area of invention
H4$Area<-case_when(substr(H4$IPCmain,1,3)=="F21"~"A",
                   substr(H4$IPCmain,1,4)=="H01B"|substr(H4$IPCmain,1,4)=="H01C"|substr(H4$IPCmain,1,4)=="H01F"|
                      substr(H4$IPCmain,1,4)=="H01G"|substr(H4$IPCmain,1,4)=="H01H"|substr(H4$IPCmain,1,4)=="H01J"|
                      substr(H4$IPCmain,1,4)=="H01K"|substr(H4$IPCmain,1,4)=="H01M"|substr(H4$IPCmain,1,4)=="H01R"|
                      substr(H4$IPCmain,1,4)=="H01T"|substr(H4$IPCmain,1,3)=="H02"|substr(H4$IPCmain,1,4)=="H05B"|
                      substr(H4$IPCmain,1,4)=="H05C"|substr(H4$IPCmain,1,4)=="H05F"|substr(H4$IPCmain,1,4)=="H99Z"~"A",
                   substr(H4$IPCmain,1,4)=="G09F"|substr(H4$IPCmain,1,4)=="G09G"|substr(H4$IPCmain,1,4)=="G11B"|
                      substr(H4$IPCmain,1,6)=="H04N003"|substr(H4$IPCmain,1,6)=="H04N003"|substr(H4$IPCmain,1,6)=="H04N009"|
                      substr(H4$IPCmain,1,6)==" H04N013"|substr(H4$IPCmain,1,6)=="H04N015"|substr(H4$IPCmain,1,6)=="H04N017"|
                      substr(H4$IPCmain,1,4)=="H04R"|substr(H4$IPCmain,1,4)=="H04S"|substr(H4$IPCmain,1,4)=="H05K"~"B",
                   substr(H4$IPCmain,1,4)=="G08C"|substr(H4$IPCmain,1,4)=="H01P"|substr(H4$IPCmain,1,4)=="H01Q"|
                      substr(H4$IPCmain,1,4)=="H04B"|substr(H4$IPCmain,1,4)=="H04H"|substr(H4$IPCmain,1,4)==" H04J"|
                      substr(H4$IPCmain,1,4)=="H04K"|substr(H4$IPCmain,1,4)=="H04M"|substr(H4$IPCmain,1,6)=="H04N001"|
                      substr(H4$IPCmain,1,6)=="H04N007"|substr(H4$IPCmain,1,6)=="H04N011"|substr(H4$IPCmain,1,4)=="H04Q"~"C",
                   substr(H4$IPCmain,1,4)=="H04L"~"D",
                   substr(H4$IPCmain,1,3)=="H03"~"E",
                   substr(H4$IPCmain,1,3)=="G06"|substr(H4$IPCmain,1,4)=="G11C"|substr(H4$IPCmain,1,4)=="G10L"~"F",
                   substr(H4$IPCmain,1,4)=="G06Q"~"G",
                   substr(H4$IPCmain,1,4)=="H01L"~"H",
                   substr(H4$IPCmain,1,3)=="G02"|substr(H4$IPCmain,1,4)=="G03B"|substr(H4$IPCmain,1,4)=="G03C"|
                      substr(H4$IPCmain,1,6)=="G03D"|substr(H4$IPCmain,1,4)=="G03F"|substr(H4$IPCmain,1,4)=="G03G"|
                      substr(H4$IPCmain,1,4)=="G03H"|substr(H4$IPCmain,1,4)=="H01S"~"I",
                   substr(H4$IPCmain,1,4)=="G01B"|substr(H4$IPCmain,1,4)=="G01C"|substr(H4$IPCmain,1,4)=="G01D"|
                      substr(H4$IPCmain,1,4)=="G01F"|substr(H4$IPCmain,1,4)=="G01G"|substr(H4$IPCmain,1,4)=="G01H"|
                      substr(H4$IPCmain,1,4)=="G01J"|substr(H4$IPCmain,1,4)=="G01K"|substr(H4$IPCmain,1,4)=="G01L"|
                      substr(H4$IPCmain,1,4)=="G01M"|substr(H4$IPCmain,1,4)=="G01N"|substr(H4$IPCmain,1,4)=="G01P"|
                      substr(H4$IPCmain,1,4)=="G01R"|substr(H4$IPCmain,1,4)=="G01S"|substr(H4$IPCmain,1,4)=="G01V"|
                      substr(H4$IPCmain,1,4)=="G01W"|substr(H4$IPCmain,1,3)=="G04"|substr(H4$IPCmain,1,4)=="G12B"|
                      substr(H4$IPCmain,1,4)=="G99Z"~"J",
                   substr(H4$IPCmain,1,6)=="G01N033"~"K",
                   substr(H4$IPCmain,1,4)=="G05B"|substr(H4$IPCmain,1,4)=="G05D"|substr(H4$IPCmain,1,4)=="G05F"|
                      substr(H4$IPCmain,1,3)=="G07"|substr(H4$IPCmain,1,4)=="G08B"|substr(H4$IPCmain,1,4)=="G08G"|
                      substr(H4$IPCmain,1,4)=="G09B"|substr(H4$IPCmain,1,4)=="G09C"|substr(H4$IPCmain,1,4)=="G09D"~"L",
                   substr(H4$IPCmain,1,4)=="A61B"|substr(H4$IPCmain,1,4)=="A61C"|substr(H4$IPCmain,1,4)=="A61D"|
                      substr(H4$IPCmain,1,4)=="A61F"|substr(H4$IPCmain,1,4)=="A61G"|substr(H4$IPCmain,1,4)=="A61H"|
                      substr(H4$IPCmain,1,4)=="A61J"|substr(H4$IPCmain,1,4)=="A61L"|substr(H4$IPCmain,1,4)=="A61M"|
                      substr(H4$IPCmain,1,4)=="A61N"|substr(H4$IPCmain,1,4)=="H05G"~"M",
                   substr(H4$IPCmain,1,4)=="C07B"|substr(H4$IPCmain,1,4)=="C07C"|substr(H4$IPCmain,1,4)=="C07D"|
                      substr(H4$IPCmain,1,4)=="C07F"|substr(H4$IPCmain,1,4)=="C07H"|substr(H4$IPCmain,1,4)=="C07J"|
                      substr(H4$IPCmain,1,4)=="C40B"|substr(H4$IPCmain,1,7)=="A61K008"|substr(H4$IPCmain,1,4)=="A61Q"~"N",
                   substr(H4$IPCmain,1,4)=="C07G"|substr(H4$IPCmain,1,4)=="C07K"|substr(H4$IPCmain,1,4)=="C12M"|
                      substr(H4$IPCmain,1,4)=="C12N"|substr(H4$IPCmain,1,4)=="C12P"|substr(H4$IPCmain,1,4)=="C12Q"|
                      substr(H4$IPCmain,1,4)=="C12R"|substr(H4$IPCmain,1,4)=="C12S"~"O",
                   substr(H4$IPCmain,1,4)=="A61K"~"P",
                   substr(H4$IPCmain,1,4)=="C08B"|substr(H4$IPCmain,1,4)=="C08C"|substr(H4$IPCmain,1,4)=="C08F"|
                      substr(H4$IPCmain,1,4)=="C08G"|substr(H4$IPCmain,1,4)=="C08H"|substr(H4$IPCmain,1,4)=="C08K"|
                      substr(H4$IPCmain,1,4)=="C08L"~"Q",
                   substr(H4$IPCmain,1,4)=="A01H"|substr(H4$IPCmain,1,4)=="A21D"|substr(H4$IPCmain,1,4)=="A23B"|
                      substr(H4$IPCmain,1,4)=="A23C"|substr(H4$IPCmain,1,4)=="A23D"|substr(H4$IPCmain,1,4)=="A23F"|
                      substr(H4$IPCmain,1,4)=="A23G"|substr(H4$IPCmain,1,4)=="A23J"|substr(H4$IPCmain,1,4)=="A23K"|
                      substr(H4$IPCmain,1,4)=="A23L"|substr(H4$IPCmain,1,4)=="C12C"|substr(H4$IPCmain,1,4)=="C12F"|
                      substr(H4$IPCmain,1,4)=="C12G"|substr(H4$IPCmain,1,4)=="C12H"|substr(H4$IPCmain,1,4)=="C12J"|
                      substr(H4$IPCmain,1,4)=="C13D"|substr(H4$IPCmain,1,4)=="C13F"|substr(H4$IPCmain,1,4)=="C13J"|
                      substr(H4$IPCmain,1,4)=="C13K"~"R",
                   substr(H4$IPCmain,1,4)=="A01N"|substr(H4$IPCmain,1,4)=="A01P"|substr(H4$IPCmain,1,3)=="C05"|
                      substr(H4$IPCmain,1,3)=="C06"|substr(H4$IPCmain,1,4)=="C09B"|substr(H4$IPCmain,1,4)=="C09C"|
                      substr(H4$IPCmain,1,4)=="C09F"|substr(H4$IPCmain,1,4)=="C09G"|substr(H4$IPCmain,1,4)=="C09H"|
                      substr(H4$IPCmain,1,4)=="C09K"|substr(H4$IPCmain,1,4)=="C09D"|substr(H4$IPCmain,1,4)=="C09J"|
                      substr(H4$IPCmain,1,4)=="C10B"|substr(H4$IPCmain,1,4)=="C10C"|substr(H4$IPCmain,1,4)=="C10F"|
                      substr(H4$IPCmain,1,4)=="C10G"|substr(H4$IPCmain,1,4)=="C10H"|substr(H4$IPCmain,1,4)=="C10J"|
                      substr(H4$IPCmain,1,4)=="C10K"|substr(H4$IPCmain,1,4)=="C10L"|substr(H4$IPCmain,1,4)=="C10M"|
                      substr(H4$IPCmain,1,4)=="C10N"|substr(H4$IPCmain,1,4)=="C11B"|substr(H4$IPCmain,1,4)=="C11C"|
                      substr(H4$IPCmain,1,4)=="C11D"|substr(H4$IPCmain,1,4)=="C99Z"~"S",
                   substr(H4$IPCmain,1,3)=="C01"|substr(H4$IPCmain,1,4)=="C03C"|substr(H4$IPCmain,1,3)=="C04"|
                      substr(H4$IPCmain,1,3)=="C21"|substr(H4$IPCmain,1,3)=="C22"|substr(H4$IPCmain,1,3)=="B22"~"T",
                   substr(H4$IPCmain,1,4)=="B05C"|substr(H4$IPCmain,1,4)=="B05D"|substr(H4$IPCmain,1,3)=="B32"|
                      substr(H4$IPCmain,1,3)=="C23"|substr(H4$IPCmain,1,3)=="C25"|substr(H4$IPCmain,1,3)=="C30"~"U",
                   substr(H4$IPCmain,1,3)=="B81"|substr(H4$IPCmain,1,3)=="B82"~"V",
                   substr(H4$IPCmain,1,4)=="B01B"|substr(H4$IPCmain,1,7)=="B01D000"|substr(H4$IPCmain,1,6)=="B01D01"|
                      substr(H4$IPCmain,1,6)=="B01D02"|substr(H4$IPCmain,1,6)=="B01D03"|substr(H4$IPCmain,1,7)=="B01D041"|
                      substr(H4$IPCmain,1,7)=="B01D043"|substr(H4$IPCmain,1,7)=="B01D057"|substr(H4$IPCmain,1,7)=="B01D059"|
                      substr(H4$IPCmain,1,6)=="B01D06"|substr(H4$IPCmain,1,6)=="B01D07"|substr(H4$IPCmain,1,4)=="B01F"|
                      substr(H4$IPCmain,1,4)=="B01J"|substr(H4$IPCmain,1,4)=="B01L"|substr(H4$IPCmain,1,4)=="B02C"|
                      substr(H4$IPCmain,1,3)=="B03"|substr(H4$IPCmain,1,3)=="B04"|substr(H4$IPCmain,1,4)=="B05B"|
                      substr(H4$IPCmain,1,4)=="B06B"|substr(H4$IPCmain,1,3)=="B07"|substr(H4$IPCmain,1,3)=="B08"|
                      substr(H4$IPCmain,1,4)=="D06B"|substr(H4$IPCmain,1,4)=="D06C"|substr(H4$IPCmain,1,4)=="D06L"|
                      substr(H4$IPCmain,1,4)=="F25J"|substr(H4$IPCmain,1,3)=="F26"|substr(H4$IPCmain,1,4)=="C14C"|
                      substr(H4$IPCmain,1,4)=="H05H"~"W",
                   substr(H4$IPCmain,1,4)=="A62D"|substr(H4$IPCmain,1,7)=="B01D045"|substr(H4$IPCmain,1,7)=="B01D046"|
                      substr(H4$IPCmain,1,7)=="B01D047"|substr(H4$IPCmain,1,7)=="B01D049"|substr(H4$IPCmain,1,7)=="B01D050"|
                      substr(H4$IPCmain,1,7)=="B01D051"|substr(H4$IPCmain,1,7)=="B01D052"|substr(H4$IPCmain,1,7)=="B01D053"|
                      substr(H4$IPCmain,1,3)=="B09"|substr(H4$IPCmain,1,4)=="B65F"|substr(H4$IPCmain,1,3)=="C02"|
                      substr(H4$IPCmain,1,4)=="F01N"|substr(H4$IPCmain,1,4)=="F23G"|substr(H4$IPCmain,1,4)=="F23J"|
                      substr(H4$IPCmain,1,4)=="G01T"|substr(H4$IPCmain,1,7)=="E01F008"|substr(H4$IPCmain,1,4)=="A62C"~"X",
                   substr(H4$IPCmain,1,4)=="B25J"|substr(H4$IPCmain,1,4)=="B65B"|substr(H4$IPCmain,1,4)=="B65C"|
                      substr(H4$IPCmain,1,4)=="B65D"|substr(H4$IPCmain,1,4)=="B65G"|substr(H4$IPCmain,1,4)=="B65H"|
                      substr(H4$IPCmain,1,3)=="B66"|substr(H4$IPCmain,1,3)=="B67"~"Y",
                   substr(H4$IPCmain,1,3)=="B21"|substr(H4$IPCmain,1,3)=="B23"|substr(H4$IPCmain,1,3)=="B24"|
                      substr(H4$IPCmain,1,4)=="B26D"|substr(H4$IPCmain,1,4)=="B26F"|substr(H4$IPCmain,1,3)=="B27"|
                      substr(H4$IPCmain,1,3)=="B30"|substr(H4$IPCmain,1,4)=="B25B"|substr(H4$IPCmain,1,4)=="B25C"|
                      substr(H4$IPCmain,1,4)=="B25D"|substr(H4$IPCmain,1,4)=="B25F"|substr(H4$IPCmain,1,4)=="B25G"|
                      substr(H4$IPCmain,1,4)=="B25H"|substr(H4$IPCmain,1,4)=="B26B"~"Z",
                   substr(H4$IPCmain,1,4)=="F01B"|substr(H4$IPCmain,1,4)=="F01C"|substr(H4$IPCmain,1,4)=="F01D"|
                      substr(H4$IPCmain,1,4)=="F01K"|substr(H4$IPCmain,1,4)=="F01L"|substr(H4$IPCmain,1,4)=="F01M"|
                      substr(H4$IPCmain,1,4)=="F01P"|substr(H4$IPCmain,1,3)=="F02"|substr(H4$IPCmain,1,3)=="F03"|
                      substr(H4$IPCmain,1,3)=="F04"|substr(H4$IPCmain,1,4)=="F23R"|substr(H4$IPCmain,1,3)=="G21"|
                      substr(H4$IPCmain,1,4)=="F99Z"~"AA",
                   substr(H4$IPCmain,1,4)=="A41H"|substr(H4$IPCmain,1,4)=="A43D"|substr(H4$IPCmain,1,4)=="A46D"|
                      substr(H4$IPCmain,1,4)=="C14B"|substr(H4$IPCmain,1,3)=="D01"|substr(H4$IPCmain,1,3)=="D02"|
                      substr(H4$IPCmain,1,3)=="D03"|substr(H4$IPCmain,1,4)=="D04B"|substr(H4$IPCmain,1,4)=="D04C"|
                      substr(H4$IPCmain,1,4)=="D04G"|substr(H4$IPCmain,1,4)=="D04H"|substr(H4$IPCmain,1,4)=="D06J"|
                      substr(H4$IPCmain,1,4)=="D06M"|substr(H4$IPCmain,1,4)=="D06P"|substr(H4$IPCmain,1,4)=="D06Q"|
                      substr(H4$IPCmain,1,4)=="D99Z"|substr(H4$IPCmain,1,3)=="B31"|substr(H4$IPCmain,1,3)=="D21"|
                      substr(H4$IPCmain,1,3)=="B41"~"AB",
                   substr(H4$IPCmain,1,4)=="A01B"|substr(H4$IPCmain,1,4)=="A01C"|substr(H4$IPCmain,1,4)=="A01D"|
                      substr(H4$IPCmain,1,4)=="A01F"|substr(H4$IPCmain,1,4)=="A01G"|substr(H4$IPCmain,1,4)=="A01J"|
                      substr(H4$IPCmain,1,4)=="A01K"|substr(H4$IPCmain,1,4)=="A01L"|substr(H4$IPCmain,1,4)=="A01M"|
                      substr(H4$IPCmain,1,4)=="A21B"|substr(H4$IPCmain,1,4)=="A21C"|substr(H4$IPCmain,1,3)=="A22"|
                      substr(H4$IPCmain,1,4)=="A23N"|substr(H4$IPCmain,1,4)=="A23P"|substr(H4$IPCmain,1,4)=="B02B"|
                      substr(H4$IPCmain,1,4)=="C12L"|substr(H4$IPCmain,1,4)=="C13C"|substr(H4$IPCmain,1,4)=="C13G"|
                      substr(H4$IPCmain,1,4)=="C13H"|substr(H4$IPCmain,1,3)=="B28"|substr(H4$IPCmain,1,3)=="B29"|
                      substr(H4$IPCmain,1,4)=="C03B"|substr(H4$IPCmain,1,4)=="C08J"|substr(H4$IPCmain,1,4)=="B99Z"|
                      substr(H4$IPCmain,1,3)=="F41"|substr(H4$IPCmain,1,3)=="F42"~"AC",
                   substr(H4$IPCmain,1,3)=="F22"|substr(H4$IPCmain,1,4)=="F23B"|substr(H4$IPCmain,1,4)=="F23C"|
                      substr(H4$IPCmain,1,4)=="F23D"|substr(H4$IPCmain,1,4)=="F23H"|substr(H4$IPCmain,1,4)=="F23K"|
                      substr(H4$IPCmain,1,4)=="F23L"|substr(H4$IPCmain,1,4)=="F23M"|substr(H4$IPCmain,1,4)=="F23N"|
                      substr(H4$IPCmain,1,4)=="F23Q"|substr(H4$IPCmain,1,3)=="F24"|substr(H4$IPCmain,1,4)=="F25B"|
                      substr(H4$IPCmain,1,4)=="F25C"|substr(H4$IPCmain,1,3)=="F27"|substr(H4$IPCmain,1,3)=="F28"~"AD",
                   substr(H4$IPCmain,1,3)=="F15"|substr(H4$IPCmain,1,3)=="F16"|substr(H4$IPCmain,1,3)=="F17"|
                      substr(H4$IPCmain,1,4)=="G05G"~"AE",
                   substr(H4$IPCmain,1,3)=="B60"|substr(H4$IPCmain,1,3)=="B61"|substr(H4$IPCmain,1,3)=="B62"|
                      substr(H4$IPCmain,1,4)=="B63B"|substr(H4$IPCmain,1,4)=="B63C"|substr(H4$IPCmain,1,4)=="B63G"|
                      substr(H4$IPCmain,1,4)=="B63H"|substr(H4$IPCmain,1,4)=="B63J"|substr(H4$IPCmain,1,3)=="B64"~"AF",
                   substr(H4$IPCmain,1,3)=="A47"|substr(H4$IPCmain,1,3)=="A63"~"AG",
                   substr(H4$IPCmain,1,3)=="A24"|substr(H4$IPCmain,1,4)=="A41B"|substr(H4$IPCmain,1,4)=="A41C"|
                      substr(H4$IPCmain,1,4)=="A41D"|substr(H4$IPCmain,1,4)=="A41F"|substr(H4$IPCmain,1,4)=="A41G"|
                      substr(H4$IPCmain,1,3)=="A42"|substr(H4$IPCmain,1,4)=="A43B"|substr(H4$IPCmain,1,4)=="A43C"|
                      substr(H4$IPCmain,1,3)=="A44"|substr(H4$IPCmain,1,3)=="A45"|substr(H4$IPCmain,1,4)=="A46B"|
                      substr(H4$IPCmain,1,4)=="A62B"|substr(H4$IPCmain,1,3)=="B42"|substr(H4$IPCmain,1,3)=="B43"|
                      substr(H4$IPCmain,1,4)=="D04D"|substr(H4$IPCmain,1,3)=="D07"|substr(H4$IPCmain,1,4)=="G10B"|
                      substr(H4$IPCmain,1,4)=="G10C"|substr(H4$IPCmain,1,4)=="G10D"|substr(H4$IPCmain,1,4)=="G10F"|
                      substr(H4$IPCmain,1,4)=="G10G"|substr(H4$IPCmain,1,4)=="G10H"|substr(H4$IPCmain,1,4)=="G10K"|
                      substr(H4$IPCmain,1,3)=="B44"|substr(H4$IPCmain,1,3)=="B68"|substr(H4$IPCmain,1,4)=="D06F"|
                      substr(H4$IPCmain,1,4)=="D06N"|substr(H4$IPCmain,1,4)=="F25D"|substr(H4$IPCmain,1,4)=="A99Z"~"AH",
                   substr(H4$IPCmain,1,3)=="E02"|substr(H4$IPCmain,1,4)=="E01B"|substr(H4$IPCmain,1,4)=="E01C"|
                      substr(H4$IPCmain,1,4)=="E01C"|substr(H4$IPCmain,1,7)=="E01F001"|substr(H4$IPCmain,1,7)=="E01F003"|
                      substr(H4$IPCmain,1,7)=="E01F005"|substr(H4$IPCmain,1,7)=="E01F007"|substr(H4$IPCmain,1,7)=="E01F009"|
                      substr(H4$IPCmain,1,6)=="E01F01"|substr(H4$IPCmain,1,4)=="E01H"|substr(H4$IPCmain,1,3)=="E03"|
                      substr(H4$IPCmain,1,3)=="E04"|substr(H4$IPCmain,1,3)=="E05"|substr(H4$IPCmain,1,3)=="E06"|
                      substr(H4$IPCmain,1,3)=="E21"|substr(H4$IPCmain,1,4)=="E99Z"~"AI")

#Sum of different IPC Codes by company
H4$DivTech<-ave(H4$IPCmain,H4$GUO, FUN=function(x) length(unique(x))) #now with variety of IPC codes in area, otherwise total number of patents with H4$DivTech2<-ifelse(!is.na(H4$IPCmain),1,0)
H4$DivTech1<-ifelse(is.na(H4$IPCmain),0,H4$DivTech)
H4$DivTech1<-as.numeric(H4$DivTech1)

#Sum of different IPC Codes within each technical area in which the company is active 
H4$DivTech2<-ave(H4$IPCmain,H4[,c('GUO', 'Area')],FUN=function(x) length(unique(x)))
H4$DivTech2<-as.numeric(H4$DivTech2)

#Distribution of area patents among all patents
H4$DivTech3<-H4$DivTech2/H4$DivTech1

#Account for not classified patents here -- increases the final number immensely
H4$DivTech3[is.na(H4$DivTech3)] <- 0

#Remove duplicates to sum unique values
H4$Test<-paste(H4$GUO,H4$Area,H4$DivTech3,sep=" ")
H4<-H4[!duplicated(H4$Test),]

#Sum the shares, square them and substract from 1 = From concentration to diversification
H4$DivTech4<-ave(H4$DivTech3,H4$GUO,FUN=sum)
H4$DivTech5<-H4$DivTech4^2
H4$DivTech6<-1-H4$DivTech5

#Correct for the total number of patents (the measure is biased for small numbers)
H4$DivTech7<-ave(H4$DivTech1,H4$GUO,FUN=sum)
H4$DivTech8<-ifelse(H4$DivTech1==0 & H4$DivTech7!=0,NA,1)
H4<-subset(H4,!is.na(H4$DivTech8))
H4$DivTech<-H4$DivTech6*(1-1/H4$DivTech1) # different than in Rahko formular but makes more sense

H4$DivTech[H4$DivTech=="-Inf"] <-0

H4<-H4[,c(-2,-3,-4,-6,-7,-8,-9,-10,-11,-12,-13,-14)]
names(H4)<-c("BvDID","DivTech")

H4<-H4[!duplicated(H4$BvDID),]


#2015
Stock2015<-fread("files_created_code1/Stock2015.csv", dec=",")

Stock2015<-Stock2015[,c(-2,-3,-4,-5,-7,-8,-9)]
Full5<-fread("files_created_code1/Full5.csv", dec=",")
Full5[,3:33]<-NULL
names(Full5) <- c("GUO","Subsidiaries")
names(Stock2015) <- c("Subsidiaries","IPCmain")

H5<-left_join(Full5,Stock2015,by="Subsidiaries",na_matches="never")

H5$IPCmain<-as.character(H5$IPCmain)

#Filter for technological area of invention
H5$Area<-case_when(substr(H5$IPCmain,1,3)=="F21"~"A",
                   substr(H5$IPCmain,1,4)=="H01B"|substr(H5$IPCmain,1,4)=="H01C"|substr(H5$IPCmain,1,4)=="H01F"|
                      substr(H5$IPCmain,1,4)=="H01G"|substr(H5$IPCmain,1,4)=="H01H"|substr(H5$IPCmain,1,4)=="H01J"|
                      substr(H5$IPCmain,1,4)=="H01K"|substr(H5$IPCmain,1,4)=="H01M"|substr(H5$IPCmain,1,4)=="H01R"|
                      substr(H5$IPCmain,1,4)=="H01T"|substr(H5$IPCmain,1,3)=="H02"|substr(H5$IPCmain,1,4)=="H05B"|
                      substr(H5$IPCmain,1,4)=="H05C"|substr(H5$IPCmain,1,4)=="H05F"|substr(H5$IPCmain,1,4)=="H99Z"~"A",
                   substr(H5$IPCmain,1,4)=="G09F"|substr(H5$IPCmain,1,4)=="G09G"|substr(H5$IPCmain,1,4)=="G11B"|
                      substr(H5$IPCmain,1,6)=="H04N003"|substr(H5$IPCmain,1,6)=="H04N003"|substr(H5$IPCmain,1,6)=="H04N009"|
                      substr(H5$IPCmain,1,6)==" H04N013"|substr(H5$IPCmain,1,6)=="H04N015"|substr(H5$IPCmain,1,6)=="H04N017"|
                      substr(H5$IPCmain,1,4)=="H04R"|substr(H5$IPCmain,1,4)=="H04S"|substr(H5$IPCmain,1,4)=="H05K"~"B",
                   substr(H5$IPCmain,1,4)=="G08C"|substr(H5$IPCmain,1,4)=="H01P"|substr(H5$IPCmain,1,4)=="H01Q"|
                      substr(H5$IPCmain,1,4)=="H04B"|substr(H5$IPCmain,1,4)=="H04H"|substr(H5$IPCmain,1,4)==" H04J"|
                      substr(H5$IPCmain,1,4)=="H04K"|substr(H5$IPCmain,1,4)=="H04M"|substr(H5$IPCmain,1,6)=="H04N001"|
                      substr(H5$IPCmain,1,6)=="H04N007"|substr(H5$IPCmain,1,6)=="H04N011"|substr(H5$IPCmain,1,4)=="H04Q"~"C",
                   substr(H5$IPCmain,1,4)=="H04L"~"D",
                   substr(H5$IPCmain,1,3)=="H03"~"E",
                   substr(H5$IPCmain,1,3)=="G06"|substr(H5$IPCmain,1,4)=="G11C"|substr(H5$IPCmain,1,4)=="G10L"~"F",
                   substr(H5$IPCmain,1,4)=="G06Q"~"G",
                   substr(H5$IPCmain,1,4)=="H01L"~"H",
                   substr(H5$IPCmain,1,3)=="G02"|substr(H5$IPCmain,1,4)=="G03B"|substr(H5$IPCmain,1,4)=="G03C"|
                      substr(H5$IPCmain,1,6)=="G03D"|substr(H5$IPCmain,1,4)=="G03F"|substr(H5$IPCmain,1,4)=="G03G"|
                      substr(H5$IPCmain,1,4)=="G03H"|substr(H5$IPCmain,1,4)=="H01S"~"I",
                   substr(H5$IPCmain,1,4)=="G01B"|substr(H5$IPCmain,1,4)=="G01C"|substr(H5$IPCmain,1,4)=="G01D"|
                      substr(H5$IPCmain,1,4)=="G01F"|substr(H5$IPCmain,1,4)=="G01G"|substr(H5$IPCmain,1,4)=="G01H"|
                      substr(H5$IPCmain,1,4)=="G01J"|substr(H5$IPCmain,1,4)=="G01K"|substr(H5$IPCmain,1,4)=="G01L"|
                      substr(H5$IPCmain,1,4)=="G01M"|substr(H5$IPCmain,1,4)=="G01N"|substr(H5$IPCmain,1,4)=="G01P"|
                      substr(H5$IPCmain,1,4)=="G01R"|substr(H5$IPCmain,1,4)=="G01S"|substr(H5$IPCmain,1,4)=="G01V"|
                      substr(H5$IPCmain,1,4)=="G01W"|substr(H5$IPCmain,1,3)=="G04"|substr(H5$IPCmain,1,4)=="G12B"|
                      substr(H5$IPCmain,1,4)=="G99Z"~"J",
                   substr(H5$IPCmain,1,6)=="G01N033"~"K",
                   substr(H5$IPCmain,1,4)=="G05B"|substr(H5$IPCmain,1,4)=="G05D"|substr(H5$IPCmain,1,4)=="G05F"|
                      substr(H5$IPCmain,1,3)=="G07"|substr(H5$IPCmain,1,4)=="G08B"|substr(H5$IPCmain,1,4)=="G08G"|
                      substr(H5$IPCmain,1,4)=="G09B"|substr(H5$IPCmain,1,4)=="G09C"|substr(H5$IPCmain,1,4)=="G09D"~"L",
                   substr(H5$IPCmain,1,4)=="A61B"|substr(H5$IPCmain,1,4)=="A61C"|substr(H5$IPCmain,1,4)=="A61D"|
                      substr(H5$IPCmain,1,4)=="A61F"|substr(H5$IPCmain,1,4)=="A61G"|substr(H5$IPCmain,1,4)=="A61H"|
                      substr(H5$IPCmain,1,4)=="A61J"|substr(H5$IPCmain,1,4)=="A61L"|substr(H5$IPCmain,1,4)=="A61M"|
                      substr(H5$IPCmain,1,4)=="A61N"|substr(H5$IPCmain,1,4)=="H05G"~"M",
                   substr(H5$IPCmain,1,4)=="C07B"|substr(H5$IPCmain,1,4)=="C07C"|substr(H5$IPCmain,1,4)=="C07D"|
                      substr(H5$IPCmain,1,4)=="C07F"|substr(H5$IPCmain,1,4)=="C07H"|substr(H5$IPCmain,1,4)=="C07J"|
                      substr(H5$IPCmain,1,4)=="C40B"|substr(H5$IPCmain,1,7)=="A61K008"|substr(H5$IPCmain,1,4)=="A61Q"~"N",
                   substr(H5$IPCmain,1,4)=="C07G"|substr(H5$IPCmain,1,4)=="C07K"|substr(H5$IPCmain,1,4)=="C12M"|
                      substr(H5$IPCmain,1,4)=="C12N"|substr(H5$IPCmain,1,4)=="C12P"|substr(H5$IPCmain,1,4)=="C12Q"|
                      substr(H5$IPCmain,1,4)=="C12R"|substr(H5$IPCmain,1,4)=="C12S"~"O",
                   substr(H5$IPCmain,1,4)=="A61K"~"P",
                   substr(H5$IPCmain,1,4)=="C08B"|substr(H5$IPCmain,1,4)=="C08C"|substr(H5$IPCmain,1,4)=="C08F"|
                      substr(H5$IPCmain,1,4)=="C08G"|substr(H5$IPCmain,1,4)=="C08H"|substr(H5$IPCmain,1,4)=="C08K"|
                      substr(H5$IPCmain,1,4)=="C08L"~"Q",
                   substr(H5$IPCmain,1,4)=="A01H"|substr(H5$IPCmain,1,4)=="A21D"|substr(H5$IPCmain,1,4)=="A23B"|
                      substr(H5$IPCmain,1,4)=="A23C"|substr(H5$IPCmain,1,4)=="A23D"|substr(H5$IPCmain,1,4)=="A23F"|
                      substr(H5$IPCmain,1,4)=="A23G"|substr(H5$IPCmain,1,4)=="A23J"|substr(H5$IPCmain,1,4)=="A23K"|
                      substr(H5$IPCmain,1,4)=="A23L"|substr(H5$IPCmain,1,4)=="C12C"|substr(H5$IPCmain,1,4)=="C12F"|
                      substr(H5$IPCmain,1,4)=="C12G"|substr(H5$IPCmain,1,4)=="C12H"|substr(H5$IPCmain,1,4)=="C12J"|
                      substr(H5$IPCmain,1,4)=="C13D"|substr(H5$IPCmain,1,4)=="C13F"|substr(H5$IPCmain,1,4)=="C13J"|
                      substr(H5$IPCmain,1,4)=="C13K"~"R",
                   substr(H5$IPCmain,1,4)=="A01N"|substr(H5$IPCmain,1,4)=="A01P"|substr(H5$IPCmain,1,3)=="C05"|
                      substr(H5$IPCmain,1,3)=="C06"|substr(H5$IPCmain,1,4)=="C09B"|substr(H5$IPCmain,1,4)=="C09C"|
                      substr(H5$IPCmain,1,4)=="C09F"|substr(H5$IPCmain,1,4)=="C09G"|substr(H5$IPCmain,1,4)=="C09H"|
                      substr(H5$IPCmain,1,4)=="C09K"|substr(H5$IPCmain,1,4)=="C09D"|substr(H5$IPCmain,1,4)=="C09J"|
                      substr(H5$IPCmain,1,4)=="C10B"|substr(H5$IPCmain,1,4)=="C10C"|substr(H5$IPCmain,1,4)=="C10F"|
                      substr(H5$IPCmain,1,4)=="C10G"|substr(H5$IPCmain,1,4)=="C10H"|substr(H5$IPCmain,1,4)=="C10J"|
                      substr(H5$IPCmain,1,4)=="C10K"|substr(H5$IPCmain,1,4)=="C10L"|substr(H5$IPCmain,1,4)=="C10M"|
                      substr(H5$IPCmain,1,4)=="C10N"|substr(H5$IPCmain,1,4)=="C11B"|substr(H5$IPCmain,1,4)=="C11C"|
                      substr(H5$IPCmain,1,4)=="C11D"|substr(H5$IPCmain,1,4)=="C99Z"~"S",
                   substr(H5$IPCmain,1,3)=="C01"|substr(H5$IPCmain,1,4)=="C03C"|substr(H5$IPCmain,1,3)=="C04"|
                      substr(H5$IPCmain,1,3)=="C21"|substr(H5$IPCmain,1,3)=="C22"|substr(H5$IPCmain,1,3)=="B22"~"T",
                   substr(H5$IPCmain,1,4)=="B05C"|substr(H5$IPCmain,1,4)=="B05D"|substr(H5$IPCmain,1,3)=="B32"|
                      substr(H5$IPCmain,1,3)=="C23"|substr(H5$IPCmain,1,3)=="C25"|substr(H5$IPCmain,1,3)=="C30"~"U",
                   substr(H5$IPCmain,1,3)=="B81"|substr(H5$IPCmain,1,3)=="B82"~"V",
                   substr(H5$IPCmain,1,4)=="B01B"|substr(H5$IPCmain,1,7)=="B01D000"|substr(H5$IPCmain,1,6)=="B01D01"|
                      substr(H5$IPCmain,1,6)=="B01D02"|substr(H5$IPCmain,1,6)=="B01D03"|substr(H5$IPCmain,1,7)=="B01D041"|
                      substr(H5$IPCmain,1,7)=="B01D043"|substr(H5$IPCmain,1,7)=="B01D057"|substr(H5$IPCmain,1,7)=="B01D059"|
                      substr(H5$IPCmain,1,6)=="B01D06"|substr(H5$IPCmain,1,6)=="B01D07"|substr(H5$IPCmain,1,4)=="B01F"|
                      substr(H5$IPCmain,1,4)=="B01J"|substr(H5$IPCmain,1,4)=="B01L"|substr(H5$IPCmain,1,4)=="B02C"|
                      substr(H5$IPCmain,1,3)=="B03"|substr(H5$IPCmain,1,3)=="B04"|substr(H5$IPCmain,1,4)=="B05B"|
                      substr(H5$IPCmain,1,4)=="B06B"|substr(H5$IPCmain,1,3)=="B07"|substr(H5$IPCmain,1,3)=="B08"|
                      substr(H5$IPCmain,1,4)=="D06B"|substr(H5$IPCmain,1,4)=="D06C"|substr(H5$IPCmain,1,4)=="D06L"|
                      substr(H5$IPCmain,1,4)=="F25J"|substr(H5$IPCmain,1,3)=="F26"|substr(H5$IPCmain,1,4)=="C14C"|
                      substr(H5$IPCmain,1,4)=="H05H"~"W",
                   substr(H5$IPCmain,1,4)=="A62D"|substr(H5$IPCmain,1,7)=="B01D045"|substr(H5$IPCmain,1,7)=="B01D046"|
                      substr(H5$IPCmain,1,7)=="B01D047"|substr(H5$IPCmain,1,7)=="B01D049"|substr(H5$IPCmain,1,7)=="B01D050"|
                      substr(H5$IPCmain,1,7)=="B01D051"|substr(H5$IPCmain,1,7)=="B01D052"|substr(H5$IPCmain,1,7)=="B01D053"|
                      substr(H5$IPCmain,1,3)=="B09"|substr(H5$IPCmain,1,4)=="B65F"|substr(H5$IPCmain,1,3)=="C02"|
                      substr(H5$IPCmain,1,4)=="F01N"|substr(H5$IPCmain,1,4)=="F23G"|substr(H5$IPCmain,1,4)=="F23J"|
                      substr(H5$IPCmain,1,4)=="G01T"|substr(H5$IPCmain,1,7)=="E01F008"|substr(H5$IPCmain,1,4)=="A62C"~"X",
                   substr(H5$IPCmain,1,4)=="B25J"|substr(H5$IPCmain,1,4)=="B65B"|substr(H5$IPCmain,1,4)=="B65C"|
                      substr(H5$IPCmain,1,4)=="B65D"|substr(H5$IPCmain,1,4)=="B65G"|substr(H5$IPCmain,1,4)=="B65H"|
                      substr(H5$IPCmain,1,3)=="B66"|substr(H5$IPCmain,1,3)=="B67"~"Y",
                   substr(H5$IPCmain,1,3)=="B21"|substr(H5$IPCmain,1,3)=="B23"|substr(H5$IPCmain,1,3)=="B24"|
                      substr(H5$IPCmain,1,4)=="B26D"|substr(H5$IPCmain,1,4)=="B26F"|substr(H5$IPCmain,1,3)=="B27"|
                      substr(H5$IPCmain,1,3)=="B30"|substr(H5$IPCmain,1,4)=="B25B"|substr(H5$IPCmain,1,4)=="B25C"|
                      substr(H5$IPCmain,1,4)=="B25D"|substr(H5$IPCmain,1,4)=="B25F"|substr(H5$IPCmain,1,4)=="B25G"|
                      substr(H5$IPCmain,1,4)=="B25H"|substr(H5$IPCmain,1,4)=="B26B"~"Z",
                   substr(H5$IPCmain,1,4)=="F01B"|substr(H5$IPCmain,1,4)=="F01C"|substr(H5$IPCmain,1,4)=="F01D"|
                      substr(H5$IPCmain,1,4)=="F01K"|substr(H5$IPCmain,1,4)=="F01L"|substr(H5$IPCmain,1,4)=="F01M"|
                      substr(H5$IPCmain,1,4)=="F01P"|substr(H5$IPCmain,1,3)=="F02"|substr(H5$IPCmain,1,3)=="F03"|
                      substr(H5$IPCmain,1,3)=="F04"|substr(H5$IPCmain,1,4)=="F23R"|substr(H5$IPCmain,1,3)=="G21"|
                      substr(H5$IPCmain,1,4)=="F99Z"~"AA",
                   substr(H5$IPCmain,1,4)=="A41H"|substr(H5$IPCmain,1,4)=="A43D"|substr(H5$IPCmain,1,4)=="A46D"|
                      substr(H5$IPCmain,1,4)=="C14B"|substr(H5$IPCmain,1,3)=="D01"|substr(H5$IPCmain,1,3)=="D02"|
                      substr(H5$IPCmain,1,3)=="D03"|substr(H5$IPCmain,1,4)=="D04B"|substr(H5$IPCmain,1,4)=="D04C"|
                      substr(H5$IPCmain,1,4)=="D04G"|substr(H5$IPCmain,1,4)=="D04H"|substr(H5$IPCmain,1,4)=="D06J"|
                      substr(H5$IPCmain,1,4)=="D06M"|substr(H5$IPCmain,1,4)=="D06P"|substr(H5$IPCmain,1,4)=="D06Q"|
                      substr(H5$IPCmain,1,4)=="D99Z"|substr(H5$IPCmain,1,3)=="B31"|substr(H5$IPCmain,1,3)=="D21"|
                      substr(H5$IPCmain,1,3)=="B41"~"AB",
                   substr(H5$IPCmain,1,4)=="A01B"|substr(H5$IPCmain,1,4)=="A01C"|substr(H5$IPCmain,1,4)=="A01D"|
                      substr(H5$IPCmain,1,4)=="A01F"|substr(H5$IPCmain,1,4)=="A01G"|substr(H5$IPCmain,1,4)=="A01J"|
                      substr(H5$IPCmain,1,4)=="A01K"|substr(H5$IPCmain,1,4)=="A01L"|substr(H5$IPCmain,1,4)=="A01M"|
                      substr(H5$IPCmain,1,4)=="A21B"|substr(H5$IPCmain,1,4)=="A21C"|substr(H5$IPCmain,1,3)=="A22"|
                      substr(H5$IPCmain,1,4)=="A23N"|substr(H5$IPCmain,1,4)=="A23P"|substr(H5$IPCmain,1,4)=="B02B"|
                      substr(H5$IPCmain,1,4)=="C12L"|substr(H5$IPCmain,1,4)=="C13C"|substr(H5$IPCmain,1,4)=="C13G"|
                      substr(H5$IPCmain,1,4)=="C13H"|substr(H5$IPCmain,1,3)=="B28"|substr(H5$IPCmain,1,3)=="B29"|
                      substr(H5$IPCmain,1,4)=="C03B"|substr(H5$IPCmain,1,4)=="C08J"|substr(H5$IPCmain,1,4)=="B99Z"|
                      substr(H5$IPCmain,1,3)=="F41"|substr(H5$IPCmain,1,3)=="F42"~"AC",
                   substr(H5$IPCmain,1,3)=="F22"|substr(H5$IPCmain,1,4)=="F23B"|substr(H5$IPCmain,1,4)=="F23C"|
                      substr(H5$IPCmain,1,4)=="F23D"|substr(H5$IPCmain,1,4)=="F23H"|substr(H5$IPCmain,1,4)=="F23K"|
                      substr(H5$IPCmain,1,4)=="F23L"|substr(H5$IPCmain,1,4)=="F23M"|substr(H5$IPCmain,1,4)=="F23N"|
                      substr(H5$IPCmain,1,4)=="F23Q"|substr(H5$IPCmain,1,3)=="F24"|substr(H5$IPCmain,1,4)=="F25B"|
                      substr(H5$IPCmain,1,4)=="F25C"|substr(H5$IPCmain,1,3)=="F27"|substr(H5$IPCmain,1,3)=="F28"~"AD",
                   substr(H5$IPCmain,1,3)=="F15"|substr(H5$IPCmain,1,3)=="F16"|substr(H5$IPCmain,1,3)=="F17"|
                      substr(H5$IPCmain,1,4)=="G05G"~"AE",
                   substr(H5$IPCmain,1,3)=="B60"|substr(H5$IPCmain,1,3)=="B61"|substr(H5$IPCmain,1,3)=="B62"|
                      substr(H5$IPCmain,1,4)=="B63B"|substr(H5$IPCmain,1,4)=="B63C"|substr(H5$IPCmain,1,4)=="B63G"|
                      substr(H5$IPCmain,1,4)=="B63H"|substr(H5$IPCmain,1,4)=="B63J"|substr(H5$IPCmain,1,3)=="B64"~"AF",
                   substr(H5$IPCmain,1,3)=="A47"|substr(H5$IPCmain,1,3)=="A63"~"AG",
                   substr(H5$IPCmain,1,3)=="A24"|substr(H5$IPCmain,1,4)=="A41B"|substr(H5$IPCmain,1,4)=="A41C"|
                      substr(H5$IPCmain,1,4)=="A41D"|substr(H5$IPCmain,1,4)=="A41F"|substr(H5$IPCmain,1,4)=="A41G"|
                      substr(H5$IPCmain,1,3)=="A42"|substr(H5$IPCmain,1,4)=="A43B"|substr(H5$IPCmain,1,4)=="A43C"|
                      substr(H5$IPCmain,1,3)=="A44"|substr(H5$IPCmain,1,3)=="A45"|substr(H5$IPCmain,1,4)=="A46B"|
                      substr(H5$IPCmain,1,4)=="A62B"|substr(H5$IPCmain,1,3)=="B42"|substr(H5$IPCmain,1,3)=="B43"|
                      substr(H5$IPCmain,1,4)=="D04D"|substr(H5$IPCmain,1,3)=="D07"|substr(H5$IPCmain,1,4)=="G10B"|
                      substr(H5$IPCmain,1,4)=="G10C"|substr(H5$IPCmain,1,4)=="G10D"|substr(H5$IPCmain,1,4)=="G10F"|
                      substr(H5$IPCmain,1,4)=="G10G"|substr(H5$IPCmain,1,4)=="G10H"|substr(H5$IPCmain,1,4)=="G10K"|
                      substr(H5$IPCmain,1,3)=="B44"|substr(H5$IPCmain,1,3)=="B68"|substr(H5$IPCmain,1,4)=="D06F"|
                      substr(H5$IPCmain,1,4)=="D06N"|substr(H5$IPCmain,1,4)=="F25D"|substr(H5$IPCmain,1,4)=="A99Z"~"AH",
                   substr(H5$IPCmain,1,3)=="E02"|substr(H5$IPCmain,1,4)=="E01B"|substr(H5$IPCmain,1,4)=="E01C"|
                      substr(H5$IPCmain,1,4)=="E01C"|substr(H5$IPCmain,1,7)=="E01F001"|substr(H5$IPCmain,1,7)=="E01F003"|
                      substr(H5$IPCmain,1,7)=="E01F005"|substr(H5$IPCmain,1,7)=="E01F007"|substr(H5$IPCmain,1,7)=="E01F009"|
                      substr(H5$IPCmain,1,6)=="E01F01"|substr(H5$IPCmain,1,4)=="E01H"|substr(H5$IPCmain,1,3)=="E03"|
                      substr(H5$IPCmain,1,3)=="E04"|substr(H5$IPCmain,1,3)=="E05"|substr(H5$IPCmain,1,3)=="E06"|
                      substr(H5$IPCmain,1,3)=="E21"|substr(H5$IPCmain,1,4)=="E99Z"~"AI")

#Sum of different IPC Codes by company
H5$DivTech<-ave(H5$IPCmain,H5$GUO, FUN=function(x) length(unique(x))) #now with variety of IPC codes in area, otherwise total number of patents with H5$DivTech2<-ifelse(!is.na(H5$IPCmain),1,0)
H5$DivTech1<-ifelse(is.na(H5$IPCmain),0,H5$DivTech)
H5$DivTech1<-as.numeric(H5$DivTech1)

#Sum of different IPC Codes within each technical area in which the company is active 
H5$DivTech2<-ave(H5$IPCmain,H5[,c('GUO', 'Area')],FUN=function(x) length(unique(x)))
H5$DivTech2<-as.numeric(H5$DivTech2)

#Distribution of area patents among all patents
H5$DivTech3<-H5$DivTech2/H5$DivTech1

#Account for not classified patents here -- increases the final number immensely
H5$DivTech3[is.na(H5$DivTech3)] <- 0

#Remove duplicates to sum unique values
H5$Test<-paste(H5$GUO,H5$Area,H5$DivTech3,sep=" ")
H5<-H5[!duplicated(H5$Test),]

#Sum the shares, square them and substract from 1 = From concentration to diversification
H5$DivTech4<-ave(H5$DivTech3,H5$GUO,FUN=sum)
H5$DivTech5<-H5$DivTech4^2
H5$DivTech6<-1-H5$DivTech5

#Correct for the total number of patents (the measure is biased for small numbers)
H5$DivTech7<-ave(H5$DivTech1,H5$GUO,FUN=sum)
H5$DivTech8<-ifelse(H5$DivTech1==0 & H5$DivTech7!=0,NA,1)
H5<-subset(H5,!is.na(H5$DivTech8))
H5$DivTech<-H5$DivTech6*(1-1/H5$DivTech1) # different than in Rahko formular but makes more sense

H5$DivTech[H5$DivTech=="-Inf"] <-0

H5<-H5[,c(-2,-3,-4,-6,-7,-8,-9,-10,-11,-12,-13,-14)]
names(H5)<-c("BvDID","DivTech")

H5<-H5[!duplicated(H5$BvDID),]


#2016
Stock2016<-fread("files_created_code1/Stock2016.csv", dec=",")

Stock2016<-Stock2016[,c(-2,-3,-4,-5,-7,-8,-9)]
Full6<-fread("files_created_code1/Full6.csv", dec=",")
Full6[,3:33]<-NULL
names(Full6) <- c("GUO","Subsidiaries")
names(Stock2016) <- c("Subsidiaries","IPCmain")

H6<-left_join(Full6,Stock2016,by="Subsidiaries",na_matches="never")

H6$IPCmain<-as.character(H6$IPCmain)

#Filter for technological area of invention
H6$Area<-case_when(substr(H6$IPCmain,1,3)=="F21"~"A",
                   substr(H6$IPCmain,1,4)=="H01B"|substr(H6$IPCmain,1,4)=="H01C"|substr(H6$IPCmain,1,4)=="H01F"|
                      substr(H6$IPCmain,1,4)=="H01G"|substr(H6$IPCmain,1,4)=="H01H"|substr(H6$IPCmain,1,4)=="H01J"|
                      substr(H6$IPCmain,1,4)=="H01K"|substr(H6$IPCmain,1,4)=="H01M"|substr(H6$IPCmain,1,4)=="H01R"|
                      substr(H6$IPCmain,1,4)=="H01T"|substr(H6$IPCmain,1,3)=="H02"|substr(H6$IPCmain,1,4)=="H05B"|
                      substr(H6$IPCmain,1,4)=="H05C"|substr(H6$IPCmain,1,4)=="H05F"|substr(H6$IPCmain,1,4)=="H99Z"~"A",
                   substr(H6$IPCmain,1,4)=="G09F"|substr(H6$IPCmain,1,4)=="G09G"|substr(H6$IPCmain,1,4)=="G11B"|
                      substr(H6$IPCmain,1,6)=="H04N003"|substr(H6$IPCmain,1,6)=="H04N003"|substr(H6$IPCmain,1,6)=="H04N009"|
                      substr(H6$IPCmain,1,6)==" H04N013"|substr(H6$IPCmain,1,6)=="H04N015"|substr(H6$IPCmain,1,6)=="H04N017"|
                      substr(H6$IPCmain,1,4)=="H04R"|substr(H6$IPCmain,1,4)=="H04S"|substr(H6$IPCmain,1,4)=="H05K"~"B",
                   substr(H6$IPCmain,1,4)=="G08C"|substr(H6$IPCmain,1,4)=="H01P"|substr(H6$IPCmain,1,4)=="H01Q"|
                      substr(H6$IPCmain,1,4)=="H04B"|substr(H6$IPCmain,1,4)=="H04H"|substr(H6$IPCmain,1,4)==" H04J"|
                      substr(H6$IPCmain,1,4)=="H04K"|substr(H6$IPCmain,1,4)=="H04M"|substr(H6$IPCmain,1,6)=="H04N001"|
                      substr(H6$IPCmain,1,6)=="H04N007"|substr(H6$IPCmain,1,6)=="H04N011"|substr(H6$IPCmain,1,4)=="H04Q"~"C",
                   substr(H6$IPCmain,1,4)=="H04L"~"D",
                   substr(H6$IPCmain,1,3)=="H03"~"E",
                   substr(H6$IPCmain,1,3)=="G06"|substr(H6$IPCmain,1,4)=="G11C"|substr(H6$IPCmain,1,4)=="G10L"~"F",
                   substr(H6$IPCmain,1,4)=="G06Q"~"G",
                   substr(H6$IPCmain,1,4)=="H01L"~"H",
                   substr(H6$IPCmain,1,3)=="G02"|substr(H6$IPCmain,1,4)=="G03B"|substr(H6$IPCmain,1,4)=="G03C"|
                      substr(H6$IPCmain,1,6)=="G03D"|substr(H6$IPCmain,1,4)=="G03F"|substr(H6$IPCmain,1,4)=="G03G"|
                      substr(H6$IPCmain,1,4)=="G03H"|substr(H6$IPCmain,1,4)=="H01S"~"I",
                   substr(H6$IPCmain,1,4)=="G01B"|substr(H6$IPCmain,1,4)=="G01C"|substr(H6$IPCmain,1,4)=="G01D"|
                      substr(H6$IPCmain,1,4)=="G01F"|substr(H6$IPCmain,1,4)=="G01G"|substr(H6$IPCmain,1,4)=="G01H"|
                      substr(H6$IPCmain,1,4)=="G01J"|substr(H6$IPCmain,1,4)=="G01K"|substr(H6$IPCmain,1,4)=="G01L"|
                      substr(H6$IPCmain,1,4)=="G01M"|substr(H6$IPCmain,1,4)=="G01N"|substr(H6$IPCmain,1,4)=="G01P"|
                      substr(H6$IPCmain,1,4)=="G01R"|substr(H6$IPCmain,1,4)=="G01S"|substr(H6$IPCmain,1,4)=="G01V"|
                      substr(H6$IPCmain,1,4)=="G01W"|substr(H6$IPCmain,1,3)=="G04"|substr(H6$IPCmain,1,4)=="G12B"|
                      substr(H6$IPCmain,1,4)=="G99Z"~"J",
                   substr(H6$IPCmain,1,6)=="G01N033"~"K",
                   substr(H6$IPCmain,1,4)=="G05B"|substr(H6$IPCmain,1,4)=="G05D"|substr(H6$IPCmain,1,4)=="G05F"|
                      substr(H6$IPCmain,1,3)=="G07"|substr(H6$IPCmain,1,4)=="G08B"|substr(H6$IPCmain,1,4)=="G08G"|
                      substr(H6$IPCmain,1,4)=="G09B"|substr(H6$IPCmain,1,4)=="G09C"|substr(H6$IPCmain,1,4)=="G09D"~"L",
                   substr(H6$IPCmain,1,4)=="A61B"|substr(H6$IPCmain,1,4)=="A61C"|substr(H6$IPCmain,1,4)=="A61D"|
                      substr(H6$IPCmain,1,4)=="A61F"|substr(H6$IPCmain,1,4)=="A61G"|substr(H6$IPCmain,1,4)=="A61H"|
                      substr(H6$IPCmain,1,4)=="A61J"|substr(H6$IPCmain,1,4)=="A61L"|substr(H6$IPCmain,1,4)=="A61M"|
                      substr(H6$IPCmain,1,4)=="A61N"|substr(H6$IPCmain,1,4)=="H05G"~"M",
                   substr(H6$IPCmain,1,4)=="C07B"|substr(H6$IPCmain,1,4)=="C07C"|substr(H6$IPCmain,1,4)=="C07D"|
                      substr(H6$IPCmain,1,4)=="C07F"|substr(H6$IPCmain,1,4)=="C07H"|substr(H6$IPCmain,1,4)=="C07J"|
                      substr(H6$IPCmain,1,4)=="C40B"|substr(H6$IPCmain,1,7)=="A61K008"|substr(H6$IPCmain,1,4)=="A61Q"~"N",
                   substr(H6$IPCmain,1,4)=="C07G"|substr(H6$IPCmain,1,4)=="C07K"|substr(H6$IPCmain,1,4)=="C12M"|
                      substr(H6$IPCmain,1,4)=="C12N"|substr(H6$IPCmain,1,4)=="C12P"|substr(H6$IPCmain,1,4)=="C12Q"|
                      substr(H6$IPCmain,1,4)=="C12R"|substr(H6$IPCmain,1,4)=="C12S"~"O",
                   substr(H6$IPCmain,1,4)=="A61K"~"P",
                   substr(H6$IPCmain,1,4)=="C08B"|substr(H6$IPCmain,1,4)=="C08C"|substr(H6$IPCmain,1,4)=="C08F"|
                      substr(H6$IPCmain,1,4)=="C08G"|substr(H6$IPCmain,1,4)=="C08H"|substr(H6$IPCmain,1,4)=="C08K"|
                      substr(H6$IPCmain,1,4)=="C08L"~"Q",
                   substr(H6$IPCmain,1,4)=="A01H"|substr(H6$IPCmain,1,4)=="A21D"|substr(H6$IPCmain,1,4)=="A23B"|
                      substr(H6$IPCmain,1,4)=="A23C"|substr(H6$IPCmain,1,4)=="A23D"|substr(H6$IPCmain,1,4)=="A23F"|
                      substr(H6$IPCmain,1,4)=="A23G"|substr(H6$IPCmain,1,4)=="A23J"|substr(H6$IPCmain,1,4)=="A23K"|
                      substr(H6$IPCmain,1,4)=="A23L"|substr(H6$IPCmain,1,4)=="C12C"|substr(H6$IPCmain,1,4)=="C12F"|
                      substr(H6$IPCmain,1,4)=="C12G"|substr(H6$IPCmain,1,4)=="C12H"|substr(H6$IPCmain,1,4)=="C12J"|
                      substr(H6$IPCmain,1,4)=="C13D"|substr(H6$IPCmain,1,4)=="C13F"|substr(H6$IPCmain,1,4)=="C13J"|
                      substr(H6$IPCmain,1,4)=="C13K"~"R",
                   substr(H6$IPCmain,1,4)=="A01N"|substr(H6$IPCmain,1,4)=="A01P"|substr(H6$IPCmain,1,3)=="C05"|
                      substr(H6$IPCmain,1,3)=="C06"|substr(H6$IPCmain,1,4)=="C09B"|substr(H6$IPCmain,1,4)=="C09C"|
                      substr(H6$IPCmain,1,4)=="C09F"|substr(H6$IPCmain,1,4)=="C09G"|substr(H6$IPCmain,1,4)=="C09H"|
                      substr(H6$IPCmain,1,4)=="C09K"|substr(H6$IPCmain,1,4)=="C09D"|substr(H6$IPCmain,1,4)=="C09J"|
                      substr(H6$IPCmain,1,4)=="C10B"|substr(H6$IPCmain,1,4)=="C10C"|substr(H6$IPCmain,1,4)=="C10F"|
                      substr(H6$IPCmain,1,4)=="C10G"|substr(H6$IPCmain,1,4)=="C10H"|substr(H6$IPCmain,1,4)=="C10J"|
                      substr(H6$IPCmain,1,4)=="C10K"|substr(H6$IPCmain,1,4)=="C10L"|substr(H6$IPCmain,1,4)=="C10M"|
                      substr(H6$IPCmain,1,4)=="C10N"|substr(H6$IPCmain,1,4)=="C11B"|substr(H6$IPCmain,1,4)=="C11C"|
                      substr(H6$IPCmain,1,4)=="C11D"|substr(H6$IPCmain,1,4)=="C99Z"~"S",
                   substr(H6$IPCmain,1,3)=="C01"|substr(H6$IPCmain,1,4)=="C03C"|substr(H6$IPCmain,1,3)=="C04"|
                      substr(H6$IPCmain,1,3)=="C21"|substr(H6$IPCmain,1,3)=="C22"|substr(H6$IPCmain,1,3)=="B22"~"T",
                   substr(H6$IPCmain,1,4)=="B05C"|substr(H6$IPCmain,1,4)=="B05D"|substr(H6$IPCmain,1,3)=="B32"|
                      substr(H6$IPCmain,1,3)=="C23"|substr(H6$IPCmain,1,3)=="C25"|substr(H6$IPCmain,1,3)=="C30"~"U",
                   substr(H6$IPCmain,1,3)=="B81"|substr(H6$IPCmain,1,3)=="B82"~"V",
                   substr(H6$IPCmain,1,4)=="B01B"|substr(H6$IPCmain,1,7)=="B01D000"|substr(H6$IPCmain,1,6)=="B01D01"|
                      substr(H6$IPCmain,1,6)=="B01D02"|substr(H6$IPCmain,1,6)=="B01D03"|substr(H6$IPCmain,1,7)=="B01D041"|
                      substr(H6$IPCmain,1,7)=="B01D043"|substr(H6$IPCmain,1,7)=="B01D057"|substr(H6$IPCmain,1,7)=="B01D059"|
                      substr(H6$IPCmain,1,6)=="B01D06"|substr(H6$IPCmain,1,6)=="B01D07"|substr(H6$IPCmain,1,4)=="B01F"|
                      substr(H6$IPCmain,1,4)=="B01J"|substr(H6$IPCmain,1,4)=="B01L"|substr(H6$IPCmain,1,4)=="B02C"|
                      substr(H6$IPCmain,1,3)=="B03"|substr(H6$IPCmain,1,3)=="B04"|substr(H6$IPCmain,1,4)=="B05B"|
                      substr(H6$IPCmain,1,4)=="B06B"|substr(H6$IPCmain,1,3)=="B07"|substr(H6$IPCmain,1,3)=="B08"|
                      substr(H6$IPCmain,1,4)=="D06B"|substr(H6$IPCmain,1,4)=="D06C"|substr(H6$IPCmain,1,4)=="D06L"|
                      substr(H6$IPCmain,1,4)=="F25J"|substr(H6$IPCmain,1,3)=="F26"|substr(H6$IPCmain,1,4)=="C14C"|
                      substr(H6$IPCmain,1,4)=="H05H"~"W",
                   substr(H6$IPCmain,1,4)=="A62D"|substr(H6$IPCmain,1,7)=="B01D045"|substr(H6$IPCmain,1,7)=="B01D046"|
                      substr(H6$IPCmain,1,7)=="B01D047"|substr(H6$IPCmain,1,7)=="B01D049"|substr(H6$IPCmain,1,7)=="B01D050"|
                      substr(H6$IPCmain,1,7)=="B01D051"|substr(H6$IPCmain,1,7)=="B01D052"|substr(H6$IPCmain,1,7)=="B01D053"|
                      substr(H6$IPCmain,1,3)=="B09"|substr(H6$IPCmain,1,4)=="B65F"|substr(H6$IPCmain,1,3)=="C02"|
                      substr(H6$IPCmain,1,4)=="F01N"|substr(H6$IPCmain,1,4)=="F23G"|substr(H6$IPCmain,1,4)=="F23J"|
                      substr(H6$IPCmain,1,4)=="G01T"|substr(H6$IPCmain,1,7)=="E01F008"|substr(H6$IPCmain,1,4)=="A62C"~"X",
                   substr(H6$IPCmain,1,4)=="B25J"|substr(H6$IPCmain,1,4)=="B65B"|substr(H6$IPCmain,1,4)=="B65C"|
                      substr(H6$IPCmain,1,4)=="B65D"|substr(H6$IPCmain,1,4)=="B65G"|substr(H6$IPCmain,1,4)=="B65H"|
                      substr(H6$IPCmain,1,3)=="B66"|substr(H6$IPCmain,1,3)=="B67"~"Y",
                   substr(H6$IPCmain,1,3)=="B21"|substr(H6$IPCmain,1,3)=="B23"|substr(H6$IPCmain,1,3)=="B24"|
                      substr(H6$IPCmain,1,4)=="B26D"|substr(H6$IPCmain,1,4)=="B26F"|substr(H6$IPCmain,1,3)=="B27"|
                      substr(H6$IPCmain,1,3)=="B30"|substr(H6$IPCmain,1,4)=="B25B"|substr(H6$IPCmain,1,4)=="B25C"|
                      substr(H6$IPCmain,1,4)=="B25D"|substr(H6$IPCmain,1,4)=="B25F"|substr(H6$IPCmain,1,4)=="B25G"|
                      substr(H6$IPCmain,1,4)=="B25H"|substr(H6$IPCmain,1,4)=="B26B"~"Z",
                   substr(H6$IPCmain,1,4)=="F01B"|substr(H6$IPCmain,1,4)=="F01C"|substr(H6$IPCmain,1,4)=="F01D"|
                      substr(H6$IPCmain,1,4)=="F01K"|substr(H6$IPCmain,1,4)=="F01L"|substr(H6$IPCmain,1,4)=="F01M"|
                      substr(H6$IPCmain,1,4)=="F01P"|substr(H6$IPCmain,1,3)=="F02"|substr(H6$IPCmain,1,3)=="F03"|
                      substr(H6$IPCmain,1,3)=="F04"|substr(H6$IPCmain,1,4)=="F23R"|substr(H6$IPCmain,1,3)=="G21"|
                      substr(H6$IPCmain,1,4)=="F99Z"~"AA",
                   substr(H6$IPCmain,1,4)=="A41H"|substr(H6$IPCmain,1,4)=="A43D"|substr(H6$IPCmain,1,4)=="A46D"|
                      substr(H6$IPCmain,1,4)=="C14B"|substr(H6$IPCmain,1,3)=="D01"|substr(H6$IPCmain,1,3)=="D02"|
                      substr(H6$IPCmain,1,3)=="D03"|substr(H6$IPCmain,1,4)=="D04B"|substr(H6$IPCmain,1,4)=="D04C"|
                      substr(H6$IPCmain,1,4)=="D04G"|substr(H6$IPCmain,1,4)=="D04H"|substr(H6$IPCmain,1,4)=="D06J"|
                      substr(H6$IPCmain,1,4)=="D06M"|substr(H6$IPCmain,1,4)=="D06P"|substr(H6$IPCmain,1,4)=="D06Q"|
                      substr(H6$IPCmain,1,4)=="D99Z"|substr(H6$IPCmain,1,3)=="B31"|substr(H6$IPCmain,1,3)=="D21"|
                      substr(H6$IPCmain,1,3)=="B41"~"AB",
                   substr(H6$IPCmain,1,4)=="A01B"|substr(H6$IPCmain,1,4)=="A01C"|substr(H6$IPCmain,1,4)=="A01D"|
                      substr(H6$IPCmain,1,4)=="A01F"|substr(H6$IPCmain,1,4)=="A01G"|substr(H6$IPCmain,1,4)=="A01J"|
                      substr(H6$IPCmain,1,4)=="A01K"|substr(H6$IPCmain,1,4)=="A01L"|substr(H6$IPCmain,1,4)=="A01M"|
                      substr(H6$IPCmain,1,4)=="A21B"|substr(H6$IPCmain,1,4)=="A21C"|substr(H6$IPCmain,1,3)=="A22"|
                      substr(H6$IPCmain,1,4)=="A23N"|substr(H6$IPCmain,1,4)=="A23P"|substr(H6$IPCmain,1,4)=="B02B"|
                      substr(H6$IPCmain,1,4)=="C12L"|substr(H6$IPCmain,1,4)=="C13C"|substr(H6$IPCmain,1,4)=="C13G"|
                      substr(H6$IPCmain,1,4)=="C13H"|substr(H6$IPCmain,1,3)=="B28"|substr(H6$IPCmain,1,3)=="B29"|
                      substr(H6$IPCmain,1,4)=="C03B"|substr(H6$IPCmain,1,4)=="C08J"|substr(H6$IPCmain,1,4)=="B99Z"|
                      substr(H6$IPCmain,1,3)=="F41"|substr(H6$IPCmain,1,3)=="F42"~"AC",
                   substr(H6$IPCmain,1,3)=="F22"|substr(H6$IPCmain,1,4)=="F23B"|substr(H6$IPCmain,1,4)=="F23C"|
                      substr(H6$IPCmain,1,4)=="F23D"|substr(H6$IPCmain,1,4)=="F23H"|substr(H6$IPCmain,1,4)=="F23K"|
                      substr(H6$IPCmain,1,4)=="F23L"|substr(H6$IPCmain,1,4)=="F23M"|substr(H6$IPCmain,1,4)=="F23N"|
                      substr(H6$IPCmain,1,4)=="F23Q"|substr(H6$IPCmain,1,3)=="F24"|substr(H6$IPCmain,1,4)=="F25B"|
                      substr(H6$IPCmain,1,4)=="F25C"|substr(H6$IPCmain,1,3)=="F27"|substr(H6$IPCmain,1,3)=="F28"~"AD",
                   substr(H6$IPCmain,1,3)=="F15"|substr(H6$IPCmain,1,3)=="F16"|substr(H6$IPCmain,1,3)=="F17"|
                      substr(H6$IPCmain,1,4)=="G05G"~"AE",
                   substr(H6$IPCmain,1,3)=="B60"|substr(H6$IPCmain,1,3)=="B61"|substr(H6$IPCmain,1,3)=="B62"|
                      substr(H6$IPCmain,1,4)=="B63B"|substr(H6$IPCmain,1,4)=="B63C"|substr(H6$IPCmain,1,4)=="B63G"|
                      substr(H6$IPCmain,1,4)=="B63H"|substr(H6$IPCmain,1,4)=="B63J"|substr(H6$IPCmain,1,3)=="B64"~"AF",
                   substr(H6$IPCmain,1,3)=="A47"|substr(H6$IPCmain,1,3)=="A63"~"AG",
                   substr(H6$IPCmain,1,3)=="A24"|substr(H6$IPCmain,1,4)=="A41B"|substr(H6$IPCmain,1,4)=="A41C"|
                      substr(H6$IPCmain,1,4)=="A41D"|substr(H6$IPCmain,1,4)=="A41F"|substr(H6$IPCmain,1,4)=="A41G"|
                      substr(H6$IPCmain,1,3)=="A42"|substr(H6$IPCmain,1,4)=="A43B"|substr(H6$IPCmain,1,4)=="A43C"|
                      substr(H6$IPCmain,1,3)=="A44"|substr(H6$IPCmain,1,3)=="A45"|substr(H6$IPCmain,1,4)=="A46B"|
                      substr(H6$IPCmain,1,4)=="A62B"|substr(H6$IPCmain,1,3)=="B42"|substr(H6$IPCmain,1,3)=="B43"|
                      substr(H6$IPCmain,1,4)=="D04D"|substr(H6$IPCmain,1,3)=="D07"|substr(H6$IPCmain,1,4)=="G10B"|
                      substr(H6$IPCmain,1,4)=="G10C"|substr(H6$IPCmain,1,4)=="G10D"|substr(H6$IPCmain,1,4)=="G10F"|
                      substr(H6$IPCmain,1,4)=="G10G"|substr(H6$IPCmain,1,4)=="G10H"|substr(H6$IPCmain,1,4)=="G10K"|
                      substr(H6$IPCmain,1,3)=="B44"|substr(H6$IPCmain,1,3)=="B68"|substr(H6$IPCmain,1,4)=="D06F"|
                      substr(H6$IPCmain,1,4)=="D06N"|substr(H6$IPCmain,1,4)=="F25D"|substr(H6$IPCmain,1,4)=="A99Z"~"AH",
                   substr(H6$IPCmain,1,3)=="E02"|substr(H6$IPCmain,1,4)=="E01B"|substr(H6$IPCmain,1,4)=="E01C"|
                      substr(H6$IPCmain,1,4)=="E01C"|substr(H6$IPCmain,1,7)=="E01F001"|substr(H6$IPCmain,1,7)=="E01F003"|
                      substr(H6$IPCmain,1,7)=="E01F005"|substr(H6$IPCmain,1,7)=="E01F007"|substr(H6$IPCmain,1,7)=="E01F009"|
                      substr(H6$IPCmain,1,6)=="E01F01"|substr(H6$IPCmain,1,4)=="E01H"|substr(H6$IPCmain,1,3)=="E03"|
                      substr(H6$IPCmain,1,3)=="E04"|substr(H6$IPCmain,1,3)=="E05"|substr(H6$IPCmain,1,3)=="E06"|
                      substr(H6$IPCmain,1,3)=="E21"|substr(H6$IPCmain,1,4)=="E99Z"~"AI")

#Sum of different IPC Codes by company
H6$DivTech<-ave(H6$IPCmain,H6$GUO, FUN=function(x) length(unique(x))) #now with variety of IPC codes in area, otherwise total number of patents with H6$DivTech2<-ifelse(!is.na(H6$IPCmain),1,0)
H6$DivTech1<-ifelse(is.na(H6$IPCmain),0,H6$DivTech)
H6$DivTech1<-as.numeric(H6$DivTech1)

#Sum of different IPC Codes within each technical area in which the company is active 
H6$DivTech2<-ave(H6$IPCmain,H6[,c('GUO', 'Area')],FUN=function(x) length(unique(x)))
H6$DivTech2<-as.numeric(H6$DivTech2)

#Distribution of area patents among all patents
H6$DivTech3<-H6$DivTech2/H6$DivTech1

#Account for not classified patents here -- increases the final number immensely
H6$DivTech3[is.na(H6$DivTech3)] <- 0

# Test Company: CL869632007

#Remove duplicates to sum unique values
H6$Test<-paste(H6$GUO,H6$Area,H6$DivTech3,sep=" ")
H6<-H6[!duplicated(H6$Test),]

#Sum the shares, square them and substract from 1 = From concentration to diversification
H6$DivTech4<-ave(H6$DivTech3,H6$GUO,FUN=sum)
H6$DivTech5<-H6$DivTech4^2
H6$DivTech6<-1-H6$DivTech5

#Correct for the total number of patents (the measure is biased for small numbers)
H6$DivTech7<-ave(H6$DivTech1,H6$GUO,FUN=sum)
H6$DivTech8<-ifelse(H6$DivTech1==0 & H6$DivTech7!=0,NA,1)
H6<-subset(H6,!is.na(H6$DivTech8))
H6$DivTech<-H6$DivTech6*(1-1/H6$DivTech1) # different than in Rahko formular but makes more sense

H6$DivTech[H6$DivTech=="-Inf"] <-0

H6<-H6[,c(-2,-3,-4,-6,-7,-8,-9,-10,-11,-12,-13,-14)]
names(H6)<-c("BvDID","DivTech")

H6<-H6[!duplicated(H6$BvDID),]


#2017
Stock2017<-fread("files_created_code1/Stock2017.csv", dec=",")

Stock2017<-Stock2017[,c(-2,-3,-4,-5,-7,-8,-9)]
Full7<-fread("files_created_code1/Full7.csv", dec=",")
Full7[,3:33]<-NULL
names(Full7) <- c("GUO","Subsidiaries")
names(Stock2017) <- c("Subsidiaries","IPCmain")

H7<-left_join(Full7,Stock2017,by="Subsidiaries",na_matches="never")

H7$IPCmain<-as.character(H7$IPCmain)

#Filter for technological area of invention
H7$Area<-case_when(substr(H7$IPCmain,1,3)=="F21"~"A",
                   substr(H7$IPCmain,1,4)=="H01B"|substr(H7$IPCmain,1,4)=="H01C"|substr(H7$IPCmain,1,4)=="H01F"|
                      substr(H7$IPCmain,1,4)=="H01G"|substr(H7$IPCmain,1,4)=="H01H"|substr(H7$IPCmain,1,4)=="H01J"|
                      substr(H7$IPCmain,1,4)=="H01K"|substr(H7$IPCmain,1,4)=="H01M"|substr(H7$IPCmain,1,4)=="H01R"|
                      substr(H7$IPCmain,1,4)=="H01T"|substr(H7$IPCmain,1,3)=="H02"|substr(H7$IPCmain,1,4)=="H05B"|
                      substr(H7$IPCmain,1,4)=="H05C"|substr(H7$IPCmain,1,4)=="H05F"|substr(H7$IPCmain,1,4)=="H99Z"~"A",
                   substr(H7$IPCmain,1,4)=="G09F"|substr(H7$IPCmain,1,4)=="G09G"|substr(H7$IPCmain,1,4)=="G11B"|
                      substr(H7$IPCmain,1,6)=="H04N003"|substr(H7$IPCmain,1,6)=="H04N003"|substr(H7$IPCmain,1,6)=="H04N009"|
                      substr(H7$IPCmain,1,6)==" H04N013"|substr(H7$IPCmain,1,6)=="H04N015"|substr(H7$IPCmain,1,6)=="H04N017"|
                      substr(H7$IPCmain,1,4)=="H04R"|substr(H7$IPCmain,1,4)=="H04S"|substr(H7$IPCmain,1,4)=="H05K"~"B",
                   substr(H7$IPCmain,1,4)=="G08C"|substr(H7$IPCmain,1,4)=="H01P"|substr(H7$IPCmain,1,4)=="H01Q"|
                      substr(H7$IPCmain,1,4)=="H04B"|substr(H7$IPCmain,1,4)=="H04H"|substr(H7$IPCmain,1,4)==" H04J"|
                      substr(H7$IPCmain,1,4)=="H04K"|substr(H7$IPCmain,1,4)=="H04M"|substr(H7$IPCmain,1,6)=="H04N001"|
                      substr(H7$IPCmain,1,6)=="H04N007"|substr(H7$IPCmain,1,6)=="H04N011"|substr(H7$IPCmain,1,4)=="H04Q"~"C",
                   substr(H7$IPCmain,1,4)=="H04L"~"D",
                   substr(H7$IPCmain,1,3)=="H03"~"E",
                   substr(H7$IPCmain,1,3)=="G06"|substr(H7$IPCmain,1,4)=="G11C"|substr(H7$IPCmain,1,4)=="G10L"~"F",
                   substr(H7$IPCmain,1,4)=="G06Q"~"G",
                   substr(H7$IPCmain,1,4)=="H01L"~"H",
                   substr(H7$IPCmain,1,3)=="G02"|substr(H7$IPCmain,1,4)=="G03B"|substr(H7$IPCmain,1,4)=="G03C"|
                      substr(H7$IPCmain,1,6)=="G03D"|substr(H7$IPCmain,1,4)=="G03F"|substr(H7$IPCmain,1,4)=="G03G"|
                      substr(H7$IPCmain,1,4)=="G03H"|substr(H7$IPCmain,1,4)=="H01S"~"I",
                   substr(H7$IPCmain,1,4)=="G01B"|substr(H7$IPCmain,1,4)=="G01C"|substr(H7$IPCmain,1,4)=="G01D"|
                      substr(H7$IPCmain,1,4)=="G01F"|substr(H7$IPCmain,1,4)=="G01G"|substr(H7$IPCmain,1,4)=="G01H"|
                      substr(H7$IPCmain,1,4)=="G01J"|substr(H7$IPCmain,1,4)=="G01K"|substr(H7$IPCmain,1,4)=="G01L"|
                      substr(H7$IPCmain,1,4)=="G01M"|substr(H7$IPCmain,1,4)=="G01N"|substr(H7$IPCmain,1,4)=="G01P"|
                      substr(H7$IPCmain,1,4)=="G01R"|substr(H7$IPCmain,1,4)=="G01S"|substr(H7$IPCmain,1,4)=="G01V"|
                      substr(H7$IPCmain,1,4)=="G01W"|substr(H7$IPCmain,1,3)=="G04"|substr(H7$IPCmain,1,4)=="G12B"|
                      substr(H7$IPCmain,1,4)=="G99Z"~"J",
                   substr(H7$IPCmain,1,6)=="G01N033"~"K",
                   substr(H7$IPCmain,1,4)=="G05B"|substr(H7$IPCmain,1,4)=="G05D"|substr(H7$IPCmain,1,4)=="G05F"|
                      substr(H7$IPCmain,1,3)=="G07"|substr(H7$IPCmain,1,4)=="G08B"|substr(H7$IPCmain,1,4)=="G08G"|
                      substr(H7$IPCmain,1,4)=="G09B"|substr(H7$IPCmain,1,4)=="G09C"|substr(H7$IPCmain,1,4)=="G09D"~"L",
                   substr(H7$IPCmain,1,4)=="A61B"|substr(H7$IPCmain,1,4)=="A61C"|substr(H7$IPCmain,1,4)=="A61D"|
                      substr(H7$IPCmain,1,4)=="A61F"|substr(H7$IPCmain,1,4)=="A61G"|substr(H7$IPCmain,1,4)=="A61H"|
                      substr(H7$IPCmain,1,4)=="A61J"|substr(H7$IPCmain,1,4)=="A61L"|substr(H7$IPCmain,1,4)=="A61M"|
                      substr(H7$IPCmain,1,4)=="A61N"|substr(H7$IPCmain,1,4)=="H05G"~"M",
                   substr(H7$IPCmain,1,4)=="C07B"|substr(H7$IPCmain,1,4)=="C07C"|substr(H7$IPCmain,1,4)=="C07D"|
                      substr(H7$IPCmain,1,4)=="C07F"|substr(H7$IPCmain,1,4)=="C07H"|substr(H7$IPCmain,1,4)=="C07J"|
                      substr(H7$IPCmain,1,4)=="C40B"|substr(H7$IPCmain,1,7)=="A61K008"|substr(H7$IPCmain,1,4)=="A61Q"~"N",
                   substr(H7$IPCmain,1,4)=="C07G"|substr(H7$IPCmain,1,4)=="C07K"|substr(H7$IPCmain,1,4)=="C12M"|
                      substr(H7$IPCmain,1,4)=="C12N"|substr(H7$IPCmain,1,4)=="C12P"|substr(H7$IPCmain,1,4)=="C12Q"|
                      substr(H7$IPCmain,1,4)=="C12R"|substr(H7$IPCmain,1,4)=="C12S"~"O",
                   substr(H7$IPCmain,1,4)=="A61K"~"P",
                   substr(H7$IPCmain,1,4)=="C08B"|substr(H7$IPCmain,1,4)=="C08C"|substr(H7$IPCmain,1,4)=="C08F"|
                      substr(H7$IPCmain,1,4)=="C08G"|substr(H7$IPCmain,1,4)=="C08H"|substr(H7$IPCmain,1,4)=="C08K"|
                      substr(H7$IPCmain,1,4)=="C08L"~"Q",
                   substr(H7$IPCmain,1,4)=="A01H"|substr(H7$IPCmain,1,4)=="A21D"|substr(H7$IPCmain,1,4)=="A23B"|
                      substr(H7$IPCmain,1,4)=="A23C"|substr(H7$IPCmain,1,4)=="A23D"|substr(H7$IPCmain,1,4)=="A23F"|
                      substr(H7$IPCmain,1,4)=="A23G"|substr(H7$IPCmain,1,4)=="A23J"|substr(H7$IPCmain,1,4)=="A23K"|
                      substr(H7$IPCmain,1,4)=="A23L"|substr(H7$IPCmain,1,4)=="C12C"|substr(H7$IPCmain,1,4)=="C12F"|
                      substr(H7$IPCmain,1,4)=="C12G"|substr(H7$IPCmain,1,4)=="C12H"|substr(H7$IPCmain,1,4)=="C12J"|
                      substr(H7$IPCmain,1,4)=="C13D"|substr(H7$IPCmain,1,4)=="C13F"|substr(H7$IPCmain,1,4)=="C13J"|
                      substr(H7$IPCmain,1,4)=="C13K"~"R",
                   substr(H7$IPCmain,1,4)=="A01N"|substr(H7$IPCmain,1,4)=="A01P"|substr(H7$IPCmain,1,3)=="C05"|
                      substr(H7$IPCmain,1,3)=="C06"|substr(H7$IPCmain,1,4)=="C09B"|substr(H7$IPCmain,1,4)=="C09C"|
                      substr(H7$IPCmain,1,4)=="C09F"|substr(H7$IPCmain,1,4)=="C09G"|substr(H7$IPCmain,1,4)=="C09H"|
                      substr(H7$IPCmain,1,4)=="C09K"|substr(H7$IPCmain,1,4)=="C09D"|substr(H7$IPCmain,1,4)=="C09J"|
                      substr(H7$IPCmain,1,4)=="C10B"|substr(H7$IPCmain,1,4)=="C10C"|substr(H7$IPCmain,1,4)=="C10F"|
                      substr(H7$IPCmain,1,4)=="C10G"|substr(H7$IPCmain,1,4)=="C10H"|substr(H7$IPCmain,1,4)=="C10J"|
                      substr(H7$IPCmain,1,4)=="C10K"|substr(H7$IPCmain,1,4)=="C10L"|substr(H7$IPCmain,1,4)=="C10M"|
                      substr(H7$IPCmain,1,4)=="C10N"|substr(H7$IPCmain,1,4)=="C11B"|substr(H7$IPCmain,1,4)=="C11C"|
                      substr(H7$IPCmain,1,4)=="C11D"|substr(H7$IPCmain,1,4)=="C99Z"~"S",
                   substr(H7$IPCmain,1,3)=="C01"|substr(H7$IPCmain,1,4)=="C03C"|substr(H7$IPCmain,1,3)=="C04"|
                      substr(H7$IPCmain,1,3)=="C21"|substr(H7$IPCmain,1,3)=="C22"|substr(H7$IPCmain,1,3)=="B22"~"T",
                   substr(H7$IPCmain,1,4)=="B05C"|substr(H7$IPCmain,1,4)=="B05D"|substr(H7$IPCmain,1,3)=="B32"|
                      substr(H7$IPCmain,1,3)=="C23"|substr(H7$IPCmain,1,3)=="C25"|substr(H7$IPCmain,1,3)=="C30"~"U",
                   substr(H7$IPCmain,1,3)=="B81"|substr(H7$IPCmain,1,3)=="B82"~"V",
                   substr(H7$IPCmain,1,4)=="B01B"|substr(H7$IPCmain,1,7)=="B01D000"|substr(H7$IPCmain,1,6)=="B01D01"|
                      substr(H7$IPCmain,1,6)=="B01D02"|substr(H7$IPCmain,1,6)=="B01D03"|substr(H7$IPCmain,1,7)=="B01D041"|
                      substr(H7$IPCmain,1,7)=="B01D043"|substr(H7$IPCmain,1,7)=="B01D057"|substr(H7$IPCmain,1,7)=="B01D059"|
                      substr(H7$IPCmain,1,6)=="B01D06"|substr(H7$IPCmain,1,6)=="B01D07"|substr(H7$IPCmain,1,4)=="B01F"|
                      substr(H7$IPCmain,1,4)=="B01J"|substr(H7$IPCmain,1,4)=="B01L"|substr(H7$IPCmain,1,4)=="B02C"|
                      substr(H7$IPCmain,1,3)=="B03"|substr(H7$IPCmain,1,3)=="B04"|substr(H7$IPCmain,1,4)=="B05B"|
                      substr(H7$IPCmain,1,4)=="B06B"|substr(H7$IPCmain,1,3)=="B07"|substr(H7$IPCmain,1,3)=="B08"|
                      substr(H7$IPCmain,1,4)=="D06B"|substr(H7$IPCmain,1,4)=="D06C"|substr(H7$IPCmain,1,4)=="D06L"|
                      substr(H7$IPCmain,1,4)=="F25J"|substr(H7$IPCmain,1,3)=="F26"|substr(H7$IPCmain,1,4)=="C14C"|
                      substr(H7$IPCmain,1,4)=="H05H"~"W",
                   substr(H7$IPCmain,1,4)=="A62D"|substr(H7$IPCmain,1,7)=="B01D045"|substr(H7$IPCmain,1,7)=="B01D046"|
                      substr(H7$IPCmain,1,7)=="B01D047"|substr(H7$IPCmain,1,7)=="B01D049"|substr(H7$IPCmain,1,7)=="B01D050"|
                      substr(H7$IPCmain,1,7)=="B01D051"|substr(H7$IPCmain,1,7)=="B01D052"|substr(H7$IPCmain,1,7)=="B01D053"|
                      substr(H7$IPCmain,1,3)=="B09"|substr(H7$IPCmain,1,4)=="B65F"|substr(H7$IPCmain,1,3)=="C02"|
                      substr(H7$IPCmain,1,4)=="F01N"|substr(H7$IPCmain,1,4)=="F23G"|substr(H7$IPCmain,1,4)=="F23J"|
                      substr(H7$IPCmain,1,4)=="G01T"|substr(H7$IPCmain,1,7)=="E01F008"|substr(H7$IPCmain,1,4)=="A62C"~"X",
                   substr(H7$IPCmain,1,4)=="B25J"|substr(H7$IPCmain,1,4)=="B65B"|substr(H7$IPCmain,1,4)=="B65C"|
                      substr(H7$IPCmain,1,4)=="B65D"|substr(H7$IPCmain,1,4)=="B65G"|substr(H7$IPCmain,1,4)=="B65H"|
                      substr(H7$IPCmain,1,3)=="B66"|substr(H7$IPCmain,1,3)=="B67"~"Y",
                   substr(H7$IPCmain,1,3)=="B21"|substr(H7$IPCmain,1,3)=="B23"|substr(H7$IPCmain,1,3)=="B24"|
                      substr(H7$IPCmain,1,4)=="B26D"|substr(H7$IPCmain,1,4)=="B26F"|substr(H7$IPCmain,1,3)=="B27"|
                      substr(H7$IPCmain,1,3)=="B30"|substr(H7$IPCmain,1,4)=="B25B"|substr(H7$IPCmain,1,4)=="B25C"|
                      substr(H7$IPCmain,1,4)=="B25D"|substr(H7$IPCmain,1,4)=="B25F"|substr(H7$IPCmain,1,4)=="B25G"|
                      substr(H7$IPCmain,1,4)=="B25H"|substr(H7$IPCmain,1,4)=="B26B"~"Z",
                   substr(H7$IPCmain,1,4)=="F01B"|substr(H7$IPCmain,1,4)=="F01C"|substr(H7$IPCmain,1,4)=="F01D"|
                      substr(H7$IPCmain,1,4)=="F01K"|substr(H7$IPCmain,1,4)=="F01L"|substr(H7$IPCmain,1,4)=="F01M"|
                      substr(H7$IPCmain,1,4)=="F01P"|substr(H7$IPCmain,1,3)=="F02"|substr(H7$IPCmain,1,3)=="F03"|
                      substr(H7$IPCmain,1,3)=="F04"|substr(H7$IPCmain,1,4)=="F23R"|substr(H7$IPCmain,1,3)=="G21"|
                      substr(H7$IPCmain,1,4)=="F99Z"~"AA",
                   substr(H7$IPCmain,1,4)=="A41H"|substr(H7$IPCmain,1,4)=="A43D"|substr(H7$IPCmain,1,4)=="A46D"|
                      substr(H7$IPCmain,1,4)=="C14B"|substr(H7$IPCmain,1,3)=="D01"|substr(H7$IPCmain,1,3)=="D02"|
                      substr(H7$IPCmain,1,3)=="D03"|substr(H7$IPCmain,1,4)=="D04B"|substr(H7$IPCmain,1,4)=="D04C"|
                      substr(H7$IPCmain,1,4)=="D04G"|substr(H7$IPCmain,1,4)=="D04H"|substr(H7$IPCmain,1,4)=="D06J"|
                      substr(H7$IPCmain,1,4)=="D06M"|substr(H7$IPCmain,1,4)=="D06P"|substr(H7$IPCmain,1,4)=="D06Q"|
                      substr(H7$IPCmain,1,4)=="D99Z"|substr(H7$IPCmain,1,3)=="B31"|substr(H7$IPCmain,1,3)=="D21"|
                      substr(H7$IPCmain,1,3)=="B41"~"AB",
                   substr(H7$IPCmain,1,4)=="A01B"|substr(H7$IPCmain,1,4)=="A01C"|substr(H7$IPCmain,1,4)=="A01D"|
                      substr(H7$IPCmain,1,4)=="A01F"|substr(H7$IPCmain,1,4)=="A01G"|substr(H7$IPCmain,1,4)=="A01J"|
                      substr(H7$IPCmain,1,4)=="A01K"|substr(H7$IPCmain,1,4)=="A01L"|substr(H7$IPCmain,1,4)=="A01M"|
                      substr(H7$IPCmain,1,4)=="A21B"|substr(H7$IPCmain,1,4)=="A21C"|substr(H7$IPCmain,1,3)=="A22"|
                      substr(H7$IPCmain,1,4)=="A23N"|substr(H7$IPCmain,1,4)=="A23P"|substr(H7$IPCmain,1,4)=="B02B"|
                      substr(H7$IPCmain,1,4)=="C12L"|substr(H7$IPCmain,1,4)=="C13C"|substr(H7$IPCmain,1,4)=="C13G"|
                      substr(H7$IPCmain,1,4)=="C13H"|substr(H7$IPCmain,1,3)=="B28"|substr(H7$IPCmain,1,3)=="B29"|
                      substr(H7$IPCmain,1,4)=="C03B"|substr(H7$IPCmain,1,4)=="C08J"|substr(H7$IPCmain,1,4)=="B99Z"|
                      substr(H7$IPCmain,1,3)=="F41"|substr(H7$IPCmain,1,3)=="F42"~"AC",
                   substr(H7$IPCmain,1,3)=="F22"|substr(H7$IPCmain,1,4)=="F23B"|substr(H7$IPCmain,1,4)=="F23C"|
                      substr(H7$IPCmain,1,4)=="F23D"|substr(H7$IPCmain,1,4)=="F23H"|substr(H7$IPCmain,1,4)=="F23K"|
                      substr(H7$IPCmain,1,4)=="F23L"|substr(H7$IPCmain,1,4)=="F23M"|substr(H7$IPCmain,1,4)=="F23N"|
                      substr(H7$IPCmain,1,4)=="F23Q"|substr(H7$IPCmain,1,3)=="F24"|substr(H7$IPCmain,1,4)=="F25B"|
                      substr(H7$IPCmain,1,4)=="F25C"|substr(H7$IPCmain,1,3)=="F27"|substr(H7$IPCmain,1,3)=="F28"~"AD",
                   substr(H7$IPCmain,1,3)=="F15"|substr(H7$IPCmain,1,3)=="F16"|substr(H7$IPCmain,1,3)=="F17"|
                      substr(H7$IPCmain,1,4)=="G05G"~"AE",
                   substr(H7$IPCmain,1,3)=="B60"|substr(H7$IPCmain,1,3)=="B61"|substr(H7$IPCmain,1,3)=="B62"|
                      substr(H7$IPCmain,1,4)=="B63B"|substr(H7$IPCmain,1,4)=="B63C"|substr(H7$IPCmain,1,4)=="B63G"|
                      substr(H7$IPCmain,1,4)=="B63H"|substr(H7$IPCmain,1,4)=="B63J"|substr(H7$IPCmain,1,3)=="B64"~"AF",
                   substr(H7$IPCmain,1,3)=="A47"|substr(H7$IPCmain,1,3)=="A63"~"AG",
                   substr(H7$IPCmain,1,3)=="A24"|substr(H7$IPCmain,1,4)=="A41B"|substr(H7$IPCmain,1,4)=="A41C"|
                      substr(H7$IPCmain,1,4)=="A41D"|substr(H7$IPCmain,1,4)=="A41F"|substr(H7$IPCmain,1,4)=="A41G"|
                      substr(H7$IPCmain,1,3)=="A42"|substr(H7$IPCmain,1,4)=="A43B"|substr(H7$IPCmain,1,4)=="A43C"|
                      substr(H7$IPCmain,1,3)=="A44"|substr(H7$IPCmain,1,3)=="A45"|substr(H7$IPCmain,1,4)=="A46B"|
                      substr(H7$IPCmain,1,4)=="A62B"|substr(H7$IPCmain,1,3)=="B42"|substr(H7$IPCmain,1,3)=="B43"|
                      substr(H7$IPCmain,1,4)=="D04D"|substr(H7$IPCmain,1,3)=="D07"|substr(H7$IPCmain,1,4)=="G10B"|
                      substr(H7$IPCmain,1,4)=="G10C"|substr(H7$IPCmain,1,4)=="G10D"|substr(H7$IPCmain,1,4)=="G10F"|
                      substr(H7$IPCmain,1,4)=="G10G"|substr(H7$IPCmain,1,4)=="G10H"|substr(H7$IPCmain,1,4)=="G10K"|
                      substr(H7$IPCmain,1,3)=="B44"|substr(H7$IPCmain,1,3)=="B68"|substr(H7$IPCmain,1,4)=="D06F"|
                      substr(H7$IPCmain,1,4)=="D06N"|substr(H7$IPCmain,1,4)=="F25D"|substr(H7$IPCmain,1,4)=="A99Z"~"AH",
                   substr(H7$IPCmain,1,3)=="E02"|substr(H7$IPCmain,1,4)=="E01B"|substr(H7$IPCmain,1,4)=="E01C"|
                      substr(H7$IPCmain,1,4)=="E01C"|substr(H7$IPCmain,1,7)=="E01F001"|substr(H7$IPCmain,1,7)=="E01F003"|
                      substr(H7$IPCmain,1,7)=="E01F005"|substr(H7$IPCmain,1,7)=="E01F007"|substr(H7$IPCmain,1,7)=="E01F009"|
                      substr(H7$IPCmain,1,6)=="E01F01"|substr(H7$IPCmain,1,4)=="E01H"|substr(H7$IPCmain,1,3)=="E03"|
                      substr(H7$IPCmain,1,3)=="E04"|substr(H7$IPCmain,1,3)=="E05"|substr(H7$IPCmain,1,3)=="E06"|
                      substr(H7$IPCmain,1,3)=="E21"|substr(H7$IPCmain,1,4)=="E99Z"~"AI")

#Sum of different IPC Codes by company
H7$DivTech<-ave(H7$IPCmain,H7$GUO, FUN=function(x) length(unique(x))) #now with variety of IPC codes in area, otherwise total number of patents with H7$DivTech2<-ifelse(!is.na(H7$IPCmain),1,0)
H7$DivTech1<-ifelse(is.na(H7$IPCmain),0,H7$DivTech)
H7$DivTech1<-as.numeric(H7$DivTech1)

#Sum of different IPC Codes within each technical area in which the company is active 
H7$DivTech2<-ave(H7$IPCmain,H7[,c('GUO', 'Area')],FUN=function(x) length(unique(x)))
H7$DivTech2<-as.numeric(H7$DivTech2)

#Distribution of area patents among all patents
H7$DivTech3<-H7$DivTech2/H7$DivTech1

#Account for not classified patents here -- increases the final number immensely
H7$DivTech3[is.na(H7$DivTech3)] <- 0

# Test Company: CL869632007

#Remove duplicates to sum unique values
H7$Test<-paste(H7$GUO,H7$Area,H7$DivTech3,sep=" ")
H7<-H7[!duplicated(H7$Test),]

#Sum the shares, square them and substract from 1 = From concentration to diversification
H7$DivTech4<-ave(H7$DivTech3,H7$GUO,FUN=sum)
H7$DivTech5<-H7$DivTech4^2
H7$DivTech6<-1-H7$DivTech5

#Correct for the total number of patents (the measure is biased for small numbers)
H7$DivTech7<-ave(H7$DivTech1,H7$GUO,FUN=sum)
H7$DivTech8<-ifelse(H7$DivTech1==0 & H7$DivTech7!=0,NA,1)
H7<-subset(H7,!is.na(H7$DivTech8))
H7$DivTech<-H7$DivTech6*(1-1/H7$DivTech1) # different than in Rahko formular but makes more sense

H7$DivTech[H7$DivTech=="-Inf"] <-0

H7<-H7[,c(-2,-3,-4,-6,-7,-8,-9,-10,-11,-12,-13,-14)]
names(H7)<-c("BvDID","DivTech")

H7<-H7[!duplicated(H7$BvDID),]


#2018
Stock2018<-fread("files_created_code1/Stock2018.csv", dec=",")

Stock2018<-Stock2018[,c(-2,-3,-4,-5,-7,-8,-9)]
Full8<-fread("files_created_code1/Full8.csv", dec=",")
Full8[,3:33]<-NULL
names(Full8) <- c("GUO","Subsidiaries")
names(Stock2018) <- c("Subsidiaries","IPCmain")

H8<-left_join(Full8,Stock2018,by="Subsidiaries",na_matches="never")

H8$IPCmain<-as.character(H8$IPCmain)

#Filter for technological area of invention
H8$Area<-case_when(substr(H8$IPCmain,1,3)=="F21"~"A",
                   substr(H8$IPCmain,1,4)=="H01B"|substr(H8$IPCmain,1,4)=="H01C"|substr(H8$IPCmain,1,4)=="H01F"|
                      substr(H8$IPCmain,1,4)=="H01G"|substr(H8$IPCmain,1,4)=="H01H"|substr(H8$IPCmain,1,4)=="H01J"|
                      substr(H8$IPCmain,1,4)=="H01K"|substr(H8$IPCmain,1,4)=="H01M"|substr(H8$IPCmain,1,4)=="H01R"|
                      substr(H8$IPCmain,1,4)=="H01T"|substr(H8$IPCmain,1,3)=="H02"|substr(H8$IPCmain,1,4)=="H05B"|
                      substr(H8$IPCmain,1,4)=="H05C"|substr(H8$IPCmain,1,4)=="H05F"|substr(H8$IPCmain,1,4)=="H99Z"~"A",
                   substr(H8$IPCmain,1,4)=="G09F"|substr(H8$IPCmain,1,4)=="G09G"|substr(H8$IPCmain,1,4)=="G11B"|
                      substr(H8$IPCmain,1,6)=="H04N003"|substr(H8$IPCmain,1,6)=="H04N003"|substr(H8$IPCmain,1,6)=="H04N009"|
                      substr(H8$IPCmain,1,6)==" H04N013"|substr(H8$IPCmain,1,6)=="H04N015"|substr(H8$IPCmain,1,6)=="H04N017"|
                      substr(H8$IPCmain,1,4)=="H04R"|substr(H8$IPCmain,1,4)=="H04S"|substr(H8$IPCmain,1,4)=="H05K"~"B",
                   substr(H8$IPCmain,1,4)=="G08C"|substr(H8$IPCmain,1,4)=="H01P"|substr(H8$IPCmain,1,4)=="H01Q"|
                      substr(H8$IPCmain,1,4)=="H04B"|substr(H8$IPCmain,1,4)=="H04H"|substr(H8$IPCmain,1,4)==" H04J"|
                      substr(H8$IPCmain,1,4)=="H04K"|substr(H8$IPCmain,1,4)=="H04M"|substr(H8$IPCmain,1,6)=="H04N001"|
                      substr(H8$IPCmain,1,6)=="H04N007"|substr(H8$IPCmain,1,6)=="H04N011"|substr(H8$IPCmain,1,4)=="H04Q"~"C",
                   substr(H8$IPCmain,1,4)=="H04L"~"D",
                   substr(H8$IPCmain,1,3)=="H03"~"E",
                   substr(H8$IPCmain,1,3)=="G06"|substr(H8$IPCmain,1,4)=="G11C"|substr(H8$IPCmain,1,4)=="G10L"~"F",
                   substr(H8$IPCmain,1,4)=="G06Q"~"G",
                   substr(H8$IPCmain,1,4)=="H01L"~"H",
                   substr(H8$IPCmain,1,3)=="G02"|substr(H8$IPCmain,1,4)=="G03B"|substr(H8$IPCmain,1,4)=="G03C"|
                      substr(H8$IPCmain,1,6)=="G03D"|substr(H8$IPCmain,1,4)=="G03F"|substr(H8$IPCmain,1,4)=="G03G"|
                      substr(H8$IPCmain,1,4)=="G03H"|substr(H8$IPCmain,1,4)=="H01S"~"I",
                   substr(H8$IPCmain,1,4)=="G01B"|substr(H8$IPCmain,1,4)=="G01C"|substr(H8$IPCmain,1,4)=="G01D"|
                      substr(H8$IPCmain,1,4)=="G01F"|substr(H8$IPCmain,1,4)=="G01G"|substr(H8$IPCmain,1,4)=="G01H"|
                      substr(H8$IPCmain,1,4)=="G01J"|substr(H8$IPCmain,1,4)=="G01K"|substr(H8$IPCmain,1,4)=="G01L"|
                      substr(H8$IPCmain,1,4)=="G01M"|substr(H8$IPCmain,1,4)=="G01N"|substr(H8$IPCmain,1,4)=="G01P"|
                      substr(H8$IPCmain,1,4)=="G01R"|substr(H8$IPCmain,1,4)=="G01S"|substr(H8$IPCmain,1,4)=="G01V"|
                      substr(H8$IPCmain,1,4)=="G01W"|substr(H8$IPCmain,1,3)=="G04"|substr(H8$IPCmain,1,4)=="G12B"|
                      substr(H8$IPCmain,1,4)=="G99Z"~"J",
                   substr(H8$IPCmain,1,6)=="G01N033"~"K",
                   substr(H8$IPCmain,1,4)=="G05B"|substr(H8$IPCmain,1,4)=="G05D"|substr(H8$IPCmain,1,4)=="G05F"|
                      substr(H8$IPCmain,1,3)=="G07"|substr(H8$IPCmain,1,4)=="G08B"|substr(H8$IPCmain,1,4)=="G08G"|
                      substr(H8$IPCmain,1,4)=="G09B"|substr(H8$IPCmain,1,4)=="G09C"|substr(H8$IPCmain,1,4)=="G09D"~"L",
                   substr(H8$IPCmain,1,4)=="A61B"|substr(H8$IPCmain,1,4)=="A61C"|substr(H8$IPCmain,1,4)=="A61D"|
                      substr(H8$IPCmain,1,4)=="A61F"|substr(H8$IPCmain,1,4)=="A61G"|substr(H8$IPCmain,1,4)=="A61H"|
                      substr(H8$IPCmain,1,4)=="A61J"|substr(H8$IPCmain,1,4)=="A61L"|substr(H8$IPCmain,1,4)=="A61M"|
                      substr(H8$IPCmain,1,4)=="A61N"|substr(H8$IPCmain,1,4)=="H05G"~"M",
                   substr(H8$IPCmain,1,4)=="C07B"|substr(H8$IPCmain,1,4)=="C07C"|substr(H8$IPCmain,1,4)=="C07D"|
                      substr(H8$IPCmain,1,4)=="C07F"|substr(H8$IPCmain,1,4)=="C07H"|substr(H8$IPCmain,1,4)=="C07J"|
                      substr(H8$IPCmain,1,4)=="C40B"|substr(H8$IPCmain,1,7)=="A61K008"|substr(H8$IPCmain,1,4)=="A61Q"~"N",
                   substr(H8$IPCmain,1,4)=="C07G"|substr(H8$IPCmain,1,4)=="C07K"|substr(H8$IPCmain,1,4)=="C12M"|
                      substr(H8$IPCmain,1,4)=="C12N"|substr(H8$IPCmain,1,4)=="C12P"|substr(H8$IPCmain,1,4)=="C12Q"|
                      substr(H8$IPCmain,1,4)=="C12R"|substr(H8$IPCmain,1,4)=="C12S"~"O",
                   substr(H8$IPCmain,1,4)=="A61K"~"P",
                   substr(H8$IPCmain,1,4)=="C08B"|substr(H8$IPCmain,1,4)=="C08C"|substr(H8$IPCmain,1,4)=="C08F"|
                      substr(H8$IPCmain,1,4)=="C08G"|substr(H8$IPCmain,1,4)=="C08H"|substr(H8$IPCmain,1,4)=="C08K"|
                      substr(H8$IPCmain,1,4)=="C08L"~"Q",
                   substr(H8$IPCmain,1,4)=="A01H"|substr(H8$IPCmain,1,4)=="A21D"|substr(H8$IPCmain,1,4)=="A23B"|
                      substr(H8$IPCmain,1,4)=="A23C"|substr(H8$IPCmain,1,4)=="A23D"|substr(H8$IPCmain,1,4)=="A23F"|
                      substr(H8$IPCmain,1,4)=="A23G"|substr(H8$IPCmain,1,4)=="A23J"|substr(H8$IPCmain,1,4)=="A23K"|
                      substr(H8$IPCmain,1,4)=="A23L"|substr(H8$IPCmain,1,4)=="C12C"|substr(H8$IPCmain,1,4)=="C12F"|
                      substr(H8$IPCmain,1,4)=="C12G"|substr(H8$IPCmain,1,4)=="C12H"|substr(H8$IPCmain,1,4)=="C12J"|
                      substr(H8$IPCmain,1,4)=="C13D"|substr(H8$IPCmain,1,4)=="C13F"|substr(H8$IPCmain,1,4)=="C13J"|
                      substr(H8$IPCmain,1,4)=="C13K"~"R",
                   substr(H8$IPCmain,1,4)=="A01N"|substr(H8$IPCmain,1,4)=="A01P"|substr(H8$IPCmain,1,3)=="C05"|
                      substr(H8$IPCmain,1,3)=="C06"|substr(H8$IPCmain,1,4)=="C09B"|substr(H8$IPCmain,1,4)=="C09C"|
                      substr(H8$IPCmain,1,4)=="C09F"|substr(H8$IPCmain,1,4)=="C09G"|substr(H8$IPCmain,1,4)=="C09H"|
                      substr(H8$IPCmain,1,4)=="C09K"|substr(H8$IPCmain,1,4)=="C09D"|substr(H8$IPCmain,1,4)=="C09J"|
                      substr(H8$IPCmain,1,4)=="C10B"|substr(H8$IPCmain,1,4)=="C10C"|substr(H8$IPCmain,1,4)=="C10F"|
                      substr(H8$IPCmain,1,4)=="C10G"|substr(H8$IPCmain,1,4)=="C10H"|substr(H8$IPCmain,1,4)=="C10J"|
                      substr(H8$IPCmain,1,4)=="C10K"|substr(H8$IPCmain,1,4)=="C10L"|substr(H8$IPCmain,1,4)=="C10M"|
                      substr(H8$IPCmain,1,4)=="C10N"|substr(H8$IPCmain,1,4)=="C11B"|substr(H8$IPCmain,1,4)=="C11C"|
                      substr(H8$IPCmain,1,4)=="C11D"|substr(H8$IPCmain,1,4)=="C99Z"~"S",
                   substr(H8$IPCmain,1,3)=="C01"|substr(H8$IPCmain,1,4)=="C03C"|substr(H8$IPCmain,1,3)=="C04"|
                      substr(H8$IPCmain,1,3)=="C21"|substr(H8$IPCmain,1,3)=="C22"|substr(H8$IPCmain,1,3)=="B22"~"T",
                   substr(H8$IPCmain,1,4)=="B05C"|substr(H8$IPCmain,1,4)=="B05D"|substr(H8$IPCmain,1,3)=="B32"|
                      substr(H8$IPCmain,1,3)=="C23"|substr(H8$IPCmain,1,3)=="C25"|substr(H8$IPCmain,1,3)=="C30"~"U",
                   substr(H8$IPCmain,1,3)=="B81"|substr(H8$IPCmain,1,3)=="B82"~"V",
                   substr(H8$IPCmain,1,4)=="B01B"|substr(H8$IPCmain,1,7)=="B01D000"|substr(H8$IPCmain,1,6)=="B01D01"|
                      substr(H8$IPCmain,1,6)=="B01D02"|substr(H8$IPCmain,1,6)=="B01D03"|substr(H8$IPCmain,1,7)=="B01D041"|
                      substr(H8$IPCmain,1,7)=="B01D043"|substr(H8$IPCmain,1,7)=="B01D057"|substr(H8$IPCmain,1,7)=="B01D059"|
                      substr(H8$IPCmain,1,6)=="B01D06"|substr(H8$IPCmain,1,6)=="B01D07"|substr(H8$IPCmain,1,4)=="B01F"|
                      substr(H8$IPCmain,1,4)=="B01J"|substr(H8$IPCmain,1,4)=="B01L"|substr(H8$IPCmain,1,4)=="B02C"|
                      substr(H8$IPCmain,1,3)=="B03"|substr(H8$IPCmain,1,3)=="B04"|substr(H8$IPCmain,1,4)=="B05B"|
                      substr(H8$IPCmain,1,4)=="B06B"|substr(H8$IPCmain,1,3)=="B07"|substr(H8$IPCmain,1,3)=="B08"|
                      substr(H8$IPCmain,1,4)=="D06B"|substr(H8$IPCmain,1,4)=="D06C"|substr(H8$IPCmain,1,4)=="D06L"|
                      substr(H8$IPCmain,1,4)=="F25J"|substr(H8$IPCmain,1,3)=="F26"|substr(H8$IPCmain,1,4)=="C14C"|
                      substr(H8$IPCmain,1,4)=="H05H"~"W",
                   substr(H8$IPCmain,1,4)=="A62D"|substr(H8$IPCmain,1,7)=="B01D045"|substr(H8$IPCmain,1,7)=="B01D046"|
                      substr(H8$IPCmain,1,7)=="B01D047"|substr(H8$IPCmain,1,7)=="B01D049"|substr(H8$IPCmain,1,7)=="B01D050"|
                      substr(H8$IPCmain,1,7)=="B01D051"|substr(H8$IPCmain,1,7)=="B01D052"|substr(H8$IPCmain,1,7)=="B01D053"|
                      substr(H8$IPCmain,1,3)=="B09"|substr(H8$IPCmain,1,4)=="B65F"|substr(H8$IPCmain,1,3)=="C02"|
                      substr(H8$IPCmain,1,4)=="F01N"|substr(H8$IPCmain,1,4)=="F23G"|substr(H8$IPCmain,1,4)=="F23J"|
                      substr(H8$IPCmain,1,4)=="G01T"|substr(H8$IPCmain,1,7)=="E01F008"|substr(H8$IPCmain,1,4)=="A62C"~"X",
                   substr(H8$IPCmain,1,4)=="B25J"|substr(H8$IPCmain,1,4)=="B65B"|substr(H8$IPCmain,1,4)=="B65C"|
                      substr(H8$IPCmain,1,4)=="B65D"|substr(H8$IPCmain,1,4)=="B65G"|substr(H8$IPCmain,1,4)=="B65H"|
                      substr(H8$IPCmain,1,3)=="B66"|substr(H8$IPCmain,1,3)=="B67"~"Y",
                   substr(H8$IPCmain,1,3)=="B21"|substr(H8$IPCmain,1,3)=="B23"|substr(H8$IPCmain,1,3)=="B24"|
                      substr(H8$IPCmain,1,4)=="B26D"|substr(H8$IPCmain,1,4)=="B26F"|substr(H8$IPCmain,1,3)=="B27"|
                      substr(H8$IPCmain,1,3)=="B30"|substr(H8$IPCmain,1,4)=="B25B"|substr(H8$IPCmain,1,4)=="B25C"|
                      substr(H8$IPCmain,1,4)=="B25D"|substr(H8$IPCmain,1,4)=="B25F"|substr(H8$IPCmain,1,4)=="B25G"|
                      substr(H8$IPCmain,1,4)=="B25H"|substr(H8$IPCmain,1,4)=="B26B"~"Z",
                   substr(H8$IPCmain,1,4)=="F01B"|substr(H8$IPCmain,1,4)=="F01C"|substr(H8$IPCmain,1,4)=="F01D"|
                      substr(H8$IPCmain,1,4)=="F01K"|substr(H8$IPCmain,1,4)=="F01L"|substr(H8$IPCmain,1,4)=="F01M"|
                      substr(H8$IPCmain,1,4)=="F01P"|substr(H8$IPCmain,1,3)=="F02"|substr(H8$IPCmain,1,3)=="F03"|
                      substr(H8$IPCmain,1,3)=="F04"|substr(H8$IPCmain,1,4)=="F23R"|substr(H8$IPCmain,1,3)=="G21"|
                      substr(H8$IPCmain,1,4)=="F99Z"~"AA",
                   substr(H8$IPCmain,1,4)=="A41H"|substr(H8$IPCmain,1,4)=="A43D"|substr(H8$IPCmain,1,4)=="A46D"|
                      substr(H8$IPCmain,1,4)=="C14B"|substr(H8$IPCmain,1,3)=="D01"|substr(H8$IPCmain,1,3)=="D02"|
                      substr(H8$IPCmain,1,3)=="D03"|substr(H8$IPCmain,1,4)=="D04B"|substr(H8$IPCmain,1,4)=="D04C"|
                      substr(H8$IPCmain,1,4)=="D04G"|substr(H8$IPCmain,1,4)=="D04H"|substr(H8$IPCmain,1,4)=="D06J"|
                      substr(H8$IPCmain,1,4)=="D06M"|substr(H8$IPCmain,1,4)=="D06P"|substr(H8$IPCmain,1,4)=="D06Q"|
                      substr(H8$IPCmain,1,4)=="D99Z"|substr(H8$IPCmain,1,3)=="B31"|substr(H8$IPCmain,1,3)=="D21"|
                      substr(H8$IPCmain,1,3)=="B41"~"AB",
                   substr(H8$IPCmain,1,4)=="A01B"|substr(H8$IPCmain,1,4)=="A01C"|substr(H8$IPCmain,1,4)=="A01D"|
                      substr(H8$IPCmain,1,4)=="A01F"|substr(H8$IPCmain,1,4)=="A01G"|substr(H8$IPCmain,1,4)=="A01J"|
                      substr(H8$IPCmain,1,4)=="A01K"|substr(H8$IPCmain,1,4)=="A01L"|substr(H8$IPCmain,1,4)=="A01M"|
                      substr(H8$IPCmain,1,4)=="A21B"|substr(H8$IPCmain,1,4)=="A21C"|substr(H8$IPCmain,1,3)=="A22"|
                      substr(H8$IPCmain,1,4)=="A23N"|substr(H8$IPCmain,1,4)=="A23P"|substr(H8$IPCmain,1,4)=="B02B"|
                      substr(H8$IPCmain,1,4)=="C12L"|substr(H8$IPCmain,1,4)=="C13C"|substr(H8$IPCmain,1,4)=="C13G"|
                      substr(H8$IPCmain,1,4)=="C13H"|substr(H8$IPCmain,1,3)=="B28"|substr(H8$IPCmain,1,3)=="B29"|
                      substr(H8$IPCmain,1,4)=="C03B"|substr(H8$IPCmain,1,4)=="C08J"|substr(H8$IPCmain,1,4)=="B99Z"|
                      substr(H8$IPCmain,1,3)=="F41"|substr(H8$IPCmain,1,3)=="F42"~"AC",
                   substr(H8$IPCmain,1,3)=="F22"|substr(H8$IPCmain,1,4)=="F23B"|substr(H8$IPCmain,1,4)=="F23C"|
                      substr(H8$IPCmain,1,4)=="F23D"|substr(H8$IPCmain,1,4)=="F23H"|substr(H8$IPCmain,1,4)=="F23K"|
                      substr(H8$IPCmain,1,4)=="F23L"|substr(H8$IPCmain,1,4)=="F23M"|substr(H8$IPCmain,1,4)=="F23N"|
                      substr(H8$IPCmain,1,4)=="F23Q"|substr(H8$IPCmain,1,3)=="F24"|substr(H8$IPCmain,1,4)=="F25B"|
                      substr(H8$IPCmain,1,4)=="F25C"|substr(H8$IPCmain,1,3)=="F27"|substr(H8$IPCmain,1,3)=="F28"~"AD",
                   substr(H8$IPCmain,1,3)=="F15"|substr(H8$IPCmain,1,3)=="F16"|substr(H8$IPCmain,1,3)=="F17"|
                      substr(H8$IPCmain,1,4)=="G05G"~"AE",
                   substr(H8$IPCmain,1,3)=="B60"|substr(H8$IPCmain,1,3)=="B61"|substr(H8$IPCmain,1,3)=="B62"|
                      substr(H8$IPCmain,1,4)=="B63B"|substr(H8$IPCmain,1,4)=="B63C"|substr(H8$IPCmain,1,4)=="B63G"|
                      substr(H8$IPCmain,1,4)=="B63H"|substr(H8$IPCmain,1,4)=="B63J"|substr(H8$IPCmain,1,3)=="B64"~"AF",
                   substr(H8$IPCmain,1,3)=="A47"|substr(H8$IPCmain,1,3)=="A63"~"AG",
                   substr(H8$IPCmain,1,3)=="A24"|substr(H8$IPCmain,1,4)=="A41B"|substr(H8$IPCmain,1,4)=="A41C"|
                      substr(H8$IPCmain,1,4)=="A41D"|substr(H8$IPCmain,1,4)=="A41F"|substr(H8$IPCmain,1,4)=="A41G"|
                      substr(H8$IPCmain,1,3)=="A42"|substr(H8$IPCmain,1,4)=="A43B"|substr(H8$IPCmain,1,4)=="A43C"|
                      substr(H8$IPCmain,1,3)=="A44"|substr(H8$IPCmain,1,3)=="A45"|substr(H8$IPCmain,1,4)=="A46B"|
                      substr(H8$IPCmain,1,4)=="A62B"|substr(H8$IPCmain,1,3)=="B42"|substr(H8$IPCmain,1,3)=="B43"|
                      substr(H8$IPCmain,1,4)=="D04D"|substr(H8$IPCmain,1,3)=="D07"|substr(H8$IPCmain,1,4)=="G10B"|
                      substr(H8$IPCmain,1,4)=="G10C"|substr(H8$IPCmain,1,4)=="G10D"|substr(H8$IPCmain,1,4)=="G10F"|
                      substr(H8$IPCmain,1,4)=="G10G"|substr(H8$IPCmain,1,4)=="G10H"|substr(H8$IPCmain,1,4)=="G10K"|
                      substr(H8$IPCmain,1,3)=="B44"|substr(H8$IPCmain,1,3)=="B68"|substr(H8$IPCmain,1,4)=="D06F"|
                      substr(H8$IPCmain,1,4)=="D06N"|substr(H8$IPCmain,1,4)=="F25D"|substr(H8$IPCmain,1,4)=="A99Z"~"AH",
                   substr(H8$IPCmain,1,3)=="E02"|substr(H8$IPCmain,1,4)=="E01B"|substr(H8$IPCmain,1,4)=="E01C"|
                      substr(H8$IPCmain,1,4)=="E01C"|substr(H8$IPCmain,1,7)=="E01F001"|substr(H8$IPCmain,1,7)=="E01F003"|
                      substr(H8$IPCmain,1,7)=="E01F005"|substr(H8$IPCmain,1,7)=="E01F007"|substr(H8$IPCmain,1,7)=="E01F009"|
                      substr(H8$IPCmain,1,6)=="E01F01"|substr(H8$IPCmain,1,4)=="E01H"|substr(H8$IPCmain,1,3)=="E03"|
                      substr(H8$IPCmain,1,3)=="E04"|substr(H8$IPCmain,1,3)=="E05"|substr(H8$IPCmain,1,3)=="E06"|
                      substr(H8$IPCmain,1,3)=="E21"|substr(H8$IPCmain,1,4)=="E99Z"~"AI")

#Sum of different IPC Codes by company
H8$DivTech<-ave(H8$IPCmain,H8$GUO, FUN=function(x) length(unique(x))) #now with variety of IPC codes in area, otherwise total number of patents with H8$DivTech2<-ifelse(!is.na(H8$IPCmain),1,0)
H8$DivTech1<-ifelse(is.na(H8$IPCmain),0,H8$DivTech)
H8$DivTech1<-as.numeric(H8$DivTech1)

#Sum of different IPC Codes within each technical area in which the company is active 
H8$DivTech2<-ave(H8$IPCmain,H8[,c('GUO', 'Area')],FUN=function(x) length(unique(x)))
H8$DivTech2<-as.numeric(H8$DivTech2)

#Distribution of area patents among all patents
H8$DivTech3<-H8$DivTech2/H8$DivTech1

#Account for not classified patents here -- increases the final number immensely
H8$DivTech3[is.na(H8$DivTech3)] <- 0

# Test Company: CL869632007

#Remove duplicates to sum unique values
H8$Test<-paste(H8$GUO,H8$Area,H8$DivTech3,sep=" ")
H8<-H8[!duplicated(H8$Test),]

#Sum the shares, square them and substract from 1 = From concentration to diversification
H8$DivTech4<-ave(H8$DivTech3,H8$GUO,FUN=sum)
H8$DivTech5<-H8$DivTech4^2
H8$DivTech6<-1-H8$DivTech5

#Correct for the total number of patents (the measure is biased for small numbers)
H8$DivTech7<-ave(H8$DivTech1,H8$GUO,FUN=sum)
H8$DivTech8<-ifelse(H8$DivTech1==0 & H8$DivTech7!=0,NA,1)
H8<-subset(H8,!is.na(H8$DivTech8))
H8$DivTech<-H8$DivTech6*(1-1/H8$DivTech1) # different than in Rahko formular but makes more sense

H8$DivTech[H8$DivTech=="-Inf"] <-0

H8<-H8[,c(-2,-3,-4,-6,-7,-8,-9,-10,-11,-12,-13,-14)]
names(H8)<-c("BvDID","DivTech")

H8<-H8[!duplicated(H8$BvDID),]


#2019
Stock2019<-fread("files_created_code1/Stock2019.csv", dec=",")

Stock2019<-Stock2019[,c(-2,-3,-4,-5,-7,-8,-9)]
Full9<-fread("files_created_code1/Full9.csv", dec=",")
Full9[,3:33]<-NULL
names(Full9) <- c("GUO","Subsidiaries")
names(Stock2019) <- c("Subsidiaries","IPCmain")

H9<-left_join(Full9,Stock2019,by="Subsidiaries",na_matches="never")

H9$IPCmain<-as.character(H9$IPCmain)

#Filter for technological area of invention
H9$Area<-case_when(substr(H9$IPCmain,1,3)=="F21"~"A",
                   substr(H9$IPCmain,1,4)=="H01B"|substr(H9$IPCmain,1,4)=="H01C"|substr(H9$IPCmain,1,4)=="H01F"|
                      substr(H9$IPCmain,1,4)=="H01G"|substr(H9$IPCmain,1,4)=="H01H"|substr(H9$IPCmain,1,4)=="H01J"|
                      substr(H9$IPCmain,1,4)=="H01K"|substr(H9$IPCmain,1,4)=="H01M"|substr(H9$IPCmain,1,4)=="H01R"|
                      substr(H9$IPCmain,1,4)=="H01T"|substr(H9$IPCmain,1,3)=="H02"|substr(H9$IPCmain,1,4)=="H05B"|
                      substr(H9$IPCmain,1,4)=="H05C"|substr(H9$IPCmain,1,4)=="H05F"|substr(H9$IPCmain,1,4)=="H99Z"~"A",
                   substr(H9$IPCmain,1,4)=="G09F"|substr(H9$IPCmain,1,4)=="G09G"|substr(H9$IPCmain,1,4)=="G11B"|
                      substr(H9$IPCmain,1,6)=="H04N003"|substr(H9$IPCmain,1,6)=="H04N003"|substr(H9$IPCmain,1,6)=="H04N009"|
                      substr(H9$IPCmain,1,6)==" H04N013"|substr(H9$IPCmain,1,6)=="H04N015"|substr(H9$IPCmain,1,6)=="H04N017"|
                      substr(H9$IPCmain,1,4)=="H04R"|substr(H9$IPCmain,1,4)=="H04S"|substr(H9$IPCmain,1,4)=="H05K"~"B",
                   substr(H9$IPCmain,1,4)=="G08C"|substr(H9$IPCmain,1,4)=="H01P"|substr(H9$IPCmain,1,4)=="H01Q"|
                      substr(H9$IPCmain,1,4)=="H04B"|substr(H9$IPCmain,1,4)=="H04H"|substr(H9$IPCmain,1,4)==" H04J"|
                      substr(H9$IPCmain,1,4)=="H04K"|substr(H9$IPCmain,1,4)=="H04M"|substr(H9$IPCmain,1,6)=="H04N001"|
                      substr(H9$IPCmain,1,6)=="H04N007"|substr(H9$IPCmain,1,6)=="H04N011"|substr(H9$IPCmain,1,4)=="H04Q"~"C",
                   substr(H9$IPCmain,1,4)=="H04L"~"D",
                   substr(H9$IPCmain,1,3)=="H03"~"E",
                   substr(H9$IPCmain,1,3)=="G06"|substr(H9$IPCmain,1,4)=="G11C"|substr(H9$IPCmain,1,4)=="G10L"~"F",
                   substr(H9$IPCmain,1,4)=="G06Q"~"G",
                   substr(H9$IPCmain,1,4)=="H01L"~"H",
                   substr(H9$IPCmain,1,3)=="G02"|substr(H9$IPCmain,1,4)=="G03B"|substr(H9$IPCmain,1,4)=="G03C"|
                      substr(H9$IPCmain,1,6)=="G03D"|substr(H9$IPCmain,1,4)=="G03F"|substr(H9$IPCmain,1,4)=="G03G"|
                      substr(H9$IPCmain,1,4)=="G03H"|substr(H9$IPCmain,1,4)=="H01S"~"I",
                   substr(H9$IPCmain,1,4)=="G01B"|substr(H9$IPCmain,1,4)=="G01C"|substr(H9$IPCmain,1,4)=="G01D"|
                      substr(H9$IPCmain,1,4)=="G01F"|substr(H9$IPCmain,1,4)=="G01G"|substr(H9$IPCmain,1,4)=="G01H"|
                      substr(H9$IPCmain,1,4)=="G01J"|substr(H9$IPCmain,1,4)=="G01K"|substr(H9$IPCmain,1,4)=="G01L"|
                      substr(H9$IPCmain,1,4)=="G01M"|substr(H9$IPCmain,1,4)=="G01N"|substr(H9$IPCmain,1,4)=="G01P"|
                      substr(H9$IPCmain,1,4)=="G01R"|substr(H9$IPCmain,1,4)=="G01S"|substr(H9$IPCmain,1,4)=="G01V"|
                      substr(H9$IPCmain,1,4)=="G01W"|substr(H9$IPCmain,1,3)=="G04"|substr(H9$IPCmain,1,4)=="G12B"|
                      substr(H9$IPCmain,1,4)=="G99Z"~"J",
                   substr(H9$IPCmain,1,6)=="G01N033"~"K",
                   substr(H9$IPCmain,1,4)=="G05B"|substr(H9$IPCmain,1,4)=="G05D"|substr(H9$IPCmain,1,4)=="G05F"|
                      substr(H9$IPCmain,1,3)=="G07"|substr(H9$IPCmain,1,4)=="G08B"|substr(H9$IPCmain,1,4)=="G08G"|
                      substr(H9$IPCmain,1,4)=="G09B"|substr(H9$IPCmain,1,4)=="G09C"|substr(H9$IPCmain,1,4)=="G09D"~"L",
                   substr(H9$IPCmain,1,4)=="A61B"|substr(H9$IPCmain,1,4)=="A61C"|substr(H9$IPCmain,1,4)=="A61D"|
                      substr(H9$IPCmain,1,4)=="A61F"|substr(H9$IPCmain,1,4)=="A61G"|substr(H9$IPCmain,1,4)=="A61H"|
                      substr(H9$IPCmain,1,4)=="A61J"|substr(H9$IPCmain,1,4)=="A61L"|substr(H9$IPCmain,1,4)=="A61M"|
                      substr(H9$IPCmain,1,4)=="A61N"|substr(H9$IPCmain,1,4)=="H05G"~"M",
                   substr(H9$IPCmain,1,4)=="C07B"|substr(H9$IPCmain,1,4)=="C07C"|substr(H9$IPCmain,1,4)=="C07D"|
                      substr(H9$IPCmain,1,4)=="C07F"|substr(H9$IPCmain,1,4)=="C07H"|substr(H9$IPCmain,1,4)=="C07J"|
                      substr(H9$IPCmain,1,4)=="C40B"|substr(H9$IPCmain,1,7)=="A61K008"|substr(H9$IPCmain,1,4)=="A61Q"~"N",
                   substr(H9$IPCmain,1,4)=="C07G"|substr(H9$IPCmain,1,4)=="C07K"|substr(H9$IPCmain,1,4)=="C12M"|
                      substr(H9$IPCmain,1,4)=="C12N"|substr(H9$IPCmain,1,4)=="C12P"|substr(H9$IPCmain,1,4)=="C12Q"|
                      substr(H9$IPCmain,1,4)=="C12R"|substr(H9$IPCmain,1,4)=="C12S"~"O",
                   substr(H9$IPCmain,1,4)=="A61K"~"P",
                   substr(H9$IPCmain,1,4)=="C08B"|substr(H9$IPCmain,1,4)=="C08C"|substr(H9$IPCmain,1,4)=="C08F"|
                      substr(H9$IPCmain,1,4)=="C08G"|substr(H9$IPCmain,1,4)=="C08H"|substr(H9$IPCmain,1,4)=="C08K"|
                      substr(H9$IPCmain,1,4)=="C08L"~"Q",
                   substr(H9$IPCmain,1,4)=="A01H"|substr(H9$IPCmain,1,4)=="A21D"|substr(H9$IPCmain,1,4)=="A23B"|
                      substr(H9$IPCmain,1,4)=="A23C"|substr(H9$IPCmain,1,4)=="A23D"|substr(H9$IPCmain,1,4)=="A23F"|
                      substr(H9$IPCmain,1,4)=="A23G"|substr(H9$IPCmain,1,4)=="A23J"|substr(H9$IPCmain,1,4)=="A23K"|
                      substr(H9$IPCmain,1,4)=="A23L"|substr(H9$IPCmain,1,4)=="C12C"|substr(H9$IPCmain,1,4)=="C12F"|
                      substr(H9$IPCmain,1,4)=="C12G"|substr(H9$IPCmain,1,4)=="C12H"|substr(H9$IPCmain,1,4)=="C12J"|
                      substr(H9$IPCmain,1,4)=="C13D"|substr(H9$IPCmain,1,4)=="C13F"|substr(H9$IPCmain,1,4)=="C13J"|
                      substr(H9$IPCmain,1,4)=="C13K"~"R",
                   substr(H9$IPCmain,1,4)=="A01N"|substr(H9$IPCmain,1,4)=="A01P"|substr(H9$IPCmain,1,3)=="C05"|
                      substr(H9$IPCmain,1,3)=="C06"|substr(H9$IPCmain,1,4)=="C09B"|substr(H9$IPCmain,1,4)=="C09C"|
                      substr(H9$IPCmain,1,4)=="C09F"|substr(H9$IPCmain,1,4)=="C09G"|substr(H9$IPCmain,1,4)=="C09H"|
                      substr(H9$IPCmain,1,4)=="C09K"|substr(H9$IPCmain,1,4)=="C09D"|substr(H9$IPCmain,1,4)=="C09J"|
                      substr(H9$IPCmain,1,4)=="C10B"|substr(H9$IPCmain,1,4)=="C10C"|substr(H9$IPCmain,1,4)=="C10F"|
                      substr(H9$IPCmain,1,4)=="C10G"|substr(H9$IPCmain,1,4)=="C10H"|substr(H9$IPCmain,1,4)=="C10J"|
                      substr(H9$IPCmain,1,4)=="C10K"|substr(H9$IPCmain,1,4)=="C10L"|substr(H9$IPCmain,1,4)=="C10M"|
                      substr(H9$IPCmain,1,4)=="C10N"|substr(H9$IPCmain,1,4)=="C11B"|substr(H9$IPCmain,1,4)=="C11C"|
                      substr(H9$IPCmain,1,4)=="C11D"|substr(H9$IPCmain,1,4)=="C99Z"~"S",
                   substr(H9$IPCmain,1,3)=="C01"|substr(H9$IPCmain,1,4)=="C03C"|substr(H9$IPCmain,1,3)=="C04"|
                      substr(H9$IPCmain,1,3)=="C21"|substr(H9$IPCmain,1,3)=="C22"|substr(H9$IPCmain,1,3)=="B22"~"T",
                   substr(H9$IPCmain,1,4)=="B05C"|substr(H9$IPCmain,1,4)=="B05D"|substr(H9$IPCmain,1,3)=="B32"|
                      substr(H9$IPCmain,1,3)=="C23"|substr(H9$IPCmain,1,3)=="C25"|substr(H9$IPCmain,1,3)=="C30"~"U",
                   substr(H9$IPCmain,1,3)=="B81"|substr(H9$IPCmain,1,3)=="B82"~"V",
                   substr(H9$IPCmain,1,4)=="B01B"|substr(H9$IPCmain,1,7)=="B01D000"|substr(H9$IPCmain,1,6)=="B01D01"|
                      substr(H9$IPCmain,1,6)=="B01D02"|substr(H9$IPCmain,1,6)=="B01D03"|substr(H9$IPCmain,1,7)=="B01D041"|
                      substr(H9$IPCmain,1,7)=="B01D043"|substr(H9$IPCmain,1,7)=="B01D057"|substr(H9$IPCmain,1,7)=="B01D059"|
                      substr(H9$IPCmain,1,6)=="B01D06"|substr(H9$IPCmain,1,6)=="B01D07"|substr(H9$IPCmain,1,4)=="B01F"|
                      substr(H9$IPCmain,1,4)=="B01J"|substr(H9$IPCmain,1,4)=="B01L"|substr(H9$IPCmain,1,4)=="B02C"|
                      substr(H9$IPCmain,1,3)=="B03"|substr(H9$IPCmain,1,3)=="B04"|substr(H9$IPCmain,1,4)=="B05B"|
                      substr(H9$IPCmain,1,4)=="B06B"|substr(H9$IPCmain,1,3)=="B07"|substr(H9$IPCmain,1,3)=="B08"|
                      substr(H9$IPCmain,1,4)=="D06B"|substr(H9$IPCmain,1,4)=="D06C"|substr(H9$IPCmain,1,4)=="D06L"|
                      substr(H9$IPCmain,1,4)=="F25J"|substr(H9$IPCmain,1,3)=="F26"|substr(H9$IPCmain,1,4)=="C14C"|
                      substr(H9$IPCmain,1,4)=="H05H"~"W",
                   substr(H9$IPCmain,1,4)=="A62D"|substr(H9$IPCmain,1,7)=="B01D045"|substr(H9$IPCmain,1,7)=="B01D046"|
                      substr(H9$IPCmain,1,7)=="B01D047"|substr(H9$IPCmain,1,7)=="B01D049"|substr(H9$IPCmain,1,7)=="B01D050"|
                      substr(H9$IPCmain,1,7)=="B01D051"|substr(H9$IPCmain,1,7)=="B01D052"|substr(H9$IPCmain,1,7)=="B01D053"|
                      substr(H9$IPCmain,1,3)=="B09"|substr(H9$IPCmain,1,4)=="B65F"|substr(H9$IPCmain,1,3)=="C02"|
                      substr(H9$IPCmain,1,4)=="F01N"|substr(H9$IPCmain,1,4)=="F23G"|substr(H9$IPCmain,1,4)=="F23J"|
                      substr(H9$IPCmain,1,4)=="G01T"|substr(H9$IPCmain,1,7)=="E01F008"|substr(H9$IPCmain,1,4)=="A62C"~"X",
                   substr(H9$IPCmain,1,4)=="B25J"|substr(H9$IPCmain,1,4)=="B65B"|substr(H9$IPCmain,1,4)=="B65C"|
                      substr(H9$IPCmain,1,4)=="B65D"|substr(H9$IPCmain,1,4)=="B65G"|substr(H9$IPCmain,1,4)=="B65H"|
                      substr(H9$IPCmain,1,3)=="B66"|substr(H9$IPCmain,1,3)=="B67"~"Y",
                   substr(H9$IPCmain,1,3)=="B21"|substr(H9$IPCmain,1,3)=="B23"|substr(H9$IPCmain,1,3)=="B24"|
                      substr(H9$IPCmain,1,4)=="B26D"|substr(H9$IPCmain,1,4)=="B26F"|substr(H9$IPCmain,1,3)=="B27"|
                      substr(H9$IPCmain,1,3)=="B30"|substr(H9$IPCmain,1,4)=="B25B"|substr(H9$IPCmain,1,4)=="B25C"|
                      substr(H9$IPCmain,1,4)=="B25D"|substr(H9$IPCmain,1,4)=="B25F"|substr(H9$IPCmain,1,4)=="B25G"|
                      substr(H9$IPCmain,1,4)=="B25H"|substr(H9$IPCmain,1,4)=="B26B"~"Z",
                   substr(H9$IPCmain,1,4)=="F01B"|substr(H9$IPCmain,1,4)=="F01C"|substr(H9$IPCmain,1,4)=="F01D"|
                      substr(H9$IPCmain,1,4)=="F01K"|substr(H9$IPCmain,1,4)=="F01L"|substr(H9$IPCmain,1,4)=="F01M"|
                      substr(H9$IPCmain,1,4)=="F01P"|substr(H9$IPCmain,1,3)=="F02"|substr(H9$IPCmain,1,3)=="F03"|
                      substr(H9$IPCmain,1,3)=="F04"|substr(H9$IPCmain,1,4)=="F23R"|substr(H9$IPCmain,1,3)=="G21"|
                      substr(H9$IPCmain,1,4)=="F99Z"~"AA",
                   substr(H9$IPCmain,1,4)=="A41H"|substr(H9$IPCmain,1,4)=="A43D"|substr(H9$IPCmain,1,4)=="A46D"|
                      substr(H9$IPCmain,1,4)=="C14B"|substr(H9$IPCmain,1,3)=="D01"|substr(H9$IPCmain,1,3)=="D02"|
                      substr(H9$IPCmain,1,3)=="D03"|substr(H9$IPCmain,1,4)=="D04B"|substr(H9$IPCmain,1,4)=="D04C"|
                      substr(H9$IPCmain,1,4)=="D04G"|substr(H9$IPCmain,1,4)=="D04H"|substr(H9$IPCmain,1,4)=="D06J"|
                      substr(H9$IPCmain,1,4)=="D06M"|substr(H9$IPCmain,1,4)=="D06P"|substr(H9$IPCmain,1,4)=="D06Q"|
                      substr(H9$IPCmain,1,4)=="D99Z"|substr(H9$IPCmain,1,3)=="B31"|substr(H9$IPCmain,1,3)=="D21"|
                      substr(H9$IPCmain,1,3)=="B41"~"AB",
                   substr(H9$IPCmain,1,4)=="A01B"|substr(H9$IPCmain,1,4)=="A01C"|substr(H9$IPCmain,1,4)=="A01D"|
                      substr(H9$IPCmain,1,4)=="A01F"|substr(H9$IPCmain,1,4)=="A01G"|substr(H9$IPCmain,1,4)=="A01J"|
                      substr(H9$IPCmain,1,4)=="A01K"|substr(H9$IPCmain,1,4)=="A01L"|substr(H9$IPCmain,1,4)=="A01M"|
                      substr(H9$IPCmain,1,4)=="A21B"|substr(H9$IPCmain,1,4)=="A21C"|substr(H9$IPCmain,1,3)=="A22"|
                      substr(H9$IPCmain,1,4)=="A23N"|substr(H9$IPCmain,1,4)=="A23P"|substr(H9$IPCmain,1,4)=="B02B"|
                      substr(H9$IPCmain,1,4)=="C12L"|substr(H9$IPCmain,1,4)=="C13C"|substr(H9$IPCmain,1,4)=="C13G"|
                      substr(H9$IPCmain,1,4)=="C13H"|substr(H9$IPCmain,1,3)=="B28"|substr(H9$IPCmain,1,3)=="B29"|
                      substr(H9$IPCmain,1,4)=="C03B"|substr(H9$IPCmain,1,4)=="C08J"|substr(H9$IPCmain,1,4)=="B99Z"|
                      substr(H9$IPCmain,1,3)=="F41"|substr(H9$IPCmain,1,3)=="F42"~"AC",
                   substr(H9$IPCmain,1,3)=="F22"|substr(H9$IPCmain,1,4)=="F23B"|substr(H9$IPCmain,1,4)=="F23C"|
                      substr(H9$IPCmain,1,4)=="F23D"|substr(H9$IPCmain,1,4)=="F23H"|substr(H9$IPCmain,1,4)=="F23K"|
                      substr(H9$IPCmain,1,4)=="F23L"|substr(H9$IPCmain,1,4)=="F23M"|substr(H9$IPCmain,1,4)=="F23N"|
                      substr(H9$IPCmain,1,4)=="F23Q"|substr(H9$IPCmain,1,3)=="F24"|substr(H9$IPCmain,1,4)=="F25B"|
                      substr(H9$IPCmain,1,4)=="F25C"|substr(H9$IPCmain,1,3)=="F27"|substr(H9$IPCmain,1,3)=="F28"~"AD",
                   substr(H9$IPCmain,1,3)=="F15"|substr(H9$IPCmain,1,3)=="F16"|substr(H9$IPCmain,1,3)=="F17"|
                      substr(H9$IPCmain,1,4)=="G05G"~"AE",
                   substr(H9$IPCmain,1,3)=="B60"|substr(H9$IPCmain,1,3)=="B61"|substr(H9$IPCmain,1,3)=="B62"|
                      substr(H9$IPCmain,1,4)=="B63B"|substr(H9$IPCmain,1,4)=="B63C"|substr(H9$IPCmain,1,4)=="B63G"|
                      substr(H9$IPCmain,1,4)=="B63H"|substr(H9$IPCmain,1,4)=="B63J"|substr(H9$IPCmain,1,3)=="B64"~"AF",
                   substr(H9$IPCmain,1,3)=="A47"|substr(H9$IPCmain,1,3)=="A63"~"AG",
                   substr(H9$IPCmain,1,3)=="A24"|substr(H9$IPCmain,1,4)=="A41B"|substr(H9$IPCmain,1,4)=="A41C"|
                      substr(H9$IPCmain,1,4)=="A41D"|substr(H9$IPCmain,1,4)=="A41F"|substr(H9$IPCmain,1,4)=="A41G"|
                      substr(H9$IPCmain,1,3)=="A42"|substr(H9$IPCmain,1,4)=="A43B"|substr(H9$IPCmain,1,4)=="A43C"|
                      substr(H9$IPCmain,1,3)=="A44"|substr(H9$IPCmain,1,3)=="A45"|substr(H9$IPCmain,1,4)=="A46B"|
                      substr(H9$IPCmain,1,4)=="A62B"|substr(H9$IPCmain,1,3)=="B42"|substr(H9$IPCmain,1,3)=="B43"|
                      substr(H9$IPCmain,1,4)=="D04D"|substr(H9$IPCmain,1,3)=="D07"|substr(H9$IPCmain,1,4)=="G10B"|
                      substr(H9$IPCmain,1,4)=="G10C"|substr(H9$IPCmain,1,4)=="G10D"|substr(H9$IPCmain,1,4)=="G10F"|
                      substr(H9$IPCmain,1,4)=="G10G"|substr(H9$IPCmain,1,4)=="G10H"|substr(H9$IPCmain,1,4)=="G10K"|
                      substr(H9$IPCmain,1,3)=="B44"|substr(H9$IPCmain,1,3)=="B68"|substr(H9$IPCmain,1,4)=="D06F"|
                      substr(H9$IPCmain,1,4)=="D06N"|substr(H9$IPCmain,1,4)=="F25D"|substr(H9$IPCmain,1,4)=="A99Z"~"AH",
                   substr(H9$IPCmain,1,3)=="E02"|substr(H9$IPCmain,1,4)=="E01B"|substr(H9$IPCmain,1,4)=="E01C"|
                      substr(H9$IPCmain,1,4)=="E01C"|substr(H9$IPCmain,1,7)=="E01F001"|substr(H9$IPCmain,1,7)=="E01F003"|
                      substr(H9$IPCmain,1,7)=="E01F005"|substr(H9$IPCmain,1,7)=="E01F007"|substr(H9$IPCmain,1,7)=="E01F009"|
                      substr(H9$IPCmain,1,6)=="E01F01"|substr(H9$IPCmain,1,4)=="E01H"|substr(H9$IPCmain,1,3)=="E03"|
                      substr(H9$IPCmain,1,3)=="E04"|substr(H9$IPCmain,1,3)=="E05"|substr(H9$IPCmain,1,3)=="E06"|
                      substr(H9$IPCmain,1,3)=="E21"|substr(H9$IPCmain,1,4)=="E99Z"~"AI")

#Sum of different IPC Codes by company
H9$DivTech<-ave(H9$IPCmain,H9$GUO, FUN=function(x) length(unique(x))) #now with variety of IPC codes in area, otherwise total number of patents with H9$DivTech2<-ifelse(!is.na(H9$IPCmain),1,0)
H9$DivTech1<-ifelse(is.na(H9$IPCmain),0,H9$DivTech)
H9$DivTech1<-as.numeric(H9$DivTech1)

#Sum of different IPC Codes within each technical area in which the company is active 
H9$DivTech2<-ave(H9$IPCmain,H9[,c('GUO', 'Area')],FUN=function(x) length(unique(x)))
H9$DivTech2<-as.numeric(H9$DivTech2)

#Distribution of area patents among all patents
H9$DivTech3<-H9$DivTech2/H9$DivTech1

#Account for not classified patents here -- increases the final number immensely
H9$DivTech3[is.na(H9$DivTech3)] <- 0

# Test Company: CL869632007

#Remove duplicates to sum unique values
H9$Test<-paste(H9$GUO,H9$Area,H9$DivTech3,sep=" ")
H9<-H9[!duplicated(H9$Test),]

#Sum the shares, square them and substract from 1 = From concentration to diversification
H9$DivTech4<-ave(H9$DivTech3,H9$GUO,FUN=sum)
H9$DivTech5<-H9$DivTech4^2
H9$DivTech6<-1-H9$DivTech5

#Correct for the total number of patents (the measure is biased for small numbers)
H9$DivTech7<-ave(H9$DivTech1,H9$GUO,FUN=sum)
H9$DivTech8<-ifelse(H9$DivTech1==0 & H9$DivTech7!=0,NA,1)
H9<-subset(H9,!is.na(H9$DivTech8))
H9$DivTech<-H9$DivTech6*(1-1/H9$DivTech1) # different than in Rahko formular but makes more sense

H9$DivTech[H9$DivTech=="-Inf"] <-0

H9<-H9[,c(-2,-3,-4,-6,-7,-8,-9,-10,-11,-12,-13,-14)]
names(H9)<-c("BvDID","DivTech")

H9<-H9[!duplicated(H9$BvDID),]

#Remove unnecessary files
rm(Stock2011,Stock2012,Stock2013,Stock2014,Stock2015,Stock2016,Stock2017,Stock2018,Stock2019,
   Full1,Full2,Full3,Full4,Full5,Full6,Full7,Full8,Full9)

#Save Herfindahl Files
#write.csv2(H1, file = "Felix/Herfindahl11.csv",row.names = F)
#write.csv2(H2, file = "Felix/Herfindahl12.csv",row.names = F)
#write.csv2(H3, file = "Felix/Herfindahl13.csv",row.names = F)
#write.csv2(H4, file = "Felix/Herfindahl14.csv",row.names = F)
#write.csv2(H5, file = "Felix/Herfindahl15.csv",row.names = F)
#write.csv2(H6, file = "Felix/Herfindahl16.csv",row.names = F)
#write.csv2(H7, file = "Felix/Herfindahl17.csv",row.names = F)
#write.csv2(H8, file = "Felix/Herfindahl18.csv",row.names = F)
#write.csv2(H9, file = "Felix/Herfindahl19.csv",row.names = F)

##Read the output from the Herfindahl Code
#H1<-fread("Felix/Herfindahl11.csv",sep=";")
#H2<-fread("Felix/Herfindahl12.csv",sep=";")
#H3<-fread("Felix/Herfindahl13.csv",sep=";")
#H4<-fread("Felix/Herfindahl14.csv",sep=";")
#H5<-fread("Felix/Herfindahl15.csv",sep=";")
#H6<-fread("Felix/Herfindahl16.csv",sep=";")
#H7<-fread("Felix/Herfindahl17.csv",sep=";")
#H8<-fread("Felix/Herfindahl18.csv",sep=";")
#H9<-fread("Felix/Herfindahl19.csv",sep=";")

#Merge data with Herfindahl data
All11<-left_join(All11,H1,by="BvDID",na_matches="never")
All12<-left_join(All12,H2,by="BvDID",na_matches="never")
All13<-left_join(All13,H3,by="BvDID",na_matches="never")
All14<-left_join(All14,H4,by="BvDID",na_matches="never")
All15<-left_join(All15,H5,by="BvDID",na_matches="never")
All16<-left_join(All16,H6,by="BvDID",na_matches="never")
All17<-left_join(All17,H7,by="BvDID",na_matches="never")
All18<-left_join(All18,H8,by="BvDID",na_matches="never")
All19<-left_join(All19,H9,by="BvDID",na_matches="never")

#Merge data with multinationality data
All11<-left_join(All11,M1,by="BvDID",na_matches="never")
All12<-left_join(All12,M2,by="BvDID",na_matches="never")
All13<-left_join(All13,M3,by="BvDID",na_matches="never")
All14<-left_join(All14,M4,by="BvDID",na_matches="never")
All15<-left_join(All15,M5,by="BvDID",na_matches="never")
All16<-left_join(All16,M6,by="BvDID",na_matches="never")
All17<-left_join(All17,M7,by="BvDID",na_matches="never")
All18<-left_join(All18,M8,by="BvDID",na_matches="never")
All19<-left_join(All19,M9,by="BvDID",na_matches="never")

rm(H1,H2,H3,H4,H5,H6,H7,H8,H9,M1,M2,M3,M4,M5,M6,M7,M8,M9)




##Interim Step:
##Upload BvDID file to Orbis
##Filter for medium and large firms
#Up<-bind_rows(All11[1],All12[1],All13[1],All14[1],All15[1],All16[1],All17[1],All18[1],All19[1])
#Write excel for upload to Orbis
#Up<-Up[!duplicated(Up$BvDID),]
#Up<-as.data.frame(Up)
#write.csv2(Up, file = "Up.csv",row.names = F) #476101
##Download files and upload to R:

#### . ####




###Part VI: Match data #####

#   1. Create a dataset based on the GUOs identified in the previous steps - based on Orbis ####

#Read shareholder data (1:Name,2:Shareholder-BvDID,3:GUOBvDID,4:Subsidiaries-BvDID):

#Read data of foreign subsidiaries (1:Name,2:Company-BvDID,3:GUOBvDID,4:Parent-BvDID):
Final1<-read_excel("Dataset/Final/Final1.xlsx",sheet = "Results")
Final2<-read_excel("Dataset/Final/Final2.xlsx",sheet = "Results")
Final3<-read_excel("Dataset/Final/Final3.xlsx",sheet = "Results")
Final4<-read_excel("Dataset/Final/Final4.xlsx",sheet = "Results")
Final5<-read_excel("Dataset/Final/Final5.xlsx",sheet = "Results")
Final6<-read_excel("Dataset/Final/Final6.xlsx",sheet = "Results")
Final7<-read_excel("Dataset/Final/Final7.xlsx",sheet = "Results")
Final8<-read_excel("Dataset/Final/Final8.xlsx",sheet = "Results")
Final9<-read_excel("Dataset/Final/Final9.xlsx",sheet = "Results")
Final10<-read_excel("Dataset/Final/Final10.xlsx",sheet = "Results")
Final11<-read_excel("Dataset/Final/Final11.xlsx",sheet = "Results")
Final12<-read_excel("Dataset/Final/Final12.xlsx",sheet = "Results")
Final13<-read_excel("Dataset/Final/Final13.xlsx",sheet = "Results")
Final14<-read_excel("Dataset/Final/Final14.xlsx",sheet = "Results")
Final15<-read_excel("Dataset/Final/Final15.xlsx",sheet = "Results")
Final16<-read_excel("Dataset/Final/Final16.xlsx",sheet = "Results")
Final17<-read_excel("Dataset/Final/Final17.xlsx",sheet = "Results")
Final18<-read_excel("Dataset/Final/Final18.xlsx",sheet = "Results")
Final19<-read_excel("Dataset/Final/Final19.xlsx",sheet = "Results")
Final20<-read_excel("Dataset/Final/Final20.xlsx",sheet = "Results")
Final21<-read_excel("Dataset/Final/Final21.xlsx",sheet = "Results")
Final22<-read_excel("Dataset/Final/Final22.xlsx",sheet = "Results")
Final23<-read_excel("Dataset/Final/Final23.xlsx",sheet = "Results")
Final24<-read_excel("Dataset/Final/Final24.xlsx",sheet = "Results")
Final25<-read_excel("Dataset/Final/Final25.xlsx",sheet = "Results")
Final26<-read_excel("Dataset/Final/Final26.xlsx",sheet = "Results")
Final27<-read_excel("Dataset/Final/Final27.xlsx",sheet = "Results")
Final28<-read_excel("Dataset/Final/Final28.xlsx",sheet = "Results")
Final29<-read_excel("Dataset/Final/Final29.xlsx",sheet = "Results")
Final30<-read_excel("Dataset/Final/Final30.xlsx",sheet = "Results")
Final31<-read_excel("Dataset/Final/Final31.xlsx",sheet = "Results")
Final32<-read_excel("Dataset/Final/Final32.xlsx",sheet = "Results")
Final33<-read_excel("Dataset/Final/Final33.xlsx",sheet = "Results")
Final34<-read_excel("Dataset/Final/Final34.xlsx",sheet = "Results")
Final35<-read_excel("Dataset/Final/Final35.xlsx",sheet = "Results")
Final36<-read_excel("Dataset/Final/Final36.xlsx",sheet = "Results")
Final37<-read_excel("Dataset/Final/Final37.xlsx",sheet = "Results")
Final38<-read_excel("Dataset/Final/Final38.xlsx",sheet = "Results")
Final39<-read_excel("Dataset/Final/Final38.xlsx",sheet = "Results")
Final40<-read_excel("Dataset/Final/Final40.xlsx",sheet = "Results")
Final41<-read_excel("Dataset/Final/Final41.xlsx",sheet = "Results")
Final42<-read_excel("Dataset/Final/Final42.xlsx",sheet = "Results")
Final43<-read_excel("Dataset/Final/Final43.xlsx",sheet = "Results")
Final44<-read_excel("Dataset/Final/Final44.xlsx",sheet = "Results")
Final45<-read_excel("Dataset/Final/Final45.xlsx",sheet = "Results")
Final46<-read_excel("Dataset/Final/Final46.xlsx",sheet = "Results")
Final47<-read_excel("Dataset/Final/Final47.xlsx",sheet = "Results")
Final48<-read_excel("Dataset/Final/Final48.xlsx",sheet = "Results")
Final49<-read_excel("Dataset/Final/Final49.xlsx",sheet = "Results")
Final50<-read_excel("Dataset/Final/Final50.xlsx",sheet = "Results")
Final51<-read_excel("Dataset/Final/Final51.xlsx",sheet = "Results")
Final52<-read_excel("Dataset/Final/Final52.xlsx",sheet = "Results")
Final53<-read_excel("Dataset/Final/Final53.xlsx",sheet = "Results")
Final54<-read_excel("Dataset/Final/Final54.xlsx",sheet = "Results")
Final55<-read_excel("Dataset/Final/Final55.xlsx",sheet = "Results")
Final56<-read_excel("Dataset/Final/Final56.xlsx",sheet = "Results")
Final57<-read_excel("Dataset/Final/Final57.xlsx",sheet = "Results")
Final58<-read_excel("Dataset/Final/Final58.xlsx",sheet = "Results")
Final59<-read_excel("Dataset/Final/Final59.xlsx",sheet = "Results")
Final60<-read_excel("Dataset/Final/Final60.xlsx",sheet = "Results")

names(Final1) <- names(Final10)
names(Final2) <- names(Final10) 
names(Final3) <- names(Final10) 

#Combine data
Data1<-rbind(Final1,Final2,Final3,Final4,Final5,Final6,Final7,Final8,Final9,Final10,Final11,Final12,Final13,Final14,
             Final15,Final16,Final17,Final18,Final19,Final20,Final21,Final22,Final23,Final24,Final25,Final26,Final28,
             Final28,Final29,Final30,Final31,Final32,Final33,Final34,Final35,Final36,Final37,Final38,Final39,Final40,
             Final41,Final42,Final43,Final44,Final45,Final46,Final47,Final48,Final49,Final50,Final51,Final52,Final53,
             Final54,Final55,Final56,Final57,Final58,Final59,Final60)

#Data1<-Data1[,c(-1,-2)]
#Data1[,2:5]<-NULL
#Data1[,3:77]<-NULL
#Data1<-as.data.frame(Data1)
#names(Data1)<-c("Company","NACE")
#Data1<-subset(Data1,!is.na(Data1$NACE))
#write.csv2(Data1, file = "NACE.csv",row.names = F)

#Remove unnecessary files
rm(Final1,Final2,Final3,Final4,Final5,Final6,Final7,Final8,Final9,Final10,Final11,Final12,Final13,Final14,
   Final15,Final16,Final17,Final18,Final19,Final20,Final21,Final22,Final23,Final24,Final25,Final26,Final27,
   Final28,Final29,Final30,Final31,Final32,Final33,Final34,Final35,Final36,Final37,Final38,Final39,Final40,
   Final41,Final42,Final43,Final44,Final45,Final46,Final47,Final48,Final49,Final50,Final51,Final52,Final53,
   Final54,Final55,Final56,Final57,Final58,Final59,Final60,Data1_files)

##Restructure dataset
#Remove Row of Numbers and Company Names + Rename/Order Columns
Data1<-Data1[,c(-1,-15,-25,-35,-45,-55,-65)]

names(Data1) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                  "PeerGroup","PeerSize","Date","OR19","OR18","OR17","OR16","OR15",
                  "OR14","OR13","OR12","OR11","TA19","TA18","TA17","TA16","TA15","TA14",
                  "TA13","TA12","TA11","ROE19","ROE18","ROE17","ROE16","ROE15","ROE14",
                  "ROE13","ROE12","ROE11","EMP19","EMP18","EMP17","EMP16","EMP15","EMP14",
                  "EMP13","EMP12","EMP11","INT19","INT18","INT17","INT16","INT15","INT14",
                  "INT13","INT12","INT11","CR19","CR18","CR17","CR16","CR15","CR14",
                  "CR13","CR12","CR11","NoSub2","SubBvDID","SubCountry","SubCity","SubType","SubNACE",
                  "SubOR","SubTA","SubEMP")

#Indicate numeric and character variables
cols.num <- c("AllPatents","PeerSize","OR19","OR18","OR17","OR16","OR15","OR14","OR13",
              "OR12","OR11","TA19","TA18","TA17","TA16","TA15","TA14","TA13","TA12","TA11",
              "ROE19","ROE18","ROE17","ROE16","ROE15","ROE14","ROE13","ROE12","ROE11",
              "EMP19","EMP18","EMP17","EMP16","EMP15","EMP14","EMP13","EMP12","EMP11",
              "INT19","INT18","INT17","INT16","INT15","INT14","INT13","INT12","INT11",
              "CR19","CR18","CR17","CR16","CR15","CR14","CR13","CR12","CR11","NoSub2")

Data1[cols.num] <- sapply(Data1[cols.num],as.numeric)
sapply(Data1, class)

#Indicate missing values
Data1<-na_if(Data1,"n.a.")

##Straighten dataset
#Remove Companies without patents
Data1$Pat1<-case_when(is.na(Data1$AllPatents) & !is.na(Data1$BvDID)~0,
                      !is.na(Data1$AllPatents) & !is.na(Data1$BvDID)~1)
Data1$Pat<-na.locf(Data1$Pat1)
sum(Data1$Pat,na.rm=TRUE)
Data1<-Data1[!(Data1$Pat==0),]
Data1$Pat1<-NULL
Data1$Pat<-NULL

#Test if there are companies without subsidiaries
Data1$Test<-ifelse(Data1$BvDID==lead(Data1$BvDID),1,NA)
length(which(!is.na(Data1$Test)))
Data1$Test<-NULL

#Create Company Age Variable
Date_files<-list.files(path="Dataset/Date",pattern ='.xlsx', full.names = T)
Date_data <- sapply(Date_files, read_excel, sheet=2,simplify=FALSE) %>% 
   bind_rows(.id = "id")

Date_data[1:3] <- NULL
names(Date_data)<-c("BvDID","Date2")

Data1<-left_join(Data1,Date_data,by="BvDID",na_matches="never")
Data1$Date<-Data1$Date2
Data1$Date2<-NULL

All2019<-Data1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,23,32,41,50,59,68,69,70,71,72,73,74,75,76)]
All2018<-Data1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,15,24,33,42,51,60,68,69,70,71,72,73,74,75,76)]
All2017<-Data1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,16,25,34,43,52,61,68,69,70,71,72,73,74,75,76)]
All2016<-Data1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,17,26,35,44,53,62,68,69,70,71,72,73,74,75,76)]
All2015<-Data1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,18,27,36,45,54,63,68,69,70,71,72,73,74,75,76)]
All2014<-Data1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,19,28,37,46,55,64,68,69,70,71,72,73,74,75,76)]
All2013<-Data1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,20,29,38,47,56,65,68,69,70,71,72,73,74,75,76)]
All2012<-Data1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,21,30,39,48,57,66,68,69,70,71,72,73,74,75,76)]
All2011<-Data1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,22,31,40,49,58,67,68,69,70,71,72,73,74,75,76)]

rm(Data1,cols.num,Date_data)

#Read type data
BU_Type<-fread("files_created_code1/Type.csv",sep=";", dec=",")
Type<-subset(BU_Type,Type=="PA")
names(Type)<-c("BvDID","State")
Type<-unique(Type,)

##Create variables - 2011

names(All2011) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                    "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR","NoSub2","SubBvDID",
                    "SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP")

#Create Company Age Variable
All2011$Age1<-str_sub(All2011$Date,-4,-1)
All2011$Age1<-as.numeric(All2011$Age1)
All2011$Age<-2011-All2011$Age1
All2011$Age1<-NULL

#Create a variable of intangible assets as share of tangible assets
All2011$ShareINT<-All2011$INT / All2011$TA

#Industry position - average ROE in the industry - laggard leader
All2011$NACE2<-substr(All2011$NACE,1,2)
All2011$IndPos0<-with(All2011, ave(All2011$ROE,All2011$NACE2, FUN=function(x) mean(x, na.rm=T)))
All2011$IndPos<-ifelse(All2011$ROE>All2011$IndPos0,1,0)

#High tech dummy
All2011$HighTech<-ifelse(substr(All2011$NACE,1,2)==21 |substr(All2011$NACE,1,2)==26 | substr(All2011$NACE,1,2)==20 |
                           substr(All2011$NACE,1,2)==27 | substr(All2011$NACE,1,2)==28 |substr(All2011$NACE,1,2)==29 |substr(All2011$NACE,1,2)==30,1,0)

##Create subsidiary based variables
#Create a new variable with BvIDs filled
All2011$BvDID<-na.locf(All2011$BvDID)
All2011$NewID<-na.locf(All2011$BvDID)
All2011$NewCountry<-na.locf(All2011$Country)

#Creating a variable of foreign subsidiaries
All2011$ForeignSub1<-ifelse(All2011$NewCountry!=All2011$SubCountry,1,NA)
All2011$ForeignSub1<-ifelse(is.na(All2011$ForeignSub1),0,All2011$ForeignSub1)
All2011$ForeignSub1<-as.numeric(All2011$ForeignSub1)
All2011$ForSub2<- with(All2011, ave(All2011$ForeignSub1,All2011$BvDID, FUN=sum))

#Share of foreign subsidiaries
All2011$MultiSub2<-All2011$ForSub2 / All2011$NoSub2

#Creating a variable of total foreign revenue
All2011$ForOR1<-ifelse(All2011$NewCountry!=All2011$SubCountry,All2011$SubOR,NA)
All2011$ForOR1<-ifelse(is.na(All2011$ForOR1),0,All2011$ForOR1)
All2011$ForOR1<-as.numeric(All2011$ForOR1)
All2011$ForOR<- with(All2011,ave(All2011$ForOR1,All2011$BvDID, FUN=sum))

#Creating a variable of total foreign assets
All2011$ForTA1<-ifelse(All2011$NewCountry!=All2011$SubCountry,All2011$SubTA,NA)
All2011$ForTA1<-ifelse(is.na(All2011$ForTA1),0,All2011$ForTA1)
All2011$ForTA1<-as.numeric(All2011$ForTA1)
All2011$ForTA<- with(All2011,ave(All2011$ForTA1,All2011$BvDID, FUN=sum))

#Creating a variable of total foreign employees
All2011$ForEMP1<-ifelse(All2011$NewCountry!=All2011$SubCountry,All2011$SubEMP,NA)
All2011$ForEMP1<-ifelse(is.na(All2011$ForEMP1),0,All2011$ForEMP1)
All2011$ForEMP1<-as.numeric(All2011$ForEMP1)
All2011$ForEMP<- with(All2011,ave(All2011$ForEMP1,All2011$BvDID, FUN=sum))

#Share of foreign revenue
All2011$MultiOR<-All2011$ForOR / as.numeric(All2011$OR)

#Share of foreign assets
All2011$MultiTA<-All2011$ForTA / as.numeric(All2011$TA)

#Share of foreign employees
All2011$MultiEmp<-All2011$ForEMP / as.numeric(All2011$EMP)

#Creating a variable of subsidiary revenue
All2011$SubOR1<-ifelse(All2011$BvDID!=All2011$SubBvDID,All2011$SubOR,NA)
All2011$SubOR1<-ifelse(is.na(All2011$SubOR1),0,All2011$SubOR1)
All2011$SubOR1<-as.numeric(All2011$SubOR1)
All2011$SubRev<- with(All2011,ave(All2011$SubOR1,All2011$BvDID, FUN=sum))

#Creating a variable of total subsidiary assets
All2011$SubTA1<-ifelse(All2011$BvDID!=All2011$SubBvDID,All2011$SubTA,0)
All2011$SubTA1<-ifelse(is.na(All2011$ForTA1),0,All2011$SubTA1)
All2011$SubTA1<-as.numeric(All2011$SubTA1)
All2011$SubAssets<- with(All2011,ave(All2011$SubTA1,All2011$BvDID, FUN=sum))

#Creating a variable of total subsidiary employees
All2011$SubEmp1<-ifelse(All2011$BvDID!=All2011$SubBvDID,All2011$SubEMP,0)
All2011$SubEmp1<-ifelse(is.na(All2011$SubEmp1),0,All2011$SubEmp1)
All2011$SubEmp1<-as.numeric(All2011$SubEmp1)
All2011$SubEmp<- with(All2011,ave(All2011$SubEmp1,All2011$BvDID, FUN=sum))

#Share of subsidiary revenue
All2011$ForSubOR<-All2011$SubRev / as.numeric(All2011$OR)

#Share of subsidiary assets
All2011$ForSubTA<-All2011$SubAssets / as.numeric(All2011$TA)

#Share of subsidiary employees
All2011$ForSubEmp<-All2011$SubEmp / as.numeric(All2011$EMP)

##Remove unnesseary rows and columns
All2011<-All2011[,c(-32,-35,-36,-37,-40,-42,-44,-49,-51,-53)]

#Remain the GUOs
All2011<-All2011[!duplicated(All2011$BvDID),]
length(which(!is.na(All2011$GUOBvDID)))

#Double check for duplicate companies
All2011$BvDID[duplicated(All2011$BvDID)] <- NA 
All2011<-All2011[complete.cases(All2011[,1]), ]


##Join AI with GUO data
All11$BvDID<-as.character(All11$BvDID)
All2011$BvDID<-as.character(All2011$BvDID)

#Merge dataset
All1<-left_join(All2011,All11,by="BvDID",na_matches="never")
length(which(!is.na(All1$GUOBvDID)))

#Remove all non-matches
All1<-subset(All1,!is.na(All1$GUOBvDID))

#Create the final variables
#Create dummy for AI Patents
All1$AIDummy<-ifelse(All1$AIPatents>0,1,0)

#Average AI Patents by other firms in the industry adoption Percentage patents
All1$AIPatents[is.na(All1$AIPatents)] <- 0
All1$IndAdopt<-with(All1, ave(All1$AIPatents,All1$NACE2, FUN=mean))

#Create an inverse dummy of AI Adoption
All1$InvDummy<-ifelse(All1$AIDummy==1 | is.na(All1$AIDummy),0,1)
All1$InvDummy[is.na(All1$InvDummy)] <- 0

# Average AI Patents by other firms in the industry doption Percentage patents
All1$AIDummy[is.na(All1$AIDummy)] <- 0
All1$AdPer01<-with(All1, ave(All1$AIDummy,All1$NACE2, FUN=sum))
All1$AdPer01[is.na(All1$AdPer01)] <- 0
All1$AIDummy[is.na(All1$AIDummy)] <- 0
All1$AdPer02<-with(All1, ave(All1$InvDummy,All1$NACE2, FUN=sum))
All1$AdPer02[is.na(All1$AdPer02)] <- 0
All1$AdPer<-All1$AdPer01/(All1$AdPer01+All1$AdPer02)

#Remove unnecessary variables
All1$InvDummy<-NULL
All1$AdPer01<-NULL
All1$AdPer02<-NULL

#Create dummy variable if state-connected
All1<-left_join(All1,Type,by="BvDID",na_matches="never")
All1$State<-ifelse(All1$State=="PA",1,0)
All1$State[is.na(All1$State)] <- 0

#Make sure year variable is correct
All1$Year<-2011

names(All1) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                 "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR",
                 "NoSub2","SubBvDID","SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP",
                 "Age","ShareINT","NACE2","IndPos","HighTech","ForSub2","MultiSub2","ForOR","ForTA","ForEmp",
                 "MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
                 "Year","NoSub1","ForSub1","MultiSub1","Countries","AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR",
                 "Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP","CGDPpc",
                 "CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub",
                 "Patents","Applications","GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents",
                 "SubPat","SubApp","ForPat","ForApp","AIPatents","AIApplications","AIGrantedPatents",
                 "AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents","AITechPatents","AITechApps",
                 "AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","TechDiv","MTB","MTD","MIB","MID",
                 "AIDummy","IndAdopt","AdPer","State")

col_order<-c("BvDID","Year","Name","GUOBvDID","Country","CRegion","WRegion","NACE","NACE2","Type","Products","PeerGroup","PeerSize",
             "Date","Age","State","AllPatents","Patents","Applications","GrantedPatents","GreenfieldPatents","BrownfieldPatents","SDPatents",
             "HighTechPatents","SubPat","SubApp","ForPat","ForApp","TechDiv",
             "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
             "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","AIDummy",
             "IndAdopt","AdPer","HighTech","IndPos",
             "MTB","MTD","MIB","MID","NoSub1","NoSub2","ForSub1","ForSub2","MultiSub1","MultiSub2","Countries","AvSub",
             "OR","TA","ROE","EMP","INT","ShareINT","CR",
             "ForOR","ForTA","ForEmp","MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
             "IPR","TP","GDP","GDPpc","RD","LR","TR","Inno","CPat","CPatRat","CGDP","TotalGDP","CGDPpc","CRD",
             "IPRDist","IPRDist2","IPRCountries","IPRSub","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")
All1<-All1[,col_order]


##Create variables - 2012

names(All2012) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                    "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR","NoSub2","SubBvDID",
                    "SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP")

#Create Company Age Variable
All2012$Age1<-str_sub(All2012$Date,-4,-1)
All2012$Age1<-as.numeric(All2012$Age1)
All2012$Age<-2012-All2012$Age1
All2012$Age1<-NULL

#Create a variable of intangible assets as share of tangible assets
All2012$ShareINT<-All2012$INT / All2012$TA

#Industry position - average ROE in the industry - laggard leader
All2012$NACE2<-substr(All2012$NACE,1,2)
All2012$IndPos0<-with(All2012, ave(All2012$ROE,All2012$NACE2, FUN=function(x) mean(x, na.rm=T)))
All2012$IndPos<-ifelse(All2012$ROE>All2012$IndPos0,1,0)

#High tech dummy
All2012$HighTech<-ifelse(substr(All2012$NACE,1,2)==21 |substr(All2012$NACE,1,2)==26 | substr(All2012$NACE,1,2)==20 |
                           substr(All2012$NACE,1,2)==27 | substr(All2012$NACE,1,2)==28 |substr(All2012$NACE,1,2)==29 |substr(All2012$NACE,1,2)==30,1,0)

##Create subsidiary based variables
#Create a new variable with BvIDs filled
All2012$BvDID<-na.locf(All2012$BvDID)
All2012$NewID<-na.locf(All2012$BvDID)
All2012$NewCountry<-na.locf(All2012$Country)

#Creating a variable of foreign subsidiaries
All2012$ForeignSub1<-ifelse(All2012$NewCountry!=All2012$SubCountry,1,NA)
All2012$ForeignSub1<-ifelse(is.na(All2012$ForeignSub1),0,All2012$ForeignSub1)
All2012$ForeignSub1<-as.numeric(All2012$ForeignSub1)
All2012$ForSub2<- with(All2012, ave(All2012$ForeignSub1,All2012$BvDID, FUN=sum))

#Share of foreign subsidiaries
All2012$MultiSub2<-All2012$ForSub2 / All2012$NoSub2

#Creating a variable of total foreign revenue
All2012$ForOR1<-ifelse(All2012$NewCountry!=All2012$SubCountry,All2012$SubOR,NA)
All2012$ForOR1<-ifelse(is.na(All2012$ForOR1),0,All2012$ForOR1)
All2012$ForOR1<-as.numeric(All2012$ForOR1)
All2012$ForOR<- with(All2012,ave(All2012$ForOR1,All2012$BvDID, FUN=sum))

#Creating a variable of total foreign assets
All2012$ForTA1<-ifelse(All2012$NewCountry!=All2012$SubCountry,All2012$SubTA,NA)
All2012$ForTA1<-ifelse(is.na(All2012$ForTA1),0,All2012$ForTA1)
All2012$ForTA1<-as.numeric(All2012$ForTA1)
All2012$ForTA<- with(All2012,ave(All2012$ForTA1,All2012$BvDID, FUN=sum))

#Creating a variable of total foreign employees
All2012$ForEMP1<-ifelse(All2012$NewCountry!=All2012$SubCountry,All2012$SubEMP,NA)
All2012$ForEMP1<-ifelse(is.na(All2012$ForEMP1),0,All2012$ForEMP1)
All2012$ForEMP1<-as.numeric(All2012$ForEMP1)
All2012$ForEMP<- with(All2012,ave(All2012$ForEMP1,All2012$BvDID, FUN=sum))

#Share of foreign revenue
All2012$MultiOR<-All2012$ForOR / as.numeric(All2012$OR)

#Share of foreign assets
All2012$MultiTA<-All2012$ForTA / as.numeric(All2012$TA)

#Share of foreign employees
All2012$MultiEmp<-All2012$ForEMP / as.numeric(All2012$EMP)

#Creating a variable of subsidiary revenue
All2012$SubOR1<-ifelse(All2012$BvDID!=All2012$SubBvDID,All2012$SubOR,NA)
All2012$SubOR1<-ifelse(is.na(All2012$SubOR1),0,All2012$SubOR1)
All2012$SubOR1<-as.numeric(All2012$SubOR1)
All2012$SubRev<- with(All2012,ave(All2012$SubOR1,All2012$BvDID, FUN=sum))

#Creating a variable of total subsidiary assets
All2012$SubTA1<-ifelse(All2012$BvDID!=All2012$SubBvDID,All2012$SubTA,0)
All2012$SubTA1<-ifelse(is.na(All2012$ForTA1),0,All2012$SubTA1)
All2012$SubTA1<-as.numeric(All2012$SubTA1)
All2012$SubAssets<- with(All2012,ave(All2012$SubTA1,All2012$BvDID, FUN=sum))

#Creating a variable of total subsidiary employees
All2012$SubEmp1<-ifelse(All2012$BvDID!=All2012$SubBvDID,All2012$SubEMP,0)
All2012$SubEmp1<-ifelse(is.na(All2012$SubEmp1),0,All2012$SubEmp1)
All2012$SubEmp1<-as.numeric(All2012$SubEmp1)
All2012$SubEmp<- with(All2012,ave(All2012$SubEmp1,All2012$BvDID, FUN=sum))

#Share of subsidiary revenue
All2012$ForSubOR<-All2012$SubRev / as.numeric(All2012$OR)

#Share of subsidiary assets
All2012$ForSubTA<-All2012$SubAssets / as.numeric(All2012$TA)

#Share of subsidiary employees
All2012$ForSubEmp<-All2012$SubEmp / as.numeric(All2012$EMP)

##Remove unnesseary rows and columns
All2012<-All2012[,c(-32,-35,-36,-37,-40,-42,-44,-49,-51,-53)]

#Remain the GUOs
All2012<-All2012[!duplicated(All2012$BvDID),]
length(which(!is.na(All2012$GUOBvDID)))

#Double check for duplicate companies
All2012$BvDID[duplicated(All2012$BvDID)] <- NA 
All2012<-All2012[complete.cases(All2012[,1]), ]

##Join AI with GUO data
All12$BvDID<-as.character(All12$BvDID)
All2012$BvDID<-as.character(All2012$BvDID)

#Merge dataset
All2<-left_join(All2012,All12,by="BvDID",na_matches="never")
length(which(!is.na(All2$GUOBvDID)))

#Remove all non-matches
All2<-subset(All2,!is.na(All2$GUOBvDID))

#Create the final variables
#Create dummy for AI Patents
All2$AIDummy<-ifelse(All2$AIPatents>0,1,0)

#Average AI Patents by other firms in the industry adoption Percentage patents
All2$AIPatents[is.na(All2$AIPatents)] <- 0
All2$IndAdopt<-with(All2, ave(All2$AIPatents,All2$NACE2, FUN=mean))

#Create an inverse dummy of AI Adoption
All2$InvDummy<-ifelse(All2$AIDummy==1 | is.na(All2$AIDummy),0,1)
All2$InvDummy[is.na(All2$InvDummy)] <- 0

# Average AI Patents by other firms in the industry doption Percentage patents
All2$AIDummy[is.na(All2$AIDummy)] <- 0
All2$AdPer01<-with(All2, ave(All2$AIDummy,All2$NACE2, FUN=sum))
All2$AdPer01[is.na(All2$AdPer01)] <- 0
All2$AIDummy[is.na(All2$AIDummy)] <- 0
All2$AdPer02<-with(All2, ave(All2$InvDummy,All2$NACE2, FUN=sum))
All2$AdPer01[is.na(All2$AdPer01)] <- 0
All2$AdPer<-All2$AdPer01/(All2$AdPer01+All2$AdPer02)

#Remove unnecessary variables
All2$InvDummy<-NULL
All2$AdPer01<-NULL
All2$AdPer02<-NULL

#Create dummy variable if state-connected
All2<-left_join(All2,Type,by="BvDID",na_matches="never")
All2$State<-ifelse(All2$State=="PA",1,0)
All2$State[is.na(All2$State)] <- 0

#MAke sure year variable is correct
All2$Year<-2012

names(All2) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                 "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR",
                 "NoSub2","SubBvDID","SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP",
                 "Age","ShareINT","NACE2","IndPos","HighTech","ForSub2","MultiSub2","ForOR","ForTA","ForEmp",
                 "MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
                 "Year","NoSub1","ForSub1","MultiSub1","Countries","AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR",
                 "Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP","CGDPpc",
                 "CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub",
                 "Patents","Applications","GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents",
                 "SubPat","SubApp","ForPat","ForApp","AIPatents","AIApplications","AIGrantedPatents",
                 "AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents","AITechPatents","AITechApps",
                 "AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","TechDiv","MTB","MTD","MIB","MID",
                 "AIDummy","IndAdopt","AdPer","State")

col_order<-c("BvDID","Year","Name","GUOBvDID","Country","CRegion","WRegion","NACE","NACE2","Type","Products","PeerGroup","PeerSize",
             "Date","Age","State","AllPatents","Patents","Applications","GrantedPatents","GreenfieldPatents","BrownfieldPatents","SDPatents",
             "HighTechPatents","SubPat","SubApp","ForPat","ForApp","TechDiv",
             "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
             "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","AIDummy",
             "IndAdopt","AdPer","HighTech","IndPos",
             "MTB","MTD","MIB","MID","NoSub1","NoSub2","ForSub1","ForSub2","MultiSub1","MultiSub2","Countries","AvSub",
             "OR","TA","ROE","EMP","INT","ShareINT","CR",
             "ForOR","ForTA","ForEmp","MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
             "IPR","TP","GDP","GDPpc","RD","LR","TR","Inno","CPat","CPatRat","CGDP","TotalGDP","CGDPpc","CRD",
             "IPRDist","IPRDist2","IPRCountries","IPRSub","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")
All2<-All2[,col_order]


##Create variables - 2013

names(All2013) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                    "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR","NoSub2","SubBvDID",
                    "SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP")

#Create Company Age Variable
All2013$Age1<-str_sub(All2013$Date,-4,-1)
All2013$Age1<-as.numeric(All2013$Age1)
All2013$Age<-2013-All2013$Age1
All2013$Age1<-NULL

#Create a variable of intangible assets as share of tangible assets
All2013$ShareINT<-All2013$INT / All2013$TA

#Industry position - average ROE in the industry - laggard leader
All2013$NACE2<-substr(All2013$NACE,1,2)
All2013$IndPos0<-with(All2013, ave(All2013$ROE,All2013$NACE2, FUN=function(x) mean(x, na.rm=T)))
All2013$IndPos<-ifelse(All2013$ROE>All2013$IndPos0,1,0)

#High tech dummy
All2013$HighTech<-ifelse(substr(All2013$NACE,1,2)==21 |substr(All2013$NACE,1,2)==26 | substr(All2013$NACE,1,2)==20 |
                           substr(All2013$NACE,1,2)==27 | substr(All2013$NACE,1,2)==28 |substr(All2013$NACE,1,2)==29 |substr(All2013$NACE,1,2)==30,1,0)

##Create subsidiary based variables
#Create a new variable with BvIDs filled
All2013$BvDID<-na.locf(All2013$BvDID)
All2013$NewID<-na.locf(All2013$BvDID)
All2013$NewCountry<-na.locf(All2013$Country)

#Creating a variable of foreign subsidiaries
All2013$ForeignSub1<-ifelse(All2013$NewCountry!=All2013$SubCountry,1,NA)
All2013$ForeignSub1<-ifelse(is.na(All2013$ForeignSub1),0,All2013$ForeignSub1)
All2013$ForeignSub1<-as.numeric(All2013$ForeignSub1)
All2013$ForSub2<- with(All2013, ave(All2013$ForeignSub1,All2013$BvDID, FUN=sum))

#Share of foreign subsidiaries
All2013$MultiSub2<-All2013$ForSub2 / All2013$NoSub2

#Creating a variable of total foreign revenue
All2013$ForOR1<-ifelse(All2013$NewCountry!=All2013$SubCountry,All2013$SubOR,NA)
All2013$ForOR1<-ifelse(is.na(All2013$ForOR1),0,All2013$ForOR1)
All2013$ForOR1<-as.numeric(All2013$ForOR1)
All2013$ForOR<- with(All2013,ave(All2013$ForOR1,All2013$BvDID, FUN=sum))

#Creating a variable of total foreign assets
All2013$ForTA1<-ifelse(All2013$NewCountry!=All2013$SubCountry,All2013$SubTA,NA)
All2013$ForTA1<-ifelse(is.na(All2013$ForTA1),0,All2013$ForTA1)
All2013$ForTA1<-as.numeric(All2013$ForTA1)
All2013$ForTA<- with(All2013,ave(All2013$ForTA1,All2013$BvDID, FUN=sum))

#Creating a variable of total foreign employees
All2013$ForEMP1<-ifelse(All2013$NewCountry!=All2013$SubCountry,All2013$SubEMP,NA)
All2013$ForEMP1<-ifelse(is.na(All2013$ForEMP1),0,All2013$ForEMP1)
All2013$ForEMP1<-as.numeric(All2013$ForEMP1)
All2013$ForEMP<- with(All2013,ave(All2013$ForEMP1,All2013$BvDID, FUN=sum))

#Share of foreign revenue
All2013$MultiOR<-All2013$ForOR / as.numeric(All2013$OR)

#Share of foreign assets
All2013$MultiTA<-All2013$ForTA / as.numeric(All2013$TA)

#Share of foreign employees
All2013$MultiEmp<-All2013$ForEMP / as.numeric(All2013$EMP)

#Creating a variable of subsidiary revenue
All2013$SubOR1<-ifelse(All2013$BvDID!=All2013$SubBvDID,All2013$SubOR,NA)
All2013$SubOR1<-ifelse(is.na(All2013$SubOR1),0,All2013$SubOR1)
All2013$SubOR1<-as.numeric(All2013$SubOR1)
All2013$SubRev<- with(All2013,ave(All2013$SubOR1,All2013$BvDID, FUN=sum))

#Creating a variable of total subsidiary assets
All2013$SubTA1<-ifelse(All2013$BvDID!=All2013$SubBvDID,All2013$SubTA,0)
All2013$SubTA1<-ifelse(is.na(All2013$ForTA1),0,All2013$SubTA1)
All2013$SubTA1<-as.numeric(All2013$SubTA1)
All2013$SubAssets<- with(All2013,ave(All2013$SubTA1,All2013$BvDID, FUN=sum))

#Creating a variable of total subsidiary employees
All2013$SubEmp1<-ifelse(All2013$BvDID!=All2013$SubBvDID,All2013$SubEMP,0)
All2013$SubEmp1<-ifelse(is.na(All2013$SubEmp1),0,All2013$SubEmp1)
All2013$SubEmp1<-as.numeric(All2013$SubEmp1)
All2013$SubEmp<- with(All2013,ave(All2013$SubEmp1,All2013$BvDID, FUN=sum))

#Share of subsidiary revenue
All2013$ForSubOR<-All2013$SubRev / as.numeric(All2013$OR)

#Share of subsidiary assets
All2013$ForSubTA<-All2013$SubAssets / as.numeric(All2013$TA)

#Share of subsidiary employees
All2013$ForSubEmp<-All2013$SubEmp / as.numeric(All2013$EMP)

##Remove unnesseary rows and columns
All2013<-All2013[,c(-32,-35,-36,-37,-40,-42,-44,-49,-51,-53)]

#Remain the GUOs
All2013<-All2013[!duplicated(All2013$BvDID),]
length(which(!is.na(All2013$GUOBvDID)))

#Double check for duplicate companies
All2013$BvDID[duplicated(All2013$BvDID)] <- NA 
All2013<-All2013[complete.cases(All2013[,1]), ]

##Join AI with GUO data
All13$BvDID<-as.character(All13$BvDID)
All2013$BvDID<-as.character(All2013$BvDID)

#Merge dataset
All3<-left_join(All2013,All13,by="BvDID",na_matches="never")
length(which(!is.na(All3$GUOBvDID)))

#Remove all non-matches
All3<-subset(All3,!is.na(All3$GUOBvDID))

#Create the final variables
#Create dummy for AI Patents
All3$AIDummy<-ifelse(All3$AIPatents>0,1,0)

#Average AI Patents by other firms in the industry adoption Percentage patents
All3$AIPatents[is.na(All3$AIPatents)] <- 0
All3$IndAdopt<-with(All3, ave(All3$AIPatents,All3$NACE2, FUN=mean))

#Create an inverse dummy of AI Adoption
All3$InvDummy<-ifelse(All3$AIDummy==1 | is.na(All3$AIDummy),0,1)
All3$InvDummy[is.na(All3$InvDummy)] <- 0

# Average AI Patents by other firms in the industry doption Percentage patents
All3$AIDummy[is.na(All3$AIDummy)] <- 0
All3$AdPer01<-with(All3, ave(All3$AIDummy,All3$NACE2, FUN=sum))
All3$AdPer01[is.na(All3$AdPer01)] <- 0
All3$AIDummy[is.na(All3$AIDummy)] <- 0
All3$AdPer02<-with(All3, ave(All3$InvDummy,All3$NACE2, FUN=sum))
All3$AdPer01[is.na(All3$AdPer01)] <- 0
All3$AdPer<-All3$AdPer01/(All3$AdPer01+All3$AdPer02)

#Remove unnecessary variables
All3$InvDummy<-NULL
All3$AdPer01<-NULL
All3$AdPer02<-NULL

#Create dummy variable if state-connected
All3<-left_join(All3,Type,by="BvDID",na_matches="never")
All3$State<-ifelse(All3$State=="PA",1,0)
All3$State[is.na(All3$State)] <- 0

#Make sure year variable is correct
All3$Year<-2013

names(All3) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                 "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR",
                 "NoSub2","SubBvDID","SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP",
                 "Age","ShareINT","NACE2","IndPos","HighTech","ForSub2","MultiSub2","ForOR","ForTA","ForEmp",
                 "MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
                 "Year","NoSub1","ForSub1","MultiSub1","Countries","AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR",
                 "Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP","CGDPpc",
                 "CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub",
                 "Patents","Applications","GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents",
                 "SubPat","SubApp","ForPat","ForApp","AIPatents","AIApplications","AIGrantedPatents",
                 "AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents","AITechPatents","AITechApps",
                 "AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","TechDiv","MTB","MTD","MIB","MID",
                 "AIDummy","IndAdopt","AdPer","State")

col_order<-c("BvDID","Year","Name","GUOBvDID","Country","CRegion","WRegion","NACE","NACE2","Type","Products","PeerGroup","PeerSize",
             "Date","Age","State","AllPatents","Patents","Applications","GrantedPatents","GreenfieldPatents","BrownfieldPatents","SDPatents",
             "HighTechPatents","SubPat","SubApp","ForPat","ForApp","TechDiv",
             "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
             "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","AIDummy",
             "IndAdopt","AdPer","HighTech","IndPos",
             "MTB","MTD","MIB","MID","NoSub1","NoSub2","ForSub1","ForSub2","MultiSub1","MultiSub2","Countries","AvSub",
             "OR","TA","ROE","EMP","INT","ShareINT","CR",
             "ForOR","ForTA","ForEmp","MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
             "IPR","TP","GDP","GDPpc","RD","LR","TR","Inno","CPat","CPatRat","CGDP","TotalGDP","CGDPpc","CRD",
             "IPRDist","IPRDist2","IPRCountries","IPRSub","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")
All3<-All3[,col_order]



##Create variables - 2014

names(All2014) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                    "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR","NoSub2","SubBvDID",
                    "SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP")

#Create Company Age Variable
All2014$Age1<-str_sub(All2014$Date,-4,-1)
All2014$Age1<-as.numeric(All2014$Age1)
All2014$Age<-2014-All2014$Age1
All2014$Age1<-NULL

#Create a variable of intangible assets as share of tangible assets
All2014$ShareINT<-All2014$INT / All2014$TA

#Industry position - average ROE in the industry - laggard leader
All2014$NACE2<-substr(All2014$NACE,1,2)
All2014$IndPos0<-with(All2014, ave(All2014$ROE,All2014$NACE2, FUN=function(x) mean(x, na.rm=T)))
All2014$IndPos<-ifelse(All2014$ROE>All2014$IndPos0,1,0)

#High tech dummy
All2014$HighTech<-ifelse(substr(All2014$NACE,1,2)==21 |substr(All2014$NACE,1,2)==26 | substr(All2014$NACE,1,2)==20 |
                           substr(All2014$NACE,1,2)==27 | substr(All2014$NACE,1,2)==28 |substr(All2014$NACE,1,2)==29 |substr(All2014$NACE,1,2)==30,1,0)

##Create subsidiary based variables
#Create a new variable with BvIDs filled
All2014$BvDID<-na.locf(All2014$BvDID)
All2014$NewID<-na.locf(All2014$BvDID)
All2014$NewCountry<-na.locf(All2014$Country)

#Creating a variable of foreign subsidiaries
All2014$ForeignSub1<-ifelse(All2014$NewCountry!=All2014$SubCountry,1,NA)
All2014$ForeignSub1<-ifelse(is.na(All2014$ForeignSub1),0,All2014$ForeignSub1)
All2014$ForeignSub1<-as.numeric(All2014$ForeignSub1)
All2014$ForSub2<- with(All2014, ave(All2014$ForeignSub1,All2014$BvDID, FUN=sum))

#Share of foreign subsidiaries
All2014$MultiSub2<-All2014$ForSub2 / All2014$NoSub2

#Creating a variable of total foreign revenue
All2014$ForOR1<-ifelse(All2014$NewCountry!=All2014$SubCountry,All2014$SubOR,NA)
All2014$ForOR1<-ifelse(is.na(All2014$ForOR1),0,All2014$ForOR1)
All2014$ForOR1<-as.numeric(All2014$ForOR1)
All2014$ForOR<- with(All2014,ave(All2014$ForOR1,All2014$BvDID, FUN=sum))

#Creating a variable of total foreign assets
All2014$ForTA1<-ifelse(All2014$NewCountry!=All2014$SubCountry,All2014$SubTA,NA)
All2014$ForTA1<-ifelse(is.na(All2014$ForTA1),0,All2014$ForTA1)
All2014$ForTA1<-as.numeric(All2014$ForTA1)
All2014$ForTA<- with(All2014,ave(All2014$ForTA1,All2014$BvDID, FUN=sum))

#Creating a variable of total foreign employees
All2014$ForEMP1<-ifelse(All2014$NewCountry!=All2014$SubCountry,All2014$SubEMP,NA)
All2014$ForEMP1<-ifelse(is.na(All2014$ForEMP1),0,All2014$ForEMP1)
All2014$ForEMP1<-as.numeric(All2014$ForEMP1)
All2014$ForEMP<- with(All2014,ave(All2014$ForEMP1,All2014$BvDID, FUN=sum))

#Share of foreign revenue
All2014$MultiOR<-All2014$ForOR / as.numeric(All2014$OR)

#Share of foreign assets
All2014$MultiTA<-All2014$ForTA / as.numeric(All2014$TA)

#Share of foreign employees
All2014$MultiEmp<-All2014$ForEMP / as.numeric(All2014$EMP)

#Creating a variable of subsidiary revenue
All2014$SubOR1<-ifelse(All2014$BvDID!=All2014$SubBvDID,All2014$SubOR,NA)
All2014$SubOR1<-ifelse(is.na(All2014$SubOR1),0,All2014$SubOR1)
All2014$SubOR1<-as.numeric(All2014$SubOR1)
All2014$SubRev<- with(All2014,ave(All2014$SubOR1,All2014$BvDID, FUN=sum))

#Creating a variable of total subsidiary assets
All2014$SubTA1<-ifelse(All2014$BvDID!=All2014$SubBvDID,All2014$SubTA,0)
All2014$SubTA1<-ifelse(is.na(All2014$ForTA1),0,All2014$SubTA1)
All2014$SubTA1<-as.numeric(All2014$SubTA1)
All2014$SubAssets<- with(All2014,ave(All2014$SubTA1,All2014$BvDID, FUN=sum))

#Creating a variable of total subsidiary employees
All2014$SubEmp1<-ifelse(All2014$BvDID!=All2014$SubBvDID,All2014$SubEMP,0)
All2014$SubEmp1<-ifelse(is.na(All2014$SubEmp1),0,All2014$SubEmp1)
All2014$SubEmp1<-as.numeric(All2014$SubEmp1)
All2014$SubEmp<- with(All2014,ave(All2014$SubEmp1,All2014$BvDID, FUN=sum))

#Share of subsidiary revenue
All2014$ForSubOR<-All2014$SubRev / as.numeric(All2014$OR)

#Share of subsidiary assets
All2014$ForSubTA<-All2014$SubAssets / as.numeric(All2014$TA)

#Share of subsidiary employees
All2014$ForSubEmp<-All2014$SubEmp / as.numeric(All2014$EMP)

##Remove unnesseary rows and columns
All2014<-All2014[,c(-32,-35,-36,-37,-40,-42,-44,-49,-51,-53)]

#Remain the GUOs
All2014<-All2014[!duplicated(All2014$BvDID),]
length(which(!is.na(All2014$GUOBvDID)))

#Double check for duplicate companies
All2014$BvDID[duplicated(All2014$BvDID)] <- NA 
All2014<-All2014[complete.cases(All2014[,1]), ]

##Join AI with GUO data
All14$BvDID<-as.character(All14$BvDID)
All2014$BvDID<-as.character(All2014$BvDID)

#Merge dataset
All4<-left_join(All2014,All14,by="BvDID",na_matches="never")
length(which(!is.na(All4$GUOBvDID)))

#Remove all non-matches
All4<-subset(All4,!is.na(All4$GUOBvDID))

#Create the final variables
#Create dummy for AI Patents
All4$AIDummy<-ifelse(All4$AIPatents>0,1,0)

#Average AI Patents by other firms in the industry adoption Percentage patents
All4$AIPatents[is.na(All4$AIPatents)] <- 0
All4$IndAdopt<-with(All4, ave(All4$AIPatents,All4$NACE2, FUN=mean))

#Create an inverse dummy of AI Adoption
All4$InvDummy<-ifelse(All4$AIDummy==1 | is.na(All4$AIDummy),0,1)
All4$InvDummy[is.na(All4$InvDummy)] <- 0

# Average AI Patents by other firms in the industry doption Percentage patents
All4$AIDummy[is.na(All4$AIDummy)] <- 0
All4$AdPer01<-with(All4, ave(All4$AIDummy,All4$NACE2, FUN=sum))
All4$AdPer01[is.na(All4$AdPer01)] <- 0
All4$AIDummy[is.na(All4$AIDummy)] <- 0
All4$AdPer02<-with(All4, ave(All4$InvDummy,All4$NACE2, FUN=sum))
All4$AdPer01[is.na(All4$AdPer01)] <- 0
All4$AdPer<-All4$AdPer01/(All4$AdPer01+All4$AdPer02)

#Remove unnecessary variables
All4$InvDummy<-NULL
All4$AdPer01<-NULL
All4$AdPer02<-NULL

#Create dummy variable if state-connected
All4<-left_join(All4,Type,by="BvDID",na_matches="never")
All4$State<-ifelse(All4$State=="PA",1,0)
All4$State[is.na(All4$State)] <- 0

#MAke sure year variable is correct
All4$Year<-2014

names(All4) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                 "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR",
                 "NoSub2","SubBvDID","SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP",
                 "Age","ShareINT","NACE2","IndPos","HighTech","ForSub2","MultiSub2","ForOR","ForTA","ForEmp",
                 "MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
                 "Year","NoSub1","ForSub1","MultiSub1","Countries","AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR",
                 "Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP","CGDPpc",
                 "CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub",
                 "Patents","Applications","GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents",
                 "SubPat","SubApp","ForPat","ForApp","AIPatents","AIApplications","AIGrantedPatents",
                 "AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents","AITechPatents","AITechApps",
                 "AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","TechDiv","MTB","MTD","MIB","MID",
                 "AIDummy","IndAdopt","AdPer","State")

col_order<-c("BvDID","Year","Name","GUOBvDID","Country","CRegion","WRegion","NACE","NACE2","Type","Products","PeerGroup","PeerSize",
             "Date","Age","State","AllPatents","Patents","Applications","GrantedPatents","GreenfieldPatents","BrownfieldPatents","SDPatents",
             "HighTechPatents","SubPat","SubApp","ForPat","ForApp","TechDiv",
             "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
             "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","AIDummy",
             "IndAdopt","AdPer","HighTech","IndPos",
             "MTB","MTD","MIB","MID","NoSub1","NoSub2","ForSub1","ForSub2","MultiSub1","MultiSub2","Countries","AvSub",
             "OR","TA","ROE","EMP","INT","ShareINT","CR",
             "ForOR","ForTA","ForEmp","MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
             "IPR","TP","GDP","GDPpc","RD","LR","TR","Inno","CPat","CPatRat","CGDP","TotalGDP","CGDPpc","CRD",
             "IPRDist","IPRDist2","IPRCountries","IPRSub","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")
All4<-All4[,col_order]


##Create variables - 2015
names(All2015) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                    "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR","NoSub2","SubBvDID",
                    "SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP")

#Create Company Age Variable
All2015$Age1<-str_sub(All2015$Date,-4,-1)
All2015$Age1<-as.numeric(All2015$Age1)
All2015$Age<-2015-All2015$Age1
All2015$Age1<-NULL

#Create a variable of intangible assets as share of tangible assets
All2015$ShareINT<-All2015$INT / All2015$TA

#Industry position - average ROE in the industry - laggard leader
All2015$NACE2<-substr(All2015$NACE,1,2)
All2015$IndPos0<-with(All2015, ave(All2015$ROE,All2015$NACE2, FUN=function(x) mean(x, na.rm=T)))
All2015$IndPos<-ifelse(All2015$ROE>All2015$IndPos0,1,0)

#High tech dummy
All2015$HighTech<-ifelse(substr(All2015$NACE,1,2)==21 |substr(All2015$NACE,1,2)==26 | substr(All2015$NACE,1,2)==20 |
                           substr(All2015$NACE,1,2)==27 | substr(All2015$NACE,1,2)==28 |substr(All2015$NACE,1,2)==29 |substr(All2015$NACE,1,2)==30,1,0)

##Create subsidiary based variables
#Create a new variable with BvIDs filled
All2015$BvDID<-na.locf(All2015$BvDID)
All2015$NewID<-na.locf(All2015$BvDID)
All2015$NewCountry<-na.locf(All2015$Country)

#Creating a variable of foreign subsidiaries
All2015$ForeignSub1<-ifelse(All2015$NewCountry!=All2015$SubCountry,1,NA)
All2015$ForeignSub1<-ifelse(is.na(All2015$ForeignSub1),0,All2015$ForeignSub1)
All2015$ForeignSub1<-as.numeric(All2015$ForeignSub1)
All2015$ForSub2<- with(All2015, ave(All2015$ForeignSub1,All2015$BvDID, FUN=sum))

#Share of foreign subsidiaries
All2015$MultiSub2<-All2015$ForSub2 / All2015$NoSub2

#Creating a variable of total foreign revenue
All2015$ForOR1<-ifelse(All2015$NewCountry!=All2015$SubCountry,All2015$SubOR,NA)
All2015$ForOR1<-ifelse(is.na(All2015$ForOR1),0,All2015$ForOR1)
All2015$ForOR1<-as.numeric(All2015$ForOR1)
All2015$ForOR<- with(All2015,ave(All2015$ForOR1,All2015$BvDID, FUN=sum))

#Creating a variable of total foreign assets
All2015$ForTA1<-ifelse(All2015$NewCountry!=All2015$SubCountry,All2015$SubTA,NA)
All2015$ForTA1<-ifelse(is.na(All2015$ForTA1),0,All2015$ForTA1)
All2015$ForTA1<-as.numeric(All2015$ForTA1)
All2015$ForTA<- with(All2015,ave(All2015$ForTA1,All2015$BvDID, FUN=sum))

#Creating a variable of total foreign employees
All2015$ForEMP1<-ifelse(All2015$NewCountry!=All2015$SubCountry,All2015$SubEMP,NA)
All2015$ForEMP1<-ifelse(is.na(All2015$ForEMP1),0,All2015$ForEMP1)
All2015$ForEMP1<-as.numeric(All2015$ForEMP1)
All2015$ForEMP<- with(All2015,ave(All2015$ForEMP1,All2015$BvDID, FUN=sum))

#Share of foreign revenue
All2015$MultiOR<-All2015$ForOR / as.numeric(All2015$OR)

#Share of foreign assets
All2015$MultiTA<-All2015$ForTA / as.numeric(All2015$TA)

#Share of foreign employees
All2015$MultiEmp<-All2015$ForEMP / as.numeric(All2015$EMP)

#Creating a variable of subsidiary revenue
All2015$SubOR1<-ifelse(All2015$BvDID!=All2015$SubBvDID,All2015$SubOR,NA)
All2015$SubOR1<-ifelse(is.na(All2015$SubOR1),0,All2015$SubOR1)
All2015$SubOR1<-as.numeric(All2015$SubOR1)
All2015$SubRev<- with(All2015,ave(All2015$SubOR1,All2015$BvDID, FUN=sum))

#Creating a variable of total subsidiary assets
All2015$SubTA1<-ifelse(All2015$BvDID!=All2015$SubBvDID,All2015$SubTA,0)
All2015$SubTA1<-ifelse(is.na(All2015$ForTA1),0,All2015$SubTA1)
All2015$SubTA1<-as.numeric(All2015$SubTA1)
All2015$SubAssets<- with(All2015,ave(All2015$SubTA1,All2015$BvDID, FUN=sum))

#Creating a variable of total subsidiary employees
All2015$SubEmp1<-ifelse(All2015$BvDID!=All2015$SubBvDID,All2015$SubEMP,0)
All2015$SubEmp1<-ifelse(is.na(All2015$SubEmp1),0,All2015$SubEmp1)
All2015$SubEmp1<-as.numeric(All2015$SubEmp1)
All2015$SubEmp<- with(All2015,ave(All2015$SubEmp1,All2015$BvDID, FUN=sum))

#Share of subsidiary revenue
All2015$ForSubOR<-All2015$SubRev / as.numeric(All2015$OR)

#Share of subsidiary assets
All2015$ForSubTA<-All2015$SubAssets / as.numeric(All2015$TA)

#Share of subsidiary employees
All2015$ForSubEmp<-All2015$SubEmp / as.numeric(All2015$EMP)

##Remove unnesseary rows and columns
All2015<-All2015[,c(-32,-35,-36,-37,-40,-42,-44,-49,-51,-53)]

#Remain the GUOs
All2015<-All2015[!duplicated(All2015$BvDID),]
length(which(!is.na(All2015$GUOBvDID)))

#Double check for duplicate companies
All2015$BvDID[duplicated(All2015$BvDID)] <- NA 
All2015<-All2015[complete.cases(All2015[,1]), ]

##Join AI with GUO data
All15$BvDID<-as.character(All15$BvDID)
All2015$BvDID<-as.character(All2015$BvDID)

#Merge dataset
All5<-left_join(All2015,All15,by="BvDID",na_matches="never")
length(which(!is.na(All5$GUOBvDID)))

#Remove all non-matches
All5<-subset(All5,!is.na(All5$GUOBvDID))

#Create the final variables
#Create dummy for AI Patents
All5$AIDummy<-ifelse(All5$AIPatents>0,1,0)

#Average AI Patents by other firms in the industry adoption Percentage patents
All5$AIPatents[is.na(All5$AIPatents)] <- 0
All5$IndAdopt<-with(All5, ave(All5$AIPatents,All5$NACE2, FUN=mean))

#Create an inverse dummy of AI Adoption
All5$InvDummy<-ifelse(All5$AIDummy==1 | is.na(All5$AIDummy),0,1)
All5$InvDummy[is.na(All5$InvDummy)] <- 0

# Average AI Patents by other firms in the industry doption Percentage patents
All5$AIDummy[is.na(All5$AIDummy)] <- 0
All5$AdPer01<-with(All5, ave(All5$AIDummy,All5$NACE2, FUN=sum))
All5$AdPer01[is.na(All5$AdPer01)] <- 0
All5$AIDummy[is.na(All5$AIDummy)] <- 0
All5$AdPer02<-with(All5, ave(All5$InvDummy,All5$NACE2, FUN=sum))
All5$AdPer01[is.na(All5$AdPer01)] <- 0
All5$AdPer<-All5$AdPer01/(All5$AdPer01+All5$AdPer02)

#Remove unnecessary variables
All5$InvDummy<-NULL
All5$AdPer01<-NULL
All5$AdPer02<-NULL

#Create dummy variable if state-connected
All5<-left_join(All5,Type,by="BvDID",na_matches="never")
All5$State<-ifelse(All5$State=="PA",1,0)
All5$State[is.na(All5$State)] <- 0

#MAke sure year variable is correct
All5$Year<-2015

names(All5) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                 "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR",
                 "NoSub2","SubBvDID","SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP",
                 "Age","ShareINT","NACE2","IndPos","HighTech","ForSub2","MultiSub2","ForOR","ForTA","ForEmp",
                 "MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
                 "Year","NoSub1","ForSub1","MultiSub1","Countries","AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR",
                 "Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP","CGDPpc",
                 "CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub",
                 "Patents","Applications","GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents",
                 "SubPat","SubApp","ForPat","ForApp","AIPatents","AIApplications","AIGrantedPatents",
                 "AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents","AITechPatents","AITechApps",
                 "AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","TechDiv","MTB","MTD","MIB","MID",
                 "AIDummy","IndAdopt","AdPer","State")

col_order<-c("BvDID","Year","Name","GUOBvDID","Country","CRegion","WRegion","NACE","NACE2","Type","Products","PeerGroup","PeerSize",
             "Date","Age","State","AllPatents","Patents","Applications","GrantedPatents","GreenfieldPatents","BrownfieldPatents","SDPatents",
             "HighTechPatents","SubPat","SubApp","ForPat","ForApp","TechDiv",
             "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
             "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","AIDummy",
             "IndAdopt","AdPer","HighTech","IndPos",
             "MTB","MTD","MIB","MID","NoSub1","NoSub2","ForSub1","ForSub2","MultiSub1","MultiSub2","Countries","AvSub",
             "OR","TA","ROE","EMP","INT","ShareINT","CR",
             "ForOR","ForTA","ForEmp","MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
             "IPR","TP","GDP","GDPpc","RD","LR","TR","Inno","CPat","CPatRat","CGDP","TotalGDP","CGDPpc","CRD",
             "IPRDist","IPRDist2","IPRCountries","IPRSub","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")
All5<-All5[,col_order]


##Create variables - 2016

names(All2016) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                    "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR","NoSub2","SubBvDID",
                    "SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP")

#Create Company Age Variable
All2016$Age1<-str_sub(All2016$Date,-4,-1)
All2016$Age1<-as.numeric(All2016$Age1)
All2016$Age<-2016-All2016$Age1
All2016$Age1<-NULL

#Create a variable of intangible assets as share of tangible assets
All2016$ShareINT<-All2016$INT / All2016$TA

#Industry position - average ROE in the industry - laggard leader
All2016$NACE2<-substr(All2016$NACE,1,2)
All2016$IndPos0<-with(All2016, ave(All2016$ROE,All2016$NACE2, FUN=function(x) mean(x, na.rm=T)))
All2016$IndPos<-ifelse(All2016$ROE>All2016$IndPos0,1,0)

#High tech dummy
All2016$HighTech<-ifelse(substr(All2016$NACE,1,2)==21 |substr(All2016$NACE,1,2)==26 | substr(All2016$NACE,1,2)==20 |
                           substr(All2016$NACE,1,2)==27 | substr(All2016$NACE,1,2)==28 |substr(All2016$NACE,1,2)==29 |substr(All2016$NACE,1,2)==30,1,0)

##Create subsidiary based variables
#Create a new variable with BvIDs filled
All2016$BvDID<-na.locf(All2016$BvDID)
All2016$NewID<-na.locf(All2016$BvDID)
All2016$NewCountry<-na.locf(All2016$Country)

#Creating a variable of foreign subsidiaries
All2016$ForeignSub1<-ifelse(All2016$NewCountry!=All2016$SubCountry,1,NA)
All2016$ForeignSub1<-ifelse(is.na(All2016$ForeignSub1),0,All2016$ForeignSub1)
All2016$ForeignSub1<-as.numeric(All2016$ForeignSub1)
All2016$ForSub2<- with(All2016, ave(All2016$ForeignSub1,All2016$BvDID, FUN=sum))

#Share of foreign subsidiaries
All2016$MultiSub2<-All2016$ForSub2 / All2016$NoSub2

#Creating a variable of total foreign revenue
All2016$ForOR1<-ifelse(All2016$NewCountry!=All2016$SubCountry,All2016$SubOR,NA)
All2016$ForOR1<-ifelse(is.na(All2016$ForOR1),0,All2016$ForOR1)
All2016$ForOR1<-as.numeric(All2016$ForOR1)
All2016$ForOR<- with(All2016,ave(All2016$ForOR1,All2016$BvDID, FUN=sum))

#Creating a variable of total foreign assets
All2016$ForTA1<-ifelse(All2016$NewCountry!=All2016$SubCountry,All2016$SubTA,NA)
All2016$ForTA1<-ifelse(is.na(All2016$ForTA1),0,All2016$ForTA1)
All2016$ForTA1<-as.numeric(All2016$ForTA1)
All2016$ForTA<- with(All2016,ave(All2016$ForTA1,All2016$BvDID, FUN=sum))

#Creating a variable of total foreign employees
All2016$ForEMP1<-ifelse(All2016$NewCountry!=All2016$SubCountry,All2016$SubEMP,NA)
All2016$ForEMP1<-ifelse(is.na(All2016$ForEMP1),0,All2016$ForEMP1)
All2016$ForEMP1<-as.numeric(All2016$ForEMP1)
All2016$ForEMP<- with(All2016,ave(All2016$ForEMP1,All2016$BvDID, FUN=sum))

#Share of foreign revenue
All2016$MultiOR<-All2016$ForOR / as.numeric(All2016$OR)

#Share of foreign assets
All2016$MultiTA<-All2016$ForTA / as.numeric(All2016$TA)

#Share of foreign employees
All2016$MultiEmp<-All2016$ForEMP / as.numeric(All2016$EMP)

#Creating a variable of subsidiary revenue
All2016$SubOR1<-ifelse(All2016$BvDID!=All2016$SubBvDID,All2016$SubOR,NA)
All2016$SubOR1<-ifelse(is.na(All2016$SubOR1),0,All2016$SubOR1)
All2016$SubOR1<-as.numeric(All2016$SubOR1)
All2016$SubRev<- with(All2016,ave(All2016$SubOR1,All2016$BvDID, FUN=sum))

#Creating a variable of total subsidiary assets
All2016$SubTA1<-ifelse(All2016$BvDID!=All2016$SubBvDID,All2016$SubTA,0)
All2016$SubTA1<-ifelse(is.na(All2016$ForTA1),0,All2016$SubTA1)
All2016$SubTA1<-as.numeric(All2016$SubTA1)
All2016$SubAssets<- with(All2016,ave(All2016$SubTA1,All2016$BvDID, FUN=sum))

#Creating a variable of total subsidiary employees
All2016$SubEmp1<-ifelse(All2016$BvDID!=All2016$SubBvDID,All2016$SubEMP,0)
All2016$SubEmp1<-ifelse(is.na(All2016$SubEmp1),0,All2016$SubEmp1)
All2016$SubEmp1<-as.numeric(All2016$SubEmp1)
All2016$SubEmp<- with(All2016,ave(All2016$SubEmp1,All2016$BvDID, FUN=sum))

#Share of subsidiary revenue
All2016$ForSubOR<-All2016$SubRev / as.numeric(All2016$OR)

#Share of subsidiary assets
All2016$ForSubTA<-All2016$SubAssets / as.numeric(All2016$TA)

#Share of subsidiary employees
All2016$ForSubEmp<-All2016$SubEmp / as.numeric(All2016$EMP)

##Remove unnesseary rows and columns
All2016<-All2016[,c(-32,-35,-36,-37,-40,-42,-44,-49,-51,-53)]

#Remain the GUOs
All2016<-All2016[!duplicated(All2016$BvDID),]
length(which(!is.na(All2016$GUOBvDID)))

#Double check for duplicate companies
All2016$BvDID[duplicated(All2016$BvDID)] <- NA 
All2016<-All2016[complete.cases(All2016[,1]), ]

##Join AI with GUO data
All16$BvDID<-as.character(All16$BvDID)
All2016$BvDID<-as.character(All2016$BvDID)

#Merge dataset
All6<-left_join(All2016,All16,by="BvDID",na_matches="never")
length(which(!is.na(All6$GUOBvDID)))

#Remove all non-matches
All6<-subset(All6,!is.na(All6$GUOBvDID))

#Create the final variables
#Create dummy for AI Patents
All6$AIDummy<-ifelse(All6$AIPatents>0,1,0)

#Average AI Patents by other firms in the industry adoption Percentage patents
All6$AIPatents[is.na(All6$AIPatents)] <- 0
All6$IndAdopt<-with(All6, ave(All6$AIPatents,All6$NACE2, FUN=mean))

#Create an inverse dummy of AI Adoption
All6$InvDummy<-ifelse(All6$AIDummy==1 | is.na(All6$AIDummy),0,1)
All6$InvDummy[is.na(All6$InvDummy)] <- 0

# Average AI Patents by other firms in the industry doption Percentage patents
All6$AIDummy[is.na(All6$AIDummy)] <- 0
All6$AdPer01<-with(All6, ave(All6$AIDummy,All6$NACE2, FUN=sum))
All6$AdPer01[is.na(All6$AdPer01)] <- 0
All6$AIDummy[is.na(All6$AIDummy)] <- 0
All6$AdPer02<-with(All6, ave(All6$InvDummy,All6$NACE2, FUN=sum))
All6$AdPer01[is.na(All6$AdPer01)] <- 0
All6$AdPer<-All6$AdPer01/(All6$AdPer01+All6$AdPer02)

#Remove unnecessary variables
All6$InvDummy<-NULL
All6$AdPer01<-NULL
All6$AdPer02<-NULL

#Create dummy variable if state-connected
All6<-left_join(All6,Type,by="BvDID",na_matches="never")
All6$State<-ifelse(All6$State=="PA",1,0)
All6$State[is.na(All6$State)] <- 0

#MAke sure year variable is correct
All6$Year<-2016

names(All6) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                 "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR",
                 "NoSub2","SubBvDID","SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP",
                 "Age","ShareINT","NACE2","IndPos","HighTech","ForSub2","MultiSub2","ForOR","ForTA","ForEmp",
                 "MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
                 "Year","NoSub1","ForSub1","MultiSub1","Countries","AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR",
                 "Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP","CGDPpc",
                 "CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub",
                 "Patents","Applications","GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents",
                 "SubPat","SubApp","ForPat","ForApp","AIPatents","AIApplications","AIGrantedPatents",
                 "AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents","AITechPatents","AITechApps",
                 "AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","TechDiv","MTB","MTD","MIB","MID",
                 "AIDummy","IndAdopt","AdPer","State")

col_order<-c("BvDID","Year","Name","GUOBvDID","Country","CRegion","WRegion","NACE","NACE2","Type","Products","PeerGroup","PeerSize",
             "Date","Age","State","AllPatents","Patents","Applications","GrantedPatents","GreenfieldPatents","BrownfieldPatents","SDPatents",
             "HighTechPatents","SubPat","SubApp","ForPat","ForApp","TechDiv",
             "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
             "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","AIDummy",
             "IndAdopt","AdPer","HighTech","IndPos",
             "MTB","MTD","MIB","MID","NoSub1","NoSub2","ForSub1","ForSub2","MultiSub1","MultiSub2","Countries","AvSub",
             "OR","TA","ROE","EMP","INT","ShareINT","CR",
             "ForOR","ForTA","ForEmp","MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
             "IPR","TP","GDP","GDPpc","RD","LR","TR","Inno","CPat","CPatRat","CGDP","TotalGDP","CGDPpc","CRD",
             "IPRDist","IPRDist2","IPRCountries","IPRSub","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")
All6<-All6[,col_order]


##Create variables - 2017

names(All2017) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                    "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR","NoSub2","SubBvDID",
                    "SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP")

#Create Company Age Variable
All2017$Age1<-str_sub(All2017$Date,-4,-1)
All2017$Age1<-as.numeric(All2017$Age1)
All2017$Age<-2017-All2017$Age1
All2017$Age1<-NULL

#Create a variable of intangible assets as share of tangible assets
All2017$ShareINT<-All2017$INT / All2017$TA

#Industry position - average ROE in the industry - laggard leader
All2017$NACE2<-substr(All2017$NACE,1,2)
All2017$IndPos0<-with(All2017, ave(All2017$ROE,All2017$NACE2, FUN=function(x) mean(x, na.rm=T)))
All2017$IndPos<-ifelse(All2017$ROE>All2017$IndPos0,1,0)

#High tech dummy
All2017$HighTech<-ifelse(substr(All2017$NACE,1,2)==21 |substr(All2017$NACE,1,2)==26 | substr(All2017$NACE,1,2)==20 |
                           substr(All2017$NACE,1,2)==27 | substr(All2017$NACE,1,2)==28 |substr(All2017$NACE,1,2)==29 |substr(All2017$NACE,1,2)==30,1,0)

##Create subsidiary based variables
#Create a new variable with BvIDs filled
All2017$BvDID<-na.locf(All2017$BvDID)
All2017$NewID<-na.locf(All2017$BvDID)
All2017$NewCountry<-na.locf(All2017$Country)

#Creating a variable of foreign subsidiaries
All2017$ForeignSub1<-ifelse(All2017$NewCountry!=All2017$SubCountry,1,NA)
All2017$ForeignSub1<-ifelse(is.na(All2017$ForeignSub1),0,All2017$ForeignSub1)
All2017$ForeignSub1<-as.numeric(All2017$ForeignSub1)
All2017$ForSub2<- with(All2017, ave(All2017$ForeignSub1,All2017$BvDID, FUN=sum))

#Share of foreign subsidiaries
All2017$MultiSub2<-All2017$ForSub2 / All2017$NoSub2

#Creating a variable of total foreign revenue
All2017$ForOR1<-ifelse(All2017$NewCountry!=All2017$SubCountry,All2017$SubOR,NA)
All2017$ForOR1<-ifelse(is.na(All2017$ForOR1),0,All2017$ForOR1)
All2017$ForOR1<-as.numeric(All2017$ForOR1)
All2017$ForOR<- with(All2017,ave(All2017$ForOR1,All2017$BvDID, FUN=sum))

#Creating a variable of total foreign assets
All2017$ForTA1<-ifelse(All2017$NewCountry!=All2017$SubCountry,All2017$SubTA,NA)
All2017$ForTA1<-ifelse(is.na(All2017$ForTA1),0,All2017$ForTA1)
All2017$ForTA1<-as.numeric(All2017$ForTA1)
All2017$ForTA<- with(All2017,ave(All2017$ForTA1,All2017$BvDID, FUN=sum))

#Creating a variable of total foreign employees
All2017$ForEMP1<-ifelse(All2017$NewCountry!=All2017$SubCountry,All2017$SubEMP,NA)
All2017$ForEMP1<-ifelse(is.na(All2017$ForEMP1),0,All2017$ForEMP1)
All2017$ForEMP1<-as.numeric(All2017$ForEMP1)
All2017$ForEMP<- with(All2017,ave(All2017$ForEMP1,All2017$BvDID, FUN=sum))

#Share of foreign revenue
All2017$MultiOR<-round(All2017$ForOR / as.numeric(All2017$OR))

#Share of foreign assets
All2017$MultiTA<-All2017$ForTA / as.numeric(All2017$TA)

#Share of foreign employees
All2017$MultiEmp<-All2017$ForEMP / as.numeric(All2017$EMP)

#Creating a variable of subsidiary revenue
All2017$SubOR1<-ifelse(All2017$BvDID!=All2017$SubBvDID,All2017$SubOR,NA)
All2017$SubOR1<-ifelse(is.na(All2017$SubOR1),0,All2017$SubOR1)
All2017$SubOR1<-as.numeric(All2017$SubOR1)
All2017$SubRev<- with(All2017,ave(All2017$SubOR1,All2017$BvDID, FUN=sum))

#Creating a variable of total subsidiary assets
All2017$SubTA1<-ifelse(All2017$BvDID!=All2017$SubBvDID,All2017$SubTA,0)
All2017$SubTA1<-ifelse(is.na(All2017$ForTA1),0,All2017$SubTA1)
All2017$SubTA1<-as.numeric(All2017$SubTA1)
All2017$SubAssets<- with(All2017,ave(All2017$SubTA1,All2017$BvDID, FUN=sum))

#Creating a variable of total subsidiary employees
All2017$SubEmp1<-ifelse(All2017$BvDID!=All2017$SubBvDID,All2017$SubEMP,0)
All2017$SubEmp1<-ifelse(is.na(All2017$SubEmp1),0,All2017$SubEmp1)
All2017$SubEmp1<-as.numeric(All2017$SubEmp1)
All2017$SubEmp<- with(All2017,ave(All2017$SubEmp1,All2017$BvDID, FUN=sum))

#Share of subsidiary revenue
All2017$ForSubOR<-All2017$SubRev / as.numeric(All2017$OR)

#Share of subsidiary assets
All2017$ForSubTA<-All2017$SubAssets / as.numeric(All2017$TA)

#Share of subsidiary employees
All2017$ForSubEmp<-All2017$SubEmp / as.numeric(All2017$EMP)

##Remove unnesseary rows and columns
All2017<-All2017[,c(-32,-35,-36,-37,-40,-42,-44,-49,-51,-53)]

#Remain the GUOs
All2017<-All2017[!duplicated(All2017$BvDID),]
length(which(!is.na(All2017$GUOBvDID)))

#Double check for duplicate companies
All2017$BvDID[duplicated(All2017$BvDID)] <- NA 
All2017<-All2017[complete.cases(All2017[,1]), ]

##Join AI with GUO data
All17$BvDID<-as.character(All17$BvDID)
All2017$BvDID<-as.character(All2017$BvDID)

#Merge dataset
All7<-left_join(All2017,All17,by="BvDID",na_matches="never")
length(which(!is.na(All7$GUOBvDID)))

#Remove all non-matches
All7<-subset(All7,!is.na(All7$GUOBvDID))

#Create the final variables
#Create dummy for AI Patents
All7$AIDummy<-ifelse(All7$AIPatents>0,1,0)

#Average AI Patents by other firms in the industry adoption Percentage patents
All7$AIPatents[is.na(All7$AIPatents)] <- 0
All7$IndAdopt<-with(All7, ave(All7$AIPatents,All7$NACE2, FUN=mean))

#Create an inverse dummy of AI Adoption
All7$InvDummy<-ifelse(All7$AIDummy==1 | is.na(All7$AIDummy),0,1)
All7$InvDummy[is.na(All7$AIDummy)] <- 0

# Average AI Patents by other firms in the industry doption Percentage patents
All7$AIDummy[is.na(All7$AIDummy)] <- 0
All7$AdPer01<-with(All7, ave(All7$AIDummy,All7$NACE2, FUN=sum))
All7$AdPer01[is.na(All7$AdPer01)] <- 0
All7$AIDummy[is.na(All7$AIDummy)] <- 0
All7$AdPer02<-with(All7, ave(All7$InvDummy,All7$NACE2, FUN=sum))
All7$AdPer01[is.na(All7$AdPer01)] <- 0
All7$AdPer<-All7$AdPer01/(All7$AdPer01+All7$AdPer02)

#Remove unnecessary variables
All7$InvDummy<-NULL
All7$AdPer01<-NULL
All7$AdPer02<-NULL

#Create dummy variable if state-connected
All7<-left_join(All7,Type,by="BvDID",na_matches="never")
All7$State<-ifelse(All7$State=="PA",1,0)
All7$State[is.na(All7$State)] <- 0

#MAke sure year variable is correct
All7$Year<-2017

names(All7) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                 "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR",
                 "NoSub2","SubBvDID","SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP",
                 "Age","ShareINT","NACE2","IndPos","HighTech","ForSub2","MultiSub2","ForOR","ForTA","ForEmp",
                 "MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
                 "Year","NoSub1","ForSub1","MultiSub1","Countries","AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR",
                 "Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP","CGDPpc",
                 "CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub",
                 "Patents","Applications","GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents",
                 "SubPat","SubApp","ForPat","ForApp","AIPatents","AIApplications","AIGrantedPatents",
                 "AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents","AITechPatents","AITechApps",
                 "AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","TechDiv","MTB","MTD","MIB","MID",
                 "AIDummy","IndAdopt","AdPer","State")

col_order<-c("BvDID","Year","Name","GUOBvDID","Country","CRegion","WRegion","NACE","NACE2","Type","Products","PeerGroup","PeerSize",
             "Date","Age","State","AllPatents","Patents","Applications","GrantedPatents","GreenfieldPatents","BrownfieldPatents","SDPatents",
             "HighTechPatents","SubPat","SubApp","ForPat","ForApp","TechDiv",
             "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
             "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","AIDummy",
             "IndAdopt","AdPer","HighTech","IndPos",
             "MTB","MTD","MIB","MID","NoSub1","NoSub2","ForSub1","ForSub2","MultiSub1","MultiSub2","Countries","AvSub",
             "OR","TA","ROE","EMP","INT","ShareINT","CR",
             "ForOR","ForTA","ForEmp","MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
             "IPR","TP","GDP","GDPpc","RD","LR","TR","Inno","CPat","CPatRat","CGDP","TotalGDP","CGDPpc","CRD",
             "IPRDist","IPRDist2","IPRCountries","IPRSub","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")
All7<-All7[,col_order]


##Create variables - 2018

names(All2018) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                    "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR","NoSub2","SubBvDID",
                    "SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP")

#Create Company Age Variable
All2018$Age1<-str_sub(All2018$Date,-4,-1)
All2018$Age1<-as.numeric(All2018$Age1)
All2018$Age<-2018-All2018$Age1
All2018$Age1<-NULL

#Create a variable of intangible assets as share of tangible assets
All2018$ShareINT<-All2018$INT / All2018$TA

#Industry position - average ROE in the industry - laggard leader
All2018$NACE2<-substr(All2018$NACE,1,2)
All2018$IndPos0<-with(All2018, ave(All2018$ROE,All2018$NACE2, FUN=function(x) mean(x, na.rm=T)))
All2018$IndPos<-ifelse(All2018$ROE>All2018$IndPos0,1,0)

#High tech dummy
All2018$HighTech<-ifelse(substr(All2018$NACE,1,2)==21 |substr(All2018$NACE,1,2)==26 | substr(All2018$NACE,1,2)==20 |
                           substr(All2018$NACE,1,2)==27 | substr(All2018$NACE,1,2)==28 |substr(All2018$NACE,1,2)==29 |substr(All2018$NACE,1,2)==30,1,0)

##Create subsidiary based variables
#Create a new variable with BvIDs filled
All2018$BvDID<-na.locf(All2018$BvDID)
All2018$NewID<-na.locf(All2018$BvDID)
All2018$NewCountry<-na.locf(All2018$Country)

#Creating a variable of foreign subsidiaries
All2018$ForeignSub1<-ifelse(All2018$NewCountry!=All2018$SubCountry,1,NA)
All2018$ForeignSub1<-ifelse(is.na(All2018$ForeignSub1),0,All2018$ForeignSub1)
All2018$ForeignSub1<-as.numeric(All2018$ForeignSub1)
All2018$ForSub2<- with(All2018, ave(All2018$ForeignSub1,All2018$BvDID, FUN=sum))

#Share of foreign subsidiaries
All2018$MultiSub2<-All2018$ForSub2 / All2018$NoSub2

#Creating a variable of total foreign revenue
All2018$ForOR1<-ifelse(All2018$NewCountry!=All2018$SubCountry,All2018$SubOR,NA)
All2018$ForOR1<-ifelse(is.na(All2018$ForOR1),0,All2018$ForOR1)
All2018$ForOR1<-as.numeric(All2018$ForOR1)
All2018$ForOR<- with(All2018,ave(All2018$ForOR1,All2018$BvDID, FUN=sum))

#Creating a variable of total foreign assets
All2018$ForTA1<-ifelse(All2018$NewCountry!=All2018$SubCountry,All2018$SubTA,NA)
All2018$ForTA1<-ifelse(is.na(All2018$ForTA1),0,All2018$ForTA1)
All2018$ForTA1<-as.numeric(All2018$ForTA1)
All2018$ForTA<- with(All2018,ave(All2018$ForTA1,All2018$BvDID, FUN=sum))

#Creating a variable of total foreign employees
All2018$ForEMP1<-ifelse(All2018$NewCountry!=All2018$SubCountry,All2018$SubEMP,NA)
All2018$ForEMP1<-ifelse(is.na(All2018$ForEMP1),0,All2018$ForEMP1)
All2018$ForEMP1<-as.numeric(All2018$ForEMP1)
All2018$ForEMP<- with(All2018,ave(All2018$ForEMP1,All2018$BvDID, FUN=sum))

#Share of foreign revenue
All2018$MultiOR<-All2018$ForOR / as.numeric(All2018$OR)

#Share of foreign assets
All2018$MultiTA<-All2018$ForTA / as.numeric(All2018$TA)

#Share of foreign employees
All2018$MultiEmp<-All2018$ForEMP / as.numeric(All2018$EMP)

#Creating a variable of subsidiary revenue
All2018$SubOR1<-ifelse(All2018$BvDID!=All2018$SubBvDID,All2018$SubOR,NA)
All2018$SubOR1<-ifelse(is.na(All2018$SubOR1),0,All2018$SubOR1)
All2018$SubOR1<-as.numeric(All2018$SubOR1)
All2018$SubRev<- with(All2018,ave(All2018$SubOR1,All2018$BvDID, FUN=sum))

#Creating a variable of total subsidiary assets
All2018$SubTA1<-ifelse(All2018$BvDID!=All2018$SubBvDID,All2018$SubTA,0)
All2018$SubTA1<-ifelse(is.na(All2018$ForTA1),0,All2018$SubTA1)
All2018$SubTA1<-as.numeric(All2018$SubTA1)
All2018$SubAssets<- with(All2018,ave(All2018$SubTA1,All2018$BvDID, FUN=sum))

#Creating a variable of total subsidiary employees
All2018$SubEmp1<-ifelse(All2018$BvDID!=All2018$SubBvDID,All2018$SubEMP,0)
All2018$SubEmp1<-ifelse(is.na(All2018$SubEmp1),0,All2018$SubEmp1)
All2018$SubEmp1<-as.numeric(All2018$SubEmp1)
All2018$SubEmp<- with(All2018,ave(All2018$SubEmp1,All2018$BvDID, FUN=sum))

#Share of subsidiary revenue
All2018$ForSubOR<-All2018$SubRev / as.numeric(All2018$OR)

#Share of subsidiary assets
All2018$ForSubTA<-All2018$SubAssets / as.numeric(All2018$TA)

#Share of subsidiary employees
All2018$ForSubEmp<-All2018$SubEmp / as.numeric(All2018$EMP)

##Remove unnesseary rows and columns
All2018<-All2018[,c(-32,-35,-36,-37,-40,-42,-44,-49,-51,-53)]

#Remain the GUOs
All2018<-All2018[!duplicated(All2018$BvDID),]
length(which(!is.na(All2018$GUOBvDID)))

#Double check for duplicate companies
All2018$BvDID[duplicated(All2018$BvDID)] <- NA 
All2018<-All2018[complete.cases(All2018[,1]), ]

##Join AI with GUO data
All18$BvDID<-as.character(All18$BvDID)
All2018$BvDID<-as.character(All2018$BvDID)

#Merge dataset
All8<-left_join(All2018,All18,by="BvDID",na_matches="never")
length(which(!is.na(All8$GUOBvDID)))

#Remove all non-matches
All8<-subset(All8,!is.na(All8$GUOBvDID))

#Create the final variables
#Create dummy for AI Patents
All8$AIDummy<-ifelse(All8$AIPatents>0,1,0)

#Average AI Patents by other firms in the industry adoption Percentage patents
All8$AIPatents[is.na(All8$AIPatents)] <- 0
All8$IndAdopt<-with(All8, ave(All8$AIPatents,All8$NACE2, FUN=mean))

#Create an inverse dummy of AI Adoption
All8$InvDummy<-ifelse(All8$AIDummy==1 | is.na(All8$AIDummy),0,1)
All8$InvDummy[is.na(All8$InvDummy)] <- 0

# Average AI Patents by other firms in the industry doption Percentage patents
All8$AIDummy[is.na(All8$AIDummy)] <- 0
All8$AdPer01<-with(All8, ave(All8$AIDummy,All8$NACE2, FUN=sum))
All8$AdPer01[is.na(All8$AdPer01)] <- 0
All8$AIDummy[is.na(All8$AIDummy)] <- 0
All8$AdPer02<-with(All8, ave(All8$InvDummy,All8$NACE2, FUN=sum))
All8$AdPer01[is.na(All8$AdPer01)] <- 0
All8$AdPer<-All8$AdPer01/(All8$AdPer01+All8$AdPer02)

#Remove unnecessary variables
All8$InvDummy<-NULL
All8$AdPer01<-NULL
All8$AdPer02<-NULL

#Create dummy variable if state-connected
All8<-left_join(All8,Type,by="BvDID",na_matches="never")
All8$State<-ifelse(All8$State=="PA",1,0)
All8$State[is.na(All8$State)] <- 0

#MAke sure year variable is correct
All8$Year<-2018

names(All8) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                 "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR",
                 "NoSub2","SubBvDID","SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP",
                 "Age","ShareINT","NACE2","IndPos","HighTech","ForSub2","MultiSub2","ForOR","ForTA","ForEmp",
                 "MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
                 "Year","NoSub1","ForSub1","MultiSub1","Countries","AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR",
                 "Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP","CGDPpc",
                 "CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub",
                 "Patents","Applications","GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents",
                 "SubPat","SubApp","ForPat","ForApp","AIPatents","AIApplications","AIGrantedPatents",
                 "AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents","AITechPatents","AITechApps",
                 "AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","TechDiv","MTB","MTD","MIB","MID",
                 "AIDummy","IndAdopt","AdPer","State")

col_order<-c("BvDID","Year","Name","GUOBvDID","Country","CRegion","WRegion","NACE","NACE2","Type","Products","PeerGroup","PeerSize",
             "Date","Age","State","AllPatents","Patents","Applications","GrantedPatents","GreenfieldPatents","BrownfieldPatents","SDPatents",
             "HighTechPatents","SubPat","SubApp","ForPat","ForApp","TechDiv",
             "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
             "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","AIDummy",
             "IndAdopt","AdPer","HighTech","IndPos",
             "MTB","MTD","MIB","MID","NoSub1","NoSub2","ForSub1","ForSub2","MultiSub1","MultiSub2","Countries","AvSub",
             "OR","TA","ROE","EMP","INT","ShareINT","CR",
             "ForOR","ForTA","ForEmp","MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
             "IPR","TP","GDP","GDPpc","RD","LR","TR","Inno","CPat","CPatRat","CGDP","TotalGDP","CGDPpc","CRD",
             "IPRDist","IPRDist2","IPRCountries","IPRSub","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")
All8<-All8[,col_order]


##Create variables - 2019

names(All2019) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                    "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR","NoSub2","SubBvDID",
                    "SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP")

#Create Company Age Variable
All2019$Age1<-str_sub(All2019$Date,-4,-1)
All2019$Age1<-as.numeric(All2019$Age1)
All2019$Age<-2019-All2019$Age1
All2019$Age1<-NULL

#Create a variable of intangible assets as share of tangible assets
All2019$ShareINT<-All2019$INT / All2019$TA

#Industry position - average ROE in the industry - laggard leader
All2019$NACE2<-substr(All2019$NACE,1,2)
All2019$IndPos0<-with(All2019, ave(All2019$ROE,All2019$NACE2, FUN=function(x) mean(x, na.rm=T)))
All2019$IndPos<-ifelse(All2019$ROE>All2019$IndPos0,1,0)

#High tech dummy
All2019$HighTech<-ifelse(substr(All2019$NACE,1,2)==21 |substr(All2019$NACE,1,2)==26 | substr(All2019$NACE,1,2)==20 |
                           substr(All2019$NACE,1,2)==27 | substr(All2019$NACE,1,2)==28 |substr(All2019$NACE,1,2)==29 |substr(All2019$NACE,1,2)==30,1,0)

##Create subsidiary based variables
#Create a new variable with BvIDs filled
All2019$BvDID<-na.locf(All2019$BvDID)
All2019$NewID<-na.locf(All2019$BvDID)
All2019$NewCountry<-na.locf(All2019$Country)

#Creating a variable of foreign subsidiaries
All2019$ForeignSub1<-ifelse(All2019$NewCountry!=All2019$SubCountry,1,NA)
All2019$ForeignSub1<-ifelse(is.na(All2019$ForeignSub1),0,All2019$ForeignSub1)
All2019$ForeignSub1<-as.numeric(All2019$ForeignSub1)
All2019$ForSub2<- with(All2019, ave(All2019$ForeignSub1,All2019$BvDID, FUN=sum))

#Share of foreign subsidiaries
All2019$MultiSub2<-All2019$ForSub2 / All2019$NoSub2

#Creating a variable of total foreign revenue
All2019$ForOR1<-ifelse(All2019$NewCountry!=All2019$SubCountry,All2019$SubOR,NA)
All2019$ForOR1<-ifelse(is.na(All2019$ForOR1),0,All2019$ForOR1)
All2019$ForOR1<-as.numeric(All2019$ForOR1)
All2019$ForOR<- with(All2019,ave(All2019$ForOR1,All2019$BvDID, FUN=sum))

#Creating a variable of total foreign assets
All2019$ForTA1<-ifelse(All2019$NewCountry!=All2019$SubCountry,All2019$SubTA,NA)
All2019$ForTA1<-ifelse(is.na(All2019$ForTA1),0,All2019$ForTA1)
All2019$ForTA1<-as.numeric(All2019$ForTA1)
All2019$ForTA<- with(All2019,ave(All2019$ForTA1,All2019$BvDID, FUN=sum))

#Creating a variable of total foreign employees
All2019$ForEMP1<-ifelse(All2019$NewCountry!=All2019$SubCountry,All2019$SubEMP,NA)
All2019$ForEMP1<-ifelse(is.na(All2019$ForEMP1),0,All2019$ForEMP1)
All2019$ForEMP1<-as.numeric(All2019$ForEMP1)
All2019$ForEMP<- with(All2019,ave(All2019$ForEMP1,All2019$BvDID, FUN=sum))

#Share of foreign revenue
All2019$MultiOR<-All2019$ForOR / as.numeric(All2019$OR)

#Share of foreign assets
All2019$MultiTA<-All2019$ForTA / as.numeric(All2019$TA)

#Share of foreign employees
All2019$MultiEmp<-All2019$ForEMP / as.numeric(All2019$EMP)

#Creating a variable of subsidiary revenue
All2019$SubOR1<-ifelse(All2019$BvDID!=All2019$SubBvDID,All2019$SubOR,NA)
All2019$SubOR1<-ifelse(is.na(All2019$SubOR1),0,All2019$SubOR1)
All2019$SubOR1<-as.numeric(All2019$SubOR1)
All2019$SubRev<- with(All2019,ave(All2019$SubOR1,All2019$BvDID, FUN=sum))

#Creating a variable of total subsidiary assets
All2019$SubTA1<-ifelse(All2019$BvDID!=All2019$SubBvDID,All2019$SubTA,0)
All2019$SubTA1<-ifelse(is.na(All2019$ForTA1),0,All2019$SubTA1)
All2019$SubTA1<-as.numeric(All2019$SubTA1)
All2019$SubAssets<- with(All2019,ave(All2019$SubTA1,All2019$BvDID, FUN=sum))

#Creating a variable of total subsidiary employees
All2019$SubEmp1<-ifelse(All2019$BvDID!=All2019$SubBvDID,All2019$SubEMP,0)
All2019$SubEmp1<-ifelse(is.na(All2019$SubEmp1),0,All2019$SubEmp1)
All2019$SubEmp1<-as.numeric(All2019$SubEmp1)
All2019$SubEmp<- with(All2019,ave(All2019$SubEmp1,All2019$BvDID, FUN=sum))

#Share of subsidiary revenue
All2019$ForSubOR<-All2019$SubRev / as.numeric(All2019$OR)

#Share of subsidiary assets
All2019$ForSubTA<-All2019$SubAssets / as.numeric(All2019$TA)

#Share of subsidiary employees
All2019$ForSubEmp<-All2019$SubEmp / as.numeric(All2019$EMP)

##Remove unnesseary rows and columns
All2019<-All2019[,c(-32,-35,-36,-37,-40,-42,-44,-49,-51,-53)]

#Remain the GUOs
All2019<-All2019[!duplicated(All2019$BvDID),]
length(which(!is.na(All2019$GUOBvDID)))

#Double check for duplicate companies
All2019$BvDID[duplicated(All2019$BvDID)] <- NA 
All2019<-All2019[complete.cases(All2019[,1]), ]

##Join AI with GUO data
All19$BvDID<-as.character(All19$BvDID)
All2019$BvDID<-as.character(All2019$BvDID)

#Merge dataset
All9<-left_join(All2019,All19,by="BvDID",na_matches="never")
length(which(!is.na(All9$GUOBvDID)))

#Remove all non-matches
All9<-subset(All9,!is.na(All9$GUOBvDID))

#Create the final variables
#Create dummy for AI Patents
All9$AIDummy<-ifelse(All9$AIPatents>0,1,0)

#Average AI Patents by other firms in the industry adoption Percentage patents
All9$AIPatents[is.na(All9$AIPatents)] <- 0
All9$IndAdopt<-with(All9, ave(All9$AIPatents,All9$NACE2, FUN=mean))

#Create an inverse dummy of AI Adoption
All9$InvDummy<-ifelse(All9$AIDummy==1 | is.na(All9$AIDummy),0,1)
All9$InvDummy[is.na(All9$InvDummy)] <- 0

# Average AI Patents by other firms in the industry doption Percentage patents
All9$AIDummy[is.na(All9$AIDummy)] <- 0
All9$AdPer01<-with(All9, ave(All9$AIDummy,All9$NACE2, FUN=sum))
All9$AdPer01[is.na(All9$AdPer01)] <- 0
All9$AIDummy[is.na(All9$AIDummy)] <- 0
All9$AdPer02<-with(All9, ave(All9$InvDummy,All9$NACE2, FUN=sum))
All9$AdPer01[is.na(All9$AdPer01)] <- 0
All9$AdPer<-All9$AdPer01/(All9$AdPer01+All9$AdPer02)

#Remove unnecessary variables
All9$InvDummy<-NULL
All9$AdPer01<-NULL
All9$AdPer02<-NULL

#Create dummy variable if state-connected
All9<-left_join(All9,Type,by="BvDID",na_matches="never")
All9$State<-ifelse(All9$State=="PA",1,0)
All9$State[is.na(All9$State)] <- 0

#MAke sure year variable is correct
All9$Year<-2019

names(All9) <- c("Name","BvDID","GUOBvDID","Country","CRegion","AllPatents","NACE","WRegion","Type","Products",
                 "PeerGroup","PeerSize","Date","OR","TA","ROE","EMP","INT","CR",
                 "NoSub2","SubBvDID","SubCountry","SubCity","SubType","SubNACE","SubOR","SubTA","SubEMP",
                 "Age","ShareINT","NACE2","IndPos","HighTech","ForSub2","MultiSub2","ForOR","ForTA","ForEmp",
                 "MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
                 "Year","NoSub1","ForSub1","MultiSub1","Countries","AvSub","IPR","TP","GDP","GDPpc","RD","LR","TR",
                 "Inno","IPRDist","IPRDist2","IPRCountries","IPRSub","CPat","CPatRat","CGDP","TotalGDP","CGDPpc",
                 "CRD","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub",
                 "Patents","Applications","GreenfieldPatents","BrownfieldPatents","SDPatents","GrantedPatents","HighTechPatents",
                 "SubPat","SubApp","ForPat","ForApp","AIPatents","AIApplications","AIGrantedPatents",
                 "AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents","AITechPatents","AITechApps",
                 "AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","TechDiv","MTB","MTD","MIB","MID",
                 "AIDummy","IndAdopt","AdPer","State")

col_order<-c("BvDID","Year","Name","GUOBvDID","Country","CRegion","WRegion","NACE","NACE2","Type","Products","PeerGroup","PeerSize",
             "Date","Age","State","AllPatents","Patents","Applications","GrantedPatents","GreenfieldPatents","BrownfieldPatents","SDPatents",
             "HighTechPatents","SubPat","SubApp","ForPat","ForApp","TechDiv",
             "AIPatents","AIApplications","AIGrantedPatents","AIGreenfieldPatents","AIBrownfieldPatents","AISDPatents",
             "AITechPatents","AITechApps","AIFunctPatents","AIFunctApps","AISubPat","AISubApp","AIForPat","AIForApp","AIDummy",
             "IndAdopt","AdPer","HighTech","IndPos",
             "MTB","MTD","MIB","MID","NoSub1","NoSub2","ForSub1","ForSub2","MultiSub1","MultiSub2","Countries","AvSub",
             "OR","TA","ROE","EMP","INT","ShareINT","CR",
             "ForOR","ForTA","ForEmp","MultiOR","MultiTA","MultiEmp","SubRev","SubAssets","SubEmp","ForSubOR","ForSubTA","ForSubEmp",
             "IPR","TP","GDP","GDPpc","RD","LR","TR","Inno","CPat","CPatRat","CGDP","TotalGDP","CGDPpc","CRD",
             "IPRDist","IPRDist2","IPRCountries","IPRSub","LRDist","TRDist","InnoDist","InnoDist2","InnoCountries","InnoSub")
All9<-All9[,col_order]




#   2. Refine and combina dataset ####

##Combine all datasets to a large panel
Full1<-bind_rows(All1,All2,All3,All4,All5,All6,All7,All8,All9)

Full<-Full1[order(Full1$BvDID),]

Full1<-Full[c(2,3,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42)]

#Patent data descriptives
describeBy(Full1,Full1$Year)
xtabs(Patents ~ Year, Full1)
xtabs(GreenfieldPatents ~ Year, Full1)
xtabs(HighTechPatents ~ Year, Full1)
xtabs(SubPat ~ Year, Full1)
xtabs(ForPat ~ Year, Full1)
xtabs(AIPatents ~ Year, Full1)
xtabs(AIGreenfieldPatents ~ Year, Full1)
xtabs(AIGrantedPatents ~ Year, Full1)
xtabs(AITechPatents ~ Year, Full1)
xtabs(AIFunctPatents ~ Year, Full1)
xtabs(AISubPat ~ Year, Full1)
xtabs(AIForPat ~ Year, Full1)

#xyplot (Full1$AIPatents ~ Full1$Year, groups=Full1$Year)
#densityplot (~Full1$AIPatents, groups=Full1$Year)

#Reduce the number of GUOs
Full$EMP[is.na(Full$EMP)] <- 0
Full$SubEmp[is.na(Full$SubEmp)] <- 0
Full$EMP<-as.numeric(Full$EMP)
Full$SubEmp<-as.numeric(Full$SubEmp)
Full$TotalEmp<-Full$EMP+Full$SubEmp
Full$Check<- with(Full,ave(Full$TotalEmp,Full$BvDID, FUN=mean))
Full<-subset(Full,Full$Check>50)

#Reduce the number of GUOs
Full$OR[is.na(Full$OR)] <- 0
Full$SubRev[is.na(Full$SubRev)] <- 0
Full$OR<-as.numeric(Full$OR)
Full$SubRev<-as.numeric(Full$SubRev)
Full$TotalRev<-Full$OR+Full$SubRev
Full$Check<- with(Full,ave(Full$TotalRev,Full$BvDID, FUN=mean))
Full<-subset(Full,Full$Check>50000)

Full<-subset(Full,Full$Type=="Corporate")
Full$Date<-str_sub(Full$Date,-4,-1)

#Final Dataset
#write.xlsx(Full, file = "Panel.xlsx", row.names = FALSE)
#write.csv2(Full, file = "Panel.csv", header = TRUE)
write.csv(Full, file = "files_created_code3/Full.csv",row.names = F)

#Test Company: DE7330003759

#Remove unnecessary files
rm(All1,All2,All3,All4,All5,All6,All7,All8,All9,BU_AI,BU_GUO,BU_Share,BU_Type,Country,Trans,Type,col_order,Full1,
   Patents2011,Patents2012,Patents2013,Patents2014,Patents2015,Patents2016,Patents2017,Patents2018,Patents2019,Patents,
   AI2011,AI2012,AI2013,AI2014,AI2015,AI2016,AI2017,AI2018,AI2019,AICheck,AICheck2,All11,All12,All13,All14,All15,All16,All17,
   All18,All19,All2011,All2012,All2013,All2014,All2015,All2016,All2017,All2018,All2019,Country,Country2011,Country2012,Country2013,
   Country2014,Country2015,Country2016,Country2017,Country2018,Country2019,Data1,Full1,Full2,Full3,Full4,Full5,Full6,Full7,
   Full8,Full9,Final,Up,Own,Share,cols.num,col_order,Trans,Type,Subs)


#Check Final Dataset for consistency
## End