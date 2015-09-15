#Kim Fruge and Jordan Holsinger 
#Florida State University 
# 15 Sept 2015
# Subsetting ICEWS Data, first by government target, then HRO source, then events, collapsing data, and saving 


library(foreign)
library(tidyr)
library(doBy)

Events95 <- read.csv2("/Users/KimFruge/Desktop/Projects/Events1995.tab",  sep="\t", header=TRUE)

data95a<-separate(Events95, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data95<-separate(data95a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data95b<- subset(data95,
                 (data95$Target.Sectors_1=="Government" | data95$Target.Sectors_1=="Executive" | data95$Target.Sectors_1=="Police" | data95$Target.Sectors_1=="Military" | data95$Target.Sectors_1=="Legislative / Parliamentarty" | data95$Target.Sectors_1=="Judicial" | data95$Target.Sectors_1=="Local" | data95$Target.Sectors_1=="Parties") |
                   (data95$Target.Sectors_2=="Government" | data95$Target.Sectors_2=="Executive" | data95$Target.Sectors_2=="Police" | data95$Target.Sectors_2=="Military" | data95$Target.Sectors_2=="Legislative / Parliamentarty" | data95$Target.Sectors_2=="Judicial" | data95$Target.Sectors_2=="Local" | data95$Target.Sectors_2=="Parties") |
                   (data95$Target.Sectors_3=="Government" | data95$Target.Sectors_3=="Executive" | data95$Target.Sectors_3=="Police" | data95$Target.Sectors_3=="Military" | data95$Target.Sectors_3=="Legislative / Parliamentarty" | data95$Target.Sectors_3=="Judicial" | data95$Target.Sectors_3=="Local" | data95$Target.Sectors_3=="Parties") |
                   (data95$Target.Sectors_4=="Government" | data95$Target.Sectors_4=="Executive" | data95$Target.Sectors_4=="Police" | data95$Target.Sectors_4=="Military" | data95$Target.Sectors_4=="Legislative / Parliamentarty" | data95$Target.Sectors_4=="Judicial" | data95$Target.Sectors_4=="Local" | data95$Target.Sectors_4=="Parties") |
                   (data95$Target.Sectors_5=="Government" | data95$Target.Sectors_5=="Executive" | data95$Target.Sectors_5=="Police" | data95$Target.Sectors_5=="Military" | data95$Target.Sectors_5=="Legislative / Parliamentarty" | data95$Target.Sectors_5=="Judicial" | data95$Target.Sectors_5=="Local" | data95$Target.Sectors_5=="Parties") |
                   (data95$Target.Sectors_6=="Government" | data95$Target.Sectors_6=="Executive" | data95$Target.Sectors_6=="Police" | data95$Target.Sectors_6=="Military" | data95$Target.Sectors_6=="Legislative / Parliamentarty" | data95$Target.Sectors_6=="Judicial" | data95$Target.Sectors_6=="Local" | data95$Target.Sectors_6=="Parties") | 
                   (data95$Target.Sectors_7=="Government" | data95$Target.Sectors_7=="Executive" | data95$Target.Sectors_7=="Police" | data95$Target.Sectors_7=="Military" | data95$Target.Sectors_7=="Legislative / Parliamentarty" | data95$Target.Sectors_7=="Judicial" | data95$Target.Sectors_7=="Local" | data95$Target.Sectors_7=="Parties") |
                   (data95$Target.Sectors_8=="Government" | data95$Target.Sectors_8=="Executive" | data95$Target.Sectors_8=="Police" | data95$Target.Sectors_8=="Military" | data95$Target.Sectors_8=="Legislative / Parliamentarty" | data95$Target.Sectors_8=="Judicial" | data95$Target.Sectors_8=="Local" | data95$Target.Sectors_8=="Parties") | 
                   (data95$Target.Sectors_9=="Government" | data95$Target.Sectors_9=="Executive" | data95$Target.Sectors_9=="Police" | data95$Target.Sectors_9=="Military" | data95$Target.Sectors_9=="Legislative / Parliamentarty" | data95$Target.Sectors_9=="Judicial" | data95$Target.Sectors_9=="Local" | data95$Target.Sectors_9=="Parties") |
                   (data95$Target.Sectors_10=="Government" | data95$Target.Sectors_10=="Executive" | data95$Target.Sectors_10=="Police" | data95$Target.Sectors_10=="Military" | data95$Target.Sectors_10=="Legislative / Parliamentarty" | data95$Target.Sectors_10=="Judicial" | data95$Target.Sectors_10=="Local" | data95$Target.Sectors_10=="Parties") |
                   (data95$Target.Sectors_11=="Government" | data95$Target.Sectors_11=="Executive" | data95$Target.Sectors_11=="Police" | data95$Target.Sectors_11=="Military" | data95$Target.Sectors_11=="Legislative / Parliamentarty" | data95$Target.Sectors_11=="Judicial" | data95$Target.Sectors_11=="Local" | data95$Target.Sectors_11=="Parties") |
                   (data95$Target.Sectors_12=="Government" | data95$Target.Sectors_12=="Executive" | data95$Target.Sectors_12=="Police" | data95$Target.Sectors_12=="Military" | data95$Target.Sectors_12=="Legislative / Parliamentarty" | data95$Target.Sectors_12=="Judicial" | data95$Target.Sectors_12=="Local" | data95$Target.Sectors_12=="Parties") | 
                   (data95$Target.Sectors_13=="Government" | data95$Target.Sectors_13=="Executive" | data95$Target.Sectors_13=="Police" | data95$Target.Sectors_13=="Military" | data95$Target.Sectors_13=="Legislative / Parliamentarty" | data95$Target.Sectors_13=="Judicial" | data95$Target.Sectors_13=="Local" | data95$Target.Sectors_13=="Parties") |
                   (data95$Target.Sectors_14=="Government" | data95$Target.Sectors_14=="Executive" | data95$Target.Sectors_14=="Police" | data95$Target.Sectors_14=="Military" | data95$Target.Sectors_14=="Legislative / Parliamentarty" | data95$Target.Sectors_14=="Judicial" | data95$Target.Sectors_14=="Local" | data95$Target.Sectors_14=="Parties") |
                   (data95$Target.Sectors_15=="Government" | data95$Target.Sectors_15=="Executive" | data95$Target.Sectors_15=="Police" | data95$Target.Sectors_15=="Military" | data95$Target.Sectors_15=="Legislative / Parliamentarty" | data95$Target.Sectors_15=="Judicial" | data95$Target.Sectors_15=="Local" | data95$Target.Sectors_15=="Parties") |
                   (data95$Target.Sectors_16=="Government" | data95$Target.Sectors_16=="Executive" | data95$Target.Sectors_16=="Police" | data95$Target.Sectors_16=="Military" | data95$Target.Sectors_16=="Legislative / Parliamentarty" | data95$Target.Sectors_16=="Judicial" | data95$Target.Sectors_16=="Local" | data95$Target.Sectors_16=="Parties") | 
                   (data95$Target.Sectors_17=="Government" | data95$Target.Sectors_17=="Executive" | data95$Target.Sectors_17=="Police" | data95$Target.Sectors_17=="Military" | data95$Target.Sectors_17=="Legislative / Parliamentarty" | data95$Target.Sectors_17=="Judicial" | data95$Target.Sectors_17=="Local" | data95$Target.Sectors_17=="Parties") 
)


data95c<-subset(data95b, 
                (data95b$Source.Sectors_1=="Human Rights IGOs" | data95b$Source.Sectors_1=="Global Human Rights IGOs" | data95b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                (data95b$Source.Sectors_2=="Human Rights IGOs" | data95b$Source.Sectors_2=="Global Human Rights IGOs" | data95b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                (data95b$Source.Sectors_3=="Human Rights IGOs" | data95b$Source.Sectors_3=="Global Human Rights IGOs" | data95b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                (data95b$Source.Sectors_4=="Human Rights IGOs" | data95b$Source.Sectors_4=="Global Human Rights IGOs" | data95b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                (data95b$Source.Sectors_5=="Human Rights IGOs" | data95b$Source.Sectors_5=="Global Human Rights IGOs" | data95b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                (data95b$Source.Sectors_6=="Human Rights IGOs" | data95b$Source.Sectors_6=="Global Human Rights IGOs" | data95b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                (data95b$Source.Sectors_7=="Human Rights IGOs" | data95b$Source.Sectors_7=="Global Human Rights IGOs" | data95b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                (data95b$Source.Sectors_8=="Human Rights IGOs" | data95b$Source.Sectors_8=="Global Human Rights IGOs" | data95b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                (data95b$Source.Sectors_9=="Human Rights IGOs" | data95b$Source.Sectors_9=="Global Human Rights IGOs" | data95b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                (data95b$Source.Sectors_10=="Human Rights IGOs" | data95b$Source.Sectors_10=="Global Human Rights IGOs" | data95b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                (data95b$Source.Sectors_11=="Human Rights IGOs" | data95b$Source.Sectors_11=="Global Human Rights IGOs" | data95b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                (data95b$Source.Sectors_12=="Human Rights IGOs" | data95b$Source.Sectors_12=="Global Human Rights IGOs" | data95b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                (data95b$Source.Sectors_13=="Human Rights IGOs" | data95b$Source.Sectors_13=="Global Human Rights IGOs" | data95b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                (data95b$Source.Sectors_14=="Human Rights IGOs" | data95b$Source.Sectors_14=="Global Human Rights IGOs" | data95b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                (data95b$Source.Sectors_15=="Human Rights IGOs" | data95b$Source.Sectors_15=="Global Human Rights IGOs" | data95b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                (data95b$Source.Sectors_16=="Human Rights IGOs" | data95b$Source.Sectors_16=="Global Human Rights IGOs" | data95b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                (data95b$Source.Sectors_17=="Human Rights IGOs" | data95b$Source.Sectors_17=="Global Human Rights IGOs" | data95b$Source.Sectors_17=="Regional Human Rights IGOs") 
        )

data95d<-data95c
data95d$Appeal_Jud_Coop<-ifelse(data95d$CAMEO.Code=="213", 1, NA)
data95d$Appeal_Change_Leadership<-ifelse(data95d$CAMEO.Code=="241", 1, NA)
data95d$Appeal_Policy_Change<-ifelse(data95d$CAMEO.Code=="242", 1, NA)
data95d$Appeal_Rights<-ifelse(data95d$CAMEO.Code=="243", 1, NA)
data95d$Appeal_Change_Inst<-ifelse(data95d$CAMEO.Code=="244", 1, NA)
data95d$Appeal_Release<-ifelse(data95d$CAMEO.Code=="253", 1, NA)
data95d$Demand<-ifelse(data95d$CAMEO.Code=="100", 1, NA)
data95d$Demand_Change_Leadership<-ifelse(data95d$CAMEO.Code=="1041", 1, NA) 
data95d$Demand_Policy_Change<-ifelse(data95d$CAMEO.Code=="1042", 1, NA)
data95d$Demand_Rights<-ifelse(data95d$CAMEO.Code=="1043", 1, NA)
data95d$Demand_Change_Inst<-ifelse(data95d$CAMEO.Code=="1044", 1, NA)
data95d$Accuse_HR<-ifelse(data95d$CAMEO.Code=="1122", 1, NA)
data95d$Accuse<-ifelse(data95d$CAMEO.Code=="112", 1, NA)
data95d$Criticize<-ifelse(data95d$CAMEO.Code=="111", 1, NA)
data95d$Investigate_War_Crimes<-ifelse(data95d$CAMEO.Code=="94", 1, NA)
data95d$Investigate_HR<-ifelse(data95d$CAMEO.Code=="92", 1, NA)
data95d$Demand_Jud_Coop<-ifelse(data95d$CAMEO.Code=="1013", 1, NA)
data95d$Demand_Hum_Aid<-ifelse(data95d$CAMEO.Code=="1033", 1, NA)
data95d$Demand_Release<-ifelse(data95d$CAMEO.Code=="1053", 1, NA)
data95d$Accuse_War_Crimes<-ifelse(data95d$CAMEO.Code=="1124", 1, NA)



data95e<-separate(data95d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts1995<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                        Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                        + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                        + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                        + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                        FUN=sum, data=data95e)



write.csv(event.counts1995, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts1995.cvs") 

#________________________

Events96 <- read.csv2("/Users/KimFruge/Desktop/Projects/Events1996.tab",  sep="\t", header=TRUE)

data96a<-separate(Events96, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data96<-separate(data96a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data96b<- subset(data96,
                 (data96$Target.Sectors_1=="Government" | data96$Target.Sectors_1=="Executive" | data96$Target.Sectors_1=="Police" | data96$Target.Sectors_1=="Military" | data96$Target.Sectors_1=="Legislative / Parliamentarty" | data96$Target.Sectors_1=="Judicial" | data96$Target.Sectors_1=="Local" | data96$Target.Sectors_1=="Parties") |
                         (data96$Target.Sectors_2=="Government" | data96$Target.Sectors_2=="Executive" | data96$Target.Sectors_2=="Police" | data96$Target.Sectors_2=="Military" | data96$Target.Sectors_2=="Legislative / Parliamentarty" | data96$Target.Sectors_2=="Judicial" | data96$Target.Sectors_2=="Local" | data96$Target.Sectors_2=="Parties") |
                         (data96$Target.Sectors_3=="Government" | data96$Target.Sectors_3=="Executive" | data96$Target.Sectors_3=="Police" | data96$Target.Sectors_3=="Military" | data96$Target.Sectors_3=="Legislative / Parliamentarty" | data96$Target.Sectors_3=="Judicial" | data96$Target.Sectors_3=="Local" | data96$Target.Sectors_3=="Parties") |
                         (data96$Target.Sectors_4=="Government" | data96$Target.Sectors_4=="Executive" | data96$Target.Sectors_4=="Police" | data96$Target.Sectors_4=="Military" | data96$Target.Sectors_4=="Legislative / Parliamentarty" | data96$Target.Sectors_4=="Judicial" | data96$Target.Sectors_4=="Local" | data96$Target.Sectors_4=="Parties") |
                         (data96$Target.Sectors_5=="Government" | data96$Target.Sectors_5=="Executive" | data96$Target.Sectors_5=="Police" | data96$Target.Sectors_5=="Military" | data96$Target.Sectors_5=="Legislative / Parliamentarty" | data96$Target.Sectors_5=="Judicial" | data96$Target.Sectors_5=="Local" | data96$Target.Sectors_5=="Parties") |
                         (data96$Target.Sectors_6=="Government" | data96$Target.Sectors_6=="Executive" | data96$Target.Sectors_6=="Police" | data96$Target.Sectors_6=="Military" | data96$Target.Sectors_6=="Legislative / Parliamentarty" | data96$Target.Sectors_6=="Judicial" | data96$Target.Sectors_6=="Local" | data96$Target.Sectors_6=="Parties") | 
                         (data96$Target.Sectors_7=="Government" | data96$Target.Sectors_7=="Executive" | data96$Target.Sectors_7=="Police" | data96$Target.Sectors_7=="Military" | data96$Target.Sectors_7=="Legislative / Parliamentarty" | data96$Target.Sectors_7=="Judicial" | data96$Target.Sectors_7=="Local" | data96$Target.Sectors_7=="Parties") |
                         (data96$Target.Sectors_8=="Government" | data96$Target.Sectors_8=="Executive" | data96$Target.Sectors_8=="Police" | data96$Target.Sectors_8=="Military" | data96$Target.Sectors_8=="Legislative / Parliamentarty" | data96$Target.Sectors_8=="Judicial" | data96$Target.Sectors_8=="Local" | data96$Target.Sectors_8=="Parties") | 
                         (data96$Target.Sectors_9=="Government" | data96$Target.Sectors_9=="Executive" | data96$Target.Sectors_9=="Police" | data96$Target.Sectors_9=="Military" | data96$Target.Sectors_9=="Legislative / Parliamentarty" | data96$Target.Sectors_9=="Judicial" | data96$Target.Sectors_9=="Local" | data96$Target.Sectors_9=="Parties") |
                         (data96$Target.Sectors_10=="Government" | data96$Target.Sectors_10=="Executive" | data96$Target.Sectors_10=="Police" | data96$Target.Sectors_10=="Military" | data96$Target.Sectors_10=="Legislative / Parliamentarty" | data96$Target.Sectors_10=="Judicial" | data96$Target.Sectors_10=="Local" | data96$Target.Sectors_10=="Parties") |
                         (data96$Target.Sectors_11=="Government" | data96$Target.Sectors_11=="Executive" | data96$Target.Sectors_11=="Police" | data96$Target.Sectors_11=="Military" | data96$Target.Sectors_11=="Legislative / Parliamentarty" | data96$Target.Sectors_11=="Judicial" | data96$Target.Sectors_11=="Local" | data96$Target.Sectors_11=="Parties") |
                         (data96$Target.Sectors_12=="Government" | data96$Target.Sectors_12=="Executive" | data96$Target.Sectors_12=="Police" | data96$Target.Sectors_12=="Military" | data96$Target.Sectors_12=="Legislative / Parliamentarty" | data96$Target.Sectors_12=="Judicial" | data96$Target.Sectors_12=="Local" | data96$Target.Sectors_12=="Parties") | 
                         (data96$Target.Sectors_13=="Government" | data96$Target.Sectors_13=="Executive" | data96$Target.Sectors_13=="Police" | data96$Target.Sectors_13=="Military" | data96$Target.Sectors_13=="Legislative / Parliamentarty" | data96$Target.Sectors_13=="Judicial" | data96$Target.Sectors_13=="Local" | data96$Target.Sectors_13=="Parties") |
                         (data96$Target.Sectors_14=="Government" | data96$Target.Sectors_14=="Executive" | data96$Target.Sectors_14=="Police" | data96$Target.Sectors_14=="Military" | data96$Target.Sectors_14=="Legislative / Parliamentarty" | data96$Target.Sectors_14=="Judicial" | data96$Target.Sectors_14=="Local" | data96$Target.Sectors_14=="Parties") |
                         (data96$Target.Sectors_15=="Government" | data96$Target.Sectors_15=="Executive" | data96$Target.Sectors_15=="Police" | data96$Target.Sectors_15=="Military" | data96$Target.Sectors_15=="Legislative / Parliamentarty" | data96$Target.Sectors_15=="Judicial" | data96$Target.Sectors_15=="Local" | data96$Target.Sectors_15=="Parties") |
                         (data96$Target.Sectors_16=="Government" | data96$Target.Sectors_16=="Executive" | data96$Target.Sectors_16=="Police" | data96$Target.Sectors_16=="Military" | data96$Target.Sectors_16=="Legislative / Parliamentarty" | data96$Target.Sectors_16=="Judicial" | data96$Target.Sectors_16=="Local" | data96$Target.Sectors_16=="Parties") | 
                         (data96$Target.Sectors_17=="Government" | data96$Target.Sectors_17=="Executive" | data96$Target.Sectors_17=="Police" | data96$Target.Sectors_17=="Military" | data96$Target.Sectors_17=="Legislative / Parliamentarty" | data96$Target.Sectors_17=="Judicial" | data96$Target.Sectors_17=="Local" | data96$Target.Sectors_17=="Parties") 
)


data96c<-subset(data96b, 
                (data96b$Source.Sectors_1=="Human Rights IGOs" | data96b$Source.Sectors_1=="Global Human Rights IGOs" | data96b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data96b$Source.Sectors_2=="Human Rights IGOs" | data96b$Source.Sectors_2=="Global Human Rights IGOs" | data96b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data96b$Source.Sectors_3=="Human Rights IGOs" | data96b$Source.Sectors_3=="Global Human Rights IGOs" | data96b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data96b$Source.Sectors_4=="Human Rights IGOs" | data96b$Source.Sectors_4=="Global Human Rights IGOs" | data96b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data96b$Source.Sectors_5=="Human Rights IGOs" | data96b$Source.Sectors_5=="Global Human Rights IGOs" | data96b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data96b$Source.Sectors_6=="Human Rights IGOs" | data96b$Source.Sectors_6=="Global Human Rights IGOs" | data96b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data96b$Source.Sectors_7=="Human Rights IGOs" | data96b$Source.Sectors_7=="Global Human Rights IGOs" | data96b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data96b$Source.Sectors_8=="Human Rights IGOs" | data96b$Source.Sectors_8=="Global Human Rights IGOs" | data96b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data96b$Source.Sectors_9=="Human Rights IGOs" | data96b$Source.Sectors_9=="Global Human Rights IGOs" | data96b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data96b$Source.Sectors_10=="Human Rights IGOs" | data96b$Source.Sectors_10=="Global Human Rights IGOs" | data96b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data96b$Source.Sectors_11=="Human Rights IGOs" | data96b$Source.Sectors_11=="Global Human Rights IGOs" | data96b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data96b$Source.Sectors_12=="Human Rights IGOs" | data96b$Source.Sectors_12=="Global Human Rights IGOs" | data96b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data96b$Source.Sectors_13=="Human Rights IGOs" | data96b$Source.Sectors_13=="Global Human Rights IGOs" | data96b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data96b$Source.Sectors_14=="Human Rights IGOs" | data96b$Source.Sectors_14=="Global Human Rights IGOs" | data96b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data96b$Source.Sectors_15=="Human Rights IGOs" | data96b$Source.Sectors_15=="Global Human Rights IGOs" | data96b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data96b$Source.Sectors_16=="Human Rights IGOs" | data96b$Source.Sectors_16=="Global Human Rights IGOs" | data96b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data96b$Source.Sectors_17=="Human Rights IGOs" | data96b$Source.Sectors_17=="Global Human Rights IGOs" | data96b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data96d<-data96c
data96d$Appeal_Jud_Coop<-ifelse(data96d$CAMEO.Code=="213", 1, NA)
data96d$Appeal_Change_Leadership<-ifelse(data96d$CAMEO.Code=="241", 1, NA)
data96d$Appeal_Policy_Change<-ifelse(data96d$CAMEO.Code=="242", 1, NA)
data96d$Appeal_Rights<-ifelse(data96d$CAMEO.Code=="243", 1, NA)
data96d$Appeal_Change_Inst<-ifelse(data96d$CAMEO.Code=="244", 1, NA)
data96d$Appeal_Release<-ifelse(data96d$CAMEO.Code=="253", 1, NA)
data96d$Demand<-ifelse(data96d$CAMEO.Code=="100", 1, NA)
data96d$Demand_Change_Leadership<-ifelse(data96d$CAMEO.Code=="1041", 1, NA) 
data96d$Demand_Policy_Change<-ifelse(data96d$CAMEO.Code=="1042", 1, NA)
data96d$Demand_Rights<-ifelse(data96d$CAMEO.Code=="1043", 1, NA)
data96d$Demand_Change_Inst<-ifelse(data96d$CAMEO.Code=="1044", 1, NA)
data96d$Accuse_HR<-ifelse(data96d$CAMEO.Code=="1122", 1, NA)
data96d$Accuse<-ifelse(data96d$CAMEO.Code=="112", 1, NA)
data96d$Criticize<-ifelse(data96d$CAMEO.Code=="111", 1, NA)
data96d$Investigate_War_Crimes<-ifelse(data96d$CAMEO.Code=="94", 1, NA)
data96d$Investigate_HR<-ifelse(data96d$CAMEO.Code=="92", 1, NA)
data96d$Demand_Jud_Coop<-ifelse(data96d$CAMEO.Code=="1013", 1, NA)
data96d$Demand_Hum_Aid<-ifelse(data96d$CAMEO.Code=="1033", 1, NA)
data96d$Demand_Release<-ifelse(data96d$CAMEO.Code=="1053", 1, NA)
data96d$Accuse_War_Crimes<-ifelse(data96d$CAMEO.Code=="1124", 1, NA)



data96e<-separate(data96d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts1996<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data96e)



write.csv(event.counts1996, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts1996.cvs") 

#____________________________

Events97 <- read.csv2("/Users/KimFruge/Desktop/Projects/Events1997.tab",  sep="\t", header=TRUE)

data97a<-separate(Events97, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data97<-separate(data97a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data97b<- subset(data97,
                 (data97$Target.Sectors_1=="Government" | data97$Target.Sectors_1=="Executive" | data97$Target.Sectors_1=="Police" | data97$Target.Sectors_1=="Military" | data97$Target.Sectors_1=="Legislative / Parliamentarty" | data97$Target.Sectors_1=="Judicial" | data97$Target.Sectors_1=="Local" | data97$Target.Sectors_1=="Parties") |
                         (data97$Target.Sectors_2=="Government" | data97$Target.Sectors_2=="Executive" | data97$Target.Sectors_2=="Police" | data97$Target.Sectors_2=="Military" | data97$Target.Sectors_2=="Legislative / Parliamentarty" | data97$Target.Sectors_2=="Judicial" | data97$Target.Sectors_2=="Local" | data97$Target.Sectors_2=="Parties") |
                         (data97$Target.Sectors_3=="Government" | data97$Target.Sectors_3=="Executive" | data97$Target.Sectors_3=="Police" | data97$Target.Sectors_3=="Military" | data97$Target.Sectors_3=="Legislative / Parliamentarty" | data97$Target.Sectors_3=="Judicial" | data97$Target.Sectors_3=="Local" | data97$Target.Sectors_3=="Parties") |
                         (data97$Target.Sectors_4=="Government" | data97$Target.Sectors_4=="Executive" | data97$Target.Sectors_4=="Police" | data97$Target.Sectors_4=="Military" | data97$Target.Sectors_4=="Legislative / Parliamentarty" | data97$Target.Sectors_4=="Judicial" | data97$Target.Sectors_4=="Local" | data97$Target.Sectors_4=="Parties") |
                         (data97$Target.Sectors_5=="Government" | data97$Target.Sectors_5=="Executive" | data97$Target.Sectors_5=="Police" | data97$Target.Sectors_5=="Military" | data97$Target.Sectors_5=="Legislative / Parliamentarty" | data97$Target.Sectors_5=="Judicial" | data97$Target.Sectors_5=="Local" | data97$Target.Sectors_5=="Parties") |
                         (data97$Target.Sectors_6=="Government" | data97$Target.Sectors_6=="Executive" | data97$Target.Sectors_6=="Police" | data97$Target.Sectors_6=="Military" | data97$Target.Sectors_6=="Legislative / Parliamentarty" | data97$Target.Sectors_6=="Judicial" | data97$Target.Sectors_6=="Local" | data97$Target.Sectors_6=="Parties") | 
                         (data97$Target.Sectors_7=="Government" | data97$Target.Sectors_7=="Executive" | data97$Target.Sectors_7=="Police" | data97$Target.Sectors_7=="Military" | data97$Target.Sectors_7=="Legislative / Parliamentarty" | data97$Target.Sectors_7=="Judicial" | data97$Target.Sectors_7=="Local" | data97$Target.Sectors_7=="Parties") |
                         (data97$Target.Sectors_8=="Government" | data97$Target.Sectors_8=="Executive" | data97$Target.Sectors_8=="Police" | data97$Target.Sectors_8=="Military" | data97$Target.Sectors_8=="Legislative / Parliamentarty" | data97$Target.Sectors_8=="Judicial" | data97$Target.Sectors_8=="Local" | data97$Target.Sectors_8=="Parties") | 
                         (data97$Target.Sectors_9=="Government" | data97$Target.Sectors_9=="Executive" | data97$Target.Sectors_9=="Police" | data97$Target.Sectors_9=="Military" | data97$Target.Sectors_9=="Legislative / Parliamentarty" | data97$Target.Sectors_9=="Judicial" | data97$Target.Sectors_9=="Local" | data97$Target.Sectors_9=="Parties") |
                         (data97$Target.Sectors_10=="Government" | data97$Target.Sectors_10=="Executive" | data97$Target.Sectors_10=="Police" | data97$Target.Sectors_10=="Military" | data97$Target.Sectors_10=="Legislative / Parliamentarty" | data97$Target.Sectors_10=="Judicial" | data97$Target.Sectors_10=="Local" | data97$Target.Sectors_10=="Parties") |
                         (data97$Target.Sectors_11=="Government" | data97$Target.Sectors_11=="Executive" | data97$Target.Sectors_11=="Police" | data97$Target.Sectors_11=="Military" | data97$Target.Sectors_11=="Legislative / Parliamentarty" | data97$Target.Sectors_11=="Judicial" | data97$Target.Sectors_11=="Local" | data97$Target.Sectors_11=="Parties") |
                         (data97$Target.Sectors_12=="Government" | data97$Target.Sectors_12=="Executive" | data97$Target.Sectors_12=="Police" | data97$Target.Sectors_12=="Military" | data97$Target.Sectors_12=="Legislative / Parliamentarty" | data97$Target.Sectors_12=="Judicial" | data97$Target.Sectors_12=="Local" | data97$Target.Sectors_12=="Parties") | 
                         (data97$Target.Sectors_13=="Government" | data97$Target.Sectors_13=="Executive" | data97$Target.Sectors_13=="Police" | data97$Target.Sectors_13=="Military" | data97$Target.Sectors_13=="Legislative / Parliamentarty" | data97$Target.Sectors_13=="Judicial" | data97$Target.Sectors_13=="Local" | data97$Target.Sectors_13=="Parties") |
                         (data97$Target.Sectors_14=="Government" | data97$Target.Sectors_14=="Executive" | data97$Target.Sectors_14=="Police" | data97$Target.Sectors_14=="Military" | data97$Target.Sectors_14=="Legislative / Parliamentarty" | data97$Target.Sectors_14=="Judicial" | data97$Target.Sectors_14=="Local" | data97$Target.Sectors_14=="Parties") |
                         (data97$Target.Sectors_15=="Government" | data97$Target.Sectors_15=="Executive" | data97$Target.Sectors_15=="Police" | data97$Target.Sectors_15=="Military" | data97$Target.Sectors_15=="Legislative / Parliamentarty" | data97$Target.Sectors_15=="Judicial" | data97$Target.Sectors_15=="Local" | data97$Target.Sectors_15=="Parties") |
                         (data97$Target.Sectors_16=="Government" | data97$Target.Sectors_16=="Executive" | data97$Target.Sectors_16=="Police" | data97$Target.Sectors_16=="Military" | data97$Target.Sectors_16=="Legislative / Parliamentarty" | data97$Target.Sectors_16=="Judicial" | data97$Target.Sectors_16=="Local" | data97$Target.Sectors_16=="Parties") | 
                         (data97$Target.Sectors_17=="Government" | data97$Target.Sectors_17=="Executive" | data97$Target.Sectors_17=="Police" | data97$Target.Sectors_17=="Military" | data97$Target.Sectors_17=="Legislative / Parliamentarty" | data97$Target.Sectors_17=="Judicial" | data97$Target.Sectors_17=="Local" | data97$Target.Sectors_17=="Parties") 
)


data97c<-subset(data97b, 
                (data97b$Source.Sectors_1=="Human Rights IGOs" | data97b$Source.Sectors_1=="Global Human Rights IGOs" | data97b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data97b$Source.Sectors_2=="Human Rights IGOs" | data97b$Source.Sectors_2=="Global Human Rights IGOs" | data97b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data97b$Source.Sectors_3=="Human Rights IGOs" | data97b$Source.Sectors_3=="Global Human Rights IGOs" | data97b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data97b$Source.Sectors_4=="Human Rights IGOs" | data97b$Source.Sectors_4=="Global Human Rights IGOs" | data97b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data97b$Source.Sectors_5=="Human Rights IGOs" | data97b$Source.Sectors_5=="Global Human Rights IGOs" | data97b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data97b$Source.Sectors_6=="Human Rights IGOs" | data97b$Source.Sectors_6=="Global Human Rights IGOs" | data97b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data97b$Source.Sectors_7=="Human Rights IGOs" | data97b$Source.Sectors_7=="Global Human Rights IGOs" | data97b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data97b$Source.Sectors_8=="Human Rights IGOs" | data97b$Source.Sectors_8=="Global Human Rights IGOs" | data97b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data97b$Source.Sectors_9=="Human Rights IGOs" | data97b$Source.Sectors_9=="Global Human Rights IGOs" | data97b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data97b$Source.Sectors_10=="Human Rights IGOs" | data97b$Source.Sectors_10=="Global Human Rights IGOs" | data97b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data97b$Source.Sectors_11=="Human Rights IGOs" | data97b$Source.Sectors_11=="Global Human Rights IGOs" | data97b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data97b$Source.Sectors_12=="Human Rights IGOs" | data97b$Source.Sectors_12=="Global Human Rights IGOs" | data97b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data97b$Source.Sectors_13=="Human Rights IGOs" | data97b$Source.Sectors_13=="Global Human Rights IGOs" | data97b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data97b$Source.Sectors_14=="Human Rights IGOs" | data97b$Source.Sectors_14=="Global Human Rights IGOs" | data97b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data97b$Source.Sectors_15=="Human Rights IGOs" | data97b$Source.Sectors_15=="Global Human Rights IGOs" | data97b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data97b$Source.Sectors_16=="Human Rights IGOs" | data97b$Source.Sectors_16=="Global Human Rights IGOs" | data97b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data97b$Source.Sectors_17=="Human Rights IGOs" | data97b$Source.Sectors_17=="Global Human Rights IGOs" | data97b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data97d<-data97c
data97d$Appeal_Jud_Coop<-ifelse(data97d$CAMEO.Code=="213", 1, NA)
data97d$Appeal_Change_Leadership<-ifelse(data97d$CAMEO.Code=="241", 1, NA)
data97d$Appeal_Policy_Change<-ifelse(data97d$CAMEO.Code=="242", 1, NA)
data97d$Appeal_Rights<-ifelse(data97d$CAMEO.Code=="243", 1, NA)
data97d$Appeal_Change_Inst<-ifelse(data97d$CAMEO.Code=="244", 1, NA)
data97d$Appeal_Release<-ifelse(data97d$CAMEO.Code=="253", 1, NA)
data97d$Demand<-ifelse(data97d$CAMEO.Code=="100", 1, NA)
data97d$Demand_Change_Leadership<-ifelse(data97d$CAMEO.Code=="1041", 1, NA) 
data97d$Demand_Policy_Change<-ifelse(data97d$CAMEO.Code=="1042", 1, NA)
data97d$Demand_Rights<-ifelse(data97d$CAMEO.Code=="1043", 1, NA)
data97d$Demand_Change_Inst<-ifelse(data97d$CAMEO.Code=="1044", 1, NA)
data97d$Accuse_HR<-ifelse(data97d$CAMEO.Code=="1122", 1, NA)
data97d$Accuse<-ifelse(data97d$CAMEO.Code=="112", 1, NA)
data97d$Criticize<-ifelse(data97d$CAMEO.Code=="111", 1, NA)
data97d$Investigate_War_Crimes<-ifelse(data97d$CAMEO.Code=="94", 1, NA)
data97d$Investigate_HR<-ifelse(data97d$CAMEO.Code=="92", 1, NA)
data97d$Demand_Jud_Coop<-ifelse(data97d$CAMEO.Code=="1013", 1, NA)
data97d$Demand_Hum_Aid<-ifelse(data97d$CAMEO.Code=="1033", 1, NA)
data97d$Demand_Release<-ifelse(data97d$CAMEO.Code=="1053", 1, NA)
data97d$Accuse_War_Crimes<-ifelse(data97d$CAMEO.Code=="1124", 1, NA)



data97e<-separate(data97d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts1997<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data97e)



write.csv(event.counts1997, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts1997.cvs") 


#____________________________

Events98 <- read.csv2("/Users/KimFruge/Desktop/Projects/Events1998.tab",  sep="\t", header=TRUE)

data98a<-separate(Events98, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data98<-separate(data98a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data98b<- subset(data98,
                 (data98$Target.Sectors_1=="Government" | data98$Target.Sectors_1=="Executive" | data98$Target.Sectors_1=="Police" | data98$Target.Sectors_1=="Military" | data98$Target.Sectors_1=="Legislative / Parliamentarty" | data98$Target.Sectors_1=="Judicial" | data98$Target.Sectors_1=="Local" | data98$Target.Sectors_1=="Parties") |
                         (data98$Target.Sectors_2=="Government" | data98$Target.Sectors_2=="Executive" | data98$Target.Sectors_2=="Police" | data98$Target.Sectors_2=="Military" | data98$Target.Sectors_2=="Legislative / Parliamentarty" | data98$Target.Sectors_2=="Judicial" | data98$Target.Sectors_2=="Local" | data98$Target.Sectors_2=="Parties") |
                         (data98$Target.Sectors_3=="Government" | data98$Target.Sectors_3=="Executive" | data98$Target.Sectors_3=="Police" | data98$Target.Sectors_3=="Military" | data98$Target.Sectors_3=="Legislative / Parliamentarty" | data98$Target.Sectors_3=="Judicial" | data98$Target.Sectors_3=="Local" | data98$Target.Sectors_3=="Parties") |
                         (data98$Target.Sectors_4=="Government" | data98$Target.Sectors_4=="Executive" | data98$Target.Sectors_4=="Police" | data98$Target.Sectors_4=="Military" | data98$Target.Sectors_4=="Legislative / Parliamentarty" | data98$Target.Sectors_4=="Judicial" | data98$Target.Sectors_4=="Local" | data98$Target.Sectors_4=="Parties") |
                         (data98$Target.Sectors_5=="Government" | data98$Target.Sectors_5=="Executive" | data98$Target.Sectors_5=="Police" | data98$Target.Sectors_5=="Military" | data98$Target.Sectors_5=="Legislative / Parliamentarty" | data98$Target.Sectors_5=="Judicial" | data98$Target.Sectors_5=="Local" | data98$Target.Sectors_5=="Parties") |
                         (data98$Target.Sectors_6=="Government" | data98$Target.Sectors_6=="Executive" | data98$Target.Sectors_6=="Police" | data98$Target.Sectors_6=="Military" | data98$Target.Sectors_6=="Legislative / Parliamentarty" | data98$Target.Sectors_6=="Judicial" | data98$Target.Sectors_6=="Local" | data98$Target.Sectors_6=="Parties") | 
                         (data98$Target.Sectors_7=="Government" | data98$Target.Sectors_7=="Executive" | data98$Target.Sectors_7=="Police" | data98$Target.Sectors_7=="Military" | data98$Target.Sectors_7=="Legislative / Parliamentarty" | data98$Target.Sectors_7=="Judicial" | data98$Target.Sectors_7=="Local" | data98$Target.Sectors_7=="Parties") |
                         (data98$Target.Sectors_8=="Government" | data98$Target.Sectors_8=="Executive" | data98$Target.Sectors_8=="Police" | data98$Target.Sectors_8=="Military" | data98$Target.Sectors_8=="Legislative / Parliamentarty" | data98$Target.Sectors_8=="Judicial" | data98$Target.Sectors_8=="Local" | data98$Target.Sectors_8=="Parties") | 
                         (data98$Target.Sectors_9=="Government" | data98$Target.Sectors_9=="Executive" | data98$Target.Sectors_9=="Police" | data98$Target.Sectors_9=="Military" | data98$Target.Sectors_9=="Legislative / Parliamentarty" | data98$Target.Sectors_9=="Judicial" | data98$Target.Sectors_9=="Local" | data98$Target.Sectors_9=="Parties") |
                         (data98$Target.Sectors_10=="Government" | data98$Target.Sectors_10=="Executive" | data98$Target.Sectors_10=="Police" | data98$Target.Sectors_10=="Military" | data98$Target.Sectors_10=="Legislative / Parliamentarty" | data98$Target.Sectors_10=="Judicial" | data98$Target.Sectors_10=="Local" | data98$Target.Sectors_10=="Parties") |
                         (data98$Target.Sectors_11=="Government" | data98$Target.Sectors_11=="Executive" | data98$Target.Sectors_11=="Police" | data98$Target.Sectors_11=="Military" | data98$Target.Sectors_11=="Legislative / Parliamentarty" | data98$Target.Sectors_11=="Judicial" | data98$Target.Sectors_11=="Local" | data98$Target.Sectors_11=="Parties") |
                         (data98$Target.Sectors_12=="Government" | data98$Target.Sectors_12=="Executive" | data98$Target.Sectors_12=="Police" | data98$Target.Sectors_12=="Military" | data98$Target.Sectors_12=="Legislative / Parliamentarty" | data98$Target.Sectors_12=="Judicial" | data98$Target.Sectors_12=="Local" | data98$Target.Sectors_12=="Parties") | 
                         (data98$Target.Sectors_13=="Government" | data98$Target.Sectors_13=="Executive" | data98$Target.Sectors_13=="Police" | data98$Target.Sectors_13=="Military" | data98$Target.Sectors_13=="Legislative / Parliamentarty" | data98$Target.Sectors_13=="Judicial" | data98$Target.Sectors_13=="Local" | data98$Target.Sectors_13=="Parties") |
                         (data98$Target.Sectors_14=="Government" | data98$Target.Sectors_14=="Executive" | data98$Target.Sectors_14=="Police" | data98$Target.Sectors_14=="Military" | data98$Target.Sectors_14=="Legislative / Parliamentarty" | data98$Target.Sectors_14=="Judicial" | data98$Target.Sectors_14=="Local" | data98$Target.Sectors_14=="Parties") |
                         (data98$Target.Sectors_15=="Government" | data98$Target.Sectors_15=="Executive" | data98$Target.Sectors_15=="Police" | data98$Target.Sectors_15=="Military" | data98$Target.Sectors_15=="Legislative / Parliamentarty" | data98$Target.Sectors_15=="Judicial" | data98$Target.Sectors_15=="Local" | data98$Target.Sectors_15=="Parties") |
                         (data98$Target.Sectors_16=="Government" | data98$Target.Sectors_16=="Executive" | data98$Target.Sectors_16=="Police" | data98$Target.Sectors_16=="Military" | data98$Target.Sectors_16=="Legislative / Parliamentarty" | data98$Target.Sectors_16=="Judicial" | data98$Target.Sectors_16=="Local" | data98$Target.Sectors_16=="Parties") | 
                         (data98$Target.Sectors_17=="Government" | data98$Target.Sectors_17=="Executive" | data98$Target.Sectors_17=="Police" | data98$Target.Sectors_17=="Military" | data98$Target.Sectors_17=="Legislative / Parliamentarty" | data98$Target.Sectors_17=="Judicial" | data98$Target.Sectors_17=="Local" | data98$Target.Sectors_17=="Parties") 
)


data98c<-subset(data98b, 
                (data98b$Source.Sectors_1=="Human Rights IGOs" | data98b$Source.Sectors_1=="Global Human Rights IGOs" | data98b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data98b$Source.Sectors_2=="Human Rights IGOs" | data98b$Source.Sectors_2=="Global Human Rights IGOs" | data98b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data98b$Source.Sectors_3=="Human Rights IGOs" | data98b$Source.Sectors_3=="Global Human Rights IGOs" | data98b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data98b$Source.Sectors_4=="Human Rights IGOs" | data98b$Source.Sectors_4=="Global Human Rights IGOs" | data98b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data98b$Source.Sectors_5=="Human Rights IGOs" | data98b$Source.Sectors_5=="Global Human Rights IGOs" | data98b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data98b$Source.Sectors_6=="Human Rights IGOs" | data98b$Source.Sectors_6=="Global Human Rights IGOs" | data98b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data98b$Source.Sectors_7=="Human Rights IGOs" | data98b$Source.Sectors_7=="Global Human Rights IGOs" | data98b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data98b$Source.Sectors_8=="Human Rights IGOs" | data98b$Source.Sectors_8=="Global Human Rights IGOs" | data98b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data98b$Source.Sectors_9=="Human Rights IGOs" | data98b$Source.Sectors_9=="Global Human Rights IGOs" | data98b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data98b$Source.Sectors_10=="Human Rights IGOs" | data98b$Source.Sectors_10=="Global Human Rights IGOs" | data98b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data98b$Source.Sectors_11=="Human Rights IGOs" | data98b$Source.Sectors_11=="Global Human Rights IGOs" | data98b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data98b$Source.Sectors_12=="Human Rights IGOs" | data98b$Source.Sectors_12=="Global Human Rights IGOs" | data98b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data98b$Source.Sectors_13=="Human Rights IGOs" | data98b$Source.Sectors_13=="Global Human Rights IGOs" | data98b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data98b$Source.Sectors_14=="Human Rights IGOs" | data98b$Source.Sectors_14=="Global Human Rights IGOs" | data98b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data98b$Source.Sectors_15=="Human Rights IGOs" | data98b$Source.Sectors_15=="Global Human Rights IGOs" | data98b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data98b$Source.Sectors_16=="Human Rights IGOs" | data98b$Source.Sectors_16=="Global Human Rights IGOs" | data98b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data98b$Source.Sectors_17=="Human Rights IGOs" | data98b$Source.Sectors_17=="Global Human Rights IGOs" | data98b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data98d<-data98c
data98d$Appeal_Jud_Coop<-ifelse(data98d$CAMEO.Code=="213", 1, NA)
data98d$Appeal_Change_Leadership<-ifelse(data98d$CAMEO.Code=="241", 1, NA)
data98d$Appeal_Policy_Change<-ifelse(data98d$CAMEO.Code=="242", 1, NA)
data98d$Appeal_Rights<-ifelse(data98d$CAMEO.Code=="243", 1, NA)
data98d$Appeal_Change_Inst<-ifelse(data98d$CAMEO.Code=="244", 1, NA)
data98d$Appeal_Release<-ifelse(data98d$CAMEO.Code=="253", 1, NA)
data98d$Demand<-ifelse(data98d$CAMEO.Code=="100", 1, NA)
data98d$Demand_Change_Leadership<-ifelse(data98d$CAMEO.Code=="1041", 1, NA) 
data98d$Demand_Policy_Change<-ifelse(data98d$CAMEO.Code=="1042", 1, NA)
data98d$Demand_Rights<-ifelse(data98d$CAMEO.Code=="1043", 1, NA)
data98d$Demand_Change_Inst<-ifelse(data98d$CAMEO.Code=="1044", 1, NA)
data98d$Accuse_HR<-ifelse(data98d$CAMEO.Code=="1122", 1, NA)
data98d$Accuse<-ifelse(data98d$CAMEO.Code=="112", 1, NA)
data98d$Criticize<-ifelse(data98d$CAMEO.Code=="111", 1, NA)
data98d$Investigate_War_Crimes<-ifelse(data98d$CAMEO.Code=="94", 1, NA)
data98d$Investigate_HR<-ifelse(data98d$CAMEO.Code=="92", 1, NA)
data98d$Demand_Jud_Coop<-ifelse(data98d$CAMEO.Code=="1013", 1, NA)
data98d$Demand_Hum_Aid<-ifelse(data98d$CAMEO.Code=="1033", 1, NA)
data98d$Demand_Release<-ifelse(data98d$CAMEO.Code=="1053", 1, NA)
data98d$Accuse_War_Crimes<-ifelse(data98d$CAMEO.Code=="1124", 1, NA)



data98e<-separate(data98d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts1998<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data98e)



write.csv(event.counts1998, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts1998.cvs") 



#____________________________

Events99 <- read.csv2("/Users/KimFruge/Desktop/Projects/Events1999.tab",  sep="\t", header=TRUE)

data99a<-separate(Events99, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data99<-separate(data99a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data99b<- subset(data99,
                 (data99$Target.Sectors_1=="Government" | data99$Target.Sectors_1=="Executive" | data99$Target.Sectors_1=="Police" | data99$Target.Sectors_1=="Military" | data99$Target.Sectors_1=="Legislative / Parliamentarty" | data99$Target.Sectors_1=="Judicial" | data99$Target.Sectors_1=="Local" | data99$Target.Sectors_1=="Parties") |
                         (data99$Target.Sectors_2=="Government" | data99$Target.Sectors_2=="Executive" | data99$Target.Sectors_2=="Police" | data99$Target.Sectors_2=="Military" | data99$Target.Sectors_2=="Legislative / Parliamentarty" | data99$Target.Sectors_2=="Judicial" | data99$Target.Sectors_2=="Local" | data99$Target.Sectors_2=="Parties") |
                         (data99$Target.Sectors_3=="Government" | data99$Target.Sectors_3=="Executive" | data99$Target.Sectors_3=="Police" | data99$Target.Sectors_3=="Military" | data99$Target.Sectors_3=="Legislative / Parliamentarty" | data99$Target.Sectors_3=="Judicial" | data99$Target.Sectors_3=="Local" | data99$Target.Sectors_3=="Parties") |
                         (data99$Target.Sectors_4=="Government" | data99$Target.Sectors_4=="Executive" | data99$Target.Sectors_4=="Police" | data99$Target.Sectors_4=="Military" | data99$Target.Sectors_4=="Legislative / Parliamentarty" | data99$Target.Sectors_4=="Judicial" | data99$Target.Sectors_4=="Local" | data99$Target.Sectors_4=="Parties") |
                         (data99$Target.Sectors_5=="Government" | data99$Target.Sectors_5=="Executive" | data99$Target.Sectors_5=="Police" | data99$Target.Sectors_5=="Military" | data99$Target.Sectors_5=="Legislative / Parliamentarty" | data99$Target.Sectors_5=="Judicial" | data99$Target.Sectors_5=="Local" | data99$Target.Sectors_5=="Parties") |
                         (data99$Target.Sectors_6=="Government" | data99$Target.Sectors_6=="Executive" | data99$Target.Sectors_6=="Police" | data99$Target.Sectors_6=="Military" | data99$Target.Sectors_6=="Legislative / Parliamentarty" | data99$Target.Sectors_6=="Judicial" | data99$Target.Sectors_6=="Local" | data99$Target.Sectors_6=="Parties") | 
                         (data99$Target.Sectors_7=="Government" | data99$Target.Sectors_7=="Executive" | data99$Target.Sectors_7=="Police" | data99$Target.Sectors_7=="Military" | data99$Target.Sectors_7=="Legislative / Parliamentarty" | data99$Target.Sectors_7=="Judicial" | data99$Target.Sectors_7=="Local" | data99$Target.Sectors_7=="Parties") |
                         (data99$Target.Sectors_8=="Government" | data99$Target.Sectors_8=="Executive" | data99$Target.Sectors_8=="Police" | data99$Target.Sectors_8=="Military" | data99$Target.Sectors_8=="Legislative / Parliamentarty" | data99$Target.Sectors_8=="Judicial" | data99$Target.Sectors_8=="Local" | data99$Target.Sectors_8=="Parties") | 
                         (data99$Target.Sectors_9=="Government" | data99$Target.Sectors_9=="Executive" | data99$Target.Sectors_9=="Police" | data99$Target.Sectors_9=="Military" | data99$Target.Sectors_9=="Legislative / Parliamentarty" | data99$Target.Sectors_9=="Judicial" | data99$Target.Sectors_9=="Local" | data99$Target.Sectors_9=="Parties") |
                         (data99$Target.Sectors_10=="Government" | data99$Target.Sectors_10=="Executive" | data99$Target.Sectors_10=="Police" | data99$Target.Sectors_10=="Military" | data99$Target.Sectors_10=="Legislative / Parliamentarty" | data99$Target.Sectors_10=="Judicial" | data99$Target.Sectors_10=="Local" | data99$Target.Sectors_10=="Parties") |
                         (data99$Target.Sectors_11=="Government" | data99$Target.Sectors_11=="Executive" | data99$Target.Sectors_11=="Police" | data99$Target.Sectors_11=="Military" | data99$Target.Sectors_11=="Legislative / Parliamentarty" | data99$Target.Sectors_11=="Judicial" | data99$Target.Sectors_11=="Local" | data99$Target.Sectors_11=="Parties") |
                         (data99$Target.Sectors_12=="Government" | data99$Target.Sectors_12=="Executive" | data99$Target.Sectors_12=="Police" | data99$Target.Sectors_12=="Military" | data99$Target.Sectors_12=="Legislative / Parliamentarty" | data99$Target.Sectors_12=="Judicial" | data99$Target.Sectors_12=="Local" | data99$Target.Sectors_12=="Parties") | 
                         (data99$Target.Sectors_13=="Government" | data99$Target.Sectors_13=="Executive" | data99$Target.Sectors_13=="Police" | data99$Target.Sectors_13=="Military" | data99$Target.Sectors_13=="Legislative / Parliamentarty" | data99$Target.Sectors_13=="Judicial" | data99$Target.Sectors_13=="Local" | data99$Target.Sectors_13=="Parties") |
                         (data99$Target.Sectors_14=="Government" | data99$Target.Sectors_14=="Executive" | data99$Target.Sectors_14=="Police" | data99$Target.Sectors_14=="Military" | data99$Target.Sectors_14=="Legislative / Parliamentarty" | data99$Target.Sectors_14=="Judicial" | data99$Target.Sectors_14=="Local" | data99$Target.Sectors_14=="Parties") |
                         (data99$Target.Sectors_15=="Government" | data99$Target.Sectors_15=="Executive" | data99$Target.Sectors_15=="Police" | data99$Target.Sectors_15=="Military" | data99$Target.Sectors_15=="Legislative / Parliamentarty" | data99$Target.Sectors_15=="Judicial" | data99$Target.Sectors_15=="Local" | data99$Target.Sectors_15=="Parties") |
                         (data99$Target.Sectors_16=="Government" | data99$Target.Sectors_16=="Executive" | data99$Target.Sectors_16=="Police" | data99$Target.Sectors_16=="Military" | data99$Target.Sectors_16=="Legislative / Parliamentarty" | data99$Target.Sectors_16=="Judicial" | data99$Target.Sectors_16=="Local" | data99$Target.Sectors_16=="Parties") | 
                         (data99$Target.Sectors_17=="Government" | data99$Target.Sectors_17=="Executive" | data99$Target.Sectors_17=="Police" | data99$Target.Sectors_17=="Military" | data99$Target.Sectors_17=="Legislative / Parliamentarty" | data99$Target.Sectors_17=="Judicial" | data99$Target.Sectors_17=="Local" | data99$Target.Sectors_17=="Parties") 
)


data99c<-subset(data99b, 
                (data99b$Source.Sectors_1=="Human Rights IGOs" | data99b$Source.Sectors_1=="Global Human Rights IGOs" | data99b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data99b$Source.Sectors_2=="Human Rights IGOs" | data99b$Source.Sectors_2=="Global Human Rights IGOs" | data99b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data99b$Source.Sectors_3=="Human Rights IGOs" | data99b$Source.Sectors_3=="Global Human Rights IGOs" | data99b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data99b$Source.Sectors_4=="Human Rights IGOs" | data99b$Source.Sectors_4=="Global Human Rights IGOs" | data99b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data99b$Source.Sectors_5=="Human Rights IGOs" | data99b$Source.Sectors_5=="Global Human Rights IGOs" | data99b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data99b$Source.Sectors_6=="Human Rights IGOs" | data99b$Source.Sectors_6=="Global Human Rights IGOs" | data99b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data99b$Source.Sectors_7=="Human Rights IGOs" | data99b$Source.Sectors_7=="Global Human Rights IGOs" | data99b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data99b$Source.Sectors_8=="Human Rights IGOs" | data99b$Source.Sectors_8=="Global Human Rights IGOs" | data99b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data99b$Source.Sectors_9=="Human Rights IGOs" | data99b$Source.Sectors_9=="Global Human Rights IGOs" | data99b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data99b$Source.Sectors_10=="Human Rights IGOs" | data99b$Source.Sectors_10=="Global Human Rights IGOs" | data99b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data99b$Source.Sectors_11=="Human Rights IGOs" | data99b$Source.Sectors_11=="Global Human Rights IGOs" | data99b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data99b$Source.Sectors_12=="Human Rights IGOs" | data99b$Source.Sectors_12=="Global Human Rights IGOs" | data99b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data99b$Source.Sectors_13=="Human Rights IGOs" | data99b$Source.Sectors_13=="Global Human Rights IGOs" | data99b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data99b$Source.Sectors_14=="Human Rights IGOs" | data99b$Source.Sectors_14=="Global Human Rights IGOs" | data99b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data99b$Source.Sectors_15=="Human Rights IGOs" | data99b$Source.Sectors_15=="Global Human Rights IGOs" | data99b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data99b$Source.Sectors_16=="Human Rights IGOs" | data99b$Source.Sectors_16=="Global Human Rights IGOs" | data99b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data99b$Source.Sectors_17=="Human Rights IGOs" | data99b$Source.Sectors_17=="Global Human Rights IGOs" | data99b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data99d<-data99c
data99d$Appeal_Jud_Coop<-ifelse(data99d$CAMEO.Code=="213", 1, NA)
data99d$Appeal_Change_Leadership<-ifelse(data99d$CAMEO.Code=="241", 1, NA)
data99d$Appeal_Policy_Change<-ifelse(data99d$CAMEO.Code=="242", 1, NA)
data99d$Appeal_Rights<-ifelse(data99d$CAMEO.Code=="243", 1, NA)
data99d$Appeal_Change_Inst<-ifelse(data99d$CAMEO.Code=="244", 1, NA)
data99d$Appeal_Release<-ifelse(data99d$CAMEO.Code=="253", 1, NA)
data99d$Demand<-ifelse(data99d$CAMEO.Code=="100", 1, NA)
data99d$Demand_Change_Leadership<-ifelse(data99d$CAMEO.Code=="1041", 1, NA) 
data99d$Demand_Policy_Change<-ifelse(data99d$CAMEO.Code=="1042", 1, NA)
data99d$Demand_Rights<-ifelse(data99d$CAMEO.Code=="1043", 1, NA)
data99d$Demand_Change_Inst<-ifelse(data99d$CAMEO.Code=="1044", 1, NA)
data99d$Accuse_HR<-ifelse(data99d$CAMEO.Code=="1122", 1, NA)
data99d$Accuse<-ifelse(data99d$CAMEO.Code=="112", 1, NA)
data99d$Criticize<-ifelse(data99d$CAMEO.Code=="111", 1, NA)
data99d$Investigate_War_Crimes<-ifelse(data99d$CAMEO.Code=="94", 1, NA)
data99d$Investigate_HR<-ifelse(data99d$CAMEO.Code=="92", 1, NA)
data99d$Demand_Jud_Coop<-ifelse(data99d$CAMEO.Code=="1013", 1, NA)
data99d$Demand_Hum_Aid<-ifelse(data99d$CAMEO.Code=="1033", 1, NA)
data99d$Demand_Release<-ifelse(data99d$CAMEO.Code=="1053", 1, NA)
data99d$Accuse_War_Crimes<-ifelse(data99d$CAMEO.Code=="1124", 1, NA)



data99e<-separate(data99d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts1999<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data99e)



write.csv(event.counts1999, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts1999.cvs") 



#_____________________

Events00 <- read.csv2("/Users/KimFruge/Desktop/Projects/Event2000.tab",  sep="\t", header=TRUE)

data00a<-separate(Events00, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data00<-separate(data00a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data00b<- subset(data00,
                 (data00$Target.Sectors_1=="Government" | data00$Target.Sectors_1=="Executive" | data00$Target.Sectors_1=="Police" | data00$Target.Sectors_1=="Military" | data00$Target.Sectors_1=="Legislative / Parliamentarty" | data00$Target.Sectors_1=="Judicial" | data00$Target.Sectors_1=="Local" | data00$Target.Sectors_1=="Parties") |
                         (data00$Target.Sectors_2=="Government" | data00$Target.Sectors_2=="Executive" | data00$Target.Sectors_2=="Police" | data00$Target.Sectors_2=="Military" | data00$Target.Sectors_2=="Legislative / Parliamentarty" | data00$Target.Sectors_2=="Judicial" | data00$Target.Sectors_2=="Local" | data00$Target.Sectors_2=="Parties") |
                         (data00$Target.Sectors_3=="Government" | data00$Target.Sectors_3=="Executive" | data00$Target.Sectors_3=="Police" | data00$Target.Sectors_3=="Military" | data00$Target.Sectors_3=="Legislative / Parliamentarty" | data00$Target.Sectors_3=="Judicial" | data00$Target.Sectors_3=="Local" | data00$Target.Sectors_3=="Parties") |
                         (data00$Target.Sectors_4=="Government" | data00$Target.Sectors_4=="Executive" | data00$Target.Sectors_4=="Police" | data00$Target.Sectors_4=="Military" | data00$Target.Sectors_4=="Legislative / Parliamentarty" | data00$Target.Sectors_4=="Judicial" | data00$Target.Sectors_4=="Local" | data00$Target.Sectors_4=="Parties") |
                         (data00$Target.Sectors_5=="Government" | data00$Target.Sectors_5=="Executive" | data00$Target.Sectors_5=="Police" | data00$Target.Sectors_5=="Military" | data00$Target.Sectors_5=="Legislative / Parliamentarty" | data00$Target.Sectors_5=="Judicial" | data00$Target.Sectors_5=="Local" | data00$Target.Sectors_5=="Parties") |
                         (data00$Target.Sectors_6=="Government" | data00$Target.Sectors_6=="Executive" | data00$Target.Sectors_6=="Police" | data00$Target.Sectors_6=="Military" | data00$Target.Sectors_6=="Legislative / Parliamentarty" | data00$Target.Sectors_6=="Judicial" | data00$Target.Sectors_6=="Local" | data00$Target.Sectors_6=="Parties") | 
                         (data00$Target.Sectors_7=="Government" | data00$Target.Sectors_7=="Executive" | data00$Target.Sectors_7=="Police" | data00$Target.Sectors_7=="Military" | data00$Target.Sectors_7=="Legislative / Parliamentarty" | data00$Target.Sectors_7=="Judicial" | data00$Target.Sectors_7=="Local" | data00$Target.Sectors_7=="Parties") |
                         (data00$Target.Sectors_8=="Government" | data00$Target.Sectors_8=="Executive" | data00$Target.Sectors_8=="Police" | data00$Target.Sectors_8=="Military" | data00$Target.Sectors_8=="Legislative / Parliamentarty" | data00$Target.Sectors_8=="Judicial" | data00$Target.Sectors_8=="Local" | data00$Target.Sectors_8=="Parties") | 
                         (data00$Target.Sectors_9=="Government" | data00$Target.Sectors_9=="Executive" | data00$Target.Sectors_9=="Police" | data00$Target.Sectors_9=="Military" | data00$Target.Sectors_9=="Legislative / Parliamentarty" | data00$Target.Sectors_9=="Judicial" | data00$Target.Sectors_9=="Local" | data00$Target.Sectors_9=="Parties") |
                         (data00$Target.Sectors_10=="Government" | data00$Target.Sectors_10=="Executive" | data00$Target.Sectors_10=="Police" | data00$Target.Sectors_10=="Military" | data00$Target.Sectors_10=="Legislative / Parliamentarty" | data00$Target.Sectors_10=="Judicial" | data00$Target.Sectors_10=="Local" | data00$Target.Sectors_10=="Parties") |
                         (data00$Target.Sectors_11=="Government" | data00$Target.Sectors_11=="Executive" | data00$Target.Sectors_11=="Police" | data00$Target.Sectors_11=="Military" | data00$Target.Sectors_11=="Legislative / Parliamentarty" | data00$Target.Sectors_11=="Judicial" | data00$Target.Sectors_11=="Local" | data00$Target.Sectors_11=="Parties") |
                         (data00$Target.Sectors_12=="Government" | data00$Target.Sectors_12=="Executive" | data00$Target.Sectors_12=="Police" | data00$Target.Sectors_12=="Military" | data00$Target.Sectors_12=="Legislative / Parliamentarty" | data00$Target.Sectors_12=="Judicial" | data00$Target.Sectors_12=="Local" | data00$Target.Sectors_12=="Parties") | 
                         (data00$Target.Sectors_13=="Government" | data00$Target.Sectors_13=="Executive" | data00$Target.Sectors_13=="Police" | data00$Target.Sectors_13=="Military" | data00$Target.Sectors_13=="Legislative / Parliamentarty" | data00$Target.Sectors_13=="Judicial" | data00$Target.Sectors_13=="Local" | data00$Target.Sectors_13=="Parties") |
                         (data00$Target.Sectors_14=="Government" | data00$Target.Sectors_14=="Executive" | data00$Target.Sectors_14=="Police" | data00$Target.Sectors_14=="Military" | data00$Target.Sectors_14=="Legislative / Parliamentarty" | data00$Target.Sectors_14=="Judicial" | data00$Target.Sectors_14=="Local" | data00$Target.Sectors_14=="Parties") |
                         (data00$Target.Sectors_15=="Government" | data00$Target.Sectors_15=="Executive" | data00$Target.Sectors_15=="Police" | data00$Target.Sectors_15=="Military" | data00$Target.Sectors_15=="Legislative / Parliamentarty" | data00$Target.Sectors_15=="Judicial" | data00$Target.Sectors_15=="Local" | data00$Target.Sectors_15=="Parties") |
                         (data00$Target.Sectors_16=="Government" | data00$Target.Sectors_16=="Executive" | data00$Target.Sectors_16=="Police" | data00$Target.Sectors_16=="Military" | data00$Target.Sectors_16=="Legislative / Parliamentarty" | data00$Target.Sectors_16=="Judicial" | data00$Target.Sectors_16=="Local" | data00$Target.Sectors_16=="Parties") | 
                         (data00$Target.Sectors_17=="Government" | data00$Target.Sectors_17=="Executive" | data00$Target.Sectors_17=="Police" | data00$Target.Sectors_17=="Military" | data00$Target.Sectors_17=="Legislative / Parliamentarty" | data00$Target.Sectors_17=="Judicial" | data00$Target.Sectors_17=="Local" | data00$Target.Sectors_17=="Parties") 
)


data00c<-subset(data00b, 
                (data00b$Source.Sectors_1=="Human Rights IGOs" | data00b$Source.Sectors_1=="Global Human Rights IGOs" | data00b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data00b$Source.Sectors_2=="Human Rights IGOs" | data00b$Source.Sectors_2=="Global Human Rights IGOs" | data00b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data00b$Source.Sectors_3=="Human Rights IGOs" | data00b$Source.Sectors_3=="Global Human Rights IGOs" | data00b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data00b$Source.Sectors_4=="Human Rights IGOs" | data00b$Source.Sectors_4=="Global Human Rights IGOs" | data00b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data00b$Source.Sectors_5=="Human Rights IGOs" | data00b$Source.Sectors_5=="Global Human Rights IGOs" | data00b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data00b$Source.Sectors_6=="Human Rights IGOs" | data00b$Source.Sectors_6=="Global Human Rights IGOs" | data00b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data00b$Source.Sectors_7=="Human Rights IGOs" | data00b$Source.Sectors_7=="Global Human Rights IGOs" | data00b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data00b$Source.Sectors_8=="Human Rights IGOs" | data00b$Source.Sectors_8=="Global Human Rights IGOs" | data00b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data00b$Source.Sectors_9=="Human Rights IGOs" | data00b$Source.Sectors_9=="Global Human Rights IGOs" | data00b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data00b$Source.Sectors_10=="Human Rights IGOs" | data00b$Source.Sectors_10=="Global Human Rights IGOs" | data00b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data00b$Source.Sectors_11=="Human Rights IGOs" | data00b$Source.Sectors_11=="Global Human Rights IGOs" | data00b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data00b$Source.Sectors_12=="Human Rights IGOs" | data00b$Source.Sectors_12=="Global Human Rights IGOs" | data00b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data00b$Source.Sectors_13=="Human Rights IGOs" | data00b$Source.Sectors_13=="Global Human Rights IGOs" | data00b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data00b$Source.Sectors_14=="Human Rights IGOs" | data00b$Source.Sectors_14=="Global Human Rights IGOs" | data00b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data00b$Source.Sectors_15=="Human Rights IGOs" | data00b$Source.Sectors_15=="Global Human Rights IGOs" | data00b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data00b$Source.Sectors_16=="Human Rights IGOs" | data00b$Source.Sectors_16=="Global Human Rights IGOs" | data00b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data00b$Source.Sectors_17=="Human Rights IGOs" | data00b$Source.Sectors_17=="Global Human Rights IGOs" | data00b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data00d<-data00c
data00d$Appeal_Jud_Coop<-ifelse(data00d$CAMEO.Code=="213", 1, NA)
data00d$Appeal_Change_Leadership<-ifelse(data00d$CAMEO.Code=="241", 1, NA)
data00d$Appeal_Policy_Change<-ifelse(data00d$CAMEO.Code=="242", 1, NA)
data00d$Appeal_Rights<-ifelse(data00d$CAMEO.Code=="243", 1, NA)
data00d$Appeal_Change_Inst<-ifelse(data00d$CAMEO.Code=="244", 1, NA)
data00d$Appeal_Release<-ifelse(data00d$CAMEO.Code=="253", 1, NA)
data00d$Demand<-ifelse(data00d$CAMEO.Code=="100", 1, NA)
data00d$Demand_Change_Leadership<-ifelse(data00d$CAMEO.Code=="1041", 1, NA) 
data00d$Demand_Policy_Change<-ifelse(data00d$CAMEO.Code=="1042", 1, NA)
data00d$Demand_Rights<-ifelse(data00d$CAMEO.Code=="1043", 1, NA)
data00d$Demand_Change_Inst<-ifelse(data00d$CAMEO.Code=="1044", 1, NA)
data00d$Accuse_HR<-ifelse(data00d$CAMEO.Code=="1122", 1, NA)
data00d$Accuse<-ifelse(data00d$CAMEO.Code=="112", 1, NA)
data00d$Criticize<-ifelse(data00d$CAMEO.Code=="111", 1, NA)
data00d$Investigate_War_Crimes<-ifelse(data00d$CAMEO.Code=="94", 1, NA)
data00d$Investigate_HR<-ifelse(data00d$CAMEO.Code=="92", 1, NA)
data00d$Demand_Jud_Coop<-ifelse(data00d$CAMEO.Code=="1013", 1, NA)
data00d$Demand_Hum_Aid<-ifelse(data00d$CAMEO.Code=="1033", 1, NA)
data00d$Demand_Release<-ifelse(data00d$CAMEO.Code=="1053", 1, NA)
data00d$Accuse_War_Crimes<-ifelse(data00d$CAMEO.Code=="1124", 1, NA)



data00e<-separate(data00d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts2000<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data00e)



write.csv(event.counts2000, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts2000.cvs") 



#______________________


Events01<- read.csv2("/Users/KimFruge/Desktop/Projects/Events2001.tab",  sep="\t", header=TRUE)

data01a<-separate(Events01, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data01<-separate(data01a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data01b<- subset(data01,
                 (data01$Target.Sectors_1=="Government" | data01$Target.Sectors_1=="Executive" | data01$Target.Sectors_1=="Police" | data01$Target.Sectors_1=="Military" | data01$Target.Sectors_1=="Legislative / Parliamentarty" | data01$Target.Sectors_1=="Judicial" | data01$Target.Sectors_1=="Local" | data01$Target.Sectors_1=="Parties") |
                         (data01$Target.Sectors_2=="Government" | data01$Target.Sectors_2=="Executive" | data01$Target.Sectors_2=="Police" | data01$Target.Sectors_2=="Military" | data01$Target.Sectors_2=="Legislative / Parliamentarty" | data01$Target.Sectors_2=="Judicial" | data01$Target.Sectors_2=="Local" | data01$Target.Sectors_2=="Parties") |
                         (data01$Target.Sectors_3=="Government" | data01$Target.Sectors_3=="Executive" | data01$Target.Sectors_3=="Police" | data01$Target.Sectors_3=="Military" | data01$Target.Sectors_3=="Legislative / Parliamentarty" | data01$Target.Sectors_3=="Judicial" | data01$Target.Sectors_3=="Local" | data01$Target.Sectors_3=="Parties") |
                         (data01$Target.Sectors_4=="Government" | data01$Target.Sectors_4=="Executive" | data01$Target.Sectors_4=="Police" | data01$Target.Sectors_4=="Military" | data01$Target.Sectors_4=="Legislative / Parliamentarty" | data01$Target.Sectors_4=="Judicial" | data01$Target.Sectors_4=="Local" | data01$Target.Sectors_4=="Parties") |
                         (data01$Target.Sectors_5=="Government" | data01$Target.Sectors_5=="Executive" | data01$Target.Sectors_5=="Police" | data01$Target.Sectors_5=="Military" | data01$Target.Sectors_5=="Legislative / Parliamentarty" | data01$Target.Sectors_5=="Judicial" | data01$Target.Sectors_5=="Local" | data01$Target.Sectors_5=="Parties") |
                         (data01$Target.Sectors_6=="Government" | data01$Target.Sectors_6=="Executive" | data01$Target.Sectors_6=="Police" | data01$Target.Sectors_6=="Military" | data01$Target.Sectors_6=="Legislative / Parliamentarty" | data01$Target.Sectors_6=="Judicial" | data01$Target.Sectors_6=="Local" | data01$Target.Sectors_6=="Parties") | 
                         (data01$Target.Sectors_7=="Government" | data01$Target.Sectors_7=="Executive" | data01$Target.Sectors_7=="Police" | data01$Target.Sectors_7=="Military" | data01$Target.Sectors_7=="Legislative / Parliamentarty" | data01$Target.Sectors_7=="Judicial" | data01$Target.Sectors_7=="Local" | data01$Target.Sectors_7=="Parties") |
                         (data01$Target.Sectors_8=="Government" | data01$Target.Sectors_8=="Executive" | data01$Target.Sectors_8=="Police" | data01$Target.Sectors_8=="Military" | data01$Target.Sectors_8=="Legislative / Parliamentarty" | data01$Target.Sectors_8=="Judicial" | data01$Target.Sectors_8=="Local" | data01$Target.Sectors_8=="Parties") | 
                         (data01$Target.Sectors_9=="Government" | data01$Target.Sectors_9=="Executive" | data01$Target.Sectors_9=="Police" | data01$Target.Sectors_9=="Military" | data01$Target.Sectors_9=="Legislative / Parliamentarty" | data01$Target.Sectors_9=="Judicial" | data01$Target.Sectors_9=="Local" | data01$Target.Sectors_9=="Parties") |
                         (data01$Target.Sectors_10=="Government" | data01$Target.Sectors_10=="Executive" | data01$Target.Sectors_10=="Police" | data01$Target.Sectors_10=="Military" | data01$Target.Sectors_10=="Legislative / Parliamentarty" | data01$Target.Sectors_10=="Judicial" | data01$Target.Sectors_10=="Local" | data01$Target.Sectors_10=="Parties") |
                         (data01$Target.Sectors_11=="Government" | data01$Target.Sectors_11=="Executive" | data01$Target.Sectors_11=="Police" | data01$Target.Sectors_11=="Military" | data01$Target.Sectors_11=="Legislative / Parliamentarty" | data01$Target.Sectors_11=="Judicial" | data01$Target.Sectors_11=="Local" | data01$Target.Sectors_11=="Parties") |
                         (data01$Target.Sectors_12=="Government" | data01$Target.Sectors_12=="Executive" | data01$Target.Sectors_12=="Police" | data01$Target.Sectors_12=="Military" | data01$Target.Sectors_12=="Legislative / Parliamentarty" | data01$Target.Sectors_12=="Judicial" | data01$Target.Sectors_12=="Local" | data01$Target.Sectors_12=="Parties") | 
                         (data01$Target.Sectors_13=="Government" | data01$Target.Sectors_13=="Executive" | data01$Target.Sectors_13=="Police" | data01$Target.Sectors_13=="Military" | data01$Target.Sectors_13=="Legislative / Parliamentarty" | data01$Target.Sectors_13=="Judicial" | data01$Target.Sectors_13=="Local" | data01$Target.Sectors_13=="Parties") |
                         (data01$Target.Sectors_14=="Government" | data01$Target.Sectors_14=="Executive" | data01$Target.Sectors_14=="Police" | data01$Target.Sectors_14=="Military" | data01$Target.Sectors_14=="Legislative / Parliamentarty" | data01$Target.Sectors_14=="Judicial" | data01$Target.Sectors_14=="Local" | data01$Target.Sectors_14=="Parties") |
                         (data01$Target.Sectors_15=="Government" | data01$Target.Sectors_15=="Executive" | data01$Target.Sectors_15=="Police" | data01$Target.Sectors_15=="Military" | data01$Target.Sectors_15=="Legislative / Parliamentarty" | data01$Target.Sectors_15=="Judicial" | data01$Target.Sectors_15=="Local" | data01$Target.Sectors_15=="Parties") |
                         (data01$Target.Sectors_16=="Government" | data01$Target.Sectors_16=="Executive" | data01$Target.Sectors_16=="Police" | data01$Target.Sectors_16=="Military" | data01$Target.Sectors_16=="Legislative / Parliamentarty" | data01$Target.Sectors_16=="Judicial" | data01$Target.Sectors_16=="Local" | data01$Target.Sectors_16=="Parties") | 
                         (data01$Target.Sectors_17=="Government" | data01$Target.Sectors_17=="Executive" | data01$Target.Sectors_17=="Police" | data01$Target.Sectors_17=="Military" | data01$Target.Sectors_17=="Legislative / Parliamentarty" | data01$Target.Sectors_17=="Judicial" | data01$Target.Sectors_17=="Local" | data01$Target.Sectors_17=="Parties") 
)


data01c<-subset(data01b, 
                (data01b$Source.Sectors_1=="Human Rights IGOs" | data01b$Source.Sectors_1=="Global Human Rights IGOs" | data01b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data01b$Source.Sectors_2=="Human Rights IGOs" | data01b$Source.Sectors_2=="Global Human Rights IGOs" | data01b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data01b$Source.Sectors_3=="Human Rights IGOs" | data01b$Source.Sectors_3=="Global Human Rights IGOs" | data01b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data01b$Source.Sectors_4=="Human Rights IGOs" | data01b$Source.Sectors_4=="Global Human Rights IGOs" | data01b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data01b$Source.Sectors_5=="Human Rights IGOs" | data01b$Source.Sectors_5=="Global Human Rights IGOs" | data01b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data01b$Source.Sectors_6=="Human Rights IGOs" | data01b$Source.Sectors_6=="Global Human Rights IGOs" | data01b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data01b$Source.Sectors_7=="Human Rights IGOs" | data01b$Source.Sectors_7=="Global Human Rights IGOs" | data01b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data01b$Source.Sectors_8=="Human Rights IGOs" | data01b$Source.Sectors_8=="Global Human Rights IGOs" | data01b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data01b$Source.Sectors_9=="Human Rights IGOs" | data01b$Source.Sectors_9=="Global Human Rights IGOs" | data01b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data01b$Source.Sectors_10=="Human Rights IGOs" | data01b$Source.Sectors_10=="Global Human Rights IGOs" | data01b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data01b$Source.Sectors_11=="Human Rights IGOs" | data01b$Source.Sectors_11=="Global Human Rights IGOs" | data01b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data01b$Source.Sectors_12=="Human Rights IGOs" | data01b$Source.Sectors_12=="Global Human Rights IGOs" | data01b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data01b$Source.Sectors_13=="Human Rights IGOs" | data01b$Source.Sectors_13=="Global Human Rights IGOs" | data01b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data01b$Source.Sectors_14=="Human Rights IGOs" | data01b$Source.Sectors_14=="Global Human Rights IGOs" | data01b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data01b$Source.Sectors_15=="Human Rights IGOs" | data01b$Source.Sectors_15=="Global Human Rights IGOs" | data01b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data01b$Source.Sectors_16=="Human Rights IGOs" | data01b$Source.Sectors_16=="Global Human Rights IGOs" | data01b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data01b$Source.Sectors_17=="Human Rights IGOs" | data01b$Source.Sectors_17=="Global Human Rights IGOs" | data01b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data01d<-data01c
data01d$Appeal_Jud_Coop<-ifelse(data01d$CAMEO.Code=="213", 1, NA)
data01d$Appeal_Change_Leadership<-ifelse(data01d$CAMEO.Code=="241", 1, NA)
data01d$Appeal_Policy_Change<-ifelse(data01d$CAMEO.Code=="242", 1, NA)
data01d$Appeal_Rights<-ifelse(data01d$CAMEO.Code=="243", 1, NA)
data01d$Appeal_Change_Inst<-ifelse(data01d$CAMEO.Code=="244", 1, NA)
data01d$Appeal_Release<-ifelse(data01d$CAMEO.Code=="253", 1, NA)
data01d$Demand<-ifelse(data01d$CAMEO.Code=="100", 1, NA)
data01d$Demand_Change_Leadership<-ifelse(data01d$CAMEO.Code=="1041", 1, NA) 
data01d$Demand_Policy_Change<-ifelse(data01d$CAMEO.Code=="1042", 1, NA)
data01d$Demand_Rights<-ifelse(data01d$CAMEO.Code=="1043", 1, NA)
data01d$Demand_Change_Inst<-ifelse(data01d$CAMEO.Code=="1044", 1, NA)
data01d$Accuse_HR<-ifelse(data01d$CAMEO.Code=="1122", 1, NA)
data01d$Accuse<-ifelse(data01d$CAMEO.Code=="112", 1, NA)
data01d$Criticize<-ifelse(data01d$CAMEO.Code=="111", 1, NA)
data01d$Investigate_War_Crimes<-ifelse(data01d$CAMEO.Code=="94", 1, NA)
data01d$Investigate_HR<-ifelse(data01d$CAMEO.Code=="92", 1, NA)
data01d$Demand_Jud_Coop<-ifelse(data01d$CAMEO.Code=="1013", 1, NA)
data01d$Demand_Hum_Aid<-ifelse(data01d$CAMEO.Code=="1033", 1, NA)
data01d$Demand_Release<-ifelse(data01d$CAMEO.Code=="1053", 1, NA)
data01d$Accuse_War_Crimes<-ifelse(data01d$CAMEO.Code=="1124", 1, NA)



data01e<-separate(data01d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts2001<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data01e)



write.csv(event.counts2001, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts2001.cvs") 



#__________________________

Events02<- read.csv2("/Users/KimFruge/Desktop/Projects/Events2002.tab",  sep="\t", header=TRUE)

data02a<-separate(Events02, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data02<-separate(data02a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data02b<- subset(data02,
                 (data02$Target.Sectors_1=="Government" | data02$Target.Sectors_1=="Executive" | data02$Target.Sectors_1=="Police" | data02$Target.Sectors_1=="Military" | data02$Target.Sectors_1=="Legislative / Parliamentarty" | data02$Target.Sectors_1=="Judicial" | data02$Target.Sectors_1=="Local" | data02$Target.Sectors_1=="Parties") |
                         (data02$Target.Sectors_2=="Government" | data02$Target.Sectors_2=="Executive" | data02$Target.Sectors_2=="Police" | data02$Target.Sectors_2=="Military" | data02$Target.Sectors_2=="Legislative / Parliamentarty" | data02$Target.Sectors_2=="Judicial" | data02$Target.Sectors_2=="Local" | data02$Target.Sectors_2=="Parties") |
                         (data02$Target.Sectors_3=="Government" | data02$Target.Sectors_3=="Executive" | data02$Target.Sectors_3=="Police" | data02$Target.Sectors_3=="Military" | data02$Target.Sectors_3=="Legislative / Parliamentarty" | data02$Target.Sectors_3=="Judicial" | data02$Target.Sectors_3=="Local" | data02$Target.Sectors_3=="Parties") |
                         (data02$Target.Sectors_4=="Government" | data02$Target.Sectors_4=="Executive" | data02$Target.Sectors_4=="Police" | data02$Target.Sectors_4=="Military" | data02$Target.Sectors_4=="Legislative / Parliamentarty" | data02$Target.Sectors_4=="Judicial" | data02$Target.Sectors_4=="Local" | data02$Target.Sectors_4=="Parties") |
                         (data02$Target.Sectors_5=="Government" | data02$Target.Sectors_5=="Executive" | data02$Target.Sectors_5=="Police" | data02$Target.Sectors_5=="Military" | data02$Target.Sectors_5=="Legislative / Parliamentarty" | data02$Target.Sectors_5=="Judicial" | data02$Target.Sectors_5=="Local" | data02$Target.Sectors_5=="Parties") |
                         (data02$Target.Sectors_6=="Government" | data02$Target.Sectors_6=="Executive" | data02$Target.Sectors_6=="Police" | data02$Target.Sectors_6=="Military" | data02$Target.Sectors_6=="Legislative / Parliamentarty" | data02$Target.Sectors_6=="Judicial" | data02$Target.Sectors_6=="Local" | data02$Target.Sectors_6=="Parties") | 
                         (data02$Target.Sectors_7=="Government" | data02$Target.Sectors_7=="Executive" | data02$Target.Sectors_7=="Police" | data02$Target.Sectors_7=="Military" | data02$Target.Sectors_7=="Legislative / Parliamentarty" | data02$Target.Sectors_7=="Judicial" | data02$Target.Sectors_7=="Local" | data02$Target.Sectors_7=="Parties") |
                         (data02$Target.Sectors_8=="Government" | data02$Target.Sectors_8=="Executive" | data02$Target.Sectors_8=="Police" | data02$Target.Sectors_8=="Military" | data02$Target.Sectors_8=="Legislative / Parliamentarty" | data02$Target.Sectors_8=="Judicial" | data02$Target.Sectors_8=="Local" | data02$Target.Sectors_8=="Parties") | 
                         (data02$Target.Sectors_9=="Government" | data02$Target.Sectors_9=="Executive" | data02$Target.Sectors_9=="Police" | data02$Target.Sectors_9=="Military" | data02$Target.Sectors_9=="Legislative / Parliamentarty" | data02$Target.Sectors_9=="Judicial" | data02$Target.Sectors_9=="Local" | data02$Target.Sectors_9=="Parties") |
                         (data02$Target.Sectors_10=="Government" | data02$Target.Sectors_10=="Executive" | data02$Target.Sectors_10=="Police" | data02$Target.Sectors_10=="Military" | data02$Target.Sectors_10=="Legislative / Parliamentarty" | data02$Target.Sectors_10=="Judicial" | data02$Target.Sectors_10=="Local" | data02$Target.Sectors_10=="Parties") |
                         (data02$Target.Sectors_11=="Government" | data02$Target.Sectors_11=="Executive" | data02$Target.Sectors_11=="Police" | data02$Target.Sectors_11=="Military" | data02$Target.Sectors_11=="Legislative / Parliamentarty" | data02$Target.Sectors_11=="Judicial" | data02$Target.Sectors_11=="Local" | data02$Target.Sectors_11=="Parties") |
                         (data02$Target.Sectors_12=="Government" | data02$Target.Sectors_12=="Executive" | data02$Target.Sectors_12=="Police" | data02$Target.Sectors_12=="Military" | data02$Target.Sectors_12=="Legislative / Parliamentarty" | data02$Target.Sectors_12=="Judicial" | data02$Target.Sectors_12=="Local" | data02$Target.Sectors_12=="Parties") | 
                         (data02$Target.Sectors_13=="Government" | data02$Target.Sectors_13=="Executive" | data02$Target.Sectors_13=="Police" | data02$Target.Sectors_13=="Military" | data02$Target.Sectors_13=="Legislative / Parliamentarty" | data02$Target.Sectors_13=="Judicial" | data02$Target.Sectors_13=="Local" | data02$Target.Sectors_13=="Parties") |
                         (data02$Target.Sectors_14=="Government" | data02$Target.Sectors_14=="Executive" | data02$Target.Sectors_14=="Police" | data02$Target.Sectors_14=="Military" | data02$Target.Sectors_14=="Legislative / Parliamentarty" | data02$Target.Sectors_14=="Judicial" | data02$Target.Sectors_14=="Local" | data02$Target.Sectors_14=="Parties") |
                         (data02$Target.Sectors_15=="Government" | data02$Target.Sectors_15=="Executive" | data02$Target.Sectors_15=="Police" | data02$Target.Sectors_15=="Military" | data02$Target.Sectors_15=="Legislative / Parliamentarty" | data02$Target.Sectors_15=="Judicial" | data02$Target.Sectors_15=="Local" | data02$Target.Sectors_15=="Parties") |
                         (data02$Target.Sectors_16=="Government" | data02$Target.Sectors_16=="Executive" | data02$Target.Sectors_16=="Police" | data02$Target.Sectors_16=="Military" | data02$Target.Sectors_16=="Legislative / Parliamentarty" | data02$Target.Sectors_16=="Judicial" | data02$Target.Sectors_16=="Local" | data02$Target.Sectors_16=="Parties") | 
                         (data02$Target.Sectors_17=="Government" | data02$Target.Sectors_17=="Executive" | data02$Target.Sectors_17=="Police" | data02$Target.Sectors_17=="Military" | data02$Target.Sectors_17=="Legislative / Parliamentarty" | data02$Target.Sectors_17=="Judicial" | data02$Target.Sectors_17=="Local" | data02$Target.Sectors_17=="Parties") 
)


data02c<-subset(data02b, 
                (data02b$Source.Sectors_1=="Human Rights IGOs" | data02b$Source.Sectors_1=="Global Human Rights IGOs" | data02b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data02b$Source.Sectors_2=="Human Rights IGOs" | data02b$Source.Sectors_2=="Global Human Rights IGOs" | data02b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data02b$Source.Sectors_3=="Human Rights IGOs" | data02b$Source.Sectors_3=="Global Human Rights IGOs" | data02b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data02b$Source.Sectors_4=="Human Rights IGOs" | data02b$Source.Sectors_4=="Global Human Rights IGOs" | data02b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data02b$Source.Sectors_5=="Human Rights IGOs" | data02b$Source.Sectors_5=="Global Human Rights IGOs" | data02b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data02b$Source.Sectors_6=="Human Rights IGOs" | data02b$Source.Sectors_6=="Global Human Rights IGOs" | data02b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data02b$Source.Sectors_7=="Human Rights IGOs" | data02b$Source.Sectors_7=="Global Human Rights IGOs" | data02b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data02b$Source.Sectors_8=="Human Rights IGOs" | data02b$Source.Sectors_8=="Global Human Rights IGOs" | data02b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data02b$Source.Sectors_9=="Human Rights IGOs" | data02b$Source.Sectors_9=="Global Human Rights IGOs" | data02b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data02b$Source.Sectors_10=="Human Rights IGOs" | data02b$Source.Sectors_10=="Global Human Rights IGOs" | data02b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data02b$Source.Sectors_11=="Human Rights IGOs" | data02b$Source.Sectors_11=="Global Human Rights IGOs" | data02b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data02b$Source.Sectors_12=="Human Rights IGOs" | data02b$Source.Sectors_12=="Global Human Rights IGOs" | data02b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data02b$Source.Sectors_13=="Human Rights IGOs" | data02b$Source.Sectors_13=="Global Human Rights IGOs" | data02b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data02b$Source.Sectors_14=="Human Rights IGOs" | data02b$Source.Sectors_14=="Global Human Rights IGOs" | data02b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data02b$Source.Sectors_15=="Human Rights IGOs" | data02b$Source.Sectors_15=="Global Human Rights IGOs" | data02b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data02b$Source.Sectors_16=="Human Rights IGOs" | data02b$Source.Sectors_16=="Global Human Rights IGOs" | data02b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data02b$Source.Sectors_17=="Human Rights IGOs" | data02b$Source.Sectors_17=="Global Human Rights IGOs" | data02b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data02d<-data02c
data02d$Appeal_Jud_Coop<-ifelse(data02d$CAMEO.Code=="213", 1, NA)
data02d$Appeal_Change_Leadership<-ifelse(data02d$CAMEO.Code=="241", 1, NA)
data02d$Appeal_Policy_Change<-ifelse(data02d$CAMEO.Code=="242", 1, NA)
data02d$Appeal_Rights<-ifelse(data02d$CAMEO.Code=="243", 1, NA)
data02d$Appeal_Change_Inst<-ifelse(data02d$CAMEO.Code=="244", 1, NA)
data02d$Appeal_Release<-ifelse(data02d$CAMEO.Code=="253", 1, NA)
data02d$Demand<-ifelse(data02d$CAMEO.Code=="100", 1, NA)
data02d$Demand_Change_Leadership<-ifelse(data02d$CAMEO.Code=="1041", 1, NA) 
data02d$Demand_Policy_Change<-ifelse(data02d$CAMEO.Code=="1042", 1, NA)
data02d$Demand_Rights<-ifelse(data02d$CAMEO.Code=="1043", 1, NA)
data02d$Demand_Change_Inst<-ifelse(data02d$CAMEO.Code=="1044", 1, NA)
data02d$Accuse_HR<-ifelse(data02d$CAMEO.Code=="1122", 1, NA)
data02d$Accuse<-ifelse(data02d$CAMEO.Code=="112", 1, NA)
data02d$Criticize<-ifelse(data02d$CAMEO.Code=="111", 1, NA)
data02d$Investigate_War_Crimes<-ifelse(data02d$CAMEO.Code=="94", 1, NA)
data02d$Investigate_HR<-ifelse(data02d$CAMEO.Code=="92", 1, NA)
data02d$Demand_Jud_Coop<-ifelse(data02d$CAMEO.Code=="1013", 1, NA)
data02d$Demand_Hum_Aid<-ifelse(data02d$CAMEO.Code=="1033", 1, NA)
data02d$Demand_Release<-ifelse(data02d$CAMEO.Code=="1053", 1, NA)
data02d$Accuse_War_Crimes<-ifelse(data02d$CAMEO.Code=="1124", 1, NA)



data02e<-separate(data02d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts2002<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data02e)



write.csv(event.counts2002, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts2002.cvs") 



#________________________

Events03 <- read.csv2("/Users/KimFruge/Desktop/Projects/Events2003.tab",  sep="\t", header=TRUE)

data03a<-separate(Events03, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data03<-separate(data03a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data03b<- subset(data03,
                 (data03$Target.Sectors_1=="Government" | data03$Target.Sectors_1=="Executive" | data03$Target.Sectors_1=="Police" | data03$Target.Sectors_1=="Military" | data03$Target.Sectors_1=="Legislative / Parliamentarty" | data03$Target.Sectors_1=="Judicial" | data03$Target.Sectors_1=="Local" | data03$Target.Sectors_1=="Parties") |
                         (data03$Target.Sectors_2=="Government" | data03$Target.Sectors_2=="Executive" | data03$Target.Sectors_2=="Police" | data03$Target.Sectors_2=="Military" | data03$Target.Sectors_2=="Legislative / Parliamentarty" | data03$Target.Sectors_2=="Judicial" | data03$Target.Sectors_2=="Local" | data03$Target.Sectors_2=="Parties") |
                         (data03$Target.Sectors_3=="Government" | data03$Target.Sectors_3=="Executive" | data03$Target.Sectors_3=="Police" | data03$Target.Sectors_3=="Military" | data03$Target.Sectors_3=="Legislative / Parliamentarty" | data03$Target.Sectors_3=="Judicial" | data03$Target.Sectors_3=="Local" | data03$Target.Sectors_3=="Parties") |
                         (data03$Target.Sectors_4=="Government" | data03$Target.Sectors_4=="Executive" | data03$Target.Sectors_4=="Police" | data03$Target.Sectors_4=="Military" | data03$Target.Sectors_4=="Legislative / Parliamentarty" | data03$Target.Sectors_4=="Judicial" | data03$Target.Sectors_4=="Local" | data03$Target.Sectors_4=="Parties") |
                         (data03$Target.Sectors_5=="Government" | data03$Target.Sectors_5=="Executive" | data03$Target.Sectors_5=="Police" | data03$Target.Sectors_5=="Military" | data03$Target.Sectors_5=="Legislative / Parliamentarty" | data03$Target.Sectors_5=="Judicial" | data03$Target.Sectors_5=="Local" | data03$Target.Sectors_5=="Parties") |
                         (data03$Target.Sectors_6=="Government" | data03$Target.Sectors_6=="Executive" | data03$Target.Sectors_6=="Police" | data03$Target.Sectors_6=="Military" | data03$Target.Sectors_6=="Legislative / Parliamentarty" | data03$Target.Sectors_6=="Judicial" | data03$Target.Sectors_6=="Local" | data03$Target.Sectors_6=="Parties") | 
                         (data03$Target.Sectors_7=="Government" | data03$Target.Sectors_7=="Executive" | data03$Target.Sectors_7=="Police" | data03$Target.Sectors_7=="Military" | data03$Target.Sectors_7=="Legislative / Parliamentarty" | data03$Target.Sectors_7=="Judicial" | data03$Target.Sectors_7=="Local" | data03$Target.Sectors_7=="Parties") |
                         (data03$Target.Sectors_8=="Government" | data03$Target.Sectors_8=="Executive" | data03$Target.Sectors_8=="Police" | data03$Target.Sectors_8=="Military" | data03$Target.Sectors_8=="Legislative / Parliamentarty" | data03$Target.Sectors_8=="Judicial" | data03$Target.Sectors_8=="Local" | data03$Target.Sectors_8=="Parties") | 
                         (data03$Target.Sectors_9=="Government" | data03$Target.Sectors_9=="Executive" | data03$Target.Sectors_9=="Police" | data03$Target.Sectors_9=="Military" | data03$Target.Sectors_9=="Legislative / Parliamentarty" | data03$Target.Sectors_9=="Judicial" | data03$Target.Sectors_9=="Local" | data03$Target.Sectors_9=="Parties") |
                         (data03$Target.Sectors_10=="Government" | data03$Target.Sectors_10=="Executive" | data03$Target.Sectors_10=="Police" | data03$Target.Sectors_10=="Military" | data03$Target.Sectors_10=="Legislative / Parliamentarty" | data03$Target.Sectors_10=="Judicial" | data03$Target.Sectors_10=="Local" | data03$Target.Sectors_10=="Parties") |
                         (data03$Target.Sectors_11=="Government" | data03$Target.Sectors_11=="Executive" | data03$Target.Sectors_11=="Police" | data03$Target.Sectors_11=="Military" | data03$Target.Sectors_11=="Legislative / Parliamentarty" | data03$Target.Sectors_11=="Judicial" | data03$Target.Sectors_11=="Local" | data03$Target.Sectors_11=="Parties") |
                         (data03$Target.Sectors_12=="Government" | data03$Target.Sectors_12=="Executive" | data03$Target.Sectors_12=="Police" | data03$Target.Sectors_12=="Military" | data03$Target.Sectors_12=="Legislative / Parliamentarty" | data03$Target.Sectors_12=="Judicial" | data03$Target.Sectors_12=="Local" | data03$Target.Sectors_12=="Parties") | 
                         (data03$Target.Sectors_13=="Government" | data03$Target.Sectors_13=="Executive" | data03$Target.Sectors_13=="Police" | data03$Target.Sectors_13=="Military" | data03$Target.Sectors_13=="Legislative / Parliamentarty" | data03$Target.Sectors_13=="Judicial" | data03$Target.Sectors_13=="Local" | data03$Target.Sectors_13=="Parties") |
                         (data03$Target.Sectors_14=="Government" | data03$Target.Sectors_14=="Executive" | data03$Target.Sectors_14=="Police" | data03$Target.Sectors_14=="Military" | data03$Target.Sectors_14=="Legislative / Parliamentarty" | data03$Target.Sectors_14=="Judicial" | data03$Target.Sectors_14=="Local" | data03$Target.Sectors_14=="Parties") |
                         (data03$Target.Sectors_15=="Government" | data03$Target.Sectors_15=="Executive" | data03$Target.Sectors_15=="Police" | data03$Target.Sectors_15=="Military" | data03$Target.Sectors_15=="Legislative / Parliamentarty" | data03$Target.Sectors_15=="Judicial" | data03$Target.Sectors_15=="Local" | data03$Target.Sectors_15=="Parties") |
                         (data03$Target.Sectors_16=="Government" | data03$Target.Sectors_16=="Executive" | data03$Target.Sectors_16=="Police" | data03$Target.Sectors_16=="Military" | data03$Target.Sectors_16=="Legislative / Parliamentarty" | data03$Target.Sectors_16=="Judicial" | data03$Target.Sectors_16=="Local" | data03$Target.Sectors_16=="Parties") | 
                         (data03$Target.Sectors_17=="Government" | data03$Target.Sectors_17=="Executive" | data03$Target.Sectors_17=="Police" | data03$Target.Sectors_17=="Military" | data03$Target.Sectors_17=="Legislative / Parliamentarty" | data03$Target.Sectors_17=="Judicial" | data03$Target.Sectors_17=="Local" | data03$Target.Sectors_17=="Parties") 
)


data03c<-subset(data03b, 
                (data03b$Source.Sectors_1=="Human Rights IGOs" | data03b$Source.Sectors_1=="Global Human Rights IGOs" | data03b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data03b$Source.Sectors_2=="Human Rights IGOs" | data03b$Source.Sectors_2=="Global Human Rights IGOs" | data03b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data03b$Source.Sectors_3=="Human Rights IGOs" | data03b$Source.Sectors_3=="Global Human Rights IGOs" | data03b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data03b$Source.Sectors_4=="Human Rights IGOs" | data03b$Source.Sectors_4=="Global Human Rights IGOs" | data03b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data03b$Source.Sectors_5=="Human Rights IGOs" | data03b$Source.Sectors_5=="Global Human Rights IGOs" | data03b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data03b$Source.Sectors_6=="Human Rights IGOs" | data03b$Source.Sectors_6=="Global Human Rights IGOs" | data03b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data03b$Source.Sectors_7=="Human Rights IGOs" | data03b$Source.Sectors_7=="Global Human Rights IGOs" | data03b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data03b$Source.Sectors_8=="Human Rights IGOs" | data03b$Source.Sectors_8=="Global Human Rights IGOs" | data03b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data03b$Source.Sectors_9=="Human Rights IGOs" | data03b$Source.Sectors_9=="Global Human Rights IGOs" | data03b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data03b$Source.Sectors_10=="Human Rights IGOs" | data03b$Source.Sectors_10=="Global Human Rights IGOs" | data03b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data03b$Source.Sectors_11=="Human Rights IGOs" | data03b$Source.Sectors_11=="Global Human Rights IGOs" | data03b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data03b$Source.Sectors_12=="Human Rights IGOs" | data03b$Source.Sectors_12=="Global Human Rights IGOs" | data03b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data03b$Source.Sectors_13=="Human Rights IGOs" | data03b$Source.Sectors_13=="Global Human Rights IGOs" | data03b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data03b$Source.Sectors_14=="Human Rights IGOs" | data03b$Source.Sectors_14=="Global Human Rights IGOs" | data03b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data03b$Source.Sectors_15=="Human Rights IGOs" | data03b$Source.Sectors_15=="Global Human Rights IGOs" | data03b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data03b$Source.Sectors_16=="Human Rights IGOs" | data03b$Source.Sectors_16=="Global Human Rights IGOs" | data03b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data03b$Source.Sectors_17=="Human Rights IGOs" | data03b$Source.Sectors_17=="Global Human Rights IGOs" | data03b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data03d<-data03c
data03d$Appeal_Jud_Coop<-ifelse(data03d$CAMEO.Code=="213", 1, NA)
data03d$Appeal_Change_Leadership<-ifelse(data03d$CAMEO.Code=="241", 1, NA)
data03d$Appeal_Policy_Change<-ifelse(data03d$CAMEO.Code=="242", 1, NA)
data03d$Appeal_Rights<-ifelse(data03d$CAMEO.Code=="243", 1, NA)
data03d$Appeal_Change_Inst<-ifelse(data03d$CAMEO.Code=="244", 1, NA)
data03d$Appeal_Release<-ifelse(data03d$CAMEO.Code=="253", 1, NA)
data03d$Demand<-ifelse(data03d$CAMEO.Code=="100", 1, NA)
data03d$Demand_Change_Leadership<-ifelse(data03d$CAMEO.Code=="1041", 1, NA) 
data03d$Demand_Policy_Change<-ifelse(data03d$CAMEO.Code=="1042", 1, NA)
data03d$Demand_Rights<-ifelse(data03d$CAMEO.Code=="1043", 1, NA)
data03d$Demand_Change_Inst<-ifelse(data03d$CAMEO.Code=="1044", 1, NA)
data03d$Accuse_HR<-ifelse(data03d$CAMEO.Code=="1122", 1, NA)
data03d$Accuse<-ifelse(data03d$CAMEO.Code=="112", 1, NA)
data03d$Criticize<-ifelse(data03d$CAMEO.Code=="111", 1, NA)
data03d$Investigate_War_Crimes<-ifelse(data03d$CAMEO.Code=="94", 1, NA)
data03d$Investigate_HR<-ifelse(data03d$CAMEO.Code=="92", 1, NA)
data03d$Demand_Jud_Coop<-ifelse(data03d$CAMEO.Code=="1013", 1, NA)
data03d$Demand_Hum_Aid<-ifelse(data03d$CAMEO.Code=="1033", 1, NA)
data03d$Demand_Release<-ifelse(data03d$CAMEO.Code=="1053", 1, NA)
data03d$Accuse_War_Crimes<-ifelse(data03d$CAMEO.Code=="1124", 1, NA)



data03e<-separate(data03d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts2003<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data03e)



write.csv(event.counts2003, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts2003.cvs")
##2004

Events04 <- read.csv2("/Users/KimFruge/Desktop/Projects/Events2004.tab",  sep="\t", header=TRUE)

data04a<-separate(Events04, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data04<-separate(data04a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data04b<- subset(data04,
                 (data04$Target.Sectors_1=="Government" | data04$Target.Sectors_1=="Executive" | data04$Target.Sectors_1=="Police" | data04$Target.Sectors_1=="Military" | data04$Target.Sectors_1=="Legislative / Parliamentarty" | data04$Target.Sectors_1=="Judicial" | data04$Target.Sectors_1=="Local" | data04$Target.Sectors_1=="Parties") |
                         (data04$Target.Sectors_2=="Government" | data04$Target.Sectors_2=="Executive" | data04$Target.Sectors_2=="Police" | data04$Target.Sectors_2=="Military" | data04$Target.Sectors_2=="Legislative / Parliamentarty" | data04$Target.Sectors_2=="Judicial" | data04$Target.Sectors_2=="Local" | data04$Target.Sectors_2=="Parties") |
                         (data04$Target.Sectors_3=="Government" | data04$Target.Sectors_3=="Executive" | data04$Target.Sectors_3=="Police" | data04$Target.Sectors_3=="Military" | data04$Target.Sectors_3=="Legislative / Parliamentarty" | data04$Target.Sectors_3=="Judicial" | data04$Target.Sectors_3=="Local" | data04$Target.Sectors_3=="Parties") |
                         (data04$Target.Sectors_4=="Government" | data04$Target.Sectors_4=="Executive" | data04$Target.Sectors_4=="Police" | data04$Target.Sectors_4=="Military" | data04$Target.Sectors_4=="Legislative / Parliamentarty" | data04$Target.Sectors_4=="Judicial" | data04$Target.Sectors_4=="Local" | data04$Target.Sectors_4=="Parties") |
                         (data04$Target.Sectors_5=="Government" | data04$Target.Sectors_5=="Executive" | data04$Target.Sectors_5=="Police" | data04$Target.Sectors_5=="Military" | data04$Target.Sectors_5=="Legislative / Parliamentarty" | data04$Target.Sectors_5=="Judicial" | data04$Target.Sectors_5=="Local" | data04$Target.Sectors_5=="Parties") |
                         (data04$Target.Sectors_6=="Government" | data04$Target.Sectors_6=="Executive" | data04$Target.Sectors_6=="Police" | data04$Target.Sectors_6=="Military" | data04$Target.Sectors_6=="Legislative / Parliamentarty" | data04$Target.Sectors_6=="Judicial" | data04$Target.Sectors_6=="Local" | data04$Target.Sectors_6=="Parties") | 
                         (data04$Target.Sectors_7=="Government" | data04$Target.Sectors_7=="Executive" | data04$Target.Sectors_7=="Police" | data04$Target.Sectors_7=="Military" | data04$Target.Sectors_7=="Legislative / Parliamentarty" | data04$Target.Sectors_7=="Judicial" | data04$Target.Sectors_7=="Local" | data04$Target.Sectors_7=="Parties") |
                         (data04$Target.Sectors_8=="Government" | data04$Target.Sectors_8=="Executive" | data04$Target.Sectors_8=="Police" | data04$Target.Sectors_8=="Military" | data04$Target.Sectors_8=="Legislative / Parliamentarty" | data04$Target.Sectors_8=="Judicial" | data04$Target.Sectors_8=="Local" | data04$Target.Sectors_8=="Parties") | 
                         (data04$Target.Sectors_9=="Government" | data04$Target.Sectors_9=="Executive" | data04$Target.Sectors_9=="Police" | data04$Target.Sectors_9=="Military" | data04$Target.Sectors_9=="Legislative / Parliamentarty" | data04$Target.Sectors_9=="Judicial" | data04$Target.Sectors_9=="Local" | data04$Target.Sectors_9=="Parties") |
                         (data04$Target.Sectors_10=="Government" | data04$Target.Sectors_10=="Executive" | data04$Target.Sectors_10=="Police" | data04$Target.Sectors_10=="Military" | data04$Target.Sectors_10=="Legislative / Parliamentarty" | data04$Target.Sectors_10=="Judicial" | data04$Target.Sectors_10=="Local" | data04$Target.Sectors_10=="Parties") |
                         (data04$Target.Sectors_11=="Government" | data04$Target.Sectors_11=="Executive" | data04$Target.Sectors_11=="Police" | data04$Target.Sectors_11=="Military" | data04$Target.Sectors_11=="Legislative / Parliamentarty" | data04$Target.Sectors_11=="Judicial" | data04$Target.Sectors_11=="Local" | data04$Target.Sectors_11=="Parties") |
                         (data04$Target.Sectors_12=="Government" | data04$Target.Sectors_12=="Executive" | data04$Target.Sectors_12=="Police" | data04$Target.Sectors_12=="Military" | data04$Target.Sectors_12=="Legislative / Parliamentarty" | data04$Target.Sectors_12=="Judicial" | data04$Target.Sectors_12=="Local" | data04$Target.Sectors_12=="Parties") | 
                         (data04$Target.Sectors_13=="Government" | data04$Target.Sectors_13=="Executive" | data04$Target.Sectors_13=="Police" | data04$Target.Sectors_13=="Military" | data04$Target.Sectors_13=="Legislative / Parliamentarty" | data04$Target.Sectors_13=="Judicial" | data04$Target.Sectors_13=="Local" | data04$Target.Sectors_13=="Parties") |
                         (data04$Target.Sectors_14=="Government" | data04$Target.Sectors_14=="Executive" | data04$Target.Sectors_14=="Police" | data04$Target.Sectors_14=="Military" | data04$Target.Sectors_14=="Legislative / Parliamentarty" | data04$Target.Sectors_14=="Judicial" | data04$Target.Sectors_14=="Local" | data04$Target.Sectors_14=="Parties") |
                         (data04$Target.Sectors_15=="Government" | data04$Target.Sectors_15=="Executive" | data04$Target.Sectors_15=="Police" | data04$Target.Sectors_15=="Military" | data04$Target.Sectors_15=="Legislative / Parliamentarty" | data04$Target.Sectors_15=="Judicial" | data04$Target.Sectors_15=="Local" | data04$Target.Sectors_15=="Parties") |
                         (data04$Target.Sectors_16=="Government" | data04$Target.Sectors_16=="Executive" | data04$Target.Sectors_16=="Police" | data04$Target.Sectors_16=="Military" | data04$Target.Sectors_16=="Legislative / Parliamentarty" | data04$Target.Sectors_16=="Judicial" | data04$Target.Sectors_16=="Local" | data04$Target.Sectors_16=="Parties") | 
                         (data04$Target.Sectors_17=="Government" | data04$Target.Sectors_17=="Executive" | data04$Target.Sectors_17=="Police" | data04$Target.Sectors_17=="Military" | data04$Target.Sectors_17=="Legislative / Parliamentarty" | data04$Target.Sectors_17=="Judicial" | data04$Target.Sectors_17=="Local" | data04$Target.Sectors_17=="Parties") 
)


data04c<-subset(data04b, 
                (data04b$Source.Sectors_1=="Human Rights IGOs" | data04b$Source.Sectors_1=="Global Human Rights IGOs" | data04b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data04b$Source.Sectors_2=="Human Rights IGOs" | data04b$Source.Sectors_2=="Global Human Rights IGOs" | data04b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data04b$Source.Sectors_3=="Human Rights IGOs" | data04b$Source.Sectors_3=="Global Human Rights IGOs" | data04b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data04b$Source.Sectors_4=="Human Rights IGOs" | data04b$Source.Sectors_4=="Global Human Rights IGOs" | data04b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data04b$Source.Sectors_5=="Human Rights IGOs" | data04b$Source.Sectors_5=="Global Human Rights IGOs" | data04b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data04b$Source.Sectors_6=="Human Rights IGOs" | data04b$Source.Sectors_6=="Global Human Rights IGOs" | data04b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data04b$Source.Sectors_7=="Human Rights IGOs" | data04b$Source.Sectors_7=="Global Human Rights IGOs" | data04b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data04b$Source.Sectors_8=="Human Rights IGOs" | data04b$Source.Sectors_8=="Global Human Rights IGOs" | data04b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data04b$Source.Sectors_9=="Human Rights IGOs" | data04b$Source.Sectors_9=="Global Human Rights IGOs" | data04b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data04b$Source.Sectors_10=="Human Rights IGOs" | data04b$Source.Sectors_10=="Global Human Rights IGOs" | data04b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data04b$Source.Sectors_11=="Human Rights IGOs" | data04b$Source.Sectors_11=="Global Human Rights IGOs" | data04b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data04b$Source.Sectors_12=="Human Rights IGOs" | data04b$Source.Sectors_12=="Global Human Rights IGOs" | data04b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data04b$Source.Sectors_13=="Human Rights IGOs" | data04b$Source.Sectors_13=="Global Human Rights IGOs" | data04b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data04b$Source.Sectors_14=="Human Rights IGOs" | data04b$Source.Sectors_14=="Global Human Rights IGOs" | data04b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data04b$Source.Sectors_15=="Human Rights IGOs" | data04b$Source.Sectors_15=="Global Human Rights IGOs" | data04b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data04b$Source.Sectors_16=="Human Rights IGOs" | data04b$Source.Sectors_16=="Global Human Rights IGOs" | data04b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data04b$Source.Sectors_17=="Human Rights IGOs" | data04b$Source.Sectors_17=="Global Human Rights IGOs" | data04b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data04d<-data04c
data04d$Appeal_Jud_Coop<-ifelse(data04d$CAMEO.Code=="213", 1, NA)
data04d$Appeal_Change_Leadership<-ifelse(data04d$CAMEO.Code=="241", 1, NA)
data04d$Appeal_Policy_Change<-ifelse(data04d$CAMEO.Code=="242", 1, NA)
data04d$Appeal_Rights<-ifelse(data04d$CAMEO.Code=="243", 1, NA)
data04d$Appeal_Change_Inst<-ifelse(data04d$CAMEO.Code=="244", 1, NA)
data04d$Appeal_Release<-ifelse(data04d$CAMEO.Code=="253", 1, NA)
data04d$Demand<-ifelse(data04d$CAMEO.Code=="100", 1, NA)
data04d$Demand_Change_Leadership<-ifelse(data04d$CAMEO.Code=="1041", 1, NA) 
data04d$Demand_Policy_Change<-ifelse(data04d$CAMEO.Code=="1042", 1, NA)
data04d$Demand_Rights<-ifelse(data04d$CAMEO.Code=="1043", 1, NA)
data04d$Demand_Change_Inst<-ifelse(data04d$CAMEO.Code=="1044", 1, NA)
data04d$Accuse_HR<-ifelse(data04d$CAMEO.Code=="1122", 1, NA)
data04d$Accuse<-ifelse(data04d$CAMEO.Code=="112", 1, NA)
data04d$Criticize<-ifelse(data04d$CAMEO.Code=="111", 1, NA)
data04d$Investigate_War_Crimes<-ifelse(data04d$CAMEO.Code=="94", 1, NA)
data04d$Investigate_HR<-ifelse(data04d$CAMEO.Code=="92", 1, NA)
data04d$Demand_Jud_Coop<-ifelse(data04d$CAMEO.Code=="1013", 1, NA)
data04d$Demand_Hum_Aid<-ifelse(data04d$CAMEO.Code=="1033", 1, NA)
data04d$Demand_Release<-ifelse(data04d$CAMEO.Code=="1053", 1, NA)
data04d$Accuse_War_Crimes<-ifelse(data04d$CAMEO.Code=="1124", 1, NA)



data04e<-separate(data04d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts2004<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data04e)



write.csv(event.counts2004, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts2004.cvs")

##2005

Events05 <- read.csv2("/Users/KimFruge/Desktop/Projects/Events2005.tab",  sep="\t", header=TRUE)

data05a<-separate(Events05, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data05<-separate(data05a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data05b<- subset(data05,
                 (data05$Target.Sectors_1=="Government" | data05$Target.Sectors_1=="Executive" | data05$Target.Sectors_1=="Police" | data05$Target.Sectors_1=="Military" | data05$Target.Sectors_1=="Legislative / Parliamentarty" | data05$Target.Sectors_1=="Judicial" | data05$Target.Sectors_1=="Local" | data05$Target.Sectors_1=="Parties") |
                         (data05$Target.Sectors_2=="Government" | data05$Target.Sectors_2=="Executive" | data05$Target.Sectors_2=="Police" | data05$Target.Sectors_2=="Military" | data05$Target.Sectors_2=="Legislative / Parliamentarty" | data05$Target.Sectors_2=="Judicial" | data05$Target.Sectors_2=="Local" | data05$Target.Sectors_2=="Parties") |
                         (data05$Target.Sectors_3=="Government" | data05$Target.Sectors_3=="Executive" | data05$Target.Sectors_3=="Police" | data05$Target.Sectors_3=="Military" | data05$Target.Sectors_3=="Legislative / Parliamentarty" | data05$Target.Sectors_3=="Judicial" | data05$Target.Sectors_3=="Local" | data05$Target.Sectors_3=="Parties") |
                         (data05$Target.Sectors_4=="Government" | data05$Target.Sectors_4=="Executive" | data05$Target.Sectors_4=="Police" | data05$Target.Sectors_4=="Military" | data05$Target.Sectors_4=="Legislative / Parliamentarty" | data05$Target.Sectors_4=="Judicial" | data05$Target.Sectors_4=="Local" | data05$Target.Sectors_4=="Parties") |
                         (data05$Target.Sectors_5=="Government" | data05$Target.Sectors_5=="Executive" | data05$Target.Sectors_5=="Police" | data05$Target.Sectors_5=="Military" | data05$Target.Sectors_5=="Legislative / Parliamentarty" | data05$Target.Sectors_5=="Judicial" | data05$Target.Sectors_5=="Local" | data05$Target.Sectors_5=="Parties") |
                         (data05$Target.Sectors_6=="Government" | data05$Target.Sectors_6=="Executive" | data05$Target.Sectors_6=="Police" | data05$Target.Sectors_6=="Military" | data05$Target.Sectors_6=="Legislative / Parliamentarty" | data05$Target.Sectors_6=="Judicial" | data05$Target.Sectors_6=="Local" | data05$Target.Sectors_6=="Parties") | 
                         (data05$Target.Sectors_7=="Government" | data05$Target.Sectors_7=="Executive" | data05$Target.Sectors_7=="Police" | data05$Target.Sectors_7=="Military" | data05$Target.Sectors_7=="Legislative / Parliamentarty" | data05$Target.Sectors_7=="Judicial" | data05$Target.Sectors_7=="Local" | data05$Target.Sectors_7=="Parties") |
                         (data05$Target.Sectors_8=="Government" | data05$Target.Sectors_8=="Executive" | data05$Target.Sectors_8=="Police" | data05$Target.Sectors_8=="Military" | data05$Target.Sectors_8=="Legislative / Parliamentarty" | data05$Target.Sectors_8=="Judicial" | data05$Target.Sectors_8=="Local" | data05$Target.Sectors_8=="Parties") | 
                         (data05$Target.Sectors_9=="Government" | data05$Target.Sectors_9=="Executive" | data05$Target.Sectors_9=="Police" | data05$Target.Sectors_9=="Military" | data05$Target.Sectors_9=="Legislative / Parliamentarty" | data05$Target.Sectors_9=="Judicial" | data05$Target.Sectors_9=="Local" | data05$Target.Sectors_9=="Parties") |
                         (data05$Target.Sectors_10=="Government" | data05$Target.Sectors_10=="Executive" | data05$Target.Sectors_10=="Police" | data05$Target.Sectors_10=="Military" | data05$Target.Sectors_10=="Legislative / Parliamentarty" | data05$Target.Sectors_10=="Judicial" | data05$Target.Sectors_10=="Local" | data05$Target.Sectors_10=="Parties") |
                         (data05$Target.Sectors_11=="Government" | data05$Target.Sectors_11=="Executive" | data05$Target.Sectors_11=="Police" | data05$Target.Sectors_11=="Military" | data05$Target.Sectors_11=="Legislative / Parliamentarty" | data05$Target.Sectors_11=="Judicial" | data05$Target.Sectors_11=="Local" | data05$Target.Sectors_11=="Parties") |
                         (data05$Target.Sectors_12=="Government" | data05$Target.Sectors_12=="Executive" | data05$Target.Sectors_12=="Police" | data05$Target.Sectors_12=="Military" | data05$Target.Sectors_12=="Legislative / Parliamentarty" | data05$Target.Sectors_12=="Judicial" | data05$Target.Sectors_12=="Local" | data05$Target.Sectors_12=="Parties") | 
                         (data05$Target.Sectors_13=="Government" | data05$Target.Sectors_13=="Executive" | data05$Target.Sectors_13=="Police" | data05$Target.Sectors_13=="Military" | data05$Target.Sectors_13=="Legislative / Parliamentarty" | data05$Target.Sectors_13=="Judicial" | data05$Target.Sectors_13=="Local" | data05$Target.Sectors_13=="Parties") |
                         (data05$Target.Sectors_14=="Government" | data05$Target.Sectors_14=="Executive" | data05$Target.Sectors_14=="Police" | data05$Target.Sectors_14=="Military" | data05$Target.Sectors_14=="Legislative / Parliamentarty" | data05$Target.Sectors_14=="Judicial" | data05$Target.Sectors_14=="Local" | data05$Target.Sectors_14=="Parties") |
                         (data05$Target.Sectors_15=="Government" | data05$Target.Sectors_15=="Executive" | data05$Target.Sectors_15=="Police" | data05$Target.Sectors_15=="Military" | data05$Target.Sectors_15=="Legislative / Parliamentarty" | data05$Target.Sectors_15=="Judicial" | data05$Target.Sectors_15=="Local" | data05$Target.Sectors_15=="Parties") |
                         (data05$Target.Sectors_16=="Government" | data05$Target.Sectors_16=="Executive" | data05$Target.Sectors_16=="Police" | data05$Target.Sectors_16=="Military" | data05$Target.Sectors_16=="Legislative / Parliamentarty" | data05$Target.Sectors_16=="Judicial" | data05$Target.Sectors_16=="Local" | data05$Target.Sectors_16=="Parties") | 
                         (data05$Target.Sectors_17=="Government" | data05$Target.Sectors_17=="Executive" | data05$Target.Sectors_17=="Police" | data05$Target.Sectors_17=="Military" | data05$Target.Sectors_17=="Legislative / Parliamentarty" | data05$Target.Sectors_17=="Judicial" | data05$Target.Sectors_17=="Local" | data05$Target.Sectors_17=="Parties") 
)


data05c<-subset(data05b, 
                (data05b$Source.Sectors_1=="Human Rights IGOs" | data05b$Source.Sectors_1=="Global Human Rights IGOs" | data05b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data05b$Source.Sectors_2=="Human Rights IGOs" | data05b$Source.Sectors_2=="Global Human Rights IGOs" | data05b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data05b$Source.Sectors_3=="Human Rights IGOs" | data05b$Source.Sectors_3=="Global Human Rights IGOs" | data05b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data05b$Source.Sectors_4=="Human Rights IGOs" | data05b$Source.Sectors_4=="Global Human Rights IGOs" | data05b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data05b$Source.Sectors_5=="Human Rights IGOs" | data05b$Source.Sectors_5=="Global Human Rights IGOs" | data05b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data05b$Source.Sectors_6=="Human Rights IGOs" | data05b$Source.Sectors_6=="Global Human Rights IGOs" | data05b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data05b$Source.Sectors_7=="Human Rights IGOs" | data05b$Source.Sectors_7=="Global Human Rights IGOs" | data05b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data05b$Source.Sectors_8=="Human Rights IGOs" | data05b$Source.Sectors_8=="Global Human Rights IGOs" | data05b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data05b$Source.Sectors_9=="Human Rights IGOs" | data05b$Source.Sectors_9=="Global Human Rights IGOs" | data05b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data05b$Source.Sectors_10=="Human Rights IGOs" | data05b$Source.Sectors_10=="Global Human Rights IGOs" | data05b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data05b$Source.Sectors_11=="Human Rights IGOs" | data05b$Source.Sectors_11=="Global Human Rights IGOs" | data05b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data05b$Source.Sectors_12=="Human Rights IGOs" | data05b$Source.Sectors_12=="Global Human Rights IGOs" | data05b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data05b$Source.Sectors_13=="Human Rights IGOs" | data05b$Source.Sectors_13=="Global Human Rights IGOs" | data05b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data05b$Source.Sectors_14=="Human Rights IGOs" | data05b$Source.Sectors_14=="Global Human Rights IGOs" | data05b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data05b$Source.Sectors_15=="Human Rights IGOs" | data05b$Source.Sectors_15=="Global Human Rights IGOs" | data05b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data05b$Source.Sectors_16=="Human Rights IGOs" | data05b$Source.Sectors_16=="Global Human Rights IGOs" | data05b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data05b$Source.Sectors_17=="Human Rights IGOs" | data05b$Source.Sectors_17=="Global Human Rights IGOs" | data05b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data05d<-data05c
data05d$Appeal_Jud_Coop<-ifelse(data05d$CAMEO.Code=="213", 1, NA)
data05d$Appeal_Change_Leadership<-ifelse(data05d$CAMEO.Code=="241", 1, NA)
data05d$Appeal_Policy_Change<-ifelse(data05d$CAMEO.Code=="242", 1, NA)
data05d$Appeal_Rights<-ifelse(data05d$CAMEO.Code=="243", 1, NA)
data05d$Appeal_Change_Inst<-ifelse(data05d$CAMEO.Code=="244", 1, NA)
data05d$Appeal_Release<-ifelse(data05d$CAMEO.Code=="253", 1, NA)
data05d$Demand<-ifelse(data05d$CAMEO.Code=="100", 1, NA)
data05d$Demand_Change_Leadership<-ifelse(data05d$CAMEO.Code=="1041", 1, NA) 
data05d$Demand_Policy_Change<-ifelse(data05d$CAMEO.Code=="1042", 1, NA)
data05d$Demand_Rights<-ifelse(data05d$CAMEO.Code=="1043", 1, NA)
data05d$Demand_Change_Inst<-ifelse(data05d$CAMEO.Code=="1044", 1, NA)
data05d$Accuse_HR<-ifelse(data05d$CAMEO.Code=="1122", 1, NA)
data05d$Accuse<-ifelse(data05d$CAMEO.Code=="112", 1, NA)
data05d$Criticize<-ifelse(data05d$CAMEO.Code=="111", 1, NA)
data05d$Investigate_War_Crimes<-ifelse(data05d$CAMEO.Code=="94", 1, NA)
data05d$Investigate_HR<-ifelse(data05d$CAMEO.Code=="92", 1, NA)
data05d$Demand_Jud_Coop<-ifelse(data05d$CAMEO.Code=="1013", 1, NA)
data05d$Demand_Hum_Aid<-ifelse(data05d$CAMEO.Code=="1033", 1, NA)
data05d$Demand_Release<-ifelse(data05d$CAMEO.Code=="1053", 1, NA)
data05d$Accuse_War_Crimes<-ifelse(data05d$CAMEO.Code=="1124", 1, NA)



data05e<-separate(data05d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts2005<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data05e)



write.csv(event.counts2005, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts2005.cvs")

##2006

Events2006 <- read.csv2("/Users/KimFruge/Desktop/Projects/Events2006.tab",  sep="\t", header=TRUE)

data06a<-separate(Events2006, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data06<-separate(data06a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data06b<- subset(data06,
                 (data06$Target.Sectors_1=="Government" | data06$Target.Sectors_1=="Executive" | data06$Target.Sectors_1=="Police" | data06$Target.Sectors_1=="Military" | data06$Target.Sectors_1=="Legislative / Parliamentarty" | data06$Target.Sectors_1=="Judicial" | data06$Target.Sectors_1=="Local" | data06$Target.Sectors_1=="Parties") |
                         (data06$Target.Sectors_2=="Government" | data06$Target.Sectors_2=="Executive" | data06$Target.Sectors_2=="Police" | data06$Target.Sectors_2=="Military" | data06$Target.Sectors_2=="Legislative / Parliamentarty" | data06$Target.Sectors_2=="Judicial" | data06$Target.Sectors_2=="Local" | data06$Target.Sectors_2=="Parties") |
                         (data06$Target.Sectors_3=="Government" | data06$Target.Sectors_3=="Executive" | data06$Target.Sectors_3=="Police" | data06$Target.Sectors_3=="Military" | data06$Target.Sectors_3=="Legislative / Parliamentarty" | data06$Target.Sectors_3=="Judicial" | data06$Target.Sectors_3=="Local" | data06$Target.Sectors_3=="Parties") |
                         (data06$Target.Sectors_4=="Government" | data06$Target.Sectors_4=="Executive" | data06$Target.Sectors_4=="Police" | data06$Target.Sectors_4=="Military" | data06$Target.Sectors_4=="Legislative / Parliamentarty" | data06$Target.Sectors_4=="Judicial" | data06$Target.Sectors_4=="Local" | data06$Target.Sectors_4=="Parties") |
                         (data06$Target.Sectors_5=="Government" | data06$Target.Sectors_5=="Executive" | data06$Target.Sectors_5=="Police" | data06$Target.Sectors_5=="Military" | data06$Target.Sectors_5=="Legislative / Parliamentarty" | data06$Target.Sectors_5=="Judicial" | data06$Target.Sectors_5=="Local" | data06$Target.Sectors_5=="Parties") |
                         (data06$Target.Sectors_6=="Government" | data06$Target.Sectors_6=="Executive" | data06$Target.Sectors_6=="Police" | data06$Target.Sectors_6=="Military" | data06$Target.Sectors_6=="Legislative / Parliamentarty" | data06$Target.Sectors_6=="Judicial" | data06$Target.Sectors_6=="Local" | data06$Target.Sectors_6=="Parties") | 
                         (data06$Target.Sectors_7=="Government" | data06$Target.Sectors_7=="Executive" | data06$Target.Sectors_7=="Police" | data06$Target.Sectors_7=="Military" | data06$Target.Sectors_7=="Legislative / Parliamentarty" | data06$Target.Sectors_7=="Judicial" | data06$Target.Sectors_7=="Local" | data06$Target.Sectors_7=="Parties") |
                         (data06$Target.Sectors_8=="Government" | data06$Target.Sectors_8=="Executive" | data06$Target.Sectors_8=="Police" | data06$Target.Sectors_8=="Military" | data06$Target.Sectors_8=="Legislative / Parliamentarty" | data06$Target.Sectors_8=="Judicial" | data06$Target.Sectors_8=="Local" | data06$Target.Sectors_8=="Parties") | 
                         (data06$Target.Sectors_9=="Government" | data06$Target.Sectors_9=="Executive" | data06$Target.Sectors_9=="Police" | data06$Target.Sectors_9=="Military" | data06$Target.Sectors_9=="Legislative / Parliamentarty" | data06$Target.Sectors_9=="Judicial" | data06$Target.Sectors_9=="Local" | data06$Target.Sectors_9=="Parties") |
                         (data06$Target.Sectors_10=="Government" | data06$Target.Sectors_10=="Executive" | data06$Target.Sectors_10=="Police" | data06$Target.Sectors_10=="Military" | data06$Target.Sectors_10=="Legislative / Parliamentarty" | data06$Target.Sectors_10=="Judicial" | data06$Target.Sectors_10=="Local" | data06$Target.Sectors_10=="Parties") |
                         (data06$Target.Sectors_11=="Government" | data06$Target.Sectors_11=="Executive" | data06$Target.Sectors_11=="Police" | data06$Target.Sectors_11=="Military" | data06$Target.Sectors_11=="Legislative / Parliamentarty" | data06$Target.Sectors_11=="Judicial" | data06$Target.Sectors_11=="Local" | data06$Target.Sectors_11=="Parties") |
                         (data06$Target.Sectors_12=="Government" | data06$Target.Sectors_12=="Executive" | data06$Target.Sectors_12=="Police" | data06$Target.Sectors_12=="Military" | data06$Target.Sectors_12=="Legislative / Parliamentarty" | data06$Target.Sectors_12=="Judicial" | data06$Target.Sectors_12=="Local" | data06$Target.Sectors_12=="Parties") | 
                         (data06$Target.Sectors_13=="Government" | data06$Target.Sectors_13=="Executive" | data06$Target.Sectors_13=="Police" | data06$Target.Sectors_13=="Military" | data06$Target.Sectors_13=="Legislative / Parliamentarty" | data06$Target.Sectors_13=="Judicial" | data06$Target.Sectors_13=="Local" | data06$Target.Sectors_13=="Parties") |
                         (data06$Target.Sectors_14=="Government" | data06$Target.Sectors_14=="Executive" | data06$Target.Sectors_14=="Police" | data06$Target.Sectors_14=="Military" | data06$Target.Sectors_14=="Legislative / Parliamentarty" | data06$Target.Sectors_14=="Judicial" | data06$Target.Sectors_14=="Local" | data06$Target.Sectors_14=="Parties") |
                         (data06$Target.Sectors_15=="Government" | data06$Target.Sectors_15=="Executive" | data06$Target.Sectors_15=="Police" | data06$Target.Sectors_15=="Military" | data06$Target.Sectors_15=="Legislative / Parliamentarty" | data06$Target.Sectors_15=="Judicial" | data06$Target.Sectors_15=="Local" | data06$Target.Sectors_15=="Parties") |
                         (data06$Target.Sectors_16=="Government" | data06$Target.Sectors_16=="Executive" | data06$Target.Sectors_16=="Police" | data06$Target.Sectors_16=="Military" | data06$Target.Sectors_16=="Legislative / Parliamentarty" | data06$Target.Sectors_16=="Judicial" | data06$Target.Sectors_16=="Local" | data06$Target.Sectors_16=="Parties") | 
                         (data06$Target.Sectors_17=="Government" | data06$Target.Sectors_17=="Executive" | data06$Target.Sectors_17=="Police" | data06$Target.Sectors_17=="Military" | data06$Target.Sectors_17=="Legislative / Parliamentarty" | data06$Target.Sectors_17=="Judicial" | data06$Target.Sectors_17=="Local" | data06$Target.Sectors_17=="Parties") 
)


data06c<-subset(data06b, 
                (data06b$Source.Sectors_1=="Human Rights IGOs" | data06b$Source.Sectors_1=="Global Human Rights IGOs" | data06b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data06b$Source.Sectors_2=="Human Rights IGOs" | data06b$Source.Sectors_2=="Global Human Rights IGOs" | data06b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data06b$Source.Sectors_3=="Human Rights IGOs" | data06b$Source.Sectors_3=="Global Human Rights IGOs" | data06b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data06b$Source.Sectors_4=="Human Rights IGOs" | data06b$Source.Sectors_4=="Global Human Rights IGOs" | data06b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data06b$Source.Sectors_5=="Human Rights IGOs" | data06b$Source.Sectors_5=="Global Human Rights IGOs" | data06b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data06b$Source.Sectors_6=="Human Rights IGOs" | data06b$Source.Sectors_6=="Global Human Rights IGOs" | data06b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data06b$Source.Sectors_7=="Human Rights IGOs" | data06b$Source.Sectors_7=="Global Human Rights IGOs" | data06b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data06b$Source.Sectors_8=="Human Rights IGOs" | data06b$Source.Sectors_8=="Global Human Rights IGOs" | data06b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data06b$Source.Sectors_9=="Human Rights IGOs" | data06b$Source.Sectors_9=="Global Human Rights IGOs" | data06b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data06b$Source.Sectors_10=="Human Rights IGOs" | data06b$Source.Sectors_10=="Global Human Rights IGOs" | data06b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data06b$Source.Sectors_11=="Human Rights IGOs" | data06b$Source.Sectors_11=="Global Human Rights IGOs" | data06b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data06b$Source.Sectors_12=="Human Rights IGOs" | data06b$Source.Sectors_12=="Global Human Rights IGOs" | data06b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data06b$Source.Sectors_13=="Human Rights IGOs" | data06b$Source.Sectors_13=="Global Human Rights IGOs" | data06b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data06b$Source.Sectors_14=="Human Rights IGOs" | data06b$Source.Sectors_14=="Global Human Rights IGOs" | data06b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data06b$Source.Sectors_15=="Human Rights IGOs" | data06b$Source.Sectors_15=="Global Human Rights IGOs" | data06b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data06b$Source.Sectors_16=="Human Rights IGOs" | data06b$Source.Sectors_16=="Global Human Rights IGOs" | data06b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data06b$Source.Sectors_17=="Human Rights IGOs" | data06b$Source.Sectors_17=="Global Human Rights IGOs" | data06b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data06d<-data06c
data06d$Appeal_Jud_Coop<-ifelse(data06d$CAMEO.Code=="213", 1, NA)
data06d$Appeal_Change_Leadership<-ifelse(data06d$CAMEO.Code=="241", 1, NA)
data06d$Appeal_Policy_Change<-ifelse(data06d$CAMEO.Code=="242", 1, NA)
data06d$Appeal_Rights<-ifelse(data06d$CAMEO.Code=="243", 1, NA)
data06d$Appeal_Change_Inst<-ifelse(data06d$CAMEO.Code=="244", 1, NA)
data06d$Appeal_Release<-ifelse(data06d$CAMEO.Code=="253", 1, NA)
data06d$Demand<-ifelse(data06d$CAMEO.Code=="100", 1, NA)
data06d$Demand_Change_Leadership<-ifelse(data06d$CAMEO.Code=="1041", 1, NA) 
data06d$Demand_Policy_Change<-ifelse(data06d$CAMEO.Code=="1042", 1, NA)
data06d$Demand_Rights<-ifelse(data06d$CAMEO.Code=="1043", 1, NA)
data06d$Demand_Change_Inst<-ifelse(data06d$CAMEO.Code=="1044", 1, NA)
data06d$Accuse_HR<-ifelse(data06d$CAMEO.Code=="1122", 1, NA)
data06d$Accuse<-ifelse(data06d$CAMEO.Code=="112", 1, NA)
data06d$Criticize<-ifelse(data06d$CAMEO.Code=="111", 1, NA)
data06d$Investigate_War_Crimes<-ifelse(data06d$CAMEO.Code=="94", 1, NA)
data06d$Investigate_HR<-ifelse(data06d$CAMEO.Code=="92", 1, NA)
data06d$Demand_Jud_Coop<-ifelse(data06d$CAMEO.Code=="1013", 1, NA)
data06d$Demand_Hum_Aid<-ifelse(data06d$CAMEO.Code=="1033", 1, NA)
data06d$Demand_Release<-ifelse(data06d$CAMEO.Code=="1053", 1, NA)
data06d$Accuse_War_Crimes<-ifelse(data06d$CAMEO.Code=="1124", 1, NA)



data06e<-separate(data06d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts2006<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data06e)



write.csv(event.counts2006, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts2006.cvs")

##2007

Events2007 <- read.csv2("/Users/KimFruge/Desktop/Projects/Events2007.tab",  sep="\t", header=TRUE)

data07a<-separate(Events2007, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data07<-separate(data07a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data07b<- subset(data07,
                 (data07$Target.Sectors_1=="Government" | data07$Target.Sectors_1=="Executive" | data07$Target.Sectors_1=="Police" | data07$Target.Sectors_1=="Military" | data07$Target.Sectors_1=="Legislative / Parliamentarty" | data07$Target.Sectors_1=="Judicial" | data07$Target.Sectors_1=="Local" | data07$Target.Sectors_1=="Parties") |
                         (data07$Target.Sectors_2=="Government" | data07$Target.Sectors_2=="Executive" | data07$Target.Sectors_2=="Police" | data07$Target.Sectors_2=="Military" | data07$Target.Sectors_2=="Legislative / Parliamentarty" | data07$Target.Sectors_2=="Judicial" | data07$Target.Sectors_2=="Local" | data07$Target.Sectors_2=="Parties") |
                         (data07$Target.Sectors_3=="Government" | data07$Target.Sectors_3=="Executive" | data07$Target.Sectors_3=="Police" | data07$Target.Sectors_3=="Military" | data07$Target.Sectors_3=="Legislative / Parliamentarty" | data07$Target.Sectors_3=="Judicial" | data07$Target.Sectors_3=="Local" | data07$Target.Sectors_3=="Parties") |
                         (data07$Target.Sectors_4=="Government" | data07$Target.Sectors_4=="Executive" | data07$Target.Sectors_4=="Police" | data07$Target.Sectors_4=="Military" | data07$Target.Sectors_4=="Legislative / Parliamentarty" | data07$Target.Sectors_4=="Judicial" | data07$Target.Sectors_4=="Local" | data07$Target.Sectors_4=="Parties") |
                         (data07$Target.Sectors_5=="Government" | data07$Target.Sectors_5=="Executive" | data07$Target.Sectors_5=="Police" | data07$Target.Sectors_5=="Military" | data07$Target.Sectors_5=="Legislative / Parliamentarty" | data07$Target.Sectors_5=="Judicial" | data07$Target.Sectors_5=="Local" | data07$Target.Sectors_5=="Parties") |
                         (data07$Target.Sectors_6=="Government" | data07$Target.Sectors_6=="Executive" | data07$Target.Sectors_6=="Police" | data07$Target.Sectors_6=="Military" | data07$Target.Sectors_6=="Legislative / Parliamentarty" | data07$Target.Sectors_6=="Judicial" | data07$Target.Sectors_6=="Local" | data07$Target.Sectors_6=="Parties") | 
                         (data07$Target.Sectors_7=="Government" | data07$Target.Sectors_7=="Executive" | data07$Target.Sectors_7=="Police" | data07$Target.Sectors_7=="Military" | data07$Target.Sectors_7=="Legislative / Parliamentarty" | data07$Target.Sectors_7=="Judicial" | data07$Target.Sectors_7=="Local" | data07$Target.Sectors_7=="Parties") |
                         (data07$Target.Sectors_8=="Government" | data07$Target.Sectors_8=="Executive" | data07$Target.Sectors_8=="Police" | data07$Target.Sectors_8=="Military" | data07$Target.Sectors_8=="Legislative / Parliamentarty" | data07$Target.Sectors_8=="Judicial" | data07$Target.Sectors_8=="Local" | data07$Target.Sectors_8=="Parties") | 
                         (data07$Target.Sectors_9=="Government" | data07$Target.Sectors_9=="Executive" | data07$Target.Sectors_9=="Police" | data07$Target.Sectors_9=="Military" | data07$Target.Sectors_9=="Legislative / Parliamentarty" | data07$Target.Sectors_9=="Judicial" | data07$Target.Sectors_9=="Local" | data07$Target.Sectors_9=="Parties") |
                         (data07$Target.Sectors_10=="Government" | data07$Target.Sectors_10=="Executive" | data07$Target.Sectors_10=="Police" | data07$Target.Sectors_10=="Military" | data07$Target.Sectors_10=="Legislative / Parliamentarty" | data07$Target.Sectors_10=="Judicial" | data07$Target.Sectors_10=="Local" | data07$Target.Sectors_10=="Parties") |
                         (data07$Target.Sectors_11=="Government" | data07$Target.Sectors_11=="Executive" | data07$Target.Sectors_11=="Police" | data07$Target.Sectors_11=="Military" | data07$Target.Sectors_11=="Legislative / Parliamentarty" | data07$Target.Sectors_11=="Judicial" | data07$Target.Sectors_11=="Local" | data07$Target.Sectors_11=="Parties") |
                         (data07$Target.Sectors_12=="Government" | data07$Target.Sectors_12=="Executive" | data07$Target.Sectors_12=="Police" | data07$Target.Sectors_12=="Military" | data07$Target.Sectors_12=="Legislative / Parliamentarty" | data07$Target.Sectors_12=="Judicial" | data07$Target.Sectors_12=="Local" | data07$Target.Sectors_12=="Parties") | 
                         (data07$Target.Sectors_13=="Government" | data07$Target.Sectors_13=="Executive" | data07$Target.Sectors_13=="Police" | data07$Target.Sectors_13=="Military" | data07$Target.Sectors_13=="Legislative / Parliamentarty" | data07$Target.Sectors_13=="Judicial" | data07$Target.Sectors_13=="Local" | data07$Target.Sectors_13=="Parties") |
                         (data07$Target.Sectors_14=="Government" | data07$Target.Sectors_14=="Executive" | data07$Target.Sectors_14=="Police" | data07$Target.Sectors_14=="Military" | data07$Target.Sectors_14=="Legislative / Parliamentarty" | data07$Target.Sectors_14=="Judicial" | data07$Target.Sectors_14=="Local" | data07$Target.Sectors_14=="Parties") |
                         (data07$Target.Sectors_15=="Government" | data07$Target.Sectors_15=="Executive" | data07$Target.Sectors_15=="Police" | data07$Target.Sectors_15=="Military" | data07$Target.Sectors_15=="Legislative / Parliamentarty" | data07$Target.Sectors_15=="Judicial" | data07$Target.Sectors_15=="Local" | data07$Target.Sectors_15=="Parties") |
                         (data07$Target.Sectors_16=="Government" | data07$Target.Sectors_16=="Executive" | data07$Target.Sectors_16=="Police" | data07$Target.Sectors_16=="Military" | data07$Target.Sectors_16=="Legislative / Parliamentarty" | data07$Target.Sectors_16=="Judicial" | data07$Target.Sectors_16=="Local" | data07$Target.Sectors_16=="Parties") | 
                         (data07$Target.Sectors_17=="Government" | data07$Target.Sectors_17=="Executive" | data07$Target.Sectors_17=="Police" | data07$Target.Sectors_17=="Military" | data07$Target.Sectors_17=="Legislative / Parliamentarty" | data07$Target.Sectors_17=="Judicial" | data07$Target.Sectors_17=="Local" | data07$Target.Sectors_17=="Parties") 
)


data07c<-subset(data07b, 
                (data07b$Source.Sectors_1=="Human Rights IGOs" | data07b$Source.Sectors_1=="Global Human Rights IGOs" | data07b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data07b$Source.Sectors_2=="Human Rights IGOs" | data07b$Source.Sectors_2=="Global Human Rights IGOs" | data07b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data07b$Source.Sectors_3=="Human Rights IGOs" | data07b$Source.Sectors_3=="Global Human Rights IGOs" | data07b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data07b$Source.Sectors_4=="Human Rights IGOs" | data07b$Source.Sectors_4=="Global Human Rights IGOs" | data07b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data07b$Source.Sectors_5=="Human Rights IGOs" | data07b$Source.Sectors_5=="Global Human Rights IGOs" | data07b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data07b$Source.Sectors_6=="Human Rights IGOs" | data07b$Source.Sectors_6=="Global Human Rights IGOs" | data07b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data07b$Source.Sectors_7=="Human Rights IGOs" | data07b$Source.Sectors_7=="Global Human Rights IGOs" | data07b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data07b$Source.Sectors_8=="Human Rights IGOs" | data07b$Source.Sectors_8=="Global Human Rights IGOs" | data07b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data07b$Source.Sectors_9=="Human Rights IGOs" | data07b$Source.Sectors_9=="Global Human Rights IGOs" | data07b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data07b$Source.Sectors_10=="Human Rights IGOs" | data07b$Source.Sectors_10=="Global Human Rights IGOs" | data07b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data07b$Source.Sectors_11=="Human Rights IGOs" | data07b$Source.Sectors_11=="Global Human Rights IGOs" | data07b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data07b$Source.Sectors_12=="Human Rights IGOs" | data07b$Source.Sectors_12=="Global Human Rights IGOs" | data07b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data07b$Source.Sectors_13=="Human Rights IGOs" | data07b$Source.Sectors_13=="Global Human Rights IGOs" | data07b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data07b$Source.Sectors_14=="Human Rights IGOs" | data07b$Source.Sectors_14=="Global Human Rights IGOs" | data07b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data07b$Source.Sectors_15=="Human Rights IGOs" | data07b$Source.Sectors_15=="Global Human Rights IGOs" | data07b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data07b$Source.Sectors_16=="Human Rights IGOs" | data07b$Source.Sectors_16=="Global Human Rights IGOs" | data07b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data07b$Source.Sectors_17=="Human Rights IGOs" | data07b$Source.Sectors_17=="Global Human Rights IGOs" | data07b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data07d<-data07c
data07d$Appeal_Jud_Coop<-ifelse(data07d$CAMEO.Code=="213", 1, NA)
data07d$Appeal_Change_Leadership<-ifelse(data07d$CAMEO.Code=="241", 1, NA)
data07d$Appeal_Policy_Change<-ifelse(data07d$CAMEO.Code=="242", 1, NA)
data07d$Appeal_Rights<-ifelse(data07d$CAMEO.Code=="243", 1, NA)
data07d$Appeal_Change_Inst<-ifelse(data07d$CAMEO.Code=="244", 1, NA)
data07d$Appeal_Release<-ifelse(data07d$CAMEO.Code=="253", 1, NA)
data07d$Demand<-ifelse(data07d$CAMEO.Code=="100", 1, NA)
data07d$Demand_Change_Leadership<-ifelse(data07d$CAMEO.Code=="1041", 1, NA) 
data07d$Demand_Policy_Change<-ifelse(data07d$CAMEO.Code=="1042", 1, NA)
data07d$Demand_Rights<-ifelse(data07d$CAMEO.Code=="1043", 1, NA)
data07d$Demand_Change_Inst<-ifelse(data07d$CAMEO.Code=="1044", 1, NA)
data07d$Accuse_HR<-ifelse(data07d$CAMEO.Code=="1122", 1, NA)
data07d$Accuse<-ifelse(data07d$CAMEO.Code=="112", 1, NA)
data07d$Criticize<-ifelse(data07d$CAMEO.Code=="111", 1, NA)
data07d$Investigate_War_Crimes<-ifelse(data07d$CAMEO.Code=="94", 1, NA)
data07d$Investigate_HR<-ifelse(data07d$CAMEO.Code=="92", 1, NA)
data07d$Demand_Jud_Coop<-ifelse(data07d$CAMEO.Code=="1013", 1, NA)
data07d$Demand_Hum_Aid<-ifelse(data07d$CAMEO.Code=="1033", 1, NA)
data07d$Demand_Release<-ifelse(data07d$CAMEO.Code=="1053", 1, NA)
data07d$Accuse_War_Crimes<-ifelse(data07d$CAMEO.Code=="1124", 1, NA)



data07e<-separate(data07d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts2007<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data07e)



write.csv(event.counts2007, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts2007.cvs")

##2008

Events08 <- read.csv2("/Users/KimFruge/Desktop/Projects/Events2008.tab",  sep="\t", header=TRUE)

data08a<-separate(Events08, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data08<-separate(data08a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data08b<- subset(data08,
                 (data08$Target.Sectors_1=="Government" | data08$Target.Sectors_1=="Executive" | data08$Target.Sectors_1=="Police" | data08$Target.Sectors_1=="Military" | data08$Target.Sectors_1=="Legislative / Parliamentarty" | data08$Target.Sectors_1=="Judicial" | data08$Target.Sectors_1=="Local" | data08$Target.Sectors_1=="Parties") |
                         (data08$Target.Sectors_2=="Government" | data08$Target.Sectors_2=="Executive" | data08$Target.Sectors_2=="Police" | data08$Target.Sectors_2=="Military" | data08$Target.Sectors_2=="Legislative / Parliamentarty" | data08$Target.Sectors_2=="Judicial" | data08$Target.Sectors_2=="Local" | data08$Target.Sectors_2=="Parties") |
                         (data08$Target.Sectors_3=="Government" | data08$Target.Sectors_3=="Executive" | data08$Target.Sectors_3=="Police" | data08$Target.Sectors_3=="Military" | data08$Target.Sectors_3=="Legislative / Parliamentarty" | data08$Target.Sectors_3=="Judicial" | data08$Target.Sectors_3=="Local" | data08$Target.Sectors_3=="Parties") |
                         (data08$Target.Sectors_4=="Government" | data08$Target.Sectors_4=="Executive" | data08$Target.Sectors_4=="Police" | data08$Target.Sectors_4=="Military" | data08$Target.Sectors_4=="Legislative / Parliamentarty" | data08$Target.Sectors_4=="Judicial" | data08$Target.Sectors_4=="Local" | data08$Target.Sectors_4=="Parties") |
                         (data08$Target.Sectors_5=="Government" | data08$Target.Sectors_5=="Executive" | data08$Target.Sectors_5=="Police" | data08$Target.Sectors_5=="Military" | data08$Target.Sectors_5=="Legislative / Parliamentarty" | data08$Target.Sectors_5=="Judicial" | data08$Target.Sectors_5=="Local" | data08$Target.Sectors_5=="Parties") |
                         (data08$Target.Sectors_6=="Government" | data08$Target.Sectors_6=="Executive" | data08$Target.Sectors_6=="Police" | data08$Target.Sectors_6=="Military" | data08$Target.Sectors_6=="Legislative / Parliamentarty" | data08$Target.Sectors_6=="Judicial" | data08$Target.Sectors_6=="Local" | data08$Target.Sectors_6=="Parties") | 
                         (data08$Target.Sectors_7=="Government" | data08$Target.Sectors_7=="Executive" | data08$Target.Sectors_7=="Police" | data08$Target.Sectors_7=="Military" | data08$Target.Sectors_7=="Legislative / Parliamentarty" | data08$Target.Sectors_7=="Judicial" | data08$Target.Sectors_7=="Local" | data08$Target.Sectors_7=="Parties") |
                         (data08$Target.Sectors_8=="Government" | data08$Target.Sectors_8=="Executive" | data08$Target.Sectors_8=="Police" | data08$Target.Sectors_8=="Military" | data08$Target.Sectors_8=="Legislative / Parliamentarty" | data08$Target.Sectors_8=="Judicial" | data08$Target.Sectors_8=="Local" | data08$Target.Sectors_8=="Parties") | 
                         (data08$Target.Sectors_9=="Government" | data08$Target.Sectors_9=="Executive" | data08$Target.Sectors_9=="Police" | data08$Target.Sectors_9=="Military" | data08$Target.Sectors_9=="Legislative / Parliamentarty" | data08$Target.Sectors_9=="Judicial" | data08$Target.Sectors_9=="Local" | data08$Target.Sectors_9=="Parties") |
                         (data08$Target.Sectors_10=="Government" | data08$Target.Sectors_10=="Executive" | data08$Target.Sectors_10=="Police" | data08$Target.Sectors_10=="Military" | data08$Target.Sectors_10=="Legislative / Parliamentarty" | data08$Target.Sectors_10=="Judicial" | data08$Target.Sectors_10=="Local" | data08$Target.Sectors_10=="Parties") |
                         (data08$Target.Sectors_11=="Government" | data08$Target.Sectors_11=="Executive" | data08$Target.Sectors_11=="Police" | data08$Target.Sectors_11=="Military" | data08$Target.Sectors_11=="Legislative / Parliamentarty" | data08$Target.Sectors_11=="Judicial" | data08$Target.Sectors_11=="Local" | data08$Target.Sectors_11=="Parties") |
                         (data08$Target.Sectors_12=="Government" | data08$Target.Sectors_12=="Executive" | data08$Target.Sectors_12=="Police" | data08$Target.Sectors_12=="Military" | data08$Target.Sectors_12=="Legislative / Parliamentarty" | data08$Target.Sectors_12=="Judicial" | data08$Target.Sectors_12=="Local" | data08$Target.Sectors_12=="Parties") | 
                         (data08$Target.Sectors_13=="Government" | data08$Target.Sectors_13=="Executive" | data08$Target.Sectors_13=="Police" | data08$Target.Sectors_13=="Military" | data08$Target.Sectors_13=="Legislative / Parliamentarty" | data08$Target.Sectors_13=="Judicial" | data08$Target.Sectors_13=="Local" | data08$Target.Sectors_13=="Parties") |
                         (data08$Target.Sectors_14=="Government" | data08$Target.Sectors_14=="Executive" | data08$Target.Sectors_14=="Police" | data08$Target.Sectors_14=="Military" | data08$Target.Sectors_14=="Legislative / Parliamentarty" | data08$Target.Sectors_14=="Judicial" | data08$Target.Sectors_14=="Local" | data08$Target.Sectors_14=="Parties") |
                         (data08$Target.Sectors_15=="Government" | data08$Target.Sectors_15=="Executive" | data08$Target.Sectors_15=="Police" | data08$Target.Sectors_15=="Military" | data08$Target.Sectors_15=="Legislative / Parliamentarty" | data08$Target.Sectors_15=="Judicial" | data08$Target.Sectors_15=="Local" | data08$Target.Sectors_15=="Parties") |
                         (data08$Target.Sectors_16=="Government" | data08$Target.Sectors_16=="Executive" | data08$Target.Sectors_16=="Police" | data08$Target.Sectors_16=="Military" | data08$Target.Sectors_16=="Legislative / Parliamentarty" | data08$Target.Sectors_16=="Judicial" | data08$Target.Sectors_16=="Local" | data08$Target.Sectors_16=="Parties") | 
                         (data08$Target.Sectors_17=="Government" | data08$Target.Sectors_17=="Executive" | data08$Target.Sectors_17=="Police" | data08$Target.Sectors_17=="Military" | data08$Target.Sectors_17=="Legislative / Parliamentarty" | data08$Target.Sectors_17=="Judicial" | data08$Target.Sectors_17=="Local" | data08$Target.Sectors_17=="Parties") 
)


data08c<-subset(data08b, 
                (data08b$Source.Sectors_1=="Human Rights IGOs" | data08b$Source.Sectors_1=="Global Human Rights IGOs" | data08b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data08b$Source.Sectors_2=="Human Rights IGOs" | data08b$Source.Sectors_2=="Global Human Rights IGOs" | data08b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data08b$Source.Sectors_3=="Human Rights IGOs" | data08b$Source.Sectors_3=="Global Human Rights IGOs" | data08b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data08b$Source.Sectors_4=="Human Rights IGOs" | data08b$Source.Sectors_4=="Global Human Rights IGOs" | data08b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data08b$Source.Sectors_5=="Human Rights IGOs" | data08b$Source.Sectors_5=="Global Human Rights IGOs" | data08b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data08b$Source.Sectors_6=="Human Rights IGOs" | data08b$Source.Sectors_6=="Global Human Rights IGOs" | data08b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data08b$Source.Sectors_7=="Human Rights IGOs" | data08b$Source.Sectors_7=="Global Human Rights IGOs" | data08b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data08b$Source.Sectors_8=="Human Rights IGOs" | data08b$Source.Sectors_8=="Global Human Rights IGOs" | data08b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data08b$Source.Sectors_9=="Human Rights IGOs" | data08b$Source.Sectors_9=="Global Human Rights IGOs" | data08b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data08b$Source.Sectors_10=="Human Rights IGOs" | data08b$Source.Sectors_10=="Global Human Rights IGOs" | data08b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data08b$Source.Sectors_11=="Human Rights IGOs" | data08b$Source.Sectors_11=="Global Human Rights IGOs" | data08b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data08b$Source.Sectors_12=="Human Rights IGOs" | data08b$Source.Sectors_12=="Global Human Rights IGOs" | data08b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data08b$Source.Sectors_13=="Human Rights IGOs" | data08b$Source.Sectors_13=="Global Human Rights IGOs" | data08b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data08b$Source.Sectors_14=="Human Rights IGOs" | data08b$Source.Sectors_14=="Global Human Rights IGOs" | data08b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data08b$Source.Sectors_15=="Human Rights IGOs" | data08b$Source.Sectors_15=="Global Human Rights IGOs" | data08b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data08b$Source.Sectors_16=="Human Rights IGOs" | data08b$Source.Sectors_16=="Global Human Rights IGOs" | data08b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data08b$Source.Sectors_17=="Human Rights IGOs" | data08b$Source.Sectors_17=="Global Human Rights IGOs" | data08b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data08d<-data08c
data08d$Appeal_Jud_Coop<-ifelse(data08d$CAMEO.Code=="213", 1, NA)
data08d$Appeal_Change_Leadership<-ifelse(data08d$CAMEO.Code=="241", 1, NA)
data08d$Appeal_Policy_Change<-ifelse(data08d$CAMEO.Code=="242", 1, NA)
data08d$Appeal_Rights<-ifelse(data08d$CAMEO.Code=="243", 1, NA)
data08d$Appeal_Change_Inst<-ifelse(data08d$CAMEO.Code=="244", 1, NA)
data08d$Appeal_Release<-ifelse(data08d$CAMEO.Code=="253", 1, NA)
data08d$Demand<-ifelse(data08d$CAMEO.Code=="100", 1, NA)
data08d$Demand_Change_Leadership<-ifelse(data08d$CAMEO.Code=="1041", 1, NA) 
data08d$Demand_Policy_Change<-ifelse(data08d$CAMEO.Code=="1042", 1, NA)
data08d$Demand_Rights<-ifelse(data08d$CAMEO.Code=="1043", 1, NA)
data08d$Demand_Change_Inst<-ifelse(data08d$CAMEO.Code=="1044", 1, NA)
data08d$Accuse_HR<-ifelse(data08d$CAMEO.Code=="1122", 1, NA)
data08d$Accuse<-ifelse(data08d$CAMEO.Code=="112", 1, NA)
data08d$Criticize<-ifelse(data08d$CAMEO.Code=="111", 1, NA)
data08d$Investigate_War_Crimes<-ifelse(data08d$CAMEO.Code=="94", 1, NA)
data08d$Investigate_HR<-ifelse(data08d$CAMEO.Code=="92", 1, NA)
data08d$Demand_Jud_Coop<-ifelse(data08d$CAMEO.Code=="1013", 1, NA)
data08d$Demand_Hum_Aid<-ifelse(data08d$CAMEO.Code=="1033", 1, NA)
data08d$Demand_Release<-ifelse(data08d$CAMEO.Code=="1053", 1, NA)
data08d$Accuse_War_Crimes<-ifelse(data08d$CAMEO.Code=="1124", 1, NA)



data08e<-separate(data08d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts2008<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data08e)



write.csv(event.counts2008, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts2008.cvs")

##2009

Events09 <- read.csv2("/Users/KimFruge/Desktop/Projects/Events2009.tab",  sep="\t", header=TRUE)

data09a<-separate(Events09, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data09<-separate(data09a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data09b<- subset(data09,
                 (data09$Target.Sectors_1=="Government" | data09$Target.Sectors_1=="Executive" | data09$Target.Sectors_1=="Police" | data09$Target.Sectors_1=="Military" | data09$Target.Sectors_1=="Legislative / Parliamentarty" | data09$Target.Sectors_1=="Judicial" | data09$Target.Sectors_1=="Local" | data09$Target.Sectors_1=="Parties") |
                         (data09$Target.Sectors_2=="Government" | data09$Target.Sectors_2=="Executive" | data09$Target.Sectors_2=="Police" | data09$Target.Sectors_2=="Military" | data09$Target.Sectors_2=="Legislative / Parliamentarty" | data09$Target.Sectors_2=="Judicial" | data09$Target.Sectors_2=="Local" | data09$Target.Sectors_2=="Parties") |
                         (data09$Target.Sectors_3=="Government" | data09$Target.Sectors_3=="Executive" | data09$Target.Sectors_3=="Police" | data09$Target.Sectors_3=="Military" | data09$Target.Sectors_3=="Legislative / Parliamentarty" | data09$Target.Sectors_3=="Judicial" | data09$Target.Sectors_3=="Local" | data09$Target.Sectors_3=="Parties") |
                         (data09$Target.Sectors_4=="Government" | data09$Target.Sectors_4=="Executive" | data09$Target.Sectors_4=="Police" | data09$Target.Sectors_4=="Military" | data09$Target.Sectors_4=="Legislative / Parliamentarty" | data09$Target.Sectors_4=="Judicial" | data09$Target.Sectors_4=="Local" | data09$Target.Sectors_4=="Parties") |
                         (data09$Target.Sectors_5=="Government" | data09$Target.Sectors_5=="Executive" | data09$Target.Sectors_5=="Police" | data09$Target.Sectors_5=="Military" | data09$Target.Sectors_5=="Legislative / Parliamentarty" | data09$Target.Sectors_5=="Judicial" | data09$Target.Sectors_5=="Local" | data09$Target.Sectors_5=="Parties") |
                         (data09$Target.Sectors_6=="Government" | data09$Target.Sectors_6=="Executive" | data09$Target.Sectors_6=="Police" | data09$Target.Sectors_6=="Military" | data09$Target.Sectors_6=="Legislative / Parliamentarty" | data09$Target.Sectors_6=="Judicial" | data09$Target.Sectors_6=="Local" | data09$Target.Sectors_6=="Parties") | 
                         (data09$Target.Sectors_7=="Government" | data09$Target.Sectors_7=="Executive" | data09$Target.Sectors_7=="Police" | data09$Target.Sectors_7=="Military" | data09$Target.Sectors_7=="Legislative / Parliamentarty" | data09$Target.Sectors_7=="Judicial" | data09$Target.Sectors_7=="Local" | data09$Target.Sectors_7=="Parties") |
                         (data09$Target.Sectors_8=="Government" | data09$Target.Sectors_8=="Executive" | data09$Target.Sectors_8=="Police" | data09$Target.Sectors_8=="Military" | data09$Target.Sectors_8=="Legislative / Parliamentarty" | data09$Target.Sectors_8=="Judicial" | data09$Target.Sectors_8=="Local" | data09$Target.Sectors_8=="Parties") | 
                         (data09$Target.Sectors_9=="Government" | data09$Target.Sectors_9=="Executive" | data09$Target.Sectors_9=="Police" | data09$Target.Sectors_9=="Military" | data09$Target.Sectors_9=="Legislative / Parliamentarty" | data09$Target.Sectors_9=="Judicial" | data09$Target.Sectors_9=="Local" | data09$Target.Sectors_9=="Parties") |
                         (data09$Target.Sectors_10=="Government" | data09$Target.Sectors_10=="Executive" | data09$Target.Sectors_10=="Police" | data09$Target.Sectors_10=="Military" | data09$Target.Sectors_10=="Legislative / Parliamentarty" | data09$Target.Sectors_10=="Judicial" | data09$Target.Sectors_10=="Local" | data09$Target.Sectors_10=="Parties") |
                         (data09$Target.Sectors_11=="Government" | data09$Target.Sectors_11=="Executive" | data09$Target.Sectors_11=="Police" | data09$Target.Sectors_11=="Military" | data09$Target.Sectors_11=="Legislative / Parliamentarty" | data09$Target.Sectors_11=="Judicial" | data09$Target.Sectors_11=="Local" | data09$Target.Sectors_11=="Parties") |
                         (data09$Target.Sectors_12=="Government" | data09$Target.Sectors_12=="Executive" | data09$Target.Sectors_12=="Police" | data09$Target.Sectors_12=="Military" | data09$Target.Sectors_12=="Legislative / Parliamentarty" | data09$Target.Sectors_12=="Judicial" | data09$Target.Sectors_12=="Local" | data09$Target.Sectors_12=="Parties") | 
                         (data09$Target.Sectors_13=="Government" | data09$Target.Sectors_13=="Executive" | data09$Target.Sectors_13=="Police" | data09$Target.Sectors_13=="Military" | data09$Target.Sectors_13=="Legislative / Parliamentarty" | data09$Target.Sectors_13=="Judicial" | data09$Target.Sectors_13=="Local" | data09$Target.Sectors_13=="Parties") |
                         (data09$Target.Sectors_14=="Government" | data09$Target.Sectors_14=="Executive" | data09$Target.Sectors_14=="Police" | data09$Target.Sectors_14=="Military" | data09$Target.Sectors_14=="Legislative / Parliamentarty" | data09$Target.Sectors_14=="Judicial" | data09$Target.Sectors_14=="Local" | data09$Target.Sectors_14=="Parties") |
                         (data09$Target.Sectors_15=="Government" | data09$Target.Sectors_15=="Executive" | data09$Target.Sectors_15=="Police" | data09$Target.Sectors_15=="Military" | data09$Target.Sectors_15=="Legislative / Parliamentarty" | data09$Target.Sectors_15=="Judicial" | data09$Target.Sectors_15=="Local" | data09$Target.Sectors_15=="Parties") |
                         (data09$Target.Sectors_16=="Government" | data09$Target.Sectors_16=="Executive" | data09$Target.Sectors_16=="Police" | data09$Target.Sectors_16=="Military" | data09$Target.Sectors_16=="Legislative / Parliamentarty" | data09$Target.Sectors_16=="Judicial" | data09$Target.Sectors_16=="Local" | data09$Target.Sectors_16=="Parties") | 
                         (data09$Target.Sectors_17=="Government" | data09$Target.Sectors_17=="Executive" | data09$Target.Sectors_17=="Police" | data09$Target.Sectors_17=="Military" | data09$Target.Sectors_17=="Legislative / Parliamentarty" | data09$Target.Sectors_17=="Judicial" | data09$Target.Sectors_17=="Local" | data09$Target.Sectors_17=="Parties") 
)


data09c<-subset(data09b, 
                (data09b$Source.Sectors_1=="Human Rights IGOs" | data09b$Source.Sectors_1=="Global Human Rights IGOs" | data09b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data09b$Source.Sectors_2=="Human Rights IGOs" | data09b$Source.Sectors_2=="Global Human Rights IGOs" | data09b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data09b$Source.Sectors_3=="Human Rights IGOs" | data09b$Source.Sectors_3=="Global Human Rights IGOs" | data09b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data09b$Source.Sectors_4=="Human Rights IGOs" | data09b$Source.Sectors_4=="Global Human Rights IGOs" | data09b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data09b$Source.Sectors_5=="Human Rights IGOs" | data09b$Source.Sectors_5=="Global Human Rights IGOs" | data09b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data09b$Source.Sectors_6=="Human Rights IGOs" | data09b$Source.Sectors_6=="Global Human Rights IGOs" | data09b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data09b$Source.Sectors_7=="Human Rights IGOs" | data09b$Source.Sectors_7=="Global Human Rights IGOs" | data09b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data09b$Source.Sectors_8=="Human Rights IGOs" | data09b$Source.Sectors_8=="Global Human Rights IGOs" | data09b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data09b$Source.Sectors_9=="Human Rights IGOs" | data09b$Source.Sectors_9=="Global Human Rights IGOs" | data09b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data09b$Source.Sectors_10=="Human Rights IGOs" | data09b$Source.Sectors_10=="Global Human Rights IGOs" | data09b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data09b$Source.Sectors_11=="Human Rights IGOs" | data09b$Source.Sectors_11=="Global Human Rights IGOs" | data09b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data09b$Source.Sectors_12=="Human Rights IGOs" | data09b$Source.Sectors_12=="Global Human Rights IGOs" | data09b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data09b$Source.Sectors_13=="Human Rights IGOs" | data09b$Source.Sectors_13=="Global Human Rights IGOs" | data09b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data09b$Source.Sectors_14=="Human Rights IGOs" | data09b$Source.Sectors_14=="Global Human Rights IGOs" | data09b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data09b$Source.Sectors_15=="Human Rights IGOs" | data09b$Source.Sectors_15=="Global Human Rights IGOs" | data09b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data09b$Source.Sectors_16=="Human Rights IGOs" | data09b$Source.Sectors_16=="Global Human Rights IGOs" | data09b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data09b$Source.Sectors_17=="Human Rights IGOs" | data09b$Source.Sectors_17=="Global Human Rights IGOs" | data09b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data09d<-data09c
data09d$Appeal_Jud_Coop<-ifelse(data09d$CAMEO.Code=="213", 1, NA)
data09d$Appeal_Change_Leadership<-ifelse(data09d$CAMEO.Code=="241", 1, NA)
data09d$Appeal_Policy_Change<-ifelse(data09d$CAMEO.Code=="242", 1, NA)
data09d$Appeal_Rights<-ifelse(data09d$CAMEO.Code=="243", 1, NA)
data09d$Appeal_Change_Inst<-ifelse(data09d$CAMEO.Code=="244", 1, NA)
data09d$Appeal_Release<-ifelse(data09d$CAMEO.Code=="253", 1, NA)
data09d$Demand<-ifelse(data09d$CAMEO.Code=="100", 1, NA)
data09d$Demand_Change_Leadership<-ifelse(data09d$CAMEO.Code=="1041", 1, NA) 
data09d$Demand_Policy_Change<-ifelse(data09d$CAMEO.Code=="1042", 1, NA)
data09d$Demand_Rights<-ifelse(data09d$CAMEO.Code=="1043", 1, NA)
data09d$Demand_Change_Inst<-ifelse(data09d$CAMEO.Code=="1044", 1, NA)
data09d$Accuse_HR<-ifelse(data09d$CAMEO.Code=="1122", 1, NA)
data09d$Accuse<-ifelse(data09d$CAMEO.Code=="112", 1, NA)
data09d$Criticize<-ifelse(data09d$CAMEO.Code=="111", 1, NA)
data09d$Investigate_War_Crimes<-ifelse(data09d$CAMEO.Code=="94", 1, NA)
data09d$Investigate_HR<-ifelse(data09d$CAMEO.Code=="92", 1, NA)
data09d$Demand_Jud_Coop<-ifelse(data09d$CAMEO.Code=="1013", 1, NA)
data09d$Demand_Hum_Aid<-ifelse(data09d$CAMEO.Code=="1033", 1, NA)
data09d$Demand_Release<-ifelse(data09d$CAMEO.Code=="1053", 1, NA)
data09d$Accuse_War_Crimes<-ifelse(data09d$CAMEO.Code=="1124", 1, NA)



data09e<-separate(data09d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts2009<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data09e)



write.csv(event.counts2009, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts2009.cvs")

##2010

Events10 <- read.csv2("/Users/KimFruge/Desktop/Projects/Events2010.tab",  sep="\t", header=TRUE)

data10a<-separate(Events10, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data10<-separate(data10a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data10b<- subset(data10,
                 (data10$Target.Sectors_1=="Government" | data10$Target.Sectors_1=="Executive" | data10$Target.Sectors_1=="Police" | data10$Target.Sectors_1=="Military" | data10$Target.Sectors_1=="Legislative / Parliamentarty" | data10$Target.Sectors_1=="Judicial" | data10$Target.Sectors_1=="Local" | data10$Target.Sectors_1=="Parties") |
                         (data10$Target.Sectors_2=="Government" | data10$Target.Sectors_2=="Executive" | data10$Target.Sectors_2=="Police" | data10$Target.Sectors_2=="Military" | data10$Target.Sectors_2=="Legislative / Parliamentarty" | data10$Target.Sectors_2=="Judicial" | data10$Target.Sectors_2=="Local" | data10$Target.Sectors_2=="Parties") |
                         (data10$Target.Sectors_3=="Government" | data10$Target.Sectors_3=="Executive" | data10$Target.Sectors_3=="Police" | data10$Target.Sectors_3=="Military" | data10$Target.Sectors_3=="Legislative / Parliamentarty" | data10$Target.Sectors_3=="Judicial" | data10$Target.Sectors_3=="Local" | data10$Target.Sectors_3=="Parties") |
                         (data10$Target.Sectors_4=="Government" | data10$Target.Sectors_4=="Executive" | data10$Target.Sectors_4=="Police" | data10$Target.Sectors_4=="Military" | data10$Target.Sectors_4=="Legislative / Parliamentarty" | data10$Target.Sectors_4=="Judicial" | data10$Target.Sectors_4=="Local" | data10$Target.Sectors_4=="Parties") |
                         (data10$Target.Sectors_5=="Government" | data10$Target.Sectors_5=="Executive" | data10$Target.Sectors_5=="Police" | data10$Target.Sectors_5=="Military" | data10$Target.Sectors_5=="Legislative / Parliamentarty" | data10$Target.Sectors_5=="Judicial" | data10$Target.Sectors_5=="Local" | data10$Target.Sectors_5=="Parties") |
                         (data10$Target.Sectors_6=="Government" | data10$Target.Sectors_6=="Executive" | data10$Target.Sectors_6=="Police" | data10$Target.Sectors_6=="Military" | data10$Target.Sectors_6=="Legislative / Parliamentarty" | data10$Target.Sectors_6=="Judicial" | data10$Target.Sectors_6=="Local" | data10$Target.Sectors_6=="Parties") | 
                         (data10$Target.Sectors_7=="Government" | data10$Target.Sectors_7=="Executive" | data10$Target.Sectors_7=="Police" | data10$Target.Sectors_7=="Military" | data10$Target.Sectors_7=="Legislative / Parliamentarty" | data10$Target.Sectors_7=="Judicial" | data10$Target.Sectors_7=="Local" | data10$Target.Sectors_7=="Parties") |
                         (data10$Target.Sectors_8=="Government" | data10$Target.Sectors_8=="Executive" | data10$Target.Sectors_8=="Police" | data10$Target.Sectors_8=="Military" | data10$Target.Sectors_8=="Legislative / Parliamentarty" | data10$Target.Sectors_8=="Judicial" | data10$Target.Sectors_8=="Local" | data10$Target.Sectors_8=="Parties") | 
                         (data10$Target.Sectors_9=="Government" | data10$Target.Sectors_9=="Executive" | data10$Target.Sectors_9=="Police" | data10$Target.Sectors_9=="Military" | data10$Target.Sectors_9=="Legislative / Parliamentarty" | data10$Target.Sectors_9=="Judicial" | data10$Target.Sectors_9=="Local" | data10$Target.Sectors_9=="Parties") |
                         (data10$Target.Sectors_10=="Government" | data10$Target.Sectors_10=="Executive" | data10$Target.Sectors_10=="Police" | data10$Target.Sectors_10=="Military" | data10$Target.Sectors_10=="Legislative / Parliamentarty" | data10$Target.Sectors_10=="Judicial" | data10$Target.Sectors_10=="Local" | data10$Target.Sectors_10=="Parties") |
                         (data10$Target.Sectors_11=="Government" | data10$Target.Sectors_11=="Executive" | data10$Target.Sectors_11=="Police" | data10$Target.Sectors_11=="Military" | data10$Target.Sectors_11=="Legislative / Parliamentarty" | data10$Target.Sectors_11=="Judicial" | data10$Target.Sectors_11=="Local" | data10$Target.Sectors_11=="Parties") |
                         (data10$Target.Sectors_12=="Government" | data10$Target.Sectors_12=="Executive" | data10$Target.Sectors_12=="Police" | data10$Target.Sectors_12=="Military" | data10$Target.Sectors_12=="Legislative / Parliamentarty" | data10$Target.Sectors_12=="Judicial" | data10$Target.Sectors_12=="Local" | data10$Target.Sectors_12=="Parties") | 
                         (data10$Target.Sectors_13=="Government" | data10$Target.Sectors_13=="Executive" | data10$Target.Sectors_13=="Police" | data10$Target.Sectors_13=="Military" | data10$Target.Sectors_13=="Legislative / Parliamentarty" | data10$Target.Sectors_13=="Judicial" | data10$Target.Sectors_13=="Local" | data10$Target.Sectors_13=="Parties") |
                         (data10$Target.Sectors_14=="Government" | data10$Target.Sectors_14=="Executive" | data10$Target.Sectors_14=="Police" | data10$Target.Sectors_14=="Military" | data10$Target.Sectors_14=="Legislative / Parliamentarty" | data10$Target.Sectors_14=="Judicial" | data10$Target.Sectors_14=="Local" | data10$Target.Sectors_14=="Parties") |
                         (data10$Target.Sectors_15=="Government" | data10$Target.Sectors_15=="Executive" | data10$Target.Sectors_15=="Police" | data10$Target.Sectors_15=="Military" | data10$Target.Sectors_15=="Legislative / Parliamentarty" | data10$Target.Sectors_15=="Judicial" | data10$Target.Sectors_15=="Local" | data10$Target.Sectors_15=="Parties") |
                         (data10$Target.Sectors_16=="Government" | data10$Target.Sectors_16=="Executive" | data10$Target.Sectors_16=="Police" | data10$Target.Sectors_16=="Military" | data10$Target.Sectors_16=="Legislative / Parliamentarty" | data10$Target.Sectors_16=="Judicial" | data10$Target.Sectors_16=="Local" | data10$Target.Sectors_16=="Parties") | 
                         (data10$Target.Sectors_17=="Government" | data10$Target.Sectors_17=="Executive" | data10$Target.Sectors_17=="Police" | data10$Target.Sectors_17=="Military" | data10$Target.Sectors_17=="Legislative / Parliamentarty" | data10$Target.Sectors_17=="Judicial" | data10$Target.Sectors_17=="Local" | data10$Target.Sectors_17=="Parties") 
)


data10c<-subset(data10b, 
                (data10b$Source.Sectors_1=="Human Rights IGOs" | data10b$Source.Sectors_1=="Global Human Rights IGOs" | data10b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data10b$Source.Sectors_2=="Human Rights IGOs" | data10b$Source.Sectors_2=="Global Human Rights IGOs" | data10b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data10b$Source.Sectors_3=="Human Rights IGOs" | data10b$Source.Sectors_3=="Global Human Rights IGOs" | data10b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data10b$Source.Sectors_4=="Human Rights IGOs" | data10b$Source.Sectors_4=="Global Human Rights IGOs" | data10b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data10b$Source.Sectors_5=="Human Rights IGOs" | data10b$Source.Sectors_5=="Global Human Rights IGOs" | data10b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data10b$Source.Sectors_6=="Human Rights IGOs" | data10b$Source.Sectors_6=="Global Human Rights IGOs" | data10b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data10b$Source.Sectors_7=="Human Rights IGOs" | data10b$Source.Sectors_7=="Global Human Rights IGOs" | data10b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data10b$Source.Sectors_8=="Human Rights IGOs" | data10b$Source.Sectors_8=="Global Human Rights IGOs" | data10b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data10b$Source.Sectors_9=="Human Rights IGOs" | data10b$Source.Sectors_9=="Global Human Rights IGOs" | data10b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data10b$Source.Sectors_10=="Human Rights IGOs" | data10b$Source.Sectors_10=="Global Human Rights IGOs" | data10b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data10b$Source.Sectors_11=="Human Rights IGOs" | data10b$Source.Sectors_11=="Global Human Rights IGOs" | data10b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data10b$Source.Sectors_12=="Human Rights IGOs" | data10b$Source.Sectors_12=="Global Human Rights IGOs" | data10b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data10b$Source.Sectors_13=="Human Rights IGOs" | data10b$Source.Sectors_13=="Global Human Rights IGOs" | data10b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data10b$Source.Sectors_14=="Human Rights IGOs" | data10b$Source.Sectors_14=="Global Human Rights IGOs" | data10b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data10b$Source.Sectors_15=="Human Rights IGOs" | data10b$Source.Sectors_15=="Global Human Rights IGOs" | data10b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data10b$Source.Sectors_16=="Human Rights IGOs" | data10b$Source.Sectors_16=="Global Human Rights IGOs" | data10b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data10b$Source.Sectors_17=="Human Rights IGOs" | data10b$Source.Sectors_17=="Global Human Rights IGOs" | data10b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data10d<-data10c
data10d$Appeal_Jud_Coop<-ifelse(data10d$CAMEO.Code=="213", 1, NA)
data10d$Appeal_Change_Leadership<-ifelse(data10d$CAMEO.Code=="241", 1, NA)
data10d$Appeal_Policy_Change<-ifelse(data10d$CAMEO.Code=="242", 1, NA)
data10d$Appeal_Rights<-ifelse(data10d$CAMEO.Code=="243", 1, NA)
data10d$Appeal_Change_Inst<-ifelse(data10d$CAMEO.Code=="244", 1, NA)
data10d$Appeal_Release<-ifelse(data10d$CAMEO.Code=="253", 1, NA)
data10d$Demand<-ifelse(data10d$CAMEO.Code=="100", 1, NA)
data10d$Demand_Change_Leadership<-ifelse(data10d$CAMEO.Code=="1041", 1, NA) 
data10d$Demand_Policy_Change<-ifelse(data10d$CAMEO.Code=="1042", 1, NA)
data10d$Demand_Rights<-ifelse(data10d$CAMEO.Code=="1043", 1, NA)
data10d$Demand_Change_Inst<-ifelse(data10d$CAMEO.Code=="1044", 1, NA)
data10d$Accuse_HR<-ifelse(data10d$CAMEO.Code=="1122", 1, NA)
data10d$Accuse<-ifelse(data10d$CAMEO.Code=="112", 1, NA)
data10d$Criticize<-ifelse(data10d$CAMEO.Code=="111", 1, NA)
data10d$Investigate_War_Crimes<-ifelse(data10d$CAMEO.Code=="94", 1, NA)
data10d$Investigate_HR<-ifelse(data10d$CAMEO.Code=="92", 1, NA)
data10d$Demand_Jud_Coop<-ifelse(data10d$CAMEO.Code=="1013", 1, NA)
data10d$Demand_Hum_Aid<-ifelse(data10d$CAMEO.Code=="1033", 1, NA)
data10d$Demand_Release<-ifelse(data10d$CAMEO.Code=="1053", 1, NA)
data10d$Accuse_War_Crimes<-ifelse(data10d$CAMEO.Code=="1124", 1, NA)



data10e<-separate(data10d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts2010<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data10e)



write.csv(event.counts2010, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts2010.cvs")

#2011

Events11 <- read.csv2("/Users/KimFruge/Desktop/Projects/Events2011.tab",  sep="\t", header=TRUE)

data11a<-separate(Events11, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data11<-separate(data11a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data11b<- subset(data11,
                 (data11$Target.Sectors_1=="Government" | data11$Target.Sectors_1=="Executive" | data11$Target.Sectors_1=="Police" | data11$Target.Sectors_1=="Military" | data11$Target.Sectors_1=="Legislative / Parliamentarty" | data11$Target.Sectors_1=="Judicial" | data11$Target.Sectors_1=="Local" | data11$Target.Sectors_1=="Parties") |
                         (data11$Target.Sectors_2=="Government" | data11$Target.Sectors_2=="Executive" | data11$Target.Sectors_2=="Police" | data11$Target.Sectors_2=="Military" | data11$Target.Sectors_2=="Legislative / Parliamentarty" | data11$Target.Sectors_2=="Judicial" | data11$Target.Sectors_2=="Local" | data11$Target.Sectors_2=="Parties") |
                         (data11$Target.Sectors_3=="Government" | data11$Target.Sectors_3=="Executive" | data11$Target.Sectors_3=="Police" | data11$Target.Sectors_3=="Military" | data11$Target.Sectors_3=="Legislative / Parliamentarty" | data11$Target.Sectors_3=="Judicial" | data11$Target.Sectors_3=="Local" | data11$Target.Sectors_3=="Parties") |
                         (data11$Target.Sectors_4=="Government" | data11$Target.Sectors_4=="Executive" | data11$Target.Sectors_4=="Police" | data11$Target.Sectors_4=="Military" | data11$Target.Sectors_4=="Legislative / Parliamentarty" | data11$Target.Sectors_4=="Judicial" | data11$Target.Sectors_4=="Local" | data11$Target.Sectors_4=="Parties") |
                         (data11$Target.Sectors_5=="Government" | data11$Target.Sectors_5=="Executive" | data11$Target.Sectors_5=="Police" | data11$Target.Sectors_5=="Military" | data11$Target.Sectors_5=="Legislative / Parliamentarty" | data11$Target.Sectors_5=="Judicial" | data11$Target.Sectors_5=="Local" | data11$Target.Sectors_5=="Parties") |
                         (data11$Target.Sectors_6=="Government" | data11$Target.Sectors_6=="Executive" | data11$Target.Sectors_6=="Police" | data11$Target.Sectors_6=="Military" | data11$Target.Sectors_6=="Legislative / Parliamentarty" | data11$Target.Sectors_6=="Judicial" | data11$Target.Sectors_6=="Local" | data11$Target.Sectors_6=="Parties") | 
                         (data11$Target.Sectors_7=="Government" | data11$Target.Sectors_7=="Executive" | data11$Target.Sectors_7=="Police" | data11$Target.Sectors_7=="Military" | data11$Target.Sectors_7=="Legislative / Parliamentarty" | data11$Target.Sectors_7=="Judicial" | data11$Target.Sectors_7=="Local" | data11$Target.Sectors_7=="Parties") |
                         (data11$Target.Sectors_8=="Government" | data11$Target.Sectors_8=="Executive" | data11$Target.Sectors_8=="Police" | data11$Target.Sectors_8=="Military" | data11$Target.Sectors_8=="Legislative / Parliamentarty" | data11$Target.Sectors_8=="Judicial" | data11$Target.Sectors_8=="Local" | data11$Target.Sectors_8=="Parties") | 
                         (data11$Target.Sectors_9=="Government" | data11$Target.Sectors_9=="Executive" | data11$Target.Sectors_9=="Police" | data11$Target.Sectors_9=="Military" | data11$Target.Sectors_9=="Legislative / Parliamentarty" | data11$Target.Sectors_9=="Judicial" | data11$Target.Sectors_9=="Local" | data11$Target.Sectors_9=="Parties") |
                         (data11$Target.Sectors_10=="Government" | data11$Target.Sectors_10=="Executive" | data11$Target.Sectors_10=="Police" | data11$Target.Sectors_10=="Military" | data11$Target.Sectors_10=="Legislative / Parliamentarty" | data11$Target.Sectors_10=="Judicial" | data11$Target.Sectors_10=="Local" | data11$Target.Sectors_10=="Parties") |
                         (data11$Target.Sectors_11=="Government" | data11$Target.Sectors_11=="Executive" | data11$Target.Sectors_11=="Police" | data11$Target.Sectors_11=="Military" | data11$Target.Sectors_11=="Legislative / Parliamentarty" | data11$Target.Sectors_11=="Judicial" | data11$Target.Sectors_11=="Local" | data11$Target.Sectors_11=="Parties") |
                         (data11$Target.Sectors_12=="Government" | data11$Target.Sectors_12=="Executive" | data11$Target.Sectors_12=="Police" | data11$Target.Sectors_12=="Military" | data11$Target.Sectors_12=="Legislative / Parliamentarty" | data11$Target.Sectors_12=="Judicial" | data11$Target.Sectors_12=="Local" | data11$Target.Sectors_12=="Parties") | 
                         (data11$Target.Sectors_13=="Government" | data11$Target.Sectors_13=="Executive" | data11$Target.Sectors_13=="Police" | data11$Target.Sectors_13=="Military" | data11$Target.Sectors_13=="Legislative / Parliamentarty" | data11$Target.Sectors_13=="Judicial" | data11$Target.Sectors_13=="Local" | data11$Target.Sectors_13=="Parties") |
                         (data11$Target.Sectors_14=="Government" | data11$Target.Sectors_14=="Executive" | data11$Target.Sectors_14=="Police" | data11$Target.Sectors_14=="Military" | data11$Target.Sectors_14=="Legislative / Parliamentarty" | data11$Target.Sectors_14=="Judicial" | data11$Target.Sectors_14=="Local" | data11$Target.Sectors_14=="Parties") |
                         (data11$Target.Sectors_15=="Government" | data11$Target.Sectors_15=="Executive" | data11$Target.Sectors_15=="Police" | data11$Target.Sectors_15=="Military" | data11$Target.Sectors_15=="Legislative / Parliamentarty" | data11$Target.Sectors_15=="Judicial" | data11$Target.Sectors_15=="Local" | data11$Target.Sectors_15=="Parties") |
                         (data11$Target.Sectors_16=="Government" | data11$Target.Sectors_16=="Executive" | data11$Target.Sectors_16=="Police" | data11$Target.Sectors_16=="Military" | data11$Target.Sectors_16=="Legislative / Parliamentarty" | data11$Target.Sectors_16=="Judicial" | data11$Target.Sectors_16=="Local" | data11$Target.Sectors_16=="Parties") | 
                         (data11$Target.Sectors_17=="Government" | data11$Target.Sectors_17=="Executive" | data11$Target.Sectors_17=="Police" | data11$Target.Sectors_17=="Military" | data11$Target.Sectors_17=="Legislative / Parliamentarty" | data11$Target.Sectors_17=="Judicial" | data11$Target.Sectors_17=="Local" | data11$Target.Sectors_17=="Parties") 
)


data11c<-subset(data11b, 
                (data11b$Source.Sectors_1=="Human Rights IGOs" | data11b$Source.Sectors_1=="Global Human Rights IGOs" | data11b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data11b$Source.Sectors_2=="Human Rights IGOs" | data11b$Source.Sectors_2=="Global Human Rights IGOs" | data11b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data11b$Source.Sectors_3=="Human Rights IGOs" | data11b$Source.Sectors_3=="Global Human Rights IGOs" | data11b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data11b$Source.Sectors_4=="Human Rights IGOs" | data11b$Source.Sectors_4=="Global Human Rights IGOs" | data11b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data11b$Source.Sectors_5=="Human Rights IGOs" | data11b$Source.Sectors_5=="Global Human Rights IGOs" | data11b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data11b$Source.Sectors_6=="Human Rights IGOs" | data11b$Source.Sectors_6=="Global Human Rights IGOs" | data11b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data11b$Source.Sectors_7=="Human Rights IGOs" | data11b$Source.Sectors_7=="Global Human Rights IGOs" | data11b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data11b$Source.Sectors_8=="Human Rights IGOs" | data11b$Source.Sectors_8=="Global Human Rights IGOs" | data11b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data11b$Source.Sectors_9=="Human Rights IGOs" | data11b$Source.Sectors_9=="Global Human Rights IGOs" | data11b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data11b$Source.Sectors_10=="Human Rights IGOs" | data11b$Source.Sectors_10=="Global Human Rights IGOs" | data11b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data11b$Source.Sectors_11=="Human Rights IGOs" | data11b$Source.Sectors_11=="Global Human Rights IGOs" | data11b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data11b$Source.Sectors_12=="Human Rights IGOs" | data11b$Source.Sectors_12=="Global Human Rights IGOs" | data11b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data11b$Source.Sectors_13=="Human Rights IGOs" | data11b$Source.Sectors_13=="Global Human Rights IGOs" | data11b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data11b$Source.Sectors_14=="Human Rights IGOs" | data11b$Source.Sectors_14=="Global Human Rights IGOs" | data11b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data11b$Source.Sectors_15=="Human Rights IGOs" | data11b$Source.Sectors_15=="Global Human Rights IGOs" | data11b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data11b$Source.Sectors_16=="Human Rights IGOs" | data11b$Source.Sectors_16=="Global Human Rights IGOs" | data11b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data11b$Source.Sectors_17=="Human Rights IGOs" | data11b$Source.Sectors_17=="Global Human Rights IGOs" | data11b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data11d<-data11c
data11d$Appeal_Jud_Coop<-ifelse(data11d$CAMEO.Code=="213", 1, NA)
data11d$Appeal_Change_Leadership<-ifelse(data11d$CAMEO.Code=="241", 1, NA)
data11d$Appeal_Policy_Change<-ifelse(data11d$CAMEO.Code=="242", 1, NA)
data11d$Appeal_Rights<-ifelse(data11d$CAMEO.Code=="243", 1, NA)
data11d$Appeal_Change_Inst<-ifelse(data11d$CAMEO.Code=="244", 1, NA)
data11d$Appeal_Release<-ifelse(data11d$CAMEO.Code=="253", 1, NA)
data11d$Demand<-ifelse(data11d$CAMEO.Code=="100", 1, NA)
data11d$Demand_Change_Leadership<-ifelse(data11d$CAMEO.Code=="1041", 1, NA) 
data11d$Demand_Policy_Change<-ifelse(data11d$CAMEO.Code=="1042", 1, NA)
data11d$Demand_Rights<-ifelse(data11d$CAMEO.Code=="1043", 1, NA)
data11d$Demand_Change_Inst<-ifelse(data11d$CAMEO.Code=="1044", 1, NA)
data11d$Accuse_HR<-ifelse(data11d$CAMEO.Code=="1122", 1, NA)
data11d$Accuse<-ifelse(data11d$CAMEO.Code=="112", 1, NA)
data11d$Criticize<-ifelse(data11d$CAMEO.Code=="111", 1, NA)
data11d$Investigate_War_Crimes<-ifelse(data11d$CAMEO.Code=="94", 1, NA)
data11d$Investigate_HR<-ifelse(data11d$CAMEO.Code=="92", 1, NA)
data11d$Demand_Jud_Coop<-ifelse(data11d$CAMEO.Code=="1013", 1, NA)
data11d$Demand_Hum_Aid<-ifelse(data11d$CAMEO.Code=="1033", 1, NA)
data11d$Demand_Release<-ifelse(data11d$CAMEO.Code=="1053", 1, NA)
data11d$Accuse_War_Crimes<-ifelse(data11d$CAMEO.Code=="1124", 1, NA)



data11e<-separate(data11d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts2011<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data11e)



write.csv(event.counts2011, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts2011.cvs")

##2012

Events12 <- read.csv2("/Users/KimFruge/Desktop/Projects/Events2012.tab",  sep="\t", header=TRUE)

data12a<-separate(Events12, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data12<-separate(data12a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data12b<- subset(data12,
                 (data12$Target.Sectors_1=="Government" | data12$Target.Sectors_1=="Executive" | data12$Target.Sectors_1=="Police" | data12$Target.Sectors_1=="Military" | data12$Target.Sectors_1=="Legislative / Parliamentarty" | data12$Target.Sectors_1=="Judicial" | data12$Target.Sectors_1=="Local" | data12$Target.Sectors_1=="Parties") |
                         (data12$Target.Sectors_2=="Government" | data12$Target.Sectors_2=="Executive" | data12$Target.Sectors_2=="Police" | data12$Target.Sectors_2=="Military" | data12$Target.Sectors_2=="Legislative / Parliamentarty" | data12$Target.Sectors_2=="Judicial" | data12$Target.Sectors_2=="Local" | data12$Target.Sectors_2=="Parties") |
                         (data12$Target.Sectors_3=="Government" | data12$Target.Sectors_3=="Executive" | data12$Target.Sectors_3=="Police" | data12$Target.Sectors_3=="Military" | data12$Target.Sectors_3=="Legislative / Parliamentarty" | data12$Target.Sectors_3=="Judicial" | data12$Target.Sectors_3=="Local" | data12$Target.Sectors_3=="Parties") |
                         (data12$Target.Sectors_4=="Government" | data12$Target.Sectors_4=="Executive" | data12$Target.Sectors_4=="Police" | data12$Target.Sectors_4=="Military" | data12$Target.Sectors_4=="Legislative / Parliamentarty" | data12$Target.Sectors_4=="Judicial" | data12$Target.Sectors_4=="Local" | data12$Target.Sectors_4=="Parties") |
                         (data12$Target.Sectors_5=="Government" | data12$Target.Sectors_5=="Executive" | data12$Target.Sectors_5=="Police" | data12$Target.Sectors_5=="Military" | data12$Target.Sectors_5=="Legislative / Parliamentarty" | data12$Target.Sectors_5=="Judicial" | data12$Target.Sectors_5=="Local" | data12$Target.Sectors_5=="Parties") |
                         (data12$Target.Sectors_6=="Government" | data12$Target.Sectors_6=="Executive" | data12$Target.Sectors_6=="Police" | data12$Target.Sectors_6=="Military" | data12$Target.Sectors_6=="Legislative / Parliamentarty" | data12$Target.Sectors_6=="Judicial" | data12$Target.Sectors_6=="Local" | data12$Target.Sectors_6=="Parties") | 
                         (data12$Target.Sectors_7=="Government" | data12$Target.Sectors_7=="Executive" | data12$Target.Sectors_7=="Police" | data12$Target.Sectors_7=="Military" | data12$Target.Sectors_7=="Legislative / Parliamentarty" | data12$Target.Sectors_7=="Judicial" | data12$Target.Sectors_7=="Local" | data12$Target.Sectors_7=="Parties") |
                         (data12$Target.Sectors_8=="Government" | data12$Target.Sectors_8=="Executive" | data12$Target.Sectors_8=="Police" | data12$Target.Sectors_8=="Military" | data12$Target.Sectors_8=="Legislative / Parliamentarty" | data12$Target.Sectors_8=="Judicial" | data12$Target.Sectors_8=="Local" | data12$Target.Sectors_8=="Parties") | 
                         (data12$Target.Sectors_9=="Government" | data12$Target.Sectors_9=="Executive" | data12$Target.Sectors_9=="Police" | data12$Target.Sectors_9=="Military" | data12$Target.Sectors_9=="Legislative / Parliamentarty" | data12$Target.Sectors_9=="Judicial" | data12$Target.Sectors_9=="Local" | data12$Target.Sectors_9=="Parties") |
                         (data12$Target.Sectors_10=="Government" | data12$Target.Sectors_10=="Executive" | data12$Target.Sectors_10=="Police" | data12$Target.Sectors_10=="Military" | data12$Target.Sectors_10=="Legislative / Parliamentarty" | data12$Target.Sectors_10=="Judicial" | data12$Target.Sectors_10=="Local" | data12$Target.Sectors_10=="Parties") |
                         (data12$Target.Sectors_11=="Government" | data12$Target.Sectors_11=="Executive" | data12$Target.Sectors_11=="Police" | data12$Target.Sectors_11=="Military" | data12$Target.Sectors_11=="Legislative / Parliamentarty" | data12$Target.Sectors_11=="Judicial" | data12$Target.Sectors_11=="Local" | data12$Target.Sectors_11=="Parties") |
                         (data12$Target.Sectors_12=="Government" | data12$Target.Sectors_12=="Executive" | data12$Target.Sectors_12=="Police" | data12$Target.Sectors_12=="Military" | data12$Target.Sectors_12=="Legislative / Parliamentarty" | data12$Target.Sectors_12=="Judicial" | data12$Target.Sectors_12=="Local" | data12$Target.Sectors_12=="Parties") | 
                         (data12$Target.Sectors_13=="Government" | data12$Target.Sectors_13=="Executive" | data12$Target.Sectors_13=="Police" | data12$Target.Sectors_13=="Military" | data12$Target.Sectors_13=="Legislative / Parliamentarty" | data12$Target.Sectors_13=="Judicial" | data12$Target.Sectors_13=="Local" | data12$Target.Sectors_13=="Parties") |
                         (data12$Target.Sectors_14=="Government" | data12$Target.Sectors_14=="Executive" | data12$Target.Sectors_14=="Police" | data12$Target.Sectors_14=="Military" | data12$Target.Sectors_14=="Legislative / Parliamentarty" | data12$Target.Sectors_14=="Judicial" | data12$Target.Sectors_14=="Local" | data12$Target.Sectors_14=="Parties") |
                         (data12$Target.Sectors_15=="Government" | data12$Target.Sectors_15=="Executive" | data12$Target.Sectors_15=="Police" | data12$Target.Sectors_15=="Military" | data12$Target.Sectors_15=="Legislative / Parliamentarty" | data12$Target.Sectors_15=="Judicial" | data12$Target.Sectors_15=="Local" | data12$Target.Sectors_15=="Parties") |
                         (data12$Target.Sectors_16=="Government" | data12$Target.Sectors_16=="Executive" | data12$Target.Sectors_16=="Police" | data12$Target.Sectors_16=="Military" | data12$Target.Sectors_16=="Legislative / Parliamentarty" | data12$Target.Sectors_16=="Judicial" | data12$Target.Sectors_16=="Local" | data12$Target.Sectors_16=="Parties") | 
                         (data12$Target.Sectors_17=="Government" | data12$Target.Sectors_17=="Executive" | data12$Target.Sectors_17=="Police" | data12$Target.Sectors_17=="Military" | data12$Target.Sectors_17=="Legislative / Parliamentarty" | data12$Target.Sectors_17=="Judicial" | data12$Target.Sectors_17=="Local" | data12$Target.Sectors_17=="Parties") 
)


data12c<-subset(data12b, 
                (data12b$Source.Sectors_1=="Human Rights IGOs" | data12b$Source.Sectors_1=="Global Human Rights IGOs" | data12b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data12b$Source.Sectors_2=="Human Rights IGOs" | data12b$Source.Sectors_2=="Global Human Rights IGOs" | data12b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data12b$Source.Sectors_3=="Human Rights IGOs" | data12b$Source.Sectors_3=="Global Human Rights IGOs" | data12b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data12b$Source.Sectors_4=="Human Rights IGOs" | data12b$Source.Sectors_4=="Global Human Rights IGOs" | data12b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data12b$Source.Sectors_5=="Human Rights IGOs" | data12b$Source.Sectors_5=="Global Human Rights IGOs" | data12b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data12b$Source.Sectors_6=="Human Rights IGOs" | data12b$Source.Sectors_6=="Global Human Rights IGOs" | data12b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data12b$Source.Sectors_7=="Human Rights IGOs" | data12b$Source.Sectors_7=="Global Human Rights IGOs" | data12b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data12b$Source.Sectors_8=="Human Rights IGOs" | data12b$Source.Sectors_8=="Global Human Rights IGOs" | data12b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data12b$Source.Sectors_9=="Human Rights IGOs" | data12b$Source.Sectors_9=="Global Human Rights IGOs" | data12b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data12b$Source.Sectors_10=="Human Rights IGOs" | data12b$Source.Sectors_10=="Global Human Rights IGOs" | data12b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data12b$Source.Sectors_11=="Human Rights IGOs" | data12b$Source.Sectors_11=="Global Human Rights IGOs" | data12b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data12b$Source.Sectors_12=="Human Rights IGOs" | data12b$Source.Sectors_12=="Global Human Rights IGOs" | data12b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data12b$Source.Sectors_13=="Human Rights IGOs" | data12b$Source.Sectors_13=="Global Human Rights IGOs" | data12b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data12b$Source.Sectors_14=="Human Rights IGOs" | data12b$Source.Sectors_14=="Global Human Rights IGOs" | data12b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data12b$Source.Sectors_15=="Human Rights IGOs" | data12b$Source.Sectors_15=="Global Human Rights IGOs" | data12b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data12b$Source.Sectors_16=="Human Rights IGOs" | data12b$Source.Sectors_16=="Global Human Rights IGOs" | data12b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data12b$Source.Sectors_17=="Human Rights IGOs" | data12b$Source.Sectors_17=="Global Human Rights IGOs" | data12b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data12d<-data12c
data12d$Appeal_Jud_Coop<-ifelse(data12d$CAMEO.Code=="213", 1, NA)
data12d$Appeal_Change_Leadership<-ifelse(data12d$CAMEO.Code=="241", 1, NA)
data12d$Appeal_Policy_Change<-ifelse(data12d$CAMEO.Code=="242", 1, NA)
data12d$Appeal_Rights<-ifelse(data12d$CAMEO.Code=="243", 1, NA)
data12d$Appeal_Change_Inst<-ifelse(data12d$CAMEO.Code=="244", 1, NA)
data12d$Appeal_Release<-ifelse(data12d$CAMEO.Code=="253", 1, NA)
data12d$Demand<-ifelse(data12d$CAMEO.Code=="100", 1, NA)
data12d$Demand_Change_Leadership<-ifelse(data12d$CAMEO.Code=="1041", 1, NA) 
data12d$Demand_Policy_Change<-ifelse(data12d$CAMEO.Code=="1042", 1, NA)
data12d$Demand_Rights<-ifelse(data12d$CAMEO.Code=="1043", 1, NA)
data12d$Demand_Change_Inst<-ifelse(data12d$CAMEO.Code=="1044", 1, NA)
data12d$Accuse_HR<-ifelse(data12d$CAMEO.Code=="1122", 1, NA)
data12d$Accuse<-ifelse(data12d$CAMEO.Code=="112", 1, NA)
data12d$Criticize<-ifelse(data12d$CAMEO.Code=="111", 1, NA)
data12d$Investigate_War_Crimes<-ifelse(data12d$CAMEO.Code=="94", 1, NA)
data12d$Investigate_HR<-ifelse(data12d$CAMEO.Code=="92", 1, NA)
data12d$Demand_Jud_Coop<-ifelse(data12d$CAMEO.Code=="1013", 1, NA)
data12d$Demand_Hum_Aid<-ifelse(data12d$CAMEO.Code=="1033", 1, NA)
data12d$Demand_Release<-ifelse(data12d$CAMEO.Code=="1053", 1, NA)
data12d$Accuse_War_Crimes<-ifelse(data12d$CAMEO.Code=="1124", 1, NA)



data12e<-separate(data12d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts2012<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data12e)



write.csv(event.counts2012, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts2012.cvs")

##2013

Events13 <- read.csv2("/Users/KimFruge/Desktop/Projects/Events2013.tab",  sep="\t", header=TRUE)

data13a<-separate(Events13, "Target.Sectors", paste("Target.Sectors", 1:17, sep="_"), sep=",", extra="drop")

data13<-separate(data13a, "Source.Sectors", paste("Source.Sectors", 1:17, sep="_"), sep=",", extra="drop")


data13b<- subset(data13,
                 (data13$Target.Sectors_1=="Government" | data13$Target.Sectors_1=="Executive" | data13$Target.Sectors_1=="Police" | data13$Target.Sectors_1=="Military" | data13$Target.Sectors_1=="Legislative / Parliamentarty" | data13$Target.Sectors_1=="Judicial" | data13$Target.Sectors_1=="Local" | data13$Target.Sectors_1=="Parties") |
                         (data13$Target.Sectors_2=="Government" | data13$Target.Sectors_2=="Executive" | data13$Target.Sectors_2=="Police" | data13$Target.Sectors_2=="Military" | data13$Target.Sectors_2=="Legislative / Parliamentarty" | data13$Target.Sectors_2=="Judicial" | data13$Target.Sectors_2=="Local" | data13$Target.Sectors_2=="Parties") |
                         (data13$Target.Sectors_3=="Government" | data13$Target.Sectors_3=="Executive" | data13$Target.Sectors_3=="Police" | data13$Target.Sectors_3=="Military" | data13$Target.Sectors_3=="Legislative / Parliamentarty" | data13$Target.Sectors_3=="Judicial" | data13$Target.Sectors_3=="Local" | data13$Target.Sectors_3=="Parties") |
                         (data13$Target.Sectors_4=="Government" | data13$Target.Sectors_4=="Executive" | data13$Target.Sectors_4=="Police" | data13$Target.Sectors_4=="Military" | data13$Target.Sectors_4=="Legislative / Parliamentarty" | data13$Target.Sectors_4=="Judicial" | data13$Target.Sectors_4=="Local" | data13$Target.Sectors_4=="Parties") |
                         (data13$Target.Sectors_5=="Government" | data13$Target.Sectors_5=="Executive" | data13$Target.Sectors_5=="Police" | data13$Target.Sectors_5=="Military" | data13$Target.Sectors_5=="Legislative / Parliamentarty" | data13$Target.Sectors_5=="Judicial" | data13$Target.Sectors_5=="Local" | data13$Target.Sectors_5=="Parties") |
                         (data13$Target.Sectors_6=="Government" | data13$Target.Sectors_6=="Executive" | data13$Target.Sectors_6=="Police" | data13$Target.Sectors_6=="Military" | data13$Target.Sectors_6=="Legislative / Parliamentarty" | data13$Target.Sectors_6=="Judicial" | data13$Target.Sectors_6=="Local" | data13$Target.Sectors_6=="Parties") | 
                         (data13$Target.Sectors_7=="Government" | data13$Target.Sectors_7=="Executive" | data13$Target.Sectors_7=="Police" | data13$Target.Sectors_7=="Military" | data13$Target.Sectors_7=="Legislative / Parliamentarty" | data13$Target.Sectors_7=="Judicial" | data13$Target.Sectors_7=="Local" | data13$Target.Sectors_7=="Parties") |
                         (data13$Target.Sectors_8=="Government" | data13$Target.Sectors_8=="Executive" | data13$Target.Sectors_8=="Police" | data13$Target.Sectors_8=="Military" | data13$Target.Sectors_8=="Legislative / Parliamentarty" | data13$Target.Sectors_8=="Judicial" | data13$Target.Sectors_8=="Local" | data13$Target.Sectors_8=="Parties") | 
                         (data13$Target.Sectors_9=="Government" | data13$Target.Sectors_9=="Executive" | data13$Target.Sectors_9=="Police" | data13$Target.Sectors_9=="Military" | data13$Target.Sectors_9=="Legislative / Parliamentarty" | data13$Target.Sectors_9=="Judicial" | data13$Target.Sectors_9=="Local" | data13$Target.Sectors_9=="Parties") |
                         (data13$Target.Sectors_10=="Government" | data13$Target.Sectors_10=="Executive" | data13$Target.Sectors_10=="Police" | data13$Target.Sectors_10=="Military" | data13$Target.Sectors_10=="Legislative / Parliamentarty" | data13$Target.Sectors_10=="Judicial" | data13$Target.Sectors_10=="Local" | data13$Target.Sectors_10=="Parties") |
                         (data13$Target.Sectors_11=="Government" | data13$Target.Sectors_11=="Executive" | data13$Target.Sectors_11=="Police" | data13$Target.Sectors_11=="Military" | data13$Target.Sectors_11=="Legislative / Parliamentarty" | data13$Target.Sectors_11=="Judicial" | data13$Target.Sectors_11=="Local" | data13$Target.Sectors_11=="Parties") |
                         (data13$Target.Sectors_12=="Government" | data13$Target.Sectors_12=="Executive" | data13$Target.Sectors_12=="Police" | data13$Target.Sectors_12=="Military" | data13$Target.Sectors_12=="Legislative / Parliamentarty" | data13$Target.Sectors_12=="Judicial" | data13$Target.Sectors_12=="Local" | data13$Target.Sectors_12=="Parties") | 
                         (data13$Target.Sectors_13=="Government" | data13$Target.Sectors_13=="Executive" | data13$Target.Sectors_13=="Police" | data13$Target.Sectors_13=="Military" | data13$Target.Sectors_13=="Legislative / Parliamentarty" | data13$Target.Sectors_13=="Judicial" | data13$Target.Sectors_13=="Local" | data13$Target.Sectors_13=="Parties") |
                         (data13$Target.Sectors_14=="Government" | data13$Target.Sectors_14=="Executive" | data13$Target.Sectors_14=="Police" | data13$Target.Sectors_14=="Military" | data13$Target.Sectors_14=="Legislative / Parliamentarty" | data13$Target.Sectors_14=="Judicial" | data13$Target.Sectors_14=="Local" | data13$Target.Sectors_14=="Parties") |
                         (data13$Target.Sectors_15=="Government" | data13$Target.Sectors_15=="Executive" | data13$Target.Sectors_15=="Police" | data13$Target.Sectors_15=="Military" | data13$Target.Sectors_15=="Legislative / Parliamentarty" | data13$Target.Sectors_15=="Judicial" | data13$Target.Sectors_15=="Local" | data13$Target.Sectors_15=="Parties") |
                         (data13$Target.Sectors_16=="Government" | data13$Target.Sectors_16=="Executive" | data13$Target.Sectors_16=="Police" | data13$Target.Sectors_16=="Military" | data13$Target.Sectors_16=="Legislative / Parliamentarty" | data13$Target.Sectors_16=="Judicial" | data13$Target.Sectors_16=="Local" | data13$Target.Sectors_16=="Parties") | 
                         (data13$Target.Sectors_17=="Government" | data13$Target.Sectors_17=="Executive" | data13$Target.Sectors_17=="Police" | data13$Target.Sectors_17=="Military" | data13$Target.Sectors_17=="Legislative / Parliamentarty" | data13$Target.Sectors_17=="Judicial" | data13$Target.Sectors_17=="Local" | data13$Target.Sectors_17=="Parties") 
)


data13c<-subset(data13b, 
                (data13b$Source.Sectors_1=="Human Rights IGOs" | data13b$Source.Sectors_1=="Global Human Rights IGOs" | data13b$Source.Sectors_1=="Regional Human Rights IGOs") | 
                        (data13b$Source.Sectors_2=="Human Rights IGOs" | data13b$Source.Sectors_2=="Global Human Rights IGOs" | data13b$Source.Sectors_2=="Regional Human Rights IGOs") | 
                        (data13b$Source.Sectors_3=="Human Rights IGOs" | data13b$Source.Sectors_3=="Global Human Rights IGOs" | data13b$Source.Sectors_3=="Regional Human Rights IGOs") | 
                        (data13b$Source.Sectors_4=="Human Rights IGOs" | data13b$Source.Sectors_4=="Global Human Rights IGOs" | data13b$Source.Sectors_4=="Regional Human Rights IGOs") | 
                        (data13b$Source.Sectors_5=="Human Rights IGOs" | data13b$Source.Sectors_5=="Global Human Rights IGOs" | data13b$Source.Sectors_5=="Regional Human Rights IGOs") | 
                        (data13b$Source.Sectors_6=="Human Rights IGOs" | data13b$Source.Sectors_6=="Global Human Rights IGOs" | data13b$Source.Sectors_6=="Regional Human Rights IGOs") | 
                        (data13b$Source.Sectors_7=="Human Rights IGOs" | data13b$Source.Sectors_7=="Global Human Rights IGOs" | data13b$Source.Sectors_7=="Regional Human Rights IGOs") | 
                        (data13b$Source.Sectors_8=="Human Rights IGOs" | data13b$Source.Sectors_8=="Global Human Rights IGOs" | data13b$Source.Sectors_8=="Regional Human Rights IGOs") | 
                        (data13b$Source.Sectors_9=="Human Rights IGOs" | data13b$Source.Sectors_9=="Global Human Rights IGOs" | data13b$Source.Sectors_9=="Regional Human Rights IGOs") | 
                        (data13b$Source.Sectors_10=="Human Rights IGOs" | data13b$Source.Sectors_10=="Global Human Rights IGOs" | data13b$Source.Sectors_10=="Regional Human Rights IGOs") | 
                        (data13b$Source.Sectors_11=="Human Rights IGOs" | data13b$Source.Sectors_11=="Global Human Rights IGOs" | data13b$Source.Sectors_11=="Regional Human Rights IGOs") | 
                        (data13b$Source.Sectors_12=="Human Rights IGOs" | data13b$Source.Sectors_12=="Global Human Rights IGOs" | data13b$Source.Sectors_12=="Regional Human Rights IGOs") | 
                        (data13b$Source.Sectors_13=="Human Rights IGOs" | data13b$Source.Sectors_13=="Global Human Rights IGOs" | data13b$Source.Sectors_13=="Regional Human Rights IGOs") | 
                        (data13b$Source.Sectors_14=="Human Rights IGOs" | data13b$Source.Sectors_14=="Global Human Rights IGOs" | data13b$Source.Sectors_14=="Regional Human Rights IGOs") | 
                        (data13b$Source.Sectors_15=="Human Rights IGOs" | data13b$Source.Sectors_15=="Global Human Rights IGOs" | data13b$Source.Sectors_15=="Regional Human Rights IGOs") | 
                        (data13b$Source.Sectors_16=="Human Rights IGOs" | data13b$Source.Sectors_16=="Global Human Rights IGOs" | data13b$Source.Sectors_16=="Regional Human Rights IGOs") | 
                        (data13b$Source.Sectors_17=="Human Rights IGOs" | data13b$Source.Sectors_17=="Global Human Rights IGOs" | data13b$Source.Sectors_17=="Regional Human Rights IGOs") 
)

data13d<-data13c
data13d$Appeal_Jud_Coop<-ifelse(data13d$CAMEO.Code=="213", 1, NA)
data13d$Appeal_Change_Leadership<-ifelse(data13d$CAMEO.Code=="241", 1, NA)
data13d$Appeal_Policy_Change<-ifelse(data13d$CAMEO.Code=="242", 1, NA)
data13d$Appeal_Rights<-ifelse(data13d$CAMEO.Code=="243", 1, NA)
data13d$Appeal_Change_Inst<-ifelse(data13d$CAMEO.Code=="244", 1, NA)
data13d$Appeal_Release<-ifelse(data13d$CAMEO.Code=="253", 1, NA)
data13d$Demand<-ifelse(data13d$CAMEO.Code=="100", 1, NA)
data13d$Demand_Change_Leadership<-ifelse(data13d$CAMEO.Code=="1041", 1, NA) 
data13d$Demand_Policy_Change<-ifelse(data13d$CAMEO.Code=="1042", 1, NA)
data13d$Demand_Rights<-ifelse(data13d$CAMEO.Code=="1043", 1, NA)
data13d$Demand_Change_Inst<-ifelse(data13d$CAMEO.Code=="1044", 1, NA)
data13d$Accuse_HR<-ifelse(data13d$CAMEO.Code=="1122", 1, NA)
data13d$Accuse<-ifelse(data13d$CAMEO.Code=="112", 1, NA)
data13d$Criticize<-ifelse(data13d$CAMEO.Code=="111", 1, NA)
data13d$Investigate_War_Crimes<-ifelse(data13d$CAMEO.Code=="94", 1, NA)
data13d$Investigate_HR<-ifelse(data13d$CAMEO.Code=="92", 1, NA)
data13d$Demand_Jud_Coop<-ifelse(data13d$CAMEO.Code=="1013", 1, NA)
data13d$Demand_Hum_Aid<-ifelse(data13d$CAMEO.Code=="1033", 1, NA)
data13d$Demand_Release<-ifelse(data13d$CAMEO.Code=="1053", 1, NA)
data13d$Accuse_War_Crimes<-ifelse(data13d$CAMEO.Code=="1124", 1, NA)



data13e<-separate(data13d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.counts2013<-summaryBy(Appeal_Jud_Coop + Appeal_Change_Leadership + Appeal_Policy_Change + 
                                   Appeal_Rights + Appeal_Change_Inst + Demand + Demand_Change_Leadership
                           + Demand_Policy_Change + Demand_Rights + Demand_Change_Inst + Appeal_Release
                           + Accuse_HR + Accuse + Criticize + Investigate_War_Crimes + Investigate_HR + Demand_Jud_Coop + Demand_Hum_Aid 
                           + Demand_Release + Accuse_War_Crimes ~ Country + Date_1, 
                           FUN=sum, data=data13e)



write.csv(event.counts2013, "/Users/KimFruge/Desktop/Projects/JAHRO/event.counts2013.cvs")

#_________________________


HRO_data<-rbind(event.counts1995, event.counts1996, event.counts1997, event.counts1998, event.counts1999, event.counts2000, event.counts2001, event.counts2002, event.counts2003, event.counts2004, event.counts2005, event.counts2006, event.counts2007, event.counts2008, event.counts2009, event.counts2010, event.counts2011, event.counts2012, event.counts2013)

HRO_data[is.na(HRO_data)] <- 0

HRO_data$NS_Jud<-HRO_data$Appeal_Jud_Coop + HRO_data$Demand_Jud_Coop
        
HRO_data$NS <- (HRO_data$Appeal_Change_Leadership.sum + HRO_data$Appeal_Policy_Change.sum  
               + HRO_data$Appeal_Rights.sum  + HRO_data$Appeal_Change_Inst.sum  
               + HRO_data$Demand_Change_Leadership.sum  + HRO_data$Demand_Policy_Change.sum  
               + HRO_data$Demand_Rights.sum  + HRO_data$Demand_Change_Inst.sum  
               + HRO_data$Appeal_Release.sum  + HRO_data$Accuse_HR.sum 
               + HRO_data$Criticize.sum  + HRO_data$Investigate_War_Crimes.sum 
               + HRO_data$Investigate_HR.sum  + HRO_data$Demand_Hum_Aid.sum  
               + HRO_data$Demand_Release.sum  + HRO_data$Accuse_War_Crimes.sum )


write.csv(HRO_data, "/Users/KimFruge/Desktop/Projects/JAHRO/HRO_data.cvs")




