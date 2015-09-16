#Kim Fruge and Jordan Holsinger 
#Florida State University 
# 15 Sept 2015
# Subsetting ICEWS Data, first by government target, then HRO source, then events, collapsing data, and saving 


library(foreign)
library(tidyr)
library(doBy)

Events95 <- read.csv2("/Volumes/Lexar/Events1995.tab",  sep="\t", header=TRUE)

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
data95d$CC10<-ifelse(data95d$CAMEO.Code=="10", 1, 0)
data95d$CC12<-ifelse(data95d$CAMEO.Code=="12", 1, 0)
data95d$CC20<-ifelse(data95d$CAMEO.Code=="20", 1, 0)
data95d$CC22<-ifelse(data95d$CAMEO.Code=="22", 1, 0)
data95d$CC23<-ifelse(data95d$CAMEO.Code=="23", 1, 0)
data95d$CC24<-ifelse(data95d$CAMEO.Code=="24", 1, 0)
data95d$CC25<-ifelse(data95d$CAMEO.Code=="25", 1, 0)
data95d$CC90<-ifelse(data95d$CAMEO.Code=="90", 1, 0)
data95d$CC91<-ifelse(data95d$CAMEO.Code=="91", 1, 0)
data95d$CC92<-ifelse(data95d$CAMEO.Code=="92", 1, 0)
data95d$CC94<-ifelse(data95d$CAMEO.Code=="94", 1, 0)
data95d$CC100<-ifelse(data95d$CAMEO.Code=="100", 1, 0)
data95d$CC101<-ifelse(data95d$CAMEO.Code=="101", 1, 0)
data95d$CC102<-ifelse(data95d$CAMEO.Code=="102", 1, 0)
data95d$CC103<-ifelse(data95d$CAMEO.Code=="103", 1, 0)
data95d$CC104<-ifelse(data95d$CAMEO.Code=="104", 1, 0)
data95d$CC106<-ifelse(data95d$CAMEO.Code=="106", 1, 0)
data95d$CC108<-ifelse(data95d$CAMEO.Code=="108", 1, 0)
data95d$CC111<-ifelse(data95d$CAMEO.Code=="111", 1, 0)
data95d$CC112<-ifelse(data95d$CAMEO.Code=="112", 1, 0)
data95d$CC113<-ifelse(data95d$CAMEO.Code=="113", 1, 0)
data95d$CC114<-ifelse(data95d$CAMEO.Code=="114", 1, 0)
data95d$CC124<-ifelse(data95d$CAMEO.Code=="124", 1, 0)
data95d$CC130<-ifelse(data95d$CAMEO.Code=="130", 1, 0)
data95d$CC131<-ifelse(data95d$CAMEO.Code=="131", 1, 0)
data95d$CC133<-ifelse(data95d$CAMEO.Code=="133", 1, 0)
data95d$CC138<-ifelse(data95d$CAMEO.Code=="138", 1, 0)
data95d$CC139<-ifelse(data95d$CAMEO.Code=="139", 1, 0)
data95d$CC163<-ifelse(data95d$CAMEO.Code=="163", 1, 0)
data95d$CC213<-ifelse(data95d$CAMEO.Code=="213", 1, 0)
data95d$CC214<-ifelse(data95d$CAMEO.Code=="214", 1, 0)
data95d$CC241<-ifelse(data95d$CAMEO.Code=="241", 1, 0)
data95d$CC242<-ifelse(data95d$CAMEO.Code=="242", 1, 0)
data95d$CC243<-ifelse(data95d$CAMEO.Code=="243", 1, 0)
data95d$CC244<-ifelse(data95d$CAMEO.Code=="244", 1, 0)
data95d$CC253<-ifelse(data95d$CAMEO.Code=="253", 1, 0)
data95d$CC255<-ifelse(data95d$CAMEO.Code=="255", 1, 0)
data95d$CC256<-ifelse(data95d$CAMEO.Code=="256", 1, 0)
data95d$CC1041<-ifelse(data95d$CAMEO.Code=="1041", 1, 0)
data95d$CC1042<-ifelse(data95d$CAMEO.Code=="1042", 1, 0)
data95d$CC1043<-ifelse(data95d$CAMEO.Code=="1043", 1, 0)
data95d$CC1053<-ifelse(data95d$CAMEO.Code=="1053", 1, 0)
data95d$CC1056<-ifelse(data95d$CAMEO.Code=="1056", 1, 0)
data95d$CC1121<-ifelse(data95d$CAMEO.Code=="1121", 1, 0)
data95d$CC1122<-ifelse(data95d$CAMEO.Code=="1122", 1, 0)
data95d$CC1123<-ifelse(data95d$CAMEO.Code=="1123", 1, 0)
data95d$CC1124<-ifelse(data95d$CAMEO.Code=="1124", 1, 0)
data95d$CC1125<-ifelse(data95d$CAMEO.Code=="1125", 1, 0)
data95d$CC1242<-ifelse(data95d$CAMEO.Code=="1242", 1, 0)
data95d$CC93 <- ifelse(data95d$CAMEO.Code=="93", 1, 0)
data95d$CC1014 <- ifelse(data95d$CAMEO.Code=="1014", 1, 0)



data95e<-separate(data95d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count1995<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242  ~ Country + Date_1, 
                        FUN=sum, data=data95e)



write.csv(event.count1995, "/Volumes/Lexar/event.counts1995.csv") 

#________________________

Events96 <- read.csv2("/Volumes/Lexar/Events1996.tab",  sep="\t", header=TRUE)

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
data96d$CC10<-ifelse(data96d$CAMEO.Code=="10", 1, 0)
data96d$CC12<-ifelse(data96d$CAMEO.Code=="12", 1, 0)
data96d$CC20<-ifelse(data96d$CAMEO.Code=="20", 1, 0)
data96d$CC22<-ifelse(data96d$CAMEO.Code=="22", 1, 0)
data96d$CC23<-ifelse(data96d$CAMEO.Code=="23", 1, 0)
data96d$CC24<-ifelse(data96d$CAMEO.Code=="24", 1, 0)
data96d$CC25<-ifelse(data96d$CAMEO.Code=="25", 1, 0)
data96d$CC90<-ifelse(data96d$CAMEO.Code=="90", 1, 0)
data96d$CC91<-ifelse(data96d$CAMEO.Code=="91", 1, 0)
data96d$CC92<-ifelse(data96d$CAMEO.Code=="92", 1, 0)
data96d$CC94<-ifelse(data96d$CAMEO.Code=="94", 1, 0)
data96d$CC100<-ifelse(data96d$CAMEO.Code=="100", 1, 0)
data96d$CC101<-ifelse(data96d$CAMEO.Code=="101", 1, 0)
data96d$CC102<-ifelse(data96d$CAMEO.Code=="102", 1, 0)
data96d$CC103<-ifelse(data96d$CAMEO.Code=="103", 1, 0)
data96d$CC104<-ifelse(data96d$CAMEO.Code=="104", 1, 0)
data96d$CC106<-ifelse(data96d$CAMEO.Code=="106", 1, 0)
data96d$CC108<-ifelse(data96d$CAMEO.Code=="108", 1, 0)
data96d$CC111<-ifelse(data96d$CAMEO.Code=="111", 1, 0)
data96d$CC112<-ifelse(data96d$CAMEO.Code=="112", 1, 0)
data96d$CC113<-ifelse(data96d$CAMEO.Code=="113", 1, 0)
data96d$CC114<-ifelse(data96d$CAMEO.Code=="114", 1, 0)
data96d$CC124<-ifelse(data96d$CAMEO.Code=="124", 1, 0)
data96d$CC130<-ifelse(data96d$CAMEO.Code=="130", 1, 0)
data96d$CC131<-ifelse(data96d$CAMEO.Code=="131", 1, 0)
data96d$CC133<-ifelse(data96d$CAMEO.Code=="133", 1, 0)
data96d$CC138<-ifelse(data96d$CAMEO.Code=="138", 1, 0)
data96d$CC139<-ifelse(data96d$CAMEO.Code=="139", 1, 0)
data96d$CC163<-ifelse(data96d$CAMEO.Code=="163", 1, 0)
data96d$CC213<-ifelse(data96d$CAMEO.Code=="213", 1, 0)
data96d$CC214<-ifelse(data96d$CAMEO.Code=="214", 1, 0)
data96d$CC241<-ifelse(data96d$CAMEO.Code=="241", 1, 0)
data96d$CC242<-ifelse(data96d$CAMEO.Code=="242", 1, 0)
data96d$CC243<-ifelse(data96d$CAMEO.Code=="243", 1, 0)
data96d$CC244<-ifelse(data96d$CAMEO.Code=="244", 1, 0)
data96d$CC253<-ifelse(data96d$CAMEO.Code=="253", 1, 0)
data96d$CC255<-ifelse(data96d$CAMEO.Code=="255", 1, 0)
data96d$CC256<-ifelse(data96d$CAMEO.Code=="256", 1, 0)
data96d$CC1041<-ifelse(data96d$CAMEO.Code=="1041", 1, 0)
data96d$CC1042<-ifelse(data96d$CAMEO.Code=="1042", 1, 0)
data96d$CC1043<-ifelse(data96d$CAMEO.Code=="1043", 1, 0)
data96d$CC1053<-ifelse(data96d$CAMEO.Code=="1053", 1, 0)
data96d$CC1056<-ifelse(data96d$CAMEO.Code=="1056", 1, 0)
data96d$CC1121<-ifelse(data96d$CAMEO.Code=="1121", 1, 0)
data96d$CC1122<-ifelse(data96d$CAMEO.Code=="1122", 1, 0)
data96d$CC1123<-ifelse(data96d$CAMEO.Code=="1123", 1, 0)
data96d$CC1124<-ifelse(data96d$CAMEO.Code=="1124", 1, 0)
data96d$CC1125<-ifelse(data96d$CAMEO.Code=="1125", 1, 0)
data96d$CC1242<-ifelse(data96d$CAMEO.Code=="1242", 1, 0)
data96d$CC93 <- ifelse(data96d$CAMEO.Code=="93", 1, 0)
data96d$CC1014 <- ifelse(data96d$CAMEO.Code=="1014", 1, 0)


data96e<-separate(data96d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count1996<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242  ~ Country + Date_1, 
                           FUN=sum, data=data96e)



write.csv(event.count1996, "/Volumes/Lexar/event.counts1996.csv") 

#____________________________

Events97 <- read.csv2("/Volumes/Lexar/Events1997.tab",  sep="\t", header=TRUE)

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
data97d$CC10<-ifelse(data97d$CAMEO.Code=="10", 1, 0)
data97d$CC12<-ifelse(data97d$CAMEO.Code=="12", 1, 0)
data97d$CC20<-ifelse(data97d$CAMEO.Code=="20", 1, 0)
data97d$CC22<-ifelse(data97d$CAMEO.Code=="22", 1, 0)
data97d$CC23<-ifelse(data97d$CAMEO.Code=="23", 1, 0)
data97d$CC24<-ifelse(data97d$CAMEO.Code=="24", 1, 0)
data97d$CC25<-ifelse(data97d$CAMEO.Code=="25", 1, 0)
data97d$CC90<-ifelse(data97d$CAMEO.Code=="90", 1, 0)
data97d$CC91<-ifelse(data97d$CAMEO.Code=="91", 1, 0)
data97d$CC92<-ifelse(data97d$CAMEO.Code=="92", 1, 0)
data97d$CC94<-ifelse(data97d$CAMEO.Code=="94", 1, 0)
data97d$CC100<-ifelse(data97d$CAMEO.Code=="100", 1, 0)
data97d$CC101<-ifelse(data97d$CAMEO.Code=="101", 1, 0)
data97d$CC102<-ifelse(data97d$CAMEO.Code=="102", 1, 0)
data97d$CC103<-ifelse(data97d$CAMEO.Code=="103", 1, 0)
data97d$CC104<-ifelse(data97d$CAMEO.Code=="104", 1, 0)
data97d$CC106<-ifelse(data97d$CAMEO.Code=="106", 1, 0)
data97d$CC108<-ifelse(data97d$CAMEO.Code=="108", 1, 0)
data97d$CC111<-ifelse(data97d$CAMEO.Code=="111", 1, 0)
data97d$CC112<-ifelse(data97d$CAMEO.Code=="112", 1, 0)
data97d$CC113<-ifelse(data97d$CAMEO.Code=="113", 1, 0)
data97d$CC114<-ifelse(data97d$CAMEO.Code=="114", 1, 0)
data97d$CC124<-ifelse(data97d$CAMEO.Code=="124", 1, 0)
data97d$CC130<-ifelse(data97d$CAMEO.Code=="130", 1, 0)
data97d$CC131<-ifelse(data97d$CAMEO.Code=="131", 1, 0)
data97d$CC133<-ifelse(data97d$CAMEO.Code=="133", 1, 0)
data97d$CC138<-ifelse(data97d$CAMEO.Code=="138", 1, 0)
data97d$CC139<-ifelse(data97d$CAMEO.Code=="139", 1, 0)
data97d$CC163<-ifelse(data97d$CAMEO.Code=="163", 1, 0)
data97d$CC213<-ifelse(data97d$CAMEO.Code=="213", 1, 0)
data97d$CC214<-ifelse(data97d$CAMEO.Code=="214", 1, 0)
data97d$CC241<-ifelse(data97d$CAMEO.Code=="241", 1, 0)
data97d$CC242<-ifelse(data97d$CAMEO.Code=="242", 1, 0)
data97d$CC243<-ifelse(data97d$CAMEO.Code=="243", 1, 0)
data97d$CC244<-ifelse(data97d$CAMEO.Code=="244", 1, 0)
data97d$CC253<-ifelse(data97d$CAMEO.Code=="253", 1, 0)
data97d$CC255<-ifelse(data97d$CAMEO.Code=="255", 1, 0)
data97d$CC256<-ifelse(data97d$CAMEO.Code=="256", 1, 0)
data97d$CC1041<-ifelse(data97d$CAMEO.Code=="1041", 1, 0)
data97d$CC1042<-ifelse(data97d$CAMEO.Code=="1042", 1, 0)
data97d$CC1043<-ifelse(data97d$CAMEO.Code=="1043", 1, 0)
data97d$CC1053<-ifelse(data97d$CAMEO.Code=="1053", 1, 0)
data97d$CC1056<-ifelse(data97d$CAMEO.Code=="1056", 1, 0)
data97d$CC1121<-ifelse(data97d$CAMEO.Code=="1121", 1, 0)
data97d$CC1122<-ifelse(data97d$CAMEO.Code=="1122", 1, 0)
data97d$CC1123<-ifelse(data97d$CAMEO.Code=="1123", 1, 0)
data97d$CC1124<-ifelse(data97d$CAMEO.Code=="1124", 1, 0)
data97d$CC1125<-ifelse(data97d$CAMEO.Code=="1125", 1, 0)
data97d$CC1242<-ifelse(data97d$CAMEO.Code=="1242", 1, 0)
data97d$CC93 <- ifelse(data97d$CAMEO.Code=="93", 1, 0)
data97d$CC1014 <- ifelse(data97d$CAMEO.Code=="1014", 1, 0)



data97e<-separate(data97d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count1997<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242  ~ Country + Date_1, 
                           FUN=sum, data=data97e)



write.csv(event.count1997, "/Volumes/Lexar/event.counts1997.csv") 


#____________________________

Events98 <- read.csv2("/Volumes/Lexar/Events1998.tab",  sep="\t", header=TRUE)

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
data98d$CC10<-ifelse(data98d$CAMEO.Code=="10", 1, 0)
data98d$CC12<-ifelse(data98d$CAMEO.Code=="12", 1, 0)
data98d$CC20<-ifelse(data98d$CAMEO.Code=="20", 1, 0)
data98d$CC22<-ifelse(data98d$CAMEO.Code=="22", 1, 0)
data98d$CC23<-ifelse(data98d$CAMEO.Code=="23", 1, 0)
data98d$CC24<-ifelse(data98d$CAMEO.Code=="24", 1, 0)
data98d$CC25<-ifelse(data98d$CAMEO.Code=="25", 1, 0)
data98d$CC90<-ifelse(data98d$CAMEO.Code=="90", 1, 0)
data98d$CC91<-ifelse(data98d$CAMEO.Code=="91", 1, 0)
data98d$CC92<-ifelse(data98d$CAMEO.Code=="92", 1, 0)
data98d$CC94<-ifelse(data98d$CAMEO.Code=="94", 1, 0)
data98d$CC100<-ifelse(data98d$CAMEO.Code=="100", 1, 0)
data98d$CC101<-ifelse(data98d$CAMEO.Code=="101", 1, 0)
data98d$CC102<-ifelse(data98d$CAMEO.Code=="102", 1, 0)
data98d$CC103<-ifelse(data98d$CAMEO.Code=="103", 1, 0)
data98d$CC104<-ifelse(data98d$CAMEO.Code=="104", 1, 0)
data98d$CC106<-ifelse(data98d$CAMEO.Code=="106", 1, 0)
data98d$CC108<-ifelse(data98d$CAMEO.Code=="108", 1, 0)
data98d$CC111<-ifelse(data98d$CAMEO.Code=="111", 1, 0)
data98d$CC112<-ifelse(data98d$CAMEO.Code=="112", 1, 0)
data98d$CC113<-ifelse(data98d$CAMEO.Code=="113", 1, 0)
data98d$CC114<-ifelse(data98d$CAMEO.Code=="114", 1, 0)
data98d$CC124<-ifelse(data98d$CAMEO.Code=="124", 1, 0)
data98d$CC130<-ifelse(data98d$CAMEO.Code=="130", 1, 0)
data98d$CC131<-ifelse(data98d$CAMEO.Code=="131", 1, 0)
data98d$CC133<-ifelse(data98d$CAMEO.Code=="133", 1, 0)
data98d$CC138<-ifelse(data98d$CAMEO.Code=="138", 1, 0)
data98d$CC139<-ifelse(data98d$CAMEO.Code=="139", 1, 0)
data98d$CC163<-ifelse(data98d$CAMEO.Code=="163", 1, 0)
data98d$CC213<-ifelse(data98d$CAMEO.Code=="213", 1, 0)
data98d$CC214<-ifelse(data98d$CAMEO.Code=="214", 1, 0)
data98d$CC241<-ifelse(data98d$CAMEO.Code=="241", 1, 0)
data98d$CC242<-ifelse(data98d$CAMEO.Code=="242", 1, 0)
data98d$CC243<-ifelse(data98d$CAMEO.Code=="243", 1, 0)
data98d$CC244<-ifelse(data98d$CAMEO.Code=="244", 1, 0)
data98d$CC253<-ifelse(data98d$CAMEO.Code=="253", 1, 0)
data98d$CC255<-ifelse(data98d$CAMEO.Code=="255", 1, 0)
data98d$CC256<-ifelse(data98d$CAMEO.Code=="256", 1, 0)
data98d$CC1041<-ifelse(data98d$CAMEO.Code=="1041", 1, 0)
data98d$CC1042<-ifelse(data98d$CAMEO.Code=="1042", 1, 0)
data98d$CC1043<-ifelse(data98d$CAMEO.Code=="1043", 1, 0)
data98d$CC1053<-ifelse(data98d$CAMEO.Code=="1053", 1, 0)
data98d$CC1056<-ifelse(data98d$CAMEO.Code=="1056", 1, 0)
data98d$CC1121<-ifelse(data98d$CAMEO.Code=="1121", 1, 0)
data98d$CC1122<-ifelse(data98d$CAMEO.Code=="1122", 1, 0)
data98d$CC1123<-ifelse(data98d$CAMEO.Code=="1123", 1, 0)
data98d$CC1124<-ifelse(data98d$CAMEO.Code=="1124", 1, 0)
data98d$CC1125<-ifelse(data98d$CAMEO.Code=="1125", 1, 0)
data98d$CC1242<-ifelse(data98d$CAMEO.Code=="1242", 1, 0)
data98d$CC93 <- ifelse(data98d$CAMEO.Code=="93", 1, 0)
data98d$CC1014 <- ifelse(data98d$CAMEO.Code=="1014", 1, 0)



data98e<-separate(data98d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count1998<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242  ~ Country + Date_1, 
                           FUN=sum, data=data98e)



write.csv(event.count1998, "/Volumes/Lexar/event.counts1998.csv") 



#____________________________

Events99 <- read.csv2("/Volumes/Lexar/Events1999.tab",  sep="\t", header=TRUE)

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
data99d$CC10<-ifelse(data99d$CAMEO.Code=="10", 1, 0)
data99d$CC12<-ifelse(data99d$CAMEO.Code=="12", 1, 0)
data99d$CC20<-ifelse(data99d$CAMEO.Code=="20", 1, 0)
data99d$CC22<-ifelse(data99d$CAMEO.Code=="22", 1, 0)
data99d$CC23<-ifelse(data99d$CAMEO.Code=="23", 1, 0)
data99d$CC24<-ifelse(data99d$CAMEO.Code=="24", 1, 0)
data99d$CC25<-ifelse(data99d$CAMEO.Code=="25", 1, 0)
data99d$CC90<-ifelse(data99d$CAMEO.Code=="90", 1, 0)
data99d$CC91<-ifelse(data99d$CAMEO.Code=="91", 1, 0)
data99d$CC92<-ifelse(data99d$CAMEO.Code=="92", 1, 0)
data99d$CC94<-ifelse(data99d$CAMEO.Code=="94", 1, 0)
data99d$CC100<-ifelse(data99d$CAMEO.Code=="100", 1, 0)
data99d$CC101<-ifelse(data99d$CAMEO.Code=="101", 1, 0)
data99d$CC102<-ifelse(data99d$CAMEO.Code=="102", 1, 0)
data99d$CC103<-ifelse(data99d$CAMEO.Code=="103", 1, 0)
data99d$CC104<-ifelse(data99d$CAMEO.Code=="104", 1, 0)
data99d$CC106<-ifelse(data99d$CAMEO.Code=="106", 1, 0)
data99d$CC108<-ifelse(data99d$CAMEO.Code=="108", 1, 0)
data99d$CC111<-ifelse(data99d$CAMEO.Code=="111", 1, 0)
data99d$CC112<-ifelse(data99d$CAMEO.Code=="112", 1, 0)
data99d$CC113<-ifelse(data99d$CAMEO.Code=="113", 1, 0)
data99d$CC114<-ifelse(data99d$CAMEO.Code=="114", 1, 0)
data99d$CC124<-ifelse(data99d$CAMEO.Code=="124", 1, 0)
data99d$CC130<-ifelse(data99d$CAMEO.Code=="130", 1, 0)
data99d$CC131<-ifelse(data99d$CAMEO.Code=="131", 1, 0)
data99d$CC133<-ifelse(data99d$CAMEO.Code=="133", 1, 0)
data99d$CC138<-ifelse(data99d$CAMEO.Code=="138", 1, 0)
data99d$CC139<-ifelse(data99d$CAMEO.Code=="139", 1, 0)
data99d$CC163<-ifelse(data99d$CAMEO.Code=="163", 1, 0)
data99d$CC213<-ifelse(data99d$CAMEO.Code=="213", 1, 0)
data99d$CC214<-ifelse(data99d$CAMEO.Code=="214", 1, 0)
data99d$CC241<-ifelse(data99d$CAMEO.Code=="241", 1, 0)
data99d$CC242<-ifelse(data99d$CAMEO.Code=="242", 1, 0)
data99d$CC243<-ifelse(data99d$CAMEO.Code=="243", 1, 0)
data99d$CC244<-ifelse(data99d$CAMEO.Code=="244", 1, 0)
data99d$CC253<-ifelse(data99d$CAMEO.Code=="253", 1, 0)
data99d$CC255<-ifelse(data99d$CAMEO.Code=="255", 1, 0)
data99d$CC256<-ifelse(data99d$CAMEO.Code=="256", 1, 0)
data99d$CC1041<-ifelse(data99d$CAMEO.Code=="1041", 1, 0)
data99d$CC1042<-ifelse(data99d$CAMEO.Code=="1042", 1, 0)
data99d$CC1043<-ifelse(data99d$CAMEO.Code=="1043", 1, 0)
data99d$CC1053<-ifelse(data99d$CAMEO.Code=="1053", 1, 0)
data99d$CC1056<-ifelse(data99d$CAMEO.Code=="1056", 1, 0)
data99d$CC1121<-ifelse(data99d$CAMEO.Code=="1121", 1, 0)
data99d$CC1122<-ifelse(data99d$CAMEO.Code=="1122", 1, 0)
data99d$CC1123<-ifelse(data99d$CAMEO.Code=="1123", 1, 0)
data99d$CC1124<-ifelse(data99d$CAMEO.Code=="1124", 1, 0)
data99d$CC1125<-ifelse(data99d$CAMEO.Code=="1125", 1, 0)
data99d$CC1242<-ifelse(data99d$CAMEO.Code=="1242", 1, 0)
data99d$CC93 <- ifelse(data99d$CAMEO.Code=="93", 1, 0)
data99d$CC1014 <- ifelse(data99d$CAMEO.Code=="1014", 1, 0)



data99e<-separate(data99d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count1999<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242  ~ Country + Date_1, 
                           FUN=sum, data=data99e)



write.csv(event.count1999, "/Volumes/Lexar/event.counts1999.csv") 



#_____________________

Events00 <- read.csv2("/Volumes/Lexar/Events2000.tab",  sep="\t", header=TRUE)

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
data00d$CC10<-ifelse(data00d$CAMEO.Code=="10", 1, 0)
data00d$CC12<-ifelse(data00d$CAMEO.Code=="12", 1, 0)
data00d$CC20<-ifelse(data00d$CAMEO.Code=="20", 1, 0)
data00d$CC22<-ifelse(data00d$CAMEO.Code=="22", 1, 0)
data00d$CC23<-ifelse(data00d$CAMEO.Code=="23", 1, 0)
data00d$CC24<-ifelse(data00d$CAMEO.Code=="24", 1, 0)
data00d$CC25<-ifelse(data00d$CAMEO.Code=="25", 1, 0)
data00d$CC90<-ifelse(data00d$CAMEO.Code=="90", 1, 0)
data00d$CC91<-ifelse(data00d$CAMEO.Code=="91", 1, 0)
data00d$CC92<-ifelse(data00d$CAMEO.Code=="92", 1, 0)
data00d$CC94<-ifelse(data00d$CAMEO.Code=="94", 1, 0)
data00d$CC100<-ifelse(data00d$CAMEO.Code=="100", 1, 0)
data00d$CC101<-ifelse(data00d$CAMEO.Code=="101", 1, 0)
data00d$CC102<-ifelse(data00d$CAMEO.Code=="102", 1, 0)
data00d$CC103<-ifelse(data00d$CAMEO.Code=="103", 1, 0)
data00d$CC104<-ifelse(data00d$CAMEO.Code=="104", 1, 0)
data00d$CC106<-ifelse(data00d$CAMEO.Code=="106", 1, 0)
data00d$CC108<-ifelse(data00d$CAMEO.Code=="108", 1, 0)
data00d$CC111<-ifelse(data00d$CAMEO.Code=="111", 1, 0)
data00d$CC112<-ifelse(data00d$CAMEO.Code=="112", 1, 0)
data00d$CC113<-ifelse(data00d$CAMEO.Code=="113", 1, 0)
data00d$CC114<-ifelse(data00d$CAMEO.Code=="114", 1, 0)
data00d$CC124<-ifelse(data00d$CAMEO.Code=="124", 1, 0)
data00d$CC130<-ifelse(data00d$CAMEO.Code=="130", 1, 0)
data00d$CC131<-ifelse(data00d$CAMEO.Code=="131", 1, 0)
data00d$CC133<-ifelse(data00d$CAMEO.Code=="133", 1, 0)
data00d$CC138<-ifelse(data00d$CAMEO.Code=="138", 1, 0)
data00d$CC139<-ifelse(data00d$CAMEO.Code=="139", 1, 0)
data00d$CC163<-ifelse(data00d$CAMEO.Code=="163", 1, 0)
data00d$CC213<-ifelse(data00d$CAMEO.Code=="213", 1, 0)
data00d$CC214<-ifelse(data00d$CAMEO.Code=="214", 1, 0)
data00d$CC241<-ifelse(data00d$CAMEO.Code=="241", 1, 0)
data00d$CC242<-ifelse(data00d$CAMEO.Code=="242", 1, 0)
data00d$CC243<-ifelse(data00d$CAMEO.Code=="243", 1, 0)
data00d$CC244<-ifelse(data00d$CAMEO.Code=="244", 1, 0)
data00d$CC253<-ifelse(data00d$CAMEO.Code=="253", 1, 0)
data00d$CC255<-ifelse(data00d$CAMEO.Code=="255", 1, 0)
data00d$CC256<-ifelse(data00d$CAMEO.Code=="256", 1, 0)
data00d$CC1041<-ifelse(data00d$CAMEO.Code=="1041", 1, 0)
data00d$CC1042<-ifelse(data00d$CAMEO.Code=="1042", 1, 0)
data00d$CC1043<-ifelse(data00d$CAMEO.Code=="1043", 1, 0)
data00d$CC1053<-ifelse(data00d$CAMEO.Code=="1053", 1, 0)
data00d$CC1056<-ifelse(data00d$CAMEO.Code=="1056", 1, 0)
data00d$CC1121<-ifelse(data00d$CAMEO.Code=="1121", 1, 0)
data00d$CC1122<-ifelse(data00d$CAMEO.Code=="1122", 1, 0)
data00d$CC1123<-ifelse(data00d$CAMEO.Code=="1123", 1, 0)
data00d$CC1124<-ifelse(data00d$CAMEO.Code=="1124", 1, 0)
data00d$CC1125<-ifelse(data00d$CAMEO.Code=="1125", 1, 0)
data00d$CC1242<-ifelse(data00d$CAMEO.Code=="1242", 1, 0)
data00d$CC93 <- ifelse(data00d$CAMEO.Code=="93", 1, 0)
data00d$CC1014 <- ifelse(data00d$CAMEO.Code=="1014", 1, 0)



data00e<-separate(data00d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count2000<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242  ~ Country + Date_1, 
                           FUN=sum, data=data00e)



write.csv(event.count2000, "/Volumes/Lexar/event.counts2000.csv") 



#______________________


Events01<- read.csv2("/Volumes/Lexar/Events2001.tab",  sep="\t", header=TRUE)

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
data01d$CC10<-ifelse(data01d$CAMEO.Code=="10", 1, 0)
data01d$CC12<-ifelse(data01d$CAMEO.Code=="12", 1, 0)
data01d$CC20<-ifelse(data01d$CAMEO.Code=="20", 1, 0)
data01d$CC22<-ifelse(data01d$CAMEO.Code=="22", 1, 0)
data01d$CC23<-ifelse(data01d$CAMEO.Code=="23", 1, 0)
data01d$CC24<-ifelse(data01d$CAMEO.Code=="24", 1, 0)
data01d$CC25<-ifelse(data01d$CAMEO.Code=="25", 1, 0)
data01d$CC90<-ifelse(data01d$CAMEO.Code=="90", 1, 0)
data01d$CC91<-ifelse(data01d$CAMEO.Code=="91", 1, 0)
data01d$CC92<-ifelse(data01d$CAMEO.Code=="92", 1, 0)
data01d$CC94<-ifelse(data01d$CAMEO.Code=="94", 1, 0)
data01d$CC100<-ifelse(data01d$CAMEO.Code=="100", 1, 0)
data01d$CC101<-ifelse(data01d$CAMEO.Code=="101", 1, 0)
data01d$CC102<-ifelse(data01d$CAMEO.Code=="102", 1, 0)
data01d$CC103<-ifelse(data01d$CAMEO.Code=="103", 1, 0)
data01d$CC104<-ifelse(data01d$CAMEO.Code=="104", 1, 0)
data01d$CC106<-ifelse(data01d$CAMEO.Code=="106", 1, 0)
data01d$CC108<-ifelse(data01d$CAMEO.Code=="108", 1, 0)
data01d$CC111<-ifelse(data01d$CAMEO.Code=="111", 1, 0)
data01d$CC112<-ifelse(data01d$CAMEO.Code=="112", 1, 0)
data01d$CC113<-ifelse(data01d$CAMEO.Code=="113", 1, 0)
data01d$CC114<-ifelse(data01d$CAMEO.Code=="114", 1, 0)
data01d$CC124<-ifelse(data01d$CAMEO.Code=="124", 1, 0)
data01d$CC130<-ifelse(data01d$CAMEO.Code=="130", 1, 0)
data01d$CC131<-ifelse(data01d$CAMEO.Code=="131", 1, 0)
data01d$CC133<-ifelse(data01d$CAMEO.Code=="133", 1, 0)
data01d$CC138<-ifelse(data01d$CAMEO.Code=="138", 1, 0)
data01d$CC139<-ifelse(data01d$CAMEO.Code=="139", 1, 0)
data01d$CC163<-ifelse(data01d$CAMEO.Code=="163", 1, 0)
data01d$CC213<-ifelse(data01d$CAMEO.Code=="213", 1, 0)
data01d$CC214<-ifelse(data01d$CAMEO.Code=="214", 1, 0)
data01d$CC241<-ifelse(data01d$CAMEO.Code=="241", 1, 0)
data01d$CC242<-ifelse(data01d$CAMEO.Code=="242", 1, 0)
data01d$CC243<-ifelse(data01d$CAMEO.Code=="243", 1, 0)
data01d$CC244<-ifelse(data01d$CAMEO.Code=="244", 1, 0)
data01d$CC253<-ifelse(data01d$CAMEO.Code=="253", 1, 0)
data01d$CC255<-ifelse(data01d$CAMEO.Code=="255", 1, 0)
data01d$CC256<-ifelse(data01d$CAMEO.Code=="256", 1, 0)
data01d$CC1041<-ifelse(data01d$CAMEO.Code=="1041", 1, 0)
data01d$CC1042<-ifelse(data01d$CAMEO.Code=="1042", 1, 0)
data01d$CC1043<-ifelse(data01d$CAMEO.Code=="1043", 1, 0)
data01d$CC1053<-ifelse(data01d$CAMEO.Code=="1053", 1, 0)
data01d$CC1056<-ifelse(data01d$CAMEO.Code=="1056", 1, 0)
data01d$CC1121<-ifelse(data01d$CAMEO.Code=="1121", 1, 0)
data01d$CC1122<-ifelse(data01d$CAMEO.Code=="1122", 1, 0)
data01d$CC1123<-ifelse(data01d$CAMEO.Code=="1123", 1, 0)
data01d$CC1124<-ifelse(data01d$CAMEO.Code=="1124", 1, 0)
data01d$CC1125<-ifelse(data01d$CAMEO.Code=="1125", 1, 0)
data01d$CC1242<-ifelse(data01d$CAMEO.Code=="1242", 1, 0)
data01d$CC93 <- ifelse(data01d$CAMEO.Code=="93", 1, 0)
data01d$CC1014 <- ifelse(data01d$CAMEO.Code=="1014", 1, 0)




data01e<-separate(data01d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count2001<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242  ~ Country + Date_1, 
                           FUN=sum, data=data01e)



write.csv(event.count2001, "/Volumes/Lexar/event.counts2001.csv") 



#__________________________

Events02<- read.csv2("/Volumes/Lexar/Events2002.tab",  sep="\t", header=TRUE)

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
data02d$CC10<-ifelse(data02d$CAMEO.Code=="10", 1, 0)
data02d$CC12<-ifelse(data02d$CAMEO.Code=="12", 1, 0)
data02d$CC20<-ifelse(data02d$CAMEO.Code=="20", 1, 0)
data02d$CC22<-ifelse(data02d$CAMEO.Code=="22", 1, 0)
data02d$CC23<-ifelse(data02d$CAMEO.Code=="23", 1, 0)
data02d$CC24<-ifelse(data02d$CAMEO.Code=="24", 1, 0)
data02d$CC25<-ifelse(data02d$CAMEO.Code=="25", 1, 0)
data02d$CC90<-ifelse(data02d$CAMEO.Code=="90", 1, 0)
data02d$CC91<-ifelse(data02d$CAMEO.Code=="91", 1, 0)
data02d$CC92<-ifelse(data02d$CAMEO.Code=="92", 1, 0)
data02d$CC94<-ifelse(data02d$CAMEO.Code=="94", 1, 0)
data02d$CC100<-ifelse(data02d$CAMEO.Code=="100", 1, 0)
data02d$CC101<-ifelse(data02d$CAMEO.Code=="101", 1, 0)
data02d$CC102<-ifelse(data02d$CAMEO.Code=="102", 1, 0)
data02d$CC103<-ifelse(data02d$CAMEO.Code=="103", 1, 0)
data02d$CC104<-ifelse(data02d$CAMEO.Code=="104", 1, 0)
data02d$CC106<-ifelse(data02d$CAMEO.Code=="106", 1, 0)
data02d$CC108<-ifelse(data02d$CAMEO.Code=="108", 1, 0)
data02d$CC111<-ifelse(data02d$CAMEO.Code=="111", 1, 0)
data02d$CC112<-ifelse(data02d$CAMEO.Code=="112", 1, 0)
data02d$CC113<-ifelse(data02d$CAMEO.Code=="113", 1, 0)
data02d$CC114<-ifelse(data02d$CAMEO.Code=="114", 1, 0)
data02d$CC124<-ifelse(data02d$CAMEO.Code=="124", 1, 0)
data02d$CC130<-ifelse(data02d$CAMEO.Code=="130", 1, 0)
data02d$CC131<-ifelse(data02d$CAMEO.Code=="131", 1, 0)
data02d$CC133<-ifelse(data02d$CAMEO.Code=="133", 1, 0)
data02d$CC138<-ifelse(data02d$CAMEO.Code=="138", 1, 0)
data02d$CC139<-ifelse(data02d$CAMEO.Code=="139", 1, 0)
data02d$CC163<-ifelse(data02d$CAMEO.Code=="163", 1, 0)
data02d$CC213<-ifelse(data02d$CAMEO.Code=="213", 1, 0)
data02d$CC214<-ifelse(data02d$CAMEO.Code=="214", 1, 0)
data02d$CC241<-ifelse(data02d$CAMEO.Code=="241", 1, 0)
data02d$CC242<-ifelse(data02d$CAMEO.Code=="242", 1, 0)
data02d$CC243<-ifelse(data02d$CAMEO.Code=="243", 1, 0)
data02d$CC244<-ifelse(data02d$CAMEO.Code=="244", 1, 0)
data02d$CC253<-ifelse(data02d$CAMEO.Code=="253", 1, 0)
data02d$CC255<-ifelse(data02d$CAMEO.Code=="255", 1, 0)
data02d$CC256<-ifelse(data02d$CAMEO.Code=="256", 1, 0)
data02d$CC1041<-ifelse(data02d$CAMEO.Code=="1041", 1, 0)
data02d$CC1042<-ifelse(data02d$CAMEO.Code=="1042", 1, 0)
data02d$CC1043<-ifelse(data02d$CAMEO.Code=="1043", 1, 0)
data02d$CC1053<-ifelse(data02d$CAMEO.Code=="1053", 1, 0)
data02d$CC1056<-ifelse(data02d$CAMEO.Code=="1056", 1, 0)
data02d$CC1121<-ifelse(data02d$CAMEO.Code=="1121", 1, 0)
data02d$CC1122<-ifelse(data02d$CAMEO.Code=="1122", 1, 0)
data02d$CC1123<-ifelse(data02d$CAMEO.Code=="1123", 1, 0)
data02d$CC1124<-ifelse(data02d$CAMEO.Code=="1124", 1, 0)
data02d$CC1125<-ifelse(data02d$CAMEO.Code=="1125", 1, 0)
data02d$CC1242<-ifelse(data02d$CAMEO.Code=="1242", 1, 0)
data02d$CC93 <- ifelse(data02d$CAMEO.Code=="93", 1, 0)
data02d$CC1014 <- ifelse(data02d$CAMEO.Code=="1014", 1, 0)



data02e<-separate(data02d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count2002<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242  ~ Country + Date_1, 
                           FUN=sum, data=data02e)



write.csv(event.count2002, "E:/event.counts2002.csv") 



#________________________

Events03 <- read.csv2("/Volumes/Lexar/Events2003.tab",  sep="\t", header=TRUE)

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
data03d$CC10<-ifelse(data03d$CAMEO.Code=="10", 1, 0)
data03d$CC12<-ifelse(data03d$CAMEO.Code=="12", 1, 0)
data03d$CC20<-ifelse(data03d$CAMEO.Code=="20", 1, 0)
data03d$CC22<-ifelse(data03d$CAMEO.Code=="22", 1, 0)
data03d$CC23<-ifelse(data03d$CAMEO.Code=="23", 1, 0)
data03d$CC24<-ifelse(data03d$CAMEO.Code=="24", 1, 0)
data03d$CC25<-ifelse(data03d$CAMEO.Code=="25", 1, 0)
data03d$CC90<-ifelse(data03d$CAMEO.Code=="90", 1, 0)
data03d$CC91<-ifelse(data03d$CAMEO.Code=="91", 1, 0)
data03d$CC92<-ifelse(data03d$CAMEO.Code=="92", 1, 0)
data03d$CC94<-ifelse(data03d$CAMEO.Code=="94", 1, 0)
data03d$CC100<-ifelse(data03d$CAMEO.Code=="100", 1, 0)
data03d$CC101<-ifelse(data03d$CAMEO.Code=="101", 1, 0)
data03d$CC102<-ifelse(data03d$CAMEO.Code=="102", 1, 0)
data03d$CC103<-ifelse(data03d$CAMEO.Code=="103", 1, 0)
data03d$CC104<-ifelse(data03d$CAMEO.Code=="104", 1, 0)
data03d$CC106<-ifelse(data03d$CAMEO.Code=="106", 1, 0)
data03d$CC108<-ifelse(data03d$CAMEO.Code=="108", 1, 0)
data03d$CC111<-ifelse(data03d$CAMEO.Code=="111", 1, 0)
data03d$CC112<-ifelse(data03d$CAMEO.Code=="112", 1, 0)
data03d$CC113<-ifelse(data03d$CAMEO.Code=="113", 1, 0)
data03d$CC114<-ifelse(data03d$CAMEO.Code=="114", 1, 0)
data03d$CC124<-ifelse(data03d$CAMEO.Code=="124", 1, 0)
data03d$CC130<-ifelse(data03d$CAMEO.Code=="130", 1, 0)
data03d$CC131<-ifelse(data03d$CAMEO.Code=="131", 1, 0)
data03d$CC133<-ifelse(data03d$CAMEO.Code=="133", 1, 0)
data03d$CC138<-ifelse(data03d$CAMEO.Code=="138", 1, 0)
data03d$CC139<-ifelse(data03d$CAMEO.Code=="139", 1, 0)
data03d$CC163<-ifelse(data03d$CAMEO.Code=="163", 1, 0)
data03d$CC213<-ifelse(data03d$CAMEO.Code=="213", 1, 0)
data03d$CC214<-ifelse(data03d$CAMEO.Code=="214", 1, 0)
data03d$CC241<-ifelse(data03d$CAMEO.Code=="241", 1, 0)
data03d$CC242<-ifelse(data03d$CAMEO.Code=="242", 1, 0)
data03d$CC243<-ifelse(data03d$CAMEO.Code=="243", 1, 0)
data03d$CC244<-ifelse(data03d$CAMEO.Code=="244", 1, 0)
data03d$CC253<-ifelse(data03d$CAMEO.Code=="253", 1, 0)
data03d$CC255<-ifelse(data03d$CAMEO.Code=="255", 1, 0)
data03d$CC256<-ifelse(data03d$CAMEO.Code=="256", 1, 0)
data03d$CC1041<-ifelse(data03d$CAMEO.Code=="1041", 1, 0)
data03d$CC1042<-ifelse(data03d$CAMEO.Code=="1042", 1, 0)
data03d$CC1043<-ifelse(data03d$CAMEO.Code=="1043", 1, 0)
data03d$CC1053<-ifelse(data03d$CAMEO.Code=="1053", 1, 0)
data03d$CC1056<-ifelse(data03d$CAMEO.Code=="1056", 1, 0)
data03d$CC1121<-ifelse(data03d$CAMEO.Code=="1121", 1, 0)
data03d$CC1122<-ifelse(data03d$CAMEO.Code=="1122", 1, 0)
data03d$CC1123<-ifelse(data03d$CAMEO.Code=="1123", 1, 0)
data03d$CC1124<-ifelse(data03d$CAMEO.Code=="1124", 1, 0)
data03d$CC1125<-ifelse(data03d$CAMEO.Code=="1125", 1, 0)
data03d$CC1242<-ifelse(data03d$CAMEO.Code=="1242", 1, 0)
data03d$CC93 <- ifelse(data03d$CAMEO.Code=="93", 1, 0)
data03d$CC1014 <- ifelse(data03d$CAMEO.Code=="1014", 1, 0)



data03e<-separate(data03d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count2003<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242  ~ Country + Date_1, 
                           FUN=sum, data=data03e)



write.csv(event.count2003, "/Volumes/Lexar/event.counts2003.csv")
##2004

Events04 <- read.csv2("/Volumes/Lexar/Events2004.tab",  sep="\t", header=TRUE)

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
data04d$CC10<-ifelse(data04d$CAMEO.Code=="10", 1, 0)
data04d$CC12<-ifelse(data04d$CAMEO.Code=="12", 1, 0)
data04d$CC20<-ifelse(data04d$CAMEO.Code=="20", 1, 0)
data04d$CC22<-ifelse(data04d$CAMEO.Code=="22", 1, 0)
data04d$CC23<-ifelse(data04d$CAMEO.Code=="23", 1, 0)
data04d$CC24<-ifelse(data04d$CAMEO.Code=="24", 1, 0)
data04d$CC25<-ifelse(data04d$CAMEO.Code=="25", 1, 0)
data04d$CC90<-ifelse(data04d$CAMEO.Code=="90", 1, 0)
data04d$CC91<-ifelse(data04d$CAMEO.Code=="91", 1, 0)
data04d$CC92<-ifelse(data04d$CAMEO.Code=="92", 1, 0)
data04d$CC94<-ifelse(data04d$CAMEO.Code=="94", 1, 0)
data04d$CC100<-ifelse(data04d$CAMEO.Code=="100", 1, 0)
data04d$CC101<-ifelse(data04d$CAMEO.Code=="101", 1, 0)
data04d$CC102<-ifelse(data04d$CAMEO.Code=="102", 1, 0)
data04d$CC103<-ifelse(data04d$CAMEO.Code=="103", 1, 0)
data04d$CC104<-ifelse(data04d$CAMEO.Code=="104", 1, 0)
data04d$CC106<-ifelse(data04d$CAMEO.Code=="106", 1, 0)
data04d$CC108<-ifelse(data04d$CAMEO.Code=="108", 1, 0)
data04d$CC111<-ifelse(data04d$CAMEO.Code=="111", 1, 0)
data04d$CC112<-ifelse(data04d$CAMEO.Code=="112", 1, 0)
data04d$CC113<-ifelse(data04d$CAMEO.Code=="113", 1, 0)
data04d$CC114<-ifelse(data04d$CAMEO.Code=="114", 1, 0)
data04d$CC124<-ifelse(data04d$CAMEO.Code=="124", 1, 0)
data04d$CC130<-ifelse(data04d$CAMEO.Code=="130", 1, 0)
data04d$CC131<-ifelse(data04d$CAMEO.Code=="131", 1, 0)
data04d$CC133<-ifelse(data04d$CAMEO.Code=="133", 1, 0)
data04d$CC138<-ifelse(data04d$CAMEO.Code=="138", 1, 0)
data04d$CC139<-ifelse(data04d$CAMEO.Code=="139", 1, 0)
data04d$CC163<-ifelse(data04d$CAMEO.Code=="163", 1, 0)
data04d$CC213<-ifelse(data04d$CAMEO.Code=="213", 1, 0)
data04d$CC214<-ifelse(data04d$CAMEO.Code=="214", 1, 0)
data04d$CC241<-ifelse(data04d$CAMEO.Code=="241", 1, 0)
data04d$CC242<-ifelse(data04d$CAMEO.Code=="242", 1, 0)
data04d$CC243<-ifelse(data04d$CAMEO.Code=="243", 1, 0)
data04d$CC244<-ifelse(data04d$CAMEO.Code=="244", 1, 0)
data04d$CC253<-ifelse(data04d$CAMEO.Code=="253", 1, 0)
data04d$CC255<-ifelse(data04d$CAMEO.Code=="255", 1, 0)
data04d$CC256<-ifelse(data04d$CAMEO.Code=="256", 1, 0)
data04d$CC1041<-ifelse(data04d$CAMEO.Code=="1041", 1, 0)
data04d$CC1042<-ifelse(data04d$CAMEO.Code=="1042", 1, 0)
data04d$CC1043<-ifelse(data04d$CAMEO.Code=="1043", 1, 0)
data04d$CC1053<-ifelse(data04d$CAMEO.Code=="1053", 1, 0)
data04d$CC1056<-ifelse(data04d$CAMEO.Code=="1056", 1, 0)
data04d$CC1121<-ifelse(data04d$CAMEO.Code=="1121", 1, 0)
data04d$CC1122<-ifelse(data04d$CAMEO.Code=="1122", 1, 0)
data04d$CC1123<-ifelse(data04d$CAMEO.Code=="1123", 1, 0)
data04d$CC1124<-ifelse(data04d$CAMEO.Code=="1124", 1, 0)
data04d$CC1125<-ifelse(data04d$CAMEO.Code=="1125", 1, 0)
data04d$CC1242<-ifelse(data04d$CAMEO.Code=="1242", 1, 0)
data04d$CC93 <- ifelse(data04d$CAMEO.Code=="93", 1, 0)
data04d$CC1014 <- ifelse(data04d$CAMEO.Code=="1014", 1, 0)



data04e<-separate(data04d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count2004<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242  ~ Country + Date_1, 
                           FUN=sum, data=data04e)



write.csv(event.count2004, "/Volumes/Lexar/event.counts2004.csv")

##2005

Events05 <- read.csv2("/Volumes/Lexar/Events2005.tab",  sep="\t", header=TRUE)

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
data05d$CC10<-ifelse(data05d$CAMEO.Code=="10", 1, 0)
data05d$CC12<-ifelse(data05d$CAMEO.Code=="12", 1, 0)
data05d$CC20<-ifelse(data05d$CAMEO.Code=="20", 1, 0)
data05d$CC22<-ifelse(data05d$CAMEO.Code=="22", 1, 0)
data05d$CC23<-ifelse(data05d$CAMEO.Code=="23", 1, 0)
data05d$CC24<-ifelse(data05d$CAMEO.Code=="24", 1, 0)
data05d$CC25<-ifelse(data05d$CAMEO.Code=="25", 1, 0)
data05d$CC90<-ifelse(data05d$CAMEO.Code=="90", 1, 0)
data05d$CC91<-ifelse(data05d$CAMEO.Code=="91", 1, 0)
data05d$CC92<-ifelse(data05d$CAMEO.Code=="92", 1, 0)
data05d$CC94<-ifelse(data05d$CAMEO.Code=="94", 1, 0)
data05d$CC100<-ifelse(data05d$CAMEO.Code=="100", 1, 0)
data05d$CC101<-ifelse(data05d$CAMEO.Code=="101", 1, 0)
data05d$CC102<-ifelse(data05d$CAMEO.Code=="102", 1, 0)
data05d$CC103<-ifelse(data05d$CAMEO.Code=="103", 1, 0)
data05d$CC104<-ifelse(data05d$CAMEO.Code=="104", 1, 0)
data05d$CC106<-ifelse(data05d$CAMEO.Code=="106", 1, 0)
data05d$CC108<-ifelse(data05d$CAMEO.Code=="108", 1, 0)
data05d$CC111<-ifelse(data05d$CAMEO.Code=="111", 1, 0)
data05d$CC112<-ifelse(data05d$CAMEO.Code=="112", 1, 0)
data05d$CC113<-ifelse(data05d$CAMEO.Code=="113", 1, 0)
data05d$CC114<-ifelse(data05d$CAMEO.Code=="114", 1, 0)
data05d$CC124<-ifelse(data05d$CAMEO.Code=="124", 1, 0)
data05d$CC130<-ifelse(data05d$CAMEO.Code=="130", 1, 0)
data05d$CC131<-ifelse(data05d$CAMEO.Code=="131", 1, 0)
data05d$CC133<-ifelse(data05d$CAMEO.Code=="133", 1, 0)
data05d$CC138<-ifelse(data05d$CAMEO.Code=="138", 1, 0)
data05d$CC139<-ifelse(data05d$CAMEO.Code=="139", 1, 0)
data05d$CC163<-ifelse(data05d$CAMEO.Code=="163", 1, 0)
data05d$CC213<-ifelse(data05d$CAMEO.Code=="213", 1, 0)
data05d$CC214<-ifelse(data05d$CAMEO.Code=="214", 1, 0)
data05d$CC241<-ifelse(data05d$CAMEO.Code=="241", 1, 0)
data05d$CC242<-ifelse(data05d$CAMEO.Code=="242", 1, 0)
data05d$CC243<-ifelse(data05d$CAMEO.Code=="243", 1, 0)
data05d$CC244<-ifelse(data05d$CAMEO.Code=="244", 1, 0)
data05d$CC253<-ifelse(data05d$CAMEO.Code=="253", 1, 0)
data05d$CC255<-ifelse(data05d$CAMEO.Code=="255", 1, 0)
data05d$CC256<-ifelse(data05d$CAMEO.Code=="256", 1, 0)
data05d$CC1041<-ifelse(data05d$CAMEO.Code=="1041", 1, 0)
data05d$CC1042<-ifelse(data05d$CAMEO.Code=="1042", 1, 0)
data05d$CC1043<-ifelse(data05d$CAMEO.Code=="1043", 1, 0)
data05d$CC1053<-ifelse(data05d$CAMEO.Code=="1053", 1, 0)
data05d$CC1056<-ifelse(data05d$CAMEO.Code=="1056", 1, 0)
data05d$CC1121<-ifelse(data05d$CAMEO.Code=="1121", 1, 0)
data05d$CC1122<-ifelse(data05d$CAMEO.Code=="1122", 1, 0)
data05d$CC1123<-ifelse(data05d$CAMEO.Code=="1123", 1, 0)
data05d$CC1124<-ifelse(data05d$CAMEO.Code=="1124", 1, 0)
data05d$CC1125<-ifelse(data05d$CAMEO.Code=="1125", 1, 0)
data05d$CC1242<-ifelse(data05d$CAMEO.Code=="1242", 1, 0)
data05d$CC93 <- ifelse(data05d$CAMEO.Code=="93", 1, 0)
data05d$CC1014 <- ifelse(data05d$CAMEO.Code=="1014", 1, 0)



data05e<-separate(data05d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count2005<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242  ~ Country + Date_1, 
                           FUN=sum, data=data05e)



write.csv(event.count2005, "/Volumes/Lexar/event.counts2005.csv")

##2006

Events2006 <- read.csv2("/Volumes/Lexar/Events2006.tab",  sep="\t", header=TRUE)

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
data06d$CC10<-ifelse(data06d$CAMEO.Code=="10", 1, 0)
data06d$CC12<-ifelse(data06d$CAMEO.Code=="12", 1, 0)
data06d$CC20<-ifelse(data06d$CAMEO.Code=="20", 1, 0)
data06d$CC22<-ifelse(data06d$CAMEO.Code=="22", 1, 0)
data06d$CC23<-ifelse(data06d$CAMEO.Code=="23", 1, 0)
data06d$CC24<-ifelse(data06d$CAMEO.Code=="24", 1, 0)
data06d$CC25<-ifelse(data06d$CAMEO.Code=="25", 1, 0)
data06d$CC90<-ifelse(data06d$CAMEO.Code=="90", 1, 0)
data06d$CC91<-ifelse(data06d$CAMEO.Code=="91", 1, 0)
data06d$CC92<-ifelse(data06d$CAMEO.Code=="92", 1, 0)
data06d$CC94<-ifelse(data06d$CAMEO.Code=="94", 1, 0)
data06d$CC100<-ifelse(data06d$CAMEO.Code=="100", 1, 0)
data06d$CC101<-ifelse(data06d$CAMEO.Code=="101", 1, 0)
data06d$CC102<-ifelse(data06d$CAMEO.Code=="102", 1, 0)
data06d$CC103<-ifelse(data06d$CAMEO.Code=="103", 1, 0)
data06d$CC104<-ifelse(data06d$CAMEO.Code=="104", 1, 0)
data06d$CC106<-ifelse(data06d$CAMEO.Code=="106", 1, 0)
data06d$CC108<-ifelse(data06d$CAMEO.Code=="108", 1, 0)
data06d$CC111<-ifelse(data06d$CAMEO.Code=="111", 1, 0)
data06d$CC112<-ifelse(data06d$CAMEO.Code=="112", 1, 0)
data06d$CC113<-ifelse(data06d$CAMEO.Code=="113", 1, 0)
data06d$CC114<-ifelse(data06d$CAMEO.Code=="114", 1, 0)
data06d$CC124<-ifelse(data06d$CAMEO.Code=="124", 1, 0)
data06d$CC130<-ifelse(data06d$CAMEO.Code=="130", 1, 0)
data06d$CC131<-ifelse(data06d$CAMEO.Code=="131", 1, 0)
data06d$CC133<-ifelse(data06d$CAMEO.Code=="133", 1, 0)
data06d$CC138<-ifelse(data06d$CAMEO.Code=="138", 1, 0)
data06d$CC139<-ifelse(data06d$CAMEO.Code=="139", 1, 0)
data06d$CC163<-ifelse(data06d$CAMEO.Code=="163", 1, 0)
data06d$CC213<-ifelse(data06d$CAMEO.Code=="213", 1, 0)
data06d$CC214<-ifelse(data06d$CAMEO.Code=="214", 1, 0)
data06d$CC241<-ifelse(data06d$CAMEO.Code=="241", 1, 0)
data06d$CC242<-ifelse(data06d$CAMEO.Code=="242", 1, 0)
data06d$CC243<-ifelse(data06d$CAMEO.Code=="243", 1, 0)
data06d$CC244<-ifelse(data06d$CAMEO.Code=="244", 1, 0)
data06d$CC253<-ifelse(data06d$CAMEO.Code=="253", 1, 0)
data06d$CC255<-ifelse(data06d$CAMEO.Code=="255", 1, 0)
data06d$CC256<-ifelse(data06d$CAMEO.Code=="256", 1, 0)
data06d$CC1041<-ifelse(data06d$CAMEO.Code=="1041", 1, 0)
data06d$CC1042<-ifelse(data06d$CAMEO.Code=="1042", 1, 0)
data06d$CC1043<-ifelse(data06d$CAMEO.Code=="1043", 1, 0)
data06d$CC1053<-ifelse(data06d$CAMEO.Code=="1053", 1, 0)
data06d$CC1056<-ifelse(data06d$CAMEO.Code=="1056", 1, 0)
data06d$CC1121<-ifelse(data06d$CAMEO.Code=="1121", 1, 0)
data06d$CC1122<-ifelse(data06d$CAMEO.Code=="1122", 1, 0)
data06d$CC1123<-ifelse(data06d$CAMEO.Code=="1123", 1, 0)
data06d$CC1124<-ifelse(data06d$CAMEO.Code=="1124", 1, 0)
data06d$CC1125<-ifelse(data06d$CAMEO.Code=="1125", 1, 0)
data06d$CC1242<-ifelse(data06d$CAMEO.Code=="1242", 1, 0)
data06d$CC93 <- ifelse(data06d$CAMEO.Code=="93", 1, 0)
data06d$CC1014 <- ifelse(data06d$CAMEO.Code=="1014", 1, 0)


data06e<-separate(data06d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count2006<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242  ~ Country + Date_1, 
                           FUN=sum, data=data06e)



write.csv(event.count2006, "/Volumes/Lexar/event.counts2006.csv")

##2007

Events2007 <- read.csv2("/Volumes/Lexar/Events2007.tab",  sep="\t", header=TRUE)

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
data07d$CC10<-ifelse(data07d$CAMEO.Code=="10", 1, 0)
data07d$CC12<-ifelse(data07d$CAMEO.Code=="12", 1, 0)
data07d$CC20<-ifelse(data07d$CAMEO.Code=="20", 1, 0)
data07d$CC22<-ifelse(data07d$CAMEO.Code=="22", 1, 0)
data07d$CC23<-ifelse(data07d$CAMEO.Code=="23", 1, 0)
data07d$CC24<-ifelse(data07d$CAMEO.Code=="24", 1, 0)
data07d$CC25<-ifelse(data07d$CAMEO.Code=="25", 1, 0)
data07d$CC90<-ifelse(data07d$CAMEO.Code=="90", 1, 0)
data07d$CC91<-ifelse(data07d$CAMEO.Code=="91", 1, 0)
data07d$CC92<-ifelse(data07d$CAMEO.Code=="92", 1, 0)
data07d$CC94<-ifelse(data07d$CAMEO.Code=="94", 1, 0)
data07d$CC100<-ifelse(data07d$CAMEO.Code=="100", 1, 0)
data07d$CC101<-ifelse(data07d$CAMEO.Code=="101", 1, 0)
data07d$CC102<-ifelse(data07d$CAMEO.Code=="102", 1, 0)
data07d$CC103<-ifelse(data07d$CAMEO.Code=="103", 1, 0)
data07d$CC104<-ifelse(data07d$CAMEO.Code=="104", 1, 0)
data07d$CC106<-ifelse(data07d$CAMEO.Code=="106", 1, 0)
data07d$CC108<-ifelse(data07d$CAMEO.Code=="108", 1, 0)
data07d$CC111<-ifelse(data07d$CAMEO.Code=="111", 1, 0)
data07d$CC112<-ifelse(data07d$CAMEO.Code=="112", 1, 0)
data07d$CC113<-ifelse(data07d$CAMEO.Code=="113", 1, 0)
data07d$CC114<-ifelse(data07d$CAMEO.Code=="114", 1, 0)
data07d$CC124<-ifelse(data07d$CAMEO.Code=="124", 1, 0)
data07d$CC130<-ifelse(data07d$CAMEO.Code=="130", 1, 0)
data07d$CC131<-ifelse(data07d$CAMEO.Code=="131", 1, 0)
data07d$CC133<-ifelse(data07d$CAMEO.Code=="133", 1, 0)
data07d$CC138<-ifelse(data07d$CAMEO.Code=="138", 1, 0)
data07d$CC139<-ifelse(data07d$CAMEO.Code=="139", 1, 0)
data07d$CC163<-ifelse(data07d$CAMEO.Code=="163", 1, 0)
data07d$CC213<-ifelse(data07d$CAMEO.Code=="213", 1, 0)
data07d$CC214<-ifelse(data07d$CAMEO.Code=="214", 1, 0)
data07d$CC241<-ifelse(data07d$CAMEO.Code=="241", 1, 0)
data07d$CC242<-ifelse(data07d$CAMEO.Code=="242", 1, 0)
data07d$CC243<-ifelse(data07d$CAMEO.Code=="243", 1, 0)
data07d$CC244<-ifelse(data07d$CAMEO.Code=="244", 1, 0)
data07d$CC253<-ifelse(data07d$CAMEO.Code=="253", 1, 0)
data07d$CC255<-ifelse(data07d$CAMEO.Code=="255", 1, 0)
data07d$CC256<-ifelse(data07d$CAMEO.Code=="256", 1, 0)
data07d$CC1041<-ifelse(data07d$CAMEO.Code=="1041", 1, 0)
data07d$CC1042<-ifelse(data07d$CAMEO.Code=="1042", 1, 0)
data07d$CC1043<-ifelse(data07d$CAMEO.Code=="1043", 1, 0)
data07d$CC1053<-ifelse(data07d$CAMEO.Code=="1053", 1, 0)
data07d$CC1056<-ifelse(data07d$CAMEO.Code=="1056", 1, 0)
data07d$CC1121<-ifelse(data07d$CAMEO.Code=="1121", 1, 0)
data07d$CC1122<-ifelse(data07d$CAMEO.Code=="1122", 1, 0)
data07d$CC1123<-ifelse(data07d$CAMEO.Code=="1123", 1, 0)
data07d$CC1124<-ifelse(data07d$CAMEO.Code=="1124", 1, 0)
data07d$CC1125<-ifelse(data07d$CAMEO.Code=="1125", 1, 0)
data07d$CC1242<-ifelse(data07d$CAMEO.Code=="1242", 1, 0)
data07d$CC93 <- ifelse(data07d$CAMEO.Code=="93", 1, 0)
data07d$CC1014 <- ifelse(data07d$CAMEO.Code=="1014", 1, 0)



data07e<-separate(data07d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count2007<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242  ~ Country + Date_1, 
                           FUN=sum, data=data07e)



write.csv(event.count2007, "/Volumes/Lexar/event.counts2007.csv")

##2008

Events08 <- read.csv2("/Volumes/Lexar/Events2008.tab",  sep="\t", header=TRUE)

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
data08d$CC10<-ifelse(data08d$CAMEO.Code=="10", 1, 0)
data08d$CC12<-ifelse(data08d$CAMEO.Code=="12", 1, 0)
data08d$CC20<-ifelse(data08d$CAMEO.Code=="20", 1, 0)
data08d$CC22<-ifelse(data08d$CAMEO.Code=="22", 1, 0)
data08d$CC23<-ifelse(data08d$CAMEO.Code=="23", 1, 0)
data08d$CC24<-ifelse(data08d$CAMEO.Code=="24", 1, 0)
data08d$CC25<-ifelse(data08d$CAMEO.Code=="25", 1, 0)
data08d$CC90<-ifelse(data08d$CAMEO.Code=="90", 1, 0)
data08d$CC91<-ifelse(data08d$CAMEO.Code=="91", 1, 0)
data08d$CC92<-ifelse(data08d$CAMEO.Code=="92", 1, 0)
data08d$CC94<-ifelse(data08d$CAMEO.Code=="94", 1, 0)
data08d$CC100<-ifelse(data08d$CAMEO.Code=="100", 1, 0)
data08d$CC101<-ifelse(data08d$CAMEO.Code=="101", 1, 0)
data08d$CC102<-ifelse(data08d$CAMEO.Code=="102", 1, 0)
data08d$CC103<-ifelse(data08d$CAMEO.Code=="103", 1, 0)
data08d$CC104<-ifelse(data08d$CAMEO.Code=="104", 1, 0)
data08d$CC106<-ifelse(data08d$CAMEO.Code=="106", 1, 0)
data08d$CC108<-ifelse(data08d$CAMEO.Code=="108", 1, 0)
data08d$CC111<-ifelse(data08d$CAMEO.Code=="111", 1, 0)
data08d$CC112<-ifelse(data08d$CAMEO.Code=="112", 1, 0)
data08d$CC113<-ifelse(data08d$CAMEO.Code=="113", 1, 0)
data08d$CC114<-ifelse(data08d$CAMEO.Code=="114", 1, 0)
data08d$CC124<-ifelse(data08d$CAMEO.Code=="124", 1, 0)
data08d$CC130<-ifelse(data08d$CAMEO.Code=="130", 1, 0)
data08d$CC131<-ifelse(data08d$CAMEO.Code=="131", 1, 0)
data08d$CC133<-ifelse(data08d$CAMEO.Code=="133", 1, 0)
data08d$CC138<-ifelse(data08d$CAMEO.Code=="138", 1, 0)
data08d$CC139<-ifelse(data08d$CAMEO.Code=="139", 1, 0)
data08d$CC163<-ifelse(data08d$CAMEO.Code=="163", 1, 0)
data08d$CC213<-ifelse(data08d$CAMEO.Code=="213", 1, 0)
data08d$CC214<-ifelse(data08d$CAMEO.Code=="214", 1, 0)
data08d$CC241<-ifelse(data08d$CAMEO.Code=="241", 1, 0)
data08d$CC242<-ifelse(data08d$CAMEO.Code=="242", 1, 0)
data08d$CC243<-ifelse(data08d$CAMEO.Code=="243", 1, 0)
data08d$CC244<-ifelse(data08d$CAMEO.Code=="244", 1, 0)
data08d$CC253<-ifelse(data08d$CAMEO.Code=="253", 1, 0)
data08d$CC255<-ifelse(data08d$CAMEO.Code=="255", 1, 0)
data08d$CC256<-ifelse(data08d$CAMEO.Code=="256", 1, 0)
data08d$CC1041<-ifelse(data08d$CAMEO.Code=="1041", 1, 0)
data08d$CC1042<-ifelse(data08d$CAMEO.Code=="1042", 1, 0)
data08d$CC1043<-ifelse(data08d$CAMEO.Code=="1043", 1, 0)
data08d$CC1053<-ifelse(data08d$CAMEO.Code=="1053", 1, 0)
data08d$CC1056<-ifelse(data08d$CAMEO.Code=="1056", 1, 0)
data08d$CC1121<-ifelse(data08d$CAMEO.Code=="1121", 1, 0)
data08d$CC1122<-ifelse(data08d$CAMEO.Code=="1122", 1, 0)
data08d$CC1123<-ifelse(data08d$CAMEO.Code=="1123", 1, 0)
data08d$CC1124<-ifelse(data08d$CAMEO.Code=="1124", 1, 0)
data08d$CC1125<-ifelse(data08d$CAMEO.Code=="1125", 1, 0)
data08d$CC1242<-ifelse(data08d$CAMEO.Code=="1242", 1, 0)
data08d$CC93 <- ifelse(data08d$CAMEO.Code=="93", 1, 0)
data08d$CC1014 <- ifelse(data08d$CAMEO.Code=="1014", 1, 0)



data08e<-separate(data08d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count2008<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242  ~ Country + Date_1, 
                           FUN=sum, data=data08e)



write.csv(event.count2008, "/Volumes/Lexar/event.counts2008.csv")

##2009

Events09 <- read.csv2("/Volumes/Lexar/Events2009.tab",  sep="\t", header=TRUE)

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
data09d$CC10<-ifelse(data09d$CAMEO.Code=="10", 1, 0)
data09d$CC12<-ifelse(data09d$CAMEO.Code=="12", 1, 0)
data09d$CC20<-ifelse(data09d$CAMEO.Code=="20", 1, 0)
data09d$CC22<-ifelse(data09d$CAMEO.Code=="22", 1, 0)
data09d$CC23<-ifelse(data09d$CAMEO.Code=="23", 1, 0)
data09d$CC24<-ifelse(data09d$CAMEO.Code=="24", 1, 0)
data09d$CC25<-ifelse(data09d$CAMEO.Code=="25", 1, 0)
data09d$CC90<-ifelse(data09d$CAMEO.Code=="90", 1, 0)
data09d$CC91<-ifelse(data09d$CAMEO.Code=="91", 1, 0)
data09d$CC92<-ifelse(data09d$CAMEO.Code=="92", 1, 0)
data09d$CC94<-ifelse(data09d$CAMEO.Code=="94", 1, 0)
data09d$CC100<-ifelse(data09d$CAMEO.Code=="100", 1, 0)
data09d$CC101<-ifelse(data09d$CAMEO.Code=="101", 1, 0)
data09d$CC102<-ifelse(data09d$CAMEO.Code=="102", 1, 0)
data09d$CC103<-ifelse(data09d$CAMEO.Code=="103", 1, 0)
data09d$CC104<-ifelse(data09d$CAMEO.Code=="104", 1, 0)
data09d$CC106<-ifelse(data09d$CAMEO.Code=="106", 1, 0)
data09d$CC108<-ifelse(data09d$CAMEO.Code=="108", 1, 0)
data09d$CC111<-ifelse(data09d$CAMEO.Code=="111", 1, 0)
data09d$CC112<-ifelse(data09d$CAMEO.Code=="112", 1, 0)
data09d$CC113<-ifelse(data09d$CAMEO.Code=="113", 1, 0)
data09d$CC114<-ifelse(data09d$CAMEO.Code=="114", 1, 0)
data09d$CC124<-ifelse(data09d$CAMEO.Code=="124", 1, 0)
data09d$CC130<-ifelse(data09d$CAMEO.Code=="130", 1, 0)
data09d$CC131<-ifelse(data09d$CAMEO.Code=="131", 1, 0)
data09d$CC133<-ifelse(data09d$CAMEO.Code=="133", 1, 0)
data09d$CC138<-ifelse(data09d$CAMEO.Code=="138", 1, 0)
data09d$CC139<-ifelse(data09d$CAMEO.Code=="139", 1, 0)
data09d$CC163<-ifelse(data09d$CAMEO.Code=="163", 1, 0)
data09d$CC213<-ifelse(data09d$CAMEO.Code=="213", 1, 0)
data09d$CC214<-ifelse(data09d$CAMEO.Code=="214", 1, 0)
data09d$CC241<-ifelse(data09d$CAMEO.Code=="241", 1, 0)
data09d$CC242<-ifelse(data09d$CAMEO.Code=="242", 1, 0)
data09d$CC243<-ifelse(data09d$CAMEO.Code=="243", 1, 0)
data09d$CC244<-ifelse(data09d$CAMEO.Code=="244", 1, 0)
data09d$CC253<-ifelse(data09d$CAMEO.Code=="253", 1, 0)
data09d$CC255<-ifelse(data09d$CAMEO.Code=="255", 1, 0)
data09d$CC256<-ifelse(data09d$CAMEO.Code=="256", 1, 0)
data09d$CC1041<-ifelse(data09d$CAMEO.Code=="1041", 1, 0)
data09d$CC1042<-ifelse(data09d$CAMEO.Code=="1042", 1, 0)
data09d$CC1043<-ifelse(data09d$CAMEO.Code=="1043", 1, 0)
data09d$CC1053<-ifelse(data09d$CAMEO.Code=="1053", 1, 0)
data09d$CC1056<-ifelse(data09d$CAMEO.Code=="1056", 1, 0)
data09d$CC1121<-ifelse(data09d$CAMEO.Code=="1121", 1, 0)
data09d$CC1122<-ifelse(data09d$CAMEO.Code=="1122", 1, 0)
data09d$CC1123<-ifelse(data09d$CAMEO.Code=="1123", 1, 0)
data09d$CC1124<-ifelse(data09d$CAMEO.Code=="1124", 1, 0)
data09d$CC1125<-ifelse(data09d$CAMEO.Code=="1125", 1, 0)
data09d$CC1242<-ifelse(data09d$CAMEO.Code=="1242", 1, 0)
data09d$CC93 <- ifelse(data09d$CAMEO.Code=="93", 1, 0)
data09d$CC1014 <- ifelse(data09d$CAMEO.Code=="1014", 1, 0)



data09e<-separate(data09d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count2009<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242  ~ Country + Date_1, 
                           FUN=sum, data=data09e)



write.csv(event.count2009, "/Volumes/Lexar/event.counts2009.csv")

##2010

Events10 <- read.csv2("/Volumes/Lexar/Events2010.tab",  sep="\t", header=TRUE)

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
data10d$CC10<-ifelse(data10d$CAMEO.Code=="10", 1, 0)
data10d$CC12<-ifelse(data10d$CAMEO.Code=="12", 1, 0)
data10d$CC20<-ifelse(data10d$CAMEO.Code=="20", 1, 0)
data10d$CC22<-ifelse(data10d$CAMEO.Code=="22", 1, 0)
data10d$CC23<-ifelse(data10d$CAMEO.Code=="23", 1, 0)
data10d$CC24<-ifelse(data10d$CAMEO.Code=="24", 1, 0)
data10d$CC25<-ifelse(data10d$CAMEO.Code=="25", 1, 0)
data10d$CC90<-ifelse(data10d$CAMEO.Code=="90", 1, 0)
data10d$CC91<-ifelse(data10d$CAMEO.Code=="91", 1, 0)
data10d$CC92<-ifelse(data10d$CAMEO.Code=="92", 1, 0)
data10d$CC94<-ifelse(data10d$CAMEO.Code=="94", 1, 0)
data10d$CC100<-ifelse(data10d$CAMEO.Code=="100", 1, 0)
data10d$CC101<-ifelse(data10d$CAMEO.Code=="101", 1, 0)
data10d$CC102<-ifelse(data10d$CAMEO.Code=="102", 1, 0)
data10d$CC103<-ifelse(data10d$CAMEO.Code=="103", 1, 0)
data10d$CC104<-ifelse(data10d$CAMEO.Code=="104", 1, 0)
data10d$CC106<-ifelse(data10d$CAMEO.Code=="106", 1, 0)
data10d$CC108<-ifelse(data10d$CAMEO.Code=="108", 1, 0)
data10d$CC111<-ifelse(data10d$CAMEO.Code=="111", 1, 0)
data10d$CC112<-ifelse(data10d$CAMEO.Code=="112", 1, 0)
data10d$CC113<-ifelse(data10d$CAMEO.Code=="113", 1, 0)
data10d$CC114<-ifelse(data10d$CAMEO.Code=="114", 1, 0)
data10d$CC124<-ifelse(data10d$CAMEO.Code=="124", 1, 0)
data10d$CC130<-ifelse(data10d$CAMEO.Code=="130", 1, 0)
data10d$CC131<-ifelse(data10d$CAMEO.Code=="131", 1, 0)
data10d$CC133<-ifelse(data10d$CAMEO.Code=="133", 1, 0)
data10d$CC138<-ifelse(data10d$CAMEO.Code=="138", 1, 0)
data10d$CC139<-ifelse(data10d$CAMEO.Code=="139", 1, 0)
data10d$CC163<-ifelse(data10d$CAMEO.Code=="163", 1, 0)
data10d$CC213<-ifelse(data10d$CAMEO.Code=="213", 1, 0)
data10d$CC214<-ifelse(data10d$CAMEO.Code=="214", 1, 0)
data10d$CC241<-ifelse(data10d$CAMEO.Code=="241", 1, 0)
data10d$CC242<-ifelse(data10d$CAMEO.Code=="242", 1, 0)
data10d$CC243<-ifelse(data10d$CAMEO.Code=="243", 1, 0)
data10d$CC244<-ifelse(data10d$CAMEO.Code=="244", 1, 0)
data10d$CC253<-ifelse(data10d$CAMEO.Code=="253", 1, 0)
data10d$CC255<-ifelse(data10d$CAMEO.Code=="255", 1, 0)
data10d$CC256<-ifelse(data10d$CAMEO.Code=="256", 1, 0)
data10d$CC1041<-ifelse(data10d$CAMEO.Code=="1041", 1, 0)
data10d$CC1042<-ifelse(data10d$CAMEO.Code=="1042", 1, 0)
data10d$CC1043<-ifelse(data10d$CAMEO.Code=="1043", 1, 0)
data10d$CC1053<-ifelse(data10d$CAMEO.Code=="1053", 1, 0)
data10d$CC1056<-ifelse(data10d$CAMEO.Code=="1056", 1, 0)
data10d$CC1121<-ifelse(data10d$CAMEO.Code=="1121", 1, 0)
data10d$CC1122<-ifelse(data10d$CAMEO.Code=="1122", 1, 0)
data10d$CC1123<-ifelse(data10d$CAMEO.Code=="1123", 1, 0)
data10d$CC1124<-ifelse(data10d$CAMEO.Code=="1124", 1, 0)
data10d$CC1125<-ifelse(data10d$CAMEO.Code=="1125", 1, 0)
data10d$CC1242<-ifelse(data10d$CAMEO.Code=="1242", 1, 0)
data10d$CC93 <- ifelse(data10d$CAMEO.Code=="93", 1, 0)
data10d$CC1014 <- ifelse(data10d$CAMEO.Code=="1014", 1, 0)



data10e<-separate(data10d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count2010<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242  ~ Country + Date_1, 
                           FUN=sum, data=data10e)



write.csv(event.count2010, "/Volumes/Lexar/event.counts2010.csv")

#2011

Events11 <- read.csv2("/Volumes/Lexar/Events2011.tab",  sep="\t", header=TRUE)

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
data11d$CC10<-ifelse(data11d$CAMEO.Code=="10", 1, 0)
data11d$CC12<-ifelse(data11d$CAMEO.Code=="12", 1, 0)
data11d$CC20<-ifelse(data11d$CAMEO.Code=="20", 1, 0)
data11d$CC22<-ifelse(data11d$CAMEO.Code=="22", 1, 0)
data11d$CC23<-ifelse(data11d$CAMEO.Code=="23", 1, 0)
data11d$CC24<-ifelse(data11d$CAMEO.Code=="24", 1, 0)
data11d$CC25<-ifelse(data11d$CAMEO.Code=="25", 1, 0)
data11d$CC90<-ifelse(data11d$CAMEO.Code=="90", 1, 0)
data11d$CC91<-ifelse(data11d$CAMEO.Code=="91", 1, 0)
data11d$CC92<-ifelse(data11d$CAMEO.Code=="92", 1, 0)
data11d$CC94<-ifelse(data11d$CAMEO.Code=="94", 1, 0)
data11d$CC100<-ifelse(data11d$CAMEO.Code=="100", 1, 0)
data11d$CC101<-ifelse(data11d$CAMEO.Code=="101", 1, 0)
data11d$CC102<-ifelse(data11d$CAMEO.Code=="102", 1, 0)
data11d$CC103<-ifelse(data11d$CAMEO.Code=="103", 1, 0)
data11d$CC104<-ifelse(data11d$CAMEO.Code=="104", 1, 0)
data11d$CC106<-ifelse(data11d$CAMEO.Code=="106", 1, 0)
data11d$CC108<-ifelse(data11d$CAMEO.Code=="108", 1, 0)
data11d$CC111<-ifelse(data11d$CAMEO.Code=="111", 1, 0)
data11d$CC112<-ifelse(data11d$CAMEO.Code=="112", 1, 0)
data11d$CC113<-ifelse(data11d$CAMEO.Code=="113", 1, 0)
data11d$CC114<-ifelse(data11d$CAMEO.Code=="114", 1, 0)
data11d$CC124<-ifelse(data11d$CAMEO.Code=="124", 1, 0)
data11d$CC130<-ifelse(data11d$CAMEO.Code=="130", 1, 0)
data11d$CC131<-ifelse(data11d$CAMEO.Code=="131", 1, 0)
data11d$CC133<-ifelse(data11d$CAMEO.Code=="133", 1, 0)
data11d$CC138<-ifelse(data11d$CAMEO.Code=="138", 1, 0)
data11d$CC139<-ifelse(data11d$CAMEO.Code=="139", 1, 0)
data11d$CC163<-ifelse(data11d$CAMEO.Code=="163", 1, 0)
data11d$CC213<-ifelse(data11d$CAMEO.Code=="213", 1, 0)
data11d$CC214<-ifelse(data11d$CAMEO.Code=="214", 1, 0)
data11d$CC241<-ifelse(data11d$CAMEO.Code=="241", 1, 0)
data11d$CC242<-ifelse(data11d$CAMEO.Code=="242", 1, 0)
data11d$CC243<-ifelse(data11d$CAMEO.Code=="243", 1, 0)
data11d$CC244<-ifelse(data11d$CAMEO.Code=="244", 1, 0)
data11d$CC253<-ifelse(data11d$CAMEO.Code=="253", 1, 0)
data11d$CC255<-ifelse(data11d$CAMEO.Code=="255", 1, 0)
data11d$CC256<-ifelse(data11d$CAMEO.Code=="256", 1, 0)
data11d$CC1041<-ifelse(data11d$CAMEO.Code=="1041", 1, 0)
data11d$CC1042<-ifelse(data11d$CAMEO.Code=="1042", 1, 0)
data11d$CC1043<-ifelse(data11d$CAMEO.Code=="1043", 1, 0)
data11d$CC1053<-ifelse(data11d$CAMEO.Code=="1053", 1, 0)
data11d$CC1056<-ifelse(data11d$CAMEO.Code=="1056", 1, 0)
data11d$CC1121<-ifelse(data11d$CAMEO.Code=="1121", 1, 0)
data11d$CC1122<-ifelse(data11d$CAMEO.Code=="1122", 1, 0)
data11d$CC1123<-ifelse(data11d$CAMEO.Code=="1123", 1, 0)
data11d$CC1124<-ifelse(data11d$CAMEO.Code=="1124", 1, 0)
data11d$CC1125<-ifelse(data11d$CAMEO.Code=="1125", 1, 0)
data11d$CC1242<-ifelse(data11d$CAMEO.Code=="1242", 1, 0)
data11d$CC93 <- ifelse(data11d$CAMEO.Code=="93", 1, 0)
data11d$CC1014 <- ifelse(data11d$CAMEO.Code=="1014", 1, 0)



data11e<-separate(data11d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count2011<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242  ~ Country + Date_1, 
                           FUN=sum, data=data11e)



write.csv(event.count2011, "/Volumes/Lexar/event.counts2011.csv")

##2012

Events12 <- read.csv2("/Volumes/Lexar/Events2012.tab",  sep="\t", header=TRUE)

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
data12d$CC10<-ifelse(data12d$CAMEO.Code=="10", 1, 0)
data12d$CC12<-ifelse(data12d$CAMEO.Code=="12", 1, 0)
data12d$CC20<-ifelse(data12d$CAMEO.Code=="20", 1, 0)
data12d$CC22<-ifelse(data12d$CAMEO.Code=="22", 1, 0)
data12d$CC23<-ifelse(data12d$CAMEO.Code=="23", 1, 0)
data12d$CC24<-ifelse(data12d$CAMEO.Code=="24", 1, 0)
data12d$CC25<-ifelse(data12d$CAMEO.Code=="25", 1, 0)
data12d$CC90<-ifelse(data12d$CAMEO.Code=="90", 1, 0)
data12d$CC91<-ifelse(data12d$CAMEO.Code=="91", 1, 0)
data12d$CC92<-ifelse(data12d$CAMEO.Code=="92", 1, 0)
data12d$CC94<-ifelse(data12d$CAMEO.Code=="94", 1, 0)
data12d$CC100<-ifelse(data12d$CAMEO.Code=="100", 1, 0)
data12d$CC101<-ifelse(data12d$CAMEO.Code=="101", 1, 0)
data12d$CC102<-ifelse(data12d$CAMEO.Code=="102", 1, 0)
data12d$CC103<-ifelse(data12d$CAMEO.Code=="103", 1, 0)
data12d$CC104<-ifelse(data12d$CAMEO.Code=="104", 1, 0)
data12d$CC106<-ifelse(data12d$CAMEO.Code=="106", 1, 0)
data12d$CC108<-ifelse(data12d$CAMEO.Code=="108", 1, 0)
data12d$CC111<-ifelse(data12d$CAMEO.Code=="111", 1, 0)
data12d$CC112<-ifelse(data12d$CAMEO.Code=="112", 1, 0)
data12d$CC113<-ifelse(data12d$CAMEO.Code=="113", 1, 0)
data12d$CC114<-ifelse(data12d$CAMEO.Code=="114", 1, 0)
data12d$CC124<-ifelse(data12d$CAMEO.Code=="124", 1, 0)
data12d$CC130<-ifelse(data12d$CAMEO.Code=="130", 1, 0)
data12d$CC131<-ifelse(data12d$CAMEO.Code=="131", 1, 0)
data12d$CC133<-ifelse(data12d$CAMEO.Code=="133", 1, 0)
data12d$CC138<-ifelse(data12d$CAMEO.Code=="138", 1, 0)
data12d$CC139<-ifelse(data12d$CAMEO.Code=="139", 1, 0)
data12d$CC163<-ifelse(data12d$CAMEO.Code=="163", 1, 0)
data12d$CC213<-ifelse(data12d$CAMEO.Code=="213", 1, 0)
data12d$CC214<-ifelse(data12d$CAMEO.Code=="214", 1, 0)
data12d$CC241<-ifelse(data12d$CAMEO.Code=="241", 1, 0)
data12d$CC242<-ifelse(data12d$CAMEO.Code=="242", 1, 0)
data12d$CC243<-ifelse(data12d$CAMEO.Code=="243", 1, 0)
data12d$CC244<-ifelse(data12d$CAMEO.Code=="244", 1, 0)
data12d$CC253<-ifelse(data12d$CAMEO.Code=="253", 1, 0)
data12d$CC255<-ifelse(data12d$CAMEO.Code=="255", 1, 0)
data12d$CC256<-ifelse(data12d$CAMEO.Code=="256", 1, 0)
data12d$CC1041<-ifelse(data12d$CAMEO.Code=="1041", 1, 0)
data12d$CC1042<-ifelse(data12d$CAMEO.Code=="1042", 1, 0)
data12d$CC1043<-ifelse(data12d$CAMEO.Code=="1043", 1, 0)
data12d$CC1053<-ifelse(data12d$CAMEO.Code=="1053", 1, 0)
data12d$CC1056<-ifelse(data12d$CAMEO.Code=="1056", 1, 0)
data12d$CC1121<-ifelse(data12d$CAMEO.Code=="1121", 1, 0)
data12d$CC1122<-ifelse(data12d$CAMEO.Code=="1122", 1, 0)
data12d$CC1123<-ifelse(data12d$CAMEO.Code=="1123", 1, 0)
data12d$CC1124<-ifelse(data12d$CAMEO.Code=="1124", 1, 0)
data12d$CC1125<-ifelse(data12d$CAMEO.Code=="1125", 1, 0)
data12d$CC1242<-ifelse(data12d$CAMEO.Code=="1242", 1, 0)
data12d$CC93 <- ifelse(data12d$CAMEO.Code=="93", 1, 0)
data12d$CC1014 <- ifelse(data12d$CAMEO.Code=="1014", 1, 0)



data12e<-separate(data12d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count2012<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                           + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                                   CC101 + CC102 + CC103 + CC104 + CC106 + 
                                   CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                           + CC130 + CC131 + CC133 + CC138 + CC139 + 
                                   CC163 + CC213 + CC214 + CC241 + CC242 + 
                                   CC243 + CC244 + CC253 + CC255 + CC256 + 
                                   CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                                   CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                                   CC1125 + CC1242 ~ Country + Date_1, 
                           FUN=sum, data=data12e)



write.csv(event.count2012, "/Volumes/Lexar/event.counts2012.csv")

##2013

Events13 <- read.csv2("/Volumes/Lexar/Events2013.tab",  sep="\t", header=TRUE)

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
data13d$CC10<-ifelse(data13d$CAMEO.Code=="10", 1, 0)
data13d$CC12<-ifelse(data13d$CAMEO.Code=="12", 1, 0)
data13d$CC20<-ifelse(data13d$CAMEO.Code=="20", 1, 0)
data13d$CC22<-ifelse(data13d$CAMEO.Code=="22", 1, 0)
data13d$CC23<-ifelse(data13d$CAMEO.Code=="23", 1, 0)
data13d$CC24<-ifelse(data13d$CAMEO.Code=="24", 1, 0)
data13d$CC25<-ifelse(data13d$CAMEO.Code=="25", 1, 0)
data13d$CC90<-ifelse(data13d$CAMEO.Code=="90", 1, 0)
data13d$CC91<-ifelse(data13d$CAMEO.Code=="91", 1, 0)
data13d$CC92<-ifelse(data13d$CAMEO.Code=="92", 1, 0)
data13d$CC94<-ifelse(data13d$CAMEO.Code=="94", 1, 0)
data13d$CC100<-ifelse(data13d$CAMEO.Code=="100", 1, 0)
data13d$CC101<-ifelse(data13d$CAMEO.Code=="101", 1, 0)
data13d$CC102<-ifelse(data13d$CAMEO.Code=="102", 1, 0)
data13d$CC103<-ifelse(data13d$CAMEO.Code=="103", 1, 0)
data13d$CC104<-ifelse(data13d$CAMEO.Code=="104", 1, 0)
data13d$CC106<-ifelse(data13d$CAMEO.Code=="106", 1, 0)
data13d$CC108<-ifelse(data13d$CAMEO.Code=="108", 1, 0)
data13d$CC111<-ifelse(data13d$CAMEO.Code=="111", 1, 0)
data13d$CC112<-ifelse(data13d$CAMEO.Code=="112", 1, 0)
data13d$CC113<-ifelse(data13d$CAMEO.Code=="113", 1, 0)
data13d$CC114<-ifelse(data13d$CAMEO.Code=="114", 1, 0)
data13d$CC124<-ifelse(data13d$CAMEO.Code=="124", 1, 0)
data13d$CC130<-ifelse(data13d$CAMEO.Code=="130", 1, 0)
data13d$CC131<-ifelse(data13d$CAMEO.Code=="131", 1, 0)
data13d$CC133<-ifelse(data13d$CAMEO.Code=="133", 1, 0)
data13d$CC138<-ifelse(data13d$CAMEO.Code=="138", 1, 0)
data13d$CC139<-ifelse(data13d$CAMEO.Code=="139", 1, 0)
data13d$CC163<-ifelse(data13d$CAMEO.Code=="163", 1, 0)
data13d$CC213<-ifelse(data13d$CAMEO.Code=="213", 1, 0)
data13d$CC214<-ifelse(data13d$CAMEO.Code=="214", 1, 0)
data13d$CC241<-ifelse(data13d$CAMEO.Code=="241", 1, 0)
data13d$CC242<-ifelse(data13d$CAMEO.Code=="242", 1, 0)
data13d$CC243<-ifelse(data13d$CAMEO.Code=="243", 1, 0)
data13d$CC244<-ifelse(data13d$CAMEO.Code=="244", 1, 0)
data13d$CC253<-ifelse(data13d$CAMEO.Code=="253", 1, 0)
data13d$CC255<-ifelse(data13d$CAMEO.Code=="255", 1, 0)
data13d$CC256<-ifelse(data13d$CAMEO.Code=="256", 1, 0)
data13d$CC1041<-ifelse(data13d$CAMEO.Code=="1041", 1, 0)
data13d$CC1042<-ifelse(data13d$CAMEO.Code=="1042", 1, 0)
data13d$CC1043<-ifelse(data13d$CAMEO.Code=="1043", 1, 0)
data13d$CC1053<-ifelse(data13d$CAMEO.Code=="1053", 1, 0)
data13d$CC1056<-ifelse(data13d$CAMEO.Code=="1056", 1, 0)
data13d$CC1121<-ifelse(data13d$CAMEO.Code=="1121", 1, 0)
data13d$CC1122<-ifelse(data13d$CAMEO.Code=="1122", 1, 0)
data13d$CC1123<-ifelse(data13d$CAMEO.Code=="1123", 1, 0)
data13d$CC1124<-ifelse(data13d$CAMEO.Code=="1124", 1, 0)
data13d$CC1125<-ifelse(data13d$CAMEO.Code=="1125", 1, 0)
data13d$CC1242<-ifelse(data13d$CAMEO.Code=="1242", 1, 0)
data13d$CC93 <- ifelse(data13d$CAMEO.Code=="93", 1, 0)
data13d$CC1014 <- ifelse(data13d$CAMEO.Code=="1014", 1, 0)



data13e<-separate(data13d, "Event.Date", paste("Date", 1:3, sep="_"), sep="-", extra="drop")

event.count2013<-summaryBy(CC10+ CC12 + CC20 + CC22 + CC23 + CC24 + CC25 
                        + CC90 + CC91+ CC92+ CC93+ CC94 + CC100 + 
                        CC101 + CC102 + CC103 + CC104 + CC106 + 
                        CC108+ CC111 +CC112 + CC113 + CC114 + CC124 
                        + CC130 + CC131 + CC133 + CC138 + CC139 + 
                        CC163 + CC213 + CC214 + CC241 + CC242 + 
                        CC243 + CC244 + CC253 + CC255 + CC256 + 
                        CC1014 + CC1041 + CC1042 + CC1043 + CC1053 + 
                        CC1056 + CC1121 + CC1122 + CC1123 + CC1124 + 
                        CC1125 + CC1242  ~ Country + Date_1, 
                        FUN=sum, data=data13e)



write.csv(event.count2013, "/Volumes/Lexar/event.counts2013.csv")
