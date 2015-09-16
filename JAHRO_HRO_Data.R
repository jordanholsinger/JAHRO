event.counts1995 <- read.csv("E:/event.counts1995.csv")
event.counts1996 <- read.csv("E:/event.counts1996.csv")
event.counts1997 <- read.csv("E:/event.counts1997.csv")
event.counts1998 <- read.csv("E:/event.counts1998.csv")
event.counts1999 <- read.csv("E:/event.counts1999.csv")
event.counts2000 <- read.csv("E:/event.counts2000.csv")
event.counts2001 <- read.csv("E:/event.counts2001.csv")
event.counts2002 <- read.csv("E:/event.counts2002.csv")
event.counts2003 <- read.csv("E:/event.counts2003.csv")
event.counts2004 <- read.csv("E:/event.counts2004.csv")
event.counts2005 <- read.csv("E:/event.counts2005.csv")
event.counts2006 <- read.csv("E:/event.counts2006.csv")
event.counts2007 <- read.csv("E:/event.counts2007.csv")
event.counts2008 <- read.csv("E:/event.counts2008.csv")
event.counts2009 <- read.csv("E:/event.counts2009.csv")
event.counts2010 <- read.csv("E:/event.counts2010.csv")
event.counts2011 <- read.csv("E:/event.counts2011.csv")
event.counts2012 <- read.csv("E:/event.counts2012.csv")
event.counts2013 <- read.csv("E:/event.counts2013.csv")


HRO_Data<-rbind(event.counts1995,event.counts1996, event.counts1997, event.counts1998, 
      event.counts1999, event.counts2000, event.counts2001, event.counts2002, 
      event.counts2003, event.counts2004, event.counts2005, event.counts2006, 
      event.counts2007, event.counts2008, event.counts2009, event.counts2010, 
      event.counts2011, event.counts2012, event.counts2013)

for(i in 1:nrow(HRO_Data)){ 
  HRO_Data[i,55]=sum(HRO_Data[i,4:54])
  
}



write.csv(HRO_Data, "E:/JAHRO_HRO_Data.csv")
