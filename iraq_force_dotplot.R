library(googlesheets4)

Sheet  <- read_sheet("https://docs.google.com/spreadsheets/d/17xk7rBsTFC41Q4GZ5gJBz8f8f2NEdMCUnkVcJCCyNkw/edit#gid=1276040235")


library(stringr)
Sheet$House <- str_sub(Sheet$MasterID,-4,-4)
Sheet$Party <- str_sub(Sheet$MasterID,-7,-5)
Sheet$Party <- ifelse(Sheet$Party == "SBI", 100, Sheet$Party )
Sheet$HouseParty <- paste(Sheet$House, Sheet$Party)


Sheet$HouseParty <- ifelse(Sheet$HouseParty == "H 100", "House Democrat", Sheet$HouseParty)
Sheet$HouseParty <- ifelse(Sheet$HouseParty == "S 100", "Senate Democrat", Sheet$HouseParty)
Sheet$HouseParty <- ifelse(Sheet$HouseParty == "H 200", "House Republican", Sheet$HouseParty)
Sheet$HouseParty <- ifelse(Sheet$HouseParty == "S 200", "Senate Republican", Sheet$HouseParty)


Sheet$Irrelevant <- as.character(Sheet$Irrelevant)

Sheet <- subset(Sheet, Sheet$Irrelevant == "NA")


Sheet$Advocates_for_Use_of_American_Military_Force <- as.character(Sheet$Advocates_for_Use_of_American_Military_Force)
Sheet$Advocates_against_Use_of_American_Military_Force <- as.character(Sheet$Advocates_against_Use_of_American_Military_Force)

Sheet <- subset(Sheet, Sheet$Advocates_for_Use_of_American_Military_Force == 1 | Sheet$Advocates_against_Use_of_American_Military_Force == 1)


Sheet$date<-as.Date(as.character(Sheet$date),format="%Y%m%d")

Sheet$HD <- ifelse(Sheet$Advocates_for_Use_of_American_Military_Force == "NA",0,1)


Sheet$HD <- ifelse(Sheet$HD == 1,"Support Force","Oppose Force")

#only includ after Sept Oct 1962
#CMC <- subset(CMC, CMC$date > as.Date('1962-08-31') )

library(ggplot2)


ggplot(Sheet, aes(date, HD)) +  geom_jitter(aes(colour = HouseParty),width = 2, height = 0.3, size = 3, alpha = .6)  + ggtitle("Iraq 2002-03") +  
  xlab("Date") + # for the x axis label
  ylab("Sentiment") +
  geom_text(
    label="Sarin Attack",
    x=as.Date('2013-08-21'), angle = 90,hjust = -0, size = 2.4, vjust = 0)+
  geom_text(
    label="Obama Requests AUMF",
    x=as.Date('2013-08-31'), angle = 90,hjust = 0.4, size = 2.4, vjust = 0)+ scale_color_manual(values = c("House Democrat" = "dodgerblue2", "House Republican"="red",
       
                                                                                                                                                                                                               "Senate Democrat" = "dark blue", "Senate Republican" = "firebrick4")) 