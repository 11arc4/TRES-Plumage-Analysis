library(lubridate)
library(tidyverse)

nests <- read.csv("file:///C:/Users/11arc/OneDrive/Documents/Montogomerie Work/TRES Plumage/ARC TRES data/ALL NESTS.csv")


str(nests)


nests$First.Egg.Date <- as.Date(as.character(nests$First.Egg.Date), "%d-%b-%y")
nests$Last.Egg.Date <- as.Date(as.character(nests$Last.Egg.Date), "%d-%b-%y")
nests$Mean.Hatch.Date <- as.Date(as.character(nests$Mean.Hatch.Date), "%d-%b-%y")

nests$First.Egg.Julian <- yday(nests$First.Egg.Date)
nests$Last.Egg.Julian <- yday(nests$Last.Egg.Date)
nests$Mean.Hatch.Julian <- yday(nests$Mean.Hatch.Date)

hist(nests$First.Egg.Julian, breaks=50)


nests$BoxID [which(nests$First.Egg.Julian>142)][order(nests$BoxID [which(nests$First.Egg.Julian>142)])]


nests$Spatial.Order[which(nests$First.Egg.Julian>142)]


#Not overly clear which were the new boxes. 


ggplot(nests %>% filter(Year=="2000"), aes(x=Box.No, y=First.Egg.Julian, color=Site))+
  geom_point()

#I think that all box numbers >60 are probably the second round of boxes. 


nests$Extra.Box <- ifelse(nests$Box.No>60, "Y", "N")


ggplot(nests, aes(x=Extra.Box, y=First.Egg.Julian))+
  geom_boxplot()


#THe dataset listed as final and with QCing in the folders. 

allnests <- read.csv("file:///C:/Users/11arc/OneDrive/Documents/Montogomerie Work/TRES Plumage/ARC TRES data/All Nests with Parental BandID (Stephen - Nests Final).csv", na.strings = "")

#Fix the date format
#For the dates, some of the 1999s are in d-m-y and the others are in m/d/y form.
#For 2000 all first eggs are in m-d-y format
allnests$FirstEggDate2 <- as.Date(NA)
allnests$FirstEggDate2[which(allnests$Year==2000)] <- as.Date(as.character(allnests$FirstEggDate[which(allnests$Year==2000)]), "%m-%d-%y")
allnests$FirstEggDate2[grepl(allnests$FirstEggDate, pattern="-") & allnests$Year==1999] <- as.Date(as.character(allnests$FirstEggDate [grepl(allnests$FirstEggDate, pattern="-") & allnests$Year==1999]), "%d-%m-%y")
allnests$FirstEggDate2[grepl(allnests$FirstEggDate, pattern="/") & allnests$Year==1999] <- as.Date(as.character(allnests$FirstEggDate [grepl(allnests$FirstEggDate, pattern="/") & allnests$Year==1999]), "%m/%d/%y")

allnests$LastEggDate2 <- as.Date(NA)
allnests$LastEggDate2[which(allnests$Year==2000)] <- 
  as.Date(as.character(allnests$LastEggDate[which(allnests$Year==2000)]), "%m-%d-%y")
allnests$LastEggDate2[grepl(allnests$LastEggDate, pattern="-") & allnests$Year==1999] <- 
  as.Date(as.character(allnests$LastEggDate [grepl(allnests$LastEggDate, pattern="-") & allnests$Year==1999]), "%d-%m-%y")
allnests$LastEggDate2[grepl(allnests$LastEggDate, pattern="/") & allnests$Year==1999] <- 
  as.Date(as.character(allnests$LastEggDate [grepl(allnests$LastEggDate, pattern="/") & allnests$Year==1999]), "%m/%d/%y")

allnests$MeanHatchDate2 <- as.Date(NA)
allnests$MeanHatchDate2[which(allnests$Year==2000)] <- 
  as.Date(as.character(allnests$MeanHatchDate[which(allnests$Year==2000)]), "%m-%d-%y")
allnests$MeanHatchDate2[grepl(allnests$MeanHatchDate, pattern="-") & allnests$Year==1999] <- 
  as.Date(as.character(allnests$MeanHatchDate [grepl(allnests$MeanHatchDate, pattern="-") & allnests$Year==1999]), "%d-%m-%y")
allnests$MeanHatchDate2[grepl(allnests$MeanHatchDate, pattern="/") & allnests$Year==1999] <- 
  as.Date(as.character(allnests$MeanHatchDate [grepl(allnests$MeanHatchDate, pattern="/") & allnests$Year==1999]), "%m/%d/%y")

#Calculate Julian dates-- much less fussy. 
allnests$FirstEggJulian <- yday(allnests$FirstEggDate2)
allnests$LastEggJulian <- yday(allnests$LastEggDate2)
allnests$HatchJulian <- yday(allnests$MeanHatchDate2)


#Assign whether this was a box that was put up late or not

allnests$LateBox <- ifelse(allnests$Box.No>60, "Y", "N")



ggplot(data=allnests, aes(x=Box.No, y=FirstEggJulian, color=LateBox))+
  geom_point()

ggplot(data=allnests, aes(x=Box.No, y=LastEggJulian, color=LateBox))+
  geom_point()

ggplot(data=allnests, aes(x=Box.No, y=HatchJulian, color=LateBox))+
  geom_point()



#There was a lot of correspondance about how they weren't sure whether all of
#the adult males and females band IDs had been appropriately assigned.



#Need to match up Male and Female Band ID with nests (if at all possible)
adults <- read.csv("~/Montogomerie Work/TRES Plumage/ARC TRES data/Adult Morphometrics.csv", na.strings = c("")) 
adults$Date2 <- as.Date(as.character(adults$Date), "%m-%d-%y")
adults$JulianDate <- yday((adults$Date2))
adults$NestID <- paste(adults$Year, adults$SiteID, adults$BoxID, sep="-")
allnests$NestID <- paste(allnests$Year, allnests$Site.ID, allnests$Box.No, sep="-")

#Only interested in 1999 and 2000 nests
allnests2 <- allnests %>% filter(Year %in% c(1999, 2000))

#calculate resonable catching dates. 
allnests2$LastCatchDate <- ifelse(allnests2$BroodSize==0, allnests2$LastEggJulian+14, allnests2$HatchJulian+18)
#Make the new columns that are needed. 
allnests2$MaleMass <- NA_real_
allnests2$MaleWingChord <- NA_real_
allnests2$MaleAgeClass <- NA
allnests2$MaleYears <- NA
allnests2$MaleAgeAccuracy <- NA
allnests2$MaleRecap <-NA
allnests2$MaleCaptureDate <-NA
allnests2$MaleCaptureDateJulian <-NA

allnests2$FemaleMass <- NA
allnests2$FemaleWingChord <- NA
allnests2$FemaleAgeClass <-NA
allnests2$FemaleYears <- NA
allnests2$FemaleAgeAccuracy <- NA
allnests2$FemaleRecap <-NA
allnests2$FemaleCaptureDate <-NA
allnests2$FemaleCaptureDateJulian <-NA

for(i in 1:nrow(allnests2)){
  #need a warning if male and female ID listed for this nest are not in the birds file
  
  #FEMALEs
  if(is.na(allnests2$BandFem[i])){
    if(any(adults$NestID==allnests2$NestID[i] & adults$SexMorph=="Female" )){
      message("confirm no female ID for nest" ,allnests2$NestID[i], sep=" ")
      
      
      adults$JulianDate[adults$NestID==allnests2$NestID[i] & adults$SexMorph=="Female" ] >=allnests2$FirstEggJulian[i] &
        adults$JulianDate[adults$NestID==allnests2$NestID[i] & adults$SexMorph=="Female"] <=allnests2$LastCatchDate[i]
    }
  }
  
  
  if(!is.na(allnests2$BandFem[i])){
    fem <- which(adults$BandNo==allnests2$BandFem[i] & 
                   allnests2$Year[i]==adults$Year & 
                   ((adults$JulianDate>=allnests2$FirstEggJulian[i] &
                       adults$JulianDate<=allnests2$LastCatchDate[i]) | is.na(adults$JulianDate)))
    if(length(fem)==1){
      obs <- fem
    } else {
      if(length(fem)>1){
        #Pick the first measurement after 
        #message("Multiple observations of female from", allnests2$NestID[i], length(fem), sep=" ")
        obs <- fem[which.min(abs(allnests2$HatchJulian[i] -adults$JulianDate[fem]))]
      } else {
        message("Female ", allnests2$BandFem[i], "from nest ", allnests2$NestID[i], "may not be truly from this nest", sep=" ")
      }
      
      #
    } 
    allnests2$FemaleCaptureDate[i] <- as.character(adults$Date[obs])
    allnests2$FemaleCaptureDateJulian[i] <- adults$JulianDate[obs]
    allnests2$FemaleMass[i] <- adults$Mass[obs]
    allnests2$FemaleWingChord[i] <- adults$Wing[obs]
    allnests2$FemaleAgeClass[i] <- as.character(adults$AgeClass[obs])
    allnests2$FemaleYears[i] <- adults$AgeYrs[obs]
    allnests2$FemaleAgeAccuracy[i] <- as.character(adults$AgeAccuracy[obs])
    allnests2$FemaleRecap[i] <-as.character(adults$Retrap[obs])
  }
  
  
  
  #MALES
  if(is.na(allnests2$BandMale[i])){
    if(any(adults$NestID==allnests2$NestID[i] & adults$SexMorph=="Male" )){
      message("confirm no male ID for nest" ,allnests2$NestID[i], sep=" ")
      
      
      adults$JulianDate[adults$NestID==allnests2$NestID[i] & adults$SexMorph=="Male" ] >=allnests2$FirstEggJulian[i] &
        adults$JulianDate[adults$NestID==allnests2$NestID[i] & adults$SexMorph=="Male"] <=allnests2$LastCatchDate[i]
    }
  }
  
  
  if(!is.na(allnests2$BandMale[i])){
    male <- which(adults$BandNo==allnests2$BandMale[i] & 
                    allnests2$Year[i]==adults$Year & 
                    ((adults$JulianDate>=allnests2$FirstEggJulian[i] &
                        adults$JulianDate<=allnests2$LastCatchDate[i]) | is.na(adults$JulianDate)))
    if(length(male)==1){
      obs <- male
    } else {
      if(length(male)>1){
        #Pick the first measurement after 
        #message("Multiple observations of female from", allnests2$NestID[i], length(fem), sep=" ")
        obs <- male[which.min(abs(allnests2$HatchJulian[i] -adults$JulianDate[male]))]
      } else {
        message("Male ", allnests2$BandMale[i], "from nest ", allnests2$NestID[i], "may not be truly from this nest", sep=" ")
      }
      
      #
    } 
    allnests2$MaleCaptureDate[i] <- as.character(adults$Date[obs])
    allnests2$MaleCaptureDateJulian[i] <- adults$JulianDate[obs]
    allnests2$MaleMass[i] <- as.numeric(adults$Mass[obs])
    allnests2$MaleWingChord[i] <- adults$Wing[obs]
    allnests2$MaleAgeClass[i] <- as.character(adults$AgeClass[obs])
    allnests2$MaleYears[i] <- adults$AgeYrs[obs]
    allnests2$MaleAgeAccuracy[i] <- as.character(adults$AgeAccuracy[obs] )
    allnests2$MaleRecap[i] <-as.character(adults$Retrap[obs])
  }
  
}

#Females and males all fixed. Main problem is the renests--males and females all listed as
#from the first nest, even when the dates just odn't make any sense




ggplot(allnests2, aes()) 


hist(allnests2$FemaleCaptureDateJulian-allnests2$HatchJulian)
#Most females were caught after the nestlings hatched. 


hist(allnests2$MaleCaptureDateJulian-allnests2$HatchJulian)
#males captured on the same date as the female




#Body condition looks like it declines a lot with laying date.  
ggplot(allnests2, aes(x=FirstEggJulian, y=MaleMass))+
  geom_point()

ggplot(allnests2, aes(x=FirstEggJulian, y=FemaleMass))+
  geom_point()




##Assign whether these nests are renests or not BASED ON THE MALE'S IDENTITY

for (male in unique(allnests2$BandMale[!is.na(allnests2$BandMale)])){
  for (year in c(1999, 2000)){
    #for each unique male, at each year, pull out all their nests (by row number)
    nr <- which(allnests2$BandMale==male & allnests2$Year==year)
    #if they have more than one nest in a given year, then we need to label the second one as a renest. 
    if(length(nr)>1){
      message(male, "in year", year, "has more than one nest")
      
      #whichever nest had the earliest first egg date is the First nest
      allnests2$RenestStatus [nr[which.min(allnests2$FirstEggDate[nr])]] <- "First"
      #all others are renests
      allnests2$RenestStatus [nr[-which.min(allnests2$FirstEggDate[nr])]] <- "Renest"
      
    } else {
      if(length(nr)==1){
        #if there's only on nest that year then that nest is for sure a first nest
        allnests2$RenestStatus <- "First"
      }
      #other alternative lumped in here is that there is no nest for that bird
      #this year--in that case we can just do nothing.
    }
    
  }
  
}

#UPSHOT: Every time a male is listed twice, they are probably actuallynot from
#one of those nests. It doesn't make sense otherwise. They'd have to have been
#polyamorous which we know doesn't really happen. More likely they were
#floating.

#312125996 is listed as being from both box MC38 and MC39 in 2000. That's
#probably not correct. He is probably associated with only one of these boxes
#and was just checking out the second box when he as caught. Alternatively,
#might be a copy error. I will check
adults[adults$BandNo=="312125996",] 
#really could be either one. The two nests are pretty similar and on similar
#schedules. Shit


#same thing with male 165103810 and bos SL30 and SL31 in 2000
#FIXED : SL31 should be male 312126514


#same thing with male 165103833 and boxes SL70 and 72 in 2000
#3 birds listed here 165103833	female 165103829 and female 312125632
adults[adults$BandNo=="312125632",]
adults[adults$BandNo=="165103829",] #definitely from box 70 not box 72
adults[adults$BandNo=="165103833",] #caught at site 72 on June 17, SL 70 on Jun 28

#FIXED 
allnests2[which(allnests2$BandMale=="165103833"),] #caught at site 72 on June 17, SL 70 on Jun 28
#probably from SL 72 then, not SL 70 
  

#312125658 in SL45 and SL64  (2000) seems unlikely but is possible. Odd though
#because first egg date of second nest is the hatch date of the first nest.
allnests2[which(allnests2$BandMale=="312125658"),] 
adults[adults$BandNo=="312125658",]




#Still 2 birds that are probably mistakenly listed as from 2 nests (312125996
#and 312125658)

rm(fem, i, male, nr, obs, year)


##########################################
hist(allnests2$BroodSize)

nestlings <- read.csv("~/Montogomerie Work/TRES Plumage/ARC TRES data/Nestling Morphometrics.csv", na.strings = "")

nestlingSex <- read.csv("~/Montogomerie Work/TRES Plumage/ARC TRES data/Nestlings Sexes (TRES CHD SEXING 99-00).csv", na.strings = "")

#1999 nestlings are labeled based on Nestling ID. Need to match them up based on BoxID and last bit of their ID

nestlingSex$Site <- ifelse("m"==substr(nestlingSex$NestlingID, 1,1),"MC" ,"SL" )

nestlingSex$BoxNum<- str_extract(nestlingSex$NestlingID, "\\-*\\d+\\.*\\d*")
nestlingSex$MARK<- gsub(nestlingSex$NestlingID, pattern="[a-z]+[0-9]+", replacement = "")



nestlings$Sex <- NA
for(i in 1:nrow(nestlings)){
  if(nestlings$Year[i]==2000){
    if( any(nestlingSex$BIN==nestlings$BIN[i], na.rm=T)){
      nestlings$Sex[i] <- as.character(nestlingSex$CHDSex.YEZ[which(nestlingSex$BIN==nestlings$BIN[i])])
    }
  } else {
    # year ==1999
    if(any(nestlingSex$YEAR==nestlings$Year[i] &
           nestlingSex$MARK==nestlings$Mark[i] & 
           nestlingSex$Site==nestlings$SiteID[i]&
           nestlingSex$BoxNum==nestlings$BoxID[i], na.rm=T)){
      nestlings$Sex[i] <- as.character(nestlingSex$CHDSex.YEZ[which(nestlingSex$YEAR==nestlings$Year[i] &
                                                                      nestlingSex$MARK==nestlings$Mark[i] & 
                                                                      nestlingSex$Site==nestlings$SiteID[i]&
                                                                      nestlingSex$BoxNum==nestlings$BoxID[i])])
    }
  }
 
}

nestlings$BIN

rm(nestlingSex, i, x, allnests)

nestlings$NestID <- paste(nestlings$Year, nestlings$SiteID, nestlings$BoxID, sep="-")

#add nestling band numbers/ ID codes to the nests that they were from. 
allnests2 <- allnests2 %>% mutate(Nestling1=NA, 
                     Nestling2=NA,
                     Nestling3=NA, 
                     Nestling4=NA, 
                     Nestling5=NA, 
                     Nestling6=NA, 
                     Nestling7=NA, 
                     Nestling8=NA, 
                     Nestling9=NA, 
                     Sex1=NA, 
                     Sex2=NA,
                     Sex3=NA, 
                     Sex4=NA, 
                     Sex5=NA, 
                     Sex6=NA, 
                     Sex7=NA, 
                     Sex8=NA, 
                     Sex9=NA, 
                     )


for(i in 1:nrow(allnests2)){
  
  if(any(nestlings$NestID==allnests2$NestID[i])){
    chicks <- nestlings %>% filter(allnests2$NestID[i]==NestID)
    
    IDs <- ifelse(is.na(chicks$BandNo.modified.by.Hussell), as.character(chicks$Mark), chicks$BandNo.modified.by.Hussell)
    allnests2[i, 47: (46 + nrow(chicks))] <- IDs
    allnests2[i, 56: (55+nrow(chicks))] <- as.character(chicks$Sex)
  }

}

#Calculate sex ratios for each nest (female)

allnests2$SexRatioF <- ifelse(rowSums(!is.na(allnests2[,56:64]))==0, #if none of the nestlings were sexed, 
                              NA, #return NA
                              rowSums(allnests2[,56:64]=="F", na.rm = T)/rowSums(!is.na(allnests2[,56:64]))) #else return the female nestlings out of total nestlings


ggplot(allnests2, aes(x=FirstEggJulian, y=SexRatioF))+
  geom_count()+
  facet_grid(~Year)+
  geom_smooth(method="glm", method.args=list(family="binomial"))
#Possibly more female offspring later in the season, but likely not significant. 


write.csv(allnests2, "C:/Users/11arc/OneDrive/Documents/Montogomerie Work/TRES Plumage/ARC TRES data/Compiled TRES Data/Nest Data 1999-2000.csv", na="", row.names = F)
#Might want to reorder and only export the good date columns. 
