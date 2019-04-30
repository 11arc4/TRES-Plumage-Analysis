

library(tidyverse)
library(MuMIn)

options(na.action = "na.fail")


pat <- read.csv("~/Montogomerie Work/TRES Plumage/ARC TRES data/TRES pat.csv", na.strings = "")
bird <- read.csv("~/Montogomerie Work/TRES Plumage/ARC TRES data/ALL SWALLOWS.csv", na.strings = "")

pat2 <- pat %>% select(BIN, Source, Sex, NestMatch, Dad_Cervus, Dad_NewPat)

pat$BIN <- factor(pat$BIN)
bird$BIN <- factor(bird$BIN)

nestlingpat <- full_join(pat2, bird) %>% filter(Source %in% c("nestling", "nestling from other nest 2000SL11"))

#Remove words "(null)" in the NEWPAT dat labels

nestlingpat$Dad_NewPat<- gsub(pattern="(null)", replacement = "", x=nestlingpat$Dad_NewPat, fixed="T")




#if paternity was only assigned based on one method, go with that method. 
#if paternity was assigned by both methods and agrees, go with that paternity. 

#if there is a discrepency based on the two methods, report both males ID and
#make a list of nestlings with discrepencies.
nestlingpat$Dad_Cervus <- as.character(nestlingpat$Dad_Cervus)
nestlingpat$Dad_NewPat <- as.character(nestlingpat$Dad_NewPat)

nestlingpat$FathersID <- NA




#919 AND 920 BINS don't seem to correspong to ANY birds. I will set them to NA for now. 
View(nestlingpat %>% filter(Dad_NewPat %in% c(919, 920) |Dad_Cervus %in% c(919, 920)  ))


nestlingpat$Dad_Cervus[which(nestlingpat$Dad_Cervus %in% c(919, 920, "Cervus"))] <- NA
nestlingpat$Dad_NewPat[which(nestlingpat$Dad_NewPat %in% c(919, 920, "NewPat"))] <- NA


for (i in 1: nrow(nestlingpat)){
  if (is.na(nestlingpat$Dad_Cervus[i]) & is.na(nestlingpat$Dad_NewPat[i])){
    nestlingpat$FathersID[i] <-  "Unknown"
    
  } else {
    if (is.na(nestlingpat$Dad_Cervus[i]) | is.na(nestlingpat$Dad_NewPat[i])){
      if(is.na(nestlingpat$Dad_Cervus[i])){
        nestlingpat$FathersID[i] <- bird$BandNo[bird$BIN==nestlingpat$Dad_NewPat[i]]
        
      } else {
        if(any(bird$BIN==nestlingpat$Dad_Cervus[i])){
          nestlingpat$FathersID[i] <- bird$BandNo[bird$BIN==nestlingpat$Dad_Cervus[i]]
          #if that's a known BIN number
        } else {
          message(nestlingpat$NestlingID[i], "should be from father with BIN ",nestlingpat$Dad_Cervus[i], "but BIN unknoww", sep=" " )
        }
        
      }
    } else {
      #Know ID based on both CERVUS and NEWPAT
      if (nestlingpat$Dad_Cervus[i]==nestlingpat$Dad_NewPat[i]){
        if(any(bird$BIN==nestlingpat$Dad_Cervus[i])){
          nestlingpat$FathersID[i] <- bird$BandNo[bird$BIN==nestlingpat$Dad_Cervus[i]]
          #if that's a known BIN number
        } else {
          message(nestlingpat$NestlingID[i], "should be from father with BIN ",nestlingpat$Dad_Cervus[i], "but BIN unknoww", sep=" " )
        }
        
      }  else {
        #CERVUS and NEWPAT disagree
        #Probably want to pick cervus father
        sex <- c(NA, NA)
        sex[1] <- as.character(bird$Sex1[bird$BIN==nestlingpat$Dad_Cervus[i]])
        sex[2] <- as.character(bird$Sex1[bird$BIN==nestlingpat$Dad_NewPat[i]])
        message("Paternity confusion at row ", i, "fathers sex ", paste(sex, collapse ="/"), sep=" ")
        nestlingpat$FathersID[i] <- paste(bird$BandNo[bird$BIN==nestlingpat$Dad_Cervus[i]], bird$BandNo[bird$BIN==nestlingpat$Dad_NewPat[i]], sep="/")
        
        
      }
    }
    
  }
}

nestlingpat$FathersID [576]<- "165105441"

#Which ones have disagreement? 38 nestlings
View(nestlingpat %>% filter(grepl(FathersID, pattern="/")))
                                


nestlingpat2 <- nestlingpat %>% select(BandNo, NestlingID, Year, SiteID, BoxID,  FathersID)

nest <- read.csv("C:/Users/11arc/OneDrive/Documents/Montogomerie Work/TRES Plumage/ARC TRES data/Compiled TRES Data/Nest Data 1999-2000.csv", na.strings = "")

nest$Box.No <- as.factor(nest$Box.No)

nestlingpat3<- full_join(nestlingpat2, nest %>% filter(BroodSize>0), by=c("Year"="Year","SiteID"="Site.ID", "BoxID"="Box.No" ))



for(i in 1:nrow(nestlingpat3)){
  nestlingpat3$WithinPairOffspring[i]<- grepl(nestlingpat3$FathersID[i], pattern=nestlingpat3$BandMale[i])
}

nestlingpat3$WithinPairOffspring[nestlingpat3$FathersID=="Unknown"] <- FALSE

View(nestlingpat3 %>% select(BandMale, FathersID, WithinPairOffspring, NestID))


nestPat <- nestlingpat3 %>% 
  group_by(Year, SiteID, BoxID) %>% 
  mutate(EPY=sum(WithinPairOffspring==TRUE), 
         WPY=sum(WithinPairOffspring==FALSE)) %>%
  select(c(Year, SiteID, BoxID, Attempt, BandMale, BandFem, EPY, WPY, FirstEggJulian))%>%
  summarise_all(first)
  
  
  
View(nestPat %>% select(BandMale, FathersID, WithinPairOffspring, NestID, EPY, WPY))


hist(nestPat$EPY)
hist(nestPat$WPY)

hist(nest)

ggplot(nestPat %>% filter(!is.na(Year)), aes(100*WPY/(WPY+EPY)))+
  geom_histogram()+
  facet_grid(Year~.)+
  labs(x="% WPY")


nestPat$LateNest <- factor(ifelse(as.integer(nestPat$BoxID) >60, "Late", ifelse(nestPat$FirstEggJulian>153 & !is.na(nestPat$FirstEggJulian), "Late", "Normal")), levels=c("Normal", "Late"))

#Merge nest data, plumage Data, and paternity data
adultsSpec <- read.csv("~/Montogomerie Work/TRES Plumage/ARC TRES data/Compiled TRES Data/All Adult Spec.csv", na.strings = "")

adultsSpec$BoxID <- as.character(adultsSpec$BoxID)
nestPat$BandFem <- as.character(nestPat$BandFem)
nestPat$BandMale <- as.character(nestPat$BandMale)

nestPat_male <- inner_join(adultsSpec, nestPat, by = c("SiteID", "BoxID", "Year", "BandNo"="BandMale"))
nestPat_female <- inner_join(adultsSpec, nestPat, by = c("SiteID", "BoxID", "Year", "BandNo"="BandFem"))




#Do any of males color variables influence paternity? 
nestPat_male2 <- nestPat_male %>% 
  filter(!is.na(WPY) & !is.na(RC1_blue) & LateNest=="Normal") %>%
  mutate(SiringSuccess= WPY/(WPY+EPY))
  
mod<- glm(WPY ~ RC1_blue + RC2_blue + RC3_blue + RC1_white + RC2_white + Year, 
         data=nestPat_male2, family="poisson") 
plot(mod)
hist(resid(mod)) #Not the greatest, does look a little bimodal, but also not aweful
dredge(mod)
avg <- model.avg(dredge(mod), subset= delta < 2, revised.var = TRUE )
summary(avg)
#Maybe, but not a strong effect at all. 

ggplot(nestPat_male, aes(x=RC2_blue, y=WPY))+
  geom_point()+
  geom_smooth(method="glm", method.args=list(family="poisson"))+
  facet_grid(~Year)


mod<- glm(SiringSuccess ~ RC1_blue + RC2_blue + RC3_blue + RC1_white + RC2_white + Year, 
          data=nestPat_male2, family="binomial") 
plot(mod)
hist(resid(mod)) #Not the greatest, does look a little bimodal, but also not aweful
dredge(mod)
avg <- model.avg(dredge(mod), subset= delta < 2, revised.var = TRUE )
summary(avg)


ggplot(nestPat_male2, aes(x=RC2_blue, y=SiringSuccess))+
  geom_point()+
  geom_smooth(method="glm", method.args=list(family="binomial"))+
  facet_grid(~Year)
#Slightly higher success siring your own offspring if you're bluer (RC2_blue increased), but it's quite a weak relationship. 



#How about the females? Do they cheat less depening on their color?
nestPat_female2 <- nestPat_female %>% 
  filter(!is.na(WPY) & !is.na(RC1_blue)& LateNest=="Normal")%>%
  mutate(SiringSuccess= WPY/(WPY+EPY))

mod<- glm(WPY ~ RC1_blue + RC2_blue + RC3_blue + RC1_white + RC2_white + Year, 
         data=nestPat_female2, 
         family="poisson") 
plot(mod)
hist(resid(mod)) #Not normal at all. Looks almost bimodal actually
dredge(mod)
avg <- model.avg(dredge(mod), subset= delta < 2, revised.var = TRUE )
summary(avg)
#No not really

ggplot(nestPat_female2, aes(x=RC1_blue, y=WPY))+
  geom_point()+
  geom_smooth(method="glm", method.args=list(family="poisson"))+
  facet_grid(~Year)

ggplot(nestPat_female2, aes(x=RC2_white, y=WPY))+
  geom_point()+
  geom_smooth(method="glm", method.args=list(family="poisson"))+
  facet_grid(~Year)


mod<- glm(SiringSuccess ~ RC1_blue + RC2_blue + RC3_blue + RC1_white + RC2_white + Year, 
          data=nestPat_female2, family="binomial") 
plot(mod)
hist(resid(mod)) #Not the greatest, does look a little bimodal, but also not aweful
dredge(mod)
avg <- model.avg(dredge(mod), subset= delta < 2, revised.var = TRUE )
summary(avg)
#Really no effects of female color on male siring success. 


#Are there differences in levels of cheating in early and late nests? 
nestPat2000<- nestPat %>% filter(Year==2000 & !is.na(WPY)) %>% 
  mutate(SiringSuccess=WPY/(EPY+WPY))

mod <- glm(WPY ~ LateNest, data=nestPat2000, family="poisson")
plot(mod)
hist(resid(mod)) #Not the greatest, does look a little bimodal, but also not aweful
dredge(mod)
summary(mod)
#Slightly fewer WPY when you're a late nest. 

ggplot(nestPat2000, aes(x=LateNest, y=WPY))+
  geom_count()

ggplot(nestPat2000, aes(x=LateNest, y=WPY))+
  geom_boxplot()

ggplot(nestPat2000, aes(x=LateNest, y=SiringSuccess))+
  geom_count()
