#How does provisioning rate relate to plumage color? 

feeding <- read.csv("~/Montogomerie Work/TRES Plumage/ARC TRES data/Provisioning Data 1999 (Feeding Rates 1999).csv", na.strings = "")[,2:26] %>% 
  select(-c(fspecd, fspect, mspecd, mspect, fvis1nes, mvis1nes, fvis2nes, mvis2nes, Fband2))%>% 
  mutate(Year=1999)
  
names(feeding) <- c("BoxNo", "SiteID", "FemaleID", "MaleID", 
                    "BroodSize_1", "BroodSize_2", "NestlingAge_1", 
                    "DateJulian_1","Time_1", "FemaleVisitRate_1", "MaleVisitRate_1", 
                    "DateJulian_2","Time_2", "FemaleVisitRate_2", "MaleVisitRate_2",
                    "DataGood", "Year" )
feeding$SiteID<- recode(feeding$SiteID, sl="SL", mc="MC")
feeding$BoxNo <- substr(feeding$BoxNo, 2, nchar(as.character(feeding$BoxNo)))
feeding$FemaleID <- as.character(feeding$FemaleID)
feeding$MaleID <- as.character(feeding$MaleID)
feeding$NestID <- paste(feeding$Year, feeding$SiteID, feeding$BoxNo, sep="-")


adultSpec <- read.csv( "~/Montogomerie Work/TRES Plumage/ARC TRES data/Compiled TRES Data/All Adult Spec.csv", na.strings = "")[, c(1:16, 81:87)]
names(adultSpec)

adultSpec2<- adultSpec %>% 
  select(-c(SiteID, BoxID, Retrap, Sex, Wing, Mass, SMI, Date )) %>% 
  mutate(BandNo=as.character(BandNo)) %>% 
  filter(Year==1999 & !is.na(RC1_blue))

#add female Spec Data
feeding2 <- left_join(feeding, adultSpec2, by=c("FemaleID" ="BandNo", "NestID"="NestID", "Year"="Year"))

#Add male spec data
feeding3 <- left_join(feeding2, adultSpec2, by=c("MaleID" ="BandNo", "NestID"="NestID", "Year"="Year"), suffix=c("_female", "_male"))

feeding3$MeanFemaleVisitRate <- (feeding3$FemaleVisitRate_1 + feeding3$FemaleVisitRate_2)/2

feeding3$MeanMaleVisitRate <- (feeding3$MaleVisitRate_1 + feeding3$MaleVisitRate_2)/2




ggplot(feeding3, aes(x=FemaleVisitRate_1, y=FemaleVisitRate_2))+
  geom_point()
#Pretty correlated


ggplot(feeding3, aes(x=MaleVisitRate_1, y=MaleVisitRate_2))+
  geom_point()
#NOT correlated (or at least not strongly correlated)


ggplot(feeding3, aes(x=MeanFemaleVisitRate , y=MeanMaleVisitRate))+
  geom_point() +
  labs(x="Female provisioning rate (visits/hr)", y="Male provisioning rate (visits/hr)")+
  theme_classic()+
  geom_smooth(method="lm")

#As females feed more, so do their males. 



ggplot(feeding3, aes(x=BroodSize_1 , y=MeanMaleVisitRate))+
  geom_point()

ggplot(feeding3, aes(x=BroodSize_1 , y=MeanFemaleVisitRate))+
  geom_point() 

#No indication that brood size is playing a role 


#Do bluer or whiter males 

options(na.action="na.fail")
malefeeding <- feeding3 %>% filter(!is.na(RC1_blue_male) & !is.na(MeanMaleVisitRate))
mod <- lm(MeanMaleVisitRate ~ BroodSize_1 + RC1_blue_male + RC2_blue_male + RC3_blue_male + RC1_white_male + RC2_white_male , data=malefeeding)
plot(mod)
dredge(mod)
avg <- model.avg(dredge(mod), subset= delta < 2, revised.var = TRUE )
summary(avg)
#male color is not indicitive of their feeding rate

femalefeeding_male <- feeding3 %>% filter(!is.na(RC1_blue_male) & !is.na(MeanFemaleVisitRate))
mod <- lm(MeanFemaleVisitRate ~ BroodSize_1 + RC1_blue_male + RC2_blue_male + RC3_blue_male + RC1_white_male + RC2_white_male , data=femalefeeding_male)
plot(mod)
dredge(mod)
avg <- model.avg(dredge(mod), subset= delta < 2, revised.var = TRUE )
summary(avg)
#male color also does not cause females to increase their feeding rates



femalefeeding <- feeding3 %>% filter(!is.na(RC1_blue_female) & !is.na(MeanFemaleVisitRate) & AgeClass2_female !="SY")
mod <- lm(MeanFemaleVisitRate ~ BroodSize_1 + RC1_blue_female + RC2_blue_female + RC3_blue_female + RC1_white_female + RC2_white_female , data=femalefeeding)
plot(mod)
dredge(mod)
avg <- model.avg(dredge(mod), subset= delta < 2, revised.var = TRUE )
summary(avg)
#female color is not indicitive of her feeding rate

malefeeding_female <- feeding3 %>% filter(!is.na(RC1_blue_female) & !is.na(MeanMaleVisitRate) & AgeClass2_female !="SY")
mod <- lm(MeanMaleVisitRate ~ BroodSize_1 + RC1_blue_female + RC2_blue_female + RC3_blue_female + RC1_white_female + RC2_white_female , data=femalefeeding)
plot(mod)
dredge(mod)
avg <- model.avg(dredge(mod), subset= delta < 2, revised.var = TRUE )
summary(avg)
#Males adjust their feeding rate based on the female's blueness (RC1_blue).
#Bluer females have lazier males.




feeding3$AgeClass3_female <- ifelse(feeding3$AgeClass2_female!="SY" | is.na(feeding3$AgeClass2_female) , "ASY", "SY")


ggplot(feeding3, aes(x=RC1_blue_female, y=MeanMaleVisitRate))+
  geom_smooth(method="lm", data=malefeeding_female, color="black")+
  geom_point(aes(color=AgeClass3_female))+
  labs(x="Female blue RC1", y="Male provisioning rate (visits/hr)", color="Female Age")+
  theme_classic()
ggsave(filename="C:/Users/11arc/OneDrive/Documents/Montogomerie Work/TRES Plumage/TRES Plumage Plots/Plumage and Provisioning Plot.png", 
       height=3, 
       width=4, 
       units="in")

#This is different from Dakin et al. 2016 BUT they were that super super crappy
#weather year at QUBS so possible that that made the results very different.


