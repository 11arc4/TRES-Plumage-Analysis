#Which color variables are most indicitive of breeding success and body
#condition?

#load libraries
library(psych)
library(tidyverse)
library(MuMIn)

options(na.action = "na.fail")


adultsSpec <- read.csv("~/Montogomerie Work/TRES Plumage/ARC TRES data/Compiled TRES Data/Adult Plumage and Morphometrics in Standard Daylight.csv", na.strings = "")

#Calculate SMI for the males and females seperately 

# smiMod_F <- lmodel2::lmodel2(log(Mass) ~ log(Wing), data=adultsSpec %>% filter(Sex=="Female"))
# BSMA_F <- smiMod_F$regression.results[3,]
# 
# smiMod_M <- lmodel2::lmodel2(log(Mass) ~ log(Wing), data=adultsSpec %>% filter(Sex=="Male"))
# BSMA_M <- smiMod_M$regression.results[3,]
# 
# 
# L0_F <- mean(adultsSpec$Wing[adultsSpec$Sex=="Female"], na.rm=T)
# L0_M <- mean(adultsSpec$Wing[adultsSpec$Sex=="Male"], na.rm=T)
# #Females are slightly smaller than males 
# 
# 
# #there aren't any birds where they were measured (mass and wingchord) but not sexed
# adultsSpec$SMI <- ifelse(adultsSpec$Sex=="Female", 
#                          adultsSpec$Mass * ((L0_F/adultsSpec$Wing)^BSMA_F[[3]]), 
#                          adultsSpec$Mass * ((L0_M/adultsSpec$Wing)^BSMA_M[[3]]))
# 
# 


# 
# #Calculate a PCA of ALL color variables for the males and females seperately
# 
# 
# #First take all our color data for the males and put it into a PCA
# fit <- prcomp(scale(adultsSpec[which(adultsSpec$Sex=="Male" & rowSums(is.na(adultsSpec[,41:70]))==0), 41:70]))
# summary(fit) # print variance accounted for
# fit # pc loadings
# plot(fit,type="lines") # scree plot
# biplot(fit) 
# #Should probably have 3 axes, explains 45% of variation
# 
# #Varimax rotate the PCA into 2 axes.
# PCA_male <- principal(scale(adultsSpec[which(adultsSpec$Sex=="Male" & rowSums(is.na(adultsSpec[,41:70]))==0), 41:70]), 
#                      nfactors=3, 
#                      rotate = "varimax", 
#                      missing=F)
# summary(PCA_male)
# PCA_male
# 
# 
# adultsSpec$RC1[which(adultsSpec$Sex=="Male" & rowSums(is.na(adultsSpec[,41:70]))==0)] <- predict(PCA_male, 
#                                                                                                  data=scale(adultsSpec[which(adultsSpec$Sex=="Male" & rowSums(is.na(adultsSpec[,41:70]))==0), 41:70]))[,1] 
# 
# adultsSpec$RC2[which(adultsSpec$Sex=="Male" & rowSums(is.na(adultsSpec[,41:70]))==0)] <- predict(PCA_male, 
#                                                                                                  data=scale(adultsSpec[which(adultsSpec$Sex=="Male" & rowSums(is.na(adultsSpec[,41:70]))==0), 41:70]))[,2] 
# adultsSpec$RC3[which(adultsSpec$Sex=="Male" & rowSums(is.na(adultsSpec[,41:70]))==0)] <- predict(PCA_male, 
#                                                                                                  data=scale(adultsSpec[which(adultsSpec$Sex=="Male" & rowSums(is.na(adultsSpec[,41:70]))==0), 41:70]))[,3] 
# 
# 
# 
# 
# #Now for the females
# fit <- prcomp(scale(adultsSpec[which(adultsSpec$Sex=="Female" & rowSums(is.na(adultsSpec[,41:70]))==0), 41:70]))
# summary(fit) # print variance accounted for
# fit # pc loadings
# plot(fit,type="lines")
# biplot(fit) 
# #Not as clear of a break as the males-- might require a 4th axis. 50% variation at 3 axes, 57 at 4
# 
# #Varimax rotate the PCA into 3 axes.
# PCA_female <- principal(scale(adultsSpec[which(adultsSpec$Sex=="Female" & rowSums(is.na(adultsSpec[,41:70]))==0), 41:70]), 
#                       nfactors=3, 
#                       rotate = "varimax", 
#                       missing=F)
# summary(PCA_female)
# PCA_female
# 
# adultsSpec$RC1[which(adultsSpec$Sex=="Female" & rowSums(is.na(adultsSpec[,41:70]))==0)] <- predict(PCA_female, 
#                                                                                                  data=scale(adultsSpec[which(adultsSpec$Sex=="Female" & rowSums(is.na(adultsSpec[,41:70]))==0), 41:70]))[,1] 
# 
# adultsSpec$RC2[which(adultsSpec$Sex=="Female" & rowSums(is.na(adultsSpec[,41:70]))==0)] <- predict(PCA_female, 
#                                                                                                  data=scale(adultsSpec[which(adultsSpec$Sex=="Female" & rowSums(is.na(adultsSpec[,41:70]))==0), 41:70]))[,2] 
# adultsSpec$RC3[which(adultsSpec$Sex=="Female" & rowSums(is.na(adultsSpec[,41:70]))==0)] <- predict(PCA_female, 
#                                                                                                  data=scale(adultsSpec[which(adultsSpec$Sex=="Female" & rowSums(is.na(adultsSpec[,41:70]))==0), 41:70]))[,3] 



adultsSpec$AgeClass2 <- ifelse(adultsSpec$Sex=="Female", ifelse(adultsSpec$AgeClass=="SY", "SY", "ASY"), "AHY")






#Even the rotated PCs aren't a very good description of bird color. I wonder if
#splitting the blue body parts from the white body parts might allow us to make
#2 pcs that better explain each body part? In an ideal world I would love to
#have the PCs compareable males to females...
whiteparts <- c("belly", "undertail.coverts", "throat", "breast")
blueparts <- c("back", "crown",  "rump", "nape",  "lesser.coverts") #Tail is black not blue so maybe shouldn't be in there? 

#lesser coverts are the epaulets, undertail coverlets are the bottom of the tail

#Everyone's white parts
fit <- prcomp(scale(adultsSpec[41:70][which(rowSums(is.na(adultsSpec[,41:70]))==0), grep(paste(whiteparts,collapse="|"), names(adultsSpec)[41:70])]))
summary(fit) # print variance accounted for
fit # pc loadings
plot(fit,type="lines")
biplot(fit) 
#Can use 2 PCs to explain white parts and get 53% of variance. Not too bad. Scree plot is funny looking though


fit <- prcomp(scale(adultsSpec[41:70][which( rowSums(is.na(adultsSpec[,41:70]))==0), grep(paste(blueparts,collapse="|"), names(adultsSpec)[41:70])]))
summary(fit) # print variance accounted for
fit # pc loadings
plot(fit,type="lines")
biplot(fit)
#can use 3 PCs to explain 71% of variance in blue parts (excluding the tail)

PCA_blue <- principal(scale(adultsSpec[41:70][which( rowSums(is.na(adultsSpec[,41:70]))==0), grep(paste(blueparts,collapse="|"), names(adultsSpec)[41:70])]), 
                        nfactors=3, 
                        rotate = "varimax", 
                        missing=F)
summary(PCA_blue)
PCA_blue
#RC1_blue correlates with theta mainly, and phi of the crown and nape. 
#RC2_blue correlates with all R.achieved values
#RC3_blue correlates with Phi, particularly of the back and rump

adultsSpec$RC1_blue[rowSums(is.na(adultsSpec[,41:70]))==0] <-
  predict(PCA_blue, data=scale(adultsSpec[41:70][which( rowSums(is.na(adultsSpec[,41:70]))==0), grep(paste(blueparts,collapse="|"), names(adultsSpec)[41:70])]))[,1] 

adultsSpec$RC2_blue[rowSums(is.na(adultsSpec[,41:70]))==0] <-
  predict(PCA_blue, data=scale(adultsSpec[41:70][which( rowSums(is.na(adultsSpec[,41:70]))==0), grep(paste(blueparts,collapse="|"), names(adultsSpec)[41:70])]))[,2] 

adultsSpec$RC3_blue[rowSums(is.na(adultsSpec[,41:70]))==0] <-
  predict(PCA_blue, data=scale(adultsSpec[41:70][which( rowSums(is.na(adultsSpec[,41:70]))==0), grep(paste(blueparts,collapse="|"), names(adultsSpec)[41:70])]))[,3] 


ggplot(adultsSpec %>% filter(!is.na(Sex) & !is.na(AgeClass2)), aes(x=RC1_blue, y=RC2_blue, color=AgeClass2))+
  geom_point()+
  facet_grid(~Sex)

PCA_white <- principal(scale(adultsSpec[41:70][which( rowSums(is.na(adultsSpec[,41:70]))==0), grep(paste(whiteparts,collapse="|"), names(adultsSpec)[41:70])]), 
                      nfactors=2, 
                      rotate = "varimax", 
                      missing=F)
summary(PCA_white)
PCA_white
#RC1_white is color of breast, belly, throat
#RC2_white is color of undertail coverts. 

adultsSpec$RC1_white[rowSums(is.na(adultsSpec[,41:70]))==0] <-
  predict(PCA_white, data=scale(adultsSpec[41:70][which( rowSums(is.na(adultsSpec[,41:70]))==0), grep(paste(whiteparts,collapse="|"), names(adultsSpec)[41:70])]))[,1] 

adultsSpec$RC2_white[rowSums(is.na(adultsSpec[,41:70]))==0] <-
  predict(PCA_white, data=scale(adultsSpec[41:70][which( rowSums(is.na(adultsSpec[,41:70]))==0), grep(paste(whiteparts,collapse="|"), names(adultsSpec)[41:70])]))[,2] 

ggplot(adultsSpec %>% filter(!is.na(Sex) & !is.na(AgeClass2)), aes(x=RC1_white, y=RC2_white, color=AgeClass2))+
  geom_point()+
  facet_grid(~Sex)
#White is a tricky color because it gets dirty as the summer goes on. Blue is
#probably the better body area to be looking at. Hard to say though since bird
#might be picking up on something else.


write.csv(adultsSpec %>% arrange(BandNo),
          "~/Montogomerie Work/TRES Plumage/ARC TRES data/Compiled TRES Data/All Adult Spec.csv", 
          na = "", 
          row.names = F) 




#What doe the plumage RCs mean? ie are they correlated with bluer, greener? 

#blue RCs first

ggplot(adultsSpec, aes(x=BlueChroma_back, y=RC1_blue))+
  geom_point()
ggplot(adultsSpec, aes(x=GreenChroma_back, y=RC1_blue))+
  geom_point()
ggplot(adultsSpec, aes(x=UVChroma_back, y=RC1_blue))+
  geom_point()
#Increasing RC1blue = bluer, no relationship with green or UV

ggplot(adultsSpec, aes(x=BlueChroma_back, y=RC2_blue))+
  geom_point()
ggplot(adultsSpec, aes(x=GreenChroma_back, y=RC2_blue))+
  geom_point()
ggplot(adultsSpec, aes(x=UVChroma_back, y=RC2_blue))+
  geom_point()
#Increasing RC2 = greener, less UV, also some seperation between SY and ASY birds in blueness (less blue as it increases,). 


ggplot(adultsSpec, aes(x=BlueChroma_back, y=RC3_blue, color=AgeClass2))+
  geom_point()
ggplot(adultsSpec, aes(x=GreenChroma_back, y=RC3_blue))+
  geom_point()
ggplot(adultsSpec, aes(x=UVChroma_back, y=RC3_blue))+
  geom_point()
#RC3 increases with increasing blue for the blue birds. Also can be high in SY females, Also increases with greenness and UV

#Now white RCs
#Not sure what to looks at 



#Are plumage colors related to body conditions? 

ggplot(adultsSpec %>% filter(!is.na(Sex)), aes(x=SMI, y=RC1_blue, color=AgeClass2 ))+
  geom_point()+
  facet_grid(~Sex)+
  geom_smooth(method="lm")


ggplot(adultsSpec %>% filter(!is.na(Sex)), aes(x=SMI, y=RC2_blue, color=AgeClass2 ))+
  geom_point()+
  facet_grid(~Sex)+
  geom_smooth(method="lm")

ggplot(adultsSpec %>% filter(!is.na(Sex)), aes(x=SMI, y=RC3_blue, color=AgeClass2 ))+
  geom_point()+
  facet_grid(~Sex)+
  geom_smooth(method="lm")

#Doesn't really look like much is going on here. 





#Are plumage colors related to age? 

ggplot(adultsSpec %>% filter(!is.na(Sex)), aes(x=AgeYrs, y=RC1_blue, color=AgeClass2 ))+
  geom_point()+
  facet_grid(~Sex)+
  geom_smooth(method="lm")


ggplot(adultsSpec %>% filter(!is.na(Sex)), aes(x=AgeYrs, y=RC2_blue, color=AgeClass2 ))+
  geom_point()+
  facet_grid(~Sex)+
  geom_smooth(method="lm")
ggplot(adultsSpec %>% filter(!is.na(Sex)), aes(x=AgeYrs, y=RC3_blue, color=AgeClass2 ))+
  geom_point()+
  facet_grid(~Sex)+
  geom_smooth(method="lm")
#Very much but looks like almost the only difference is the SY females (and
#males are slightly brighter than the females) RC3_blue might be declining in the ASY females with age as well. 





##COMBINE NEST DATA WITH PLUMAGE DATA-- FILE EXPORTED TO DROPBOX SO CAN SKIP THIS AND LOAD IT IN LATER ON
# nest <- read.csv("C:/Users/11arc/OneDrive/Documents/Montogomerie Work/TRES Plumage/ARC TRES data/Compiled TRES Data/Nest Data 1999-2000.csv", na.strings = "")
# 
# 
# nest2 <- nest %>% 
#   select(Year, Site.ID, Box.No, LateBox, BandMale, BandFem, FirstEggJulian, LastEggJulian, HatchJulian, ClutchSize, BroodSize, NumFledged) %>% 
#   rename(FledgeSize=NumFledged) %>% 
#   mutate(BandMale=as.character(BandMale),
#          BandFem=as.character(BandFem))
# 
# 
# male_nests <- inner_join(adultsSpec, nest2, by=c("BandNo"="BandMale", "Year"="Year" ) ) %>% 
#   mutate(NestID = paste(Year, Site.ID, Box.No, sep="-"))%>%
#   select(-c(BandFem, SiteID, BoxID)) %>%
#   select(BandNo, NestID, Site.ID, Box.No, NestID, 
#          everything())
# #Shoudl be 210 males with nests
# 
# female_nests <- inner_join(adultsSpec, nest2, by=c("BandNo"="BandFem", "Year"="Year" ) ) %>% 
#   mutate(NestID = paste(Year, Site.ID, Box.No, sep="-"))%>%
#   select(-c(BandMale, SiteID, BoxID)) %>%
#   select(BandNo, NestID, Site.ID, Box.No, NestID, 
#          everything())
# #should be 222 females with nests. 
# 
# 
# 
# nestSpec <- rbind(male_nests, female_nests)
# 
# 
# write.csv(nestSpec %>% arrange(NestID),
#           "~/Montogomerie Work/TRES Plumage/ARC TRES data/Compiled TRES Data/Nest data with plumage and Morphometrics in Standard Daylight.csv", 
#           na = "", 
#           row.names = F) 
# 






nestSpec <- read.csv( "~/Montogomerie Work/TRES Plumage/ARC TRES data/Compiled TRES Data/Nest data with plumage and Morphometrics in Standard Daylight.csv")

#What questions are we interested in? 


#First egg date
#clutch size
#hatching success
#Fledging success
#body condition

nestSpec <-  nestSpec %>% mutate(HatchRate = BroodSize/ClutchSize, 
                                 FledgeRate = FledgeSize/BroodSize, 
                                 Year =factor(Year))

#Make datasets for each ext that includ eonly nests that weren't laid it late
#boxes (e.g. were from natural variation) AND we speced the parent Only very few
#SY females so we won't look at them here. ALso, will only look at
#non-experimental boxes (excluding floaters!)

male_nests2 <- male_nests  %>%
  filter( !is.na(RC1_blue) & LateBox=="N")%>%
  mutate(HatchRate = BroodSize/ClutchSize, 
         FledgeRate = FledgeSize/BroodSize, 
         Year =factor(Year)) %>% 
  group_by(BandNo, NestID) %>% 
  summarise_all(first) #remove duplicates of 3 males 

female_nests2 <- female_nests %>% 
  filter(!is.na(RC1_blue) & LateBox=="N" & !is.na(AgeClass2) & AgeClass2 !="SY")%>% 
  mutate(HatchRate = BroodSize/ClutchSize, 
         FledgeRate = FledgeSize/BroodSize, 
         Year =factor(Year))%>% 
  group_by(BandNo, NestID) %>% 
  summarise_all(first) #remove duplicates of 6 females 


#Does parental color influence any of these traits based on the natural variation?
#male color on first egg date?
male_nests3 <- male_nests2 %>% filter(!is.na(FirstEggJulian))

mod<- lm(FirstEggJulian ~ RC1_blue + RC2_blue + RC3_blue + RC1_white + RC2_white + Year, data=male_nests3) 
#198 observations, probably fine to use.
plot(mod)
hist(resid(mod))
dredge(mod)
avg <- model.avg(dredge(mod), subset= delta < 2, revised.var = TRUE )
summary(avg)
car::Anova(mod)
#Males that have higher RC2_blue scores (less green, more UV) and higher RC2_white scores started nests earlier

plot_DateA <- ggplot(male_nests2, aes(x=RC2_blue, y=FirstEggJulian))+
  geom_point()+
  geom_smooth(method="lm")+ #points aren't excerting leverage
  labs(x="Male Blue RC2", y="First Egg Date")


plot_DateB <- ggplot(male_nests2, aes(x=RC2_white, y=FirstEggJulian))+
  geom_point()+
  geom_smooth(method="lm")+ #Points aren't excerting leverage
  labs(x="Male White RC2", y="First Egg Date")



#female color on first egg date
female_nests3 <- female_nests2 %>% filter(!is.na(FirstEggJulian) )

mod<- lm(FirstEggJulian ~ RC1_blue + RC2_blue + RC3_blue + RC1_white + RC2_white + Year, data=female_nests3) 
#187 observations, probably fine to use.
plot(mod)
hist(resid(mod))
dredge(mod)
avg <- model.avg(dredge(mod), subset= delta < 2, revised.var = TRUE )
summary(avg)
#Females that have higher RC3_blue scores  lay eggs earlier. ALso some year differences. 

plot_DateC <- ggplot(female_nests2, aes(x=RC3_blue, y=FirstEggJulian, color=Year))+
  geom_point()+
  geom_smooth(method="lm")+ #points aren't excerting leverage
  labs(x="Female Blue RC2", y="First Egg Date")
  #facet_grid(~Year)



#male color on clutch size?
male_nests3 <- male_nests2 %>% filter(!is.na(ClutchSize))

mod<- lm(ClutchSize ~ RC1_blue + RC2_blue + RC3_blue + RC1_white + RC2_white + Year, 
          #family="poisson", 
          data=male_nests3) 
#179 observations, probably fine to use.
plot(mod)
hist(resid(mod))
dredge(mod)
avg <- model.avg(dredge(mod), subset= delta < 2, revised.var = TRUE )
summary(avg)
#Bluer and greener males have larger clutch sizes (higher RC1_blue and RC2_blue)


#female color on clutch size
female_nests3 <- female_nests2 %>% filter(!is.na(ClutchSize))

mod<- lm(ClutchSize ~ RC1_blue + RC2_blue + RC3_blue + RC1_white + RC2_white + Year , 
         #family="poisson",
         data=female_nests3) 
#187 observations, probably fine to use.
hist(resid(mod))
plot(mod)
hist(resid(mod))
dredge(mod)
avg <- model.avg(dredge(mod), subset= delta < 2, revised.var = TRUE )
summary(avg)
#Female color not associated with clutch size  


#male color on hatch rate?
male_nests3 <- male_nests2 %>% filter(!is.na(BroodSize))

mod<- lm(BroodSize ~ RC1_blue + RC2_blue + RC3_blue + RC1_white + RC2_white + Year, 
          #family="poisson", 
          data=male_nests3) 
#178 observations, probably fine to use.
#underdispersed if poisson, but seems to fit normal
plot(mod)
hist(resid(mod))
dredge(mod) # LOT of models with delta <2
avg <- model.avg(dredge(mod), subset= delta < 2, revised.var = TRUE )
summary(avg)
#Male color does not influence brood size. 

#female color on brood size
female_nests3 <- female_nests2 %>% filter(!is.na(BroodSize) )

mod<- lm(BroodSize ~ RC1_blue + RC2_blue + RC3_blue + RC1_white + RC2_white + Year , 
          #family="poisson",
          data=female_nests3) 
#186 observations, probably fine to use.
plot(mod)
hist(resid(mod))
dredge(mod)
avg <- model.avg(dredge(mod), subset= delta < 2, revised.var = TRUE )
summary(avg)
#Female color not associated with brood size either. 




#male color on fledge rate?
male_nests3 <- male_nests2 %>% filter(!is.na(FledgeSize)  ) 
mod<- lm(FledgeSize ~ RC1_blue + RC2_blue + RC3_blue + RC1_white + RC2_white + Year, 
      #    family="poisson", 
          data=male_nests3) 
summary(mod)
#113 observations
plot(mod)  #Really doesn't fit normal distribution
hist(resid(mod))
dredge(mod)
avg <- model.avg(dredge(mod), subset= delta < 2, revised.var = TRUE )
summary(avg)
car::Anova(mod)
#Male color does not influence fledge rate

#female color on fledge rate
female_nests3 <- female_nests2 %>% filter(!is.na(FledgeSize)) # & BroodSize>0)

mod<- lm(FledgeSize ~ RC1_blue + RC2_blue + RC3_blue + RC1_white + RC2_white + Year , 
      #    family="poisson",
          data=female_nests3) 
#186 observations, probably fine to use.
AER::dispersiontest(mod) #dispersion looks OK
plot(mod) #Not normal, should not use normal distrubution
hist(resid(mod))
dredge(mod)
avg <- model.avg(dredge(mod), subset= delta < 2, revised.var = TRUE )
summary(avg)
#Blue color of females (Theta) associated with higher fledging rates when we do binomial
#No color variables associated otherwise




#Is there assortative mating happening?



mated_pairs <- inner_join(female_nests %>% select(c('NestID', 'Year', 'BandNo', 'AgeClass2', 'RC1_blue', 'RC2_blue', 'RC3_blue', 'RC1_white', 'RC1_white')), 
                          male_nests %>% select(c('NestID', 'Year', 'BandNo', 'AgeClass2', 'RC1_blue', 'RC2_blue', 'RC3_blue', 'RC1_white', 'RC1_white')), 
                          by=c('NestID'='NestID', 'Year'='Year'), 
                          suffix=(c(".female", ".male"))) 

association <- cor(mated_pairs[c(5:8, 11:14)], use="pairwise.complete.obs")
p.values <- corrplot::cor.mtest(association, p.mat = res1$p, sig.level = .95)

p.values$p[1:4, 5:8]
corrplot::corrplot(association[1:4, 5:8], p.mat = p.values$p[1:4, 5:8] , method = "ellipse", insig = "p-value")



ggplot(mated_pairs, aes(x=RC2_blue.male, y=RC2_blue.female))+
  geom_point()+
  geom_smooth()


