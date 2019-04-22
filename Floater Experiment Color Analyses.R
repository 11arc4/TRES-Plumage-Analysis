#Differences in tree swallow plumage between floaters and competetive males (and females?). 
library(lme4)
library(tidyverse)
library(lmerTest)

#Import all nests for 2000
nestSpec <- read.csv(
          "~/Montogomerie Work/TRES Plumage/ARC TRES data/Compiled TRES Data/Nest data with plumage and Morphometrics in Standard Daylight.csv", 
          na.strings = "", 
          as.is=T) %>% 
  filter(Year==2000 & !is.na(RC1_blue) )


#Some birds were speced more than once-- remove those duplicates, keeping only the first measurement of the bird 
nestSpec<- nestSpec %>% group_by(BandNo, NestID) %>% arrange(JulianDate) %>% summarise_all(first)

#Seperate female and male observations
female <- nestSpec %>% filter(Sex=="Female")
male <- nestSpec %>% filter(Sex=="Male")





#THere were a couple of re-used boxes that should proabbly be considered floaters too
ggplot(nestSpec, aes(y=FirstEggJulian, x=Box.No ))+
  geom_point()+
  facet_grid(~Site.ID)+
  geom_hline(yintercept = 143)

nestSpec$LateNest <- factor(ifelse(nestSpec$Box.No >60, "Late", ifelse(nestSpec$FirstEggJulian>153, "Late", "Normal")), levels=c("Normal", "Late"))




#Are floaters less blue than competetors?
mod <- lmer(RC1_blue ~ LateNest*Sex + (1|NestID) , data=nestSpec)
summary(mod)
plot(mod)
car::Anova(mod, type="III")
#YES different between sexes and late nests based on RC1_blue
ggplot(nestSpec, aes(fill=LateNest, y=RC1_blue, x=Sex))+
  geom_boxplot()

mod <- lmer(RC2_blue ~ LateNest*Sex + (1|NestID) , data=nestSpec)
summary(mod)
plot(mod)
car::Anova(mod, type="III")
#YES: different between late nests based on RC2_blue (both sexes same)
ggplot(nestSpec, aes(fill=LateNest, y=RC2_blue, x=LateNest))+
  geom_boxplot()


mod <- lmer(RC3_blue ~ LateNest*Sex + (1|NestID) , data=nestSpec)
summary(mod)
plot(mod)
car::Anova(mod, type="III")
#YES: different response to late nestling between males and females (males differ, females don't)
TukeyHSD(aov(mod))
ggplot(nestSpec, aes(fill=LateNest, y=RC3_blue, x=Sex))+
  geom_boxplot()

mod <- lmer(RC1_white ~ LateNest*Sex + (1|NestID) , data=nestSpec)
summary(mod)
plot(mod)
car::Anova(mod, type="III")
#Nope, just differs by sex
ggplot(nestSpec, aes(y=RC1_white, x=Sex))+
  geom_boxplot()

mod <- lmer(RC2_white ~ LateNest*Sex + (1|NestID), data=nestSpec)
summary(mod)
plot(mod)
car::Anova(mod, type="III")
ggplot(nestSpec, aes(fill=LateNest, y=RC2_white, x=Sex))+
  geom_boxplot()
#No differences between sexes of nests-- probably because it's just due to
#whether there's a lot of poop there or not.





#What else do birds from late and normal nests differ based on?
#body condition?
nestSpec2 <- nestSpec %>% filter(!is.na(SMI)) %>% 
  mutate(DaysSinceHatching= JulianDate-HatchJulian)
mod <- lmer(SMI ~ LateNest*Sex + DaysSinceHatching*Sex+ (1|NestID), data=nestSpec2 %>% filter(DaysSinceHatching>-10)) #singular so not a good fit 
summary(mod)
plot(mod)
car::Anova(mod, type="III")
ggplot(nestSpec2%>% filter(DaysSinceHatching>-10), aes(color=LateNest, y=SMI, x=DaysSinceHatching))+
  geom_point()+
  facet_grid(~Sex)
#Late nesting females tend to have lower SMI
