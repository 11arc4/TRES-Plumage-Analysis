#Differences in tree swallow plumage between floaters and competetive males (and females?). 
library(lme4)
library(tidyverse)
library(lmerTest)
library(cowplot)
library(MuMIn)
options(na.action = "na.fail")

#Import all nests for 2000
nestSpec <- read.csv(
          "~/Montogomerie Work/TRES Plumage/ARC TRES data/Compiled TRES Data/Nest data with plumage and Morphometrics in Standard Daylight.csv", 
          na.strings = "", 
          as.is=T) %>% 
  filter(Year==2000 & !is.na(RC1_blue) ) %>%
  mutate(FledgeSize= ifelse(BroodSize>0, FledgeSize, 0))

nest <- read.csv("C:/Users/11arc/OneDrive/Documents/Montogomerie Work/TRES Plumage/ARC TRES data/Compiled TRES Data/Nest Data 1999-2000.csv", na.strings = "")%>% 
  filter(Year==2000 & ClutchSize>0 ) %>% #Remove nests where no eggs were laid... They're not nests....
  mutate(NumFledged= ifelse(BroodSize>0, NumFledged, 0))
#Some birds were speced more than once-- remove those duplicates, keeping only the first measurement of the bird 
nestSpec<- nestSpec %>% group_by(BandNo, NestID) %>% arrange(JulianDate) %>% summarise_all(first)




#THere were a couple of re-used boxes that should proabbly be considered floaters too
ggplot(nestSpec, aes(y=FirstEggJulian, x=Box.No ))+
  geom_point()+
  facet_grid(~Site.ID)+
  geom_hline(yintercept = 143)

nestSpec$LateNest <- factor(ifelse(nestSpec$Box.No >60, "Late", ifelse(nestSpec$FirstEggJulian>153, "Late", "Normal")), levels=c("Normal", "Late"))
nest$LateNest <- factor(ifelse(nest$Box.No >60, "Late", ifelse(nest$FirstEggJulian>153, "Late", "Normal")), levels=c("Normal", "Late"))

#Seperate female and male observations
female <- nestSpec %>% filter(Sex=="Female")
male <- nestSpec %>% filter(Sex=="Male")





#Are floaters less blue than competetors?
mod <- lmer(RC1_blue ~ LateNest*Sex + (1|NestID) , data=nestSpec)
summary(mod)
plot(mod)
car::Anova(mod, type="III")
dredge
#Only one top model
mam <- lmer(RC1_blue ~ LateNest+Sex + (1|NestID) , data=nestSpec)
summary(mam)
#YES different between sexes and late nests based on RC1_blue
plot_blue1 <- ggplot(nestSpec, aes(fill=LateNest, y=RC1_blue, x=Sex))+
  geom_boxplot()+
  labs(x=NULL, y="Blue RC1", fill=NULL)+
  theme_classic()

mod <- lmer(RC2_blue ~ LateNest*Sex + (1|NestID) , data=nestSpec)
summary(mod)
plot(mod)
car::Anova(mod, type="III")
dredge(mod)
#only one top model
mam <- lmer(RC2_blue ~ LateNest + (1|NestID) , data=nestSpec)
summary(mam)
#YES: different between late nests based on RC2_blue (both sexes same)
plot_blue2 <- ggplot(nestSpec, aes(fill=LateNest, y=RC2_blue, x=LateNest))+
  geom_boxplot()+
  labs(x=NULL, y="Blue RC2", fill=NULL)+
  theme_classic()


mod <- lmer(RC3_blue ~ LateNest*Sex + (1|NestID) , data=nestSpec)
summary(mod)
plot(mod)
car::Anova(mod, type="III")
dredge(mod)
#only one top model-- full model
mam <- mod 
summary(mam)
#YES: different response to late nestling between males and females (males differ, females don't)
TukeyHSD(aov(mod))
plot_blue3 <- ggplot(nestSpec, aes(fill=LateNest, y=RC3_blue, x=Sex))+
  geom_boxplot()+
  labs(x=NULL, y="Blue RC3", fill=NULL)+
  theme_classic()

#How does whiteness differ between late and early nests?
mod <- lmer(RC1_white ~ LateNest*Sex + (1|NestID) , data=nestSpec)
summary(mod)
plot(mod)
car::Anova(mod, type="III")
dredge(mod)
#Only one top model
mam <- lmer(RC1_white ~ Sex + (1|NestID) , data=nestSpec)
#Nope, just differs by sex
plot_white1 <- ggplot(nestSpec, aes(y=RC1_white, x=Sex))+
  geom_boxplot(show.legend = F, fill="grey")+
  labs(x=NULL, y="White RC1")+
  theme_classic()

mod <- lmer(RC2_white ~ LateNest*Sex + (1|NestID), data=nestSpec)
summary(mod)
plot(mod)
car::Anova(mod, type="III")
dredge(mod)
#only one top model-- null
mam <- lmer(RC2_white ~  (1|NestID), data=nestSpec)
summary(mam)
plot_white2 <- ggplot(nestSpec, aes(fill=LateNest, y=RC2_white, x=Sex))+
  geom_boxplot()+
  labs(x=NULL, y="White RC2", fill=NULL)+
  theme_classic()
#No differences between sexes of nests-- probably because it's just due to
#whether there's a lot of poop there or not.





#What else do birds from late and normal nests differ based on?
#body condition?
nestSpec2 <- nestSpec %>% 
  mutate(DaysSinceHatching= JulianDate-HatchJulian) %>%
  filter(!is.na(SMI) & DaysSinceHatching>-10) # removes one bird that was assessed way earlier than everyone else. 
mod <- lmer(SMI ~ LateNest*Sex + DaysSinceHatching+ (1|NestID), data=nestSpec2) #singular so not a good fit 
mod <- lm(SMI ~ LateNest*Sex + DaysSinceHatching, data=nestSpec2) #Because lmer was singular it was reporting p values from the simple model
summary(mod)
plot(mod)
car::Anova(mod, type="III")
dr <- dredge(mod)
#4 top models, all including days since hatching, some including late nest
model.avg(dr, subset= delta < 2, revised.var = TRUE )
#SMI declines as nestlings age (feeding related stress). Late nesting birds have
#lower SMI than earlier nestings birds in females, but not males.

plot_SMI <- ggplot(nestSpec2, aes(color=LateNest, y=SMI, x=DaysSinceHatching))+
  geom_point()+
  facet_grid(~Sex)+
  geom_smooth(method="lm")+
  labs(x="Days since Hatch", y="Scaled Mass Index")+
  theme_classic()
plot_SMI
#Late nesting females tend to have lower SMI: no difference for males. 

#age?
ggplot(nestSpec, aes(color=LateNest, y=AgeYrs, x=Sex))+
  geom_count()
#looks identical.
ggplot(female, aes(x=LateNest, y=AgeClass2))+
  geom_count()
#Only 6 SY females--probably not worth analysing. 
ggplot(nestSpec %>% filter(AgeAccuracy=="Exact"), aes(color=LateNest, y=AgeYrs, x=Sex))+
  geom_count()


#How productive are first vs late nests? IE is your fitness higher if you get a first next?
#Clutch Size
mod <- glm(ClutchSize ~LateNest, family="poisson", data=nest)
plot(mod)
hist(resid(mod))
AER::dispersiontest(mod, alternative="two.sided") #I think we're under dispersed. THat's less of a problem. but in an ideal world would be dealt with
car::Anova(mod, type="III")
summary(mod)
dredge(mod) 
#full model is the best model
#smaller clutches in late nests
plot_clutch <- ggplot(nest, aes(x=LateNest, y=ClutchSize))+
  geom_boxplot()+
  geom_count()+
  labs(x=NULL, y="Clutch Size")+
  theme_classic()

#Hatch size
nest_h <- nest %>% filter(!is.na(BroodSize))
mod <- glm(BroodSize ~LateNest, family="poisson", data=nest_h)
plot(mod)
AER::dispersiontest(mod, alternative="two.sided") #a bit underdispersed, but totally acceptable now.   
car::Anova(mod, type="III")
summary(mod)
dredge(mod)
#full model is the best model
#fewer nestlings in late nests (may still be a holdover from smaller clutches)
plot_brood <- ggplot(nest, aes(x=LateNest, y=BroodSize))+
  geom_boxplot()+
  geom_count()+
  labs(x=NULL, y="Brood Size")+
  theme_classic()

#Fledge Num
nest_f <- nest %>% filter(!is.na(NumFledged))
mod <- glm(NumFledged ~LateNest, family="poisson", data=nest_f)
plot(mod)
AER::dispersiontest(mod, alternative="two.sided") # now a touch over dispersed but egregious  
car::Anova(mod, type="III")
summary(mod)
dredge(mod)
#Best model by far is the full model. 
plot_fledge <- ggplot(nest, aes(x=LateNest, y=NumFledged))+
  geom_boxplot()+
  geom_count()+
  labs(x=NULL, y="Fledge Size")+
  theme_classic()




cowplot::plot_grid(plot_clutch, plot_brood, plot_fledge, nrow=3, ncol=1, labels = "auto")
ggsave(filename="C:/Users/11arc/OneDrive/Documents/Montogomerie Work/TRES Plumage/TRES Plumage Plots/Floater Fitness Plot.png", 
       height=8, 
       width=3, 
       units="in")

image_tres <- ggdraw() + 
  draw_image("https://d1ia71hq4oe7pn.cloudfront.net/photo/68280061-1280px.jpg")

cowplot::plot_grid(plot_blue1, plot_blue2, plot_blue3, plot_white1, plot_white2, image_tres, nrow=2, ncol=3, labels = "auto")
ggsave(filename="C:/Users/11arc/OneDrive/Documents/Montogomerie Work/TRES Plumage/TRES Plumage Plots/Floater Color Plot.png", 
       height=6, 
       width=8, 
       units="in")

