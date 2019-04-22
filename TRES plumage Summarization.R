library(tidyverse)



dir <- "C:/Users/11arc/OneDrive/Documents/Montogomerie Work/TRES Plumage/ARC TRES data/Compiled TRES Data/TRES Plumage Data 1999-2000 (blue tit visual model, standard daylight)"

filenames<- list.files(dir)

allPlum <- as.data.frame(matrix(NA, nrow=length(filenames)*10, ncol=10))

#Regions 1-10, theta, phi, r.achieved, Blue, Green and UV chroma
names(allPlum) <- c("BandID",  "Date", "Time", "Region","Theta", "Phi", "R.achieved", "BlueChroma","GreenChroma", "UVChroma")


# names(allPlum) <- c("BandID", "Date", "Time",
#                     paste("Theta", 1:10), 
#                     paste("Phi", 1:10), 
#                     paste("R.achieved", 1:10), 
#                     paste("BlueChroma", 1:10),
#                     paste("GreenChroma", 1:10), 
#                     paste("UVChroma", 1:10)
#                     )

i <- 1
for (file in filenames){
  plum <- read.csv(paste(dir, file, sep="/"))
  regions <- length(unique(plum$Region))
  plum$Date <- as.character(plum$Date)
  plum$Time <- as.character(plum$Time)
plum$BandID <- as.character(plum$BandID)  
  allPlum [i:(i+regions-1), ]<- plum %>% 
    select(BandID, Date, Time, Region, h.theta, h.phi, r.achieved, S1B, S1G, S1U) %>% 
    group_by(BandID, Date, Time, Region) %>% 
    summarise_all(mean)
  i <- i+regions
}


allPlum <- allPlum[1:(i-1), ]


allPlum$Date <- as.Date(allPlum$Date, format = "%Y-%m-%d")
allPlum$Region <- recode(allPlum$Region, 
                         `1`="nape", 
                         `2`="crown", 
                         `3`="back", 
                         `4`="rump", 
                         `5`="tail",
                         `6`="lesser coverts", 
                         `7`="throat", 
                         `8`="breast", 
                         `9`="belly", 
                         `10`="undertail coverts", 
                         `11`="median coverts")

write.csv(allPlum %>% arrange(BandID),
          "~/Montogomerie Work/TRES Plumage/ARC TRES data/Compiled TRES Data/Adult Plumage by Region Standard Daylight.csv", 
          na = "", 
          row.names = F) 

rm(plum, dir, file, filenames, i, regions)

allPlum<- read.csv("~/Montogomerie Work/TRES Plumage/ARC TRES data/Compiled TRES Data/Adult Plumage by Region Standard Daylight.csv")


allPlum_wide <- allPlum %>% 
  filter(Region!="median coverts")%>% #only one time it's measured
  gather (c(-BandID, -Date, -Time, -Region), key="Color", value="value") %>% 
  unite("Color_region", c("Color","Region")) %>% 
  spread( key=Color_region, value=value)

cor(allPlum_wide[,24:53], method="pearson", use="complete")


#Phi
corrplot::corrplot(cor(allPlum_wide[,24:33], method="pearson", use="complete"))
#Strong correlation between blue parts (back, crown, lesser coverst, nape) BUT NOT THE TAIL
#Strong correlation between white parts (thoat, belly, breast, undertail coverts) 

#Theta
corrplot::corrplot(cor(allPlum_wide[,44:53], method="pearson", use="complete"))
#no clear pattern in who is correlated to what in terms of theta. 
#THeta isn't measured very accurately (in discrete stages it seems? I'm not sure why that is.)

#R.achieved
corrplot::corrplot(cor(allPlum_wide[,34:43], method="pearson", use="complete"))
#Blue parts (back, crown, lesser coverst, nape) BUT NOT TAIL are correlated
#White parts aren't correlated




adults <- read.csv("~/Montogomerie Work/TRES Plumage/ARC TRES data/Adult Morphometrics.csv", na.strings = c("")) 
adults$Date2 <- as.Date(as.character(adults$Date), "%m-%d-%y")
adults$JulianDate <- yday((adults$Date2))
adults$NestID <- paste(adults$Year, adults$SiteID, adults$BoxID, sep="-")
adults$BandNo <- as.character(adults$BandNo)

allPlum_wide$Year <-  lubridate::year(allPlum_wide$Date)
allPlum_wide$JulianSpecDate <-  lubridate::yday(allPlum_wide$Date)


adultsSpec<- full_join(adults, allPlum_wide, by=c("BandNo"="BandID", "Year"="Year" )) %>%   #Merge Spec and Adult morphometrics data
  select(-c(Researcher, BP, CP, X)) %>% #remove empty columns
  mutate(Date3 = ifelse(!is.na(Date2), as.character(Date2), as.character(Date.y))) %>% 
  select(c(-Date2, -Date.x, -Date.y, -JulianSpecDate)) %>%
  rename(Date=Date3, TimeMeasured=Time.x, TimeSpec= Time.y, Sex=SexMorph) %>%
  mutate(JulianDate = lubridate::yday(Date))%>%
  select(BandNo, SiteID, BoxID, NestID, Year, Date, JulianDate, TimeMeasured, TimeSpec, 
         Retrap, Sex, AgeClass, AgeYrs, AgeAccuracy, Retrap, 
         Mass, Wing, Bled, Speced, R4,  
         everything()) #reorder into a logical order




ggplot(data=adultsSpec, aes(y=Phi_back, x=BlueChroma_back, color=Sex))+
  geom_point()

ggplot(data=adultsSpec, aes(y=Phi_back, x=GreenChroma_back))+
  geom_point()

ggplot(data=adultsSpec, aes(y=Phi_back, x=UVChroma_back))+
  geom_point()

#Increasing Phi=  more UV, something kind of weird going on with blue and green chroma as well. 


ggplot(data=adultsSpec, aes(y=Theta_back, x=BlueChroma_back, color=Sex))+
  geom_point()

ggplot(data=adultsSpec, aes(y=Theta_back, x=GreenChroma_back, color=Sex))+
  geom_point()


ggplot(data=adultsSpec, aes(y=Theta_back, x=UVChroma_back, color=Sex))+
  geom_point()
#Increasing theta= less green, more blue

ggplot(data=adultsSpec, aes(y=R.achieved_back, x=BlueChroma_back, color=Sex))+
  geom_point()

ggplot(data=adultsSpec, aes(y=R.achieved_back, x=GreenChroma_back, color=Sex))+
  geom_point()

ggplot(data=adultsSpec, aes(y=R.achieved_back, x=UVChroma_back, color=AgeClass))+
  geom_point()
#R.achieved increased with less UV and more green

#THIS ALL MATCHES ROZ now. 






write.csv(adultsSpec %>% arrange(BandNo),
          "~/Montogomerie Work/TRES Plumage/ARC TRES data/Compiled TRES Data/Adult Plumage and Morphometrics in Standard Daylight.csv", 
          na = "", 
          row.names = F) 




