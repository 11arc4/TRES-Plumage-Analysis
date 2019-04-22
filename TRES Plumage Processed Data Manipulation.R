library(pavo)
library(tidyverse)
library(readxl)

#Function to fix the damn time numerics. Stolen from stack overflow, works like a dream
time_since_midnight <- function(x, decimal_second = FALSE){
  hours <- x %/% 60
  minutes <- x %% 60
  
  seconds_since_last_hour <- minutes * 60
  minutes <- seconds_since_last_hour %/% 60
  seconds <- seconds_since_last_hour %% 60
  
  if (decimal_second){
    sprintf("%02d:%02d:%02.3f", hours, minutes, seconds)
  } else {
    sprintf("%02d:%02d:%02d", hours, minutes, floor(seconds))
  }
}


folders <- c("~/Montogomerie Work/TRES Plumage/SMY swallows/5. spec TRES.1999/TRES.PROCESSED step2", 
          "~/Montogomerie Work/TRES Plumage/SMY swallows/6. spec TRES.2000/TRES.PROCESSED step2")
for (dir in folders){
  filenames<- list.files(dir) [grep(".gtr", list.files(dir))] #list only the filenames ending in ".gtr
  
  
  for(file in filenames){
    
    #Read in the excel file
    AndrewSpec <- read_xls(paste(dir, file, sep="/"), col_types = "text")
    
    #fix date
    AndrewSpec[1,] <- as.character(as.Date(as.numeric(AndrewSpec[1,]), origin = "1904-1-1"))
    
    #Fix time
    AndrewSpec[2,] <- time_since_midnight(as.numeric(AndrewSpec[2,]) * 24 * 60)
    
    
    
    # #Reading in these files is going to be a problem. I can't seem to read them in as .gtr
    # AndrewSpec<- read.csv("~/Montogomerie Work/TRES Plumage/ARC TRES data/TEST 165103010.gtr.csv", as.is=T)
    
    
    MetaData <- as.data.frame(t(AndrewSpec[1:7,])[-1,])
    names(MetaData) <- c("Date", "Time", "BandID", "File", "Region", "Sensitivity", "Unknown")
    row.names(MetaData) <- NULL
    
    
    procSpec <- AndrewSpec[8:nrow(AndrewSpec), ]
    names(procSpec)<- c("wl", paste("Region ", MetaData$Region, ".", MetaData$File, sep=""))
    
    
    #Make all the columns numeric
    procSpec[, 1] <- as.numeric(as.character(unlist(procSpec[, 1])))
    procSpec[, 2:ncol(procSpec)] <- as.numeric(as.character(unlist(procSpec[, 2:ncol(procSpec)])))
    
    
    
    #Turn Proc SPec into the rspec object that pavo requires. 
    procSpec<- as.rspec(procSpec, lim=c(300,700) )
    
    #graph it all
    
    #explorespec(rspecdata=procSpec, by=5, lwd=2, ylim=c(0, .3))
    
    ##Calculate all 23 variables listed in Montgomerie (2006)-- I don't think we
    ##actually want to use any of them though so I won't put them into my data. 
    #summary(procSpec)
    #cbind(MetaData, summary(procSpec))
    
    # Model using absorbance at each wavelength for each cone for blue tit (includes UV)
    # achromatic cone sensitivity from blue-tit, 
    # against blue sky
    vm.bluetit <- vismodel(procSpec, 
                           visual= "bluetit", 
                           achromatic = "bt.dc", 
                           illum = "D65", #illum = "bluesky", #switched from bluesky to standard daylight 
                           trans = "bluetit", 
                           relative = TRUE)
    
    #summary(vm.bluetit)
    tcs.bluetit <- colspace(vm.bluetit) #Models reflectance spectra in a colorspace based on the blue tit vismodel I made
    
    #We will want h.theta, h.phi, and r.achieved
    birdCol<- cbind(MetaData, (tcs.bluetit %>% select(c(h.theta, h.phi, r.achieved))), summary(procSpec) %>% select(c(S1B, S1G, S1U)))
    
    
    
    
    #plot(tcs.bluetit, pch = 21, bg = spec2rgb(procSpec), perspective = TRUE, cex = 0.5)
    
    #tcsplot(tcs.bluetit, size=0.02) # needs rgl package which won't install
    
    write.csv(birdCol, 
              paste("C:/Users/11arc/OneDrive/Documents/Montogomerie Work/TRES Plumage/ARC TRES data/Compiled TRES Data/TRES Plumage Data 1999-2000 (blue tit visual model, standard daylight)/Plumage Color ", MetaData$BandID[1], " ", MetaData$Date[1] , ".csv", sep=""), 
              na="", 
              row.names = F)
    
  }
}

