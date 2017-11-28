##################################
## OEB 201 Final Project Script ##
##################################

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

#Load packages
require(ggplot2)
require(arm)
require(rstanarm)
require(readr)
require(dplyr)
require(gtools)
require(ggthemes)
require(loo)
require(shinystan)

#Set WD
  setwd("C:/Users/USER/Documents/School/Classes/2017 Fall/OEB 201 R Stats/semester project/Data/")


##################################
##         Data Cleanup         ##
##################################

#I need to collate these variables:
  #lateral force
  #shape
  #flapping frequency
  #SPS
  #phase angle
  #flapping amplitude
#They are currently all in different files (because they are recorded on different sensors)

#Load all data frames
  allForceData <- read_csv("allForceAmpResultsAtSPS.csv")
  allPhaseData <- read_csv("allPhaseDataCoralPink.csv")
  allCoralSPSData <- read_csv("CoralSPSData.csv")
  allPinkSPSData <- read_csv("PinkSPSData.csv")
  phaseRepeatData <- read_csv("AllPhaseandAmpDataFoilREPEATABILITY.csv")

#Add new column to SPS data specifying material, then combine data collected on pink and coral 
    matVect <- rep("coral",nrow(allCoralSPSData))
    allCoralSPSData <- cbind(allCoralSPSData,matVect)
    matVect <- rep("pink",nrow(allPinkSPSData))
    allPinkSPSData <- cbind(allPinkSPSData,matVect)
    allSPSData <- rbind(allCoralSPSData,allPinkSPSData)
  #There is a meaningless filler column at the beginning of allSPSData. Get rid of this an rename variables
    SPSData <- allSPSData[,2:7]
    SPSData <- dplyr::rename(SPSData, freq = Frequency)
    SPSData <- dplyr::rename(SPSData, pos = Position)

#Different rows measure different parameters (Fx, Fy, and Fz). I only want the ones measuring Fy
  #Of this subset, I only care about the variables that let me ID the trial, as well as the measure of Fy
  forceData <- dplyr::filter(allForceData, measurement=='Fy')
  forceData <- forceData[,c(2,3,5,7)]
  forceData <- dplyr::rename(forceData, Fy = meanAmp)

#Subset this data frame to get all the variables in order to identify each trial as well as the phase angle and flapping amplitudes for both fins
  #Then rename the variables for easier use
  phaseData <- allPhaseData[,c(2,3,6,8:10,13,17:18)]
  phaseData <- dplyr::rename(phaseData, phaseAng = angPhaseTop)
  phaseData <- dplyr::rename(phaseData, dorFlapAmp = cmAmpDor)
  phaseData <- dplyr::rename(phaseData, caudFlapAmp = cmAmpCaud)


#Combine all these data frames into one data frame. Clean it
  #first sort them so that the rows line up correctly when I concatenate the data
  SPSData <- SPSData[order(SPSData[,6], SPSData[,2],SPSData[,1]),]
#forceData <- forceData[order(forceData[,1], forceData[,2],forceData[,3]),]
  #THIS DIDN"T WORK, DONT KNOW WHY
  #instead, I'll do it in a much clunkier way
  forceData <- forceData[mixedorder(forceData$flapFreq),]
  forceData <- forceData[mixedorder(forceData$position),]
  forceData <- forceData[mixedorder(forceData$material),]

  concData <- cbind(forceData,SPSData)

      #Check that they line up correctly
      identical(concData$material,concData$matVect)
      identical(concData$position,concData$pos)
      identical(concData$flapFreq,concData$freq)
        #If they are all "TRUE", then we can proceed.

#Remove redundant columns
  concData <- dplyr::select(concData, material,position,freq,Fy,SPS,Power,SwimEcon)


#Now correct the data so that y forces are expressed as a difference from control
  #first separate into two dataframes, one control and one experimental
  expData <- dplyr::filter(concData, position=='near' | position=='far')
  ctrlData <- dplyr::filter(concData, position=='ctrl')
#Then subtract control value (at the same flapping parameters) from each experimental Fy  
  for (i in 1:nrow(expData)) {
    mat <- expData[i,1]
    freq <- expData[i,3]
    
    expData$Fy[i] <- expData$Fy[i]-ctrlData$Fy[ctrlData$material==mat & ctrlData$freq==freq]
  }
  
#Organize phase data so rows align correctly
#phaseData <- phaseData[order(phaseData[,1], phaseData[,2],phaseData[,4]),]
  #This also didn't work
  phaseData <- phaseData[mixedorder(phaseData$freq),]
  phaseData <- phaseData[mixedorder(phaseData$position),]
  phaseData <- phaseData[mixedorder(phaseData$species),]
  
#Change column names so there is no redundancy when data is concatenated  
  phaseData <- dplyr::rename(phaseData, pos = position)
  phaseData <- dplyr::rename(phaseData, frequency = freq)
  
  allData <- cbind(expData, phaseData)

    #Check that they line up correctly
    identical(allData$material,allData$species)
    identical(allData$position,allData$pos)
    identical(allData$frequency,allData$freq)
    #If they are all "TRUE", then we can proceed.

#Remove redundant columns
  allData <- dplyr::select(allData, -species, -pos, -freq, -speed)

#Convert forces from Newtons to milliNewtons
  allData$Fy <- (allData$Fy)*1000

#Convert SPS from m/s to cm/s
  allData$SPS <- (allData$SPS)*100

#reorder for clarity
  allData <- allData[,c(1,2,8,9,7,3,4,5,6,10,11,12)]

#My final data frame is allData



##################################
##Complete data set w/ fake data##
##################################

###First define my errors

    #SPS errors first. These are based on measurements at .5 hz and 1.5 hz. Then I picked values for the other frequencies based on these measurements
      material <- c(rep("pink", 6), rep("coral", 5))
      frequency <- c(seq(from=.5, to=3, by=.5), seq(from=.5, to=2.5, by=.5))
      error <- c(6.994, 1, .176, .1, .1, .1, 6.994, 1, .176, .1, .1)
      SPSError <- data.frame(material, frequency, error)
    
    #Fy error. Based on actual measurements of error within one trial
      #In other words, this error is the deviation in measurements taken at different time points of one trial
      FyError <- dplyr::filter(allForceData, measurement=='Fy' & position=="ctrl")
      FyError <- FyError[,c(2,5,8)]
      FyError <- dplyr::rename(FyError, frequency = flapFreq)
      FyError <- dplyr::rename(FyError, error = sdAmp)
    #Convert errors to milliNewtons
      FyError$error <- (FyError$error)*1000
    
    #Phase angle error
      #I measured the same video 5 times, and am using the standard deviation of these measurements
      farPhaseErrorData <- dplyr::filter(phaseRepeatData, position=="far")
      phaseError <- sd(farPhaseErrorData$angPhaseTop)
    
    #Amplitude error
      #These are also directly measured at the same time as the phase angle errors.
      farAmpErrorData <- dplyr::filter(phaseRepeatData, position=="far")
      ampError <- sd(farAmpErrorData$cmAmpCaud)
    
    #No error for frequency because I am treating it as a factor.

###Create 10 rows of fake data for each set of flapping parameters.
      #Basically, for each row in allData, create 10 fake data points
  fakeData <- allData[c(1,2,3,4,5,6,7,10,11,12)]
  
  for (i in 1:nrow(allData)) {
    k <- 1
    while (k <=10) {
      material <- allData[i,1]
      position <- allData[i,2]
      heave <- allData[i,3]
      pitch <- allData[i,4]
      frequency <- allData[i,5]
      Fy <- rnorm(n=1,mean=allData[i,6], sd=FyError$error[SPSError$material==material & SPSError$frequency==frequency])
      SPS <- rnorm(n=1,mean=allData[i,7], sd=SPSError$error[SPSError$material==material & SPSError$frequency==frequency])
      phaseAng <- rnorm(n=1,mean=allData[i,10], sd=phaseError)
      dorFlapAmp <- rnorm(n=1,mean=allData[i,11], sd=ampError)
      caudFlapAmp <- rnorm(n=1,mean=allData[i,12], sd=ampError)
      addendum <- c(material,position,heave,pitch,frequency,Fy,SPS,phaseAng,dorFlapAmp,caudFlapAmp)
      
      fakeData <- rbind(fakeData, addendum)
      
      k <- k+1
    }
  }

#Make sure all the numeric variables are treated as numeric
  for(i in 3:10) {
    fakeData[,i] <- as.numeric(fakeData[,i])
  }

#I primarily care about the ratio between the amplitudes of my two fins. This tells me the relative amount of motion in each.
  #Here I create a variable that reflects this ratio
  flapAmpRatio <- fakeData$dorFlapAmp/fakeData$caudFlapAmp
  fakeData <- data.frame(fakeData, flapAmpRatio)

####Center all my data
  
  #Most predictors get centered to their average value when flapping at 1.5Hz. This insures that the intercept tells me about an actual condition
      #First create a vector of the fake data where frequency=1.5, so I can center on the averages of these values
        fakeData1.5 <- dplyr::filter(fakeData, frequency==1.5)
      #Now center based on this vector
    fakeData$SPS <- fakeData$SPS - mean(fakeData1.5$SPS)
    fakeData$phaseAng <- fakeData$phaseAng - mean(fakeData1.5$phaseAng)
    fakeData$flapAmpRatio <- fakeData$flapAmpRatio - mean(fakeData1.5$flapAmpRatio)
  #Frequency gets centered to 1.5 Hz, my "intermediate value"
    fakeData$frequency <- fakeData$frequency-1.5
  
  
#Two separate data frames, one for each material
  fakeDataPink <- dplyr::filter(fakeData, material=='pink')
  fakeDataCoral <- dplyr::filter(fakeData, material=='coral')


##################################
##         Some Graphs          ##
##################################

#compare my real data to my fake data. Make sure it looks reasonable.

###Y force by frequency###
  #Just real data
    ggplot(data=allData, aes(x=frequency, y=Fy, color=material, shape=position))+
      geom_point(size=5)+
      geom_line() +
      theme_few() +
      labs(x="Frequency", y="Fy (mN)")+
  scale_color_manual(values=c("coral","pink"))
  #Fake Data w/ real data overlayed
    ggplot(data=fakeData, aes(x=frequency, y=Fy, color=material, shape=position))+
     geom_point(size=5, alpha=.7)+
      geom_point(data=allData, aes(x=frequency-1.5, y=Fy, color="material", shape=position))+
      theme_few()+
      labs(x="Frequency (Hz centered at 1.5)", y="Fy (mN)")+
      scale_color_manual(labels=c("coral","real data","pink"),values=c("coral","green","pink"))
    
###SPS by frequency###
  #Just real data
    ggplot(data=allData, aes(x=frequency, y=SPS, color=material, shape=position))+
      geom_point(size=5)+
      geom_line() +
      theme_few() +
      labs(x="Frequency", y="SPS (cm/s)")+
      scale_color_manual(values=c("coral","pink"))
  #Fake Data w/ real data overlayed
    ggplot(data=fakeData, aes(x=frequency, y=SPS, color=material, shape=position))+
      geom_point(size=5, alpha=.7)+
      geom_point(data=allData, aes(x=frequency-1.5, y=SPS-mean(fakeData1.5$SPS), color="material", shape=position))+
      theme_few()+
      labs(x="Frequency (Hz centered at 1.5)", y="SPS (cm/s)(centered)")+
      scale_color_manual(labels=c("coral","real data","pink"),values=c("coral","green","pink")) 
      
    
###Flapping amplitude ratio by frequency###
  #Just real data
    ggplot(data=allData, aes(x=frequency, y=dorFlapAmp/caudFlapAmp, color=material, shape=position))+
      geom_point(size=5)+
      geom_line() +
      theme_few() +
      labs(x="Frequency", y="Dorsal fin amplitude : caudal fin amplitude")+
      scale_color_manual(values=c("coral","pink"))
  #Fake Data w/ real data overlayed
    ggplot(data=fakeData, aes(x=frequency, y=dorFlapAmp/caudFlapAmp, color=material, shape=position))+
      geom_point(size=5, alpha=.7)+
      geom_point(data=allData, aes(x=frequency-1.5, y=(dorFlapAmp/caudFlapAmp), color="material", shape=position))+
      theme_few()+
      labs(x="Frequency (Hz centered at 1.5)", y="Dorsal fin amplitude : caudal fin amplitude")+
      scale_color_manual(labels=c("coral","real data","pink"),values=c("coral","green","pink"))
    
###Phase angle by frequency###
  #Just real data
    ggplot(data=allData, aes(x=frequency, y=phaseAng, color=material, shape=position))+
      geom_point(size=5)+
      geom_line() +
      theme_few() +
      labs(x="Frequency", y="Phase Angle ("~degree~")")+
      scale_color_manual(values=c("coral","pink"))
  #Fake Data w/ real data overlayed
    ggplot(data=fakeData, aes(x=frequency, y=phaseAng, color=material, shape=position))+
      geom_point(size=5, alpha=.7)+
      geom_point(data=allData, aes(x=frequency-1.5, y=phaseAng-mean(fakeData1.5$phaseAng), color="material", shape=position))+
      theme_few()+
      labs(x="Frequency (Hz centered at 1.5)", y="Phase Angle ("~degree~") (centered)")+
      scale_color_manual(labels=c("coral","real data","pink"),values=c("coral","green","pink")) 
    

##################################
##          Test Data           ##
##################################


#The model: Fy~position+SPS+phaseAng+flapAmpRatio+(1|frequency)

  nFreq <- 6 #There are 6 frequencies that we will group by
  nTrial <- 20 #I am assuming 10 trials at each position (2 possible) over the 6 frequencies
  nTot <- nFreq*nTrial

  range(fakeData$Fy) #The range of my response variable is -228 to 45 mN. This will inform my choice of coefficients

#Define all my coefficients
  coefPos <- 50
  range(fakeData$SPS) #-37 to 42
  coefSPS <- 1
  range(fakeData$phaseAng) #-51 to 49
  coefAng <- -1
  range(fakeData$flapAmpRatio) #-.22 to .12
  coefAmpl <- -100

#Create vectors to represent my data
  posVect <- c(rep(0,nTot/2), rep(1,nTot/2))
  fakePosition <- sample(posVect, replace=FALSE)
  fakeSPS <- rnorm(nTot, 35, 10)
  fakeAng <- rnorm(nTot, 50, 15)
  fakeAmpl <- rnorm(nTot, .15, .05)
  freqGroup <- c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10),rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10),rep(6,10)) #6


#Define simple intercepts for my model (One distribution)
  meansIntercept <- rnorm(6,-60,10)

#Now make more complicated intercepts to better reflect data
  #Basically, each frequency has a separate distribution of intercepts and I just sample one
  intComplex <- c(rnorm(1,10,2),rnorm(1,-15,2),rnorm(1,-40,2),rnorm(1,-65,2),rnorm(1,-90,2),rnorm(1,-115,2))


#Compare distributions of intercepts
  #Find means of Fy for the real data, I'm assuming that this is close to the intercept
    realInt <- data.frame()
    
    for (i in c(.5,1,1.5,2,2.5,3)) {
      temp <- dplyr::filter(fakeDataPink, frequency==i-1.5)
      a <- mean(temp$Fy)
      both <- c(i,a)
      realInt <- rbind(realInt,both)
    }
    
    colnames(realInt) <- c("frequency","meanFy")
    
  #Plot it
    freqList <- c(.5,1,1.5,2,2.5,3)
    
    #The data that shows the intercept that the model wants to see
    intData <- data.frame(freqList,meansIntercept,intComplex)
    ggplot(data=intData,aes(x=c(.5,1,1.5,2,2.5,3), y=meansIntercept), legend=FALSE) +
      geom_point(aes(size=2, alpha=.6), col="blue") +
      theme_few() +
      theme(legend.position = "none") +
      labs(x="Frequency", y="Intercept") +
      ylim(-120,10)
    
    #The data that shows the more realistic intercepts and the real data together
    ggplot(data=intData,aes(x=freqList, y=intComplex), legend=FALSE) +
      geom_point(aes(size=2, alpha=.6), col="blue") +
      geom_point(data=realInt, aes(x=frequency, y=meanFy, size=2), col="red") +
      ylim(-120,10) +
      theme_few() +
      theme(legend.position = "none")+
      labs(x="Frequency", y="Intercept")
    
    #Overlapping all 3
    ggplot(data=intData,aes(x=freqList, y=intComplex), legend=FALSE) +
      geom_point(aes(size=2, alpha=.6), col="blue") +
      geom_line(aes(alpha=.6), col="blue")+
      geom_point(aes(size=2, alpha=.6, y=meansIntercept), col="purple") +
      geom_line(aes(alpha=.6, y=meansIntercept), col="purple") +
      geom_point(data=realInt, aes(x=frequency, y=meanFy, size=2), col="red") +
      geom_line(data=realInt, aes(x=frequency, y=meanFy, alpha=.6), col="red") +
      ylim(-120,10) +
      theme_few() +
      theme(legend.position = "none")+
      labs(x="Frequency", y="Intercept")

#The Model (intercepts that the model wants to see)
  #Produce my test response variable
    FyTest <- rep("NA", length(fakeSPS))
    for(i in 1:length(fakeSPS)) {
      FyTest[i] <-  meansIntercept[freqGroup[i]]+coefPos*fakePosition[i]+coefSPS*fakeSPS[i]+coefAng*fakeAng[i]+coefAmpl*fakeAmpl[i]+rnorm(1,0,5)
    }
    FyTest <- as.numeric(FyTest)

#The Model (more realistic intercepts)
  #Produce my test response variable
    FyTestComp <- rep("NA", length(fakeSPS))
    for(i in 1:length(fakeSPS)) {
      FyTestComp[i] <-  intComplex[freqGroup[i]]+coefPos*fakePosition[i]+coefSPS*fakeSPS[i]+coefAng*fakeAng[i]+coefAmpl*fakeAmpl[i]+rnorm(1,0,5)
    }
    FyTestComp <- as.numeric(FyTestComp)

#Fit a hierarchical model (simple intercepts)
  testModel <- stan_lmer(FyTest~fakePosition+fakeSPS+fakeAng+fakeAmpl+(1|freqGroup))
  print(testModel)
  summary(testModel)
  plot(testModel)
  launch_shinystan(testModel)

#Fit a hierarchical model (realistic intercepts)
  testModelComp <- stan_lmer(FyTestComp~fakePosition+fakeSPS+fakeAng+fakeAmpl+(1|freqGroup))
  print(testModelComp)
  summary(testModelComp)
  plot(testModelComp)
  launch_shinystan(testModelComp)

#So even though the model assumes the data looks different than it actually does (in terms of the intercepts),
  #this doesn't affect my ability to recover the true parameter values for most predictors.
  #It seems like the primary effect is to just create a high uncertainty in the intercept measurement.

#The bottom line is that I feel comfortable using this model even though it treats the data as if it were structured a bit differently.

##################################
##      model on real data      ##
##################################

##########DO MODEL FOR BOTH CORAL AND PINK, THEN COMPARE THE COEFFICIENTS####################


#To reiterate, my variables are the fin position, flapping frequency, swimming speed, phase angle, and flapping amplitude ratio
  #I am grouping by frequency
  #The response variable is Fy, the lateral force

#Model
  #The model: Fy~position+SPS+phaseAng+flapAmpRatio+(1|frequency)

  #I'm running the model first on all the pink data, then I will run it on the coral data
    #The distributions of the variables are different between the two materials, so I want to get different parameter estimates for each

  #Model data collected on pink foil
      glm1Pink <- stan_glmer(data=fakeDataPink, Fy~position+SPS+phaseAng+flapAmpRatio+(1|frequency),
                              prior=normal(-50,500))
      #I also tried a null prior (since I really do expect my data to be evenly distributed), but loo comparison said the fits were practically identical.
      plot(glm1Pink)
      summary(glm1Pink)
      print(glm1Pink)
      launch_shinystan(glm1Pink)
      #Get confidence intervals
      conInt95 <- posterior_interval(glm1Pink, prob=.95)
      round(conInt95,2)


  #Model data collected on coral foil
      glm1Coral <- stan_glmer(data=fakeDataCoral, Fy~position+SPS+phaseAng+flapAmpRatio+(1|frequency),
                             prior=normal(-50,500))
      plot(glm1Coral)
      summary(glm1Coral)
      print(glm1Coral)
      launch_shinystan(glm1Coral)
      #Get confidence intervals
      conInt95 <- posterior_interval(glm1Coral, prob=.95)
      round(conInt95,2)


#### I ran a model with interaction terms instead of a grouping variable
#glm2Pink <- stan_glm(data=fakeDataPink, Fy~position+position*frequency+SPS+SPS*frequency+phaseAng+phaseAng*frequency+flapAmpRatio+flapAmpRatio*frequency+frequency)
#                      prior=normal(-50,500))
# plot(glm2Pink)
# print(glm2Pink)
# launch_shinystan(glm2Pink)

#Then I compared the models with leave one out cross validation
  #This model was much worse than the hierarchical one, so I got rid of it.

#loo1 <- loo::loo(glm1Pink)
#loo2 <- loo::loo(glm2Pink)
# 
#loo::compare(loo1,loo2)


##################################
##  Posterior Predictive Checks ##
##################################

launch_shinystan(glm1Pink)
launch_shinystan(glm1Coral)
