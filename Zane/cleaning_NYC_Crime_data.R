########
  # Author: Zane Wolf
  # Date Created: 9/25/2017
  # Purpose: To explore and clean NYC Crime data file. 
    # Transformed some categorical variables to indicator variables
    # Filled in or deleted mising Location data
    # Organized and parsed time variables, added indicator variables
    # Filled in missing Crime Data


  # Date Last Updated: 11/18/2017

########

####################################################################################################################################################

#                                                                 HOUSEKEEPING

####################################################################################################################################################
rm(list=ls())
options(stringsAsFactors = FALSE)

setwd("C:/Users/zane/Documents/School/Classes/Harvard/OEB 201 Experimental Design and Stats/NYC_Crime")

# libraries
library(tidyverse)
  #includes ggplot2, tibble, tidyr, readr, purrr, dplyr
library(plyr)
library(stringr)
library(lubridate)

####################################################################################################################################################

#                                                                   IMPORT

####################################################################################################################################################
nyc <- read_csv("NYPD_Complaint_Data_Historic.csv")
View(nyc)

headers <- names(nyc)


####################################################################################################################################################

#                                                                DATA CLEANING

####################################################################################################################################################


#select for Completed Crimes
as.data.frame(table(nyc$CRM_ATPT_CPTD_CD)) #find out just how many were attempted....negligible, ~90,000 out of 5.5million
unique(nyc$CRM_ATPT_CPTD_CD) #Completed, attempted, NA
nyc <- subset(nyc, CRM_ATPT_CPTD_CD=="COMPLETED")

# delete the following columns: CMPLNT_TO_DT, CMPLNT_TO_TM, RPT_DT, X_COORD_CD, Y_COORD_CD, 
# and Lat_Lon

bad_vars <- names(nyc) %in% c("CRM_ATPT_CPTD_CD", "ADDR_PCT_CD", "CMPLNT_NUM", "CMPLNT_TO_DT", "CMPLNT_TO_TM", "RPT_DT", "X_COORD_CD", "Y_COORD_CD", "Lat_Lon")
nyc <- nyc[!bad_vars]

#########################################################     INDICATOR VARIABLES   #################################################################
#Reduce parks to binary
  #if it occurred in a park, doesn't matter which park, -> 1
  #if it did not occur in a park -> 0
nyc$PARKS_NM[is.na(nyc$PARKS_NM)] <- 0
nyc$PARKS_NM[nyc$PARKS_NM!=0] <- 1

#Reduce housing developments to binary
  #if it occurred in a housing development -> 1
  #if it did not -> 0
nyc$HADEVELOPT[is.na(nyc$HADEVELOPT)] <- 0
nyc$HADEVELOPT[nyc$HADEVELOPT!=0] <- 1

#Reduce jurisdiction to binary
  #if NY Police Department -> 1
  #if any other department -> 0 
      # unique(nyc$JURIS_DESC)
      # [1] "1"                                   "N.Y. HOUSING POLICE"                 "N.Y. TRANSIT POLICE"                
      # [4] "N.Y. STATE POLICE"                   "DEPT OF CORRECTIONS"                 "TRI-BORO BRDG TUNNL"                
      # [7] "OTHER"                               "PORT AUTHORITY"                      "NYC PARKS"                          
      # [10] "HEALTH & HOSP CORP"                  "METRO NORTH"                         "LONG ISLAND RAILRD"                 
      # [13] "STATN IS RAPID TRANS"                "N.Y. STATE PARKS"                    "U.S. PARK POLICE"                   
      # [16] "NEW YORK CITY SHERIFF OFFICE"        "NYS DEPT TAX AND FINANCE"            "AMTRACK"                            
      # [19] "CONRAIL"                             "POLICE DEPT NYC"                     "FIRE DEPT (FIRE MARSHAL)"           
      # [22] "NYC DEPT ENVIRONMENTAL PROTECTION"   "SEA GATE POLICE DEPT"                "DISTRICT ATTORNEY OFFICE"           
      # [25] "NYS DEPT ENVIRONMENTAL CONSERVATION"
nyc$JURIS_DESC[nyc$JURIS_DESC=='N.Y. POLICE DEPT'] <- 1
nyc$JURIS_DESC[nyc$JURIS_DESC!=1] <- 0

#########################################################         LOCATION     #####################################################################
# ~~~~~~~~~~~~~~~~~~~~~    MISSING DATA    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Reduce location of occurance description to binary
  #if inside -> 1
  #if anything else -> 0 
  # as.data.frame(table(nyc$LOC_OF_OCCUR_DESC))
    # Var1    Freq
    # 1    FRONT OF 1276501
    # 2      INSIDE 2731916
    # 3 OPPOSITE OF  151301
    # 4     OUTSIDE    2962
    # 5     REAR OF  118900

sum(is.na(nyc$BORO_NM)) #75
nyc <- nyc[!with(nyc, is.na(nyc$BORO_NM) | is.na(nyc$CMPLNT_FR_TM)),]

#define all othe others
nyc$LOC_OF_OCCUR_DESC[nyc$LOC_OF_OCCUR_DESC!='INSIDE'] <- 0
nyc$LOC_OF_OCCUR_DESC[nyc$LOC_OF_OCCUR_DESC=='INSIDE'] <- 1
#there are still a bunch of NAs...277,996 to be exact
sum(is.na(nyc$LOC_OF_OCCUR_DESC))
#based on first 100 entries, I'm setting anything with a premise description as Street, Park/playground, or Gas Station as outside
nyc$LOC_OF_OCCUR_DESC[nyc$PREM_TYP_DESC=='STREET'] <- 0
nyc$LOC_OF_OCCUR_DESC[nyc$PREM_TYP_DESC=='PARK/PLAYGROUND'] <- 0
nyc$LOC_OF_OCCUR_DESC[nyc$PREM_TYP_DESC=='GAS STATION'] <- 0
sum(is.na(nyc$LOC_OF_OCCUR_DESC))
#which type of premise descriptions have NA as location description....
unique(nyc$PREM_TYP_DESC[is.na(nyc$LOC_OF_OCCUR_DESC)])
#interesting, there are some that have NA for both...how many? 
sum(is.na(nyc$LOC_OF_OCCUR_DESC)&is.na(nyc$PREM_TYP_DESC))
#...5,897
#are they all from the same borrow? 
as.data.frame(table(nyc$BORO_NM[(is.na(nyc$LOC_OF_OCCUR_DESC)&is.na(nyc$PREM_TYP_DESC))]))
#they seem to be pretty evenly distributed, except for staten island. 
as.data.frame(table(nyc$BORO_NM))
#but staten island has a lower amount of crimes reported anyway, so having a reduced number of missing location seems to be in proportion

#since part of my goal is to model based off of location, crime reports without locational description data is rather useless.
#I'm going to go ahead and delete the 5,897 incidences without that data
nyc <- nyc[!with(nyc,is.na(nyc$LOC_OF_OCCUR_DESC) & is.na(nyc$PREM_TYP_DESC)),]

#now back to the others...
na_premises <- unique(nyc$PREM_TYP_DESC[is.na(nyc$LOC_OF_OCCUR_DESC)])
  # [1] "OTHER"                        "TRANSIT - NYC SUBWAY"         "RESIDENCE - APT. HOUSE"       "BUS STOP"                    
  # [5] "GROCERY/BODEGA"               "TUNNEL"                       "RESIDENCE-HOUSE"              "BRIDGE"                      
  # [9] "AIRPORT TERMINAL"             "PUBLIC BUILDING"              "FOOD SUPERMARKET"             "BUS (NYC TRANSIT)"           
  # [13] "OPEN AREAS (OPEN LOTS)"       "PARKING LOT/GARAGE (PUBLIC)"  "PARKING LOT/GARAGE (PRIVATE)" "HIGHWAY/PARKWAY"             
  # [17] "DRY CLEANER/LAUNDRY"          "HOTEL/MOTEL"                  "CLOTHING/BOUTIQUE"            "STORAGE FACILITY"            
  # [21] "COMMERCIAL BUILDING"          "BAR/NIGHT CLUB"               "CONSTRUCTION SITE"            "FAST FOOD"                   
  # [25] "BANK"                         "CHAIN STORE"                  "TAXI (LIVERY LICENSED)"       "HOSPITAL"                    
  # [29] "SMALL MERCHANT"               "TAXI (YELLOW LICENSED)"       "TAXI/LIVERY (UNLICENSED)"     "TRANSIT FACILITY (OTHER)"    
  # [33] "BUS TERMINAL"                 "PUBLIC SCHOOL"                "BUS (OTHER)"                  "RESTAURANT/DINER"            
  # [37] "BEAUTY & NAIL SALON"          "MARINA/PIER"                  NA                             "RESIDENCE - PUBLIC HOUSING"  
  # [41] "DEPARTMENT STORE"             "CANDY STORE"                  "TELECOMM. STORE"              "STORE UNCLASSIFIED"          
  # [45] "DRUG STORE"                   "GYM/FITNESS FACILITY"         "CHURCH"                       "BOOK/CARD"                   
  # [49] "CHECK CASHING BUSINESS"       "ABANDONED BUILDING"           "SYNAGOGUE"                    "LIQUOR STORE"                
  # [53] "OTHER HOUSE OF WORSHIP"       "DOCTOR/DENTIST OFFICE"        "FACTORY/WAREHOUSE"            "ATM"                         
  # [57] "PRIVATE/PAROCHIAL SCHOOL"     "CEMETERY"                     "JEWELRY"                      "SOCIAL CLUB/POLICY"          
  # [61] "VARIETY STORE"                "TRAMWAY"                      "FERRY/FERRY TERMINAL"         "PHOTO/COPY"                  
  # [65] "VIDEO STORE"                  "SHOE"                         "MOSQUE"                       "LOAN COMPANY" 


#going to set the following as outside....
na_prem_outside <- na_premises[c(2,4,6,8,12:16,23,27,29:33,35,38,55,57,61,62)]
na_premises[c(2,4,6,8,12:16,23,27,29:33,35,38,55,57,61,62)]

nyc$LOC_OF_OCCUR_DESC[with(nyc, nyc$PREM_TYP_DESC %in% na_prem_outside & is.na(nyc$LOC_OF_OCCUR_DESC))] <- 0

# #set these as the inside...
na_prem_inside <-  na_premises[c(3,5,7,9:11,17:22,24:26,28,34,36,37,39:54,56,58:60,63,64,66,67)]
na_prem_inside

nyc$LOC_OF_OCCUR_DESC[with(nyc, nyc$PREM_TYP_DESC %in% na_prem_inside & is.na(nyc$LOC_OF_OCCUR_DESC))] <- 1

# #what's left....hmm, Just 'SHOE' and 'OTHER'....what the hell type is shoe? maybe shoe store?? 
unique(nyc$PREM_TYP_DESC[is.na(nyc$LOC_OF_OCCUR_DESC)])
as.data.frame(table(nyc$PREM_TYP_DESC[is.na(nyc$LOC_OF_OCCUR_DESC)]))

#set 'SHOE' to inside....why not, only a couple hundred of them
nyc$LOC_OF_OCCUR_DESC[nyc$PREM_TYP_DESC=='SHOE'] <- 1

#so what happens at Other?? 
as.data.frame(table(nyc$PD_DESC[is.na(nyc$LOC_OF_OCCUR_DESC)])) 
# a freaking lot
# lets just set it all to outside, the crimes with high frequencies (>500 occurrences) are ones that occur in 'open areas'
#--makes sense, nondescript open area as 'other'
nyc$LOC_OF_OCCUR_DESC[nyc$PREM_TYP_DESC=='OTHER'] <- 1
#no more 'missing' data and all locations set to either inside (1) or outside (0)...whew. 


############################################################        TIME      #######################################################################

#Time for Time
  #want to bin time into 1-hour segments
  #divide Hour:Minute:Second into three separate columns
nyc$Time <- nyc$CMPLNT_FR_TM
library(chron)
nyc$Time2 <- 60 * 24 * as.numeric(times(nyc$Time))
nyc <- separate(nyc, CMPLNT_FR_TM, sep= ":", into=c("Hour", "Minute", "Second"), fill='right', remove=FALSE)


#separate date, similarly, into Month, Day, Year
nyc <- separate(nyc, CMPLNT_FR_DT, sep= "/", into=c("Month", "Day", "Year"), fill='right', remove=FALSE)

as.data.frame(table(nyc$Year))
#DATA DELETION 
  #this database was supposed to be 2006-2016, but there are years here from 1905 and 1015 (prob a typo). Gonna delete the few thousand from before 2006
  #2005 is also a little skewed, though. Even though it has 10,000+ events, all the other years have nearly half a million data points. 
yearsIWant <- c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
nyc <- subset(nyc, nyc$Year %in% yearsIWant)

nyc <- subset(nyc, !nyc$Day=="01")
unique(nyc$Day)

#create month-day column for use in holiday determination
nyc$MonthDay <- paste( nyc$Month, nyc$Day, sep="-" )

#deal with date...convert to standard format
nyc$Date <- as.Date(nyc$CMPLNT_FR_DT, "%m/%d/%Y")

#find out day of week
nyc$DayName <-  weekdays(as.Date(nyc$Date))

#find out weekday or weekend
daysoftheweek <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
daysoftheweekend <- c("Saturday", "Sunday")

#create indicator variable, where 0 is a weekday and 1 is a weekend. 
nyc$Weekend <- as.integer(nyc$DayName %in% daysoftheweekend)

#some holidays are always the same day
holidays_list <- c("01-01", "02-14", "07-04", "09-04", "10-31", "12-25" )
easter_list <- c("2006-04-23", "2007-04-08", "2008-03-27", "2009-04-19", "2010-04-04", "2011-04-24", "2012-04-15", "2013-05-05", "2014-04-20", "2015-04-12")
thanksgiving_list <- c("2005-11-24", "2006-11-23", "2007-11-22", "2008-11-27", "2009-11-26", "2010-11-25", "2011-11-24", "2012-11-22", "2013-11-28", "2014-11-27", "2015-11-26", "2016-11-24")

#create Holiday indicator variable, 0 if not holiday, 1 if it matches any of the holidays specified above
nyc$Holiday <- as.integer(nyc$MonthDay %in% holidays_list, nyc$CMPLNT_FR_DT %in% easter_list, nyc$CMPLNT_FR_DT %in% thanksgiving_list)
  # > as.data.frame(table(nyc$Holiday))
    # Var1    Freq
    # 1    0 5375860
    # 2    1   93413

############################################################       CRIME       #######################################################################


unique(nyc$OFNS_DESC) #...71 different classifiers
unique(nyc$PD_DESC) #...410 different classifiers
unique(nyc$LAW_CAT_CD) #...3 different classifiers

#well, 71 is a lot better than 410...
#I'm not sure there's anything between 3 and 71 without loosing a lot of data. 71 will have to do. 
# so I guess I'm looking at a hierarchical (between boros) multinomial (unordered categorial crime type) model? 

# ~~~~~~~~~~~~~~~~~~~~~    MISSING DATA    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#oh look...NAs. Yay. 
  # sum(is.na(nyc$OFNS_DESC))
  # as.data.frame(table(nyc$PD_DESC[is.na(nyc$OFNS_DESC)]))
    #for the 56 cases where the OFNS_DESC is NA but the PD_DESC is not, I could substitute the PD_DESC for the OFNS_DESC
    #...or I could go through and find previous incidences where the PD_DESC is the same OFNS_DESC is not NA, and substitute that OFNS_DESC for the NA
    #probably the better way to go....but probs a lot more involved. Ugh. 

#solution....this takes a bit. Just FYI. 
#Feel free to delete the message(i) lines if you don't want to see a bazilion numbers on your screen
#I use it as a progress measurement, but that's just me
for (i in 1:nrow(nyc)){
  if (is.na(nyc$OFNS_DESC[i])){
    crimetype=nyc$PD_DESC[i]
    othercrimetypes=unique(nyc$OFNS_DESC[nyc$PD_DESC==crimetype])
    if (length(othercrimetypes)==2){
      nyc$OFNS_DESC[i] <- othercrimetypes[2]
      message(i)
    }
    else if (is.na(othercrimetypes)){
      nyc$OFNS_DESC[i] <- nyc$PD_DESC[i]
      message(i)
    }
  }
}

# sum(is.na(nyc$OFNS_DESC))  
#   
# #no more OFNS_DESC NAs. Yay!

#reorder the columns to put similar variables together 
#don't care about Minute or Second, so not including those
#nixing PD_DESC since I'm going to use OFNS_DESC as my response variable
nyc_clean <- nyc[,c("CMPLNT_FR_DT", "Time2","Date", "Month", "Day", "Year", "MonthDay", 
                    "Holiday", "DayName", "Weekend", "CMPLNT_FR_TM", "Hour", "OFNS_DESC", "LAW_CAT_CD", 
                    "JURIS_DESC", "BORO_NM", "LOC_OF_OCCUR_DESC", "PARKS_NM", "HADEVELOPT")]


####################################################################################################################################################

#                                                                   CRIME CONDENSATION 

####################################################################################################################################################
#need to par 72 crime types down to something manageable. 

#get rid of boring crimes
useless_crimes <- c("ABORTION", "AGRICULTURE & MRKTS LAW-UNCLASSIFIED", "ALCOHOLIC BEVERAGE CONTROL LAW", "ANTICIPATORY OFFENSES", "CHILD ABANDONMENT/NON SUPPORT", "DISORDERLY CONDUCT", "DISRUPTION OF A RELIGIOUS SERV", "ENDAN WELFARE INCOMP", "ESCAPE 3", "FORTUNE TELLING", "GAMBLING", "JOSTLING", "NEW YORK CITY HEALTH CODE", "NYS LAWS-UNCLASSIFIED FELONY", "NYS LAWS-UNCLASSIFIED VIOLATION", "OTHER STATE LAWS", "OTHER STATE LAWS (NON PENAL LA", "OTHER STATE LAWS (NON PENAL LAW)", "OTHER TRAFFIC INFRACTION", "PROSTITUTION & RELATED OFFENSES", "THEFT,RELATED OFFENSES,UNCLASS", "UNDER THE INFLUENCE OF DRUGS", "UNLAWFUL POSS. WEAP. ON SCHOOL")

nyc_2 <- subset(nyc_clean, !nyc_clean$OFNS_DESC %in% useless_crimes)

#Combine similar crimes
  #So I originally did this with a for loop and it ran for DAYS. Not joking. I'm never running a for loop in R ever again if I can help it. 
  #the original for-loop is commented and located at the bottom of this code. 
  #these apply functions literally took about a minute to finish. versus FOUR FREAKING DAYS. Never ever again. Lesson learned. 

var <- "OFNS_DESC"

cd_old <- c("UNAUTHORIZED USE OF A VEHICLE", "VEHICLE AND TRAFFIC LAWS")
nyc_2[,var] <- sapply(nyc_2[,var],function(x) ifelse(x %in% cd_old, "VEHICLE/TRAFFIC LAWS RELATED",x))

cd_old_2 <- c("HOMICIDE-NEGLIGENT,UNCLASSIFIE", "HOMICIDE-NEGLIGENT-VEHICLE", "MURDER & NON-NEGL. MANSLAUGHTER")
nyc_2[,var] <- sapply(nyc_2[,var],function(x) ifelse(x %in% cd_old_2, "MURDER",x))

cd_old_3 <- c("OFFENSES AGAINST PUBLIC ADMINI", "OFF. AGNST PUB ORD SENSBLTY &", "OFFENSES AGAINST MARRIAGE UNCL", "OFFENSES AGAINST PUBLIC SAFETY")
nyc_2[,var] <- sapply(nyc_2[,var],function(x) ifelse(x %in% cd_old_3, "MISC. OFFENSES",x))

cd_old_4 <- c("RAPE", "SEX CRIMES")
nyc_2[,var] <- sapply(nyc_2[,var],function(x) ifelse(x %in% cd_old_4, "RAPE OR SEX CRIME",x))

cd_old_5 <- c("ADMINISTRATIVE CODES", "ADMINISTRATIVE CODES")
nyc_2[,var] <- sapply(nyc_2[,var],function(x) ifelse(x %in% cd_old_5, "MISCELLANEOUS PENAL LAW",x))

cd_old_6 <- c("BURGLAR'S TOOLS", "BURGLARY")
nyc_2[,var] <- sapply(nyc_2[,var],function(x) ifelse(x %in% cd_old_6, "BURGLARY RELATED",x))

cd_old_7 <- c("FRAUDS", "FRAUDULENT ACCOSTING", "OFFENSES INVOLVING FRAUD")
nyc_2[,var] <- sapply(nyc_2[,var],function(x) ifelse(x %in% cd_old_7, "FRAUD RELATED",x))

cd_old_8 <- c("GRAND LARCENY", "PETIT LARCENY")
nyc_2[,var] <- sapply(nyc_2[,var],function(x) ifelse(x %in% cd_old_8, "GRAND/PETIT LARCENY",x))

cd_old_9 <- c("GRAND LARCENY OF MOTOR VEHICLE", "PETIT LARCENY OF MOTOR VEHICLE")
nyc_2[,var] <- sapply(nyc_2[,var],function(x) ifelse(x %in% cd_old_9, "VEHICULAR GRAND/PETIT LARCENY",x))

cd_old_10 <- c("INTOXICATED & IMPAIRED DRIVING", "INTOXICATED/IMPAIRED DRIVING")
nyc_2[,var] <- sapply(nyc_2[,var],function(x) ifelse(x %in% cd_old_10, "DUI",x))

cd_old_11 <- c("KIDNAPPING", "KIDNAPPING AND RELATED OFFENSES", "KIDNAPPING & RELATED OFFENSES")
nyc_2[,var] <- sapply(nyc_2[,var],function(x) ifelse(x %in% cd_old_11, "KIDNAPPING RELATED",x))

cd_old_12 <- c("LOITERING", "LOITERING FOR DRUG PURPOSES", "LOITERING FOR PROSTITUTION OR", "LOITERING/DEVIATE SEX", "LOITERING/GAMBLING (CARDS, DIC")
nyc_2[,var] <- sapply(nyc_2[,var],function(x) ifelse(x %in% cd_old_12, "LOITERING RELATED",x))

cd_old_13 <- c("OFFENSES AGAINST THE PERSON", "OFFENSES RELATED TO CHILDREN")
nyc_2[,var] <- sapply(nyc_2[,var],function(x) ifelse(x %in% cd_old_13, "OFFENSES AGAINST HUMANS",x))

cd_old_14 <- c("OTHER OFFENSES RELATED TO THEF", "THEFT-FRAUD", "THEFT OF SERVICES")
nyc_2[,var] <- sapply(nyc_2[,var],function(x) ifelse(x %in% cd_old_14, "THEFT RELATED",x))

as.data.frame(table(nyc_2$OFNS_DESC))
#down to only 26 crimes. Sweet. 


#narrow it down to modeling differences between Misc. Offenses and criminal mischief
#the ratio between these two, which one is more likely, varies by hours and neighborhoods
nyc_M <- subset(nyc_2, nyc_2$LAW_CAT_CD =="MISDEMEANOR") #ultimately an unnecessary step, but part of the original thought process

nyc_M_small <- subset(nyc_M, nyc_M$OFNS_DESC %in% c("MISC. OFFENSES", "CRIMINAL MISCHIEF & RELATED OF"))

nyc_2010 <- subset(nyc_M_small, nyc_M_small$Year==2010)



####################################################################################################################################################

#                                                                   SAVE CLEANED DATA FILE 

####################################################################################################################################################
write.csv(nyc_2010, "NYPD_Crime_Data_CLEAN_2010.csv")


####################################################################################################################################################

#                                                                        OLD CODE

####################################################################################################################################################


#Here is the original for loop: 
# for (i in 1906650:nrow(nyc_clean)){
#   if (nyc_clean$OFNS_DESC[i] == "ADMINISTRATIVE CODE" || nyc_clean$OFNS_DESC == "ADMINISTRATIVE CODES"){
#     nyc_clean$OFNS_DESC[i] <- "MISCELLANEOUS PENAL LAW"
#     message(i)
#   } else if (nyc_clean$OFNS_DESC[i] == "BURGLAR'S TOOLS" || nyc_clean$OFNS_DESC[i] == "BURGLARY"){
#     nyc_clean$OFNS_DESC[i] <- "BURGLARY RELATED"
#     message(i)
#   } else if (nyc_clean$OFNS_DESC[i] == "FRAUDS" || nyc_clean$OFNS_DESC[i] == "FRAUDULENT ACCOSTING" || nyc_clean$OFNS_DESC[i] == "OFFENSES INVOLVING FRAUD"){
#     nyc_clean$OFNS_DESC[i] <- "FRAUD RELATED"
#     message(i)
#   } else if (nyc_clean$OFNS_DESC[i] == "GRAND LARCENY" || nyc_clean$OFNS_DESC[i] == "PETIT LARCENY"){
#     nyc_clean$OFNS_DESC[i] <- "GRAND/PETIT LARCENY"
#     message(i)
#   } else if (nyc_clean$OFNS_DESC[i] == "GRAND LARCENY OF MOTOR VEHICLE" || nyc_clean$OFNS_DESC[i] == "PETIT LARCENY OF MOTOR VEHICLE"){
#     nyc_clean$OFNS_DESC[i] <- "VEHICULAR GRAND/PETIT LARCENY"
#     message(i)
#   } else if (nyc_clean$OFNS_DESC[i] == "HOMICIDE-NEGLIGENT-VEHICLE" || nyc_clean$OFNS_DESC[i] == "HOMICIDE-NEGLIGENT,UNCLASSIFIED" ||nyc_clean$OFNS_DESC[i] == "MURDER & NON-NEGL. MANSLAUGHTER"){
#     nyc_clean$OFNS_DESC[i] <- "MURDER"
#     message(i)
#   } else if (nyc_clean$OFNS_DESC[i] == "INTOXICATED & IMPAIRED DRIVING" || nyc_clean$OFNS_DESC[i] == "INTOXICATED/IMPAIRED DRIVING"){
#     nyc_clean$OFNS_DESC[i] <- "DUI"
#     message(i)
#   } else if (nyc_clean$OFNS_DESC[i] == "KIDNAPPING" || nyc_clean$OFNS_DESC[i] == "KIDNAPPING AND RELATED OFFENSES"|| nyc_clean$OFNS_DESC[i] == "KIDNAPPING & RELATED OFFENSES"){
#     nyc_clean$OFNS_DESC[i] <- "KIDNAPPING RELATED"
#     message(i)
#   } else if (nyc_clean$OFNS_DESC[i] == "LOITERING" || nyc_clean$OFNS_DESC[i] == "LOITERING FOR DRUG PURPOSES"|| nyc_clean$OFNS_DESC[i] == "LOITERING FOR PROSTITUTION OR"|| nyc_clean$OFNS_DESC[i] == "LOITERING/DEVIATE SEX"|| nyc_clean$OFNS_DESC[i] == "LOITERING/GAMBLING (CARDS, DIC"){
#     nyc_clean$OFNS_DESC[i] <- "LOITERING RELATED"
#     message(i)
#   } else if (nyc_clean$OFNS_DESC[i] == "OFF. AGNST PUB ORD SENSBLTY &" || nyc_clean$OFNS_DESC[i] == "OFFENSES AGAINST MARRIAGE UNCL"|| nyc_clean$OFNS_DESC[i] == "OFFENSES AGINST PUBLIC ADMINI"|| nyc_clean$OFNS_DESC[i] == "OFFENSES AGAINST PUBLIC SAFETY"){
#     nyc_clean$OFNS_DESC[i] <- "MISC. OFFENSES"
#     message(i)
#   } else if (nyc_clean$OFNS_DESC[i] == "OFFENSES AGAINST THE PERSON" || nyc_clean$OFNS_DESC[i] == "OFFENSES RELATED TO CHILDREN"){
#     nyc_clean$OFNS_DESC[i] <- "OFFENSES AGAINST HUMANS"
#     message(i)
#   } else if (nyc_clean$OFNS_DESC[i] == "OTHER OFFENSES RELATED TO THEF" || nyc_clean$OFNS_DESC[i] == "THEFT-FRAUD"|| nyc_clean$OFNS_DESC[i] == "THEFT OF SERVICES"){
#     nyc_clean$OFNS_DESC[i] <- "THEFT RELATED"
#     message(i)
#   } else if (nyc_clean$OFNS_DESC[i] == "RAPE" || nyc_clean$OFNS_DESC[i] == "SEX CRIMES"){
#     nyc_clean$OFNS_DESC[i] <- "RAPE OR SEX CRIME"
#     message(i)
#   }
# }
