#### February 21 2024 ########################
#### Mitchell Parsons ########################
#### Cougar kill rates and pig scavenging ####
#### Uses data from GPS collared cougars #####
#### ClusterMasterSheet.csv ##################
#### cougarclusterinvestigations.csv #########
#### Uses data from cameras at kill sites ####

#### Load packages ####
library(tidyverse)
library(lme4)
library(survival)

#### Read in cluster data ####
clusters <- read.csv("../RawData/ClustersMasterSheet.csv")

#Clean up animal IDs and format dates 
clusters <- clusters %>% 
  filter(AID %in% c("SF09","SF01","SM06","SF1","SF9","Shared","SM6")) %>% 
  mutate(AID = case_when(
    AID %in% c("SF1","SF01") ~ "SF1",
    AID %in% c("SF9","SF09") ~ "SF9",
    AID %in% c("SM6","SM06") ~ "SM6"
  )) %>% 
  mutate(clus_start = mdy_hm(clus_start),
         clus_end = mdy_hm(clus_end),
         inv.date = mdy(inv.date))

head(clusters)


#### Identify periods of continuous monitoring ####
# For each cougar, identify periods of continuous monitoring
# Continuous periods end when a cluster isn't investigated
# Or if > ndays passes between investigated clusters

ndays <- 20

# For SM6
# Filter cluster data to just SM6
SM6_dat <- clusters %>% 
  filter(AID == "SM6")

#Remove uninvestigated clusters that are known not to be kills
SM6_dat <- SM6_dat[SM6_dat$clus_ID != "S23-C20",]

# Create an empty vector to monitor continuous periods
cont.mon <- vector()
cont.mon[1] <- 1
temp <- 1
# Loop through all clusters, if a cluster was missed or >ndays passed
# counter increases by one. Continuous periods all have same value of cont.mon

for(i in 2:nrow(SM6_dat)){
  temp <- temp + as.numeric(is.na(SM6_dat$inv.date[i])) + 
    as.numeric(as.numeric(difftime(SM6_dat$clus_start[i],SM6_dat$clus_start[i-1],units = "days")) > ndays)
  cont.mon[i] <- temp
}

# Add cont.mon to data
cont.mon[is.na(SM6_dat$inv.date)] <- NA
SM6_dat$cont.mon <- cont.mon

## Repeat process For SF1 ##
SF1_dat <- clusters %>% 
  filter(AID == "SF1")

# Remove uninvestigated clusters that are known not to be kills
SF1_dat <- SF1_dat[!SF1_dat$clus_ID %in% c("CXX","Den1","Den2"),]
SF1_dat <- SF1_dat[-1,]

cont.mon <- vector()
cont.mon[1] <- 1
temp <- 1
for(i in 2:nrow(SF1_dat)){
  temp <- temp + as.numeric(is.na(SF1_dat$inv.date[i])) + 
    as.numeric(as.numeric(difftime(SF1_dat$clus_start[i],SF1_dat$clus_start[i-1],units = "days")) > 20)
  cont.mon[i] <- temp
}

cont.mon[is.na(SF1_dat$inv.date)] <- NA
SF1_dat$cont.mon <- cont.mon

## Repeat For SF9 ##
SF9_dat <- clusters %>% 
  filter(AID == "SF9")

# Remove uninvestigated clusters that are known not to be kills
SF9_dat <- SF9_dat[!SF9_dat$clus_ID %in% c("W21-17","S22-C11A","S22-C20",
                                           "S22-C26","S22-C27","W22 - 10/28",
                                           "W22-Capture","S23-C06.1","S23-C35",
                                           "S23-C53","W23-C08"),]

cont.mon <- vector()
cont.mon[1] <- 1
temp <- 1
for(i in 2:nrow(SF9_dat)){
  temp <- temp + as.numeric(is.na(SF9_dat$inv.date[i])) + 
    as.numeric(as.numeric(difftime(SF9_dat$clus_start[i],SF9_dat$clus_start[i-1],units = "days")) > 20)
  cont.mon[i] <- temp
}

cont.mon[is.na(SF9_dat$inv.date)] <- NA
SF9_dat$cont.mon <- cont.mon

################################
#### Read in kill site data ####
################################

#This file has been corrected to properly account for clusters with multiple prey
killsitedata <- read.csv("../RawData/cougar_cluster_investigations_correctedkill2_age.csv")

#### Read in camera data for pig scavenging ####
# Seperaete timelapse files from different monitoring periods
# Read each file in, format dates and times
photodat <- 
  read.csv("../RawData/TimelapseData_KillSites_Summer2021-Summer2022.csv")
photodat$Date <- as.Date(photodat$Date, format = "%d-%b-%y")
photodat$Time <- hms(photodat$Time)

photodat2 <- 
  read.csv("../RawData//TimelapseData_KillSites_Winter2022.csv")
photodat2$Date <- as.Date(photodat2$Date, format = "%d-%b-%y")
photodat2$Time <- hms(photodat2$Time)

photodat3 <-
  read.csv("../RawData/TimelapseData_KillSites_Summer2023.csv")
photodat3$Date <- as.Date(photodat3$Date, format = "%d-%b-%y")
photodat3$Time <- hms(photodat3$Time)

photodat4 <-
  read.csv("../RawData/TimelapseData_KillSites_Winter2023.csv")
photodat4$Date <- as.Date(photodat4$Date, format = "%d-%b-%y")
photodat4$Time <- hms(photodat4$Time)

#Combine into one file and remove indidual files
photodatfull <- rbind(photodat,photodat2,photodat3,photodat4)
rm(photodat,photodat2,photodat3,photodat4)

#### Processing scavenging data ####

# create cacheID variable and add to photo data
# Some file names had extra info appended, trimming all names to just include
# cluster name to be consistent with killsite data
cacheID <- vector()
for(i in 1:nrow(photodatfull)){
  if(nchar(photodatfull$RelativePath[i]) > 13){
    cacheID[i] <- substr(photodatfull$RelativePath[i],1,8)
  } else(cacheID[i] <- photodatfull$RelativePath[i])
}
unique(cacheID)

#Correct a few mismatches in cluster IDs
cacheID[cacheID == "SF01-C01"] <- "SF1_C01"
cacheID[cacheID == "SF01-C04"] <- "SF1_C04"
cacheID[cacheID == "SF01-C16"] <- "SF1_C16"
cacheID[cacheID == "SF09-C06"] <- "SF9_C06"
cacheID[cacheID == "SF09-C16"] <- "SF9_C16"
cacheID[cacheID == "SF09_C09"] <- "SF9_C09"
cacheID[cacheID == "SF01_W21-C03"] <- "SF1_W21-03"
cacheID[cacheID == "SF09_S22-C12"] <- "SF9_S22-C12"
cacheID[cacheID == "SF09_S22-C19"] <- "SF9_S22-C19"
cacheID[cacheID == "SF09_W21-C03"] <- "SF9_W21-03"
cacheID[cacheID == "SF09_W21-C18"] <- "SF9_W21-C18"
cacheID[cacheID == "SM06_S22-C05"] <- "SM6_S22-C05"
cacheID[cacheID == "SM06_S22-C06"] <- "SM6_S22-C06"
cacheID[cacheID == "SM06_W21-C06"] <- "SM6_W21-06"

photodatfull$cacheID <- cacheID
# Check if all cacheIDs from cameras match kills
# This should return "character(0)"
unique(photodatfull$cacheID)[which(unique(photodatfull$cacheID) 
                                   %in% killsitedata$cluster_id == FALSE)]


#### Calculate pig visits and behaviors for each carcass ####
# For each cluster with a camera, extract whether pigs visited
# the date of the first visit, whether pigs fed, and date of first feed

kills <- unique(photodatfull$cacheID)
pigsvisit <- vector()
firstvisit <- vector()
pigfeed <- vector()
firstfeeding <- vector()

for(i in 1:length(kills)){
  temp <- photodatfull[photodatfull$cacheID == kills[i],]
  pigsvisit[i] <- as.numeric(any(temp$Species == "Pig"))
  pigfeed[i] <- as.numeric(any(temp$PigBehavior == "Feeding"))
}

for(i in which(pigsvisit == 1)){
  temp <- photodatfull[photodatfull$cacheID == kills[i],]
  temp2 <- temp[temp$Species == "Pig",]
  firstvisit[i] <- (min(temp2$Date))
}

for(i in which(pigfeed == 1)){
  temp <- photodatfull[photodatfull$cacheID == kills[i],]
  temp2 <- temp[temp$PigBehavior == "Feeding",]
  firstfeeding[i] <- (min(temp2$Date))
}

# Add on NAs for cameras that did not have pig visits or feeding
# The last camera in order had a visit, so no change to first visit
firstvisit <- c(firstvisit)
# The last camera in order did not have feeding, so add one NA so lengths match
firstfeeding <- c(firstfeeding,NA)

# Bind data into a dataframe
# Format dates and rename columns

visitdata <- data.frame(cbind(kills,pigsvisit,pigfeed,firstvisit,firstfeeding))
visitdata$firstvisit <- as.Date(firstvisit,origin = mdy("01-01-1970"))
visitdata$firstfeeding <- as.Date(firstfeeding,origin = mdy("01-01-1970"))
colnames(visitdata) <- c("cluster_id","pigsvisit","pigfeed",
                         "firstvisit","firstfeeding")

#### Combine camera data with kill site investigation data ####
#This keeps all feeding sites, and adds camera data to those with cameras
killsitedata <- killsitedata %>% 
  left_join(visitdata, by = "cluster_id")

# Filter feeding sites data to "current" clusters, clusters of focal cougars
# add data for prey weights based on literature
# and select only needed columns for subsequent analyses

killsitedata <- killsitedata[killsitedata$type == "current",]
killsitedata <- killsitedata[killsitedata$cougar_id %in% c("SF09","SF01","SM06"),]
killsitedata <- killsitedata %>% 
  mutate(species_age = paste0(species,"_",age)) %>% 
  mutate(prey_mass = case_when(
    species_age == "deer_neonate" ~ 15,
    species_age == "deer_fawn" ~ 27,
    species_age == "deer_yearling" ~ 49,
    species_age == "deer_adult" ~ 55,
    species_age == "deer_UNK" ~ 21,
    species_age == "pig_piglet" ~ 6,
    species_age == "pig_juvenile" ~ 27,
    species_age == "pig_yearling" ~ 47,
    species_age == "pig_subadult" ~ 64,
    species_age == "pig_adult" ~ 85,
    species_age == "pig_UNK" ~ 20,
    species_age == "elk_neonate" ~ 30,
    species_age == "elk_calf" ~ 80,
    species_age == "elk_yearling" ~ 150,
    species_age == "elk_adult" ~ 200,
    species_age == "band tailed pigeon_adult" ~ 0.5,
    species_age == "beaver_" ~ 20,
    species_age == "beaver_neonate" ~ 10,
    species_age == "bobcat_UNK" ~ 10,
    species_age == "coyote_adult" ~ 15,
    species_age == "coyote_neonate" ~ 5,
    species_age == "coyote_UNK" ~ 10,
    species_age == "deer_" ~ 21,
    species_age == "duck_neonate" ~ 0.5,
    species_age == "ground squirrel_adult" ~ 0.5,
    species_age == "lagomorph_adult" ~ 2.5,
    species_age == "mallard/wood duck_adult" ~ 0.5,
    species_age == "mallard_adult" ~ 0.5,
    species_age == "opossum_neonate" ~ 2,
    species_age == "rodent_UNK" ~ 0.5,
    species_age == "squirrel_adult" ~ 0.5,
    species_age == "turkey_adult" ~ 5,
    species_age == "UNK_UNK" ~ 1,
    TRUE ~ 0
  )) %>% 
  mutate(prey2_mass = case_when(
    species.1 == "deer_neonate" ~ 15,
    species.1 == "pig_neonate" ~ 4,
    species.1 == "ground squirrel_adult" ~ 0.5,
    species.1 == "mallard_adult" ~ 0.5,
    species.1 == "Kangaroo rat" ~ 0.5,
    species.1 == "scrub jay" ~ 0.5,
    TRUE ~ 0
  )) %>% 
  mutate(total_biomass = (prey_mass*n_prey + prey2_mass)) %>%
  select(cluster_id,cougar_id,form_date,duration_h,carcass_found,species,sex,age,est_age,
         n_prey,confidence_cougar_kill,other_sign,scavenged,scavenged_by,
         kill_2,species.1,species_age,prey_mass,prey2_mass,
         total_biomass,notes,pigsvisit,pigfeed,firstvisit,firstfeeding)

# Format and add additional columns related to pig scavenging
killsitedata$form_date <- mdy(killsitedata$form_date)
# Add time to pig discovery in days
killsitedata$timetodisc <- as.numeric(difftime(killsitedata$firstvisit,killsitedata$form_date,units = "days"))
# Add time to pig feeding in days
killsitedata$timetofeed <- as.numeric(difftime(killsitedata$firstfeeding, killsitedata$form_date, units = "days"))
# Add whether the prey species monitored was a pig
killsitedata$is.pig <- as.numeric(killsitedata$species == "pig")
# Add a 1/0 column for if pigs visited 
killsitedata$pigsvisit <- as.numeric(killsitedata$pigsvisit)
# Add a 1/0 column for if pigs fed
killsitedata$pigfeed <- as.numeric(killsitedata$pigfeed)

# Add a column specifying if pig scavenging occurred based on both evidence
# at the site when investigated and camera data
# camera data is only counted if pigs arrived within 'scavenge.days' of 
# the cougar cluster beginning

scavenge.days <- 5
killsitedata$pig.scavenge <- as.numeric(grepl("pig",killsitedata$scavenged_by))
killsitedata$pig.scavenge[!is.na(killsitedata$timetofeed) & 
                                   killsitedata$timetofeed < scavenge.days] <- 1

#How many of each cougars kills were scavenged
tapply(killsitedata$pig.scavenge,killsitedata$cougar_id,sum)

# Create seperate data frame that includes only kills with cameras for later use
killswithcams <- killsitedata[killsitedata$cluster_id %in% visitdata$cluster_id,]

#### Combine kill data with cluster data ####
# For each individual, combine killsite data with the cluster data
# That includes the continuous monitoring periods

# For SM6
# Select just SM6's kills
SM6_kills <- killsitedata[killsitedata$cougar_id == "SM06",]
# Create a cluster ID column that matches with the kill data
SM6_dat$cluster_id <- paste0(SM6_dat$AID,"_",SM6_dat$clus_ID)
# Format dates and time
SM6_dat$form <- substring(as.character(SM6_dat$clus_start),1,10)
SM6_dat$form <- ymd(SM6_dat$form)
SM6_dat$hour <- hour(SM6_dat$clus_start)
# Join, keeping all clusters even if no kill was found
SM6_combined <- SM6_dat %>%
  left_join(SM6_kills,by = "cluster_id")

# Repeat for SF1
SF1_kills <- killsitedata[killsitedata$cougar_id == "SF01",]

SF1_dat$cluster_id <- paste0(SF1_dat$AID,"_",SF1_dat$clus_ID)
SF1_dat$form <- substring(as.character(SF1_dat$clus_start),1,10)
SF1_dat$form <- ymd(SF1_dat$form)
SF1_dat$hour <- hour(SF1_dat$clus_start)

SF1_combined <- SF1_dat %>%
  left_join(SF1_kills,by = "cluster_id")

# Repeat for SF9
SF9_kills <- killsitedata[killsitedata$cougar_id == "SF09",]

SF9_dat$cluster_id <- paste0(SF9_dat$AID,"_",SF9_dat$clus_ID)
SF9_dat$form <- substring(as.character(SF9_dat$clus_start),1,10)
SF9_dat$form <- ymd(SF9_dat$form)
SF9_dat$hour <- hour(SF9_dat$clus_start)

SF9_combined <- SF9_dat %>%
  left_join(SF9_kills,by = "cluster_id")

#### Add in kitten data ####
# Read in data on when female cougars had kittens with them 
kittendat <- 
  read.csv("../RawData/cougar_kitten_dates.csv")
# Format dates
kittendat$kitten_start <- mdy(kittendat$kitten_start)
kittendat$kitten_end <- mdy(kittendat$kitten_end)
# Correct animal ID's to match with other data
kittendat$cougar_id[kittendat$cougar_id == "SF09"] <- "SF9"
kittendat$cougar_id[kittendat$cougar_id == "SF01"] <- "SF1"

# For each female cougar, use dates to determine if kittens were present during
# The time of each kill
haskittens_SF9 <- vector()
for(i in 1:nrow(SF9_combined)){
  temp <- SF9_combined[i,]
  temp2 <- kittendat[kittendat$cougar_id == temp$AID,]
  haskittens_SF9[i] <- as.numeric(temp$clus_start > temp2$kitten_start[1] & temp$clus_start < temp2$kitten_end[1] |
                                temp$clus_start > temp2$kitten_start[2] & temp$clus_start < temp2$kitten_end[2])
}

SF9_combined$haskittens <- haskittens_SF9

haskittens_SF1 <- vector()
for(i in 1:nrow(SF1_combined)){
  temp <- SF1_combined[i,]
  temp2 <- kittendat[kittendat$cougar_id == temp$AID,]
  haskittens_SF1[i] <- as.numeric(temp$clus_start > temp2$kitten_start[1] & temp$clus_start < temp2$kitten_end[1] |
                                    temp$clus_start > temp2$kitten_start[2] & temp$clus_start < temp2$kitten_end[2])
}

SF1_combined$haskittens <- haskittens_SF1

# SM6 was a male, so never had kittens
SM6_combined$haskittens <- 0

#### Group by monitoring period and sum total prey consumed ####
# For each cougar, use combined data to group by monitoring period (cont.mon)
# Then calculate the total number of kills, biomass of kills, number scavenged,
# biomass scavenged, length of the monitoring period, kill rate (kg/day), and 
# whether kittens were present during each period
# Select only columns needed for future analysis

# SM6
SM6_periods <- SM6_combined %>%
  select(AID,clus_start,clus_end,lat,lon,found,carcass_found,notes.x,
         cont.mon,cluster_id,duration_h,species.y,sex,age,est_age,
         confidence_cougar_kill,other_sign,scavenged,
         scavenged_by,kill_2,species.1,species_age,total_biomass,
         pig.scavenge,haskittens) %>% 
  filter(carcass_found == "Y") %>% 
  mutate(clus_start_date = ymd(substring(as.character(clus_start),1,10)),
         clus_end_date = ymd(substring(as.character(clus_end),1,10))) %>% 
  group_by(cont.mon) %>% 
  summarise(biomass.consumed = sum(total_biomass),
            biomass.scavenged = sum(total_biomass*pig.scavenge),
            prop.bio.scav = biomass.scavenged/biomass.consumed,
            nkills = n(),
            nscavenged = sum(pig.scavenge),
            prop.scavenged = nscavenged/nkills,
            period_start = min(clus_start_date),
            period_end = max(clus_end_date),
            period_length = as.numeric(difftime(period_end,period_start,units = "days")),
            kg.per.day = biomass.consumed/period_length,
            kittens = mean(haskittens))

# SF1
SF1_periods <- SF1_combined %>%
  select(AID,clus_start,clus_end,lat,lon,found,carcass_found,notes.x,
         cont.mon,cluster_id,duration_h,species.y,sex,age,est_age,
         confidence_cougar_kill,other_sign,scavenged,
         scavenged_by,kill_2,species.1,species_age,total_biomass,
         pig.scavenge,haskittens) %>% 
  filter(carcass_found == "Y") %>% 
  mutate(clus_start_date = ymd(substring(as.character(clus_start),1,10)),
         clus_end_date = ymd(substring(as.character(clus_end),1,10))) %>% 
  group_by(cont.mon) %>% 
  summarise(biomass.consumed = sum(total_biomass),
            biomass.scavenged = sum(total_biomass*pig.scavenge),
            prop.bio.scav = biomass.scavenged/biomass.consumed,
            nkills = n(),
            nscavenged = sum(pig.scavenge),
            prop.scavenged = nscavenged/nkills,
            period_start = min(clus_start_date),
            period_end = max(clus_end_date),
            period_length = as.numeric(difftime(period_end,period_start,units = "days")),
            kg.per.day = biomass.consumed/period_length,
            kittens = mean(haskittens))

# SF9
SF9_periods <- SF9_combined %>%
  select(AID,clus_start,clus_end,lat,lon,found,carcass_found,notes.x,
         cont.mon,cluster_id,duration_h,species.y,sex,age,est_age,
         confidence_cougar_kill,other_sign,scavenged,
         scavenged_by,kill_2,species.1,species_age,total_biomass,
         pig.scavenge,haskittens) %>% 
  filter(carcass_found == "Y") %>% 
  mutate(clus_start_date = ymd(substring(as.character(clus_start),1,10)),
         clus_end_date = ymd(substring(as.character(clus_end),1,10))) %>% 
  group_by(cont.mon) %>% 
  summarise(biomass.consumed = sum(total_biomass),
            biomass.scavenged = sum(total_biomass*pig.scavenge),
            prop.bio.scav = biomass.scavenged/biomass.consumed,
            nkills = n(),
            nscavenged = sum(pig.scavenge),
            prop.scavenged = nscavenged/nkills,
            period_start = min(clus_start_date),
            period_end = max(clus_end_date),
            period_length = as.numeric(difftime(period_end,period_start,units = "days")),
            kg.per.day = biomass.consumed/period_length,
            kittens = mean(haskittens))

#### Kill Rate Analysis ####
# Then combine data for all 3 cougars for analysis
SM6_periods$AID <- "SM6"
SF9_periods$AID <- "SF9"
SF1_periods$AID <- "SF1"

all_periods <- data.frame(rbind(SM6_periods,SF9_periods,SF1_periods))

# Add column for cougar sex
all_periods$sexF <- as.numeric(grepl("F",all_periods$AID))

# Add column for month that monitoring started and then what season period was in
all_periods$month <- month(all_periods$period_start)
all_periods <- all_periods %>% 
  mutate(season = case_when(
    month %in% c(4,5,6,7) ~ "summer",
    month %in% c(10,11,12,1) ~ "winter"
  ))

# Filter to only monitoring periods that were greater than min.period
min.period <- 21
all_periods.20 <- all_periods[all_periods$period_length >= min.period,]

# plot a histogram of kill rates
hist(all_periods.20$kg.per.day)

all_periods.20$prop.bio.scav2 <- as.numeric(scale(all_periods.20$prop.bio.scav))

# Combined model for all individuals
# Does kill rate vary by season, sex, kittens, or pig scavenging 
# Including kittens with random effect results in singular fit
mod1 <- glm(kg.per.day ~ season + sexF + kittens + prop.bio.scav2,
             family = Gamma, data = all_periods.20)
summary(mod1)

# Separate model for each individual. Small sample sizes
mod_SF9 <- glm(kg.per.day ~ season + kittens + prop.bio.scav2,
              data = all_periods.20[all_periods.20$AID == "SF9",],
              family = Gamma)
summary(mod_SF9)

mod_SM6 <- glm(kg.per.day ~ season + prop.bio.scav2,
               data = all_periods.20[all_periods.20$AID == "SM6",],
              family = Gamma)
summary(mod_SM6)
par(mar = c(5,5,2,2))

temp <- seq(-0.8,3,length = 100)
temp2 <- rep(c("summer","winter"),each = 100)
temp3 <- rep(0,200)
temp4 <- rep(1,200)
newdat <- data.frame(season = c(temp2,temp2),
                     kittens = c(temp3,temp4),
                     prop.bio.scav2 = c(temp,temp,temp,temp),
                     sexF = rep(1,400),
                     AID = "SF9")


center <- 0.09417825
scale <- 0.1203059
backtrans <- c(0,1,2,3)*scale + center

labellocs <- c((0 - center)/scale,(0.1 - center)/scale,(0.2 - center)/scale,
               (0.3 - center)/scale,(0.4 - center)/scale)

newdat$prediction <- predict(mod1, newdat, type = "response", se.fit = T)$fit
newdat$pred.se <- predict(mod1, newdat, type = "response", se.fit = T)$se.fit
all_periods.20 <- all_periods.20 %>% 
  mutate(plotcol = case_when(
    kittens > 0.5 ~ "#403A75FF",
    kittens < 0.5 ~ "#43BBADFF"
  ))

# par(bg = "#F8F5EE")

tiff("../Figures/PigScavengePlot.tiff",width = 5, height = 3.9,
     res = 300, compression = "lzw",units = "in")

par(mar = c(4,4,1,1))
plot(all_periods.20$kg.per.day ~ all_periods.20$prop.bio.scav2, pch = 16, cex = 1,
     xlab = "Proportion of kills scavenged", ylab = "Cougar kill rate (kg/day)",
     cex.lab = 2, cex.axis = 1, ylim = c(0,20),xaxt = "n", col = all_periods.20$plotcol)
axis(side = 1, at = labellocs, labels = c(0,0.1,0.2,0.3,0.4),cex.axis = 1)


lines(newdat$prop.bio.scav2[1:100],newdat$prediction[1:100],lwd = 3, col = "#43BBADFF")
lines(newdat$prop.bio.scav2[1:100],newdat$prediction[1:100] - newdat$pred.se[1:100],
      lwd = 2, col = "#43BBADFF",lty = "dashed")
lines(newdat$prop.bio.scav2[1:100],newdat$prediction[1:100] + newdat$pred.se[1:100],
      lwd = 2, col = "#43BBADFF",lty = "dashed")

lines(newdat$prop.bio.scav2[201:300],newdat$prediction[201:300], lwd = 3, col = "#403A75FF")
lines(newdat$prop.bio.scav2[201:300],newdat$prediction[201:300] - newdat$pred.se[201:300],
      lwd = 2, col = "#403A75FF",lty = "dashed")
lines(newdat$prop.bio.scav2[201:300],newdat$prediction[201:300] + newdat$pred.se[201:300],
      lwd = 2, col = "#403A75FF",lty = "dashed")
legend(x = "topleft", legend = c("Kittens","No kittens"), lwd = 3,
       col = c("#403A75FF","#43BBADFF"),cex = 1,bty = "n",)

dev.off()

# In all cases, estimates pigs scavenging increases kill rates
# but very high standard error and insignificant p-value

#### Group all kill data together for pig scavenging analysis ####

alldat <- rbind(SF9_combined,SF1_combined,SM6_combined)
# Create new dataframe that is just the kills where cameras were deployed
killswithcams <- alldat[alldat$cluster_id %in% visitdata$cluster_id,]

# Select needed columns and filter to clusters where prey was found
# that were large ungulate prey, and that were likely cougar kills
alldat <- alldat %>% 
  select(AID,clus_start,clus_end,lat,lon,dur,locs,radius,found,cont.mon,
         cluster_id,form,duration_h,carcass_found,species.y,sex,age,est_age,n_prey,
         confidence_cougar_kill,other_sign,scavenged,scavenged_by,kill_2,
         species.1,species_age,prey_mass,prey2_mass,total_biomass,
         pigsvisit,pigfeed,firstvisit,firstfeeding,timetodisc,timetofeed,
         is.pig,pig.scavenge,haskittens) %>% 
  filter(carcass_found == "Y") %>% 
  filter(species.y %in% c("deer","pig","elk")) %>% 
  filter(confidence_cougar_kill %in% c("probable","positive"))

# Add sex column
alldat$lionsex <- as.numeric(grepl("F",alldat$AID))
# Calculate duration based on start and end times
alldat$duration_h <- as.numeric(difftime(alldat$clus_end,alldat$clus_start,
                                         units = "hours"))
# Calculate time present as the number of locations times 3 hours (the fix rate)
alldat$time.present <- (alldat$locs-1)*3
hist(alldat$time.present)

# Add season data
alldat$month <- month(alldat$form)
alldat$season <- case_when(
  alldat$month %in% c(4,5,6,7) ~ "summer",
  alldat$month %in% c(10,11,12,1,2) ~ "winter"
)
#### Is feeding duration related to pig scavenging ####
#### while accounting for prey size and lion sex ####

# alldat <- alldat[alldat$dur < 100,]
alldat$status <- 1
# Create survival object for cox proportional hazards modeling
FeedDur <- Surv(alldat$time.present,alldat$status)

# Run cph model including biomass, sex, season, kittens, and pig scavenging
m6 <- coxph(FeedDur ~ total_biomass + lionsex + season +
              haskittens + pig.scavenge, data = alldat)
summary(m6)

## Results vary if 1. I exclude or include kills with duration > 100 hrs and 
## 2. If I use #locs * 3 as a the duration instead. But in all cases, the effect
## is positive (higher hazard rate) and p is <=0.05.

boxplot(alldat$time.present ~ alldat$pig.scavenge)

combined.scav <- alldat[alldat$total_biomass %in% c(55),]

plot(combined.scav$time.present ~ combined.scav$pig.scavenge,
     col = as.factor(combined.scav$total_biomass),pch = 19,
     ylab = "Mountain lion feeding duration",cex = 1.5,
     xlab = "Scavenged by pig",cex.lab = 1.75,cex.axis = 1.25)

#### Analyze pig scavenging behavior ####
#### Initial Analyses ####
# for unvisited/unfed on carcasses, make visit date maximum camera deployment
killswithcams$timetodisc[is.na(killswithcams$timetodisc)] <- 18
killswithcams$timetofeed[is.na(killswithcams$timetofeed)] <- 18
# create data frame of only visited kills and only fed on kills
visitedkills <- killswithcams[killswithcams$pigsvisit == 1,]
fedkills <- killswithcams[killswithcams$pigfeed == 1,]

# Create function for calculating visitation rates
propvisits <- function(x){
  sum(x)/length(x)
}
#### Visits ####
# Does time to discovery depend on species?
# proportion of kills visited
propvisits(killswithcams$pigsvisit)
tapply(killswithcams$pigsvisit,killswithcams$is.pig,propvisits)

m1 <- glm(killswithcams$pigsvisit ~ killswithcams$is.pig,
          family = binomial(link = "logit"))
summary(m1)
# no difference in proportion of kills visited

# mean time to discovery for visited kills
summary(visitedkills$timetodisc)
tapply(visitedkills$timetodisc,visitedkills$is.pig,mean)

DiscDat <- Surv(rep(0, nrow(killswithcams)), killswithcams$timetodisc,killswithcams$pigsvisit)
m2 <- coxph(DiscDat ~ killswithcams$is.pig)
summary(m2)
# No difference in time to discovery of kills

#Does feeding behavior depend on species?
propvisits(killswithcams$pigfeed)
propvisits(visitedkills$pigfeed)

tapply(killswithcams$pigfeed,killswithcams$is.pig,propvisits)
tapply(visitedkills$pigfeed,visitedkills$is.pig,propvisits)

m3 <- glm(pigfeed ~ is.pig,family = binomial(link = "logit"),
          data = visitedkills)
summary(m3)
# Pigs were less likely to scavenge at killed pigs than deer

# Does time to feed vary by species
mean(fedkills$timetofeed)
tapply(fedkills$timetofeed,fedkills$is.pig,mean)

FeedDat <- Surv(rep(0, nrow(killswithcams)), 
                killswithcams$timetofeed,killswithcams$pigfeed)
m4 <- coxph(FeedDat ~ killswithcams$is.pig)
summary(m4)
# Time to feeding did not vary by prey species.

# Is feeding duration at camera monitored kills different depending on pig scavenging

# calculate cougar stay duration based on cluster start and end
killswithcams$duration_h <- as.numeric(difftime(killswithcams$clus_end,
                                                killswithcams$clus_start,
                                         units = "hours"))

# calculate cougar time present as number of locs times 3 (fix rate)
killswithcams$time.present <- (killswithcams$locs-1)*3

# Add season data
killswithcams$month <- month(killswithcams$form)
killswithcams$season <- case_when(
  killswithcams$month %in% c(4,5,6,7) ~ "summer",
  killswithcams$month %in% c(10,11,12,1,2) ~ "winter"
)
killswithcams$lionsex <- as.numeric(grepl("F",killswithcams$AID))

# Calculate survival object based on the two different duration methods
FeedDur <- Surv(rep(0, nrow(killswithcams)),killswithcams$duration_h,rep(1,nrow(killswithcams)))
FeedDur2 <- Surv(rep(0, nrow(killswithcams)),killswithcams$time.present,rep(1,nrow(killswithcams)))

m5 <- coxph(FeedDur ~ total_biomass + pig.scavenge + lionsex + haskittens + season, 
            data = killswithcams)
summary(m5)
m6 <- coxph(FeedDur2 ~ total_biomass + pig.scavenge + lionsex + haskittens + season, 
            data = killswithcams)
summary(m6)

# Scavenging by pigs reduces how long cougars feed at individual kills
head(killswithcams)
table(killswithcams$AID)
