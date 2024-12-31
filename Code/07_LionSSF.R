#### Step selection function for cougars, including old collars ####
#### Mitchell Parsons ####
#### April 24, 2024 ####

#####################################################################

#### Load packages ####

library(amt)
library(terra)
library(tidyverse)
library(suncalc)
library(AICcmodavg)

#### Read in data ####

olddat <- read.csv("../RawData/CougarLocations/SLO Data 211130.csv")
head(olddat)

# Initial filtering of impossible locations
olddat <- olddat %>% 
  filter(Longitude < -100,
         Latitude < 37)

# Plot data to identify who was at Fort Hunter Liggett
ggplot(data = olddat, aes(x = Longitude, y = Latitude, color = ID))+
  geom_point()

# Remove animals that were south of FHL and that we have other data for
olddat <- olddat %>% 
  filter(ID %in% c('SF1','SF2','SF3','SF4','SF5','SM3','SM4'))
ggplot(data = olddat, aes(x = Longitude, y = Latitude, color = ID))+
  geom_point()

# format dates

olddat$LMT_Date <- mdy(olddat$LMT_Date)

# remove post deployment points for SM3

olddat <- olddat[-c(which(olddat$ID == "SM3" & olddat$LMT_Date > mdy("12/08/2020"))),]
olddat

olddat$dt <- mdy_hms(paste0(olddat$Month,"/",olddat$Day,"/",olddat$Year,
                            " ",olddat$Hour,":00:00"))
head(olddat)

startdays <- as.numeric(tapply(olddat$LMT_Date,olddat$ID,min))
enddays <- as.numeric(tapply(olddat$LMT_Date,olddat$ID,max))
as.Date(startdays, origin = mdy("01/01/1970"))
as.Date(enddays, origin = mdy("01/01/1970"))
mean(enddays - startdays)


##### Read in new data #####
#### Read in location data ####
# separate file for each collar from SF9 and SM6

sf9_1 <- read.csv("../RawData/CougarLocations/collar44744_SF09_02-10-2021_12-21-2022.csv",header = F)
sf9_2 <- read.csv("../RawData/CougarLocations/collar44746_SF09_12-21-2022_12-31-2023.csv",header = F)
sm6_1 <- read.csv("../RawData/CougarLocations/collar44747_SM06_02-09-2021_12-18-2022.csv",header = F)
sm6_2 <- read.csv("../RawData/CougarLocations/collar45771_SM06_12-18-2022_12-31-2023.csv",header = F)

#### Select only needed columns, rename, and change data types as needed ####
# SF9 - collar 44744
head(sf9_1)
sf9_1 <- sf9_1[-1,c(1,2,3,4,13,14,15,16,49,50)]
colnames(sf9_1) <- c("LocID","CollarID","UTC_Date","UTC_Time","Latitude","Longitude",
                     "Height","DOP","Easting","Northing")
# set needed columns as numeric
sf9_1 <- sf9_1 %>% 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude),
         Height = as.numeric(Height),
         DOP = as.numeric(DOP),
         Easting = as.numeric(Easting)-10000000,
         Northing = as.numeric(Northing))



# SF9 - collar 44746
head(sf9_2)
sf9_2 <- sf9_2[-1,c(1,2,3,4,13,14,15,16,49,50)]
colnames(sf9_2) <- c("LocID","CollarID","UTC_Date","UTC_Time","Latitude","Longitude",
                     "Height","DOP","Easting","Northing")
sf9_2 <- sf9_2 %>% 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude),
         Height = as.numeric(Height),
         DOP = as.numeric(DOP),
         Easting = as.numeric(Easting)-10000000,
         Northing = as.numeric(Northing))

# SM6 - collar 44747
head(sm6_1)
sm6_1 <- sm6_1[-1,c(1,2,3,4,13,14,15,16,49,50)]
colnames(sm6_1) <- c("LocID","CollarID","UTC_Date","UTC_Time","Latitude","Longitude",
                     "Height","DOP","Easting","Northing")
sm6_1 <- sm6_1 %>% 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude),
         Height = as.numeric(Height),
         DOP = as.numeric(DOP),
         Easting = as.numeric(Easting)-10000000,
         Northing = as.numeric(Northing))

# SM6 - collar 45771
head(sm6_2)
sm6_2 <- sm6_2[-1,c(1,2,3,4,13,14,15,16,49,50)]
colnames(sm6_2) <- c("LocID","CollarID","UTC_Date","UTC_Time","Latitude","Longitude",
                     "Height","DOP","Easting","Northing")
sm6_2 <- sm6_2 %>% 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude),
         Height = as.numeric(Height),
         DOP = as.numeric(DOP),
         Easting = as.numeric(Easting)-10000000,
         Northing = as.numeric(Northing))

#rbind sf9 and sm6 collars into single data frame for each animal
sf9_dat <- rbind(sf9_1,sf9_2)
sm6_dat <- rbind(sm6_1,sm6_2)
rm(sf9_1,sf9_2,sm6_1,sm6_2)

# repeat for sf1 who only had 1 collar
# SF1 - collar 44748
sf1_dat <- read.csv("../RawData/CougarLocations/collar44748_SF1_02-05-2021_11-26-2021.csv",header = F)
sf1_dat <- sf1_dat[-1,c(1,2,3,4,13,14,15,16,49,50)]
colnames(sf1_dat) <- c("LocID","CollarID","UTC_Date","UTC_Time","Latitude","Longitude",
                       "Height","DOP","Easting","Northing")
sf1_dat <- sf1_dat %>% 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude),
         Height = as.numeric(Height),
         DOP = as.numeric(DOP),
         Easting = as.numeric(Easting)-10000000,
         Northing = as.numeric(Northing))

# add animal ID column to each data frame
sf1_dat$AID <- "SF1"
sf9_dat$AID <- "SF9"
sm6_dat$AID <- "SM6"

# combine all locations into a single data frame
locs <- rbind(sf1_dat,sf9_dat,sm6_dat)


# set date and time and create time stamp
locs$Date <- mdy(locs$UTC_Date)
locs$Time <- hms(locs$UTC_Time)
head(locs)
locs$dt <- mdy_hms(paste(locs$UTC_Date,locs$UTC_Time)) - hours(7)

head(locs)

# reseparate into individual columns because I forgot to do something before
# filter to only those locations that occurred while the collar was deployed
# do this for each collar, then recombine
collar44748 <- locs[locs$CollarID == 44748,]
collar44748 <- collar44748[collar44748$dt > mdy("02/06/2021") & 
                             collar44748$dt < mdy("11/24/2021"),]

collar44744 <- locs[locs$CollarID == 44744,]
collar44744 <- collar44744[collar44744$dt > mdy("2/11/2021") & 
                             collar44744$dt < mdy("12/20/2022"),]

collar44746 <- locs[locs$CollarID == 44746,]
collar44746 <- collar44746[collar44746$dt > mdy("12/22/2022") & 
                             collar44746$dt < mdy("1/1/2024"),]

collar44747 <- locs[locs$CollarID == 44747,]
collar44747 <- collar44747[collar44747$dt > mdy("2/10/2021") & 
                             collar44747$dt < mdy("12/17/2022"),]

collar45771 <- locs[locs$CollarID == 45771,]
collar45771 <- collar45771[collar45771$dt > mdy("12/19/2022") & 
                             collar45771$dt < mdy("1/1/2024"),]

locs <- rbind(collar44748,
              collar44744,
              collar44746,
              collar44747,
              collar45771)
# remove missed fixes
locs <- locs[!is.na(locs$Latitude),]
head(locs)
head(olddat)

startdays <- as.numeric(tapply(locs$Date,locs$AID,min))
enddays <- as.numeric(tapply(locs$Date,locs$AID,max))
as.Date(startdays, origin = mdy("01/01/1970"))
as.Date(enddays, origin = mdy("01/01/1970"))
mean(enddays - startdays)
enddays - startdays

#### Combine data from old and new collars
locs <- locs %>% 
  mutate(Day = day(Date),
         Month = month(Date),
         Year = year(Date),
         Hour = hour(dt),
         Sex = as.numeric(grepl("M",AID)),
         StudyArea = "CC") %>% 
  dplyr::select(AID,Date,Latitude,Longitude,Easting,Northing,Day,
         Month,Year,Hour,Sex,StudyArea,dt)

colnames(olddat) <- colnames(locs)

alldat <- rbind(locs,olddat)


tapply(alldat$Date,alldat$AID,summary)

alldat$dt <- force_tz(alldat$dt,tzone = "America/Los_Angeles")
dsterrors <- which(is.na(alldat$dt))

alldat$dt[alldat$Date == mdy("03/08/2020") & alldat$Hour == 2] <- mdy_hms("03/08/2020 10:00:00")
alldat$dt[alldat$Date == mdy("03/10/2019") & alldat$Hour == 2] <- mdy_hms("03/10/2019 10:00:00")
alldat$dt[alldat$Date == mdy("03/14/2021") & alldat$Hour == 2] <- mdy_hms("03/14/2021 10:00:00")

alldat[dsterrors,]
head(alldat)
#### Read in GIS data ####

# is.land is used to mask cells that occur in the ocean to avoid unrealistic
# prediction values

is.land <- rast("../ProcessedData/is_land.tiff")
deer <- rast("../ProcessedData/DeerDensityPred_average.tif")*is.land
pig <- rast("../ProcessedData/PigDensityPred_average.tif")*is.land
forest_dist <- rast("../ProcessedData/log_forest_dist.tiff")*is.land
grass_dist <- rast("../ProcessedData/log_grass_dist.tiff")*is.land
shrub_dist <- rast("../ProcessedData/log_shrub_dist.tiff")*is.land
riparian_dist <- rast("../ProcessedData/log_riparian_dist.tiff")*is.land
TRI <- rast("../ProcessedData/resamp_TRI.tiff")*is.land
TPI <- rast("../ProcessedData/resamp_TPI.tiff")*is.land
DEM <- rast("../ProcessedData/resamp_DEM.tiff")*is.land
Slope <- rast("../ProcessedData/resamp_Slope.tiff")*is.land

# create aspect from the DEM
# transform aspect into North-South and East-West components
aspect <- terra::terrain(DEM,"aspect")
aspect_rad <- (aspect * pi)/180
aspectNS <- cos(aspect_rad)
aspectEW <- sin(aspect_rad)

# create template raster that covers the needed area
xmin <- min(alldat$Easting) - 10000
ymin <- min(alldat$Northing) - 10000
xmax <- max(alldat$Easting) + 10000
ymax <- max(alldat$Northing) + 10000

r <- rast(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,nrow = 30, ncol = 30)

# crop all covariates to the needed area to decrease storage size
deer <- crop(deer,r)
pig <- crop(pig,r)
forest_dist <- crop(forest_dist,r)
grass_dist <- crop(grass_dist,r)
shrub_dist <- crop(shrub_dist,r)
riparian_dist <- crop(riparian_dist,r)
TRI <- crop(TRI,r)
TPI <- crop(TPI,r)
DEM <- crop(DEM,r)
Slope <- crop (Slope,r)
aspectNS <- crop(aspectNS,r)
aspectEW <- crop(aspectEW,r)

deermean <- mean(values(deer),na.rm = T)
pigmean <- mean(values(pig),na.rm = T)

deersd <- sd(values(deer),na.rm = T)
pigsd <- sd(values(pig),na.rm = T)
# And finally scale all covariates to facilitate model convergence and 
# interpretation of parameters
deer <- terra::scale(deer)
pig <- terra::scale(pig)
DEM <- terra::scale(DEM)
Slope <- terra::scale(Slope)

# create a raster stack to extract covariates from and rename to short
# yet effective names
allcovs <- c(deer,pig,forest_dist,grass_dist,shrub_dist,
             riparian_dist,TRI,TPI,DEM,Slope,aspectNS,aspectEW)
names(allcovs) <- c("deer","pig","forest","grass","shrub","riparian","TRI","TPI",
                    "elev","slope","aspectNS","aspectEW")


stepdist <- vector()
for(i in 1:nrow(alldat)-1){
  stepdist[i] <- sqrt((alldat$Easting[i] - alldat$Easting[i+1])^2 + 
                        (alldat$Northing[i] - alldat$Northing[i+1])^2)
}

alldat$stepdist <- c(stepdist,NA)

lion_steps_all <- alldat %>% 
  nest(data = -"AID") %>% 
  mutate(tracks = lapply(data, function(d){
    res <- make_track(d,.x = Easting, .y = Northing, .t = dt, crs = 32610)
    return(res)
  })) %>% 
  # calculate steps and correct for 0-length steps
  mutate(lion.steps = lapply(tracks, function(d){
    res <- steps(d)
    res$ta_[is.na(res$ta_)] <- 0.001
    res$sl_[res$sl_ == 0] <- 0.5
    res$direction_p[is.na(res$direction_p)] <- 0.001
    res <- res[res$dt_ < period(4, units = "hours"),]
    return(res)
  }))

lion_steps_all
allsteps <- rbind(lion_steps_all$lion.steps[[1]],
                  lion_steps_all$lion.steps[[2]],
                  lion_steps_all$lion.steps[[3]],
                  lion_steps_all$lion.steps[[4]],
                  lion_steps_all$lion.steps[[5]],
                  lion_steps_all$lion.steps[[6]],
                  lion_steps_all$lion.steps[[7]],
                  lion_steps_all$lion.steps[[8]],
                  lion_steps_all$lion.steps[[9]])

all_sl_dist <- fit_distr(allsteps$sl_ + 0.5,"gamma")
all_ta_dist <- fit_distr(allsteps$ta_[!is.na(allsteps$ta_)],"vonmises")

lion_steps <- alldat %>% 
  # nest data by pig ID
  nest(data = -"AID") %>% 
  # create tracks for each pig
  mutate(tracks = lapply(data, function(d){
    res <- make_track(d,.x = Easting, .y = Northing, .t = dt, crs = 32610)
    return(res)
  })) %>% 
  # calculate steps and correct for 0-length steps
  mutate(lion.steps = lapply(tracks, function(d){
    res <- steps(d)
    res$ta_[is.na(res$ta_)] <- 0.001
    res$sl_[res$sl_ == 0] <- 0.5
    res$direction_p[is.na(res$direction_p)] <- 0.001
    res <- res[res$dt_ < period(4, units = "hours"),]
    return(res)
  })) %>%
  # add 15 random steps for each observed step from observed distribution
  mutate(rand_stps = lapply(lion.steps,function(d){
    res <- random_steps(d, n_control = 15,
                        sl_distr = all_sl_dist,
                        ta_distr = all_ta_dist)
    res <- extract_covariates(x = res,covariates = allcovs, where = "both")%>% 
      mutate(log_sl_ = log(sl_),
             cos_ta_ = cos(ta_),
             sunrise = getSunlightTimes(date = date(t1_),
                                        lat = alldat$Latitude[1],
                                        lon = alldat$Longitude[1],
                                        tz = "America/Los_Angeles")$sunrise,
             
             sunset = getSunlightTimes(date = date(t1_),
                                       lat = alldat$Latitude[1],
                                       lon = alldat$Longitude[1],
                                       tz = "America/Los_Angeles")$sunset,
             night = case_when(
               t1_ < (sunrise + hours(1)) | t1_ > (sunset - hours(1)) ~ 1,
               TRUE ~ 0)
      ) %>% 
      filter(!is.na(pig_end)) %>% 
    return(res)
  }))



summary(lion_steps$rand_stps[[3]]$pig_end)

# View(lion_steps$rand_stps[[3]][is.na(lion_steps$rand_stps[[3]]$pig_end),])

for(i in 1:nrow(lion_steps)){
  temp <- data.frame(lion_steps$rand_stps[[i]]$deer_end,lion_steps$rand_stps[[i]]$pig_end)
  colnames(temp) <- c("deer","pig")
  print(cor(temp))
}
# temp <- data.frame(lion_steps$rand_stps[[2]]$forest_end,lion_steps$rand_stps[[2]]$shrub_end,
#           lion_steps$rand_stps[[2]]$grass_end,lion_steps$rand_stps[[2]]$riparian_end,
#           lion_steps$rand_stps[[2]]$deer_end,lion_steps$rand_stps[[2]]$pig_end)
# colnames(temp) <- c("forest","shrub","grass","riparian","deer","pig")
# cor(temp)
# 
# 
# deervals <- values(deer)[!is.na(values(deer))]
# pigvals <- values(pig)[!is.na(values(pig))]
# forestvals <- values(forest_dist)[!is.na(values(forest_dist))]
# shrubvals <- values(shrub_dist)[!is.na(values(shrub_dist))]
# grassvals <- values(grass_dist)[!is.na(values(grass_dist))]
# riparianvals <- values(riparian_dist)[!is.na(values(riparian_dist))]
# 
# temp <- data.frame(deervals,pigvals,forestvals,shrubvals,grassvals,riparianvals)
# cor(temp)

rm(pig,deer,grass_dist,forest_dist,shrub_dist,riparian_dist,TRI,TPI)
gc()

lion_steps <- lion_steps %>% 
  mutate(issf_noprey = lapply(rand_stps, function(each_ind) {
    res <- each_ind %>% 
      fit_clogit(case_ ~ forest_end +
                   riparian_end +
                   shrub_end + 
                   TRI_end + 
                   aspectEW_end*aspectNS_end +
                   forest_end:night +
                   riparian_end:night +
                   shrub_end:night + 
                   TRI_end:night + 
                   aspectEW_end:night +
                   aspectNS_end:night +
                   aspectEW_end:aspectNS_end:night +
                   sl_ + log_sl_ + cos_ta_ +
                   log_sl_:forest_start +
                   log_sl_:riparian_start +
                   log_sl_:shrub_start +
                   log_sl_:TRI_start +
                   log_sl_:aspectEW_start +
                   log_sl_:aspectNS_start +
                   log_sl_:night +
                   cos_ta_:forest_start +
                   cos_ta_:riparian_start +
                   cos_ta_:shrub_start +
                   cos_ta_:TRI_start +
                   cos_ta_:aspectEW_start +
                   cos_ta_:aspectNS_start +
                   cos_ta_:night +
                   strata(step_id_), 
                 model = TRUE)
    return(res)
  })) 

lion_steps <- lion_steps %>% 
  mutate(issf_prey = lapply(rand_stps, function(each_ind) {
    res <- each_ind %>% 
      fit_clogit(case_ ~ forest_end +
                   riparian_end +
                   shrub_end + 
                   TRI_end + 
                   aspectEW_end*aspectNS_end +
                   pig_end +
                   deer_end +
                   forest_end:night +
                   riparian_end:night +
                   shrub_end:night + 
                   TRI_end:night + 
                   aspectEW_end:night +
                   aspectNS_end:night +
                   aspectEW_end:aspectNS_end:night +
                   pig_end:night +
                   deer_end:night +
                   sl_ + log_sl_ + cos_ta_ +
                   log_sl_:forest_start +
                   log_sl_:riparian_start +
                   log_sl_:shrub_start +
                   log_sl_:TRI_start +
                   log_sl_:aspectEW_start +
                   log_sl_:aspectNS_start +
                   log_sl_:pig_start +
                   log_sl_:deer_start +
                   log_sl_:night +
                   cos_ta_:forest_start +
                   cos_ta_:riparian_start +
                   cos_ta_:shrub_start +
                   cos_ta_:TRI_start +
                   cos_ta_:aspectEW_start +
                   cos_ta_:aspectNS_start +
                   cos_ta_:pig_start +
                   cos_ta_:deer_start + 
                   cos_ta_:night +
                   strata(step_id_), 
                 model = TRUE)
    return(res)
  })) 

# lion_steps <- lion_steps %>% 
#   # fit issa without prey
#   mutate(issf_deer = lapply(rand_stps, function(each_ind) {
#     res <- each_ind %>% 
#       fit_clogit(case_ ~ forest_end +
#                    riparian_end +
#                    shrub_end + 
#                    TRI_end + 
#                    aspectEW_end*aspectNS_end +
#                    deer_end +
#                    forest_end:night +
#                    riparian_end:night +
#                    shrub_end:night + 
#                    TRI_end:night + 
#                    aspectEW_end:night +
#                    aspectNS_end:night +
#                    aspectEW_end:aspectNS_end:night +
#                    deer_end:night +
#                    sl_ + log_sl_ + cos_ta_ +
#                    log_sl_:forest_start +
#                    log_sl_:riparian_start +
#                    log_sl_:shrub_start +
#                    log_sl_:TRI_start +
#                    log_sl_:aspectEW_start +
#                    log_sl_:aspectNS_start +
#                    log_sl_:deer_start +
#                    log_sl_:night +
#                    cos_ta_:forest_start +
#                    cos_ta_:riparian_start +
#                    cos_ta_:shrub_start +
#                    cos_ta_:TRI_start +
#                    cos_ta_:aspectEW_start +
#                    cos_ta_:aspectNS_start +
#                    cos_ta_:deer_start +
#                    cos_ta_:night +
#                    strata(step_id_), 
#                  model = TRUE)
#     return(res)
#   })) 

# extract parameter estimates for each individual pig's issa
issa_df_noprey <- lapply(lion_steps$issf_noprey, function(x){
  broom::tidy(x$model)
}) %>% 
  bind_rows()

# We could easily take the mean of each beta now
issa_df_noprey %>% 
  group_by(term) %>% 
  summarize(beta = mean(estimate))

# for each term, calculate the weighted mean and standard error
# using inverse variance weighted regression
terms <- unique(issa_df_noprey$term)
coefs <- vector()
SEs <- vector()
t <- vector()
p <- vector()
CI.low <- vector()
CI.high <- vector()

for(i in 1:length(terms)){
  temp <- lm(estimate ~ 1, 
             data = issa_df_noprey,
             subset = term == terms[i],
             weights = 1/(std.error^2))
  coefs[i] <- summary(temp)$coefficients[1]
  SEs[i] <- summary(temp)$coefficients[2]
  t[i] <- summary(temp)$coefficients[3]
  p[i] <- summary(temp)$coefficients[4]
  CI.low[i] <- confint(temp)[1]
  CI.high[i] <- confint(temp)[2]
}
# combine estimates into a dataframe
mean_estimates_noprey <- data.frame(term = terms,
                                  coef = round(coefs,3),
                                  SE = round(SEs,3),
                                  t = round(t,3),
                                  p = round(p,3),
                                  ci.low = round(CI.low,3),
                                  ci.high = round(CI.high,3))
mean_estimates_noprey


# extract parameter estimates for each individual pig's issa
issa_df_prey <- lapply(lion_steps$issf_prey, function(x){
  broom::tidy(x$model)
}) %>% 
  bind_rows()

# We could easily take the mean of each beta now
issa_df_prey %>% 
  group_by(term) %>% 
  summarize(beta = mean(estimate))

# for each term, calculate the weighted mean and standard error
# using inverse variance weighted regression
terms <- unique(issa_df_prey$term)
coefs <- vector()
SEs <- vector()
t <- vector()
p <- vector()
CI.low <- vector()
CI.high <- vector()

for(i in 1:length(terms)){
  temp <- lm(estimate ~ 1, 
             data = issa_df_prey,
             subset = term == terms[i],
             weights = 1/(std.error^2))
  coefs[i] <- summary(temp)$coefficients[1]
  SEs[i] <- summary(temp)$coefficients[2]
  t[i] <- summary(temp)$coefficients[3]
  p[i] <- summary(temp)$coefficients[4]
  CI.low[i] <- confint(temp)[1]
  CI.high[i] <- confint(temp)[2]
}
# combine estimates into a dataframe
mean_estimates_prey <- data.frame(term = terms,
                                    coef = round(coefs,3),
                                    SE = round(SEs,3),
                                    t = round(t,3),
                                    p = round(p,3),
                                    ci.low = round(CI.low,3),
                                    ci.high = round(CI.high,3))
mean_estimates_prey


AIC.noprey <- vector()
for(i in 1:nrow(lion_steps)){
  temp <- lion_steps$issf_noprey[[i]]$model
  AIC.noprey[i] <- AICc(temp)
}

AIC.prey <- vector()
for(i in 1:nrow(lion_steps)){
  temp <- lion_steps$issf_prey[[i]]$model
  AIC.prey[i] <- AICc(temp)
}
AIC.noprey
AIC.prey

resultsframe <- data.frame(AID = lion_steps$AID,
                           nopreymod  = AIC.noprey,
                           preymod = AIC.prey,
                           prey.delta = AIC.noprey - AIC.prey)
resultsframe


deerscores <- issa_df_prey[issa_df_prey$term %in% c("deer_end","deer_end:night"),]
pigscores <- issa_df_prey[issa_df_prey$term %in% c("pig_end","pig_end:night"),]

deerscores[,2:5] <- round(deerscores[,2:5],4)
pigscores[,2:5] <- round(pigscores[,2:5],4)

plot(x = 1:18, y = deerscores$estimate, 
     col = as.factor(deerscores$term), pch = 16,
     ylim = c(-0.2,0.25))
abline(h = 0)
arrows(x0 = 1:18, y0 = deerscores$estimate - deerscores$std.error,
       x1 = 1:18, y1 = deerscores$estimate + deerscores$std.error,
       angle = 90, length = 0.15, code = 3)

plot(x = 1:18, y = pigscores$estimate, 
     col = as.factor(pigscores$term), pch = 16)
abline(h = 0)
arrows(x0 = 1:18, y0 = pigscores$estimate - pigscores$std.error,
       x1 = 1:18, y1 = pigscores$estimate + pigscores$std.error,
       angle = 90, length = 0.15, code = 3)

pigscoresday <- pigscores[pigscores$term == "pig_end",]
pigscoresnight <- pigscores[pigscores$term == "pig_end:night",]
deerscoresday <- deerscores[deerscores$term == "deer_end",]
deerscoresnight <- deerscores[deerscores$term == "deer_end:night",]

sum(pigscoresday$estimate > 0)
sum(pigscoresnight$estimate > 0)
sum(pigscoresday$estimate + pigscoresnight$estimate > 0)

sum(deerscoresday$estimate > 0)
sum(deerscoresnight$estimate > 0)
sum(deerscoresnight$estimate + deerscoresnight$estimate > 0)



effectstoplot <- mean_estimates_prey %>% 
  filter(term %in% c("forest_end","riparian_end",
                     "shrub_end","TRI_end",
                     "aspectEW_end","aspectNS_end",
                     "pig_end","deer_end",
                     "aspectEW_end:aspectNS_end",
                     "forest_end:night","riparian_end:night",
                     "shrub_end:night","TRI_end:night",
                     "aspectEW_end:night","aspectNS_end:night",
                     "pig_end:night","deer_end:night",
                     "aspectEW_end:aspectNS_end:night"))
effectstoplot <- effectstoplot[c(16,17,18,15,14,13,12,11,10,7,8,9,6,5,4,3,2,1),]
par(mar = c(5,10,2,2))
plot(y = 1:18, x = effectstoplot$coef, pch = 16,
     col = rep(c("black","grey65"), each = 9),
     xlim = c(-0.8, 0.4),cex.lab = 1.75, cex.axis = 1.15,
     xlab = "Parameter estimate", yaxt = "n",
     bty = "L", ylab = "")
arrows(y0 = 1:18, x0 = effectstoplot$ci.low,
       y1 = 1:18, x1 = effectstoplot$ci.high,
       code = 3, angle = 90, length = 0.1, lwd = 1.25,
       col = rep(c("black","grey65"),each = 9))
axis(side = 2, labels = FALSE, at = 1:18)
labtext <- c("pig","deer","aspect_int","aspect_NS","aspect_EW","TRI","shrub","riparian","forest")
text(x = -0.875, y = seq(1, 18, by=1), par("usr")[3] - 0.2, 
     labels = labtext, srt = 0, pos = 2, xpd = TRUE, cex = 1.15)
title(ylab = "Covariate", line = 7, cex.lab = 1.75)
abline(v = 0, lwd = 1.5, lty = 1)
legend(x = "topleft",pch = 16, col = c("grey65","black"), legend = c("Day", "Night interaction"),
       bty = "n", cex = 1.15)


allsteps <- rbind(lion_steps$rand_stps[[1]],
                  lion_steps$rand_stps[[2]],
                  lion_steps$rand_stps[[3]],
                  lion_steps$rand_stps[[4]],
                  lion_steps$rand_stps[[5]],
                  lion_steps$rand_stps[[6]],
                  lion_steps$rand_stps[[7]],
                  lion_steps$rand_stps[[8]],
                  lion_steps$rand_stps[[9]])

newdat <- data.frame(forest_end = mean(allsteps$forest_end,na.rm = T),
                     riparian_end = mean(allsteps$riparian_end,na.rm = T),
                     shrub_end = mean(allsteps$shrub_end,na.rm = T),
                     TRI_end = mean(allsteps$TRI_end,na.rm = T),
                     aspectEW_end = mean(allsteps$aspectEW_end,na.rm = T),
                     aspectNS_end = mean(allsteps$aspectNS_end,na.rm = T),
                     pig_end = -1*pigmean/pigsd,
                     deer_end = -1*deermean/deersd,
                     sl_ = mean(allsteps$sl_,na.rm = T),
                     log_sl_ = mean(allsteps$log_sl_,na.rm = T),
                     cos_ta_ = mean(allsteps$cos_ta_,na.rm = T),
                     night = 1,
                     forest_start = mean(allsteps$forest_start,na.rm = T),
                     riparian_start = mean(allsteps$riparian_start,na.rm = T),
                     shrub_start = mean(allsteps$shrub_start,na.rm = T),
                     TRI_start = mean(allsteps$TRI_start,na.rm = T),
                     aspectEW_start = mean(allsteps$aspectEW_start,na.rm = T),
                     aspectNS_start = mean(allsteps$aspectNS_start,na.rm = T),
                     pig_start = mean(allsteps$pig_start,na.rm = T),
                     deer_start = mean(allsteps$deer_start,na.rm = T)
                     
)

newdat_deer <- data.frame(forest_end = mean(allsteps$forest_end,na.rm = T),
                     riparian_end = mean(allsteps$riparian_end,na.rm = T),
                     shrub_end = mean(allsteps$shrub_end,na.rm = T),
                     TRI_end = mean(allsteps$TRI_end,na.rm = T),
                     aspectEW_end = mean(allsteps$aspectEW_end,na.rm = T),
                     aspectNS_end = mean(allsteps$aspectNS_end,na.rm = T),
                     pig_end = -1*pigmean/pigsd,
                     deer_end = seq(-1*deermean/deersd,
                                    max(allsteps$deer_end),
                                    length.out = 100),
                     sl_ = mean(allsteps$sl_,na.rm = T),
                     log_sl_ = mean(allsteps$log_sl_,na.rm = T),
                     cos_ta_ = mean(allsteps$cos_ta_,na.rm = T),
                     night = 1,
                     forest_start = mean(allsteps$forest_start,na.rm = T),
                     riparian_start = mean(allsteps$riparian_start,na.rm = T),
                     shrub_start = mean(allsteps$shrub_start,na.rm = T),
                     TRI_start = mean(allsteps$TRI_start,na.rm = T),
                     aspectEW_start = mean(allsteps$aspectEW_start,na.rm = T),
                     aspectNS_start = mean(allsteps$aspectNS_start,na.rm = T),
                     pig_start = mean(allsteps$pig_start,na.rm = T),
                     deer_start = mean(allsteps$deer_start,na.rm = T)
                     
)

newdat_pig <- data.frame(forest_end = mean(allsteps$forest_end,na.rm = T),
                          riparian_end = mean(allsteps$riparian_end,na.rm = T),
                          shrub_end = mean(allsteps$shrub_end,na.rm = T),
                          TRI_end = mean(allsteps$TRI_end,na.rm = T),
                          aspectEW_end = mean(allsteps$aspectEW_end,na.rm = T),
                          aspectNS_end = mean(allsteps$aspectNS_end,na.rm = T),
                          pig_end = seq(-1*pigmean/pigsd,
                                        max(allsteps$pig_end),
                                        length.out = 100),
                          deer_end = -1*deermean/deersd,
                          sl_ = mean(allsteps$sl_,na.rm = T),
                          log_sl_ = mean(allsteps$log_sl_,na.rm = T),
                          cos_ta_ = mean(allsteps$cos_ta_,na.rm = T),
                          night = 0,
                          forest_start = mean(allsteps$forest_start,na.rm = T),
                          riparian_start = mean(allsteps$riparian_start,na.rm = T),
                          shrub_start = mean(allsteps$shrub_start,na.rm = T),
                          TRI_start = mean(allsteps$TRI_start,na.rm = T),
                          aspectEW_start = mean(allsteps$aspectEW_start,na.rm = T),
                          aspectNS_start = mean(allsteps$aspectNS_start,na.rm = T),
                          pig_start = mean(allsteps$pig_start,na.rm = T),
                          deer_start = mean(allsteps$deer_start,na.rm = T)
                          
)

rssdat_deer <- newdat_deer[1,] - newdat
rssdat_pig <- newdat_pig[1,] - newdat

for(i in 2:nrow(newdat_deer)){
  temp <- newdat_deer[i,] - newdat
  rssdat_deer <- rbind(rssdat_deer,temp)
  
  temp2 <- newdat_pig[i,] - newdat
  rssdat_pig <- rbind(rssdat_pig,temp2)
}

rel_sel_strength_deer_day <- exp(mean_estimates_prey$coef[8] * rssdat_deer$deer_end)
rel_sel_strength_deer_day_lwr <- exp(mean_estimates_prey$ci.low[8] * rssdat_deer$deer_end)
rel_sel_strength_deer_day_upr <- exp(mean_estimates_prey$ci.high[8] * rssdat_deer$deer_end)

rel_sel_strength_deer_night <- exp(mean_estimates_prey$coef[8] * rssdat_deer$deer_end +
                          mean_estimates_prey$coef[20] * rssdat_deer$deer_end)
rel_sel_strength_deer_night_lwr <- exp(mean_estimates_prey$ci.low[8] * rssdat_deer$deer_end +
                                     mean_estimates_prey$ci.low[20] * rssdat_deer$deer_end)
rel_sel_strength_deer_night_upr <- exp(mean_estimates_prey$ci.high[8] * rssdat_deer$deer_end +
                                     mean_estimates_prey$ci.high[20] * rssdat_deer$deer_end)

rel_sel_strength_pig_day <- exp(mean_estimates_prey$coef[7] * rssdat_pig$pig_end)
rel_sel_strength_pig_day_lwr <- exp(mean_estimates_prey$ci.low[7] * rssdat_pig$pig_end)
rel_sel_strength_pig_day_upr <- exp(mean_estimates_prey$ci.high[7] * rssdat_pig$pig_end)

rel_sel_strength_pig_night <- exp(mean_estimates_prey$coef[7] * rssdat_pig$pig_end +
                                     mean_estimates_prey$coef[19] * rssdat_pig$pig_end)
rel_sel_strength_pig_night_lwr <- exp(mean_estimates_prey$ci.low[7] * rssdat_pig$pig_end +
                                    mean_estimates_prey$ci.low[19] * rssdat_pig$pig_end)
rel_sel_strength_pig_night_upr <- exp(mean_estimates_prey$ci.high[7] * rssdat_pig$pig_end +
                                    mean_estimates_prey$ci.high[19] * rssdat_pig$pig_end)

tiff("../Figures/RSSPlot.tiff",width = 9, height = 4, res = 300,
     compression = "lzw",units = "in")
par(mfrow = c(1,2))
par(mar = c(5,5,2,2))

plot(newdat_deer$deer_end,log(rel_sel_strength_deer_day),type = "l",
     lwd = 2, col = "#6CD3AD",ylim = c(-1,3),bty = "l",
     xlab = expression("Deer density (#/km" ^2 *")"),ylab = "log(Relative selection strength)",
     xaxt = "n")
lines(newdat_deer$deer_end,log(rel_sel_strength_deer_day_lwr),lwd = 1.5,lty = 2, col = "#6CD3AD")
lines(newdat_deer$deer_end,log(rel_sel_strength_deer_day_upr),lwd = 1.5,lty = 2, col = "#6CD3AD")

lines(newdat_deer$deer_end,log(rel_sel_strength_deer_night),type = "l",lwd = 2, col = "#35264C")
lines(newdat_deer$deer_end,log(rel_sel_strength_deer_night_lwr),lwd = 1.5,lty = 2, col = "#35264C")
lines(newdat_deer$deer_end,log(rel_sel_strength_deer_night_upr),lwd = 1.5,lty = 2, col = "#35264C")

axis(side = 1, at = c((c(0,4,8,12,16,20)-deermean)/deersd),labels = seq(0,20,4))
text(x = -2, y = 3, label = "(a)")

plot(newdat_pig$pig_end,log(rel_sel_strength_pig_day),type = "l",lwd = 2, col = "#6CD3AD",
     ylim = c(-5,1),bty = "l",
     xlab = expression("Pig density (#/km" ^2 *")"),ylab = "log(Relative selection strength)",
     xaxt = "n")
lines(newdat_pig$pig_end,log(rel_sel_strength_pig_day_lwr),lwd = 1.5,lty = 2, col = "#6CD3AD")
lines(newdat_pig$pig_end,log(rel_sel_strength_pig_day_upr),lwd = 1.5,lty = 2, col = "#6CD3AD")

lines(newdat_pig$pig_end,log(rel_sel_strength_pig_night),type = "l",lwd = 2, col = "#35264C")
lines(newdat_pig$pig_end,log(rel_sel_strength_pig_night_lwr),lwd = 1.5,lty = 2, col = "#35264C")
lines(newdat_pig$pig_end,log(rel_sel_strength_pig_night_upr),lwd = 1.5,lty = 2, col = "#35264C")

axis(side = 1, at = c((c(0,12,24,36,48,60)-pigmean)/pigsd),labels = seq(0,60,12))
text(x = -0.3, y = 1, label = "(b)")


legend(x = "topright",legend = c("Day","Night"),col = c("#6CD3AD","#35264C"),
       lty = 1,bty = "n",lwd = 2)
dev.off()


#### Accounting for error propogation ####

AICsims <- read.csv("ErrorPropResults/SSFAICsims.csv")

mean(c(sum(AICsims[,1] < AIC.noprey[1]),
       sum(AICsims[,2] < AIC.noprey[2]),
       sum(AICsims[,3] < AIC.noprey[3]),
       sum(AICsims[,4] < AIC.noprey[4]),
       sum(AICsims[,5] < AIC.noprey[5]),
       sum(AICsims[,6] < AIC.noprey[6]),
       sum(AICsims[,7] < AIC.noprey[7]),
       sum(AICsims[,8] < AIC.noprey[8]),
       sum(AICsims[,9] < AIC.noprey[9])))

deerdayests <- readRDS("ErrorPropResults/SSFdeerdayestsims.rds")
pigdayests <- readRDS("ErrorPropResults/SSFpigdayestsims.rds")
deernightests <- readRDS("ErrorPropResults/SSFdeernightestsims.rds")
pignightests <- readRDS("ErrorPropResults/SSFdpignightestsims.rds")

preysims <- readRDS("ErrorPropResults/SSFests.rds")

deerdaylow <- c()
deerdayhigh <- c()
pigdaylow <- c()
pigdayhigh <- c()
deernightlow <- c()
deernighthigh <- c()
pignightlow <- c()
pignighthigh <- c()

for(i in 1:1000){
  deerdaylow[i] <- preysims[[i]]$ci.low[preysims[[i]]$term == "deer_end"]  
  deerdayhigh[i] <- preysims[[i]]$ci.high[preysims[[i]]$term == "deer_end"]
  pigdaylow[i] <- preysims[[i]]$ci.low[preysims[[i]]$term == "pig_end"]  
  pigdayhigh[i] <- preysims[[i]]$ci.high[preysims[[i]]$term == "pig_end"]
  deernightlow[i] <- preysims[[i]]$ci.low[preysims[[i]]$term == "deer_end:night"]  
  deernighthigh[i] <- preysims[[i]]$ci.high[preysims[[i]]$term == "deer_end:night"]
  pignightlow[i] <- preysims[[i]]$ci.low[preysims[[i]]$term == "pig_end:night"]  
  pignighthigh[i] <- preysims[[i]]$ci.high[preysims[[i]]$term == "pig_end:night"]
}

pigdnlow <- pigdaylow + pignightlow
pigdnhigh <- pigdayhigh + pignighthigh
deerdnlow <- deerdaylow + deernightlow
deerdnhigh <- deerdayhigh + deerdayhigh

hist(deerdaylow,breaks = 50)
quantile(deerdaylow,probs = c(0.025,0.5,0.975))

hist(deerdayhigh,breaks = 50)
quantile(deerdayhigh,probs = c(0.025,0.5,0.975))

hist(deernightlow,breaks = 50)
quantile(deernightlow,probs = c(0.025,0.5,0.975))

hist(deernighthigh,breaks = 50)
quantile(deernighthigh,probs = c(0.025,0.5,0.975))

hist(deerdnlow,breaks = 50)
quantile(deerdnlow,probs = c(0.025,0.5,0.975))

hist(deernighthigh,breaks = 50)
quantile(deerdnhigh,probs = c(0.025,0.5,0.975))

hist(pigdaylow,breaks = 50)
quantile(pigdaylow,probs = c(0.025,0.5,0.975))

hist(pigdayhigh,breaks = 50)
quantile(pigdayhigh,probs = c(0.025,0.5,0.975))

hist(pignightlow,breaks = 50)
quantile(pignightlow,probs = c(0.025,0.5,0.975))

hist(pignighthigh,breaks = 50)
quantile(pignighthigh,probs = c(0.025,0.5,0.975))

hist(pigdnlow,breaks = 50)
quantile(pigdnlow,probs = c(0.025,0.5,0.975))

hist(pignighthigh,breaks = 50)
quantile(pigdnhigh,probs = c(0.025,0.5,0.975))



