###############################################################################
########### Run REST model with random effects and covariates for deer ########
################ Mitchell Parsons #############################################
################ February 23, 2024 ############################################

#### Load packages ####
library(tidyverse)
library(activity)
library(lubridate)
library(jagsUI)
library(stringr)
library(overlap)
library(circular)


#### Load data ####
# this file is the visit information for deer and pigs
# each row is a visit and provides visit lengths
data<-read.csv("../RawData/Deer_and_pig_visits.csv")

# split out only deer data and do some initial formatting of dates and times
deer <- data[data$Species == "Deer",]
deer$CameraID<-factor(deer$CameraID, levels = sort(unique(deer$CameraID)))
length(unique(deer$CameraID))
deer$Date_Time<-paste0(deer$Date, " ",deer$Time)
deer$Date_Time<-as.POSIXct(deer$Date_Time,tz="America/Los_Angeles",format = "%Y-%m-%d %H:%M:%S")
deer$Date <- ymd(deer$Date)

# split out the pig data and format dates and times.
pig <- data[data$Species == "Pig",]
pig$CameraID<-factor(pig$CameraID, levels = sort(unique(pig$CameraID)))
length(unique(pig$CameraID))
pig$Date_Time<-paste0(pig$Date, " ",pig$Time)
pig$Date_Time<-as.POSIXct(pig$Date_Time,tz="America/Los_Angeles",format = "%Y-%m-%d %H:%M:%S")
pig$Date <- ymd(pig$Date)

# Calculate proportion of time spent active based on kernel smoothing of visit times
# calculate this for each season since activity changes through the year
# for summer
dtime.summer<-deer$Date_Time[deer$season == "summer"]
dt<-2*pi*(hour(dtime.summer)*60*60+minute(dtime.summer)*60+second(dtime.summer))/(24*60*60)
model<-fitact(dt, bw = 1.5*bwcalc(dt, K = 3),reps=1)
plot(model)
actv.summer<-print(model@act[1])

# for fall
dtime.fall<-deer$Date_Time[deer$season == "fall"]
dt<-2*pi*(hour(dtime.fall)*60*60+minute(dtime.fall)*60+second(dtime.fall))/(24*60*60)
model<-fitact(dt, bw = 1.5*bwcalc(dt, K = 3),reps=1)
plot(model)
actv.fall<-print(model@act[1])

# for winter
dtime.winter<-deer$Date_Time[deer$season == "winter"]
dt<-2*pi*(hour(dtime.winter)*60*60+minute(dtime.winter)*60+second(dtime.winter))/(24*60*60)
model<-fitact(dt, bw = 1.5*bwcalc(dt, K = 3),reps=1)
plot(model)
actv.winter<-print(model@act[1])

# for spring
dtime.spring<-deer$Date_Time[deer$season == "spring"]
dt<-2*pi*(hour(dtime.spring)*60*60+minute(dtime.spring)*60+second(dtime.spring))/(24*60*60)
model<-fitact(dt, bw = 1.5*bwcalc(dt, K = 3),reps=1)
plot(model)
actv.spring<-print(model@act[1])

actv <- c(actv.summer,actv.fall,actv.winter,actv.spring)


#### This commented out section looks at activity overlap of deer and pigs ####
# 
# ind_p <- vector()
# ind_p[1] <- 1
# for(i in 2:nrow(pig)){
#   ind_p[i] <- as.numeric(difftime(pig$Date_Time[i], pig$Date_Time[i-1],units = "mins") > 60)
# }
# sum(ind_p)
# 
# ind_d <- vector()
# ind_d[1] <- 1
# for(i in 2:nrow(deer)){
#   ind_d[i] <- as.numeric(difftime(deer$Date_Time[i], deer$Date_Time[i-1],units = "mins") > 60)
# }
# sum(ind_d)
# 
# pig$Timerad <- gettime(pig$Time,format = "%H:%M:%S", scale = c("radian"))
# deer$Timerad <- gettime(deer$Time,format = "%H:%M:%S", scale = c("radian"))
# 
# pig.ind.spring <- pig[ind_p == 1 & pig$season == "spring",]
# pig.ind.summer <- pig[ind_p == 1 & pig$season == "summer",]
# pig.ind.fall <- pig[ind_p == 1 & pig$season == "fall",]
# pig.ind.winter <- pig[ind_p == 1 & pig$season == "winter",]
# 
# deer.ind.spring <- deer[ind_d == 1 & deer$season == "spring",]
# deer.ind.summer <- deer[ind_d == 1 & deer$season == "summer",]
# deer.ind.fall <- deer[ind_d == 1 & deer$season == "fall",]
# deer.ind.winter <- deer[ind_d == 1 & deer$season == "winter",]
# 
# # temporal plot
# png(filename = "../Figures/pigdeeroverlap.png",height = 8, width = 6,
#     units = "in",res = 300)
# par(mfrow = c(2,1))
# par(mar = c(5,5,2,2))
# overlapPlot(pig.ind.summer$Timerad,deer.ind.summer$Timerad, xcenter = ("midnight"),
#             linetype = c(1,2),linewidth = c(2,2),
#             linecol = c("black","gray40"),main = "",
#             cex.lab = 1.75, cex.axis = 1.25,rug = T)
# text(x = 12, y = 0.1, labels = "A",cex = 1.5)
# legend(x = "topleft", legend = c("pig", "deer"),
#        col = c("black","gray40"),lwd = 2,lty = c(1,2),cex = 1.5)
# # overlapPlot(pig.ind.fall$Timerad,deer.ind.fall$Timerad, xcenter = ("midnight"),
# #             linetype = c(1,2),linewidth = c(2,2),
# #             linecol = c("black","gray40"),main = "",
# #             cex.lab = 1.75, cex.axis = 1.25,rug = T)
# # text(x = 12, y = 0.088, labels = "B",cex = 1.5)
# overlapPlot(pig.ind.winter$Timerad,deer.ind.winter$Timerad, xcenter = ("midnight"),
#             linetype = c(1,2),linewidth = c(2,2),
#             linecol = c("black","gray40"),main = "",
#             cex.lab = 1.75, cex.axis = 1.25,rug = T)
# text(x = 12, y = 0.095, labels = "B",cex = 1.5)
# # overlapPlot(pig.ind.spring$Timerad,deer.ind.spring$Timerad, xcenter = ("midnight"),
# #             linetype = c(1,2),linewidth = c(2,2),
# #             linecol = c("black","gray40"),main = "",
# #             cex.lab = 1.75, cex.axis = 1.25,rug = T)
# # text(x = 12, y = 0.095, labels = "D",cex = 1.5)
# dev.off()
# 
# #calculate delta overlap stat and check for significant difference in temporal patterns
# overlapEst(pig.ind.summer$Timerad,deer.ind.summer$Timerad,type = "Dhat4")
# overlapEst(pig.ind.fall$Timerad,deer.ind.fall$Timerad,type = "Dhat4")
# overlapEst(pig.ind.winter$Timerad,deer.ind.winter$Timerad,type = "Dhat4")
# overlapEst(pig.ind.spring$Timerad,deer.ind.spring$Timerad,type = "Dhat4")
# 
# sum.est <- bootstrap(pig.ind.summer$Timerad, deer.ind.summer$Timerad,nb = 1000)
# mean(sum.est)
# sd(sum.est)/sqrt(sd(sum.est))
# 
# win.est <- bootstrap(pig.ind.winter$Timerad, deer.ind.winter$Timerad,nb = 1000)
# mean(win.est)
# sd(win.est)/sqrt(sd(win.est))
# 
# 
# 
# # # use Watson's test for homogeneity to test if
# # # timing of visits varied by treatment
# watson.two.test(pig.ind.summer$Timerad,deer.ind.summer$Timerad, alpha = 0.05)
# watson.two.test(pig.ind.fall$Timerad,deer.ind.fall$Timerad, alpha = 0.05)
# watson.two.test(pig.ind.winter$Timerad,deer.ind.winter$Timerad, alpha = 0.05)
# watson.two.test(pig.ind.spring$Timerad,deer.ind.spring$Timerad, alpha = 0.05)


#### Read in camera covariates ####
# each row is a camera station and its associated covariates 
covs<-read.csv("../ProcessedData/CameraCovariates_NDVI_289_log.csv")
covs <- covs[order(covs$Station.ID),]
# calculate the area monitored by each camera 
covs$area <- covs$Distance * tan(15*pi/180)*covs$Distance
# format dates and make 4/30/2023 the final date of monitoring to match photo data
covs$Set.Date <- mdy(covs$Set.Date)
covs$Pull <- mdy(covs$Pull)
covs[covs$Pull > mdy("4/30/2023"),"Pull"] <- mdy("4/30/2023")

# calculate total deployment days for camera effort.
covs$Deployment_days <- as.numeric(covs$Pull - covs$Set.Date)

# Calculate duration of any camera operation problems so effort can be adjusted
# Problem1, 2, 3, 4 are just columns denoting when problems started and ended
covs$Problem1_From <- mdy(covs$Problem1_From)
covs$Problem1_to <- mdy(covs$Problem1_to)
covs[!is.na(covs$Problem1_From) & covs$Problem1_From > mdy("4/30/2023"),"Problem1_From"] <- mdy("4/30/2023")
covs[!is.na(covs$Problem1_to) & covs$Problem1_to > mdy("4/30/2023"),"Problem1_to"] <- mdy("4/30/2023")
covs$Problem1_days <- as.numeric(covs$Problem1_to - covs$Problem1_From)
covs$Problem1_days[is.na(covs$Problem1_days)] <- 0

covs$Problem2_From <- mdy(covs$Problem2_From)
covs$Problem2_to <- mdy(covs$Problem2_to)
covs[!is.na(covs$Problem2_From) & covs$Problem2_From > mdy("4/30/2023"),"Problem2_From"] <- mdy("4/30/2023")
covs[!is.na(covs$Problem2_to) & covs$Problem2_to > mdy("4/30/2023"),"Problem2_to"] <- mdy("4/30/2023")
covs$Problem2_days <- as.numeric(covs$Problem2_to - covs$Problem2_From)
covs$Problem2_days[is.na(covs$Problem2_days)] <- 0

covs$Problem3_From <- mdy(covs$Problem3_From)
covs$Problem3_to <- mdy(covs$Problem3_to)
covs[!is.na(covs$Problem3_From) & covs$Problem3_From > mdy("4/30/2023"),"Problem3_From"] <- mdy("4/30/2023")
covs[!is.na(covs$Problem3_to) & covs$Problem3_to > mdy("4/30/2023"),"Problem3_to"] <- mdy("4/30/2023")
covs$Problem3_days <- as.numeric(covs$Problem3_to - covs$Problem3_From)
covs$Problem3_days[is.na(covs$Problem3_days)] <- 0

covs$Problem4_From <- mdy(covs$Problem4_From)
covs$Problem4_to <- mdy(covs$Problem4_to)
covs[!is.na(covs$Problem4_From) & covs$Problem4_From > mdy("4/30/2023"),"Problem4_From"] <- mdy("4/30/2023")
covs[!is.na(covs$Problem4_to) & covs$Problem4_to > mdy("4/30/2023"),"Problem4_to"] <- mdy("4/30/2023")
covs$Problem4_days <- as.numeric(covs$Problem4_to - covs$Problem4_From)
covs$Problem4_days[is.na(covs$Problem4_days)] <- 0


# Calculate survey effort for each camera. Deployment days minus any problem days
covs$effort <- covs$Deployment_days - covs$Problem1_days - covs$Problem2_days -
  covs$Problem3_days - covs$Problem4_days
covs$seasonstart <- ymd(covs$seasonstart)
covs$seasonend <- ymd(covs$seasonend)

# calcuate bioyear column that groups cameras by years relative to deer reproduction
covs$seasonyear <- paste0(covs$Season,"_",covs$Year)
covs <- covs %>% 
  mutate(bioyear = case_when(
    seasonyear %in% c("summer_2021","fall_2021","winter_2021","spring_2022") ~ 2021,
    seasonyear %in% c("summer_2022","fall_2022","winter_2022","spring_2023") ~ 2022
  ))


# Add column to deer visit data that is the unique combination of
# camera, season, and year for each visit
# this will be used to join covariates

CSY2 <- vector()
for(i in 1:nrow(deer)){
  temp <- deer[i,]
  CSY2[i] <- covs[covs$CSY == temp$CSY & temp$Date >= covs$Set.Date & temp$Date <= covs$Pull,"CSY2"]
}

deer$CSY2 <- CSY2
# set levels of factor to levels of covs so that all cameras are included even if 0 visits
deer$CSY2 <- factor(deer$CSY2, levels = levels(as.factor(covs$CSY2)))

# order data by CSY2 so everything lines up
deer <- deer[order(deer$CSY2),]
covs <- covs[order(covs$CSY2),]

# correct a couple errors in CSY2 for detections that occurred on deployment days
deer[deer$CSY2 == "C59_summer_2021_1","CSY2"] <- "C59_summer_2021"
deer <- deer[deer$CSY2 != "C37_summer_2021",]


# remove detections that occurred during problem periods 
# since these periods aren't included in effort calculations

deer_toremove <- deer[1,]

# For each camera station, identify deer detections that occurred during 
# problem periods. Then we will remove these detections since they shouldn't have
# occurred giving our effort calculation

for(i in 2:nrow(covs)){
  camset <- covs[covs$CSY2 == covs$CSY2[i],]
  temp <- deer[deer$CSY2 == camset$CSY2,]
  if(!is.na(camset$Problem1_From)){
    temp <- temp[temp$Date > camset$Problem1_From & temp$Date < camset$Problem1_to,]
  }
  if(!is.na(camset$Problem2_From)){
    temp <- temp[temp$Date > camset$Problem2_From & temp$Date < camset$Problem2_to,]
  }
  if(!is.na(camset$Problem3_From)){
    temp <- temp[temp$Date > camset$Problem3_From & temp$Date < camset$Problem3_to,]
  }
  if(!is.na(camset$Problem4_From)){
    temp <- temp[temp$Date > camset$Problem4_From & temp$Date < camset$Problem4_to,]
  }
  if(!is.na(camset$Problem1_From)){
    deer_toremove <- rbind(deer_toremove, temp)
  }
}

deer_toremove <- deer_toremove[-1,]

# remove the few observations that need to be removed.
# remove by start time since that is unique for each visit
deer <- deer[!deer$Start %in% deer_toremove$Start,]

# make season numeric so it can be used as an index for actv numbers 
covs <- covs %>% 
  mutate(season.num = case_when(
    Season == "summer" ~ 1,
    Season == "fall" ~ 2,
    Season == "winter" ~ 3,
    Season == "spring" ~ 4
  ))

#### Convert covariates to vectors for easy referencing ####
# scale numeric vectors for model convergence and interpretation 
y<-as.numeric(table(deer$CSY2))
camID <- substr(names(table(deer$CSY2)),1,3)
camID_fact <- as.numeric(factor(camID))
cameras <- as.numeric(factor(unique(camID)))
S = covs$area/1000000
eff = covs$effort*24*60*60
stay = deer$visit_length
stay[stay==0]<-1.667
Ncam = nrow(covs)
forest <- covs$PropForest
forest2 <- forest^2
shrub <- covs$PropShrub
shrub2 <- shrub^2
grass <- covs$PropGrass
grass2 <- grass^2
riparian <- covs$PropRiparian
riparian2 <- riparian^2
riparian_dist <- covs$RiparianDist
riparian_dist2 <- riparian_dist^2
elev <- as.numeric(scale(covs$Elevation))
elev2 <- elev^2
slope <- as.numeric(scale(covs$slope))
TPI <- covs$TPI
TRI <- covs$TRI
hunting <- covs$hunting
spring <- as.numeric(covs$Season == "spring")
summer <- as.numeric(covs$Season == "summer")
fall <- as.numeric(covs$Season == "fall")
winter <- as.numeric(covs$Season == "winter")
season.num <- covs$season.num
year21 <- as.numeric(covs$bioyear == 2021)
NDVI <- covs$NDVI_Proper
grass_dist <- covs$grass_dist
grass_dist2 <- grass_dist^2
shrub_dist <- covs$shrub_dist
shrub_dist2 <- shrub_dist^2
forest_dist <- covs$forest_dist
forest_dist2 <- forest_dist^2
censval=30
cens<-stay
cens[cens>censval]<-censval

# Create the data list for the rest model
datalist<-list(y=y,S=S,eff=eff,actv=actv,
               cameras=cameras,
               camID_fact = camID_fact,
               stay=stay,
               cens=cens,
               Nstay=length(stay),
               Ncam=Ncam,
               forest = forest,
               forest2 = forest2,
               grass = grass,
               grass2 = grass2,
               shrub = shrub,
               shrub2 = shrub2,
               riparian = riparian,
               riparian2 = riparian2,
               riparian_dist = riparian_dist,
               riparian_dist2 = riparian_dist2,
               grass_dist = grass_dist,
               grass_dist2 = grass_dist2,
               shrub_dist = shrub_dist,
               shrub_dist2 = shrub_dist2,
               forest_dist = forest_dist,
               forest_dist2 = forest_dist2,
               elev = elev,
               elev2 = elev2,
               slope = slope,
               TPI = TPI,
               TRI = TRI,
               hunting = hunting,
               spring = spring,
               summer = summer,
               fall = fall,
               winter = winter,
               year21 = year21,
               NDVI = NDVI,
               season.num = season.num
               )



#################################################
#### Run REST model ####

# create model text and save to file
model.file<-"covmod2.txt"
sink(model.file)
cat("model
    {
    # Estimate parameters for staying time
    for(i in 1:Nstay){
    is.censored[i] ~ dinterval(stay[i],cens[i])
    stay[i] ~ dlnorm(mean,tau)
    }
    # parameters for lognormal distribution of staying time
    mean ~ dgamma(0.001,0.001)
    tau ~ dgamma(0.001,0.001)    
    
    # calculate random effect for each camera
    # random effect is a gamma distributed multiplier for each camera
    alpha ~ dunif(0,100)
    for(j in 1:length(cameras)){
    u[j] ~ dgamma(alpha+0.01, alpha+0.01)

    }
    
    # Estimate parameters for number of visits
    for(i in 1:Ncam){
    pcy[i]~dpois(mu[i])
    #number of visits (y) is poisson distributed with mean based on mu
    y[i]~dpois(mu.y[i])
    # u is the random effect for each camera
    mu.y[i]<-mu[i]*u[camID_fact[i]]
    #Estimate mu for each camera based on area (S), effort, density (rho), active time, and staying time parameters
    log(mu[i])<-log(S[i])+log(eff[i])+log(rho[i])+log(actv[season.num[i]])-mean-1/(2*tau)
    #Estimate density based on habitat covariates
    log(rho[i])<-
    effects[1]+
    effects[2]*riparian_dist[i]+
    effects[3]*forest[i]+
    effects[4]*forest[i]*fall[i]+
    effects[5]*forest[i]*winter[i]+
    effects[6]*hunting[i]+
    effects[7]*hunting[i]*fall[i]+
    effects[8]*riparian_dist[i]*summer[i]+
    effects[9]*riparian_dist[i]*fall[i]
    
    }
    # diffuse priors for beta estimates
    effects[1] ~ dnorm(0,0.001)
    effects[2] ~ dnorm(0,0.001)
    effects[3] ~ dnorm(0,0.001)
    effects[4] ~ dnorm(0,0.001)
    effects[5] ~ dnorm(0,0.001)
    effects[6] ~ dnorm(0,0.001)
    effects[7] ~ dnorm(0,0.001)
    effects[8] ~ dnorm(0,0.001)
    effects[9] ~ dnorm(0,0.001)

    }
    ")
sink()

# set initial values 
inits<-function(){
  list(effects=rep(0,9))
}
# set parameters to monitor 
parameters<-c("effects","mean","tau","u","alpha")
# run the model
model<-jags(datalist,inits,parameters,model.file,
            n.chains=3, n.iter=500000,n.burnin=10000,n.thin=3,
            DIC=FALSE,parallel = T, n.cores = 3)
# look at model output and check convergence
model
plot(model)

# #alpha of 0.947 (0.635 - 1.330) with     effects[2]*riparian_dist[i]+
# effects[3]*grass[i]+
#   effects[4]*elev[i]+
#   effects[5]*elev2[i]+
#   effects[6]*forest[i]+
#   effects[7]*forest[i]*fall[i]+
#   effects[8]*forest[i]*winter[i]+
#   effects[9]*NDVI[i]+
#   effects[10]*hunting[i]+
#   effects[11]*hunting[i]*fall[i]+
#   effects[12]*riparian_dist[i]*summer[i]+
#   effects[13]*riparian_dist[i]*fall[i]

# no strong correlation between alpha and any remaining covariates
# tried adding shrub and TPI which didn't help
# tried changing to distance instead of proportion metrics 
# but also didn't change things
# unclear what is missing

# extract random effect values
covs$randeffs <- model$mean$u[camID_fact]

# plot random effects to see if there is spatial correlation
# plot with raster layers (from CameraCovariates.R) to see if REs correlate
# with any unused variables
par(mfrow = c(1,2))
plot(covs$xcord,covs$ycord,cex = covs$randeffs, pch = 19)
plot(covs$xcord,covs$ycord,cex = scale(covs$shrub_dist, center = F) + 0.1, pch = 19)

plot(covs$randeffs ~ covs$NDVI_Proper)
cor(covs$randeffs,covs$NDVI_Proper)
# save deer REST model fiel
saveRDS(model, file = "deercovsmodel_log.rds")


#### Read in both deer and pig models for making predictions ####

deermod <- readRDS("deercovsmodel_log.rds")
pigmod <- readRDS("pigcovsmodel.rds")

# combine deer visit information into a data frame

deercamerastay <- as.data.frame(cbind(names(tapply(deer$visit_length, deer$CSY2, sum)),
                                     as.numeric(tapply(deer$visit_length, deer$CSY2, sum))))
colnames(deercamerastay) <- c("CSY2","TotalStay")
deercamerastay$TotalStay <- as.numeric(deercamerastay$TotalStay)
deercamerastay$TotalStay[is.na(deercamerastay$TotalStay)] <- 0
deercamerastay$TotalTime <- covs$effort*60*60*24
deercamerastay$Area <- covs$area/1000000

# Use the back of the napkin rest calculation to estimate density for each camera
deercamerastay$Density <- deercamerastay$TotalStay/(deercamerastay$TotalTime * deercamerastay$Area)
deercamerastay$Density <- round(deercamerastay$Density,4)
deercamerastay$season <- covs$Season
deercamerastay$Density[is.na(deercamerastay$Density)] <- 0
deercamerastay$x <- covs$xcord
deercamerastay$y <- covs$ycord


# repeat this process for pigs
pigcamerastay <- as.data.frame(cbind(names(tapply(pig$visit_length, pig$CSY2, sum)),
                                     as.numeric(tapply(pig$visit_length, pig$CSY2, sum))))
colnames(pigcamerastay) <- c("CameraID","TotalStay")
pigcamerastay$TotalStay <- as.numeric(pigcamerastay$TotalStay)
pigcamerastay$TotalStay[is.na(pigcamerastay$TotalStay)] <- 0
pigcamerastay$TotalTime <- covs$effort*60*60*24
pigcamerastay$Area <- covs$area/1000000
pigcamerastay$Density <- pigcamerastay$TotalStay/(pigcamerastay$TotalTime * pigcamerastay$Area)
pigcamerastay$Density <- round(pigcamerastay$Density,4)
pigcamerastay$season <- covs$Season
pigcamerastay$Density[is.na(pigcamerastay$Density)] <- 0
pigcamerastay$x <- covs$xcord
pigcamerastay$y <- covs$ycord


#Best model as of 10/10/23
covs$pigpred <- exp(1.426 + 0.150*riparian_dist + -0.698*elev + 0.050*grass + 0.319*grass*spring +
                      -0.476*grass*riparian_dist + 0.533*forest + 1.172*forest*fall + -2.125*NDVI +
                      -0.528*hunting + 0.206*hunting*fall + 0.180*riparian_dist*summer)
# gives cor w/ estimated density of 0.17


covs$deerpred <- exp(3.322 + 0.111*riparian_dist + -0.569*grass + 0.253*grass*spring +
                       -0.313*grass*summer + 0.081*forest + -1.081*forest*fall +
                       -0.592*forest*winter + -2.087*NDVI + -0.474*hunting + 0.263*hunting*fall +
                       -0.115*riparian_dist*summer + -0.369*riparian_dist*fall)
#gives cor w/ estimated density of 0.12

# check correlation between REST model density estimates and raw calcualtions
cor(covs$pigpred, pigcamerastay$Density)
cor(covs$deerpred, deercamerastay$Density)


  

#### Cross Validation ####
# this section splits the data into 5 folds, runs the model based on 4 of the folds,
# and then checks how it predicts the final fold.

camID <- substr(names(table(deer$CSY2)),1,3)
camID <- unique(camID)

covs$camID_fact <- camID_fact
rand.effs <- as.vector(deermod$mean$u)

# split data into 5 folds, structured by camera ID
# each fold is missing 1/5 of the camera data
set.seed(1001)
randsamp <- sample(1:length(camID), size = length(camID))
train1 <- camID[randsamp[-c(1:12)]]
train2 <- camID[randsamp[-c(13:24)]]
train3 <- camID[randsamp[-c(25:36)]]
train4 <- camID[randsamp[-c(37:48)]]
train5 <- camID[randsamp[-c(49:59)]]

train.list <- list(train1,
                   train2,
                   train3,
                   train4,
                   train5)

outputs <- list()
cv_cors <- vector()
cv_cors_RE <- vector()

for(i in 1:length(train.list)){
  
  deer.temp <- deer[deer$CameraID %in% train.list[[i]],]
  covs.temp <- covs[covs$Station.ID %in% train.list[[i]],]
  deer.temp$CSY2 <- factor(deer.temp$CSY2, levels(as.factor(covs.temp$CSY2)))
  
  # extract covariates for the 4 included folds
  y.temp<-as.numeric(table(deer.temp$CSY2))
  camID.temp <- substr(names(table(deer.temp$CSY2)),1,3)
  camID_fact.temp <- as.numeric(factor(camID.temp))
  cameras.temp <- as.numeric(factor(unique(camID.temp)))
  S.temp = covs.temp$area/1000000
  eff.temp = covs.temp$effort*24*60*60
  stay.temp = deer.temp$visit_length
  stay.temp[stay.temp==0]<-1.3
  Ncam.temp = nrow(covs.temp)
  forest.temp <- covs.temp$PropForest
  forest2.temp <- forest.temp^2
  shrub.temp <- covs.temp$PropShrub
  shrub2.temp <- shrub.temp^2
  grass.temp <- covs.temp$PropGrass
  grass2.temp <- grass.temp^2
  riparian.temp <- covs.temp$PropRiparian
  riparian2.temp <- riparian.temp^2
  riparian_dist.temp <- covs.temp$RiparianDist
  riparian_dist2.temp <- riparian_dist.temp^2
  elev.temp <- as.numeric(scale(covs.temp$Elevation))
  elev2.temp <- elev.temp^2
  hunting.temp <- covs.temp$hunting
  spring.temp <- as.numeric(covs.temp$Season == "spring")
  summer.temp <- as.numeric(covs.temp$Season == "summer")
  fall.temp <- as.numeric(covs.temp$Season == "fall")
  winter.temp <- as.numeric(covs.temp$Season == "winter")
  season.num.temp <- covs.temp$season.num
  year21.temp <- as.numeric(covs.temp$bioyear == 2021)
  NDVI.temp <- as.numeric(covs.temp$NDVI_Proper)
  censval.temp =30
  cens.temp <- stay.temp
  cens.temp[cens.temp>censval.temp] <- censval.temp
  
  
  datalist1<-list(y=y.temp,
                  cameras=cameras.temp,
                  camID_fact = camID_fact.temp,
                  S=S.temp,
                  eff=eff.temp,
                  actv=actv,
                  stay=stay.temp,
                  cens=cens.temp,
                  Nstay=length(stay.temp),
                  Ncam=Ncam.temp,
                  forest = forest.temp,
                  forest2 = forest2.temp,
                  grass = grass.temp,
                  grass2 = grass2.temp,
                  shrub = shrub.temp,
                  shrub2 = shrub2.temp,
                  riparian = riparian.temp,
                  riparian2 = riparian2.temp,
                  riparian_dist = riparian_dist.temp,
                  riparian_dist2 = riparian_dist2.temp,
                  elev = elev.temp,
                  elev2 = elev2.temp,
                  hunting = hunting.temp,
                  spring = spring.temp,
                  summer = summer.temp,
                  fall = fall.temp,
                  winter = winter.temp,
                  year21 = year21.temp,
                  NDVI = NDVI.temp,
                  season.num = season.num.temp
  )
  
  inits<-function(){
    list(effects=rep(0,9))
  }
  parameters<-c("effects","mean","tau")
# run the model for the 4 included folds
  model_cv1 <- jags(datalist1,inits,parameters,model.file,
                    n.chains=3, n.iter=100000,n.burnin=10000,n.thin=10,DIC=FALSE,
                    parallel = T, n.cores = 3)
  
  outputs[[i]] <- model_cv1
# predict deer density based on those 4 folds
  covs$deerpred <- exp(model_cv1$mean$effects[1] +
                         model_cv1$mean$effects[2]*riparian_dist +
                         model_cv1$mean$effects[3]*forest +
                         model_cv1$mean$effects[4]*forest*fall +
                         model_cv1$mean$effects[5]*forest*winter +
                         model_cv1$mean$effects[6]*hunting +
                         model_cv1$mean$effects[7]*hunting*fall +
                         model_cv1$mean$effects[8]*riparian_dist*summer +
                         model_cv1$mean$effects[9]*riparian_dist*fall)
  # predict number of visits based on those 4 folds
  covs$visits.pred <- exp(log(covs$area) +
                            log(covs$effort) +
                            log(covs$deerpred) +
                            log(actv[covs$season.num]) -
                            model_cv1$mean$mean - 1/(2*model_cv1$mean$tau))
  
  covs$visits.pred.RE <- covs$visits.pred * rand.effs[covs$camID_fact]
# check correlation between predicted number of visits and actual visits for
  # testing data
  cv_cors[i] <- cor(covs$visits.pred[!covs$Station.ID %in% train.list[[i]]],
                    y[!covs$Station.ID %in% train.list[[i]]])
  
  cv_cors_RE[i] <- cor(covs$visits.pred.RE[!covs$Station.ID %in% train.list[[i]]],
                    y[!covs$Station.ID %in% train.list[[i]]])

}

cbind(outputs[[1]]$mean$effects,outputs[[1]]$overlap0$effects,
      outputs[[2]]$mean$effects,outputs[[2]]$overlap0$effects,
      outputs[[3]]$mean$effects,outputs[[3]]$overlap0$effects,
      outputs[[4]]$mean$effects,outputs[[4]]$overlap0$effects,
      outputs[[5]]$mean$effects,outputs[[5]]$overlap0$effects)

cv_cors
mean(cv_cors)
cv_cors_RE
mean(cv_cors_RE)

saveRDS(outputs, file = "deer_cross_log.RDS")

deercross <- readRDS("deer_cross.RDS")

for(i in 1:5){
  model_cv1 <- deercross[[i]]
  covs$deerpred <- exp(model_cv1$mean$effects[1] +
                         model_cv1$mean$effects[2]*riparian_dist +
                         model_cv1$mean$effects[3]*forest +
                         model_cv1$mean$effects[4]*forest*fall +
                         model_cv1$mean$effects[5]*forest*winter +
                         model_cv1$mean$effects[6]*hunting +
                         model_cv1$mean$effects[7]*hunting*fall +
                         model_cv1$mean$effects[8]*riparian_dist*summer +
                         model_cv1$mean$effects[9]*riparian_dist*fall)
  # predict number of visits based on those 4 folds
  covs$visits.pred <- exp(log(covs$area) +
                            log(covs$effort) +
                            log(covs$deerpred) +
                            log(actv[covs$season.num]) -
                            model_cv1$mean$mean - 1/(2*model_cv1$mean$tau))
  
  covs$visits.pred.RE <- covs$visits.pred * rand.effs[covs$camID_fact]
  # check correlation between predicted number of visits and actual visits for
  # testing data
  cv_cors[i] <- cor(covs$visits.pred[!covs$Station.ID %in% train.list[[i]]],
                    y[!covs$Station.ID %in% train.list[[i]]])
  
  cv_cors_RE[i] <- cor(covs$visits.pred.RE[!covs$Station.ID %in% train.list[[i]]],
                       y[!covs$Station.ID %in% train.list[[i]]])
}
cv_cors
cv_cors_RE
