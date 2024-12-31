
#### With Covariates ####

library(tidyverse)
library(activity)
library(lubridate)
library(jagsUI)
library(stringr)
library(overlap)
library(circular)

data<-read.csv("../RawData/Deer_and_pig_visits.csv")
deer <- data[data$Species == "Deer",]
deer$CameraID<-factor(deer$CameraID, levels = sort(unique(deer$CameraID)))
length(unique(deer$CameraID))
deer$Date_Time<-paste0(deer$Date, " ",deer$Time)
deer$Date_Time<-as.POSIXct(deer$Date_Time,tz="America/Los_Angeles",format = "%Y-%m-%d %H:%M:%S")
deer$Date <- ymd(deer$Date)


pig <- data[data$Species == "Pig",]
pig$CameraID<-factor(pig$CameraID, levels = sort(unique(pig$CameraID)))
length(unique(pig$CameraID))
pig$Date_Time<-paste0(pig$Date, " ",pig$Time)
pig$Date_Time<-as.POSIXct(pig$Date_Time,tz="America/Los_Angeles",format = "%Y-%m-%d %H:%M:%S")
pig$Date <- ymd(pig$Date)


dtime.summer<-pig$Date_Time[pig$season == "summer"]
dt<-2*pi*(hour(dtime.summer)*60*60+minute(dtime.summer)*60+second(dtime.summer))/(24*60*60)
model<-fitact(dt, bw = 1.5*bwcalc(dt, K = 3),reps=1)
plot(model)
actv.summer<-print(model@act[1])

dtime.fall<-pig$Date_Time[pig$season == "fall"]
dt<-2*pi*(hour(dtime.fall)*60*60+minute(dtime.fall)*60+second(dtime.fall))/(24*60*60)
model<-fitact(dt, bw = 1.5*bwcalc(dt, K = 3),reps=1)
plot(model)
actv.fall<-print(model@act[1])

dtime.winter<-pig$Date_Time[pig$season == "winter"]
dt<-2*pi*(hour(dtime.winter)*60*60+minute(dtime.winter)*60+second(dtime.winter))/(24*60*60)
model<-fitact(dt, bw = 1.5*bwcalc(dt, K = 3),reps=1)
plot(model)
actv.winter<-print(model@act[1])

dtime.spring<-pig$Date_Time[pig$season == "spring"]
dt<-2*pi*(hour(dtime.spring)*60*60+minute(dtime.spring)*60+second(dtime.spring))/(24*60*60)
model<-fitact(dt, bw = 1.5*bwcalc(dt, K = 3),reps=1)
plot(model)
actv.spring<-print(model@act[1])

actv <- c(actv.summer,actv.fall,actv.winter,actv.spring)

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
# pig.ind <- pig[ind_p == 1,]
# deer.ind <- deer[ind_d == 1,]
# 
# pig.ind$Timerad <- gettime(pig.ind$Time,format = "%H:%M:%S", scale = c("radian"))
# deer.ind$Timerad <- gettime(deer.ind$Time,format = "%H:%M:%S", scale = c("radian"))
# 
# # temporal plot
# par(mar = c(5,5,2,2))
# overlapPlot(pig.ind$Timerad,deer.ind$Timerad, xcenter = ("midnight"), 
#             linetype = c(1,2),linewidth = c(2,2), 
#             linecol = c("black","gray40"),main = "",
#             cex.lab = 1.75, cex.axis = 1.25,rug = T)
# legend(x = "topleft", legend = c("pig", "deer"),
#        col = c("black","gray40"),lwd = 2,lty = c(1,2),cex = 1.5)
# 
# #calculate delta overlap stat and check for significant difference in temporal patterns
# overlapEst(pig.ind$Timerad,deer.ind$Timerad,type = "Dhat4")
# 
# # # use Watson's test for homogeneity to test if 
# # # timing of visits varied by treatment
# # watson.two.test(pig.ind$Timerad,deer.ind$Timerad, alpha = 0.05)
# 


covs<-read.csv("../ProcessedData/CameraCovariates_NDVI_289_log.csv")
covs <- covs[order(covs$Station.ID),]
covs$area <- covs$Distance * tan(15*pi/180)*covs$Distance
covs$Set.Date <- mdy(covs$Set.Date)
covs$Pull <- mdy(covs$Pull)
covs[covs$Pull > mdy("4/30/2023"),"Pull"] <- mdy("4/30/2023")

covs$Deployment_days <- as.numeric(covs$Pull - covs$Set.Date)

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

covs$effort <- covs$Deployment_days - covs$Problem1_days - covs$Problem2_days -
  covs$Problem3_days - covs$Problem4_days
covs$seasonstart <- ymd(covs$seasonstart)
covs$seasonend <- ymd(covs$seasonend)

covs$seasonyear <- paste0(covs$Season,"_",covs$Year)
covs <- covs %>% 
  mutate(bioyear = case_when(
    seasonyear %in% c("summer_2021","fall_2021","winter_2021","spring_2022") ~ 2021,
    seasonyear %in% c("summer_2022","fall_2022","winter_2022","spring_2023") ~ 2022
  ))


# pig$cameranumeric <- as.numeric(str_sub(pig$CameraID,2,3))
# pig$even <- pig$cameranumeric %% 2 == 0
# 
# pig[pig$even == TRUE & grepl("winter_2022",pig$CSY),"CSY"] <- 
#   str_replace(pig[pig$even == TRUE & grepl("winter_2022",pig$CSY),"CSY"], "winter","fall")
# pig[pig$even == FALSE & grepl("winter_2021",pig$CSY),"CSY"] <-
#   str_replace(pig[pig$even == FALSE & grepl("winter_2021",pig$CSY),"CSY"], "winter","fall")


CSY2 <- vector()
for(i in 1:nrow(pig)){
  temp <- pig[i,]
  CSY2[i] <- covs[covs$CSY == temp$CSY & temp$Date >= covs$Set.Date & temp$Date <= covs$Pull,"CSY2"]
}

pig$CSY2 <- CSY2
pig$CSY2 <- factor(pig$CSY2, levels = levels(as.factor(covs$CSY2)))

pig <- pig[order(pig$CSY2),]
covs <- covs[order(covs$CSY2),]

pig[pig$CSY2 == "C59_summer_2021_1","CSY2"] <- "C59_summer_2021"
pig <- pig[pig$CSY2 != "C37_summer_2021",]


# remove detections that occurred during problem periods 
# since these periods aren't included in effort calculations

pig_toremove <- pig[1,]

for(i in 2:nrow(covs)){
  camset <- covs[covs$CSY2 == covs$CSY2[i],]
  temp <- pig[pig$CSY2 == camset$CSY2,]
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
    pig_toremove <- rbind(pig_toremove, temp)
  }
}

pig_toremove <- pig_toremove[-1,]

pig <- pig[!pig$Start %in% pig_toremove$Start,]

# pigs <- pig[pig$season == "summer",]
# covs_s <- covs[covs$Season == "summer",]
# pigs$CSY2 <- factor(pigs$CSY2,levels = levels(as.factor(covs_s$CSY2)))

covs <- covs %>% 
  mutate(season.num = case_when(
    Season == "summer" ~ 1,
    Season == "fall" ~ 2,
    Season == "winter" ~ 3,
    Season == "spring" ~ 4
  ))

covs$RiparianDist[covs$RiparianDist == 0] <- 1
covs$grass_dist[covs$grass_dist == 0] <- 1
covs$forest_dist[covs$forest_dist == 0] <- 1
covs$shrub_dist[covs$shrub_dist == 0] <- 1

y<-as.numeric(table(pig$CSY2))
camID <- substr(names(table(pig$CSY2)),1,3)
camID_fact <- as.numeric(factor(camID))
cameras <- as.numeric(factor(unique(camID)))
S = covs$area/1000000
eff = covs$effort*24*60*60
stay = pig$visit_length
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
TRI_500 <- covs$TRI_500
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
agri_dist <- covs$agri_dist
agri_dist2 <- agri_dist^2
censval=30
cens<-stay
cens[cens>censval]<-censval

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
               agri_dist = agri_dist,
               agri_dist2 = agri_dist2,
               elev = elev,
               elev2 = elev2,
               TPI = TPI,
               TRI = TRI,
               slope = slope,
               hunting = hunting,
               spring = spring,
               summer = summer,
               fall = fall,
               winter = winter,
               year21 = year21,
               NDVI = NDVI,
               season.num = season.num,
               TRI_500 = TRI_500)

#################################################
model.file<-"covmod2.txt"
sink(model.file)
cat("model
    {
    # Estimate parameters for staying time
    for(i in 1:Nstay){
    is.censored[i] ~ dinterval(stay[i],cens[i])
    stay[i] ~ dlnorm(mean,tau)
    }
    mean ~ dgamma(0.01,0.01)
    tau ~ dgamma(0.01,0.01)    
    
    alpha ~ dunif(0,100)
    # alpha2 ~ dunif(0,100)
    for(j in 1:length(cameras)){
    u[j] ~ dgamma(alpha+0.01, alpha+0.01)

    }
    
    # Estimate parameters for number of visits
    for(i in 1:Ncam){
    pcy[i]~dpois(mu[i])
    #number of visits (y) is poisson distributed with mean based on mu
    y[i]~dpois(mu.y[i])
    mu.y[i]<-mu[i]*u[camID_fact[i]]
    # # u modifies expected count for each camera. Essentially a random effect for camera
    # # that adjust expectation from habitat alone
    # u[i]~dgamma(alpha+0.1,alpha+0.1)
    # 
    #Estimate mu for each camera based on area (S), effort, density (rho), active time, and staying time parameters
    log(mu[i])<-log(S[i])+log(eff[i])+log(rho[i])+log(actv[season.num[i]])-mean-1/(2*tau)
    #Estimate density based on habitat covariates
    log(rho[i])<-
    effects[1]+
    effects[2]*riparian_dist[i]+
    effects[3]*grass_dist[i]+
    effects[4]*grass_dist[i]*fall[i]+
    effects[5]*grass_dist[i]*winter[i]+
    effects[6]*hunting[i]+
    effects[7]*hunting[i]*fall[i]+
    effects[8]*riparian_dist[i]*summer[i]+
    effects[9]*riparian_dist[i]*fall[i]+
    effects[10]*TRI_500[i]
    
    }
    # alpha~dunif(0,100)
    effects[1] ~ dnorm(0,0.1)
    effects[2] ~ dnorm(0,0.1)
    effects[3] ~ dnorm(0,0.1)
    effects[4] ~ dnorm(0,0.1)
    effects[5] ~ dnorm(0,0.1)
    effects[6] ~ dnorm(0,0.1)
    effects[7] ~ dnorm(0,0.1)
    effects[8] ~ dnorm(0,0.1)
    effects[9] ~ dnorm(0,0.1)
    effects[10] ~ dnorm(0,0.1)

    }
    ")
sink()

inits<-function(){
  list(effects=rep(0,10))
}
parameters<-c("effects","mean","tau","alpha","u")
model3<-jags(datalist,inits,parameters,model.file,
            n.chains=3, n.iter=500000,n.burnin=10000,n.thin=3,
            DIC=FALSE,parallel = T, n.cores = 3)
model3
plot(model3)
covs$randeffs <- model$mean$u[camID_fact]


covs$randeffs <- model$mean$u[camID_fact]
hist(covs$randeffs,breaks = 40)
# par(mfrow = c(1,2))
plot(DEM,xlim = c(640000,680000), ylim = c(3960000,4000000))
lines(FHLBoundUTM)
points(covs$xcord,covs$ycord,cex = covs$randeffs, pch = 19)
# plot(covs$xcord,covs$ycord,cex = scale(covs$shrub_dist, center = F) + 0.1, pch = 19)
plot(covs$xcord,covs$ycord,cex = scale(covs$randeffs, center = F) + 0.1, pch = 19)

plot(covs$randeffs ~ covs$TPI, cex = (covs$PropGrass + 0.1)*5)
plot(covs$randeffs ~ covs$grass_dist,cex = scale(covs$grass_dist,center = F) + 0.5)
abline(h = 1, lwd = 2)

cor(covs$randeffs,covs$grass_dist)
cor(covs$randeffs,covs$Elevation)


saveRDS(model3, file = "pigcovsmodel_log3.rds")
# deermod <- readRDS("deercovsmodel.rds")
pigmod <- readRDS("pigcovsmodel_log.rds")

#### Cross Validation ####

camID <- substr(names(table(pig$CSY2)),1,3)
camID <- unique(camID)

covs$camID_fact <- camID_fact
rand.effs <- as.vector(pigmod$mean$u)

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
  
  pig.temp <- pig[pig$CameraID %in% train.list[[i]],]
  covs.temp <- covs[covs$Station.ID %in% train.list[[i]],]
  pig.temp$CSY2 <- factor(pig.temp$CSY2, levels(as.factor(covs.temp$CSY2)))
  
  y.temp<-as.numeric(table(pig.temp$CSY2))
  camID.temp <- substr(names(table(pig.temp$CSY2)),1,3)
  camID_fact.temp <- as.numeric(factor(camID.temp))
  cameras.temp <- as.numeric(factor(unique(camID.temp)))
  S.temp = covs.temp$area/1000000
  eff.temp = covs.temp$effort*24*60*60
  stay.temp = pig.temp$visit_length
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
  year21.temp <- as.numeric(covs.temp$bioyear == 2021)
  NDVI.temp <- as.numeric(covs.temp$NDVI_Proper)
  censval.temp = 30
  cens.temp <- stay.temp
  cens.temp[cens.temp>censval.temp] <- censval.temp
  season.num.temp <- as.numeric(covs.temp$season.num)
  grass_dist.temp <- covs.temp$grass_dist
  TRI_500.temp <- covs.temp$TRI_500
  
  
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
                  season.num = season.num.temp,
                  grass_dist = grass_dist.temp,
                  TRI_500 = TRI_500.temp
  )
  
  inits<-function(){
    list(effects=rep(0,10))
  }
  parameters<-c("effects","mean","tau")

  model_cv1 <- jags(datalist1,inits,parameters,model.file,
                    n.chains=3, n.iter=500000,n.burnin=50000,n.thin=10,
                    DIC=FALSE,n.adapt = 10000, parallel = T, n.cores = 3)
  
  outputs[[i]] <- model_cv1

  covs$pigpred <- exp(model_cv1$mean$effects[1] +
                         model_cv1$mean$effects[2]*riparian_dist +
                         model_cv1$mean$effects[3]*grass_dist +
                         model_cv1$mean$effects[4]*grass_dist*fall +
                         model_cv1$mean$effects[5]*grass_dist*winter +
                         model_cv1$mean$effects[6]*hunting +
                         model_cv1$mean$effects[7]*hunting*fall +
                         model_cv1$mean$effects[8]*riparian_dist*summer +
                         model_cv1$mean$effects[9]*riparian_dist*fall+
                         model_cv1$mean$effects[10]*TRI_500)

  covs$visits.pred <- exp(log(covs$area) +
                            log(covs$effort) +
                            log(covs$pigpred) +
                            log(actv[covs$season.num]) -
                            model_cv1$mean$mean - 1/(2*model_cv1$mean$tau))

  covs$visits.pred.RE <- covs$visits.pred * rand.effs[covs$camID_fact]

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
cv_cors_RE  
mean(cv_cors)  
mean(cv_cors_RE)

saveRDS(outputs, file = "pig_cross_log.RDS")

pigcross <- readRDS("pig_cross_log.RDS")

for(i in 1:5){
  model_cv1 <- pigcross[[i]]
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
mean(cv_cors)
cv_cors_RE
mean(cv_cors_RE)
