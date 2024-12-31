#### Load packages ####
library(amt)
library(sf)
library(tidyverse)
library(nngeo)
library(terra)
library(broom)
library(lme4)
library(AICcmodavg)


# Read in output from pig REST model
pigmod <- readRDS("pigcovsmodel_log.rds")

#read in deer model
deermod <- readRDS("deercovsmodel_log.rds")


#### RSF Setup ####
#### Read in data ####

# clusters is each investigated GPS cluster and what we found there
# Read in and filter to only current clusters where a prey item was found
# current excludes very old clusters that were investigated, dens, and day beds
clusters <- read.csv("../RawData/cougar_cluster_investigations.csv")
clusters <- clusters[clusters$type == "current",]
clusters <- clusters[clusters$carcass_found == "Y",]
# correct longitudes that were entered without the negative
clusters[clusters$carcass_lon > 0,"carcass_lon"] <- clusters[clusters$carcass_lon > 0,"carcass_lon"] * -1
# filter to the three cougars we have many kills for
clusters <- clusters[clusters$cougar_id %in% c("SF09","SF01","SM06"),]


#### Data processing ####

# create a simple feature object for plotting, creating home ranges, and 
# extracting covariates. crs 4326 is WGS 84 lat long, 32610 is UTM zone 10N

clusters <- st_as_sf(clusters, coords = c("carcass_lon", "carcass_lat"), crs = 4326)
clusters <- st_transform(clusters, 32610)

# lump prey into broad categories
clusters <- clusters %>% 
  mutate(prey_cat = case_when(
    species == "deer" ~ "Deer",
    species == "pig" ~ "Pig",
    species %in% c("coyote","bobcat","opossum") ~ "Carnivore",
    TRUE ~ "Other"
  ))

# Order prey by broad categories for plotting
clusters$prey_cat <- ordered(clusters$prey_cat,levels = c("Deer","Pig","Carnivore","Other"))

# extract x and y coordiantes as their own values for creating tracks
clusters$x <- st_coordinates(clusters$geometry)[,1]
clusters$y <- st_coordinates(clusters$geometry)[,2]

# create tracks out of the GPS data
lion_track <- track(clusters$x, clusters$y,id = clusters$cougar_id)

# Group the track by individual cougars
lion_track_group <- lion_track %>% 
  nest(data = -"id")

#### Home ranges and random points ####

# Read in MCP files for each individual cougar
SF9_hr <- read_sf("../ProcessedData/SF9_HomeRange_MCP.shp")
SF1_hr <- read_sf("../ProcessedData/SF1_HomeRange_MCP.shp")
SM6_hr <- read_sf("../ProcessedData/SM6_HomeRange_MCP.shp")

# Add a 500m buffer around each home range. This is the area from which
# we will draw random points.
SF9_hr <- SF9_hr %>%  
  sf::st_buffer(dist = 500)
SF1_hr <- SF1_hr %>%  
  sf::st_buffer(dist = 500)
SM6_hr <- SM6_hr %>%  
  sf::st_buffer(dist = 500)

# Add homes ranges to our nested data frame
lion_track_group$hrs <- list(SF9_hr,SF1_hr,SM6_hr)

# Create random points for each individual cougar
# creating 1000 random points for each actual kill
# making points within the MCP+500 of each cougar

rand <- list()
for(i in 1:nrow(lion_track_group)){
  rand[[i]] <- random_points(lion_track_group$hrs[[i]], 
                             n = nrow(lion_track_group$data[[i]])*1000,
                             presence = lion_track_group$data[[i]])
}
# add random points to the nested dataframe
lion_track_group$rand <- rand



#### Read in GIS covariates that will be used in the RSF ####
# is.land is used to mask cells that occur in the ocean to avoid unrealistic
# prediction values

is.land <- rast("../ProcessedData/is_land.tiff")
forest_dist <- rast("../ProcessedData/log_forest_dist.tiff")*is.land
grass_dist <- rast("../ProcessedData/log_grass_dist.tiff")*is.land
shrub_dist <- rast("../ProcessedData/log_shrub_dist.tiff")*is.land
riparian_dist <- rast("../ProcessedData/log_riparian_dist.tiff")*is.land
TRI <- rast("../ProcessedData/resamp_TRI.tiff")*is.land
TPI <- rast("../ProcessedData/resamp_TPI.tiff")*is.land
DEM <- rast("../ProcessedData/resamp_DEM.tiff")*is.land
Slope <- rast("../ProcessedData/resamp_Slope.tiff")*is.land
TRI_500 <- rast("../ProcessedData/TRI_500m.tif")*is.land
propforest <- rast("../ProcessedData/propforested_289cell.tif")*is.land
hunting_rast <- rast("../ProcessedData/HuntingRaster.tif")*is.land
grass_dist <- rast("../ProcessedData/log_grass_dist.tiff")*is.land


windowsize <- 5
window <- matrix(rep(1,windowsize^2),ncol = windowsize, nrow = windowsize)
TRI <- focal(x = TRI, w = window, fun = mean)

# create aspect from the DEM
# transform aspect into North-South and East-West components
aspect <- terra::terrain(DEM,"aspect")
aspect_rad <- (aspect * pi)/180
aspectNS <- cos(aspect_rad)
aspectEW <- sin(aspect_rad)

# create template raster that covers the needed area
xmin <- as.numeric(round(st_bbox(SM6_hr))[1])
ymin <- as.numeric(round(st_bbox(SM6_hr))[2])-10000
xmax <- as.numeric(round(st_bbox(SM6_hr))[3])+5000
ymax <- as.numeric(round(st_bbox(SM6_hr))[4])+5000

r <- rast(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,nrow = 30, ncol = 30)

# crop all covariates to the needed area to decrease storage size
is.land <- crop(is.land,r)
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
TRI_500 <- crop(TRI_500,r)
propforest <- crop(propforest,r)
hunting_rast <- crop(hunting_rast,r)
grass_dist <- crop(grass_dist,r)

# And finally scale all covariates to facilitate model convergence and 
# interpretation of parameters
DEM <- terra::scale(DEM)
Slope <- terra::scale(Slope)

deer.iters <- deermod$sims.list$effects[sample(1:nrow(deermod$sims.list$effects),
                                               1000,replace = T),]
pig.iters <- pigmod$sims.list$effects[sample(1:nrow(pigmod$sims.list$effects),
                                             1000,replace = T),]
rm(deermod)
rm(pigmod)
gc()


somecovs <- c(forest_dist,grass_dist,shrub_dist,
             riparian_dist,TRI,TPI,DEM,Slope,aspectNS,aspectEW)
names(somecovs) <- c("forest","grass","shrub","riparian","TRI","TPI",
                    "elev","slope","aspectNS","aspectEW")

rm(forest_dist,shrub_dist,TRI,TPI,DEM,Slope,aspectNS,aspectEW,aspect_rad,aspect)
gc()

nboot <- 1000
RSFests <- list()
RSFdeerests <- list()
RSFpigests <- list()
AIC <- matrix(NA,nrow = nboot, ncol = 3)
AICdeer <- matrix(NA,nrow = nboot, ncol = 3)
AICpigs <- matrix(NA,nrow = nboot, ncol = 3)
est.pig <- c()
est.deer <- c()

write.csv(AICdeer,"deerAICsims.csv",row.names = F)
write.csv(AICpigs,"pigAICsims.csv",row.names = F)
saveRDS(est.pig,file = "pigestsims.rds")
saveRDS(est.deer,file = "deerestsims.rds")
saveRDS(RSFdeerests,file = "RSFdeerests.rds")
saveRDS(RSFpigests,file = "RSFpigests.rds")

AICdeer <- read.csv("deerAICsims.csv")
AICpigs <- read.csv("pigAICsims.csv")
est.pig <- read_rds("pigestsims.rds")
est.deer <- read_rds("deerestsims.rds")
RSFdeerests <- readRDS("RSFdeerests.rds")
RSFpigests <- read_rds("RSFpigests.rds")

start<-Sys.time()
for(n in 901:1000){
  
  deer <- terra::scale(exp(deer.iters[n,1] + 
                             deer.iters[n,2]*riparian_dist + 
                             deer.iters[n,3]*propforest +
                             deer.iters[n,6]*hunting_rast))
  
  pig <- terra::scale(exp(pig.iters[n,1] + 
                            pig.iters[n,2]*riparian_dist + 
                            pig.iters[n,3]*grass_dist +
                            pig.iters[n,6]*hunting_rast+
                            pig.iters[n,10]*TRI_500))
  
  # create a raster stack to extract covariates from and rename to short
  # yet effective names
  allcovs <- c(deer,pig,somecovs)
  names(allcovs) <- c("deer","pig","forest","grass","shrub","riparian","TRI","TPI",
                      "elev","slope","aspectNS","aspectEW")
  
  # Finally, extract covariate values for each used and available location
  lion_track_group <- lion_track_group %>% 
    mutate(covs = lapply(rand,extract_covariates,allcovs))
  
  rm(deer,pig,allcovs)
  gc()
  
  #### Fit RSF for moutnain lion kill sites
  # this fits an individual model for each individual cougar
  lion_track_group <- lion_track_group %>% 
    # mutate(rsf = lapply(covs, fit_rsf, case_ ~ forest + 
    #                       riparian + 
    #                       shrub +
    #                       TRI +
    #                       aspectNS*aspectEW))%>% 
    mutate(rsf_deer = lapply(covs, fit_rsf, case_ ~ forest + 
                               riparian + 
                               shrub +
                               TRI +
                               aspectNS*aspectEW +
                               deer)) %>% 
    mutate(rsf_pig = lapply(covs, fit_rsf, case_ ~ forest +
                              riparian +
                              shrub +
                              TRI +
                              aspectNS*aspectEW +
                              pig))
  
  # extract AICc values for the fit models
  # aic.models <- lion_track_group %>% 
  #   # This will create a list column
  #   mutate(aics = lapply(rsf, function(each_rsf) {
  #     res <- AICc(each_rsf$model)
  #     return(res)
  #   })) %>% 
  #   dplyr::select(aics)
  aic.modelsdeer <- lion_track_group %>% 
    # This will create a list column
    mutate(aics = lapply(rsf_deer, function(each_rsf) {
      res <- AICc(each_rsf$model)
      return(res)
    })) %>% 
    dplyr::select(aics)
  aic.modelspig <- lion_track_group %>%
    # This will create a list column
    mutate(aics = lapply(rsf_pig, function(each_rsf) {
      res <- AICc(each_rsf$model)
      return(res)
    })) %>%
    dplyr::select(aics)
  
  # AIC[n,] <- as.numeric(unlist(aic.models))
  AICdeer[n,] <- as.numeric(unlist(aic.modelsdeer))
  AICpigs[n,] <- as.numeric(unlist(aic.modelspig))
  
  # extract parameter estimates for each cougar's rsf
  # uni_beta <- lion_track_group %>% 
  #   # This will create a list column
  #   mutate(betas = lapply(rsf, function(each_rsf) {
  #     res <- tidy(each_rsf$model)
  #     return(res)
  #   })) %>% 
  #   # Now we want to drop the other columns and unnest
  #   dplyr::select(id, betas) %>% 
  #   unnest(cols = betas)
  # # look at unweighted means and SEs for each parameter estimate
  # uni_beta %>% 
  #   group_by(term) %>% 
  #   summarize(mean = mean(estimate),
  #             se = sd(estimate)/sqrt(3),
  #             t = mean/se) 
  # 
  # # add inverse variance for weighting parameters
  # uni_beta <- uni_beta %>% 
  #   mutate(inv_var = 1/(std.error^2))
  # 
  # # Loop through all model terms, run a linear model on parameter estimates,
  # # and calculate weighted means, ses, and p values for each parameter
  # terms <- unique(uni_beta$term)
  # param.est <- vector()
  # se.est <- vector()
  # t.est <- vector()
  # p.est <- vector()
  # ci.95.L <- vector()
  # ci.95.U <- vector()
  # 
  # for(i in 1:length(terms)){
  #   temp <- lm(estimate ~ 1,
  #              data = uni_beta,
  #              subset = term == terms[i],
  #              weights = inv_var)
  #   param.est[i] <- round(summary(temp)$coefficients[1],3)
  #   se.est[i] <- round(summary(temp)$coefficients[2],3)
  #   t.est[i] <- round(summary(temp)$coefficients[3],3)
  #   p.est[i] <- round(summary(temp)$coefficients[4],3)
  #   ci.95.L[i] <- round(confint(temp)[1],3)
  #   ci.95.U[i] <- round(confint(temp)[2],3)
  #   
  # }
  # 
  # # combine weighted estimates and confidence intervals into a data frame 
  # # and label columns
  # weighted.est <- data.frame(terms,param.est,se.est,t.est,p.est,ci.95.L,ci.95.U)
  # colnames(weighted.est) <- c("Parameter","Estimate","SE","t","p","Lower 95 CI","Upper 95 CI")
  # 
  # extract parameter estimates for each cougar's rsf with deer
  uni_beta <- lion_track_group %>% 
    # This will create a list column
    mutate(betas = lapply(rsf_deer, function(each_rsf) {
      res <- tidy(each_rsf$model)
      return(res)
    })) %>% 
    # Now we want to drop the other columns and unnest
    dplyr::select(id, betas) %>% 
    unnest(cols = betas)
  # look at unweighted means and SEs for each parameter estimate
  uni_beta %>% 
    group_by(term) %>% 
    summarize(mean = mean(estimate),
              se = sd(estimate)/sqrt(3),
              t = mean/se) 
  
  # add inverse variance for weighting parameters
  uni_beta <- uni_beta %>% 
    mutate(inv_var = 1/(std.error^2))
  
  # Loop through all model terms, run a linear model on parameter estimates,
  # and calculate weighted means, ses, and p values for each parameter
  terms <- unique(uni_beta$term)
  param.est <- vector()
  se.est <- vector()
  t.est <- vector()
  p.est <- vector()
  ci.95.L <- vector()
  ci.95.U <- vector()
  
  for(i in 1:length(terms)){
    temp <- lm(estimate ~ 1,
               data = uni_beta,
               subset = term == terms[i],
               weights = inv_var)
    param.est[i] <- round(summary(temp)$coefficients[1],3)
    se.est[i] <- round(summary(temp)$coefficients[2],3)
    t.est[i] <- round(summary(temp)$coefficients[3],3)
    p.est[i] <- round(summary(temp)$coefficients[4],3)
    ci.95.L[i] <- round(confint(temp)[1],3)
    ci.95.U[i] <- round(confint(temp)[2],3)
    
  }
  
  # combine weighted estimates and confidence intervals into a data frame 
  # and label columns
  weighted.est_deer <- data.frame(terms,param.est,se.est,t.est,p.est,ci.95.L,ci.95.U)
  colnames(weighted.est_deer) <- c("Parameter","Estimate","SE","t","p","Lower 95 CI","Upper 95 CI")

  # extract parameter estimates for each cougar's rsf with deer
  uni_beta <- lion_track_group %>%
    # This will create a list column
    mutate(betas = lapply(rsf_pig, function(each_rsf) {
      res <- tidy(each_rsf$model)
      return(res)
    })) %>%
    # Now we want to drop the other columns and unnest
    dplyr::select(id, betas) %>%
    unnest(cols = betas)
  # look at unweighted means and SEs for each parameter estimate
  uni_beta %>%
    group_by(term) %>%
    summarize(mean = mean(estimate),
              se = sd(estimate)/sqrt(3),
              t = mean/se)

  # add inverse variance for weighting parameters
  uni_beta <- uni_beta %>%
    mutate(inv_var = 1/(std.error^2))

  # Loop through all model terms, run a linear model on parameter estimates,
  # and calculate weighted means, ses, and p values for each parameter
  terms <- unique(uni_beta$term)
  param.est <- vector()
  se.est <- vector()
  t.est <- vector()
  p.est <- vector()
  ci.95.L <- vector()
  ci.95.U <- vector()

  for(i in 1:length(terms)){
    temp <- lm(estimate ~ 1,
               data = uni_beta,
               subset = term == terms[i],
               weights = inv_var)
    param.est[i] <- round(summary(temp)$coefficients[1],3)
    se.est[i] <- round(summary(temp)$coefficients[2],3)
    t.est[i] <- round(summary(temp)$coefficients[3],3)
    p.est[i] <- round(summary(temp)$coefficients[4],3)
    ci.95.L[i] <- round(confint(temp)[1],3)
    ci.95.U[i] <- round(confint(temp)[2],3)

  }

  # combine weighted estimates and confidence intervals into a data frame
  # and label columns
  weighted.est_pig <- data.frame(terms,param.est,se.est,t.est,p.est,ci.95.L,ci.95.U)
  colnames(weighted.est_pig) <- c("Parameter","Estimate","SE","t","p","Lower 95 CI","Upper 95 CI")

  
  # RSFests[[n]] <- weighted.est
  RSFdeerests[[n]] <- weighted.est_deer
  RSFpigests[[n]] <- weighted.est_pig
  est.pig[n] <- weighted.est_pig$Estimate[8]
  est.deer[n] <- weighted.est_deer$Estimate[8]
  if(n%%1 == 0){print(n)}
  gc()
}
end <- Sys.time()


x <- c()
for(i in 1:length(RSFdeerests)){
  x[i] <- RSFdeerests[[i]]$`Lower 95 CI`[8]
}
hist(x)

weighted.est
weighted.est_deer
weighted.est_pig


effectstoplot <- weighted.est %>% 
  filter(Parameter %in% c("forest", "riparian",
                          "shrub","TRI",
                          "aspectNS","aspectEW",
                          "aspectNS:aspectEW"))

effectstoplot <- rbind(effectstoplot, weighted.est_deer[8,], weighted.est_pig[8,])




effectstoplot <- effectstoplot[c(9,8,7,5,6,4,3,2,1),]
par(mar = c(5,10,2,2))
plot(y = c(1,2,5:11), x = effectstoplot$Estimate, pch = 16,
     xlim = c(-1.2, 1.2),cex.lab = 1.75, cex.axis = 1.15,
     xlab = "Parameter estimate", yaxt = "n",
     bty = "L", ylab = "",ylim = c(0,12))
arrows(y0 = c(1,2,5:11), x0 = effectstoplot$`Lower 95 CI`,
       y1 = c(1,2,5:11), x1 = effectstoplot$`Upper 95 CI`,
       code = 3, angle = 90, length = 0.1, lwd = 1.25,
       col = rep(c("black"),each = 9))
axis(side = 2, labels = FALSE, at = c(1,2,5:11))
labtext <- c("pig density","deer density","aspect_int","aspect_NS","aspect_EW","TRI","shrub","riparian","forest")
text(x = -1.35, y = c(1,2,5:11), par("usr")[3] - 0.2, 
     labels = labtext, srt = 0, pos = 2, xpd = TRUE, cex = 1.15)
title(ylab = "Covariate", line = 6.5, cex.lab = 1.75)
abline(v = 0, lwd = 1.5, lty = 1)



allpoints <- rbind(lion_track_group$covs[[1]],
                   lion_track_group$covs[[2]],
                   lion_track_group$covs[[3]])

newdat <- data.frame(forest = mean(allpoints$forest,na.rm = T),
                     riparian = mean(allpoints$riparian,na.rm = T),
                     shrub = mean(allpoints$shrub,na.rm = T),
                     TRI = mean(allpoints$TRI,na.rm = T),
                     aspectEW = mean(allpoints$aspectEW,na.rm = T),
                     aspectNS = mean(allpoints$aspectNS,na.rm = T),
                     pig = -1*pigmean/pigsd,
                     deer = -1*deermean/deersd)

newdat_deer <- data.frame(forest = mean(allpoints$forest,na.rm = T),
                          riparian = mean(allpoints$riparian,na.rm = T),
                          shrub = mean(allpoints$shrub,na.rm = T),
                          TRI = mean(allpoints$TRI,na.rm = T),
                          aspectEW = mean(allpoints$aspectEW,na.rm = T),
                          aspectNS = mean(allpoints$aspectNS,na.rm = T),
                          pig = -1*pigmean/pigsd,
                          deer = seq(-1*deermean/deersd,
                                     max(allpoints$deer),
                                     length.out = 100))

newdat_pig <- data.frame(forest = mean(allpoints$forest,na.rm = T),
                         riparian = mean(allpoints$riparian,na.rm = T),
                         shrub = mean(allpoints$shrub,na.rm = T),
                         TRI = mean(allpoints$TRI,na.rm = T),
                         aspectEW = mean(allpoints$aspectEW,na.rm = T),
                         aspectNS = mean(allpoints$aspectNS,na.rm = T),
                         pig = seq(-1*pigmean/pigsd,
                                   max(allpoints$pig),
                                   length.out = 100),
                         deer = -1*deermean/deersd)

rssdat_deer <- newdat_deer[1,] - newdat
rssdat_pig <- newdat_pig[1,] - newdat

for(i in 2:nrow(newdat_deer)){
  temp <- newdat_deer[i,] - newdat
  rssdat_deer <- rbind(rssdat_deer,temp)
  
  temp2 <- newdat_pig[i,] - newdat
  rssdat_pig <- rbind(rssdat_pig,temp2)
}

rel_sel_strength_deer <- exp(weighted.est_deer$Estimate[8] * rssdat_deer$deer)
rel_sel_strength_deer_lwr <- exp(weighted.est_deer$`Lower 95 CI`[8] * rssdat_deer$deer)
rel_sel_strength_deer_upr <- exp(weighted.est_deer$`Upper 95 CI`[8] * rssdat_deer$deer)


rel_sel_strength_pig <- exp(weighted.est_pig$Estimate[8] * rssdat_pig$pig)
rel_sel_strength_pig_lwr <- exp(weighted.est_pig$`Lower 95 CI`[8] * rssdat_pig$pig)
rel_sel_strength_pig_upr <- exp(weighted.est_pig$`Upper 95 CI`[8] * rssdat_pig$pig)


tiff("../Figures/RSSPlot_killsites.tiff",width = 9, height = 4, res = 300,
     compression = "lzw",units = "in")
par(mfrow = c(1,2))
par(mar = c(5,5,2,2))

plot(newdat_deer$deer,log(rel_sel_strength_deer),type = "l",
     lwd = 2, col = "#6CD3AD",ylim = c(-5,10),bty = "l",
     xlab = expression("Deer density (#/km" ^2 *")"),ylab = "log(Relative selection strength)",
     xaxt = "n")
lines(newdat_deer$deer,log(rel_sel_strength_deer_lwr),lwd = 1.5,lty = 2, col = "#6CD3AD")
lines(newdat_deer$deer,log(rel_sel_strength_deer_upr),lwd = 1.5,lty = 2, col = "#6CD3AD")

axis(side = 1, at = c((c(0,4,8,12,16,20)-deermean)/deersd),labels = seq(0,20,4))
text(x = -2, y = 10, label = "A")


plot(newdat_pig$pig,log(rel_sel_strength_pig),type = "l",lwd = 2, col = "#35264C",
     ylim = c(-5,10),bty = "l",
     xlab = expression("Pig density (#/km" ^2 *")"),ylab = "log(Relative selection strength)",
     xaxt = "n")
lines(newdat_pig$pig,log(rel_sel_strength_pig_lwr),lwd = 1.5,lty = 2, col = "#35264C")
lines(newdat_pig$pig,log(rel_sel_strength_pig_upr),lwd = 1.5,lty = 2, col = "#35264C")

axis(side = 1, at = c((c(0,12,24,36,48,60)-pigmean)/pigsd),labels = seq(0,60,12))
text(x = -0.5, y = 10, label = "B")


dev.off()




write.csv(weighted.est,file = "lionkillrsf.csv",row.names = F)