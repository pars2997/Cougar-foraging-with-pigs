############## Estimate prey proportions for cougar feeding ####################
#################### Mitchell Parsons ##########################################
#################### February 23, 2024 #########################################
################################################################################

#### Load packages ####
library(tidyverse)
library(lubridate)
library(viridis)
library(ggplot2)
library(lme4)
library(ggpubr)
library(gridExtra)

##### Load data ####
#read in cougar cluster investigation data 
clusters <- read.csv("../RawData/cougar_cluster_investigations_correctedkill2_age.csv")

# select necessary columns 
# filter to current kills
# format dates
clusters <- clusters %>% 
  as_tibble() %>% 
  dplyr::select(cluster_id,cougar_id,type,n_points,radius,duration_h,form_date:inv_date,
         habitat:carcass_found,carcass_lat:est_age,n_prey,marrow,confidence_cougar_kill,
         scavenged,scavenged_by,kill_2,species.1) %>% 
  filter(type == "current") %>% 
  mutate(form_date = mdy(form_date)) %>% 
  mutate(inv_date = mdy(inv_date))

# add seasons based on cluster investigation dates
clusters <- clusters %>% 
  mutate(season = case_when(
    inv_date > mdy("04/15/2021") & inv_date < mdy("08/15/2021") ~ "summer21",
    inv_date > mdy("10/15/2021") & inv_date < mdy("02/15/2022") ~ "winter21",
    inv_date > mdy("04/30/2022") & inv_date < mdy("08/01/2022") ~ "summer22",
    inv_date > mdy("10/15/2022") & inv_date < mdy("02/15/2023") ~ "winter22",
    inv_date > mdy("04/15/2023") & inv_date < mdy("08/15/2023") ~ "summer23",
    inv_date > mdy("10/15/2023") & inv_date < mdy("2/15/2024") ~ "winter23"
    )
  ) %>% 
  mutate(prey_cat = case_when(
    species %in% c("deer") ~ "Deer",
    species == "pig" ~ "Pig",
    species == "" ~ "",
    TRUE ~ "Other"
  ))

# table of clusters investigated and kills found per season-year
tapply(clusters$carcass_found,clusters$season,table)

# Add general season column for summer and winter
clusters <- clusters %>% 
  mutate(season2 = case_when(
    grepl("summer",season) == "TRUE" ~ "Summer",
    grepl("winter",season) == "TRUE" ~ "Winter"
  ))


# add column for species_age combination
# then add prey mass for each species
# calculate total biomass of all prey at each cluster
clusters <- clusters %>% 
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
  mutate(total_biomass = (prey_mass*n_prey + prey2_mass))


#### Initial analysis of cougar diet by season ####
# summarize kill data by season
killsumm <- clusters %>% 
  filter(carcass_found == "Y") %>% 
  group_by(season2,prey_cat) %>% 
  summarise(species.biomass = sum(total_biomass, na.rm = T),
            nkills = n())

# add total count for each season
killsumm$seasontotal <- c(rep(sum(killsumm$nkills[killsumm$season2 == "Summer"]),3),
                          rep(sum(killsumm$nkills[killsumm$season2 == "Winter"]),3))
# add proportion of kills for each species for each season
killsumm$seasonpercent <- killsumm$nkills/killsumm$seasontotal
killsumm$prey_cat <- as.factor(killsumm$prey_cat)
killsumm$prey_cat <- ordered(killsumm$prey_cat,levels = c("Other","Pig","Deer"))
killsumm$total.biomass <- c(rep(sum(killsumm$species.biomass[killsumm$season2 == "Summer"]),3),
                            rep(sum(killsumm$species.biomass[killsumm$season2 == "Winter"]),3))
killsumm$biomass.prop <- killsumm$species.biomass/killsumm$total.biomass

# Bootstrap 95% confidence intervals
# nboot <- 1000
# sampsize <- nrow(clusters)
# poss_samps <- 1:sampsize
# 
# kill.prop.deer.summer <- c()
# kill.prop.pig.summer <- c()
# kill.prop.other.summer <- c()
# biomass.prop.deer.summer <- c()
# biomass.prop.pig.summer <- c()
# biomass.prop.other.summer <- c()
# kill.prop.deer.winter <- c()
# kill.prop.pig.winter <- c()
# kill.prop.other.winter <- c()
# biomass.prop.deer.winter <- c()
# biomass.prop.pig.winter <- c()
# biomass.prop.other.winter <- c()
# 
# for(i in 1:nboot){
#   samp_rows <- sample(poss_samps,sampsize,replace = T)
#   clusters.boot <- clusters[samp_rows[1],]
#   for(j in 2:sampsize){
#     clusters.boot <- rbind(clusters.boot,clusters[samp_rows[j],])
#   }
#   killsumm.boot <- clusters.boot %>% 
#     filter(carcass_found == "Y") %>% 
#     group_by(season2,prey_cat) %>% 
#     summarise(species.biomass = sum(total_biomass, na.rm = T),
#               nkills = n())
#   
#   # add total count for each season
#   killsumm.boot$seasontotal <- c(rep(sum(killsumm.boot$nkills[killsumm.boot$season2 == "Summer"]),3),
#                             rep(sum(killsumm.boot$nkills[killsumm.boot$season2 == "Winter"]),3))
#   # add proportion of kills for each species for each season
#   killsumm.boot$seasonpercent <- killsumm.boot$nkills/killsumm.boot$seasontotal
#   killsumm.boot$prey_cat <- as.factor(killsumm.boot$prey_cat)
#   killsumm.boot$prey_cat <- ordered(killsumm.boot$prey_cat,levels = c("Other","Pig","Deer"))
#   killsumm.boot$total.biomass <- c(rep(sum(killsumm.boot$species.biomass[killsumm.boot$season2 == "Summer"]),3),
#                               rep(sum(killsumm.boot$species.biomass[killsumm.boot$season2 == "Winter"]),3))
#   killsumm.boot$biomass.prop <- killsumm.boot$species.biomass/killsumm.boot$total.biomass
#   
#   kill.prop.deer.summer[i] <- killsumm.boot$seasonpercent[1]
#   kill.prop.pig.summer[i] <- killsumm.boot$seasonpercent[3]
#   kill.prop.other.summer[i] <- killsumm.boot$seasonpercent[2]
#   biomass.prop.deer.summer[i] <- killsumm.boot$biomass.prop[1]
#   biomass.prop.pig.summer[i] <- killsumm.boot$biomass.prop[3]
#   biomass.prop.other.summer[i] <- killsumm.boot$biomass.prop[2]
#   
#   kill.prop.deer.winter[i] <- killsumm.boot$seasonpercent[4]
#   kill.prop.pig.winter[i] <- killsumm.boot$seasonpercent[6]
#   kill.prop.other.winter[i] <- killsumm.boot$seasonpercent[5]
#   biomass.prop.deer.winter[i] <- killsumm.boot$biomass.prop[4]
#   biomass.prop.pig.winter[i] <- killsumm.boot$biomass.prop[6]
#   biomass.prop.other.winter[i] <- killsumm.boot$biomass.prop[5]
# }
# 
# killsumm$biomass.lcl95 <- c(quantile(biomass.prop.deer.summer,0.025),
#                             quantile(biomass.prop.other.summer,0.025),
#                             quantile(biomass.prop.pig.summer,0.025),
#                             quantile(biomass.prop.deer.winter,0.025),
#                             quantile(biomass.prop.other.winter,0.025),
#                             quantile(biomass.prop.pig.winter,0.025))
# killsumm$biomass.ucl95 <- c(quantile(biomass.prop.deer.summer,0.975),
#                             quantile(biomass.prop.other.summer,0.975),
#                             quantile(biomass.prop.pig.summer,0.975),
#                             quantile(biomass.prop.deer.winter,0.975),
#                             quantile(biomass.prop.other.winter,0.975),
#                             quantile(biomass.prop.pig.winter,0.975))
# 
# killsumm$seasonpercent.lcl95 <- c(quantile(kill.prop.deer.summer,0.025),
#                             quantile(kill.prop.other.summer,0.025),
#                             quantile(kill.prop.pig.summer,0.025),
#                             quantile(kill.prop.deer.winter,0.025),
#                             quantile(kill.prop.other.winter,0.025),
#                             quantile(kill.prop.pig.winter,0.025))
# killsumm$seasonpercent.ucl95 <- c(quantile(kill.prop.deer.summer,0.975),
#                             quantile(kill.prop.other.summer,0.975),
#                             quantile(kill.prop.pig.summer,0.975),
#                             quantile(kill.prop.deer.winter,0.975),
#                             quantile(kill.prop.other.winter,0.975),
#                             quantile(kill.prop.pig.winter,0.975))
# 
# countplot <- ggplot(data = killsumm,mapping = aes(x = prey_cat, y = seasonpercent, fill = season2))+
#   geom_bar(stat = "identity",position = position_dodge())+
#   geom_errorbar(aes(x = prey_cat, ymin = seasonpercent.lcl95,ymax = seasonpercent.ucl95),
#                 position = position_dodge(width = 0.9),width = 0.2, lwd = 0.8)+
#   theme_classic()+
#   xlab("Prey species")+
#   ylab("Proportion of kills")+
#   theme(axis.title = element_text(size = 10))+
#   theme(axis.text = element_text(size = 8))+
#   theme(legend.text = element_text(size = 8))+
#   theme(legend.title = element_text(size = 8))+
#   scale_fill_viridis_d(name = "Season ",option = "mako",direction = -1,end = 0.8,begin = 0.3)+
#   ggplot2::annotate("text", x = 0.7, y = 1, label = "(a)", size = 4)+
#   theme(legend.position = "none")+
#   ylim(0,1)
#   
# 
# # # plot prey proportions by season
# # countplot <- ggplot(data = killsumm, mapping = aes(x = season2, y = seasonpercent, fill = prey_cat))+
# #   geom_bar(stat = "identity")+
# #   theme_classic()+
# #   xlab("Season")+
# #   ylab("Proportion of kills")+
# #   theme(axis.title = element_text(size = 10))+
# #   theme(axis.text = element_text(size = 8))+
# #   theme(legend.text = element_text(size = 8))+
# #   theme(legend.title = element_text(size = 8))+
# #   # theme(legend.background = element_rect(fill = "#F8F5EE"))+
# #   # theme(plot.background = element_rect(fill = "#F8F5EE"))+
# #   # theme(panel.background = element_rect(fill = "#F8F5EE"))+
# #   scale_fill_viridis_d(name = "Prey ID",option = "mako",direction = -1,begin = 0.2)+
# #   # scale_fill_manual(name = "Prey ID", values = c("burlywood1","olivedrab3","darkolivegreen4"))+
# #   ggplot2::annotate("text", x = 0.5, y = 1, label = "(a)", size = 4)+
# #   theme(legend.position = "none")+
# #   ylim(0,1)
# 
# biomassplot <- ggplot(data = killsumm,mapping = aes(x = prey_cat, y = biomass.prop, fill = season2))+
#   geom_bar(stat = "identity",position = position_dodge())+
#   geom_errorbar(aes(x = prey_cat, ymin = biomass.lcl95,ymax = biomass.ucl95),
#                 position = position_dodge(width = 0.9),width = 0.2, lwd = 0.8)+
#   theme_classic()+
#   xlab("Prey species")+
#   ylab("Proportion of biomass")+
#   theme(axis.title = element_text(size = 10))+
#   theme(axis.text = element_text(size = 8))+
#   theme(legend.text = element_text(size = 8))+
#   theme(legend.title = element_text(size = 8))+
#   scale_fill_viridis_d(name = "Season ",option = "mako",direction = -1,end = 0.8,begin = 0.3)+
#   ggplot2::annotate("text", x = 0.7, y = 1, label = "(b)", size = 4)+
#   ylim(0,1)

# biomassplot <- ggplot(data = killsumm, mapping = aes(x = season2, y = biomass.prop, fill = prey_cat))+
#   geom_bar(stat = "identity")+
#   theme_classic()+
#   xlab("Season")+
#   ylab("Proportion of biomass")+
#   theme(axis.title = element_text(size = 10))+
#   theme(axis.text = element_text(size = 8))+
#   theme(legend.text = element_text(size = 8))+
#   theme(legend.title = element_text(size = 8))+
#   # theme(legend.background = element_rect(fill = "#F8F5EE"))+
#   # theme(plot.background = element_rect(fill = "#F8F5EE"))+
#   # theme(panel.background = element_rect(fill = "#F8F5EE"))+
#   scale_fill_viridis_d(name = "Prey ID",option = "mako",direction = -1,begin = 0.2)+
#   ggplot2::annotate("text", x = 0.5, y = 1, label = "(b)", size = 4)+
#   # scale_fill_manual(name = "Prey ID", values = c("burlywood1","olivedrab3","darkolivegreen4"))+
#   ylim(0,1)

# plotsize <- matrix(c(1,1,1,2,2,2,2),
#                     nrow = 1)
# grid.arrange(countplot,biomassplot,layout_matrix = plotsize)
# add save plot to a png for publication
# png(file = "../Figures/cougar_killsumm.png",height = 6, width = 10,
#     res = 300, units = "in",type = "cairo")
# bothUDs
# dev.off()
# 
# 
# par(mfrow = c(1,2))

clusters$month <- month(clusters$form_date)

# add data for broad categories of prey ages ( < or > 18 months old)
clusters <- clusters %>% 
  mutate(age2 = case_when(
    age == "neonate" ~ "Juvenile",
    age == "fawn" & month %in% c(10,11,12,1,2) ~ "Juvenile",
    age == "neonate/fawn" ~ "Juvenile",
    age == "piglet" ~ "Juvenile",
    age == "juvenile" ~ "Juvenile",
    TRUE ~ "Adult"
  ))

# summarize use of prey and age classes by season
killsummage <- clusters %>% 
  filter(carcass_found == "Y") %>% 
  filter(prey_cat %in% c("Pig","Deer")) %>% 
  group_by(season2,prey_cat,age2) %>% 
  count(prey_cat)

killsummage
# add column of species_season for grouping
killsummage$seasonprey <- paste0(killsummage$prey_cat, " ",killsummage$season2)
# summarize age class proportions by season
killsummage <- killsummage %>% 
  group_by(season2,prey_cat) %>% 
  mutate(seasonpercent = n/sum(n))

killsummage %>% 
  group_by(prey_cat,age2) %>% 
  summarise(total =sum(n))


# Bootstrap 95% confidence intervals
# adult.prop.deer.summer <- c()
# juvenile.prop.deer.summer <- c()
# adult.prop.pig.summer <- c()
# juvenile.prop.pig.summer <- c()
# adult.prop.deer.winter <- c()
# juvenile.prop.deer.winter <- c()
# adult.prop.pig.winter <- c()
# juvenile.prop.pig.winter <- c()
# 
# for(i in 1:nboot){
#   samp_rows <- sample(poss_samps,sampsize,replace = T)
#   clusters.boot <- clusters[samp_rows[1],]
#   for(j in 2:sampsize){
#     clusters.boot <- rbind(clusters.boot,clusters[samp_rows[j],])
#   }
#   
#   killsummage.boot <- clusters.boot %>% 
#     filter(carcass_found == "Y") %>% 
#     filter(prey_cat %in% c("Pig","Deer")) %>% 
#     group_by(season2,prey_cat,age2) %>% 
#     count(prey_cat)
#   
#   # add column of species_season for grouping
#   killsummage.boot$seasonprey <- paste0(killsummage.boot$prey_cat, " ",killsummage.boot$season2)
#   # summarize age class proportions by season
#   killsummage.boot <- killsummage.boot %>% 
#     group_by(season2,prey_cat) %>% 
#     mutate(seasonpercent = n/sum(n))
#   
#   adult.prop.deer.summer[i] <- killsummage.boot$seasonpercent[1]
#   juvenile.prop.deer.summer[i] <- killsummage.boot$seasonpercent[2]
#   adult.prop.pig.summer[i] <- killsummage.boot$seasonpercent[3]
#   juvenile.prop.pig.summer[i] <- killsummage.boot$seasonpercent[4]
#   adult.prop.deer.winter[i] <- killsummage.boot$seasonpercent[5]
#   juvenile.prop.deer.winter[i] <- killsummage.boot$seasonpercent[6]
#   adult.prop.pig.winter[i] <- killsummage.boot$seasonpercent[7]
#   juvenile.prop.pig.winter[i] <- killsummage.boot$seasonpercent[8]
#   if(i%%10 == 0){print(i)}
# }
# 
# killsummage$seasonpercent.lcl95 <- c(quantile(adult.prop.deer.summer,0.025),
#                             quantile(juvenile.prop.deer.summer,0.025),
#                             quantile(adult.prop.pig.summer,0.025),
#                             quantile(juvenile.prop.pig.summer,0.025),
#                             quantile(adult.prop.deer.winter,0.025),
#                             quantile(juvenile.prop.deer.winter,0.025),
#                             quantile(adult.prop.pig.winter,0.025),
#                             quantile(juvenile.prop.pig.winter,0.025))
# killsummage$seasonpercent.ucl95 <- c(quantile(adult.prop.deer.summer,0.975),
#                                      quantile(juvenile.prop.deer.summer,0.975),
#                                      quantile(adult.prop.pig.summer,0.975),
#                                      quantile(juvenile.prop.pig.summer,0.975),
#                                      quantile(adult.prop.deer.winter,0.975),
#                                      quantile(juvenile.prop.deer.winter,0.975),
#                                      quantile(adult.prop.pig.winter,0.975),
#                                      quantile(juvenile.prop.pig.winter,0.975))
# 
# ageplot <- ggplot(data = killsummage, mapping = aes(x = seasonprey, y = seasonpercent, fill = age2))+
#   geom_bar(stat = "identity", position = position_dodge())+
#   geom_errorbar(aes(x = seasonprey, ymin = seasonpercent.lcl95,ymax = seasonpercent.ucl95),
#                 position = position_dodge(width = 0.9),width = 0.2, lwd = 0.8)+
#   theme_classic()+
#   xlab("Season")+
#   ylab("Proportion of prey items")+
#   theme(axis.title = element_text(size = 10))+
#   theme(axis.text = element_text(size = 8))+
#   theme(legend.text = element_text(size = 8))+
#   theme(legend.title = element_text(size = 8))+
#   ggplot2::annotate("text", x = 0.5, y = 1, label = "(c)", size = 4)+
#   scale_fill_viridis_d(name = "Prey age",option = "mako",begin = 0.3,end = 0.8,direction = -1)+
#   ylim(0,1)
# # scale_fill_manual(name = "Prey ID", values = c("burlywood","darkolivegreen4"))
# 
# 
# 
# # # plot age class proportions by season
# # ageplot <- ggplot(data = killsummage, mapping = aes(x = seasonprey, y = seasonpercent, fill = age2))+
# #   geom_bar(stat = "identity")+
# #   theme_classic()+
# #   xlab("Season")+
# #   ylab("Proportion of prey items")+
# #   theme(axis.title = element_text(size = 10))+
# #   theme(axis.text = element_text(size = 8))+
# #   theme(legend.text = element_text(size = 8))+
# #   theme(legend.title = element_text(size = 8))+
# #   # theme(legend.background = element_rect(fill = "#F8F5EE"))+
# #   # theme(plot.background = element_rect(fill = "#F8F5EE"))+
# #   # theme(panel.background = element_rect(fill = "#F8F5EE"))+
# #   ggplot2::annotate("text", x = 0.5, y = 1, label = "(c)", size = 4)+
# #   scale_fill_viridis_d(name = "Prey age",option = "mako",begin = 0.2,end = 0.94,direction = -1)+
# #   ylim(0,1)
# #   # scale_fill_manual(name = "Prey ID", values = c("burlywood","darkolivegreen4"))
# #   
# tiff("../Figures/ForagingPlots.tiff",height = 4, width = 6, units = "in",
#      res = 300, compression = "lzw")
# plotsize <- matrix(c(1,1,1,2,2,2,2,
#                      3,3,3,3,3,3,3),
#                    nrow = 2, byrow = T)
# grid.arrange(countplot,biomassplot,ageplot,layout_matrix = plotsize)
# dev.off()


# add cougar sex to kill site data
clusters <- clusters %>% 
  mutate(lion_sex = case_when(
    grepl("F", cougar_id) == TRUE ~ "Female",
    grepl("M", cougar_id) == TRUE ~ "Male"
  ))

# summarize diet composition seperately for male and female cougars
# calculate the percenta age of each prey species-age combination in the diet
killsummsex <- clusters %>% 
  filter(carcass_found == "Y") %>% 
  # filter(prey_cat %in% c("Pig")) %>% 
  group_by(lion_sex,age2,prey_cat) %>%
  filter(prey_cat %in% c("Deer", "Pig")) %>% 
  count(prey_cat) %>% 
  group_by(lion_sex, prey_cat) %>% 
  mutate(agepercent = n/sum(n)) %>% 
  mutate(preybylionsex = paste0(lion_sex,"_", prey_cat))
  
ggplot(data = killsummsex, mapping = aes(x = preybylionsex, y = n, fill = age2))+
  geom_bar(stat = "identity")+
  theme_classic()+
  xlab("Cougar sex_Prey species")+
  ylab("Count of prey items")+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size = 16))+
  theme(legend.text = element_text(size = 16))+
  theme(legend.title = element_text(size = 18))+
  scale_fill_viridis_d(name = "Prey age",option = "mako",begin = 0.2,end = 0.94,direction = -1)

#### Diet composition by individual cougar ####

# set prey category as ordered factor so all prey are present for all individuals
clusters$prey_cat <- as.factor(clusters$prey_cat)
clusters$prey_cat <- ordered(clusters$prey_cat, levels = c("Other","Pig","Deer"))

# Group by individual cougar, season, and year, and calculate diet composition

individual_comp <- clusters %>% 
  filter(carcass_found == "Y") %>% 
  filter(cougar_id %in% c("SF01","SF09","SM06")) %>% 
  group_by(cougar_id, season, prey_cat) %>% 
  summarise(species.biomass = sum(total_biomass),
            nkills = n()) %>% 
  group_by(cougar_id, season) %>% 
  mutate(season_total_kills = sum(nkills),
         season_total_biomass = sum(species.biomass)) %>% 
  mutate(species.prop.kills = nkills/season_total_kills,
         species.prop.biomass = species.biomass/season_total_biomass)

# calculate summary for each season that provides proportion of kills and biomass
season.summary <- individual_comp %>% 
  select(cougar_id,season,season_total_kills,season_total_biomass,prey_cat,species.prop.kills,species.prop.biomass) %>% 
  pivot_wider(names_from = prey_cat,
              values_from = c(species.prop.kills,species.prop.biomass))
# shorten column names
colnames(season.summary) <- c("cougar_id","season","tot_kills","tot_bio",
                              "propkills_other","propkills_pig","propkills_deer",
                              "propbio_other","propbio_pig","propbio_deer")
# replace NAs with zeros
season.summary[is.na(season.summary)] <- 0
# Add cougar sex
season.summary$sexF <- as.numeric(grepl("F",season.summary$cougar_id))

# add kitten data, when did females have kittens
kittens <- read.csv("../RawData/cougar_kitten_dates.csv")
head(kittens)

season.summary <- season.summary %>% 
  mutate(kittens = case_when(
    cougar_id == "SF01" & season == "winter21" ~ 1,
    cougar_id == "SF09" & season %in% c("summer21","winter22") ~ 1,
    TRUE ~ 0
  )) %>% 
  mutate(season2 = substr(season,1,6),
         year = substr(season,7,8))

#### GLM of diet proportions ####
# Make weights based on mean biomass consumed
mean.bio <- mean(season.summary$tot_bio)
season.summary <- season.summary %>% 
  mutate(mod.weight = tot_bio/mean.bio)

library(glmmTMB)

mod1 <- glmer(propkills_pig ~ season2 + sexF + kittens + (1|cougar_id), data = season.summary,
            family = binomial, weights= tot_kills)
summary(mod1)

season.summary$propbio_pig <- season.summary$propbio_pig + 0.001
season.summary$propbio_deer <- season.summary$propbio_deer + 0.001
season.summary$propbio_other <- season.summary$propbio_other + 0.001


mod2 <- glmmTMB(propbio_pig ~ season2 + sexF + kittens + (1|cougar_id), data = season.summary,
                family = beta_family, weights = mod.weight)
summary(mod2)

deerage <- matrix(c(32,37,
                    84,17), nrow = 2, byrow = T)
chisq.test(deerage)

pigage <- matrix(c(10,9,
                   25,36),nrow = 2, byrow = T)
chisq.test(pigage)

temp <- clusters %>% 
  filter(carcass_found == "Y") %>% 
  filter(cougar_id %in% c("SF01","SF09","SM06","SM06 AND SF09"))
table(temp$species,temp$lion_sex)


season.summary.pub <- season.summary %>% 
  dplyr::select(cougar_id,season2,year,tot_kills,propkills_deer,propkills_pig,propkills_other,
         tot_bio, propbio_deer,propbio_pig,propbio_other)
season.summary.pub <- data.frame(season.summary.pub)
season.summary.pub[,c(6:8,10:12)] <- round(season.summary.pub[,c(6:8,10:12)],3)
write.csv(season.summary.pub, file = "../ProcessedData/Killsummary.csv", row.names = F)
