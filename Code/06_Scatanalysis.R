library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggpubr)

#### Data Manipulation and Prep ####
#Load Data
scatdat <- read.csv("../RawData/scats_NODUPLICATES.csv")
head(scatdat)

table(scatdat$Method)
scatdat <- scatdat[,1:14]

#Convert data so each species has its own column with volume percentage
species <- names(table(c(scatdat$Species_1,scatdat$Species_2,scatdat$Species_3,scatdat$Species_4)))[-1]

scatdat[,c(5,7,9,11)][scatdat[,c(5,7,9,11)] == "T"] <- 0.5
scatdat$Sp2_. <- as.numeric(scatdat$Sp2_.)
scatdat$Sp3_. <- as.numeric(scatdat$Sp3_.)
scatdat$Sp4_. <- as.numeric(scatdat$Sp4_.)


blank <- matrix(NA,ncol = length(species),
                nrow = nrow(scatdat))

scatdat <- as.data.frame(cbind(scatdat,blank))

colnames(scatdat) <- c(colnames(scatdat)[1:14],species)

x <- ncol(scatdat) - length(species) + 1

#For loop that fills in new species column with volumes from each scat
for(i in x:ncol(scatdat)){
  label <- colnames(scatdat)[i]
  for(j in 1:nrow(scatdat)){
    if(scatdat$Species_1[j] == label){
      scatdat[j,i] <- scatdat$Sp1_.[j]
    }
    if(scatdat$Species_2[j] == label){
      scatdat[j,i] <- scatdat$Sp2_.[j]
    }
    if(scatdat$Species_3[j] == label){
      scatdat[j,i] <- scatdat$Sp3_.[j]
    }
    if(scatdat$Species_4[j] == label){
      scatdat[j,i] <- scatdat$Sp4_.[j]
    }
  }
}                       

#Replace NAs with zeros
scatdat[,15:35][is.na(scatdat[,15:35])] <- 0
str(scatdat)

#Calculate volume % that excludes bone for calculating volumetric measures by species
species <- names(scatdat)
species <- species[15:33]
species <- species[-grep("BONE",species)]
species <- species[-grep("PLANT",species)]

scatdat$Prop_wo_Bone <- rowSums(scatdat[,species])

#Calculate % volume of each species excluding bone volume
scatdat$BIRD_nobone <- round(scatdat$BIRD/scatdat$Prop_wo_Bone, 3)
scatdat$CALA_nobone <- round(scatdat$CALA/scatdat$Prop_wo_Bone, 3)
scatdat$CECA_nobone <- round(scatdat$CECA/scatdat$Prop_wo_Bone, 3)
scatdat$LECA_nobone <- round(scatdat$LECA/scatdat$Prop_wo_Bone, 3)
scatdat$MEGA_nobone <- round(scatdat$MEGA/scatdat$Prop_wo_Bone, 3)
scatdat$ODHE_nobone <- round(scatdat$ODHE/scatdat$Prop_wo_Bone, 3)
scatdat$OTBE_nobone <- round(scatdat$OTBE/scatdat$Prop_wo_Bone, 3)
scatdat$PRLO_nobone <- round(scatdat$PRLO/scatdat$Prop_wo_Bone, 3)
scatdat$PUCO_nobone <- round(scatdat$PUCO/scatdat$Prop_wo_Bone, 3)
scatdat$SmallMammal_nobone <- round(scatdat$`Small Mammal`/scatdat$Prop_wo_Bone, 3)
scatdat$SYBA_nobone <- round(scatdat$SYBA/scatdat$Prop_wo_Bone,3)
scatdat$SUSC_nobone <- round(scatdat$SUSC/scatdat$Prop_wo_Bone, 3)
scatdat$CACA_nobone <- round(scatdat$CACA/scatdat$Prop_wo_Bone, 3)
scatdat$SCGR_nobone <- round(scatdat$SCGR/scatdat$Prop_wo_Bone, 3)
scatdat$UNK_nobone <- round(scatdat$UNK/scatdat$Prop_wo_Bone, 3)
scatdat$UNK1_nobone <- round(scatdat$UNK1/scatdat$Prop_wo_Bone, 3)


#### Calculate Diet Metrics ####

# Set threshold for Frequency of Occurrence
# 0 counts everything, 0.01 excludes trace detections
threshold = 0.011

#Calculate frequency of occurrence for five species groups
scatdat$ODHE_FO <- as.numeric(scatdat$ODHE_nobone + scatdat$CECA_nobone > threshold)
scatdat$SUSC_FO <- as.numeric(scatdat$SUSC_nobone > threshold)
scatdat$SmallMammal_FO <- as.numeric(scatdat$LECA_nobone + scatdat$OTBE_nobone + 
                                        scatdat$SmallMammal_nobone + scatdat$SYBA_nobone +
                                       scatdat$CACA_nobone + scatdat$SCGR_nobone > threshold)
scatdat$Carnivore_FO <- as.numeric(scatdat$CALA_nobone + scatdat$PRLO_nobone > threshold )
scatdat$Other_FO <- as.numeric(scatdat$MEGA_nobone +scatdat$BIRD_nobone +
                                 scatdat$UNK_nobone + scatdat$UNK1_nobone > threshold)

#Calculate volumetric occurrence for five species groups
scatdat$ODHE_VO <- as.numeric(scatdat$ODHE_nobone + scatdat$CECA_nobone)
scatdat$SUSC_VO <- as.numeric(scatdat$SUSC_nobone)
scatdat$SmallMammal_VO <- as.numeric(scatdat$LECA_nobone + scatdat$OTBE_nobone + 
                                        scatdat$SmallMammal_nobone + scatdat$SYBA_nobone +
                                       scatdat$CACA_nobone + scatdat$SCGR_nobone)
scatdat$Carnivore_VO <- as.numeric(scatdat$CALA_nobone + scatdat$PRLO_nobone)
scatdat$Other_VO <- as.numeric(scatdat$MEGA_nobone +scatdat$BIRD_nobone +
                                 scatdat$UNK_nobone + scatdat$UNK1_nobone)

#Sum total number of prey in each scat
scatdat$PreyCount <- rowSums(scatdat[,grep("_FO",names(scatdat))])

#Calculate numeric occurrence for five species groups
scatdat$ODHE_NO <- scatdat$ODHE_FO/scatdat$PreyCount
scatdat$SUSC_NO <- scatdat$SUSC_FO/scatdat$PreyCount
scatdat$SmallMammal_NO <- scatdat$SmallMammal_FO/scatdat$PreyCount
scatdat$Carnivore_NO <- scatdat$Carnivore_FO/scatdat$PreyCount
scatdat$Other_NO <- scatdat$Other_FO/scatdat$PreyCount

#Calculate IRI for five species groups
scatdat$ODHE_IRI <- scatdat$ODHE_FO * (scatdat$ODHE_NO + scatdat$ODHE_VO)
scatdat$SUSC_IRI <- scatdat$SUSC_FO * (scatdat$SUSC_NO + scatdat$SUSC_VO)
scatdat$SmallMammal_IRI <- scatdat$SmallMammal_FO * (scatdat$SmallMammal_NO + scatdat$SmallMammal_VO)
scatdat$Carnivore_IRI <- scatdat$Carnivore_FO * (scatdat$Carnivore_NO + scatdat$Carnivore_VO)
scatdat$Other_IRI <- scatdat$Other_FO * (scatdat$Other_NO + scatdat$Other_VO)

#Calculate total IRI for five species groups
scatdat$IRI_total <- scatdat$ODHE_IRI + scatdat$SUSC_IRI + scatdat$SmallMammal_IRI +
  scatdat$Carnivore_IRI + scatdat$Other_IRI

#Calculate Percent IRI for five species groups
scatdat$ODHE_IRIPer <- scatdat$ODHE_IRI/scatdat$IRI_total
scatdat$SUSC_IRIPer <- scatdat$SUSC_IRI/scatdat$IRI_total
scatdat$SmallMammal_IRIPer <- scatdat$SmallMammal_IRI/scatdat$IRI_total
scatdat$Carnivore_IRIPer <- scatdat$Carnivore_IRI/scatdat$IRI_total
scatdat$Other_IRIPer <- scatdat$Other_IRI/scatdat$IRI_total

#Remove rows of NAs
scatdat <- scatdat[!is.na(scatdat$BIRD_nobone),]

wholescat <- c()

for(i in 1:nrow(scatdat)){
  temp <- scatdat[i,]
  wholescat[i] <- any(temp[,37:50] == 1)
}

sum(wholescat)

scatdat[any(scatdat[,37:50] == 1),]

FO_dat <- data.frame(species = c("Deer", "Pig","Sm. mamm.", "Carn.","Other"),
                        freq.occ = c(mean(scatdat$ODHE_FO),
                                     mean(scatdat$SUSC_FO),
                                     mean(scatdat$SmallMammal_FO),
                                     mean(scatdat$Carnivore_FO),
                                     mean(scatdat$Other_FO)),
                        se = c(sd(scatdat$ODHE_FO)/sqrt(nrow(scatdat)),
                               sd(scatdat$SUSC_FO)/sqrt(nrow(scatdat)),
                               sd(scatdat$SmallMammal_FO)/sqrt(nrow(scatdat)),
                               sd(scatdat$Carnivore_FO)/sqrt(nrow(scatdat)),
                               sd(scatdat$Other_FO)/sqrt(nrow(scatdat))))

FO_dat$species <- ordered(FO_dat$species,levels = c("Deer","Pig",
                                                      "Carn.","Sm. mamm.","Other"))

##### Plot ####
ggplot(data = FO_dat,aes(x = species, y = freq.occ, fill = species))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = freq.occ - se, ymax = freq.occ + se),width = 0.2,
                linewidth = 1.3)+
  theme_classic()+
  xlab("Prey species")+
  ylab("Frequency of occurrence")+
  ylim(0,1)+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size = 16))+
  theme(legend.text = element_text(size = 16))+
  theme(legend.title = element_text(size = 18))+
  theme(legend.position = "none")+
  scale_fill_viridis_d(option = "mako", begin = 0.3)


#### Season analysis ####

scatdatseason <- scatdat[scatdat$Season %in% c("winter","summer"),]

summary(glm(ODHE_FO ~ Season, family = binomial, data = scatdatseason))
summary(glm(SUSC_FO ~ Season, family = binomial, data = scatdatseason))
summary(glm(SmallMammal_FO ~ Season, family = binomial, data = scatdatseason))
summary(glm(Carnivore_FO ~ Season, family = binomial, data = scatdatseason))


#### Cluster vs no analysis ####

summary(glm(ODHE_FO ~ Cluster, family = binomial, data = scatdat))
summary(glm(SUSC_FO ~ Cluster, family = binomial, data = scatdat))
summary(glm(SmallMammal_FO ~ Cluster, family = binomial, data = scatdat))
summary(glm(Carnivore_FO ~ Cluster, family = binomial, data = scatdat))

table(scatdat$ODHE_FO,scatdat$Cluster)

chisq.test(matrix(c(21,20,48,35),byrow=T,nrow = T))

#Calculate average and standard error IRI for each prey group
IRI_dat <- as.data.frame(cbind(c("Deer", "Pig","Small Mammal", "Carnivore","Other"),
                              c(mean(scatdat$ODHE_IRIPer),
                                mean(scatdat$SUSC_IRIPer),
                                mean(scatdat$SmallMammal_IRIPer),
                                mean(scatdat$Carnivore_IRIPer),
                                mean(scatdat$Other_IRIPer)),
                              c(sd(scatdat$ODHE_IRIPer)/sqrt(nrow(scatdat)),
                                sd(scatdat$SUSC_IRIPer)/sqrt(nrow(scatdat)),
                                sd(scatdat$SmallMammal_IRIPer)/sqrt(nrow(scatdat)),
                                sd(scatdat$Carnivore_IRIPer)/sqrt(nrow(scatdat)),
                                sd(scatdat$Other_IRIPer)/sqrt(nrow(scatdat)))
                              ))
#Change labels, round values
colnames(IRI_dat) <- c("Species","IRI","SE")
IRI_dat$Species <- ordered(IRI_dat$Species,levels = c("Deer","Pig",
                                                    "Carnivore","Small Mammal","Other"))
IRI_dat$IRI <- as.numeric(IRI_dat$IRI)
IRI_dat$IRI <- round(IRI_dat$IRI,3)
IRI_dat$SE <- as.numeric(IRI_dat$SE)
IRI_dat$SE <- round(IRI_dat$SE,3)

##### Plot ####
ggplot(data = IRI_dat,aes(x = Species, y = IRI, fill = Species))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = IRI - SE, ymax = IRI + SE),width = 0.2,
                linewidth = 1.3)+
  theme_classic()+
  xlab("Prey species")+
  ylab("Index of relative importance")+
  ylim(0,1)+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size = 16))+
  theme(legend.text = element_text(size = 16))+
  theme(legend.title = element_text(size = 18))+
  theme(legend.position = "none")+
  scale_fill_viridis_d(option = "mako", begin = 0.3)




#### By Season ####
scatdatseason <- scatdat[scatdat$Season %in% c("summer", "winter"),]


IRI_dat_summer <- as.data.frame(cbind(c("Deer", "Pig","Small Mammal", "Carnivore","Other"),
                               c(mean(scatdatseason[scatdatseason$Season == "summer",]$ODHE_IRIPer),
                                 mean(scatdatseason[scatdatseason$Season == "summer",]$SUSC_IRIPer),
                                 mean(scatdatseason[scatdatseason$Season == "summer",]$SmallMammal_IRIPer),
                                 mean(scatdatseason[scatdatseason$Season == "summer",]$Carnivore_IRIPer),
                                 mean(scatdatseason[scatdatseason$Season == "summer",]$Other_IRIPer)),
                               c(sd(scatdatseason[scatdatseason$Season == "summer",]$ODHE_IRIPer)/sqrt(nrow(scatdatseason[scatdatseason$Season == "summer",])),
                                 sd(scatdatseason[scatdatseason$Season == "summer",]$SUSC_IRIPer)/sqrt(nrow(scatdatseason[scatdatseason$Season == "summer",])),
                                 sd(scatdatseason[scatdatseason$Season == "summer",]$SmallMammal_IRIPer)/sqrt(nrow(scatdatseason[scatdatseason$Season == "summer",])),
                                 sd(scatdatseason[scatdatseason$Season == "summer",]$Carnivore_IRIPer)/sqrt(nrow(scatdatseason[scatdatseason$Season == "summer",])),
                                 sd(scatdatseason[scatdatseason$Season == "summer",]$Other_IRIPer)/sqrt(nrow(scatdatseason[scatdatseason$Season == "summer",])))
))

IRI_dat_winter <- as.data.frame(cbind(c("Deer", "Pig","Small Mammal", "Carnivore","Other"),
                                      c(mean(scatdatseason[scatdatseason$Season == "winter",]$ODHE_IRIPer),
                                        mean(scatdatseason[scatdatseason$Season == "winter",]$SUSC_IRIPer),
                                        mean(scatdatseason[scatdatseason$Season == "winter",]$SmallMammal_IRIPer),
                                        mean(scatdatseason[scatdatseason$Season == "winter",]$Carnivore_IRIPer),
                                        mean(scatdatseason[scatdatseason$Season == "winter",]$Other_IRIPer)),
                                      c(sd(scatdatseason[scatdatseason$Season == "winter",]$ODHE_IRIPer)/sqrt(nrow(scatdatseason[scatdatseason$Season == "winter",])),
                                        sd(scatdatseason[scatdatseason$Season == "winter",]$SUSC_IRIPer)/sqrt(nrow(scatdatseason[scatdatseason$Season == "winter",])),
                                        sd(scatdatseason[scatdatseason$Season == "winter",]$SmallMammal_IRIPer)/sqrt(nrow(scatdatseason[scatdatseason$Season == "winter",])),
                                        sd(scatdatseason[scatdatseason$Season == "winter",]$Carnivore_IRIPer)/sqrt(nrow(scatdatseason[scatdatseason$Season == "winter",])),
                                        sd(scatdatseason[scatdatseason$Season == "winter",]$Other_IRIPer)/sqrt(nrow(scatdatseason[scatdatseason$Season == "winter",])))
))

#Change labels, round values
colnames(IRI_dat_summer) <- c("Species","IRI","SE")
IRI_dat_summer$season <- "summer"
colnames(IRI_dat_winter) <- c("Species","IRI","SE")
IRI_dat_winter$season <- "winter"
IRI_dat_summer$Species <- ordered(IRI_dat_summer$Species,levels = c("Deer","Pig",
                                                      "Carnivore","Small Mammal","Other"))
IRI_dat_winter$Species <- ordered(IRI_dat_winter$Species,levels = c("Deer","Pig",
                                                             "Carnivore","Small Mammal","Other"))
IRI_dat_season <- rbind(IRI_dat_summer,IRI_dat_winter)

IRI_dat_season$IRI <- as.numeric(IRI_dat_season$IRI)
IRI_dat_season$IRI <- round(IRI_dat_season$IRI,3)
IRI_dat_season$SE <- as.numeric(IRI_dat_season$SE)
IRI_dat_season$SE <- round(IRI_dat_season$SE,3)

IRI_dat_season$Species <- as.character(IRI_dat_season$Species)

IRI_dat_season$Species[IRI_dat_season$Species == "Small Mammal"] <- "Sm. Mamm."
IRI_dat_season$Species[IRI_dat_season$Species == "Carnivore"] <- "Carn."

IRI_dat_season$Species <- ordered(IRI_dat_season$Species,levels = c("Deer","Pig",
                                                                    "Carn.","Sm. Mamm.","Other"))

par(mfrow = c(2,2))

summerplot <- ggplot(data = IRI_dat_season[IRI_dat_season$season == "summer",],aes(x = Species, y = IRI, fill = Species))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = IRI - SE, ymax = IRI + SE),width = 0.2,
                size = 1.3)+
  theme_classic()+
  xlab("Prey species")+
  ylab("Index of relative importance")+
  ylim(0,1)+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text = element_text(size = 14))+
  ggplot2::annotate("text", x = 0.8, y = 1.0, label = "A", size = 8)+
  theme(plot.title = element_text(size = 24, hjust = 0.5))+
  theme(legend.position = "none")+
  # theme(plot.background = element_rect(fill = "#F8F5EE"))+
  # theme(panel.background = element_rect(fill = "#F8F5EE"))+
  # scale_fill_manual(values = c("darkolivegreen","darkolivegreen3","olivedrab1","burlywood2","lemonchiffon2"))
  scale_fill_viridis_d(option = "mako", begin = 0.3)

winterplot <- ggplot(data = IRI_dat_season[IRI_dat_season$season == "winter",],aes(x = Species, y = IRI, fill = Species))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = IRI - SE, ymax = IRI + SE),width = 0.2,
                size = 1.3)+
  theme_classic()+
  xlab("Prey species")+
  ylab("Index of relative importance")+
  ylim(0,1)+
  theme(axis.title = element_text(size = 14))+
  theme(axis.text = element_text(size = 12))+
  theme(legend.position = "none")+
  ggplot2::annotate("text", x = 0.8, y = 1.0, label = "B", size = 8)+
  theme(plot.title = element_text(size = 24, hjust = 0.5))+
  # theme(plot.background = element_rect(fill = "#F8F5EE"))+
  # theme(panel.background = element_rect(fill = "#F8F5EE"))+
  # scale_fill_manual(values = c("darkolivegreen","darkolivegreen3","olivedrab1","burlywood2","lemonchiffon2"))
  scale_fill_viridis_d(option = "mako", begin = 0.3)

#### By Cluster ####

IRI_dat_cluster <- as.data.frame(cbind(c("Deer", "Pig","Small Mammal", "Carnivore","Other"),
                                      c(mean(scatdat[scatdat$Cluster == "Y",]$ODHE_IRIPer),
                                        mean(scatdat[scatdat$Cluster == "Y",]$SUSC_IRIPer),
                                        mean(scatdat[scatdat$Cluster == "Y",]$SmallMammal_IRIPer),
                                        mean(scatdat[scatdat$Cluster == "Y",]$Carnivore_IRIPer),
                                        mean(scatdat[scatdat$Cluster == "Y",]$Other_IRIPer)),
                                      c(sd(scatdat[scatdat$Cluster == "Y",]$ODHE_IRIPer)/sqrt(nrow(scatdat[scatdat$Cluster == "Y",])),
                                        sd(scatdat[scatdat$Cluster == "Y",]$SUSC_IRIPer)/sqrt(nrow(scatdat[scatdat$Cluster == "Y",])),
                                        sd(scatdat[scatdat$Cluster == "Y",]$SmallMammal_IRIPer)/sqrt(nrow(scatdat[scatdat$Cluster == "Y",])),
                                        sd(scatdat[scatdat$Cluster == "Y",]$Carnivore_IRIPer)/sqrt(nrow(scatdat[scatdat$Cluster == "Y",])),
                                        sd(scatdat[scatdat$Cluster == "Y",]$Other_IRIPer)/sqrt(nrow(scatdat[scatdat$Cluster == "Y",])))
))

IRI_dat_nocluster <- as.data.frame(cbind(c("Deer", "Pig","Small Mammal", "Carnivore","Other"),
                                      c(mean(scatdat[scatdat$Cluster == "N",]$ODHE_IRIPer),
                                        mean(scatdat[scatdat$Cluster == "N",]$SUSC_IRIPer),
                                        mean(scatdat[scatdat$Cluster == "N",]$SmallMammal_IRIPer),
                                        mean(scatdat[scatdat$Cluster == "N",]$Carnivore_IRIPer),
                                        mean(scatdat[scatdat$Cluster == "N",]$Other_IRIPer)),
                                      c(sd(scatdat[scatdat$Cluster == "N",]$ODHE_IRIPer)/sqrt(nrow(scatdat[scatdat$Cluster == "N",])),
                                        sd(scatdat[scatdat$Cluster == "N",]$SUSC_IRIPer)/sqrt(nrow(scatdat[scatdat$Cluster == "N",])),
                                        sd(scatdat[scatdat$Cluster == "N",]$SmallMammal_IRIPer)/sqrt(nrow(scatdat[scatdat$Cluster == "N",])),
                                        sd(scatdat[scatdat$Cluster == "N",]$Carnivore_IRIPer)/sqrt(nrow(scatdat[scatdat$Cluster == "N",])),
                                        sd(scatdat[scatdat$Cluster == "N",]$Other_IRIPer)/sqrt(nrow(scatdat[scatdat$Cluster == "N",])))
))

#Change labels, round values
colnames(IRI_dat_cluster) <- c("Species","IRI","SE")
IRI_dat_cluster$cluster <- "yes"
colnames(IRI_dat_nocluster) <- c("Species","IRI","SE")
IRI_dat_nocluster$cluster <- "no"
IRI_dat_cluster$Species <- ordered(IRI_dat_cluster$Species,levels = c("Deer","Pig",
                                                                    "Carnivore","Small Mammal","Other"))
IRI_dat_nocluster$Species <- ordered(IRI_dat_nocluster$Species,levels = c("Deer","Pig",
                                                                    "Carnivore","Small Mammal","Other"))
IRI_dat_clusttest <- rbind(IRI_dat_cluster,IRI_dat_nocluster)

IRI_dat_clusttest$IRI <- as.numeric(IRI_dat_clusttest$IRI)
IRI_dat_clusttest$IRI <- round(IRI_dat_clusttest$IRI,3)
IRI_dat_clusttest$SE <- as.numeric(IRI_dat_clusttest$SE)
IRI_dat_clusttest$SE <- round(IRI_dat_clusttest$SE,3)

IRI_dat_clusttest$Species <- as.character(IRI_dat_clusttest$Species)

IRI_dat_clusttest$Species[IRI_dat_clusttest$Species == "Small Mammal"] <- "Sm. Mamm."
IRI_dat_clusttest$Species[IRI_dat_clusttest$Species == "Carnivore"] <- "Carn."

IRI_dat_clusttest$Species <- ordered(IRI_dat_clusttest$Species,levels = c("Deer","Pig",
                                                                    "Carn.","Sm. Mamm.","Other"))

clusterplot <- ggplot(data = IRI_dat_clusttest[IRI_dat_clusttest$cluster == "yes",],aes(x = Species, y = IRI, fill = Species))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = IRI - SE, ymax = IRI + SE),width = 0.2,
                linewidth = 1.3)+
  theme_classic()+
  xlab("Prey species")+
  ylab("Index of relative importance")+
  ylim(0,1)+
  theme(axis.title = element_text(size = 16))+
  theme(axis.text = element_text(size = 14))+
  ggplot2::annotate("text", x = 0.8, y = 1.0, label = "C", size = 8)+
  theme(legend.position = "none")+
  scale_fill_viridis_d(option = "mako", begin = 0.3)

dogplot <- ggplot(data = IRI_dat_clusttest[IRI_dat_clusttest$cluster == "no",],aes(x = Species, y = IRI, fill = Species))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = IRI - SE, ymax = IRI + SE),width = 0.2,
                linewidth = 1.3)+
  theme_classic()+
  xlab("Prey species")+
  ylab("Index of relative importance")+
  ylim(0,1)+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size = 16))+
  ggplot2::annotate("text", x = 0.8, y = 1.0, label = "D", size = 8)+
  theme(legend.position = "none")+
  scale_fill_viridis_d(option = "mako", begin = 0.3)


ggarrange(plotlist = list(summerplot, winterplot,
                          clusterplot, dogplot),ncol = 2,
          nrow = 2)




scatdatseason
deer.season <- table(scatdatseason$ODHE_FO, scatdatseason$Season)
pig.season <- table(scatdatseason$SUSC_FO, scatdatseason$Season)
SM.season <- table(scatdatseason$SmallMammal_FO, scatdatseason$Season)
Carn.season <- table(scatdatseason$Carnivore_FO, scatdatseason$Season)

chisq.test(deer.season)
chisq.test(pig.season)
chisq.test(SM.season)
chisq.test(Carn.season)

deer.cluster <- table(scatdatseason$ODHE_FO, scatdatseason$Cluster)
pig.cluster <- table(scatdatseason$SUSC_FO, scatdatseason$Cluster)
SM.cluster <- table(scatdatseason$SmallMammal_FO, scatdatseason$Cluster)
Carn.cluster <- table(scatdatseason$Carnivore_FO, scatdatseason$Cluster)

chisq.test(deer.cluster)
chisq.test(pig.cluster)
chisq.test(SM.cluster)
chisq.test(Carn.cluster)
