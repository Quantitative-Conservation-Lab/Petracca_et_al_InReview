library(ggplot2)
library(reshape2)
library(tidyverse)
library(sf)
library(viridis)
library(here)
library(ggpubr)

#read in things we will need 
memory.limit(5000000000)

nSims <- 100 
nSamples <- 500

getwd()
file_names=as.list(dir(path=getwd(),
                       pattern=".RData", full.names=T))
#file_names <- file_names[11]

file_names <- file_names[c(1,4,5,7,9:12)]
file_names <- file_names[c(1,3,4,5,6,7,8,2)]

#file_names <- file_names[c(1,4,5,2,8,7,6,3)]

#for sarah's figures
#file_names <- file_names[c(1:3,8,11,12,15)]

#for main ms figures
#file_names <- file_names[1]


#reading in all scenario output into a scenarios list LOADING ONLY ONCE
scenarios <- list()
wolf_RR <- list()
scenarios_recov <- list()
scenarios_quasi <- list()

for(i in 1:length(file_names)){
  load(file_names[[i]])
  scenarios[[i]] <- list(Ntot.site_mean, Ntot.site_median, Nglobal_state_wmove.mean,
                         Newguys.mean, Lambda.mean, BP_Presence_summary, Pack_Size_max, Two_Adult_summary,
                         NSite_state.mean)
  wolf_RR[[i]] <- list(NAdult_EWash.mean, NAdult_NCasc.mean, NAdult_SCasc.mean,
                       NSite_EWash.mean, NSite_NCasc.mean, NSite_SCasc.mean)
  scenarios_recov[[i]] <- list(NSite_state.mean, NSite_EWash.mean, NSite_NCasc.mean, NSite_SCasc.mean)
  scenarios_quasi[[i]] <- list(NAdult_state.mean, 
                               NAdult_EWash.mean, NAdult_NCasc.mean, NAdult_SCasc.mean)
}

#ONE BY ONE
# scenarios <- list()
# for(i in 1:length(file_names)){
#   load(file_names[[i]])
#   scenarios[[i]] <- list(Ntot.site_mean, Ntot.site_median, Nglobal_state_wmove.mean,
#                          Newguys.mean, Lambda.mean, BP_Presence_summary, Pack_Size_max, Two_Adult_summary,
#                          NSite_state.mean)
# }
# 
# wolf_RR <- list()
# for(i in 1:length(file_names)){
#   load(file_names[[i]])
#   wolf_RR[[i]] <- list(NAdult_EWash.mean, NAdult_NCasc.mean, NAdult_SCasc.mean,
#                        NSite_EWash.mean, NSite_NCasc.mean, NSite_SCasc.mean)
# }
# 
# scenarios_recov <- list()
# for(i in 1:length(file_names)){
#   load(file_names[[i]])
#   scenarios_recov[[i]] <- list(NSite_state.mean, NSite_EWash.mean, NSite_NCasc.mean, NSite_SCasc.mean)
# }
# 
# scenarios_quasi <- list()
# for(i in 1:length(file_names)){
#   load(file_names[[i]])
#   scenarios_quasi[[i]] <- list(NAdult_state.mean, 
#                                NAdult_EWash.mean, NAdult_NCasc.mean, NAdult_SCasc.mean)
# }

###### LETS GET PROB 2+ AD BY SITE ######

dim(scenarios[[1]][[1]])
#extracting rows for years 1,10,20,30...
#array N_bysite has six rows, 224 sites, 15 scenarios
Prob_TwoAd_bysite <- array(NA, dim=c(6,224,length(file_names)))

for(i in 1:length(file_names)){
  Prob_TwoAd_bysite[,,i] <- scenarios[[i]][[8]][c(1,10,20,30,40,50),]
}

#make it 224x6x15
Prob_TwoAd_bysite <- aperm(Prob_TwoAd_bysite, c(2,1,3))
dim(Prob_TwoAd_bysite)
Prob_TwoAd_bysite[196,2,1]

WA <- st_read("G:/My Drive/Data/Data/GIS/washington_UTM_mainland.shp")
areas <- st_read("G:/My Drive/Data/Data/GIS/RecoveryRegions/WolfRecoveryZones.shp")
areas_UTM <- st_transform(areas, crs=32610)
areas_UTM <- st_make_valid(areas_UTM)
st_is_valid(areas_UTM)
packs <- st_read("G:/My Drive/GitHub/Wolves/Outputs/spatial_model/territory_polygons.shp")
st_crs(packs) <- 32610

dim(Prob_TwoAd_bysite)
# packs[,7:12] <- Prob_TwoAd_bysite[,,i]
# packs[,7:12] <- Prob_TwoAd_bysite[,2,1]
test <- list()
# test <- ggplot() +
#   geom_sf(data = WA, col = 'black', fill = NA, lwd = 1) +
#   geom_sf(data = packs, aes(fill=V7)) +
#   theme_bw()+
#   scale_fill_viridis(discrete=F,name="Probability of 2+ Adults")#+
#   #theme(legend.position="none")

for(i in 1:length(file_names)){
  packs[,7:12] <- Prob_TwoAd_bysite[,,i]
  test <- list()
  test[[1]] <- ggplot() +
    geom_sf(data = WA, col = 'black', fill = NA, lwd = 1) +
    geom_sf(data = packs, aes(fill=V7)) +
    theme_bw()+
    scale_fill_gradient(low = "white", high = "black", breaks=seq(0,1,by=0.25),
                        name="Probability of 2+ adults")+
    theme(legend.position="none")
  #guides(fill=guide_legend(title="Recovery Regions",nrow=2,byrow=TRUE))
  test[[2]] <- ggplot() +
    geom_sf(data = WA, col = 'black', fill = NA, lwd = 1) +
    geom_sf(data = packs, aes(fill=V8)) +
    theme_bw()+
    scale_fill_gradient(low = "white", high = "black", breaks=seq(0,1,by=0.25),
                        name="Probability of 2+ adults")+
    theme(legend.position="none")
  test[[3]] <- ggplot() +
    geom_sf(data = WA, col = 'black', fill = NA, lwd = 1) +
    geom_sf(data = packs, aes(fill=V9)) +
    theme_bw()+
    scale_fill_gradient(low = "white", high = "black", breaks=seq(0,1,by=0.25),
                        name="Probability of 2+ adults")+
    theme(legend.position="none")
  test[[4]] <- ggplot() +
    geom_sf(data = WA, col = 'black', fill = NA, lwd = 1) +
    geom_sf(data = packs, aes(fill=V10)) +
    theme_bw()+
    scale_fill_gradient(low = "white", high = "black", breaks=seq(0,1,by=0.25),
                        name="Probability of 2+ adults")+
    theme(legend.position="none")
  test[[5]] <- ggplot() +
    geom_sf(data = WA, col = 'black', fill = NA, lwd = 1) +
    geom_sf(data = packs, aes(fill=V11)) +
    theme_bw()+
    scale_fill_gradient(low = "white", high = "black", breaks=seq(0,1,by=0.25),
                        name="Probability of 2+ adults")+
    theme(legend.position="none")
  test[[6]] <- ggplot() +
    geom_sf(data = WA, col = 'black', fill = NA, lwd = 1) +
    geom_sf(data = packs, aes(fill=V12)) +
    theme_bw()+
    scale_fill_gradient(low = "white", high = "black", breaks=seq(0,1,by=0.25),
                        name="Probability of 2+ adults")+
    theme(legend.position="none")

  library(ggpubr)
  ggpubr::ggarrange(test[[1]], test[[2]], test[[3]],
                    test[[4]], test[[5]], test[[6]],# list of plots
                    labels = c("Year 1", "Year 10", "Year 20",
                               "Year 30", "Year 40", "Year 50"), # labels
                    common.legend = T, # COMMON LEGEND
                    font.label=list(color="black",size=20),
                    legend = "bottom")
  ggsave(paste0("Outputs/a_composite_model/Scenarios/Figures/Prob_TwoAd_bysite_",i,".jpg"))
}

##### LETS TRY A THING WITH DISCRETE PROBABILITY #####

dim(Prob_TwoAd_bysite)
Prob_TwoAd_bysite_copy <- Prob_TwoAd_bysite

Prob_TwoAd_bysite_copy[which(Prob_TwoAd_bysite_copy<0.5)] <- 0
Prob_TwoAd_bysite_copy[which(Prob_TwoAd_bysite_copy>=0.5)] <- 1

tail(Prob_TwoAd_bysite_copy[,,15])
tail(Prob_TwoAd_bysite[,,15])

#packs[,7:12] <- Prob_TwoAd_bysite_copy[,2,1]
# test <- list()
# test <- ggplot() +
#   geom_sf(data = WA, col = 'black', fill = NA, lwd = 1) +
#   geom_sf(data = packs, aes(fill=V7)) +
#   theme_bw()+
#   scale_fill_viridis(discrete=F,name=">50% Probability of 2+ Adults")
#   #theme(legend.position="none")

for(i in 1:length(file_names)){
  packs[,7:12] <- Prob_TwoAd_bysite_copy[,,i]
  test <- list()
  test[[1]] <- ggplot() +
    geom_sf(data = WA, col = 'black', fill = NA, lwd = 1) +
    geom_sf(data = packs, aes(fill=as.factor(V7))) +
    theme_bw()+
    scale_fill_manual(values = c("white", "black"), 
                      labels = c("no", "yes"),
                      name=">50% Probability of 2+ Adults")+     
    theme(legend.position="none")
  #guides(fill=guide_legend(title="Recovery Regions",nrow=2,byrow=TRUE))
  test[[2]] <- ggplot() +
    geom_sf(data = WA, col = 'black', fill = NA, lwd = 1) +
    geom_sf(data = packs, aes(fill=as.factor(V8))) +
    theme_bw()+
    scale_fill_manual(values = c("white", "black"), 
                      labels = c("no", "yes"),
                      name=">50% Probability of 2+ Adults")+    
    theme(legend.position="none")    
  test[[3]] <- ggplot() +
    geom_sf(data = WA, col = 'black', fill = NA, lwd = 1) +
    geom_sf(data = packs, aes(fill=as.factor(V9))) +
    theme_bw()+
    scale_fill_manual(values = c("white", "black"), 
                      labels = c("no", "yes"),
                      name=">50% Probability of 2+ Adults")+   
    theme(legend.position="none")
  test[[4]] <- ggplot() +
    geom_sf(data = WA, col = 'black', fill = NA, lwd = 1) +
    geom_sf(data = packs, aes(fill=as.factor(V10))) +
    theme_bw()+
    scale_fill_manual(values = c("white", "black"), 
                      labels = c("no", "yes"),
                      name=">50% Probability of 2+ Adults")+     
    theme(legend.position="none")
  test[[5]] <- ggplot() +
    geom_sf(data = WA, col = 'black', fill = NA, lwd = 1) +
    geom_sf(data = packs, aes(fill=as.factor(V11))) +
    theme_bw()+
    scale_fill_manual(values = c("white", "black"), 
                      labels = c("no", "yes"),
                      name=">50% Probability of 2+ Adults")+     
    theme(legend.position="none")
  test[[6]] <- ggplot() +
    geom_sf(data = WA, col = 'black', fill = NA, lwd = 1) +
    geom_sf(data = packs, aes(fill=as.factor(V12))) +
    theme_bw()+
    scale_fill_manual(values = c("white", "black"),
                      labels = c("no", "yes"),
                      name=">50% Probability of 2+ Adults")+    
    theme(legend.position="none")
  
  library(ggpubr)
  ggpubr::ggarrange(test[[1]], test[[2]], test[[3]],
                    test[[4]], test[[5]], test[[6]],# list of plots
                    labels = c("Year 1", "Year 10", "Year 20",
                               "Year 30", "Year 40", "Year 50"), # labels
                    common.legend = T, # COMMON LEGEND
                    font.label=list(color="black",size=20),
                    legend = "bottom") 
  ggsave(paste0("Outputs/a_composite_model/Scenarios/Figures/paper/Prob_TwoAd_bysite_50perc_Scenario",i,".pdf"))
}

###### LETS MOVE ON TO PROB OF RECOVERY ######

dim(scenarios_recov[[1]][[1]])

p.recovery <- matrix(NA, nrow=nSamples, ncol=length(file_names))

for(i in 1:length(file_names)){
  for(j in 1:nSamples){
    p.recovery[j,i] <- mean((scenarios_recov[[i]][[1]][j,,]>=15 &
                       scenarios_recov[[i]][[2]][j,,]>=4 &
                       scenarios_recov[[i]][[3]][j,,]>=4 &
                       scenarios_recov[[i]][[4]][j,,]>=4)>=1)
  }}

p.recovery <- as.data.frame(p.recovery)

p.recovery <- p.recovery %>% #N_sadimmig.mean NAdult_state.mean
  melt(value.name = 'total')# %>%
  # dplyr::rename(sample = Var1, year = Var2,
  #               sim= Var3) %>%
  # group_by(sample, year) %>% summarize(mean_N = mean(total))

# p.recovery.summary <- apply(p.recovery, 2, quantile, probs = c(0.025, 0.5, 0.975))

#p.recovery.summary <- as.data.frame(t(p.recovery.summary))
#colnames(p.recovery.summary) <- c("lower", "median", "upper")
# labs <- c("Baseline", "Inc remove", "No immig", "Disease", 
#           "Harvest 2.5 C", "Harvest 5% A",
#     

#reordering for this one only
labs <- c(
  `V1` = "Baseline",
  `V2` = "Translocation",
  `V3` = "Increase removals",
  `V4` = "Harvest 2.5%",
  `V5` = "Harvest 5%",
  `V6` = "50% Immigration",
  `V7` = "No Immigration",
  `V8` = "Disease 75%")

head(p.recovery)
unique(p.recovery$variable)
p.recovery$variable <- factor(p.recovery$variable, 
                                    levels = c("V1", "V2", "V6", "V8", "V4", 
                                               "V3", "V7", "V5"))

ggplot() + 
  #geom_errorbar(data=p.recovery, mapping=aes(x=as.factor(1:15), ymin=lower, ymax=upper), width=0.2, size=1, color="black") + 
  geom_boxplot(data=p.recovery, mapping=aes(x=variable, y=total)) +
  xlab("Scenario") + ylab("Probability of plan recovery \nacross all years (2021-2070)")+
  scale_x_discrete(labels = labs)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave("figures/Figure2_new.eps")
ggsave("figures/Figure2_new.jpg", width = 6, height = 4, units="in")

#####----- PROB OF RECOVERY FOR PAPER WITH REDUCED SCENARIOS AND YEARS ----####

#getting only certain years and scenarios

head(p.recovery)

# p.recovery_paper <- p.recovery %>% filter(variable == "V1" | variable =="V2" |variable =="V3" |
#                                     variable =="V8" |variable =="V12" |variable =="V14") 
#changing levels for scenarios
#3 first, then 12, 8, 2, 14, 1
p.recovery_paper$variable <- factor(p.recovery_paper$variable, 
                                levels = c("V1", "V14", "V2", 
                                           "V8", "V12", "V3"))
ggplot() + 
  #geom_errorbar(data=p.recovery, mapping=aes(x=as.factor(1:15), ymin=lower, ymax=upper), width=0.2, size=1, color="black") + 
  geom_boxplot(data=p.recovery, mapping=aes(x=variable, y=total)) +
  xlab("Scenario") + ylab("Prob of plan recovery at any point in 50 yrs")+
  scale_x_discrete(labels = labs)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Outputs/a_composite_model/Scenarios/Figures/Paper_Prob_recovery_anypt_50yrs.jpg")

#getting quantile of lambda
head(p.recovery_paper)
p.recovery_paper_summary <- p.recovery %>% group_by(variable) %>%  drop_na(total) %>% 
  summarise(quantile(total, probs=c(0.025, 0.5, 0.975))) 
#getting mean, not median
print(p.recovery_paper_summary, n=30)
labs <- c(
  `V1` = "Baseline",
  `V2` = "Translocation",
  `V3` = "Immigration 50%",
  `V4` = "Harvest 2.5%",
  `V5` = "Disease 75%",
  `V6` = "No Immigration",
  `V7` = "Increase Removals",
  `V8` = "Harvest 5%")

###### LETS MOVE ONTO TIME TO PLAN RECOVERY ######


# #prob of recovering at any point in 100 years
# p.recovery[sim] <- mean((NSite_state.proj[,1:proj]>=15 &
#                            NSite_EWash.proj[,1:proj]>=4 &
#                            NSite_NCasc.proj[,1:proj]>=4 &
#                            NSite_SCasc.proj[,1:proj]>=4)>=1)

dim(scenarios_recov[[1]][[1]])

p.recovery.time <- array(NA, dim=c(nSamples, 51, length(file_names)))

for(i in 1:length(file_names)){
  for(j in 1:nSamples){
    for(k in 1:51){
      p.recovery.time[j,k,i] <- mean((scenarios_recov[[i]][[1]][j,k,]>=15 &
                               scenarios_recov[[i]][[2]][j,k,]>=4 &
                               scenarios_recov[[i]][[3]][j,k,]>=4 &
                               scenarios_recov[[i]][[4]][j,k,]>=4)>=1)
    }}}

dim(p.recovery.time)

p.recovery.time_summary <- aperm(p.recovery.time, c(2,3,1))
dim(p.recovery.time_summary)
p.recovery.time_summary <- matrix(p.recovery.time_summary, 51*length(file_names), 500)
p.recovery.time_summary <- as.data.frame(p.recovery.time_summary)
p.recovery.time_summary$scenario <- rep(1:length(file_names),each=51)
p.recovery.time_summary$year <- rep(1:51,times=length(file_names))
p.recovery.time <- p.recovery.time_summary %>% #N_sadimmig.mean NAdult_state.mean
  melt(id=c("scenario", "year"), value.name = 'total') #%>%

labs <- c("Baseline", "Inc remove", "No immig", "Disease 25 A", "Disease 25 C", 
          "Disease 50 A", "Disease 50 C", "Disease 75 A", "Disease 75 C", 
          "Harvest 5 A", "Harvest 5 C", "Harvest 10 A", "Harvest 10 C", "Tran St H",
          "Tran Olympic")

p.recovery.20 <- subset(p.recovery.time,  year<=20)
head(p.recovery.20)

ggplot() + 
geom_boxplot(data=p.recovery.20, mapping=aes(x=as.factor(year), y=total)) +
  xlab("Year") + ylab("Probability of meeting plan recovery")+
  facet_wrap(.~as.factor(scenario), labeller = as_labeller(panel_names))



ggsave("Outputs/a_composite_model/Scenarios/Figures/TimePlanRecovery.jpg")

#####----- TIME TO PLAN RECOVERY FOR PAPER WITH REDUCED SCENARIOS AND YEARS ----####

#getting only certain years and scenarios

head(p.recovery.time)
unique(p.recovery.time$scenario)

#getting only certain years and scenarios
p.recovery.time_paper <- p.recovery.time %>% filter(year==11 | year==21 | year==31 | year==41 | year==51)

#changing levels for scenarios
#3 first, then 12, 8, 2, 14, 1
unique(p.recovery.time_paper$scenario)
unique(p.recovery.time_paper$variable)
head(p.recovery.time_paper)
p.recovery.time_paper$scenario <- factor(p.recovery.time_paper$scenario ,
                                 levels = c("1", "2", "6",
                                            "8", "4", "3", "7", "5"))

library(MetBrewer)

ggplot(p.recovery.time_paper, aes(x=as.factor(year), y=total, fill=as.factor(scenario))) + 
  geom_boxplot() +
  ylab("Probability of meeting plan recovery")+
  scale_fill_manual(name = "Scenario", 
                    labels = c("Baseline", "Translocation", "Immig 50%",
                               "Disease 75%", "Harvest 2.5%", "Inc Removals", "No Immig", "Harvest 5%"),
                    values=met.brewer("Signac", 8))+
  scale_x_discrete(name = "Year", labels=c("11" = "2030", "21" = "2040","31" = "2050",
                                           "41" = "2060","51" = "2070"))+
  theme(legend.position="bottom")
ggsave("Outputs/a_composite_model/Scenarios/Figures/papertwo/Chapter2_Figure3.eps")
ggsave("figures/Figure3_new.jpg", width = 6, height = 4, units="in")


#getting quantiles
head(p.recovery.time_paper)
p.recovery.time_paper_summary <- p.recovery.time_paper %>% group_by(scenario,year) %>%  drop_na(total) %>% 
  summarise(mean(total)) 
#getting mean, not median
print(p.recovery.time_paper_summary, n=150)

###### LETS MOVE ON TO PROB OF QUASI-EXTINCTION ######

# p.quasiext[sim] <- mean((NAdult_state.proj[,1:proj]<92 &
#                            NAdult_EWash.proj[,1:proj]<24 &
#                            NAdult_NCasc.proj[,1:proj]<24 &
#                            NAdult_SCasc.proj[,1:proj]<24)>=1)

dim(scenarios_quasi[[1]][[1]])

p.quasi <- matrix(NA, nrow=nSamples, ncol=length(file_names))

for(i in 1:length(file_names)){
  for(j in 1:nSamples){
    p.quasi[j,i] <- mean((scenarios_quasi[[i]][[1]][j,,]<92 &
                            scenarios_quasi[[i]][[2]][j,,]<24 &
                            scenarios_quasi[[i]][[3]][j,,]<24 &
                            scenarios_quasi[[i]][[4]][j,,]<24)>=1)
  }}

p.quasi <- as.data.frame(p.quasi)

p.quasi <- p.quasi %>% #N_sadimmig.mean NAdult_state.mean
  melt(value.name = 'total')

# p.quasi.summary <- apply(p.quasi, 2, quantile, probs = c(0.025, 0.5, 0.975))
# p.quasi.summary <- as.data.frame(t(p.quasi.summary))
# colnames(p.quasi) <- c("lower", "median", "upper")
labs <- c("Baseline", "Inc remove", "No immig", "Disease 25 A", "Disease 25 C", 
          "Disease 50 A", "Disease 50 C", "Disease 75 A", "Disease 75 C", 
          "Harvest 5 A", "Harvest 5 C", "Harvest 10 A", "Harvest 10 C", "Tran St H",
          "Tran Olympic")
ggplot() + 
  #geom_errorbar(data=p.quasi.summary, mapping=aes(x=as.factor(1:15), ymin=lower, ymax=upper), width=0.2, size=1, color="black") + 
  geom_boxplot(data=p.quasi, mapping=aes(x=variable, y=total)) +
  xlab("Scenario") + ylab("Prob of quasi-extinction at any pt in 50 yrs")+
  scale_x_discrete(labels = labs)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
ggsave("Outputs/a_composite_model/Scenarios/Figures/ProbQuasiExtinction.jpg")


#####----- PROB OF QUASIEXT FOR PAPER WITH REDUCED SCENARIOS AND YEARS ----####

#getting only certain years and scenarios

head(p.quasi)
unique(p.quasi$variable)

p.quasi_paper <- p.quasi %>% filter(variable == "V1" | variable =="V2" |variable =="V3" |
                                            variable =="V8" |variable =="V12" |variable =="V14") 
#changing levels for scenarios
#3 first, then 12, 8, 2, 14, 1
head(p.quasi)
unique(p.quasi$variable)
p.quasi$variable <- factor(p.quasi$variable, 
                           levels = c("V1", "V2", "V6", "V8", "V4", 
                                      "V3", "V7", "V5"))

ggplot() + 
  #geom_errorbar(data=p.recovery, mapping=aes(x=as.factor(1:15), ymin=lower, ymax=upper), width=0.2, size=1, color="black") + 
  geom_boxplot(data=p.quasi, mapping=aes(x=variable, y=total)) +
  xlab("Scenario") + ylab("Probability of quasi-extinction \nacross all years (2021-2070)")+
  scale_x_discrete(labels = labs)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Outputs/a_composite_model/Scenarios/Figures/papertwo/Chapter2_Figure5.eps")
ggsave("figures/Figure5_new.jpg", width = 6, height = 4, units="in")


#getting quantiles
head(p.quasi_paper)
p.quasi_paper_summary <- p.quasi %>% group_by(variable) %>%  drop_na(total) %>% 
  summarise(quantile(total, probs=c(0.025, 0.5, 0.975))) 
#getting mean, not median
print(p.quasi_paper_summary, n=30)

###### LETS MOVE ON TO PROB OF EXTINCTION ######

# p.quasiext[sim] <- mean((NAdult_state.proj[,1:proj]<92 &
#                            NAdult_EWash.proj[,1:proj]<24 &
#                            NAdult_NCasc.proj[,1:proj]<24 &
#                            NAdult_SCasc.proj[,1:proj]<24)>=1)

dim(scenarios_quasi[[1]][[1]])

p.ext <- matrix(NA, nrow=nSamples, ncol=length(file_names))

for(i in 1:length(file_names)){
  for(j in 1:nSamples){
    p.ext[j,i] <- mean((scenarios_quasi[[i]][[1]][j,50,]==0)>=1)
  }}

# p.ext.summary <- apply(p.ext, 2, quantile, probs = c(0.025, 0.5, 0.975))
# p.ext.summary <- as.data.frame(t(p.ext.summary))

p.ext <- as.data.frame(p.ext)

p.ext <- p.ext %>% #N_sadimmig.mean NAdult_state.mean
  melt(value.name = 'total')

#colnames(p.ext) <- c("lower", "median", "upper")
#keep same as quasi
head(p.ext)
unique(p.ext$variable)
p.ext$variable <- factor(p.ext$variable,
                         levels = c("V1", "V2", "V7",
                                    "V4", "V5", "V3", "V8", "V6"))

ggplot() + 
  #geom_errorbar(data=p.ext.summary, mapping=aes(x=as.factor(1:15), ymin=lower, ymax=upper), width=0.2, size=1, color="black") + 
  geom_boxplot(data=p.ext, mapping=aes(x=variable, y=total)) +
  xlab("Scenario") + ylab("Probability of extinction in 2070")+
  scale_x_discrete(labels = labs)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Outputs/a_composite_model/Scenarios/Figures/papertwo/ProbExtinction_paper_Dec9.jpg")

#####----- PROB OF EXTINCTION FOR PAPER WITH REDUCED SCENARIOS AND YEARS ----####

#getting only certain years and scenarios

head(p.ext)

p.ext_paper <- p.ext %>% filter(variable == "V1" | variable =="V2" |variable =="V3" |
                                      variable =="V8" |variable =="V12" |variable =="V14") 
#changing levels for scenarios
#3 first, then 12, 8, 2, 14, 1
p.ext_paper$variable <- factor(p.ext_paper$variable, 
                                 levels = c("V1", "V14", "V2", 
                                            "V8", "V12", "V3"))
labs <- c(
  `V1` = "Baseline",
  `V14` = "Translocation MSH",
  `V2` = "Increase removals",
  `V8` = "Disease",
  `V12` = "Harvest",
  `V3` = "No immigration")

ggplot() + 
  #geom_errorbar(data=p.recovery, mapping=aes(x=as.factor(1:15), ymin=lower, ymax=upper), width=0.2, size=1, color="black") + 
  geom_boxplot(data=p.ext, mapping=aes(x=variable, y=total)) +
  xlab("Scenario") + ylab("Probability of extinction at Yr 50")+
  scale_x_discrete(labels = labs)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Outputs/a_composite_model/Scenarios/Figures/Paper_ProbExtinction.jpg")

#getting quantile of lambda
head(p.ext_paper)
p.ext_paper_summary <- p.ext %>% group_by(variable) %>%  drop_na(total) %>% 
  summarise(quantile(total, probs=c(0.025, 0.5, 0.975))) 
#getting mean, not median
print(p.ext_paper_summary, n=30)

###### LETS MOVE ON TO LAMBDA ######

#extracting rows for years 1,10,20,30...
#array N_bysite has six rows, 224 sites, 15 scenarios

lambda <- array(NA, dim=c(500,50,length(file_names)))
for(i in 1:length(file_names)){
  lambda[,,i] <- apply(scenarios[[i]][[5]], c(1,2), mean, na.rm=T)
}
dim(scenarios[[1]][[5]])
dim(lambda)

lambda_mean <- array(NA, dim=c(500,length(file_names)))
for(i in 1:length(file_names)){
  for(j in 1:nSamples){
  lambda_mean[j,i] <- prod(lambda[j,1:50,i]) ^(1/50) }}


#lambdaext <- apply(scenarios[[1]][[5]], 2, mean)
      
# for(i in 1:length(file_names)){
#   for(j in 1:49){
#     lambda[j,i] <- mean(scenarios[[i]][[5]][,j,i], na.rm=T)
#   }}

lambda <- as.data.frame(lambda_mean)

lambda <- lambda %>% #N_sadimmig.mean NAdult_state.mean
  melt(value.name = 'total')
lambda$variable <- factor(lambda$variable,
                          levels = c("V1", "V2", "V6", "V8", "V4", 
                                     "V3", "V7", "V5"))
head(lambda)
unique(lambda$variable)

ggplot(data=lambda, mapping=aes(x=variable, y=total)) + 
  #geom_errorbar(data=p.ext.summary, mapping=aes(x=as.factor(1:15), ymin=lower, ymax=upper), width=0.2, size=1, color="black") + 
  geom_violin() +
  stat_summary(fun="median", geom="point")+
  xlab("Scenario") + ylab("Geometric mean of lambda")+
  scale_x_discrete(labels = labs)+ ylim(0.8,1.15)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Outputs/a_composite_model/Scenarios/Figures/papertwo/Chapter2_Figure4.eps")
ggsave("figures/Figure4_new.jpg", width = 6, height = 4, units="in")


#####----- LAMBDA FOR PAPER WITH REDUCED SCENARIOS AND YEARS ----####

#getting only certain years and scenarios

head(lambda)

lambda_paper <- lambda %>% filter(variable == "V1" | variable =="V2" |variable =="V3" |
                                     variable =="V8" |variable =="V12" |variable =="V14") 
#changing levels for scenarios
#3 first, then 12, 8, 2, 14, 1
lambda_paper$variable <- factor(lambda_paper$variable, 
                                 levels = c("V1", "V14", "V2", 
                                            "V8", "V12", "V3"))
labs <- c(
  `V1` = "Baseline",
  `V14` = "Translocation MSH",
  `V2` = "Increase removals",
  `V8` = "Disease",
  `V12` = "Harvest",
  `V3` = "No immigration")

ggplot(data=lambda_paper, mapping=aes(x=variable, y=total)) + 
  #geom_errorbar(data=p.ext.summary, mapping=aes(x=as.factor(1:15), ymin=lower, ymax=upper), width=0.2, size=1, color="black") + 
  geom_violin() +
  stat_summary(fun="median", geom="point")+
  xlab("Scenario") + ylab("Geometric mean of lambda")+
  scale_x_discrete(labels = labs)+ ylim(0.8,1.2)+
 # theme(axis.text.x = element_text(vjust = grid::unit(c(-2, 0), "points")))
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Outputs/a_composite_model/Scenarios/Figures/Paper_Geometric_mean_lambda.jpg")


#getting quantile of lambda
lambda_paper_summary <- lambda %>% group_by(variable) %>%  drop_na(total) %>% 
  summarise(quantile(total, probs=c(0.025, 0.5, 0.975))) 
print(lambda_paper_summary,n=24)
#reordering for this one only
labs <- c(
  `V1` = "Baseline",
  `V2` = "Translocation",
  `V3` = "75% Disease",
  `V4` = "Increase Removals",
  `V5` = "Harvest 2.5%",
  `V6` = "Harvest 5%",
  `V7` = "50% Immigration",
  `V8` = "No Immigration")

#getting mean, not median
lambda_paper_summary <- lambda %>% group_by(variable) %>%  drop_na(total) %>% 
  filter(variable == "V1" | variable == "V3") %>% summarise(mean(total)) 


###### LETS MOVE ON TO NUMBER TOTAL WOLVES ######
dim(scenarios[[1]][[3]])
NWolves_Total <- array(NA, dim=c(500,51,100,length(file_names)))

#dplyr::rename(scenario = Var1, year = Var2) %>%NWolves_Total <- array(NA, dim=c(nSamples,51,nSims,length(file_names)))
for(i in 1:length(file_names)){
  NWolves_Total[,,,i] <- scenarios[[i]][[3]]
}
#dim(NWolves_Total_summary)
# NWolves_Total_summary <- apply(NWolves_Total, c(2,4), 
#                                quantile, probs = c(0.25, 0.5, 0.75),  na.rm = TRUE)
# #test <- NWolves_Total_summary[2,c(1,50),1]

dim(NWolves_Total)
NWolves_Total_summary <- aperm(NWolves_Total, c(2,4,1,3))
dim(NWolves_Total_summary)

NWolves_Total_summary_2d <- matrix(NWolves_Total_summary, 51*length(file_names), 500*100)
NWolves_Total_summary_2d <- as.data.frame(NWolves_Total_summary_2d)
dim(NWolves_Total_summary_2d)

NWolves_Total_summary_2d$scenario <- rep(1:length(file_names),each=51)
NWolves_Total_summary_2d$year <- rep(1:51,times=length(file_names))
#head(NWolves_Total_summary_2d)

NWolves <- NWolves_Total_summary_2d %>% #N_sadimmig.mean NAdult_state.mean
  melt(id=c("scenario", "year"), value.name = 'total') #%>%
#group_by(sample, year) %>% summarize(mean_N = mean(total))

# 
# NWolves_Total_summary <- aperm(NWolves_Total_summary, c(2,3,1))
# #dim(NWolves_Total_summary)   
# 
# dim(NWolves_Total_summary)

# NWolves_Total_summary_2d <- matrix(NWolves_Total_summary, 50*length(file_names), 3)
# NWolves_Total_summary_2d <- as.data.frame(NWolves_Total_summary_2d)
# 
# NWolves_Total_summary_2d$scenario <- rep(1:length(file_names),each=50)
# NWolves_Total_summary_2d$year <- rep(1:50,times=length(file_names))
# 
# colnames(NWolves_Total_summary_2d)[1:3] <- c("lower", "median", "upper")

scenario1 <- subset(NWolves, scenario==1)

ggplot() + 
  #geom_errorbar(data=NWolves_Total_summary_2d, mapping=aes(x=year, ymin=lower, ymax=upper), width=0.1, size=.5) + 
  geom_boxplot(data=NWolves, mapping=aes(x=as.factor(year), y=total)) +
  xlab("Year") + ylab("Number of wolves")+
  facet_wrap(.~as.factor(scenario)) #labeller = as_labeller(panel_names))#+

ggsave("Outputs/a_composite_model/Scenarios/Figures/Number_of_Wolves_Baseline.jpg")

scenario1 <- subset(NWolves, scenario==3)

ggplot() + 
  #geom_errorbar(data=NWolves_Total_summary_2d, mapping=aes(x=year, ymin=lower, ymax=upper), width=0.1, size=.5) + 
  geom_boxplot(data=scenario1, mapping=aes(x=as.factor(year), y=total)) +
  xlab("Year") + ylab("Number of wolves")+
  facet_wrap(.~as.factor(scenario), labeller = as_labeller(panel_names))#+

ggsave("Outputs/a_composite_model/Scenarios/Figures/Number_of_Wolves_NoImmig.jpg")

#####----- NUMBER OF WOLVES FOR PAPER WITH REDUCED SCENARIOS AND YEARS ----####

#getting only certain years and scenarios
Nwolves_paper <- NWolves %>% filter(year==1 | year==11 | year==21 | year==31 | year==41 | year==51)
head(Nwolves_paper)
#changing levels for scenarios
#3 first, then 12, 8, 2, 14, 1
Nwolves_paper$scenario <- factor(Nwolves_paper$scenario ,
                                 levels = c("1", "2", "6", "8", "4", 
                                            "3", "7", "5"))
levels(Nwolves_paper$scenario)

#getting quantile of values in year 50
dim(Nwolves_paper)
head(Nwolves_paper)
Nwolves_paper_summary <- Nwolves_paper %>% group_by(scenario, year) %>%  summarise(quantile(total, probs=c(0.025, 0.5, 0.975))) %>% 
  filter(year == 1 | year==51) #%>% filter(scenario == 7)
print(Nwolves_paper_summary,n=48)

head(Nwolves_paper)

library(MetBrewer)
Nwolves_paper_figure <- Nwolves_paper %>% filter(year==11 | year==21 | year==31 | year==41 | year==51)

ggplot(Nwolves_paper_figure, aes(x=as.factor(year), y=total, fill=as.factor(scenario))) + 
  geom_boxplot() +
  ylab("Number of wolves")+
  scale_fill_manual(name = "Scenario", 
                    labels = c("Baseline", "Translocation", 
                               "Immig 50%", "Disease 75%", "Harvest 2.5%", 
                               "Inc Removals", "No Immig", 
                               "Harvest 5%"),
                    values=met.brewer("Signac", 8))+
  scale_x_discrete(name = "Year", labels=c("11" = "2030", "21" = "2040","31" = "2050",
                            "41" = "2060","51" = "2070"))+
  theme(legend.position="bottom")
ggsave("Outputs/a_composite_model/Scenarios/Figures/papertwo/Chapter2_Figure6.eps")
ggsave("figures/Figure6_new.jpg", width = 6, height = 4, units="in")

#####----- NUMBER OF WOLVES FOR PAPER WITH BASELINE SCENARIO AND ONLY A FEW YEARS ----####

#reading in data model
memory.limit(size=20000000)

out <- readRDS("Outputs/a_composite_model/jags_removals_ddirch_Dec14_trunc_epsB.rds")

library(ggplot2)
library(tidyverse)
library(MCMCvis)
library(here)
library(tidybayes)
library(coda)

out1_df <- as.data.frame(MCMCchains(out))
#out2_df <- as.data.frame(MCMCchains(out2))

sum_noNA <- function(mat) {
  sum <- sum(mat, na.rm=T)
  return(sum)
}

#now let's do a histogram comparing these
Nglobal_wmove_rem <- out1_df %>% dplyr::select("Nglobal_wmove[1]","Nglobal_wmove[2]","Nglobal_wmove[3]","Nglobal_wmove[4]","Nglobal_wmove[5]","Nglobal_wmove[6]",
                                               "Nglobal_wmove[7]","Nglobal_wmove[8]","Nglobal_wmove[9]","Nglobal_wmove[10]","Nglobal_wmove[11]","Nglobal_wmove[12]")

test <- as_tibble(Nglobal_wmove_rem) %>% 
  pivot_longer(everything(), names_to = "param") %>% 
  mutate(year = as.numeric(rep(c(1:12), 60000)),
         type=rep("IPM Estimate", 720000))

# all$param <- factor(all$param, levels = c("Nglobal[1]","Nglobal[2]","Nglobal[3]","Nglobal[4]","Nglobal[5]","Nglobal[6]",
#                                              "Nglobal[7]","Nglobal[8]","Nglobal[9]","Nglobal[10]","Nglobal[11]","Nglobal[12]"))

test$param <- factor(test$param, levels = c("Nglobal_wmove[1]","Nglobal_wmove[2]","Nglobal_wmove[3]","Nglobal_wmove[4]","Nglobal_wmove[5]","Nglobal_wmove[6]",
                                            "Nglobal_wmove[7]","Nglobal_wmove[8]","Nglobal_wmove[9]","Nglobal_wmove[10]","Nglobal_wmove[11]","Nglobal_wmove[12]"))
levels(test$param) <- 1:12

head(test)
#labs <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
labs <- c(2009:2020)

head(test)
#ok, so 'test' has estimates from data period

NWolves_Total <- array(NA, dim=c(nSamples,50,nSims,length(file_names)))
for(i in 1:length(file_names)){
  NWolves_Total[,,,i] <- scenarios[[i]][[3]]
}

dim(NWolves_Total)
NWolves_Total_summary <- aperm(NWolves_Total, c(2,4,1,3))
dim(NWolves_Total_summary)

NWolves_Total_summary_2d <- matrix(NWolves_Total_summary, 50*length(file_names), 500*100)
NWolves_Total_summary_2d <- as.data.frame(NWolves_Total_summary_2d)
dim(NWolves_Total_summary_2d)

NWolves_Total_summary_2d$scenario <- rep(1:length(file_names),each=50)
NWolves_Total_summary_2d$year <- rep(1:50,times=length(file_names))
head(NWolves_Total_summary_2d)

NWolves <- NWolves_Total_summary_2d %>% #N_sadimmig.mean NAdult_state.mean
  melt(id=c("scenario", "year"), value.name = 'total') #%>%

head(NWolves)
head(test)

NWolves_data <- test %>% rename(total=value) %>% mutate(year=year+2008) %>% dplyr::select(year, total)
NWolves_proj <- NWolves %>% mutate(year = year+2019) %>% dplyr::select(year, total) %>% filter(year!=2020)
NWolves_all <- rbind(NWolves_data, NWolves_proj)

###COMBINED VERSION

ggplot(data=NWolves_all, aes(x = as.factor(year), y = total)) +  
  stat_lineribbon() + 
  scale_fill_brewer()+
  xlab("year")+
  ylab("number of wolves")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, 
                                      hjust = 1, vjust = 0.5))
ggsave("Outputs/a_composite_model/Scenarios/Figures/paper/TotalWolves.jpg")

#getting only certain years and scenarios
Nwolves_paper <- NWolves %>% filter(year==10 | year==20 | year==30 | year==40 | year==50)
Nwolves_paper <-  Nwolves_paper %>% filter(scenario==1 | scenario==2 | scenario==3 | scenario==8 |
                                             scenario==12 | scenario==14) 
#changing levels for scenarios
#3 first, then 12, 8, 2, 14, 1
Nwolves_paper$scenario <- factor(Nwolves_paper$scenario , 
                                 levels = c("1", "14", "2", 
                                            "8", "12", "3"))

#getting quantile of values in year 50
dim(Nwolves_paper)
head(Nwolves_paper)
Nwolves_paper_summary <- Nwolves_paper %>% group_by(scenario, year) %>%  summarise(quantile(total)) %>% 
  filter(year == 10 | year==50) %>% filter(scenario == 1 | scenario==3)
print(Nwolves_paper_summary)

panel_names <- c(
  `1` = "Baseline",
  `2` = "Increase removals",
  `3` = "No immigration",
  `8` = "Disease 75% Additive Loss",
  `12` = "Harvest 10% Additive Loss",
  `14` = "Translocation St. Helens",
  `15` = "Translocation Olympic")

head(Nwolves_paper)

library(MetBrewer)

ggplot(Nwolves_paper, aes(x=as.factor(year), y=total, fill=as.factor(scenario))) + 
  geom_boxplot() +
  ylab("Number of wolves")+
  scale_fill_manual(name = "Scenario", 
                    labels = c("Baseline", "Translocation MSH", "Increase Removals",
                               "Disease 75% Additive", "Harvest 10% Additive", 
                               "No Immigration"),
                    values=met.brewer("Johnson", 6))+
  scale_x_discrete(name = "Year", labels=c("10" = "2030", "20" = "2040","30" = "2050",
                                           "40" = "2060","50" = "2070"))+
  theme(legend.position="bottom")
ggsave("Outputs/a_composite_model/Scenarios/Figures/Paper_Number_of_Wolves.jpg")

###### LETS MOVE ON TO NUMBER OCCUPIED SITES BY YEAR ######

dim(scenarios[[1]][[4]])
NSites_Total <- array(NA, dim=c(nSamples,50,nSims,length(file_names)))
for(i in 1:length(file_names)){
  NSites_Total[,,,i] <- scenarios[[i]][[9]]
}
#dim(NSites_Total_summary)
# NSites_Total_summary <- apply(NSites_Total, c(2,4), 
#                                quantile, probs = c(0.25, 0.5, 0.75),  na.rm = TRUE)
# #test <- NSites_Total_summary[2,c(1,50),1]

dim(NSites_Total)
NSites_Total_summary <- aperm(NSites_Total, c(2,4,1,3))
dim(NSites_Total_summary)

NSites_Total_summary_2d <- matrix(NSites_Total_summary, 50*length(file_names), 500*100)
NSites_Total_summary_2d <- as.data.frame(NSites_Total_summary_2d)
dim(NSites_Total_summary_2d)

NSites_Total_summary_2d$scenario <- rep(1:length(file_names),each=50)
NSites_Total_summary_2d$year <- rep(1:50,times=length(file_names))
head(NSites_Total_summary_2d)

NSites <- NSites_Total_summary_2d %>% #N_sadimmig.mean NAdult_state.mean
  melt(id=c("scenario", "year"), value.name = 'total') #%>%
#dplyr::rename(scenario = Var1, year = Var2) %>%
#group_by(sample, year) %>% summarize(mean_N = mean(total))

# 
# NSites_Total_summary <- aperm(NSites_Total_summary, c(2,3,1))
# #dim(NSites_Total_summary)   
# 
# dim(NSites_Total_summary)

# NSites_Total_summary_2d <- matrix(NSites_Total_summary, 50*length(file_names), 3)
# NSites_Total_summary_2d <- as.data.frame(NSites_Total_summary_2d)
# 
# NSites_Total_summary_2d$scenario <- rep(1:length(file_names),each=50)
# NSites_Total_summary_2d$year <- rep(1:50,times=length(file_names))
# 
# colnames(NSites_Total_summary_2d)[1:3] <- c("lower", "median", "upper")

panel_names <- c(
  `1` = "Baseline",
  `2` = "Inc remove",
  `3` = "No immig",
  `4` = "Disease 25 A",  
  `5` = "Disease 25 C",
  `6` = "Disease 50 A",
  `7` = "Disease 50 C",
  `8` = "Disease 75 A",
  `9` = "Disease 75 C",
  `10` = "Harvest 5 A",
  `11` = "Harvest 5 C",
  `12` = "Harvest 10 A",
  `13` = "Harvest 10 C",
  `14` = "Tran St H",
  `15` = "Tran Olympic")

scenario1 <- subset(NSites, scenario==1)

ggplot() + 
  #geom_errorbar(data=NSites_Total_summary_2d, mapping=aes(x=year, ymin=lower, ymax=upper), width=0.1, size=.5) + 
  geom_boxplot(data=scenario1, mapping=aes(x=as.factor(year), y=total)) +
  xlab("Year") + ylab("Number of sites with breeding pair")+
  facet_wrap(.~as.factor(scenario), labeller = as_labeller(panel_names))#+

ggsave("Outputs/a_composite_model/Scenarios/Figures/Number_of_Sites_Baseline.jpg")

scenario1 <- subset(NSites, scenario==3)

ggplot() + 
  #geom_errorbar(data=NSites_Total_summary_2d, mapping=aes(x=year, ymin=lower, ymax=upper), width=0.1, size=.5) + 
  geom_boxplot(data=scenario1, mapping=aes(x=as.factor(year), y=total)) +
  xlab("Year") + ylab("Number of sites with breeding pair")+
  facet_wrap(.~as.factor(scenario), labeller = as_labeller(panel_names))#+

ggsave("Outputs/a_composite_model/Scenarios/Figures/Number_of_Sites_NoImmig.jpg")




###### LETS MOVE ON TO NUMBER TOTAL WOLVES BY RECOVERY REGION ######

dim(scenarios[[1]][[4]])
NWolves_EWash <- NWolves_NCasc <- NWolves_SCasc <- array(NA, dim=c(nSamples,50,nSims,length(file_names)))
for(i in 1:length(file_names)){
  NWolves_EWash[,,,i] <- wolf_RR[[i]][[1]]
  NWolves_NCasc[,,,i] <- wolf_RR[[i]][[2]]
  NWolves_SCasc[,,,i] <- wolf_RR[[i]][[3]]
}
#dim(NWolves_Total_summary)
# NWolves_Total_summary <- apply(NWolves_Total, c(2,4), 
#                                quantile, probs = c(0.25, 0.5, 0.75),  na.rm = TRUE)
# #test <- NWolves_Total_summary[2,c(1,50),1]

NWolves_EWA_summary <- aperm(NWolves_EWash, c(2,4,1,3))
NWolves_EWA_summary_2d <- matrix(NWolves_EWA_summary, 50*length(file_names), 500*100)
NWolves_EWA_summary_2d <- as.data.frame(NWolves_EWA_summary_2d)
NWolves_EWA_summary_2d$scenario <- rep(1:length(file_names),each=50)
NWolves_EWA_summary_2d$year <- rep(1:50,times=length(file_names))
NWolves_EWA_summary_2d$region <- "E_WA"
NWolves_EWA <- NWolves_EWA_summary_2d %>% #N_sadimmig.mean NAdult_state.mean
  melt(id=c("scenario", "year", "region"), value.name = 'total') #%>%


NWolves_NCA_summary <- aperm(NWolves_NCasc, c(2,4,1,3))
NWolves_NCA_summary_2d <- matrix(NWolves_NCA_summary, 50*length(file_names), 500*100)
NWolves_NCA_summary_2d <- as.data.frame(NWolves_NCA_summary_2d)
NWolves_NCA_summary_2d$region <- "N_Casc"
NWolves_NCA_summary_2d$scenario <- rep(1:length(file_names),each=50)
NWolves_NCA_summary_2d$year <- rep(1:50,times=length(file_names))
NWolves_NCA <- NWolves_NCA_summary_2d %>% #N_sadimmig.mean NAdult_state.mean
  melt(id=c("scenario", "year", "region"), value.name = 'total') #%>%


NWolves_SCA_summary <- aperm(NWolves_SCasc, c(2,4,1,3))
NWolves_SCA_summary_2d <- matrix(NWolves_SCA_summary, 50*length(file_names), 500*100)
NWolves_SCA_summary_2d <- as.data.frame(NWolves_SCA_summary_2d)
NWolves_SCA_summary_2d$region <- "S_Casc"
NWolves_SCA_summary_2d$scenario <- rep(1:length(file_names),each=50)
NWolves_SCA_summary_2d$year <- rep(1:50,times=length(file_names))
NWolves_SCA <- NWolves_SCA_summary_2d %>% #N_sadimmig.mean NAdult_state.mean
  melt(id=c("scenario", "year", "region"), value.name = 'total') #%>%

NWolves <- rbind(NWolves_EWA, NWolves_NCA, NWolves_SCA)
head(NWolves)
dim(NWolves)
panel_names <- c(
  `1` = "Baseline",
  `2` = "Inc remove",
  `3` = "No immig",
  `4` = "Disease 25 A",  
  `5` = "Disease 25 C",
  `6` = "Disease 50 A",
  `7` = "Disease 50 C",
  `8` = "Disease 75 A",
  `9` = "Disease 75 C",
  `10` = "Harvest 5 A",
  `11` = "Harvest 5 C",
  `12` = "Harvest 10 A",
  `13` = "Harvest 10 C",
  `14` = "Tran St H",
  `15` = "Tran Olympic")

require(ggplot2)
 
scenario1 <- subset(NWolves, scenario==1)
head(scenario1)
ggplot() + 
  #geom_errorbar(data=NWolves_Total_summary_2d, mapping=aes(x=year, ymin=lower, ymax=upper), width=0.1, size=.5) + 
  geom_boxplot(outlier.size=-1, data=scenario1, mapping=aes(x=as.factor(year), y=total, fill=region)) +
  xlab("Year") + ylab("Number of wolves")+ ylim(0,500)+
  scale_fill_manual(name = "region", labels = c("Eastern Washington", 
                                                  "Northern Cascades",
                                                  "Southern Cascades & NW Coast"),#+
          values=c("gold1","lightblue2","mediumorchid"))+
  theme(legend.position="bottom")
#facet_wrap(.~as.factor(scenario), labeller = as_labeller(panel_names))#+

ggsave("Outputs/a_composite_model/Scenarios/Figures/Number_of_Wolves_RR_Baseline.jpg")

scenario1 <- subset(NWolves, scenario==3)

ggplot() + 
  #geom_errorbar(data=NWolves_Total_summary_2d, mapping=aes(x=year, ymin=lower, ymax=upper), width=0.1, size=.5) + 
  geom_boxplot(outlier.size=-1, data=scenario1, mapping=aes(x=as.factor(year), y=total, fill=region)) +
  xlab("Year") + ylab("Number of wolves")+ ylim(0,500)+
  scale_fill_manual(name = "region", labels = c("Eastern Washington", 
                                                "Northern Cascades",
                                                "Southern Cascades & NW Coast"),
                    values=c("gold1","lightblue2","mediumorchid"))+
                    theme(legend.position="bottom")
                    
ggsave("Outputs/a_composite_model/Scenarios/Figures/Number_of_Wolves_RR_NoImmig.jpg")

###### LETS MOVE ON TO NUMBER OCCUPIED SITES BY RECOVERY REGION ######

dim(scenarios[[1]][[4]])
NSites_EWash <- NSites_NCasc <- NSites_SCasc <- array(NA, dim=c(nSamples,50,nSims,length(file_names)))
for(i in 1:length(file_names)){
  NSites_EWash[,,,i] <- wolf_RR[[i]][[4]]
  NSites_NCasc[,,,i] <- wolf_RR[[i]][[5]]
  NSites_SCasc[,,,i] <- wolf_RR[[i]][[6]]
}
#dim(NSites_Total_summary)
# NSites_Total_summary <- apply(NSites_Total, c(2,4), 
#                                quantile, probs = c(0.25, 0.5, 0.75),  na.rm = TRUE)
# #test <- NSites_Total_summary[2,c(1,50),1]

NSites_EWA_summary <- aperm(NSites_EWash, c(2,4,1,3))
NSites_EWA_summary_2d <- matrix(NSites_EWA_summary, 50*length(file_names), 500*100)
NSites_EWA_summary_2d <- as.data.frame(NSites_EWA_summary_2d)
NSites_EWA_summary_2d$scenario <- rep(1:length(file_names),each=50)
NSites_EWA_summary_2d$year <- rep(1:50,times=length(file_names))
NSites_EWA_summary_2d$region <- "E_WA"
NSites_EWA <- NSites_EWA_summary_2d %>% #N_sadimmig.mean NAdult_state.mean
  melt(id=c("scenario", "year", "region"), value.name = 'total') #%>%


NSites_NCA_summary <- aperm(NSites_NCasc, c(2,4,1,3))
NSites_NCA_summary_2d <- matrix(NSites_NCA_summary, 50*length(file_names), 500*100)
NSites_NCA_summary_2d <- as.data.frame(NSites_NCA_summary_2d)
NSites_NCA_summary_2d$region <- "N_Casc"
NSites_NCA_summary_2d$scenario <- rep(1:length(file_names),each=50)
NSites_NCA_summary_2d$year <- rep(1:50,times=length(file_names))
NSites_NCA <- NSites_NCA_summary_2d %>% #N_sadimmig.mean NAdult_state.mean
  melt(id=c("scenario", "year", "region"), value.name = 'total') #%>%


NSites_SCA_summary <- aperm(NSites_SCasc, c(2,4,1,3))
NSites_SCA_summary_2d <- matrix(NSites_SCA_summary, 50*length(file_names), 500*100)
NSites_SCA_summary_2d <- as.data.frame(NSites_SCA_summary_2d)
NSites_SCA_summary_2d$region <- "S_Casc"
NSites_SCA_summary_2d$scenario <- rep(1:length(file_names),each=50)
NSites_SCA_summary_2d$year <- rep(1:50,times=length(file_names))
NSites_SCA <- NSites_SCA_summary_2d %>% #N_sadimmig.mean NAdult_state.mean
  melt(id=c("scenario", "year", "region"), value.name = 'total') #%>%

NSites <- rbind(NSites_EWA, NSites_NCA, NSites_SCA)
head(NSites)
dim(NSites)
panel_names <- c(
  `1` = "Baseline",
  `2` = "Inc remove",
  `3` = "No immig",
  `4` = "Disease 25 A",  
  `5` = "Disease 25 C",
  `6` = "Disease 50 A",
  `7` = "Disease 50 C",
  `8` = "Disease 75 A",
  `9` = "Disease 75 C",
  `10` = "Harvest 5 A",
  `11` = "Harvest 5 C",
  `12` = "Harvest 10 A",
  `13` = "Harvest 10 C",
  `14` = "Tran St H",
  `15` = "Tran Olympic")

require(ggplot2)

scenario1 <- subset(NSites, scenario==1)
head(scenario1)
ggplot() + 
  #geom_errorbar(data=NSites_Total_summary_2d, mapping=aes(x=year, ymin=lower, ymax=upper), width=0.1, size=.5) + 
  geom_boxplot(outlier.size=-1, data=scenario1, mapping=aes(x=as.factor(year), y=total, fill=region)) +
  xlab("Year") + ylab("Number of sites with a breeding pair")+ ylim(0,40)+
  scale_fill_manual(name = "region", labels = c("Eastern Washington", 
                                                "Northern Cascades",
                                                "Southern Cascades & NW Coast"),#+
                    values=c("gold1","lightblue2","mediumorchid"))+
  theme(legend.position="bottom")
#facet_wrap(.~as.factor(scenario), labeller = as_labeller(panel_names))#+

ggsave("Outputs/a_composite_model/Scenarios/Figures/Number_of_Sites_RR_Baseline.jpg")

scenario1 <- subset(NSites, scenario==3)

ggplot() + 
  #geom_errorbar(data=NSites_Total_summary_2d, mapping=aes(x=year, ymin=lower, ymax=upper), width=0.1, size=.5) + 
  geom_boxplot(outlier.size=-1, data=scenario1, mapping=aes(x=as.factor(year), y=total, fill=region)) +
  xlab("Year") + ylab("Number of sites with a breeding pair")+ ylim(0,40)+
  scale_fill_manual(name = "region", labels = c("Eastern Washington", 
                                                "Northern Cascades",
                                                "Southern Cascades & NW Coast"),
                    values=c("gold1","lightblue2","mediumorchid"))+
  theme(legend.position="bottom")

ggsave("Outputs/a_composite_model/Scenarios/Figures/Number_of_Sites_RR_NoImmig.jpg")


#########################################################################
##################### END OF CODE ########################################
#########################################################################


# ###### LETS SEE HOW MANY MOVERS THERE ARE ######
# 
# dim(scenarios[[1]][[2]])
# dim(scenarios[[1]][[5]])
# 
# NWolves_Total <- NWolves_Total_wMovers <- nMovers <- array(NA, dim=c(nSamples,50,nSims,length(file_names)))
# for(i in 1:length(file_names)){
#   NWolves_Total_wMovers[,,,i] <- scenarios[[i]][[2]]
# }
# for(i in 1:length(file_names)){
#   NWolves_Total[,,,i] <- scenarios[[i]][[5]]
# }
# nMovers <- NWolves_Total_wMovers - NWolves_Total
# 
# #dim(NWolves_Total_summary)
# NMovers_summary <- apply(nMovers, c(2,4), 
#                          quantile, probs = c(0.025, 0.5, 0.975),  na.rm = TRUE)
# NMovers_summary <- aperm(NMovers_summary, c(2,3,1))
# #dim(NMovers_summary)   
# 
# NMovers_summary_2d <- matrix(NMovers_summary, 50*length(file_names), 3)
# NMovers_summary_2d <- as.data.frame(NMovers_summary_2d)
# 
# NMovers_summary_2d$scenario <- rep(1:length(file_names),each=50)
# NMovers_summary_2d$year <- rep(1:50,times=length(file_names))
# 
# colnames(NMovers_summary_2d)[1:3] <- c("lower", "median", "upper")
# 
# 
# ggplot() + 
#   geom_errorbar(data=NMovers_summary_2d, mapping=aes(x=year, ymin=lower, ymax=upper), width=0.1, size=.5) + 
#   geom_point(data=NMovers_summary_2d, mapping=aes(x=year, y=median), size=1, shape=16) +
#   xlab("Year") + ylab("Number of wolves")+
#   facet_wrap(1~scenario)+
#   theme(strip.background = element_blank(),
#         strip.text.x = element_blank())
# ggsave("Outputs/a_composite_model/Scenarios/Figures/Number_of_Wolves.jpg")





library(ggplot2)
library(reshape2)
library(tidyverse)
Nglobal.plot <- N_sadimmig.mean %>% #N_sadimmig.mean NAdult_state.mean
  melt(value.name = 'total') %>%
  dplyr::rename(sample = Var1, year = Var2,
                sim= Var3) %>%
  group_by(sample, year) %>% summarize(mean_N = mean(total))

ggplot(Nglobal.plot,
       aes(as.factor(year), mean_N, group = sample, col = sample)) +
  geom_line() +
  xlab('') + ylab('Number of adults in state') +
  # facet_wrap(~mod) +
  theme(legend.position = 'none')# +
# scale_x_continuous(limits = c(1,nyears.ch+nyears.gap+proj),
#                    breaks = seq(2, nyears.ch+nyears.gap+proj, by = 10),
#                    labels = seq(2005, (2005+nyears.ch+nyears.gap+proj), by = 10))
ggsave("Outputs/a_composite_model/Scenarios/Figures/13_harvest_05_compens_smaller.jpg")
ggsave("Outputs/a_composite_model/Scenarios/Figures/14_transloc_sthelens_smaller.jpg")
ggsave("Outputs/a_composite_model/Scenarios/Figures/13_harvest_05_compens_smaller.jpg")
ggsave("Outputs/a_composite_model/Scenarios/Figures/12_harvest_05_additive_smaller.jpg")
ggsave("Outputs/a_composite_model/Scenarios/Figures/11_harvest_025_compens_smaller.jpg")
ggsave("Outputs/a_composite_model/Scenarios/Figures/10_harvest_025_additive_smaller.jpg")
ggsave("Outputs/a_composite_model/Scenarios/Figures/9_disease_75_compens_smaller.jpg")
ggsave("Outputs/a_composite_model/Scenarios/Figures/8_disease_75_additive_smaller.jpg")
ggsave("Outputs/a_composite_model/Scenarios/Figures/7_disease_50_compens_smaller.jpg")
ggsave("Outputs/a_composite_model/Scenarios/Figures/6_disease_50_additive_smaller.jpg")
ggsave("Outputs/a_composite_model/Scenarios/Figures/5_disease_25_compens_smaller.jpg")
ggsave("Outputs/a_composite_model/Scenarios/Figures/4_disease_25_additive_smaller.jpg")
ggsave("Outputs/a_composite_model/Scenarios/Figures/3_noimmig_smaller.jpg")
ggsave("Outputs/a_composite_model/Scenarios/Figures/2_increase_removals_smaller.jpg")
ggsave("Outputs/a_composite_model/Scenarios/Figures/1_Baseline_smaller.jpg")
















#reading in spatial stuff from projection
load("Data/composite_model/proj_spatial_inputs.RData")
#reading in projection model output
memory.limit(5000000000)
out <- readRDS("Outputs/a_composite_model/jags_removals_ddirch_Dec7_trunc.rds")




#############GARBAGE STUFF FOR TOMORROW

#probability of population being below qe individuals at the end year
p.qe.end[which(mods==m)] <- mean(Ntot[,(nyears.gap+nyears.ch+proj)]<100)
p.qe.end.lower[which(mods==m)] <- quantile(Ntot[,(nyears.gap+nyears.ch+proj)]<100, probs = 0.025)
#see around code L. 8118 in kitchen sink to see how amanda handled this

#there is a mod ticker that maybe goes outside sims loop

p.qe.end.upper[which(mods==m)] <- quantile(Ntot[,(nyears.gap+nyears.ch+proj)]<100, probs = 0.975)

# for each row, is out$sims.list$Ntot[,1:(nyears.gap+nyears.ch)]<qe true at any column?
# take mean (find proportion) of times across all mcmc rows that the answer is yes, falls below qe at any column; row == mcmc sample, column == year
p.qe.ever[which(mods==m)] <- mean(rowSums(Ntot[,1:(nyears.gap+nyears.ch+proj)]<100)>1)

#population size at end of data period
end.size[which(mods==m)] <- mean(Ntot[,(nyears.gap+nyears.ch)])

#growth rate
lambda.yr.mean <- lambda.yr.2.5 <- lambda.yr.97.5 <- numeric(length = length(2:(nyears.ch+nyears.gap+proj)))

for (t in 2:(nyears.ch+nyears.gap+proj)) {
  lambda.yr.mean[t] <- mean(Ntot[,t]/(Ntot[,t-1]), na.rm = T) #mean per year, across mcmc iterations
  lambda.yr.2.5[t] <- quantile(Ntot[,t]/(Ntot[,t-1]), probs = 0.025, 
                               names = F, na.rm = T) #lower across mcmc iterations
  lambda.yr.97.5[t] <- quantile(Ntot[,t]/(Ntot[,t-1]), probs = 0.975, 
                                names = F, na.rm = T) #upper across mcmc iterations
  
  #probability across iterations that population growth rate each year is positive, 
  prob.positive.yr[t] <- sum(Ntot[,t]/(Ntot[,t-1])>1.0, na.rm = T)/length(Ntot[,t]/(Ntot[,t-1]))
  
}

max.lambda[which(mods==m)] <- max(lambda.yr.mean)

#probability that growth rate is positive over study period
#mean of prob.positive.yr? mean of lambda.yr.mean?
prob.positive.proj[which(mods==m)] <- mean(prob.positive.yr, na.rm = T)
# prob.positive.proj[which(mod==m)] <- sum(lambda.yr.mean>1)/(length(!is.nan(lambda.yr.mean>0)))

#mean, upper and lower quantiles for pop growth over the study period, arithmetic mean
lambda.mean[which(mods==m)] <- mean(lambda.yr.mean[which(lambda.yr.mean>0)])
lambda.lower[which(mods==m)] <- mean(lambda.yr.2.5[which(lambda.yr.2.5>0)])
lambda.upper[which(mods==m)] <- mean(lambda.yr.97.5[which(lambda.yr.97.5>0)])

#all trajectories per age
Ntot <- data.frame(Ntot)
Ntot$mod <- m
Ntot_all <- bind_rows(Ntot_all, Ntot)

} #mod




for(i in 1:nSamples){
  for(t in 1:proj){
    Nglobal.proj[i,t] <- sum(Ntot.proj[i,t,]) #+ sum(N.movers.proj[i,3,1,t,]) + sum(N.movers.proj[i,2,1,t,])
    NAdult.SouthCasc.proj[i,t] <- sum(NAdult.SouthCasc.bysite.proj[i,t,])
    NAdult.EWash.proj[i,t] <- sum(NAdult.NorthCasc.bysite.proj[i,t,])
    NAdult.NorthCasc.proj[i,t] <- sum(NAdult.EWash.bysite.proj[i,t,])
  }}

save(N_sim, file="Outputs/a_composite_model/N_sim.rds")
dim(N_sim)
N_sim[nSamples,3,5,19,5]
N.proj[nSamples,3,1,5,19]for(i in 1:nSamples){
  for(t in 1:proj){
    Nglobal.proj[i,t] <- sum(Ntot.proj[i,t,])
  }}



dim(N.settlers.proj)
N.settlers.proj[nSamples,,1,10,]
#there are too many settlers coming in (upwards of five per pack year in later years?)  

dim(Tot.immig.proj)
Tot.immig.proj[nSamples,1,3,]
#possibly two many immigrants (four per pack year in year 3?)

dim(N.proj)
N.proj[nSamples,1,1,10,]
#6-mo old numbers up to 14 per pack year in year 5

#testing the rcat thing
rcat(100,probs.pup[nSamples,])-1

#looking at three classes
dim(N.stayers.proj)
N.settlers.proj[nSamples,,1,2,] #these should all go to immigrant class, or remain where they are
#problem here. it's the jump to the second period in a given year that is the problem.
N.stayers.proj[nSamples,,1,1,19] #there are zero wolves in site 19
N.stayers.proj[nSamples,,2,1,19] #now there are 2 and 6 in age classes 2 and 3, respectively; imposs
N.stayers.proj[nSamples,,1,2,19] #now they are all adults
N.stayers.proj[nSamples,,2,2,19] #now there are 2 and 17 wolves in in age classes 2 and 3, respectively; imposs
dim(N.immig.proj)
N.immig[nSamples,3,2,,]
N.movers.proj[nSamples,,1,2,] #relatively high no of movers
N.proj[nSamples,,1,2,] #yeah, a ton

#getting avg number of wolves per pack in final year
dim(Ntot.proj)
#this is across all years
Ntot_mean <- apply(Ntot.proj, c(3,2), mean)

Ntot_mean <- apply(Ntot.proj, c(3,2), mean)
dim(Tot.immig)
Tot.immig_mean <- apply(Tot.immig, c(4,3), mean)
Ntot.proj[nSamples,,]
#now this is for final year
Ntot_year10 <- as.matrix(Ntot_mean[,10])
Ntot_year5 <- as.matrix(Ntot_mean[,5])
Ntot_year1 <- as.matrix(Ntot_mean[,1])
Ntot_year50 <- as.matrix(Ntot_mean[,50])

#TO DO
#update dispersal distances
#extend calculation of radii to more than 150 km
#check on occu psis

head(packs)
library(sf)
packs <- st_read("Outputs/spatial_model/territory_polygons.shp")
WA_UTM <- st_read("G:/My Drive/Data/Data/GIS/washington_UTM_mainland.shp")
packs$wolves_10 <- Ntot_year10
packs$wolves_1 <- Ntot_year1
packs$wolves_5 <- Ntot_year5
packs$wolves_50 <- Ntot_year50


ggplot() +
  geom_sf(data=WA_UTM, fill=NA, color = "gray60", size=1)+
  geom_sf(data = packs, aes(fill = wolves_10)) + 
  theme_bw() +
  ggtitle('simulated wolf numbers across WA in 2030')+
  guides(fill=guide_legend(title="# wolves"))
ggplot() +
  geom_sf(data=WA_UTM, fill=NA, color = "gray60", size=1)+
  geom_sf(data = packs, aes(fill = wolves_1)) + 
  theme_bw() +
  ggtitle('simulated wolf numbers across WA in 2020')+
  guides(fill=guide_legend(title="# wolves"))
ggplot() +
  geom_sf(data=WA_UTM, fill=NA, color = "gray60", size=1)+
  geom_sf(data = packs, aes(fill = wolves_5)) + 
  theme_bw() +
  ggtitle('simulated wolf numbers across WA in 2025')+
  guides(fill=guide_legend(title="# wolves"))
ggplot() +
  geom_sf(data=WA_UTM, fill=NA, color = "gray60", size=1)+
  geom_sf(data = packs, aes(fill = wolves_5)) + 
  theme_bw() +
  ggtitle('simulated wolf numbers across WA in 2070')+
  guides(fill=guide_legend(title="# wolves"))

packid
packid_all <-1:224
packid_unocc <- packid_all[-packid]

dim(Tot.immig)
dim(N.proj)
dim(N.proj)

#these are the formerly unoccupied packs
N.proj.unocc <- N.proj[,3,1,,packid_unocc]
count(N.proj.unocc>2)
table(N.proj.unocc)

packid_unocc[19]
N.proj[225,3,1,4,20]

library(ggplot2)
library(reshape2)

#getting at how number of adults changes in sites where there were >2 adults at some point

NAdult.proj <- N.proj.unocc %>%
  melt(value.name = 'total') %>%
  dplyr::rename(sim = Var1, year = Var2, site=Var3) %>%
  filter(total>=2) #%>% filter(site==19 | site==27 | site==36 | site==46 | site==55) #%>% 
#group_by(year) %>% summarise(n=n())

length(unique(NAdult.proj$site)) #29 sites previously unoccupied w at least two wolves

NAdult.proj <- N.proj.unocc %>%
  melt(value.name = 'total') %>%
  dplyr::rename(sim = Var1, year = Var2, site=Var3) %>%
  filter(total==1)   %>%  group_by(year, site) %>% summarise(n=n())
#group_by(year) %>% summarise(n=n())
print(NAdult.proj,n=100)

length(unique(NAdult.proj$site)) #31 sites previously unoccupied w at least one wolf

max(NAdult.proj$site)
max(NAdult.proj$year)
max(NAdult.proj$sim)

ggplot(NAdult.proj,
       aes(as.factor(year), total, group = sim, col = sim)) +
  geom_line() +
  xlab('') + ylab('Number of adults') +
  facet_wrap(~site) +
  theme(legend.position = 'none')# +
scale_x_continuous(limits = c(1,nyears.ch+nyears.gap+proj),
                   breaks = seq(2, nyears.ch+nyears.gap+proj, by = 10),
                   labels = seq(2005, (2005+nyears.ch+nyears.gap+proj), by = 10))

#getting at proportion of nSamples iterations with >2 adults per year across all sites
NAdult.prop <- N.proj.unocc %>%
  melt(value.name = 'total') %>%
  dplyr::rename(sim = Var1, year = Var2, site=Var3) %>%
  filter(total>=2) %>% group_by(year, site) %>% summarise(n=n()) %>% mutate(percent = ((n/nSamples)*100))


length(unique(NAdult.prop$site)) #41 sites of 190 with iterations of >=2 adults

ggplot(NAdult.prop,
       aes(as.factor(site), percent, group = year, col = year)) +
  geom_point() +
  xlab('Unoccupied sites in year 1') + ylab('Percent of sims with >2 adults')+
  theme(axis.text.x = element_blank())

#with year on x now
ggplot(NAdult.prop,
       aes(as.factor(year), percent, group = site, col = site)) +
  geom_point() +
  xlab('Year') + ylab('Percent of sims with >2 adults')

#producing mean and max of all iterations for unoccupied sites
mean_greater2 <- apply(N.proj.unocc, c(3,2), mean)
max_greater2 <- apply(N.proj.unocc, c(3,2), max)
mean_greater2.plot <- mean_greater2 %>%
  melt(value.name = 'param') %>%
  dplyr::rename(site = Var1, year = Var2) %>% mutate(type="mean")
max_greater2.plot <- max_greater2 %>%
  melt(value.name = 'param') %>%
  dplyr::rename(site = Var1, year = Var2) %>% mutate(type="max")
all <- rbind(mean_greater2.plot,max_greater2.plot)
ggplot(all,
       aes(as.factor(site), param, group = type, col = type)) +
  geom_point() +
  xlab('Unoccupied sites in year 1') + ylab('Value across all simulations')+
  theme(axis.text.x = element_blank())

#i like it by year better? both needed
ggplot(all,
       aes(as.factor(year), param, group = type, col = type)) +
  geom_point() +
  xlab('Year') + ylab('Value across all simulations')#+
#theme(axis.text.x = element_blank())

#Dec 7 looking at stuff
dim(N.proj)
N.proj[nSamples,1:3,1,1:5,223] 
Ntot.proj[nSamples,1:5,223]
Nglobal.proj[nSamples,1:5]

N.proj[2000,1:3,1,1:5,223] 
Ntot.proj[2000,1:5,223]
Nglobal.proj[2000,1:5]

Ntot.proj[2000,1:5,223]
N.stayers.proj[2000,2:3,1,1:5,223]
N.immig.proj[2000,1:3,1,1:5,223]
N.settlers.proj[2000,2:3,1,1:5,223] #why are there NAs here; first year, age2

sum(is.na(Ntot.proj))
sum(is.na(N.stayers.proj))
sum(is.na(N.immig.proj))
sum(is.na(N.settlers.proj))
sum(is.na(N.movers.proj))
