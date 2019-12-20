#' ---
#' title: "R-Code for MEM models"
#' author: "Matthias F. Biber"
#' date: "16.06.2019"
#' ---

#' setup, eval=F
#Load required packages

#Automatically install required packages, which are not yet installed
packages <- c("sp", "raster", "dplyr", "ggplot2", "ggpmisc", "ModelMetrics",
              "gbm", "mgcv", "magrittr", "tidyr", "devtools")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages); rm(new.packages)

#Load packages
l <- sapply(packages, require, character.only = TRUE, quietly=TRUE); rm(packages, l)

#' species_data, eval=F
#Load species data
amphibians_dist <- readr::read_csv("data/amphibians_dist.csv.xz")
birds_dist <-  readr::read_csv("data/ter_birds_dist.csv.xz")
mammals_dist <-  readr::read_csv("data/ter_mammals_dist.csv.xz")

#Create presence column
amphibians_dist$presence <- 1
mammals_dist$presence <- 1
birds_dist$presence <- 1

#Create taxa column
amphibians_dist$taxa <- "amphibians"
mammals_dist$taxa <- "mammals"
birds_dist$taxa <- "birds"

length(unique(amphibians_dist$species))
length(unique(birds_dist$species))
length(unique(mammals_dist$species))

#' species_richness, eval=F
#Calulate amphibian SR
sr_amphibians <- amphibians_dist %>% group_by(x, y, taxa) %>% 
  summarise(sum = sum(presence))

#Calulate terrestrial mammal SR
sr_mammals <- mammals_dist %>% group_by(x, y, taxa) %>% 
  summarise(sum = sum(presence))

#Calulate terrestrial bird SR
sr_birds <- birds_dist %>% group_by(x, y, taxa) %>% 
  summarise(sum = sum(presence))

#Merge data into list
data <- list(sr_amphibians, sr_mammals, sr_birds)
names(data) <- c("amphibians", "mammals", "birds")

#Create observed SR data
sr_observed <- do.call("rbind", data)
readr::write_csv(sr_observed, "data/sr_observed_all.csv.xz")

#' add_envdata, eval=F
sr_observed <- read.csv("data/sr_observed_all.csv.xz")
load("data/bioclim_EWEMBI_1995_landonly.rda")

#Extract environmental data for each location
sr_observed %<>% left_join(bioclim_EWEMBI_1995_landonly) %>% 
  dplyr::select("x", "y", "sum", "bio4", "bio5", "bio18", 
                "bio19")

#' ssdm_speciesnames, eval=F

#Set taxa
spnames <- lapply(c("Amphibian", "Bird", "Mammal"), function(taxa){
  #Read data
  AUC_data <- lapply(c("GAM", "GBM", "MaxEnt", "RF"), function(model_type){
    readr::read_csv(paste0("data/AUCvalues_All_", 
                           model_type, "_", taxa, ".csv.xz"))})
  AUC_data <- do.call(rbind, AUC_data)
  
  #Aggregate the different AUC values from the 10 iterations per species
  #and filter by AUC > 0.7
  library(dplyr)
  AUC_sum <- AUC_data %>% group_by(Species, taxa, model_type) %>% 
    summarise(mean = mean(AUC, na.rm=T)) %>% filter(mean >= 0.7) %>% ungroup() %>% 
    group_by(Species, taxa) %>% summarise(n = n()) %>% filter(n == 4)
  spNames <- unique(AUC_sum$Species)
  return(spNames)
})
names(spnames) <- c("amphibians", "birds", "mammals")
length(spnames)
saveRDS(spnames, "data/spnames.rds")

#' ssdm_obs_sr, eval=F
spnames <- readRDS("data/spnames.rds")

#Calulate amphibian SR
sr_amphibians_ssdm <- amphibians_dist %>% 
  filter(species %in% sub("_", " ", spnames[[1]])) %>% 
  group_by(x, y, taxa) %>% summarise(sum = sum(presence))

#Calulate terrestrial bird SR
sr_birds_ssdm <- birds_dist %>% 
  filter(species %in% sub("_", " ", spnames[[2]])) %>% 
  group_by(x, y, taxa) %>% summarise(sum = sum(presence))

#Calulate terrestrial mammal SR
sr_mammals_ssdm <- mammals_dist %>% 
  filter(species %in% sub("_", " ", spnames[[3]])) %>% 
  group_by(x, y, taxa) %>% summarise(sum = sum(presence))

rm(amphibians_dist, mammals_dist, birds_dist)

#Merge observed richness
sr_observed <- rbind(sr_amphibians_ssdm, sr_mammals_ssdm, sr_birds_ssdm)
rm(sr_amphibians_ssdm, sr_mammals_ssdm, sr_birds_ssdm)

#Save observed SR ssdm
readr::write_csv(sr_observed, "data/sr_observed_ssdm.csv.xz")

#' ssdm_cur_sr, eval=F
#Load Probabilities

#Set Dispersal
lapply(c("presence", "dispersal1", "dispersal2", "dispersal3", "dispersal4", "fulldisp"), function(disp){
  year <- 1995
  taxa <- c("Amphibian", "Mammal", "Bird")
  
  # Predicted SR
  amphi_sr_gam <- read.csv(paste0("data/", taxa[1], "_prob_GAM_", disp, ".csv.xz")) %>% 
    select(c(x,y), matches(paste0(year)))
  amphi_sr_gbm <- read.csv(paste0("data/", taxa[1], "_prob_GBM_", disp, ".csv.xz")) %>% 
    select(c(x,y), matches(paste0(year)))
  mammal_sr_gam <- read.csv(paste0("data/", taxa[2], "_prob_GAM_", disp, ".csv.xz")) %>% 
    select(c(x,y), matches(paste0(year)))
  mammal_sr_gbm <- read.csv(paste0("data/", taxa[2], "_prob_GBM_", disp, ".csv.xz")) %>% 
    select(c(x,y), matches(paste0(year)))
  bird_sr_gam <- read.csv(paste0("data/", taxa[3], "_prob_GAM_", disp, ".csv.xz")) %>% 
    select(c(x,y), matches(paste0(year)))
  bird_sr_gbm <- read.csv(paste0("data/", taxa[3], "_prob_GBM_", disp, ".csv.xz")) %>% 
    select(c(x,y), matches(paste0(year)))
  
  # Merge predicted richness
  amphi_sr_gam$model <- "GAM"
  amphi_sr_gam$taxa <- "amphibians"
  mammal_sr_gam$model <- "GAM"
  mammal_sr_gam$taxa <- "mammals"
  bird_sr_gam$model <- "GAM"
  bird_sr_gam$taxa <- "birds"
  amphi_sr_gbm$model <- "GBM"
  amphi_sr_gbm$taxa <- "amphibians"
  mammal_sr_gbm$model <- "GBM"
  mammal_sr_gbm$taxa <- "mammals"
  bird_sr_gbm$model <- "GBM"
  bird_sr_gbm$taxa <- "birds"
  
  sr_predicted <- rbind(amphi_sr_gam, mammal_sr_gam, bird_sr_gam, amphi_sr_gbm, mammal_sr_gbm, bird_sr_gbm, amphi_sr_rf, mammal_sr_rf, bird_sr_rf)
  rm(amphi_sr_gam, mammal_sr_gam, bird_sr_gam, 
     amphi_sr_gbm, mammal_sr_gbm, bird_sr_gbm,
     amphi_sr_rf, mammal_sr_rf, bird_sr_rf)
  
  readr::write_csv(sr_predicted, paste0("data/sr_predicted_ssdm_1995_", disp, ".csv.xz"))
})


#' ssdm_fut_sr, eval=F
#Load Probabilities

#Set Dispersal
library(dplyr)
lapply(c("presence", "dispersal1", "dispersal2", "dispersal3", "dispersal4", "fulldisp"), function(disp){
  year <- 2080
  taxa <- c("Amphibian", "Mammal", "Bird")
  
  # Predicted SR
  amphi_sr_gam <- read.csv(paste0("data/", taxa[1], "_prob_GAM_", disp, ".csv.xz")) %>% 
    select(c(x,y), matches(paste0(year)))
  amphi_sr_gbm <- read.csv(paste0("data/", taxa[1], "_prob_GBM_", disp, ".csv.xz")) %>% 
    select(c(x,y), matches(paste0(year)))
  mammal_sr_gam <- read.csv(paste0("data/", taxa[2], "_prob_GAM_", disp, ".csv.xz")) %>% 
    select(c(x,y), matches(paste0(year)))
  mammal_sr_gbm <- read.csv(paste0("data/", taxa[2], "_prob_GBM_", disp, ".csv.xz")) %>% 
    select(c(x,y), matches(paste0(year)))
  bird_sr_gam <- read.csv(paste0("data/", taxa[3], "_prob_GAM_", disp, ".csv.xz")) %>% 
    select(c(x,y), matches(paste0(year)))
  bird_sr_gbm <- read.csv(paste0("data/", taxa[3], "_prob_GBM_", disp, ".csv.xz")) %>% 
    select(c(x,y), matches(paste0(year)))
  
  # Merge predicted richness
  amphi_sr_gam$model <- "GAM"
  amphi_sr_gam$taxa <- "amphibians"
  mammal_sr_gam$model <- "GAM"
  mammal_sr_gam$taxa <- "mammals"
  bird_sr_gam$model <- "GAM"
  bird_sr_gam$taxa <- "birds"
  amphi_sr_gbm$model <- "GBM"
  amphi_sr_gbm$taxa <- "amphibians"
  mammal_sr_gbm$model <- "GBM"
  mammal_sr_gbm$taxa <- "mammals"
  bird_sr_gbm$model <- "GBM"
  bird_sr_gbm$taxa <- "birds"

  sr_predicted <- rbind(amphi_sr_gam, mammal_sr_gam, bird_sr_gam, 
                        amphi_sr_gbm, mammal_sr_gbm, bird_sr_gbm,
                        amphi_sr_rf, mammal_sr_rf, bird_sr_rf)
  rm(amphi_sr_gam, mammal_sr_gam, bird_sr_gam, amphi_sr_gbm, 
     mammal_sr_gbm, bird_sr_gbm, amphi_sr_rf, mammal_sr_rf, bird_sr_rf)
  readr::write_csv(sr_predicted, 
            paste0("data/sr_predicted_ssdm_", disp, "_", year, ".csv.xz"))
})

#' load_climate, eval=F
#Load climate predictor combs
climCombs <- list(c("bio4", "bio5", "bio18", "bio19"),
                  c("bio4","bio5","bio12","bio15"),
                  c("bio4", "bio5", "bio12", "bio15")) 
names(climCombs) <- c("amphibians", "mammals", "birds")

## Load climate data and blocks
climVar <- lapply(climCombs, function(x){
  climVar <- read.csv(paste0("data/Blocking_SU_1995_", 
                             paste0(x, collapse="_"), ".csv.xz"))[,c("x","y", x, "block")]
  colnames(climVar) <- c("x","y", x,"block")
  return(climVar)
})
names(climVar) <- c("amphibians", "mammals", "birds")

#' load_species_data, eval=F
#Read SSDM data
sr_observed_ssdm <- read.csv("data/sr_observed_ssdm.csv.xz")

#Turn into list
data_ssdm <- split(x=sr_observed_ssdm, f=sr_observed_ssdm$taxa)

#Read all data
sr_observed_all <- read.csv("data/sr_observed_all.csv.xz")

#Turn into list
data_all <- split(x=sr_observed_all, f=sr_observed_all$taxa)

#' ## Macroecological models

#' mem_eco_mod, eval=F

#Set working directory
setwd("/home/matt/Documents/Github/Biocen1.5_MEM")

#Set file directory
filedir <- "data"

#Set results Path
plotPath_MEM <- filedir
resultsPath_MEM <- filedir

# Create model, family, dataset combinations
model_fam <- c("GAM_poisson", "GBM_poisson")
x <- 1:2
df <- expand.grid(model_fam=model_fam, x=x)

#Load Model function
library(mgcv); library(gbm); library(PresenceAbsence); 
library(dplyr); library(snowfall)
source("R/GAM_func.R"); source("R/GBM_func.R")

#Run model for each algorithm
sfInit(parallel=TRUE, cpus=nrow(df))
sfLibrary(mgcv); sfLibrary(dplyr); sfLibrary(gbm); sfLibrary(PresenceAbsence)
sfExport(list=c("data_ssdm", "data_all", "GAM_eco","GAM_split", 
                "resultsPath_MEM", "filedir", 
                "climCombs", "climVar", "GBM_eco", "GBM_split", 
                "plotPath_MEM", "df")) 
sfLapply(1:nrow(df), function(y){
  model_fam <- df$model_fam[[y]]
  model_type <- strsplit(as.character(model_fam), split="_")[[1]][1]
  family <- strsplit(as.character(model_fam), split="_")[[1]][2]
    
  data <- list(data_ssdm, data_all)[[df$x[[y]]]]
  name <- c("sub", "all")[df$x[[y]]]
  
  #Run model for each taxa
  lapply(c("amphibians", "mammals", "birds"), function(species){
    clim.var <- as.character(unlist(climCombs[[species]]))
    if(!file.exists(paste(resultsPath_MEM, "/", species, "_", name, "_", 
                          paste(clim.var, collapse="_"),
                          "_", model_fam, "_Eco_block.RData",sep=""))){
      ## Get species data
      species.data <- data[[species]]
      spPseudoRep <- na.omit(species.data)
      colnames(spPseudoRep)[1:4] <-c("x", "y", "taxa", "presence")
      spPseudoRep <- merge(spPseudoRep, climVar[[species]],by=c("x","y"), all.x=T)
      spPseudoRep <- dplyr::select(spPseudoRep, x,y,presence, 
                                   dplyr::one_of("block"), 
                                   dplyr::one_of(clim.var))
      spPseudoRep["cell.id"] <- seq(1,nrow(spPseudoRep))
      
      spPseudoRep <- na.omit(spPseudoRep)
      
      ## Model function GAM
      if(model_type == "GAM"){
        if(family == "nb"){family <- nb()}
        GAM_eco(data.model=spPseudoRep, 
                outDir=resultsPath_MEM, 
                species=species,
                PA=paste0(species, "_", name), family=family, 
                clim.var=clim.var,
                fx=FALSE, k=-1, bs="tp",
                blocks=c(1:10))
      } else if(model_type == "GBM"){
        ## Model function GBM
        GBM_eco(data.model=spPseudoRep, 
                outDir=resultsPath_MEM,
                plotPath=plotPath_MEM, eval=FALSE,
                species=species, learn.rate=c(0.01, 0.001),
                PA=paste0(species, "_", name), distribution=family,
                clim.var=clim.var,
                blocks=c(1:10))
      }
    }
  })
})
sfStop()

#' ## Runs predictions for 1995 and 2080!

#' mem_eco_pred, eval=F

# Create model, family, dataset combinations
model_fam <- c("GAM_poisson", "GBM_poisson")
x <- 1:2
df <- expand.grid(model_fam=model_fam, x=x)

#Run model for each algorithm
library(snowfall); library(mgcv); library(dplyr); library(gbm)
sfInit(parallel=TRUE, cpus=nrow(df))
sfLibrary(mgcv); sfLibrary(dplyr); sfLibrary(gbm)
sfExport(list=c("df"))
sfLapply(1:nrow(df), function(y){
  model_fam <- df$model_fam[[y]]
  name <- c("sub", "all")[df$x[[y]]]
  if(!file.exists(paste0("data/sr_predicted_mem_", name, "_", model_fam, 
                         "_eco_2080.csv.xz"))){
    # Read models into R
    m1 <- get(load(paste0("data/amphibians_", name,
                          "_bio4_bio5_bio18_bio19_", 
                          model_fam, "_Eco_block.RData")))
    m2 <- get(load(paste0("data/mammals_", name, "_bio4_bio5_bio12_bio15_", 
                          model_fam,"_Eco_block.RData")))
    m3 <- get(load(paste0("data/birds_", name, "_bio4_bio5_bio12_bio15_", 
                          model_fam, "_Eco_block.RData")))
    
    if(!file.exists(paste0("data/sr_predicted_mem_", name, "_", model_fam, 
                           "_eco_1995.csv.xz"))){
      # Run current predictions
      load("data/bioclim_EWEMBI_1995_landonly.rda")
      
      # Predict function depends on mgcv library
      pred <- lapply(list(m1, m2, m3), function(model){
        pred <- lapply(1:length(model), function(x){
          predict <- round(as.numeric(
            predict(model[[x]]$mod, newdata=bioclim_EWEMBI_1995_landonly, 
                    type="response",se.fit=FALSE)),4)
          predict <- as.data.frame(cbind(
            bioclim_EWEMBI_1995_landonly[,c("x","y")],predict))
          colnames(predict) <- c("x", "y", x)
          return(predict)
        })
        Reduce(function(...) merge(..., by=c("x","y"), all.x=T), pred)
      })
      
      # Calculate mean prediction
      pred[[1]]$mean <- rowMeans(pred[[1]][,c(3:12)])
      pred[[2]]$mean <- rowMeans(pred[[2]][,c(3:12)])
      pred[[3]]$mean <- rowMeans(pred[[3]][,c(3:12)])
      
      # Merge predicted richness
      pred[[1]]$model <- model_fam
      pred[[1]]$taxa <- "amphibians"
      pred[[2]]$model <- model_fam
      pred[[2]]$taxa <- "mammals"
      pred[[3]]$model <- model_fam
      pred[[3]]$taxa <- "birds"
      sr_predicted <- do.call("rbind", pred)
      readr::write_csv(sr_predicted, 
                       paste0("data/sr_predicted_mem_", name, "_", 
                              model_fam, "_eco_1995.csv.xz")); rm(pred)
    }
    
    # Select climate data
    climatenames<- c("data/bioclim_GFDL-ESM2M_rcp26_2080_landonly.rda",
                     "data/bioclim_GFDL-ESM2M_rcp60_2080_landonly.rda", 
                     "data/bioclim_HadGEM2-ES_rcp26_2080_landonly.rda", 
                     "data/bioclim_HadGEM2-ES_rcp60_2080_landonly.rda", 
                     "data/bioclim_IPSL-CM5A-LR_rcp26_2080_landonly.rda", 
                     "data/bioclim_IPSL-CM5A-LR_rcp60_2080_landonly.rda", 
                     "data/bioclim_MIROC5_rcp26_2080_landonly.rda", 
                     "data/bioclim_MIROC5_rcp60_2080_landonly.rda")
    
    # Turn data into list
    climatedata <- lapply(climatenames, function(x){
      data <- get(load(x))
      dplyr::select(data, "x", "y", "bio4", "bio5", 
                    "bio12", "bio15", "bio18", "bio19")
    })
    
    # Predict function depends on mgcv library
    sr_predicted <- lapply(climatedata, function(data){
      pred <- lapply(list(m1, m2, m3), function(model){
        pred <- lapply(1:length(model), function(x){
          predict <- round(as.numeric(
            predict(model[[x]]$mod, newdata=data, 
                    type="response",se.fit=FALSE)), 4)
          predict <- as.data.frame(cbind(data[,c("x","y")],predict))
          colnames(predict) <- c("x", "y", x)
          return(predict)
        })
        Reduce(function(...) merge(..., by=c("x","y"), all.x=T), pred)
      })
      # Calculate mean prediction
      pred[[1]]$mean <- rowMeans(pred[[1]][,c(3:12)])
      pred[[2]]$mean <- rowMeans(pred[[2]][,c(3:12)])
      pred[[3]]$mean <- rowMeans(pred[[3]][,c(3:12)])
      
      # Merge predicted richness
      pred[[1]]$model <- model_fam
      pred[[1]]$taxa <- "amphibians"
      pred[[2]]$model <- model_fam
      pred[[2]]$taxa <- "mammals"
      pred[[3]]$model <- model_fam
      pred[[3]]$taxa <- "birds"
      sr_predicted <- do.call("rbind", pred)
      return(sr_predicted)
    })
    
    # Change column names
    sr_predicted <- lapply(1:length(sr_predicted), function(x){
      data <- sr_predicted[[x]] %>% select(x,y,mean,model,taxa)
      data$gcm <- paste0(
        strsplit(climatenames[x], split="_")[[1]][2:4], collapse="_")
      data <- tidyr::spread(data, gcm, mean)
      return(data)
    })
    
    # Merge data
    sr_predicted <- Reduce(function(...) dplyr::left_join(..., by=c("x","y", "model", "taxa"), all.x=TRUE), sr_predicted)
    
    rm(m1, m2, m3)
    readr::write_csv(sr_predicted, paste0("data/sr_predicted_mem_", 
                                          name, "_", model_fam, 
                                          "_eco_2080.csv.xz"))
  } else{
    NULL
  }
})
sfStop()
