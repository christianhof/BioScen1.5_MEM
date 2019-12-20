#' ---
#' title: "R-Code for creating MEM Output"
#' author: "Matthias F. Biber"
#' date: "03.06.2019"
#' ---

# Create model, family combinations
model_fam <- c("GAM_poisson", "GBM_poisson")

#Run model for each model_fam combination
library(snowfall)
sfInit(parallel=TRUE, cpus=2)
sfLibrary(mgcv); sfLibrary(dplyr); sfLibrary(gbm)
sfLapply(model_fam, function(model_fam){
  climdir <- "/media/matt/Data/Documents/Wissenschaft/Data/ISIMIP2b/ClimateData/"
  name <- "sub"
  # Read models into R
  m1 <- get(load(paste0("data/amphibians_", name, "_bio4_bio5_bio18_bio19_", 
                        model_fam, "_Eco_block.RData")))
  m2 <- get(load(paste0("data/mammals_", name, "_bio4_bio5_bio12_bio15_", 
                        model_fam,"_Eco_block.RData")))
  m3 <- get(load(paste0("data/birds_", name, "_bio4_bio5_bio12_bio15_", 
                        model_fam, "_Eco_block.RData")))
  
  # Run predictions for individual years
  year <- c("1845", "1990", "1995", "2009", "2010", "2020",
            "2026", "2032", "2048", "2050", "2052", "2056", "2080",
            "2100", "2150", "2200", "2250")
  lapply(year, function(year){
    if(!file.exists(paste0("data/sr_predicted_mem_", name, "_", model_fam, 
                           "_eco_", year, ".csv.xz"))){
      if(year == "1995"){
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
                                model_fam, "_eco_", year, ".csv.xz")); rm(pred)
      } else{
        # Select climate data
        climatenames <- list.files(climdir, pattern=year, full.names=T)
        
        # Turn data into list
        climatedata <- lapply(climatenames, function(x){
          readr::read_csv(x) %>% 
            dplyr::select("x", "y", "bio4", "bio5", "bio12", "bio15", "bio18", "bio19")
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
        readr::write_csv(sr_predicted, paste0("data/sr_predicted_mem_", 
                                              name, "_", model_fam, 
                                              "_eco_", year, ".csv.xz"))
      }
    }
  })
  rm(m1, m2, m3); gc()
})
sfStop()

########################################

# Save summed probability to .nc file

# Empty R environment
rm(list=ls())

# Load required packages
packages <- c("dplyr", "snowfall", "tidyr", "ncdf4", "readr")
l <- sapply(packages, require, character.only = TRUE); rm(packages, l)

# Set file directory
filedir <- getwd()

# Model type
k <- 2; model_type <- c("GAM_poisson", "GBM_poisson")[k]

# Taxa
taxa <- c("amphibians", "birds", "mammals")

# Set up NetCDF file combinations
df <- expand.grid(rcp=c("rcp26", "rcp60", "historical", "piControl"), 
                  model=c("MIROC5", "HadGEM2.ES", "IPSL.CM5A.LR", "GFDL.ESM2M"))
df <- rbind(expand.grid(rcp="1995", model="EWEMBI"), df)
df <- rbind(df, expand.grid(rcp="2100rcp26", 
                            model=c("MIROC5", "HadGEM2.ES", "IPSL.CM5A.LR")))
df <- df[11,]

library(snowfall)
#sfInit(parallel=TRUE, cpus=8)
#sfLibrary(dplyr); sfLibrary(tidyr); sfLibrary(ncdf4)
#sfExport(list=c("filedir", "taxa", "model_type", "df"))
sfLapply(1:nrow(df), function(rcp_mod){
  #Define years
  if(df$rcp[rcp_mod] == "1995"){
    year <- 1995
    filenames <- sapply(taxa, function(i){
      paste0(filedir, "/bioscen1.5-mem-", sub("_poisson", "", tolower(model_type)),
             "_ewembi_nobc_hist_nosoc_co2_", sub("s", "", tolower(i)), 
             "sr_global_30year-mean_", min(year), "_", max(year), ".nc4")})
  } else{
    rcp <- df$rcp[rcp_mod]
    if(df$rcp[rcp_mod] == "historical"){
      year <- 1990
    } else if(df$rcp[rcp_mod] == "piControl"){
      year <- 1845
    } else if(df$rcp[rcp_mod] == "rcp26"){
      year <- c(2009, 2010, 2020, 2026, 2032, 2048, 2050, 2052, 2056, 2080)
    } else if(df$rcp[rcp_mod] == "rcp60"){
      year <- c(2009, 2010, 2020, 2026, 2032, 2048, 2050, 2052, 2056, 2080)
    } else if(df$rcp[rcp_mod] == "2100rcp26"){
      year <- c(2100, 2150, 2200, 2250)
      rcp <- "rcp26"
    }
    filenames <- sapply(taxa, function(i){
      paste0(filedir, "/bioscen1.5-mem-", sub("_poisson", "", tolower(model_type)), "_",
             tolower(gsub("[.]", "-", df$model[rcp_mod])), 
             "_ewembi_", rcp, "_nosoc_co2_", sub("s", "", tolower(i)), 
             "sr_global_30year-mean_", min(year), "_", max(year), ".nc4")})
  }
  
  # Define the dimensions
  dimX = ncdim_def(name="lon", units="degrees", vals=seq(-179.75, 179.75, length = 720))
  dimY = ncdim_def(name="lat", units="degrees", vals=seq(89.75, -89.75, length = 360))
  dimT = ncdim_def(name="time", units="years since 1661-1-1 00:00:00", 
                   vals=c(year-1661), calendar="standard")
  
  # Define data for NetCDF file
  vard <- ncvar_def("sr", "Species richness per cell", 
                    list(dimX,dimY,dimT), 1.e+20, prec="double", compression=9)
  
  # Create the NetCDF file
  nc <- lapply(filenames, function(x) nc_create(x, vard))
  lapply(nc, function(y) ncatt_put(y, varid=0, attname="contact", attval="Matthias Biber <matthias.biber@tum.de>"))
  lapply(nc, function(y) ncatt_put(y, varid=0, attname="institution", attval="Technical University Munich (Germany)"))
  
  # Read data
  if(any(year == 1995)){
    predData <- read.csv(paste0("data/sr_predicted_mem_sub_", model_type, 
                                       "_eco_1995.csv.xz")) %>% 
      select(x,y,mean,taxa)
    colnames(predData[,c("mean")]) <- "EWEMBI_1995"
  } else if(any(year %in% c(1845, 1990))){
    predData <- read.csv(paste0("data/sr_predicted_mem_sub_", model_type, 
                                       "_eco_", year, ".csv.xz"))
    predData <- predData %>% dplyr::select(x, y, taxa, matches(paste(df$model[rcp_mod], 
                                                                     df$rcp[rcp_mod], sep="_")))
  } else{
    predData <- lapply(year, function(x){
      read.csv(paste0("data/sr_predicted_mem_sub_", model_type, 
                             "_eco_", x, ".csv.xz"))
    })
    predData <- Reduce(function(...) dplyr::left_join(..., by=c("x","y", "model", "taxa"), all.x=TRUE), predData)
    
    ## Select all data for one model_rcp combination
    if(df$rcp[rcp_mod] == "2100rcp26"){
      predData <- predData %>% select(x,y, taxa, `HadGEM2.ES_rcp26_2100`:`MIROC5_rcp26_2250`) %>%
        dplyr::select(x, y, taxa, matches(paste(df$model[rcp_mod], "rcp26", sep="_")))
    } else{
      predData <- predData %>% select(x,y, taxa, `GFDL.ESM2M_rcp26_2009`:`MIROC5_rcp60_2080`) %>%
        dplyr::select(x, y, taxa, matches(paste(df$model[rcp_mod], 
                                                df$rcp[rcp_mod], sep="_")))
    }
  }
  
  lapply(1:length(taxa), function(k){
    # Turn data into array
    dat <- predData %>% filter(taxa == taxa[[k]]) %>% 
      dplyr::select(-taxa)
    
    # Expand dataframe with NAs
    df_spat <- expand.grid(x=seq(-179.75, 179.75, length = 720),
                           y=seq(89.75, -89.75, length = 360))
    dat <- left_join(df_spat, dat) %>% select(-x,-y); rm(df_spat)
    
    colnames(dat) <- year
    data <- array(unlist(dat), dim=c(720, 360, ncol(dat)), 
                  dimnames=list(NULL, NULL, colnames(dat)))
    
    # Write data to the NetCDF file
    ncvar_put(nc[[k]], vard, data, start=c(1,1,1), count=c(-1,-1,-1))
  })
  
  # Close your file to finish writing
  lapply(nc, function(y) nc_close(y)); rm(nc)
})
sfStop()

########################################

# Test NetCDF file
library(raster); library(ncdf4)
filedir <- "/home/matt/Documents/Github/BioScen1.5_MEM/"
files <- list.files(filedir, pattern = ".nc", full.names=T)
for(i in files){
  test <- stack(i)
  print(nlayers(test))
  plot(test[[4]])
}
