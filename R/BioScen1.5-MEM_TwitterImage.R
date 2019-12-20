#' Load packages
library(dplyr); library(magrittr); library(ggplot2); 
library(ggpmisc)

# Install patchwork from GitHub
if(!"patchwork" %in% installed.packages()[,"Package"]) devtools::install_github("thomasp85/patchwork", dependencies=T)
library(patchwork)

#' Create ggplot2 themes
theme_map <- function(base_size = 10, base_family = "") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), axis.title = element_blank(), panel.border = element_blank(),
          plot.title = element_text(size=10, face="bold", hjust=0.5),
          strip.background = element_blank(),
          plot.tag.position = c(0.05,0.9),
          strip.text = element_text(size=10, face="bold"))
}

theme_plot <- function(base_size = 10, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.grid = element_blank(),
          plot.title = element_text(size=12, face="bold", vjust=1, hjust=0.5, 
                                    margin = margin(t = 0, r = 0, b = 10, l = 0)),
          axis.title.x = element_text(size=12, face="bold", 
                                      margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.y = element_blank(), 
          panel.spacing.y = unit(0., "lines"))}

# Define standard colour scheme
bluered <- colorRampPalette(c("white", "#00007F", "blue", "#007FFF", "cyan", 
                              "#7FFF7F", "yellow", "#FF7F00", "red", 
                              "#7F0000"))(255)

# Load world outline
load("data/outline.rda")

df_transform <- function(data, coords=c("x","y"), crs.in=sp::CRS("+init=epsg:4326"), crs.out=NA){
    data <- raster::rasterFromXYZ(data)
    raster::projection(data) <- crs.in
    data <- raster::projectRaster(data, crs=crs.out)
    as.data.frame(raster::rasterToPoints(data))
}

## Twitter image  

sr_observed <- read.csv("data/sr_observed_ssdm.csv.xz")
sr_observed$taxa <- factor(sr_observed$taxa, 
                           labels=c("Amphibians", "Birds", "Mammals"))

sr_ssdm <- read.csv("data/sr_predicted_ssdm_1995_dispersal1.csv.xz")
sr_ssdm$taxa <- factor(sr_ssdm$taxa, 
                       labels=c("Amphibians", "Birds", "Mammals"))

sr_mem <- rbind(read.csv("data/sr_predicted_mem_sub_GAM_poisson_eco_1995.csv.xz"),
                read.csv("data/sr_predicted_mem_sub_GBM_poisson_eco_1995.csv.xz")) %>%
  dplyr::select(x,y,mean,model,taxa)
colnames(sr_mem) <- c("x", "y", "mn", "model", "taxa")
sr_mem$model <- factor(sr_mem$model, labels=c("GAM", "GBM"))
sr_mem$taxa <- factor(sr_mem$taxa, 
                      labels=c("Amphibians", "Birds", "Mammals"))

sr_mem_ssdm <- inner_join(sr_ssdm, sr_mem)
colnames(sr_ssdm)[3] <- "mn"
sr_ssdm$type <- "S-SDM"
sr_mem$type <- "MEM"
sr_obs_pred <- rbind(inner_join(sr_observed, sr_ssdm), 
                     inner_join(sr_observed, sr_mem)) %>% 
  data.frame()
colnames(sr_obs_pred) <- c("x", "y", "taxa", "sr", "mn", "model", "type")
rm(sr_ssdm, sr_mem)

## Create ensemble mean
sr_obs_pred %<>% group_by(x, y, model, type) %>% 
  dplyr::summarise(mn = sum(mn, na.rm=T),
                   sr = sum(sr, na.rm=T)) %>% 
  group_by(x,y,type) %>%
  dplyr::summarise(mn = mean(mn, na.rm=T),
                   sum = mean(sr, na.rm=T)) %>% data.frame()

colnames(sr_mem_ssdm)[6] <- "mn"
sr_mem_ssdm %<>% group_by(x,y,model) %>% 
  dplyr::summarise(mn = sum(mn, na.rm=T),
                   EWEMBI_1995 = sum(EWEMBI_1995, na.rm=T)) %>% group_by(x,y) %>%
  dplyr::summarise(mn = mean(mn, na.rm=T),
                   EWEMBI_1995 = mean(EWEMBI_1995, na.rm=T)) %>% data.frame()

# Load data
sr_future_ssdm <- read.csv("data/sr_predicted_ssdm_dispersal1_2080.csv.xz")

# Calculate ensemble among different GCMs for RCP2.6
sr_future_ssdm <- sr_future_ssdm %>% 
  dplyr::select(c(x,y,model), matches("rcp26")) %>% 
  group_by(x,y,model) %>% summarise_all(sum) %>%
  ungroup %>% mutate(mn = rowMeans(.[,4:7], na.rm=TRUE)) %>% 
  dplyr::select(x,y,model,mn)

## Create ensemble mean among GAM & GBM
sr_future_ssdm %<>% group_by(x,y) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>%
  data.frame()
sr_future_ssdm$type <- "S-SDM"

sr_future_mem <- rbind(
  read.csv("data/sr_predicted_mem_sub_GAM_poisson_eco_2080.csv.xz"),
  read.csv("data/sr_predicted_mem_sub_GBM_poisson_eco_2080.csv.xz"))
sr_future_mem$model <- factor(sr_future_mem$model, labels=c("GAM", "GBM"))

# Calculate ensemble among different GCMs for RCP2.6
sr_future_mem %<>% 
  dplyr::select(c(x,y,model), matches("rcp26")) %>% 
  group_by(x,y,model) %>% summarise_all(sum) %>%
  ungroup() %>% mutate(mn = rowMeans(.[,4:7], na.rm=TRUE)) %>% 
  dplyr::select(x,y,model,mn)

## Create ensemble mean among GAM and GBM
sr_future_mem %<>% group_by(x,y) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>%
  data.frame()
sr_future_mem$type <- "MEM"

sr_future <- rbind(sr_future_ssdm, sr_future_mem)

sr_cor <- sr_future %>% tidyr::spread(type, mn) %>% df_transform(crs.out=sp::CRS("+init=esri:54009"))

outline <- sp::spTransform(outline, sp::CRS("+init=esri:54009"))

p1 <- sr_obs_pred %>% filter(type == "S-SDM") %>% 
  df_transform(crs.out=sp::CRS("+init=esri:54009")) %>%
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + labs(y="S-SDM richness", x="", title="Current") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0,915), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(strip.text = element_blank(), axis.text.y = element_blank(),
                      axis.title = element_text(size=12, face="bold", angle=90), 
                      legend.position="none") + 
  coord_equal() + lims(x=c(-17572590,17851410), y=c(-6473255,8740645))
p2 <- sr_obs_pred %>% filter(type == "MEM") %>% 
  df_transform(crs.out=sp::CRS("+init=esri:54009")) %>%
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + labs(y="MEM richness", x="") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0,915), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(strip.text = element_blank(),
                      axis.text.y = element_blank(), legend.position="none",
                      axis.title = element_text(size=12, face="bold", angle=90)) + 
  coord_equal() + lims(x=c(-17572590,17851410), y=c(-6473255,8740645))
p3 <- sr_mem_ssdm %>% df_transform(crs.out=sp::CRS("+init=esri:54009")) %>%
  ggplot(aes(x=mn, y=EWEMBI_1995)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label =..eq.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.95, parse=TRUE, colour="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.85, parse=TRUE, colour="#000000") + 
  scale_x_continuous("MEM richness", limits=c(0,915), expand=c(0,0)) + 
  scale_y_continuous("S-SDM richness", limits=c(0,915), expand=c(0,0)) + 
  theme_plot() + theme(axis.title.y=element_text(face="bold", angle=90, size=12,
                                                 margin = margin(t = 0, r = 10, b = 0, l = 0))) + 
  coord_equal()

p4 <- sr_future %>% filter(type=="S-SDM") %>% 
  df_transform(crs.out=sp::CRS("+init=esri:54009")) %>%
  ggplot() +  geom_hline(yintercept=0, colour="grey40", 
                         linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + labs(title="Future") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) +  
  scale_fill_gradientn(name="", limits=c(0, 915), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(legend.key.width=unit(0.3,"cm"), axis.text.x = element_blank(),
                      axis.title = element_blank()) +
  coord_equal() + lims(x=c(-17572590,17851410), y=c(-6473255,8740645))
p5 <- sr_future %>% filter(type=="MEM") %>% 
  df_transform(crs.out=sp::CRS("+init=esri:54009")) %>%
  ggplot() +  geom_hline(yintercept=0, colour="grey40", 
                         linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, 915), colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(axis.text = element_blank(),
                      legend.key.width=unit(0.3,"cm")) + 
  coord_equal() + lims(x=c(-17572590,17851410), y=c(-6473255,8740645))
p6 <- ggplot(data=sr_cor, aes(x=MEM, y=`S.SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label =..eq.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.95, parse=TRUE, colour="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.85, parse=TRUE, colour="#000000") + 
  scale_x_continuous("MEM richness", limits=c(0,915), expand=c(0,0)) + 
  scale_y_continuous("S-SDM richness", limits=c(0,915), expand=c(0,0)) + 
  theme_plot() + theme(axis.title.y=element_text(face="bold", angle=90, size=12,
                                                 margin = margin(t = 0, r = 10, b = 0, l = 0))) + 
  coord_equal()

p <- p1 + p4 + p2 + p5 + p3 + p6 + plot_layout(nrow=3)
ggsave("figures/twitter_image.png", p, dpi = 300, width=11, height=7)
