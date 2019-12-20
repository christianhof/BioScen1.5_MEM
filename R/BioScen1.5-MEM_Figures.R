#' ---
#' title: "Script for creating main and SI figures of MEM paper"
#' author: "Matthias F. Biber"
#' date: "16.06.2019"
#' ---

#' Load packages
library(dplyr); library(magrittr); library(ggplot2); library(ggdendro); library(ggpmisc); library(ModelMetrics)

#' **Note:** Don't load the raster package as the select function conflicts 
#' with the select function of the dplyr package

# Install patchwork from GitHub
if(!"patchwork" %in% installed.packages()[,"Package"]) devtools::install_github("thomasp85/patchwork", dependencies=T)
library(patchwork)

####################

### Supporting Figure 1 

#-#-# Summarize frequencies and plot as stacked bar chart #-#-#

AUC_amp <- read.csv(paste0("data/FinalRank_Amphibian.csv.xz"))
AUC_amp$Taxa <- "Amphibians"
AUC_birds <- read.csv(paste0("data/FinalRank_Bird.csv.xz"))
AUC_birds$Taxa <- "Birds"
AUC_mam <- read.csv(paste0("data/FinalRank_Mammal.csv.xz"))
AUC_mam$Taxa <- "Mammals"
AUCall <- rbind(AUC_amp, AUC_birds, AUC_mam) 

#-#-# Select the ten best variable combinations to display in graph #-#-#
AUCallTop <- AUCall ## Add data here
AUCallTop$rank[AUCallTop$rank >= 4] <- 0
AUCallTop <- AUCallTop[,c("Taxa", "Species","Models","rank")]

AUCTopTable<-data.frame(table(AUCallTop$Taxa, 
                              AUCallTop$Models, AUCallTop$rank))
colnames(AUCTopTable) <- c("Taxa", "ClimateVariable", "Rank","Frequency")

AUCTopTable$Rank <- as.numeric(as.character(AUCTopTable$Rank))
AUCTopTable$Frequency <- as.numeric(as.character(AUCTopTable$Frequency))
AUCTopTable <- AUCTopTable %>% filter(Rank >= 1) %>% group_by(ClimateVariable, Taxa) %>% summarise(Frequency = sum(Frequency))
AUCTopTable <- AUCTopTable[order(-AUCTopTable$Frequency),]
AUCTopVariables <- AUCTopTable[1:10,]

TopVariableList <- as.vector(AUCTopVariables$ClimateVariable)

#-#-# Subset the entire results data frame 
# choosing only the best variables #-#-#
AUCall$rank[AUCall$rank >= 10] <- "Other"
AUCSub <- AUCall[,c("Taxa", "Species","Models","rank")]
AUCFreqTable<-data.frame(table(AUCSub$Taxa, AUCSub$Models, AUCSub$rank))
colnames(AUCFreqTable) <- c("Taxa", "ClimateVariable","Rank","Frequency")

AUCallFinal <- AUCFreqTable[AUCFreqTable$ClimateVariable %in% TopVariableList, ]
AUCallFinal <- subset(AUCallFinal, Frequency > 0)

#-#-# Set colour scheme #-#-#
PaletteBlue2 <-c('blue4','dodgerblue4','deepskyblue','gray20',
                 'gray28','gray39','gray49','gray53','gray63','gray73')

#-#-# Plot the variables #-#-#
ggplot(AUCallFinal, aes(x = ClimateVariable, y = Frequency)) +
  facet_grid(Taxa ~ ., scales="free", switch="y") + 
  geom_bar(aes(fill = Rank), stat="identity") +
  scale_fill_manual(values=PaletteBlue2)+
  scale_y_continuous(name="Number of species", expand=c(0,0), limits=c(0,NA)) + 
  scale_x_discrete(name="") + 
  guides(fill = guide_legend(ncol = 1)) + theme_bw() + 
  theme(strip.background = element_blank(),
        plot.title = element_text(lineheight=2, face="bold",hjust = 0),
        axis.text=element_text(size=10, colour="black"), 
        axis.title=element_text(size=12, face="bold"),
        strip.placement="outside",
        strip.text = element_text(size=10),
        axis.line=element_line(colour="black"),
        axis.text.x=element_text(angle = 90, hjust = 0))
ggsave("figures/auc_varsel-1.png", width=6, height=7, dpi=600)

####################

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
bluewhitered <- rev(colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                       "white", "yellow", "#FF7F00", "red", 
                       "#7F0000"))(255))

# Create world outline and save to file
#data(countriesHigh, package="rworldxtra")
#countriesHigh <- rgeos::gPolygonize(rgeos::gNode(as(countriesHigh, "SpatialLines")))
#outline <- rgeos::gUnaryUnion()
#save(outline, file="data/outline.rda", compress="xz")

# Load world outline
load("data/outline.rda")

####################

### Figure 1 (Scatterplot observed vs. richness)

# Merge predicted and observed richness
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
rm(sr_ssdm, sr_mem)

## Create ensemble mean
ensemble <- sr_obs_pred %>% group_by(x, y, taxa, type) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T),
                   sum = mean(sum, na.rm=T)) %>% data.frame()
ensemble$model <- "Ensemble"
sr_obs_pred <- rbind(sr_obs_pred, ensemble); rm(ensemble)
sr_obs_pred$model <- factor(sr_obs_pred$model, 
                             levels=c("GAM", "GBM", "Ensemble"))

colnames(sr_mem_ssdm)[6] <- "mn"
ensemble <- sr_mem_ssdm %>% group_by(x,y,taxa) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T),
                   EWEMBI_1995 = mean(EWEMBI_1995, na.rm=T)) %>% data.frame()
ensemble$model <- "Ensemble"
sr_mem_ssdm <- rbind(sr_mem_ssdm, ensemble); rm(ensemble)
sr_mem_ssdm$model <- factor(sr_mem_ssdm$model, 
                            levels=c("GAM", "GBM", "Ensemble"))

# Plot observed vs. predicted species richness
p1 <- sr_obs_pred %>% filter(model=="Ensemble", taxa=="Amphibians", type=="MEM") %>% 
  ggplot(aes(x=sum, y=mn)) + geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  labs(tag="(a)", title="Amphibians") + 
  stat_poly_eq(aes(label =..eq.label..), coef.digits = 2, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.85, parse=TRUE, colour="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits = 2, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.75, parse=TRUE, colour="#000000") + 
  scale_x_continuous("", limits=c(0,152), expand=c(0,0)) + 
  scale_y_continuous("MEM richness", limits=c(0,152), 
                     expand=c(0,0)) + coord_equal() + theme_plot() + 
  theme(plot.tag.position = c(0.25,0.85),
        axis.title.y = element_text(size=12, face="bold", angle=90, vjust=1, 
                                    hjust=0.5, margin = margin(t = 0, r = 10, b = 0, l = 0)))
p2 <- sr_obs_pred %>% filter(model=="Ensemble", taxa=="Birds", type=="MEM") %>% 
  ggplot(aes(x=sum, y=mn)) + geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  labs(tag="(b)", title="Birds") + 
  stat_poly_eq(aes(label =..eq.label..), coef.digits = 2, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.85, parse=TRUE, colour="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits = 2, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.75, parse=TRUE, colour="#000000") + 
  scale_x_continuous("", limits=c(0,877), expand=c(0,0)) + 
  scale_y_continuous("", limits=c(0,877), 
                     expand=c(0,0)) + theme_plot() + coord_equal() + 
  theme(plot.tag.position = c(0.175,0.85))
p4 <- sr_obs_pred %>% filter(model=="Ensemble", taxa=="Amphibians", type=="S-SDM") %>% 
  ggplot(aes(x=sum, y=mn)) + geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  labs(tag="(d)", ylab="") + 
  stat_poly_eq(aes(label =..eq.label..), coef.digits = 2, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.85, parse=TRUE, colour="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits = 2, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.75, parse=TRUE, colour="#000000") + 
  scale_x_continuous("EOO-based richness", limits=c(0,152), expand=c(0,0)) + 
  scale_y_continuous("S-SDM richness", limits=c(0,152), 
                     expand=c(0,0)) + coord_equal() + theme_plot() +
  theme(plot.tag.position = c(0.25,0.95),
        axis.title.y = element_text(size=12, face="bold", angle=90, vjust=1, 
                                    hjust=0.5, margin = margin(t = 0, r = 10, b = 0, l = 0)))
p5 <- sr_obs_pred %>% filter(model=="Ensemble", taxa=="Birds", type=="S-SDM") %>% 
  ggplot(aes(x=sum, y=mn)) + geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + labs(tag="(e)") + 
  stat_poly_eq(aes(label =..eq.label..), coef.digits = 2, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.85, parse=TRUE, colour="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits = 2, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.75, parse=TRUE, colour="#000000") + 
  scale_x_continuous("EOO-based richness", limits=c(0,877), expand=c(0,0)) + 
  scale_y_continuous("", limits=c(0,877), expand=c(0,0)) + 
  theme_plot() + coord_equal() + 
  theme(plot.tag.position = c(0.175,0.95))
p7 <- sr_mem_ssdm %>% filter(model == "Ensemble", taxa == "Amphibians") %>% 
  ggplot(aes(x=mn, y=EWEMBI_1995)) + geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + labs(tag="(g)") + 
  stat_poly_eq(aes(label =..eq.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.85, parse=TRUE, colour="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.75, parse=TRUE, colour="#000000") + 
  scale_x_continuous("MEM richness", limits=c(0,152), expand=c(0,0)) + 
  scale_y_continuous("S-SDM richness", limits=c(0,152), expand=c(0,0)) + 
  theme_plot() + coord_equal() + 
  theme(plot.tag.position = c(0.25,0.95),
        axis.title.y = element_text(size=12, face="bold", angle=90, vjust=1, 
                                    hjust=0.5, margin = margin(t = 0, r = 10, b = 0, l = 0)))
p8 <- sr_mem_ssdm %>% filter(model == "Ensemble", taxa == "Birds") %>% 
  ggplot(aes(x=mn, y=EWEMBI_1995)) + geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + labs(tag="(h)") + 
  stat_poly_eq(aes(label =..eq.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.85, parse=TRUE, colour="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.75, parse=TRUE, colour="#000000") + 
  scale_x_continuous("MEM richness", limits=c(0,877), expand=c(0,0)) + 
  scale_y_continuous("", limits=c(0,877), 
                     expand=c(0,0)) + theme_plot() + 
  coord_equal() + theme(plot.tag.position = c(0.175,0.95))

p3 <- sr_obs_pred %>% filter(model=="Ensemble", taxa=="Mammals", type=="MEM") %>% 
  ggplot(aes(x=sum, y=mn)) + geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  labs(tag="(c)", title="Mammals") + 
  stat_poly_eq(aes(label =..eq.label..), coef.digits = 2, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.85, parse=TRUE, colour="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits = 2, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.75, parse=TRUE, colour="#000000") + 
  scale_x_continuous("", limits=c(0,252), expand=c(0,0)) + 
  scale_y_continuous("", limits=c(0,252), expand=c(0,0)) + theme_plot() + 
  theme(plot.tag.position = c(0.175,0.85)) + coord_equal()
p6 <- sr_obs_pred %>% filter(model=="Ensemble", taxa=="Mammals", type=="S-SDM") %>% 
  ggplot(aes(x=sum, y=mn)) + geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + labs(tag="(f)") +
  stat_poly_eq(aes(label =..eq.label..), coef.digits = 2, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.85, parse=TRUE, colour="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits = 2, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.75, parse=TRUE, colour="#000000") + 
  scale_x_continuous("EOO-based richness", limits=c(0,252), expand=c(0,0)) + 
  scale_y_continuous("", limits=c(0,252), expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(0.175,0.95)) + coord_equal()
p9 <- sr_mem_ssdm %>% filter(model == "Ensemble", taxa == "Mammals") %>% 
  ggplot(aes(x=mn, y=EWEMBI_1995)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + labs(tag="(i)") + 
  stat_poly_eq(aes(label =..eq.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.85, parse=TRUE, colour="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits = 3, 
               formula = y ~ x, label.x.npc = 0.05, 
               label.y.npc = 0.75, parse=TRUE, colour="#000000") + 
  scale_x_continuous("MEM richness", limits=c(0,252), expand=c(0,0)) + 
  scale_y_continuous("", limits=c(0,252), expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(0.175,0.95)) + coord_equal()
p <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(ncol=3)
ggsave("figures/cor_ssdm_mem_disp1-1.png", p, width=9, height=9, dpi=600)
ggsave("figures/Figure1.pdf", p, width=9, height=9)
rm(p1, p2, p3, p4, p5, p6, p7, p8, p9, p)

####################

### Figure 2 (Map of observed SR, residuals and best fit)

# Observed - predicted
sr_obs_pred$dif <- sr_obs_pred$sum - sr_obs_pred$mn
sr_new <- sr_obs_pred %>% filter(model == "Ensemble") %>% 
  dplyr::select(x, y, taxa, type, dif) %>% group_by(x,y,taxa) %>% 
  tidyr::spread(type, dif) %>% tidyr::drop_na()

max_lim1 <- round(max(sr_obs_pred$dif[sr_obs_pred$model == "Ensemble" & sr_obs_pred$taxa=="Amphibians"], na.rm=T), digits=-1)
max_lim2 <- round(max(sr_obs_pred$dif[sr_obs_pred$model == "Ensemble" & sr_obs_pred$taxa=="Birds"], na.rm=T), digits=-1)
max_lim3 <- round(max(sr_obs_pred$dif[sr_obs_pred$model == "Ensemble" & sr_obs_pred$taxa=="Mammals"], na.rm=T), digits=-1)
min_lim1 <- round(min(sr_obs_pred$dif[sr_obs_pred$model == "Ensemble" & sr_obs_pred$taxa=="Amphibians"], na.rm=T), digits=-1)
min_lim2 <- round(min(sr_obs_pred$dif[sr_obs_pred$model == "Ensemble" & sr_obs_pred$taxa=="Birds"], na.rm=T), digits=-1)
min_lim3 <- round(min(sr_obs_pred$dif[sr_obs_pred$model == "Ensemble" & sr_obs_pred$taxa=="Mammals"], na.rm=T), digits=-1)
col_val1 <- scales::rescale(unique(c(seq(min_lim1, 0, length=5), seq(0, max_lim1, length=5))))
col_val2 <- scales::rescale(unique(c(seq(min_lim2, 0, length=5), seq(0, max_lim2, length=5))))
col_val3 <- scales::rescale(unique(c(seq(min_lim3, 0, length=5), seq(0, max_lim3, length=5))))

p1 <- ggplot() + geom_hline(yintercept=0, colour="grey40", 
                            linetype="dashed", size=0.25) + 
  geom_tile(data=sr_new[sr_new$taxa == "Amphibians",], 
            aes(x=x, y=y, fill=`MEM`)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colors=bluewhitered,
                       limits=c(min_lim1, max_lim1), values=col_val1) + 
  labs(tag="(a)", x="", y="Amphibians", title="EOO - MEM") + 
  theme_map() + theme(legend.position="none", plot.tag.position = c(0.15,0.8),
                      axis.title=element_text(size=10, face="bold", angle=90)) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p4 <- ggplot() + geom_hline(yintercept=0, colour="grey40", 
                            linetype="dashed", size=0.25) + 
  geom_tile(data=sr_new[sr_new$taxa == "Birds",], 
            aes(x=x, y=y, fill=`MEM`)) +
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colors=bluewhitered,
                       limits=c(min_lim2, max_lim2), values=col_val2) + 
  theme_map() + labs(tag="(d)", x="", y="Birds") + 
  theme(legend.position="none", plot.tag.position = c(0.15,0.9),
        axis.title=element_text(size=10, face="bold", angle=90)) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p7 <- ggplot() + geom_hline(yintercept=0, colour="grey40", 
                             linetype="dashed", size=0.25) + 
  geom_tile(data=sr_new[sr_new$taxa == "Mammals",], 
            aes(x=x, y=y, fill=`MEM`)) +  
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colors=bluewhitered,
                       limits=c(min_lim3, max_lim3), values=col_val3) + 
  theme_map() + labs(tag="(g)", x="", y="Mammals") + 
  theme(legend.position="none", plot.tag.position = c(0.15,0.9),
        axis.title=element_text(size=10, face="bold", angle=90)) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p2 <- ggplot() + geom_hline(yintercept=0, colour="grey40", 
                            linetype="dashed", size=0.25) + 
  geom_tile(data=sr_new[sr_new$taxa == "Amphibians",], 
            aes(x=x, y=y, fill=`S-SDM`)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colors=bluewhitered,
                       limits=c(min_lim1, max_lim1), values=col_val1) + 
  theme_map() + labs(tag="(b)", title="EOO - S-SDM") + 
  theme(legend.key.width = unit(0.3,"cm"), legend.key.height = unit(0.5, "cm"), 
        plot.tag.position = c(0.05,0.8)) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p5 <- ggplot() + geom_hline(yintercept=0, colour="grey40", 
                            linetype="dashed", size=0.25) + 
  geom_tile(data=sr_new[sr_new$taxa == "Birds",], 
            aes(x=x, y=y, fill=`S-SDM`)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colors=bluewhitered,
                       limits=c(min_lim2, max_lim2), values=col_val2) + 
  labs(tag="(e)") + theme_map() + 
  theme(legend.key.width = unit(0.3,"cm"),
        legend.key.height = unit(0.5, "cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p8 <- ggplot() + geom_hline(yintercept=0, colour="grey40", 
                             linetype="dashed", size=0.25) + 
  geom_tile(data=sr_new[sr_new$taxa == "Mammals",], 
            aes(x=x, y=y, fill=`S-SDM`)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colors=bluewhitered,
                       limits=c(min_lim3, max_lim3), values=col_val3) + 
  labs(tag="(h)") + 
  theme_map() + theme(legend.key.width = unit(0.3,"cm"),
                      legend.key.height = unit(0.5, "cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

sr_better <- sr_new %>% mutate(`S-SDM` = abs(`S-SDM`), `MEM`= abs(`MEM`)) %>%
  mutate(Best = ifelse(`S-SDM` < `MEM`, "S-SDM", "MEM"))

# Calculate area of each model type
area <- raster::area(raster::rasterFromXYZ(sr_obs_pred))
area <- as.data.frame(raster::rasterToPoints(area))
sr_new %>% left_join(area) %>%
  mutate(`S-SDM` = abs(`S-SDM`), `MEM`= abs(`MEM`)) %>%
  mutate(Best = ifelse(`S-SDM` < `MEM`, "S-SDM", "MEM")) %>%
  group_by(taxa) %>% mutate(total=sum(layer)) %>% group_by(Best, taxa) %>% 
  summarise(area=sum(layer), total=last(total)) %>%
  mutate(area=(area/total)*100)

p3 <- ggplot() + geom_hline(yintercept=0, colour="grey40", 
                            linetype="dashed", size=0.25) + 
  geom_tile(data=sr_better[sr_better$taxa=="Amphibians",], 
            aes(x=x, y=y, fill=Best)) + labs(tag="(c)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_manual(name="", values=c("#333333", "#CCCCCC")) + 
  theme_map() + theme(legend.position="none", plot.tag.position = c(0.05,0.8)) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p6 <- ggplot() + geom_hline(yintercept=0, colour="grey40", 
                            linetype="dashed", size=0.25) + 
  geom_tile(data=sr_better[sr_better$taxa=="Birds",], 
            aes(x=x, y=y, fill=Best)) + labs(tag="(f)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_manual(name="", values=c("#333333", "#CCCCCC")) + 
  theme_map() + coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p9 <- ggplot() + geom_hline(yintercept=0, colour="grey40", 
                             linetype="dashed", size=0.25) + 
  geom_tile(data=sr_better[sr_better$taxa=="Mammals",], 
            aes(x=x, y=y, fill=Best)) + labs(tag="(i)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_manual(name="", values=c("#333333", "#CCCCCC")) + 
  theme_map() + theme(legend.position="none") + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(ncol=3)
ggsave("figures/res_sr_sub-1.png", p, width=10, height=5, dpi=600)
ggsave("figures/Figure2.pdf", p, width=10, height=5)
rm(p, p1, p2, p3, p4, p5, p6, p7, p8, p9)

####################

### Figure S2

# Merge predicted and observed richness
sr_ssdm_presence <- read.csv("data/sr_predicted_ssdm_1995_presence.csv.xz")
sr_ssdm_presence$dispersal <- "No dispersal"
sr_ssdm_disp1 <- read.csv("data/sr_predicted_ssdm_1995_dispersal1.csv.xz")
sr_ssdm_disp1$dispersal <- "d/4"
sr_ssdm_disp2 <- read.csv("data/sr_predicted_ssdm_1995_dispersal2.csv.xz")
sr_ssdm_disp2$dispersal <- "d/2"
sr_ssdm_disp3 <- read.csv("data/sr_predicted_ssdm_1995_dispersal3.csv.xz")
sr_ssdm_disp3$dispersal <- "d"
sr_ssdm_disp4 <- read.csv("data/sr_predicted_ssdm_1995_dispersal4.csv.xz")
sr_ssdm_disp4$dispersal <- "2*d"
sr_ssdm_fulldisp <- read.csv("data/sr_predicted_ssdm_1995_fulldisp.csv.xz")
sr_ssdm_fulldisp$dispersal <- "Full dispersal"

sr_ssdm <- rbind(sr_ssdm_presence, sr_ssdm_disp1, sr_ssdm_disp2, sr_ssdm_disp3, 
                 sr_ssdm_disp4, sr_ssdm_fulldisp)
rm(sr_ssdm_presence, sr_ssdm_disp1, sr_ssdm_disp2, sr_ssdm_disp3,
   sr_ssdm_disp4, sr_ssdm_fulldisp)
sr_ssdm$dispersal <- factor(sr_ssdm$dispersal, levels=c("No dispersal", "d/4",
                                                        "d/2", "d",
                                                        "2*d", "Full dispersal"))
sr_ssdm$taxa <- factor(sr_ssdm$taxa, labels=c("Amphibians", "Birds", "Mammals"))
colnames(sr_ssdm)[3] <- "mn"
sr_obs_ssdm <- inner_join(sr_ssdm, sr_observed); rm(sr_ssdm)

## Create ensemble mean
ensemble <- sr_obs_ssdm %>% group_by(x, y, dispersal, taxa) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T),
                   sum = mean(sum, na.rm=T)) %>% data.frame()
ensemble$model <- "Ensemble"
sr_obs_ssdm <- rbind(sr_obs_ssdm, ensemble); rm(ensemble)
sr_obs_ssdm$model <- factor(sr_obs_ssdm$model, 
                             levels=c("GAM", "GBM", "Ensemble"))
sr_obs_ssdm$taxa <- factor(sr_obs_ssdm$taxa, 
                            labels=c("Amphibians", "Birds", "Mammals"))

# Plot observed vs. predicted species richness
p1 <- sr_obs_ssdm %>% filter(model == "Ensemble", taxa=="Amphibians") %>% 
  ggplot(aes(x=sum, y=mn)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  facet_wrap(dispersal ~ ., strip.position = "left", ncol=1) + 
  stat_poly_eq(aes(label = ..eq.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.15, parse=TRUE, col="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.05, parse=TRUE, col="#000000") + 
  scale_y_continuous("Predicted richness", limits=c(0,150), expand=c(0,0)) + 
  scale_x_continuous("", limits=c(0,150), expand=c(0,0)) +
  ggtitle("Amphibians") + 
  theme_plot() + theme(strip.text = element_text(size=12, face="bold"),
                     panel.spacing.y = unit(0.75, "lines"),
                     strip.placement="outside", 
                     plot.title = element_text(size=12, face="bold", 
                                               hjust=0.5),
                     axis.title.y = element_text(size = 14, face="bold", angle=90),
                     strip.background= element_blank()) + coord_equal()
p2 <- sr_obs_ssdm %>% filter(model == "Ensemble", taxa=="Birds") %>% 
  ggplot(aes(x=sum, y=mn)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) +
  facet_wrap(dispersal ~ ., strip.position = "left", ncol=1) + 
  stat_poly_eq(aes(label = ..eq.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.15, parse=TRUE, col="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.05, parse=TRUE, col="#000000") + 
  scale_y_continuous("", limits=c(0,805), expand=c(0,0)) + 
  scale_x_continuous("EOO-based richness", limits=c(0,805), expand=c(0,0)) +
  ggtitle("Birds") + theme_plot() + 
  theme(strip.text = element_blank(),
        panel.spacing.y = unit(0.75, "lines"),
        plot.title = element_text(size=12, face="bold", hjust=0.5),
        axis.title.x = element_text(size = 14, face="bold"),
        strip.background= element_blank()) + coord_equal()
p3 <- sr_obs_ssdm %>% filter(model == "Ensemble", taxa=="Mammals") %>% 
  ggplot(aes(x=sum, y=mn)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) +
  facet_wrap(dispersal ~ ., strip.position = "left", ncol=1) + 
  stat_poly_eq(aes(label = ..eq.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.15, parse=TRUE, col="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.05, parse=TRUE, col="#000000") + 
  scale_y_continuous(" ", limits=c(0,255), expand=c(0,0)) + 
  scale_x_continuous(" ", limits=c(0,255), expand=c(0,0)) +
  ggtitle("Mammals") + theme_plot() + 
  theme(strip.text = element_blank(),
        panel.spacing.y = unit(0.75, "lines"),
        strip.placement="outside", 
        plot.title = element_text(size=12, face="bold", hjust=0.5),
        axis.title = element_text(size = 14, face="bold"),
        strip.background= element_blank()) + coord_equal()
p <- p1 + p2 + p3 + plot_layout(nrow=1)
ggsave("figures/cor_disp-1.png", p, dpi=300, width=9, height=15)
rm(p, p1, p2, p3)

####################

### Figure S3

sr_observed2 <- sr_observed
sr_observed2$dispersal <- "EOO-based"
colnames(sr_observed2)[4] <- "mn"
sr_obs_ssdm <- sr_obs_ssdm %>% filter(model =="Ensemble") %>% select(-model, -sum)
sr_obs_ssdm <- rbind(sr_observed2, sr_obs_ssdm); rm(sr_observed2)
sr_obs_ssdm$dispersal <- factor(sr_obs_ssdm$dispersal, 
                                levels=c("EOO-based", "No dispersal", "d/4",
                                         "d/2", "d", "2*d", "Full dispersal"))

# Create map
max_lim1 <- round(max(sr_obs_ssdm$mn[sr_obs_ssdm$taxa=="Amphibians"]), digits=-1)
max_lim2 <- round(max(sr_obs_ssdm$mn[sr_obs_ssdm$taxa=="Birds"]), digits=-1)
max_lim3 <- round(max(sr_obs_ssdm$mn[sr_obs_ssdm$taxa=="Mammals"]), digits=-1)
p1 <- ggplot() + geom_hline(yintercept=0, colour="grey40", 
                            linetype="dashed", size=0.25) + 
  geom_tile(data=sr_obs_ssdm[sr_obs_ssdm$taxa=="Amphibians",], 
            aes(x=x, y=y, fill=mn)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  facet_wrap(dispersal ~ ., strip.position="left", ncol=1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim1),
                       colours=colorRampPalette(c("white", "#00007F", "blue",
                                                  "#007FFF", "cyan", 
                                                  "#7FFF7F", "yellow", "#FF7F00", "red", 
                                                  "#7F0000"))(255), 
                       na.value="transparent", 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  ggtitle("Amphibians") + theme_map() + 
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), 
        strip.text=element_text(size=12, face="bold"), 
        legend.position="bottom", legend.key.width=unit(1, "cm"),
        legend.key.height = unit(0.3, "cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p2 <- ggplot() + geom_hline(yintercept=0, colour="grey40", 
                            linetype="dashed", size=0.25) + 
  geom_tile(data=sr_obs_ssdm[sr_obs_ssdm$taxa=="Birds",], 
            aes(x=x, y=y, fill=mn)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  facet_wrap(dispersal ~ ., ncol=1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim2),
                       colours=colorRampPalette(c("white", "#00007F", "blue", 
                                                  "#007FFF", "cyan", 
                                                  "#7FFF7F", "yellow", "#FF7F00", "red", 
                                                  "#7F0000"))(255), 
                       na.value="transparent", 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  ggtitle("Birds") + theme_map() + 
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), 
        strip.text=element_blank(),
        legend.position="bottom", legend.key.width=unit(1, "cm"),
        legend.key.height = unit(0.3, "cm"),
        axis.text.y = element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p3 <- ggplot() + geom_hline(yintercept=0, colour="grey40", 
                             linetype="dashed", size=0.25) + 
  geom_tile(data=sr_obs_ssdm[sr_obs_ssdm$taxa=="Mammals",], 
            aes(x=x, y=y, fill=mn)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  facet_wrap(dispersal ~ ., ncol=1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim3),
                       colours=colorRampPalette(c("white", "#00007F", "blue", 
                                                  "#007FFF", "cyan", 
                                                  "#7FFF7F", "yellow", "#FF7F00", "red", 
                                                  "#7F0000"))(255), 
                       na.value="transparent", 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  ggtitle("Mammals") + theme_map() + 
  theme(plot.title = element_text(size=12, face="bold", hjust=0.5), 
        strip.text = element_blank(), legend.position="bottom", 
        legend.key.width=unit(1, "cm"), axis.text.y = element_blank(),
        legend.key.height=unit(0.3, "cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p <- p1 + p2 + p3 + plot_layout(nrow=1)
ggsave("figures/cur_sr_sub-1.png", p, width=10, height=12, dpi=600)
rm(p, p1, p2, p3)

####################

## Figure S4

# Create map
#max_lim1 <- round(max(sr_obs_pred$mn[sr_obs_pred$taxa=="Amphibians"]), digits=-1)
#max_lim2 <- round(max(sr_obs_pred$mn[sr_obs_pred$taxa=="Birds"]), digits=-1)
#max_lim3 <- round(max(sr_obs_pred$mn[sr_obs_pred$taxa=="Mammals"]), digits=-1)
max_lim1 <- round(max(sr_obs_pred$sum[sr_obs_pred$taxa=="Amphibians"]), digits=-1)
max_lim2 <- round(max(sr_obs_pred$sum[sr_obs_pred$taxa=="Birds"]), digits=-1)
max_lim3 <- round(max(sr_obs_pred$sum[sr_obs_pred$taxa=="Mammals"]), digits=-1)
p1 <- sr_obs_pred %>% filter(taxa == "Amphibians", model == "Ensemble") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=sum)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  labs(title="EOO-based", y="Amphibians", x="", tag="(a)") + 
  scale_fill_gradientn(name="", limits=c(0, max_lim1), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(axis.title = element_text(size=10, face="bold", angle=90),
                      plot.tag.position = c(0.05, 0.85), legend.position="none", 
                      axis.text.x = element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p4 <- sr_obs_pred %>% filter(taxa == "Birds", model == "Ensemble") %>% 
  ggplot() + 
  geom_hline(yintercept=0, colour="grey40", 
             linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=sum)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  labs(y="Birds", x="", tag="(d)") + 
  scale_fill_gradientn(name="", limits=c(0, max_lim2), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(axis.title = element_text(size=10, face="bold", angle=90),
                      legend.position = "none", axis.text.x=element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p7 <- sr_obs_pred %>% filter(taxa == "Mammals", model == "Ensemble") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=sum)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  labs(y="Mammals", x="", tag="(g)") + 
  scale_fill_gradientn(name="", limits=c(0, max_lim3), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(axis.title = element_text(size=10, face="bold", angle=90),
                      legend.position="none") + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

p2 <- sr_obs_pred %>% filter(taxa == "Amphibians", type == "MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  labs(title="MEM", tag="(b)") + 
  scale_fill_gradientn(name="", limits=c(0, max_lim1), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(plot.tag.position = c(0.05, 0.85), 
                      legend.position="none", axis.text=element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p5 <- sr_obs_pred %>% filter(taxa == "Birds", type == "MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + labs(tag="(e)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim2), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(legend.position = "none", axis.text = element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p8 <- sr_obs_pred %>% filter(taxa == "Mammals", type == "MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + labs(tag="(h)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim3), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(strip.placement="none", axis.text.y=element_blank(),
                      legend.position="none") + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

p3 <- sr_obs_pred %>% filter(taxa == "Amphibians", type == "S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + labs(title="S-SDM", tag="(c)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim1), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(plot.tag.position = c(0.05, 0.85), 
                      strip.text.y=element_blank(), 
                      legend.key.width=unit(0.3,"cm"),
                      axis.text = element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p6 <- sr_obs_pred %>% filter(taxa == "Birds", type == "S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + labs(tag="(f)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim2), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(strip.text=element_blank(), axis.text=element_blank(),
                      legend.key.width=unit(0.3, "cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p9 <- sr_obs_pred %>% filter(taxa == "Mammals", type == "S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + labs(tag="(i)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim3), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(strip.text = element_blank(),
                      axis.text.y = element_blank(),
                      legend.key.width=unit(0.3, "cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(ncol=3)
ggsave("figures/cur_sr_sub_disp1-1.png", p, width=10, height=5, dpi=600)

####################

## Figure S5

sr_observed <- read.csv("data/sr_observed_ssdm.csv.xz")
sr_observed$taxa <- factor(sr_observed$taxa, 
                           labels=c("Amphibians", "Birds", "Mammals"))
max(sr_observed$sum, na.rm=T)

# Create map
sr_full <- read.csv("data/sr_observed_all.csv.xz")
colnames(sr_full) <- c("x", "y", "taxa", "total")
sr_full$taxa <- factor(sr_full$taxa, 
                       labels=c("Amphibians", "Birds", "Mammals"))
max(sr_full$total, na.rm=T)

load("data/landseamask_generic.rda")
area_df <- raster::area(landseamask_generic)
area_df <- as.data.frame(raster::rasterToPoints(area_df))

sr_prop_obs <- left_join(sr_observed, sr_full) %>%
  left_join(area_df) %>% mutate(prop=sum/total*100) %>% 
  mutate(prop = round(prop,digits=0),
         prop2 = cut(prop, c(0,85,90,95,100))) %>%
  mutate(prop2 = factor(prop2, labels=c("0-85 %", "85 - 90 %",
                                           "90 - 95 %", "95 - 100 %")))
max_lim <- round(max(sr_prop_obs$prop, na.rm=T), digits=-1)

# Identify values for text
sr_prop_obs %>% group_by(taxa) %>%
  mutate(total = sum(layer,na.rm=T)) %>% 
  filter(prop > 85) %>% 
  summarise(sum(layer/total,na.rm=T)*100)

p1 <- sr_prop_obs %>% filter(taxa == "Amphibians") %>% ggplot() + 
  geom_hline(yintercept=0, colour="grey40", 
             linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=prop2)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  labs(x="", y="Amphibians", tag="(a)") + theme_map() + 
  scale_fill_manual(name="", values=c("#D43F3AFF", "#EEA236FF",
                                      "#5CB85CFF", "#357EBDFF")) + 
  theme(axis.title = element_text(size=10, face="bold", angle=90),
        legend.position="none", axis.text.x = element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p3 <- sr_prop_obs %>% filter(taxa == "Birds") %>% ggplot() + 
  geom_hline(yintercept=0, colour="grey40", 
             linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=prop2)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  labs(x="", y="Birds", tag="(c)") + theme_map() + 
  scale_fill_manual(name="", values=c("#D43F3AFF", "#EEA236FF",
                                      "#5CB85CFF", "#357EBDFF")) + 
  theme(axis.title = element_text(size=10, face="bold", angle=90),
        legend.position="none", axis.text.x = element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p5 <- sr_prop_obs %>% filter(taxa == "Mammals") %>% ggplot() + 
  geom_hline(yintercept=0, colour="grey40", 
             linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=prop2)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  labs(x="", y="Mammals", tag="(e)") + theme_map() + 
  scale_fill_manual(name="", values=c("#D43F3AFF", "#EEA236FF",
                                      "#5CB85CFF", "#357EBDFF")) + 
  theme(axis.title = element_text(size=10, face="bold", angle=90),
        legend.position="none", axis.text.x = element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

p2 <- sr_prop_obs %>% group_by(prop, taxa, prop2) %>% 
  summarise(area=sum(layer,na.rm=T)) %>% filter(taxa == "Amphibians") %>%
  ggplot() + geom_bar(aes(x=prop-0.5, y=area/10000, fill=prop2), 
                      colour=NA, stat="identity") + 
  labs(x="", y="", tag="(b)") + 
  scale_fill_manual(name="", values=c("#D43F3AFF", "#EEA236FF",
                                      "#5CB85CFF", "#357EBDFF")) + 
  scale_y_continuous(limits=c(0,NA), 
                     expand = expand_scale(mult = c(0, .1))) + 
  scale_x_continuous(limits=c(0,101), expand=c(0,0)) + 
  theme_plot() + theme(legend.position="none", plot.tag.position = c(0.025,0.9))
p4 <- sr_prop_obs %>% group_by(prop, taxa, prop2) %>% 
  summarise(area=sum(layer,na.rm=T)) %>% filter(taxa == "Birds") %>%
  ggplot() + geom_bar(aes(x=prop-0.5, y=area/10000, fill=prop2), 
                      colour=NA, stat="identity") + 
  labs(x="", y="Area (ha)", tag="(d)") + 
  scale_fill_manual(name="", values=c("#D43F3AFF", "#EEA236FF",
                                      "#5CB85CFF", "#357EBDFF")) + 
  scale_y_continuous(limits=c(0,NA), 
                     expand = expand_scale(mult = c(0, .1))) + 
  scale_x_continuous(limits=c(0,101), expand=c(0,0)) + 
  theme_plot() + 
  theme(axis.title.y = element_text(size=12, face="bold", angle=90),
        plot.tag.position = c(0.025,0.9))
p6 <- sr_prop_obs %>% group_by(prop, taxa, prop2) %>% 
  summarise(area=sum(layer,na.rm=T)) %>% filter(taxa == "Mammals") %>%
  ggplot() + geom_bar(aes(x=prop-0.5, y=area/10000, fill=prop2), 
                      colour=NA, stat="identity") + 
  labs(x="% Species coverage", y="", tag="(f)") + 
  scale_fill_manual(name="", values=c("#D43F3AFF", "#EEA236FF",
                                      "#5CB85CFF", "#357EBDFF")) + 
  scale_y_continuous(limits=c(0,NA), 
                     expand = expand_scale(mult = c(0, .1))) + 
  scale_x_continuous(limits=c(0,101), expand=c(0,0)) + 
  theme_plot() + theme(legend.position="none", plot.tag.position = c(0.025,0.9))
p <- p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(width=c(2,1), ncol=2)
ggsave("figures/iucn_sr_sub_full-1.png", p, width=7, height=6, dpi=600)

####################

### Figure S6

sr_mem <- sr_obs_pred %>% filter(type == "MEM", model=="Ensemble")

sr_mem_all <- rbind(read.csv("data/sr_predicted_mem_all_GAM_poisson_eco_1995.csv.xz"),
                read.csv("data/sr_predicted_mem_all_GBM_poisson_eco_1995.csv.xz")) %>%
  dplyr::select(x,y,mean,model,taxa)
sr_mem_all$model <- factor(sr_mem_all$model, labels=c("GAM", "GBM"))
sr_mem_all$taxa <- factor(sr_mem_all$taxa, 
                      labels=c("Amphibians", "Birds", "Mammals"))
colnames(sr_mem_all)[3] <- "mn"

## Create ensemble mean
sr_mem_all <- sr_mem_all %>% group_by(x, y, taxa) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>% data.frame()
sr_mem_all$model <- "Ensemble"

# Create map
max_lim1 <- round(max(sr_mem_all$mn[sr_mem_all$taxa=="Amphibians"]), digits=-1)
max_lim2 <- round(max(sr_mem_all$mn[sr_mem_all$taxa=="Birds"]), digits=-1)
max_lim3 <- round(max(sr_mem_all$mn[sr_mem_all$taxa=="Mammals"]), digits=-1)
p2 <- sr_mem_all %>% filter(taxa == "Amphibians") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + 
  labs(title="MEM All", tag="(a)", x="", y="Amphibians") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim1), 
                       colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(axis.title=element_text(size=12, face="bold"),
                      legend.key.width=unit(0.3,"cm"),
                      plot.tag.position = c(0.15, 0.9)) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p5 <- sr_mem_all %>% filter(taxa == "Birds") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + 
  labs(tag="(d)", x="", y="Birds") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim2), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(axis.title=element_text(size=12, face="bold"),
                      legend.key.width=unit(0.3,"cm"),
                      plot.tag.position = c(0.15, 0.9)) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p8 <- sr_mem_all %>% filter(taxa == "Mammals") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + 
  labs(tag="(g)", x="", y="Mammals") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim3), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(axis.title=element_text(size=12, face="bold"),
                      legend.key.width=unit(0.3,"cm"),
                      plot.tag.position = c(0.15, 0.9)) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

sr_mem_dif <- inner_join(sr_mem_all,sr_mem, 
                         by=c("x","y","taxa","model"))
sr_mem_dif$dif <- (sr_mem_dif$mn.x - sr_mem_dif$mn.y)
sr_mem_dif$perc_dif <- round(sr_mem_dif$dif/sr_mem_dif$mn.y*100, digits=0)

sr_mem_dif %>% summarise(mean(perc_dif))
sr_mem_dif %>% group_by(taxa) %>% summarise(mean(perc_dif))

max_lim1 <- round(max(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Amphibians"]), digits=-1)
max_lim2 <- round(max(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Birds"]), digits=-1)
max_lim3 <- round(max(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Mammals"]), digits=-1)
min_lim1 <- round(min(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Amphibians"]), digits=0)
min_lim2 <- round(min(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Birds"]), digits=-1)
min_lim3 <- round(min(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Mammals"]), digits=-1)
p3 <- sr_mem_dif %>% filter(taxa == "Amphibians") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=perc_dif)) + 
  labs(title="MEM All - MEM (%)", tag="(b)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(min_lim1, max_lim1), colours=bluewhitered,
                       values=scales::rescale(unique(c(seq(min_lim1, 0, length=5), 
                                                       seq(0, max_lim1, length=5))))) + 
  theme_map() + theme(legend.key.width=unit(0.3,"cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p6 <- sr_mem_dif %>% filter(taxa == "Birds") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=perc_dif)) + labs(tag="(e)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colors=bluewhitered, limits=c(min_lim2, max_lim2), 
    values=scales::rescale(unique(c(seq(min_lim2, 0, length=5), 
                                    seq(0, max_lim2, length=5))))) + 
  theme_map() + theme(legend.key.width=unit(0.3,"cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p9 <- sr_mem_dif %>% filter(taxa == "Mammals") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=perc_dif)) + labs(tag="(h)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colors=bluewhitered, limits=c(min_lim3, max_lim3), 
    values=scales::rescale(unique(c(seq(min_lim3, 0, length=5), 
                                    seq(0, max_lim3, length=5))))) + 
  theme_map() + theme(legend.key.width=unit(0.3,"cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

# Add histograms of Residuals
load("data/landseamask_generic.rda")
area_df <- raster::area(landseamask_generic)
area_df <- as.data.frame(raster::rasterToPoints(area_df))

mem_dif_sum <- sr_mem_dif %>% left_join(area_df) %>% 
  group_by(perc_dif, taxa) %>% 
  summarise(layer=sum(layer,na.rm=T))
p1 <- mem_dif_sum %>% filter(taxa == "Amphibians") %>% 
  ggplot() + geom_bar(aes(x=perc_dif, y=layer/10000), stat="identity") + labs(x="") + 
  scale_y_continuous(limits=c(0,NA), expand = expand_scale(mult = c(0, .1))) + 
  theme_plot() + theme(plot.tag.position = c(0.05,0.9)) + labs(tag="(c)")
p4 <- mem_dif_sum %>% filter(taxa == "Birds") %>% 
  ggplot() + geom_bar(aes(x=perc_dif, y=layer/10000), stat="identity") + 
  labs(x="", y="Area (ha)") + scale_x_continuous() + 
  scale_y_continuous(limits=c(0,NA), expand = expand_scale(mult = c(0, .1))) + 
  theme_plot() + theme(plot.tag.position = c(0.05,0.9)) + labs(tag="(f)") + 
  theme(axis.title.y=element_text(size=12, face="bold", angle=90))
p7 <- mem_dif_sum %>% filter(taxa == "Mammals") %>% 
  ggplot() + geom_bar(aes(x=perc_dif, y=layer/10000), 
                      stat="identity") + labs(x="MEM All - MEM (%)") + 
  scale_y_continuous(limits=c(0,NA), expand = expand_scale(mult = c(0, .1))) + 
  theme_plot() + theme(plot.tag.position = c(0.05,0.9)) + labs(tag="(i)")
p <- p2 + p3 + p1 + p5 + p6 + p4 + p8 + p9 + p7 + 
  plot_layout(ncol=3, width=c(2,2,1))
ggsave("figures/mem_cur_sr_sub_full-1.png", p, width=9, height=6, dpi=600)

####################

### Figure 3

# Load data
sr_future_ssdm <- read.csv("data/sr_predicted_ssdm_dispersal1_2080.csv.xz")

# Calculate ensemble among different GCMs for RCP2.6
sr_future_ssdm <- sr_future_ssdm %>% 
  dplyr::select(c(x,y,model,taxa), matches("rcp26")) %>% 
  group_by(x,y,model,taxa) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,5:8], na.rm=TRUE)) %>% 
  dplyr::select(x,y,model,taxa,mn)

## Create ensemble mean among GAM & GBM
ensemble <- sr_future_ssdm %>% group_by(x,y,taxa) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>%
  data.frame()
ensemble$model <- "Ensemble"

sr_future_ssdm <- rbind(sr_future_ssdm, ensemble); rm(ensemble)
sr_future_ssdm$taxa <- factor(sr_future_ssdm$taxa, 
                              labels=c("Amphibians", "Birds", "Mammals"))
sr_future_ssdm$type <- "S-SDM"

sr_future_mem <- rbind(
  read.csv("data/sr_predicted_mem_sub_GAM_poisson_eco_2080.csv.xz"),
  read.csv("data/sr_predicted_mem_sub_GBM_poisson_eco_2080.csv.xz"))
sr_future_mem$model <- factor(sr_future_mem$model, labels=c("GAM", "GBM"))

# Calculate ensemble among different GCMs for RCP2.6
sr_future_mem <- sr_future_mem %>% 
  dplyr::select(c(x,y,model,taxa), matches("rcp26")) %>% 
  group_by(x,y,model,taxa) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,5:8], na.rm=TRUE)) %>% 
  dplyr::select(x,y,model,taxa,mn)

## Create ensemble mean among GAM and GBM
ensemble <- sr_future_mem %>% group_by(x,y,taxa) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>%
  data.frame()
ensemble$model <- "Ensemble"

sr_future_mem <- rbind(sr_future_mem, ensemble); rm(ensemble)
sr_future_mem$taxa <- factor(sr_future_mem$taxa, 
                             labels=c("Amphibians", "Birds", "Mammals"))
sr_future_mem$type <- "MEM"

sr_future1 <- rbind(sr_future_ssdm, sr_future_mem) %>% 
  filter(model == "Ensemble")

# Create map
max_lim1 <- round(max(sr_future1$mn[sr_future1$taxa=="Amphibians"]), digits=-1)
max_lim2 <- round(max(sr_future1$mn[sr_future1$taxa=="Birds"]), digits=-1)
max_lim3 <- round(max(sr_future1$mn[sr_future1$taxa=="Mammals"]), digits=-1)

p1 <- sr_future1 %>% filter(taxa == "Amphibians", type=="MEM") %>% 
  ggplot() +  geom_hline(yintercept=0, colour="grey40", 
                         linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + 
  labs(tag="(a)", title="MEM", x="", y="Amphibians") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) +  
  scale_fill_gradientn(name="", limits=c(0, max_lim1), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(legend.position = "none", axis.text.x = element_blank(),
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p2 <- sr_future1 %>% filter(taxa == "Amphibians", type=="S-SDM") %>% 
  ggplot() +  geom_hline(yintercept=0, colour="grey40", 
                         linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + 
  labs(tag="(b)", title="S-SDM") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim1), colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(axis.text = element_blank(),
                      legend.key.width=unit(0.3,"cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p4 <- sr_future1 %>% filter(taxa == "Birds", type=="MEM") %>% 
  ggplot() +  geom_hline(yintercept=0, colour="grey40", 
                         linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + 
  labs(tag="(d)", y="Birds", x="") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim2), colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(legend.position = "none", axis.text.x = element_blank(),
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p5 <- sr_future1 %>% filter(taxa == "Birds", type=="S-SDM") %>% 
  ggplot() +  geom_hline(yintercept=0, colour="grey40", 
                         linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + labs(tag="(e)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) +
  scale_fill_gradientn(name="", limits=c(0, max_lim2), colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(axis.text = element_blank(),
                      legend.key.width = unit(0.3, "cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

p7 <- sr_future1 %>% filter(taxa == "Mammals", type=="MEM") %>% 
  ggplot() +  geom_hline(yintercept=0, colour="grey40", 
                         linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + 
  labs(tag="(g)", y="Mammals", x="") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim3), colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(legend.position = "none",
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p8 <- sr_future1 %>% filter(taxa == "Mammals", type=="S-SDM") %>% 
  ggplot() +  geom_hline(yintercept=0, colour="grey40", 
                         linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + labs(tag="(h)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim3), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(axis.text.y = element_blank(),
                      legend.key.width=unit(0.3,"cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

sr_cor <- sr_future1 %>% tidyr::spread(type, mn)

p3 <- ggplot(data=sr_cor[sr_cor$taxa=="Amphibians",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(c)") + 
  scale_x_continuous("", limits=c(0,150), expand=c(0,0)) + 
  scale_y_continuous("", limits=c(0,150), expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + 
  labs(tag="(c)") + coord_equal()
p6 <- ggplot(data=sr_cor[sr_cor$taxa=="Birds",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(f)") + 
  scale_x_continuous("", limits=c(0,800), expand=c(0,0)) + 
  scale_y_continuous("S-SDM richness", limits=c(0,800), expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9),
                       axis.title.y = element_text(size=12, face="bold", angle=90)) + coord_equal()
p9 <- ggplot(data=sr_cor[sr_cor$taxa=="Mammals",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(i)") + 
  scale_x_continuous("MEM richness", limits=c(0,250), expand=c(0,0)) + 
  scale_y_continuous("", limits=c(0,250), expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + coord_equal()
p <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(ncol=3, widths=c(2,2,1))
ggsave("figures/fut_sr_sub_rcp26-1.png", p, width = 9, height = 6, dpi=600)
ggsave("figures/Figure3.pdf", p, width = 9, height = 6)

####################

### Figure S7

# Load data
sr_future_ssdm <- read.csv("data/sr_predicted_ssdm_dispersal1_2080.csv.xz")

# Calculate ensemble among different GCMs for RCP6.0
sr_future_ssdm <- sr_future_ssdm %>% 
  dplyr::select(c(x,y,model,taxa), matches("rcp60")) %>% 
  group_by(x,y,model,taxa) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,5:8], na.rm=TRUE)) %>% 
  dplyr::select(x,y,model,taxa,mn)

## Create ensemble mean among GAM & GBM
ensemble <- sr_future_ssdm %>% group_by(x,y,taxa) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>%
  data.frame()
ensemble$model <- "Ensemble"

sr_future_ssdm <- rbind(sr_future_ssdm, ensemble); rm(ensemble)
sr_future_ssdm$taxa <- factor(sr_future_ssdm$taxa, 
                              labels=c("Amphibians", "Birds", "Mammals"))
sr_future_ssdm$type <- "S-SDM"

sr_future_mem <- rbind(
  read.csv("data/sr_predicted_mem_sub_GAM_poisson_eco_2080.csv.xz"),
  read.csv("data/sr_predicted_mem_sub_GBM_poisson_eco_2080.csv.xz"))
sr_future_mem$model <- factor(sr_future_mem$model, labels=c("GAM", "GBM"))

# Calculate ensemble among different GCMs for RCP6.0
sr_future_mem <- sr_future_mem %>% 
  dplyr::select(c(x,y,model,taxa), matches("rcp60")) %>% 
  group_by(x,y,model,taxa) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,5:8], na.rm=TRUE)) %>% 
  dplyr::select(x,y,model,taxa,mn)

## Create ensemble mean among GAM and GBM
ensemble <- sr_future_mem %>% group_by(x,y,taxa) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>%
  data.frame()
ensemble$model <- "Ensemble"

sr_future_mem <- rbind(sr_future_mem, ensemble); rm(ensemble)
sr_future_mem$taxa <- factor(sr_future_mem$taxa, 
                             labels=c("Amphibians", "Birds", "Mammals"))
sr_future_mem$type <- "MEM"

sr_future2 <- rbind(sr_future_ssdm, sr_future_mem) %>% 
  filter(model == "Ensemble")

# Create map
max_lim1 <- round(max(sr_future2$mn[sr_future2$taxa=="Amphibians"]), digits=-1)
max_lim2 <- round(max(sr_future2$mn[sr_future2$taxa=="Birds"]), digits=-1)
max_lim3 <- round(max(sr_future2$mn[sr_future2$taxa=="Mammals"]), digits=-1)

p1 <- sr_future2 %>% filter(taxa == "Amphibians", type=="MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + 
  labs(tag="(a)", title="MEM", x="", y="Amphibians") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) +  
  scale_fill_gradientn(name="", limits=c(0, max_lim1),
                       colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(legend.position = "none", axis.text.x = element_blank(),
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p2 <- sr_future2 %>% filter(taxa == "Amphibians", type=="S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + 
  labs(tag="(b)", title="S-SDM") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim1), colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(axis.text=element_blank(),
                      legend.key.width= unit(0.3, "cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p4 <- sr_future2 %>% filter(taxa == "Birds", type=="MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + 
  labs(tag="(d)", x="", y="Birds") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim2), colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(legend.position = "none", axis.text.x = element_blank(),
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p5 <- sr_future2 %>% filter(taxa == "Birds", type=="S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + labs(tag="(e)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) +
  scale_fill_gradientn(name="", limits=c(0, max_lim2),
                       colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(axis.text=element_blank(),
                      legend.key.width=unit(0.3, "cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

p7 <- sr_future2 %>% filter(taxa == "Mammals", type=="MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + 
  labs(tag="(g)", x="", y="Mammals") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim3),
                       colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(legend.position = "none",
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p8 <- sr_future2 %>% filter(taxa == "Mammals", type=="S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + 
  labs(tag="(h)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim3), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(axis.text.y=element_blank(),
                      legend.key.width = unit(0.3, "cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

sr_cor <- sr_future2 %>% tidyr::spread(type, mn)

p3 <- ggplot(data=sr_cor[sr_cor$taxa=="Amphibians",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(c)") + 
  scale_x_continuous("", limits=c(0,150), expand=c(0,0)) + 
  scale_y_continuous("", limits=c(0,150), expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + 
  labs(tag="(c)") + coord_equal()
p6 <- ggplot(data=sr_cor[sr_cor$taxa=="Birds",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(f)") + 
  scale_x_continuous("", limits=c(0,800), expand=c(0,0)) + 
  scale_y_continuous("S-SDM richness", limits=c(0,800), expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9),
                       axis.title.y = element_text(size=12, face="bold", angle=90)) + coord_equal()
p9 <- ggplot(data=sr_cor[sr_cor$taxa=="Mammals",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(i)") + 
  scale_x_continuous("MEM richness", limits=c(0,250), expand=c(0,0)) + 
  scale_y_continuous("", limits=c(0,250), expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + coord_equal()
p <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + 
  plot_layout(ncol=3, widths=c(2,2,1))
ggsave("figures/fut_sr_sub_rcp60-1.png", p, dpi = 600, width=9, height=6)

####################

### Figure S8

# Load data
sr_future_ssdm <- read.csv("data/sr_predicted_ssdm_dispersal1_2080.csv.xz")

# Calculate ensemble among different GCMs and model algorithm for RCP2.6 & RCP6.0
ensemble_sdm_rcp60 <- sr_future_ssdm %>% 
  dplyr::select(c(x,y,model,taxa), matches("rcp60")) %>% 
  group_by(x,y,model,taxa) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,5:8], na.rm=TRUE),
         rcp="RCP6.0") %>% 
  dplyr::select(x,y,model,rcp,taxa,mn) %>% group_by(x,y,taxa,rcp) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>%
  data.frame()
ensemble_sdm_rcp26 <- sr_future_ssdm %>% 
  dplyr::select(c(x,y,model,taxa), matches("rcp26")) %>% 
  group_by(x,y,model,taxa) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,5:8], na.rm=TRUE),
         rcp = "RCP2.6") %>% 
  dplyr::select(x,y,model,rcp,taxa,mn) %>% group_by(x,y,taxa,rcp) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>%
  data.frame()
ensemble_ssdm <- bind_rows(ensemble_sdm_rcp26, ensemble_sdm_rcp60)
ensemble_ssdm$taxa <- factor(ensemble_ssdm$taxa, 
                             labels=c("Amphibians", "Birds", "Mammals"))
ensemble_ssdm$type <- "S-SDM"

sr_future_mem <- rbind(
  read.csv("data/sr_predicted_mem_sub_GAM_poisson_eco_2080.csv.xz"),
  read.csv("data/sr_predicted_mem_sub_GBM_poisson_eco_2080.csv.xz"))
sr_future_mem$model <- factor(sr_future_mem$model, labels=c("GAM", "GBM"))

# Calculate ensemble among different GCMs and model algorithm for RCP6.0
ensemble_mem_rcp60 <- sr_future_mem %>% 
  dplyr::select(c(x,y,model,taxa), matches("rcp60")) %>% 
  group_by(x,y,model,taxa) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,5:8], na.rm=TRUE),
         rcp="RCP6.0") %>% 
  dplyr::select(x,y,model,taxa,rcp,mn) %>% group_by(x,y,taxa,rcp) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>%
  data.frame()
ensemble_mem_rcp26 <- sr_future_mem %>% 
  dplyr::select(c(x,y,model,taxa), matches("rcp26")) %>% 
  group_by(x,y,model,taxa) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,5:8], na.rm=TRUE),
         rcp="RCP2.6") %>% 
  dplyr::select(x,y,model,taxa,mn,rcp) %>% group_by(x,y,taxa,rcp) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>%
  data.frame()
ensemble_mem <- bind_rows(ensemble_mem_rcp26, ensemble_mem_rcp60)
ensemble_mem$taxa <- factor(ensemble_mem$taxa, labels=c("Amphibians", "Birds", "Mammals"))
ensemble_mem$type <- "MEM"

ensemble <- rbind(ensemble_ssdm, ensemble_mem)
ensemble <- ensemble %>% tidyr::spread(type, mn) %>% tidyr::drop_na()
ensemble$dif <- ensemble$MEM-ensemble$`S-SDM`

# Create map
max_lim1 <- round(max(ensemble$dif[ensemble$taxa=="Amphibians"], na.rm=T), digits=-1)
max_lim2 <- round(max(ensemble$dif[ensemble$taxa=="Birds"], na.rm=T), digits=-1)
max_lim3 <- round(max(ensemble$dif[ensemble$taxa=="Mammals"], na.rm=T), digits=-1)
min_lim1 <- round(min(ensemble$dif[ensemble$taxa=="Amphibians"], na.rm=T), digits=-1)
min_lim2 <- round(min(ensemble$dif[ensemble$taxa=="Birds"], na.rm=T), digits=-1)
min_lim3 <- round(min(ensemble$dif[ensemble$taxa=="Mammals"], na.rm=T), digits=-1)

p1 <- ensemble %>% filter(taxa == "Amphibians", rcp=="RCP2.6") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=dif)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  labs(tag="(a)", title="RCP2.6", y="Amphibians", x="") + 
  scale_fill_gradientn(name="", colors=bluewhitered, limits=c(min_lim1, max_lim1),
    values=scales::rescale(unique(c(seq(min_lim1, 0, length=5), 
                                    seq(0, max_lim1, length=5))))) + 
  theme_map() + theme(legend.position = "none", axis.text.x = element_blank(),
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p2 <- ensemble %>% filter(taxa == "Amphibians", rcp=="RCP6.0") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=dif)) + 
  labs(tag="(b)", title="RCP6.0") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colors=bluewhitered, limits=c(min_lim1, max_lim1),
    values=scales::rescale(unique(c(seq(min_lim1, 0, length=5), 
                                    seq(0, max_lim1, length=5))))) + 
  theme_map() + theme(axis.text=element_blank(),
                      legend.key.width = unit(0.3, "cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p3 <- ensemble %>% filter(taxa == "Birds", rcp=="RCP2.6") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=dif)) + 
  labs(tag="(c)", y="Birds", x="") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colors=bluewhitered, limits=c(min_lim2, max_lim2),
    values=scales::rescale(unique(c(seq(min_lim2, 0, length=5), 
                                    seq(0, max_lim2, length=5))))) + 
  theme_map() + theme(legend.position = "none", axis.text.x=element_blank(),
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p4 <- ensemble %>% filter(taxa == "Birds", rcp=="RCP6.0") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=dif)) + labs(tag="(d)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colors=bluewhitered, limits=c(min_lim2, max_lim2),
    values=scales::rescale(unique(c(seq(min_lim2, 0, length=5), 
                                    seq(0, max_lim2, length=5))))) + 
  theme_map() + theme(legend.key.width = unit(0.3, "cm"),
                      axis.text=element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p5 <- ensemble %>% filter(taxa == "Mammals", rcp=="RCP2.6") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=dif)) + 
  labs(tag="(e)", y="Mammals", x="") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colors=bluewhitered, limits=c(min_lim3, max_lim3),
    values=scales::rescale(unique(c(seq(min_lim3, 0, length=5), 
                                    seq(0, max_lim3, length=5))))) + 
  theme_map() + theme(legend.position = "none", 
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p6 <- ensemble %>% filter(taxa == "Mammals", rcp=="RCP6.0") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=dif)) + 
  labs(tag="(f)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colors=bluewhitered, limits=c(min_lim3, max_lim3),
    values=scales::rescale(unique(c(seq(min_lim3, 0, length=5), 
                                    seq(0, max_lim3, length=5))))) + 
  theme_map() + theme(legend.key.width=unit(0.3, "cm"),
                      axis.text.y = element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p <- p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol=2)
ggsave("figures/fut_sr_dif-1.png", p, width=8, height=6, dpi=600)

####################

### Figure S9+10

sr_future_mem <- rbind(read.csv("data/sr_predicted_mem_sub_GAM_poisson_eco_2080.csv.xz"),
                      read.csv("data/sr_predicted_mem_sub_GBM_poisson_eco_2080.csv.xz"))
sr_future_mem$model <- factor(sr_future_mem$model, labels=c("GAM", "GBM"))
sr_future_mem$taxa <- factor(sr_future_mem$taxa, 
                            labels=c("Amphibians", "Birds", "Mammals"))

# Calculate ensemble among different GCMs for RCP2.6 & RCP6.0
sr_mem_rcp26 <- sr_future_mem %>% 
  dplyr::select(c(x,y,model,taxa), matches("rcp26")) %>% 
  group_by(x,y,model,taxa) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,5:8], na.rm=TRUE)) %>% 
  dplyr::select(x,y,model,taxa,mn)
sr_mem_rcp60 <- sr_future_mem %>% 
  dplyr::select(c(x,y,model,taxa), matches("rcp60")) %>% 
  group_by(x,y,model,taxa) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,5:8], na.rm=TRUE)) %>% 
  dplyr::select(x,y,model,taxa,mn)

# Merge predicted SSDM and MEM richness
sr_ssdm_presence <- read.csv("data/sr_predicted_ssdm_presence_2080.csv.xz")
sr_ssdm_presence$dispersal <- "No dispersal"
sr_ssdm_disp1 <- read.csv("data/sr_predicted_ssdm_dispersal1_2080.csv.xz")
sr_ssdm_disp1$dispersal <- "d/4"
sr_ssdm_disp2 <- read.csv("data/sr_predicted_ssdm_dispersal2_2080.csv.xz")
sr_ssdm_disp2$dispersal <- "d/2"
sr_ssdm_disp3 <- read.csv("data/sr_predicted_ssdm_dispersal3_2080.csv.xz")
sr_ssdm_disp3$dispersal <- "d"
sr_ssdm_disp4 <- read.csv("data/sr_predicted_ssdm_dispersal4_2080.csv.xz")
sr_ssdm_disp4$dispersal <- "2*d"
sr_ssdm_fulldisp <- read.csv("data/sr_predicted_ssdm_fulldisp_2080.csv.xz")
sr_ssdm_fulldisp$dispersal <- "Full dispersal"

sr_ssdm <- rbind(sr_ssdm_presence, sr_ssdm_disp1, sr_ssdm_disp2, sr_ssdm_disp3, 
                 sr_ssdm_disp4, sr_ssdm_fulldisp)
rm(sr_ssdm_presence, sr_ssdm_disp1, sr_ssdm_disp2, sr_ssdm_disp3,
   sr_ssdm_disp4, sr_ssdm_fulldisp)
sr_ssdm$dispersal <- factor(sr_ssdm$dispersal, levels=c("No dispersal", "d/4",
                                                        "d/2", "d",
                                                        "2*d", "Full dispersal"))
sr_ssdm$taxa <- factor(sr_ssdm$taxa, labels=c("Amphibians", "Birds", "Mammals"))
sr_ssdm_rcp26 <- sr_ssdm %>% group_by(x,y,model,taxa,dispersal) %>%
  select(matches("rcp26")) %>% ungroup() %>% 
  mutate(sum=rowMeans(.[,-1:-5]))
sr_ssdm_rcp60 <- sr_ssdm %>% group_by(x,y,model,taxa,dispersal) %>%
  select(matches("rcp60")) %>% ungroup() %>% 
  mutate(sum=rowMeans(.[,-1:-5]))
sr_mem_ssdm_rcp26 <- inner_join(sr_ssdm_rcp26, sr_mem_rcp26)
sr_mem_ssdm_rcp60 <- inner_join(sr_ssdm_rcp60, sr_mem_rcp60)

## Create ensemble mean
ensemble_rcp26 <- sr_mem_ssdm_rcp26 %>% 
  group_by(x, y, dispersal, taxa) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T),
                   sum = mean(sum, na.rm=T)) %>% 
  data.frame()
ensemble_rcp60 <- sr_mem_ssdm_rcp60 %>% 
  group_by(x, y, dispersal, taxa) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T),
                   sum = mean(sum, na.rm=T)) %>% 
  data.frame()

# Plot MEM vs. S-SDM species richness
p1 <- ensemble_rcp26 %>% filter(taxa=="Amphibians") %>% 
  ggplot(aes(x=mn, y=sum)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  facet_wrap(dispersal ~ ., strip.position = "left", ncol=1) + 
  stat_poly_eq(aes(label = ..eq.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.15, parse=TRUE, col="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.05, parse=TRUE, col="#000000") + 
  scale_y_continuous("S-SDM richness", limits=c(0,150), expand=c(0,0)) + 
  scale_x_continuous("", limits=c(0,150), expand=c(0,0)) +
  ggtitle("Amphibians") + theme_plot() + 
  theme(strip.text = element_text(size=12, face="bold"),
        panel.spacing.y = unit(0.75, "lines"),
        strip.placement="outside", 
        plot.title = element_text(size=12, face="bold", hjust=0.5),
        axis.title.y = element_text(size = 14, face="bold", angle=90),
        strip.background= element_blank()) + coord_equal()
p2 <- ensemble_rcp26 %>% filter(taxa=="Birds") %>% 
  ggplot(aes(x=mn, y=sum)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  facet_wrap(dispersal ~ ., strip.position = "left", ncol=1) + 
  stat_poly_eq(aes(label = ..eq.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.15, parse=TRUE, col="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.05, parse=TRUE, col="#000000") + 
  scale_y_continuous("", limits=c(0,805), expand=c(0,0)) + 
  scale_x_continuous("MEM richness", limits=c(0,805), expand=c(0,0)) +
  ggtitle("Birds") + theme_plot() + 
  theme(strip.text = element_blank(),
        panel.spacing.y = unit(0.75, "lines"),
        plot.title = element_text(size=12, face="bold", hjust=0.5),
        axis.title.x = element_text(size = 14, face="bold"),
        strip.background= element_blank()) + coord_equal()
p3 <- ensemble_rcp26 %>% filter(taxa=="Mammals") %>% 
  ggplot(aes(x=mn, y=sum)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  facet_wrap(dispersal ~ ., strip.position = "left", ncol=1) + 
  stat_poly_eq(aes(label = ..eq.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.15, parse=TRUE, col="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.05, parse=TRUE, col="#000000") + 
  scale_y_continuous(" ", limits=c(0,255), expand=c(0,0)) + 
  scale_x_continuous(" ", limits=c(0,255), expand=c(0,0)) +
  ggtitle("Mammals") + theme_plot() + 
  theme(strip.text = element_blank(),
        panel.spacing.y = unit(0.75, "lines"),
        plot.title = element_text(size=12, face="bold", hjust=0.5),
        strip.background= element_blank()) + coord_equal()
p <- p1 + p2 + p3 + plot_layout(nrow=1)
ggsave("figures/fut_cor_disp_rcp26-1.png", p, dpi=600, width=9, height=15)
rm(p, p1, p2, p3)

# Plot MEM vs. S-SDM species richness
p1 <- ensemble_rcp60 %>% filter(taxa=="Amphibians") %>% 
  ggplot(aes(x=mn, y=sum)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  facet_wrap(dispersal ~ ., strip.position = "left", ncol=1) + 
  stat_poly_eq(aes(label = ..eq.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.15, parse=TRUE, col="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.05, parse=TRUE, col="#000000") + 
  scale_y_continuous("S-SDM richness", limits=c(0,150), expand=c(0,0)) + 
  scale_x_continuous("", limits=c(0,150), expand=c(0,0)) +
  ggtitle("Amphibians") + theme_plot() + 
  theme(strip.text = element_text(size=12, face="bold"),
        panel.spacing.y = unit(0.75, "lines"),
        strip.placement="outside", 
        plot.title = element_text(size=12, face="bold", hjust=0.5),
        axis.title.y = element_text(size = 14, face="bold", angle=90),
        strip.background= element_blank()) + coord_equal()
p2 <- ensemble_rcp60 %>% filter(taxa=="Birds") %>% 
  ggplot(aes(x=mn, y=sum)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  facet_wrap(dispersal ~ ., strip.position = "left", ncol=1) + 
  stat_poly_eq(aes(label = ..eq.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.15, parse=TRUE, col="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.05, parse=TRUE, col="#000000") + 
  scale_y_continuous("", limits=c(0,805), expand=c(0,0)) + 
  scale_x_continuous("MEM richness", limits=c(0,805), expand=c(0,0)) +
  ggtitle("Birds") + theme_plot() + 
  theme(strip.text = element_blank(),
        panel.spacing.y = unit(0.75, "lines"),
        plot.title = element_text(size=12, face="bold", hjust=0.5),
        axis.title.x = element_text(size = 14, face="bold"),
        strip.background= element_blank()) + coord_equal()
p3 <- ensemble_rcp60 %>% filter(taxa=="Mammals") %>% 
  ggplot(aes(x=mn, y=sum)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  facet_wrap(dispersal ~ ., strip.position = "left", ncol=1) + 
  stat_poly_eq(aes(label = ..eq.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.15, parse=TRUE, col="#000000") + 
  stat_poly_eq(aes(label = ..rr.label..), coef.digits=2, 
               formula = y ~ x, label.x.npc = 0.95, 
               label.y.npc = 0.05, parse=TRUE, col="#000000") + 
  scale_y_continuous(" ", limits=c(0,255), expand=c(0,0)) + 
  scale_x_continuous(" ", limits=c(0,255), expand=c(0,0)) +
  ggtitle("Mammals") + theme_plot() + 
  theme(strip.text = element_blank(),
        panel.spacing.y = unit(0.75, "lines"),
        plot.title = element_text(size=12, face="bold", hjust=0.5),
        strip.background= element_blank()) + coord_equal()
p <- p1 + p2 + p3 + plot_layout(nrow=1)
ggsave("figures/fut_cor_disp_rcp60-1.png", p, dpi=600, width=9, height=15)
rm(p, p1, p2, p3)

####################

### Figure S11

# Load current data
sr_current_ssdm <- read.csv("data/sr_predicted_ssdm_1995_dispersal1.csv.xz")
colnames(sr_current_ssdm) <- c("x", "y", "mn", "model", "taxa")
sr_current_ssdm$type <- "S-SDM"

sr_current_mem <- rbind(
  read.csv("data/sr_predicted_mem_sub_GAM_poisson_eco_1995.csv.xz"),
  read.csv("data/sr_predicted_mem_sub_GBM_poisson_eco_1995.csv.xz")) %>% 
  select(x,y,mean,model,taxa)
colnames(sr_current_mem) <- c("x", "y", "mn", "model", "taxa")
sr_current_mem$type <- "MEM"
sr_current <- bind_rows(sr_current_ssdm, sr_current_mem)
rm(sr_current_ssdm, sr_current_mem)

## Create ensemble mean among model algorithms
ensemble <- sr_current %>% group_by(x,y,taxa,type) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>%
  data.frame()
ensemble$model <- "Ensemble"
sr_current<- bind_rows(sr_current, ensemble); rm(ensemble)
sr_current$taxa <- factor(sr_current$taxa, labels=c("Amphibians", "Birds", "Mammals"))

# Load future data
sr_future_ssdm <- read.csv("data/sr_predicted_ssdm_dispersal1_2080.csv.xz")
sr_future_ssdm$type <- "S-SDM"

# Calculate ensemble among different GCMs for RCP2.6
sr_future_ssdm <- sr_future_ssdm %>% 
  dplyr::select(c(x,y,model,taxa,type), matches("rcp26")) %>% 
  group_by(x,y,model,taxa,type) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,6:9], na.rm=TRUE)) %>% 
  select(x,y,model,taxa,mn,type)

sr_future_mem <- rbind(
  read.csv("data/sr_predicted_mem_sub_GAM_poisson_eco_2080.csv.xz"),
  read.csv("data/sr_predicted_mem_sub_GBM_poisson_eco_2080.csv.xz"))

# Calculate ensemble among different GCMs for RCP2.6
sr_future_mem <- sr_future_mem %>% 
  dplyr::select(c(x,y,model,taxa), matches("rcp26")) %>% 
  group_by(x,y,model,taxa) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,5:8], na.rm=TRUE)) %>% 
  dplyr::select(x,y,model,taxa,mn)
sr_future_mem$type <- "MEM"

sr_future <- bind_rows(sr_future_ssdm, sr_future_mem)
rm(sr_future_ssdm, sr_future_mem)

## Create ensemble mean among models
ensemble <- sr_future %>% group_by(x,y,taxa,type) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>%
  data.frame()
ensemble$model <- "Ensemble"

sr_future <- bind_rows(sr_future, ensemble); rm(ensemble)
sr_future$taxa <- factor(sr_future$taxa, labels=c("Amphibians", "Birds", "Mammals"))

colnames(sr_current) <- c("x", "y", "EWEMBI_1995", "model", "taxa", "type")
sr_dif <- dplyr::inner_join(sr_current, sr_future) %>% 
  filter(model == "Ensemble")
sr_dif$delta <-  sr_dif$mn - sr_dif$EWEMBI_1995
sr_dif$rel_delta <- (sr_dif$mn - sr_dif$EWEMBI_1995)/sr_dif$EWEMBI_1995*100

# Create map
col_val1 <- scales::rescale(unique(c(seq(min(sr_dif$delta[sr_dif$taxa=="Amphibians"], na.rm=T), 0, length=5), seq(0, max(sr_dif$delta[sr_dif$taxa=="Amphibians"], na.rm=T), length=5))))
col_val2 <- scales::rescale(unique(c(seq(min(sr_dif$delta[sr_dif$taxa=="Birds"], na.rm=T), 0, length=5), seq(0, max(sr_dif$delta[sr_dif$taxa=="Birds"], na.rm=T), length=5))))
col_val3 <- scales::rescale(unique(c(seq(min(sr_dif$delta[sr_dif$taxa=="Mammals"], na.rm=T), 0, length=5), seq(0, max(sr_dif$delta[sr_dif$taxa=="Mammals"], na.rm=T), length=5))))

lim1 <- c(min(sr_dif$delta[sr_dif$taxa=="Amphibians"], na.rm=T),
          max(sr_dif$delta[sr_dif$taxa=="Amphibians"], na.rm=T))
lim2 <- c(min(sr_dif$delta[sr_dif$taxa=="Birds"], na.rm=T), 
          max(sr_dif$delta[sr_dif$taxa=="Birds"], na.rm=T))
lim3 <- c(min(sr_dif$delta[sr_dif$taxa=="Mammals"], na.rm=T),
          max(sr_dif$delta[sr_dif$taxa=="Mammals"], na.rm=T))

p1 <- sr_dif %>% filter(taxa == "Amphibians", type=="MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=delta)) + 
  labs(tag="(a)", title="MEM", x="", y="Amphibians") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) +  
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim1, values=col_val1) + 
  theme_map() + theme(legend.position = "none", axis.text.x = element_blank(), 
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p2 <- sr_dif %>% filter(taxa == "Amphibians", type=="S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=delta)) + 
  labs(tag="(b)", title="S-SDM") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim1, values=col_val1) + 
  theme_map() + theme(legend.key.width = unit(0.3, "cm"),
                      axis.text = element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p4 <- sr_dif %>% filter(taxa == "Birds", type=="MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=delta)) + 
  labs(tag="(d)", y="Birds", x="") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim2, values=col_val2) + 
  theme_map() + theme(legend.position = "none", axis.text.x = element_blank(),
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p5 <- sr_dif %>% filter(taxa == "Birds", type=="S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=delta)) + labs(tag="(e)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) +
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim2, values=col_val2) + 
  theme_map() + theme(axis.text = element_blank(), legend.key.width = unit(0.3, "cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

p7 <- sr_dif %>% filter(taxa == "Mammals", type=="MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=delta)) + 
  labs(tag="(g)", y="Mammals", x="") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim3, values=col_val3) + 
  theme_map() + theme(legend.position = "none",
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p8 <- sr_dif %>% filter(taxa == "Mammals", type=="S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=delta)) + 
  labs(tag="(h)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim3, values=col_val3) + 
  theme_map() + theme(axis.text.y = element_blank(),
                      legend.key.width = unit(0.3, "cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

sr_cor <- sr_dif %>% dplyr::select(x,y,model,taxa,type,delta) %>% 
  group_by(taxa) %>% tidyr::spread(type, delta)

(lim1 <- c(floor(min(sr_dif$delta[sr_dif$taxa=="Amphibians"], na.rm=T)/5), 
           ceiling(max(sr_dif$delta[sr_dif$taxa=="Amphibians"], na.rm=T)/5))*5)
(lim2 <-c(floor(min(sr_dif$delta[sr_dif$taxa=="Birds"], na.rm=T)/5), 
          ceiling(max(sr_dif$delta[sr_dif$taxa=="Birds"], na.rm=T)/5))*5)
(lim3 <- c(floor(min(sr_dif$delta[sr_dif$taxa=="Mammals"], na.rm=T)/5),
           ceiling(max(sr_dif$delta[sr_dif$taxa=="Mammals"], na.rm=T)/5))*5)

p3 <- ggplot(data=sr_cor[sr_cor$taxa=="Amphibians",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(c)") + 
  scale_x_continuous("", limits=lim1, expand=c(0,0)) + 
  scale_y_continuous("", limits=lim1, expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + 
  labs(tag="(c)") + coord_equal()
p6 <- ggplot(data=sr_cor[sr_cor$taxa=="Birds",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(f)") + 
  scale_x_continuous("", limits=lim2, expand=c(0,0)) + 
  scale_y_continuous("S-SDM richness", limits=lim2, expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9),
                       axis.title.y = element_text(size=12, face="bold", angle=90)) + 
  coord_equal()
p9 <- ggplot(data=sr_cor[sr_cor$taxa=="Mammals",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(i)") + 
  scale_x_continuous("MEM richness", limits=lim3, expand=c(0,0)) + 
  scale_y_continuous("", limits=lim3, expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + coord_equal()
p <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + 
  plot_layout(ncol=3, widths=c(2,2,1))
ggsave("figures/change_sub_rcp26-1.png", p, dpi = 600, width=9, height=6)

####################

### Figure S12

# Create map
sr_dif$rel_delta[is.infinite(sr_dif$rel_delta)] <- NA

col_val1 <- scales::rescale(unique(c(seq(min(sr_dif$rel_delta[sr_dif$taxa=="Amphibians"], na.rm=T), 0, length=5), seq(0, max(sr_dif$rel_delta[sr_dif$taxa=="Amphibians"], na.rm=T), length=5))))
col_val2 <- scales::rescale(unique(c(seq(min(sr_dif$rel_delta[sr_dif$taxa=="Birds"], na.rm=T), 0, length=5), seq(0, max(sr_dif$rel_delta[sr_dif$taxa=="Birds"], na.rm=T), length=5))))
col_val3 <- scales::rescale(unique(c(seq(min(sr_dif$rel_delta[sr_dif$taxa=="Mammals"], na.rm=T), 0, length=5), seq(0, max(sr_dif$rel_delta[sr_dif$taxa=="Mammals"], na.rm=T), length=5))))

lim1 <- c(min(sr_dif$rel_delta[sr_dif$taxa=="Amphibians"], na.rm=T),
          max(sr_dif$rel_delta[sr_dif$taxa=="Amphibians"], na.rm=T))
lim2 <- c(min(sr_dif$rel_delta[sr_dif$taxa=="Birds"], na.rm=T), 
          max(sr_dif$rel_delta[sr_dif$taxa=="Birds"], na.rm=T))
lim3 <- c(min(sr_dif$rel_delta[sr_dif$taxa=="Mammals"], na.rm=T),
          max(sr_dif$rel_delta[sr_dif$taxa=="Mammals"], na.rm=T))

p1 <- sr_dif %>% filter(taxa == "Amphibians", type=="MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=rel_delta)) + 
  labs(tag="(a)", title="MEM", y="Amphibians", x="") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) +  
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim1, values=col_val1) + 
  theme_map() + theme(legend.position = "none", axis.text.x = element_blank(),
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p2 <- sr_dif %>% filter(taxa == "Amphibians", type=="S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=rel_delta)) + 
  labs(tag="(b)", title="S-SDM") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim1, values=col_val1) + 
  theme_map() + theme(legend.key.width=unit(0.3, "cm"),
                      axis.text = element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p4 <- sr_dif %>% filter(taxa == "Birds", type=="MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=rel_delta)) + 
  labs(tag="(d)", y="Birds", x="") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim2, values=col_val2) + 
  theme_map() + theme(legend.position = "none", axis.text.x=element_blank(),
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p5 <- sr_dif %>% filter(taxa == "Birds", type=="S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=rel_delta)) + labs(tag="(e)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) +
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim2, values=col_val2) + 
  theme_map() + theme(legend.key.width=unit(0.3, "cm"),
                      axis.text = element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

p7 <- sr_dif %>% filter(taxa == "Mammals", type=="MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=rel_delta)) + 
  labs(tag="(g)", y="Mammals", x="") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim3, values=col_val3) + 
  theme_map() + theme(legend.position = "none",
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p8 <- sr_dif %>% filter(taxa == "Mammals", type=="S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=rel_delta)) + labs(tag="(h)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim3, values=col_val3) + 
  theme_map() + theme(legend.key.width=unit(0.3, "cm"),
                      axis.text.y = element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

sr_cor <- sr_dif %>% dplyr::select(x,y,model,taxa,type,rel_delta) %>% tidyr::spread(type, rel_delta)

(lim1 <- c(floor(min(sr_dif$rel_delta[sr_dif$taxa=="Amphibians"], na.rm=T)/5), 
  ceiling(max(sr_dif$rel_delta[sr_dif$taxa=="Amphibians"], na.rm=T)/5))*5)
(lim2 <-c(floor(min(sr_dif$rel_delta[sr_dif$taxa=="Birds"], na.rm=T)/5), 
  ceiling(max(sr_dif$rel_delta[sr_dif$taxa=="Birds"], na.rm=T)/5))*5)
(lim3 <- c(floor(min(sr_dif$rel_delta[sr_dif$taxa=="Mammals"], na.rm=T)/5),
  ceiling(max(sr_dif$rel_delta[sr_dif$taxa=="Mammals"], na.rm=T)/5))*5)

p3 <- ggplot(data=sr_cor[sr_cor$taxa=="Amphibians",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(c)") + 
  scale_x_continuous("", limits=lim1, expand=c(0,0)) + 
  scale_y_continuous("", limits=lim1, expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + 
  labs(tag="(c)") + coord_equal()
p6 <- ggplot(data=sr_cor[sr_cor$taxa=="Birds",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(f)") + 
  scale_x_continuous("", limits=lim2, expand=c(0,0)) + 
  scale_y_continuous("S-SDM richness", limits=lim2, expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9),
                       axis.title.y = element_text(size=12, face="bold", angle=90)) + coord_equal()
p9 <- ggplot(data=sr_cor[sr_cor$taxa=="Mammals",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(i)") + 
  scale_x_continuous("MEM richness", limits=lim3, expand=c(0,0)) + 
  scale_y_continuous("", limits=lim3, expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + coord_equal()
p <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(ncol=3, widths=c(2,2,1))
ggsave("figures/rel_change_sub_rcp26-1.png", p, dpi = 600, width=9, height=6)

####################

### Figure S13

# Load current data
sr_current_ssdm <- read.csv("data/sr_predicted_ssdm_1995_dispersal1.csv.xz")
colnames(sr_current_ssdm) <- c("x", "y", "mn", "model", "taxa")
sr_current_ssdm$type <- "S-SDM"

sr_current_mem <- rbind(
  read.csv("data/sr_predicted_mem_sub_GAM_poisson_eco_1995.csv.xz"),
  read.csv("data/sr_predicted_mem_sub_GBM_poisson_eco_1995.csv.xz")) %>% 
  select(x,y,mean,model,taxa)
colnames(sr_current_mem) <- c("x", "y", "mn", "model", "taxa")
sr_current_mem$type <- "MEM"
sr_current <- bind_rows(sr_current_ssdm, sr_current_mem)
rm(sr_current_ssdm, sr_current_mem)

## Create ensemble mean among model algorithms
ensemble <- sr_current %>% group_by(x,y,taxa,type) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>%
  data.frame()
ensemble$model <- "Ensemble"
sr_current<- bind_rows(sr_current, ensemble); rm(ensemble)
sr_current$taxa <- factor(sr_current$taxa, labels=c("Amphibians", "Birds", "Mammals"))

# Load future data
sr_future_ssdm <- read.csv("data/sr_predicted_ssdm_dispersal1_2080.csv.xz")
sr_future_ssdm$type <- "S-SDM"

# Calculate ensemble among different GCMs for RCP6.0
sr_future_ssdm <- sr_future_ssdm %>% 
  dplyr::select(c(x,y,model,taxa,type), matches("rcp60")) %>% 
  group_by(x,y,model,taxa,type) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,6:9], na.rm=TRUE)) %>% 
  select(x,y,model,taxa,mn,type)

sr_future_mem <- rbind(
  read.csv("data/sr_predicted_mem_sub_GAM_poisson_eco_2080.csv.xz"),
  read.csv("data/sr_predicted_mem_sub_GBM_poisson_eco_2080.csv.xz"))

# Calculate ensemble among different GCMs for RCP6.0
sr_future_mem <- sr_future_mem %>% 
  dplyr::select(c(x,y,model,taxa), matches("rcp60")) %>% 
  group_by(x,y,model,taxa) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,5:8], na.rm=TRUE)) %>% 
  dplyr::select(x,y,model,taxa,mn)
sr_future_mem$type <- "MEM"

sr_future <- bind_rows(sr_future_ssdm, sr_future_mem)
rm(sr_future_ssdm, sr_future_mem)

## Create ensemble mean among models
ensemble <- sr_future %>% group_by(x,y,taxa,type) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>%
  data.frame()
ensemble$model <- "Ensemble"

sr_future <- bind_rows(sr_future, ensemble); rm(ensemble)
sr_future$taxa <- factor(sr_future$taxa, 
                         labels=c("Amphibians", "Birds", "Mammals"))

colnames(sr_current) <- c("x", "y", "EWEMBI_1995", "model", "taxa", "type")
sr_dif <- dplyr::inner_join(sr_current, sr_future) %>% 
  filter(model == "Ensemble")
sr_dif$delta <-  sr_dif$mn - sr_dif$EWEMBI_1995
sr_dif$rel_delta <- (sr_dif$mn - sr_dif$EWEMBI_1995)/sr_dif$EWEMBI_1995*100

# Create map
col_val1 <- scales::rescale(unique(c(seq(min(sr_dif$delta[sr_dif$taxa=="Amphibians"], na.rm=T), 0, length=5), seq(0, max(sr_dif$delta[sr_dif$taxa=="Amphibians"], na.rm=T), length=5))))
col_val2 <- scales::rescale(unique(c(seq(min(sr_dif$delta[sr_dif$taxa=="Birds"], na.rm=T), 0, length=5), seq(0, max(sr_dif$delta[sr_dif$taxa=="Birds"], na.rm=T), length=5))))
col_val3 <- scales::rescale(unique(c(seq(min(sr_dif$delta[sr_dif$taxa=="Mammals"], na.rm=T), 0, length=5), seq(0, max(sr_dif$delta[sr_dif$taxa=="Mammals"], na.rm=T), length=5))))

(lim1 <- c(min(sr_dif$delta[sr_dif$taxa=="Amphibians"], na.rm=T),
          max(sr_dif$delta[sr_dif$taxa=="Amphibians"], na.rm=T)))
(lim2 <- c(min(sr_dif$delta[sr_dif$taxa=="Birds"], na.rm=T), 
          max(sr_dif$delta[sr_dif$taxa=="Birds"], na.rm=T)))
(lim3 <- c(min(sr_dif$delta[sr_dif$taxa=="Mammals"], na.rm=T),
          max(sr_dif$delta[sr_dif$taxa=="Mammals"], na.rm=T)))

p1 <- sr_dif %>% filter(taxa == "Amphibians", type=="MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=delta)) + 
  labs(tag="(a)", title="MEM", y="Amphibians", x="") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) +  
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim1, values=col_val1) + 
  theme_map() + theme(legend.position = "none", axis.text.x = element_blank(),
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p2 <- sr_dif %>% filter(taxa == "Amphibians", type=="S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=delta)) + 
  labs(tag="(b)", title="S-SDM") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim1, values=col_val1) + 
  theme_map() + theme(legend.key.width=unit(0.3, "cm"),
                      axis.text = element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p4 <- sr_dif %>% filter(taxa == "Birds", type=="MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=delta)) + 
  labs(tag="(d)", y="Birds", x="") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim2, values=col_val2) + 
  theme_map() + theme(legend.position = "none", axis.text.x = element_blank(),
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p5 <- sr_dif %>% filter(taxa == "Birds", type=="S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=delta)) + labs(tag="(e)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) +
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim2, values=col_val2) + 
  theme_map() + theme(legend.key.width=unit(0.3, "cm"),
                      axis.text = element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

p7 <- sr_dif %>% filter(taxa == "Mammals", type=="MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=delta)) + 
  labs(tag="(g)", y="Mammals", x="") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim3, values=col_val3) + 
  theme_map() + theme(legend.position = "none",
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p8 <- sr_dif %>% filter(taxa == "Mammals", type=="S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=delta)) + labs(tag="(h)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim3, values=col_val3) + 
  theme_map() + theme(legend.key.width=unit(0.3, "cm"),
                      axis.text.y = element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

sr_cor <- sr_dif %>% dplyr::select(x,y,model,taxa,type,delta) %>% 
  tidyr::spread(type, delta)

(lim1 <- c(floor(min(sr_dif$delta[sr_dif$taxa=="Amphibians"], na.rm=T)/5), 
           ceiling(max(sr_dif$delta[sr_dif$taxa=="Amphibians"], na.rm=T)/5))*5)
(lim2 <-c(floor(min(sr_dif$delta[sr_dif$taxa=="Birds"], na.rm=T)/5), 
          ceiling(max(sr_dif$delta[sr_dif$taxa=="Birds"], na.rm=T)/5))*5)
(lim3 <- c(floor(min(sr_dif$delta[sr_dif$taxa=="Mammals"], na.rm=T)/5),
           ceiling(max(sr_dif$delta[sr_dif$taxa=="Mammals"], na.rm=T)/5))*5)

p3 <- ggplot(data=sr_cor[sr_cor$taxa=="Amphibians",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(c)") + 
  scale_x_continuous("", limits=lim1, expand=c(0,0)) + 
  scale_y_continuous("", limits=lim1, expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + 
  labs(tag="(c)") + coord_equal()
p6 <- ggplot(data=sr_cor[sr_cor$taxa=="Birds",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(f)") + 
  scale_x_continuous("", limits=lim2, expand=c(0,0)) + 
  scale_y_continuous("S-SDM richness", limits=lim2, expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9),
                       axis.title.y = element_text(size=12, face="bold", angle=90)) + coord_equal()
p9 <- ggplot(data=sr_cor[sr_cor$taxa=="Mammals",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(i)") + 
  scale_x_continuous("MEM richness", limits=lim3, expand=c(0,0)) + 
  scale_y_continuous("", limits=lim3, expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + coord_equal()
p <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(ncol=3, widths=c(2,2,1))
ggsave("figures/change_sub_rcp60-1.png", p, dpi = 600, width=9, height=6)

####################

### Figure S14

# Create map
sr_dif$rel_delta[is.infinite(sr_dif$rel_delta)] <- NA
col_val1 <- scales::rescale(unique(c(seq(min(sr_dif$rel_delta[sr_dif$taxa=="Amphibians"], na.rm=T), 0, length=5), seq(0, max(sr_dif$rel_delta[sr_dif$taxa=="Amphibians"], na.rm=T), length=5))))
col_val2 <- scales::rescale(unique(c(seq(min(sr_dif$rel_delta[sr_dif$taxa=="Birds"], na.rm=T), 0, length=5), seq(0, max(sr_dif$rel_delta[sr_dif$taxa=="Birds"], na.rm=T), length=5))))
col_val3 <- scales::rescale(unique(c(seq(min(sr_dif$rel_delta[sr_dif$taxa=="Mammals"], na.rm=T), 0, length=5), seq(0, max(sr_dif$rel_delta[sr_dif$taxa=="Mammals"], na.rm=T),
                                                                                                                   length=5))))
lim1 <- c(min(sr_dif$rel_delta[sr_dif$taxa=="Amphibians"], na.rm=T),
          max(sr_dif$rel_delta[sr_dif$taxa=="Amphibians"], na.rm=T))
lim2 <- c(min(sr_dif$rel_delta[sr_dif$taxa=="Birds"], na.rm=T), 
          max(sr_dif$rel_delta[sr_dif$taxa=="Birds"], na.rm=T))
lim3 <- c(min(sr_dif$rel_delta[sr_dif$taxa=="Mammals"], na.rm=T),
          max(sr_dif$rel_delta[sr_dif$taxa=="Mammals"], na.rm=T))

p1 <- sr_dif %>% filter(taxa == "Amphibians", type=="MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=rel_delta)) + 
  labs(tag="(a)", title="MEM", y="Amphibians", x="") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) +  
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim1, values=col_val1) + 
  theme_map() + theme(legend.position = "none", axis.text.x=element_blank(),
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p2 <- sr_dif %>% filter(taxa == "Amphibians", type=="S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=rel_delta)) + 
  labs(tag="(b)", title="S-SDM") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim1, values=col_val1) + 
  theme_map() + theme(legend.key.width=unit(0.3, "cm"),
                      axis.text=element_blank()) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p4 <- sr_dif %>% filter(taxa == "Birds", type=="MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=rel_delta)) + 
  labs(tag="(d)", y="Birds", x="") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim2, values=col_val2) + 
  theme_map() + theme(legend.position = "none", axis.text.x=element_blank(),
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p5 <- sr_dif %>% filter(taxa == "Birds", type=="S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=rel_delta)) + labs(tag="(e)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) +
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim2, values=col_val2) + 
  theme_map() + theme(legend.key.width=unit(0.3, "cm"),
                      axis.text=element_blank()) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p7 <- sr_dif %>% filter(taxa == "Mammals", type=="MEM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=rel_delta)) + 
  labs(tag="(g)", y="Mammals", x="") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim3, values=col_val3) + 
  theme_map() + theme(legend.position = "none",
                      axis.title = element_text(size=10, face="bold", angle=90)) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p8 <- sr_dif %>% filter(taxa == "Mammals", type=="S-SDM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=rel_delta)) + labs(tag="(h)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered,
                       na.value="transparent", limits=lim3, values=col_val3) + 
  theme_map() + theme(legend.key.width=unit(0.3, "cm"),
                      axis.text.y=element_blank()) +
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

sr_cor <- sr_dif %>% dplyr::select(x,y,model,taxa,type,rel_delta) %>% tidyr::spread(type, rel_delta)


(lim1 <- c(floor(min(sr_dif$rel_delta[sr_dif$taxa=="Amphibians"], na.rm=T)/5), 
           ceiling(max(sr_dif$rel_delta[sr_dif$taxa=="Amphibians"], na.rm=T)/5))*5)
(lim2 <-c(floor(min(sr_dif$rel_delta[sr_dif$taxa=="Birds"], na.rm=T)/5), 
          ceiling(max(sr_dif$rel_delta[sr_dif$taxa=="Birds"], na.rm=T)/5))*5)
(lim3 <- c(floor(min(sr_dif$rel_delta[sr_dif$taxa=="Mammals"], na.rm=T)/5),
           ceiling(max(sr_dif$rel_delta[sr_dif$taxa=="Mammals"], na.rm=T)/5))*5)

p3 <- ggplot(data=sr_cor[sr_cor$taxa=="Amphibians",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(c)") + 
  scale_x_continuous("", limits=lim1, expand=c(0,0)) + 
  scale_y_continuous("", limits=lim1, expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + 
  labs(tag="(c)") + coord_equal()
p6 <- ggplot(data=sr_cor[sr_cor$taxa=="Birds",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(f)") + 
  scale_x_continuous("", limits=lim2, expand=c(0,0)) + 
  scale_y_continuous("S-SDM richness", limits=lim2, expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9),
                       axis.title.y = element_text(size=12, face="bold", angle=90)) + coord_equal()
p9 <- ggplot(data=sr_cor[sr_cor$taxa=="Mammals",], aes(x=MEM, y=`S-SDM`)) + 
  geom_point(alpha=0.5, pch=1, colour="#A7A7A7") + 
  geom_smooth(method = "lm", colour="#000000", size=0.75) + 
  geom_abline(col="grey30", linetype="dashed", size=0.75) + 
  stat_poly_eq(aes(label = ..rr.label..),
               formula = y ~ x, label.x.npc = 0.5, col="#000000",
               label.y.npc = 0.9, parse=TRUE) + labs(tag="(i)") + 
  scale_x_continuous("MEM richness", limits=lim3, expand=c(0,0)) + 
  scale_y_continuous("", limits=lim3, expand=c(0,0)) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + coord_equal()
p <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(ncol=3, widths=c(2,2,1))
ggsave("figures/rel_change_sub_rcp60-1.png", p, dpi = 600, width=9, height=6)

####################

### Figure S15

sr_future_mem <- rbind(read.csv("data/sr_predicted_mem_sub_GAM_poisson_eco_2080.csv.xz"),
                       read.csv("data/sr_predicted_mem_sub_GBM_poisson_eco_2080.csv.xz"))
sr_future_mem$model <- factor(sr_future_mem$model, labels=c("GAM", "GBM"))
colnames(sr_future_mem)[4] <- "taxa"
sr_future_mem$taxa <- factor(sr_future_mem$taxa, 
                             labels=c("Amphibians", "Birds", "Mammals"))

# Calculate ensemble among different GCMs for RCP2.6
sr_mem <- sr_future_mem %>% 
  dplyr::select(c(x,y,model,taxa), matches("rcp26")) %>% 
  group_by(x,y,model,taxa) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,5:8], na.rm=TRUE)) %>% 
  dplyr::select(x,y,model,taxa,mn)

sr_mem_all <- rbind(read.csv("data/sr_predicted_mem_all_GAM_poisson_eco_2080.csv.xz"),
                       read.csv("data/sr_predicted_mem_all_GBM_poisson_eco_2080.csv.xz"))
sr_mem_all$model <- factor(sr_mem_all$model, labels=c("GAM", "GBM"))
colnames(sr_mem_all)[4] <- "taxa"
sr_mem_all$taxa <- factor(sr_mem_all$taxa, labels=c("Amphibians", "Birds", "Mammals"))

# Calculate ensemble among different GCMs for RCP2.6
sr_mem_all <- sr_mem_all %>% 
  dplyr::select(c(x,y,model,taxa), matches("rcp26")) %>% 
  group_by(x,y,model,taxa) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,5:8], na.rm=TRUE)) %>% 
  dplyr::select(x,y,model,taxa,mn)

## Create ensemble mean
sr_mem <- sr_mem %>% group_by(x, y, taxa) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>% data.frame()
sr_mem$model <- "Ensemble"
sr_mem_all <- sr_mem_all %>% group_by(x, y, taxa) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>% data.frame()
sr_mem_all$model <- "Ensemble"

# Create map
max_lim1 <- round(max(sr_mem_all$mn[sr_mem_all$taxa=="Amphibians"]), digits=-1)
max_lim2 <- round(max(sr_mem_all$mn[sr_mem_all$taxa=="Birds"]), digits=-1)
max_lim3 <- round(max(sr_mem_all$mn[sr_mem_all$taxa=="Mammals"]), digits=-1)
p1 <- sr_mem_all %>% filter(taxa == "Amphibians") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + labs(title="MEM All", tag="(a)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim1), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(axis.text=element_blank(),
                      legend.key.width=unit(0.3,"cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p4 <- sr_mem_all %>% filter(taxa == "Birds") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + labs(tag="(d)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim2), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(axis.text = element_blank(),
                      legend.key.width=unit(0.3,"cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p7 <- sr_mem_all %>% filter(taxa == "Mammals") %>% 
  ggplot() + 
  geom_hline(yintercept=0, colour="grey40", 
             linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + labs(tag="(g)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim3), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(strip.placement="none", axis.text.y=element_blank(),
                      legend.key.width=unit(0.3,"cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

sr_mem_dif <- inner_join(sr_mem_all,sr_mem,by=c("x","y","taxa","model"))
sr_mem_dif$dif <- sr_mem_dif$mn.x - sr_mem_dif$mn.y
sr_mem_dif$perc_dif <- round(sr_mem_dif$dif/sr_mem_dif$mn.y*100, digits=0)

sr_mem_dif %>% summarise(mean(perc_dif))
sr_mem_dif %>% group_by(taxa) %>% summarise(mean(perc_dif))

max_lim1 <- round(max(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Amphibians"]), digits=-1)
max_lim2 <- round(max(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Birds"]), digits=-1)
max_lim3 <- round(max(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Mammals"]), digits=-1)
min_lim1 <- round(min(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Amphibians"]), digits=-1)
min_lim2 <- round(min(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Birds"]), digits=-1)
min_lim3 <- round(min(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Mammals"]), digits=-1)
p2 <- sr_mem_dif %>% filter(taxa == "Amphibians") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=perc_dif)) + 
  labs(title="MEM All - MEM (%)", tag="(b)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered, limits=c(min_lim1, max_lim1),
                       values=scales::rescale(unique(c(seq(min_lim1, 0, length=5), 
                                                              seq(0, max_lim1, length=5))))) + 
  theme_map() + theme(axis.text=element_blank(),
                      legend.key.width=unit(0.3,"cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p5 <- sr_mem_dif %>% filter(taxa == "Birds") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=perc_dif)) + labs(tag="(e)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colors=bluewhitered, limits=c(min_lim2, max_lim2),
    values=scales::rescale(unique(c(seq(min_lim2, 0, length=5), 
                                    seq(0, max_lim2, length=5))))) + 
  theme_map() + theme(axis.text = element_blank(),
                      legend.key.width=unit(0.3,"cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p8 <- sr_mem_dif %>% filter(taxa == "Mammals") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=perc_dif)) + labs(tag="(h)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colors=bluewhitered, limits=c(min_lim3, max_lim3),
    values=scales::rescale(unique(c(seq(min_lim3, 0, length=5), 
                                    seq(0, max_lim3, length=5))))) + 
  theme_map() + theme(strip.placement="none", axis.text.y=element_blank(),
                      legend.key.width=unit(0.3,"cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

# Add histograms of Residuals
load("data/landseamask_generic.rda")
area_df <- raster::area(landseamask_generic)
area_df <- as.data.frame(raster::rasterToPoints(area_df))

sr_mem_dif <- sr_mem_dif %>% left_join(area_df) %>% 
  group_by(perc_dif, taxa) %>% summarise(layer=sum(layer,na.rm=T))
p3 <- sr_mem_dif %>% filter(taxa == "Amphibians") %>% 
  ggplot() + geom_bar(aes(x=perc_dif, y=layer/10000), stat="identity") + 
  scale_y_continuous(limits=c(0,NA), expand = expand_scale(mult = c(0, .1))) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + labs(x="", tag="(c)")
p6 <- sr_mem_dif %>% filter(taxa == "Birds") %>% 
  ggplot() + geom_bar(aes(x=perc_dif, y=layer/10000), stat="identity") + 
  labs(x="", y="Area (ha)") + scale_y_continuous(limits=c(0,NA), expand = expand_scale(mult = c(0, .1))) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + labs(tag="(f)") + 
  theme(axis.title.y=element_text(size=12, face="bold", angle=90))
p9 <- sr_mem_dif %>% filter(taxa == "Mammals") %>% 
  ggplot() + geom_bar(aes(x=perc_dif, y=layer/10000), stat="identity") + 
  labs(x="MEM All - MEM (%)") + scale_y_continuous(limits=c(0,NA), expand = expand_scale(mult = c(0, .1))) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + labs(tag="(i)") 
  
p <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + 
  plot_layout(ncol=3, width=c(2,2,1))
ggsave("figures/mem_fut_rcp26_sr_sub_full-1.png", p, width=9, height=6, dpi=600)

####################

### Figure S16

sr_future_mem <- rbind(read.csv("data/sr_predicted_mem_sub_GAM_poisson_eco_2080.csv.xz"),
                       read.csv("data/sr_predicted_mem_sub_GBM_poisson_eco_2080.csv.xz"))
sr_future_mem$model <- factor(sr_future_mem$model, labels=c("GAM", "GBM"))
colnames(sr_future_mem)[4] <- "taxa"
sr_future_mem$taxa <- factor(sr_future_mem$taxa, 
                             labels=c("Amphibians", "Birds", "Mammals"))

# Calculate ensemble among different GCMs for RCP6.0
sr_mem <- sr_future_mem %>% 
  dplyr::select(c(x,y,model,taxa), matches("rcp60")) %>% 
  group_by(x,y,model,taxa) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,5:8], na.rm=TRUE)) %>% 
  dplyr::select(x,y,model,taxa,mn)

sr_mem_all <- rbind(read.csv("data/sr_predicted_mem_all_GAM_poisson_eco_2080.csv.xz"),
                    read.csv("data/sr_predicted_mem_all_GBM_poisson_eco_2080.csv.xz"))
sr_mem_all$model <- factor(sr_mem_all$model, labels=c("GAM", "GBM"))
colnames(sr_mem_all)[4] <- "taxa"
sr_mem_all$taxa <- factor(sr_mem_all$taxa, labels=c("Amphibians", "Birds", "Mammals"))

# Calculate ensemble among different GCMs for RCP6.0
sr_mem_all <- sr_mem_all %>% 
  dplyr::select(c(x,y,model,taxa), matches("rcp60")) %>% 
  group_by(x,y,model,taxa) %>% ungroup() %>% 
  mutate(mn = rowMeans(.[,5:8], na.rm=TRUE)) %>% 
  dplyr::select(x,y,model,taxa,mn)

## Create ensemble mean
sr_mem <- sr_mem %>% group_by(x, y, taxa) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>% data.frame()
sr_mem$model <- "Ensemble"
sr_mem_all <- sr_mem_all %>% group_by(x, y, taxa) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T)) %>% data.frame()
sr_mem_all$model <- "Ensemble"

# Create map
max_lim1 <- round(max(sr_mem_all$mn[sr_mem_all$taxa=="Amphibians"]), digits=-1)
max_lim2 <- round(max(sr_mem_all$mn[sr_mem_all$taxa=="Birds"]), digits=-1)
max_lim3 <- round(max(sr_mem_all$mn[sr_mem_all$taxa=="Mammals"]), digits=-1)
p1 <- sr_mem_all %>% filter(taxa == "Amphibians") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + labs(title="MEM All", tag="(a)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim1), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(axis.text=element_blank(),
                      legend.key.width=unit(0.3,"cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p4 <- sr_mem_all %>% filter(taxa == "Birds") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + labs(tag="(d)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim2), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(axis.text = element_blank(),
                      legend.key.width=unit(0.3,"cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p7 <- sr_mem_all %>% filter(taxa == "Mammals") %>% 
  ggplot() + 
  geom_hline(yintercept=0, colour="grey40", 
             linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=mn)) + labs(tag="(g)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", limits=c(0, max_lim3), colours=bluered,
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(strip.placement="none", axis.text.y=element_blank(),
                      legend.key.width=unit(0.3,"cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

sr_mem_dif <- inner_join(sr_mem_all,sr_mem,by=c("x","y","taxa","model"))
sr_mem_dif$dif <- sr_mem_dif$mn.x - sr_mem_dif$mn.y
sr_mem_dif$perc_dif <- round(sr_mem_dif$dif/sr_mem_dif$mn.y*100, digits=0)

sr_mem_dif %>% summarise(mean(perc_dif))
sr_mem_dif %>% group_by(taxa) %>% summarise(mean(perc_dif))

max_lim1 <- round(max(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Amphibians"]), digits=-1)
max_lim2 <- round(max(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Birds"]), digits=-1)
max_lim3 <- round(max(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Mammals"]), digits=-1)
min_lim1 <- round(min(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Amphibians"]), digits=-1)
min_lim2 <- round(min(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Birds"]), digits=-1)
min_lim3 <- round(min(sr_mem_dif$perc_dif[sr_mem_dif$taxa=="Mammals"]), digits=-1)
p2 <- sr_mem_dif %>% filter(taxa == "Amphibians") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=perc_dif)) + 
  labs(title="MEM All - MEM (%)", tag="(b)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colours=bluewhitered, limits=c(min_lim1, max_lim1),
                       values=scales::rescale(unique(c(seq(min_lim1, 0, length=5), 
                                                       seq(0, max_lim1, length=5))))) + 
  theme_map() + theme(axis.text=element_blank(),
                      legend.key.width=unit(0.3,"cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p5 <- sr_mem_dif %>% filter(taxa == "Birds") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=perc_dif)) + labs(tag="(e)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colors=bluewhitered, limits=c(min_lim2, max_lim2),
                       values=scales::rescale(unique(c(seq(min_lim2, 0, length=5), 
                                                       seq(0, max_lim2, length=5))))) + 
  theme_map() + theme(axis.text = element_blank(),
                      legend.key.width=unit(0.3,"cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p8 <- sr_mem_dif %>% filter(taxa == "Mammals") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=perc_dif)) + labs(tag="(h)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="", colors=bluewhitered, limits=c(min_lim3, max_lim3),
                       values=scales::rescale(unique(c(seq(min_lim3, 0, length=5), 
                                                       seq(0, max_lim3, length=5))))) + 
  theme_map() + theme(strip.placement="none", axis.text.y=element_blank(),
                      legend.key.width=unit(0.3,"cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

# Add histograms of Residuals
load("data/landseamask_generic.rda")
area_df <- raster::area(landseamask_generic)
area_df <- as.data.frame(raster::rasterToPoints(area_df))

sr_mem_dif <- sr_mem_dif %>% left_join(area_df) %>% 
  group_by(perc_dif, taxa) %>% summarise(layer=sum(layer,na.rm=T))
p3 <- sr_mem_dif %>% filter(taxa == "Amphibians") %>% 
  ggplot() + geom_bar(aes(x=perc_dif, y=layer/10000), stat="identity") + 
  scale_y_continuous(limits=c(0,NA), expand = expand_scale(mult = c(0, .1))) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + labs(x="", tag="(c)")
p6 <- sr_mem_dif %>% filter(taxa == "Birds") %>% 
  ggplot() + geom_bar(aes(x=perc_dif, y=layer/10000), stat="identity") + 
  labs(x="", y="Area (ha)") + scale_y_continuous(limits=c(0,NA), expand = expand_scale(mult = c(0, .1))) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + labs(tag="(f)") + 
  theme(axis.title.y=element_text(size=12, face="bold", angle=90))
p9 <- sr_mem_dif %>% filter(taxa == "Mammals") %>% 
  ggplot() + geom_bar(aes(x=perc_dif, y=layer/10000), stat="identity") + 
  labs(x="MEM All - MEM (%)") + scale_y_continuous(limits=c(0,NA), expand = expand_scale(mult = c(0, .1))) + 
  theme_plot() + theme(plot.tag.position = c(-0.05,0.9)) + labs(tag="(i)") 

p <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + 
  plot_layout(ncol=3, width=c(2,2,1))
ggsave("figures/mem_fut_rcp60_sr_sub_full-1.png", p, width=9, height=6, dpi=600)

####################

### Figure 4 + Figure S17

# Load data
#sr_future_ssdm <- read.csv("data/sr_predicted_ssdm_dispersal1_2080.csv.xz") %>% 
#  tidyr::gather(gcm, sr, -c(taxa,x,y,model))
#sr_future_ssdm$taxa <- factor(sr_future_ssdm$taxa, labels=c("Amphibians", "Birds", "Mammals"))
#sr_future_ssdm$model <- factor(sr_future_ssdm$model, labels=c("GAM", "GBM")) 
#colnames(sr_future_ssdm)[6] <- "S-SDM"

#sr_future_mem <- rbind(
#  read.csv("data/sr_predicted_mem_sub_GAM_poisson_eco_2080.csv.xz"),
#  read.csv("data/sr_predicted_mem_sub_GBM_poisson_eco_2080.csv.xz")) %>%
#  tidyr::gather(gcm, sr, -c(taxa,x,y,model))
#sr_future_mem$taxa <- factor(sr_future_mem$taxa, labels=c("Amphibians", "Birds", "Mammals"))
#sr_future_mem$model <- factor(sr_future_mem$model, labels=c("GAM", "GBM")) 
#colnames(sr_future_mem)[6] <- c("MEM")

#data <- sr_future_ssdm %>% inner_join(sr_future_mem) %>%
#  group_by(x,y,model,gcm,taxa) %>% 
#  summarise(MEM=sum(MEM), `S-SDM`=sum(`S-SDM`)) %>%
#  tidyr::gather(type, sr, -c(x,y,model,gcm,taxa))
#rm(sr_future_ssdm, sr_future_mem)  
#data %<>% 
#  tidyr::separate(gcm, c("gcm", "scenario"), sep="_", extra="drop") %>% 
#  tidyr::unite(tmg, type, model, gcm) %>% 
#  group_by(scenario, taxa) %>% tidyr::unite(xy, x,y) %>%
#  tidyr::spread(xy,sr)

#hc1 <- data %>% filter(scenario == "rcp26", taxa == "Amphibians") %>%
#  tibble::column_to_rownames("tmg") %>% dist %>% hclust
#hc2 <- data %>% filter(scenario == "rcp26", taxa == "Birds") %>%
#  tibble::column_to_rownames("tmg") %>% dist %>% hclust
#hc3 <- data %>% filter(scenario == "rcp26", taxa == "Mammals") %>%
#  tibble::column_to_rownames("tmg") %>% dist %>% hclust
#hc4 <- data %>% filter(scenario == "rcp60", taxa == "Amphibians") %>%
#  tibble::column_to_rownames("tmg") %>% dist %>% hclust
#hc5 <- data %>% filter(scenario == "rcp60", taxa == "Birds") %>%
#  tibble::column_to_rownames("tmg") %>% dist %>% hclust
#hc6 <- data %>% filter(scenario == "rcp60", taxa == "Mammals") %>%
#  tibble::column_to_rownames("tmg") %>% dist %>% hclust

# Warning messages:
# In dist(.) : NAs introduced by coercion
#hc <- list(hc1, hc2, hc3, hc4, hc5, hc6)
#saveRDS(hc, "data/dendro.RData", compress="xz")

hc <- readRDS("data/dendro.RData")
dendro_df <- lapply(1:length(hc), function(x){
  dhc <- as.dendrogram(hc[[x]])
  ddata <- ggdendro::dendro_data(dhc, type="rectangle")
  df <- ggdendro::segment(ddata)
  df$taxa <- c("Amphibians", "Birds", "Mammals", 
               "Amphibians", "Birds", "Mammals")[x]
  df$scenario <- rep(c("rcp26", "rcp60"),3)[x]
  df$max <- max(hc[[x]]$height)
  return(df)
})
dendro_df <- bind_rows(dendro_df)

dendro_df %>% filter(scenario =="rcp26") %>% 
  ggplot() + 
  geom_segment(aes(x = x, y = y/max, xend = xend, yend = yend/max)) + 
  coord_flip() + theme_classic() + facet_wrap(.~taxa) + 
  scale_x_continuous(name="", breaks=c(1:16), labels=hc[[1]]$labels) + 
  scale_y_continuous(name="Similarity", 
                     labels=c(100,75,50,25,0),
                     limits=c(0,1), expand=c(0,0)) + 
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(face="bold"),
        panel.spacing.x = unit(1, "lines"),
        strip.background = element_blank(), 
        strip.text=element_text(size=12, face="bold"))
ggsave("figures/dendro_rcp26-1.png", width=8, height=5, dpi=600)
ggsave("figures/Figure4.pdf", width=8, height=5)

dendro_df %>% filter(scenario =="rcp60") %>% 
  ggplot() + 
  geom_segment(aes(x = x, y = y/max, xend = xend, yend = yend/max)) + 
  coord_flip() + theme_classic() + facet_wrap(.~taxa) + 
  scale_x_continuous(name="", breaks=c(1:16), labels=hc[[1]]$labels) + 
  scale_y_continuous(name="Similarity", 
                     labels=c(100,75,50,25,0),
                     limits=c(0,1), expand=c(0,0)) + 
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(face="bold"),
        panel.spacing.x = unit(1, "lines"),
        strip.background = element_blank(), 
        strip.text=element_text(size=12, face="bold"))
ggsave("figures/dendro_rcp60-1.png", width=8, height=5, dpi=600)

####################

### Figure 5 + Figure S18-20

# Load data
#sr_future_ssdm <- read.csv("data/sr_predicted_ssdm_dispersal1_2080.csv.xz") %>% 
#  tidyr::gather(gcm, sr, -c(taxa,x,y,model))
#sr_future_ssdm$taxa <- factor(sr_future_ssdm$taxa, labels=c("Amphibians", "Birds", "Mammals"))
#sr_future_ssdm$model <- factor(sr_future_ssdm$model, labels=c("GAM", "GBM"))
#colnames(sr_future_ssdm)[6] <- "S-SDM"

#sr_future_mem <- rbind(
#  read.csv("data/sr_predicted_mem_sub_GAM_poisson_eco_2080.csv.xz"),
#  read.csv("data/sr_predicted_mem_sub_GBM_poisson_eco_2080.csv.xz")) %>%
#  tidyr::gather(gcm, sr, -c(taxa,x,y,model))
#sr_future_mem$taxa <- factor(sr_future_mem$taxa, labels=c("Amphibians", "Birds", "Mammals"))
#sr_future_mem$model <- factor(sr_future_mem$model, labels=c("GAM", "GBM")) 
#colnames(sr_future_mem)[6] <- c("MEM")

# Anova without replicate works 
# when converting x and y to factors
#dat <- sr_future_ssdm %>% inner_join(sr_future_mem) %>%
#  tidyr::gather(type, sr, -c(taxa,x,y,model,gcm)) %>% 
#  tidyr::separate(gcm, c("gcm", "scenario"), sep="_", extra="drop") %>%
#  mutate_if(is.character, as.factor) %>% arrange(x,y,taxa,scenario) %>% 
#  group_by(x, y, scenario, taxa) %>% mutate(count=n()) %>%
#  filter(count == 16)
#aov_out <- dat %>% do(broom::tidy(aov(sr ~ model + type + gcm + model*type + 
#                                      model*gcm + type*gcm + model*type*gcm, data=.)))
#readr::write_csv(aov_out, "data/aov_out_2080.csv.xz")

#Proportion of the total sum of squares
aov_out <- read.csv("data/aov_out_2080.csv.xz") %>% 
  filter(term != "Residuals") %>% 
  select(x,y,taxa, scenario, term, sumsq) %>% 
  group_by(x,y,taxa, scenario) %>% 
  mutate(sum=sum(sumsq))

aov_plot <- aov_out %>% group_by(x,y,taxa, scenario, term) %>%
  summarise(SS = sumsq/sum*100) %>%
  subset(scenario == "rcp26") %>%
  subset(term %in% c("type", "model", "gcm"))
aov_plot$term <- factor(aov_plot$term, levels=c("model", "type", "gcm") , 
                       labels=c("Model algorithm", "Model type", 
                                "GCM"))
colnames(aov_plot)[3] <- "taxa"

p1 <- aov_plot %>% filter(taxa == "Amphibians", term == "Model algorithm") %>%
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=SS)) + 
  labs(tag="(a)", y="Model algorithm", x="", title="Amphibians") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(limits=c(0, 100), colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(legend.position="none", plot.tag.position = c(0.15, 0.9),
                      axis.title=element_text(size=10, face="bold", angle=90)) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p2 <- aov_plot %>% filter(taxa == "Birds", term == "Model algorithm") %>%
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=SS)) + 
  labs(tag="(b)", title="Birds") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(limits=c(0, 100), colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(legend.position = "none") + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p3 <- aov_plot %>% filter(taxa == "Mammals", term == "Model algorithm") %>%
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=SS)) + 
  labs(tag="(c)", title="Mammals") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(limits=c(0, 100), colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(legend.position = "none") + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

p4 <- aov_plot %>% filter(taxa == "Amphibians", term == "Model type") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=SS)) + 
  labs(tag="(d)", x="", y="Model type") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(limits=c(0, 100), colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(legend.position="none", plot.tag.position = c(0.15, 0.9),
                      axis.title=element_text(size=10, face="bold", angle=90)) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p5 <- aov_plot %>% filter(taxa == "Birds", term == "Model type") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=SS)) + labs(tag="(e)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(limits=c(0, 100), colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(legend.position = "none") + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p6 <- aov_plot %>% filter(taxa == "Mammals", term == "Model type") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=SS)) + labs(tag="(f)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(name="%", limits=c(0, 100), colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(legend.key.width=unit(0.3, "cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))

p7 <- aov_plot %>% filter(taxa == "Amphibians", term == "GCM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=SS)) + 
  labs(tag="(g)", x="", y="GCM") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(limits=c(0, 100), colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(legend.position="none", plot.tag.position = c(0.15, 0.9),
                      axis.title=element_text(size=10, face="bold", angle=90)) + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p8 <- aov_plot %>% filter(taxa == "Birds", term == "GCM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=SS)) + labs(tag="(h)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(limits=c(0, 100), colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(legend.position="none") + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p9 <- aov_plot %>% filter(taxa == "Mammals", term == "GCM") %>% 
  ggplot() + geom_hline(yintercept=0, colour="grey40", 
                        linetype="dashed", size=0.25) + 
  geom_tile(aes(x=x, y=y, fill=SS)) + labs(tag="(i)") + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  scale_fill_gradientn(limits=c(0, 100), colours=bluered, 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(legend.position="none") + 
  coord_map(projection="mollweide", xlim=c(-160,160), ylim=c(-55,85))
p <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(ncol=3)
ggsave("figures/aov_rcp26-1.png", p, width=9, height=5, dpi=600)
ggsave("figures/Figure5.pdf", p, width=9, height=5)

aov_out <- read.csv("data/aov_out_2080.csv.xz") %>% 
  filter(term != "Residuals") %>% 
  select(x,y,taxa, scenario, term, sumsq) %>% 
  group_by(x,y,taxa, scenario) %>% 
  mutate(sum=sum(sumsq))

aov_plot <- aov_out %>% group_by(x,y,taxa, scenario, term) %>%
  summarise(SS = sumsq/sum*100) %>% 
  subset(scenario == "rcp26") %>%
  subset(term %in% c("model:type", "model:gcm", "type:gcm", "model:type:gcm"))
aov_plot$term <- factor(aov_plot$term, levels=c("model:type", "model:gcm", "type:gcm", "model:type:gcm"), 
                       labels=c("Algorithm * Type", "Algorithm * GCM", "Type * GCM", 
                                "Algorithm * Type * GCM"))
colnames(aov_plot)[3] <- "taxa"

p <- ggplot() + geom_hline(yintercept=0, colour="grey40", 
                      linetype="dashed", size=0.25) + 
  geom_tile(data=aov_plot, aes(x=x, y=y, fill=SS)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  facet_grid(term ~ taxa, switch="y") + 
  scale_fill_gradientn(name="%", limits=c(0, 100),
                       colours=bluered, na.value="transparent", 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(strip.placement="outside", legend.key.height=unit(1, "cm"),
                      legend.key.width=unit(0.3, "cm")) + 
  coord_map(projection="mollweide", xlim=c(-160,160), 
            ylim=c(-55,85))
ggsave("figures/aov_rcp26_2-1.png", p, width=10, height=6, dpi=600)

aov_out <- read.csv("data/aov_out_2080.csv.xz") %>% 
  filter(term != "Residuals") %>% 
  select(x,y,taxa, scenario, term, sumsq) %>% 
  group_by(x,y,taxa, scenario) %>% 
  mutate(sum=sum(sumsq))

aov_plot <- aov_out %>% group_by(x,y,taxa, scenario, term) %>%
  summarise(SS = sumsq/sum*100) %>%
  subset(scenario == "rcp60") %>%
  subset(term %in% c("type", "model", "gcm"))
aov_plot$term <- factor(aov_plot$term, levels=c("model", "type", "gcm") , 
                       labels=c("Model algorithm", "Model type", 
                                "GCM"))
colnames(aov_plot)[3] <- "taxa"

p <- ggplot() + geom_hline(yintercept=0, colour="grey40", 
                      linetype="dashed", size=0.25) + 
  geom_tile(data=aov_plot, aes(x=x, y=y, fill=SS)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  facet_grid(term ~ taxa, switch="y") + 
  scale_fill_gradientn(name="%", limits=c(0, 100),
                       colours=bluered, na.value="transparent", 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(strip.placement="outside", legend.key.width=unit(0.3, "cm"),
                      legend.key.height=unit(1, "cm"),
                      plot.title = element_text(size=10, face="bold", hjust=0.5, 
                                                margin = margin(t = 0, r = 0, b = 10, l = 0))) + 
  coord_map(projection="mollweide", xlim=c(-160,160), 
            ylim=c(-55,85))
ggsave("figures/aov_rcp60-1.png", p, width=8, height=4, dpi=600)

aov_out <- read.csv("data/aov_out_2080.csv.xz") %>% 
  filter(term != "Residuals") %>% 
  select(x,y,taxa, scenario, term, sumsq) %>% 
  group_by(x,y,taxa, scenario) %>% 
  mutate(sum=sum(sumsq))

aov_plot <- aov_out %>% group_by(x,y,taxa, scenario, term) %>%
  summarise(SS = sumsq/sum*100) %>% 
  subset(scenario == "rcp60") %>%
  subset(term %in% c("model:type", "model:gcm", "type:gcm", "model:type:gcm"))
aov_plot$term <- factor(aov_plot$term, levels=c("model:type", "model:gcm", "type:gcm", "model:type:gcm"), 
                       labels=c("Model * Type", "Algorithm * GCM", "Type * GCM", 
                                "Algorithm * Type * GCM"))
colnames(aov_plot)[3] <- "taxa"

p <- ggplot() + geom_hline(yintercept=0, colour="grey40", 
                      linetype="dashed", size=0.25) + 
  geom_tile(data=aov_plot, aes(x=x, y=y, fill=SS)) + 
  geom_polygon(data=outline, aes(x=long, y=lat, group=group), 
               colour="grey50", fill=NA, size=0.1) + 
  facet_grid(term ~ taxa, switch="y") + 
  scale_fill_gradientn(name="%", limits=c(0, 100),
                       colours=bluered, na.value="transparent", 
                       values=scales::rescale(seq(0,1, length.out=10))) + 
  theme_map() + theme(strip.placement="outside", legend.key.width=unit(0.3, "cm"),
                      legend.key.height=unit(1, "cm"),
                      plot.title = element_text(size=10, face="bold", hjust=0.5, 
                                                margin = margin(t = 0, r = 0, b = 10, l = 0))) + 
  coord_map(projection="mollweide", xlim=c(-160,160), 
            ylim=c(-55,85))
ggsave("figures/aov_rcp60_2-1.png", p, width=10, height=6, dpi=600)

####################

### Figure S21+22

# Load species coverage data
#sr_observed <- read.csv("data/sr_observed_ssdm.csv.xz")
#colnames(sr_observed) <- c("x", "y", "taxa", "sum")
#sr_observed$taxa <- factor(sr_observed$taxa, 
#                           labels=c("Amphibians", "Birds", "Mammals"))
#sr_full <- read.csv("data/sr_observed_all.csv.xz")
#colnames(sr_full) <- c("x", "y", "taxa", "total")
#sr_full$taxa <- factor(sr_full$taxa, 
#                       labels=c("Amphibians", "Birds", "Mammals"))
#sr_prop_obs <- left_join(sr_observed, sr_full) %>%
#  mutate(prop=sum/total*100, 
#         prop = cut(prop, c(0,85,90,95,100)))

# Load model data
#sr_future_ssdm <- read.csv("data/sr_predicted_ssdm_dispersal1_2080.csv.xz") %>% 
#  tidyr::gather(gcm, sr, -c(taxa,x,y,model))
#sr_future_ssdm$taxa <- factor(sr_future_ssdm$taxa, labels=c("Amphibians", "Birds", "Mammals"))
#sr_future_ssdm$model <- factor(sr_future_ssdm$model, labels=c("GAM", "GBM")) 
#colnames(sr_future_ssdm)[6] <- "S-SDM"

#sr_future_mem <- rbind(
#  read.csv("data/sr_predicted_mem_sub_GAM_poisson_eco_2080.csv.xz"),
#  read.csv("data/sr_predicted_mem_sub_GBM_poisson_eco_2080.csv.xz")) %>%
#  tidyr::gather(gcm, sr, -c(taxa,x,y,model))
#sr_future_mem$taxa <- factor(sr_future_mem$taxa, labels=c("Amphibians", "Birds", "Mammals"))
#sr_future_mem$model <- factor(sr_future_mem$model, labels=c("GAM", "GBM")) 
#colnames(sr_future_mem)[6] <- c("MEM")

# Join data
#data <- sr_future_ssdm %>% inner_join(sr_future_mem) %>% 
#  group_by(x,y,model,gcm,taxa) %>% 
#  summarise(MEM=sum(MEM), `S-SDM`=sum(`S-SDM`)) %>%
#  tidyr::gather(type, sr, -c(x,y,model,gcm,taxa))
#rm(sr_future_ssdm, sr_future_mem)  

# Add species coverage data
#data %<>% left_join(sr_prop_obs)

# Turn data into hclust format
#data %<>% select(-c(sum,total)) %>%
#  tidyr::separate(gcm, c("gcm", "scenario"), sep="_", extra="drop") %>% 
#  tidyr::unite(tmg, type, model, gcm) %>% 
#  group_by(scenario, taxa, prop) %>% tidyr::unite(xy, x,y) %>%
#  tidyr::spread(xy,sr)

#df <- expand.grid(scenario=c("rcp26", "rcp60"),
#                        taxa=c("Amphibians", "Birds", "Mammals"),
#                        prop=c("(0,85]", "(85,90]", "(90,95]", "(95,100]"))
#hc <- data %>% filter(scenario == df$scenario[1], taxa == df$taxa[1], prop == df$prop[1]) %>%
#  tibble::column_to_rownames("tmg") %>% dist %>% hclust
#dendro_df <- lapply(1:nrow(df), function(x){
#  hc <- data %>% filter(scenario == df$scenario[x], 
#                        taxa == df$taxa[x], prop == df$prop[x]) %>%
#    tibble::column_to_rownames("tmg") %>% 
#    dist %>% hclust
#  dhc <- as.dendrogram(hc)
#  ddata <- ggdendro::dendro_data(dhc, type="rectangle")
#  dendro_df <- ggdendro::segment(ddata)
#  dendro_df$taxa <- df$taxa[x]
#  dendro_df$scenario <- df$scenario[x]
#  dendro_df$prop <- df$prop[x]
#  dendro_df$max <- max(hc$height)
#  return(dendro_df)
#})
#dendro_df <- bind_rows(dendro_df)
#saveRDS(dendro_df, "data/dendro_speccov.RData", compress="xz")

dendro_df <- readRDS("data/dendro_speccov.RData")
hc_labels <- c("MEM_GAM_GFDL.ESM2M", "MEM_GAM_HadGEM2.ES", 
               "MEM_GAM_IPSL.CM5A.LR", "MEM_GAM_MIROC5", 
               "MEM_GBM_GFDL.ESM2M", "MEM_GBM_HadGEM2.ES",
               "MEM_GBM_IPSL.CM5A.LR", "MEM_GBM_MIROC5",
               "S-SDM_GAM_GFDL.ESM2M", "S-SDM_GAM_HadGEM2.ES", 
               "S-SDM_GAM_IPSL.CM5A.LR", "S-SDM_GAM_MIROC5", 
               "S-SDM_GBM_GFDL.ESM2M", "S-SDM_GBM_HadGEM2.ES",
               "S-SDM_GBM_IPSL.CM5A.LR","S-SDM_GBM_MIROC5")
dendro_df %>% filter(scenario =="rcp26") %>% 
  mutate(prop = factor(prop, labels=c("0 - 85 %", "85 - 90 %", 
                                      "90 - 95 %", "95 - 100 %"))) %>%
  ggplot() + geom_segment(aes(x = x, y = y/max, xend = xend, yend = yend/max)) + 
  theme_classic() + facet_grid(taxa~prop) + coord_flip() + 
  scale_x_continuous(name="", breaks=c(1:16), labels=hc_labels) + 
  scale_y_continuous(name="Similarity", 
                     labels=c(100,75,50,25,0),
                     limits=c(0,1), expand=c(0,0)) + 
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(face="bold"),
        panel.spacing.x = unit(1, "lines"),
        strip.background = element_blank(), 
        strip.text=element_text(size=12, face="bold"))
ggsave("figures/dendro_rcp26_speccov-1.png", width=9, height=8, dpi=600)

dendro_df %>% filter(scenario =="rcp60") %>% 
  mutate(prop = factor(prop, labels=c("0 - 85 %", "85 - 90 %", 
                                      "90 - 95 %", "95 - 100 %"))) %>%
  ggplot() + geom_segment(aes(x = x, y = y/max, xend = xend, yend = yend/max)) + 
  theme_classic() + facet_grid(taxa~prop) + coord_flip() +  
  scale_x_continuous(name="", breaks=c(1:16), labels=hc_labels) + 
  scale_y_continuous(name="Similarity", 
                     labels=c(100,75,50,25,0),
                     limits=c(0,1), expand=c(0,0)) + 
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(face="bold"),
        panel.spacing.x = unit(1, "lines"),
        strip.background = element_blank(), 
        strip.text=element_text(size=12, face="bold"))
ggsave("figures/dendro_rcp60_speccov-1.png", width=9, height=8, dpi=600)

####################

### Table 1

#Proportion of the total sum of squares
aov_out <- read.csv("data/aov_out_2080.csv.xz") %>% 
  filter(term != "Residuals") %>% 
  dplyr::select(x,y,taxa, scenario, term, sumsq) %>% 
  group_by(x,y,taxa, scenario) %>% 
  mutate(sum=sum(sumsq))
aov_rcp26 <- aov_out %>% group_by(taxa, scenario, term) %>%
  summarise(SS = round(median(sumsq/sum*100),2)) %>% 
  filter(scenario=="rcp26") %>%
  ungroup %>% select(term, taxa, SS) %>% tidyr::spread(taxa, SS)
aov_rcp60 <- aov_out %>% group_by(taxa, scenario, term) %>%
  summarise(SS = round(median(sumsq/sum*100),2)) %>% 
  filter(scenario=="rcp60") %>%
  ungroup() %>% select(term, taxa, SS) %>% tidyr::spread(taxa, SS)
aov_sum2 <- left_join(aov_rcp26, aov_rcp60, by="term")
aov_sum2$term <- factor(aov_sum2$term, 
                        levels=c("model", "type", "gcm", "model:type",
                                 "model:gcm", "type:gcm", "model:type:gcm"),
                        labels=c("Model algorithm", "Model type", "GCM", 
                                 "Algorithm * Type", 
                                 "Algorithm * GCM", 
                                 "Type * GCM", 
                                 "Algorithm * Type * GCM"))
aov_sum2 <- aov_sum2[order(aov_sum2$term),]
aov_sum2$term <- as.character(aov_sum2$term)

aov_sum2 %>% filter(term != "Model type") %>% select(-term) %>% 
  summarise_all(sum)

aov_sum2 <- rbind(c("Source", "Amphibians", "Birds", 
                    "Mammals", "Amphibians", 
                    "Birds", "Mammals"),
                  aov_sum2)
colnames(aov_sum2) <- c("", "", "RCP2.6", "", "", "RCP6.0", "")
knitr::kable(aov_sum2)


####################

### Supporting Table S1

# Merge predicted and observed richness
sr_observed <- read.csv("data/sr_observed_ssdm.csv.xz")
sr_ssdm <- read.csv("data/sr_predicted_ssdm_1995_dispersal1.csv.xz")
sr_mem <- rbind(read.csv("data/sr_predicted_mem_sub_GAM_poisson_eco_1995.csv.xz"),
                read.csv("data/sr_predicted_mem_sub_GBM_poisson_eco_1995.csv.xz")) %>%
  dplyr::select(x,y,mean,model,taxa)
colnames(sr_mem) <- c("x", "y", "mn", "model", "taxa")
sr_mem$model <- factor(sr_mem$model, labels=c("GAM", "GBM"))
sr_mem_ssdm <- inner_join(sr_ssdm, sr_mem)
colnames(sr_ssdm)[3] <- "mn"
sr_ssdm$type <- "S-SDM"
sr_mem$type <- "MEM"
sr_obs_pred <- rbind(inner_join(sr_observed, sr_ssdm), 
                      inner_join(sr_observed, sr_mem)) %>% 
  data.frame()

## Create ensemble mean
ensemble <- sr_obs_pred %>% group_by(x,y,type,taxa) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T),
                   sum = mean(sum, na.rm=T)) %>% data.frame()
ensemble$model <- "Ensemble"
sr_obs_pred <- rbind(sr_obs_pred, ensemble); rm(ensemble)
sr_obs_pred$model <- factor(sr_obs_pred$model, 
                             levels=c("GAM", "GBM", "Ensemble"))
sr_obs_pred$taxa <- factor(sr_obs_pred$taxa, 
                            labels=c("Amphibians", "Birds", "Mammals"))

lm_ssdm <- sr_obs_pred %>% filter(type == "S-SDM") %>%
  group_by(taxa, model) %>% 
  do(mod = lm(mn ~ sum, data = .)) %>%
  mutate(Intercept = summary(mod)$coeff[1],
         Slope = summary(mod)$coeff[2],
         R2 = summary(mod)$r.squared) %>%
  dplyr::select(-mod)

## Calculate AUC
AUC <- lapply(c("Amphibian", "Bird", "Mammal"), function(taxa){
  #Read data
  AUC_data <- lapply(c("GAM", "GBM", "MaxEnt", "RF"), function(model_type){
    readr::read_csv(paste0("data/AUCvalues_All_", 
                           model_type, "_", taxa, ".csv.xz"))})
  AUC_data <- do.call(rbind, AUC_data)
  
  #Aggregate the different AUC values from the 10 iterations per species
  #and filter by AUC > 0.7
  AUC_sum <- AUC_data %>% group_by(Species, taxa, model_type) %>% 
    summarise(mn = mean(AUC, na.rm=T)) %>% filter(mn >= 0.7) %>% ungroup() %>% 
    group_by(Species, taxa) %>% mutate(n = n()) %>% 
    filter(n == 4)
  AUC_sum$taxa <- taxa
  return(AUC_sum)
})
AUC <- dplyr::bind_rows(AUC)

ensemble <- AUC %>% filter(model_type %in% c("GAM", "GBM")) %>%
  droplevels %>% group_by(taxa) %>% 
  dplyr::summarise(mnAUC = mean(mn, na.rm=T),
                   std = sd(mn, na.rm=T)) %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  tidyr::unite(AUC, mnAUC, std, sep=" ± ")
ensemble$taxa <- factor(ensemble$taxa, labels=c("Amphibians", "Birds", "Mammals"))
ensemble$model_type <- "Ensemble"

AUC <- AUC %>% filter(model_type %in% c("GAM","GBM")) %>%
  droplevels %>% group_by(taxa, model_type) %>% 
  summarise(mnAUC=mean(mn,na.rm=T),
            std=sd(mn, na.rm=T)) %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  tidyr::unite(AUC, mnAUC, std, sep=" ± ")
AUC$taxa <- factor(AUC$taxa, labels=c("Amphibians", "Birds", "Mammals"))
AUC <- bind_rows(AUC, ensemble)
lm_ssdm <- left_join(lm_ssdm, AUC, 
                     by=c("taxa", "model"="model_type"))

# Calculate RMSE
rmse_mem <- sr_obs_pred %>% filter(type == "MEM") %>% 
  group_by(taxa, model) %>% summarise(RMSE=ModelMetrics::rmse(sum, mn))

lm_mem <- sr_obs_pred %>% filter(type == "MEM") %>%
  group_by(taxa, model) %>% 
  do(mod = lm(mn ~ sum, data = .)) %>%
  mutate(Intercept = summary(mod)$coeff[1],
         Slope = summary(mod)$coeff[2],
         R2 = summary(mod)$r.squared) %>%
  dplyr::select(-mod)
lm_mem <- left_join(lm_mem, rmse_mem)

ensemble <- sr_mem_ssdm %>% group_by(x,y,taxa) %>% 
  dplyr::summarise(mn = mean(mn, na.rm=T),
                   EWEMBI_1995 = mean(EWEMBI_1995, na.rm=T)) %>% data.frame()
ensemble$model <- "Ensemble"
ensemble$taxa <- factor(ensemble$taxa, 
                        labels=c("Amphibians", "Birds", "Mammals"))
sr_cor <- rbind(sr_mem_ssdm, ensemble) %>% group_by(taxa, model) %>% 
  summarise(cor=cor(EWEMBI_1995, mn))

df <- left_join(lm_mem, lm_ssdm, by=c("taxa", "model"))
df$model <- factor(df$model, levels=c("GAM", "GBM", "Ensemble"))
df <- df %>% arrange(model)
df$cor <- sr_cor$cor
colnames(df) <- c("Taxa", "Algorithm", "Interc. MEM", "Slope MEM", 
                  "R2 MEM", "RMSE", "Interc. S-SDM", "Slope S-SDM", 
                  "R2 S-SDM", "AUC", "Cor.")
knitr::kable(df, digits=2)

####################

### Supporting Table S2+3

# Load species coverage data
#sr_observed <- read.csv("data/sr_observed_ssdm.csv.xz")
#colnames(sr_observed) <- c("x", "y", "taxa", "sum")
#sr_observed$taxa <- factor(sr_observed$taxa, 
#                           labels=c("Amphibians", "Birds", "Mammals"))
#sr_full <- read.csv("data/sr_observed_all.csv.xz")
#colnames(sr_full) <- c("x", "y", "taxa", "total")
#sr_full$taxa <- factor(sr_full$taxa, 
#                       labels=c("Amphibians", "Birds", "Mammals"))
#sr_prop_obs <- left_join(sr_observed, sr_full) %>%
#  mutate(prop=sum/total*100, 
#         prop = cut(prop, c(0,85,90,95,100))) %>% 
#  select(-c(sum, total))

# Load model data
#sr_future_ssdm <- read.csv("data/sr_predicted_ssdm_dispersal1_2080.csv.xz") %>% tidyr::gather(gcm, sr, -c(taxa,x,y,model))
#sr_future_ssdm$taxa <- factor(sr_future_ssdm$taxa, labels=c("Amphibians", "Birds", "Mammals"))
#sr_future_ssdm$model <- factor(sr_future_ssdm$model, labels=c("GAM", "GBM"))
#colnames(sr_future_ssdm)[6] <- "S-SDM"

#sr_future_mem <- rbind(
#  read.csv("data/sr_predicted_mem_sub_GAM_poisson_eco_2080.csv.xz"),
#  read.csv("data/sr_predicted_mem_sub_GBM_poisson_eco_2080.csv.xz")) %>%
#  tidyr::gather(gcm, sr, -c(taxa,x,y,model))
#sr_future_mem$taxa <- factor(sr_future_mem$taxa, labels=c("Amphibians", "Birds", "Mammals"))
#sr_future_mem$model <- factor(sr_future_mem$model, labels=c("GAM", "GBM")) 
#colnames(sr_future_mem)[6] <- c("MEM")

# Anova without replicate works 
# when converting x and y to factors
#dat <- sr_future_ssdm %>% inner_join(sr_future_mem) %>%
#  tidyr::gather(type, sr, -c(taxa,x,y,model,gcm)) %>% 
#  tidyr::separate(gcm, c("gcm", "scenario"), sep="_", extra="drop") %>%
#  mutate_if(is.character, as.factor) %>% arrange(x,y,taxa,scenario) %>% 
#  left_join(sr_prop_obs) %>% tidyr::drop_na(prop) %>%
#  group_by(x, y, scenario, taxa, prop) %>% 
#  mutate(count=n()) %>% filter(count == 16)

# Run anova
#aov_out <- dat %>% do(broom::tidy(aov(sr ~ model + type + gcm + model*type + 
#                                        model*gcm + type*gcm + model*type*gcm, data=.)))
#  readr::write_csv(aov_out, "data/aov_out_speccov_2080.csv.xz")

#Proportion of the total sum of squares
aov_out <- read.csv("data/aov_out_speccov_2080.csv.xz") %>% 
  filter(term != "Residuals") %>% 
  select(x,y,taxa,prop,scenario, term, sumsq) %>% 
  group_by(x,y,taxa,prop, scenario) %>% 
  mutate(sum=sum(sumsq))

aov_sum <- aov_out %>% group_by(taxa, prop, scenario, term) %>%
  summarise(SS = round(median(sumsq/sum*100),2)) %>% ungroup %>%
  mutate(term = factor(term, levels=c("model", "type", "gcm", "model:type",
                                 "model:gcm", "type:gcm", "model:type:gcm"),
                       labels=c("Model algorithm", "Model type", "GCM", 
                                 "Algorithm * Type", "Algorithm * GCM", "Type * GCM", 
                                 "Algorithm * Type * GCM")),
         prop = factor(prop, labels=c("0 - 85 %", "85 - 90 %", 
                                      "90 - 95 %", "95 - 100 %"))) %>% 
  tidyr::spread(prop, SS) %>% arrange(taxa, term) %>% mutate_if(is.factor, as.character)

aov_rcp26 <- aov_sum %>% filter(scenario=="rcp26") %>% select(-scenario)
aov_rcp60 <- aov_sum %>% filter(scenario=="rcp60") %>% select(-scenario)
aov_sum2 <- left_join(aov_rcp26, aov_rcp60, by=c("taxa", "term"))
aov_sum2 <- rbind(c("Taxa", "Source", "0 - 85 %", "85 - 90 %", "90 - 95 %", 
                    "95 - 100 %", "0 - 85 %", "85 - 90 %", "90 - 95 %", 
                    "95 - 100 %"), aov_sum2)
colnames(aov_sum2) <- c("", "", "RCP2.6", "", "", "", "RCP6.0", "", "", "")
knitr::kable(aov_sum2, digits=2)