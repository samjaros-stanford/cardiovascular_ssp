library(ggpubr)
library(tidyverse)

# Cleaned & merged data import
full_analysis = readRDS(here::here("data/full_analysis.rds"))

ICE_labels = c("ICEedu"      = "Education",
               "ICEhome"     = "Housing",
               "ICEincome"   = "Income",
               "ICEraceeth"  = "Race/\nEthnicity",
               #"ICEincwb"    = expression(ICE[inc+wb]),
               "ICEincwnh"   = "Race/\nEthnicity\n+ Income")
strata_labels = c("county"  = "County",
                  "zcta"    = "ZCTA",
                  "tract"   = "Tract",
                  "CHS"     = "CHS",
                  "REGARDS" = "REGARDS")
#strata_fill = c("#E45904", "#648FFF", "#86E08E")
strata_fill = c("#332288", "#117733", "#44AA99")
strata_color = c("black", "black", "black")
#strata_color = c("#332288", "#117733", "#44AA99")
font = "sans"

ICE_hist_data = full_analysis %>%
  # No language or inc+wb ICEs included
  select(id, study, starts_with("ICE"), -ends_with(c("tert", "quint")), 
         -contains(c("wb", "language"))) %>%
  pivot_longer(cols = c(-id, -study),
               names_to = c("ICE","geo"),
               names_sep = "_",
               values_to = "value",
               values_drop_na = T) %>%
  mutate(ICE = factor(ICE, levels=names(ICE_labels)),
         geo = factor(geo, levels=names(strata_labels)))

# TODO: Look into removing black border around violin plot
make_ICE_dist_plot = function(plot_data, strata_var, legend_pos="top"){
  ggplot(plot_data, aes(x=factor(ICE), y=value, color=.data[[strata_var]], 
                        fill=.data[[strata_var]])) +
    stat_boxplot(geom="errorbar", linewidth=0.4) +
    geom_boxplot(outlier.shape=NA, linewidth=0.4, color="black") +
    #geom_violin(trim=F) +
    scale_color_manual(values=strata_color, labels=strata_labels) +
    scale_fill_manual(values=strata_fill, labels=strata_labels) +
    scale_x_discrete("ICE Domain", breaks=names(ICE_labels), labels=ICE_labels) +
    scale_y_continuous("ICE Value", limits=c(-1,1), breaks=seq(-1,1,0.5)) +
    theme_minimal() +
    theme(text=element_text(family=font),
          axis.text=element_text(size=8.5),
          axis.text.x=element_text(vjust=1, hjust=0.5, 
                                   margin=margin(-10,-10,-10,-10)),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.box.spacing=margin(0,0,-5,0), 
          legend.position=legend_pos,
          legend.title=element_blank())
}
CHS = make_ICE_dist_plot(ICE_hist_data %>% filter(study=="CHS"), "geo")
REGARDS = make_ICE_dist_plot(ICE_hist_data %>% filter(study=="REGARDS"), "geo")
# Figure 2
#   Side-by-side boxplots, CHS vs REGARDS
#   Each boxplot is stratified by geography level
#   X: ICE domain, Y: ICE value
figure_2 = ggarrange(CHS,
                     REGARDS,
                     nrow=1,
                     labels=c("CHS","REGARDS"), label.x=c(0.1,0.01), vjust=.8, 
                     font.label=list(family=font),
                     common.legend=T, legend="top", legend.grob=get_legend(CHS))
png(here::here("figures/ICE_box_all.png"), width=6.5, height=4, units="in", res=600)
figure_2
dev.off()

# Other ICE distribution figures
#   X: ICE domain, Y: ICE value
#   Stratified by study, CHS vs REGARDS
#   Separate plot for each geography level
# for(level in c("county", "zcta", "tract")){
#   ggsave(here::here(paste0("figures/ICE_box_",level,".png")), 
#          plot=make_ICE_dist_plot(filter(ICE_hist_data, geo==level), "study"),
#          width=3, height=4, units="in", dpi=600)
# }

