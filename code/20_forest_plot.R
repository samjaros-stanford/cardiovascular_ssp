library(here)
library(ggpubr)
library(stringr)
library(tidyverse)
library(showtext)

geography = "tract"
colors = c("#332288", "#117733", "#44AA99", "#88CCEE", "#DDCC77", "#CC6677", "#AA4499", "#882255")
line_color = "gray35"
font = "sans"
#font_add_google(font)
line_width = 1
display_ICE = tribble(~ICE,        ~ICE_display_expression,
                      "ICEincome",  "ICE[inc]",
                      "ICEraceeth", "ICE[race]",
                      "ICEincwb",   "ICE[inc+wb]",
                      "ICEincwnh",  "ICE[inc+wnh]",
                      "ICEhome",    "ICE[home]",
                      "ICEedu",     "ICE[edu]")
display_quint = tribble(~quint, ~display_quint,
                        "5",    "5(ref)",
                        "4",    "4",
                        "3",    "3",
                        "2",    "2",
                        "1",    "1")
# Change this list to change which ICE's are displayed
ice_to_use = c("ICEedu","ICEhome","ICEincome","ICEraceeth","ICEincwnh")

quint_data = read_csv(here::here("data/results/20240123_all.csv"), show_col_types=F) %>%
  filter(endsWith(ICE,"_quint"),
         model=="full") %>%
  # Get quantile numbers
  mutate(quant = str_sub(coef_name,-1),
         ICE = str_split_i(ICE,"_",1)) %>%
  select(outcome, geo, ICE, quant, est, l_ci, u_ci) %>%
  # Add row for reference group (5th/most privileged quantile at OR=1)
  group_by(outcome, geo, ICE) %>%
  group_modify(~add_row(.x,quant="5")) %>%
  ungroup() %>%
  mutate(est = case_when(
    quant=="5" & grepl("hbp", outcome) ~ 1,
    quant=="5"                         ~ 0,
    T                                  ~ est
  )) %>%
  # Factor ICE & quantile
  mutate(quint = factor(quant, labels=display_quint$display_quint, levels=display_quint$quint),
         # CHANGE THE LEVELS HERE TO INCLUDE/EXCLUDE DIFFERENT ICE DOMAINS
         ICE = factor(ICE, 
                      levels=ice_to_use),
         ICE_display = factor(ICE, 
                              labels=c("ICE[edu]","ICE[home]","ICE[inc]","ICE[race/eth]","ICE[inc+wb]","ICE[inc+race/eth]"), 
                              levels=c("ICEedu","ICEhome","ICEincome","ICEraceeth","ICEincwb","ICEincwnh"))) %>%
  drop_na(ICE, quant, est)

###########
# High BP #
###########

hbp_quint_forest = ggplot(filter(quint_data, outcome=="hbp1", geo==geography), aes(y=quint, color=quint)) +
  labs(caption="1=most deprived 5=most privileged") +
  geom_vline(xintercept=1, color=line_color, linewidth=line_width) +
  geom_point(aes(x=est), size=line_width*3) +
  geom_errorbar(aes(xmin=l_ci, xmax=u_ci), linewidth=line_width) +
  scale_color_manual(values=colors[1:5], guide="none") +
  scale_y_discrete("ICE Quintile") +
  scale_x_continuous("OR (95% CI) for High BP") +
  facet_grid(ICE_display~., switch="y", labeller=label_parsed) +
  theme_minimal() +
  theme(plot.caption=element_text(family=font, size=8, hjust=0),
        plot.caption.position="plot",
        strip.placement="outside",
        strip.text.y.left=element_text(family=font, size=12, angle=0),
        axis.title.y=element_text(family=font, size=12),
        axis.title.x=element_text(family=font, size=12, hjust=1),
        axis.text=element_text(family=font, size=10), 
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank())

ggsave(filename=here::here(paste0("figures/hbp_",geography,"_quint_forest.png")), hbp_quint_forest, 
       height=6, width=3, units="in", dpi=600)

##########
# Sys BP #
##########

sbp_quint_forest = ggplot(filter(quint_data, outcome=="sys_bp", geo==geography), aes(y=quint, color=quint)) +
  labs(caption=" ") + #1=most deprived 5=most privileged
  geom_vline(xintercept=0, color=line_color, linewidth=line_width) +
  geom_point(aes(x=est), size=line_width*3) +
  geom_errorbar(aes(xmin=l_ci, xmax=u_ci), linewidth=line_width) +
  scale_color_manual(values=colors[1:5], guide="none") +
  scale_y_discrete("ICE Quintile") +
  scale_x_continuous(expression(paste(beta," (95% CI) for Systolic BP"))) +
  facet_grid(ICE_display~., switch="y", labeller=label_parsed) +
  theme_minimal() +
  theme(plot.caption=element_text(family=font, size=8, hjust=0),
        plot.caption.position="plot",
        strip.placement="outside",
        strip.text.y.left=element_text(family=font, size=12, angle=0),
        axis.title.y=element_text(family=font, size=12),
        axis.title.x=element_text(family=font, size=12, hjust=1),
        axis.text=element_text(family=font, size=10), 
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank())

ggsave(filename=here::here(paste0("figures/sbp_",geography,"_quint_forest.png")), sbp_quint_forest, 
       height=6, width=3, units="in", dpi=600)


full_fig_3 = ggarrange(hbp_quint_forest, sbp_quint_forest, ncol=2, nrow=1, 
                       labels=c("A","B"), font.label=list(family=font), hjust=-1, vjust=2)
ggsave(filename=here::here(paste0("figures/fig3_",geography,"_quint_forest.png")),
       full_fig_3, height=6, width=6, units="in", dpi=600, bg="white")
