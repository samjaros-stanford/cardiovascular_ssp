library(here)
library(ggpubr)
library(stringr)
library(tidyverse)
library(showtext)

geography = "tract"
#colors = c("#56B4E9", "#009E73", "#D55E00", "#0072B2", "#E69F00")
colors = c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c")
line_color = "black"
font = "sans"
line_width = 0.5
# Change this tibble to change which ICE's are displayed
display_ICE = tribble(~ICE,        ~ICE_display_expression,
                      "ICEedu",     "Education",
                      "ICEhome",    "Housing",
                      "ICEincome",  "Income",
                      "ICEraceeth", "Race/Ethnicity",
                      "ICEincwnh",  "Race/Ethnicity + Income")
# Change this tibble to change how the y axis is displayed
display_quint = tribble(~quint, ~display_quint,
                        "5",    "5(ref)",
                        "4",    "4",
                        "3",    "3",
                        "2",    "2",
                        "1",    "1")

quint_data = read_csv(here::here("data/results/20240222_all.csv"), show_col_types=F) %>%
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
                      levels=display_ICE$ICE),
         ICE_display = factor(ICE, 
                              labels=display_ICE$ICE_display_expression, 
                              levels=display_ICE$ICE)) %>%
  drop_na(ICE, quant, est)

###########
# High BP #
###########

hbp_quint_forest = ggplot(filter(quint_data, outcome=="hbp1", geo==geography), aes(y=quint, color=quint)) +
  geom_vline(xintercept=1, color=line_color, linewidth=line_width) +
  geom_point(aes(x=est), size=line_width*3) +
  geom_errorbar(aes(xmin=l_ci, xmax=u_ci), linewidth=line_width) +
  scale_color_manual(values=colors[1:5], guide="none") +
  scale_y_discrete(name=NULL) +
  scale_x_continuous(name=NULL, limits=c(0.5,1.5), breaks=c(0.5,1,1.5), 
                     labels=c("0.5", "1", "1.5")) +
  facet_grid(ICE_display~.) +
  theme_classic() +
  theme(text=element_text(family=font),
        strip.text=element_blank(),
        axis.line.x=element_line(color=line_color),
        axis.line.y=element_blank(),
        axis.ticks=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(size=10, margin=margin(t=0), color=line_color),
        panel.background=element_blank(),
        panel.grid=element_blank(),
        panel.spacing=unit(3,"points"),
        plot.background=element_blank())
hbp_quint_forest

ggsave(filename=here::here(paste0("figures/hbp_",geography,"_quint_forest.png")), hbp_quint_forest, 
       height=5.4, width=1.5, units="in", dpi=600, bg="transparent")

##########
# Sys BP #
##########

sbp_quint_forest = ggplot(filter(quint_data, outcome=="sys_bp", geo==geography), aes(y=quint, color=quint)) +
  geom_vline(xintercept=0, color=line_color, linewidth=line_width) +
  geom_point(aes(x=est), size=line_width*3) +
  geom_errorbar(aes(xmin=l_ci, xmax=u_ci), linewidth=line_width) +
  scale_color_manual(values=colors[1:5], guide="none") +
  scale_y_discrete(name=NULL) +
  scale_x_continuous(name=NULL, limits=c(-1,2.7), breaks=c(-1,0,1,2), 
                     labels=c("-1","0","1","2")) +
  facet_grid(ICE_display~.) +
  theme_classic() +
  theme(text=element_text(family=font),
        strip.text=element_blank(),
        axis.line.x=element_line(color=line_color),
        axis.line.y=element_blank(),
        axis.ticks=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(size=10, margin=margin(t=0), color=line_color),
        panel.background=element_blank(),
        panel.grid=element_blank(),
        panel.spacing=unit(3,"points"),
        plot.background=element_blank())
sbp_quint_forest

ggsave(filename=here::here(paste0("figures/sbp_",geography,"_quint_forest.png")), sbp_quint_forest, 
       height=5.4, width=1.5, units="in", dpi=600, bg="transparent")


full_fig_3 = ggarrange(hbp_quint_forest, sbp_quint_forest, ncol=2, nrow=1, 
                       labels=c("A","B"), font.label=list(family=font), hjust=-1, vjust=2)
ggsave(filename=here::here(paste0("figures/fig3_",geography,"_quint_forest.png")),
       full_fig_3, height=6, width=6, units="in", dpi=600, bg="white")
