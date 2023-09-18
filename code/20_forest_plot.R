require(here)
require(stringr)
require(tidyverse)
require(showtext)

colors = c("#332288", "#117733", "#44AA99", "#88CCEE", "#DDCC77", "#CC6677", "#AA4499", "#882255")
line_color = quant_colors[8]
font = "Source Sans Pro"
line_width = 1.25
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

quint_data = read_csv(here::here("data/all_mods_20230907.csv"), show_col_types=F) %>%
  filter(endsWith(ICE,"_quant"),
         outcome=="hbp1",
         geo=="tract",
         model=="full") %>%
  # Get quantile numbers
  mutate(quant = str_sub(coef_name,-1),
         ICE = str_split_i(ICE,"_",1)) %>%
  select(ICE, quant, est, l_ci, u_ci) %>%
  # Add row for reference group (5th/most privileged quantile at OR=1)
  group_by(ICE) %>%
  group_modify(~add_row(.x,quant="5",est=1)) %>%
  ungroup() %>%
  # Factor ICE & quantile
  mutate(quint = factor(quant, labels=display_quint$display_quint, levels=display_quint$quint),
         ICE = factor(ICE, 
                      levels=c("ICEincome","ICEraceeth","ICEincwb","ICEincwnh","ICEedu","ICEhome")),
         ICE_display = factor(ICE, 
                              labels=c("ICE[inc]","ICE[race]","ICE[inc+wb]","ICE[inc+wnh]","ICE[edu]","ICE[home]"), 
                              levels=c("ICEincome","ICEraceeth","ICEincwb","ICEincwnh","ICEedu","ICEhome"))) %>%
  drop_na(ICE, quant, est)

# Enable showtext to allow for more fonts
showtext_auto()
showtext_opts(dpi=600)

hbp_quint_forest = ggplot(quint_data, aes(y=quint, color=quint)) +
  labs(caption="1=most deprived 5=most privileged") +
  geom_vline(xintercept=1, color=line_color, linewidth=line_width) +
  geom_point(aes(x=est), size=line_width*3) +
  geom_errorbar(aes(xmin=l_ci, xmax=u_ci), linewidth=line_width) +
  scale_color_manual(values=colors[1:5], guide="none") +
  scale_y_discrete("ICE Quintile") +
  scale_x_continuous("OR (95% CI) for High BP") +
  facet_grid(ICE_display~., switch="y", labeller=label_parsed) +
  theme_minimal() +
  theme(plot.caption=element_text(family=font, size=18, hjust=0),
        plot.caption.position="plot",
        strip.placement="outside",
        strip.text.y.left=element_text(family=font, size=25, angle=0),
        axis.title.y=element_text(family=font, size=30),
        axis.title.x=element_text(family=font, size=28),
        axis.text=element_text(family=font, size=22))

ggsave(filename=here::here("figures/hbp_quint_forest.png"), hbp_quint_forest, 
       height=16.94, width=8.47, units="in", dpi=600)

showtext_auto(F)
