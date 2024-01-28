before<-ggplot(Top30 %>%
         mutate(NMFS.Namev2=recode(NMFS.Namev2, "SKATE"='SKATES, RAJIDAE (FAMILY) **')),
       aes(fct_reorder(NMFS.Name, MgGp), y=IYear, colour=Management.Group)) +
  geom_point() +
  # geom_hline(yintercept=1990, linetype="dashed", color = "black", linewidth=1) +
  ylab("Year") +
  scale_color_manual(values = c("Atlantic Herring (NE)"="#AA4499",
                               "Monkfish (NE/MA)"="#CC6677",
                               "Northeast Multispecies (NE)"="#88CCEE",
                               "Northeast Skate Complex (NE)"="#DDCC77",
                               "Other"="#332288",
                               "Atlantic Sea Scallop (NE)"="#44AA99",
                               "Spiny Dogfish (NE/MA)"="#999999")) +
  theme(axis.text.x = element_text(angle = 90, hjust=1, size=13.5),
        legend.position = "right",
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        axis.title.x=element_blank()) +
  labs(colour="Fisheries\nManagement\nPlan") +
  guides(color = guide_legend(override.aes = list(size = 5)))

# before

after<-ggplot(P_Selection %>%
         mutate(NMFS.Namev2=recode(NMFS.Namev2, "SKATE"='SKATES, RAJIDAE (FAMILY) **')),
       aes(fct_reorder(NMFS.Namev2, MgGp), y=IYear, colour=Management.Group)) +
  geom_point() +
  # geom_hline(yintercept=1990, linetype="dashed", color = "black", linewidth=1) +
  ylab("Year") + xlab("Species/Spp. Aggregation") +
  scale_color_manual(values = c("Atlantic Herring (NE)"="#AA4499",
                                "Monkfish (NE/MA)"="#CC6677",
                                "Northeast Multispecies (NE)"="#88CCEE",
                                "Northeast Skate Complex (NE)"="#DDCC77",
                                "Other"="#332288",
                                "Atlantic Sea Scallop (NE)"="#44AA99",
                                "Spiny Dogfish (NE/MA)"="#999999")) +
  theme(axis.text.x = element_text(angle = 90, hjust=1, size=13.5),
        legend.position = "right",
        legend.title = element_text(size=15),
        legend.text = element_text(size=15)) +
  labs(colour="Fisheries\nManagement\nPlan") +
  guides(color = guide_legend(override.aes = list(size = 5)))


library(ggpubr)
ggarrange(before, after, common.legend=TRUE, legend="right", nrow=2,
          labels = c("a)", "b)"))
# ggsave(path = "Manuscript Plots", filename = "timeseries plots_v2.png", width = 20, height = 25, dpi = 800)
