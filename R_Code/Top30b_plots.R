land<-ggplot(Top30b%>%
         mutate(NMFS.Namev2=recode(NMFS.Namev2, "SKATE"='SKATES, RAJIDAE (FAMILY) **')),
       aes(fill=reorder(NMFS.Name, +Catch), y=Catch/10e4, x=IYear)) + 
  geom_bar(position='stack', stat='identity') +
  # scale_fill_manual(values = col_vector) +
  scale_fill_manual(values = c("ALEWIFE"="#7FC97F",
                               "CLAM, QUAHOG, OCEAN"="#BEAED4",
                               "CLAM, SOFT"="#FDC086",
                               "CLAM, SURF, ATLANTIC"="#7570B3",
                               "COD, ATLANTIC"="#386CB0",
                               "FLOUNDER, AMERICAN PLAICE"="#213D4A",
                               "FLOUNDER, WINTER"="#BF5B17",
                               "FLOUNDER, WITCH"="#666666",
                               "FLOUNDER, YELLOWTAIL"="#1B9E77",
                               "GOOSEFISH"="#D95F02",
                               "HADDOCK"="#F5A90D",
                               "HAKE, SILVER"="#521F2F",
                               "HAKE, WHITE"="#CB0189",
                               "HERRING, ATLANTIC"="#FB9A99",
                               "LOBSTER, AMERICAN"="#054BAB",
                               "MACKEREL, ATLANTIC"="#F76334",
                               "MENHADENS **"="#4F3F21",
                               "POLLOCK"="#D27DE8",
                               "REDFISH, ACADIAN"="#F0027F",
                               "SCALLOP, SEA"="#66087B",
                               "URCHINS, SEA, STRONGYLOCENTROTUS (GENUS) **"="#84AD0E",
                               "VERTEBRATES, JAWED"="red",
                               "SKATES, RAJIDAE (FAMILY) **"="blue",
                               "WITHHELD FOR CONFIDENTIALITY"="palegreen",
                               "SQUID, LONGFIN LOLIGO"="orange",
                               "SQUIDS, LOLIGINIDAE **"="black",
                               "SHARK, DOGFISH, SPINY"="#16C8D2",
                               "SHRIMPS, PENAEOID **"="pink",
                               "SQUID, SHORTFIN ILLEX"="purple",                                                                                                                                      
                               "PORGIES **"="yellow")) +
theme(legend.key.size = unit(3, 'mm'),
        legend.text = element_text(size = 13.4),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.7, size=20),) +    
  labs(x="Year", y=expression('Landings (Metric Tons x10'^4*')')) +
  # y=expression('Landings (Metric Tons x10'^12*')')
  guides(fill=guide_legend(ncol=4))
# ggsave(path = "Manuscript Plots", filename = "Top30Landings.png", width = 15, height = 10, dpi = 600, bg='transparent')

rev<-ggplot(Top30b%>%
         mutate(NMFS.Namev2=recode(NMFS.Namev2, "SKATE"='SKATES, RAJIDAE (FAMILY) **')),
       aes(fill=reorder(NMFS.Name, +Catch), y=OrigValue/1.0e8, x=IYear)) + 
  geom_bar(position='stack', stat='identity') +
  # scale_fill_manual(values = col_vector) +
  scale_fill_manual(values = c("ALEWIFE"="#7FC97F",
                               "CLAM, QUAHOG, OCEAN"="#BEAED4",
                               "CLAM, SOFT"="#FDC086",
                               "CLAM, SURF, ATLANTIC"="#7570B3",
                               "COD, ATLANTIC"="#386CB0",
                               "FLOUNDER, AMERICAN PLAICE"="#213D4A",
                               "FLOUNDER, WINTER"="#BF5B17",
                               "FLOUNDER, WITCH"="#666666",
                               "FLOUNDER, YELLOWTAIL"="#1B9E77",
                               "GOOSEFISH"="#D95F02",
                               "HADDOCK"="#F5A90D",
                               "HAKE, SILVER"="#521F2F",
                               "HAKE, WHITE"="#CB0189",
                               "HERRING, ATLANTIC"="#FB9A99",
                               "LOBSTER, AMERICAN"="#054BAB",
                               "MACKEREL, ATLANTIC"="#F76334",
                               "MENHADENS **"="#4F3F21",
                               "POLLOCK"="#D27DE8",
                               "REDFISH, ACADIAN"="#F0027F",
                               "SCALLOP, SEA"="#66087B",
                               "URCHINS, SEA, STRONGYLOCENTROTUS (GENUS) **"="#84AD0E",
                               "VERTEBRATES, JAWED"="red",
                               "SKATES, RAJIDAE (FAMILY) **"="blue",
                               "WITHHELD FOR CONFIDENTIALITY"="palegreen",
                               "SQUID, LONGFIN LOLIGO"="orange",
                               "SQUIDS, LOLIGINIDAE **"="black",
                               "SHARK, DOGFISH, SPINY"="#16C8D2",
                               "SHRIMPS, PENAEOID **"="pink",
                               "SQUID, SHORTFIN ILLEX"="purple",                                                                                                                                      
                               "PORGIES **"="yellow")) +
  theme(legend.key.size = unit(3, 'mm'),
        legend.text = element_text(size = 13.4),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.7, size=20),) +    
  labs(x="Year", y="Revenue (Hundred Million Dollars)") +
  guides(fill=guide_legend(ncol=4))
# ggsave(path = "Manuscript Plots", filename = "Top30Revenue.png", width = 15, height = 10, dpi = 600, bg='transparent')

library(ggpubr)
ggarrange(land, rev, common.legend=TRUE,
          labels = c("a)", "b)"))
ggsave(path = "Manuscript Plots", filename = "Top30col_v2.png", width = 15, height = 10, dpi = 600, bg='transparent')

