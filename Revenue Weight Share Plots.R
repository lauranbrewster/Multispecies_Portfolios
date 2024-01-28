## Implicit and Optimal revenue share weights by species ----
ggplot(data=ImpResults) + 
  geom_col(aes(x=Iteration, y=ImpWeightShare)) +
  facet_wrap(~NMFS.Namev2) +
  labs(y ="Implicit Weight Shares", x = "Year") +
  geom_hline(yintercept = 1, color="red", linetype="dashed") +
  scale_x_continuous(limits = c(2010, 2021))
# ggsave(path = "Manuscript Plots", filename = "Implicit Weights plot.png", width = 15, height = 10, dpi = 600)

ggplot(data=Taxon_Opt_Weight) + 
  geom_col(aes(x=Iteration, y=RevWeightShare), fill="darkblue") + ##Here the optimal weights are calculated as 0.5*maxweight
  facet_wrap(~NMFS.Namev2) +
  labs(y ="Optimal Revenue Weight Share", x = "Year") +
  geom_hline(yintercept = 1, color="red", linetype="dashed") 
# ggsave(path = "Manuscript Plots", filename = "Optimal Weights plot.png", width = 15, height = 10, dpi = 600)

ImpResults2 <- ImpResults %>%
  dplyr::rename(WeightShare=ImpWeightShare) %>%
  mutate(WeightType=rep("Implicit Revenue Weight Ratio")) %>%
  select(Iteration, Taxonkey, WeightShare, WeightType, NMFS.Namev2)
  
Taxon_Opt_Weight2 <- Taxon_Opt_Weight %>%
  dplyr::rename(WeightShare=RevWeightShare)%>%
  mutate(WeightType=rep("Optimal Revenue Weight Share")) %>%
  select(Iteration, Taxonkey, WeightShare, WeightType, NMFS.Namev2)
         

test<-rbind(ImpResults2, Taxon_Opt_Weight2) %>%
  filter(Iteration > 2010)

ggplot(data=test) + 
  geom_bar(aes(fill=WeightType, x=Iteration, y=WeightShare), position="dodge", stat="identity") +
  facet_wrap(~NMFS.Namev2) +
  labs(y ="Revenue Weight Share", x = "Year") +
  geom_hline(yintercept = 1, color="red", linetype="dashed") +
  scale_fill_manual(values=c("grey", "darkblue")) +
  # scale_x_continuous(limits = c(2011,2021)) +
  scale_x_continuous(breaks = c(2012,2014,2016,2018,2020)) +
  theme(legend.title=element_blank(),
        legend.text=element_text(size=16),
        axis.text.x = element_text(angle = 60, vjust = 0.7, size=10))
# ggsave(path = "Manuscript Plots", filename = "Implicit and Optimal Weights plot 2011 on.png", width = 15, height = 10, dpi = 600)

#Is this right? Want to show portfolio composition of revenue....this is showing the proportion of the catch that should be caught for optimal risk out of what could be caught as determined by setting the biological constraint.
# No, you want the optimal revenue or optimal landings as a proportion of the whole target revenue or associated landings.
ggplot(test %>%
         filter(WeightShareType=="Optimal Revenue Weight Share"), aes(fill=NMFS.Namev2, y=WeightShare, x=Iteration)) + 
  # geom_bar(position="stack", stat="identity") +
  geom_bar(position="fill", stat="identity") +
  labs(y ="Revenue Weight Share", x = "Year") +
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
                               "SKATE"="#4CFA1A",
                               "URCHINS, SEA, STRONGYLOCENTROTUS (GENUS) **"="#84AD0E",
                               "SQUIDS"="#66A61E",
                               "SPINY AGG"="#16C8D2",
                               "PORGIES_SCUP"="yellow")) +
  scale_x_continuous(breaks = c(2011:2021)) +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 60, vjust = 0.7, size=16))

###############################









