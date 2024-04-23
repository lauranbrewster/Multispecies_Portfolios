# LOAD PACKAGES ----
require(readxl)
require(tidyverse)
require(priceR) #for standardizing price with inflation
require(RColorBrewer)
require(viridis)
require(eeptools) #decomma function
require(openxlsx)
require(grDevices)
require(extrafont)
require(boot)
require(kernlab)
require(reshape)
require(matrixcalc)
require(corrplot)
require(here)


# Data import ----
# data downloaded from https://www.fisheries.noaa.gov/foss/f?p=215:200:3664061157108::NO:RP::
raw <- read.csv(here("Data_Files/FOSS_landings_NE_2021_Year_Species_State.csv"))
str(raw)

# DATA MANIPULATION ----
## 1. Add Fishery Management Plan vector (including factor: "Other")
## 2. Create vector NMFS.Namesv2 where the skates are all grouped together as SKATE
## 3. Dollars standardized to 2021
## 4. Pounds converted to metric tons

raw<-raw %>%
  mutate(Pounds=decomma(Pounds),
         Metric.Tons=decomma(Metric.Tons),
         Dollars=decomma(Dollars),
         Metric.Tons=Pounds/2204.62,
         Terminal.Yr.Dollars = adjust_for_inflation(Dollars, from_date = Year,
                                                    country = "US", to_date = 2021),
         Management.Group = case_when(grepl("Gadus morhua", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Melanogrammus aeglefinus", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Limanda ferruginea", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Pollachius virens", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Hippoglossoides platessoides", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Glyptocephalus cynoglossus", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Urophycis tenuis", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Scophthalmus aquosus", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Pseudopleuronectes americanus", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Sebastes fasciatus", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Hippoglossus hippoglossus", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Anarhichas lupus", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Zoarces", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Macrozoarces americanus", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("Merluccius bilinearis", Scientific.Name) ~ "Northeast Multispecies (NE)",
                                      grepl("SKATE", NMFS.Name, ignore.case = TRUE) ~"Northeast Skate Complex (NE)",
                                      grepl("Urophycis chuss", Scientific.Name, ignore.case = TRUE) ~"Small-Mesh Multispecies",
                                      grepl("Merluccius albidus", Scientific.Name, ignore.case = TRUE) ~"Small-Mesh Multispecies",
                                      grepl("Placopecten magellanicus", Scientific.Name, ignore.case = TRUE) ~"Atlantic Sea Scallop (NE)",
                                      grepl("Lophius americanus", Scientific.Name, ignore.case = TRUE) ~"Monkfish (NE/MA)",
                                      grepl("Chaceon quinquedens", Scientific.Name, ignore.case = TRUE) ~"Red Crab",
                                      grepl("Clupea harengus", Scientific.Name, ignore.case = TRUE) ~"Atlantic Herring (NE)",
                                      grepl("Squalus acanthias", Scientific.Name, ignore.case = TRUE) ~"Spiny Dogfish (NE/MA)",
                                      grepl("Salmo salar", Scientific.Name, ignore.case = TRUE) ~"Atlantic Salmon")) %>%
  mutate(Management.Group=replace_na(Management.Group, "Other")) %>%
  mutate(NMFS.Namev2=recode(NMFS.Name, "SKATE, ROSETTE"='SKATE',
                            "SKATE, BARNDOOR"='SKATE',
                            "SKATE, CLEARNOSE"='SKATE',
                            "SKATE, THORNY"='SKATE',
                            "SKATE, SMOOTH"='SKATE',
                            "SKATE, WINTER"='SKATE',
                            "SKATE, LITTLE"='SKATE',
                            "SKATES, RAJIDAE (FAMILY) **"="SKATE",
                            "LITTLE/WINTER SKATE MIX **"="SKATE")) %>%
  dplyr::rename(Taxonkey=Tsn) %>%
  mutate_if(is.character,as.factor) #covert all character strings to factors

str(raw)
table(raw$Management.Group)

## 5. Filter to public data only
## 6. Remove seaweeds
## 7. Aggregate dataframe to ignore states (if wanted)

YrSps <- raw %>%
  filter(Confidentiality=="Public") %>%
  filter(!grepl("SEAWEED", NMFS.Name)) %>%
  select(-c("State", "Source")) %>%
  aggregate(cbind(Terminal.Yr.Dollars, Dollars, Metric.Tons, Pounds) ~ ., sum)

# DATA SELECTION ----
## Find top_n species by landings ----
## Aggregate dataframe to ignore year and rank the top species overall by landings
Top_n=30 # set your top n value

Top30_Landings<- YrSps %>%
  group_by(NMFS.Name, Scientific.Name, NMFS.Namev2, Confidentiality, Management.Group, Collection) %>%
  summarise_all(sum) %>%
  as.data.frame(Top30_Landings) %>%
  mutate(CatchRank=rank(-Metric.Tons)) %>%
  slice_max(-CatchRank, n=Top_n, with_ties = TRUE)

Top30Sp<-factor(c(Top30_Landings$NMFS.Name)) # Extract names of top 30 species overall by landings


## Create a new dataframe with only the Top 30 by landings ----
## Create a scaled revenue vector
Top30<-YrSps %>%
  filter(NMFS.Name %in% c(Top30Sp)) %>%
         mutate(MgGp=as.numeric(Management.Group)) %>%
  droplevels()

## 1. Timeseries gaps ----
#CHECK FOR GAPS IN TIMESERIES AND DECIDE WHAT TO DO WITH MISSING DATA
beforeTime<-ggplot(Top30 %>% filter(!Terminal.Yr.Dollars==0),
                   aes(fct_reorder(NMFS.Name, MgGp), y=Year, colour=Management.Group)) +
  geom_point() +
  ylab("Year") + xlab("Species/Spp. Aggregation") +
  theme(axis.text.x = element_text(angle = 90, hjust=1, size=12),
        legend.position = "right",
        legend.title = element_text(size=15),
        legend.text = element_text(size=15)) +
  labs(colour="Fisheries\nManagement\nPlan")
beforeTime

## DATA DECISIONS ----
# 1. PORGIES** merged with SCUP
# 2. Aggregate all squids
# 3. Spiny dogfish were aggregated with the historic aggregation “SHARKS, DOGFISH **”. #Kick out smooth dogfish

#You will need these species too to deal with missing data in the Top30
ExtraSps<-factor(c("SHARKS, DOGFISH **", "SCUP"))
Top30Plus<-c(Top30Sp,ExtraSps)

Top30b<-YrSps %>%
  filter(NMFS.Name %in% c(Top30Plus)) %>%
  mutate(MgGp=as.numeric(Management.Group)) %>%
  droplevels()


# Aggregate porgies and scup so there is one revenue and landings value per year
PORGS <- Top30b %>%
  filter(NMFS.Name %in% c("SCUP", "PORGIES **")) %>%
  aggregate(.~Year, FUN='sum') %>%
  mutate(NMFS.Namev2="PORGIES_SCUP",
         Taxonkey=as.numeric(00000),
         NMFS.Name="PORGIES_SCUP",
         Scientific.Name="PORGIES_SCUP",
         Confidentiality="Public",
         Collection="Commercial",
         Management.Group="Other",
         MgGp=5)
table(PORGS$Year) #check there is only one row per year

# Aggregate squids so there is one revenue and landings value per year
SQUIDS <- Top30b %>%
  filter(grepl("SQUID", NMFS.Name)) %>%
  aggregate(.~Year, FUN='sum') %>%
  mutate(NMFS.Namev2="SQUIDS",
         Taxonkey=as.numeric(8),
         NMFS.Name="SQUIDS",
         Scientific.Name="SQUIDS",
         Confidentiality="Public",
         Collection="Commercial",
         Management.Group="Other",
         MgGp=5)
table(SQUIDS$Year) #check there is only one row per year

# Aggregate dogfish so there is one revenue and landings value per year
DOGS <- Top30b %>%
  filter(NMFS.Name %in% c("SHARK, DOGFISH, SPINY", "SHARKS, DOGFISH **")) %>%
  aggregate(.~Year, FUN='sum') %>%
  mutate(NMFS.Namev2="SPINY AGG",
         Taxonkey=as.numeric(9),
         NMFS.Name="SPINY AGG",
         Scientific.Name="SPINY AGG",
         Confidentiality="Public",
         Collection="Commercial",
         Management.Group="Other",
         MgGp=8)
table(DOGS$Year) #check there is only one row per year

#Remove "SHARK, DOGFISH, SPINY", "PORGIES **" and all SQUID from dataframe and merge with new dataframes above
RmvSps<-as.factor(c("SHARK, DOGFISH, SPINY", "SHARKS, DOGFISH **", 
                    "PORGIES **", "SCUP",
                    "SQUID, LONGFIN LOLIGO", "SQUIDS, LOLIGINIDAE **",
                    "SQUID, SHORTFIN ILLEX",
                    "WITHHELD FOR CONFIDENTIALITY", 
                    "VERTEBRATES, JAWED", 
                    "SHRIMPS, PENAEOID **"))

df1<-Top30b %>%
  filter(!NMFS.Name %in% c(RmvSps)) 

P_Selection<-rbind(df1,SQUIDS, DOGS, PORGS) %>%
  mutate(Scaled.Value=Terminal.Yr.Dollars/max(Terminal.Yr.Dollars))

# Set scaling factor ----
range(P_Selection$Scaled.Value)
SCALING_F<-max(P_Selection$Terminal.Yr.Dollars) #Save your scaling factor to easily re-scale values back to original
SCALING_F

# PLOTS ----
## Time series plots ----
## See "Timeseries plots.R" script for combined before/after manuscript plot
beforeTime

afterTime<-ggplot(P_Selection, aes(fct_reorder(NMFS.Name, MgGp), y=Year, colour=Management.Group)) +
  geom_point() +
  ylab("Year") + xlab("Species/Spp. Aggregation") +
  theme(axis.text.x = element_text(angle = 90, hjust=1, size=12),
        legend.position = "right",
        legend.title = element_text(size=15),
        legend.text = element_text(size=15)) +
  labs(colour="Fisheries\nManagement\nPlan")
afterTime

##Landings Plot ----
ggplot(P_Selection,
       aes(fill=reorder(NMFS.Name, +Metric.Tons), y=Metric.Tons/10e4, x=Year)) + 
  geom_bar(position='stack', stat='identity') +
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
  theme(legend.key.size = unit(3, 'mm'),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 0, vjust = 0.7, size=20),) +    
  labs(x="Year", y=expression('Landings (Metric Tons x10'^4*')')) +
  guides(fill=guide_legend(ncol=4))

## Revenue plot ----
range(P_Selection$Terminal.Yr.Dollars)
OM<-1.0e8 #set correct order of magnitude for revenue

ggplot(P_Selection,
       aes(fill=reorder(NMFS.Name, +Metric.Tons), y=(Terminal.Yr.Dollars)/OM, x=Year)) + 
  geom_bar(position='stack', stat='identity') +
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
  theme(legend.key.size = unit(3, 'mm'),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 0, vjust = 0.7, size=20),) +    
  labs(x="Year", y=expression('Revenue (Hundred Million $)')) + #Set label axis accordingly
  guides(fill=guide_legend(ncol=4))


LB_SUM<-aggregate(cbind(Metric.Tons, Terminal.Yr.Dollars)~Year, P_Selection, sum) %>%
  dplyr::rename(SumCT=Metric.Tons,
                SumVAL=Terminal.Yr.Dollars)

data<-merge(P_Selection, LB_SUM, by=c("Year")) %>%
  mutate(Price=Terminal.Yr.Dollars/Metric.Tons,
         MgGp=as.numeric(Management.Group)) %>%
  mutate(IYear=Year, #Naming for the function
         Catch=Metric.Tons,
         Value=Scaled.Value)


## Correlation matrix ----
# CHECK THERE IS A GOOD MIX OF COVARIANCE/CORRELATION IN YOUR SELECTED PORTFOLIO
ALL_YEAR <- unique(data$Year)
ALL_TAXA <- unique(data$NMFS.Namev2)
ALL_COMB <- expand.grid(ALL_YEAR,ALL_TAXA) %>%
  dplyr::rename(Year=Var1,
                NMFS.Namev2=Var2)

d_2 <- merge(data, ALL_COMB, all=TRUE,by=c('Year','NMFS.Namev2')) %>%
  dplyr::mutate(Terminal.Yr.Dollars = replace_na(Terminal.Yr.Dollars, 0),
                Metric.Tons = replace_na(Metric.Tons, 0),
                Price = replace_na(Terminal.Yr.Dollars/Metric.Tons,0))

dataCOR<- d_2 %>%
  select(NMFS.Namev2, Year, Terminal.Yr.Dollars) %>%
  pivot_wider(names_from = NMFS.Namev2, values_from = Terminal.Yr.Dollars) %>%
  tibble::column_to_rownames('Year')

dataCOR=cor(dataCOR)
corrplot(dataCOR, method = 'square', order = 'AOE', type = 'lower', diag = FALSE)
dev.off()

## Combined Revenue timeseries ----
ggplot(data) +
  geom_point(aes(x=Year, y=(SumVAL)/OM)) + 
  labs(y ="Revenue (Hundred Million Dollars)", x="Year")

ggplot(data) +
  geom_line(aes(x=Year, y=Terminal.Yr.Dollars/OM, colour=NMFS.Namev2), linewidth=1.5) + 
  scale_color_manual(values = c("ALEWIFE"="#7FC97F",
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
  labs(y ="Revenue (Hundred Million Dollars)", x="Year") +
  theme(legend.text=element_text(size=8), legend.title = element_blank())

ggplot(data) +
  geom_line(aes(x=Year, y=log(Terminal.Yr.Dollars/OM), colour=NMFS.Namev2), linewidth=2) + 
  scale_color_manual(values = c("ALEWIFE"="#7FC97F",
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
  labs(y ="log(Revenue (Hundred Million Dollars))",x="Year") +
  theme(legend.text=element_text(size=8), legend.title = element_blank())


