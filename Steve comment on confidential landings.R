raw <- read_excel("FOSS_landings by region 2022.xlsx")
# raw <- read_excel("FOSS_landings by state 2022.xlsx")

Confidential <- raw %>%
  filter(Confidentiality=="Confidential") %>%
  filter(NMFS.Name %in% c(Top30Plus)) %>%
  droplevels()

unique(Confidential$NMFS.Name)

Confidential_Yr<- Confidential %>%
  group_by(Year) %>%
  select(Year, NMFS.Name) %>%
  count(NMFS.Name) %>%
  dplyr::rename(IYear=Year)

P_Selection<-raw %>%
  filter(Confidentiality=="Public") %>%
  filter(NMFS.Name %in% c(Top30Plus)) %>%
  dplyr::rename(IYear=Year)

steve<-left_join(Confidential_Yr, P_Selection, join_by(IYear, NMFS.Name)) 

# steve2<-steve %>%
#    select(IYear, NMFS.Name, n, Confidentiality, NMFS.Namev2, Dollars, OrigValue, Catch)

steve2<-steve %>%
  select(IYear, NMFS.Name, n, Confidentiality, Dollars, Metric.Tons)

write.csv(steve2, "Confidential_Landings_Region.csv", row.names = FALSE)
Top30Plus


unique(steve2$NMFS.Name)


# =========================================
Region <- read.csv("~/Portfolio project/Methods paper/Methods Paper Analysis/Confidential_Landings_Region.csv")
State <- read_excel("Confidential_Landings_nonStandardizedRevenue.xlsx")

State<-State %>%
  dplyr::rename(StateDollars="$") %>%
  dplyr::rename(StatesMetric.Tons="Landings (metric tons)") %>%
  select(IYear, NMFS.Name, StateDollars, StatesMetric.Tons)

Region<-Region %>%
  dplyr::rename(RegionMetric.Tons=Metric.Tons) %>%
  dplyr::rename(RegionDollars=Dollars) %>%
  select(IYear, NMFS.Name, RegionDollars, RegionMetric.Tons)

chk<-left_join(Region, State, join_by(IYear, NMFS.Name))

chk2 <- chk %>%
  mutate(StatesMetric.Tons=as.numeric(StatesMetric.Tons),
         StateDollars=as.numeric(StateDollars),
         Metric.TonsDiff=RegionMetric.Tons-StatesMetric.Tons,
         DollarsDiff=RegionDollars-StateDollars) %>%
  select(IYear, NMFS.Name, RegionDollars, StateDollars, DollarsDiff, RegionMetric.Tons, StatesMetric.Tons, Metric.TonsDiff)

# write.csv(chk2, "Region vs States download comparison Top30.csv")

