#LOOP OVER THE FUNCTION ----
#Loop through the years so the frontier is going up until time t.
YEARS<-unique(data$IYear)
FrontierResults <-NULL
ImpResults <-NULL
Portfolio <-NULL
Taxon_Opt_Weight <- NULL
Half_Max_Rev <- NULL


for (k in YEARS) tryCatch({
  
  yr = min(YEARS):k
  N = as.numeric(length(yr))
  j = N
  LAST_YEAR = max(yr)
  
  c<-portfolio_risk(data=data,YEAR=LAST_YEAR,YEARS=yr,TAXA=Taxonkey,N=N,j=j, EBFM = TRUE)
  d<-portfolio_risk(data=data,YEAR=LAST_YEAR,YEARS=yr,TAXA=Taxonkey,N=N,j=j, EBFM = FALSE)
  
  
  EBFM_EF_Risk <- c$MEAN_VAR
  Type <- rep("EBFM",nrow(c$MEAN_VAR))
  EBFM_EF_Risk <- cbind(Type, EBFM_EF_Risk)
  
  SS_EF_Risk <- d$MEAN_VAR
  Type <- rep("SS",nrow(d$MEAN_VAR))
  
  
  SS_EF_Risk <- cbind(Type, SS_EF_Risk)
  
  All_EF_Risk <- rbind(EBFM_EF_Risk, SS_EF_Risk)
  # All_EF <- rbind(All_EF, SS_EF)
  All_EF_Risk$Iteration<-LAST_YEAR #!!
  Iteration<-LAST_YEAR
  
  FrontierResults <- rbind(All_EF_Risk, FrontierResults)
  
  TImpWeights <- c$T_IMPLICIT_WEIGHTS
  Iteration<-LAST_YEAR
  ImpWeights <- cbind(Iteration, TImpWeights)
  ImpWeights<-as.data.frame(ImpWeights) #implicit weights for the terminal year
  
  denom<-t(ImpWeights$TImpWeight)%*%ImpWeights$WVal #(denominator in Eqn. 6)
  
  ImpResults <- rbind(ImpWeights, ImpResults)
  ImpResults <-as.data.frame(ImpResults) 
  
  Front_Portfolio<- c$FRONTIER_PORTFOLIO
  Act_Portfolio_T <-c$ACTUAL_PORTFOLIO_T
  AF<-cbind(Iteration, Act_Portfolio_T, Front_Portfolio)
  AF<-cbind(AF,denom = denom)
  
  Portfolio<-rbind(AF, Portfolio)
  
  # Create a dataframe which pulls the optimal weights for each taxon key, for each target revenue run, for each year  
  alex <- c$TAXON_OPTIMAL_WEIGHTS
  alex$Iteration<-LAST_YEAR
  Taxon_Opt_Weight <- rbind(Taxon_Opt_Weight, alex)
  # Taxon_Opt_Weight<-as.data.frame(Taxon_Opt_Weight) 
  
  #Create a dataframe which shows half of the maximum revenue up until time t
  HalfMaxRev <- c$HALF_MAX_REV
  HalfMaxRev <- cbind(HalfMaxRev, Iteration)
  Half_Max_Rev <- as.data.frame(rbind(Half_Max_Rev, HalfMaxRev)) 
  
  print(Iteration)
  
  
}, error = function(e){}) #if this doesn't spit out any results try toggling dplyr off and on again. Re-try.

# How many targets per year? ----
  ggplot(FrontierResults, aes(x=Iteration, fill=Type)) +
  geom_bar(position="dodge", stat="count") +
  scale_x_continuous(breaks=seq(min(FrontierResults$Iteration), max(FrontierResults$Iteration), by = 2)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_text(size=14)) +
  labs(y ="# Targets Per Frontier", x = "Year", fill='Frontier Type', size=10)  

## Portfolio ----
Portfolio<-Portfolio %>%
  # select(-c(3:4)) %>% #drop duplicated columns including TARGET
  mutate(RiskgapNum=sqrt(ActualP_T)-sqrt(FrontierP),
         RiskgapDollar=RiskgapNum/denom) %>%
  filter(!duplicated(.)) #there are duplicated rows where the LastYearRevenue is the same as a quantile value
#so there are more in the earlier years of the timeseries when the distribution is narrow

# Add the species names back in for plotting instead of taxonkey
a<-data %>%
  select(NMFS.Namev2, Taxonkey) %>%
  filter(!duplicated(.))

ImpResults <- full_join(ImpResults, a, by="Taxonkey") 
ImpResults <- ImpResults %>%
  mutate(ImpWeightShare = TImpWeight/MaxWeight) 
# Optimal revenue weight (w,it) / Max weight for each species (W,it) where optimal weights are calculated at 0.5 of max return

# Pull the optimal weights calculated when expected revenue = half of the maximum return up until time t
Taxon_Opt_Weight<-full_join(Taxon_Opt_Weight, a, by="Taxonkey")
Taxon_Opt_Weight <- Taxon_Opt_Weight %>%
  filter(!duplicated(.)) %>%
  filter(TARGET %in% Half_Max_Rev$HalfMaxRev) %>% # Filtering out the target values to match the values in Half_Max_Rev dataframe
  mutate(RevWeightShare = Optimal_weights/MaxWeight)
