# Time series sensitivity
YEARS<-unique(data$IYear) # define timeframe here 
YEARS<-c(2010:2021)

FrontierResults <-NULL
ImpResults <-NULL
Portfolio <-NULL
Taxon_Opt_Weight <- NULL
Half_Max_Rev <- NULL

# k=2010

for (k in YEARS) tryCatch({
  
  yr = min(YEARS):k
  # yr = k:max(YEARS)
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
  All_EF_Risk$Iteration<-k #!!
  #All_EF_Risk$DroppedSpecies <- rep(i, nrow(All_EF_Risk))
  Iteration<-k
  
  FrontierResults <- rbind(All_EF_Risk, FrontierResults)

  # TImpWeights <- c$T_IMPLICIT_WEIGHTS
  # Iteration<-i
  # ImpWeights <- cbind(Iteration, TImpWeights)
  # ImpWeights<-as.data.frame(ImpWeights) #implicit weights for the terminal year
  # #
  # denom<-t(ImpWeights$TImpWeight)%*%ImpWeights$WVal #(denominator in Eqn. 6)
  # #
  # ImpResults <- rbind(ImpWeights, ImpResults)
  # ImpResults <-as.data.frame(ImpResults)
  # #
  # Front_Portfolio<- c$FRONTIER_PORTFOLIO
  # Act_Portfolio_T <-c$ACTUAL_PORTFOLIO_T
  # AF<-cbind(Iteration, Act_Portfolio_T, Front_Portfolio)
  # AF<-cbind(AF,denom = denom)
  # #
  # Portfolio<-rbind(AF, Portfolio)
  # #
  # # Create a dataframe which pulls the optimal weights for each taxon key, for each target revenue run, for each year
  # alex <- c$TAXON_OPTIMAL_WEIGHTS
  # alex$Iteration<-LAST_YEAR
  # Taxon_Opt_Weight <- rbind(Taxon_Opt_Weight, alex)
  # # Taxon_Opt_Weight<-as.data.frame(Taxon_Opt_Weight)
  #
  # #Create a dataframe which shows half of the maximum revenue up until time t
  # HalfMaxRev <- c$HALF_MAX_REV
  # HalfMaxRev <- cbind(HalfMaxRev, Iteration)
  # Half_Max_Rev <- as.data.frame(rbind(Half_Max_Rev, HalfMaxRev))
  
  print(Iteration)
  print(i)
  
}, error = function(e){})

# Full<-FrontierResults
# Sixty<-FrontierResults
# Seventy<-FrontierResults
# Eighty<-FrontierResults
# Ninety<-FrontierResults
# Naughties<-FrontierResults
Teens<-FrontierResults

Full$Time<-c("1950")
Sixty$Time<-c("1960")
Seventy$Time<-c("1970")
Eighty$Time<-c("1980")
Ninety$Time<-c("1990")
Naughties$Time<-c("2000")
Teens$Time<-c("2010")

TimeSensitivity<-rbind(Full, Sixty, Seventy, Eighty, Ninety, Naughties, Teens) %>%
  filter(Iteration=="2021")


ggplot() +
  geom_line(dat = TimeSensitivity %>%
              filter(Type=="EBFM" & !Time == "1950"),
            aes(x= (OptimizedStDev*SCALING_F)/OM, y=(OptimizedRevenue*SCALING_F)/OM, color=Time), lwd=1.5) +
  geom_line(dat = TimeSensitivity %>%
              filter(Type=="EBFM" & Time == "1950"),
            aes(x= (OptimizedStDev*SCALING_F)/OM, y=(OptimizedRevenue*SCALING_F)/OM), lwd=1.5, color="black", linetype="dotted") +
  labs(x="Risk (SD of Revenue)", y="Revenue (Hundred Million $)", color='Time Series') +
  theme(legend.title = element_text(size=15),
        legend.text = element_text(size=11)) 
ggsave(path = "Manuscript Plots", filename = "Timeseriesv2.png", width = 15, height = 10, dpi = 600, bg='transparent')


