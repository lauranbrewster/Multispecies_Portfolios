# Run the function first with the parameters set up how you want. Then run this. 
# The outermost loop is iterating through the data set
# dropping one species at a time. It will produce a column in the dataframe 
#"FrontierResults" which will tell you which species has been dropped

YEARS<-unique(data$IYear)
FrontierResults <-NULL
ImpResults <-NULL
Portfolio <-NULL
Taxon_Opt_Weight <- NULL
Half_Max_Rev <- NULL

#Create the dataframe minus one species
sps<-unique(data$NMFS.Namev2)
pp<-factor(c("FULL"))
sps<-c(sps,pp)
sps<-droplevels(sps)

#run the function with this loop around it
for (i in sps){
  temp <- data[data$NMFS.Namev2 != i,] %>%
    droplevels()
  print(unique(temp$NMFS.Namev2))
  
  for (k in YEARS) tryCatch({
    
    yr = min(YEARS):k
    N = as.numeric(length(yr))
    j = N
    LAST_YEAR = max(yr)
    
    c<-portfolio_risk(data=temp,YEAR=LAST_YEAR,YEARS=yr,TAXA=Taxonkey,N=N,j=j, EBFM = TRUE)
    d<-portfolio_risk(data=temp,YEAR=LAST_YEAR,YEARS=yr,TAXA=Taxonkey,N=N,j=j, EBFM = FALSE)
    
    
    EBFM_EF_Risk <- c$MEAN_VAR
    Type <- rep("EBFM",nrow(c$MEAN_VAR))
    EBFM_EF_Risk <- cbind(Type, EBFM_EF_Risk)
    
    SS_EF_Risk <- d$MEAN_VAR
    Type <- rep("SS",nrow(d$MEAN_VAR))
    SS_EF_Risk <- cbind(Type, SS_EF_Risk)
    
    All_EF_Risk <- rbind(EBFM_EF_Risk, SS_EF_Risk)
    # All_EF <- rbind(All_EF, SS_EF)
    All_EF_Risk$Iteration<-LAST_YEAR #!!
    All_EF_Risk$DroppedSpecies <- rep(i, nrow(All_EF_Risk))
    Iteration<-LAST_YEAR
    
    FrontierResults <- rbind(All_EF_Risk, FrontierResults)

    print(Iteration)
    print(i)
    
  }, error = function(e){})
}

FrontierResults$test<-FrontierResults$OptimizedRevenue*SCALING_F
range(FrontierResults$test)

OM<-1.0e8

ggplot() +
  geom_line(dat = FrontierResults %>%
              filter(Iteration=="2021" & Type=="EBFM" & !DroppedSpecies == "FULL"),
            aes(x= (OptimizedStDev*SCALING_F)/OM, y=(OptimizedRevenue*SCALING_F)/OM, color=DroppedSpecies), lwd=1) +
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
  geom_line(dat = FrontierResults %>%
              filter(Iteration=="2021" & Type=="EBFM" & DroppedSpecies == "FULL"),
            aes(x= (OptimizedStDev*SCALING_F)/OM, y=(OptimizedRevenue*SCALING_F)/OM), lwd=2, color="black", linetype="dotted") +
  labs(x="Risk (SD of Revenue)", y="Revenue (Hundred Million $)") +
  theme(legend.title=element_blank(),
        legend.text = element_text(size=11)) +
  guides(colour = guide_legend(override.aes = list(lwd=3)),
         linetype = guide_legend(override.aes = list(size = 15))) 
# + facet_wrap(~DroppedSpecies) #use this if you want to see each species in a separate panel

