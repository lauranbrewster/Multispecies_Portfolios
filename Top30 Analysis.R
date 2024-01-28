# LOAD PACKAGES ----
require(readxl)
require(tidyverse)
# require(tidyr)
require(priceR) #for standardizing price with inflation
require(RColorBrewer)
require(viridis)
library(qualpalr) #divergent colour palettes for plots
require(eeptools)
require(openxlsx)
require(grDevices)
require(extrafont)
require(boot)
require(kernlab)
require(reshape)
require(matrixcalc)
require(corrplot)
require(corrr) #PCA

##DATA LOOK ----
setwd("~/Portfolio project/Methods paper/Methods Paper Analysis")
raw <- read_excel("foss_landings_NMFSRegion_Year_Species_State_UpTo2021Data_StandardisedPrice.xlsx")
raw <- raw %>% mutate_if(is.character,as.factor) #covert all character strings to factors

# RAggregate dataframe to ignore states (if wanted)
YrSps<-raw %>%
  #filter(!(Management.Group=="Other") & Confidentiality=="Public") %>%
  filter(Confidentiality=="Public",
         # !NMFS.Name=="WITHHELD FOR CONFIDENTIALITY",
         !grepl("SEAWEED", NMFS.Name)) %>%
  arrange(Management.Group, Scientific.Name, Year) %>%
  select(-c("State", "Source")) %>%
  aggregate(cbind(Standardized.Price, Dollars, Metric.Tons, Pounds) ~      
              Year+NMFS.Name+Confidentiality+Collection+Scientific.Name+Tsn+Management.Group+NMFS.Namev2, sum) %>%
  dplyr::rename("IYear"="Year",   #rename to match Geret's code
                "Value"="Standardized.Price",
                "Catch"="Metric.Tons",
                "Taxonkey"="Tsn") %>%
  as.data.frame()


#Calculate proportion each species contributed to the metric.ton landing each year
YrSps <- YrSps %>%
  group_by(IYear) %>%
  drop_na(Catch) %>%
  mutate(PercentAnnualCatch = Catch / sum(Catch)*100) %>% 
  as.data.frame()

#Aggregate dataframe to ignore year and rank the top 30 species overall by landings
Top30_Overall<- YrSps %>%
  # select(-c("State", "Source", "Year"))%>%
  group_by(NMFS.Name, Scientific.Name, NMFS.Namev2, Confidentiality, Management.Group, Collection) %>%
  summarise_all(sum) %>%
  as.data.frame(Overall) %>%
  mutate(CatchRank=rank(-Catch)) %>%
  slice_max(-CatchRank, n=5, with_ties = TRUE)

Top30Sp<-factor(c(Top30_Overall$NMFS.Name)) # Names of top 30 species overall by money made

#Remove scallop and lobster from top30
# test <- Top30Sp %>%
#   as.data.frame() %>%
#   dplyr::rename("NMFS.Name"=".") %>%
#   filter(!NMFS.Name %in% c("LOBSTER, AMERICAN", "SCALLOP, SEA")) 
# 
# Top30Sp<-factor(c(test$NMFS.Name))

SCALING_F<-1.0e8 #$ !!!!

Top30<-YrSps %>%
     filter(NMFS.Name %in% c(Top30Sp)) %>%
     mutate(Value=Value/SCALING_F, 
            MgGp=as.numeric(Management.Group)) %>%
     droplevels()

# PRE-CHECKS PLOTS ----
# Set plot theme ----
theme_set(theme_bw() + 
            theme(text = element_text(size = 24),
                  axis.title = element_text(size = 24),
                  axis.text.x = element_text(angle = 60, vjust = 0.7, size=20),
                  axis.text.y = element_text(size=20),
                  legend.position="top",
                  # legend.title=element_blank(),
                  legend.text=element_text(size=10),
                  # panel.background = element_rect(fill='transparent'), #transparent panel bg
                  # plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
                  # panel.grid.major = element_blank(), #remove major gridlines
                  # panel.grid.minor = element_blank(), #remove minor gridlines
                  # legend.background = element_rect(fill='transparent'), #transparent legend bg
                  # legend.box.background = element_rect(fill='transparent', color='NA'), #transparent legend panel
                  strip.background = element_blank(),
                  strip.text = element_text(colour = 'black', size=10)
                  
            ))
## 1. Timeseries gaps ----
#CHECK FOR GAPS IN TIMESERIES AND DECIDE WHAT TO DO WITH MISSING DATA
ggplot(Top30 %>% filter(!Value==0),
       aes(fct_reorder(NMFS.Name, MgGp), y=IYear, colour=Management.Group)) +
  geom_point() +
  # geom_hline(yintercept=1990, linetype="dashed", color = "black", linewidth=1) +
  ylab("Year") + xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust=1, size=12),
        legend.position = "right",
        legend.title = element_text(size=15),
        legend.text = element_text(size=15)) +
  labs(colour="Fisheries\nManagement\nPlan")
# ggsave(path = "Manuscript Plots", filename = "Fig 1.png", width = 15, height = 10, dpi = 600, bg='transparent')

#DATA DECISIONS ----
#PORGIES** were merged with SCUP
#Aggregate all squids
#Spiny dogfish were aggregated with the historic aggregation “SHARKS, DOGFISH **”. #Kick out smooth dogfish

#You will need these species too to deal with missing data in the Top30
ExtraSps<-factor(c("SHARKS, DOGFISH **", "SCUP"))
Top30Plus<-c(Top30Sp,ExtraSps)

Top30b<-YrSps %>%
  filter(NMFS.Name %in% c(Top30Plus)) %>%
  mutate(Value=Value/SCALING_F, 
         MgGp=as.numeric(Management.Group)) %>%
  droplevels()

PORGS <- Top30b %>%
  filter(NMFS.Name %in% c("SCUP", "PORGIES **")) %>%
  aggregate(.~IYear, FUN='sum') %>%
  mutate(NMFS.Namev2="PORGIES_SCUP",
         Taxonkey=as.numeric(00000),
         NMFS.Name="PORGIES_SCUP",
         Scientific.Name="PORGIES_SCUP",
         Confidentiality="Public",
         Collection="Commercial",
         Management.Group="Other",
         MgGp=5)
table(PORGS$IYear) #check there is only one row per year

SQUIDS <- Top30b %>%
  filter(grepl("SQUID", NMFS.Name)) %>%
  aggregate(.~IYear, FUN='sum') %>%
  mutate(NMFS.Namev2="SQUIDS",
         Taxonkey=as.numeric(8),
         NMFS.Name="SQUIDS",
         Scientific.Name="SQUIDS",
         Confidentiality="Public",
         Collection="Commercial",
         Management.Group="Other",
         MgGp=5)
table(SQUIDS$IYear) #check there is only one row per year

DOGS <- Top30b %>%
  filter(NMFS.Name %in% c("SHARK, DOGFISH, SPINY", "SHARKS, DOGFISH **")) %>%
  aggregate(.~IYear, FUN='sum') %>%
  mutate(NMFS.Namev2="SPINY AGG",
         Taxonkey=as.numeric(9),
         NMFS.Name="SPINY AGG",
         Scientific.Name="SPINY AGG",
         Confidentiality="Public",
         Collection="Commercial",
         Management.Group="Other",
         MgGp=8)
table(DOGS$IYear) #check there is only one row per year

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

P_Selection<-Top30 #use this if just running top 5 or something smaller for testing

P_Selection<-rbind(df1, PORGS, SQUIDS, DOGS)

ggplot(P_Selection %>%
         mutate(NMFS.Namev2=recode(NMFS.Namev2, "SKATE"='SKATES, RAJIDAE (FAMILY) **')),
       aes(fct_reorder(NMFS.Namev2, MgGp), y=IYear, colour=Management.Group)) +
  geom_point() +
  # geom_hline(yintercept=1990, linetype="dashed", color = "black", linewidth=1) +
  ylab("Year") + xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust=1, size=12),
        legend.position = "right",
        legend.title = element_text(size=15),
        legend.text = element_text(size=15)) +
  labs(colour="Fisheries\nManagement\nPlan")
 # ggsave(path = "Manuscript Plots", filename = "Fig 2.png", width = 15, height = 10, dpi = 600, bg='transparent')

n <- 30
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=sample(col_vector, n)) 

ggplot(P_Selection%>%
         mutate(NMFS.Namev2=recode(NMFS.Namev2, "SKATE"='SKATES, RAJIDAE (FAMILY) **')),
       aes(fill=reorder(NMFS.Name, +Catch), y=Catch/10e4, x=IYear)) + 
  geom_bar(position='stack', stat='identity') +
  scale_fill_manual(values = col_vector) +
  theme(legend.key.size = unit(3, 'mm'),
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.7, size=20),) +    
  labs(x="Year", y=expression('Landings (Metric Tons x10'^4*')')) +
  # y=expression('Landings (Metric Tons x10'^12*')')
  guides(fill=guide_legend(ncol=4))
 # ggsave(path = "Manuscript Plots", filename = "Fig 3.png", width = 15, height = 10, dpi = 600, bg='transparent')


# P_Selection<-YrSps %>%
#   filter(NMFS.Name %in% c(Top30Plus)) %>%
#   mutate(Value=Value/SCALING_F) %>%
#   droplevels()

LB_SUM<-aggregate(cbind(Catch, Value)~IYear, P_Selection, sum) %>%
  dplyr::rename(SumCT=Catch,
                SumVAL=Value)

data<-merge(P_Selection, LB_SUM, by=c("IYear")) %>%
  mutate(Price=Value/Catch,
         PortRevShare = Value / SumVAL,
         MgGp=as.numeric(Management.Group))

## 2. Correlation matrix ----
ALL_YEAR <- unique(data$IYear)
ALL_TAXA <- unique(data$NMFS.Namev2)
ALL_COMB <- expand.grid(ALL_YEAR,ALL_TAXA) %>%
  dplyr::rename(IYear=Var1,
                NMFS.Namev2=Var2)

d_2 <- merge(data,ALL_COMB, all=TRUE,by=c('IYear','NMFS.Namev2')) %>%
  dplyr::mutate(Value = replace_na(Value, 0),
                Catch = replace_na(Catch, 0),
                Price = replace_na(Value/Catch,0))
# CHECK THERE IS A GOOD MIX OF COVARIANCE/CORRELATION IN YOUR SELECTED PORTFOLIO
dataCOR<- d_2 %>%
  select(NMFS.Namev2, IYear, Value) %>%
  pivot_wider(names_from = NMFS.Namev2, values_from = Value) %>%
  tibble::column_to_rownames('IYear')
# dataCOR<-as.numeric(unlist(dataCOR))
dataCOR=cor(dataCOR)

# png(width = 1600, height = 1600, file="Manuscript Plots/Fig 4.png", type = "cairo")
# corrplot(dataCOR, method = 'square', order = 'FPC', type = 'lower', diag = FALSE, tl.cex = 2) #why is this not showing SQUIDS??

corrplot.mixed(dataCOR, order = 'AOE', addCoef.col = 1, # Change font size of text labels
               tl.pos = 'd',
               tl.cex = 1)
dev.off()


## 3. Combined Revenue timeseries ----
ggplot(data) +
  geom_point(aes(x=IYear, y=SumVAL)) + 
  # geom_line(aes(x=IYear, y=SumVAL)) +
  labs(y ="Revenue (Hundred Million Dollars)", x="Year")

## 4. Species Revenue timeseries ----
#CHECK THE TIME SERIES TRENDS BETWEEN SPECIES
#(might need to use representative species if multiple species follow the same trend)
pal = qualpal(length(unique(data$NMFS.Namev2)), colorspace=list(h=c(0,360), s=c(0.3,1), l=c(0.2,0.8)))

# pal$HSL %>% as.data.frame() %>%
#   rownames_to_column(var = "color") %>%
#   arrange(Hue) %>%
#   pull(color) %>%
#   show_col(labels=FALSE)

ggplot(data) +
  geom_line(aes(x=IYear, y=Value, colour=NMFS.Namev2), linewidth=1.5) + 
  # geom_point(aes(x=IYear, y=Value, colour=NMFS.Namev2), size=3) +
  scale_color_manual(values=pal$hex) +
  labs(y ="Revenue (Hundred Million Dollars)", x="Year") +
  theme(legend.text=element_text(size=8))

ggplot(data) +
  geom_line(aes(x=IYear, y=log(Value), colour=NMFS.Namev2), linewidth=2) + 
  scale_color_manual(values=pal$hex) +
  labs(y ="log(Revenue (Hundred Million Dollars))",x="Year") +
  theme(legend.text=element_text(size=8))

# RUN THE FUNCTION ----
#Use this info (YEAR through N) if you need to go line-by-line to look at how the code is functioning
# YEAR=2021
# YEARS<-unique(data$IYear)
# j=length(unique(data$IYear))
# N=40
# EBFM=TRUE

portfolio_risk <- function(data,YEAR,YEARS,TAXA,N,j,EBFM) {
  d_2 <- data %>% 
    filter(IYear %in% c(YEARS)) %>%
    aggregate(cbind(Value,Catch)~IYear+Taxonkey, FUN='sum') %>%
    arrange(IYear, Taxonkey, Value, Catch)
  
  ALL_YEAR <- unique(d_2$IYear)
  ALL_TAXA <- unique(d_2$Taxonkey)
  ALL_COMB <- expand.grid(ALL_YEAR,ALL_TAXA) %>%
    rename(IYear=Var1,
           Taxonkey=Var2)
  
  d_2 <- merge(d_2,ALL_COMB, all=TRUE,by=c('IYear','Taxonkey')) %>%
    dplyr::mutate(Value = replace_na(Value, 0),
                  Catch = replace_na(Catch, 0),
                  Price = replace_na(Value/Catch,0))
  
  # P_LAMBDA <- 0.549 #decay factor. 5% left after 5 years. If this is 1, each year is given equal weight.
  #P_LAMBDA <- 0.741 # 5% left after 10 years.
  # Lambda ----
  P_LAMBDA <- 1 #each period receives equal weight
  P_GAMMA <- 1 #sustainability parameter
  S_I <- 0.5
  z = length(unique(d_2$Taxonkey))
  
  #Adding the decay factor into the covariance matrix (Eqn. 2)
  d_2 <- d_2 %>%
    mutate(WCatch = P_LAMBDA^(YEAR-IYear+1)*Catch, 
           WAvgVal = WCatch*Price, # Eqn. 5. Value used to estimate omega. Omega= weighted average of catches over time with decay
           Lambda = P_LAMBDA^(YEAR-IYear+1),  #this is the decay value applied to each year.#ggplot (data=d_2, aes(x=desc(IYear), y=Lambda)) + geom_point() #check this with Geret because this was d_2$Lambda <- P_LAMBDA^(YEAR-d_2$IYear+1)
           WPrice = Lambda*Price,
           WVal = Lambda*Value)
  
  # max(WCatch) by taxonkey over the timeseries and use this for MAX_WEIGHTS
  
  d_3<- d_2%>%
    filter(IYear == YEAR)%>%
    mutate(tValue = Value,
           tYear = YEAR)%>%
    select(Taxonkey,tValue, tYear)
  
  
  WAVG_CATCH <- aggregate(cbind(Value,WCatch,Lambda,WAvgVal,WPrice,WVal)~Taxonkey,data=d_2,FUN='sum') %>%
    mutate(WCatch = WCatch/Lambda,
           Omega = WAvgVal/WPrice, #Eqn. 5. average biomass. Omega= weighted average of catches over time with decay.
           WVal = WVal/Lambda)
  MAX_CATCH <- aggregate(Catch~Taxonkey,data=d_2,FUN='max') %>%
    mutate(MaxCatch = Catch*P_GAMMA) %>% # Numerator of Eqn. 4 in Jin et al.
    select(Taxonkey,MaxCatch)
  
  WAVG_CATCH <- merge(d_3,WAVG_CATCH,by='Taxonkey') 
  
  WAVG_CATCH <- merge(MAX_CATCH,WAVG_CATCH,by='Taxonkey')  %>%
    mutate(MaxWeight = MaxCatch/Omega, #Eqn. 4 of Jin. et al. maximum weight = maximum biomass/average biomass) 
           Year = YEAR,
           # ImpWeight = Value/WVal, #implicit weight from actual return.
           TImpweight = tValue/WVal, #implicit weight from actual return.
           WRatio = TImpweight/MaxWeight, #ratio of implicit to max weight
           Chk = pmin(TImpweight,MaxWeight),
           CReturn = pmin(TImpweight,MaxWeight)*WVal)
  DIFF_RETURNS <-cbind(sum(WAVG_CATCH$CReturn[which(WAVG_CATCH$Year==YEAR)]), sum(WAVG_CATCH$Value[which(WAVG_CATCH$Year==YEAR)]))
  
  #Var-Covar. Estimating weighted Variance-Covariance matrix ----
  VAR_DATA <- merge(d_2,WAVG_CATCH, by='Taxonkey') %>%
    select('Taxonkey','IYear','Lambda.x','Value.x','WVal.y') %>%
    rename(Year=IYear,
           Lambda=Lambda.x,
           Value=Value.x,
           WVal=WVal.y) %>%
    mutate(Value = Value-WVal) %>%
    select(!WVal)
  
  VAR_DATA1 <- subset(VAR_DATA, select=c('Taxonkey','Year','Value')) %>%
    cast(Taxonkey~Year)
  
  VAR_DATA <- VAR_DATA %>%
    mutate(Value = Value*Lambda) %>%
    select(!Lambda) %>%
    cast(Taxonkey~Year)
  
  VAR_names <- VAR_DATA$Taxonkey
  
  VAR_DATA <- matrix(unlist(subset(VAR_DATA, select=-Taxonkey)),nrow=z, ncol=j) #z=number of spp. in portfolio, j=years in portfolio
  VAR_DATA1 <- matrix(unlist(subset(VAR_DATA1, select=-Taxonkey)),nrow=z, ncol=j)
  VAR_COVAR <- VAR_DATA%*%t(VAR_DATA1)
  VAR_COVAR <- VAR_COVAR/WAVG_CATCH$Lambda
  pmean <- function(x,y) (x+y)/2
  VAR_COVAR <- pmean(VAR_COVAR,matrix(VAR_COVAR,nrow(VAR_COVAR),byrow=TRUE)) ##???? maybe a rounding issue
  
  VAR_COVAR_SS <- diag(diag(VAR_COVAR[])) # puts the vector above into a diagonal matrix for use in optimizing based on single species assumptions 
  
  MEAN_RETURNS <- matrix(WAVG_CATCH$WVal)
  
  # Targets. Create the targets being fed through the optimiser ----
  
  TOT_REVENUE <- aggregate(cbind(Value)~IYear, data=d_2, FUN='sum')
  HALF_MAX_REV <- max(TOT_REVENUE$Value)*0.5
  
  TARGET_RETURNS <- quantile(TOT_REVENUE$Value, seq(.00,1,by=.05))
  # TARGET_RETURNS <- quantile(TOT_REVENUE$WVal, seq(.00,1,by=.05))
  filler<-seq(0.01, c(min(TARGET_RETURNS)), by = 0.1)
  
  LastYearRevenue <- TOT_REVENUE %>%
    filter(IYear == YEAR) 
  LastYearRevenue <- LastYearRevenue[,2]
  
  TARGET_RETURNS<-as.numeric(c(filler, HALF_MAX_REV, TARGET_RETURNS, LastYearRevenue))
  
  MAX_WEIGHTS <- matrix(WAVG_CATCH$MaxWeight,nrow=z)
  #CONSTRAINT_MATRIX <- diag(z)
  #CONSTRAINT_MATRIX <- -(rbind(CONSTRAINT_MATRIX,t(MEAN_RETURNS)))
  #CONSTRAINTS <- -c(MAX_WEIGHTS,TARGET_RETURNS)
  MIN_WEIGHTS <- matrix(0.001,nrow=z) #was zero
  
  OPTIMAL_WEIGHTS <- NULL
  OPTIMAL_REVENUE <- NULL
  OPTIMAL_LANDINGS <- NULL
  OPTIMAL_VAR <- NULL
  
  #Changed the margins from 10^-4 because of the computational singularity issue
  #with some of the years. 
  # 10^-4 = 0.0001 = so with 10^-8 scaling factor its within $10,000 (10^-4*100000000)
  # 10^-3 = 0.001 = $100,000 etc etc
  #Optimiser----
  
  for (TARGET in TARGET_RETURNS) {
    if(EBFM == TRUE){
      SOLUTION <- ipop(-MEAN_RETURNS,VAR_COVAR,t(-MEAN_RETURNS),-TARGET,
                       MIN_WEIGHTS, MAX_WEIGHTS, TARGET, margin=10^-3, sig=6,
                       verb=FALSE) #verb=TRUE gives more info for computational singularity issue
    } else {
      SOLUTION <- ipop(-MEAN_RETURNS,VAR_COVAR_SS,t(-MEAN_RETURNS),-TARGET,
                       MIN_WEIGHTS, MAX_WEIGHTS, TARGET, margin=10^-3, sig=6,
                       verb=FALSE)
    }
    
    
    #Pulling the optimum weights from the solution
    TEMP_WEIGHTS <- matrix(SOLUTION@primal) 
    colnames(TEMP_WEIGHTS) <- paste("Optimal_weights")
    
    #Calculating optimal revenue, by stock complex
    # TEMP_REVENUE <- MEAN_RETURNS*TEMP_WEIGHTS*SCALING_F  
    TEMP_REVENUE <- MEAN_RETURNS*TEMP_WEIGHTS
    colnames(TEMP_REVENUE) <- paste("Optimized_Revenue_",TARGET)
    
    #Calculate optimal landings, by stock complex
    # TEMP_LANDINGS <- WAVG_CATCH$WCatch*TEMP_WEIGHTS*SCALING_F
    TEMP_LANDINGS <- WAVG_CATCH$WCatch*TEMP_WEIGHTS
    colnames(TEMP_LANDINGS) <- paste("Optimized_Landings_",TARGET)
    
    #Calculate the realized variance for each optimization solution
    ###!!!! use full COV matirx here
    TEMP_VAR<-t(TEMP_WEIGHTS)%*%VAR_COVAR%*%TEMP_WEIGHTS
    # t(OPTIMAL_WEIGHTS)%*%VAR_COVAR%*%OPTIMAL_WEIGHTS)
    rownames(TEMP_VAR)<- paste("Optimized_Variance_",TARGET)
    # TEMP_VAR<-cbind(TEMP_VAR)
    TEMP_VAR<-cbind(TEMP_VAR, TARGET)
    
    # Accumulating the results for each optimization solution
    TEMP_WEIGHTS <- as.data.frame(cbind(TEMP_WEIGHTS, TARGET, VAR_names, WAVG_CATCH$MaxWeight))
    colnames(TEMP_WEIGHTS) <- c('Optimal_weights', "TARGET", "Taxonkey", "MaxWeight")
    OPTIMAL_WEIGHTS <- rbind(TEMP_WEIGHTS, OPTIMAL_WEIGHTS) #by taxonkey
    # TEMP_WEIGHTS <- cbind(TEMP_WEIGHTS, TARGET)
    # OPTIMAL_WEIGHTS <- cbind(OPTIMAL_WEIGHTS, TEMP_WEIGHTS) #by taxonkey
    OPTIMAL_REVENUE <- cbind(OPTIMAL_REVENUE,TEMP_REVENUE) #by taxonkey
    OPTIMAL_LANDINGS <- cbind(OPTIMAL_LANDINGS, TEMP_LANDINGS) #by taxonkey
    OPTIMAL_VAR <- rbind(OPTIMAL_VAR,TEMP_VAR) #one value, variance for the associated target value
    # OPTIMAL_VAR <- cbind(OPTIMAL_VAR,TEMP_VAR)
    
    print(TARGET) # check on progress of quadratic programming optimizer
  }
  
  #rownames(OPTIMAL_WEIGHTS) <- colnames(REVENUE[,1:stockNum])
  sum(OPTIMAL_WEIGHTS)
  #rownames(OPTIMAL_LANDINGS) <- colnames(REVENUE[,1:stockNum])
  #rownames(OPTIMAL_REVENUE) <- colnames(REVENUE[,1:stockNum])
  #colnames(OPTIMAL_VAR) <- "Optimized_Variance"
  MEAN_VAR <- cbind(OPTIMAL_VAR,colSums(OPTIMAL_REVENUE))
  colnames(MEAN_VAR) <- c("OptimizedVariance","TARGET", 'OptimizedRevenue')
  MEAN_VAR <- as.data.frame(MEAN_VAR)
  # MEAN_VAR$OptimizedRevenue <- MEAN_VAR$OptimizedRevenue
  MEAN_VAR$OptimizedStDev <- sqrt(MEAN_VAR$OptimizedVariance)
  
  T_IMPLICIT_WEIGHTS = cbind(WAVG_CATCH$Taxonkey, WAVG_CATCH$TImpweight, WAVG_CATCH$MaxWeight, WAVG_CATCH$WVal)
  colnames(T_IMPLICIT_WEIGHTS) <- c( 'Taxonkey',"TImpWeight","MaxWeight", "WVal")
  
  AP_T<-matrix(t(WAVG_CATCH$TImpweight)%*%VAR_COVAR%*%WAVG_CATCH$TImpweight)
  colnames(AP_T) <- 'ActualP_T'
  AP_T<-as.data.frame(cbind(AP_T, LastYearRevenue, YEAR)) #you need to sqrt the AP to get SD
  
  # This should just be MEAN_VAR FOR THE TERMINAL YEAR
  FP<-MEAN_VAR %>%
    filter(TARGET==LastYearRevenue)
  colnames(FP) <- c('FrontierP', "TARGET", "OptimizedRevenue", "OptimizedStDev") ##you need to sqrt the FrontierP to get SD
  
  # FP<-matrix(t(OPTIMAL_WEIGHTS[,1])%*%VAR_COVAR%*%OPTIMAL_WEIGHTS[,1])
  # colnames(FP) <- "Frontier"
  # FP<-as.data.frame(cbind(FP, TARGET, YEAR)) #you need to sqrt the FP to get SD
  
  ############### List of data frames ############
  optimums <- list ("MEAN_VAR"=MEAN_VAR,"TAXON_OPTIMAL_WEIGHTS" =OPTIMAL_WEIGHTS, "COVARIANCE" =VAR_COVAR, 
                    "T_IMPLICIT_WEIGHTS" = T_IMPLICIT_WEIGHTS, 
                    "ACTUAL_PORTFOLIO_T"= AP_T,"FRONTIER_PORTFOLIO"=FP,
                    "HALF_MAX_REV" = HALF_MAX_REV)
  return(optimums)
} 

#LOOP OVER THE FUNCTION ----
#Loop through the years so the frontier is going up until time t.
YEARS<-unique(data$IYear)
FrontierResults <-NULL
ImpResults <-NULL
Portfolio <-NULL
Taxon_Opt_Weight <- NULL
Half_Max_Rev <- NULL

for (k in YEARS) {
  
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
  
}

# View(Portfolio)
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

#PLOTS #################################
## Frontier plot ----
### Removes the first year (min(Iteration))ggplot() +
ggplot() +
  geom_line(data =FrontierResults %>%
              # filter(Type=='EBFM') %>% #Use this line if you want to drop the SS blue line.
              filter(!Iteration==min(Iteration)), 
            aes(x= OptimizedStDev, y=OptimizedRevenue, color=Type),lwd=1)+
  geom_point(data =Portfolio %>%
               filter(!Iteration==min(Iteration)), aes(x = sqrt(ActualP_T), y = TARGET))+
  labs(y ="Revenue (Hundred Million $)", x = "Risk (SD of Revenue)") +
  facet_wrap(~Iteration, scales="fixed")
setwd("~/Portfolio project/Methods paper/Methods Paper Analysis")
ggsave(path = "Manuscript Plots", filename = "Jin no decay s1.png", width = 15, height = 10, dpi = 600)

## Risk gap (numerator) plot ----
ggplot(data=Portfolio) +
  geom_line(aes(x=Iteration, y=RiskgapNum)) +
  geom_point(aes(x=Iteration, y=RiskgapNum)) +
  xlab("Year") + ylab("Risk gap in dollars (Hundred Million $)")
# ggsave(path = "Figures for Dec 22 Steering Committee meeting", filename = "Riskgap numerator NE FMP portfolio.png", width = 20, height = 15, dpi = 120)

## Risk Gap Dollars Plot ----
ggplot(data=Portfolio) +
  geom_line(aes(x=Iteration, y=RiskgapDollar), colour="red") +
  geom_point(aes(x=Iteration, y=RiskgapDollar), colour="red") +
  xlab("Year") +
  ylab("Risk gap Per Dollar")
# geom_line(aes(x=Iteration, y=RiskgapNum), colour="red")
# ggsave(path = "Figures for Dec 22 Steering Committee meeting", filename = "Riskgap per dollar NE FMP portfolio.png", width = 20, height = 15, dpi = 120)

##Barplot of risk gap vs riskgap in dollars 
# coeff <- 1
# ggplot(Portfolio, aes(x=Iteration)) +
#   geom_bar( aes(y=RiskgapDollar), stat="identity", size=.1, fill="black", color="black", alpha=.4) +
#   geom_line( aes(y=RiskgapNum / coeff), size=1, color="red") +
#   geom_point( aes(y=RiskgapNum / coeff), size=2, color="red") +
#     scale_y_continuous(
#     # Features of the first axis
#     name = "Risk Gap Per Dollar",
#     # Add a second axis and specify its features
#     sec.axis = sec_axis(~.*coeff, name="Risk Gap")) +
#   theme(axis.title.y = element_text(color = "black", size=15),
#         axis.title.y.right = element_text(color = "red", size=15)) +
#   xlab("Year")


## Barplot of risk gap vs riskgap in dollars ----
coeff <- 1
ggplot(Portfolio %>% filter(!Iteration==min(Iteration)), aes(x=Iteration) ) +
  geom_bar( aes(y=RiskgapNum), stat="identity", size=.1, fill="black", color="black", alpha=.4) +
  # geom_line(aes(x=Iteration, y=RiskgapNum)) +
  geom_line( aes(y=RiskgapDollar / coeff), size=1, color="red") +
  geom_point( aes(y=RiskgapDollar / coeff), size=2, color="red") +
  scale_y_continuous(
    # Features of the first axis
    name = "Risk Gap",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Risk Gap Per Dollar")) +
  theme(axis.title.y = element_text(color = "black", size=15),
        axis.title.y.right = element_text(color = "red", size=15)) +
  xlab("Year")

## Implicit and Optimal revenue share weights by species ----
ggplot(data=ImpResults) + 
  geom_col(aes(x=Iteration, y=ImpWeightShare)) +
  facet_wrap(~NMFS.Namev2) +
  labs(y ="Implicit Weight Shares", x = "Year") +
  geom_hline(yintercept = 1, color="red", linetype="dashed")

ggplot(data=Taxon_Opt_Weight) + 
  geom_col(aes(x=Iteration, y=RevWeightShare), fill="darkblue") + ##Here the optimal weights are calculated as 0.5*maxweight
  facet_wrap(~NMFS.Namev2) +
  labs(y ="Optimal Revenue Weight Share", x = "Year") +
  geom_hline(yintercept = 1, color="red", linetype="dashed") 

# Optimal and Implicit weights, revenue share by species
# legend_colors <- c("Implicit Weights" = "dodgerblue1", "Optimal Weights" = "red", "Revenue Share" = "darkgreen")
# 
# ggplot() +
#   geom_line(data=ImpResults, aes(x=Iteration, y=TImpWeight, colour="Implicit Weights"), size=1) +
#   geom_line(data=Taxon_Opt_Weight, aes(x=Iteration, y=Optimal_weights, colour="Optimal Weights"), size=1) +
#   geom_line(data=data , aes(x=IYear, y=PortRevShare, colour="Revenue Share"), size=1,) +
#   facet_wrap(~NMFS.Namev2) +
#   labs(y ="Weights", x = "Year") +
#   scale_color_manual(values = legend_colors)

## Alternative if you need double axis for scaling issues
# coeff <- 10
# ggplot() +
#   geom_line(data=ImpResults, aes(x=Iteration, y=TImpWeight, colour="Implicit Weights"), size=1) +
#   geom_line(data=Taxon_Opt_Weight, aes(x=Iteration, y=Optimal_weights, colour="Optimal Weights"), size=1) +
#   geom_line(data=data, aes(x=IYear, y=PortRevShare*coeff, colour="Revenue Share"), size=1) +
#   scale_y_continuous(
#     # Features of the first axis
#     name = "Weights",
#     # Add a second axis and specify its features
#     sec.axis = sec_axis(~.*coeff, name="Revenue Share (%)")) + 
#   theme(axis.title.y = element_text(color = "black", size=15),
#         axis.title.y.right = element_text(color = "darkgreen", size=15)) +
#   xlab("Year") +
#   scale_color_manual(values = legend_colors) +
#   facet_wrap(~NMFS.Namev2)

######################################
require("MASS") 
require("reshape2") 
require("reshape") 
## EBFM-SS frontier difference plot. Difference is calculated at actual revenue (point b) for the year on y axis ----
link<-data %>%
  group_by(IYear, NMFS.Namev2) %>%
  aggregate(Value~IYear, sum) %>%
  rename(Iteration=IYear,
         TARGET=Value)

chk<-left_join(link, FrontierResults) %>%
  filter(!duplicated(.))

EBFM <- chk %>%
  filter(Type=="EBFM")
SS <- chk %>%
  filter(Type=="SS")
colnames(SS) <- paste(colnames(SS),"SS",sep="_") # adding suffix to column names 

chk2<-cbind(EBFM,SS) %>%
  mutate(FrontierDiff= OptimizedStDev_SS-OptimizedStDev)

ggplot(chk2, aes(x=Iteration, y=FrontierDiff)) +
  geom_point() + geom_line () +
  labs(x="Year", y="Risk gap between Frontiers Per Dollar")

#STATIC CLUSTER PLOT for Jason----
# Need to calculate the variance using the covariance matrix from the terminal year which means running the function line by line using YEAR=2021
COVAR_MAT = as.matrix(c$COVARIANCE)

StaticPlot<-NULL
ITERATION<-unique(ImpResults$Iteration)

for (j in ITERATION) {
  
  Link<-ImpResults %>%
    filter(Iteration==j)
  
  boo<-matrix(t(Link$TImpWeight)%*%COVAR_MAT%*%Link$TImpWeight)
  
  TARGET<-Portfolio %>%
    filter(Iteration==j) %>%
    dplyr::select(TARGET)
  
  boo <- cbind(boo, j, TARGET)
  colnames(boo) <- c('ActualP_T', "Iteration", "TARGET")
  
  StaticPlot<-rbind(StaticPlot, boo)
  
}

ggplot() +
  geom_line(data =FrontierResults %>%
              filter(Iteration == max(Iteration)), aes(x= OptimizedStDev, y=OptimizedRevenue, color=Type),lwd=1) +
  geom_point(data=StaticPlot %>% filter(!Iteration==min(Iteration)), aes(x= sqrt(ActualP_T), y= TARGET), color="red") + 
  geom_text(data=StaticPlot %>% filter(!Iteration==min(Iteration)), aes(x= sqrt(ActualP_T), y= TARGET, label=Iteration), hjust=1.2) +
  # geom_text(data=Portfolio, aes(x= sqrt(ActualP_T), y= round(TARGET, digits = 4), label=round(sqrt(ActualP_T), digits = 4)), hjust=0, vjust=1) +
  labs(x="Risk (SD of Revenue)", y="Revenue (Hundred Million $)")

## Jason 5-year averages of realized revenue ----
# summarises by n rows starting with the final year of the timeseries (so oldest value might be less than n years if not exactly divisible)
JL<-StaticPlot %>% 
  mutate(grp = 1+ (row_number()-1) %/% 5) %>% # divide by number of years you want to average by
  group_by(grp) %>% 
  summarise(across(everything(), mean, na.rm = TRUE))

# JL<-StaticPlot %>% 
#   filter(!Iteration==2003) %>%
#   mutate(grp = 1+ (row_number()-1) %/% 5) %>% # divide by number of years you want to average by
#   group_by(grp) %>% 
#   summarise(across(everything(), mean, na.rm = TRUE))

print(JL)

ggplot() +
  geom_line(data =FrontierResults %>%
              filter(Iteration == max(Iteration)), aes(x= OptimizedStDev, y=OptimizedRevenue, color=Type),lwd=1) +
  geom_point(data=JL, aes(x= sqrt(ActualP_T), y= TARGET), color="red") + 
  geom_text(data=JL, aes(x= sqrt(ActualP_T), y= TARGET, label=Iteration), hjust=1.2) +
  # geom_text(data=Portfolio, aes(x= sqrt(ActualP_T), y= round(TARGET, digits = 4), label=round(sqrt(ActualP_T), digits = 4)), hjust=0, vjust=1) +
  labs(x="Risk (SD of Revenue)", y="Revenue (Hundred Million $)") +
  ggtitle("Five year averages")

#EBFM frontier period comparison ----
#run the function with the YEARS splits you want and save the output of each split here
FrontierResults_Period1<-FrontierResults
FrontierResults_Period2<-FrontierResults

# library(scales)
# show_col(hue_pal()(2))

ggplot() +
  geom_line(data =FrontierResults_Period1 %>%
              filter(Type=="EBFM" & Iteration == max(Iteration)), aes(x= OptimizedStDev, y=OptimizedRevenue),color="#F8766D", lwd=1, linetype = "twodash") +
  geom_line(data =FrontierResults_Period2 %>%
              filter(Type=="EBFM" & Iteration == max(Iteration)), aes(x= OptimizedStDev, y=OptimizedRevenue),color="#F8766D",lwd=1) +
  labs(x="Risk (SD of Revenue)", y="Revenue (Hundred Million $)")




         