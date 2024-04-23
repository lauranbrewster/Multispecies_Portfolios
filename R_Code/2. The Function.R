# RUN THE FUNCTION ----
# Use this info (YEAR through N) if you need to go line-by-line to look at how the code is functioning
# or are splitting into time stanzas
# YEAR=1975
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
    dplyr::rename(IYear=Var1,
                  Taxonkey=Var2)
  
  d_2 <- merge(d_2,ALL_COMB, all=TRUE,by=c('IYear','Taxonkey')) %>%
    dplyr::mutate(Value = replace_na(Value, 0),
                  Catch = replace_na(Catch, 0),
                  Price = replace_na(Value/Catch,0))
  
  # Set Lambda & Gamma ----
  P_LAMBDA <- 1 #each period receives equal weight
  # P_LAMBDA <- 0.549 #decay factor. 5% left after 5 years. If this is 1, each year is given equal weight.
  # P_LAMBDA <- 0.741 # 5% left after 10 years.
  P_GAMMA <- 1 #sustainability parameter
  S_I <- 0.5
  z = length(unique(d_2$Taxonkey))
  
  #Adding the decay factor into the covariance matrix (Eqn. 2)
  d_2 <- d_2 %>%
    mutate(WCatch = P_LAMBDA^(YEAR-IYear+1)*Catch, 
           WAvgVal = WCatch*Price, # Eqn. 5. Value used to estimate omega. Omega= weighted average of catches over time with decay
           Lambda = P_LAMBDA^(YEAR-IYear+1),  #this is the decay value applied to each year.
           WPrice = Lambda*Price,
           WVal = Lambda*Value)
  
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
    mutate(MaxCatch = Catch*P_GAMMA) %>% # Numerator of Eqn. 4 in Jin et al. i.e., max sustainable catch for each species at time t * sustainability parameter
    select(Taxonkey,MaxCatch)
  
  WAVG_CATCH <- merge(d_3,WAVG_CATCH,by='Taxonkey') 
  
  WAVG_CATCH <- merge(MAX_CATCH,WAVG_CATCH,by='Taxonkey')  %>%
    mutate(MaxWeight = MaxCatch/Omega, #Eqn. 4 of Jin. et al. maximum weight = maximum biomass/average biomass) 
           Year = YEAR,
           TImpweight = tValue/WVal, #implicit weight from actual return.
           WRatio = TImpweight/MaxWeight, #ratio of implicit to max weight
           Chk = pmin(TImpweight,MaxWeight),
           CReturn = pmin(TImpweight,MaxWeight)*WVal)
  DIFF_RETURNS <-cbind(sum(WAVG_CATCH$CReturn[which(WAVG_CATCH$Year==YEAR)]), sum(WAVG_CATCH$Value[which(WAVG_CATCH$Year==YEAR)]))
  
  #Var-Covar. Estimating weighted Variance-Covariance matrix ----
  VAR_DATA <- merge(d_2,WAVG_CATCH, by='Taxonkey') %>%
    select('Taxonkey','IYear','Lambda.x','Value.x','WVal.y') %>%
    dplyr::rename(Year=IYear,
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
  
  # Targets ----
  # Create the targets being fed through the optimizer
    TOT_REVENUE <- aggregate(cbind(Value)~IYear, data=d_2, FUN='sum')
  HALF_MAX_REV <- max(TOT_REVENUE$Value)*0.5 #used in optimal weights figure. The revenue weight share = weight (wi,t)/maximum weight (Wi,t) for each species group i, calculated at expected return = half of the maximum return.
  
  TARGET_RETURNS <- quantile(TOT_REVENUE$Value, seq(.00,1,by=.05))
  filler<-seq(0.01, c(min(TARGET_RETURNS)), by = 0.1)
  
  LastYearRevenue <- TOT_REVENUE %>%
    filter(IYear == YEAR) 
  LastYearRevenue <- LastYearRevenue[,2]
  
  TARGET_RETURNS<-as.numeric(c(filler, HALF_MAX_REV, TARGET_RETURNS, LastYearRevenue))
  
  MAX_WEIGHTS <- matrix(WAVG_CATCH$MaxWeight,nrow=z)
  MIN_WEIGHTS <- matrix(0.00,nrow=z)
  
  OPTIMAL_WEIGHTS <- NULL
  OPTIMAL_REVENUE <- NULL
  OPTIMAL_LANDINGS <- NULL
  OPTIMAL_VAR <- NULL
  

  # Optimiser----
  # TARGET=HALF_MAX_REV if you want to run a line with a single target
  for (TARGET in TARGET_RETURNS)  tryCatch({
    {
      if(EBFM == TRUE){
        SOLUTION <- tryCatch(ipop(-MEAN_RETURNS,VAR_COVAR,t(-MEAN_RETURNS),-TARGET,
                                  MIN_WEIGHTS, MAX_WEIGHTS, TARGET, margin=10^-6, sig=7,
                                  verb=FALSE)) #verb=TRUE gives more info for computational singularity issue
      } else {
        SOLUTION <- tryCatch(ipop(-MEAN_RETURNS,VAR_COVAR_SS,t(-MEAN_RETURNS),-TARGET,
                                  MIN_WEIGHTS, MAX_WEIGHTS, TARGET, margin=10^-6, sig=7,
                                  verb=FALSE))
      }
      
    }
    
    #Pulling the optimum weights from the solution
    TEMP_WEIGHTS <- matrix(SOLUTION@primal) 
    colnames(TEMP_WEIGHTS) <- paste("Optimal_weights")
    
    #Calculating optimal revenue, by stock complex
    TEMP_REVENUE <- MEAN_RETURNS*TEMP_WEIGHTS
    colnames(TEMP_REVENUE) <- paste("Optimized_Revenue_",TARGET)
    
    #Calculate optimal landings, by stock complex
    TEMP_LANDINGS <- WAVG_CATCH$WCatch*TEMP_WEIGHTS
    colnames(TEMP_LANDINGS) <- paste("Optimized_Landings_",TARGET)
    
    #Calculate the realized variance for each optimization solution
    ###!!!! use full COV matirx here
    TEMP_VAR<-t(TEMP_WEIGHTS)%*%VAR_COVAR%*%TEMP_WEIGHTS
    rownames(TEMP_VAR)<- paste("Optimized_Variance_",TARGET)
    TEMP_VAR<-cbind(TEMP_VAR, TARGET)
    
    # Accumulating the results for each optimization solution
    TEMP_WEIGHTS <- as.data.frame(cbind(TEMP_WEIGHTS, TARGET, VAR_names, WAVG_CATCH$MaxWeight))
    colnames(TEMP_WEIGHTS) <- c('Optimal_weights', "TARGET", "Taxonkey", "MaxWeight")
    OPTIMAL_WEIGHTS <- rbind(TEMP_WEIGHTS, OPTIMAL_WEIGHTS) #by taxonkey
    OPTIMAL_REVENUE <- cbind(OPTIMAL_REVENUE,TEMP_REVENUE) #by taxonkey
    OPTIMAL_LANDINGS <- cbind(OPTIMAL_LANDINGS, TEMP_LANDINGS) #by taxonkey
    OPTIMAL_VAR <- rbind(OPTIMAL_VAR,TEMP_VAR) #one value, variance for the associated target value

    print(TARGET) # check on progress of quadratic programming optimizer
  },error = function(e){})
  
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
  
  ############### List of data frames ############
  optimums <- list ("MEAN_VAR"=MEAN_VAR,"TAXON_OPTIMAL_WEIGHTS" =OPTIMAL_WEIGHTS, "COVARIANCE" =VAR_COVAR, 
                    "T_IMPLICIT_WEIGHTS" = T_IMPLICIT_WEIGHTS, 
                    "ACTUAL_PORTFOLIO_T"= AP_T,"FRONTIER_PORTFOLIO"=FP,
                    "HALF_MAX_REV" = HALF_MAX_REV,
                    "OPTIMAL_REVENUE" = OPTIMAL_REVENUE, "OPTIMAL_LANDINGS" = OPTIMAL_LANDINGS)
  return(optimums)
}

