# FRONTIER PLOTS ----
## Dynamic plot ----
rescl<-SCALING_F/OM

ggplot() +
  geom_line(data =FrontierResults %>%
              # filter(Type=='EBFM') %>% #Use this line if you want to drop the SS blue line.
              filter(!Iteration==min(Iteration)), 
            aes(x= OptimizedStDev*rescl, y=OptimizedRevenue*rescl, color=Type),lwd=1)+
  geom_point(data =Portfolio %>%
               filter(!Iteration==min(Iteration)), aes(x = sqrt(ActualP_T)*rescl, y = TARGET*rescl))+
  labs(y ="Revenue (Hundred Million $)", x = "Risk (SD of Revenue)", color='Frontier Type', size=10) +
  facet_wrap(~Iteration, scales="fixed") +
  theme(legend.title = element_text(size=14))

## Snapshot plots----
# Static plot time t covariance matrix (Supplementary Material Figure 2a)
ggplot() +
  geom_line(data =FrontierResults %>%
              # filter(Type=='EBFM') %>% #Use this line if you want to drop the SS blue line.
              filter(Iteration == max(Iteration)), aes(x= OptimizedStDev*rescl, y=OptimizedRevenue*rescl, color=Type),lwd=1) +
  geom_point(data =Portfolio %>%
               filter(!Iteration==min(Iteration)), aes(x = sqrt(ActualP_T)*rescl, y = TARGET*rescl),color="red")+
  geom_text(data=Portfolio %>% filter(!Iteration==min(Iteration)), aes(x= sqrt(ActualP_T)*rescl, y= TARGET*rescl, label=Iteration), hjust=1.2) +
  labs(y ="Revenue (Hundred Million $)", x = "Risk (SD of Revenue)", color='Frontier Type', size=10) +
  theme(legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.title.y=element_blank(),
        axis.title.x=element_blank())

# Static plot terminal year covariance matrix
# Supplementary material Figure 2b; Sanchirico
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


# RISK GAP PLOTS ----
## Risk gap (numerator) plot ----
Num<-ggplot(data=Portfolio) +
  geom_line(aes(x=Iteration, y=RiskgapNum*rescl)) +
  geom_point(aes(x=Iteration, y=RiskgapNum*rescl)) +
  xlab("Year") + ylab("Risk gap in dollars (Hundred Million $)") +
  theme(axis.title.y = element_text(color = "black", size=16),
        axis.title.x = element_blank())

## Risk Gap Per Dollar Plot ----
Doll<-ggplot(data=Portfolio) +
  geom_line(aes(x=Iteration, y=RiskgapDollar), colour="red") +
  geom_point(aes(x=Iteration, y=RiskgapDollar), colour="red") +
  xlab("Year") +
  ylab("Risk gap Per Dollar") +
  theme(axis.title.y = element_text(color = "black", size=16))

library(ggpubr)
ggarrange(Num, Doll, common.legend=TRUE, legend="right", nrow=2)

# IMPLICIT AND OPTIMAL WEIGHT PLOTS ----
# Implicit weight and Optimal revenue share weights by species
# The ratio of implicit revenue weights (representing actual harvest) to the maximum weight. Where values are above 1 it indicates that the species were harvested above the max levels.
ggplot(data=ImpResults) +
  geom_col(aes(x=Iteration, y=ImpWeightShare)) +
  facet_wrap(~NMFS.Namev2) +
  labs(y ="Implicit Revenue Weights Ratio", x = "Year") +
  geom_hline(yintercept = 1, color="red", linetype="dashed")

ggplot(data=Taxon_Opt_Weight) + 
  geom_col(aes(x=Iteration, y=RevWeightShare), fill="darkblue") + ##Here the optimal weights are calculated as 0.5*maxweight
  facet_wrap(~NMFS.Namev2) +
  labs(y ="Optimal Revenue Weight Share", x = "Year") +
  geom_hline(yintercept = 1, color="red", linetype="dashed") 

# EBFM-SS frontier difference plot ----
## Difference is calculated at actual revenue (point b) for the year on y axis ----
require("MASS") 
require("reshape2") 
require("reshape") 

link<-data %>%
  group_by(IYear, NMFS.Namev2) %>%
  aggregate(Value~IYear, sum) %>%
  dplyr::rename(Iteration=IYear,
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


