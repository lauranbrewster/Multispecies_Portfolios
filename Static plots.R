# Static plot time t covariance matrix (new way of doing it which i think is more accurate)
timet<-ggplot() +
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
setwd("~/Portfolio project/Methods paper/Methods Paper Analysis")
# ggsave(path = "Manuscript Plots", filename = "Static plot time t covariance matrix.png", width = 15, height = 10, dpi = 600)

# Static plot terminal year covariance matrix (original way I was doing it which is how I think Sanchirico did it)
terminalyear<-ggplot() +
  geom_line(data =FrontierResults %>%
              filter(Iteration == max(Iteration)), aes(x= OptimizedStDev*rescl, y=OptimizedRevenue*rescl, color=Type),lwd=1) +
  geom_point(data=StaticPlot %>% filter(!Iteration==min(Iteration)), aes(x= sqrt(ActualP_T)*rescl, y= TARGET*rescl), color="red") +
  geom_text(data=StaticPlot %>% filter(!Iteration==min(Iteration)), aes(x= sqrt(ActualP_T)*rescl, y= TARGET*rescl, label=Iteration), hjust=1.2) +
  # geom_text(data=Portfolio, aes(x= sqrt(ActualP_T), y= round(TARGET, digits = 4), label=round(sqrt(ActualP_T), digits = 4)), hjust=0, vjust=1) +
  labs(x="Risk (SD of Revenue)", y="Revenue (Hundred Million $)", color='Frontier Type', size=15) +
  theme(legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.title.y=element_blank())
setwd("~/Portfolio project/Methods paper/Methods Paper Analysis")
# ggsave(path = "Manuscript Plots", filename = "Static plot terminal year covariance matrix.png", width = 15, height = 10, dpi = 600)

library(ggpubr)
statics<-ggarrange(timet, terminalyear, common.legend=TRUE, legend="top", nrow=2,
          labels = c("a)", "b)"), font.label = list(size = 20))
annotate_figure(statics,
                left = text_grob("Revenue (Hundred Million $)", rot = 90, size = 25))
setwd("~/Portfolio project/Methods paper/Methods Paper Analysis")
# ggsave(path = "Manuscript Plots", filename = "Static plots.png", width = 15, height = 20, dpi = 600)



#########################################################
# Static plots split into three time periods: (covariance matrix time t)
# 1. 1950-1973
# 2. 1974-1997
# 3. 1998-2021
#when you run it you need to have an extra year at the front that you can drop for the plot otherwise the first year shows zero risk. 
# So actually run:
# 1. 1950-1973
# 2. 1973-1997
# 3. 1997-2021

chunk1F<-FrontierResults%>%
  mutate(TimeChunk="1950–1973") %>%
  filter(Iteration == max(Iteration))
chunk1P<-Portfolio %>%
  mutate(TimeChunk="1950–1973") %>%
  filter(!Iteration==min(Iteration))


chunk2F<-FrontierResults%>%
  mutate(TimeChunk="1974–1997") %>%
  filter(Iteration == max(Iteration)) 
chunk2P<-Portfolio %>%
  mutate(TimeChunk="1974–1997") %>%
  filter(!Iteration==min(Iteration))


chunk3F<-FrontierResults%>%
  mutate(TimeChunk="1998–2001") %>%
  filter(Iteration == max(Iteration))
chunk3P<-Portfolio %>%
  mutate(TimeChunk="1998–2001") %>%
  filter(!Iteration==min(Iteration))
  
FrontierChunks<-rbind(chunk1F, chunk2F, chunk3F)
PortfolioChunks<-rbind(chunk1P, chunk2P, chunk3P)
str(PortfolioChunks)

ggplot() +
  geom_line(data =FrontierChunks, aes(x= OptimizedStDev*rescl, y=OptimizedRevenue*rescl, color=Type),lwd=1) +
  geom_point(data =PortfolioChunks, aes(x = sqrt(ActualP_T)*rescl, y = TARGET*rescl),color="red")+
  geom_text(data=PortfolioChunks, aes(x= sqrt(ActualP_T)*rescl, y= TARGET*rescl, label=Iteration), hjust=1.2) +
  labs(y ="Revenue (Hundred Million $)", x = "Risk (SD of Revenue)", color='Frontier Type', size=10) +
  theme(legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        strip.text.x = element_text(size = 15, colour = "black")) +
  facet_wrap(~as.factor(TimeChunk), nrow=3)
setwd("~/Portfolio project/Methods paper/Methods Paper Analysis")
# ggsave(path = "Manuscript Plots", filename = "Static plots timechunks time t covariance.png", width = 15, height = 20, dpi = 600)

#########################################################
# Static plots split into three time periods: (covariance matrix terminal year)
# 1. 1950-1973
# 2. 1974-1997
# 3. 1998-2021
#when you run it you need to have an extra year at the front that you can drop for the plot otherwise the first year shows zero risk. 
# So actually run:
# 1. 1950-1973
# 2. 1973-1997
# 3. 1997-2021

chunk1F<-FrontierResults%>%
  mutate(TimeChunk="1950–1973") %>%
  filter(Iteration == max(Iteration))
chunk1S<-StaticPlot %>%
  mutate(TimeChunk="1950–1973") %>%
  filter(!Iteration==min(Iteration))


chunk2F<-FrontierResults%>%
  mutate(TimeChunk="1974–1997") %>%
  filter(Iteration == max(Iteration)) 
chunk2S<-StaticPlot %>%
  mutate(TimeChunk="1974–1997") %>%
  filter(!Iteration==min(Iteration))


chunk3F<-FrontierResults%>%
  mutate(TimeChunk="1998–2001") %>%
  filter(Iteration == max(Iteration))
chunk3S<-StaticPlot %>%
  mutate(TimeChunk="1998–2001") %>%
  filter(!Iteration==min(Iteration))

FrontierChunks<-rbind(chunk1F, chunk2F, chunk3F)
StaticChunks<-rbind(chunk1S, chunk2S, chunk3S)
str(StaticChunks)
 
ggplot() +
  geom_line(data =FrontierChunks, aes(x= OptimizedStDev*rescl, y=OptimizedRevenue*rescl, color=Type),lwd=1) +
  geom_point(data=StaticChunks %>% filter(!Iteration==min(Iteration)), aes(x= sqrt(ActualP_T)*rescl, y= TARGET*rescl), color="red") +
  geom_text(data=StaticChunks %>% filter(!Iteration==min(Iteration)), aes(x= sqrt(ActualP_T)*rescl, y= TARGET*rescl, label=Iteration), hjust=1.2) +
  # geom_text(data=Portfolio, aes(x= sqrt(ActualP_T), y= round(TARGET, digits = 4), label=round(sqrt(ActualP_T), digits = 4)), hjust=0, vjust=1) +
  labs(x="Risk (SD of Revenue)", y="Revenue (Hundred Million $)", color='Frontier Type', size=15) +
  theme(legend.title = element_text(size=20),
        legend.text = element_text(size=20),      
        strip.text.x = element_text(size = 15, colour = "black")) +
  facet_wrap(~as.factor(TimeChunk), nrow=3)
setwd("~/Portfolio project/Methods paper/Methods Paper Analysis")
# ggsave(path = "Manuscript Plots", filename = "Static plot time chunks terminal year covariance matrix.png", width = 15, height = 10, dpi = 600)
