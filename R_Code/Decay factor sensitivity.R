#Run the function manually changing Lambda and save frontier results
#Gamma set to 1
Lambda1<-FrontierResults
Lambda0.741<-FrontierResults
Lambda0.549<-FrontierResults


Lambda1$Lambda<-c("1")
Lambda0.741$Lambda<-c("0.741")
Lambda0.549$Lambda<-c("0.549")

LambdaSensitivity<-rbind(Lambda1, Lambda0.741, Lambda0.549) %>%
  filter(Iteration=="2021")

rescl<-SCALING_F/OM

ggplot() +
  geom_line(dat = LambdaSensitivity %>%
              filter(Type=="EBFM" & !Lambda == "1"),
            aes(x= OptimizedStDev*rescl, y=OptimizedRevenue*rescl, color=Lambda)) +
  geom_line(dat = LambdaSensitivity %>%
              filter(Type=="EBFM" & Lambda == "1"),
            aes(x= OptimizedStDev*rescl, y=OptimizedRevenue*rescl), lwd=1.5, color="black", linetype="dotted") +
  labs(x="Risk (SD of Revenue)", y="Revenue (Hundred Million $)", color='Decay Factor') +
  theme(legend.title = element_text(size=14))
# ggsave(path = "Manuscript Plots", filename = "DecayFactor.png", width = 15, height = 10, dpi = 600, bg='transparent')

Decay<-ggplot(dat = LambdaSensitivity %>%
              filter(Type=="EBFM"),
            aes(x= OptimizedStDev*rescl, y=OptimizedRevenue*rescl, group=Lambda)) +
  geom_line(aes(linetype=Lambda, colour=Lambda), lwd=1.5) +
  scale_linetype_manual(values=c("solid", "solid", "longdash"))+
  scale_color_manual(values=c('#E69F00',"#56B4E9","black"))+
  labs(x="Risk (SD of Revenue)", y="Revenue (Hundred Million $)", color='Decay Factor', linetype="Decay Factor") +
  theme(legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank())
 # ggsave(path = "Manuscript Plots", filename = "DecayFactor.png", width = 15, height = 10, dpi = 600, bg='transparent')

 