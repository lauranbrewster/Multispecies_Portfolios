#Run the function manually changing gamma and save frontier results
#Lambda set to 1
Gamma1<-FrontierResults
Gamma0.75<-FrontierResults
Gamma0.5<-FrontierResults


Gamma1$Gamma<-c("1")
Gamma0.75$Gamma<-c("0.75")
Gamma0.5$Gamma<-c("0.5")

GammaSensitivity<-rbind(Gamma1, Gamma0.75, Gamma0.5) %>%
  filter(Iteration=="2021")

rescl<-SCALING_F/OM

Sust<-ggplot(dat = GammaSensitivity %>%
         filter(Type=="EBFM"),
       aes(x= OptimizedStDev*rescl, y=OptimizedRevenue*rescl, group=Gamma)) +
  geom_line(aes(linetype=Gamma, colour=Gamma), lwd=1.5) +
  scale_linetype_manual(values=c("solid", "solid", "longdash"))+
  scale_color_manual(values=c('#D55E00',"#009E73","black")) +
  scale_x_continuous(breaks=seq(0,3,0.5)) +
  # geom_line(dat = GammaSensitivity %>%
  #             filter(Type=="EBFM" & Gamma == "1"),
  #           aes(x= OptimizedStDev*rescl, y=OptimizedRevenue*rescl), lwd=1.5, color="black", linetype="dotted") +
  labs(x="Risk (SD of Revenue)", y="Revenue (Hundred Million $)", color='Sustainability Parameter',linetype="Sustainability Parameter") +
  theme(legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.title.x=element_blank())
 # ggsave(path = "Manuscript Plots", filename = "Sustainability_v2.png", width = 15, height = 10, dpi = 600, bg='transparent')



library(ggpubr)
figure<-ggarrange(Sust, Decay, common.legend=FALSE,
          labels = c("a)", "b)"), font.label = list(size = 20))

annotate_figure(figure,
                bottom = text_grob("Risk (SD of Revenue)", hjust = 2.5, x = 1, size = 25))

 ggsave(path = "Manuscript Plots", filename = "SustDecay.png", width = 15, height = 10, dpi = 600, bg='transparent')
