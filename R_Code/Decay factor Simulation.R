# decaySimPlot<-LB_SUM %>%
#   mutate(AnnualRev=(SumVAL*SCALING_F),
#          Lambda0549 = 0.549^(max(IYear)-IYear+1),
#          Lambda0741 = 0.741^(max(IYear)-IYear+1),
#          DecayedRev0549=Lambda0549*AnnualRev,
#          DecayedRev0741=Lambda0741*AnnualRev,
#          YearsFromPresent=max(IYear)-IYear)

#This makes more sense to me but check with Geret
decaySimPlot2<-LB_SUM %>%
  mutate(AnnualRev=(SumVAL*SCALING_F),
         Lambda0549 = 0.549^(max(IYear)-IYear),
         Lambda0741 = 0.741^(max(IYear)-IYear),
         DecayedRev0549=Lambda0549*AnnualRev,
         DecayedRev0741=Lambda0741*AnnualRev,
         YearsFromPresent=max(IYear)-IYear)



# ggplot(decaySimPlot) +
#   geom_line(aes(x=IYear, y=AnnualOrigValue/OM, color="1"), lwd=1) +
#   geom_line(aes(x=IYear, y=WVal1/OM, color="0.741"), linetype=2, lwd=1) +
#   geom_line(aes(x=IYear, y=WVal2/OM, color="0.549"), lwd=1.5) +
#   labs(x="Year", y=expression('Revenue (Hundred Million $)')) +
#   scale_color_manual(name = "Lambda Value", values = c("1" = "black", "0.741" = "blue", "0.549" = "grey"))
#   theme(legend.position = "bottom")

  # ggplot(decaySimPlot2) +
  #   geom_line(aes(x=YearsFromPresent, y=DecayedRev0549/OM, color="0.549"), lwd=1.5) +
  #   geom_line(aes(x=YearsFromPresent, y=DecayedRev0741/OM, color="0.741"), linetype=2, lwd=1) +
  #   geom_line(aes(x=YearsFromPresent, y=AnnualRev/OM, color="1"), lwd=1) +
  #   labs(x="Years From Present", y=expression('Revenue (Hundred Million $)')) +
  #   scale_x_continuous(n.breaks=10) +
  #   scale_color_manual(name = "Lambda Value", values = c("1" = "black", "0.741" = "blue", "0.549" = "grey")) +
  #   theme(legend.text = element_text(size=14))
  # legend.title = element_text(size=14)
 # ggsave(path = "Manuscript Plots", filename = "decay simulation plot decayed first year.png", width = 15, height = 10, dpi = 600)
