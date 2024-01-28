
# To illustrate simple right circular cone
cone <- function(x, y){
  sqrt(x ^ 2 + y ^ 2)
}

# prepare variables.
x <- y <- seq(-1, 1, length = 30)
z <- outer(x, y, cone)

# plot the 3D surface
persp(x, y, z)

##############
# Generate data for the plot
x <- seq(-5, 5, length.out = 50)
y <- seq(-5, 5, length.out = 50)
z <- outer(x, y, function(x, y) sin(sqrt(x^2 + y^2))/sqrt(x^2 + y^2))

# Create the plot
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")


susP<-seq(0.5:1, by=0.1)
Lambda<-c(0.549, 0.741, 1)

library(tidyverse)
df <- expand.grid(susP,Lambda) %>%
  rename(susP=Var1,
         Lambda=Var2) %>%
  mutate(target=seq(0.1:1, by=0.1))

P_LAMBDA <- 1 #each period receives equal weight
P_GAMMA <- 1 #sustainability parameter

G1_L1<-FrontierResults
G1_L0.741<-FrontierResults
G1_L0.549<-FrontierResults

G1_L1$Lambda<-c("1")
G1_L0.741$Lambda<-c("0.741")
G1_L0.549$Lambda<-c("0.549")

LambdaSensitivity<-rbind(G1_L1, G1_L0.741, G1_L0.549) %>%
  filter(Iteration=="2021")

persp(LambdaSensitivity$OptimizedStDev*rescl, LambdaSensitivity$OptimizedRevenue*rescl, LambdaSensitivity$Lambda, theta = 30, phi = 30, expand = 0.5, col = "lightblue")



ggplot() +
  geom_line(dat = LambdaSensitivity %>%
              filter(Type=="EBFM" & !Lambda == "1"),
            aes(x= OptimizedStDev*rescl, y=OptimizedRevenue*rescl, color=Lambda)) +
  geom_line(dat = LambdaSensitivity %>%
              filter(Type=="EBFM" & Lambda == "1"),
            aes(x= OptimizedStDev*rescl, y=OptimizedRevenue*rescl), lwd=1.5, color="black", linetype="dotted") +
  labs(x="Risk (SD of Revenue)", y="Revenue (Hundred Million $)", color='Decay Factor') +
  theme(legend.title = element_text(size=14))