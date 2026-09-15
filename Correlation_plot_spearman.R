library(tidyselect)
library(purrr)
library(ggpubr)
library(readxl)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(Publish)
library(car)
library(multcomp)
library(PMCMRplus)
library(RColorBrewer)
library(corrplot)
library(dplyr)
library(ggcorrplot)


#Load file

correlation_sc <- read.csv("LEMAR/Spider-crabs_PST.csv", header = TRUE)

#Modify to have sample, total toxicity, nb of mussels consumed, sex, and weight for the exposure phase

correlation_DG<- correlation_sc[3:10, c("X", "X.21", "X.22", "X.23", "X.24")]
colnames(correlation_DG) <- c("sample", "total_tox", "weight", "sex", "nb_mussel")
correlation_DG

correlation_DG$total_tox <- as.numeric(correlation_DG$total_tox)
correlation_DG$weight <- as.numeric(correlation_DG$weight)
correlation_DG$nb_mussel <- as.numeric(correlation_DG$nb_mussel)

#Normality

shapiro.test(correlation_DG$total_tox)
shapiro.test(correlation_DG$weight)
shapiro.test(correlation_DG$nb_mussel)

#Spearman - if p-value < 0.05; the two variables are significantly correlated with a correlation coefficient of ... and a p-value of ... 

res_spearman <-cor.test(correlation_DG$total_tox, correlation_DG$weight,  method = "spearman")
res_spearman

#scatterplot

scatterplot_weight<-ggscatter(correlation_DG, x = "weight", y = "total_tox", 
add = "reg.line", conf.int = FALSE, 
cor.coef = TRUE, cor.method = "spearman",
cor.coeff.args = list(cor.coef.name = "rho"),
xlab = "Weight (g)")

scatterplot_weight<-scatterplot_weight +
labs(y = expression(atop("PST concentration",paste("(", mu, "g STX di-HCl eq. kg"^{-1}," spider crab digestive gland)"))))

scatterplot_weight

ggsave("scatterplot_weight2.tiff",dpi=300)

#Spearman - if p-value < 0.05; the two variables are significantly correlated with a correlation coefficient of ... and a p-value of ... 

res_spearman <-cor.test(correlation_DG$total_tox, correlation_DG$nb_mussel,  method = "spearman")
res_spearman

#scatterplot

scatterplot_nb_mussel<-ggscatter(correlation_DG, x = "nb_mussel", y = "total_tox", 
                              add = "reg.line", conf.int = FALSE, 
                              cor.coef = TRUE, cor.method = "spearman",
                              cor.coeff.args = list(cor.coef.name = "rho"),
                              xlab = "Number of contaminated mussels consumed")

scatterplot_nb_mussel<-scatterplot_nb_mussel +
  labs(y = expression(atop("PST concentration",paste("(", mu, "g STX di-HCl eq. kg"^{-1}," spider crab digestive gland)"))))

scatterplot_nb_mussel

ggsave("scatterplot_nb_mussel2.tiff",dpi=300)

