source("DataProcessing.R", echo=FALSE)

########One way ANOVA#########
library(tidyverse)
library(broom)
library(AICcmodavg)
library(car)

#Conduct test and review summary
  one.way <- aov(log_ratio ~ group, data = Elution_wide)
  summary(one.way)

#Check normality assumptions
  plot(one.way)
  leveneTest(log_ratio ~ group, data = Elution_wide)
  
  # Extract the residuals
  aov_residuals <- residuals(object = one.way )
  # Run Shapiro-Wilk test
  shapiro.test(x = aov_residuals )

######Tukey–Kramer method; Basically equivalent to t-test otherwise this correct for experiment-wise error rate##########
  ##Assumptions for Tukey-Kramer method###
  #i)Observations are independent within and among groups.
  #ii)The groups for each mean in the test are normally distributed.
  #iii)There is equal within-group variance across the groups associated with each mean in the test (homogeneity of variance).
  tukey.one.way<-TukeyHSD(one.way)
  tukey.one.way

  #Secondary test to confirm orginal results - Multiple comparisons using multcomp package
  library(multcomp)
  summary(glht(one.way, linfct = mcp(group = "Tukey")))

####pairwise t test#####
ttest1 <- with(Elution_wide, t.test(log_ratio[group == 1], log_ratio[group == 2]))
with(Elution_wide, t.test(log_ratio[group == 1], log_ratio[group == 3]))
with(Elution_wide, t.test(log_ratio[group == 1], log_ratio[group == 4]))
with(Elution_wide, t.test(log_ratio[group == 2], log_ratio[group == 3]))
with(Elution_wide, t.test(log_ratio[group == 2], log_ratio[group == 4]))
with(Elution_wide, t.test(log_ratio[group == 3], log_ratio[group == 4]))




####IF NEEDED LATER ####
# GROUP1_SAMPLE <- Group1[sample(nrow(Group1), 5, replace = TRUE),]
#Bootstrap testing
#Sample 5 replicates out of Group1 1000 times(Bootstrap)
G1b<-data.frame(NA,NA)%>% setNames(c("Elution2","TotalElution"))
for(i in 1:1000){bs<- sample_n(Elution_wide1,5,replace = TRUE)[,3:4];
sub<-data.frame(sum(bs[2]),sum(bs))%>% setNames(c("Elution2","TotalElution"));
G1b <- bind_rows(G1b,sub)}

#Delete first NA row and calculate the proportion of elution 2 in each row
G1b <- G1b%>%mutate(prop = Elution2 / TotalElution)%>%slice(-1)

#Arrange the data in order of proportion and create 95% CI
G1b <- G1b[order(G1b$prop),]
G1b95CI <- G1b[c(25,975),3]



