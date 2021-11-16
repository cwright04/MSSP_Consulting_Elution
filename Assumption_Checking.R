source("DataProcessing.R", echo=FALSE)


library("ggpubr")
library("dplyr")

col=rainbow(length(levels(factor(Elution_wide$Total_Sperm))))[factor(Elution_wide$Total_Sperm)]

elution_boxplot<- Elution_wide %>% ggboxplot(x="group", y="log_ratio", add="jitter", 
                     add.params=list(shape=21, color = "black", fill=col, size=3))

elution_boxplot2 <- Elution_wide %>% ggplot(mapping = aes(x = group, y = log_ratio, group = group)) + 
    geom_boxplot() +
  geom_point(position = "jitter",mapping = aes(size = Total_Sperm, color=Total_Sperm)) + 
  ylab("Log Ratio of Elution 1 to Elution 2") + xlab("Group") + 
  ggtitle("Boxplot of Log Ratios of Elution Counts by Group") +
  theme(plot.title = element_text(size = (10)), 
       legend.title = element_text(size =(8)), 
       legend.text = element_text(size =(8)), 
       axis.title = element_text(size = (8)),
       axis.text = element_text(size = (8)))
##################################################CHECK FOR NORMALITY######################################################
    require(gridExtra)

  #Density Plot
    Density_Plots <- ggplot()+ geom_density(Elution_wide, mapping = aes(x=log_ratio), alpha=0.25, color = "blue", fill = "blue") +
      ylab("Density") +xlab("") + ggtitle("Density plot of Log Ratio of Elution 1 Count to Elution 2 Count") +
      theme(plot.title = element_text(size = (10)), 
            legend.title = element_text(size =(8)), 
            legend.text = element_text(size =(8)), 
            axis.title = element_text(size = (8)),
            axis.text = element_text(size = (8))) +
      facet_wrap(~ Elution_wide$group, scale = "free_y", ncol = 2) 


  #QQPlot
    QQ_Plots <- ggqqplot(Elution_wide$log_ratio,main = "QQ plot of Log Ratio of Elution 1 Count to Elution 2 Count") +
      theme(plot.title = element_text(size = (10)), 
            legend.title = element_text(size =(8)), 
            legend.text = element_text(size =(8)), 
            axis.title = element_text(size = (8)),
            axis.text = element_text(size = (8))) +
      facet_wrap(~ Elution_wide$group, scale = "free_y", ncol = 2) 


  #Shapiro Test--looking for a pvalue > .05
    Norm_Test_1 <- shapiro.test(Elution_wide1$log_ratio)
    Norm_Test_2 <- shapiro.test(Elution_wide2$log_ratio)
    Norm_Test_3 <- shapiro.test(Elution_wide3$log_ratio)
    Norm_Test_4 <- shapiro.test(Elution_wide4$log_ratio)
    
    P_Value1 <- Norm_Test_1$p.value
    P_Value2 <- Norm_Test_2$p.value
    P_Value3 <- Norm_Test_3$p.value
    P_Value4 <- Norm_Test_4$p.value
    
    Statistic1 <- Norm_Test_1$statistic
    Statistic2 <- Norm_Test_2$statistic
    Statistic3 <- Norm_Test_3$statistic
    Statistic4 <- Norm_Test_4$statistic
    
    g1 <- data.frame(cbind(Statistic1,P_Value1)) %>% mutate(Group = "Group 1") %>% rename(Statistic = Statistic1, P_Value = P_Value1)
    g2 <- data.frame(cbind(Statistic2,P_Value2)) %>% mutate(Group = "Group 2") %>% rename(Statistic = Statistic2, P_Value = P_Value2)
    g3 <- data.frame(cbind(Statistic3,P_Value3)) %>% mutate(Group = "Group 3") %>% rename(Statistic = Statistic3, P_Value = P_Value3)
    g4 <- data.frame(cbind(Statistic4,P_Value4)) %>% mutate(Group = "Group 4") %>% rename(Statistic = Statistic4, P_Value = P_Value4)
    
    Shap_Test <- rbind(g1,g2,g3,g4)
    
    # Shap_Test_P_Value <- data.frame(cbind(P_Value1,P_Value2,P_Value3,P_Value4))
    # Shap_Test_P_Value <- Shap_Test_P_Value %>% mutate(alpha = .05)
    # names(Shap_Test_P_Value) <- c("Group1 P-Value","Group2 P-Value","Group3 P-Value","Group4 P-Value","Alpha Level")
    
    
Density_Plots
QQ_Plots
elution_boxplot
elution_boxplot2
    