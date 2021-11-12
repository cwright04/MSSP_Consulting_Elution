source("DataProcessing.R", echo=FALSE)


library("ggpubr")

col=rainbow(length(levels(factor(Elution_wide$Total_Sperm))))[factor(Elution_wide$Total_Sperm)]

elution_boxplot<- Elution_wide %>% ggboxplot(x="group", y="log_ratio", add="jitter", 
                     add.params=list(shape=21, color = "black", fill=col, size=3))

elution_boxplot2 <- Elution_wide %>% ggplot(mapping = aes(x = group, y = log_ratio, group = group)) + 
    geom_boxplot() +
  geom_point(position = "jitter",mapping = aes(size = Total_Sperm, color=Total_Sperm))
##################################################CHECK FOR NORMALITY######################################################
    require(gridExtra)

  #Density Plot
    g1_den <- ggdensity(Elution_wide1$log_ratio,main = "Density plot of Sperm Count Elution 1 - Group 1")
    g2_den <- ggdensity(Elution_wide2$log_ratio,main = "Density plot of Sperm Count Elution 1 - Group 2")
    g3_den <- ggdensity(Elution_wide3$log_ratio,main = "Density plot of Sperm Count Elution 1 - Group 3")
    g4_den <- ggdensity(Elution_wide4$log_ratio,main = "Density plot of Sperm Count Elution 1 - Group 4")

    dens <- grid.arrange(g1_den, g2_den, g3_den, g4_den, ncol = 2)

  #QQPlot
    g1_QQ <- ggqqplot(Elution_wide1$log_ratio,main = "QQ plot of Sperm Count Elution 1 - Group 1")
    g2_QQ <- ggqqplot(Elution_wide2$log_ratio,main = "QQ plot of Sperm Count Elution 1 - Group 2")
    g3_QQ <- ggqqplot(Elution_wide3$log_ratio,main = "QQ plot of Sperm Count Elution 1 - Group 3")
    g4_QQ <- ggqqplot(Elution_wide4$log_ratio,main = "QQ plot of Sperm Count Elution 1 - Group 4")

    QQ<- grid.arrange(g1_QQ, g2_QQ, g3_QQ, g4_QQ, ncol = 2)

  #Shapiro Test--looking for a pvalue > .05
    #Note: Group 1 does not appear to meet this assumption
    shapiro.test(Elution_wide1$log_ratio)
    shapiro.test(Elution_wide2$log_ratio)
    shapiro.test(Elution_wide3$log_ratio)
    shapiro.test(Elution_wide4$log_ratio)
    
    
    grid.arrange(dens, QQ, ncol =1)
    elution_boxplot
    elution_boxplot2
    