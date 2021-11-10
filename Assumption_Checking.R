source("DataProcessing.R", echo=FALSE)

#################################Reorganize data -- (could and should be put into a function)############################
    Group1_1 <- subset(Elution_clean, group ==1 & Elution ==1)
    Group1_2 <- subset(Elution_clean, group ==1 & Elution ==2 )
    
    Group1 <- left_join(Group1_1[,c("group","Replicate","Sperm.Count")],Group1_2[,c("group","Replicate","Sperm.Count")], by=c("group", "Replicate"))
    names(Group1)[3] <-"Elution1"
    names(Group1)[4] <-"Elution2"
    
    
    Group2_1 <- subset(Elution_clean, group ==2 & Elution ==1)
    Group2_2 <- subset(Elution_clean, group ==2 & Elution ==2 )
    
    Group2 <- left_join(Group2_1[,c("group","Replicate","Sperm.Count")],Group2_2[,c("group","Replicate","Sperm.Count")], by=c("group", "Replicate"))
    names(Group2)[3] <-"Elution1"
    names(Group2)[4] <-"Elution2"
    
    Group3_1 <- subset(Elution_clean, group ==3 & Elution ==1)
    Group3_2 <- subset(Elution_clean, group ==3 & Elution ==2 )
    
    Group3 <- left_join(Group3_1[,c("group","Replicate","Sperm.Count")],Group3_2[,c("group","Replicate","Sperm.Count")], by=c("group", "Replicate"))
    names(Group3)[3] <-"Elution1"
    names(Group3)[4] <-"Elution2"
    
    Group4_1 <- subset(Elution_clean, group ==4 & Elution ==1)
    Group4_2 <- subset(Elution_clean, group ==4 & Elution ==2 )
    
    
    Group4 <- left_join(Group4_1[,c("group","Replicate","Sperm.Count")],Group4_2[,c("group","Replicate","Sperm.Count")], by=c("group", "Replicate"))
    names(Group4)[3] <-"Elution1"
    names(Group4)[4] <-"Elution2"

##################################################CHECK FOR NORMALITY######################################################
    library("ggpubr")
    require(gridExtra)

  #Density Plot
    g1_den <- ggdensity(Group1$Elution1,main = "Density plot of Sperm Count Elution 1 - Group 1")
    g2_den <- ggdensity(Group2$Elution1,main = "Density plot of Sperm Count Elution 1 - Group 2")
    g3_den <- ggdensity(Group3$Elution1,main = "Density plot of Sperm Count Elution 1 - Group 3")
    g4_den <- ggdensity(Group4$Elution1,main = "Density plot of Sperm Count Elution 1 - Group 4")

    grid.arrange(g1_den, g2_den, g3_den, g4_den, ncol = 2)

  #QQPlot
    g1_QQ <- ggqqplot(Group1$Elution1,main = "QQ plot of Sperm Count Elution 1 - Group 1")
    g2_QQ <- ggqqplot(Group2$Elution1,main = "QQ plot of Sperm Count Elution 1 - Group 2")
    g3_QQ <- ggqqplot(Group3$Elution1,main = "QQ plot of Sperm Count Elution 1 - Group 3")
    g4_QQ <- ggqqplot(Group4$Elution1,main = "QQ plot of Sperm Count Elution 1 - Group 4")

    grid.arrange(g1_QQ, g2_QQ, g3_QQ, g4_QQ, ncol = 2)

  #Shapiro Test--looking for a pvalue > .05
    #Note: Group 1 does not appear to meet this assumption
    shapiro.test(Group1$Elution1)
    shapiro.test(Group2$Elution1)
    shapiro.test(Group3$Elution1)
    shapiro.test(Group4$Elution1)