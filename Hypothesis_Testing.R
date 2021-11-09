source("DataProcessing.R", echo=FALSE)


Group_level_proportions1 <- Elution_clean %>% group_by(group,Elution) %>% summarise(count =sum(Sperm.Count))

Elution1 <- subset(Group_level_proportions1, Elution == 1) %>% rename(Elution1 = count) %>% select(c("group", "Elution1"))
Elution2 <- subset(Group_level_proportions1, Elution == 2) %>% rename(Elution2 = count) %>% select(c("group", "Elution2"))


Group_level_proportions <- inner_join(Elution1, Elution2, by= "group") %>% mutate(proportion1 = Elution1/sum(Elution1,Elution2), 
                                                                                  proportion2 = Elution2/sum(Elution1,Elution2), 
                                                                                  total = sum(Elution1,Elution2))

#Since we are assuming that group 1 is smaller than groups 2 we will want to do a  one sided test(Null: p1 < p2)

prop.test(x = c(Group_level_proportions[1,3], Group_level_proportions[2,3]), n = c(Group_level_proportions[1,6], Group_level_proportions[2,6]))



#Bootstrap testing
Group1_1 <- subset(Elution_clean, group ==1 & Elution ==1)
Group1_2 <- subset(Elution_clean, group ==1 & Elution ==2 )

Group1 <- left_join(Group1_1[,c("group","Replicate","Sperm.Count")],Group1_2[,c("group","Replicate","Sperm.Count")], by=c("group", "Replicate"))
names(Group1)[3] <-"Elution1"
names(Group1)[4] <-"Elution2"

GROUP1_SAMPLE <- Group1[sample(nrow(Group1), 5, replace = TRUE),]


