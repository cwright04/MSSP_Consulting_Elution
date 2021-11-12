source("DataProcessing.R", echo=FALSE)


Group_level_proportions1 <- Elution_clean %>% group_by(group,Elution) %>% summarise(count =sum(Sperm.Count))

Elution1 <- subset(Group_level_proportions1, Elution == 1) %>% rename(Elution1 = count) %>% select(c("group", "Elution1"))
Elution2 <- subset(Group_level_proportions1, Elution == 2) %>% rename(Elution2 = count) %>% select(c("group", "Elution2"))


Group_level_proportions <- inner_join(Elution1, Elution2, by= "group") %>% mutate(proportion1 = Elution1/sum(Elution1,Elution2), 
                                                                                  proportion2 = Elution2/sum(Elution1,Elution2), 
                                                                                  total = sum(Elution1,Elution2))

#Since we are assuming that group 1 is smaller than groups 2 we will want to do a  one sided test(Null: p1 < p2)

prop.test(x = pull(Group_level_proportions[1:2,3]), n = pull(Group_level_proportions[1:2,6]))

#Covert long format to wide format (way 1)
Group1_1 <- subset(Elution_clean, group ==1 & Elution ==1)
Group1_2 <- subset(Elution_clean, group ==1 & Elution ==2 )

Group1 <- left_join(Group1_1[,c("group","Replicate","Sperm.Count")],Group1_2[,c("group","Replicate","Sperm.Count")], by=c("group", "Replicate"))
names(Group1)[3] <-"Elution1"
names(Group1)[4] <-"Elution2"

#Covert long format to wide format (way 2)
Elution_wide <- pivot_wider(Elution_clean, names_from = Elution, values_from = c(Sperm.Count, Relative.Pct))
Elution_wide1 <- Elution_wide %>% filter(group==1)
Elution_wide2 <- Elution_wide %>% filter(group==2)
Elution_wide3 <- Elution_wide %>% filter(group==3)
Elution_wide4 <- Elution_wide %>% filter(group==4)

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


########One way ANOVA#########
library(tidyverse)
library(broom)
library(AICcmodavg)
library(car)
one.way <- aov(log_ratio ~ group, data = Elution_wide)
summary(one.way)
plot(one.way)
leveneTest(log_ratio ~ group, data = Elution_wide)

####pairwise t test#####
with(Elution_wide, t.test(log_ratio[group == 1], log_ratio[group == 2]))
with(Elution_wide, t.test(log_ratio[group == 1], log_ratio[group == 3]))
with(Elution_wide, t.test(log_ratio[group == 1], log_ratio[group == 4]))
with(Elution_wide, t.test(log_ratio[group == 2], log_ratio[group == 3]))
with(Elution_wide, t.test(log_ratio[group == 2], log_ratio[group == 4]))
with(Elution_wide, t.test(log_ratio[group == 3], log_ratio[group == 4]))
