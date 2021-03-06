# install.packages("openxlsx")
#install.packages("pwr")
library("openxlsx")
library("tidyverse")
library("dplyr")
library(pwr)

#set path name
file <- "Part 1 - counts and relative percentages.xlsx"

#pull in each sheet(group)
Elution_Raw_S1 <- read.xlsx(file,sheet = 1, colNames = TRUE, startRow = 2)
Elution_Raw_S2 <- read.xlsx(file,sheet = 2, colNames = TRUE, startRow = 2)
Elution_Raw_S3 <- read.xlsx(file,sheet = 3, colNames = TRUE, startRow = 2)
Elution_Raw_S4 <- read.xlsx(file,sheet = 4, colNames = TRUE, startRow = 2)

#Remove unecessary columns and add a group indicator
Elution_Raw_S1<- Elution_Raw_S1 %>% dplyr::select( -ncol(Elution_Raw_S1)) %>% mutate(group = 1) 
Elution_Raw_S2<- Elution_Raw_S2 %>% dplyr::select( -ncol(Elution_Raw_S2)) %>% mutate(group = 2)
Elution_Raw_S3<- Elution_Raw_S3 %>% dplyr::select( -ncol(Elution_Raw_S3)) %>% mutate(group = 3)
Elution_Raw_S4<- Elution_Raw_S4 %>% dplyr::select( -ncol(Elution_Raw_S4)) %>% mutate(group = 4)

#Stack all groups of data
Elution_Raw <- rbind(Elution_Raw_S1,Elution_Raw_S2,Elution_Raw_S3,Elution_Raw_S4)

#Create Elution and Replicate columns and remove the total sperm per replicate columns -- we can create this on our own if needed.
Elution_clean <- Elution_Raw %>% separate(col=Sample,
                    into = c("Replicate1", "Elution"),
                    sep = "E#",
                    fill = "right") %>% separate(col=Replicate1,
                                          into = c("x", "Replicate"),
                                          sep = "#", 
                                          fill = "right") %>% dplyr::select(-1) %>% dplyr::select(-4)

#rename the relative percentage to match the naming convention of the other variables
names(Elution_clean)[4] <- "Relative.Pct"



####Create Wide Dataset######
#Covert long format to wide format (way 2)
Elution_wide <- pivot_wider(Elution_clean, names_from = Elution, values_from = c(Sperm.Count, Relative.Pct))
Elution_wide <- mutate(Elution_wide, group=as.factor(group))


##Create log(elution1_porportion/elution2_proportion)
Elution_wide$log_ratio <- log(Elution_wide$Relative.Pct_1/Elution_wide$Relative.Pct_2) 
Elution_wide$ratio <- Elution_wide$Relative.Pct_1/Elution_wide$Relative.Pct_2

#Create total sperm count
Elution_wide$Total_Sperm <- Elution_wide$Sperm.Count_1+Elution_wide$Sperm.Count_2

Elution_wide1 <- Elution_wide %>% filter(group==1)
Elution_wide2 <- Elution_wide %>% filter(group==2)
Elution_wide3 <- Elution_wide %>% filter(group==3)
Elution_wide4 <- Elution_wide %>% filter(group==4)

#Sample size Calculation
meansd <- Elution_wide %>% group_by(group) %>%  summarise_at(vars(log_ratio), funs(mean,sd))
meandif <- mean(meansd$mean[-1])-meansd$mean[1]
meansd <- meansd %>% mutate(sd2 = sd^2) 
pooledsd <- sqrt(mean(meansd$sd2))
samplesize <- pwr.t.test(d=meandif/pooledsd, sig.level=0.05/6, power=0.80, type="two.sample")
pwr.t.test(d=meandif/pooledsd, sig.level=0.05/6, power=0.80, type="two.sample")

