library(tidyverse)
library(GGally)
library(corrplot)
library(ggpubr)
library(pheatmap)
leadership <- read_csv('GLOBE-Phase-2-Aggregated-Leadership-Data.csv')
societal <- read_csv('GLOBE-Phase-2-Aggregated-Societal-Culture-Data.csv')
Country <- cbind(leadership[, 1:29], societal[, 3:21])

data <- read_csv('STATS141SL_Leadership.csv')
individual <- na.omit(data)

South_Africa <- Country %>% filter(`Country Name` %in% c('South Africa (Black Sample)',
                                                         'South Africa (White Sample)')) %>%
  select(`Performance Oriented`:`Assertiveness Societal Values`) %>% apply(2, mean) %>% t()
South_Africa <- data.frame(Country = 43, 'Country Name' = 'South Africa', South_Africa, 'Country Cluster' = 'Anglo')
Germany <- Country %>% filter(`Country Name` %in% c('Germany (EAST)', 'Germany (WEST)')) %>%
  select(`Performance Oriented`:`Assertiveness Societal Values`) %>% apply(2, mean) %>% t()
Germany <- data.frame(Country = 93, 'Country Name' = 'Germany', Germany, 'Country Cluster' = 'Germanic Europe')
Switzerland <- Country %>% filter(`Country Name` %in% c('Switzerland', 'French Switzerland')) %>%
  select(`Performance Oriented`:`Assertiveness Societal Values`) %>% apply(2, mean) %>% t()
Switzerland <- data.frame(Country = 55, 'Country Name' = 'Switzerland', Switzerland, 'Country Cluster' = 'Germanic Europe')
Country_new <- Country %>% filter(!(`Country Name` %in% c('South Africa (Black Sample)',
                                                          'South Africa (White Sample)',
                                                          'Germany (EAST)', 'Germany (WEST)',
                                                          'Switzerland', 'French Switzerland')))
colnames(South_Africa) <- colnames(Country_new)
colnames(Germany) <- colnames(Country_new)
colnames(Switzerland) <- colnames(Country_new)
Country_new <- rbind(Country_new, South_Africa, Germany, Switzerland)
country_select <- individual %>% select(countryRaised, Social_EQ:Strives_4_Structure) %>%
  group_by(countryRaised) %>% summarise(Social_EQ = mean(Social_EQ),
                                        Learning_IQ = mean(Learning_IQ),
                                        Strives_4_Balance = mean(Strives_4_Balance),
                                        Strives_4_Challenge = mean(Strives_4_Challenge),
                                        Strives_4_Collaboration = mean(Strives_4_Collaboration),
                                        Strives_4_Independence = mean(Strives_4_Independence),
                                        Strives_4_Power = mean(Strives_4_Power),
                                        Strives_4_Structure = mean(Strives_4_Structure))

country_name <- Country_new %>% select(`Country Name`) %>% apply(1, as.character)
country_name <- str_remove(country_name, " \\W\\w+\\W\\w+\\W")
country_select <- country_select %>% filter(countryRaised %in% country_name)

m1 <- aov(Social_EQ ~ leadershipLevel, data = individual)
summary(m1)

m2 <- aov(Learning_IQ ~ leadershipLevel, data = individual)
summary(m2)

lowerCI_EQ <- boxplot(Social_EQ ~ leadershipLevel, data = individual)$conf
upperCI_EQ <- lowerCI_EQ[2, ]
lowerCI_EQ <- lowerCI_EQ[1, ]
mean_EQ <- individual %>% select(leadershipLevel, Social_EQ) %>% group_by(leadershipLevel) %>% summarise(mean(Social_EQ))
mean_EQ <- unlist(mean_EQ[, 2])
type_EQ <- c('a', 'c', 'b', 'd')
mean_dfEQ <- tibble(type_EQ, mean_EQ, lowerCI_EQ, upperCI_EQ)

mean_dfEQ %>% 
  ggplot(aes(x = type_EQ, y = mean_EQ, group = 1)) + 
  geom_line(col = 'cornflowerblue') +
  geom_point(col = 'cornflowerblue') + scale_x_discrete(labels = c('Executive', 'Mid-Level Leader', 'Low-Level Leader', 'None')) +
  labs(x = 'Leadership Level', y = 'Social EQ', title = 'Boxplot of Sicail EQ of Leadership Level') +
  theme_minimal(base_size = 10)

# ggsave('boxplotEQ.png', dpi = 1800)

lowerCI_IQ <- boxplot(Learning_IQ ~ leadershipLevel, data = individual)$conf
upperCI_IQ <- lowerCI_IQ[2, ]
lowerCI_IQ <- lowerCI_IQ[1, ]
mean_IQ <- individual %>% select(leadershipLevel, Learning_IQ) %>% group_by(leadershipLevel) %>% summarise(mean(Learning_IQ))
mean_IQ <- unlist(mean_IQ[, 2])
type_IQ <- c('a', 'c', 'b', 'd')
mean_dfIQ <- tibble(type_IQ, mean_EQ, lowerCI_IQ, upperCI_IQ)

mean_dfIQ %>% 
  ggplot(aes(x = type_IQ, y = mean_IQ, group = 1)) + 
  geom_line(col = 'firebrick') +
  geom_point(col = 'firebrick') + scale_x_discrete(labels = c('Executive', 'Mid-Level Leader', 'Low-Level Leader', 'None')) +
  labs(x = 'Leadership Level', y = 'Learning IQ', title = 'Boxplot of Learning IQ of Leadership Level') +
  theme_minimal(base_size = 10)

# ggsave('boxplotIQ.png', dpi = 1800)

pdf(file = "heatmap8x8.pdf")

pheatmap(cor(country_select[, 2:9]), treeheight_row = 0, treeheight_col = 0, Colv = FALSE, filename = 'heatmap8x8.jpg',
         #cluster_rows=F, cluster_cols=F, 
         # main = 'Figure 4.1.2: CEO Leadership Behavior Correlation Plot',
         border_color = 'grey', fontsize = 10, fontsize_row = 11, fontsize_col = 11)
dev.off()

pdf(file = "heatmap45x45.pdf")

pheatmap(cor(Country_new[, 3:47]), treeheight_row = 0, treeheight_col = 0, Colv = FALSE, filename = 'heatmap45x45.jpg',
         #cluster_rows=F, cluster_cols=F, 
         main = 'Figure 4.1.2: National Societal Culture and Leadership Correlation Plot',
         border_color = 'grey', fontsize = 5, fontsize_row = 7, fontsize_col = 7)
dev.off()

corr845 <- apply(country_select[, 2:9], 2, function(x) {cor(x, Country_new[, 3:47])})
rownames(corr845) <- colnames(Country_new[, 3:47])

pdf(file = "heatmap8x45.pdf")

pheatmap(corr845, treeheight_row = 0, treeheight_col = 0, filename = 'heatmap8x45.jpg',
         cluster_rows = F, cluster_cols = F, main = 'Figure 4.1.1: National Societal Culture and Leadership and \nCEO Leadership Behavior Correlation Plot',
         border_color = "grey", fontsize = 5, fontsize_row = 7, fontsize_col = 7)
dev.off()

EQ <- cor(country_select[, 2], Country_new[, 3:47])
EQ_head <- abs(EQ) %>% sort(decreasing = TRUE) %>% head(5)
EQ_tail <- abs(EQ) %>% sort(decreasing = TRUE) %>% tail(5)
EQnum_head <- EQ[which(abs(EQ) %in% EQ_head)]
EQnum_tail <- EQ[which(abs(EQ) %in% EQ_tail)]
EQ_head <- colnames(EQ)[which(abs(EQ) %in% EQ_head)]
EQ_tail <- colnames(EQ)[which(abs(EQ) %in% EQ_tail)]

IQ <- cor(country_select[, 3], Country_new[, 3:47])
IQ_head <- abs(IQ) %>% sort(decreasing = TRUE) %>% head(5)
IQ_tail <- abs(IQ) %>% sort(decreasing = TRUE) %>% tail(5)
IQnum_head <- IQ[which(abs(IQ) %in% IQ_head)]
IQnum_tail <- IQ[which(abs(IQ) %in% IQ_tail)]
IQ_head <- colnames(IQ)[which(abs(IQ) %in% IQ_head)]
IQ_tail <- colnames(IQ)[which(abs(IQ) %in% IQ_tail)]

Balance <- cor(country_select[, 4], Country_new[, 3:47])
Balance_head <- abs(Balance) %>% sort(decreasing = TRUE) %>% head(5)
Balance_tail <- abs(Balance) %>% sort(decreasing = TRUE) %>% tail(5)
Balancenum_head <- Balance[which(abs(Balance) %in% Balance_head)]
Balancenum_tail <- Balance[which(abs(Balance) %in% Balance_tail)]
Balance_head <- colnames(Balance)[which(abs(Balance) %in% Balance_head)]
Balance_tail <- colnames(Balance)[which(abs(Balance) %in% Balance_tail)]

Challenge <- cor(country_select[, 5], Country_new[, 3:47])
Challenge_head <- abs(Challenge) %>% sort(decreasing = TRUE) %>% head(5)
Challenge_tail <- abs(Challenge) %>% sort(decreasing = TRUE) %>% tail(5)
Challengenum_head <- Challenge[which(abs(Challenge) %in% Challenge_head)]
Challengenum_tail <- Challenge[which(abs(Challenge) %in% Challenge_tail)]
Challenge_head <- colnames(Challenge)[which(abs(Challenge) %in% Challenge_head)]
Challenge_tail <- colnames(Challenge)[which(abs(Challenge) %in% Challenge_tail)]

Collaboration <- cor(country_select[, 6], Country_new[, 3:47])
Collaboration_head <- abs(Collaboration) %>% sort(decreasing = TRUE) %>% head(5)
Collaboration_tail <- abs(Collaboration) %>% sort(decreasing = TRUE) %>% tail(5)
Collaborationnum_head <- Collaboration[which(abs(Collaboration) %in% Collaboration_head)]
Collaborationnum_tail <- Collaboration[which(abs(Collaboration) %in% Collaboration_tail)]
Collaboration_head <- colnames(Collaboration)[which(abs(Collaboration) %in% Collaboration_head)]
Collaboration_tail <- colnames(Collaboration)[which(abs(Collaboration) %in% Collaboration_tail)]

Independence <- cor(country_select[, 7], Country_new[, 3:47])
Independence_head <- abs(Independence) %>% sort(decreasing = TRUE) %>% head(5)
Independence_tail <- abs(Independence) %>% sort(decreasing = TRUE) %>% tail(5)
Independencenum_head <- Independence[which(abs(Independence) %in% Independence_head)]
Independencenum_tail <- Independence[which(abs(Independence) %in% Independence_tail)]
Independence_head <- colnames(Independence)[which(abs(Independence) %in% Independence_head)]
Independence_tail <- colnames(Independence)[which(abs(Independence) %in% Independence_tail)]

Power <- cor(country_select[, 8], Country_new[, 3:47])
Power_head <- abs(Power) %>% sort(decreasing = TRUE) %>% head(5)
Power_tail <- abs(Power) %>% sort(decreasing = TRUE) %>% tail(5)
Powernum_head <- Power[which(abs(Power) %in% Power_head)]
Powernum_tail <- Power[which(abs(Power) %in% Power_tail)]
Power_head <- colnames(Power)[which(abs(Power) %in% Power_head)]
Power_tail <- colnames(Power)[which(abs(Power) %in% Power_tail)]

Structure <- cor(country_select[, 9], Country_new[, 3:47])
Structure_head <- abs(Structure) %>% sort(decreasing = TRUE) %>% head(5)
Structure_tail <- abs(Structure) %>% sort(decreasing = TRUE) %>% tail(5)
Structurenum_head <- Structure[which(abs(Structure) %in% Structure_head)]
Structurenum_tail <- Structure[which(abs(Structure) %in% Structure_tail)]
Structure_head <- colnames(Structure)[which(abs(Structure) %in% Structure_head)]
Structure_tail <- colnames(Structure)[which(abs(Structure) %in% Structure_tail)]

tibble(EQ_head, EQnum_head, EQ_tail, EQnum_tail)
tibble(IQ_head, IQnum_head, IQ_tail, IQnum_tail)
tibble(Balance_head, Balancenum_head, Balance_tail, Balancenum_tail)
tibble(Challenge_head, Challengenum_head, Challenge_tail, Challengenum_tail)
tibble(Collaboration_head, Collaborationnum_head, Collaboration_tail, Collaborationnum_tail)
tibble(Independence_head, Independencenum_head, Independence_tail, Independencenum_tail)
tibble(Power_head, Powernum_head, Power_tail, Powernum_tail)
tibble(Structure_head, Structurenum_head, Structure_tail, Structurenum_tail)