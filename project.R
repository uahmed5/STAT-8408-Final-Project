#Importing Requisite Libraries
library(tidyverse)
library(nflreadr)
library(dplyr)
library(nflfastR)
library(FactoMineR)
library(ggplot2)
library(ICSNP)
library(DescTools)
library(fBasics)
library(MVN)
library(car)
library(mvnormtest)
library(biotools)

# Load schedule and team stats
sched <- load_schedules(2023)
stats <- load_team_stats(2023)
pbp <- nflfastR::load_pbp(2023)

# Summarize Offensive Team Stats
offense <- pbp %>%
  group_by(team = posteam) %>%          
  summarize(
    epa_per_play = mean(epa, na.rm = TRUE),
    pass_rate = mean(pass == 1, na.rm = TRUE),
    success_rate = mean(success, na.rm = TRUE),
    turnovers = sum(
      (interception == 1 & posteam == team) + 
        (fumble_lost == 1 & posteam == team),
      na.rm = TRUE
    )
  )

# Summarize Defensive Team Stats
defense <- pbp %>%
  group_by(team = defteam) %>%          
  summarize(
    def_epa_per_play = mean(epa, na.rm = TRUE),   
    sacks = sum(sack == 1, na.rm = TRUE),
    def_interceptions = sum(interception == 1, na.rm = TRUE),
    def_fumble_recoveries = sum(
      (fumble_recovery_1_team == team) |
        (fumble_recovery_2_team == team),
      na.rm = TRUE
    ),
    takeaways = def_interceptions + def_fumble_recoveries
  )


# Merge the Two Summaries
team_summary <- offense %>%
  full_join(defense, by = "team")

# Remove Unecessary Rows/Columns
team_summary <- team_summary[-33, ]
team_summary <- team_summary[,-9]

# Identify Playoff Teams
playoffs <- pbp %>%
  filter(season_type == "POST") %>%
  distinct(game_id, home_team, away_team)

playoff_teams <- playoffs %>%
  pivot_longer(c(home_team, away_team), values_to = "team") %>%
  distinct(team) %>%
  pull(team)

# Add Playoff Indicator to Team Summary
team_summary <- team_summary %>%
  mutate(playoff = ifelse(team %in% playoff_teams, 1, 0))

# Univariate Analysis (Summary Stats, Skewness, Kurtosis, Histograms)
summary(team_summary)

team_summary %>% 
  select_if(is.numeric) %>%  
  skewness(na.rm=TRUE)

team_summary %>% 
  select_if(is.numeric) %>%  
  kurtosis(na.rm=TRUE)

numeric_vars <- team_summary %>%
  select(where(is.numeric))
par(mfrow = c(4, 4))  
for (var in names(numeric_vars)) {
  hist(numeric_vars[[var]], main = paste("Histogram of", var), xlab = var)
}

# Multivariate Normality Check
team_summary2 <- team_summary %>%
  select(epa_per_play, pass_rate, success_rate, turnovers,
         def_epa_per_play, sacks, def_interceptions, takeaways)
MVN::mvn(team_summary2)

# MANOVA
manova_model <- manova(
  cbind(epa_per_play, pass_rate, success_rate, turnovers,
        def_epa_per_play, sacks, def_interceptions, takeaways) ~ playoff,
  data = team_summary
)
summary(manova_model, test = "Wilks")
summary.aov(manova_model)

# Perform PCA
# select only the 8 numeric variables for PCA
pca_vars <- team_summary %>% 
  select(epa_per_play, pass_rate, success_rate, turnovers,
         def_epa_per_play, sacks, def_interceptions,
         takeaways)
res.pca <- PCA(pca_vars, scale.unit = TRUE, graph = FALSE)
res.pca$eig
#summary(res.pca)
plot(res.pca)
res.pca$var$coord
plot(res.pca, choix = "var")

pc_scores <- as.data.frame(res.pca$ind$coord)
pc_scores$playoff <- team_summary$playoff
pc_scores$team <- team_summary$team

# Visualize PCA Results with Playoff Indicator
ggplot(pc_scores, aes(x = Dim.1, y = Dim.2, color = playoff, label = team)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  scale_color_manual(values = c("NonPlayoff" = "gray50", "Playoff" = "red"),
                     name   = "Team Type",
                     labels = c("NonPlayoff" = "Non-Playoff",
                                "Playoff"    = "Playoff")) +
  labs(title = "Playoff vs Non Playoff on PCA Dimensions",
       x = paste0("PC1 (38.9%)"),
       y = paste0("PC2 (22.2%)")) +
  theme_minimal()

#Logistic Regression
model <- glm(playoff ~ epa_per_play + pass_rate + success_rate +
               turnovers + def_epa_per_play + sacks + 
               def_interceptions + takeaways,
             data = team_summary,
             family = binomial)
summary(model)
vif(model)

pca_model <- glm(playoff ~ Dim.1 + Dim.2,
                 data = pc_scores,
                 family = binomial)
summary(pca_model)
exp(coef(pca_model)) #Odds Ratio 

pc_scores$pred_prob <- predict(pca_model, type = "response")
pc_scores$pred_class <- ifelse(pc_scores$pred_prob > 0.5, 1, 0)
table(pc_scores$pred_class, pc_scores$playoff) #Confusion Matrix