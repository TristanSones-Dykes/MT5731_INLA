library(tidyverse)

# Read in the data
df <- read_csv("regular_season_compact_results_2015.csv")
teams <- read_csv("teams.csv")

# Create array mapping team_id to skill parameter array
team_map <- teams$team_id
team_names <- teams$team_name

wins_data <- df %>% 
    select(wteam, lteam, wloc) %>% 
    filter(wloc != "N") %>%
    mutate(wteam = match(wteam, team_map), 
           lteam = match(lteam, team_map)) %>%
    mutate(home_team = ifelse(wloc == "H", wteam, lteam), 
           away_team = ifelse(wloc == "A", wteam, lteam)) %>%
    mutate(win_home = ifelse(wloc == "H", 1, 0)) %>%
    select(home_team, away_team, win_home)

# Create results matrix
X <- matrix(0, nrow = nrow(wins_data), ncol = length(team_map))

# Fill in results matrix
for (i in seq_len(nrow(wins_data))) {
    X[i, wins_data[[i, "home_team"]]] <- 1
    X[i, wins_data[[i, "away_team"]]] <- -1
}

y <- wins_data$win_home
input_data <- as.data.frame(cbind(y, X))

# fit glm
model <- glm(y ~ ., data = input_data, family = binomial(link = "logit"))

# get skill parameters
skill_params <- data.frame(model$coefficients[-1]) %>%
    rownames_to_column()
colnames(skill_params) <- c("team_id", "skill")
skill_params$team_id <- team_map

# calculate win % for each team
wins <- df %>% 
    group_by(wteam) %>%
    summarise(wins = n())
losses <- df %>%
    group_by(lteam) %>%
    summarise(losses = n())

win_pct <- wins %>%
    left_join(losses, by = c("wteam" = "lteam")) %>%
    mutate(win_pct = wins / (wins + losses)) %>%
    select(wteam, win_pct)

combined <- win_pct %>%
    left_join(skill_params, by = c("wteam" = "team_id")) %>%
    mutate(team_name = team_names[wteam]) %>% 
    select(wteam, win_pct, skill) %>% 
    drop_na()

ggplot(combined %>% drop_na(), aes(x = skill, y = win_pct)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    labs(x = "Skill", y = "Win %", title = "Skill vs. Win %")





# ----- NBA analysis ----- #
# ------------------------ #


basketball_teams <- read_csv("teams_nba.csv")
basketball_games <- read_csv("games.csv") %>% 
    filter(str_starts(GAME_DATE_EST, "2018")) %>% 
    select(HOME_TEAM_ID, VISITOR_TEAM_ID, HOME_TEAM_WINS) %>% 
    mutate(HOME_TEAM_ID = basketball_teams$NICKNAME[match(HOME_TEAM_ID, basketball_teams$TEAM_ID)],
           VISITOR_TEAM_ID = basketball_teams$NICKNAME[match(VISITOR_TEAM_ID, basketball_teams$TEAM_ID)]) %>% 
    drop_na() %>% 
    mutate(VISITOR_TEAM_WINS = 1 - HOME_TEAM_WINS)

teams <- sort(unique(c(basketball_games$HOME_TEAM_ID, basketball_games$VISITOR_TEAM_ID)))

get_data_vec <- function(home_team, away_team, teams) {
  vec <- rep(0, length(teams))
  vec[teams == home_team] <- 1
  vec[teams == away_team] <- -1
  vec
}

X <- apply(basketball_games, 1, 
           function(row) get_data_vec(row["HOME_TEAM_ID"], 
                                      row["VISITOR_TEAM_ID"], 
                                      teams))
X <- t(X)
colnames(X) <- teams
dim(X)

y <- as.numeric(basketball_games$HOME_TEAM_WINS)
bt_df <- as.data.frame(cbind(y, X))
colnames(bt_df) <- c("y", "Seventy_Sixers", "Bucks", "Bulls", "Cavaliers", "Celtics", "Clippers", "Grizzlies", "Hawks", "Heat", "Hornets", "Jazz", "Kings", "Knicks", "Lakers", "Magic", "Mavericks", "Nets", "Nuggets", "Pacers", "Pelicans", "Pistons", "Raptors", "Rockets", "Spurs", "Suns", "Thunder", "Timberwolves", "Trail_Blazers", "Warriors", "Wizards")

# Bradley-Terry model with home advantage
bt_mod <- glm(y ~ ., data = bt_df, family = binomial())
summary(bt_mod)

# Compare BT coefficients with overall win percentage
coef_df <- data.frame(
  team = bt_df %>% select(-y) %>% colnames() %>% as.character(),
  beta = c(summary(bt_mod)$coefficients[2:length(teams), "Estimate"], 0)
)

# get team win percentages
home_df <- basketball_games %>% group_by(HOME_TEAM_ID) %>%
  summarize(home_win  = sum(HOME_TEAM_WINS),
            home_loss = sum(!HOME_TEAM_WINS))
away_df <- basketball_games %>% group_by(VISITOR_TEAM_ID) %>%
  summarize(away_win  = sum(VISITOR_TEAM_WINS),
            away_loss = sum(!VISITOR_TEAM_WINS))
win_pct_df <- inner_join(home_df, away_df, 
                         by = c("HOME_TEAM_ID" = "VISITOR_TEAM_ID")) %>%
  transmute(team = HOME_TEAM_ID,
            win = home_win + away_win,
            loss = home_loss + away_loss) %>%
  mutate(win_pct = win / (win + loss)) %>%
  arrange(desc(win_pct))
  
library(ggrepel)

win_pct_df %>% inner_join(coef_df) %>%
    ggplot(aes(x = win_pct, y = beta)) +
    geom_point() +
    geom_text_repel(aes(label = team)) +
    labs(x = "Win percentage", y = "Bradley-Terry beta",
        title = "Bradley-Terry beta vs. Win %") + 
    geom_smooth(method = "lm", se = TRUE)


# Predictions
n_train <- nrow(X) / 2
train_df <- bt_df[1:n_train, ]
test_df <- bt_df[(n_train + 1):nrow(X), ]

train_mod_home <- glm(y ~ ., data = train_df, family = binomial())
pred_home <- predict(train_mod_home, newdata = test_df, type = "response")
train_mod_no_home <- glm(y ~ . + 0, data = train_df, family = binomial())
pred_no_home <- predict(train_mod_no_home, newdata = test_df, type = "response")

plot(pred_home, pred_no_home, pch = 16, cex = 0.5,
     xlab = "Prob. of home win (model w. home adv.)",
     ylab = "Prob. of home win (model w/o home adv.)")
abline(0, 1)

pred_df <- data.frame(
  truth = test_df$y,
  pred_home = pred_home,
  pred_no_home = pred_no_home
) %>%
  pivot_longer(pred_home:pred_no_home, 
               names_to = "model", 
               values_to = "prediction")

library(plotROC)
roc_plot <- ggplot(pred_df) +
  geom_roc(aes(d = truth, m = prediction, color = model), labels = FALSE)

roc_plot +
  labs(x = "True positive fraction",
       y = "False positive fraction",
       title = "Test set ROC plots") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom")


library(INLA)

# Specify the formula
f <- as.formula(paste0("y ~ ", # Response first
                          paste(colnames(bt_df)[2:length(colnames(bt_df))], collapse = " + ") # Collapse the vector of covariates
))

# Bradley-Terry model in INLA
inla_model <- inla(f, data = bt_df, family = "binomial")
summary(inla_model)

inla_coefs <- summary(inla_model)$fixed[, 1]

# Compare INLA and glm coefficients
coef_df$inla = inla_coefs[2:length(inla_coefs)]

ggplot(coef_df, aes(x = beta, y = inla)) +
  geom_point() +
  geom_text_repel(aes(label = team)) +
  labs(x = "glm beta", y = "INLA beta",
       title = "glm beta vs. INLA beta")

lm <- lm(beta ~ inla, data = coef_df)
summary(lm)

# use coefficients of lm to scale inla coefficients
coef_df$inla_scaled <- coef_df$inla * lm$coefficients[2] + lm$coefficients[1]

f_random <- as.formula(paste0("y ~ ", 
                        paste(colnames(bt_df)[2:length(colnames(bt_df))], collapse = " + "), 
                        " + f(", paste(colnames(bt_df)[-1], collapse = " + "), ", model = 'iid')"))
