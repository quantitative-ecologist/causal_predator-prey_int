# =========================================
# Import data - prepare data
# =========================================

github_id <- "https://raw.githubusercontent.com/quantitative-ecologist/"
repo <- "experience-hunting-tactics"
file <- "/main/data/FraserFrancoetal2023-data.csv"
dat <- read.csv(
  file.path(github_id, repo, file)
)

dat1 <- dat[
  , c(
    "prey_avg_speed",
    "pred_speed",
    "cumul_xp_pred",
    "game_duration",
    "prey_avg_rank"
  )
]

dat1$rank <- ifelse(
  dat1$prey_avg_rank <= 10,
  1,
  0
)

# Splint into training and testing
set.seed(123)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(
  c(TRUE, FALSE),
  nrow(dat1),
  replace = TRUE,
  prob = c(0.7, 0.3)
)

train <- dat1[sample, ]
test  <- dat1[!sample, ]

which(is.na(train))
which(is.na(test))
train <- na.omit(train)
# =========================================
# =========================================





# =========================================
# Fit model
# =========================================

library(grf)
Y <- as.matrix(train[, 1])
X <- as.matrix(train[, c(2:4)])
W <- as.matrix(train[, 6])

fit <- causal_forest(
  X = X,
  Y = Y,
  W = W,
  seed = 123,
  num.threads = 10
)
# =========================================
# =========================================





# =========================================
# Plot predictions using geom_smooth
# =========================================

X_test <- as.matrix(test[, c(2:4)])
preds <- predict(
  fit,
  data = X_test,
  estimate.variance = TRUE
)

library(ggplot2)
train <- cbind(train, preds)

# Set a custom theme for plots
custom_theme <- theme(
  panel.grid = element_blank(),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12)
)

p1 <- ggplot(
  data = train,
  aes(
    y = predictions,
    x = cumul_xp_pred,
    color = as.factor(rank)
  )
) +
  ylab("Predicted prey speed\n") +
  xlab("\nPredator experience") +
  labs(color = "Prey rank:") +
  geom_point(shape = ".") +
  geom_smooth() +
  theme_bw() + custom_theme

p2 <- ggplot(
  data = train,
  aes(
    y = predictions,
    x = pred_speed,
    color = as.factor(rank)
  )
) +
  ylab("Predicted prey speed\n") +
  xlab("\nPredator speed") +
  labs(color = "Prey rank:") +
  geom_point(shape = ".") +
  geom_smooth() +
  theme_bw() + custom_theme

p <- ggpubr::ggarrange(
  p1, p2,
  common.legend = TRUE
)
p
# =========================================
# =========================================



# =========================================
# Interpret model with other functions
# =========================================
library(iml)
grf::variable_importance(fit)

plot(
  rank_average_treatment_effect(
    fit,
    train$predictions
  )
)

preds <- Predictor$new(
  fit,
  data = test[, c(2:4)]
)

future::plan(multicore, workers = 10)

eff1 <- FeatureEffect$new(
  preds,
  feature = c("cumul_xp_pred", "pred_speed"),
  method = "ale"
)
eff2 <- FeatureEffect$new(
  preds,
  feature = c("cumul_xp_pred", "pred_speed"),
  method = "pdp"
)

plot(eff1) +
  xlab("\nPredator experience") +
  ylab("Predator speed\n") +
  theme_bw() +
  custom_theme

plot(eff2) +
  xlab("\nPredator experience") +
  ylab("Predator speed\n") +
  theme_bw() +
  custom_theme