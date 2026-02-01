# ==========================================================
# Project: Mental Health Crisis Prediction Model (Philadelphia)
# Methodology: Logistic Regression (GLM) & Visualization
# Author: Mehmet Meder
# ==========================================================

# --- 1. Load Libraries ---
library(tidyverse)
library(ggplot2)

# --- 2. Synthetic Data Simulation ---
# Based on sociological risk factors: Divorce, Unemployment, Social Isolation
set.seed(123)
n <- 500
philly_data <- data.frame(
  age = rnorm(n, 45, 15),
  is_divorced = sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3)),
  is_unemployed = sample(c(0, 1), n, replace = TRUE, prob = c(0.8, 0.2)),
  lives_alone = sample(c(0, 1), n, replace = TRUE, prob = c(0.6, 0.4))
)

# Generating the 'High Risk' outcome based on logit logic
logit_risk <- -3 + (1.5 * philly_data$is_divorced) + (2.0 * philly_data$is_unemployed) + (1.2 * philly_data$lives_alone)
prob_risk <- 1 / (1 + exp(-logit_risk))
philly_data$high_risk_status <- ifelse(runif(n) < prob_risk, 1, 0)

# --- 3. THE MODELING PHASE (Logistic Regression) ---
# This is the core 'Program Evaluation' skill: Predicting risk probability
model_risk <- glm(high_risk_status ~ is_divorced + is_unemployed + lives_alone, 
                  data = philly_data, 
                  family = binomial)

# Generate predictions from the model to use in the plot
philly_data$model_predictions <- predict(model_risk, type = "response")

# --- 4. DATA VISUALIZATION (Using Model Predictions) ---
ggplot(philly_data, aes(x = factor(is_divorced), y = model_predictions, fill = factor(is_unemployed))) +
  geom_boxplot(outlier.alpha = 0.5) +
  # Clear labels for US recruiters:
  scale_x_discrete(labels = c("0" = "Married/Single", "1" = "Divorced/Widowed")) + 
  scale_fill_manual(values = c("#2c3e50", "#e74c3c"), 
                    labels = c("0" = "Employed", "1" = "Unemployed")) + 
  labs(title = "Philadelphia Suicide Risk Prediction Model",
       subtitle = "Sociological Impact of Marital Disruption and Economic Shock",
       x = "Marital Status",
       y = "Predicted Probability of High Risk",
       fill = "Employment Status") + 
  theme_minimal()
