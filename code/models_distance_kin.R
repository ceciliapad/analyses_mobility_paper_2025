############################
## Distance to kin models ##
############################

region_colors <- MetPalettes$Isfahan2[[1]][-1]

# make a darker color version
region_darker_colors <- scales::muted(region_colors, l = 50) 

#load necessary libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(vtable)
library(MetBrewer)

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}


# read csv
reprod_all_bp2 <- read.csv("data/final/distance_to_kin_macmin.csv")

# create age categories
reprod_all_bp2$age_group <- ifelse(reprod_all_bp2$age == "Adolescent", "Adolescent", 
                         ifelse(reprod_all_bp2$age %in% c("Adulte", "Personne âgée"), "Adult", NA))

# Start modeling
# We will use Hurdle models to account for the zero-inflated nature of the data - and
# model separately the probability of living witha parent to the distance to them.

m5.prior_hurdle <- c(
  prior(student_t(3, 4.5, 2), class = Intercept),  # Centered around log(90 km) for positive distances
  prior(gamma(2, 0.1), class = shape),            # Shape parameter for Gamma distribution
  prior(student_t(3, 0, 5), class = sd),          # Random effect SD
  prior(normal(0, 2), class = b)                # Fixed effect coefficients
  #prior(beta(1, 1), class = hu)                   # Uniform prior for hurdle probability
)

m5.prior_hurdle_log <- c(
  prior(student_t(3, 4.5, 2), class = Intercept),  # Centered around log(90 km) for positive distances           # Shape parameter for Gamma distribution
  prior(student_t(3, 0, 5), class = sd),          
  prior(normal(0, 3), class = b),                 # Fixed effect coefficients
  prior(beta(1, 1), class = hu)                   # Uniform prior for hurdle probability
)

m6.prior_hurdle <-   c(
# Intercept for Positive Gamma Component
prior(student_t(3, 4.5, 2), class = Intercept),  
# Intercept for Hurdle Component (logit-scale)
prior(normal(1.5, 1), class = Intercept, dpar = "hu"),  
# Fixed Effects for Gamma and Hurdle Components
prior(normal(0, 1.5), class = b),  
prior(normal(0, 1.5), class = b, dpar = "hu"),  
# Standard Deviations for Random Effects
prior(student_t(3, 0, 2), class = sd),  
prior(student_t(3, 0, 2), class = sd, dpar = "hu"),  
# Shape Parameter for Gamma Distribution
prior(normal(1, 0.1), class = shape)
)

priors_m8_m9 <- c(
  # Intercept (log-scale, as we are modeling log-normal)
  prior(student_t(3, 4.5, 1.5), class = Intercept),  # Reflects reasonable prior for distances
  # Fixed Effects (Coefficients for age_group, sex, and interaction)
  prior(normal(0, 2), class = b),  # Fixed effects with moderate regularization
  # Random Effects Standard Deviations
  prior(student_t(3, 0, 2), class = sd),  # Random effects SD for region and nested terms
  # Correlation Structure for Random Slopes
  prior(lkj(2), class = cor),  # LKJ prior to regularize correlations in hierarchical slopes
  # Hurdle Component (Probability of Zero)
  prior(beta(2, 2), class = hu),  # Weakly regularizing beta prior for hurdle probability
  # Log-Scale Sigma for Log-Normal Variance
  prior(student_t(3, 0, 2), class = sigma)  # Log-scale error term for log-normal
)

formula_m5 <- bf(
  dist_living_mum_living ~ age_group + sex + 
    (1 + age_group + sex | region) +           # Random intercept and slopes by region
    (1 | region:residence_camp),               # Nested random intercept for residence_camp
  family = hurdle_gamma(link = "log"),
  hu ~ age_group + sex +                       # Fixed effects in the hurdle part
    (1 + age_group + sex | region)          # Random intercepts and slopes by region
)

formula_m5b <- bf(
  dist_living_mum_living ~ age_group + sex + 
    (1 +  sex | region) +           # Random intercept and slopes by region
    (1 | region:residence_camp),               # Nested random intercept for residence_camp
  family = hurdle_gamma(link = "log"),
  hu ~ age_group + sex +                       # Fixed effects in the hurdle part
    (1  + sex | region)       
)

formula_m5c <- bf(
  dist_living_mum_living ~ age_group + sex + 
    (1  | region) +           # Random intercept 
    (1 | region:residence_camp),               # Nested random intercept for residence_camp
  family = hurdle_gamma(link = "log"),
  hu ~ age_group + sex +                       # Fixed effects in the hurdle part
    (1 | region)       
)

formula_m6 <- bf(
  dist_living_mum_living ~ age_group * sex +   # Interaction of age_group and sex
    (1 | region) +
    (1 | region:residence_camp),               # Nested random intercept for residence_camp
  family = hurdle_gamma(link = "log"),
  hu ~ age_group*sex +                       # Fixed effects in the hurdle part
    (1  | region)             # Nested random intercept for residence_ca
)

formula_m6b <- bf(
  dist_living_mum_living ~ age_group * sex +   # Interaction of age_group and sex
    (1 + sex | region) +
    (1 | region:residence_camp),               # Nested random intercept for residence_camp
  family = hurdle_gamma(link = "log"),
  hu ~ age_group*sex +                       # Fixed effects in the hurdle part
    (1 + sex | region)       
)

formula_m7 <- bf(
  dist_living_mum_living ~ age_group * sex +   # Interaction of age_group and sex
    (1 + age_group + sex | region) +          # Random slopes for age and sex by region
    (1 | region:residence_camp),               # Nested random intercept for residence_camp
  family = hurdle_gamma(link = "log"),
  hu ~ age_group*sex +                       # Fixed effects in the hurdle part
    (1 + age_group + sex | region)       
)


formula_m8 <- bf(
  dist_living_mum_living ~ age_group*sex +
    (1 + age_group + sex | region) +
    (1 | region:residence_camp),
  family = hurdle_lognormal()
)

formula_m9 <- bf(
  dist_living_mum_living ~ age_group*sex +
    (1 + sex | region) +
    (1 | region:residence_camp),
  family = hurdle_lognormal()
)


# Model 1
kin_model_brm1 <- brm(
  formula = formula_m5,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.95),
  prior = m5.prior_hurdle
)

# Model 1 
kin_model_brm1b <- brm(
  formula = formula_m5b,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.99),
  prior = m7.prior_hurdle
)

kin_model_brm1c <- brm(
  formula = formula_m5c,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.99),
  prior = m7.prior_hurdle
)

# Model 2
kin_model_brm2 <- brm(
  formula = formula_m6,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.99),
  prior = m7.prior_hurdle
)

kin_model_brm2b <- brm(
  formula = formula_m6b,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.95),
  prior = m5.prior_hurdle
)

kin_model_brm2c <- brm(
  formula = formula_m6b,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.99),
  prior = m7.prior_hurdle
)

# Model 3
kin_model_brm3 <- brm(
  formula = formula_m7,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.95),
  prior = m5.prior_hurdle
)

# Model 4 # lognormal model - to compare with Gamma?
kin_model_brm4 <- brm(
  formula = formula_m8,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.95),
  prior = priors_m8_m9
)

kin_model_brm4b <- brm(
  formula = formula_m9,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.95),
  prior = priors_m8_m9
)

# Save all models as .rds
saveRDS(kin_model_brm1, "models/distance_to_kin_model1.rds")
saveRDS(kin_model_brm1b, "models/distance_to_kin_model1b.rds")
saveRDS(kin_model_brm1c, "models/distance_to_kin_model1c.rds")
saveRDS(kin_model_brm2, "models/distance_to_kin_model2.rds")
saveRDS(kin_model_brm2b, "models/distance_to_kin_model2b.rds")
saveRDS(kin_model_brm2c, "models/distance_to_kin_model2c.rds")
saveRDS(kin_model_brm3, "models/distance_to_kin_model3.rds")
saveRDS(kin_model_brm4, "models/distance_to_kin_model4.rds")

# load all models
kin_model_brm1 <- readRDS("models/distance_to_kin_model1.rds")
kin_model_brm1b <- readRDS("models/distance_to_kin_model1b.rds")
kin_model_brm1c <- readRDS("models/distance_to_kin_model1c.rds")
kin_model_brm2 <- readRDS("models/distance_to_kin_model2.rds")
kin_model_brm2b <- readRDS("models/distance_to_kin_model2b.rds")
kin_model_brm2c <- readRDS("models/distance_to_kin_model2c.rds")
kin_model_brm3 <- readRDS("models/distance_to_kin_model3.rds")
kin_model_brm4 <- readRDS("models/distance_to_kin_model4.rds")


# compare models 2 and 4 using loo
loo_model2 <- loo(kin_model_brm2)
loo_model4 <- loo(kin_model_brm4)

comparison <- loo_compare(loo_model2, loo_model4)
comparison # Gamma distribution does better

# save comparison
# rename rows to model types
comparison <- as.data.frame(comparison)
comparison$model <- c("gamma_model", "lognormal_model")
write.csv(comparison, "results/mating_range_expl_macmin/loo_comparison_gamma_lognormal.csv")


# compare models 1, 2, 3, using loo
#loo_model1 <- loo(kin_model_brm1)
loo_model1b <- loo(kin_model_brm1b)
loo_model1c <- loo(kin_model_brm1c)
loo_model2 <- loo(kin_model_brm2)
#loo_model3 <- loo(kin_model_brm3)
#loo_model2b <- loo(kin_model_brm2b)
loo_model2c <- loo(kin_model_brm2c)

comparison <- loo_compare( loo_model1b,loo_model1c,loo_model2,  loo_model2c)
comparison  

# save comparison
# rename rows to model types
comparison <- as.data.frame(comparison)
#comparison$model <- c("int_randomint", )
write.csv(comparison, "results/mating_range_expl_macmin/loo_comparison_models123_hurdle_gamma.csv")

## Prepare plotting

# Prepare data for predictions
region_levels <- unique(reprod_all_bp2$region)

# Expand nd to include regions
nd <- expand.grid(
  sex = levels(as.factor(reprod_all_bp2$sex)),
  age_group = levels(as.factor(reprod_all_bp2$age_group)),
  region = region_levels
)

# Predict positive distances
distance_predictions <- fitted(
  kin_model_brm1b,
  allow_new_levels = TRUE, 
  re_formula = ~ (1 + sex | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  dpar = "mu",  # Hurdle Gamma's mean of positive values
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Predict hurdle probabilities (probability of zero distances)
hurdle_predictions <- fitted(
  kin_model_brm1b,
  allow_new_levels = TRUE, 
  re_formula = ~ (1  +sex | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  dpar = "hu",  # Hurdle probability (probability of zero)
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Plot positive distances
distance_plot <- distance_predictions %>%
  ggplot(aes(x = age_group, y = Estimate / 1000, color = region, group = region)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = region), 
              alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = region), 
              alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  facet_wrap(~ sex) +
  labs(
    x = "Age group",
    y = "Distance from mother's residence (km)"
  ) +
  ggtitle("Distances by region") +
  scale_color_manual(values = region_darker_colors) +  # Central line colors
  scale_fill_manual(values = region_colors) +         # Ribbon colors
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

distance_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_positive_distances.png", plot = distance_plot, width = 10, height = 7)

# Plot probability of zero distances
hurdle_plot <- hurdle_predictions %>%
  ggplot(aes(x = age_group, y = Estimate, color = region, group = region)) +
  geom_line(size = 1, alpha = 1) +  # Line for each region
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = region), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~ sex) +
  labs(
    x = "Age group",
    y = "Probability of living with mother"
  ) +
  ggtitle("Probability of living with mother") +
  scale_color_manual(values = region_darker_colors) +
  scale_fill_manual(values = region_colors) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )
hurdle_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_zero_distance_probabilities.png", plot = hurdle_plot, width = 10, height = 7)

# Predict positive distances
distance_predictions_int <- fitted(
  kin_model_brm2,
  allow_new_levels = TRUE, 
  re_formula = ~ (1 +sex | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  dpar = "mu",  # Hurdle Gamma's mean of positive values
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Predict hurdle probabilities (probability of zero distances)
hurdle_predictions_int <- fitted(
  kin_model_brm2,
  allow_new_levels = TRUE, 
  re_formula = ~ (1  +sex | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  dpar = "hu",  # Hurdle probability (probability of zero)
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Plot positive distances
distance_plot <- distance_predictions_int %>%
  ggplot(aes(x = age_group, y = Estimate / 1000, color = region, group = region)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = region), 
              alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = region), 
              alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  facet_wrap(~ sex) +
  labs(
    x = "Age group",
    y = "Distance from mother's residence (km)"
  ) +
  ggtitle("Distances by region") +
  coord_cartesian(ylim = c(0, 150)) +
  scale_color_manual(values = region_darker_colors) +  # Central line colors
  scale_fill_manual(values = region_colors) +         # Ribbon colors
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

distance_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_positive_distances_int.png", plot = distance_plot, width = 10, height = 7)

# Plot probability of zero distances
hurdle_plot <- hurdle_predictions_int %>%
  ggplot(aes(x = age_group, y = Estimate, color = region, group = region)) +
  geom_line(size = 1, alpha = 1) +  # Line for each region
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = region), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~ sex) +
  labs(
    x = "Age group",
    y = "Probability of living with mother"
  ) +
  ggtitle("Probability of living with mother") +
  scale_color_manual(values = region_darker_colors) +
  scale_fill_manual(values = region_colors) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )
hurdle_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_zero_distance_probabilities_int.png", plot = hurdle_plot, width = 10, height = 7)

# save hurdle predictions and distance predictions
write.csv(hurdle_predictions, "results/mating_range_expl_macmin/hurdle_predictions.csv")
write.csv(distance_predictions, "results/mating_range_expl_macmin/distance_predictions.csv")
write.csv(hurdle_predictions_int, "results/mating_range_expl_macmin/hurdle_predictions_int.csv")
write.csv(distance_predictions_int, "results/mating_range_expl_macmin/distance_predictions_int.csv")

# Now plot average across region

nd <- expand.grid(
  sex = levels(as.factor(reprod_all_bp2$sex)),
  age_group = levels(as.factor(reprod_all_bp2$age_group)),
  region = "avg"
)

# Predict positive distances
distance_predictions_int <- fitted(
  kin_model_brm2,
  re_formula  ~ (1 | region),
  allow_new_levels = TRUE,
  newdata = nd,
  dpar = "mu",  # Hurdle Gamma's mean of positive values
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Predict hurdle probabilities (probability of zero distances)
hurdle_predictions_int <- fitted(
  kin_model_brm2,
  re_formula  ~ (1 | region),
  allow_new_levels = TRUE,
  newdata = nd,
  dpar = "hu",  # Hurdle probability (probability of zero)
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Replace F and M with Women and Men
distance_predictions_int <- distance_predictions_int %>%
  mutate(sex = recode(sex, "F" = "Women", "M" = "Men"))

hurdle_predictions_int <- hurdle_predictions_int %>%
  mutate(sex = recode(sex, "F" = "Women", "M" = "Men"))

# save as csv
write.csv(hurdle_predictions_int, "results/mating_range_expl_macmin/hurdle_predictions_int_average_regions.csv")
write.csv(distance_predictions_int, "results/mating_range_expl_macmin/distance_predictions_int_average_regions.csv")

# Define the colors
line_colors <- c("Women" = MetPalettes$Isfahan1[[1]][4], 
                 "Men" = MetPalettes$Isfahan1[[1]][6])
fill_colors <- line_colors  # Use the same for ribbons
#line_colors <- scales::muted(fill_colors, l = 50) 

# Plot positive distances
distance_plot <- distance_predictions_int %>%
  ggplot(aes(x = age_group, y = Estimate / 1000, color = sex, group = sex)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = sex), 
              alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = sex), 
              alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  labs(
    x = "Age group",
    y = "Distance from mother's residence (km)"
  ) +
  ggtitle("Distances by Sex and Age Group") +
  scale_color_manual(values = line_colors) +  # Line colors
  scale_fill_manual(values = fill_colors) +   # Ribbon colors
  coord_cartesian(ylim = c(0, 150)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )

# Display the updated plot
distance_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_positive_distances_int_average_region.png", plot = distance_plot, width = 8, height = 7)

# Plot probability of zero distances
hurdle_plot <- hurdle_predictions_int %>%
  ggplot(aes(x = age_group, y = Estimate, color = sex, group = sex)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = sex), alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = sex), alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  labs(
    x = "Age group",
    y = "Probability of living with mother"
  ) +
  ggtitle("Average across regions") +
  coord_cartesian(ylim = c(0, 0.9)) +
  scale_color_manual(values = line_colors) +  # Line colors
  scale_fill_manual(values = fill_colors) +   # Ribbon colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )

# Display and save the updated plot
hurdle_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_zero_distance_probabilities_int_average_regions.png", 
                plot = hurdle_plot, width = 8, height = 7)

# Predict positive distances
distance_predictions <- fitted(
  kin_model_brm1b,
  re_formula  ~ (1 | region),
  allow_new_levels = TRUE,
  newdata = nd,
  dpar = "mu",  # Hurdle Gamma's mean of positive values
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Predict hurdle probabilities (probability of zero distances)
hurdle_predictions <- fitted(
  kin_model_brm1b,
  re_formula  ~ (1 | region),
  allow_new_levels = TRUE,
  newdata = nd,
  dpar = "hu",  # Hurdle probability (probability of zero)
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Replace F and M with Women and Men
distance_predictions <- distance_predictions %>%
  mutate(sex = recode(sex, "F" = "Women", "M" = "Men"))

hurdle_predictions <- hurdle_predictions %>%
  mutate(sex = recode(sex, "F" = "Women", "M" = "Men"))

# Plot positive distances
distance_plot <- distance_predictions %>%
  ggplot(aes(x = age_group, y = Estimate / 1000, color = sex, group = sex)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = sex), 
              alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = sex), 
              alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  labs(
    x = "Age group",
    y = "Distance from mother's residence (km)"
  ) +
  ggtitle("Average across regions") +
  scale_color_manual(values = line_colors) +  # Line colors
  scale_fill_manual(values = fill_colors) +   # Ribbon colors
  coord_cartesian(ylim = c(0, 150)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )

# Display the updated plot
distance_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_positive_distances_average_region.png", plot = distance_plot, width = 8, height = 7)

# Plot probability of zero distances
hurdle_plot <- hurdle_predictions %>%
  ggplot(aes(x = age_group, y = Estimate, color = sex, group = sex)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = sex), alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = sex), alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  labs(
    x = "Age group",
    y = "Probability of living with mother"
  ) +
  ggtitle("Average across regions") +
  coord_cartesian(ylim = c(0, 0.9)) +
  scale_color_manual(values = line_colors) +  # Line colors
  scale_fill_manual(values = fill_colors) +   # Ribbon colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )

# Display and save the updated plot
hurdle_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_zero_distance_probabilities_average_regions.png", 
                plot = hurdle_plot, width = 8, height = 7)

# save as csv
write.csv(hurdle_predictions, "results/mating_range_expl_macmin/hurdle_predictions_average_regions.csv")
write.csv(distance_predictions, "results/mating_range_expl_macmin/distance_predictions_average_regions.csv")


###################################################################################
# Now father's residence

# Start modeling
# We will use Hurdle models to account for the zero-inflated nature of the data - and
# model separately the probability of living witha parent to the distance to them.
m7.prior_hurdle <-   c(
  # Intercept for Positive Gamma Component
  prior(student_t(3, 4, 2), class = Intercept),  
  # Intercept for Hurdle Component (logit-scale)
  prior(normal(1, 0.5), class = Intercept, dpar = "hu"),  
  # Fixed Effects for Gamma and Hurdle Components
  prior(normal(0, 1.5), class = b),  
  prior(normal(0, 1.5), class = b, dpar = "hu"),  
  # Standard Deviations for Random Effects
  prior(student_t(3, 0, 2), class = sd),  
  prior(student_t(3, 0, 2), class = sd, dpar = "hu"),  
  # Shape Parameter for Gamma Distribution
  prior(normal(1, 0.1), class = shape)
)

formula_m5 <- bf(
  dist_living_dad_living ~ age_group + sex + 
    (1 + age_group + sex | region) +           # Random intercept and slopes by region
    (1 | region:residence_camp),               # Nested random intercept for residence_camp
  family = hurdle_gamma(link = "log"),
  hu ~ age_group + sex +                       # Fixed effects in the hurdle part
    (1 + age_group + sex | region)          # Random intercepts and slopes by region
)

formula_m5b <- bf(
  dist_living_dad_living ~ age_group + sex + 
    (1 +  sex | region) +           # Random intercept and slopes by region
    (1 | region:residence_camp),               # Nested random intercept for residence_camp
  family = hurdle_gamma(link = "log"),
  hu ~ age_group + sex +                       # Fixed effects in the hurdle part
    (1  + sex | region)       
)

formula_m5c <- bf(
  dist_living_dad_living ~ age_group + sex + 
    (1  | region) +           # Random intercept and slopes by region
    (1 | region:residence_camp),               # Nested random intercept for residence_camp
  family = hurdle_gamma(link = "log"),
  hu ~ age_group + sex +                       # Fixed effects in the hurdle part
    (1 | region)       
)

formula_m6 <- bf(
  dist_living_dad_living ~ age_group * sex +   # Interaction of age_group and sex
    (1 | region) +
    (1 | region:residence_camp),               # Nested random intercept for residence_camp
  family = hurdle_gamma(link = "log"),
  hu ~ age_group*sex +                       # Fixed effects in the hurdle part
    (1  | region)             # Nested random intercept for residence_ca
)

formula_m6b <- bf(
  dist_living_dad_living ~ age_group * sex +   # Interaction of age_group and sex
    (1 + sex | region) +
    (1 | region:residence_camp),               # Nested random intercept for residence_camp
  family = hurdle_gamma(link = "log"),
  hu ~ age_group*sex +                       # Fixed effects in the hurdle part
    (1 + sex | region)       
)

formula_m7 <- bf(
  dist_living_dad_living ~ age_group * sex +   # Interaction of age_group and sex
    (1 + age_group + sex | region) +          # Random slopes for age and sex by region
    (1 | region:residence_camp),               # Nested random intercept for residence_camp
  family = hurdle_gamma(link = "log"),
  hu ~ age_group*sex +                       # Fixed effects in the hurdle part
    (1 + age_group + sex | region)       
)


formula_m8 <- bf(
  dist_living_dad_living ~ age_group*sex +
    (1 + age_group + sex | region) +
    (1 | region:residence_camp),
  family = hurdle_lognormal()
)

formula_m9 <- bf(
  dist_living_dad_living ~ age_group*sex +
    (1 + sex | region) +
    (1 | region:residence_camp),
  family = hurdle_lognormal()
)


# Model 1 
kin_model_brm1b_dad <- brm(
  formula = formula_m5b,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.99),
  prior = m7.prior_hurdle
)

kin_model_brm1c_dad <- brm(
  formula = formula_m5c,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.99),
  prior = m7.prior_hurdle
)


kin_model_brm2b_dad <- brm(
  formula = formula_m6,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.99),
  prior = m7.prior_hurdle
)

kin_model_brm2c_dad <- brm(
  formula = formula_m6b,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.99),
  prior = m7.prior_hurdle
)

# Save all models as .rds
saveRDS(kin_model_brm1b_dad, "models/distance_to_kin_model1b_dad.rds")
saveRDS(kin_model_brm2b_dad, "models/distance_to_kin_model2b_dad.rds")
saveRDS(kin_model_brm1c_dad, "models/distance_to_kin_model1c_dad.rds")
saveRDS(kin_model_brm2c_dad, "models/distance_to_kin_model2c_dad.rds")

# load models
kin_model_brm1b_dad <- readRDS("models/distance_to_kin_model1b_dad.rds")
kin_model_brm2b_dad <- readRDS("models/distance_to_kin_model2b_dad.rds")
kin_model_brm1c_dad <- readRDS("models/distance_to_kin_model1c_dad.rds")
kin_model_brm2c_dad <- readRDS("models/distance_to_kin_model2c_dad.rds")

# compare models 
loo_model2b_dad <- loo(kin_model_brm2b_dad)
loo_model1b_dad <- loo(kin_model_brm1b_dad)
loo_model2c_dad <- loo(kin_model_brm2c_dad)
loo_model1c_dad <- loo(kin_model_brm1c_dad)

comparison <- loo_compare(loo_model2b_dad, loo_model1b_dad, loo_model2c_dad, loo_model1c_dad)
comparison # Gamma distribution does better

# save comparisons
write.csv(comparison, "results/mating_range_expl_macmin/comparison_dad.csv")


## Prepare plotting

# Prepare data for predictions
region_levels <- unique(reprod_all_bp2$region)

# Expand nd to include regions
nd <- expand.grid(
  sex = levels(as.factor(reprod_all_bp2$sex)),
  age_group = levels(as.factor(reprod_all_bp2$age_group)),
  region = region_levels
)

# Predict positive distances
distance_predictions <- fitted(
  kin_model_brm1b_dad,
  allow_new_levels = TRUE, 
  re_formula = ~ (1  | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  dpar = "mu",  # Hurdle Gamma's mean of positive values
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Predict hurdle probabilities (probability of zero distances)
hurdle_predictions <- fitted(
  kin_model_brm1b_dad,
  allow_new_levels = TRUE, 
  re_formula = ~ (1   | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  dpar = "hu",  # Hurdle probability (probability of zero)
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Plot positive distances
distance_plot <- distance_predictions %>%
  ggplot(aes(x = age_group, y = Estimate / 1000, color = region, group = region)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = region), 
              alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = region), 
              alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  facet_wrap(~ sex) +
  labs(
    x = "Age group",
    y = "Distance from father's residence (km)"
  ) +
  ggtitle("Distances by region") +
  scale_color_manual(values = region_darker_colors) +  # Central line colors
  scale_fill_manual(values = region_colors) +         # Ribbon colors
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

distance_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_positive_distances_dad.png", plot = distance_plot, width = 10, height = 7)

# Plot probability of zero distances
hurdle_plot <- hurdle_predictions %>%
  ggplot(aes(x = age_group, y = Estimate, color = region, group = region)) +
  geom_line(size = 1, alpha = 1) +  # Line for each region
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = region), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~ sex) +
  labs(
    x = "Age group",
    y = "Probability of living with father"
  ) +
  ggtitle("Probability of living with father") +
  coord_cartesian(ylim = c(0, 0.9)) +
  scale_color_manual(values = region_darker_colors) +
  scale_fill_manual(values = region_colors) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )
hurdle_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_zero_distance_probabilities_dad.png", plot = hurdle_plot, width = 10, height = 7)

# Predict positive distances
distance_predictions_int <- fitted(
  kin_model_brm2c_dad,
  allow_new_levels = TRUE, 
  re_formula = ~ (1 +sex | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  dpar = "mu",  # Hurdle Gamma's mean of positive values
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Predict hurdle probabilities (probability of zero distances)
hurdle_predictions_int <- fitted(
  kin_model_brm2c_dad,
  allow_new_levels = TRUE, 
  re_formula = ~ (1  +sex | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  dpar = "hu",  # Hurdle probability (probability of zero)
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Plot positive distances
distance_plot <- distance_predictions_int %>%
  ggplot(aes(x = age_group, y = Estimate / 1000, color = region, group = region)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = region), 
              alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = region), 
              alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  facet_wrap(~ sex) +
  labs(
    x = "Age group",
    y = "Distance from father's residence (km)"
  ) +
  ggtitle("Distances by region") +
  scale_color_manual(values = region_darker_colors) +  # Central line colors
  scale_fill_manual(values = region_colors) +         # Ribbon colors
  theme_minimal() +
  #coord_cartesian(ylim = c(0, 301)) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

distance_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_positive_distances_int_dad.png", plot = distance_plot, width = 10, height = 7)

# Plot probability of zero distances
hurdle_plot <- hurdle_predictions_int %>%
  ggplot(aes(x = age_group, y = Estimate, color = region, group = region)) +
  geom_line(size = 1, alpha = 1) +  # Line for each region
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = region), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~ sex) +
  labs(
    x = "Age group",
    y = "Probability of living with father"
  ) +
  ggtitle("Probability of living with father") +
  coord_cartesian(ylim = c(0, 0.9)) +
  scale_color_manual(values = region_darker_colors) +
  scale_fill_manual(values = region_colors) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )
hurdle_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_zero_distance_probabilities_int_dad.png", plot = hurdle_plot, width = 10, height = 7)

# save hurdle predictions and distance predictions
write.csv(hurdle_predictions, "results/mating_range_expl_macmin/hurdle_predictions_dad.csv")
write.csv(distance_predictions, "results/mating_range_expl_macmin/distance_predictions_dad.csv")
write.csv(hurdle_predictions_int, "results/mating_range_expl_macmin/hurdle_predictions_int_dad.csv")
write.csv(distance_predictions_int, "results/mating_range_expl_macmin/distance_predictions_int_dad.csv")

# Now plot average across region

nd <- expand.grid(
  sex = levels(as.factor(reprod_all_bp2$sex)),
  age_group = levels(as.factor(reprod_all_bp2$age_group)),
  region = "avg"
)

# Predict positive distances
distance_predictions_int <- fitted(
  kin_model_brm2b_dad,
  re_formula   ~ (1 | region),
  allow_new_levels = TRUE,
  newdata = nd,
  dpar = "mu",  # Hurdle Gamma's mean of positive values
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Predict hurdle probabilities (probability of zero distances)
hurdle_predictions_int <- fitted(
  kin_model_brm2b_dad,
  re_formula  ~ (1 | region),
  allow_new_levels = TRUE,
  newdata = nd,
  dpar = "hu",  # Hurdle probability (probability of zero)
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Replace F and M with Women and Men
distance_predictions_int <- distance_predictions_int %>%
  mutate(sex = recode(sex, "F" = "Women", "M" = "Men"))

hurdle_predictions_int <- hurdle_predictions_int %>%
  mutate(sex = recode(sex, "F" = "Women", "M" = "Men"))

# save as csv
write.csv(hurdle_predictions_int, "results/mating_range_expl_macmin/hurdle_predictions_int_average_regions_dad.csv")
write.csv(distance_predictions_int, "results/mating_range_expl_macmin/distance_predictions_int_average_regions_dad.csv")

# Define the colors
line_colors <- c("Women" = MetPalettes$Isfahan1[[1]][4], 
                 "Men" = MetPalettes$Isfahan1[[1]][6])
fill_colors <- line_colors  # Use the same for ribbons
#line_colors <- scales::muted(fill_colors, l = 50) 

# Plot positive distances
distance_plot <- distance_predictions_int %>%
  ggplot(aes(x = age_group, y = Estimate / 1000, color = sex, group = sex)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = sex), 
              alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = sex), 
              alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  labs(
    x = "Age group",
    y = "Distance from father's residence (km)"
  ) +
  ggtitle("Average across regions") +
  coord_cartesian(ylim = c(0, 150)) +
  scale_color_manual(values = line_colors) +  # Line colors
  scale_fill_manual(values = fill_colors) +   # Ribbon colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )

# Display the updated plot
distance_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_positive_distances_int_average_region_dad.png", plot = distance_plot, width = 8, height = 7)

# Plot probability of zero distances
hurdle_plot <- hurdle_predictions_int %>%
  ggplot(aes(x = age_group, y = Estimate, color = sex, group = sex)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = sex), alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = sex), alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  labs(
    x = "Age group",
    y = "Probability of living with father"
  ) +
  ggtitle("Average across regions") +
  scale_color_manual(values = line_colors) +  # Line colors
  scale_fill_manual(values = fill_colors) +   # Ribbon colors
  coord_cartesian(ylim = c(0, 0.9)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )

# Display and save the updated plot
hurdle_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_zero_distance_probabilities_int_average_regions_dad.png", 
                plot = hurdle_plot, width = 8, height = 7)

# Predict positive distances
distance_predictions <- fitted(
  kin_model_brm1b_dad,
  re_formula ~ (1 | region),
  allow_new_levels = TRUE,
  newdata = nd,
  dpar = "mu",  # Hurdle Gamma's mean of positive values
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Predict hurdle probabilities (probability of zero distances)
hurdle_predictions <- fitted(
  kin_model_brm1b_dad,
  re_formula ~ (1 | region),
  allow_new_levels = TRUE,
  newdata = nd,
  dpar = "hu",  # Hurdle probability (probability of zero)
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Replace F and M with Women and Men
distance_predictions <- distance_predictions %>%
  mutate(sex = recode(sex, "F" = "Women", "M" = "Men"))

hurdle_predictions <- hurdle_predictions %>%
  mutate(sex = recode(sex, "F" = "Women", "M" = "Men"))

# save as csv
write.csv(hurdle_predictions, "results/mating_range_expl_macmin/hurdle_predictions_average_region_dad.csv")
write.csv(distance_predictions, "results/mating_range_expl_macmin/distance_predictions_average_region_dad.csv")

# Plot positive distances
distance_plot <- distance_predictions %>%
  ggplot(aes(x = age_group, y = Estimate / 1000, color = sex, group = sex)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = sex), 
              alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = sex), 
              alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  labs(
    x = "Age group",
    y = "Distance from father's residence (km)"
  ) +
  ggtitle("Average across regions") +
  coord_cartesian(ylim = c(0, 150)) +
  scale_color_manual(values = line_colors) +  # Line colors
  scale_fill_manual(values = fill_colors) +   # Ribbon colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )

# Display the updated plot
distance_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_positive_distances_average_region_dad.png", plot = distance_plot, width = 8, height = 7)

# Plot probability of zero distances
hurdle_plot <- hurdle_predictions %>%
  ggplot(aes(x = age_group, y = Estimate, color = sex, group = sex)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = sex), alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = sex), alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  labs(
    x = "Age group",
    y = "Probability of living with father"
  ) +
  ggtitle("Average across regions") +
  scale_color_manual(values = line_colors) +  # Line colors
  scale_fill_manual(values = fill_colors) +   # Ribbon colors
  theme_minimal() +
  # add y limit between 0 and 0.9
  coord_cartesian(ylim = c(0, 0.9)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )

# Display and save the updated plot
hurdle_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_zero_distance_probabilities_average_regions_dad.png", 
                plot = hurdle_plot, width = 8, height = 7)

###################################################################################

# Now models for the distance at which they live from their BP

# Start modeling
# We will use Hurdle models to account for the zero-inflated nature of the data - and
# model separately the probability of living witha parent to the distance to them.
priors_hurdle_gamma <- c(
  # Intercept for Gamma Component (Positive Values, log-scale)
  prior(student_t(3, 4.5, 2), class = Intercept),  
  # Fixed Effects for Gamma Component
  prior(normal(0, 2), class = b),  
  # Intercept for Hurdle Component (Probability of Zero, logit-scale)
  prior(normal(1, 2), class = Intercept, dpar = "hu"),  
  # Fixed Effects for Hurdle Component
  prior(normal(0, 1), class = b, dpar = "hu"),  
  # Standard Deviations for Random Effects in Gamma Component
  prior(student_t(3, 0, 2), class = sd),  
  # Standard Deviations for Random Effects in Hurdle Component
  prior(student_t(3, 0, 2), class = sd, dpar = "hu"),  
  # Correlation Structure for Random Slopes in Gamma and Hurdle Components
  prior(lkj(2), class = cor),  
  # Shape Parameter of Gamma Distribution
  prior(normal(1, 0.2), class = shape)
)

priors_hurdle_broad <- c(
  # Intercept for Gamma Component (Positive Values, log-scale)
  prior(student_t(3, 4.5, 2), class = Intercept),  
  # Fixed Effects for Gamma Component
  prior(normal(0, 2), class = b),  
  # Intercept for Hurdle Component (Probability of Zero, logit-scale)
  prior(student_t(3, 0, 2), class = Intercept, dpar = "hu"), 
  # Fixed Effects for Hurdle Component
  prior(normal(0, 2), class = b, dpar = "hu"),  
  # Standard Deviations for Random Effects in Gamma Component
  prior(student_t(3, 0, 2), class = sd),  
  # Standard Deviations for Random Effects in Hurdle Component
  prior(student_t(3, 0, 2), class = sd, dpar = "hu"),  
  # Correlation Structure for Random Slopes in Gamma and Hurdle Components
  prior(lkj(1), class = cor),  
  # Shape Parameter of Gamma Distribution
  prior(normal(2,1), class = shape)
)
priors_hurdle_gamma_ri <- c(
  # Intercept for Gamma Component (Positive Values, log-scale)
  prior(student_t(3, 4.5, 2), class = Intercept),  
  # Fixed Effects for Gamma Component
  prior(normal(0, 2), class = b),  
  # Intercept for Hurdle Component (Probability of Zero, logit-scale)
  prior(normal(1, 2), class = Intercept, dpar = "hu"),  
  # Fixed Effects for Hurdle Component
  prior(normal(0, 1), class = b, dpar = "hu"),  
  # Standard Deviations for Random Effects in Gamma Component
  prior(student_t(3, 0, 2), class = sd),  
  # Standard Deviations for Random Effects in Hurdle Component
  prior(student_t(3, 0, 2), class = sd, dpar = "hu"),  
  # Shape Parameter of Gamma Distribution
  prior(normal(1, 0.2), class = shape)
)


formula_m5b <- bf(
  dist_living_own_bp ~ age_group + sex + 
    (1 +  sex | region) +           # Random intercept and slopes by region
    (1 | region:residence_camp),               # Nested random intercept for residence_camp
  family = hurdle_gamma(link = "log"),
  hu ~ age_group + sex +                       # Fixed effects in the hurdle part
    (1  + sex | region)       
)

formula_m5c <- bf(
  dist_living_own_bp ~ age_group + sex + 
    (1  | region) +           # Random intercept by region
    (1 | region:residence_camp),               # Nested random intercept for residence_camp
  family = hurdle_gamma(link = "log"),
  hu ~ age_group + sex +                       # Fixed effects in the hurdle part
    (1  | region)       
)


formula_m6b <- bf(
  dist_living_own_bp ~ age_group * sex +   # Interaction of age_group and sex
    (1 + sex | region) +
    (1 | region:residence_camp),               # Nested random intercept for residence_camp
  family = hurdle_gamma(link = "log"),
  hu ~ age_group*sex +                       # Fixed effects in the hurdle part
    (1 + sex | region)       
)

formula_m6c <- bf(
  dist_living_own_bp ~ age_group * sex +   # Interaction of age_group and sex
    (1  | region) +
    (1 | region:residence_camp),               # Nested random intercept for residence_camp
  family = hurdle_gamma(link = "log"),
  hu ~ age_group*sex +                       # Fixed effects in the hurdle part
    (1  | region)       
)


# Model 1 
kin_model_brm1b_own_bp <- brm(
  formula = formula_m5b,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.99),
  prior = priors_hurdle_gamma,
  cores=6
)

kin_model_brm1d_own_bp <- brm(
  formula = formula_m5b,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.99),
  prior = priors_hurdle_broad,
  cores=6
)

# Random intercept model
kin_model_brm1c_own_bp <- brm(
  formula = formula_m5c,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.99),
  prior = priors_hurdle_gamma_ri,
  cores=6
)

kin_model_brm2b_own_bp <- brm(
  formula = formula_m6b,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.99),
  prior = priors_hurdle_broad,
)

# Random intercept
kin_model_brm2c_own_bp <- brm(
  formula = formula_m6c,
  data = reprod_all_bp2,
  warmup = 1000,
  iter = 5000,
  chains = 3,
  control = list(adapt_delta = 0.99),
  prior = priors_hurdle_gamma_ri,
)

# Save all models as .rds
saveRDS(kin_model_brm1b_own_bp, "models/distance_to_kin_model1b_own_bp.rds")
saveRDS(kin_model_brm2b_own_bp, "models/distance_to_kin_model2b_own_bp.rds")
saveRDS(kin_model_brm1c_own_bp, "models/distance_to_kin_model1c_own_bp.rds")
saveRDS(kin_model_brm2c_own_bp, "models/distance_to_kin_model2c_own_bp.rds")
saveRDS(kin_model_brm1d_own_bp, "models/distance_to_kin_model1d_own_bp.rds")

# load all modes
kin_model_brm1b_own_bp <- readRDS("models/distance_to_kin_model1b_own_bp.rds")
kin_model_brm2b_own_bp <- readRDS("models/distance_to_kin_model2b_own_bp.rds")
kin_model_brm1c_own_bp <- readRDS("models/distance_to_kin_model1c_own_bp.rds")
kin_model_brm2c_own_bp <- readRDS("models/distance_to_kin_model2c_own_bp.rds")
kin_model_brm1d_own_bp <- readRDS("models/distance_to_kin_model1d_own_bp.rds")

# compare models 
loo_model2b_own_bp <- loo(kin_model_brm2b_own_bp)
loo_model1b_own_bp <- loo(kin_model_brm1b_own_bp)
loo_model1c_own_bp <- loo(kin_model_brm1c_own_bp)
loo_model2c_own_bp <- loo(kin_model_brm2c_own_bp)
loo_model1d_own_bp <- loo(kin_model_brm1d_own_bp)


comparison <- loo_compare(loo_model2b_own_bp, loo_model1d_own_bp,loo_model1b_own_bp, 
                          loo_model1c_own_bp, loo_model2c_own_bp)

comparison 
# save comparison
write.csv(comparison, "results/mating_range_expl_macmin/comparison_distance_to_kin_own_bp.csv")

## Prepare plotting

# Prepare data for predictions
region_levels <- unique(reprod_all_bp2$region)

# Expand nd to include regions
nd <- expand.grid(
  sex = levels(as.factor(reprod_all_bp2$sex)),
  age_group = levels(as.factor(reprod_all_bp2$age_group)),
  region = region_levels
)

# Predict positive distances
distance_predictions <- fitted(
  kin_model_brm1b_own_bp,
  allow_new_levels = TRUE, 
  re_formula = ~ (1 +sex | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  dpar = "mu",  # Hurdle Gamma's mean of positive values
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Predict hurdle probabilities (probability of zero distances)
hurdle_predictions <- fitted(
  kin_model_brm1b_own_bp,
  allow_new_levels = TRUE, 
  re_formula = ~ (1  +sex | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  dpar = "hu",  # Hurdle probability (probability of zero)
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Plot positive distances
distance_plot <- distance_predictions %>%
  ggplot(aes(x = age_group, y = Estimate / 1000, color = region, group = region)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = region), 
              alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = region), 
              alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  facet_wrap(~ sex) +
  labs(
    x = "Age group",
    y = "Distance from birthplace (km)"
  ) +
  ggtitle("Distances by region") +
  scale_color_manual(values = region_darker_colors) +  # Central line colors
  scale_fill_manual(values = region_colors) +         # Ribbon colors
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

distance_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_positive_distances_own_bp.png", plot = distance_plot, width = 10, height = 7)

# Plot probability of zero distances
hurdle_plot <- hurdle_predictions %>%
  ggplot(aes(x = age_group, y = Estimate, color = region, group = region)) +
  geom_line(size = 1, alpha = 1) +  # Line for each region
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = region), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~ sex) +
  labs(
    x = "Age group",
    y = "Probability of living in birthplace"
  ) +
  ggtitle("Probability of living in birthplace") +
  #coord_cartesian(ylim = c(0, 0.9)) +
  scale_color_manual(values = region_darker_colors) +
  scale_fill_manual(values = region_colors) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )
hurdle_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_zero_distance_probabilities_own_bp.png", plot = hurdle_plot, width = 10, height = 7)

# Predict positive distances
distance_predictions_int <- fitted(
  kin_model_brm2b_own_bp,
  allow_new_levels = TRUE, 
  re_formula = ~ (1 +sex | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  dpar = "mu",  # Hurdle Gamma's mean of positive values
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Predict hurdle probabilities (probability of zero distances)
hurdle_predictions_int <- fitted(
  kin_model_brm2b_own_bp,
  allow_new_levels = TRUE, 
  re_formula = ~ (1  +sex | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  dpar = "hu",  # Hurdle probability (probability of zero)
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Plot positive distances
distance_plot <- distance_predictions_int %>%
  ggplot(aes(x = age_group, y = Estimate / 1000, color = region, group = region)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = region), 
              alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = region), 
              alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  facet_wrap(~ sex) +
  labs(
    x = "Age group",
    y = "Distance from birthplace (km)"
  ) +
  ggtitle("Distances by region") +
  scale_color_manual(values = region_darker_colors) +  # Central line colors
  scale_fill_manual(values = region_colors) +         # Ribbon colors
  theme_minimal() +
  #coord_cartesian(ylim = c(0, 301)) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

distance_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_positive_distances_int_own_bp.png", plot = distance_plot, width = 10, height = 7)

# Plot probability of zero distances
hurdle_plot <- hurdle_predictions_int %>%
  ggplot(aes(x = age_group, y = Estimate, color = region, group = region)) +
  geom_line(size = 1, alpha = 1) +  # Line for each region
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = region), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~ sex) +
  labs(
    x = "Age group",
    y = "Probability of living in birthplace"
  ) +
  ggtitle("Probability of living in birthplace") +
  #coord_cartesian(ylim = c(0, 0.9)) +
  scale_color_manual(values = region_darker_colors) +
  scale_fill_manual(values = region_colors) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )
hurdle_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_zero_distance_probabilities_int_own_bp.png", plot = hurdle_plot, width = 10, height = 7)

# save hurdle predictions and distance predictions
write.csv(hurdle_predictions, "results/mating_range_expl_macmin/hurdle_predictions_own_bp.csv")
write.csv(distance_predictions, "results/mating_range_expl_macmin/distance_predictions_own_bp.csv")
write.csv(hurdle_predictions_int, "results/mating_range_expl_macmin/hurdle_predictions_int_own_bp.csv")
write.csv(distance_predictions_int, "results/mating_range_expl_macmin/distance_predictions_int_own_bp.csv")

# Now plot average across region

nd <- expand.grid(
  sex = levels(as.factor(reprod_all_bp2$sex)),
  age_group = levels(as.factor(reprod_all_bp2$age_group)),
  region = "avg"
)

# Predict positive distances
distance_predictions_int <- fitted(
  kin_model_brm2b_own_bp,
  re_formula  ~ (1 | region),
  allow_new_levels = TRUE,
  newdata = nd,
  dpar = "mu",  # Hurdle Gamma's mean of positive values
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Predict hurdle probabilities (probability of zero distances)
hurdle_predictions_int <- fitted(
  kin_model_brm2b_own_bp,
  re_formula  ~ (1 | region),
  allow_new_levels = TRUE,
  newdata = nd,
  dpar = "hu",  # Hurdle probability (probability of zero)
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Replace F and M with Women and Men
distance_predictions_int <- distance_predictions_int %>%
  mutate(sex = recode(sex, "F" = "Women", "M" = "Men"))

hurdle_predictions_int <- hurdle_predictions_int %>%
  mutate(sex = recode(sex, "F" = "Women", "M" = "Men"))

# save predictions as csv
write.csv(hurdle_predictions_int, "results/mating_range_expl_macmin/hurdle_predictions_int_average_regions_own_bp.csv")
write.csv(distance_predictions_int, "results/mating_range_expl_macmin/distance_predictions_int_average_regions_own_bp.csv")

# Define the colors
line_colors <- c("Women" = MetPalettes$Isfahan1[[1]][4], 
                 "Men" = MetPalettes$Isfahan1[[1]][6])
fill_colors <- line_colors  # Use the same for ribbons
#line_colors <- scales::muted(fill_colors, l = 50) 

# Plot positive distances
distance_plot <- distance_predictions_int %>%
  ggplot(aes(x = age_group, y = Estimate / 1000, color = sex, group = sex)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = sex), 
              alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = sex), 
              alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  labs(
    x = "Age group",
    y = "Distance from birthplace (km)"
  ) +
  ggtitle("Average across regions") +
  #coord_cartesian(ylim = c(0, 301)) +
  scale_color_manual(values = line_colors) +  # Line colors
  scale_fill_manual(values = fill_colors) +   # Ribbon colors
  theme_minimal() +
  ylim(0,60)+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )

# Display the updated plot
distance_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_positive_distances_int_average_region_own_bp.png", plot = distance_plot, width = 8, height = 7)

# Plot probability of zero distances
hurdle_plot <- hurdle_predictions_int %>%
  ggplot(aes(x = age_group, y = Estimate, color = sex, group = sex)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = sex), alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = sex), alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  labs(
    x = "Age group",
    y = "Probability of living in birthplace"
  ) +
  ggtitle("Average across regions") +
  scale_color_manual(values = line_colors) +  # Line colors
  scale_fill_manual(values = fill_colors) +   # Ribbon colors
  coord_cartesian(ylim = c(0, 0.5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )

# Display and save the updated plot
hurdle_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_zero_distance_probabilities_int_average_regions_own_bp.png", 
                plot = hurdle_plot, width = 8, height = 7)

# Predict positive distances
distance_predictions <- fitted(
  kin_model_brm1b_own_bp,
  re_formula  ~ (1 | region),
  allow_new_levels = TRUE,
  newdata = nd,
  dpar = "mu",  # Hurdle Gamma's mean of positive values
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Predict hurdle probabilities (probability of zero distances)
hurdle_predictions <- fitted(
  kin_model_brm1b_own_bp,
  re_formula  ~ (1 | region),
  allow_new_levels = TRUE,
  newdata = nd,
  dpar = "hu",  # Hurdle probability (probability of zero)
  probs = c(0.025, 0.25, 0.75, 0.975, 0.10, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)

# Replace F and M with Women and Men
distance_predictions <- distance_predictions %>%
  mutate(sex = recode(sex, "F" = "Women", "M" = "Men"))

hurdle_predictions <- hurdle_predictions %>%
  mutate(sex = recode(sex, "F" = "Women", "M" = "Men"))

# save predictions as csv
write.csv(hurdle_predictions, "results/mating_range_expl_macmin/hurdle_predictions_average_regions_own_bp.csv")
write.csv(distance_predictions, "results/mating_range_expl_macmin/distance_predictions_average_regions_own_bp.csv")

# Plot positive distances
distance_plot <- distance_predictions %>%
  ggplot(aes(x = age_group, y = Estimate / 1000, color = sex, group = sex)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = sex), 
              alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = sex), 
              alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  labs(
    x = "Age group",
    y = "Distance from birthplace (km)"
  ) +
  ggtitle("Average across regions") +
  coord_cartesian(ylim = c(0, 100)) +
  scale_color_manual(values = line_colors) +  # Line colors
  scale_fill_manual(values = fill_colors) +   # Ribbon colors
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )

# Display the updated plot
distance_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_positive_distances_average_region_own_bp.png", plot = distance_plot, width = 8, height = 7)

# Plot probability of zero distances
hurdle_plot <- hurdle_predictions %>%
  ggplot(aes(x = age_group, y = Estimate, color = sex, group = sex)) +
  # 80% credible intervals
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = sex), alpha = 0.3, color = NA) +
  # 50% credible intervals
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = sex), alpha = 0.6, color = NA) +
  # Mean posterior estimates
  geom_line(size = 1.2) +
  labs(
    x = "Age group",
    y = "Probability of living in birthplace"
  ) +
  ggtitle("Average across regions") +
  scale_color_manual(values = line_colors) +  # Line colors
  scale_fill_manual(values = fill_colors) +   # Ribbon colors
  theme_minimal() +
  # add y limit between 0 and 0.9
  coord_cartesian(ylim = c(0, 0.5)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )

# Display and save the updated plot
hurdle_plot
ggplot2::ggsave("results/mating_range_expl_macmin/kin_model_zero_distance_probabilities_average_regions_own_bp.png", 
                plot = hurdle_plot, width = 8, height = 7)


