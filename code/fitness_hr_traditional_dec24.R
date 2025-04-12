#####################################################################
# Models exploring fitness consequences of hunter-gatherer mobility #
#####################################################################

library(ggplot2)
library(ggstatsplot)
library(brms)

fit_dists_all <- read.csv("results/half_range_good_include_all/exploration_range_max_dist_fitness.csv")

# we actually want to rescue new models with new HR calculations
dists_all <- read.csv("../analyses_sept23/results/half_range_traditional/exploration_range_max_dist_everyone.csv")

# Change median_dist and max_dist in fit_dists_all for values from dists_all
# for each ref
for (i in 1:nrow(fit_dists_all)) {
  for (j in 1:nrow(dists_all)) {
    if (fit_dists_all[i,3] == dists_all[j,3]) {
      fit_dists_all[i,10] <- dists_all[j,8]
      fit_dists_all[i,11] <- dists_all[j,9]
    }
  }
}

#fit_dists_bp <- read.csv("results/half_range_good_include_all/exploration_range_max_dist_fitness_from_bp.csv")

# get types of camps
regions <- read.csv("data/final/regions_locations_2023_complete_fixed.csv",  encoding = "UTF-8")
regions_sub <- subset(regions, select=c("camp_region", "loc", "region","people_number_2022"))
set_type <- vector(length=nrow(regions_sub))
for (i in 1:nrow(regions_sub)) {
  if(regions_sub[i,1] == "R")  {
    set_type[i] <- "village" 
  } else if (regions_sub[i,1] == "C" && regions_sub[i,2] == "V") {
    set_type[i] <- "village_camp"   
  } else if (regions_sub[i,1] == "C" && regions_sub[i,2] == "R") {
    set_type[i] <- "village_camp"   
  } else if (regions_sub[i,1] == "C" && regions_sub[i,2] == "F") {
    set_type[i] <- "forest_camp"  
  }
}

regions_sub <- cbind(regions_sub, set_type)
colnames(regions_sub)[3] <- "residence_camp"
fit_dists_all_2 <- left_join(fit_dists_all, regions_sub, by="residence_camp")

fit_dists_all_2 <- subset(fit_dists_all_2, fit_dists_all_2$age != "")
fit_dists_all_2 <- subset(fit_dists_all_2, fit_dists_all_2$age != "Enfant")
for (i in 1:nrow(fit_dists_all_2)) {
  if (fit_dists_all_2[i,7]==0){
    fit_dists_all_2[i,8] <- 0
  }
}
fit_dists_all_2 <- subset(fit_dists_all_2, fit_dists_all_2$children_alive != "")
fit_dists_all_2 <- subset(fit_dists_all_2, fit_dists_all_2$children_alive != "Erreur")
fit_dists_all_2$children_alive <- as.numeric(fit_dists_all_2$children_alive)
fit_dists_all_2 <- subset(fit_dists_all_2, fit_dists_all_2$sex != "")

# mean and variance are the same - we use Poisson distribution
mean(fit_dists_all_2$children_alive)
var(fit_dists_all_2$children_alive)

# plot 
fit_dists_all_2 <- subset(fit_dists_all_2, fit_dists_all_2$sex != "")

fit_dists_all_2$log_median_dist_std <- (log(fit_dists_all_2$median_dist) - mean(log(fit_dists_all_2$median_dist), na.rm=T)) / sd(log(fit_dists_all_2$median_dist), na.rm=T)
fit_dists_all_2$log_max_dist_std <- (log(fit_dists_all_2$max_dist) - mean(log(fit_dists_all_2$max_dist), na.rm=T)) / sd(log(fit_dists_all_2$max_dist), na.rm=T)

fit_dists_all_2$median_dist_std <- (fit_dists_all_2$median_dist - mean(fit_dists_all_2$median_dist, na.rm=T)) / sd(fit_dists_all_2$median_dist, na.rm=T)
fit_dists_all_2$max_dist_std <- (log(fit_dists_all_2$max_dist) - mean(log(fit_dists_all_2$max_dist), na.rm=T)) / sd(log(fit_dists_all_2$max_dist), na.rm=T)


# we will now include intercepts (nested)

m3pr.prior.stable <- c(
  # Intercept: Centered and Regularized
  prior(normal(0, 2), class = Intercept),  # Centered near observed -0.34
  # Fixed Effects (Coefficients): Tighter Regularization
  prior(normal(0, 2), class = b),        # Assumes smaller slopes
  # Random Effects Standard Deviations: Constrained and Regularized
  prior(student_t(3, 0, 1), class = sd),   # Moderate variability for group-level SDs
  # Shape Parameter for Negative Binomial: Regularized to Avoid Extremes
  prior(normal(3, 2), class = shape)       # Centered around a reasonable shape value
)

m3pr.prior.poisson <- c(
  # Intercept: Centered and Regularized
  prior(normal(0, 2), class = Intercept),  # Centered near observed mean of response
  
  # Fixed Effects (Coefficients): Tighter Regularization
  prior(normal(0, 1), class = b),        # Assumes smaller slopes
  
  # Random Effects Standard Deviations: Constrained and Regularized
  prior(student_t(3, 0, 1), class = sd)    # Moderate variability for group-level SDs
)

# random intercepts for region and residence_camp
formula_m6 <- bf(children_alive ~  log_median_dist_std + age 
                 + sex + (1 | region) +
                   (1 | region:residence_camp),
                 family = "negbinomial")

# random intercepts for region and residence_camp (int sex and age)
formula_m7 <- bf(children_alive ~  log_median_dist_std*sex + age + (1 | region) +
                   (1 | region:residence_camp),
                 family = "negbinomial")

# random intercepts for region and residence_camp
formula_m8 <- bf(children_alive ~  log_median_dist_std + age 
                 + sex + (1 + log_median_dist_std  | region) +
                   (1 | region:residence_camp),
                 family = "negbinomial")

# random intercepts for region and residence_camp (int sex and age)
formula_m9 <- bf(children_alive ~  log_median_dist_std*sex + age +
                   (1 + log_median_dist_std  | region) +
                   (1 | region:residence_camp),
                 family = "negbinomial")

formula_m10 <- bf(children_alive ~  log_median_dist_std*age + sex +
                    (1 + log_median_dist_std  | region) +
                    (1 | region:residence_camp),
                  family = "negbinomial")

fit_dists_adult <- subset(fit_dists_all_2, fit_dists_all_2$age!="Adolescent")

# fit models
fit_model_brm6b <- brm(formula=formula_m6, data=fit_dists_all_2,warmup=2000,
                       iter=10000, chains=3, control=list(adapt_delta=0.99), prior =m3pr.prior.stable)

fit_model_brm7 <- brm(formula=formula_m7, data=fit_dists_all_2,warmup=2000,
                      iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.stable)

fit_model_brm8 <- brm(formula=formula_m8, data=fit_dists_all_2,warmup=2000,
                      iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.stable)

fit_model_brm8b <- brm(formula=formula_m8, data=fit_dists_adult,warmup=2000,
                       iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.stable)


fit_model_brm9 <- brm(formula=formula_m9, data=fit_dists_all_2,warmup=2000,
                      iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.stable)

fit_model_brm10 <- brm(formula=formula_m10, data=fit_dists_all_2,warmup=2000,
                       iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.stable)


# compare models using loo
f_m6b <- add_criterion(fit_model_brm6b, "loo")
f_m7 <- add_criterion(fit_model_brm7, "loo")
f_m8 <- add_criterion(fit_model_brm8, "loo")
f_m9 <- add_criterion(fit_model_brm9, "loo")
f_m10 <- add_criterion(fit_model_brm10, "loo")

comparison <- loo_compare(f_m6b, f_m7, f_m8, f_m9, f_m10) %>% print(simplify = F)
model_weights(fit_model_brm6b, fit_model_brm7, fit_model_brm8, fit_model_brm9, fit_model_brm10, weights = "loo") %>% round(digits = 2)

# save comparison
write.csv(comparison, "results/fitness_hr_dec24_traditional/half_range_fitness_comparison.csv")

# save models
saveRDS(fit_model_brm6b, file = "models/half_range_fitness_model6b_traditional.rds")
saveRDS(fit_model_brm7, file = "models/half_range_fitness_model7_traditional.rds")
saveRDS(fit_model_brm8, file = "models/half_range_fitness_model8_traditional.rds")
saveRDS(fit_model_brm9, file = "models/half_range_fitness_model9_traditional.rds")
saveRDS(fit_model_brm10, file = "models/half_range_fitness_model10_traditional.rds")

# load models
fit_model_brm6b <- readRDS("models/half_range_fitness_model6b_traditional.rds")
fit_model_brm7 <- readRDS("models/half_range_fitness_model7_traditional.rds")
fit_model_brm8 <- readRDS("models/half_range_fitness_model8_traditional.rds")
fit_model_brm9 <- readRDS("models/half_range_fitness_model9_traditional.rds")
fit_model_brm10 <- readRDS("models/half_range_fitness_model10_traditional.rds")

# Now only with post reprod individuals to have complete fertility

fit_dists_pr <- subset(fit_dists_all_2, fit_dists_all_2$age=="Personne âgée")

# standardise median_dist within the pr dataset
fit_dists_pr$log_median_dist_std <- (log(fit_dists_pr$median_dist) - mean(log(fit_dists_pr$median_dist), na.rm=T)) / sd(log(fit_dists_pr$median_dist), na.rm=T)

# Model 8 for post reproductives
formula_m8_pr <- bf(children_alive ~  log_median_dist_std 
                    + sex + (1 + log_median_dist_std  | region) +
                      (1 | region:residence_camp),
                    family = "negbinomial")

fit_model_brm8_pr <- brm(formula=formula_m8_pr, data=fit_dists_pr,warmup=2000,
                         iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.stable)

# Model 6 for post reproductives
formula_m6_pr <- bf(children_alive ~  log_median_dist_std 
                    + sex + (1  | region) +
                      (1 | region:residence_camp),
                    family = "negbinomial")

fit_model_brm6_pr <- brm(formula=formula_m6_pr, data=fit_dists_pr,warmup=2000,
                         iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.stable)

# save as rds
saveRDS(fit_model_brm8_pr, file = "models/half_range_fitness_model8_pr_traditional.rds")
saveRDS(fit_model_brm6_pr, file = "models/half_range_fitness_model6_pr_traditional.rds")

# read in models
fit_model_brm6b <- readRDS("models/half_range_fitness_model6b_traditional.rds")
fit_model_brm7 <- readRDS("models/half_range_fitness_model7_traditional.rds")
fit_model_brm8 <- readRDS("models/half_range_fitness_model8_traditional.rds")
fit_model_brm9 <- readRDS("models/half_range_fitness_model9_traditional.rds")
fit_model_brm10 <- readRDS("models/half_range_fitness_model10_traditional.rds")
fit_model_brm8_pr <- readRDS("models/half_range_fitness_model8_pr_traditional.rds")
fit_model_brm6_pr <- readRDS("models/half_range_fitness_model6_pr_traditional.rds")

## Plot region level variation

# Prepare new data for predictions
region_levels <- unique(fit_dists_all_2$region)

# Expand the new data grid
nd <- expand.grid(
  log_median_dist_std = seq(-5, 2, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = region_levels
)


# Predict values for each region
region_predictions <- fitted(
  fit_model_brm6b, # best model random int
  allow_new_levels = TRUE,
  re_formula = ~ (1 + log_median_dist_std| region),
  sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    median_dist = exp((log_median_dist_std * sd(log(fit_dists_all_2$median_dist), na.rm = TRUE)) + 
                        mean(log(fit_dists_all_2$median_dist), na.rm = TRUE)),
    age = factor(
      age,
      levels = c("Adolescent", "Adulte", "Personne âgée"),
      labels = c("Adolescent", "Adult", "Old adult")),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")),
    region = factor(
      region,
      levels = c("Minganga", "Macao"),
      labels = c("Minganga", "Macao")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = median_dist/1000, y = Estimate, color = region, group = region)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = region), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~ age + sex, scales = "fixed", ncol = 2) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Half range (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  ylim(0, 8) +
xlim(2,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_half_range_across_regions_randomint.png", width = 7, height = 10)

nd <- expand.grid(
  log_median_dist_std = seq(-4, 2, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = "avg_region"
)

# Predict values for each region
region_predictions <- fitted(
  fit_model_brm6b, # best model
  #allow_new_levels = TRUE,
  re_formula = NA,
  #re_formula = NA,
  #sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    median_dist = exp((log_median_dist_std * sd(log(fit_dists_all_2$median_dist), na.rm = TRUE)) + 
                        mean(log(fit_dists_all_2$median_dist), na.rm = TRUE)),
    age = factor(
      age,
      levels = c("Adolescent", "Adulte", "Personne âgée"),
      labels = c("Adolescent", "Adult", "Old adult")),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = median_dist/1000, y = Estimate, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = sex), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = sex), alpha = 0.2, color = NA) +
  facet_wrap(~ age , scales = "fixed", ncol = 3) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Half range (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  #scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  ylim(0, 8)+
xlim(2,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_half_range_marginal_regions.png", width = 14, height = 5)


# Prepare new data for predictions
region_levels <- unique(fit_dists_all_2$region)

# Expand the new data grid
nd <- expand.grid(
  log_median_dist_std = seq(-5, 2, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = region_levels
)


# Predict values for each region
region_predictions <- fitted(
  fit_model_brm10, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 + log_median_dist_std| region),
  sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    median_dist = exp((log_median_dist_std * sd(log(fit_dists_all_2$median_dist), na.rm = TRUE)) + 
                        mean(log(fit_dists_all_2$median_dist), na.rm = TRUE)),
    age = factor(
      age,
      levels = c("Adolescent", "Adulte", "Personne âgée"),
      labels = c("Adolescent", "Adult", "Old adult")),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")),
    region = factor(
      region,
      levels = c("Minganga", "Macao"),
      labels = c("Minganga", "Macao")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = median_dist/1000, y = Estimate, color = region, group = region)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = region), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~ age + sex, scales = "fixed", ncol = 2) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Half range (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  ylim(0, 8) +
  xlim(2,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_half_range_across_regions.png", width = 7, height = 10)

nd <- expand.grid(
  log_median_dist_std = seq(-4, 2, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = "avg_region"
)

# Predict values for each region
region_predictions <- fitted(
  fit_model_brm10, # best model
  #allow_new_levels = TRUE,
  re_formula = NA,
  #re_formula = NA,
  #sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    median_dist = exp((log_median_dist_std * sd(log(fit_dists_all_2$median_dist), na.rm = TRUE)) + 
                        mean(log(fit_dists_all_2$median_dist), na.rm = TRUE)),
    age = factor(
      age,
      levels = c("Adolescent", "Adulte", "Personne âgée"),
      labels = c("Adolescent", "Adult", "Old adult")),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = median_dist/1000, y = Estimate, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = sex), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = sex), alpha = 0.2, color = NA) +
  facet_wrap(~ age , scales = "fixed", ncol = 3) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Half range (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  #scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  ylim(0, 8)+
  xlim(2,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_half_range_marginal_regions.png", width = 14, height = 5)

# Post reproductives
# Prepare new data for predictions
region_levels <- unique(fit_dists_all_2$region)

# Expand the new data grid
nd <- expand.grid(
  log_median_dist_std = seq(-5, 2, length.out = 100),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = region_levels
)


# Predict values for each region
region_predictions <- fitted(
  fit_model_brm8_pr, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 + log_median_dist_std| region),
  sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    median_dist = exp((log_median_dist_std * sd(log(fit_dists_pr$median_dist), na.rm = TRUE)) + 
                        mean(log(fit_dists_pr$median_dist), na.rm = TRUE)),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")),
    region = factor(
      region,
      levels = c("Minganga", "Macao"),
      labels = c("Minganga", "Macao")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = median_dist/1000, y = Estimate, color = region, group = region)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = region), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~  sex, scales = "fixed", ncol = 2) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Half range (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  #scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  ylim(0, 10) +
xlim(2,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_half_range_across_regions_pr.png", width = 10, height = 5)

nd <- expand.grid(
  log_median_dist_std = seq(-4, 2, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = "avg_region"
)

# Predict values for each region
region_predictions <- fitted(
  fit_model_brm8_pr, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 | region),
  #re_formula = NA,
  #sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    median_dist = exp((log_median_dist_std * sd(log(fit_dists_pr$median_dist), na.rm = TRUE)) + 
                        mean(log(fit_dists_pr$median_dist), na.rm = TRUE)),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = median_dist/1000, y = Estimate, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = sex), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = sex), alpha = 0.2, color = NA) +
  #facet_wrap(~ age , scales = "fixed", ncol = 3) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Half range (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  #scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  ylim(0, 10)+
  xlim(2,130) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )
#xlim(0,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_half_range_marginal_regions_pr.png", width = 7, height = 10)


# NOW POISSON

# random intercepts for region and residence_camp
formula_m6 <- bf(children_alive ~  log_median_dist_std + age 
                 + sex + (1 | region) +
                   (1 | region:residence_camp),
                 family = "poisson")

# random intercepts for region and residence_camp (int sex and age)
formula_m7 <- bf(children_alive ~  log_median_dist_std*sex + age + (1 | region) +
                   (1 | region:residence_camp),
                 family = "poisson")

# random intercepts for region and residence_camp
formula_m8 <- bf(children_alive ~  log_median_dist_std + age 
                 + sex + (1 + log_median_dist_std  | region) +
                   (1 | region:residence_camp),
                 family = "poisson")

# random intercepts for region and residence_camp (int sex and age)
formula_m9 <- bf(children_alive ~  log_median_dist_std*sex + age +
                   (1 + log_median_dist_std  | region) +
                   (1 | region:residence_camp),
                 family = "poisson")

formula_m10 <- bf(children_alive ~  log_median_dist_std*age + sex +
                    (1 + log_median_dist_std  | region) +
                    (1 | region:residence_camp),
                  family = "poisson")


# fit models

fit_model_brm6b <- brm(formula=formula_m6, data=fit_dists_all_2,warmup=2000,
                       iter=10000, chains=3, control=list(adapt_delta=0.99), prior =m3pr.prior.poisson)

fit_model_brm7 <- brm(formula=formula_m7, data=fit_dists_all_2,warmup=2000,
                      iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson)

fit_model_brm8 <- brm(formula=formula_m8, data=fit_dists_all_2,warmup=2000,
                      iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson)

fit_model_brm8b <- brm(formula=formula_m8, data=fit_dists_adult,warmup=2000,
                       iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson)


fit_model_brm9 <- brm(formula=formula_m9, data=fit_dists_all_2,warmup=2000,
                      iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson)

fit_model_brm10 <- brm(formula=formula_m10, data=fit_dists_all_2,warmup=2000,
                       iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson)


# compare models using loo
f_m6b <- add_criterion(fit_model_brm6b, "loo")
f_m7 <- add_criterion(fit_model_brm7, "loo")
f_m8 <- add_criterion(fit_model_brm8, "loo")
f_m9 <- add_criterion(fit_model_brm9, "loo")
f_m10 <- add_criterion(fit_model_brm10, "loo")

comparison <- loo_compare(f_m6b, f_m7,  f_m9,f_m8, f_m10) %>% print(simplify = F)
model_weights(fit_model_brm6b, fit_model_brm7, fit_model_brm9, fit_model_brm8,fit_model_brm10, weights = "loo") %>% round(digits = 2)

# save comparison
write.csv(comparison, "results/fitness_hr_dec24_traditional/half_range_fitness_comparison_poisson.csv")

# save models
saveRDS(fit_model_brm6b, file = "models/half_range_fitness_model6b_poisson_traditional.rds")
saveRDS(fit_model_brm7, file = "models/half_range_fitness_model7_poisson_traditional.rds")
saveRDS(fit_model_brm8, file = "models/half_range_fitness_model8_poisson_traditional.rds")
saveRDS(fit_model_brm9, file = "models/half_range_fitness_model9_poisson_traditional.rds")
saveRDS(fit_model_brm10, file = "models/half_range_fitness_model10_poisson_traditional.rds")


# # Model 6b for post reproductives
formula_m6_pr <- bf(children_alive ~  log_median_dist_std 
                    + sex + (1 + log_median_dist_std  | region) +
                      (1 | region:residence_camp),
                    family = "poisson")

formula_m8_pr <- bf(children_alive ~  log_median_dist_std 
                    + sex + (1  | region) +
                      (1 | region:residence_camp),
                    family = "poisson")

fit_model_brm6_pr <- brm(formula=formula_m6_pr, data=fit_dists_pr,warmup=2000,
                         iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson)

fit_model_brm8_pr <- brm(formula=formula_m8_pr, data=fit_dists_pr,warmup=2000,
                         iter=10000, chains=3, control=list(adapt_delta=0.99), prior =m3pr.prior.poisson)

# save as rds
saveRDS(fit_model_brm8_pr, file = "models/half_range_fitness_model8_pr_poisson_traditional.rds")
saveRDS(fit_model_brm6_pr, file = "models/half_range_fitness_model6_pr_poisson_traditional.rds")

# Read in models
fit_model_brm6b <- readRDS("models/half_range_fitness_model6b_poisson_traditional.rds")
fit_model_brm7 <- readRDS("models/half_range_fitness_model7_poisson_traditional.rds")
fit_model_brm8 <- readRDS("models/half_range_fitness_model8_poisson_traditional.rds")
fit_model_brm9 <- readRDS("models/half_range_fitness_model9_poisson_traditional.rds")
fit_model_brm10 <- readRDS("models/half_range_fitness_model10_poisson_traditional.rds")
fit_model_brm8_pr <- readRDS("models/half_range_fitness_model8_pr_poisson_traditional.rds")
fit_model_brm6_pr <- readRDS("models/half_range_fitness_model6_pr_poisson_traditional.rds")


# compare models using loo
f_m6_pr <- add_criterion(fit_model_brm6_pr, "loo")
f_m8_pr <- add_criterion(fit_model_brm8_pr, "loo")

comparison_pr <- loo_compare(f_m6_pr, f_m8_pr) %>% print(simplify = F)
write.csv(comparison_pr, "results/fitness_hr_dec24_traditional/half_range_fitness_comparison_pr_poisson.csv")

## Plot region level variation

# Prepare new data for predictions
region_levels <- unique(fit_dists_all_2$region)

# Expand the new data grid
nd <- expand.grid(
  log_median_dist_std = seq(-5, 2, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = region_levels
)


# Predict values for each region
region_predictions <- fitted(
  fit_model_brm10, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 + log_median_dist_std| region),
  sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    median_dist = exp((log_median_dist_std * sd(log(fit_dists_all_2$median_dist), na.rm = TRUE)) + 
                        mean(log(fit_dists_all_2$median_dist), na.rm = TRUE)),
    age = factor(
      age,
      levels = c("Adolescent", "Adulte", "Personne âgée"),
      labels = c("Adolescent", "Adult", "Old adult")),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")),
    region = factor(
      region,
      levels = c("Minganga", "Macao"),
      labels = c("Minganga", "Macao")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = median_dist/1000, y = Estimate, color = region, group = region)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = region), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~ age + sex, scales = "fixed", ncol = 2) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Half range (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  ylim(0, 8) +
xlim(2,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_half_range_across_regions_poisson.png", width = 7, height = 10)

# save_predictions
write.csv(region_predictions, "results/fitness_hr_dec24_traditional/predictions_half_range_across_regions_poisson.csv")

nd <- expand.grid(
  log_median_dist_std = seq(-4, 2, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = "avg_region"
)

# Predict values for each region
region_predictions <- fitted(
  fit_model_brm10, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 | region),
  #re_formula = NA,
  #sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    median_dist = exp((log_median_dist_std * sd(log(fit_dists_all_2$median_dist), na.rm = TRUE)) + 
                        mean(log(fit_dists_all_2$median_dist), na.rm = TRUE)),
    age = factor(
      age,
      levels = c("Adolescent", "Adulte", "Personne âgée"),
      labels = c("Adolescent", "Adult", "Old adult")),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = median_dist/1000, y = Estimate, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = sex), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = sex), alpha = 0.2, color = NA) +
  facet_wrap(~ age , scales = "fixed", ncol = 3) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Half range (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  #scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  ylim(0, 10)+
  xlim(5,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_half_range_marginal_regions_poisson.png", width = 12, height = 5)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24_traditional/predictions_half_range_marginal_regions_poisson.csv")

# Prepare new data for predictions
region_levels <- unique(fit_dists_all_2$region)

# Expand the new data grid
nd <- expand.grid(
  log_median_dist_std = seq(-5, 2, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = region_levels
)


# Predict values for each region
region_predictions <- fitted(
  fit_model_brm6b, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    median_dist = exp((log_median_dist_std * sd(log(fit_dists_all_2$median_dist), na.rm = TRUE)) + 
                        mean(log(fit_dists_all_2$median_dist), na.rm = TRUE)),
    age = factor(
      age,
      levels = c("Adolescent", "Adulte", "Personne âgée"),
      labels = c("Adolescent", "Adult", "Old adult")),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")),
    region = factor(
      region,
      levels = c("Minganga", "Macao"),
      labels = c("Minganga", "Macao")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = median_dist/1000, y = Estimate, color = region, group = region)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = region), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~ age + sex, scales = "fixed", ncol = 2) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Half range (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  ylim(0, 8) +
  xlim(2,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_half_range_across_regions_poisson_randomint.png", width = 7, height = 10)

# save_predictions
write.csv(region_predictions, "results/fitness_hr_dec24_traditional/predictions_half_range_across_regions_poisson_randomint.csv")

nd <- expand.grid(
  log_median_dist_std = seq(-4, 2, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = "avg_region"
)

# Predict values for each region
region_predictions <- fitted(
  fit_model_brm6b, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 | region),
  #re_formula = NA,
  #sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    median_dist = exp((log_median_dist_std * sd(log(fit_dists_all_2$median_dist), na.rm = TRUE)) + 
                        mean(log(fit_dists_all_2$median_dist), na.rm = TRUE)),
    age = factor(
      age,
      levels = c("Adolescent", "Adulte", "Personne âgée"),
      labels = c("Adolescent", "Adult", "Old adult")),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = median_dist/1000, y = Estimate, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = sex), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = sex), alpha = 0.2, color = NA) +
  facet_wrap(~ age , scales = "fixed", ncol = 3) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Half range (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  #scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  ylim(0, 8)+
  xlim(5,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_half_range_marginal_regions_poisson_randomint.png", width = 12, height = 5)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24_traditional/predictions_half_range_marginal_regions_poisson.csv")

# Prepare new data for predictions
region_levels <- unique(fit_dists_all_2$region)

# Expand the new data grid
nd <- expand.grid(
  log_median_dist_std = seq(-5, 2, length.out = 100),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = region_levels
)


# Predict values for each region
region_predictions <- fitted(
  fit_model_brm8_pr, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    median_dist = exp((log_median_dist_std * sd(log(fit_dists_pr$median_dist), na.rm = TRUE)) + 
                        mean(log(fit_dists_pr$median_dist), na.rm = TRUE)),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")),
    region = factor(
      region,
      levels = c("Minganga", "Macao"),
      labels = c("Minganga", "Macao")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = median_dist/1000, y = Estimate, color = region, group = region)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = region), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~  sex, scales = "fixed", ncol = 2) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Half range (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  #scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  ylim(0, 8) +
  xlim(2,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_half_range_across_regions_pr_poisson_randomint.png", width = 10, height = 5)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24_traditional/predictions_half_range_across_regions_pr_poisson_randomint.csv")

nd <- expand.grid(
  log_median_dist_std = seq(-4, 2, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = "avg_region"
)

# Predict values for each region
region_predictions <- fitted(
  fit_model_brm8_pr, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 | region),
  #re_formula = NA,
  #sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    median_dist = exp((log_median_dist_std * sd(log(fit_dists_pr$median_dist), na.rm = TRUE)) + 
                        mean(log(fit_dists_pr$median_dist), na.rm = TRUE)),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = median_dist/1000, y = Estimate, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = sex), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = sex), alpha = 0.2, color = NA) +
  #facet_wrap(~ age , scales = "fixed", ncol = 3) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Half range (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  #scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  ylim(0, 7)+
  xlim(5,150) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )
#xlim(0,150) # Set Y-axis range for all panels
# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_half_range_marginal_regions_pr_poisson_randomint.png", width = 7, height = 6)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24_traditional/predictions_half_range_marginal_regions_pr_poisson_randomint.csv")

# Prepare new data for predictions
region_levels <- unique(fit_dists_all_2$region)

# Expand the new data grid
nd <- expand.grid(
  log_median_dist_std = seq(-5, 2, length.out = 100),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = region_levels
)

# Predict values for each region
region_predictions <- fitted(
  fit_model_brm6_pr, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    median_dist = exp((log_median_dist_std * sd(log(fit_dists_pr$median_dist), na.rm = TRUE)) + 
                        mean(log(fit_dists_pr$median_dist), na.rm = TRUE)),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")),
    region = factor(
      region,
      levels = c("Minganga", "Macao"),
      labels = c("Minganga", "Macao")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = median_dist/1000, y = Estimate, color = region, group = region)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = region), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~  sex, scales = "fixed", ncol = 2) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Half range (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  #scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  ylim(0, 8) +
  xlim(5,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_half_range_across_regions_pr_poisson_randomint.png", width = 10, height = 5)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24_traditional/predictions_half_range_across_regions_pr_poisson_randomint.csv")

nd <- expand.grid(
  log_median_dist_std = seq(-4, 2, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = "avg_region"
)

# Predict values for each region
region_predictions <- fitted(
  fit_model_brm6_pr, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 | region),
  #re_formula = NA,
  #sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    median_dist = exp((log_median_dist_std * sd(log(fit_dists_pr$median_dist), na.rm = TRUE)) + 
                        mean(log(fit_dists_pr$median_dist), na.rm = TRUE)),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = median_dist/1000, y = Estimate, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = sex), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = sex), alpha = 0.2, color = NA) +
  #facet_wrap(~ age , scales = "fixed", ncol = 3) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Half range (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  #scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  ylim(0, 7)+
  xlim(5,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_half_range_marginal_regions_pr_poisson.png", width = 7, height = 6)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24_traditional/predictions_half_range_marginal_regions_pr_poisson.csv")


## NOW WITH MAX

# random intercepts for region and residence_camp
formula_m6 <- bf(children_alive ~  log_max_dist_std + age 
                 + sex + (1 | region) +
                   (1 | region:residence_camp),
                 family = "poisson")

# random intercepts for region and residence_camp (int sex and age)
formula_m7 <- bf(children_alive ~  log_max_dist_std*sex + age + (1 | region) +
                   (1 | region:residence_camp),
                 family = "poisson")

# random intercepts for region and residence_camp
formula_m8 <- bf(children_alive ~  log_max_dist_std + age 
                 + sex + (1 + log_max_dist_std  | region) +
                   (1 | region:residence_camp),
                 family = "poisson")

# random intercepts for region and residence_camp (int sex and age)
formula_m9 <- bf(children_alive ~  log_max_dist_std*sex + age +
                   (1 + log_max_dist_std  | region) +
                   (1 | region:residence_camp),
                 family = "poisson")

formula_m10 <- bf(children_alive ~  log_max_dist_std*age + sex +
                    (1 + log_median_dist_std  | region) +
                    (1 | region:residence_camp),
                  family = "poisson")


# fit models

fit_model_brm6b_max <- brm(formula=formula_m6, data=fit_dists_all_2,warmup=2000,
                           iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson)

fit_model_brm7_max <- brm(formula=formula_m7, data=fit_dists_all_2,warmup=2000,
                          iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson)

fit_model_brm8_max <- brm(formula=formula_m8, data=fit_dists_all_2,warmup=2000,
                          iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson)

fit_model_brm9_max <- brm(formula=formula_m9, data=fit_dists_all_2,warmup=2000,
                          iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson)

fit_model_brm10_max <- brm(formula=formula_m10, data=fit_dists_all_2,warmup=2000,
                           iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson)


# compare models using loo
f_m6b <- add_criterion(fit_model_brm6b_max, "loo")
f_m7 <- add_criterion(fit_model_brm7_max, "loo")
f_m8 <- add_criterion(fit_model_brm8_max, "loo")
f_m9 <- add_criterion(fit_model_brm9_max, "loo")
f_m10 <- add_criterion(fit_model_brm10_max, "loo")

comparison <- loo_compare(f_m6b, f_m7, f_m9,f_m8, f_m10) %>% print(simplify = F)
model_weights(fit_model_brm6b_max,  fit_model_brm7_max, fit_model_brm9_max, fit_model_brm8_max,fit_model_brm10_max, weights = "loo") %>% round(digits = 2)

# save comparison
write.csv(comparison, "results/fitness_hr_dec24_traditional/half_range_fitness_comparison_poisson_max.csv")

# save models
saveRDS(fit_model_brm6b_max, file = "models/half_range_fitness_model6b_poisson_max_traditional.rds")
saveRDS(fit_model_brm7_max, file = "models/half_range_fitness_model7_poisson_max_traditional.rds")
saveRDS(fit_model_brm8_max, file = "models/half_range_fitness_model8_poisson_max_traditional.rds")
saveRDS(fit_model_brm9_max, file = "models/half_range_fitness_model9_poisson_max_traditional.rds")
saveRDS(fit_model_brm10_max, file = "models/half_range_fitness_model10_poisson_max_traditional.rds")

# read models
fit_model_brm6b_max <- readRDS("models/half_range_fitness_model6b_poisson_max_traditional.rds")
fit_model_brm7_max <- readRDS("models/half_range_fitness_model7_poisson_max_traditional.rds")
fit_model_brm8_max <- readRDS("models/half_range_fitness_model8_poisson_max_traditional.rds")
fit_model_brm9_max <- readRDS("models/half_range_fitness_model9_poisson_max_traditional.rds")
fit_model_brm10_max <- readRDS("models/half_range_fitness_model10_poisson_max_traditional.rds")


# # Model 6b for post reproductives
formula_m5_pr <- bf(children_alive ~  log_max_dist_std 
                    + sex + (1  | region) +
                      (1 | region:residence_camp),
                    family = "poisson")


formula_m6_pr <- bf(children_alive ~  log_max_dist_std 
                    + sex + (1 + log_max_dist_std  | region) +
                      (1 | region:residence_camp),
                    family = "poisson")

formula_m9_pr <- bf(children_alive ~  log_max_dist_std*sex + (1 + log_max_dist_std  | region) +
                      (1 | region:residence_camp),
                    family = "poisson")

fit_model_brm5_pr_max <- brm(formula=formula_m5_pr, data=fit_dists_pr,warmup=2000,
                             iter=10000, chains=3, control=list(adapt_delta=0.99), prior =m3pr.prior.poisson)

fit_model_brm6_pr_max <- brm(formula=formula_m6_pr, data=fit_dists_pr,warmup=2000,
                             iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson)

fit_model_brm9_pr_max <- brm(formula=formula_m9_pr, data=fit_dists_pr,warmup=2000,
                             iter=10000, chains=3, control=list(adapt_delta=0.99), prior =m3pr.prior.poisson)

# save as rds
saveRDS(fit_model_brm5_pr_max, file = "models/half_range_fitness_model5_pr_poisson_traditional_max.rds")
saveRDS(fit_model_brm6_pr_max, file = "models/half_range_fitness_model8_pr_poisson_traditional_max.rds")
saveRDS(fit_model_brm9_pr_max, file = "models/half_range_fitness_model6_pr_poisson_traditional_max.rds")


# read models
fit_model_brm6_pr_max <- readRDS("models/half_range_fitness_model8_pr_poisson_traditional_max.rds")
fit_model_brm9_pr_max <- readRDS("models/half_range_fitness_model6_pr_poisson_traditional_max.rds")
fit_model_brm5_pr_max <- readRDS("models/half_range_fitness_model5_pr_poisson_traditional_max.rds")

# compare models using loo
f_m5_pr <- add_criterion(fit_model_brm5_pr_max, "loo")
f_m6_pr <- add_criterion(fit_model_brm6_pr_max, "loo")
f_m9_pr <- add_criterion(fit_model_brm9_pr_max, "loo")

comparison_pr <- loo_compare(f_m5_pr ,f_m6_pr, f_m9_pr) %>% print(simplify = F)
write.csv(comparison_pr, "results/fitness_hr_dec24_traditional/half_range_fitness_comparison_pr_poisson_max.csv")

## Plot region level variation

# Prepare new data for predictions
region_levels <- unique(fit_dists_all_2$region)

# Expand the new data grid
nd <- expand.grid(
  log_max_dist_std = seq(-5, 2, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = region_levels
)


# Predict values for each region
region_predictions <- fitted(
  fit_model_brm10_max, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 + log_max_dist_std| region),
  sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    max_dist = exp((log_max_dist_std * sd(log(fit_dists_all_2$max_dist), na.rm = TRUE)) + 
                     mean(log(fit_dists_all_2$max_dist), na.rm = TRUE)),
    age = factor(
      age,
      levels = c("Adolescent", "Adulte", "Personne âgée"),
      labels = c("Adolescent", "Adult", "Old adult")),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")),
    region = factor(
      region,
      levels = c("Minganga", "Macao"),
      labels = c("Minganga", "Macao")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = max_dist/1000, y = Estimate, color = region, group = region)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = region), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~ age + sex, scales = "fixed", ncol = 2) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Max distance travelled in lifetime (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  ylim(0, 10) +
xlim(5,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_half_range_across_regions_poisson_max.png", width = 7, height = 10)

# save_predictions
write.csv(region_predictions, "results/fitness_hr_dec24_traditional/predictions_half_range_across_regions_poisson_max.csv")

nd <- expand.grid(
  log_max_dist_std = seq(-4, 2, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = "avg_region"
)

# Predict values for each region
region_predictions <- fitted(
  fit_model_brm10_max, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 | region),
  #re_formula = NA,
  #sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    max_dist = exp((log_max_dist_std * sd(log(fit_dists_all_2$max_dist), na.rm = TRUE)) + 
                     mean(log(fit_dists_all_2$max_dist), na.rm = TRUE)),
    age = factor(
      age,
      levels = c("Adolescent", "Adulte", "Personne âgée"),
      labels = c("Adolescent", "Adult", "Old adult")),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = max_dist/1000, y = Estimate, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = sex), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = sex), alpha = 0.2, color = NA) +
  facet_wrap(~ age , scales = "fixed", ncol = 3) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Max distance travelled in lifetime (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  #scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  ylim(0, 10)+
xlim(5,200) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_max_range_marginal_regions_poisson_max.png", width = 12, height = 5)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24_traditional/predictions_max_range_marginal_regions_poisson_max.csv")

# Prepare new data for predictions
region_levels <- unique(fit_dists_all_2$region)

# Expand the new data grid
nd <- expand.grid(
  log_max_dist_std = seq(-5, 3, length.out = 100),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = region_levels
)


# Predict values for each region
region_predictions <- fitted(
  fit_model_brm9_pr_max, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 + log_max_dist_std| region),
  sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    max_dist = exp((log_max_dist_std * sd(log(fit_dists_all_2$max_dist), na.rm = TRUE)) + 
                     mean(log(fit_dists_all_2$max_dist), na.rm = TRUE)),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")),
    region = factor(
      region,
      levels = c("Minganga", "Macao"),
      labels = c("Minganga", "Macao")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = max_dist/1000, y = Estimate, color = region, group = region)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = region), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~  sex, scales = "fixed", ncol = 2) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Maximum distance travelled in lifetime (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  #scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  ylim(0, 10) +
  xlim(5, 150) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )# Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_max_range_across_regions_pr_poisson.png", width = 10, height = 5)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24_traditional/predictions_max_range_across_regions_pr_poisson.csv")

nd <- expand.grid(
  log_max_dist_std = seq(-4, 3, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = "avg_region"
)

# Predict values for each region
region_predictions <- fitted(
  fit_model_brm9_pr_max, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 | region),
  #re_formula = NA,
  #sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    max_dist = exp((log_max_dist_std * sd(log(fit_dists_all_2$max_dist), na.rm = TRUE)) + 
                     mean(log(fit_dists_all_2$max_dist), na.rm = TRUE)),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = max_dist/1000, y = Estimate, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = sex), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = sex), alpha = 0.2, color = NA) +
  #facet_wrap(~ age , scales = "fixed", ncol = 3) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Maximum distance travelled in lifetime (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  #scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  ylim(0, 14)+
  xlim(10,200) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )# Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_max_range_marginal_regions_pr_poisson.png", width = 7, height = 6)
# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24_traditional/predictions_max_range_marginal_regions_pr_poisson.csv")

# Now random intercept models

# Prepare new data for predictions
region_levels <- unique(fit_dists_all_2$region)

# Expand the new data grid
nd <- expand.grid(
  log_max_dist_std = seq(-5, 2, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = region_levels
)


# Predict values for each region
region_predictions <- fitted(
  fit_model_brm6b_max, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    max_dist = exp((log_max_dist_std * sd(log(fit_dists_all_2$max_dist), na.rm = TRUE)) + 
                     mean(log(fit_dists_all_2$max_dist), na.rm = TRUE)),
    age = factor(
      age,
      levels = c("Adolescent", "Adulte", "Personne âgée"),
      labels = c("Adolescent", "Adult", "Old adult")),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")),
    region = factor(
      region,
      levels = c("Minganga", "Macao"),
      labels = c("Minganga", "Macao")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = max_dist/1000, y = Estimate, color = region, group = region)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = region), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~ age + sex, scales = "fixed", ncol = 2) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Max distance travelled in lifetime (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  ylim(0, 10) +
  xlim(5,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_half_range_across_regions_poisson_max_randomint.png", width = 7, height = 10)

# save_predictions
write.csv(region_predictions, "results/fitness_hr_dec24_traditional/predictions_half_range_across_regions_poisson_max_randomint.csv")

nd <- expand.grid(
  log_max_dist_std = seq(-4, 2, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = "avg_region"
)

# Predict values for each region
region_predictions <- fitted(
  fit_model_brm6b_max, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 | region),
  #re_formula = NA,
  #sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    max_dist = exp((log_max_dist_std * sd(log(fit_dists_all_2$max_dist), na.rm = TRUE)) + 
                     mean(log(fit_dists_all_2$max_dist), na.rm = TRUE)),
    age = factor(
      age,
      levels = c("Adolescent", "Adulte", "Personne âgée"),
      labels = c("Adolescent", "Adult", "Old adult")),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = max_dist/1000, y = Estimate, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = sex), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = sex), alpha = 0.2, color = NA) +
  facet_wrap(~ age , scales = "fixed", ncol = 3) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Max distance travelled in lifetime (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  #scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  ylim(0, 8)+
  xlim(5,200) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_max_range_marginal_regions_poisson_max_randomint.png", width = 12, height = 5)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24_traditional/predictions_max_range_marginal_regions_poisson_max_randomint.csv")

# Prepare new data for predictions
region_levels <- unique(fit_dists_all_2$region)

# Expand the new data grid
nd <- expand.grid(
  log_max_dist_std = seq(-5, 3, length.out = 100),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = region_levels
)


# Predict values for each region
region_predictions <- fitted(
  fit_model_brm5_pr_max, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    max_dist = exp((log_max_dist_std * sd(log(fit_dists_all_2$max_dist), na.rm = TRUE)) + 
                     mean(log(fit_dists_all_2$max_dist), na.rm = TRUE)),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")),
    region = factor(
      region,
      levels = c("Minganga", "Macao"),
      labels = c("Minganga", "Macao")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = max_dist/1000, y = Estimate, color = region, group = region)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = region), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~  sex, scales = "fixed", ncol = 2) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Maximum distance travelled in lifetime (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  #scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2])) +
  ylim(0, 8) +
  xlim(2, 150) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )# Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_max_range_across_regions_pr_poisson_randomint.png", width = 10, height = 5)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24_traditional/predictions_max_range_across_regions_pr_poisson_randomint.csv")

nd <- expand.grid(
  log_max_dist_std = seq(-4, 3, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = "avg_region"
)

# Predict values for each region
region_predictions <- fitted(
  fit_model_brm5_pr_max, # best model
  allow_new_levels = TRUE,
  re_formula = ~ (1 | region),
  #re_formula = NA,
  #sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.025, .25, .75, .975, 0.1, 0.9)
) %>%
  as_tibble() %>%
  bind_cols(nd)


library(ggplot2)

# Ensure the age variable is properly labeled
region_predictions <- region_predictions %>%
  mutate(
    max_dist = exp((log_max_dist_std * sd(log(fit_dists_all_2$max_dist), na.rm = TRUE)) + 
                     mean(log(fit_dists_all_2$max_dist), na.rm = TRUE)),
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Women", "Men")
    )
  )

# Plot predictions with updated labels and Y-axis range
region_predictions %>%
  ggplot(aes(x = max_dist/1000, y = Estimate, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q25, ymax = Q75, fill = sex), alpha = 0.5, color = NA) +
  geom_ribbon(aes(ymin = Q10, ymax = Q90, fill = sex), alpha = 0.2, color = NA) +
  #facet_wrap(~ age , scales = "fixed", ncol = 3) +  # Fixed Y-axis range and 2 columns
  labs(
    x = "Maximum distance travelled in lifetime (km)",
    y = "Predicted children alive"
  ) +
  ggtitle("Predictions Across Regions") +
  #scale_x_continuous(trans = "log", labels = scales::comma) +  # Optional: Log-scale x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12)  # Panel label size
  ) +
  scale_color_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  scale_fill_manual(values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) +
  ylim(0, 7)+
  xlim(1,200) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )# Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24_traditional/predictions_max_range_marginal_regions_pr_poisson_randomint.png", width = 7, height = 6)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24_traditional/predictions_max_range_marginal_regions_pr_poisson_randomint.csv")

