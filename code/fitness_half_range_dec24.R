#####################################################################
# Models exploring fitness consequences of hunter-gatherer mobility #
#####################################################################

library(ggplot2)
library(ggstatsplot)
library(brms)

fit_dists_all <- read.csv("results/half_range_good_include_all/exploration_range_max_dist_fitness.csv")

# we actually want to rescue new models with new HR calculations
dists_all <- read.csv("../analyses_sept23/results/half_range/exploration_range_max_dist_everyone.csv")

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


m1.prior <- c(prior(student_t(3, 1.1, 2.5), class=Intercept),
              prior(student_t(3, 0, 2.5), class=sd), 
              prior(normal(0, 0.5), class=b))

m2.prior <- c(prior(normal(1, 1.5), class = Intercept),
              prior(student_t(3, 0, 2.5), class=sd),
              prior(normal(0, 1.5), class=b))

m3.prior <- c(prior(normal(2, 0.5), class=Intercept),
              prior(student_t(3, 0, 2.5), class=sd), 
              prior(normal(0, 0.5), class=b))

m3b.prior <- c(prior(normal(2, 0.5), class=Intercept),
               prior(student_t(3, 0, 2.5), class=sd), 
               prior(normal(0, 1), class=b))

m4n.prior <- c(prior(student_t(3, 1.1, 2.5), class=Intercept),
               prior(student_t(3, 0, 2.5), class=sd), 
               prior(normal(0, 0.5), class=b),
               prior(gamma(0.01, 0.01), class=shape))
# we will now include intercepts (nested)

m3pr.prior.stable <- c(
  # Intercept: Centered and Regularized
  prior(normal(0, 1), class = Intercept),  # Centered near observed -0.34
  # Fixed Effects (Coefficients): Tighter Regularization
  prior(normal(0, 0.5), class = b),        # Assumes smaller slopes
  # Random Effects Standard Deviations: Constrained and Regularized
  prior(student_t(3, 0, 1), class = sd),   # Moderate variability for group-level SDs
  # Shape Parameter for Negative Binomial: Regularized to Avoid Extremes
  prior(normal(3, 1), class = shape)       # Centered around a reasonable shape value
)

m3pr.prior.poisson <- c(
  # Intercept: Centered and Regularized
  prior(normal(0, 1), class = Intercept),  # Centered near observed mean of response
  
  # Fixed Effects (Coefficients): Tighter Regularization
  prior(normal(0, 1), class = b),        # Assumes smaller slopes
  
  # Random Effects Standard Deviations: Constrained and Regularized
  prior(student_t(3, 0, 1), class = sd)    # Moderate variability for group-level SDs
)

m3pr.prior.poisson.refined <- c(
  # Intercept: Centered and broader
  prior(normal(0, 1.5), class = Intercept),
  
  # Fixed effects (Coefficients): Slightly broader
  prior(normal(0, 1), class = b),
  
  # Random effects standard deviations
  prior(student_t(3, 0, 2), class = sd)
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
fit_model_brm6 <- brm(formula=formula_m6, data=fit_dists_all_2,warmup=2000, # >300 div trans
                      iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.upd.n)

fit_model_brm6b <- brm(formula=formula_m6, data=fit_dists_all_2,warmup=2000,
                       iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.stable)

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
write.csv(comparison, "results/fitness_hr_dec24/half_range_fitness_comparison.csv")

# save models
saveRDS(fit_model_brm6b, file = "models/half_range_fitness_model6b.rds")
saveRDS(fit_model_brm7, file = "models/half_range_fitness_model7.rds")
saveRDS(fit_model_brm8, file = "models/half_range_fitness_model8.rds")
saveRDS(fit_model_brm9, file = "models/half_range_fitness_model9.rds")
saveRDS(fit_model_brm10, file = "models/half_range_fitness_model10.rds")

# read models
fit_model_brm6b <- readRDS("models/half_range_fitness_model6b.rds")
fit_model_brm7 <- readRDS("models/half_range_fitness_model7.rds")
fit_model_brm8 <- readRDS("models/half_range_fitness_model8.rds")
fit_model_brm9 <- readRDS("models/half_range_fitness_model9.rds")
fit_model_brm10 <- readRDS("models/half_range_fitness_model10.rds")


# Plot priors and posteriors of parameters

library(marginaleffects)
cmp <- list(
  "Prior" = avg_comparisons(fit_model_brm4_prior),
  "Posterior" = avg_comparisons(fit_model_brm4))

draws <- lapply(names(cmp), \(x) transform(posteriordraws(cmp[[x]]), Label = x))
draws <- do.call("rbind", draws)

ggplot(draws, aes(x = draw, color = Label)) +
  xlim(c(-4, 4)) +
  geom_density() +
  facet_wrap(~term + contrast, scales = "free")

cmp <- list(
  "Prior" = avg_comparisons(fit_model_brm4n_prior),
  "Posterior" = avg_comparisons(fit_model_brm4n))

draws <- lapply(names(cmp), \(x) transform(posteriordraws(cmp[[x]]), Label = x))
draws <- do.call("rbind", draws)

ggplot(draws, aes(x = draw, color = Label)) +
  xlim(c(-4, 4)) +
  geom_density() +
  facet_wrap(~term + contrast, scales = "free")

# Simulate relationships derived from priors
n=100
sim_prior <- 
  tibble(group = seq_len(n),
         alpha=rgamma(n, 1,1/4),
         beta=rnorm(n, 0, 0.5),
         sd=rnorm(n, 0, 0.5)
  )

df <- sim_prior %>% 
  expand(nesting(group, alpha, beta, sd), median_dist_log_sd = seq(-6, 3, by=1)) %>%
  mutate(kids = rnorm(n(), alpha + beta * (median_dist_log_sd), sd))

df %>%  
  ggplot(aes(x = median_dist_log_sd, y = kids, group = group)) +
  geom_line() +
  labs(x = "Exploration range", y = "Kids alive")

#save.image("results/intermediate/half_range_fitness.RData")

# Now only with post reprod individuals to have complete fertility

fit_dists_pr <- subset(fit_dists_all_2, fit_dists_all_2$age=="Personne âgée")

# standardise median_dist within the pr dataset
fit_dists_pr$log_median_dist_std <- (log(fit_dists_pr$median_dist) - mean(log(fit_dists_pr$median_dist), na.rm=T)) / sd(log(fit_dists_pr$median_dist), na.rm=T)

m4pr.prior.upd.n <- c(prior(gamma(1, 0.25), class=Intercept),
                      prior(normal(0, 0.25), class=sd), 
                      prior(normal(0.5, 0.25), class=b),
                      prior(gamma(0.5, 0.5), class=shape))

# Model 8 for post reproductives
formula_m8_pr <- bf(children_alive ~  log_median_dist_std 
                    + sex + (1 + log_median_dist_std  | region) +
                      (1 | region:residence_camp),
                    family = "negbinomial")

fit_model_brm8_pr <- brm(formula=formula_m8_pr, data=fit_dists_pr,warmup=2000,
                         iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.stable)

# save as rds
saveRDS(fit_model_brm8_pr, file = "models/half_range_fitness_model8_pr.rds")

## Plot region level variation

# Prepare new data for predictions
region_levels <- unique(fit_dists_all_2$region)

# Expand the new data grid
nd <- expand.grid(
  log_median_dist_std = seq(-5, 4, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = region_levels
)


# Predict values for each region
region_predictions <- fitted(
  fit_model_brm8, # best model
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
  ylim(0, 3) 
#xlim(0,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_half_range_across_regions.png", width = 7, height = 10)

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
  ylim(0, 5)
#xlim(0,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_half_range_marginal_regions.png", width = 7, height = 10)

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
  ylim(0, 10) 
#xlim(0,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_half_range_across_regions_pr.png", width = 10, height = 5)

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
  ylim(0, 9)+
  xlim(17,130)
#xlim(0,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_half_range_marginal_regions_pr.png", width = 7, height = 10)


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
                       iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson)

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
write.csv(comparison, "results/fitness_hr_dec24/half_range_fitness_comparison_poisson.csv")

# save models
saveRDS(fit_model_brm6b, file = "models/half_range_fitness_model6b_poisson.rds")
saveRDS(fit_model_brm7, file = "models/half_range_fitness_model7_poisson.rds")
saveRDS(fit_model_brm8, file = "models/half_range_fitness_model8_poisson.rds")
saveRDS(fit_model_brm9, file = "models/half_range_fitness_model9_poisson.rds")
saveRDS(fit_model_brm10, file = "models/half_range_fitness_model10_poisson.rds")

# load all models
fit_model_brm6b <- readRDS("models/half_range_fitness_model6b_poisson.rds")
fit_model_brm7 <- readRDS("models/half_range_fitness_model7_poisson.rds")
fit_model_brm8 <- readRDS("models/half_range_fitness_model8_poisson.rds")
fit_model_brm9 <- readRDS("models/half_range_fitness_model9_poisson.rds")
fit_model_brm10 <- readRDS("models/half_range_fitness_model10_poisson.rds")


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
                         iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson)

# save as rds
saveRDS(fit_model_brm8_pr, file = "models/half_range_fitness_model8_pr_poisson.rds")
saveRDS(fit_model_brm6_pr, file = "models/half_range_fitness_model6_pr_poisson.rds")

# load all models
fit_model_brm6_pr <- readRDS("models/half_range_fitness_model6_pr_poisson.rds")
fit_model_brm8_pr <- readRDS("models/half_range_fitness_model8_pr_poisson.rds")

# compare models using loo
f_m6_pr <- add_criterion(fit_model_brm6_pr, "loo")
f_m8_pr <- add_criterion(fit_model_brm8_pr, "loo")

comparison_pr <- loo_compare(f_m6_pr, f_m8_pr) %>% print(simplify = F)
write.csv(comparison_pr, "results/fitness_hr_dec24/half_range_fitness_comparison_pr_poisson.csv")

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
  fit_model_brm6b, # best model
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
  ylim(0, 6) 
#xlim(0,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_half_range_across_regions_poisson.png", width = 7, height = 10)

# save_predictions
write.csv(region_predictions, "results/fitness_hr_dec24/predictions_half_range_across_regions_poisson.csv")

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
  ylim(0, 6)+
  xlim(5,150) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )
# Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_half_range_marginal_regions_poisson.png", width = 12, height = 5)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24/predictions_half_range_marginal_regions_poisson.csv")

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
  ylim(0, 7) +
  xlim(16,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_half_range_across_regions_pr_poisson.png", width = 10, height = 5)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24/predictions_half_range_across_regions_pr_poisson.csv")

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
  xlim(16,150) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )# Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_half_range_marginal_regions_pr_poisson.png", width = 7, height = 6)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24/predictions_half_range_marginal_regions_pr_poisson.csv")

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
  ylim(0, 7) +
  xlim(16,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_half_range_across_regions_pr_poisson.png", width = 10, height = 5)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24/predictions_half_range_across_regions_pr_poisson.csv")

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
  xlim(16,150) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )# Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_half_range_marginal_regions_pr_poisson.png", width = 7, height = 6)


## NOW WITH MAX

formula_max2 <- bf(children_alive ~  log_max_dist_std + (1 | region), family = "poisson")
get_prior(formula=formula_max2, data=fit_dists_all_2)

fit_model_max_brm2 <- brm(formula=formula_max2, data=fit_dists_all_2,warmup=2000,
                          iter=10000, chains=3, control=list(adapt_delta=0.95)) 

formula_max3 <- bf(children_alive ~  log_max_dist_std + age + sex + set_type + (1 | region), family = "poisson")

fit_model_max_brm3 <- brm(formula=formula_max3, data=fit_dists_all_2,warmup=2000,
                          iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3.prior) 

formula_max4 <- bf(children_alive ~  log_max_dist_std + age + sex + (1 | residence_camp), family = "poisson")
formula_max5 <- bf(children_alive ~  log_max_dist_std + age + sex + set_type + (1 | residence_camp), family = "poisson")

fit_model_max_brm4 <- brm(formula=formula_max4, data=fit_dists_all_2,warmup=2000,
                          iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3.prior) 
fit_model_max_brm5 <- brm(formula=formula_max5, data=fit_dists_all_2,warmup=2000,
                          iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3.prior) 

summary(fit_model_max_brm4)
summary(fit_model_max_brm5)

f_max4 <- add_criterion(fit_model_max_brm4, "loo")
f_max5 <- add_criterion(fit_model_max_brm5, "loo")

loo_compare(f_max4, f_max5,criterion = "loo") %>% print(simplify = F)
model_weights(fit_model_max_brm4, fit_model_max_brm5, weights = "loo") %>% round(digits = 2)

# Now only with post reprod individuals to have complete fertility
m3pr.prior <- c(prior(normal(2, 0.5), class=Intercept),
                prior(student_t(3, 0, 2.5), class=sd), 
                prior(normal(0, 0.5), class=b))
formula_pr4_max <- bf(children_alive ~  log_max_dist_std + sex + (1 | residence_camp), family = "poisson")
formula_pr4b_max <- bf(children_alive ~  log_max_dist_std + sex + (1 | residence_camp), family = "negbinomial")

fit_model_brm4pr_max <- brm(formula=formula_pr4_max, data=fit_dists_pr,warmup=2000, iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior) 
#fit_model_brm4bpr <- brm(formula=formula_pr4b, data=fit_dists_pr,warmup=2000, iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior) 

summary(fit_model_brm4pr_max)

# Define a sequence of log_max_dist_std values from -5 to 5
log_max_dist_std_values <- seq(-6, 3, length.out = 100)
age_levels <- unique(fit_dists_all_2$age)
sex_levels <- unique(fit_dists_all_2$sex)

# Create a data frame with combinations of age, sex, and log_max_dist_std
combinations2 <- expand.grid(
  age = age_levels,  
  sex = sex_levels,
  log_max_dist_std = log_max_dist_std_values
)

combinations$residence_camp <- "avg_camp"

f6 <-
  fitted(fit_model_brm4pr_max,
         # this allows us to simulate values for our counterfactual island, "my_island"
         allow_new_levels = T,
         # here we explicitly tell brms we want to include the group-level effects
         re_formula = ~ (1 | residence_camp),
         # from the brms manual, this uses the "(multivariate) normal distribution implied by 
         sample_new_levels = "gaussian",
         newdata = combinations2,
         probs = c(.015, .025, .055, .165, .835, .945, .985, .975)) %>%
  as_tibble() %>%
  bind_cols(combinations2)


f6_fem <- subset(f6, f6$sex == "F")
f6_male <- subset(f6, f6$sex == "M")

p6 <-
  f6_fem %>%
  mutate(max_dist = exp((log_max_dist_std * sd(log(fit_dists_all_2$max_dist), na.rm=T) ) + mean(log(fit_dists_all_2$max_dist), na.rm=T)))%>% 
  
  ggplot(aes(x = max_dist/1000, group = age, color = age)) +
  geom_smooth(aes(y = Estimate, ymin = Q5.5, ymax = Q94.5, fill = age),
              stat = "identity",
              alpha = 2/4, linewidth = 1) +
  # geom_point(data = bind_cols(d, b11.10$criteria$loo$diagnostics),
  #            aes(y = total_tools, size = pareto_k),
  #            alpha = 4/5) +
  #scale_x_continuous("half_range", breaks = c(0, 50000, 150000)) +
  scale_y_continuous("Number of living offspring", breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_color_manual(values=c(
    MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2],
    MetPalettes$Isfahan2[[1]][1])) +
  scale_fill_manual(values=c(
    MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2],
    MetPalettes$Isfahan2[[1]][1])) +
  xlab("Max distance in lifetime (km from residence)") +
  ggtitle("Women") + 
  theme_minimal() +
  coord_cartesian(xlim = c(0,800),
                  ylim = c(0, 10))
p6

p7 <-
  f6_male %>%
  mutate(max_dist = exp((log_max_dist_std * sd(log(fit_dists_all_2$max_dist), na.rm=T) ) + mean(log(fit_dists_all_2$max_dist), na.rm=T)))%>% 
  
  ggplot(aes(x = max_dist/1000, group = age, color = age)) +
  geom_smooth(aes(y = Estimate, ymin = Q5.5, ymax = Q94.5, fill = age),
              stat = "identity",
              alpha = 2/4, linewidth = 1) +
  # geom_point(data = bind_cols(d, b11.10$criteria$loo$diagnostics),
  #            aes(y = total_tools, size = pareto_k),
  #            alpha = 4/5) +
  #scale_x_continuous("half_range", breaks = c(0, 50000, 150000)) +
  scale_y_continuous("Number of living offspring", breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_color_manual(values=c(
    MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2],
    MetPalettes$Isfahan2[[1]][1])) +
  scale_fill_manual(values=c(
    MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2],
    MetPalettes$Isfahan2[[1]][1])) +
  xlab("Max distance in lifetime (km from residence)") +
  ggtitle("Men") + 
  theme_minimal() +
  coord_cartesian(xlim = c(0,800),
                  ylim = c(0, 10))
p7

#save.image("results/intermediate/half_range_fitness.RData")
library(gridExtra)
combined_plot <- grid.arrange(p6, p7, ncol = 2)
ggsave(plot=combined_plot, filename="results/fitness_hr/fitted_preds_fitness_max_dist_from_res.png", width = 14, height=6)


p6 <-
  f6 %>%
  mutate(max_dist = exp((log_max_dist_std * sd(log(fit_dists_pr$max_dist  + 0.0001), na.rm=T) ) + mean(log(fit_dists_pr$max_dist + 0.0001), na.rm=T)))%>% 
  
  ggplot(aes(x = max_dist/1000, group = sex, color = sex)) +
  geom_smooth(aes(y = Estimate, ymin = Q5.5, ymax = Q94.5, fill = sex),
              stat = "identity",
              alpha = 2/4, linewidth = 1) +
  # geom_point(data = bind_cols(d, b11.10$criteria$loo$diagnostics),
  #            aes(y = total_tools, size = pareto_k),
  #            alpha = 4/5) +
  #scale_x_continuous("half_range", breaks = c(0, 50000, 150000)) +
  scale_y_continuous("number of living offspring", breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_color_manual(values=c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])) +
  xlab("Max distance in lifetime (km from residence)") +
  ggtitle("Completed fertility and maximum range") + 
  theme_minimal() +
  coord_cartesian(xlim = c(0,800),
                  ylim = c(0, 10))
p6


ggsave(plot=p6, filename="results/fitness_hr/fitted_preds_fitness_max_range_from_res_post_reprod.png", width = 7, height=6)

# NOW POISSON

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
                    (1 + log_max_dist_std  | region) +
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

comparison <- loo_compare(f_m6b,f_m7, f_m9,f_m8, f_m10) %>% print(simplify = F)
model_weights(fit_model_brm6b_max, fit_model_brm7_max, fit_model_brm9_max, fit_model_brm8_max,fit_model_brm10_max, weights = "loo") %>% round(digits = 2)

# save comparison
write.csv(comparison, "results/fitness_hr_dec24/half_range_fitness_comparison_poisson_max.csv")

# save models
saveRDS(fit_model_brm6b, file = "models/half_range_fitness_model6b_poisson_max.rds")
saveRDS(fit_model_brm7, file = "models/half_range_fitness_model7_poisson_max.rds")
saveRDS(fit_model_brm8, file = "models/half_range_fitness_model8_poisson_max.rds")
saveRDS(fit_model_brm9, file = "models/half_range_fitness_model9_poisson_max.rds")
saveRDS(fit_model_brm10, file = "models/half_range_fitness_model10_poisson_max.rds")

# load models
fit_model_brm6b_max <- readRDS("models/half_range_fitness_model6b_poisson_max.rds")
fit_model_brm8_max <- readRDS("models/half_range_fitness_model8_poisson_max.rds")
fit_model_brm9_max <- readRDS("models/half_range_fitness_model9_poisson_max.rds")
fit_model_brm10_max <- readRDS("models/half_range_fitness_model10_poisson_max.rds")


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
                             iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson)

fit_model_brm5b_pr_max <- brm(formula=formula_m5_pr, data=fit_dists_pr,warmup=2000,
                              iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson.refined)

fit_model_brm6_pr_max <- brm(formula=formula_m6_pr, data=fit_dists_pr,warmup=2000,
                             iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson)

fit_model_brm9_pr_max <- brm(formula=formula_m9_pr, data=fit_dists_pr,warmup=2000,
                             iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3pr.prior.poisson)

# save as rds
saveRDS(fit_model_brm5_pr_max, file = "models/half_range_fitness_model5_pr_poisson.rds")
saveRDS(fit_model_brm6_pr_max, file = "models/half_range_fitness_model8_pr_poisson.rds")
saveRDS(fit_model_brm9_pr_max, file = "models/half_range_fitness_model6_pr_poisson.rds")


# compare models using loo
f_m5_pr <- add_criterion(fit_model_brm5_pr_max, "loo")
f_m6_pr <- add_criterion(fit_model_brm6_pr_max, "loo")
f_m9_pr <- add_criterion(fit_model_brm9_pr_max, "loo")

comparison_pr <- loo_compare(f_m5_pr, f_m6_pr, f_m9_pr) %>% print(simplify = F)
write.csv(comparison_pr, "results/fitness_hr_dec24/half_range_fitness_comparison_pr_poisson_max.csv")

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
  ylim(0, 7) 
#xlim(0,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_half_range_across_regions_poisson_max.png", width = 7, height = 10)

# save_predictions
write.csv(region_predictions, "results/fitness_hr_dec24/predictions_half_range_across_regions_poisson_max.csv")

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
  ylim(0, 7)
#xlim(0,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_half_range_marginal_regions_poisson_max.png", width = 12, height = 5)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24/predictions_half_range_marginal_regions_poisson_max.csv")

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
  fit_model_brm6_pr_max, # best model
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
  ylim(0, 12) +
  xlim(25, 800) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_max_range_across_regions_pr_poisson.png", width = 10, height = 5)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24/predictions_max_range_across_regions_pr_poisson_max.csv")

nd <- expand.grid(
  log_max_dist_std = seq(-4, 3, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = "avg_region"
)

# Predict values for each region
region_predictions <- fitted(
  fit_model_brm6_pr_max, # best model
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
  ylim(0, 14)+
  xlim(25,600) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_max_range_marginal_regions_pr_poisson.png", width = 7, height = 6)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24/predictions_max_range_marginal_regions_pr_poisson.csv")

#####################
# Random intercepts #
#####################

# Prepare new data for predictions
region_levels <- unique(fit_dists_all_2$region)

# Expand the new data grid
nd <- expand.grid(
  log_max_dist_std = seq(-5, 3, length.out = 100),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  age = levels(as.factor(fit_dists_all_2$age)),
  region = region_levels
)

# Predict values for each region
region_predictions <- fitted(
  fit_model_brm7_max, # best model
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
  ylim(0, 7) 
#xlim(0,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_half_range_across_regions_poisson_max_randomint.png", width = 7, height = 10)

# save_predictions
write.csv(region_predictions, "results/fitness_hr_dec24/predictions_half_range_across_regions_poisson_max_randomint.csv")

nd <- expand.grid(
  log_max_dist_std = seq(-4, 2, length.out = 100),
  age = levels(as.factor(fit_dists_all_2$age)),
  sex = levels(as.factor(fit_dists_all_2$sex)),
  region = "avg_region"
)

# Predict values for each region
region_predictions <- fitted(
  fit_model_brm7_max, # best model
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
  ylim(0, 8)
#xlim(0,150) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_half_range_marginal_regions_poisson_max_randomint.png", width = 12, height = 5)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24/predictions_half_range_marginal_regions_poisson_max_randomint.csv")

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
  xlim(25, 800) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_max_range_across_regions_pr_poisson_randomint.png", width = 10, height = 5)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24/predictions_max_range_across_regions_pr_poisson_max_randomint.csv")

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
  xlim(25,600) # Set Y-axis range for all panels

# Save the plot
ggsave("results/fitness_hr_dec24/predictions_max_range_marginal_regions_pr_poisson_randomint.png", width = 7, height = 6)

# save predictions
write.csv(region_predictions, "results/fitness_hr_dec24/predictions_half_range_marginal_regions_pr_poisson_randomint.csv")









###############
### FROM BP ###
###############

# mean and variance are the same - we use Poisson distribution
mean(fit_dists_bp_2$children_alive)
var(fit_dists_bp_2$children_alive)


fit_dists_bp_2$log_median_dist_std <- (log(fit_dists_bp_2$median_dist) - mean(log(fit_dists_bp_2$median_dist), na.rm=T)) / sd(log(fit_dists_bp_2$median_dist), na.rm=T)
fit_dists_bp_2$log_max_dist_std <- (log(fit_dists_bp_2$max_dist) - mean(log(fit_dists_bp_2$max_dist), na.rm=T)) / sd(log(fit_dists_bp_2$max_dist), na.rm=T)

formula_m3 <- bf(children_alive ~  log_median_dist_std + age + sex + set_type + (1 | region), family = "poisson")

m3.prior <- c(prior(normal(2, 0.5), class=Intercept),
              prior(student_t(3, 0, 2.5), class=sd), 
              prior(normal(0, 1.5), class=b))

fit_model_brm1 <- brm(formula=formula_m3, data=fit_dists_bp_2,warmup=2000,
                      iter=10000, chains=3, control=list(adapt_delta=0.95)) 

fit_model_brm3 <- brm(formula=formula_m3, data=fit_dists_bp_2,warmup=2000,
                      iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3.prior) 

formula_m4 <- bf(children_alive ~  log_median_dist_std + age + sex + (1 | residence_camp), family = "poisson")
formula_m5 <- bf(children_alive ~  log_median_dist_std + age + sex + set_type + (1 | residence_camp), family = "poisson")
formula_m6 <- bf(children_alive ~  log_median_dist_std*sex + age + (1 | residence_camp), family = "poisson")

fit_model_brm4n <- brm(formula=formula_m4n, data=fit_dists_bp_2,warmup=2000, iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m4pr.prior.upd.n) 

fit_model_brm4 <- brm(formula=formula_m4, data=fit_dists_bp_2,warmup=2000,
                      iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3.prior) 

fit_model_brm5 <- brm(formula=formula_m5, data=fit_dists_bp_2,warmup=2000,
                      iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3.prior) 
fit_model_brm6 <- brm(formula=formula_m6, data=fit_dists_bp_2,warmup=2000,
                      iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3.prior) 


summary(fit_model_brm4bp)
summary(fit_model_brm5)
summary(fit_model_brm6)

f_m4 <- add_criterion(fit_model_brm4, "loo")
f_m5 <- add_criterion(fit_model_brm5, "loo")
f_m6 <- add_criterion(fit_model_brm6, "loo")

loo_compare(f_m4, f_m5, f_m6, criterion = "loo") %>% print(simplify = F)
model_weights(fit_model_brm4, fit_model_brm5, fit_model_brm6, weights = "loo") %>% round(digits = 2)

# Define a sequence of log_median_dist_std values from -5 to 5
log_median_dist_std_values <- seq(-6, 5, length.out = 100)
age_levels <- unique(fit_dists_bp_2$age)
sex_levels <- unique(fit_dists_bp_2$sex)

# Create a data frame with combinations of age, sex, and log_median_dist_std
combinations <- expand.grid(
  age = age_levels,  
  sex = sex_levels,
  log_median_dist_std = log_median_dist_std_values
)

combinations$residence_camp <- "avg_camp"

f4 <-
  fitted(fit_model_brm4n,
         # this bpows us to simulate values for our counterfactual island, "my_island"
         allow_new_levels = T,
         # here we explicitly tell brms we want to include the group-level effects
         #re_formula = ~ (1 | residence_camp),
         # from the brms manual, this uses the "(multivariate) normal distribution implied by 
         # sample_new_levels = "gaussian",
         newdata = combinations,
         probs = c(.015, .025, .055, .165, .835, .945, .985, .975)) %>%
  as_tibble() %>%
  bind_cols(combinations)

pred4 <-
  predict(fit_model_brm4,
          # this bpows us to simulate values for our counterfactual island, "my_island"
          allow_new_levels = T,
          # here we explicitly tell brms we want to include the group-level effects
          re_formula = ~ (1 | residence_camp),
          # from the brms manual, this uses the "(multivariate) normal distribution implied by 
          sample_new_levels = "gaussian",
          newdata = combinations,
          probs = c(.015, .025, .055, .165, .835, .945, .985, .975)) %>%
  as_tibble() %>%
  bind_cols(combinations)


f4_fem <- subset(f4, f4$sex == "F")
f4_male <- subset(f4, f4$sex == "M")

p4 <-
  f4_fem %>%
  mutate(median_dist = exp((log_median_dist_std * sd(log(fit_dists_bp_2$median_dist), na.rm=T) ) + mean(log(fit_dists_bp_2$median_dist), na.rm=T)))%>% 
  
  ggplot(aes(x = median_dist/1000, group = age, color = age)) +
  geom_smooth(aes(y = Estimate, ymin = Q5.5, ymax = Q94.5, fill = age),
              stat = "identity",
              alpha = 2/4, linewidth = 1) +
  # geom_point(data = bind_cols(d, b11.10$criteria$loo$diagnostics),
  #            aes(y = total_tools, size = pareto_k),
  #            alpha = 4/5) +
  #scale_x_continuous("half_range", breaks = c(0, 50000, 150000)) +
  scale_y_continuous("number of living offspring", breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_color_manual(values=c(
    MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2],
    MetPalettes$Isfahan2[[1]][1])) +
  scale_fill_manual(values=c(
    MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2],
    MetPalettes$Isfahan2[[1]][1])) +
  xlab("half range (km from birthplace)") +
  ggtitle("Women") + 
  theme_minimal() +
  coord_cartesian(xlim = range((fit_dists_bp_2$median_dist/1000), na.rm=T),
                  ylim = c(0, 10))
p4

p5 <-
  f4_male %>%
  mutate(median_dist = exp((log_median_dist_std * sd(log(fit_dists_bp_2$median_dist), na.rm=T) ) + mean(log(fit_dists_bp_2$median_dist), na.rm=T)))%>% 
  
  ggplot(aes(x = median_dist/1000, group = age, color = age)) +
  geom_smooth(aes(y = Estimate, ymin = Q5.5, ymax = Q94.5, fill = age),
              stat = "identity",
              alpha = 2/4, linewidth = 1) +
  # geom_point(data = bind_cols(d, b11.10$criteria$loo$diagnostics),
  #            aes(y = total_tools, size = pareto_k),
  #            alpha = 4/5) +
  #scale_x_continuous("half_range", breaks = c(0, 50000, 150000)) +
  scale_y_continuous("number of living offspring", breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_color_manual(values=c(
    MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2],
    MetPalettes$Isfahan2[[1]][1])) +
  scale_fill_manual(values=c(
    MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2],
    MetPalettes$Isfahan2[[1]][1])) +
  xlab("half range (km from birthplace)") +
  ggtitle("Men") + 
  theme_minimal() +
  coord_cartesian(xlim = range((fit_dists_bp_2$median_dist/1000), na.rm=T),
                  ylim = c(0, 10))
p5

combined_plot <- grid.arrange(p4, p5, ncol = 2)
ggsave(plot=combined_plot, filename="results/fitness_hr/fitted_preds_fitness_half_range_from_bp_tighter_prior.png", width = 14, height=6)


## NOW WITH MAX

formula_max2 <- bf(children_alive ~  log_max_dist_std + (1 | region), family = "poisson")
get_prior(formula=formula_max2, data=fit_dists_bp_2)

fit_model_max_brm2 <- brm(formula=formula_max2, data=fit_dists_bp_2,warmup=2000,
                          iter=10000, chains=3, control=list(adapt_delta=0.95)) 

formula_max3 <- bf(children_alive ~  log_max_dist_std + age + sex + set_type + (1 | region), family = "poisson")

fit_model_max_brm3 <- brm(formula=formula_max3, data=fit_dists_bp_2,warmup=2000,
                          iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3.prior) 

formula_max4 <- bf(children_alive ~  log_max_dist_std + age + sex + (1 | residence_camp), family = "poisson")
formula_max5 <- bf(children_alive ~  log_max_dist_std + age + sex + set_type + (1 | residence_camp), family = "poisson")

fit_model_max_brm4 <- brm(formula=formula_max4, data=fit_dists_bp_2,warmup=2000,
                          iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3.prior) 
fit_model_max_brm5 <- brm(formula=formula_max5, data=fit_dists_bp_2,warmup=2000,
                          iter=10000, chains=3, control=list(adapt_delta=0.95), prior =m3.prior) 

summary(fit_model_max_brm4)
summary(fit_model_max_brm5)

f_max4 <- add_criterion(fit_model_max_brm4, "loo")
f_max5 <- add_criterion(fit_model_max_brm5, "loo")

loo_compare(f_max4, f_max5,criterion = "loo") %>% print(simplify = F)
model_weights(fit_model_max_brm4, fit_model_max_brm5, weights = "loo") %>% round(digits = 2)

# Define a sequence of log_max_dist_std values from -5 to 5
log_max_dist_std_values <- seq(-6, 3, length.out = 100)
age_levels <- unique(fit_dists_bp_2$age)
sex_levels <- unique(fit_dists_bp_2$sex)

# Create a data frame with combinations of age, sex, and log_max_dist_std
combinations2 <- expand.grid(
  age = age_levels,  
  sex = sex_levels,
  log_max_dist_std = log_max_dist_std_values
)

combinations$residence_camp <- "avg_camp"

f6 <-
  fitted(fit_model_max_brm4,
         # this bpows us to simulate values for our counterfactual island, "my_island"
         allow_new_levels = T,
         # here we explicitly tell brms we want to include the group-level effects
         re_formula = ~ (1 | residence_camp),
         # from the brms manual, this uses the "(multivariate) normal distribution implied by 
         sample_new_levels = "gaussian",
         newdata = combinations2,
         probs = c(.015, .025, .055, .165, .835, .945, .985, .975)) %>%
  as_tibble() %>%
  bind_cols(combinations2)


f6_fem <- subset(f6, f6$sex == "F")
f6_male <- subset(f6, f6$sex == "M")

p6 <-
  f6_fem %>%
  mutate(max_dist = exp((log_max_dist_std * sd(log(fit_dists_bp_2$max_dist), na.rm=T) ) + mean(log(fit_dists_bp_2$max_dist), na.rm=T)))%>% 
  
  ggplot(aes(x = max_dist/1000, group = age, color = age)) +
  geom_smooth(aes(y = Estimate, ymin = Q5.5, ymax = Q94.5, fill = age),
              stat = "identity",
              alpha = 2/4, linewidth = 1) +
  # geom_point(data = bind_cols(d, b11.10$criteria$loo$diagnostics),
  #            aes(y = total_tools, size = pareto_k),
  #            alpha = 4/5) +
  #scale_x_continuous("half_range", breaks = c(0, 50000, 150000)) +
  scale_y_continuous("Number of living offspring", breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_color_manual(values=c(
    MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2],
    MetPalettes$Isfahan2[[1]][1])) +
  scale_fill_manual(values=c(
    MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2],
    MetPalettes$Isfahan2[[1]][1])) +
  xlab("Max distance in lifetime (km from birthplace)") +
  ggtitle("Women") + 
  theme_minimal() +
  coord_cartesian(xlim = c(0,800),
                  ylim = c(0, 10))
p6

p7 <-
  f6_male %>%
  mutate(max_dist = exp((log_max_dist_std * sd(log(fit_dists_bp_2$max_dist), na.rm=T) ) + mean(log(fit_dists_bp_2$max_dist), na.rm=T)))%>% 
  
  ggplot(aes(x = max_dist/1000, group = age, color = age)) +
  geom_smooth(aes(y = Estimate, ymin = Q5.5, ymax = Q94.5, fill = age),
              stat = "identity",
              alpha = 2/4, linewidth = 1) +
  # geom_point(data = bind_cols(d, b11.10$criteria$loo$diagnostics),
  #            aes(y = total_tools, size = pareto_k),
  #            alpha = 4/5) +
  #scale_x_continuous("half_range", breaks = c(0, 50000, 150000)) +
  scale_y_continuous("Number of living offspring", breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_color_manual(values=c(
    MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2],
    MetPalettes$Isfahan2[[1]][1])) +
  scale_fill_manual(values=c(
    MetPalettes$Isfahan2[[1]][3], MetPalettes$Isfahan2[[1]][2],
    MetPalettes$Isfahan2[[1]][1])) +
  xlab("Max distance in lifetime (km from birthplace)") +
  ggtitle("Men") + 
  theme_minimal() +
  coord_cartesian(xlim = c(0,800),
                  ylim = c(0, 10))
p7

#save.image("results/intermediate/half_range_fitness.RData")
library(gridExtra)
combined_plot <- grid.arrange(p6, p7, ncol = 2)
ggsave(plot=combined_plot, filename="results/fitness_hr/fitted_preds_fitness_max_dist_from_bp.png", width = 14, height=6)

# Do people that have higher fitness (post-reproductives), also visit family more often?
fam_visit_times <- read.csv("results/reasons_visit 2/dist_everyone_fam_visit.csv")
df_pr_fam <- left_join(fit_dists_pr, fam_visit_times, by="ref")

cor.test(df_pr_fam$children_alive, df_pr_fam$times_fam_visit)
