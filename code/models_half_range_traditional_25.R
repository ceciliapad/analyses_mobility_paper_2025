####################################################################################################
# Models exploring drivers of hunter-gatherer half ranges and maximum distance covered in lifetime #
####################################################################################################

library(ggplot2)
library(ggstatsplot)
library(brms)

visits_dists_all <- read.csv("results/half_range_traditional/exploration_range_max_dist_everyone.csv")

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
visits_dists_all_2 <- left_join(visits_dists_all, regions_sub, by="residence_camp")
visits_dists_all_2 <- subset(visits_dists_all_2, visits_dists_all_2$age != FALSE)
visits_dists_all_2 <- subset(visits_dists_all_2, visits_dists_all_2$age != "")
visits_dists_all_2 <- subset(visits_dists_all_2, visits_dists_all_2$sex != "")
visits_dists_all_2 <- subset(visits_dists_all_2, visits_dists_all_2$age != "Enfant")

# plot 
# gamma distribution with log link function as recommended by Wood et al. 2021 NHB.

m1.prior <- c(prior(student_t(3, 10.2, 10), class=Intercept),
              prior(gamma(0.01, 0.01), class=shape),
              prior(student_t(3, 0, 10), class=sd), 
              prior(normal( 0, 10), class=b))

# alternative with flat priors for coeffs
m1.prior2 <- c(prior(student_t(3, 10.2, 10), class=Intercept),
               prior(gamma(0.01, 0.01), class=shape),
               prior(student_t(3, 0, 10), class=sd))

# recommended except no flat prior for beta coeff
m1.prior3 <- c(prior(student_t(3, 10.2, 2.5), class=Intercept),
               prior(gamma(0.01, 0.01), class=shape),
               prior(student_t(3, 0, 2.5), class=sd), 
               prior(normal( 0, 5), class=b))

m4.prior_weak <- c(
  prior(student_t(3, 3, 2), class = Intercept),  # Log-scale mean ~ log(20 km)
  prior(gamma(2, 0.1), class = shape),             # Shape parameter
  prior(student_t(3, 0, 2), class = sd),           # Random effect SD
  prior(normal(0, 2), class = b)                   # Fixed effect coefficients
)

formula_m5 <- bf(
  median_dist ~ age + sex + 
    (1 + age + sex | region) +  # Random intercept and slopes for `age` and `sex` by region
    (1 | region:residence_camp),  # Random intercept for residence_camp nested within region
  family = Gamma(link = "log")
)

formula_m6 <- bf(
  median_dist ~ age * sex +                # Fixed effects: interaction between age and sex
    (1 | region) +             
    (1 | region:residence_camp),           # Nested random intercept for residence_camp within region
  family = Gamma(link = "log")
)

formula_m7 <- bf(
  median_dist ~ age * sex +                # Fixed effects: interaction between age and sex
    (1 + age*sex | region) +               # Random slopes for interaction by region
    (1 | region:residence_camp),           # Nested random intercept for residence_camp within region
  family = Gamma(link = "log")
)

formula_m8 <- bf(
  median_dist ~ age * sex +                # Fixed effects: interaction between age and sex
    (1 + age + sex | region) +               # Random slopes for interaction by region
    (1 | region:residence_camp),           # Nested random intercept for residence_camp within region
  family = Gamma(link = "log")
)


hr_model_brm7 <- brm(formula=formula_m5, data=visits_dists_all_2,warmup=1000,
                     iter=5000, chains=3, control=list(adapt_delta=0.95), prior = m4.prior_weak, cores=2) 

hr_model_brm8 <- brm(formula=formula_m6, data=visits_dists_all_2,warmup=1000,
                     iter=5000, chains=3, control=list(adapt_delta=0.95), prior = m4.prior_weak, cores=2) 

hr_model_brm9 <- brm(formula=formula_m7, data=visits_dists_all_2,warmup=1000,
                     iter=5000, chains=3, control=list(adapt_delta=0.95), prior = m4.prior_weak, cores=2) 

hr_model_brm10 <- brm(formula=formula_m8, data=visits_dists_all_2,warmup=1000,
                      iter=5000, chains=3, control=list(adapt_delta=0.99), prior = m4.prior_weak, cores=2) 


# Save model as r object
saveRDS(hr_model_brm9, file = "models/hr_model_brm_randomslope_region_traditional.rds")
saveRDS(hr_model_brm8, file = "models/hr_model_brm_randomintercepts_region_traditional.rds")
saveRDS(hr_model_brm10, file = "models/hr_model_brm_randomslope_region_simpler_traditional.rds")


# Read in random slope and random intercept models and compare

hr_model_brm9 <- readRDS("models/hr_model_brm_randomslope_region_traditional.rds")
hr_model_brm10 <- readRDS("models/hr_model_brm_randomslope_region_simpler_traditional.rds")
hr_model_brm8 <- readRDS("models/hr_model_brm_randomintercepts_region_traditional.rds")

loo_m1 <- loo(hr_model_brm8)
loo_m2 <- loo(hr_model_brm10)
loo_m3 <- loo(hr_model_brm9)

comparisons <- loo_compare(loo_m1, loo_m2, loo_m3)
write.csv(comparisons, "results/half_range_traditional/loo_comparisons_models.csv")

# get loo weights
model_weights(hr_model_brm8, hr_model_brm10, hr_model_brm9, weights = "loo") %>% round(digits = 2)

# Plot region variation

# Get unique regions
region_levels <- unique(visits_dists_all_2$region)
visits_dists_all_2$age <- as.factor(visits_dists_all_2$age)
visits_dists_all_2$sex <- as.factor(visits_dists_all_2$sex)

# Expand nd to include regions
nd <- expand.grid(
  sex = levels(visits_dists_all_2$sex),
  age = levels(visits_dists_all_2$age),
  region = region_levels
)

# Predict values for each region
region_predictions <- fitted(
  hr_model_brm8,
  re_formula = ~ (1 | region),
  newdata = nd,
  probs = c(.015, .025, .055, 0.25, 0.75, .165,.10, .90, .945, .985, .975)) %>%
  as_tibble() %>%
  bind_cols(nd)

region_predictions_sl <- fitted(
  hr_model_brm9,
  re_formula = ~ (1 + age*sex | region),
  newdata = nd,
  probs = c(.015, .025, .055, 0.25, 0.75, .165,.10, .90, .945, .985, .975)) %>%
  as_tibble() %>%
  bind_cols(nd)

region_predictions_sl_simpler <- fitted(
  hr_model_brm10,
  re_formula = ~ (1 + age + sex | region),
  newdata = nd,
  probs = c(.015, .025, .055, 0.25, 0.75, .165,.10, .90, .945, .985, .975)) %>%
  as_tibble() %>%
  bind_cols(nd)

region_colors <- MetPalettes$Isfahan2[[1]][-1]  # Base colors
region_darker_colors <- scales::muted(region_colors, l = 50)  # Darker versions

# Plot variation across regions with lines
region_predictions_sl %>%
  ggplot(aes(x = age, y = Estimate / 1000, color = region, group = region)) +
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = region), alpha = 0.2, color=NA) +  # Outer ribbon
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = region), alpha = 0.5, color=NA) +  # Inner ribbon
  geom_line(size = 1, alpha = 1) +  # Line for each region
  facet_wrap(~ sex) +
  labs(
    x = "Age class",
    y = "Half range (km from residence)"
  ) +
  ggtitle("Variation Across Regions") +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne âgée" = "Old adult"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  scale_color_manual(values = region_darker_colors) +  # Darker lines
  scale_fill_manual(values = region_colors)           # Lighter ribbons

ggplot2::ggsave("results/half_range_traditional/region_variation_lines_randomslopes.png", width = 10, height = 7)

# Plot variation across regions with lines
region_predictions_sl %>%
  ggplot(aes(x = age, y = Estimate / 1000, color = region, group = region)) +
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = region), alpha = 0.2, color=NA) +  # Outer ribbon
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = region), alpha = 0.5, color=NA) +  # Inner ribbon
  geom_line(size = 1, alpha = 1) +  # Line for each region
  facet_wrap(~ sex) +
  labs(
    x = "Age class",
    y = "Half range (km from residence)"
  ) +
  ggtitle("Variation Across Regions") +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne âgée" = "Old adult"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  scale_color_manual(values = region_darker_colors) +  # Darker lines
  scale_fill_manual(values = region_colors)           # Lighter ribbons

ggplot2::ggsave("results/half_range_traditional/region_variation_lines_randomslopes_simpler.png", width = 10, height = 7)

# Plot variation across regions with lines
region_predictions %>%
  ggplot(aes(x = age, y = Estimate / 1000, color = region, group = region)) +
  geom_line(size = 1, alpha = 1) +  # Line for each region
  geom_ribbon(aes(ymin = Q10/1000, ymax = Q90/1000, fill = region), alpha = 0.2, color=NA) +
  geom_ribbon(aes(ymin = Q25/1000, ymax = Q75/1000, fill = region), alpha = 0.5, color=NA) +
  facet_wrap(~ sex) +
  labs(
    x = "Age class",
    y = "Half range (km from residence)"
  ) +
  ggtitle("Variation Across Regions") +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne âgée" = "Old adult"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  scale_color_manual(values = region_darker_colors) +  # Darker lines
  scale_fill_manual(values = region_colors)   

ggplot2::ggsave("results/half_range_traditional/region_variation_lines_randomintercepts.png", width = 10, height = 7)

nd <- expand.grid(
  sex = levels(visits_dists_all_2$sex),
  age = levels(visits_dists_all_2$age),
  region = "avg"
)

marginal_region <-
  fitted(hr_model_brm9 ,
         allow_new_levels = F,
         # here we explicitly tell brms we want to include the group-level effects
         re_formula = NA,
         # from the brms manual, this uses the "(multivariate) normal distribution implied by 
         #sample_new_levels = "gaussian",
         newdata = nd,
         probs = c(.015, .025, .055, 0.25, 0.75, .165,.10, .90, .945, .985, .975)) %>%
  as_tibble() %>%
  bind_cols(nd)

marginal_region_8 <-
  fitted(hr_model_brm8 ,
         allow_new_levels = F,
         # here we explicitly tell brms we want to include the group-level effects
         re_formula = NA,
         # from the brms manual, this uses the "(multivariate) normal distribution implied by 
         #sample_new_levels = "gaussian",
         newdata = nd,
         probs = c(.015, .025, .055, 0.25, 0.75, .165,.10, .90, .945, .985, .975)) %>%
  as_tibble() %>%
  bind_cols(nd)

marginal_region_10 <-
  fitted(hr_model_brm10 ,
         allow_new_levels = F,
         # here we explicitly tell brms we want to include the group-level effects
         re_formula = NA,
         # from the brms manual, this uses the "(multivariate) normal distribution implied by 
         #sample_new_levels = "gaussian",
         newdata = nd,
         probs = c(.015, .025, .055, 0.25, 0.75, .165,.10, .90, .945, .985, .975)) %>%
  as_tibble() %>%
  bind_cols(nd)

met_is2 <- c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])
dark_is2 <- scales::muted(met_is2, l = 10) 

marginal_region %>%
  ggplot(aes(x = age, y = Estimate / 1000, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = sex), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = sex), alpha = 0.6, color = NA) +
  labs(
    x = "Age class",
    y = "Half range (km from residence)",
    color = "Sex", # Updated legend title
    fill = "Sex"   # Updated legend title
  ) +
  ggtitle("Average across regions") +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne âgée" = "Old adult"
    )
  ) +  # Rename age levels for the x-axis
  scale_color_manual(
    values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]),
    labels = c("Women", "Men") # Updated legend labels
  ) +
  scale_fill_manual(
    values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]),
    labels = c("Women", "Men") # Updated legend labels
  ) +
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, 53, by = 10),# Adjust breaks as neede
    limits = c(0, 53)
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )

ggplot2::ggsave("results/half_range_traditional/fitted_preds_sex_age_int_randomslopes_marginal.png", width = 8, height=7)

marginal_region_8 %>%
  ggplot(aes(x = age, y = Estimate / 1000, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = sex), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = sex), alpha = 0.6, color = NA) +
  labs(
    x = "Age class",
    y = "Half range (km from residence)",
    color = "Sex", # Updated legend title
    fill = "Sex"   # Updated legend title
  ) +
  ggtitle("Average across regions") +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne âgée" = "Old adult"
    )
  ) +  # Rename age levels for the x-axis
  scale_color_manual(
    values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]),
    labels = c("Women", "Men") # Updated legend labels
  ) +
  scale_fill_manual(
    values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]),
    labels = c("Women", "Men") # Updated legend labels
  ) +
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, 53, by = 10),# Adjust breaks as neede
    limits = c(0, 53)
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )
ggplot2::ggsave("results/half_range_traditional/fitted_preds_sex_age_int_randomintercept_marginal.png", width = 8, height=7)


marginal_region_10 %>%
  ggplot(aes(x = age, y = Estimate / 1000, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = sex), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = sex), alpha = 0.6, color = NA) +
  labs(
    x = "Age class",
    y = "Half range (km from residence)",
    color = "Sex", # Updated legend title
    fill = "Sex"   # Updated legend title
  ) +
  ggtitle("Average across regions") +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne âgée" = "Old adult"
    )
  ) +  # Rename age levels for the x-axis
  scale_color_manual(
    values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]),
    labels = c("Women", "Men") # Updated legend labels
  ) +
  scale_fill_manual(
    values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]),
    labels = c("Women", "Men") # Updated legend labels
  ) +
  theme_minimal() +
  scale_y_continuous(
    breaks = seq(0, 50, by = 10),# Adjust breaks as neede
    limits = c(0, 50)
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )

ggplot2::ggsave("results/half_range_traditional/fitted_preds_sex_age_int_randomslopes_simpler_marginal.png", width = 8, height=7)


write.csv(region_predictions_sl, "results/half_range_traditional/region_variation_randomslopes_hr.csv")
write.csv(region_predictions_sl_simpler, "results/half_range_traditional/region_variation_randomslopes_simpler_hr.csv")
write.csv(region_predictions, "results/half_range_traditional/region_variation_randomintercept_hr.csv")
write.csv(marginal_region, "results/half_range_traditional/region_variation_randomslope_hr_average_region.csv")
write.csv(marginal_region_8, "results/half_range_traditional/region_variation_randomintercept_hr_average_region.csv")
write.csv(marginal_region_10, "results/half_range_traditional/region_variation_randomslopes_simpler_hr_average_region.csv")

##############################
## now for max dist from res #
##############################

visits_dists_all_3 <- subset(visits_dists_all_2, visits_dists_all_2$max_dist >= 0)


m1.prior <- c(prior(student_t(3, 11.5, 10), class=Intercept),
              prior(gamma(0.01, 0.01), class=shape),
              prior(student_t(3, 0, 10), class=sd), 
              prior(normal( 0, 10), class=b))

# alternative with flat priors for coeffs
m1.prior2 <- c(prior(student_t(3, 11.5, 10), class=Intercept),
               prior(gamma(0.01, 0.01), class=shape),
               prior(student_t(3, 0, 10), class=sd))

# recommended except no flat prior for beta coeff
m1.prior3 <- c(prior(student_t(3, 11.5, 2.5), class=Intercept),
               prior(gamma(0.01, 0.01), class=shape),
               prior(student_t(3, 0, 2.5), class=sd), 
               prior(normal( 0, 5), class=b))

m5.prior_weak <- c(
  prior(student_t(3, 4, 2), class = Intercept),  # Reflecting log(50 km)
  prior(gamma(2, 0.1), class = shape),          # Shape parameter
  prior(student_t(3, 0, 5), class = sd),        # Random effect SD
  prior(normal(0, 3), class = b)                # Broader fixed effect coefficients
)


formula_m5 <- bf(
  max_dist ~ age + sex + 
    (1 + age + sex | region) +  # Random intercept and slopes for `age` and `sex` by region
    (1 | region:residence_camp),  # Random intercept for residence_camp nested within region
  family = Gamma(link = "log")
)

formula_m6 <- bf(
  max_dist ~ age * sex +                # Fixed effects: interaction between age and sex
    (1 | region) +             
    (1 | region:residence_camp),           # Nested random intercept for residence_camp within region
  family = Gamma(link = "log")
)

formula_m7 <- bf(
  max_dist ~ age * sex +                # Fixed effects: interaction between age and sex
    (1 + age*sex | region) +             # Random slopes for interaction by region
    (1 | region:residence_camp),           # Nested random intercept for residence_camp within region
  family = Gamma(link = "log")
)

mr_model_brm7 <- brm(formula=formula_m5, data=visits_dists_all_3,warmup=1000,
                     iter=5000, chains=3, control=list(adapt_delta=0.95), prior = m5.prior_weak) 

mr_model_brm8 <- brm(formula=formula_m6, data=visits_dists_all_3,warmup=1000,
                     iter=5000, chains=3, control=list(adapt_delta=0.95), prior = m5.prior_weak) 

mr_model_brm9 <- brm(formula=formula_m7, data=visits_dists_all_3,warmup=1000,
                     iter=5000, chains=3, control=list(adapt_delta=0.95), prior = m5.prior_weak) 

# Save model as r object
saveRDS(mr_model_brm9, file = "models/mr_model_brm_randomslope_region_traditional.rds")
saveRDS(mr_model_brm8, file = "models/mr_model_brm_randomintercepts_region_traditional.rds")
saveRDS(mr_model_brm7, file = "models/mr_model_brm_randomslopes_region_simpler_traditional.rds")

# load models
mr_model_brm9 <- readRDS("models/mr_model_brm_randomslope_region_traditional.rds")
mr_model_brm8 <- readRDS("models/mr_model_brm_randomintercepts_region_traditional.rds")
mr_model_brm7 <- readRDS("models/mr_model_brm_randomslopes_region_simpler_traditional.rds")

# Plot region variation

# Get unique regions
region_levels <- unique(visits_dists_all_3$region)
visits_dists_all_3$sex <- as.factor(visits_dists_all_3$sex)
visits_dists_all_3$age <- as.factor(visits_dists_all_3$age)

# Expand nd to include regions
nd <- expand.grid(
  sex = levels(visits_dists_all_3$sex),
  age = levels(visits_dists_all_3$age),
  region = region_levels
)

# Predict values for each region
region_predictions <- fitted(
  mr_model_brm8,
  allow_new_levels = TRUE, 
  re_formula = ~ (1 | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.015, .025, .055, 0.25, 0.75, .165,.10, .90, .945, .985, .975)) %>%
  as_tibble() %>%
  bind_cols(nd)

region_predictions_sl <- fitted(
  mr_model_brm9,
  allow_new_levels = TRUE, 
  re_formula = ~ (1 | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.015, .025, .055, 0.25, 0.75, .165,.10, .90, .945, .985, .975)) %>%
  as_tibble() %>%
  bind_cols(nd)

region_predictions_sl_simpler <- fitted(
  mr_model_brm7,
  allow_new_levels = TRUE, 
  re_formula = ~ (1 | region),
  sample_new_levels = "gaussian",
  newdata = nd,
  probs = c(.015, .025, .055, 0.25, 0.75, .165,.10, .90, .945, .985, .975)) %>%
  as_tibble() %>%
  bind_cols(nd)


# Plot variation across regions with lines
region_predictions_sl %>%
  ggplot(aes(x = age, y = Estimate / 1000, color = region, group = region)) +
  geom_line(size = 1, alpha = 1) +  # Line for each region
  geom_ribbon(aes(ymin = Q10/1000, ymax = Q90/1000, fill = region), alpha = 0.2, color=NA) +
  geom_ribbon(aes(ymin = Q25/1000, ymax = Q75/1000, fill = region), alpha = 0.5, color=NA) +
  facet_wrap(~ sex) +
  labs(
    x = "Age class",
    y = "Max distance travelled (km from residence)"
  ) +
  ggtitle("Variation Across Regions") +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne âgée" = "Old adult"
    )
  ) +  # Rename age levels for the x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  scale_color_manual(values = region_darker_colors[-1]) +  # Darker central lines
  scale_fill_manual(values = region_colors[-1])     

ggplot2::ggsave("results/half_range_traditional/region_variation_lines_randomslopes_max_range.png", width = 10, height = 7)

# Plot variation across regions with lines
region_predictions_sl_simpler %>%
  ggplot(aes(x = age, y = Estimate / 1000, color = region, group = region)) +
  geom_line(size = 1, alpha = 1) +  # Line for each region
  geom_ribbon(aes(ymin = Q10/1000, ymax = Q90/1000, fill = region), alpha = 0.2, color=NA) +
  geom_ribbon(aes(ymin = Q25/1000, ymax = Q75/1000, fill = region), alpha = 0.5, color=NA) +
  facet_wrap(~ sex) +
  labs(
    x = "Age class",
    y = "Max distance travelled (km from residence)"
  ) +
  ggtitle("Variation Across Regions") +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne âgée" = "Old adult"
    )
  ) +  # Rename age levels for the x-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  scale_color_manual(values = region_darker_colors[-1]) +  # Darker central lines
  scale_fill_manual(values = region_colors[-1])     

ggplot2::ggsave("results/half_range_traditional/region_variation_lines_randomslopes_max_range_simpler.png", width = 10, height = 7)


# Posterior checks
pp_check(hr_model_brm9, nsamples = 1000, nreps = 1000, group = "region", theme = theme_minimal())

# Plot variation across regions with lines
region_predictions %>%
  ggplot(aes(x = age, y = Estimate / 1000, color = region, group = region)) +
  geom_line(size = 1, alpha = 1) +  # Line for each region
  geom_ribbon(aes(ymin = Q10/1000, ymax = Q90/1000, fill = region), alpha = 0.2, color=NA) +
  geom_ribbon(aes(ymin = Q25/1000, ymax = Q75/1000, fill = region), alpha = 0.5, color=NA) +
  facet_wrap(~ sex) +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne âgée" = "Old adult"
    )) + 
  labs(
    x = "Age class",
    y = "Max distance travelled (km from residence)"
  ) +
  ggtitle("Variation Across Regions") +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  ) +
  scale_color_manual(values = region_darker_colors[-1]) +  # Darker central lines
  scale_fill_manual(values = region_colors[-1])     

ggplot2::ggsave("results/half_range_traditional/region_variation_lines_randomintercepts_max_range.png", width = 10, height = 7)

nd <- expand.grid(
  sex = levels(visits_dists_all_3$sex),
  age = levels(visits_dists_all_3$age),
  region = "avg"
)


marginal_region <-
  fitted(mr_model_brm9 ,
         # this allows us to simulate values for our counterfactual island, "my_island"
         allow_new_levels = F,
         # here we explicitly tell brms we want to include the group-level effects
         re_formula = NA,
         # from the brms manual, this uses the "(multivariate) normal distribution implied by 
         sample_new_levels = "gaussian",
         newdata = nd,
         probs = c(.015, .025, .055, 0.25, 0.75, .165,.10, .90, .945, .985, .975)) %>%
  as_tibble() %>%
  bind_cols(nd)

marginal_region_8 <-
  fitted(mr_model_brm8 ,
         # this allows us to simulate values for our counterfactual island, "my_island"
         allow_new_levels = F,
         # here we explicitly tell brms we want to include the group-level effects
         re_formula = NA,
         # from the brms manual, this uses the "(multivariate) normal distribution implied by 
         sample_new_levels = "gaussian",
         newdata = nd,
         probs = c(.015, .025, .055, 0.25, 0.75, .165,.10, .90, .945, .985, .975)) %>%
  as_tibble() %>%
  bind_cols(nd)

marginal_region_7 <-
  fitted(mr_model_brm7 ,
         # this allows us to simulate values for our counterfactual island, "my_island"
         allow_new_levels = F,
         # here we explicitly tell brms we want to include the group-level effects
         re_formula = NA,
         # from the brms manual, this uses the "(multivariate) normal distribution implied by 
         #sample_new_levels = "gaussian",
         newdata = nd,
         probs = c(.015, .025, .055, 0.25, 0.75, .165,.10, .90, .945, .985, .975)) %>%
  as_tibble() %>%
  bind_cols(nd)

marginal_region %>%
  ggplot(aes(x = age, y = Estimate / 1000, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = sex), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = sex), alpha = 0.6, color = NA) +
  labs(
    x = "Age class",
    y = "Max distance travelled in lifetime (km)",
    color = "Sex", # Updated legend title
    fill = "Sex"   # Updated legend title
  ) +
  ggtitle("Average across regions") +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne âgée" = "Old adult"
    )
  ) +  # Rename age levels for the x-axis
  scale_color_manual(
    values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]),
    labels = c("Women", "Men") # Updated legend labels
  ) +
  scale_fill_manual(
    values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]),
    labels = c("Women", "Men") # Updated legend labels
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )

ggplot2::ggsave("results/half_range_traditional/fitted_preds_sex_age_int_randomslopes_marginal_max_range.png", width = 8, height=7)

marginal_region_8 %>%
  ggplot(aes(x = age, y = Estimate / 1000, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = sex), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = sex), alpha = 0.6, color = NA) +
  labs(
    x = "Age class",
    y = "Max distance travelled in lifetime (km)",
    color = "Sex", # Updated legend title
    fill = "Sex"   # Updated legend title
  ) +
  ggtitle("Average across regions") +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne âgée" = "Old adult"
    )
  ) +  # Rename age levels for the x-axis
  scale_color_manual(
    values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]),
    labels = c("Women", "Men") # Updated legend labels
  ) +
  scale_fill_manual(
    values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]),
    labels = c("Women", "Men") # Updated legend labels
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )


ggplot2::ggsave("results/half_range_traditional/fitted_preds_sex_age_int_randomintercept_marginal_max_range.png", width = 8, height=7)

marginal_region_7 %>%
  ggplot(aes(x = age, y = Estimate / 1000, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Q10 / 1000, ymax = Q90 / 1000, fill = sex), alpha = 0.2, color = NA) +
  geom_ribbon(aes(ymin = Q25 / 1000, ymax = Q75 / 1000, fill = sex), alpha = 0.6, color = NA) +
  labs(
    x = "Age class",
    y = "Max distance travelled in lifetime (km)",
    color = "Sex", # Updated legend title
    fill = "Sex"   # Updated legend title
  ) +
  ggtitle("Average across regions") +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne âgée" = "Old adult"
    )
  ) +  # Rename age levels for the x-axis
  scale_color_manual(
    values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]),
    labels = c("Women", "Men") # Updated legend labels
  ) +
  scale_fill_manual(
    values = c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6]),
    labels = c("Women", "Men") # Updated legend labels
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Larger plot title
    axis.title = element_text(size = 14),              # Larger axis titles
    axis.text = element_text(size = 12),               # Larger axis tick labels
    legend.title = element_text(size = 14),            # Larger legend title
    legend.text = element_text(size = 12)              # Larger legend text
  )
ggplot2::ggsave("results/half_range_traditional/fitted_preds_sex_age_int_randomslopes_marginal_max_range_simpler.png", width = 8, height=7)


write.csv(region_predictions_sl, "results/half_range_traditional/region_variation_randomslopes_mr.csv")
write.csv(region_predictions, "results/half_range_traditional/region_variation_randomintercept_mr.csv")
write.csv(region_predictions_sl_simpler, "results/half_range_traditional/region_variation_randomslopes_mr_simpler.csv")
write.csv(marginal_region, "results/half_range_traditional/region_variation_randomslope_mr_average_region.csv")
write.csv(marginal_region_8, "results/half_range_traditional/region_variation_randomintercept_mr_average_region.csv")
write.csv(marginal_region_7, "results/half_range_traditional/region_variation_randomslopes_simpler_mr_average_region.csv")

# Read in random slope and random intercept models and compare

loo_m1 <- loo(mr_model_brm8)
loo_m2 <- loo(mr_model_brm9)
loo_m3 <- loo(mr_model_brm7)

loo_compare(loo_m1, loo_m2, loo_m3)

# save comparison
write.csv(loo_compare(loo_m1, loo_m2, loo_m3), "results/half_range_traditional/loo_comparison_randomslopes_randomintercepts_mr.csv")

