
# load packages
library(measurements)
library(tidyverse)
library(ggplot2)
library(ggstatsplot)
library(brms)

# if overall
data <- read.csv("results/reasons_visit_traditional/places_visited_reasons_proportion_nov_24_traditional.csv")

check_encoding <- function(column) {
  bad_rows <- !is.na(iconv(column, from = "UTF-8", to = "UTF-8"))
  return(bad_rows)
}

encoding_issues <- data %>%
  mutate(across(everything(), ~check_encoding(.)))

# Step 3: Replace Problematic Characters
# Replace specific known problematic characters with clean versions
data_cleaned <- data %>%
  mutate(across(everything(), ~str_replace_all(., "[^\x20-\x7E]", ""))) %>% # Remove non-ASCII characters
  mutate(across(everything(), ~iconv(., from = "", to = "UTF-8", sub = "byte"))) # Force UTF-8

data_cleaned <- data_cleaned %>%
  mutate(across(everything(), ~na_if(., "#NAME?")))

write.csv(data_cleaned, "data/final/places_visited_reasons_proportion_nov_24_traditional_cleaned.csv")

# subset non valid sex or age
data_cleaned <- subset(data_cleaned, data_cleaned$age != "")
data_cleaned <- subset(data_cleaned, data_cleaned$sex != "")
data_cleaned <- subset(data_cleaned, data_cleaned$age != "FALSE")
data_cleaned <- subset(data_cleaned, data_cleaned$sex != "FALSE")

##########################
## Start building models #
##########################

# turn columns 11:20  to numeric
data_cleaned <- data_cleaned %>%
  mutate(across(11:19, as.numeric))

# Define response variables and their corresponding models
response_vars <- c(
  "times_fish", "times_hunt", 
  "times_friend_visit", "times_dance", "times_fam_visit", 
  "times_gather", "times_live", "times_just_visit"
)


# Cap places visited to max sum of reasons
for (i in 1:nrow(data_cleaned)) {
  if (data_cleaned$places_visited[i] < sum(data_cleaned[i, 12:19])) {
    print("places less than sum")
    data_cleaned$places_visited[i] <- sum(data_cleaned[i, 12:19])
  }
}

# Define the weak priors - basically decrease after 0.3 and be around 0.1
binomial_priors <- c(
  prior(normal(0, 1.5), class = "b"),
  prior(normal(-1,1), class = "Intercept")
)

# Fit models for each response variable with sex and age interaction
# Define a list to store model results
model_results <- list()
for (var in response_vars) {
  formula <- bf(as.formula(paste0(var, " | trials(places_visited) ~ age*sex +
                                  (1 + age + sex | region) + (1 | region:residence_camp)")))
  
  # Fit the model using brms
  model <- brm(
    formula = formula,
    data = data_cleaned, 
    family = binomial(link = "logit"),
    prior = binomial_priors,
    chains = 3, 
    iter = 5000, 
    warmup = 1000, 
    cores = 4, 
    control = list(adapt_delta = 0.99),
    file = paste0("models/", var, "_model_fit_traditional_25_delta99")
  )
  
  # Store the model in the results list
  model_results[[var]] <- model
}

# Save all models to disk for later inspection
saveRDS(model_results, file = "models/binomial_models_results_interaction_prop_places_visited_reason_traditional_delta99.rds")

# Fit models for each response variable with sex and age and NO interaction
# Define a list to store model results
model_results_no_int <- list()
for (var in response_vars) { # we dont need ID random effect as no repeated obs
  formula <- bf(as.formula(paste0(var, " | trials(places_visited) ~ age + sex +
                                  (1 + age + sex | region) + (1 | region:residence_camp)")))
  
  # Fit the model using brms
  model <- brm(
    formula = formula,
    data = data_cleaned, 
    family = binomial(link = "logit"),
    prior = binomial_priors,
    chains = 3, 
    iter = 5000, 
    warmup = 1000, 
    cores = 4, 
    control = list(adapt_delta = 0.95),
    file = paste0("models/", var, "_model_fit_no_int_traditional")
  )
  
  # Store the model in the results list
  model_results_no_int[[var]] <- model
}

# Save all models to disk for later inspection
saveRDS(model_results_no_int, file = "models/binomial_models_results_no_interaction_prop_places_visited_reason_traditional.rds")

# Fit models for each response variable with sex and age and NO interaction
# Define a list to store model results
model_results_intercept <- list()
for (var in response_vars) { # we dont need ID random effect as no repeated obs
  formula <- bf(as.formula(paste0(var, " | trials(places_visited) ~ age*sex +
                                  (1 | region) + (1 | region:residence_camp)")))
  
  # Fit the model using brms
  model <- brm(
    formula = formula,
    data = data_cleaned, 
    family = binomial(link = "logit"),
    prior = binomial_priors,
    chains = 3, 
    iter = 5000, 
    warmup = 1000, 
    cores = 4, 
    control = list(adapt_delta = 0.95),
    file = paste0("models/", var, "_model_intercept_traditional")
  )
  
  # Store the model in the results list
  model_results_intercept[[var]] <- model
}

# Save all models to disk for later inspection
saveRDS(model_results_intercept, file = "models/binomial_models_results_interaction_prop_places_visited_reason_randomintercept_traditional.rds")

#######################################################################
## Compare whether models with an interaction perform better than those without

library(loo)

options(mc.cores = 4)  # Automatically detect the maximum available cores

# Load the saved model results
model_results <- readRDS("models/binomial_models_results_interaction_prop_places_visited_reason_traditional.rds")
model_results_no_int <- readRDS("models/binomial_models_results_no_interaction_prop_places_visited_reason_traditional.rds")
model_results_intercept <- readRDS("models/binomial_models_results_interaction_prop_places_visited_reason_randomintercept_traditional.rds")

# Initialize list to store LOO comparisons
loo_comparisons <- list()

# Loop through response variables to perform LOO comparisons
for (var in response_vars) {
  # Extract the models for the current response variable
  model_with_int <- model_results[[var]]
  model_no_int <- model_results_no_int[[var]]
  model_intercept <- model_results_intercept[[var]]
  
  # Perform LOO with moment matching to handle high pareto_k values
  loo_with_int <- loo(model_with_int)
  loo_no_int <- loo(model_no_int)
  loo_intercept <- loo(model_intercept)
  
  # Check if loo_compare works without errors
  if (!is.null(loo_with_int) && !is.null(loo_no_int)) {
    # Perform LOO comparison
    loo_comparison <- loo_compare(loo_with_int, loo_no_int, loo_intercept)
    
    # Extract results into a data frame
    comparison_results <- data.frame(
      Response_Variable = var,
      Model = c("With Interaction", "No Interaction", "Random intercept"),
      #LOOIC = c(loo_comparison[1, "looic"], loo_comparison[2, "looic"]),
      ELPD_Diff = loo_comparison[,1],
      SE_ELPD_Diff = loo_comparison[,2]
    )
    
    # Append to the list
    loo_comparisons[[var]] <- comparison_results
  } else {
    warning(paste("LOO comparison failed for variable:", var))
  }
}

# Combine all comparisons into a single data frame
all_loo_comparisons <- do.call(rbind, loo_comparisons)

# Save the results to a CSV file
write.csv(all_loo_comparisons, "results/reasons_visit_proportion_traditional/loo_comparisons_results.csv", row.names = FALSE)

####################
# Plot the results #
####################

library(MetBrewer)

# Define activity labels
activity_labels <- c(
  "times_dance" = "Rituals",
  "times_fam_visit" = "Visit family",
  "times_fish" = "Fish",
  "times_friend_visit" = "Visit friends",
  "times_gather" = "Gather",
  "times_hunt" = "Hunt",
  "times_just_visit" = "Explore",
  "times_live" = "Live"
)

# Set region colors using MetBrewer
region_colors <- MetPalettes$Isfahan2[[1]]  # Base colors

# Initialize a list to store posterior predictions for each activity model
activity_predictions <- list()

# Loop through the fitted models and generate predictions
for (activity in names(model_results)) {
  # Extract the model for the activity
  model <- model_results[[activity]]
  
  # Step 1: Identify valid combinations of age, sex, and region for this activity
  valid_groups <- data_cleaned %>%
    filter(.data[[activity]] > 0) %>%  # Filter rows where the activity count is greater than 0
    group_by(age, sex, region) %>%
    summarise(
      places_visited = sum(places_visited, na.rm = TRUE), 
      n = n(), 
      .groups = "drop"
    ) %>%
    filter(n >= 1)  # Ensure there is at least 1 observation
  
  # Step 2: Create newdata for predictions
  activity_newdata <- expand.grid(
    age = unique(data_cleaned$age),
    sex = unique(data_cleaned$sex),
    region = unique(data_cleaned$region),
    places_visited = 1  # Simulate 100 places visited for each group
  )
  
  # Filter `activity_newdata` to include only valid combinations
  activity_newdata <- activity_newdata %>%
    inner_join(valid_groups, by = c("age", "sex", "region"))
  
  activity_newdata$places_visited.y <- NULL
  activity_newdata$places_visited <- activity_newdata$places_visited.x
  
  # Generate posterior predictions
  predictions <- fitted(
    model,
    newdata = activity_newdata,
    re_formula = ~ (1 + age + sex | region),  # Include random effects
    probs = c(0.025, 0.10, 0.25, 0.75, 0.90, 0.975)
  ) %>%
    as_tibble() %>%
    bind_cols(activity_newdata) %>%
    mutate(activity = activity)  # Add activity label
  
  # Store predictions
  activity_predictions[[activity]] <- predictions
}

# Combine predictions into a single data frame
all_predictions <- bind_rows(activity_predictions)

region_levels <- c("Macao", "Minganga", "Enyelle", "Betou")

# Update the `all_predictions` data frame
all_predictions <- all_predictions %>%
  mutate(
    sex = recode(sex, "F" = "Women", "M" = "Men"),
    activity = recode(activity, !!!activity_labels),
    region = factor(str_to_title(region), levels = region_levels),  # Set the factor levels for region
    age = factor(age, levels = c("Adolescent", "Adulte", "Personne ge"))  # Order age categories
  )

# save data frame
write.csv(all_predictions, "results/reasons_visit_proportion_traditional/all_predictions_regions_separate.csv")


# Generate the plot
all_predictions %>%
  ggplot(aes(x = age, y = Estimate, fill = region)) +
  # Bar plot with posterior mean
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, na.rm = TRUE) +
  # Add error bars for credible intervals
  geom_errorbar(
    aes(ymin = Q10, ymax = Q90),
    position = position_dodge(width = 0.8),
    width = 0.25,
    na.rm = TRUE
  ) +
  # Facet by activity and sex with equal row heights
  facet_grid(activity ~ sex, scales = "fixed", space = "fixed", switch = "y") +
  labs(
    x = "Age class",
    y = "Predicted probability",
    title = "Activity-specific probability predictions by region"
  ) +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne ge" = "Old adult"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    strip.text.x = element_text(size = 14),  # Sex facet labels
    strip.text.y = element_text(size = 14),  # Activity facet labels
    strip.placement = "outside",  # Place activity labels outside the plot
    panel.spacing = unit(1, "lines")  # Consistent spacing between panels
  ) +
  scale_fill_manual(values = region_colors[-1])

# Save the plot
ggplot2::ggsave("results/reasons_visit_proportion_traditional/activity_specific_probability_plot_simulated_places_fixed.png", width = 12, height = 15)

# We now average across regions

dark_sex_colors <- c( MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])  # Dark colors for lines
light_sex_colors <- scales::muted(dark_sex_colors, l = 60)  # Lighter colors for ribbons

# Initialize a list to store posterior predictions for each activity model
activity_predictions <- list()

# Loop through the fitted models and generate predictions
for (activity in names(model_results)) {
  # Extract the model for the activity
  model <- model_results[[activity]]
  
  # Step 1: Identify valid combinations of age, sex, and region for this activity
  valid_groups <- data_cleaned %>%
    group_by(age, sex) %>%
    filter(.data[[activity]] > 0) %>%  # Filter rows where the activity count is greater than 0
    summarise(
      n = n(), 
      .groups = "drop"
    ) %>%
    filter(n >= 2)  # Ensure there is at least 2 observationa
  
  # Step 2: Create newdata for predictions
  activity_newdata <- expand.grid(
    age = unique(data_cleaned$age),
    sex = unique(data_cleaned$sex),
    places_visited = 100 , # Simulate 1
    region= "average"
  )
  
  # Filter `activity_newdata` to include only valid combinations
  activity_newdata <- activity_newdata %>%
    inner_join(valid_groups, by = c("age", "sex"))
  
  # Generate posterior predictions
  predictions <- fitted(
    model,
    newdata = activity_newdata,
    re_formula =    ~ (1 | region),  # Average random effects
    allow_new_levels = TRUE,
    probs = c(0.025, 0.10, 0.25, 0.75, 0.90, 0.975)
  ) %>%
    as_tibble() %>%
    bind_cols(activity_newdata) %>%
    mutate(activity = activity)  # Add activity label
  
  # Store predictions
  activity_predictions[[activity]] <- predictions
}

# Combine predictions into a single data frame
all_predictions <- bind_rows(activity_predictions)

# Ensure predictions are on the probability scale and clean labels
all_predictions <- all_predictions %>%
  mutate(
    sex = recode(sex, "F" = "Women", "M" = "Men"),
    activity = recode(activity, !!!activity_labels),
    age = factor(age, levels = c("Adolescent", "Adulte", "Personne ge"))  # Order age categories
  )

# save data frame
write.csv(all_predictions, "results/reasons_visit_proportion_traditional/all_predictions_regions_averaged.csv")

# Generate the plot
all_predictions %>%
  ggplot(aes(x = age, y = Estimate / 100, color = sex, group = sex)) +
  geom_ribbon(aes(ymin = Q10 / 100, ymax = Q90 / 100, fill = sex), alpha = 0.2, color = NA) +
  # Inner ribbon for 25%-75% quantiles
  geom_ribbon(aes(ymin = Q25 / 100, ymax = Q75 / 100, fill = sex), alpha = 0.6, color = NA) +
  # Lines for each sex
  geom_line(size = 1) +
  # Facet by activity and sex with equal row heights
  facet_wrap(~activity , scales = "fixed", ncol=3) +
  labs(
    x = "Age class",
    y = "Predicted probability",
    title = "Average actoss regions"
  ) +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne ge" = "Old adult"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12),
    panel.spacing = unit(1, "lines")
  ) +
  scale_fill_manual(values = dark_sex_colors) +
  scale_color_manual(values = dark_sex_colors)

# Save the plot
ggplot2::ggsave("results/reasons_visit_proportion_traditional/activity_specific_probability_plot_averaged_regions.png", width = 10, height = 8)

# Now bar plot
all_predictions %>%
  ggplot(aes(x = age, y = Estimate / 100, color = sex, group = sex)) +
  geom_linerange(aes(ymin = Q25 / 100, ymax = Q75 / 100, color = sex), 
                 size = 5, alpha = 0.5, position = position_dodge(width = 0.5)) +
  
  geom_errorbar(
    aes(ymin = Q10 / 100, ymax = Q90 / 100),
    width = 0.2, position = position_dodge(width = 0.5)
  ) +
  # Points for posterior mean
  geom_point(
    size = 3,
    aes(color = sex),  # Use darker colors for the dots
    position = position_dodge(width = 0.5)
  ) +
  # Facet by activity and sex with equal row heights
  facet_wrap(~activity , scales = "fixed", ncol=3) +
  labs(
    x = "Age class",
    y = "Predicted probability",
    title = "Average actoss regions"
  ) +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult",
      "Personne ge" = "Old adult"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12),
    panel.spacing = unit(1, "lines")
  ) +
  #scale_fill_manual(values = dark_sex_colors) +
  scale_color_manual(values = light_sex_colors)

# Save the plot
ggplot2::ggsave("results/reasons_visit_proportion_traditional/activity_specific_probability_plot_averaged_regions_box.png", width = 10, height = 8)

# Combine "Adult" and "Old adult" into a single category
all_predictions <- all_predictions %>%
  mutate(age_group = case_when(
    age %in% c("Adulte", "Personne ge") ~ "Adult & Old adult",
    age == "Adolescent" ~ "Adolescent"
  )) %>%
  group_by(activity, sex, age_group) %>%
  summarise(
    Estimate = mean(Estimate, na.rm = TRUE),
    Q10 = mean(Q10, na.rm = TRUE),
    Q25 = mean(Q25, na.rm = TRUE),
    Q75 = mean(Q75, na.rm = TRUE),
    Q90 = mean(Q90, na.rm = TRUE),
    .groups = "drop"
  )

# Ribbon plot
all_predictions %>%
  ggplot(aes(x = age_group, y = Estimate / 100, color = sex, group = sex)) +
  # Outer ribbon for 10%-90% quantiles
  geom_ribbon(aes(ymin = Q10 / 100, ymax = Q90 / 100, fill = sex), alpha = 0.2, color = NA) +
  # Inner ribbon for 25%-75% quantiles
  geom_ribbon(aes(ymin = Q25 / 100, ymax = Q75 / 100, fill = sex), alpha = 0.6, color = NA) +
  # Lines for each sex
  geom_line(size = 1) +
  # Facet by activity
  facet_wrap(~activity, scales = "fixed", ncol = 3) +
  labs(
    x = "Age group",
    y = "Predicted probability",
    title = "Activity-specific probability predictions (Adult & Old adult combined)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12),
    panel.spacing = unit(1, "lines")
  ) +
  scale_fill_manual(values = dark_sex_colors) +
  scale_color_manual(values = light_sex_colors)

# Save the ribbon plot
ggplot2::ggsave("results/reasons_visit_proportion_traditional/activity_specific_probability_ribbon_averaged_age.png", width = 10, height = 8)

# Bar plot
all_predictions %>%
  ggplot(aes(x = age_group, y = Estimate / 100, color = sex, group = sex)) +
  # Interquartile range (Q25-Q75) as wide lineranges
  geom_linerange(
    aes(ymin = Q25 / 100, ymax = Q75 / 100, color = sex),
    size = 5, alpha = 0.5, position = position_dodge(width = 0.5)
  ) +
  # Add error bars for Q10-Q90
  geom_errorbar(
    aes(ymin = Q10 / 100, ymax = Q90 / 100),
    width = 0.2, position = position_dodge(width = 0.5)
  ) +
  # Points for posterior mean
  geom_point(
    size = 3,
    aes(color = sex),
    position = position_dodge(width = 0.5)
  ) +
  # Facet by activity
  facet_wrap(~activity, scales = "fixed", ncol = 3) +
  labs(
    x = "Age group",
    y = "Predicted probability",
    title = "Activity-specific probability predictions (Adult & Old adult combined)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12),
    panel.spacing = unit(1, "lines")
  ) +
  scale_color_manual(values = light_sex_colors)

# Save the bar plot
ggplot2::ggsave("results/reasons_visit_proportion_traditional/activity_specific_probability_bar_averaged_age.png", width = 10, height = 8)

#####################
# We will now merge adults and old adults in a single category and refit models

data_cleaned$age_merge <- ifelse(data_cleaned$age == "Personne ge", "Adulte", data_cleaned$age)

# Fit models for each response variable with sex and age interaction
# Define a list to store model results
model_results <- list()
for (var in response_vars) {
  formula <- bf(as.formula(paste0(var, " | trials(places_visited) ~ age_merge*sex +
                                  (1 + age_merge + sex | region) + (1 | region:residence_camp)")))
  
  # Fit the model using brms
  model <- brm(
    formula = formula,
    data = data_cleaned, 
    family = binomial(link = "logit"),
    prior = binomial_priors,
    chains = 3, 
    iter = 5000, 
    warmup = 1000, 
    cores = 4, 
    control = list(adapt_delta = 0.99),
    file = paste0("models/", var, "_model_fit_adult_old_traditional_2025_99")
  )
  
  # Store the model in the results list
  model_results[[var]] <- model
}

# Save all models to disk for later inspection
saveRDS(model_results, file = "models/binomial_models_results_interaction_prop_places_visited_reason_adults_old_together_traditional_2025.rds")

# Read in the saved model results
model_results <- readRDS("models/binomial_models_results_interaction_prop_places_visited_reason_adults_old_together_traditional_2025.rds")

# # Set region colors using MetBrewer
region_colors <- MetPalettes$Isfahan2[[1]]  # Base colors

# Initialize a list to store posterior predictions for each activity model
activity_predictions <- list()

# Loop through the fitted models and generate predictions
for (activity in names(model_results)) {
  # Extract the model for the activity
  model <- model_results[[activity]]
  
  # Step 1: Identify valid combinations of age, sex, and region for this activity
  valid_groups <- data_cleaned %>%
    filter(.data[[activity]] > 0) %>%  # Filter rows where the activity count is greater than 0
    group_by(age_merge, sex, region) %>%
    summarise(
      places_visited = sum(places_visited, na.rm = TRUE), 
      n = n(), 
      .groups = "drop"
    ) %>%
    filter(n >= 1)  # Ensure there is at least 1 observation
  
  # Step 2: Create newdata for predictions
  activity_newdata <- expand.grid(
    age_merge = unique(data_cleaned$age_merge),
    sex = unique(data_cleaned$sex),
    region = unique(data_cleaned$region),
    places_visited = 1  # Simulate 100 places visited for each group
  )
  
  # Filter `activity_newdata` to include only valid combinations
  activity_newdata <- activity_newdata %>%
    inner_join(valid_groups, by = c("age_merge", "sex", "region"))
  
  activity_newdata$places_visited.y <- NULL
  activity_newdata$places_visited <- activity_newdata$places_visited.x
  
  # Generate posterior predictions
  predictions <- fitted(
    model,
    newdata = activity_newdata,
    re_formula = ~ (1 + age_merge + sex | region),  # Include random effects
    probs = c(0.025, 0.10, 0.25, 0.75, 0.90, 0.975)
  ) %>%
    as_tibble() %>%
    bind_cols(activity_newdata) %>%
    mutate(activity = activity)  # Add activity label
  
  # Store predictions
  activity_predictions[[activity]] <- predictions
}

# Combine predictions into a single data frame
all_predictions <- bind_rows(activity_predictions)

region_levels <- c("Macao", "Minganga", "Enyelle", "Betou")

# Update the `all_predictions` data frame
all_predictions <- all_predictions %>%
  mutate(
    sex = recode(sex, "F" = "Women", "M" = "Men"),
    activity = recode(activity, !!!activity_labels),
    region = factor(str_to_title(region), levels = region_levels),  # Set the factor levels for region
    age_merge = factor(age_merge, levels = c("Adolescent", "Adulte", "Personne ge"))  # Order age categories
  )


# save data frame
write.csv(all_predictions, "results/reasons_visit_proportion_traditional/all_predictions_regions_separate_regions_adult_old_merged.csv")

# Generate the plot
all_predictions %>%
  ggplot(aes(x = age_merge, y = Estimate, fill = region)) +
  # Bar plot with posterior mean
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, na.rm = TRUE) +
  # Add error bars for credible intervals
  geom_errorbar(
    aes(ymin = Q10, ymax = Q90),
    position = position_dodge(width = 0.8),
    width = 0.25,
    na.rm = TRUE
  ) +
  # Facet by activity and sex with equal row heights
  facet_grid(activity ~ sex, scales = "fixed", space = "fixed", switch = "y") +
  labs(
    x = "Age class",
    y = "Predicted probability",
    title = "Activity-specific probability predictions by region"
  ) +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    strip.text.x = element_text(size = 14),  # Sex facet labels
    strip.text.y = element_text(size = 14),  # Activity facet labels
    strip.placement = "outside",  # Place activity labels outside the plot
    panel.spacing = unit(1, "lines")  # Consistent spacing between panels
  ) +
  scale_fill_manual(values = region_colors[-1])

# Save the plot
ggplot2::ggsave("results/reasons_visit_traditional/activity_specific_probability_plot_simulated_places_fixed_adult_old_merge_traditional.png", width = 10, height = 15)

# We now average across regions

dark_sex_colors <- c( MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])  # Dark colors for lines
light_sex_colors <- scales::muted(dark_sex_colors, l = 60)  # Lighter colors for ribbons

# Initialize a list to store posterior predictions for each activity model
activity_predictions <- list()

# Loop through the fitted models and generate predictions
for (activity in names(model_results)) {
  # Extract the model for the activity
  model <- model_results[[activity]]
  
  # Step 1: Identify valid combinations of age, sex, and region for this activity
  valid_groups <- data_cleaned %>%
    group_by(age_merge, sex) %>%
    filter(.data[[activity]] > 0) %>%  # Filter rows where the activity count is greater than 0
    summarise(
      n = n(), 
      .groups = "drop"
    ) %>%
    filter(n >= 2)  # Ensure there is at least 2 observationa
  
  # Step 2: Create newdata for predictions
  activity_newdata <- expand.grid(
    age_merge = unique(data_cleaned$age_merge),
    sex = unique(data_cleaned$sex),
    places_visited = 100 , # Simulate 1
    region= "average"
  )
  
  # Filter `activity_newdata` to include only valid combinations
  activity_newdata <- activity_newdata %>%
    inner_join(valid_groups, by = c("age_merge", "sex"))
  
  # Generate posterior predictions
  predictions <- fitted(
    model,
    newdata = activity_newdata,
    re_formula =   ~ (1 | region),  # Average random effects
    allow_new_levels = TRUE,
    probs = c(0.025, 0.10, 0.25, 0.75, 0.90, 0.975)
  ) %>%
    as_tibble() %>%
    bind_cols(activity_newdata) %>%
    mutate(activity = activity)  # Add activity label
  
  # Store predictions
  activity_predictions[[activity]] <- predictions
}

# Combine predictions into a single data frame
all_predictions <- bind_rows(activity_predictions)

# Ensure predictions are on the probability scale and clean labels
all_predictions <- all_predictions %>%
  mutate(
    sex = recode(sex, "F" = "Women", "M" = "Men"),
    activity = recode(activity, !!!activity_labels),
    age = factor(age_merge, levels = c("Adolescent", "Adulte", "Personne ge"))  # Order age categories
  )

# save data frame
write.csv(all_predictions, "results/reasons_visit_proportion_traditional/all_predictions_regions_averaged_adult_old_merge.csv")

# Generate the plot
#all_predictions$sex <- factor(all_predictions$sex, levels = c("Women", "Men"))
all_predictions %>%
  ggplot(aes(x = age_merge, y = Estimate / 100, color = sex, group = sex)) +
  geom_ribbon(aes(ymin = Q10 / 100, ymax = Q90 / 100, fill = sex), alpha = 0.2, color = NA) +
  # Inner ribbon for 25%-75% quantiles
  geom_ribbon(aes(ymin = Q25 / 100, ymax = Q75 / 100, fill = sex), alpha = 0.6, color = NA) +
  # Lines for each sex
  geom_line(size = 1) +
  # Facet by activity and sex with equal row heights
  facet_wrap(~activity , scales = "fixed", ncol=3) +
  labs(
    x = "Age class",
    y = "Predicted probability",
    title = "Average actoss regions"
  ) +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 0),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    panel.spacing = unit(1, "lines"),
    axis.text.y = element_text(size = 12) ,
    axis.text.x = element_text(size = 12)  # Increase y-axis number size
  ) +
  scale_fill_manual(
    values = rev(dark_sex_colors),  # Keep original color mapping
    breaks = c("Women", "Men")  # Ensure order in legend
  ) +
  scale_color_manual(
    values = rev(dark_sex_colors),  # Keep original color mapping
    breaks = c("Women", "Men")  # Ensure order in legend
  )

# Save the plot
ggplot2::ggsave("results/reasons_visit_proportion_traditional/activity_specific_probability_plot_averaged_regions_adult_old_merge.png", width = 10, height = 8)

# Now bar plot
all_predictions %>%
  ggplot(aes(x = age_merge, y = Estimate / 100, color = sex, group = sex)) +
  geom_linerange(aes(ymin = Q25 / 100, ymax = Q75 / 100, color = sex), 
                 size = 5, alpha = 0.5, position = position_dodge(width = 0.5)) +
  
  geom_errorbar(
    aes(ymin = Q10 / 100, ymax = Q90 / 100),
    width = 0.2, position = position_dodge(width = 0.5)
  ) +
  # Points for posterior mean
  geom_point(
    size = 3,
    aes(color = sex),  # Use darker colors for the dots
    position = position_dodge(width = 0.5)
  ) +
  # Facet by activity and sex with equal row heights
  facet_wrap(~activity , scales = "fixed", ncol=3) +
  labs(
    x = "Age class",
    y = "Predicted probability",
    title = "Average actoss regions"
  ) +
  scale_x_discrete(
    labels = c(
      "Adolescent" = "Adolescent",
      "Adulte" = "Adult"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    strip.text = element_text(size = 12),
    panel.spacing = unit(1, "lines")
  ) +
  #scale_fill_manual(values = dark_sex_colors) +
  scale_color_manual(values = light_sex_colors)

# Save the plot
ggplot2::ggsave("results/reasons_visit_proportion_traditional/activity_specific_probability_plot_averaged_regions_box_adult_old_merge.png", width = 10, height = 8)

