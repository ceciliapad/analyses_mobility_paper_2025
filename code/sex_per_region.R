# Load required libraries
library(tidyverse)
library(MetBrewer)

# Read in your data
data <- read.table(header = TRUE, sep = "\t", text = "
Var1	Var2	Var3	Freq
Adolescent	Betou	F	4
Adulte	Betou	F	22
Enfant	Betou	F	32
Personne âgée	Betou	F	4
Adolescent	Dongou	F	20
Adulte	Dongou	F	72
Enfant	Dongou	F	107
Personne âgée	Dongou	F	26
Adolescent	Enyelle	F	9
Adulte	Enyelle	F	66
Enfant	Enyelle	F	59
Personne âgée	Enyelle	F	25
Adolescent	Macao	F	18
Adulte	Macao	F	83
Enfant	Macao	F	47
Personne âgée	Macao	F	24
Adolescent	Minganga	F	13
Adulte	Minganga	F	40
Enfant	Minganga	F	38
Personne âgée	Minganga	F	16
Adolescent	Betou	M	2
Adulte	Betou	M	22
Enfant	Betou	M	35
Personne âgée	Betou	M	2
Adolescent	Dongou	M	16
Adulte	Dongou	M	55
Enfant	Dongou	M	112
Personne âgée	Dongou	M	17
Adolescent	Enyelle	M	4
Adulte	Enyelle	M	41
Enfant	Enyelle	M	62
Personne âgée	Enyelle	M	6
Adolescent	Macao	M	17
Adulte	Macao	M	76
Enfant	Macao	M	50
Personne âgée	Macao	M	16
Adolescent	Minganga	M	11
Adulte	Minganga	M	38
Enfant	Minganga	M	61
Personne âgée	Minganga	M	6
")

# Rename columns for clarity
colnames(data) <- c("AgeClass", "Region", "Gender", "Count")

# Recode age classes
data$AgeClass <- recode(data$AgeClass,
                        "Enfant" = "Child",
                        "Adolescent" = "Adolescent",
                        "Adulte" = "Adult",
                        "Personne âgée" = "Old Adult")

# Order age classes
data$AgeClass <- factor(data$AgeClass, levels = c("Child", "Adolescent", "Adult", "Old Adult"))

data <- subset(data, data$AgeClass != "Child")

# Define colors using MetBrewer palette
line_colors <- c("F" = MetBrewer::met.brewer("Isfahan1")[4],
                 "M" = MetBrewer::met.brewer("Isfahan1")[6])

# Plot
p <- ggplot(data, aes(x = Region, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ AgeClass) +
  labs(x = "Region", y = "Number of individuals") +
  scale_fill_manual(values = line_colors,
                    labels = c("F" = "Women", "M" = "Men")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

# save plot
ggsave("population_by_region_sex_ageclass.png", plot = p,
       width = 11, height = 5, dpi = 300)
        