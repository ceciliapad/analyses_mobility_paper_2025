#####################
## Distance to kin ##
#####################

#load("results/intermediate/explor_mating_range.RData")

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

#reprod_all_bp2 <- all_reprod
reprod_all_bp2 <- reprod_all_bp2[!duplicated(reprod_all_bp2$ref), ]
reprod_all_bp2 <- subset(reprod_all_bp2, reprod_all_bp2$region == "Macao" | reprod_all_bp2$region == "Minganga")

adolescent<- subset(reprod_all_bp2, reprod_all_bp2$age=="Adolescent")
adolescent%>% dplyr::select(dist_bp_dad_bp, dist_bp_mom_bp, dist_bp_dad_inlaw_bp, dist_bp_mom_inlaw_bp, sex) %>%
  pivot_longer(., cols = c(dist_bp_dad_bp, dist_bp_mom_bp, dist_bp_dad_inlaw_bp, dist_bp_mom_inlaw_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  ylab("Distance (km)")+
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ylab("Distance (km)")+
  ylim(0,200)+
  geom_boxplot(outlier.color=NULL, alpha=0.8) +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adolescent_bp_dist.png", width = 7, height=5.5)

st(adolescent, vars = c("dist_bp_dad_bp", "dist_bp_mom_bp", "dist_bp_dad_inlaw_bp", "dist_bp_mom_inlaw_bp"), 
   summ = c('notNA(x)','mean(x)','median(x)','min(x)','max(x)'), group = 'sex', group.test = TRUE,
   title="Birthplace to parents birthplace by sex", out = "csv", file="results/mating_range_expl_macmin/adolescent_bp_dist.csv")

adolescent%>% dplyr::select(dist_bp_dad_bp, dist_bp_mom_bp, dist_bp_dad_inlaw_bp, dist_bp_mom_inlaw_bp, sex) %>%
  pivot_longer(., cols = c(dist_bp_dad_bp, dist_bp_mom_bp, dist_bp_dad_inlaw_bp, dist_bp_mom_inlaw_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex,colour=sex)) +
  scale_y_continuous(limits=c(1,200), trans='log', breaks = c(2,20,150)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ylab("Distance (km)")+
  geom_boxplot(outlier.color=NULL, alpha=0.8) +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adolescent_bp_dist_log.png", width = 7, height=5.5)

adolescent%>% dplyr::select(dist_living_dad_bp, dist_living_mom_bp, dist_living_dad_inlaw_bp, dist_living_mom_inlaw_bp, sex) %>%
  pivot_longer(., cols = c(dist_living_dad_bp, dist_living_mom_bp, dist_living_dad_inlaw_bp, dist_living_mom_inlaw_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ylab("Distance (km)")+
  ylim(0,200)+
  geom_boxplot(outlier.color=NULL, alpha=0.8)  +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adolescent_living_dist.png", width = 7, height=5.5)

st(adolescent, vars = c("dist_living_dad_bp", "dist_living_mom_bp", "dist_living_dad_inlaw_bp", "dist_living_mom_inlaw_bp"), 
   summ = c('notNA(x)','mean(x)','median(x)','min(x)','max(x)'), group = 'sex', group.test = TRUE,
   title="Residence camp to parents birthplace by sex", out = "csv", file="results/mating_range_expl_macmin/adolescent_living_dist.csv")

adolescent%>% dplyr::select(dist_living_dad_bp, dist_living_mom_bp, dist_living_dad_inlaw_bp, dist_living_mom_inlaw_bp, sex) %>%
  pivot_longer(., cols = c(dist_living_dad_bp, dist_living_mom_bp, dist_living_dad_inlaw_bp, dist_living_mom_inlaw_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  scale_y_continuous(limits=c(1,500), trans='log', breaks = c(2,20,150)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][7], MetPalettes$Isfahan1[[1]][3])) +
  theme_bw() +
  ylab("Distance (km)")+
  geom_boxplot(outlier.color=NULL, alpha=0.8) 

mum_anova <- aov(adolescent$dist_living_mum_living ~ adolescent$sex)
dad_anova <- aov(adolescent$dist_living_dad_living ~ adolescent$sex)
summary(mum_anova)
summary(dad_anova)


adolescent%>% dplyr::select(dist_living_dad_living, dist_living_mum_living, dist_living_dad_inlaw_living, dist_living_mum_inlaw_living, sex) %>%
  pivot_longer(., cols = c(dist_living_dad_living, dist_living_mum_living, dist_living_dad_inlaw_living, dist_living_mum_inlaw_living), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ylab("Distance (km)")+
  ylim(0,200)+
  geom_boxplot(outlier.color=NULL, alpha=0.8)  +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adolescent_living_dist_parents_living.png", width = 7.3, height=5.5)

adolescent%>% dplyr::select(dist_living_dad_living, dist_living_mum_living, sex) %>%
  pivot_longer(., cols = c(dist_living_dad_living, dist_living_mum_living), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ylab("Distance (km)")+
  ylim(0,200)+
  geom_boxplot(outlier.color=NULL, alpha=0.8)  +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adolescent_living_dist_parents_living.png", width = 7, height=5.5)

adolescent%>% dplyr::select(dist_living_dad_living, dist_living_mum_living, sex) %>%
  pivot_longer(., cols = c(dist_living_dad_living, dist_living_mum_living), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  scale_y_continuous(limits=c(1,200), trans='log', breaks = c(2,20,150)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ylab("Distance (km)")+
  geom_boxplot(outlier.color=NULL, alpha=0.8) +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adolescent_living_dist_parents_living_log.png", width = 7, height=5.5)

st(adolescent, vars = c("dist_living_dad_living", "dist_living_mum_living", "dist_living_dad_inlaw_living", "dist_living_mum_inlaw_living"), 
   summ = c('notNA(x)','mean(x)','median(x)','min(x)','max(x)'), group = 'sex', group.test = TRUE,
   title="Residence camp to parents residence by sex", out = "csv", file="results/mating_range_expl_macmin/adolescent_living_dist_living.csv")


# ggplot(adolescent, aes(x=c(dist_bp_spouse_bp), fill=sex))+
#   geom_histogram( alpha=0.6, position="identity") +
# scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])) + 
#   theme_bw() 
# ggsave("results/mating_range_expl_macmin/dist_own_bp_adolescent.png", width = 7, height=5.5)

ggplot(adolescent, aes(x=c(dist_bp_spouse_bp/1000), fill=sex))+
  geom_histogram( alpha=0.6, position="identity", binwidth = 20) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])) + 
  theme_bw() 
ggsave("results/mating_range_expl_macmin/dist_own_bp_adolescent.png", width = 7, height=5.5)

st(adolescent, vars = c("dist_bp_spouse_bp"), 
   summ = c('notNA(x)','mean(x)','median(x)','min(x)','max(x)'), group = 'sex', group.test = TRUE,
   title="Birthplace to parents birthplace by sex", out = "csv", file="results/mating_range_expl_macmin/adolescent_dist_ownandspbp.csv")


adolescent%>% dplyr::select(dist_living_own_bp, dist_living_spouse_bp, sex) %>%
  pivot_longer(., cols = c(dist_living_own_bp, dist_living_spouse_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ylab("Distance (km)")+
  ylim(0,200)+
  geom_boxplot(outlier.color=NULL, alpha=0.8) +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adolescent_living_dist_ownbp.png", width = 5.5, height=5.5)

st(adolescent, vars = c("dist_living_own_bp", "dist_living_spouse_bp"), 
   summ = c('notNA(x)','mean(x)','median(x)','min(x)','max(x)'), group = 'sex', group.test = TRUE,
   title="Residence camp to birthplace by sex", out = "csv", file="results/mating_range_expl_macmin/adolescent_living_dist_ownbp.csv")

adolescent%>% dplyr::select(dist_living_own_bp, dist_living_spouse_bp, sex) %>%
  pivot_longer(., cols = c(dist_living_own_bp, dist_living_spouse_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex)) +
  scale_y_continuous(trans='log') +
  ylab("Distance (km)")+
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])) + 
  theme_bw() +
  ggtitle("Adolescents")+
  geom_boxplot()
ggsave("results/mating_range_expl_macmin/adolescent_living_dist_ownbp_log.png", width = 5.5, height=5.5)


###  Now all adults

adult_old <- subset(reprod_all_bp2, reprod_all_bp2$age!="Adolescent")
table(adult_old$age)
adult_old%>% dplyr::select(dist_bp_dad_bp, dist_bp_mom_bp, dist_bp_dad_inlaw_bp, dist_bp_mom_inlaw_bp, sex) %>%
  pivot_longer(., cols = c(dist_bp_dad_bp, dist_bp_mom_bp, dist_bp_dad_inlaw_bp, dist_bp_mom_inlaw_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  ylab("Distance (km)")+
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ylab("Distance (km)")+
  ylim(0,200)+
  geom_boxplot(outlier.color=NULL, alpha=0.8) +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adult_old_bp_dist.png", width = 7, height=5.5)

st(adult_old, vars = c("dist_bp_dad_bp", "dist_bp_mom_bp", "dist_bp_dad_inlaw_bp", "dist_bp_mom_inlaw_bp"), 
   summ = c('notNA(x)','mean(x)','median(x)','min(x)','max(x)'), group = 'sex', group.test = TRUE,
   title="Birthplace to parents birthplace by sex", out = "csv", file="results/mating_range_expl_macmin/adult_old_bp_dist.csv")

adult_old%>% dplyr::select(dist_bp_dad_bp, dist_bp_mom_bp, dist_bp_dad_inlaw_bp, dist_bp_mom_inlaw_bp, sex) %>%
  pivot_longer(., cols = c(dist_bp_dad_bp, dist_bp_mom_bp, dist_bp_dad_inlaw_bp, dist_bp_mom_inlaw_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex)) +
  scale_y_continuous(trans='log') +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])) + 
  theme_bw() +
  ylab("Distance (km)")+
  ggtitle("adult_olds")+
  geom_boxplot() +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adult_old_bp_dist_log.png", width = 7, height=5.5)

adult_old%>% dplyr::select(dist_living_dad_bp, dist_living_mom_bp, dist_living_dad_inlaw_bp, dist_living_mom_inlaw_bp, sex) %>%
  pivot_longer(., cols = c(dist_living_dad_bp, dist_living_mom_bp, dist_living_dad_inlaw_bp, dist_living_mom_inlaw_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ylab("Distance (km)")+
  ylim(0,200)+
  geom_boxplot(outlier.color=NULL, alpha=0.8) +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adult_old_living_dist.png", width = 7.2, height=5.5)

st(adult_old, vars = c("dist_living_dad_bp", "dist_living_mom_bp", "dist_living_dad_inlaw_bp", "dist_living_mom_inlaw_bp"), 
   summ = c('notNA(x)','mean(x)','median(x)','min(x)','max(x)'), group = 'sex', group.test = TRUE,
   title="Residence camp to parents birthplace by sex", out = "csv", file="results/mating_range_expl_macmin/adult_old_living_dist.csv")

adult_old%>% dplyr::select(dist_living_dad_bp, dist_living_mom_bp, dist_living_dad_inlaw_bp, dist_living_mom_inlaw_bp, sex) %>%
  pivot_longer(., cols = c(dist_living_dad_bp, dist_living_mom_bp, dist_living_dad_inlaw_bp, dist_living_mom_inlaw_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex)) +
  scale_y_continuous(trans='log') +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])) + 
  theme_bw() +
  ylab("Distance (km)")+
  ggtitle("adult_olds")+
  geom_boxplot() +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adult_old_living_dist_log.png", width = 7, height=5.5)


adult_old%>% dplyr::select(dist_living_dad_living, dist_living_mum_living, dist_living_dad_inlaw_living, dist_living_mum_inlaw_living, sex) %>%
  pivot_longer(., cols = c(dist_living_dad_living, dist_living_mum_living, dist_living_dad_inlaw_living, dist_living_mum_inlaw_living), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ylab("Distance (km)")+
  ylim(0,200)+
  geom_boxplot(outlier.color=NULL, alpha=0.8) +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adult_old_living_dist_parents_living.png", width = 7.3, height=5.5)

adult_old%>% dplyr::select(dist_living_dad_living, dist_living_mum_living, sex) %>%
  pivot_longer(., cols = c(dist_living_dad_living, dist_living_mum_living), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ylab("Distance (km)")+
  ylim(0,150)+
  geom_boxplot(outlier.color=NULL, alpha=0.8) +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adult_old_living_dist_parents_living_violin.png", width = 7, height=5.5)

st(adult_old, vars = c("dist_living_dad_living", "dist_living_mum_living", "dist_living_dad_inlaw_living", "dist_living_mum_inlaw_living"), 
   summ = c('notNA(x)','mean(x)','median(x)','min(x)','max(x)'), group = 'sex', group.test = TRUE,
   title="Residence camp to parents residence by sex", out = "csv", file="results/mating_range_expl_macmin/adult_old_living_dist_living.csv")

mum_anova <- aov(adult_old$dist_living_mum_living ~ adult_old$sex)
dad_anova <- aov(adult_old$dist_living_dad_living ~ adult_old$sex)
summary(mum_anova)
summary(dad_anova)

# ggplot(adult_old, aes(x=c(dist_bp_spouse_bp), fill=sex))+
#   geom_histogram( alpha=0.6, position="identity") +
# scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])) + 
#   theme_bw() 
# ggsave("results/mating_range_expl_macmin/dist_own_bp_adult_old.png", width = 7, height=5.5)

ggplot(adult_old, aes(x=c(dist_bp_spouse_bp/1000), fill=sex))+
  geom_histogram( alpha=0.6, position="identity", binwidth = 20) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])) + 
  theme_bw() 
ggsave("results/mating_range_expl_macmin/dist_own_bp_adult_old.png", width = 7, height=5.5)

st(adult_old, vars = c("dist_bp_spouse_bp"), 
   summ = c('notNA(x)','mean(x)','median(x)','min(x)','max(x)'), group = 'sex', group.test = TRUE,
   title="Birthplace to parents birthplace by sex", out = "csv", file="results/mating_range_expl_macmin/adult_old_dist_ownandspbp.csv")

adult_old%>% dplyr::select(dist_living_own_bp, dist_living_spouse_bp, sex) %>%
  pivot_longer(., cols = c(dist_living_own_bp, dist_living_spouse_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ylab("Distance (km)")+
  ggtitle("adult_olds")+
  ylim(0,200)+
  geom_boxplot(outlier.color=NULL, alpha=0.8) +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
#stat_summary(fun.data = mean_cl_boot, geom = "pointrange",
#colour = "brown", position = position_dodge(width = 0.9))

ggsave("results/mating_range_expl_macmin/adult_old_living_dist_ownbp.png", width = 5.5, height=5.5)

st(adult_old, vars = c("dist_living_own_bp", "dist_living_spouse_bp"), 
   summ = c('notNA(x)','mean(x)','median(x)','min(x)','max(x)'), group = 'sex', group.test = TRUE,
   title="Residence camp to birthplace by sex", out = "csv", file="results/mating_range_expl_macmin/adult_old_living_dist_ownbp.csv")

adult_old%>% dplyr::select(dist_living_own_bp, dist_living_spouse_bp, sex) %>%
  pivot_longer(., cols = c(dist_living_own_bp, dist_living_spouse_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex)) +
  scale_y_continuous(trans='log') +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])) + 
  theme_bw() +
  ylab("Distance (km)")+
  ggtitle("adult_olds")+
  geom_boxplot() 
ggsave("results/mating_range_expl_macmin/adult_old_living_dist_ownbp_log.png", width = 5.5, height=5.5)

bp_data <- reprod_all_bp2 %>% dplyr::select(dist_bp_dad_bp, dist_bp_mom_bp, dist_bp_dad_inlaw_bp, dist_bp_mom_inlaw_bp, sex)
bp_data %>% gather() %>% head()


reprod_all_bp2 %>% dplyr::select(dist_bp_dad_bp, dist_bp_mom_bp, dist_bp_dad_inlaw_bp, dist_bp_mom_inlaw_bp, sex) %>%
  pivot_longer(., cols = c(dist_bp_dad_bp, dist_bp_mom_bp, dist_bp_dad_inlaw_bp, dist_bp_mom_inlaw_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ggtitle("Distance from birthplace to that of parents and in-laws")+
  ylab("Distance (km)")+
  ylim(0,250)+
  geom_boxplot(outlier.color=NULL, alpha=0.8) +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/all_bp_dist.png", width = 7, height=5.5)

adult_old%>% dplyr::select(dist_bp_dad_bp, dist_bp_mom_bp, dist_bp_dad_inlaw_bp, dist_bp_mom_inlaw_bp, sex) %>%
  pivot_longer(., cols = c(dist_bp_dad_bp, dist_bp_mom_bp, dist_bp_dad_inlaw_bp, dist_bp_mom_inlaw_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  ylab("Distance (km)")+
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ylab("Distance (km)")+
  #ylim(0,300)+
  geom_boxplot(outlier.color=NULL, alpha=0.8) +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adult_old_bp_dist.png", width = 7, height=5.5)

st(adult_old, vars = c("dist_bp_dad_bp", "dist_bp_mom_bp", "dist_bp_dad_inlaw_bp", "dist_bp_mom_inlaw_bp"), 
   summ = c('notNA(x)','mean(x)','median(x)','min(x)','max(x)'), group = 'sex', group.test = TRUE,
   title="Birthplace to parents birthplace by sex", out = "csv", file="results/mating_range_expl_macmin/adult_old_bp_dist.csv")

adult_old%>% dplyr::select(dist_bp_dad_bp, dist_bp_mom_bp, dist_bp_dad_inlaw_bp, dist_bp_mom_inlaw_bp, sex) %>%
  pivot_longer(., cols = c(dist_bp_dad_bp, dist_bp_mom_bp, dist_bp_dad_inlaw_bp, dist_bp_mom_inlaw_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex)) +
  scale_y_continuous(trans='log') +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])) + 
  theme_bw() +
  ylab("Distance (km)")+
  ggtitle("adult_olds")+
  geom_boxplot() +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adult_old_bp_dist_log.png", width = 7, height=5.5)

adult_old%>% dplyr::select(dist_living_dad_bp, dist_living_mom_bp, dist_living_dad_inlaw_bp, dist_living_mom_inlaw_bp, sex) %>%
  pivot_longer(., cols = c(dist_living_dad_bp, dist_living_mom_bp, dist_living_dad_inlaw_bp, dist_living_mom_inlaw_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ylab("Distance (km)")+
  ylim(0,200)+
  geom_boxplot(outlier.color=NULL, alpha=0.8) +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adult_old_living_dist.png", width = 7.2, height=5.5)

st(adult_old, vars = c("dist_living_dad_bp", "dist_living_mom_bp", "dist_living_dad_inlaw_bp", "dist_living_mom_inlaw_bp"), 
   summ = c('notNA(x)','mean(x)','median(x)','min(x)','max(x)'), group = 'sex', group.test = TRUE,
   title="Residence camp to parents birthplace by sex", out = "csv", file="results/mating_range_expl_macmin/adult_old_living_dist.csv")

adult_old%>% dplyr::select(dist_living_dad_bp, dist_living_mom_bp, dist_living_dad_inlaw_bp, dist_living_mom_inlaw_bp, sex) %>%
  pivot_longer(., cols = c(dist_living_dad_bp, dist_living_mom_bp, dist_living_dad_inlaw_bp, dist_living_mom_inlaw_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex)) +
  scale_y_continuous(trans='log') +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])) + 
  theme_bw() +
  ylab("Distance (km)")+
  ggtitle("adult_olds")+
  geom_boxplot() +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adult_old_living_dist_log.png", width = 7, height=5.5)


adult_old%>% dplyr::select(dist_living_dad_living, dist_living_mum_living, dist_living_dad_inlaw_living, dist_living_mum_inlaw_living, sex) %>%
  pivot_longer(., cols = c(dist_living_dad_living, dist_living_mum_living, dist_living_dad_inlaw_living, dist_living_mum_inlaw_living), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ylab("Distance (km)")+
  ylim(0,200)+
  geom_boxplot(outlier.color=NULL, alpha=0.8) +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adult_old_living_dist_parents_living.png", width = 7.3, height=5.5)

adult_old%>% dplyr::select(dist_living_dad_living, dist_living_mum_living, sex) %>%
  pivot_longer(., cols = c(dist_living_dad_living, dist_living_mum_living), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ylab("Distance (km)")+
  ylim(0,150)+
  geom_boxplot(outlier.color=NULL, alpha=0.8) +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
ggsave("results/mating_range_expl_macmin/adult_old_living_dist_parents_living_violin.png", width = 7, height=5.5)

st(adult_old, vars = c("dist_living_dad_living", "dist_living_mum_living", "dist_living_dad_inlaw_living", "dist_living_mum_inlaw_living"), 
   summ = c('notNA(x)','mean(x)','median(x)','min(x)','max(x)'), group = 'sex', group.test = TRUE,
   title="Residence camp to parents residence by sex", out = "csv", file="results/mating_range_expl_macmin/adult_old_living_dist_living.csv")

mum_anova <- aov(adult_old$dist_living_mum_living ~ adult_old$sex)
dad_anova <- aov(adult_old$dist_living_dad_living ~ adult_old$sex)
summary(mum_anova)
summary(dad_anova)

# ggplot(adult_old, aes(x=c(dist_bp_spouse_bp), fill=sex))+
#   geom_histogram( alpha=0.6, position="identity") +
# scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])) + 
#   theme_bw() 
# ggsave("results/mating_range_expl_macmin/dist_own_bp_adult_old.png", width = 7, height=5.5)

ggplot(adult_old, aes(x=c(dist_bp_spouse_bp/1000), fill=sex))+
  geom_histogram( alpha=0.6, position="identity", binwidth = 20) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])) + 
  theme_bw() 
ggsave("results/mating_range_expl_macmin/dist_own_bp_adult_old.png", width = 7, height=5.5)

st(adult_old, vars = c("dist_bp_spouse_bp"), 
   summ = c('notNA(x)','mean(x)','median(x)','min(x)','max(x)'), group = 'sex', group.test = TRUE,
   title="Birthplace to parents birthplace by sex", out = "csv", file="results/mating_range_expl_macmin/adult_old_dist_ownandspbp.csv")

adult_old%>% dplyr::select(dist_living_own_bp, dist_living_spouse_bp, sex) %>%
  pivot_longer(., cols = c(dist_living_own_bp, dist_living_spouse_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex, colour=sex)) +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][4], MetPalettes$Isfahan1[[1]][6])) + 
  scale_colour_manual(values=c(MetPalettes$Isfahan1[[1]][3], MetPalettes$Isfahan1[[1]][7])) +
  theme_bw() +
  ylab("Distance (km)")+
  ggtitle("adult_olds")+
  ylim(0,200)+
  geom_boxplot(outlier.color=NULL, alpha=0.8) +
  stat_summary(fun.y="mean",color="brown", shape=9, size=0.8, position=position_dodge(width=0.8))
#stat_summary(fun.data = mean_cl_boot, geom = "pointrange",
#colour = "brown", position = position_dodge(width = 0.9))

ggsave("results/mating_range_expl_macmin/adult_old_living_dist_ownbp.png", width = 5.5, height=5.5)

st(adult_old, vars = c("dist_living_own_bp", "dist_living_spouse_bp"), 
   summ = c('notNA(x)','mean(x)','median(x)','min(x)','max(x)'), group = 'sex', group.test = TRUE,
   title="Residence camp to birthplace by sex", out = "csv", file="results/mating_range_expl_macmin/adult_old_living_dist_ownbp.csv")

adult_old%>% dplyr::select(dist_living_own_bp, dist_living_spouse_bp, sex) %>%
  pivot_longer(., cols = c(dist_living_own_bp, dist_living_spouse_bp), names_to = "Type", values_to = "Distance") %>%
  ggplot(aes(x = Type, y = Distance/1000, fill = sex)) +
  scale_y_continuous(trans='log') +
  scale_fill_manual(values=c(MetPalettes$Isfahan1[[1]][6], MetPalettes$Isfahan1[[1]][4])) + 
  theme_bw() +
  ylab("Distance (km)")+
  ggtitle("adult_olds")+
  geom_boxplot() 
ggsave("results/mating_range_expl_macmin/adult_old_living_dist_ownbp_log.png", width = 5.5, height=5.5)

#### MODELS ####

# Distance from place they live to place where mum lives
# No old adults as parents are dead
reprod_all_bp5 <- reprod_all_bp2
reprod_all_bp5$age_g <- ifelse(reprod_all_bp5$age=="Adolescent", "Adolescent", "Adult")

model_simp_mum <- bf(dist_living_mum_living ~ sex +  age_g +  set_type + (1 | region), family="gaussian")
model_int_mum <- bf(dist_living_mum_living ~ sex*age_g +  set_type + (1 | region), family="gaussian")
model_simp2_mum <- bf(dist_living_mum_living ~ sex +  age_g + (1 | residence_camp), family="gaussian")

model <- bf(median_dist ~ age + sex + sd_mating_dist + (1 | residence_camp), family="gaussian")
model_int <- bf(median_dist ~ age + sex*sd_mating_dist + (1 | residence_camp), family="gaussian")

get_prior(model_simp, data=reprod_all_bp2)

m1.prior <- c(prior(student_t(3, 0, 2.5), class=Intercept),
              prior(student_t(3, 0, 2.5), class=sd), 
              prior(normal( 0, 5000), class=b))


simple_brm_mum <- brm(formula=model_simp_mum, data=reprod_all_bp5,warmup=2000,
                      iter=10000, chains=3, control=list(adapt_delta=0.95), prior = m1.prior) 
int_brm_mum <- brm(formula=model_int_mum, data=reprod_all_bp5,warmup=2000,
                   iter=10000, chains=3, control=list(adapt_delta=0.95), prior = m1.prior) 
simple2_brm_mum <- brm(formula=model_simp2_mum, data=reprod_all_bp5,warmup=2000,
                       iter=10000, chains=3, control=list(adapt_delta=0.95), prior = m1.prior)

