########################## Generate Data -----
set.seed(123)

library(dplyr)

# ----- structure of the nested data
n_groups <- 5
levels_per_group <- c(5, 6, 7, 8, 5)   # total = 31 levels
models <- c("A", "B", "C")

# ----- true fixed effects for the 3 models
model_effects <- c(A = 0.00, B = 0.15, C = 0.35)

# ----- build data frame
dat_list <- list()

for (g in 1:n_groups) {
  for (l in 1:levels_per_group[g]) {
    
    # random intercept for group
    # one draw per group would also be fine, but this keeps code simple
    # we'll create them outside for cleaner structure below
  }
}

# cleaner version with explicit random effects
group_ids <- paste0("G", 1:n_groups)
group_re <- rnorm(n_groups, mean = 0, sd = 0.30)
names(group_re) <- group_ids

dat <- data.frame()

for (g in 1:n_groups) {
  group_name <- paste0("G", g)
  
  for (l in 1:levels_per_group[g]) {
    level_name <- paste0("L", l)
    nested_level <- paste0(group_name, "_", level_name)
    
    # random intercept for level within group
    level_re <- rnorm(1, mean = 0, sd = 0.40)
    
    for (m in models) {
      # residual noise
      eps <- rnorm(1, mean = 0, sd = 0.25)
      
      score <- 1.0 +                       # grand mean
        model_effects[m] +                # fixed model effect
        group_re[group_name] +            # random group effect
        level_re +                        # random nested-level effect
        eps                               # residual
      
      dat <- rbind(dat, data.frame(
        group = group_name,
        level = level_name,
        group_level = nested_level,
        model = m,
        score = score
      ))
    }
  }
}

dat$model <- factor(dat$model)
dat$group <- factor(dat$group)
dat$level <- factor(dat$level)
dat$group_level <- factor(dat$group_level)

head(dat)
table(dat$group)
table(dat$model)

########################## 
library(pacman)
p_load(data.table,
       reshape2,
       lme4,
       lmerTest,
       sjPlot,
       car,
       emmeans,
       ggplot2,
       dplyr,
       effects,
       multcomp,
       multcompView,glmmTMB)  # gives p-values for fixed effects


datModel = dat
DV = "score"
models = c("1+(1|group)",
           "model+(1|group)",
           "model+(1|group)+(1|group:level)")
i=1
m=list()
for (idx in seq(1,length(models))){
  m[[idx]] = lmer(paste(DV,"~",models[idx],sep = ""), 
                  data=datModel, na.action=na.exclude)
}
mComp <- do.call(anova,m)
# row.names(mComp) = models
mComp$Best = seq(1,nrow(mComp))*as.numeric(mComp$`Pr(>Chisq)`<0.05)
print(mComp)

M = m[[max(mComp$Best,na.rm=T)]]
tab_model(M,show.se = T,show.stat = T,show.est = T)
summary(M)
plot(allEffects(M))

hist(resid(M))
em <- emmeans(M, ~ model)
plot(em, by = NULL , comparisons = TRUE, adjust = "bonferroni", 
     horizontal = FALSE)
summary(em)
contrast(em, method = "pairwise", adjust = "tukey")


##############

p1 <- ggplot(dat, aes(x = model, y = score)) +
  geom_jitter(width = 0.12, alpha = 0.5, size = 2) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    linewidth = 0.8
  ) +
  theme_classic(base_size = 14) +
  labs(
    title = "Raw performance scores by model",
    x = "Model",
    y = "Performance score"
  )

print(p1)


p2 <- dat %>%
  group_by(group, model) %>%
  summarise(mean_score = mean(score), .groups = "drop") %>%
  ggplot(aes(x = model, y = mean_score, group = group)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  theme_classic(base_size = 14) +
  labs(
    title = "Group-level mean performance by model",
    x = "Model",
    y = "Mean performance score"
  )

print(p2)


emm <- emmeans(M, ~ model)
emm_df <- as.data.frame(emm)

p3 <- ggplot(emm_df, aes(x = model, y = emmean)) +
  geom_point(size = 4) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.15,
    linewidth = 0.8
  ) +
  theme_classic(base_size = 14) +
  labs(
    title = "Estimated model performance from mixed-effects model",
    x = "Model",
    y = "Estimated mean performance"
  )

print(p3)
############ Combined Graph ----
emm <- emmeans(M, ~ model)
emm_df <- as.data.frame(emm)

p4 <- ggplot(dat, aes(x = model, y = score)) +
  geom_jitter(width = 0.12, alpha = 0.35, size = 2) +
  geom_point(
    data = emm_df,
    aes(x = model, y = emmean),
    size = 4,
    inherit.aes = FALSE
  ) +
  geom_errorbar(
    data = emm_df,
    aes(x = model, y = emmean, ymin = lower.CL, ymax = upper.CL),
    width = 0.15,
    linewidth = 0.9,
    inherit.aes = FALSE
  ) +
  theme_classic(base_size = 14) +
  labs(
    title = "Performance by model",
    subtitle = "Points = raw scores, large points/error bars = mixed-model estimated means ± CI",
    x = "Model",
    y = "Performance score"
  )

print(p4)
#############


cld_df <- cld(emm, adjust = "tukey", Letters = letters)
cld_df
cld_df <- as.data.frame(cld_df)

p5 <- ggplot(cld_df, aes(x = model, y = emmean)) +
  geom_point(size = 4) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.15,
    linewidth = 0.8
  ) +
  geom_text(
    aes(y = upper.CL + 0.05, label = .group),
    size = 6
  ) +
  theme_classic(base_size = 14) +
  labs(
    title = "Estimated model means with significance groups",
    x = "Model",
    y = "Estimated mean performance"
  )

print(p5)


########################## Bounded outcome problem----
# Residuals are:
#   
# non-normal
# heteroscedastic
# biased near 0 and 1

set.seed(1)

library(dplyr)

groups <- 5
levels_per_group <- c(5,6,7,5,7)

dat <- data.frame()

for(g in 1:groups){
  for(l in 1:levels_per_group[g]){
    
    # random denominator per level
    n_trials <- sample(20:100,1)
    
    for(model in c("A","B","C")){
      
      # true model performance
      p <- c(A=.55, B=.65, C=.75)[model]
      
      # random group + level effects
      p <- p + rnorm(1,0,.05)
      p <- max(min(p,.95),.05)
      
      correct <- rbinom(1, n_trials, p)
      
      dat <- rbind(dat, data.frame(
        group=paste("G",g,sep="_"),
        level=paste("L",l,sep="_"),
        model=model,
        correct=correct,
        total=n_trials,
        proportion=correct/n_trials
      ))
    }
  }
}

head(dat)

####### Wrong analysis
aov_fit <- aov(
  proportion ~ model + Error(group/level),
  data = dat
)

summary(aov_fit)

# Correct Analysis
library(lme4)

fit <- glmer(
  cbind(correct, total-correct) ~ model +
    (1|group) + (1|group:level),
  family = binomial,
  data = dat
)

summary(fit)
tab_model(fit,show.se = T,show.stat = T,show.est = T)
plot(allEffects(fit))

ggplot(dat, aes(model, proportion)) +
  geom_jitter(width=.1, alpha=.4) +
  stat_summary(fun=mean, geom="point", size=4) +
  theme_classic()

fit <- lmer(
  proportion ~ model +
    (1|group) + (1|group:level),
  data = dat
)
summary(fit)
tab_model(fit,show.se = T,show.stat = T,show.est = T)
plot(allEffects(fit))


fit <- glmmTMB(
  proportion ~ model +
    (1|group) + (1|group:level),
  family = beta_family(),
  data = dat
)
summary(fit)
tab_model(fit,show.se = T,show.stat = T,show.est = T)
plot(allEffects(fit))
