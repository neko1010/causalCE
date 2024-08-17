library(brms)
library(tidyr)
library(marginaleffects)
library(MatchIt)
library(ggplot2)
library(tidybayes)
library(dplyr)
library(forcats)
library(stringr)
library(ggpubr)

posterior_beta <- naive%>% 
  gather_draws(`b_.*`, regex = TRUE) %>% 
  mutate(component = ifelse(str_detect(.variable, "phi_"), "Precision", "Mean"),
         intercept = str_detect(.variable, "Intercept"))%>%
  mutate(name = case_when(
    endsWith(.variable, "Intercept") ~ "Intercept",
    endsWith(.variable, "Acc.std") ~ "Contributing Area",
    endsWith(.variable, "slope.std") ~ "Slope",
    endsWith(.variable, "spei.std") ~ "SPEI",
    endsWith(.variable, "post1") ~ "Easement effect"))%>%
  mutate(name = factor(name, levels = c("Easement effect", 
                                       "Intercept", "Contributing Area", 
                                        "Slope", "SPEI")))
  
ggplot(posterior_beta, aes(x = .value, y = fct_rev(name), fill = component)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(aes(slab_alpha = intercept), 
               .width = c(0.8, 0.95), point_interval = "median_hdi") +
  scale_fill_viridis_d(option = "viridis", end = 0.6) +
  scale_slab_alpha_discrete(range = c(1, 0.4)) +
  guides(fill = "none", slab_alpha = "none") +
  labs(x = "Effect size", y = "Variable") +
  facet_wrap(vars(component), ncol = 1, scales = "free_y") +
  ggtitle("Naive mesic vegetation")
  #theme_clean()



## Model summary
summary(naive)
pp_check(naive)

## USE BELOW FOR TRANSFORMING THESE! https://www.andrewheiss.com/blog/2021/11/08/beta-regression-guide/
#plogis(intercept + coefficient) - plogis(intercept)
odds_effect = function(posts, var.name, int){
  ## filter df for only draws of interest
  draws = posts%>%
    filter(.variable == var.name)
  ## central tendency and SD
  med = median(draws$.value)
  mean = mean(draws$.value)
  sd = sd(draws$.value)
  
  est = plogis(int + mean) - plogis(int)
  upper = plogis(int + (mean + 1.96*sd)) - plogis(int)
  lower = plogis(int + (mean - 1.96*sd)) - plogis(int)
  return( c(est, lower, upper))
}

intercept = -0.85

post = odds_effect(posterior_beta, "b_post1", intercept)
post

slope = odds_effect(posterior_beta, "b_slope.std", intercept )
slope
sd(panel$slope)

spei= odds_effect(posterior_beta, "b_spei.std",intercept )
spei
sd(panel$spei)

flow.acc = odds_effect(posterior_beta, "b_flowAcc.std", intercept)
flow.acc
sd(panel$flowAcc)

r2
summary(naive)