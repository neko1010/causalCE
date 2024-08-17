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

posterior <- naive.dev%>% 
  gather_draws(`b_.*`, regex = TRUE) %>% 
  mutate(component = ifelse(str_detect(.variable, "phi_"), "Precision", "Mean"),
         intercept = str_detect(.variable, "Intercept")) %>%
  mutate(name = case_when(
    endsWith(.variable, "Intercept") ~ "Intercept",
    endsWith(.variable, "GAP12.std") ~ "Dist to GAP 1 or 2",
    endsWith(.variable, "slope.std") ~ "Slope",
    endsWith(.variable, "Public.std") ~ "Dist to public",
    endsWith(.variable, "City.std") ~ "Dist to city",
    endsWith(.variable, "post1") ~ "Easement effect",
    endsWith(.variable, "Road.std") ~ "Dist to road",
    endsWith(.variable, "elevation.std") ~ "Elevation",
    endsWith(.variable, "2009.std") ~ "Income",
    endsWith(.variable, "value.std") ~ "Value",                         
    endsWith(.variable, "pop.delta.std") ~ "Population change"))%>%
  mutate(name = factor(name, levels = c("Easement effect", 
                                        "Intercept", "Dist to GAP 1 or 2", 
                                        "Slope", "Dist to public", "Dist to city","Dist to road", "Elevation", "Income", 
                                        "Value", "Population change")))

ggplot(posterior, aes(x = .value, y = fct_rev(name), fill = component)) +
  geom_vline(xintercept = 0) +
  stat_halfeye(aes(slab_alpha = intercept), 
               .width = c(0.8, 0.95), point_interval = "median_hdi") +
  scale_fill_viridis_d(option = "viridis", end = 0.6) +
  scale_slab_alpha_discrete(range = c(1, 0.4)) +
  guides(fill = "none", slab_alpha = "none") +
  labs(x = "Effect size", y = "Variable") +
  facet_wrap(vars(component), ncol = 1, scales = "free_y") +
  ggtitle("Naive development")
  #theme_clean()

## Model summary
summary(naive.dev)
pp_check(naive.dev)

## exponentiate the coefficients to get % likelihood
effect = function(posts, var.name){
  ## filter df for only draws of interest
  draws = posts%>%
    filter(.variable == var.name)
  ## central tendency and SD
  med = median(draws$.value)
  mean = mean(draws$.value)
  sd = sd(draws$.value)
  
  est = exp(mean)
  upper = exp(mean +1.96*sd)
  lower = exp(mean - 1.96*sd)
  return( c(est, lower, upper))
}

#ce.post = effect(posterior, "b_CE1:post1")
#ce.post

post = effect(posterior, "b_post1")
post

elevation = effect(posterior, "b_elevation.std")
elevation
sd(panel$elevation)

slope = effect(posterior, "b_slope.std")
slope 
sd(panel$slope)

dist.city = effect(posterior, "b_distCity.std")
dist.city
sd(panel$distCity)

dist.GAP12 = effect(posterior, "b_distGAP12.std")
dist.GAP12
sd(panel$distGAP12)

dist.public = effect(posterior, "b_distPublic.std")
dist.public
sd(panel$distPublic)

dist.road = effect(posterior, "b_distRoad.std")
dist.road
sd(panel$distRoad)

income = effect(posterior, "b_income2009.std")
income
sd(panel$income2009)

pop.delta = effect(posterior, "b_pop.delta.std")
pop.delta
sd(panel$pop.delta)

r2

## Parallel trends
panel.trend = panel %>%
  filter(post ==0) %>%
  filter(as.numeric(year) < 13) ## 13 corresponds to 2016


#nonlin <- ggplot(data=panel.trend, aes(x=year, y=dev, group=CE)) +
nonlin <- ggplot(data=panel, aes(x=year, y=dev, group=CE)) +
  #geom_point(aes(colour= post)) +
  #geom_line(aes(colour=post)) +
  geom_smooth(aes(group=CE, colour = CE))
nonlin
