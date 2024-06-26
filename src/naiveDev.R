library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
#library(tidyext)
library(MatchIt)
library(tidycensus)
library(sf)

## ACS variables for referece
#ACSlist <- load_variables(2021, "acs5")
#ACSlist = ACSlist %>% filter(geography == "county")

#View(ACSlist)

##### Census variables - median income, population
## census info - variables found here https://api.census.gov/data/2021/acs/acs5/variables.html
idaho2009 <- get_acs(
  state = "ID",
  geography = "county",
  variables = c("B19013_001","B01003_001E"), ## mean annual income, total population
  geometry = FALSE,
  year = 2009  #first year available?
)

mt2009 <- get_acs(
  state = "MT",
  geography = "county",
  variables = c("B19013_001","B01003_001E"), ## mean annual income, total population
  geometry = FALSE,
  year = 2009  #first year available?
)

names.2009 = c(income2009 = "B19013_001", pop2009 = "B01003_001")
names.2020 = c(income2020 = "B19013_001", pop2020 = "B01003_001")

acs2009 = rbind(idaho2009, mt2009) %>%
  subset(select = -c(moe, NAME)) %>%
  pivot_wider(names_from = "variable", values_from = "estimate") %>%
  rename(all_of(names.2009))

idaho2020 <- get_acs(
  state = "ID",
  geography = "county",
  variables = c("B19013_001","B01003_001E"), ## mean annual income, total population
  geometry = FALSE,
  year = 2020 
)

mt2020 <- get_acs(
  state = "MT",
  geography = "county",
  variables = c("B19013_001","B01003_001E"), ## mean annual income, total population
  geometry = FALSE,
  year = 2020 
)

acs2020 = rbind(idaho2020, mt2020) %>%
  subset(select = -moe) %>%
  pivot_wider(names_from = "variable", values_from = "estimate") %>%
  rename(all_of(names.2020))


##WITHOUT MATCHING FOR COMPARISON

#setwd("~/BSU/diss/ch3/")

#dat = read.csv("../data/sampsFull.csv")
dat = read.csv("../data/sampsFullValue.csv")

## create a difference varaible for distinguishing pixels w the most change
dat = dat %>%
  mutate(mesic.change = X202009_mesic - X200409_mesic) %>%
  mutate(huc4 = substr(huc12, 1, 4)) %>%
  mutate(huc8 = substr(huc12, 1, 8))

## "constant" is the tenure indicator, where 100 is private no easement, 
## 101 private w easement, 102 fed (misc), 103 state, 120 BLM, 121 USFS 

## Only private land
private.df = dat%>% 
  filter(constant < 102) %>%
  mutate(CE = ifelse(constant == 100, 0, 1))

## merge w acs variables
private.df = merge(private.df, acs2009)
private.df = merge(private.df, acs2020)

## Create variable for pop change
private.df = private.df %>%
  mutate(pop.delta = pop2020 - pop2009)


## Drop rows w missingvalue estimates
private.df = private.df %>%
	drop_na(value) %>%
	rename(landValue = value) 

#----------------------- Regression ---------------------#
# uses the brms package
library(brms)
  
## Time invariant covs
covs = c("system.index", "CE", "pop.delta", "income2009", "huc4", "huc8",
              "huc12", "huc12public", "GAP", "year_est", "ceID", "elevation", "landValue", 
                "slope", "flowAcc", "distCity", "distTrust", "distRoad", "distPublic", "distGAP12", "mesic.change")
dev.df = private.df%>%
  select(contains("dev"), all_of(covs))

panel = dev.df %>%
  pivot_longer(cols = starts_with("dev")) %>%
  mutate(year = substring(name, 4)) %>%
  rename(dev = value)

## ad an id
panel = panel %>% group_by(system.index)# %>% mutate(id=cur_group_id())

# post = add an indicator to identify the post treatment period
panel$post = ifelse(panel$year_est < panel$year, 1, 0)
panel$post = panel$post %>% 
  replace(is.na(.), 0)


## Factors
panel$huc12 = factor(panel$huc12)
panel$year = factor(panel$year)
panel$CE = factor(panel$CE)
#panel$dev = factor(panel$dev)
panel$post = factor(panel$post)

## standardize the covs
panel$elevation.std = scale(panel$elevation)
panel$slope.std = scale(panel$slope)
panel$distCity.std = scale(panel$distCity)
panel$distPublic.std = scale(panel$distPublic)
panel$distRoad.std = scale(panel$distRoad)
panel$distGAP12.std = scale(panel$distGAP12)
panel$income2009.std = scale(panel$income2009)
panel$pop.delta.std = scale(panel$pop.delta)
panel$value.std = scale(panel$landValue)

## Priors
#priors.full = get_prior(dev ~ CE*post + elevation.std + slope.std + distCity.std +
priors.full = get_prior(dev ~ post + elevation.std + slope.std + distCity.std +
                        distPublic.std + distRoad.std + distGAP12.std + income2009.std + value.std +
                        pop.delta.std +  (1|year) + (1 |huc12),
                        data = panel, family = 'binomial')

#priors.full$prior[1:18] = "normal (0,1)"
priors.full$prior[1:17] = "normal (0,1)"

## randomly sample the panel
panel.samp = panel %>% sample_frac(0.2)

# implement model
#did.dev.full <- brm(dev ~ CE*post + elevation.std + slope.std + distCity.std +
naive.dev <- brm(dev ~ post + elevation.std + slope.std + distCity.std +
                     distPublic.std + distRoad.std + distGAP12.std + income2009.std + value.std+
                     pop.delta.std +  (1|year) + (1 |huc12),
              	     data = panel.samp,
              	     #family='beta',
              	     family=bernoulli(),
              		prior=priors.full, 
              		control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
              		cores=4,
              		chains = 4, ## lower to trial
              		iter= 8000## lower to trial
)


summary(naive.dev)
plot(naive.dev)
pp_check(naive.dev)
r2 = bayes_R2(naive.dev)
r2

save.image(file = "naiveDev8k.RData")

