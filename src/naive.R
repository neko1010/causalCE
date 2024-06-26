library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
#library(tidyext)
library(MatchIt)
library(tidycensus)
library(sf)


#setwd("~/BSU/diss/ch3/")

#dat = read.csv("../data/sampsFull.csv")
dat = read.csv("../data/sampsFullValue.csv")

## create a difference varaible for distinguishing pixels w the most change
dat = dat %>%
  mutate(mesic.change = X202009_mesic - X200409_mesic)

## "constant" is the tenure indicator, where 100 is private no easement, 
## 101 private w easement, 102 fed (misc), 103 state, 120 BLM, 121 USFS 

## Only private land
private.df = dat%>% 
  filter(constant < 102) %>%
  mutate(CE = ifelse(constant == 100, 0, 1))


#----------------------- Regression ---------------------#
# uses the brms package
library(brms)
  
## Time invariant covs
otherCovs = c("system.index", "CE", "huc12", "huc12public", "GAP", "year_est", "ceID", "elevation", 
                "slope", "flowAcc", "distCity", "distTrust", "distRoad", "distPublic", "mesic.change")
dat.invar = private.df[otherCovs]
create_df = function(data, year){
  df.mesic  = data %>% select(contains(paste0("X", as.character(year))))
  df.spei = data %>% select(contains(paste0("spei", as.character(year))))
  ## remove the leading "X" and year
  names(df.mesic) =  gsub(paste0("X", year), "", names(df.mesic))
  names(df.spei) = gsub(year, "", names(df.spei))
  df.mesic$year = year
  
  full.df = cbind(df.mesic, df.spei)
  return(full.df)
}

years = seq(2004, 2020)

## FIGURE OUT HOW TO DO THIS DYNAMICALLY!
##years.df = do.call(create_df, args = c(dat, years))
dat.2004 = cbind(dat.invar, create_df(private.df, 2004))
dat.2005 = cbind(dat.invar, create_df(private.df, 2005))
dat.2006 = cbind(dat.invar, create_df(private.df, 2006))
dat.2007 = cbind(dat.invar, create_df(private.df, 2007))
dat.2008 = cbind(dat.invar, create_df(private.df, 2008))
dat.2009 = cbind(dat.invar, create_df(private.df, 2009))
dat.2010 = cbind(dat.invar, create_df(private.df, 2010))
dat.2011 = cbind(dat.invar, create_df(private.df, 2011))
dat.2012 = cbind(dat.invar, create_df(private.df, 2012))
dat.2013 = cbind(dat.invar, create_df(private.df, 2013))
dat.2014 = cbind(dat.invar, create_df(private.df, 2014))
dat.2015 = cbind(dat.invar, create_df(private.df, 2015))
dat.2016 = cbind(dat.invar, create_df(private.df, 2016))
dat.2017 = cbind(dat.invar, create_df(private.df, 2017))
dat.2018 = cbind(dat.invar, create_df(private.df, 2018))
dat.2019 = cbind(dat.invar, create_df(private.df, 2019))
dat.2020 = cbind(dat.invar, create_df(private.df, 2020))


## combine to one wild df
panel = rbind(dat.2004, dat.2005, dat.2006, dat.2007, dat.2008, dat.2009, dat.2010,
              dat.2011, dat.2012, dat.2013, dat.2014, dat.2015, dat.2016, dat.2017,
              dat.2018, dat.2019, dat.2020)

## rename so cols begin with letters
#panel = panel %>% rename_at(vars(starts_with("01")), ~(sub("01_", "",.)))
mesic.cols = colnames(select(panel, starts_with("0")))
swap = function(string){
  split = strsplit(string, "_")
  return(paste0(split[[1]][2], split[[1]][1]))
}
## new names
new.names = lapply(mesic.cols, swap)
## change names 
outcomes  = select(panel, starts_with("0"))
colnames(outcomes) = new.names
## replace the old
panel = panel%>% select(-starts_with("0"))
panel = cbind(panel,outcomes)
## ad an id
panel = panel %>% group_by(system.index)# %>% mutate(id=cur_group_id())

# post = add an indicator to identify the post treatment period
panel$post = ifelse(panel$year_est < panel$year, 1, 0)
panel$post = panel$post %>% 
  replace(is.na(.), 0)
# remove 0s and 1s
panel = panel %>%
  mutate(outcome.temp = ifelse(mesic09 <= 0, 0.00001, mesic09)) %>%
  mutate(outcome = ifelse(outcome.temp >=1, 0.99999, outcome.temp))

## Factors
panel$huc12 = factor(panel$huc12)
panel$year = factor(panel$year)
panel$CE = factor(panel$CE)
panel$post = factor(panel$post)

## standardize the covs
panel$elevation.std = scale(panel$elevation)
panel$flowAcc.std = scale(panel$flowAcc)
panel$slope.std = scale(panel$slope)
panel$spei.std = scale(panel$spei)


## Priors
#priors.naive= get_prior(outcome ~ post + elevation.std + flowAcc.std + slope.std 
priors.naive= get_prior(outcome ~ post + flowAcc.std + slope.std 
                        + spei.std +  (1|year) + (1 |huc12),
                        data = panel, family = 'beta')

priors.naive$prior[1:6] = "normal (0,1)"
priors.naive$prior[8:12] = "normal (0,1)"
hist(rnorm(1000, 0,1))

## randomly sample the panel
panel.samp = panel %>% sample_frac(0.2)

# implement model
#naive <- brm(outcome ~ post + elevation.std + flowAcc.std + slope.std + spei.std +  (1|year) + (1 |huc12),
naive <- brm(outcome ~ post + flowAcc.std + slope.std + spei.std +  (1|year) + (1 |huc12),
              data = panel.samp,
              #family='beta',
              family=Beta(),
              prior=priors.naive, 
              control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
              cores=4,
              chains = 4, ## lower to trial
              iter=8000## lower to trial
)

summary(naive)
plot(naive)
pp_check(naive)
r2 = bayes_R2(naive)
r2

save.image(file = "naiveNOelev8k.RData")

  
