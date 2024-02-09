library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
#library(tidyext)
library(MatchIt)

##WITHOUT MATCHING FOR COMPARISON

#setwd("~/BSU/diss/ch3/")

dat = read.csv("../data/sampsFull.csv")

## create a difference varaible for distinguishing pixels w the most change
dat = dat %>%
  mutate(mesic.change = X202009_mesic - X200409_mesic)%>%
  mutate(huc4 = substr(dat$huc12, 1, 4)) %>%
  mutate(huc8 = substr(dat$huc12, 1, 8))

## "constant" is the tenure indicator, where 100 is private no easement, 
## 101 private w easement, 102 fed (misc), 103 state, 120 BLM, 121 USFS 

##Copy of Ch3 A Only public land - 102 FED misc (DOE, USDA, NPS, NWR), 103 STATE misc, 120 BLM, 121 USFS
fed.vec = c(102, 120, 121)

privPub.df = dat%>% 
  filter(constant != 100) %>%
  mutate(CE = ifelse(constant == 101, 1, 0)) %>%
  mutate(fed = ifelse(constant %in% fed.vec, 1, 0)) %>%
  mutate(blm = ifelse(constant == 120, 1, 0)) %>%
  mutate(usfs = ifelse(constant == 121, 1, 0)) %>%
  mutate(state = ifelse(constant == 103, 1, 0))


#----------------------- Regression ---------------------#
# uses the brms package
library(brms)
  
## Time invariant covs
otherCovs = c("system.index", "CE", "fed", "blm", "state", "usfs",
              "huc12", "huc4", "huc8", "GEOID",  "huc12public", "GAP", "year_est", "ceID", "elevation", 
                "slope", "flowAcc", "distCity", "distTrust", "distRoad", "distPublic", "distGAP12", "mesic.change")
#dat.invar = matched.df[otherCovs]
dat.invar = privPub.df[otherCovs] ## DiD only - no matching
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
dat.2004 = cbind(dat.invar, create_df(privPub.df, 2004))
dat.2005 = cbind(dat.invar, create_df(privPub.df, 2005))
dat.2006 = cbind(dat.invar, create_df(privPub.df, 2006))
dat.2007 = cbind(dat.invar, create_df(privPub.df, 2007))
dat.2008 = cbind(dat.invar, create_df(privPub.df, 2008))
dat.2009 = cbind(dat.invar, create_df(privPub.df, 2009))
dat.2010 = cbind(dat.invar, create_df(privPub.df, 2010))
dat.2011 = cbind(dat.invar, create_df(privPub.df, 2011))
dat.2012 = cbind(dat.invar, create_df(privPub.df, 2012))
dat.2013 = cbind(dat.invar, create_df(privPub.df, 2013))
dat.2014 = cbind(dat.invar, create_df(privPub.df, 2014))
dat.2015 = cbind(dat.invar, create_df(privPub.df, 2015))
dat.2016 = cbind(dat.invar, create_df(privPub.df, 2016))
dat.2017 = cbind(dat.invar, create_df(privPub.df, 2017))
dat.2018 = cbind(dat.invar, create_df(privPub.df, 2018))
dat.2019 = cbind(dat.invar, create_df(privPub.df, 2019))
dat.2020 = cbind(dat.invar, create_df(privPub.df, 2020))


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
panel$GEOID = factor(panel$GEOID)
panel$year = factor(panel$year)
panel$CE = factor(panel$CE)
panel$fed = factor(panel$fed)
panel$state= factor(panel$state)
panel$blm= factor(panel$blm)
panel$usfs= factor(panel$usfs)
panel$post = factor(panel$post)

## standardize the covs
panel$elevation.std = scale(panel$elevation)
panel$flowAcc.std = scale(panel$flowAcc)
panel$slope.std = scale(panel$slope)
panel$spei.std = scale(panel$spei)

## Priors
#priors.full = get_prior(outcome ~ CE*post + elevation.std + flowAcc.std + slope.std 
priors.full = get_prior(outcome ~ CE*post + flowAcc.std + slope.std 
                        + spei.std +  (1|year) + (1 |huc12),
                        #+ spei.std +  (1|year) + (1 |huc4),
                        #+ spei.std +  (1|year) + (1 |GEOID),
                        data = panel, family = 'beta')

hist(rexp(1000, 2))

priors.full$prior[1:8] = "normal (0,1)"
priors.full$prior[10:14] = "normal (0,1)"
hist(rnorm(1000, 0,1))

## randomly sample the panel
panel.samp = panel %>% sample_frac(0.2)

# implement model
#did.ce.public <- brm(outcome ~ CE*post + elevation.std + flowAcc.std + slope.std + spei.std +  (1|year) + (1 |GEOID),
#did.ce.public <- brm(outcome ~ CE*post + elevation.std + flowAcc.std + slope.std + spei.std +  (1|year) + (1 |huc12),
did.ce.public <- brm(outcome ~ CE*post + flowAcc.std + slope.std + spei.std +  (1|year) + (1 |huc12),
#did.ce.public <- brm(outcome ~ CE*post + elevation.std + flowAcc.std + slope.std + spei.std +  (1|year) + (1 |huc4),
              data = panel.samp,
              #family='beta',
              family=Beta(),
              prior=priors.full,
              control = list(adapt_delta = 0.99,max_treedepth = 15),
              cores=4,
              chains = 4, ## lower to trial
              iter=2000 ## lower to trial
)


summary(did.ce.public)
plot(did.ce.public)
pp_check(did.ce.public)
r2 = bayes_R2(did.ce.public)
r2

#save.image(file = "didPublicHUC4.RData")
#save.image(file = "didPublicGEOID.RData")
save.image(file = "didPublicNOelev.RData")
