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

dat = read.csv("../data/sampsFull.csv")

## create a difference varaible for distinguishing pixels w the most change
dat = dat %>%
  mutate(mesic.change = X202009_mesic - X200409_mesic) %>%
  mutate(huc4 = substr(huc12, 1, 4)) %>%
  mutate(huc8 = substr(huc12,1, 8))

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


#----------------------- Regression ---------------------#
# uses the brms package
library(brms)
  
## Time invariant covs
otherCovs = c("system.index", "CE","huc4", "huc8",  "huc12", "huc12public", "GAP", "year_est", "ceID", "elevation", 
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

## explore for easements w positive and negative effects.
panel.ce = panel %>% 
  filter(CE == 1 & post ==1)

#ggplot(data = panel.ce, mapping = aes(x = year, y = mesic09)) +
#  geom_boxplot()+
#  geom_point(mapping = aes(x = year, y = spei.std))

plot(panel.ce$year, panel.ce$mesic09, main = "Treated")

panel.NOce = panel %>% 
  filter(CE == 0)

plot(panel.NOce$year, panel.NOce$mesic09, main = "Control")

panel.ce.pre = panel %>%
  filter(CE ==1 & post == 0)

plot(panel.ce.pre$year, panel.ce.pre$mesic09, main = "Not yet treated")

panel.NOce.ANDpre = panel %>%
  filter(post == 0)

plot(panel.NOce.ANDpre$year, panel.NOce.ANDpre$mesic09, main = "Control or not yet treated")

## sort the CE df based on mesic change and plot 5 examples of high and low change
high.change = c(21505, 23329, 29430, 24995, 25595)
low.change = c(29138, 21884, 23711, 23198, 26555)

high.sbst = subset(panel%>%filter(CE == 1), system.index %in% high.change)
low.sbst = subset(panel%>%filter(CE == 1), system.index %in% low.change)

## plot these
gg.high = ggplot(high.sbst, mapping = aes(x = year, y = mesic09, color = as.factor(system.index))) +
  geom_point() +
  geom_line(aes(group = as.factor(system.index))) +
  geom_vline(aes(xintercept = as.factor(high.sbst$year_est), color = as.factor(system.index)))
  
gg.high + ggtitle("Postive mesic veg change")

gg.low = ggplot(low.sbst, mapping = aes(x = year, y = mesic09, color = as.factor(system.index))) +
  geom_point() +
  geom_line(aes(group = as.factor(system.index))) +
  geom_vline(aes(xintercept = as.factor(low.sbst$year_est), color = as.factor(system.index)))

gg.low + ggtitle("Negative mesic veg change")

## Priors
#priors.full = get_prior(outcome ~ CE*post + elevation.std + flowAcc.std + slope.std 
priors.full = get_prior(outcome ~ CE*post + flowAcc.std + slope.std 
                        + spei.std +  (1|year) + (1 |huc12),
                        #+ spei.std +  (1|year) + (1 |huc4),
                        data = panel, family = 'beta')

priors.full$prior[1:8] = "normal (0,1)"
priors.full$prior[10:14] = "normal (0,1)"
hist(rnorm(1000, 0,1))

## randomly sample the panel
panel.samp = panel %>% sample_frac(0.2)

# implement model
## Choose one year - am I capturing effect and spillovers
#did.ce.private <- brm(outcome ~ CE*post + elevation.std + flowAcc.std + slope.std + spei.std +  (1|year) + (1 |huc12),
did.ce.private <- brm(outcome ~ CE*post + flowAcc.std + slope.std + spei.std +  (1|year) + (1 |huc12),
#did.ce.private <- brm(outcome ~ CE*post + elevation.std + flowAcc.std + slope.std + spei.std +  (1|year) + (1 |huc4),
              data = panel.samp,
              family=Beta(),
              prior=priors.full, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
              control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
              cores=4,
              #chains = 4, ## lower to trial
              chains = 4, ## lower to trial
              iter=2000## lower to trial
              #iter=4000 ## lower to trial
)

summary(did.ce.private)
plot(did.ce.private)
pp_check(did.ce.private)
r2 = bayes_R2(did.ce.private)
r2

save(did.ce.private, file = "didOnlyPrivateNOelev.RData")

### k fold cross validation
#fit.val = kfold(did.ce.private, save_fits = T, chains = 1)# default K = 10
#predict.val = kfold_predict(fit.val, method = "predict")
#MAE = function(y, yrep){
#  yrep.mean = colMeans(yrep)
#  mean(abs(yrep.mean - y))
#  }
#  
#mae = MAE(predict.val$y, predict.val$yrep)
#print('MAE = ', mae)
  
