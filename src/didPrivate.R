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
  mutate(pop.delta = pop2020 - pop2009)%>%
  drop_na(value) # drop na

# The matchit command will find matched pairs 
matching <- matchit(CE ~ elevation+ distCity + distPublic + 
                      distTrust + distRoad + distGAP12 +
                      pop.delta + income2009 + value,
                    data = private.df,
                    distance = "mahalanobis", # using mahalanobis distances
                    #method = "genetic", 
                    method = "nearest", # find the nearest neighbors
                    replace = TRUE, # match with replacement, so control units are allowed to 
                    # be matched to several treated units. If FALSE, each obs. can only
                    # be matched to one other
                    caliper=c(elevation=0.4, # The caliper determines how similar the obs. have to be
                              distPublic = 0.4,
                              distCity = 0.4,
                              distTrust = 0.4,
                              distRoad= 0.4,
                              distGAP12 = 0.4,
                              pop.delta = 0.4,
                              income2009 = 0.4,
			      value = 0.4))  ## The caliper = "how many std dev" apart (rel. to sd
# of the original data) obs. are allowed to be.

## matches
matched.pairs = get_matches(matching)

# Now, let's look at a summary of what the matching did:
summary(matching)
plot(summary(matching))

# Save the matched data as it's own new dataset. How many observations did you end up with?
matched.df <- match.data(matching)

## We can also visualize how the "balance" of our treated and untreated groups differs now, in relationship to variables like "slope"
#ggplot(matched.df, aes(x =slope,   ## (change to "df" to see pre-matching balance).

gg.elev= ggplot(private.df, aes(x =elevation,   ## (change to "df" to see pre-matching balance).
                           color = as.factor(CE), 
                           fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c( "skyblue3", "orangered3"))+
  scale_fill_manual(values=c( "skyblue3", "orangered3"))+
  guides(color = "none") +
  labs(x = "Elevation (m)") + theme_bw()


gg.city= ggplot(private.df, aes(x =distCity,   ## (change to "df" to see pre-matching balance).
                           color = as.factor(CE), 
                           fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c( "skyblue3", "orangered3"))+
  scale_fill_manual(values=c( "skyblue3", "orangered3"))+
  guides(color = "none") +
  labs(x = "Distance to city center") + theme_bw()

gg.public= ggplot(private.df, aes(x =distPublic,   ## (change to "df" to see pre-matching balance).
                           color = as.factor(CE), 
                           fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c( "skyblue3", "orangered3"))+
  scale_fill_manual(values=c( "skyblue3", "orangered3"))+
  guides(color = "none") +
  labs(x = "Public land in HUC12") + theme_bw()


gg.trust = ggplot(private.df, aes(x =distTrust,   ## (change to "df" to see pre-matching balance).
                           color = as.factor(CE), 
                           fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c( "skyblue3", "orangered3"))+
  scale_fill_manual(values=c( "skyblue3", "orangered3"))+
  guides(color = "none") +
  labs(x = "Distance to land trust") + theme_bw()

gg.road= ggplot(private.df, aes(x =distRoad,   ## (change to "df" to see pre-matching balance).
                           color = as.factor(CE), 
                           fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c( "skyblue3", "orangered3"))+
  scale_fill_manual(values=c( "skyblue3", "orangered3"))+
  guides(color = "none") +
  labs(x = "Distance to road") + theme_bw()

gg.gap= ggplot(private.df, aes(x =distGAP12,   ## (change to "df" to see pre-matching balance).
                           color = as.factor(CE), 
                           fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c( "skyblue3", "orangered3"))+
  scale_fill_manual(values=c( "skyblue3", "orangered3"))+
  guides(color = "none") +
  labs(x = "Distance to Gap 1 or 2") + theme_bw()

gg.income= ggplot(private.df, aes(x =income2009,   ## (change to "df" to see pre-matching balance).
                               color = as.factor(CE), 
                               fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c( "skyblue3", "orangered3"))+
  scale_fill_manual(values=c( "skyblue3", "orangered3"))+
  guides(color = "none") +
  labs(x = "Income 2009") + theme_bw()

gg.pop= ggplot(private.df, aes(x =pop.delta,   ## (change to "df" to see pre-matching balance).
                               color = as.factor(CE), 
                               fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c( "skyblue3", "orangered3"))+
  scale_fill_manual(values=c( "skyblue3", "orangered3"))+
  guides(color = "none") +
  labs(x = "Population change") + theme_bw()

gg.val = ggplot(private.df, aes(x =value,   ## (change to "df" to see pre-matching balance).
                               color = as.factor(CE), 
                               fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c( "skyblue3", "orangered3"))+
  scale_fill_manual(values=c( "skyblue3", "orangered3"))+
  guides(color = "none") +
  labs(x = "Value") + theme_bw()


prematch.plots = ggarrange(gg.elev, gg.city, gg.public, gg.trust, gg.road, gg.gap, gg.income, gg.pop, gg.val,
                           labels = c("Elevation", "Distance city", 
                                      "Dist public land","Dist land trust", 
                                      "Dist road", "Dist GAP", "Income 2009", "Pop change", "Value"),
                           ncol = 3, nrow = 3)
prematch.plots

#### Matched plots
match.elev= ggplot(matched.df, aes(x =elevation,   ## (change to "df" to see pre-matching balance).
                           color = as.factor(CE), 
                           fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c( "yellow", "purple"))+
  scale_fill_manual(values=c( "yellow", "purple"))+
  guides(color = "none") +
  labs(x = "Elevation (m)") + theme_bw()


match.city= ggplot(matched.df, aes(x =distCity,   ## (change to "df" to see pre-matching balance).
                           color = as.factor(CE), 
                           fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c("yellow", "purple"))+
  scale_fill_manual(values=c( "yellow", "purple"))+
  guides(color = "none") +
  labs(x = "Distance to city center") + theme_bw()

match.public= ggplot(matched.df, aes(x =distPublic,   ## (change to "df" to see pre-matching balance).
                              color = as.factor(CE), 
                              fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c("yellow", "purple"))+
  scale_fill_manual(values=c( "yellow", "purple"))+
  guides(color = "none") +
  labs(x = "Distance to public land") + theme_bw()

match.trust = ggplot(matched.df, aes(x =distTrust,   ## (change to "df" to see pre-matching balance).
                             color = as.factor(CE), 
                             fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c("yellow", "purple"))+
  scale_fill_manual(values=c( "yellow", "purple"))+
  guides(color = "none") +
  labs(x = "Distance to land trust") + theme_bw()

match.road= ggplot(matched.df, aes(x =distRoad,   ## (change to "df" to see pre-matching balance).
                           color = as.factor(CE), 
                           fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c("yellow", "purple"))+
  scale_fill_manual(values=c( "yellow", "purple"))+
  guides(color = "none") +
  labs(x = "Distance to road") + theme_bw()

match.gap= ggplot(matched.df, aes(x =distGAP12,   ## (change to "df" to see pre-matching balance).
                           color = as.factor(CE), 
                           fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c("yellow", "purple"))+
  scale_fill_manual(values=c( "yellow", "purple"))+
  guides(color = "none") +
  labs(x = "Distance to GAP 1 or 2") + theme_bw()

match.income= ggplot(matched.df, aes(x =income2009,   ## (change to "df" to see pre-matching balance).
                                  color = as.factor(CE), 
                                  fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c( "yellow", "purple"))+
  scale_fill_manual(values=c( "yellow", "purple"))+
  guides(color = "none") +
  labs(x = "Income 2009") + theme_bw()

match.pop= ggplot(matched.df, aes(x =pop.delta,   ## (change to "df" to see pre-matching balance).
                               color = as.factor(CE), 
                               fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c( "yellow", "purple"))+
  scale_fill_manual(values=c( "yellow", "purple"))+
  guides(color = "none") +
  labs(x = "Population change") + theme_bw()

match.val = ggplot(matched.df, aes(x =value,   ## (change to "df" to see pre-matching balance).
                               color = as.factor(CE), 
                               fill=as.factor(CE))) +
  geom_density(alpha=0.4) +
  scale_color_manual(values=c( "yellow", "purple"))+
  scale_fill_manual(values=c( "yellow", "purple"))+
  guides(color = "none") +
  labs(x = "Value") + theme_bw()

match.plots = ggarrange(match.elev, match.city, 
                        match.public, match.trust, match.road, match.gap,
                        match.income, match.pop, match.val,
                          labels = c("Elevation","Distance city", 
                                     "Dist public land","Dist land trust", 
                                     "Dist road", "Dist GAP", "Income 2009", "Pop change", "Value"),
                           ncol = 3, nrow = 3)
match.plots

#----------------------- Regression ---------------------#
# uses the brms package
library(brms)
  
## Time invariant covs
otherCovs = c("system.index", "CE", "huc4", "huc8", "huc12", "huc12public", "GAP", "year_est", "ceID", "elevation", 
                "slope", "flowAcc", "distCity", "distTrust", "distRoad", "distPublic", "mesic.change")
dat.invar = matched.df[otherCovs]
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
dat.2004 = cbind(dat.invar, create_df(matched.df, 2004))
dat.2005 = cbind(dat.invar, create_df(matched.df, 2005))
dat.2006 = cbind(dat.invar, create_df(matched.df, 2006))
dat.2007 = cbind(dat.invar, create_df(matched.df, 2007))
dat.2008 = cbind(dat.invar, create_df(matched.df, 2008))
dat.2009 = cbind(dat.invar, create_df(matched.df, 2009))
dat.2010 = cbind(dat.invar, create_df(matched.df, 2010))
dat.2011 = cbind(dat.invar, create_df(matched.df, 2011))
dat.2012 = cbind(dat.invar, create_df(matched.df, 2012))
dat.2013 = cbind(dat.invar, create_df(matched.df, 2013))
dat.2014 = cbind(dat.invar, create_df(matched.df, 2014))
dat.2015 = cbind(dat.invar, create_df(matched.df, 2015))
dat.2016 = cbind(dat.invar, create_df(matched.df, 2016))
dat.2017 = cbind(dat.invar, create_df(matched.df, 2017))
dat.2018 = cbind(dat.invar, create_df(matched.df, 2018))
dat.2019 = cbind(dat.invar, create_df(matched.df, 2019))
dat.2020 = cbind(dat.invar, create_df(matched.df, 2020))


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
#priors.full = get_prior(outcome ~ CE*post + elevation.std + flowAcc.std + slope.std 
priors.full = get_prior(outcome ~ CE*post + flowAcc.std + slope.std 
                        + spei.std +  (1|year) + (1 |huc12),
                        #+ spei.std +  year + huc12, #fixed effects
                        #+ spei.std +  (1|year) + (1 |huc4),
                        data = panel, family = 'beta')

priors.full$prior[1:8] = "normal (0,1)"
priors.full$prior[10:14] = "normal (0,1)"
hist(rnorm(1000, 0,1))

## randomly sample the panel
panel.samp = panel %>% sample_frac(0.2)

# implement model
## Choose one year - am I capturing effect and spillovers
#did.ce.full <- brm(outcome ~ CE*post + elevation.std + flowAcc.std + slope.std + spei.std +  (1|year) + (1 |huc12),
did.ce.full <- brm(outcome ~ CE*post + flowAcc.std + slope.std + spei.std +  (1|year) + (1 |huc12),
#did.ce.full <- brm(outcome ~ CE*post + flowAcc.std + slope.std + spei.std + year + huc12, #fixed effects
#did.ce.full <- brm(outcome ~ CE*post + elevation.std + flowAcc.std + slope.std + spei.std +  (1|year) + (1 |huc4),
              data = panel.samp,
              #family='beta',
              family=Beta(),
              prior=priors.full, ## nor priors - 0,1 for any fixed effects; 0.2 for random effects
              control = list(adapt_delta = 0.999,max_treedepth = 15), ## lower to trial
              cores=4,
              #chains = 4, ## lower to trial
              chains = 4, ## lower to trial
              iter=8000 ## lower to trial
)

summary(did.ce.full)
plot(did.ce.full)

## visually check the model fit
pp_check(did.ce.full)

r2 =  bayes_R2(did.ce.full)
r2
save.image(file = "didPrivateMatch8k.RData")

