
library(broom)
library(dplyr)
library(tidyr)
library(reshape2)
library(purrr)
library(ggplot2)
library(GGally)
library(stringr)
library(tidyverse)
library(patchwork)
#arima(xreg = INDEPENDENT.VARS)


# loading model dataframes ------------------------------------------------

#model object
#fcast_taxon = readRDS("reboot/forecast_taxon.RDS")
#fcast_site = readRDS("reboot/forecast_site.RDS")


#forecast, residuals, CIs and AGBI.mid
fcast_taxon_long = readRDS("reboot/forecast_taxon_long.RDS")
fcast_site_long = readRDS("reboot/forecast_site_long.RDS")

#fitted, residuals and CIs, AGBI.mid
joined_site_AGBI = readRDS("reboot/joined_site_AGBIs.RDS")
joined_taxon_AGBI = readRDS("reboot/joined_taxon_AGBI.RDS")



# plotting AGBI over time 1950-2006 (data and site model) ---------------------------

#plottng AGBI.mid over time (data from increment model)
ggplot(data= joined_site_AGBI) +
  geom_line(aes(x=year, y= AGBI.mid, colour=site)) +
  theme_light(14) +
  labs( x = "Year", y = "biomass increment (Mg/ha)")

#from models_site 
#plotting fitted AGBI over time
ggplot(data= joined_site_AGBI) +
  geom_line(aes(x=year, y= site_fitted, colour=site)) +
  #geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi, colour=site, fill=site), alpha = 0.5) +
  theme_light(14) +
  labs( x = "Year", y = "biomass increment (Mg/ha)")

#using data (increment model) and fitted values from models_site
#plotting fitted AGBI and data, by site over time 
ggplot() +
  geom_line(data= joined_site_AGBI, aes(x=year, y= site_fitted, colour=site), linetype=2) +
  geom_line(data= joined_site_AGBI, aes(x=year, y= AGBI.mid, colour=site)) +
  # geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi, colour=site, fill=site), alpha = 0.5) +
  theme_light(14) +
  labs( x = "Year", y = "biomass increment (Mg/ha)")


# #plotting summed AGBI from taxon model over time 
#fitted_taxon_2_site
ggplot(data= joined_site_AGBI) +
  geom_line(aes(x=year, y= taxon_2site_fitted, colour=site)) +
  #geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi, colour=site, fill=site), alpha = 0.5) +
  theme_light(14) +
  labs( x = "Year", y = "biomass increment (Mg/ha)")


############plotting AGBI over time against each other########################################

#taxon model summed AGBI v. site model AGBI
p1 = ggplot(data = joined_site_AGBI) +
  geom_point(aes(x=site_fitted, y= taxon_2site_fitted, colour = site)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue")+
  theme_light(14) +
  xlab('AGBI (mg/ha): site model') +
  ylab('AGBI (mg/ha): taxon model')
p1

#data vs. taxon model fitted values
p2 = ggplot(data = joined_site_AGBI) +
  geom_point(aes(x=AGBI.mid, y= taxon_2site_fitted, colour = site)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue")+
  theme_light(14)  +
  xlab('AGBI (mg/ha): data') +
  ylab('AGBI (mg/ha): taxon model')
p2

#site model fitted values vs data
p3 = ggplot(data = joined_site_AGBI) +
  geom_point(aes(x=AGBI.mid, y= site_fitted, colour = site)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue")+
  theme_light(14)  +
  xlab('AGBI (mg/ha): data') +
  ylab('AGBI (mg/ha): site model')
p3


#taxon model summed AGBI v. site model AGBI
#facet wrap by site
p1 = ggplot(data = joined_site_AGBI) +
  geom_point(aes(x=site_fitted, y= taxon_2site_fitted, colour = site)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue")+
  theme_light(14) +
  xlab('AGBI (mg/ha): site model') +
  ylab('AGBI (mg/ha): taxon model') +
  facet_wrap(~site, scales='free')
p1

#data vs. taxon model fitted values
#facet_wrap by site
p2 = ggplot(data = joined_site_AGBI) +
  geom_point(aes(x=AGBI.mid, y= taxon_2site_fitted, colour = site)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue")+
  theme_light(14)  +
  xlab('AGBI (mg/ha): data') +
  ylab('AGBI (mg/ha): taxon model') +
  facet_wrap(~site, scales='free')
p2

#site model fitted values vs data
#facet_Wrap by site 
p3 = ggplot(data = joined_site_AGBI) +
  geom_point(aes(x=AGBI.mid, y= site_fitted, colour = site)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue")+
  theme_light(14)  +
  xlab('AGBI (mg/ha): data') +
  ylab('AGBI (mg/ha): site model') +
  facet_wrap(~site, scales='free')
p3

p1+p2+p3


#data vs site model fitted values and taxon summed fitted to site
p4 = ggplot(data = joined_site_AGBI) +
  geom_point(aes(x=AGBI.mid, y= site_fitted), colour='grey') +
  geom_point(aes(x=AGBI.mid, y= taxon_2site_fitted), alpha=0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue")+
  theme_light(14) +
  facet_wrap(~site, scales='free')
p4


p4 = ggplot(data = joined_site_AGBI) +
  geom_line(aes(x=year, y= site_fitted), colour='darkgreen') +
  geom_line(aes(x=year, y= taxon_2site_fitted),  colour='darkorange', alpha=0.7) +
  geom_line(aes(x=year, y= AGBI.mid), colour='dodgerblue', alpha=0.7) +
  theme_light(14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~site, scales='free')
p4


# difference between data and model AGBI  -----------------------------------------------------

#creating column sited_fitted - site_AGBI 
#creating column sited_fitted - site_AGBI
joined_site_AGBI$diff_site_data = joined_site_AGBI$site_fitted - joined_site_AGBI$AGBI.mid
joined_site_AGBI$diff_taxon_site_data = joined_site_AGBI$taxon_2site_fitted - joined_site_AGBI$AGBI.mid

#absolute value of differences
joined_site_AGBI$abs_diff_site_data = abs(joined_site_AGBI$diff_site_data)
joined_site_AGBI$abs_diff_taxon_site_data = abs(joined_site_AGBI$taxon_2site_fitted - joined_site_AGBI$AGBI.mid)
# pivot_long(joined_site_AGBI, )

#summing total differences between model and data
sum(joined_site_AGBI$abs_diff_site_data)
sum(joined_site_AGBI$abs_diff_taxon_site_data)


#plotting differences against each other (site vs taxon model summed)
ggplot(data=joined_site_AGBI) +
  geom_point(aes(x=diff_site_data, y=diff_taxon_site_data))+
  theme_light(14) +
  facet_wrap(~site)


#joined data with model AGBI data
joined_AGBI_diff <- joined_site_AGBI %>%
  pivot_longer(
    cols = c(diff_site_data, diff_taxon_site_data), # Columns to transform
    names_to = "AGBI_diff",      # New column for the old column names
    values_to = "value"      # New column for the cell values
  )

#density plot of differences for each site 
ggplot(data=joined_AGBI_diff) +
  geom_histogram(aes(x=value, y=after_stat(density)))+
  theme_light(14) +
  facet_grid(AGBI_diff~site)

#density plot of differences for each site
ggplot(data=joined_AGBI_diff) +
  geom_density(aes(x=value, y=after_stat(density), fill=AGBI_diff))+
  theme_light(14) +
  facet_grid(site~.)


#AGBI differences over time 
ggplot(data=joined_AGBI_diff) +
  geom_line(aes(x=year, y=value, colour=AGBI_diff))+
  theme_light(14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~site, scales='free_y')

#differences compared against each other (Site vs taxa)
#with 1:1 line 
ggplot(data=joined_AGBI_diff, aes(x=abs_diff_taxon_site_data, y=abs_diff_site_data)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  geom_smooth(method='lm', ) + facet_wrap(~site, scales='free')



# ggplot(data=joined_AGBI_diff, aes(x=abs_diff_taxon_site_data, y=abs_diff_site_data)) +
#   geom_histogram() +
#   geom_abline(intercept=0, slope=1) +
#   geom_smooth(method='lm', ) + facet_wrap(~site, scales='free')




# # disturbance years -----------------------------------------------------


AGBI_taxon_anom = readRDS('reboot/AGBI_taxon_anom.RDS')
AGBI_taxon_site_anom = readRDS('reboot/AGBI_taxon_site_anom.RDS')


AGBI_taxon_disturb = merge(joined_taxon_AGBI, 
                           AGBI_taxon_anom[,c('year', 'taxon', 'site', 'AGBI.mid', 'disturb')], 
                           by = c('year', 'taxon', 'site'))
AGBI_taxon_disturb$residuals = -AGBI_taxon_disturb$residuals
AGBI_taxon_disturb$disturb_lag0 = as.numeric(AGBI_taxon_disturb$disturb)
AGBI_taxon_disturb$disturb_lag0[which(AGBI_taxon_disturb$disturb_lag0 == 0)] = NA

AGBI_taxon_disturb$event = AGBI_taxon_disturb$year
AGBI_taxon_disturb$event[which(is.na(AGBI_taxon_disturb$disturb_lag0))] = NA

AGBI_taxon_disturb = AGBI_taxon_disturb %>% 
  group_by(site, taxon) %>% 
  arrange(year, .by_group=TRUE) %>%
  mutate(disturb_lag1 = lag(disturb_lag0, 1) * 2,
         disturb_lag2 = lag(disturb_lag0, 2) * 3,
         disturb_lag3 = lag(disturb_lag0, 3) * 4) 

AGBI_taxon_disturb = AGBI_taxon_disturb %>% 
  dplyr::mutate(disturb_lag = pmin(disturb_lag0, disturb_lag1, disturb_lag2, disturb_lag3, na.rm=TRUE))

AGBI_taxon_disturb = AGBI_taxon_disturb %>%
  group_by(site, taxon) %>% 
  dplyr::mutate(event_year = pmax(event, lag(event, 1), lag(event, 2), lag(event, 3), na.rm=TRUE))



sites <- c("GOOSE", "ROOSTER", "HARVARD", "HMC", "NRP", "SYLVANIA")
for (site in sites){
  p = ggplot(data=AGBI_taxon_disturb[which(AGBI_taxon_disturb$site==site),]) +
    geom_point(aes(x=AGBI.mid.x, y=fitted, colour=disturb)) +
    facet_wrap(~taxon, scales='free') + theme_light() + theme(aspect.ratio=1) +
    geom_abline(intercept=0, slope=1, linetype=2, colour='grey')
  print(p)
}

p = ggplot(data=subset(AGBI_taxon_disturb, !is.na(disturb_lag))) +
  geom_boxplot(aes(x=factor(disturb_lag-1), y=residuals)) +
  # geom_smooth(method='lm', aes(x=disturb_lag-1, y=-residuals), formula = y ~ poly(x, 2)) +
  facet_wrap(~taxon, scales='free') + theme_light() + 
  theme(aspect.ratio=1) +
  geom_hline(yintercept=0, linetype=2, colour='grey') +
  xlab('years since disturbance') +
  ylab('model - data (Mg/ha)') 
print(p)

pdf('figures/AGBI_stat_ARIMA_disturb_scatter.pdf')
for (site in sites){
  
  this_AGBI_taxon_disturb = AGBI_taxon_disturb[which(AGBI_taxon_disturb$site==site),]
  
  this_disturb_resids = this_AGBI_taxon_disturb[which(this_AGBI_taxon_disturb$disturb_lag==1), ]
  
  p = ggplot(data=this_disturb_resids) +
    geom_point(aes(y=residuals, x=taxon)) +
    # facet_wrap(~taxon, scales='free') +
    theme_light() + theme(aspect.ratio=1) #+
  # geom_abline(intercept=0, slope=1, linetype=2, colour='grey')
  print(p)
  
  
  # p = ggplot(data=subset(this_AGBI_taxon_disturb, !is.na(disturb_lag))) +
  #   geom_point(aes(x=disturb_lag-1, y=-residuals)) +
  #   # facet_wrap(~taxon, scales='free') + theme_light() + 
  #   theme(aspect.ratio=1) +
  #   geom_hline(yintercept=0, linetype=2, colour='grey') +
  #   xlab('years since disturbance') +
  #   ylab('model - data (Mg/ha)')
  # print(p)
  
  p = ggplot(data=subset(this_AGBI_taxon_disturb, !is.na(disturb_lag))) +
    geom_point(aes(x=disturb_lag-1, y=residuals)) +
    facet_wrap(~taxon, scales='free') + theme_light() + 
    theme(aspect.ratio=1) +
    geom_hline(yintercept=0, linetype=2, colour='grey') +
    xlab('years since disturbance') +
    ylab('model - data (Mg/ha)') +
    ggtitle(site)
  print(p)
  
  # p = ggplot(data=subset(this_AGBI_taxon_disturb, !is.na(disturb_lag))) +
  #   geom_point(aes(x=disturb_lag-1, y=-residuals, group=event_year)) +
  #   geom_smooth(method='loess', aes(x=disturb_lag-1, y=-residuals, group=event_year), colour='dodgerblue', alpha=0.5) +
  #   # geom_smooth(method='loess', aes(x=disturb_lag-1, y=-residuals)) +
  #   facet_wrap(~taxon, scales='free') + theme_light() + 
  #   theme(aspect.ratio=1) +
  #   geom_hline(yintercept=0, linetype=2, colour='grey') +
  #   xlab('years since disturbance') +
  #   ylab('model - data (Mg/ha)') +
  #   ggtitle(site)
  # print(p)
  
  
  # p = ggplot(data=AGBI_taxon_disturb[which(AGBI_taxon_disturb$site==site),], aes(x=disturb_lag, y=residuals)) +
  #   geom_point() +
  #   geom_smooth(formula = y ~ poly(x,2), method='lm') +
  #   facet_wrap(~taxon, scales='free') #+
  #   # theme_light() + 
  #   # theme(aspect.ratio=1) +
  #   # geom_hline(yintercept=0, linetype=2, colour='grey')
  # print(p)
  
  
  p = ggplot(data=AGBI_taxon_disturb[which(AGBI_taxon_disturb$site==site),]) +
    geom_point(aes(x=factor(disturb_lag-1), y=residuals)) +
    facet_wrap(~taxon, scales='free') + theme_light() + 
    theme(aspect.ratio=1) +
    geom_hline(yintercept=0, linetype=2, colour='grey') +
    xlab('years since disturbance') +
    ylab('model - data (Mg/ha)') +
    ggtitle(site)
  print(p)
  
  
  p = ggplot(data=AGBI_taxon_disturb[which(AGBI_taxon_disturb$site==site),]) +
    geom_boxplot(aes(x=factor(disturb_lag-1), y=residuals)) +
    facet_wrap(~taxon, scales='free') + theme_light() + 
    theme(aspect.ratio=1) +
    geom_hline(yintercept=0, linetype=2, colour='grey') +
    xlab('years since disturbance') +
    ylab('model - data (Mg/ha)') +
    ggtitle(site)
  print(p)
  
  # p = ggplot() +
  #   geom_density(data=this_AGBI_taxon_disturb, aes(x=residuals)) +
  #   facet_wrap(~taxon, scales='free') + theme_light() + 
  #   theme(aspect.ratio=1) +
  #   geom_vline(data=this_disturb_resids, aes(xintercept=residuals))
  # print(p)
}
dev.off()


AGBI_site_disturb = merge(joined_site_AGBI, 
                          AGBI_taxon_site_anom[,c('year', 'site', 'AGBI.mid', 'disturb')], 
                          by = c('year', 'site'))

AGBI_site_disturb$disturb_lag0 = as.numeric(AGBI_site_disturb$disturb)
AGBI_site_disturb$disturb_lag0[which(AGBI_site_disturb$disturb_lag0 == 0)] = NA

AGBI_site_disturb$event = AGBI_site_disturb$year
AGBI_site_disturb$event[which(is.na(AGBI_site_disturb$disturb_lag0))] = NA


AGBI_site_disturb = AGBI_site_disturb %>% 
  group_by(site) %>% 
  arrange(year, .by_group=TRUE) %>%
  mutate(disturb_lag1 = lag(disturb_lag0, 1) * 2,
         disturb_lag2 = lag(disturb_lag0, 2) * 3,
         disturb_lag3 = lag(disturb_lag0, 3) * 4) 

AGBI_site_disturb = AGBI_site_disturb %>% 
  dplyr::mutate(disturb_lag = pmin(disturb_lag0, disturb_lag1, disturb_lag2, disturb_lag3, na.rm=TRUE))

AGBI_site_disturb = AGBI_site_disturb %>%
  dplyr::mutate(event_year = pmax(event, lag(event, 1), lag(event, 2), lag(event, 3), na.rm=TRUE))


p = ggplot(data=AGBI_site_disturb) +
  geom_point(aes(x=disturb_lag-1, y=diff_site_data)) +
  # facet_wrap(~site, scales='free') + 
  theme_light() + 
  theme(aspect.ratio=1) +
  geom_hline(yintercept=0, linetype=2, colour='grey')
print(p)

p = ggplot(data=AGBI_site_disturb) +
  geom_point(aes(x=disturb_lag-1, y=diff_site_data)) +
  facet_wrap(~site, scales='free') + theme_light() + 
  theme(aspect.ratio=1) +
  geom_hline(yintercept=0, linetype=2, colour='grey')
print(p)

p = ggplot(data=AGBI_site_disturb) +
  geom_boxplot(aes(x=factor(disturb_lag-1), y=diff_site_data)) +
  facet_wrap(~site, scales='free') + theme_light() + 
  theme(aspect.ratio=1) +
  geom_hline(yintercept=0, linetype=2, colour='grey')
print(p)

p = ggplot(data=AGBI_site_disturb) +
  geom_point(aes(x=disturb_lag-1, y=diff_site_data, group=event_year)) +
  geom_smooth(method='loess', aes(x=disturb_lag-1, y=diff_site_data, group=event_year)) +
  facet_wrap(~site, scales='free') + theme_light() + 
  theme(aspect.ratio=1) +
  geom_hline(yintercept=0, linetype=2, colour='grey')
print(p)


p = ggplot(data=AGBI_site_disturb) +
  geom_point(aes(x=factor(disturb_lag), y=diff_site_data)) +
  facet_wrap(~site, scales='free') + theme_light() + 
  theme(aspect.ratio=1) +
  geom_hline(yintercept=0, linetype=2, colour='grey')
print(p)

p = ggplot(data=AGBI_site_disturb) +
  geom_density(aes(x=-diff_site_data)) +
  facet_wrap(~site, scales='free') + theme_light() + 
  theme(aspect.ratio=1) +
  geom_hline(yintercept=0, linetype=2, colour='grey')
print(p)

p = ggplot(data=AGBI_site_disturb) +
  geom_point(aes(x=AGBI.mid.x, y=site_fitted, colour=disturb)) +
  facet_wrap(~site, scales='free') + theme_light() + theme(aspect.ratio=1) +
  geom_abline(intercept=0, slope=1, linetype=2, colour='grey')
print(p)

#????
foo = data.frame(AGBI_site_disturb[c('year', 'site', 'diff_site_data', 'disturb_lag', 'event')], taxon='site')
colnames(foo) = c('year', 'site', 'residuals', 'disturb_lag', 'event', 'taxon')

#merging taxon column
AGBI_disturb_merged = bind_rows(AGBI_taxon_disturb[c('year', 'taxon', 'site', 'residuals', 'disturb_lag', 'event')], foo)


taxa = levels(factor(AGBI_disturb_merged$taxon))
taxa = taxa[which(taxa!='site')]
AGBI_disturb_merged$taxon = factor(AGBI_disturb_merged$taxon, levels = c(taxa, 'site'))

p = ggplot(data=subset(AGBI_disturb_merged, disturb_lag==1)) +
  geom_point(aes(y=residuals, x=taxon)) +
  facet_wrap(~site, scales='free') +
  theme_light() + theme(aspect.ratio=1) #+
# geom_abline(intercept=0, slope=1, linetype=2, colour='grey')
print(p)

bar = AGBI_taxon_disturb %>% group_by(site, year, disturb_lag) %>% dplyr::summarize(taxon_resid_sum = sum(residuals, na.rm=TRUE)) 

bar2 = AGBI_site_disturb %>% group_by(site, year, disturb_lag) %>% dplyr::summarize(site_resid_sum = sum(diff_site_data, na.rm=TRUE)) 

bar3 = left_join(bar, bar2, by=c('site', 'year'))

bar4 = subset(bar3, disturb_lag.x ==1 | disturb_lag.y == 1)
bar4$disturb_group = ifelse(bar4$disturb_lag.x == 1, 'taxon', 'site')
bar4$disturb_group[which(is.na(bar4$disturb_group))] = 'site'

ggplot(data=bar4) +
  geom_point(aes(y=taxon_resid_sum, x=site_resid_sum, colour=disturb_group))

ggplot(data=bar4) +
  geom_hline(yintercept =0, colour='grey') +
  geom_vline(xintercept = 0, colour='grey') +
  geom_point(aes(y=taxon_resid_sum, x=site_resid_sum, colour=disturb_group)) +
  facet_wrap(~site,scales='free') +
  geom_abline(intercept=0, slope=1, linetype=2, colour='grey') 

# make list of disturbance years for each site
taxon_disturb_years = AGBI_taxon_disturb[which(AGBI_taxon_disturb$disturb),c('site', 'year', 'taxon')]
site_disturb_years = AGBI_site_disturb[which(AGBI_site_disturb$disturb),c('site', 'year')]

disturb_years = rbind(taxon_disturb_years, site_disturb_years)
disturb_years = disturb_years %>% distinct()
disturb_years$disturb_any = TRUE


# foo = left_join(AGBI_taxon_disturb, disturb_years)
# foo$disturb_any[which(is.na(foo$disturb_any))] = FALSE
# 
# for (site in sites){
#   p = ggplot(data=foo[which(foo$site==site),]) +
#     geom_point(aes(x=AGBI.mid.x, y=fitted, colour=disturb_any)) +
#     facet_wrap(~taxon, scales='free') + theme_light() + theme(aspect.ratio=1) +
#     geom_abline(intercept=0, slope=1, linetype=2, colour='grey')
#   print(p)
# }

p = ggplot(data=AGBI_site_disturb) +
  geom_point(aes(x=AGBI.mid.x, y=site_fitted, colour=disturb)) +
  facet_wrap(~site, scales='free') + theme_light() + theme(aspect.ratio=1) +
  geom_abline(intercept=0, slope=1, linetype=2, colour='grey')
print(p)




# filtering cumsum --------------------------------------------------------
disturbance_years <- list(GOOSE = 1981,ROOSTER = c(1983, 1992),HARVARD = 1981)
disturbance_years = data.frame(site = c('GOOSE', 'ROOSTER', 'ROOSTER', 'HARVARD'), 
                               years = c(1981,1983,1992,1981))


#loading climate data
clim_taxon = readRDS("reboot/clim_taxon.RDS")
#clim_saveRDS(clim_seasons_taxon, "reboot/clim_seasons_taxon.RDS")
clim_seasons_taxon = readRDS("reboot/clim_seasons_taxon_long.RDS")
clim_site = readRDS("reboot/clim_site.RDS")
clim_seasons_site = readRDS("reboot/clim_seasons_site_long.RDS")




# fitted_res_joined = joined_taxon_AGBI






#adding column for disturbance years and 5 years after disturbance occurance??
joined_taxon_AGBI$disturb_year = NA
for (i in 1:nrow(disturbance_years)){
  this_site = disturbance_years$site[i]
  this_year = disturbance_years$year[i]
  
  this_idx = which(joined_taxon_AGBI$site == this_site)
  
  # this_already = which(!is.na(fit_res_joined[this_idx, 'disturb_year'])
  
  disturb_mat = cbind(joined_taxon_AGBI[this_idx, 'disturb_year'], joined_taxon_AGBI[this_idx, 'year'] - this_year)
  
  joined_taxon_AGBI[this_idx, 'disturb_year'] = apply(disturb_mat, 1, min, na.rm=TRUE)
  
  # fit_res_joined[this_idx, 'disturb_year'] = fit_res_joined[this_idx, 'year'] - this_year
  
  
  
  # which(fit_res_joined[this_idx, 'disturb_year'] <0) NA
  # fit_res_joined[which(fit_res_joined[this_idx, 'disturb_year'] >5), 'disturb_year'] = NA
}

joined_taxon_AGBI[which(joined_taxon_AGBI[, 'disturb_year'] <0), 'disturb_year'] = NA
joined_taxon_AGBI[which(joined_taxon_AGBI[, 'disturb_year'] >5), 'disturb_year'] = NA



#correlation between AGBI.mid and fitted values at the taxa level
fitted_AGBI_cor <- joined_taxon_AGBI %>%
  group_by(site, taxon) %>%
  summarise(
    correlation = cor(AGBI.mid, fitted, use = "complete.obs"),
    p_value = cor.test(AGBI.mid, fitted)$p.value,
    .groups = "drop"
  )

#plotting correlation of fitted and AGBI.mid
ggplot()+
  geom_point(data = fitted_AGBI_cor, aes(x=taxon, y=correlation))+
  scale_fill_gradient2(limits = c(-1.0, 1.0),
                       low = "red", mid = "white", high = "blue",
                       midpoint = 0)+
  # geom_point(data = cor_fitted_AGBI, aes(x=taxon, y= taxon, shape = sig), size=3)+
  # scale_shape_manual(values=c(1, NA)) +
  facet_grid(site~.)+
  # xlab('taxon') +
  # ylab('Species') +
  # ggtitle(paste0(site, '; ', var)) +
  theme(plot.title = element_text(size=18))

# more disturbance/correlation??? ---------------------------------------------------------------------

# fitted_res_joined = joined_taxon_AGBI
# fit_res_joined <- fit_res_joined %>%
#   rowwise() %>%
#   mutate(
#     disturbance = ifelse(
#       site %in% names(disturbance_years) &&
#         any(year >= disturbance_years[[site]] &
#               year <= disturbance_years[[site]] + 5),
#       1, 0
#     )
#   ) %>%
#   ungroup()
# 
# fit_res_joined <- fit_res_joined %>%
#   rowwise() %>%
#   apply(
#     disturbance = for (i in nrow(disturbance_years))
#       {ifelse(site == disturbance_years$site[i]  & 
#                 year <= disturbance_years$year[i],
#               year - disturbance_years$year[i], NA)}
#     )



# filtering taxon cumulative sum ------------------------------------------------


fit_res_joined = joined_taxon_AGBI

# 1. Calculate fraction + cumulative sum
agbi_fraction <- joined_taxon_AGBI %>%
  ungroup() %>%
  group_by(site, taxon) %>%
  mutate(taxa.AGBI = sum(AGBI.mid, na.rm = TRUE)) %>%
  group_by(site) %>%
  mutate(site.AGBI = sum(AGBI.mid, na.rm = TRUE)) %>%
  ungroup() %>%
  select(site, taxon, taxa.AGBI, site.AGBI) %>%
  distinct() %>%
  mutate(frac.AGBI = ifelse(site.AGBI > 0, taxa.AGBI / site.AGBI, NA_real_))

agbi_cumsum <- agbi_fraction %>% 
  arrange(site, desc(frac.AGBI)) %>%   
  group_by(site) %>%
  mutate(cum_sum = cumsum(frac.AGBI)) %>%
  ungroup()

# 2. Filter those making up 95% of biomass
agbi_cumsum_filter <- agbi_cumsum %>% 
  filter(cum_sum < 0.95)

# 3. Add TRUE/FALSE flag back to joined_taxon_AGBI
fit_res_flagged <- joined_taxon_AGBI %>%
  mutate(in_top95 = if_else(
    paste(site, taxon) %in% paste(agbi_cumsum_filter$site, agbi_cumsum_filter$taxon),
    TRUE, FALSE
  ))


#joining fitted, residuals, CI with filtered cumsum %
filtered_taxa_AGBI = inner_join(joined_taxon_AGBI, agbi_cumsum_filter[,c('site', 'taxon', 'cum_sum')], by = c('site', 'taxon'))
#joining forecast 2007-2011, residuals, CI with filtered cumsum %
filtered_taxa_forecast = inner_join(fcast_taxon_long, agbi_cumsum_filter[,c('site', 'taxon', 'cum_sum')], by = c('site', 'taxon'))



# PLOTTING fitted and forecast over time at the taxa and site level ----------------------------------------------------------------
# site = "GOOSE"
# taxon = "QURU"

#creating list with site and taxa names 
sites <- c("GOOSE", "ROOSTER", "HARVARD", "HMC", "NRP", "SYLVANIA")
taxa = (unique(clim_taxon$taxon))



#fitted values plotted for each taxa and site wit forecast
pdf("figures/AGBI_fitted_forecast.pdf", width=10, height=8)
for (site in sites) {
  for (taxon in taxa) {
    print(site)
    print(taxon)
    
    
    
    disturbance <- taxon_disturb_years %>%
      filter(site == !!site,
             taxon == !!taxon) %>%
      pull(year)
    
    # 
    # fitted_sub = fitted_long %>%
    #   dplyr::filter(site == !!site,
    #          taxon == !!taxon)
    
    
    forecast_sub = fcast_taxon_long %>%
      filter(site == !!site,
             taxon == !!taxon)
    
    res_fit_sub = joined_taxon_AGBI %>% 
      filter(site == !!site,
             taxon == !!taxon)
    if (nrow(res_fit_sub) == 0){ next}
    
    p = ggplot() +
      geom_point(data = res_fit_sub, aes(x = year, y = AGBI.mid, color = "Observed")) +
      geom_point(data = forecast_sub, aes(x = year, y = AGBI.mid, color = "Observed"))+
      geom_line(data = forecast_sub, aes(x = year, y = AGBI.mid, color = "Observed"))+
      geom_line(data = res_fit_sub, aes(x = year, y = AGBI.mid, color = "Observed", alpha = 0.5)) +
      geom_point(data = res_fit_sub, aes(x = year, y = fitted, color = "Fitted")) +
      geom_line(data = res_fit_sub, aes(x = year, y = fitted, color = "Fitted", alpha = 0.5)) +
      geom_vline(xintercept = disturbance, linetype = "dashed", color = "red") +
      geom_ribbon(data = forecast_sub, aes(x = year, ymin = forecast_lo, ymax = forecast_hi, fill = "Forecast CI"), alpha = 0.4) +
      geom_ribbon(data = res_fit_sub, aes(x = year, ymin = fitted_lo, ymax = fitted_hi, fill = "Fitted CI"), alpha = 0.5) +
      geom_point(data = forecast_sub, aes(x = year, y = forecast_mean, color = "Forecast")) +
      geom_line(data = forecast_sub, aes(x = year, y = forecast_mean, color = "Forecast")) +
      scale_color_manual(name = "Type", values = c("Observed" = "black", "Fitted" = "blue", 
                                                   "Forecast" = "orange")) +
      scale_fill_manual(name = "Ribbon", values = c("Forecast CI" = "orange", "Fitted CI" = "lightblue" )) +
      labs(x = "Year", y = "biomass increment (Mg/ha)")+
      ggtitle(paste0(site, '; ', taxon)) +
      theme_light(base_size = 14)
    
    print(p) 
  }
}
dev.off()


#fitted values plotted for each  site wit forecast
pdf("figures/AGBI_fitted_forecast_site.pdf", width=10, height=8)
for (site in sites) {
  print(site)
  
  
  disturbance <- site_disturb_years %>%
    filter(site == !!site,
           taxon == !!taxon) %>%
    pull(year)
  
  # 
  # fitted_sub = fitted_long %>%
  #   dplyr::filter(site == !!site,
  #          taxon == !!taxon)
  
  
  forecast_sub = fcast_site_long %>%
    filter(site == !!site)
  
  res_fit_sub = joined_site_AGBI %>% 
    filter(site == !!site)
  if (nrow(res_fit_sub) == 0){ next}
  
  p = ggplot() +
    geom_point(data = res_fit_sub, aes(x = year, y = AGBI.mid, color = "Observed")) +
    geom_line(data = res_fit_sub, aes(x = year, y = AGBI.mid, color = "Observed", alpha = 0.5)) +
    geom_point(data = forecast_sub, aes(x = year, y = AGBI.mid, color = "Observed")) +
    geom_line(data = forecast_sub, aes(x = year, y = AGBI.mid, color = "Observed", alpha = 0.5)) +
    geom_point(data = res_fit_sub, aes(x = year, y = site_fitted, color = "Fitted")) +
    geom_line(data = res_fit_sub, aes(x = year, y = site_fitted, color = "Fitted", alpha = 0.5)) +
    geom_point(data = res_fit_sub, aes(x = year, y = taxon_2site_fitted, color = "taxon_Sum")) +
    geom_line(data = res_fit_sub, aes(x = year, y = taxon_2site_fitted, color = "taxon_Sum", alpha = 0.5)) +
    geom_vline(xintercept = disturbance, linetype = "dashed", color = "red") +
    geom_ribbon(data = forecast_sub, aes(x = year, ymin = forecast_lo, ymax = forecast_hi, fill = "Forecast CI"), alpha = 0.4) +
    geom_ribbon(data = res_fit_sub, aes(x = year, ymin = fitted_lo, ymax = fitted_hi, fill = "Fitted CI"), alpha = 0.3) +
    geom_point(data = forecast_sub, aes(x = year, y = forecast_mean, color = "Forecast")) +
    geom_line(data = forecast_sub, aes(x = year, y = forecast_mean, color = "Forecast")) +
    scale_color_manual(name = "Type", values = c("Observed" = "black", "Fitted" = "darkgreen", 
                                                 "Forecast" = "orange", "taxon_Sum" = "purple")) +
    scale_fill_manual(name = "Ribbon", values = c("Forecast CI" = "orange", "Fitted CI" = "lightpink" )) +
    labs(x = "Year", y = "biomass increment (Mg/ha)")+
    ggtitle(paste0(site)) +
    theme_light(base_size = 14)
  
  print(p) 
  
}
dev.off()


# fitted and forecast over time (cumulative sum) --------------------------

site = "GOOSE"
taxon = "QURU"

#fitted values plotted for each taxa and site CUMULATIVE SUM
while (!is.null(dev.list())) dev.off()

combos <- filtered_taxa_AGBI %>% distinct(site, taxon)

pdf("figures/AGBI_fitted_forecast_CUMSUM.pdf", width = 10, height = 8)

for (i in seq_len(nrow(combos))) {
  site_i  <- combos$site[i]
  taxon_i <- combos$taxon[i]
  message("Plotting: ", site_i, " / ", taxon_i)
  
  tryCatch({
    disturbance <- taxon_disturb_years %>%
      filter(site == site_i, taxon == taxon_i) %>%
      pull(year)
    
    res_fit_sub <- filtered_taxa_AGBI %>%
      filter(site == site_i, taxon == taxon_i)
    
    forecast_sub <- filtered_taxa_forecast %>%
      filter(site == site_i, taxon == taxon_i)
    
    p <- ggplot() +
      geom_point(data = res_fit_sub, aes(x = year, y = AGBI.mid, color = "Observed")) +
      geom_line(data = res_fit_sub, aes(x = year, y = AGBI.mid, color = "Observed"), alpha = 0.5) +
      geom_point(data = res_fit_sub, aes(x = year, y = fitted, color = "Fitted")) +
      geom_line(data = res_fit_sub, aes(x = year, y = fitted, color = "Fitted"), alpha = 0.5) +
      geom_ribbon(data = res_fit_sub, aes(x = year, ymin = fitted_lo, ymax = fitted_hi, fill = "Fitted CI"), alpha = 0.5) +
      geom_point(data = forecast_sub, aes(x = year, y = AGBI.mid, color = "Observed")) +
      geom_line(data = forecast_sub, aes(x = year, y = AGBI.mid, color = "Observed")) +
      geom_ribbon(data = forecast_sub, aes(x = year, ymin = forecast_lo, ymax = forecast_hi, fill = "Forecast CI"), alpha = 0.4) +
      geom_point(data = forecast_sub, aes(x = year, y = forecast_mean, color = "Forecast")) +
      geom_line(data = forecast_sub, aes(x = year, y = forecast_mean, color = "Forecast")) +
      scale_color_manual(name = "Type",
                         values = c("Observed" = "black", "Fitted" = "blue", "Forecast" = "orange")) +
      scale_fill_manual(name = "Ribbon",
                        values = c("Forecast CI" = "orange", "Fitted CI" = "lightblue")) +
      labs(x = "Year", y = "biomass increment (Mg/ha)") +
      ggtitle(paste0(site_i, "; ", taxon_i)) +
      theme_light(base_size = 14)
    
    if (length(disturbance) > 0) {
      p <- p + geom_vline(xintercept = disturbance, linetype = "dashed", color = "red")
    }
    
    print(p)
  }, error = function(e) {
    message("  FAILED: ", conditionMessage(e))
  })
}

dev.off()

#plotting fitted and forecast over time for cumsum, facet wrap by taxon 
while (!is.null(dev.list())) dev.off()

site_list <- filtered_taxa_AGBI %>% distinct(site) %>% pull(site)

pdf("figures/AGBI_fitted_forecast_CUMSUM_facet_wrap.pdf", width = 16, height = 12)

for (site_i in site_list) {
  message("Plotting: ", site_i)
  
  tryCatch({
    res_fit_sub  <- filtered_taxa_AGBI %>% filter(site == site_i)
    forecast_sub <- filtered_taxa_forecast %>% filter(site == site_i)
    disturbance  <- taxon_disturb_years %>%
      filter(site == site_i, taxon %in% res_fit_sub$taxon)
    
    n_taxa <- n_distinct(res_fit_sub$taxon)
    ncol_i <- ceiling(sqrt(n_taxa))   # near-square grid, all on one page
    
    p <- ggplot() +
      geom_point(data = res_fit_sub, aes(x = year, y = AGBI.mid, color = "Observed"), size = 0.8) +
      geom_line(data = res_fit_sub, aes(x = year, y = AGBI.mid, color = "Observed"), alpha = 0.5) +
      geom_point(data = res_fit_sub, aes(x = year, y = fitted, color = "Fitted"), size = 0.8) +
      geom_line(data = res_fit_sub, aes(x = year, y = fitted, color = "Fitted"), alpha = 0.5) +
      geom_ribbon(data = res_fit_sub, aes(x = year, ymin = fitted_lo, ymax = fitted_hi, fill = "Fitted CI"), alpha = 0.5) +
      geom_point(data = forecast_sub, aes(x = year, y = AGBI.mid, color = "Observed"), size = 0.8) +
      geom_line(data = forecast_sub, aes(x = year, y = AGBI.mid, color = "Observed")) +
      geom_ribbon(data = forecast_sub, aes(x = year, ymin = forecast_lo, ymax = forecast_hi, fill = "Forecast CI"), alpha = 0.4) +
      geom_point(data = forecast_sub, aes(x = year, y = forecast_mean, color = "Forecast"), size = 0.8) +
      geom_line(data = forecast_sub, aes(x = year, y = forecast_mean, color = "Forecast")) +
      geom_vline(data = disturbance, aes(xintercept = year),
                 linetype = "dashed", color = "red") +
      facet_wrap(~ taxon, ncol = ncol_i, scales = "free_y") +
      scale_color_manual(name = "Type",
                         values = c("Observed" = "black", "Fitted" = "blue", "Forecast" = "orange")) +
      scale_fill_manual(name = "Ribbon",
                        values = c("Forecast CI" = "orange", "Fitted CI" = "lightblue")) +
      labs(x = "Year", y = "biomass increment (Mg/ha)") +
      ggtitle(site_i) +
      theme_light(base_size = 11)
    
    print(p)
  }, error = function(e) {
    message("  FAILED: ", conditionMessage(e))
  })
}

dev.off()



# plotting residuals ------------------------------------------------------

#correlation of residuals using ggpairs full dataset

while (!is.null(dev.list())) dev.off()
pdf("figures/ggpairs_residuals_ARIMA.pdf", width = 12, height = 12)

for (site_i in unique(joined_taxon_AGBI$site)) {
  
  site_data <- joined_taxon_AGBI %>%
    filter(site == site_i) %>%
    select(year, taxon, residuals) %>%
    pivot_wider(names_from = taxon, values_from = residuals) %>%
    select(-year)
  
  # drop taxa with too little data to correlate (e.g. all NA)
  keep <- colSums(!is.na(site_data)) >= 3
  site_data <- site_data[, keep, drop = FALSE]
  
  if (ncol(site_data) < 2) {
    message("Skipping ", site_i, ": fewer than 2 taxa with data")
    next
  }
  
  print(ggpairs(site_data, title = paste(site_i, "Correlations")))
}

dev.off()



# #correlation of residuals using ggpairs CUMSUM

while (!is.null(dev.list())) dev.off()
pdf("figures/ggpairs_residuals_ARIMA_cumsum.pdf", width = 12, height = 12)

for (site_i in unique(filtered_taxa_AGBI$site)) {
  
  site_data <- filtered_taxa_AGBI %>%
    filter(site == site_i) %>%
    select(year, taxon, residuals) %>%
    pivot_wider(names_from = taxon, values_from = residuals) %>%
    select(-year)
  
  # drop taxa with too few non-NA residuals to correlate
  keep <- colSums(!is.na(site_data)) >= 3
  site_data <- site_data[, keep, drop = FALSE]
  
  if (ncol(site_data) < 2) {
    message("Skipping ", site_i, ": fewer than 2 taxa with data")
    next
  }
  
  print(ggpairs(site_data, title = paste(site_i, "Residual correlations")))
}

dev.off()



#percent of observed values that fall within the CI
#fitted values will all fall within the CI
# percent_obs = filtered_AGBI %>% 
#   mutate(in.CI = ifelse(AGBI.mean >= fitted_lo & AGBI.mean <= fitted_hi, 1, 0))
# 
# foo = percent_obs %>% 
#   group_by(at1 %>% 
#     pivot_wider(names_from = numbers, values_from = value), taxon) %>% 
#   summarize( percent = sum(in.CI)/n())
# write.csv(foo, "fitted_percent_in.csv")


percent_forecast = filtered_forecast2 %>% 
  mutate(in.CI = ifelse(AGBI.mid >= forecast_lo & AGBI.mid <= forecast_hi, 1, 0))
percent_forecast_site = percent_forecast %>% 
  group_by(site) %>% 
  dplyr::summarize( percent = sum(in.CI)/n())
#write.csv(foo, "forecast_percent_in.csv")

percent_forecast_taxon = percent_forecast %>% 
  group_by(site, taxon) %>% 
  dplyr::summarize( percent = sum(in.CI)/n())

percent_forecast_taxon2 = percent_forecast %>% 
  group_by(taxon) %>% 
  dplyr::summarize( percent = sum(in.CI)/n())


res_wide_filter = filtered_AGBI %>% 
  mutate(site_taxa = paste0(site, "_", taxon)) %>% 
  subset(select = -c( fitted, fitted_lo, fitted_hi, 
                      AGBI.mid, cum_sum, site, taxon)) %>% 
  pivot_wider(names_from = site_taxa , values_from = residuals)


# model vs. data figures --------------------------------------------------

site = "GOOSE"
taxa = "PIST"

#observed vs. model AGBI facet_wrapped
library(dplyr)
library(ggplot2)

while (!is.null(dev.list())) dev.off()
pdf("figures/fitted_vs_AGBI_by_taxon.pdf", width = 14, height = 10)

for (site_i in unique(joined_taxon_AGBI$site)) {
  
  d <- joined_taxon_AGBI %>%
    filter(site == site_i, !is.na(fitted), !is.na(AGBI.mid))
  
  if (nrow(d) == 0) {
    message("Skipping ", site_i, ": no non-NA fitted/AGBI.mid pairs")
    next
  }
  
  p <- ggplot(d, aes(x = AGBI.mid, y = fitted)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    geom_errorbar(aes(ymin = fitted_lo, ymax = fitted_hi),
                  width = 0.01, color = "gray40") +
    geom_point(alpha = 0.7) +
    facet_wrap(~ taxon, scales = "free") +
    labs(x = "Observed AGBI.mid (Mg/ha)",
         y = "Fitted biomass increment (Mg/ha)",
         title = site_i) +
    theme_light(base_size = 11)
  
  print(p)
}

dev.off()



#observed vs.model with disturbance 
library(dplyr)
library(ggplot2)

# one row per site/taxon/disturbance year, flagged for joining
disturb_flag <- taxon_disturb_years %>%
  distinct(site, taxon, year) %>%
  mutate(disturbance = "Disturbance year")

while (!is.null(dev.list())) dev.off()
pdf("figures/fitted_vs_AGBI_by_taxon_disturb.pdf", width = 14, height = 10)

for (site_i in unique(joined_taxon_AGBI$site)) {
  
  d <- joined_taxon_AGBI %>%
    filter(site == site_i, !is.na(fitted), !is.na(AGBI.mid)) %>%
    left_join(disturb_flag, by = c("site", "taxon", "year")) %>%
    mutate(disturbance = ifelse(is.na(disturbance), "No disturbance", disturbance))
  
  if (nrow(d) == 0) {
    message("Skipping ", site_i, ": no non-NA fitted/AGBI.mid pairs")
    next
  }
  
  p <- ggplot(d, aes(x = AGBI.mid, y = fitted)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    geom_errorbar(aes(ymin = fitted_lo, ymax = fitted_hi),
                  width = 0.01, color = "gray40") +
    geom_point(aes(color = disturbance), alpha = 0.8) +
    scale_color_manual(name = NULL,
                       values = c("No disturbance" = "black",
                                  "Disturbance year" = "green3")) +
    facet_wrap(~ taxon, scales = "free") +
    labs(x = "Observed AGBI.mid (Mg/ha)",
         y = "Fitted biomass increment (Mg/ha)",
         title = site_i) +
    theme_light(base_size = 11)
  
  print(p)
}

dev.off()



















#plotting fitted/forecast vs observed with CI at each point 
#cumsum data with forecast!!
pdf("reboot/figures/AGBI_fitted_vs_observed_filtered.pdf", width = 10, height = 8)

for (site in sites) {
  for (taxon in taxa) {
    print(paste(site, taxon))
    
    dist_years <- disturbance_years[[site]] 
    
    clim_agbi_sub <- filtered_AGBI %>%
      filter(site == !!site, taxon == !!taxon)
    
    clim_forecast_sub <- filtered_forecast2 %>%
      filter(site == !!site, taxon == !!taxon)
    
    if (nrow(clim_agbi_sub) == 0 ||
        all(is.na(clim_agbi_sub$AGBI.mid)) ||
        all(is.na(clim_agbi_sub$fitted))) next
    
    p <- ggplot() +
      
      # disturbance points (all disturbance + post years)
      geom_point(
        data = filter(clim_agbi_sub, disturb_year == 1),
        aes(x = AGBI.mid, y = fitted),
        color = "orange", size = 3
      ) +
      
      # year = disturbance year + 1
      geom_point(
        data = filter(
          clim_agbi_sub,
          year %in% (dist_years + 1)
        ),
        aes(x = AGBI.mid, y = fitted),
        color = "green", size = 4
      ) +
      
      # error bars for disturbance
      geom_errorbar(
        data = filter(clim_agbi_sub, disturb_year == 1),
        aes(x = AGBI.mid, ymin = fitted_lo, ymax = fitted_hi),
        width = 0.01, color = "orange"
      ) +
      
      # forecast points + CI
      geom_point(
        data = clim_forecast_sub,
        aes(x = AGBI.mid, y = forecast_mean),
        color = "blue"
      ) +
      
      geom_errorbar(
        data = clim_forecast_sub,
        aes(x = AGBI.mid, ymin = forecast_lo, ymax = forecast_hi),
        width = 0.01, color = "blue"
      ) +
      
      geom_abline(intercept = 0, slope = 1,
                  color = "red", linetype = "dashed") +
      
      labs(
        x = "Observed AGBI.mid",
        y = "Fitted AGBI",
        title = paste0(site, "; ", taxon)
      ) +
      theme_light(base_size = 14)
    
    print(p)
  }
}

dev.off()



#plotting fitted/forecast vs observed with CI at each point 
#cumsum data with forecast!!, facet wrap by taxon
pdf("reboot/figures/AGBI_fitted_vs_observed_cumsum_facetwrap.pdf",
    width = 12, height = 8)

for (site in sites) {
  message("Processing site: ", site)
  
  # disturbance year(s) for this site
  dist_years <- disturbance_years[[site]]
  post_years <- dist_years + 1
  
  clim_agbi_sub <- filtered_AGBI %>%
    filter(
      site == !!site,
      disturb_year == 1 | year %in% post_years
    )
  
  clim_forecast_sub <- filtered_forecast2 %>%
    filter(site == !!site)
  
  # Skip empty site
  if (nrow(clim_agbi_sub) == 0 ||
      all(is.na(clim_agbi_sub$AGBI.mid)) ||
      all(is.na(clim_agbi_sub$fitted))) next
  
  p <- ggplot() +
    
    # disturbance points
    geom_point(
      data = filter(clim_agbi_sub, disturb_year == 1),
      aes(x = AGBI.mid, y = fitted),
      color = "orange", size = 3
    ) +
    
    # +1 year post-disturbance
    geom_point(
      data = filter(clim_agbi_sub, year %in% post_years),
      aes(x = AGBI.mid, y = fitted),
      color = "green", size = 4
    ) +
    
    # error bars (disturbance only)
    geom_errorbar(
      data = filter(clim_agbi_sub, disturb_year == 1),
      aes(x = AGBI.mid, ymin = fitted_lo, ymax = fitted_hi),
      width = 0.01, color = "orange"
    ) +
    
    # forecast
    geom_point(
      data = clim_forecast_sub,
      aes(x = AGBI.mid, y = forecast_mean),
      color = "blue"
    ) +
    
    geom_errorbar(
      data = clim_forecast_sub,
      aes(x = AGBI.mid, ymin = forecast_lo, ymax = forecast_hi),
      width = 0.01, color = "blue"
    ) +
    
    geom_abline(
      intercept = 0, slope = 1,
      color = "red", linetype = "dashed"
    ) +
    
    facet_wrap(~taxon, scales = "free") +
    labs(
      x = "Observed AGBI.mid",
      y = "Fitted AGBI",
      title = paste0("Site: ", site)
    ) +
    theme_light(base_size = 14)
  
  print(p)
}

dev.off()

pdf("reboot/figures/AGBI_fitted_vs_observed_cumsum_facetwrap_disturb.pdf",
    width = 12, height = 8)

for (site in sites) {
  message("Processing site: ", site)
  
  site_df <- fit_res_joined %>%
    filter(
      site == !!site,
      !is.na(disturb_year),
      disturb_year >= 0,
      disturb_year <= 5
    )
  
  # skip empty sites
  if (nrow(site_df) == 0) next
  
  p <- ggplot(
    site_df,
    aes(
      x = AGBI.mid,
      y = fitted,
      color = factor(disturb_year)
    )
  ) +
    
    geom_point(size = 3, alpha = 0.8) +
    
    geom_errorbar(
      aes(ymin = fitted_lo, ymax = fitted_hi),
      width = 0.01,
      alpha = 0.8
    ) +
    
    geom_abline(
      intercept = 0, slope = 1,
      color = "black", linetype = "dashed"
    ) +
    
    facet_wrap(~taxon, scales = "free") +
    
    scale_color_viridis_d(
      name = "Years since\ndisturbance",
      option = "C",
      direction = 1
    ) +
    
    labs(
      title = paste0("Observed vs Fitted AGBI (Disturbance window)\nSite: ", site),
      x = "Observed AGBI.mid",
      y = "Fitted AGBI"
    ) +
    
    theme_light(base_size = 14) +
    theme(
      legend.position = "right",
      strip.text = element_text(face = "bold")
    )
  
  print(p)
}

dev.off()



pdf("reboot/figures/AGBI_fitted_vs_observed_cumsum_facetwrap_disturbance.pdf",
    width = 12, height = 8)

for (site in sites) {
  message("Processing site: ", site)
  
  # disturbance year(s) for this site
  dist_years <- disturbance_years[[site]]
  post_years <- dist_years + 1
  
  clim_agbi_sub <- filtered_AGBI %>%
    filter(
      site == !!site,
      disturb_year == 1 | year %in% post_years
    )
  
  clim_forecast_sub <- filtered_forecast2 %>%
    filter(site == !!site)
  
  # Skip empty site
  if (nrow(clim_agbi_sub) == 0 ||
      all(is.na(clim_agbi_sub$AGBI.mid)) ||
      all(is.na(clim_agbi_sub$fitted))) next
  
  p <- ggplot() +
    
    # disturbance points
    geom_point(
      data = filter(clim_agbi_sub, disturb_year == 1),
      aes(x = AGBI.mid, y = fitted, color = year),
      , size = 3
    ) +
    
    
    
    # error bars (disturbance only)
    geom_errorbar(
      data = filter(clim_agbi_sub, disturb_year == 1),
      aes(x = AGBI.mid, ymin = fitted_lo, ymax = fitted_hi),
      width = 0.01, color = "orange"
    ) +
    
    
    geom_abline(
      intercept = 0, slope = 1,
      color = "red", linetype = "dashed"
    ) +
    
    facet_wrap(~taxon, scales = "free") +
    labs(
      x = "Observed AGBI.mid",
      y = "Fitted AGBI",
      title = paste0("Site: ", site)
    ) +
    theme_light(base_size = 14)
  
  print(p)
}

dev.off()



# coefficients (do we need this) ------------------------------------------------------------


fit_coefs_long <- pmap_dfr(
  list(model_forecasts$site, model_forecasts$taxon, model_forecasts$mod),
  function(site_val, taxon_val, mod_obj) {
    if (is.null(mod_obj)) return(NULL)
    
    coefs <- coef(mod_obj)
    vcov_mat <- tryCatch(mod_obj$var.coef, error = function(e) NULL)
    
    if (is.null(vcov_mat)) {
      se <- rep(NA, length(coefs))
    } else {
      se <- sqrt(diag(vcov_mat))
    }
    
    tibble(
      site = site_val,
      taxon = taxon_val,
      coef_name = names(coefs),
      coef_value = as.numeric(coefs),
      se = se,
      lower95 = coef_value - 1.96 * se,
      upper95 = coef_value + 1.96 * se
    )
  }
)


#pulling coefficients from model
fit_coefs_long <- fit_coefs_long %>%
  left_join(
    agbi_cumsum_filter %>%
      select(site, taxon) %>%
      mutate(in_top95 = TRUE),
    by = c("site", "taxon")
  ) %>%
  mutate(in_top95 = if_else(is.na(in_top95), FALSE, in_top95))

# Collapse coefficient names into groups
fit_coefs_long <- fit_coefs_long %>%
  mutate(clim_var = case_when(
    str_detect(coef_name, regex("ppt", ignore_case = TRUE)) ~ "PPT",
    str_detect(coef_name, regex("tmin", ignore_case = TRUE)) ~ "Tmin",
    str_detect(coef_name, regex("tmax", ignore_case = TRUE)) ~ "Tmax",
    str_detect(coef_name, regex("tmean", ignore_case = TRUE)) ~ "Tmean",
    str_detect(coef_name, regex("vpd", ignore_case = TRUE)) ~ "Vpdmax",
    TRUE ~ "Other"
  ))


#joining clim variable values with coef of the predictors to create table 
coefs_climvar = clim_agbi_long2 %>%
  left_join(fit_coefs_long, by = c("site", "taxon", "coef_name"))

#dataframe where we multiply coef value * matching predictor value 
multiply_table = coefs_climvar %>% 
  group_by(year, coef_name, climvar_value, coef_value ) %>% 
  dplyr::mutate(value = climvar_value * coef_value)

ggplot() +
  geom_point(data = multiply_table %>% filter(site == "ROOSTER",  
  )  , aes(x= year, y = value, colour = taxon))+
  facet_wrap(~ coef_name)


# # Plotting coefficients for each site
# pdf("reboot/figures/predictor_coefficients.pdf", width = 12, height = 8)
# 
# for (site in sites) {
#   
#   p <- ggplot(
#     data = filter(fit_coefs_long, 
#                   site == !!site &
#                     !coef_name %in% c("ar1", "intercept")),
#     aes(x = coef_name, y = coef_value, color = taxon, shape = in_top95)
#   ) +
#     geom_point(size = 3, alpha = 0.7) +
#     theme_light(base_size = 14) +
#     theme(axis.text.x = element_text(angle = -90, hjust = 0)) +
#     labs(title = paste("Predictor Coefficients - Site:", site),
#          x = "Coefficient",
#          y = "Value",
#          color = "Taxon",
#          shape = "Dominant taxa") +
#     scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1)) +
#     facet_wrap(~var_group, scales = "free")   # 👈 each facet free scales
#   
#   print(p)
# }
# 
# dev.off()
# 
# 
# 
# #coefficients with error bars
# pdf("report/figures2/predictor_coefficients_errorbars.pdf", width = 10, height = 8)
# 
# for (site in sites) {
#   
#   for (var in c("PPT", "Temperature", "VPD")) {  
#     p <- ggplot(
#       data = filter(fit_coefs_long, 
#                     site == !!site & var_group == !!var &
#                       !coef_name %in% c("ar1", "intercept")),
#       aes(x = coef_name, y = coef_value, color = taxon, shape = in_top95)
#     ) +
#       geom_point(size = 3, alpha = 0.7) +
#       geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.2, alpha = 0.3) +
#       theme_light(base_size = 14) +
#       theme(axis.text.x = element_text(angle = -90, hjust = 0)) +
#       labs(title = paste("Predictor Coefficients with 95% CI -", site, "-", var),
#            x = "Coefficient",
#            y = "Value",
#            color = "Taxon",
#            shape = "Dominant taxa") +
#       scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1))
#     
#     print(p)
#   }
# }
# 
# dev.off()