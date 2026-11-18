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

rm(list = ls())

# COMBINING AGBI AND CLIM DF ----------------------------------------------

#function to offset prev years
tree_yr <- function(df, start_month=12) {
  # Year offset
  offset = ifelse(as.numeric(df$month) >= start_month,  1, 0)
  # Water year
  adj.year = as.numeric(df$year) + offset
  # Return the water year
  adj.year
}
#################loading biomass and climate data##########

#dataframe with model iteration, year, site, taxon and AGBI
AGBI_taxon_iter = readRDS("reboot/AGBI_taxon_iter.RDS")

head(AGBI_taxon_iter)

#aggregating to site level from taxon data
AGBI_taxon_site_iter = AGBI_taxon_iter %>% group_by(year, model, site, iter_new) %>% 
  dplyr::summarize(AGB = sum(AGB, na.rm=TRUE), AGBI=sum(AGBI, na.rm=TRUE))

#plotitng AGBI.mid over time (data from increment model)
#full data 1950-2011
AGBI_taxon_site_iter |>
  dplyr::summarize(low = min(AGBI),
                   mid = median(AGBI),
                   high = max(AGBI)) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = year, y = mid, color = site)) +
  ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = low, ymax = high,
                                    fill = site), alpha = 0.2) +
  ggplot2::facet_wrap(~site) +
  ggplot2::theme_light() +
  ggplot2::labs(x = 'Year', y = 'Biomass increment (Mg/ha/y)')


# climate data ------------------------------------------------------------
#climate data in long format 
load('climate/prism_clim.RData')
clim_data = prism_long

#removing Vpdmin from dataset
df = subset(clim_data, select = -c(Vpdmin2) )

#renaming columns in climate dataframe
clim_data = df %>% 
  dplyr::rename(site = loc, PPT = PPT2, Tmean = Tmean2, Tmin = Tmin2, Tmax = Tmax2, 
                Vpdmax = Vpdmax2)
clim_data$year <- as.numeric(clim_data$year)


#list of climate predictors
predictor_names = c('PPT_winter', 'PPT_spring', 'PPT_summer', 'PPT_fall',
                    'Vpdmax_winter','Vpdmax_spring', 'Vpdmax_summer', 'Vpdmax_fall',
                    'Tmean_winter', 'Tmean_spring', 'Tmean_summer', 'Tmean_fall',
                    'Tmin_winter', 'Tmin_spring', 'Tmin_summer', 'Tmin_fall',
                    'Tmax_winter', 'Tmax_spring', 'Tmax_summer', 'Tmax_fall') 

######## reorganizing climate data to add tree_year #############

#using tree_yr function to make prev and current year 
#ex. year=1986 is sept-dec of 1985 and jan-aug of 1986
clim_data$year_tree = tree_yr(clim_data, start_month=9)
#current clim data jan-sept
clim_data_current = clim_data[which(clim_data$year == clim_data$year_tree),]
#prev climate data, prev sept-dec
clim_data_prev = clim_data[which(clim_data$year != clim_data$year_tree),]


#putting the climate variables in wide format
clim_wide_current =  pivot_wider(data = clim_data_current,
                                 names_from = month, 
                                 values_from = c(PPT, Tmean, Tmin, Tmax, Vpdmax))

#putting the climate variables in wide format
clim_wide_prev =  pivot_wider(data = subset(clim_data_prev, select = -year),
                              names_from = month, 
                              values_from = c(PPT, Tmean, Tmin, Tmax, Vpdmax))
#renaming year_tree to year 
#year_tree is clim data from the previous year 
clim_wide_prev = rename(clim_wide_prev, year = year_tree)



#merging by year which is the prev sept- current august
#year_tree in this df is the year_tree from current df which makes it =year 
clim_wide = merge(clim_wide_current, clim_wide_prev, by = c('site', 'year'))

saveRDS(clim_wide, 'reboot/clim.RDS')

clim_seasons_wide <- clim_wide |>
  dplyr::group_by(year, site) |>
  dplyr::mutate(PPT_winter = sum(dplyr::pick('PPT_12', 'PPT_01', 'PPT_02')),
                PPT_spring = sum(dplyr::pick('PPT_03', 'PPT_04', 'PPT_05')),
                PPT_summer = sum(dplyr::pick('PPT_06', 'PPT_07', 'PPT_08')),
                PPT_fall = sum(dplyr::pick('PPT_09', 'PPT_10', 'PPT_11')),
                Vpdmax_winter = rowMeans(dplyr::pick('Vpdmax_12', 'Vpdmax_01','Vpdmax_02')),
                Vpdmax_spring = rowMeans(dplyr::pick('Vpdmax_03', 'Vpdmax_04','Vpdmax_05')),
                Vpdmax_summer = rowMeans(dplyr::pick('Vpdmax_06', 'Vpdmax_07','Vpdmax_08')),
                Vpdmax_fall = rowMeans(dplyr::pick('Vpdmax_09', 'Vpdmax_10','Vpdmax_11')),
                Tmin_winter = rowMeans(dplyr::pick('Tmin_12', 'Tmin_01', 'Tmin_02')),
                Tmin_spring = rowMeans(dplyr::pick('Tmin_03', 'Tmin_04', 'Tmin_05')),
                Tmin_summer = rowMeans(dplyr::pick('Tmin_06', 'Tmin_07', 'Tmin_08')),
                Tmin_fall = rowMeans(dplyr::pick('Tmin_09', 'Tmin_10', 'Tmin_11')),
                Tmax_winter = rowMeans(dplyr::pick('Tmax_12', 'Tmax_01', 'Tmax_02')),
                Tmax_spring = rowMeans(dplyr::pick('Tmax_03', 'Tmax_04', 'Tmax_05')),
                Tmax_summer = rowMeans(dplyr::pick('Tmax_06', 'Tmax_07', 'Tmax_08')),
                Tmax_fall = rowMeans(dplyr::pick('Tmax_09', 'Tmax_10', 'Tmax_11')),
                Tmean_winter = rowMeans(dplyr::pick('Tmean_12', 'Tmean_01', 'Tmean_02')),
                Tmean_spring = rowMeans(dplyr::pick('Tmean_03', 'Tmean_04', 'Tmean_05')),
                Tmean_summer = rowMeans(dplyr::pick('Tmean_06', 'Tmean_07', 'Tmean_08')),
                Tmean_fall = rowMeans(dplyr::pick('Tmean_09', 'Tmean_10', 'Tmean_11'))
)

#  merge climate data: TAXON ----------------------------------------------

#wide format of climate variables with AGBI by tree_year
#taxon level
clim_agbi_taxon_iter <- AGBI_taxon_iter %>% 
  left_join(clim_wide, by = c('year', 'site'))

saveRDS(clim_agbi_taxon_iter, "reboot/clim_taxon_iter.RDS")


#new dataframe with seasonal climate data
#Across seasons, sum PPT, mean Tmean, max Tmax, min Tmin
clim_seasons_taxon_iter <- AGBI_taxon_iter |>
  dplyr::left_join(y = clim_seasons_wide, by = c('year', 'site'))

saveRDS(clim_seasons_taxon_iter, "reboot/clim_seasons_taxon_iter.RDS")

#seasonal clim variables in long format 
#one column for each varible_season combination
#ex. PPT_winter
clim_agbi_long2_taxon_iter <- clim_seasons_taxon_iter %>%
  select(-matches("\\d+$"), -c(year_tree)) %>%  #dropping monthly clim variables
  pivot_longer(
    cols = all_of(predictor_names),
    names_to = "coef_name",
    values_to = "climvar_value")

saveRDS(clim_agbi_long2_taxon_iter, "reboot/clim_seasons_taxon_long_iter.RDS")

# merge climate data: SITE ------------------------------------------------

#wide format of climate variables with AGBI by tree_year
clim_agbi_site_iter <- AGBI_taxon_site_iter %>% 
  left_join(clim_wide, by = c('year', 'site'))

saveRDS(clim_agbi_site_iter, "reboot/clim_site_iter.RDS")

#new dataframe with seasonal climate data
#Across seasons, sum PPT, mean Tmean, max Tmax, min Tmin
clim_seasons_site_iter = AGBI_taxon_site_iter |> 
  dplyr::left_join(y = clim_seasons_wide, by = c('year', 'site'))

saveRDS(clim_seasons_site_iter, "reboot/clim_seasons_site_iter.RDS")

#seasonal clim variables in long format 
#one column for each varible_season combination
#ex. PPT_winter
clim_agbi_long2_site_iter <- clim_seasons_site_iter %>%
  select(-matches("\\d+$"), -c(year_tree)) %>%  #dropping monthly clim variables
  pivot_longer(
    cols = all_of(predictor_names),
    names_to = "coef_name",
    values_to = "climvar_value")

saveRDS(clim_agbi_long2_site_iter, "reboot/clim_seasons_site_long_iter.RDS")

# ARIMA taxon Model -------------------------------------------------------------

### ADD ARIMA

#setting up for forecast values
#Remove last five years
clim_agbi_in_taxon_iter <- clim_seasons_taxon_iter %>%
  filter(year < 2007) %>%
  select(-matches("_[0-9]+$"))   # drop columns ending with _ + one or more digits

clim_agbi_out_taxon_iter <- clim_seasons_taxon_iter %>%
  filter(year > 2006) %>%
  select(-matches("_[0-9]+$"))   # same for forecast period


#uses seasonal climate data 
#making a time series 
# Fit ARIMA models by site, taxon, and iteration
## NOTE: TAKES ~15 MINUTES
models_taxon_iter <- clim_agbi_in_taxon_iter %>%
  tidyr::drop_na(AGBI) |>
  dplyr::group_by(site, taxon, iter_new) %>%
  dplyr::do({
    df <- .
    # Response variable as time series
    agbi_ts <- ts(df$AGBI, start = min(df$year), frequency = 1)
    
    # External regressors
    xreg <- as.matrix(df %>% dplyr::select(dplyr::all_of(predictor_names)))
    
    # Fit ARIMA model
    model <- tryCatch({
      forecast::Arima(
        y = agbi_ts,
        order = c(1, 0, 0),  # AR(1)
        xreg = xreg,
        method = "ML"
      )
    }, error = function(e) {
      warning("ARIMA failed for site=", df$site[1], ", taxon=", df$taxon[1], ": ", e$message)
      return(NULL)
    })
    
    tibble(mod = list(model), note = if (is.null(model)) "Model failed" else NA)
  })

#adding column for naming
#ex. GOOSE_ACRU
models_taxon_iter <- dplyr::mutate(models_taxon_iter, model = paste0(site, "_", taxon))

# ARIMA site Model -------------------------------------------------------------

### ADD ARIMA

#setting up for forecast values
#Remove last five years
clim_agbi_in_site_iter <- clim_seasons_site_iter %>%
  filter(year < 2007) %>%
  select(-matches("_[0-9]+$"))   # drop columns ending with _ + one or more digits

clim_agbi_out_site_iter <- clim_seasons_site_iter %>%
  filter(year > 2006) %>%
  select(-matches("_[0-9]+$"))   # same for forecast period


#uses seasonal climate data 
#making a time series 
# Fit ARIMA models by site
models_site_iter <- clim_agbi_in_site_iter %>%
  tidyr::drop_na(AGBI) |>
  group_by(site, iter_new) %>%
  do({
    df <- .
    
    # Response variable as time series
    agbi_ts <- ts(df$AGBI, start = min(df$year), frequency = 1)
    
    # External regressors
    xreg <- as.matrix(df %>% select(all_of(predictor_names)))
    
    # Fit ARIMA model
    model <- tryCatch({
      forecast::Arima(
        y = agbi_ts,
        order = c(1, 0, 0),  # AR(1)
        xreg = xreg,
        method = "ML"
      )
    }, error = function(e) {
      warning("ARIMA failed for site=", df$site[1], ", taxon=", df$taxon[1], ": ", e$message)
      return(NULL)
    })
    
    tibble(mod = list(model), note = if (is.null(model)) "Model failed" else NA)
  })


#adding column for naming
#ex. GOOSE_ACRU
models_site_iter <- dplyr::mutate(models_site_iter, model = site)

########### forecasting taxon model #################################

# Build each group's xreg matrix once, keyed by site|taxon|iter_new
xreg_lookup <- clim_agbi_out_taxon_iter %>%
  dplyr::ungroup() %>%
  dplyr::arrange(site, taxon, iter_new, year) %>%
  dplyr::mutate(key = paste(site, taxon, iter_new, sep = "|"))
xreg_lookup <- lapply(split(xreg_lookup[predictor_names], xreg_lookup$key), as.matrix)

#forecasting
fcast_taxon_iter <- models_taxon_iter %>%
  dplyr::ungroup() %>%
  dplyr::filter(!purrr::map_lgl(mod, is.null)) %>%
  dplyr::mutate(
    forecast = purrr::pmap(
      list(mod, site, taxon, iter_new),
      function(model_obj, site_val, taxon_val, iter_val) {
        future_xreg <- xreg_lookup[[paste(site_val, taxon_val, iter_val, sep = "|")]]
        if (is.null(future_xreg)) return(NA)
        tryCatch(forecast::forecast(model_obj, xreg = future_xreg), error = function(e) NA)
      }
    )
  )

# Check object
fcast_taxon_iter

#pulling forcast values into a dataframe of lists with forecast mean,lo,hi
fcast_long_taxon_iter <- fcast_taxon_iter %>%
  dplyr::filter(purrr::map_lgl(forecast, ~ inherits(.x, "forecast"))) %>%
  dplyr::mutate(values = purrr::map(forecast, ~ dplyr::tibble(
    year = time(.x$mean),
    forecast_mean = as.numeric(.x$mean),
    forecast_low  = as.numeric(.x$lower[, 2]),
    forecast_high = as.numeric(.x$upper[, 2])
  ))) %>%
  dplyr::select(site, taxon, iter_new, values) %>%
  tidyr::unnest(values)

#adding AGBI column
fcast_long_taxon2_iter = left_join(
  fcast_long_taxon_iter,
  select(AGBI_taxon_iter, year, site, taxon, iter_new, AGBI),
  by = c("site", "year", "taxon", 'iter_new')
)

saveRDS(fcast_long_taxon2_iter, "reboot/forecast_taxon_long_iter.RDS")

# extracting taxon model data ---------------------------------------------

#residuals and fitted values in long format
#pulling fitted, and residuals from the forecast model in a dataframe for the taxon model
fitted_res_taxon_iter <- fcast_taxon_iter %>%
  mutate(sigma2 = purrr::map_dbl(mod, ~ .x$sigma2)) %>%
  select(forecast, site, taxon, iter_new, sigma2) %>%
  pmap_dfr(function(forecast, site, taxon, iter_new, sigma2) {
    
    if (is.null(forecast)) return(NULL)
    
    tibble(
      site      = site,
      taxon     = taxon,
      year      = as.vector(time(forecast$fitted)),
      iter_new = iter_new,
      fitted    = as.numeric(forecast$fitted),
      residuals = as.numeric(forecast$residuals),
      sigma2    = sigma2
    )
  })

# forecasting site level --------------------------------------------------

xreg_lookup_site <- clim_agbi_out_site_iter |>
  dplyr::ungroup() |>
  dplyr::arrange(site, iter_new, year) |>
  dplyr::mutate(key = paste(site, iter_new, sep = '|'))
xreg_lookup_site <- lapply(split(xreg_lookup_site[predictor_names], xreg_lookup_site$key), as.matrix)

fcast_site_iter <- models_site_iter |>
  dplyr::ungroup() |>
  dplyr::filter(!purrr::map_lgl(mod, is.null)) |>
  dplyr::mutate(
    forecast = purrr::pmap(
      list(mod, site, iter_new),
      function(model_obj, site_val, iter_val) {
        future_xreg <- xreg_lookup_site[[paste(site_val, iter_val, sep = '|')]]
        if(is.null(future_xreg)) return(NA)
        tryCatch(forecast::forecast(model_obj, xreg = future_xreg), error = function(e) NA)
      }
    )
  )

#checking object
fcast_site_iter

# Pulling forecast values into a dataframe of lists
# with forecast mean, lower, and upper 95% CI
fcast_long_site_iter <- fcast_site_iter %>%
  dplyr::filter(purrr::map_lgl(forecast, ~ inherits(.x, "forecast"))) %>%
  dplyr::mutate(values = purrr::map(forecast, ~ dplyr::tibble(
    year = time(.x$mean),
    forecast_mean = as.numeric(.x$mean),
    forecast_low  = as.numeric(.x$lower[, 2]),
    forecast_high = as.numeric(.x$upper[, 2])
  ))) %>%
  dplyr::select(site, iter_new, values) %>%
  tidyr::unnest(values)

#adding AGBI column
fcast_long_site2_iter = left_join(
  fcast_long_site_iter,
  select(AGBI_taxon_site_iter, year, site, iter_new, AGBI),
  by = c("site", "year", 'iter_new')
)

saveRDS(fcast_long_site2_iter, "reboot/forecast_site_long_iter.RDS")

# extracting site model data ----------------------------------------------

fitted_res_site_iter <- models_site_iter %>%
  mutate(sigma2 = purrr::map_dbl(mod, ~ .x$sigma2)) %>%
  select(site, iter_new, mod, sigma2) %>%
  pmap_dfr(function(site, mod, iter_new, sigma2) {
    
    if (is.null(mod)) return(NULL)
    
    tibble(
      site      = site,
      iter_new = iter_new,
      year      = as.vector(time(mod$fitted)),
      site_fitted    = as.numeric(mod$fitted),
      residuals = as.numeric(mod$residuals),
      sigma2    = sigma2
    )
  })

# summing taxon AGBI from ARIMA model to get total site AGBI --------

#summing AGBI at a given site to plot total fitted AGBI from the taxon model
#only goes up to year 2006 since we used model data
arima_taxon_2_site_iter = fitted_res_taxon_iter %>%
  group_by(year, site, iter_new) %>%
  dplyr::summarise(taxon_2site_fitted = sum(fitted, na.rm=TRUE))

# joining data with fitted taxa -------------------------------------------

# #joining observed AGBI with taxon model AGBI 
# #DATA AGBI from AGB.data
# #goes up to 2006
joined_AGBI_taxon_iter = inner_join(fitted_res_taxon_iter, select(AGBI_taxon_iter, c(year, taxon, site, iter_new, AGBI)), 
                               by = c("site", "taxon", "year", 'iter_new'))

saveRDS(joined_AGBI_taxon_iter, "reboot/joined_taxon_AGBIs_iter.RDS")

# joining data with fitted site data across all types ----------------------------------

# #joining observed AGBI with sum taxon model AGBI and site model AGBI
# #DATA AGBI from AGB.data
# #goes up to 2006
joined_site_AGBIs_iter = left_join(
  arima_taxon_2_site_iter,
  select(fitted_res_site_iter, year, site, iter_new, site_fitted),
  by = c("site", "year", 'iter_new')
) %>%
  inner_join(
    select(AGBI_taxon_site_iter, year, site, iter_new, AGBI),
    by = c("site", "year", 'iter_new')
  )

saveRDS(joined_site_AGBIs_iter, "reboot/joined_site_AGBIs_iter.RDS")
