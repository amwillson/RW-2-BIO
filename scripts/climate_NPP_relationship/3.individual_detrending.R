## Detrending for individual trees

rm(list = ls())

# Load total increment
goose_tree_agbi <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGBI_STAN_GOOSE_v2.0_012021.RDS')
nrp_tree_agbi <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/AGBI_STAN_NORTHROUND_v2.0_082020.RDS')
rooster_tree_agbi <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/AGBI_STAN_ROOSTER_v2.0_082020.RDS')
sylv_tree_agbi <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/AGBI_STAN_SYLVANIA_v2.0_082020.RDS')

# Subset for 1960 and beyond to reduce problem of fading record
goose_tree_agbi <- goose_tree_agbi |>
  dplyr::mutate(site = 'GOOSE') |>
  dplyr::filter(year > 1959)
nrp_tree_agbi <- nrp_tree_agbi |>
  dplyr::mutate(site = 'NRP') |>
  dplyr::filter(year > 1959)
rooster_tree_agbi <- rooster_tree_agbi |>
  dplyr::mutate(site = 'ROOSTER') |>
  dplyr::filter(year > 1959)
sylv_tree_agbi <- sylv_tree_agbi |>
  dplyr::mutate(site = 'SYLVANIA') |>
  dplyr::filter(year > 1959)

# Combine sites
tree_agbi <- rbind(goose_tree_agbi, nrp_tree_agbi,
                   rooster_tree_agbi, sylv_tree_agbi)

# Save mean over iterations in dataframe
tree_agbi <- tree_agbi |>
  dplyr::group_by(tree, year, plot, taxon, site) |>
  dplyr::summarize(mean = mean(value))

# Conduct box tests for each tree over time
box_test <- tree_agbi |>
  dplyr::group_by(tree, plot, taxon, site) |>
  dplyr::summarize(box_test = Box.test(mean) |> broom::tidy())
# Proportion of trees demonstrating significant temporal autocorrelation
length(which(box_test$box_test$p.value < 0.05)) / nrow(box_test)

site <- c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA')

# Loop over each site and tree
for(i in 1:4){
  print(paste0('---------------',i,'------------------'))
  tree <- unique(tree_agbi$tree[which(tree_agbi$site == site[i])])
  site_name <- site[i]
  for(j in tree){
    sub <- dplyr::filter(tree_agbi, site == site_name &
                           tree == j)
    current_step <- ts(sub$mean, start = min(sub$year), end = max(sub$year),
                       frequency = 1)
    last_step <- lag(current_step, k = -1)
    
    combined <- as.data.frame(cbind(current_step, last_step))

    if(nrow(combined) <= 2){
      temp <- cbind(0,
                    site_name,
                    j,
                   (min(sub$year)-1):max(sub$year))
    }else{
      fit <- lm(formula = current_step ~ last_step, data = combined)
      
      temp <- cbind(residuals(fit), 
                    rep(site_name, times = length(residuals(fit))),
                    rep(j, times = length(residuals(fit))),
                    (min(sub$year)+1):max(sub$year))
    }
    if(i == 1 & j == 1){
      save <- temp
    }else{
      save <- rbind(save, temp)
    }
    print(j)
  }
}

save <- as.data.frame(save)
colnames(save) <- c('residual_AGBI', 'site', 'tree', 'year')
save$residual_AGBI <- as.numeric(save$residual_AGBI)
save$tree <- as.numeric(save$tree)
save$year <- as.numeric(save$year)

save_comb <- tree_agbi |>
  dplyr::left_join(y = save, by = c('site', 'tree', 'year')) |>
  tidyr::drop_na()

# Conduct box tests for each tree over time
box_test <- save_comb |>
  dplyr::group_by(tree, plot, taxon, site) |>
  dplyr::summarize(box_test = Box.test(residual_AGBI) |> broom::tidy())
# Proportion of trees demonstrating significant temporal autocorrelation
length(which(box_test$box_test$p.value < 0.05)) / nrow(box_test)

save(save_comb, file = 'out/tree_detrended_AGBI.RData')
