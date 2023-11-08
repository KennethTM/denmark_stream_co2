source("libs_and_funcs.R")

#Figures for manuscript

#Figure 1
q_points_modeling <- read_parquet("data/q_points_modeling.parquet")
dk_border <- st_read("data/dk_border.sqlite")
network <- st_read("data/dk_model/dk_model_hip_2020.shp")

q_points_sf <- q_points_modeling |> 
  st_as_sf(coords=c("q_point_x", "q_point_y"), crs=dk_epsg) |> 
  group_by(index) |> 
  slice(1)

fig_1_a <- ggplot()+
  geom_sf(data=dk_border, fill=NA, col="black") +
  geom_sf(data=network, col="dodgerblue", linewidth=0.25) +
  geom_sf(data=q_points_sf, shape=1, size=1)

fig_1_b <- q_points_modeling |> 
  mutate(Season = str_to_title(season)) |> 
  ggplot(aes(Season, co2))+
  geom_violin(fill="gray", col="gray")+
  geom_boxplot(width=0.1, outlier.shape=1)+
  scale_y_log10(breaks=c(10, 30, 100, 300))+
  ylab(expression(CO[2]~"(µM)"))+
  xlab("Season")

fig_1 <- fig_1_a + fig_1_b + plot_layout(ncol=1) + plot_annotation(tag_levels = "A")

fig_1

ggsave("figures/figure_1.png", fig_1, width = 129, height = 180, units = "mm")

#Figure 2
test_obs_pred <- read_csv("data/modeling/test_obs_pred.csv")
summary(test_obs_pred)

fig_2 <- test_obs_pred |> 
  ggplot(aes(x=y_test, y=yhat_test))+
  geom_abline(slope=1, intercept = 0, linetype=3)+
  geom_point(shape = 1, alpha=0.5)+
  ylab(expression(Predicted~CO[2]~"(µM)"))+
  xlab(expression(Observed~CO[2]~"(µM)"))+
  xlim(0, 850)+
  ylim(0, 850)

fig_2_marg <- ggMarginal(fig_2, type="density")

fig_2_marg

ggsave("figures/figure_2.png", fig_2_marg, width = 84, height = 84, units = "mm")

#Figure 3
q_points_predictions <- read_parquet("data/q_points_predictions.parquet")
dk_border <- st_read("data/dk_border.sqlite")

q_points_predictions_filter <- q_points_predictions |> 
  filter(snap_dist < 100,
         in_lake == 0) |> 
  select(q_point_x, q_point_y, season, co2_pred) |> 
  st_as_sf(coords=c("q_point_x", "q_point_y"), crs=dk_epsg) |> 
  mutate(Season = str_to_title(season),
         Season = factor(Season, levels=c("Spring", "Summer", "Autumn", "Winter")))

fig_3 <- ggplot()+
  geom_sf(data=dk_border, fill=NA, col="black") +
  geom_sf(data=q_points_predictions_filter, aes(col=co2_pred), size=0.3, stroke=0)+
  facet_wrap(.~Season)+
  scale_color_viridis_c(name = expression(Predicted~CO[2]~"(µM)"), trans="log10", breaks=c(100, 250, 500),
                        option="cividis", direction=-1)+
  theme(strip.background = element_blank(), legend.position = "bottom",
        axis.text = element_blank(), axis.ticks = element_blank())+
  guides(color=guide_colorbar(title.position = "top", ticks = FALSE, barwidth = 12))

fig_3

ggsave("figures/figure_3.png", fig_3, width = 174, height = 174, units = "mm")

#Figure 4
importance <- read_csv("data/modeling/variable_importance.csv")
pdp <- read_csv("data/modeling/partial_dependence.csv")

fig_4_a <- importance |> 
  mutate(variable = sub("mean.", "", variable)) |> 
  ggplot(aes(x=reorder(variable, importance_mean), 
             y=importance_mean, 
             ymin=importance_mean-importance_std, ymax=importance_mean+importance_std))+
  geom_pointrange(size=0.1)+
  coord_flip()+
  ylab("Variable importance")+
  xlab("Variable")

fig_4_b <- pdp |> 
  mutate(variable = sub("mean.", "", variable),
         variable = factor(variable, levels = c('dhym_slope', 'dhym_hand', 'lake', 'artificial_200m'))) |> 
  ggplot(aes(x, response))+
  geom_line()+
  facet_wrap(~variable, ncol=2, scales="free_x")+
  theme(strip.background = element_blank())+
  xlab(NULL)+
  ylab(expression(CO[2]~"(µM)"))

fig_4 <- fig_4_a + fig_4_b + plot_layout(ncol=2, widths=c(1.5, 2)) + plot_annotation(tag_levels = "A")

fig_4

ggsave("figures/figure_4.png", fig_4, width = 174, height = 120, units = "mm")

#Figure 5
#Density plots of estimated fluxes
q_points_flux <- read_parquet("data/q_points_flux.parquet")

fig_5 <- q_points_flux |> 
  mutate(Season = str_to_title(season),
         Season = factor(Season, levels=c("Spring", "Summer", "Autumn", "Winter"))) |> 
  ggplot(aes(co2_flux, fill=Season, col=Season))+
  geom_density(alpha=0.5)+
  ylab("Density")+
  xlab(expression("CO"[2]*" flux (mmol m"^{-2}~d^{-1}*")"))+
  scale_fill_viridis_d(direction=-1)+
  scale_color_viridis_d(direction=-1)+
  coord_cartesian(xlim=c(0, 1000))+
  scale_y_continuous(expand = expand_scale(mult=c(0, 0.1)))+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(title.position = "top"))

fig_5

ggsave("figures/figure_5.png", fig_5, width = 129, height = 100, units = "mm")

#Figure 6
#Country map with estimated fluxes
q_points_flux <- read_parquet("data/q_points_flux.parquet")
dk_border <- st_read("data/dk_border.sqlite")

q_points_flux_sf <- q_points_flux |> 
  select(q_point_x, q_point_y, season, co2_flux) |> 
  st_as_sf(coords=c("q_point_x", "q_point_y"), crs=dk_epsg) |> 
  mutate(Season = str_to_title(season),
         Season = factor(Season, levels=c("Spring", "Summer", "Autumn", "Winter"))) |> 
  filter(between(co2_flux, 0, 1000))

fig_6 <- ggplot()+
  geom_sf(data=dk_border, fill=NA, col="black") +
  geom_sf(data=q_points_flux_sf, aes(col=co2_flux), size=0.3, stroke=0)+
  facet_wrap(.~Season)+
  scale_color_viridis_c(name = expression("CO"[2]*" flux (mmol m"^{-2}~d^{-1}*")"), direction=-1)+
  theme(strip.background = element_blank(), legend.position = "bottom",
        axis.text = element_blank(), axis.ticks = element_blank())+
  guides(color=guide_colorbar(title.position = "top", ticks = FALSE, barwidth = 12))

fig_6

ggsave("figures/figure_6.png", fig_6, width = 174, height = 174, units = "mm")

#Figure 7
#Figure summer CO2 concentration and flux with zoom
#TODO
dk_lakes <- st_read(dk_lakes_path) |> 
  select(gml_id) |> 
  st_transform(dk_epsg)

network <- st_read("data/dk_model/dk_model_hip_2020.shp")

zoom_bbox <- st_bbox(c(xmin = 699100, ymin = 6211500-10000, xmax = 699100+10000, ymax = 6211500), crs=dk_epsg)

network_sub <- network |> 
  st_crop(zoom_bbox)

lakes_sub <- dk_lakes |> 
  st_crop(zoom_bbox) |> 
  mutate(area=as.numeric(st_area(geometry)))

flux_sub <- q_points_flux_sf |> 
  filter(Season == "Summer") |> 
  st_crop(zoom_bbox)

conc_sub <- q_points_predictions_filter |> 
  filter(Season == "Summer") |> 
  st_crop(zoom_bbox)

fig_conc
ggplot()+
  geom_sf(data=network_sub, col="dodgerblue")+
  geom_sf(data=lakes_sub |> filter(area > 10^5), col="dodgerblue")+
  geom_sf(data = conc_sub, aes(col=co2_pred))+
  scale_color_viridis_c(name = expression(Predicted~CO[2]~"(µM)"))

#Figure 8
#Compare estimated vs observed fluxes
#TODO get full dataset and calc co2 concentrations and k600
#TODO add plot with predicted and estimated co2 concentrations and k600

# #Write shapefile with coords and create new column with nearest qpoint
# rewet_flux <- read_excel("data/co2_rewet_sites.xlsx")
# 
# rewet_flux |>
#   st_as_sf(coords = c("longitude", "latitude"), crs=4326) |>
#   st_transform(dk_epsg) |>
#   group_by(site) |>
#   slice(1) |>
#   st_write("data/rewet_sites.sqlite")

q_points_flux <- read_parquet("data/q_points_flux.parquet")
rewet_flux <- read_excel("data/co2_rewet_sites_qpoints.xlsx")

#rewet flux in umol/m2/s
rewet_flux_qpoints <- rewet_flux |> 
  mutate(datetime = ymd_hms(datetime_0),
         date=as_date(datetime),
         month = month(date),
         flux_obs = mean_flux*60*60*24*10^-3) |> 
  left_join(season_map) |>
  left_join(q_points_flux, by=c("index", "season")) |> 
  mutate(Season = str_to_title(season),
         Season = factor(Season, levels=c("Spring", "Summer", "Autumn", "Winter"))) 

fig_8 <- rewet_flux_qpoints |> 
  ggplot(aes(flux_obs, co2_flux, col=Season))+
  geom_abline(intercept = 0, slope=1, linetype=3)+
  geom_point()+
  xlim(0, 450)+
  ylim(0, 450)+
  xlab(expression("Observed CO"[2]*" flux (mmol m"^{-2}~d^{-1}*")"))+
  ylab(expression("Estimated CO"[2]*" flux (mmol m"^{-2}~d^{-1}*")"))+
  theme(legend.position = "bottom")+
  scale_color_viridis_d(direction=-1)+
  guides(color=guide_legend(title.position = "top"))

fig_8

ggsave("figures/figure_8.png", fig_8, width = 84, height = 100, units = "mm")

#Tables and figures for supplementary material

#Table S1 
#(prepared in manuscript)

#Table S2
#Benchmark of predictive models
benchmark <- read_csv("data/modeling/model_benchmark.csv")

metric_to_label <- function(values, digits = 2){
  paste0(round(mean(values), digits), " (±", round(sd(values), digits), ")")
}

table_s2 <- benchmark |> 
  group_by(name) |> 
  summarise(r2 = mean(test_r2),
            rmse = mean(test_neg_root_mean_squared_error*-1),
            mae = mean(test_neg_mean_absolute_error*-1),
            mape = mean(test_neg_mean_absolute_percentage_error*-1),
            r2_label = metric_to_label(test_r2), 
            rmse_label = metric_to_label(test_neg_root_mean_squared_error*-1),
            mae_label = metric_to_label(test_neg_mean_absolute_error*-1),
            mape_label = metric_to_label(test_neg_mean_absolute_percentage_error*-1)) |> 
  arrange(desc(r2)) 

table_s2|> 
  select(name, contains("_label"))  |> 
  write_csv("figures/table_s2.csv")

#Figure S1
#water temperature vs air temperature for flux calculation
q_points_modeling <- read_parquet("data/q_points_modeling.parquet")

wtr_data <- q_points_modeling |> 
  select(airt, wtr, season) |> 
  na.omit()

fig_wtr_model <- wtr_data |> 
  ggplot(aes(x=airt, y=wtr))+
  geom_abline(slope=1, intercept = 0, linetype=3)+
  geom_point(shape=1)+
  geom_smooth(method="lm", col="coral")+
  ylim(0, 22)+
  xlim(0, 22)+
  xlab(expression("Air temperature ("*degree*"C)"))+
  ylab(expression("Water temperature ("*degree*"C)"))

fig_wtr_model

ggsave("figures/figure_s1.png", fig_wtr_model, width = 84, height = 84, units = "mm")

#Figure S2
#Learning curve for random forest model
learning_curve <- read_csv("data/modeling/learning_curve.csv")

fig_learning <- learning_curve |> 
  ggplot(aes(x=train_size_abs, y=test_mean, ymin=test_mean-test_sd, ymax=test_mean+test_sd))+
  geom_line()+
  geom_pointrange(shape=21, fill="white")+
  ylab(expression("Cross-validation"~R^{2}-score))+
  xlab("Training set size")+
  xlim(0, 550)

fig_learning

ggsave("figures/figure_s2.png", fig_learning, width = 129, height = 84, units = "mm")

#Figure S3
#Predictor varible inter-correlation
q_points_modeling <- read_parquet("data/q_points_modeling.parquet")

numeric_preds <- c("site_elev",
                 "discharge", "discharge_specific", "overland", "overland_drain", "sz", "sz_drain",
                 "airt", "precip",
                 "catchment_area", "mean.phraetic", "mean.chalk",  "mean.dhym",
                 "mean.dhym_slope", "mean.dhym_hand", "mean.clay_a",  "mean.clay_b",
                 "mean.clay_c",  "mean.clay_d",  "mean.artificial", "mean.agriculture",
                 "mean.forest" , "mean.nature_eks_agriculture", "mean.stream",  
                 "mean.lake", "mean.artificial_200m", "mean.agriculture_200m",
                 "mean.forest_200m", "mean.nature_eks_agriculture_200m", 
                 "mean.stream_200m", "mean.lake_200m")

cor_matrix <- q_points_modeling |> 
  select(all_of(numeric_preds)) |> 
  cor(use = "complete.obs")

cor_matrix[upper.tri(cor_matrix)] <- NA

colnames(cor_matrix) <- sub("mean.", "", colnames(cor_matrix))
rownames(cor_matrix) <- sub("mean.", "", rownames(cor_matrix))

cor_df <- reshape2::melt(cor_matrix) |> 
  na.omit()

fig_corr <- cor_df |> 
  ggplot(aes(Var1, Var2, fill=value, label=round(value, 2)))+
  geom_tile()+
  geom_text(size=1.5)+
  scale_fill_gradient2(low="dodgerblue", high="coral", mid="grey", name="Correlation\ncoefficient", limits=c(-1, 1))+
  xlab(NULL)+
  ylab(NULL)+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5), legend.position = c(0.2, 0.8))+
  coord_equal()

fig_corr

ggsave("figures/figure_s3.png", fig_corr, width = 174, height = 174, units = "mm")
