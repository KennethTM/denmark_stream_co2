source("libs_and_funcs.R")

#Figures and tables for manuscript

#Table 1
#Summary statistics of estimated fluxes
q_points_flux <- read_parquet("data/q_points_flux.parquet")

flux_summary <- q_points_flux |> 
  group_by(season) |> 
  summarise(q025 = quantile(co2_flux, probs = 0.025, na.rm=TRUE),
            median = median(co2_flux, na.rm = TRUE),
            q975 = quantile(co2_flux, probs = 0.975, na.rm=TRUE),
            mean = mean(co2_flux, na.rm = TRUE))

flux_summary |> 
  mutate_if(is.numeric, ~round(., digits = 0)) |> 
  mutate(season=str_to_title(season)) |> 
  write_csv("figures/table_1.csv")

#Figure 1
q_points_modeling <- read_parquet("data/q_points_modeling.parquet")
dk_border <- st_read("data/dk_border.sqlite")
network <- st_read("data/dk_model/dk_model_hip_2020.shp")

world_map <- getMap(resolution = "high") |> 
  st_as_sf() |> 
  st_transform(dk_epsg)

roi <- st_as_sfc(st_bbox(st_buffer(dk_border, 20000)))

countries <- st_intersection(roi, world_map) |> 
  st_cast("MULTIPOLYGON") |> 
  st_as_sf() |> 
  mutate(id = 1:n()) |> 
  filter(id != 3) |> 
  mutate(fill = ifelse(id == 2, "dk", "not_dk"))

ice_line <- st_read("data/iceage/Isrand_poly.shp") |> 
  slice(1) |> 
  st_transform(dk_epsg) |>
  st_crop(dk_border) |> 
  st_cast("LINESTRING") |> 
  st_intersection(dk_border) |> 
  st_collection_extract("LINESTRING")

q_points_sf <- q_points_modeling |> 
  st_as_sf(coords=c("q_point_x", "q_point_y"), crs=dk_epsg) |> 
  group_by(index) |> 
  slice(1)

fig_1_a <- ggplot()+
  geom_sf(data=countries, aes(fill=fill), show.legend = FALSE, col="black") +
  scale_fill_manual(values=c("white", "grey"))+
  geom_sf(data=network, col="dodgerblue", linewidth=0.25) +
  geom_sf(data=ice_line, col= "coral") +
  geom_sf(data=q_points_sf, shape=1, size=1)+
  coord_sf(expand = FALSE, xlim=c(442897.0-20000, 892801.1+20000), ylim=c(6049775.1-20000, 6402206.9+20000))

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
test_obs_pred <- read_csv("data/modeling/test_obs_pred.csv") |> 
  mutate(Season = str_to_title(season),
         Season = factor(Season, levels=c("Spring", "Summer", "Autumn", "Winter")))
summary(test_obs_pred)

test_metrics <- jsonlite::fromJSON("data/modeling/test_metrics.json")
test_metrics_label <- paste0("R² = ", round(test_metrics$r2, digits = 2), "\n", 
                             "⍴ = ", round(test_metrics$pearson, digits = 2), "\n",
                             "MAE = ", round(test_metrics$mae, digits = 1), " µM\n",
                             "RMSE = ", round(test_metrics$rmse, digits = 1), " µM\n",
                             "MAPE = ", round(test_metrics$mape*100, digits = 0), "%")

fig_2 <- test_obs_pred |> 
  ggplot(aes(x=y_test, y=yhat_test, col=Season))+
  geom_abline(slope=1, intercept = 0, linetype=3)+
  geom_point()+
  ylab(expression(Predicted~CO[2]~"(µM)"))+
  xlab(expression(Observed~CO[2]~"(µM)"))+
  scale_x_log10(limits=c(25, 850))+
  scale_y_log10(limits=c(25, 850))+
  scale_color_viridis_d(direction = -1)+
  theme(legend.position = "bottom")+
  annotate("text", x=300, y=40, label=test_metrics_label, hjust=0, size=3)+
  guides(color=guide_legend(title.position = "top"))+
  coord_equal()

fig_2_marg <- ggMarginal(fig_2, type="density")

fig_2_marg

ggsave("figures/figure_2.png", fig_2_marg, width = 129, height = 129, units = "mm")

#Figure 3
q_points_predictions <- read_parquet("data/q_points_predictions.parquet")

q_points_predictions_filter <- q_points_predictions |> 
  filter(snap_dist < 100,
         in_lake == 0) |> 
  select(q_point_x, q_point_y, season, co2_pred) |> 
  st_as_sf(coords=c("q_point_x", "q_point_y"), crs=dk_epsg) |> 
  mutate(Season = str_to_title(season),
         Season = factor(Season, levels=c("Spring", "Summer", "Autumn", "Winter")))

fig_3 <- ggplot()+
  geom_sf(data=countries, aes(fill=fill), show.legend = FALSE, col="black") +
  scale_fill_manual(values=c("white", "grey"))+
  geom_sf(data=q_points_predictions_filter, aes(col=co2_pred), size=0.3, stroke=0)+
  facet_wrap(.~Season)+
  scale_color_viridis_b(name = expression(Predicted~CO[2]~"(µM)"), 
                        limits=c(0, 600),
                        option="cividis", 
                        direction=-1, 
                        breaks = c(0, 100, 200, 300, 600),
                        labels=c("", "<100", "200", ">300", ""))+
  theme(strip.background = element_blank(), legend.position = "bottom",
        axis.text = element_blank(), axis.ticks = element_blank())+
  guides(color=guide_colorsteps(even.steps=TRUE, show.limits = FALSE, title.position = "top", ticks = FALSE, barwidth = 12))+
  coord_sf(expand = FALSE, xlim=c(442897.0-20000, 892801.1+20000), ylim=c(6049775.1-20000, 6402206.9+20000))

fig_3

ggsave("figures/figure_3.png", fig_3, width = 174, height = 174, units = "mm")

#Figure 4
importance <- read_csv("data/modeling/variable_importance.csv")
pdp <- read_csv("data/modeling/partial_dependence.csv")

fig_4_a <- importance |> 
  mutate(var_name = unlist(var_name_map[variable])) |> 
  ggplot(aes(x=reorder(var_name, importance_mean), 
             y=importance_mean, 
             ymin=importance_mean-importance_std, ymax=importance_mean+importance_std))+
  geom_pointrange(size=0.1)+
  coord_flip()+
  ylab("Variable importance")+
  xlab("Variable")

fig_4_b <- pdp |> 
  mutate(var_name = unlist(var_name_map[variable]),
         var_name = factor(var_name, levels = c("Slope", "HAND", "Air temperature", "Groundwater depth"))) |> 
  ggplot(aes(x, response))+
  geom_line()+
  facet_wrap(~var_name, ncol=2, scales="free_x")+
  theme(strip.background = element_blank())+
  xlab(NULL)+
  ylab(expression(CO[2]~"(µM)"))

fig_4 <- fig_4_a + fig_4_b + plot_layout(ncol=2, widths=c(1.5, 2)) + plot_annotation(tag_levels = "A")

fig_4

ggsave("figures/figure_4.png", fig_4, width = 174, height = 120, units = "mm")

#Figure 5
#Country map with estimated fluxes
q_points_flux <- read_parquet("data/q_points_flux.parquet")

q_points_flux_sf <- q_points_flux |> 
  select(q_point_x, q_point_y, season, co2_flux) |> 
  st_as_sf(coords=c("q_point_x", "q_point_y"), crs=dk_epsg) |> 
  mutate(Season = str_to_title(season),
         Season = factor(Season, levels=c("Spring", "Summer", "Autumn", "Winter")))

fig_5 <- ggplot()+
  geom_sf(data=countries, aes(fill=fill), show.legend = FALSE, col="black") +
  scale_fill_manual(values=c("white", "grey"))+
  geom_sf(data=q_points_flux_sf, aes(col=co2_flux), size=0.3, stroke=0)+
  facet_wrap(.~Season)+
  scale_color_viridis_b(name = expression("CO"[2]*" flux (mmol m"^{-2}~d^{-1}*")"), 
                        breaks = c(0, 200, 300, 400, 500, 10000),
                        labels=c("", "<200", "300", "400", ">500", ""),
                        limits=c(0, 10000),
                        direction=-1)+
  theme(strip.background = element_blank(), legend.position = "bottom",
        axis.text = element_blank(), axis.ticks = element_blank())+
  guides(color=guide_colorsteps(even.steps=TRUE, title.position = "top", ticks = FALSE, barwidth = 12))+
  coord_sf(expand = FALSE, xlim=c(442897.0-20000, 892801.1+20000), ylim=c(6049775.1-20000, 6402206.9+20000))

fig_5

ggsave("figures/figure_5.png", fig_5, width = 174, height = 174, units = "mm")

#Figure 6
#Figure summer CO2 concentration and flux with zoom
dk_lakes <- st_read(dk_lakes_path) |> 
  select(gml_id) |> 
  st_transform(dk_epsg)

network <- st_read("data/dk_model/dk_model_hip_2020.shp")

zoom_bbox <- st_bbox(c(xmin = 704000, ymin = 6191045-5000, xmax = 704000+20000, ymax = 6191045), crs=dk_epsg)

network_sub <- network |> 
  st_crop(zoom_bbox)

lakes_sub <- dk_lakes |> 
  st_crop(zoom_bbox) |> 
  mutate(area=as.numeric(st_area(geometry))) |> 
  filter(area > 10^4)

flux_sub <- q_points_flux_sf |> 
  filter(Season == "Summer") |> 
  st_crop(zoom_bbox) |> 
  filter(co2_flux < 500)

conc_sub <- q_points_predictions_filter |> 
  filter(Season == "Summer") |> 
  st_crop(zoom_bbox)

fig_6_a <- ggplot()+
  geom_sf(data=network_sub, col="lightblue")+
  geom_sf(data=lakes_sub , col="lightblue", fill="lightblue")+
  geom_sf(data=conc_sub, aes(col=co2_pred), size=1.5, stroke=0)+
  scale_color_viridis_c(name = expression(Predicted~CO[2]~"(µM)"), option="cividis", direction=-1)+
  theme(legend.position = "bottom")+
  guides(color=guide_colorbar(ticks = FALSE, title.position = "top"))+
  scale_y_continuous(expand = expansion(mult=0.1))+
  scale_x_continuous(expand = expansion(mult=0.1/4))

fig_6_b <- ggplot()+
  geom_sf(data=network_sub, col="lightblue")+
  geom_sf(data=lakes_sub, col="lightblue", fill="lightblue")+
  geom_sf(data=flux_sub, aes(col=co2_flux), size=1.5, stroke=0)+
  scale_color_viridis_c(name = expression("CO"[2]*" flux (mmol m"^{-2}~d^{-1}*")"), direction=-1)+
  theme(legend.position = "bottom")+
  guides(color=guide_colorbar(ticks = FALSE, title.position = "top"))+
  scale_y_continuous(expand = expansion(mult=0.1))+
  scale_x_continuous(expand = expansion(mult=0.1/4))

fig_6 <- fig_6_a + fig_6_b + plot_layout(ncol=1, guides="collect") + plot_annotation(tag_levels = "A") &
  theme(legend.position='bottom', axis.text = element_blank(), axis.ticks = element_blank())

fig_6

#Add arrows manually
ggsave("figures/figure_6.pdf", fig_6, width = 174, height = 130, units = "mm")
ggsave("figures/figure_6.png", fig_6, width = 174, height = 130, units = "mm")

#Figure 7
#Compare estimated vs observed fluxes

q_points_flux <- read_parquet("data/q_points_flux.parquet")
insitu_flux <- read_excel("data/insitu_flux/insitu_flux_qpoints.xlsx")

#rewet flux is unit umol/m2/s
insitu_flux_qpoints <- insitu_flux |> 
  mutate(datetime = ymd_hms(datetime_0),
         date = as_date(datetime),
         month = month(date),
         co2_flux_obs = mean_flux*60*60*24*10^-3) |> #umol/m2/sec to mmol/m2/d
  left_join(season_map) |>
  left_join(q_points_flux[,c("index", "season", "co2_flux", "co2_pred", "kgas")], by=c("index", "season")) |> 
  mutate(Season = str_to_title(season),
         Season = factor(Season, levels=c("Spring", "Summer", "Autumn", "Winter"))) |> 
  mutate(aquaenv = pmap(list(temp, ph, alk), ~aquaenv(S=0, t=..1, SumCO2 = NULL, pH = ..2, TA = ..3/1000)),
         co2_obs = map_dbl(aquaenv, ~ .$CO2)*10^6, 
         co2_obs_sat = map_dbl(aquaenv, ~ .$CO2_sat)*10^6,
         kgas_obs = co2_flux_obs/(co2_obs-co2_obs_sat)) |> 
  select(-aquaenv)

fig_7_a <- insitu_flux_qpoints |> 
  ggplot(aes(co2_flux_obs, co2_flux, col=Season))+
  geom_abline(intercept = 0, slope=1, linetype=3)+
  geom_point()+
  #annotate("text", x=400, y=450, label=expression(R^{2}~"= 0.02"), hjust=0)+
  #annotate("text", x=400, y=400, label="n = 40", hjust=0)+
  #geom_text(aes(label=site))+
  xlim(0, 450)+
  ylim(0, 450)+
  xlab(expression("Observed CO"[2]*" flux (mmol m"^{-2}~d^{-1}*")"))+
  ylab(expression("Estimated CO"[2]*" flux (mmol m"^{-2}~d^{-1}*")"))+
  theme(legend.position = "bottom")+
  scale_color_viridis_d(direction=-1)+
  guides(color=guide_legend(title.position = "top"))

fig_7_b <- insitu_flux_qpoints |> 
  ggplot(aes(co2_obs, co2_pred, col=Season))+
  geom_abline(intercept = 0, slope=1, linetype=3)+
  geom_point()+
  xlim(0, 460)+
  ylim(0, 460)+
  xlab(expression(Observed~CO[2]~"(µM)"))+
  ylab(expression(Predicted~CO[2]~"(µM)"))+
  theme(legend.position = "bottom")+
  scale_color_viridis_d(direction=-1)+
  guides(color=guide_legend(title.position = "top"))+
  coord_equal()

fig_7_c <- insitu_flux_qpoints |> 
  ggplot(aes(kgas_obs, kgas, col=Season))+
  geom_abline(intercept = 0, slope=1, linetype=3)+
  geom_point()+
  xlim(0, 55)+
  ylim(0, 55)+
  xlab(expression("Observed k (m"~d^{-1}*")"))+
  ylab(expression("Estimated k (m"~d^{-1}*")"))+
  theme(legend.position = "bottom")+
  scale_color_viridis_d(direction=-1)+
  guides(color=guide_legend(title.position = "top"))+
  coord_equal()

fig_7 <- fig_7_a / (fig_7_b + fig_7_c) + plot_layout(guides="collect") + plot_annotation(tag_levels = "A") &
  theme(legend.position='bottom')

fig_7

ggsave("figures/figure_7.png", fig_7, width = 129, height = 180, units = "mm")

#Tables and figures for supplementary material

#Table S1 
q_points_modeling <- read_parquet("data/q_points_modeling.parquet")

numeric_preds_stats <- q_points_modeling |> 
  select(all_of(numeric_preds)) |> 
  gather(variable, value) |> 
  group_by(variable) |> 
  summarise(median=median(value), 
            q_025 = quantile(value, 0.025),
            q_975 = quantile(value, 0.975)) |> 
  mutate(label = paste0(signif(median, digits=2), " (", signif(q_025, 2), "–", signif(q_975, 2), ")"))

#Table S2 
#(created in manuscript)

#Table S3
#Benchmark of predictive models
benchmark <- read_csv("data/modeling/model_benchmark.csv")

metric_to_label <- function(values, digits = 2){
  paste0(round(mean(values), digits), " (±", round(sd(values), digits), ")")
}

table_s3 <- benchmark |> 
  group_by(name) |> 
  summarise(r2 = mean(test_r2),
            rmse = mean(test_neg_root_mean_squared_error*-1),
            mae = mean(test_neg_mean_absolute_error*-1),
            mape = mean(test_neg_mean_absolute_percentage_error*-1),
            r2_label = metric_to_label(test_r2), 
            rmse_label = metric_to_label(test_neg_root_mean_squared_error*-1, digits=1),
            mae_label = metric_to_label(test_neg_mean_absolute_error*-1, digits=1),
            mape_label = metric_to_label(test_neg_mean_absolute_percentage_error*-1)) |> 
  arrange(desc(r2)) 

table_s3|> 
  select(name, contains("_label"))  |> 
  write_csv("figures/table_s3.csv")

#Figure S1
#Hydrological flow components in summer and winter
q_points_predictions <- read_parquet("data/q_points_predictions.parquet")

q_points_flow <- q_points_predictions |> 
  filter(snap_dist < 100,
         in_lake == 0,
         season %in% c("summer", "winter")) |> 
  select(q_point_x, q_point_y, season, overland, overland_drain, sz, sz_drain) |> 
  gather(variable, value, overland, overland_drain, sz, sz_drain) |> 
  st_as_sf(coords=c("q_point_x", "q_point_y"), crs=dk_epsg) |> 
  mutate(Season = str_to_title(season),
         Season = factor(Season, levels=c("Spring", "Summer", "Autumn", "Winter")),
         variable = as.character(var_name_map[variable]),
         value = value*1000) |> 
  na.omit() |> 
  filter(value >= 0)

fig_hydrocomps <- ggplot()+
  geom_sf(data=countries, aes(fill=fill), show.legend = FALSE, col="black") +
  scale_fill_manual(values=c("white", "grey"))+
  geom_sf(data=q_points_flow, aes(col=value), size=0.3, stroke=0)+
  facet_grid(variable~Season)+
  scale_color_viridis_c(name = expression(Flow~"(l s"^{-1}*") - log"[10]*"(x+1) transformed"), 
                        trans = "log1p", option = "mako", direction = -1,
                        breaks = c(0, 10, 30, 100, 300))+
  theme(strip.background = element_blank(), legend.position = "bottom",
        axis.text = element_blank(), axis.ticks = element_blank())+
  guides(color=guide_colorbar(title.position = "top", ticks = FALSE, barwidth = 12))+
  coord_sf(expand = FALSE, xlim=c(442897.0-20000, 892801.1+20000), ylim=c(6049775.1-20000, 6402206.9+20000))

fig_hydrocomps

ggsave("figures/figure_s1.png", fig_hydrocomps, width = 174, height = 234, units = "mm")

#Figure S2
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

ggsave("figures/figure_s2.png", fig_wtr_model, width = 84, height = 84, units = "mm")

#Figure S3
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

ggsave("figures/figure_s3.png", fig_learning, width = 129, height = 84, units = "mm")

#Figure S4
#Predictor varible inter-correlation
q_points_modeling <- read_parquet("data/q_points_modeling.parquet")

cor_matrix <- q_points_modeling |> 
  select(all_of(numeric_preds)) |> 
  cor(use = "complete.obs")

colnames(cor_matrix) <- as.character(var_name_map[colnames(cor_matrix)])
rownames(cor_matrix) <- as.character(var_name_map[rownames(cor_matrix)])

cor_matrix[upper.tri(cor_matrix)] <- NA

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

ggsave("figures/figure_s4.png", fig_corr, width = 174, height = 174, units = "mm")
