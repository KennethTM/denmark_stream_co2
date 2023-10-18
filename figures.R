source("libs_and_funcs.R")

#Figures and tables

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
  geom_sf(data=network, col="dodgerblue") +
  geom_sf(data=q_points_sf, col="coral", size=1)

fig_1_b <- q_points_modeling |> 
  mutate(Season = str_to_title(season)) |> 
  ggplot(aes(Season, co2))+
  geom_violin(fill="gray", col="gray")+
  geom_boxplot(width=0.1, outlier.shape=1)+
  scale_y_log10()+
  ylab(expression(CO[2]~"(µM)"))+
  xlab("Season")

fig_1 <- fig_1_a + fig_1_b + plot_layout(ncol=1) + plot_annotation(tag_levels = "A")

fig_1

ggsave("figures/figure_1.png", fig_1, width = 129, height = 180, units = "mm")

#Table 2
benchmark <- read_csv("data/model_benchmark.csv")

metric_to_label <- function(values, digits = 2){
  paste0(round(mean(values), digits), " (±", round(sd(values), digits), ")")
}

table_2 <- benchmark |> 
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

table_2|> 
  select(name, contains("_label"))  |> 
  write_csv("figures/table_2.csv")

#Figure 2
test_obs_pred <- read_csv("data/test_obs_pred.csv")
summary(test_obs_pred)

fig_2 <- test_obs_pred |> 
  ggplot(aes(x=y_test, y=yhat_test))+
  geom_abline(slope=1, intercept = 0, linetype=3)+
  geom_point(shape = 1)+
  ylab(expression(Predicted~CO[2]~"(µM)"))+
  xlab(expression(Observed~CO[2]~"(µM)"))+
  #scale_x_log10()+
  #scale_y_log10()+
  xlim(0, 850)+
  ylim(0, 850)

fig_2

ggsave("figures/figure_2.png", fig_2, width = 84, height = 84, units = "mm")

#Figure 3
q_points_predictions <- read_parquet("data/q_points_predictions.parquet")
dk_border <- st_read("data/dk_border.sqlite")

q_points_predictions_filter <- q_points_predictions |> 
  filter(snap_dist < 100,
         in_lake == 0,
         is.na(downstream)) |> 
  select(q_point_x, q_point_y, season, co2_pred) |> 
  st_as_sf(coords=c("q_point_x", "q_point_y"), crs=dk_epsg) |> 
  mutate(Season = str_to_title(season),
         Season = factor(Season, levels=c("Spring", "Summer", "Autumn", "Winter")))

fig_3 <- ggplot()+
  geom_sf(data=dk_border, fill=NA, col="black") +
  geom_sf(data=q_points_predictions_filter, aes(col=co2_pred), size=0.3, stroke=0)+
  facet_wrap(.~Season)+
  scale_color_viridis_c(name = expression(Predicted~CO[2]~"(µM)"), trans="log10", breaks=c(10, 20, 50, 100, 200, 500),
                        option="cividis")+
  theme(strip.background = element_blank(), legend.position = "bottom",
        axis.text = element_blank(), axis.ticks = element_blank())+
  guides(color=guide_colorbar(title.position = "top", ticks = FALSE, barwidth = 12))

ggsave("figures/figure_3.png", fig_3, width = 174, height = 174, units = "mm")

#Figure 4
importance <- read_csv("data/variable_importance.csv")
pdp <- read_csv("data/partial_dependence.csv")

fig_4_a <- importance |> 
  mutate(variable = sub("mean.", "", variable)) |> 
  ggplot(aes(x=reorder(variable, importance_mean), 
             y=importance_mean, 
             ymin=importance_mean-importance_std, ymax=importance_mean+importance_std))+
  geom_pointrange(shape=21, fill="white")+
  coord_flip()+
  ylab("Variable importance")+
  xlab("Variable")

fig_4_b <- pdp |> 
  mutate(variable = sub("mean.", "", variable),
         variable = factor(variable, levels = c('dhym_slope', 'airt', 'lake', 'artificial_200m'))) |> 
  ggplot(aes(x, response))+
  geom_line()+
  facet_wrap(~variable, ncol=2, scales="free_x")+
  theme(strip.background = element_blank())+
  xlab("Scaled values")+
  ylab(expression(CO[2]~"(µM)"))


fig_4 <- fig_4_a + fig_4_b + plot_layout(ncol=2, widths=c(1.5, 2)) + plot_annotation(tag_levels = "A")

fig_4

ggsave("figures/figure_4.png", fig_4, width = 174, height = 120, units = "mm")

#Figure 5

#Figure 6

#Figure correlation
q_points_modeling <- read_parquet("data/q_points_modeling.parquet")

numeric_preds <- c("site_elev",
                 "discharge", "overland", "overland_drain", "sz", "sz_drain",
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

ggsave("figures/figure_correlation.png", fig_corr, width = 174, height = 174, units = "mm")

#Figure learning curve
learning_curve <- read_csv("data/learning_curve.csv")

fig_learning <- learning_curve |> 
  ggplot(aes(x=train_size_abs, y=test_mean, ymin=test_mean-test_sd, ymax=test_mean+test_sd))+
  geom_line()+
  geom_pointrange(shape=21, fill="white")+
  ylab(expression("Cross-validation"~R^{2}-score))+
  xlab("Training set size")

ggsave("figures/figure_learning.png", fig_learning, width = 129, height = 84, units = "mm")
