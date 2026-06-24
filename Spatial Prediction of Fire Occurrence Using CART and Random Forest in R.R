# ─────────────────────────────────────────────────────────────────────────────
# 0. Setup
# ─────────────────────────────────────────────────────────────────────────────
setwd("C:/Users/tskri/Desktop/Plots/New folder/w7_k15_complete")

# Load Libraries
library(sf)
library(stars)
library(motif)
library(tmap)
library(dplyr)
library(readr)
library(units)
library(terra)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(scales)
library(writexl)

# ─────────────────────────────────────────────────────────────────────────────
# 1. Load and Visualize Forest Cover Raster
# ─────────────────────────────────────────────────────────────────────────────
window_size <- 7
n_clusters <- 15
linkage_method <- "complete"
raster_path <- "C:/Users/tskri/Documents/European Forestry/MASTER_THESIS/RS_Data_Goestling_Preprocessed/RS_Data_Goestling_Preprocessed.tif"
fcdata <- read_stars(raster_path) %>% droplevels()

# Plot forest cover
plot(fcdata, key.pos = 4, key.width = lcm(5), main = NULL)
tm_lc <- tm_shape(fcdata) +
  tm_raster(col.scale = tm_scale_categorical(values = "brewer.set2"),
            col.legend = tm_legend(title = "Forest Cover:")) +
  tm_layout(legend.position = c("LEFT", "BOTTOM"))
tm_lc

# ─────────────────────────────────────────────────────────────────────────────
# 2. Local Landscape Signature Calculation & Clustering
# ─────────────────────────────────────────────────────────────────────────────
forestcover_cove <- lsp_signature(fcdata, type = "cove", window = window_size, neighbourhood = 8, normalization = "pdf")
forestcover_dist <- lsp_to_dist(forestcover_cove, dist_fun = "jensen-shannon")
forestcover_hclust <- hclust(forestcover_dist, method = linkage_method)
plot(forestcover_hclust)
clusters <- cutree(forestcover_hclust, k = n_clusters)

# Cophenetic Correlation
res.coph <- cophenetic(forestcover_hclust)
cor(forestcover_dist, res.coph)

# ─────────────────────────────────────────────────────────────────────────────
# 3. Visualizing Cluster Map (sf and stars options)
# ─────────────────────────────────────────────────────────────────────────────
forestcover_grid_sf <- lsp_add_clusters(forestcover_cove, clusters)
plot(forestcover_grid_sf["clust"])
tm_clu <- tm_shape(forestcover_grid_sf) +
  tm_polygons("clust", fill.scale = tm_scale_categorical(values = "brewer.set2"),
              fill.legend = tm_legend(title = "Cluster:")) +
  tm_layout(legend.position = c("LEFT", "BOTTOM"))
tm_clu

forestcover_grid_stars <- lsp_add_clusters(forestcover_cove, clusters, output = "stars")
forestcover_grid_stars$clust <- as.factor(forestcover_grid_stars$clust)
plot(forestcover_grid_stars["clust"])
tm_clu_stars <- tm_shape(forestcover_grid_stars) +
  tm_raster("clust", col.scale = tm_scale_categorical(values = "brewer.set2"),
            col.legend = tm_legend(title = "Cluster:")) +
  tm_layout(legend.position = c("LEFT", "BOTTOM"))
tm_clu_stars

# ─────────────────────────────────────────────────────────────────────────────
# 4. Export and Dissolve Polygons
# ─────────────────────────────────────────────────────────────────────────────
reference_raster <- rast(raster_path)
forestcover_vect <- vect(forestcover_grid_sf)
forestcover_rasterized <- rasterize(forestcover_vect, reference_raster, field = "clust")
writeRaster(forestcover_rasterized, "forestcover_grid_sf.tif", overwrite = TRUE)

eco_grid_sf2 <- forestcover_grid_sf %>%
  group_by(clust) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")

tm_shape(fcdata) +
  tm_raster(col.scale = tm_scale_categorical(values = "brewer.set2")) +
  tm_shape(eco_grid_sf2) +
  tm_borders(col = "black") +
  tm_layout(legend.show = FALSE)

# ─────────────────────────────────────────────────────────────────────────────
# 5. Area Calculation per Cluster
# ─────────────────────────────────────────────────────────────────────────────
eco_grid_proj <- st_transform(eco_grid_sf2, 32633)
cluster_areas <- eco_grid_proj %>%
  mutate(area_ha = drop_units(set_units(st_area(geometry), ha))) %>%
  st_drop_geometry() %>%
  dplyr::select(clust, area_ha) %>%
  arrange(clust)

# ─────────────────────────────────────────────────────────────────────────────
# 6. Patch-Level Statistics
# ─────────────────────────────────────────────────────────────────────────────
#cluster_raster <- rast("forestcover_grid_sf.tif")
#patch_raster <- patches(cluster_raster, directions = 8, values = TRUE)
#df <- as.data.frame(c(patch_raster, cluster_raster), na.rm = TRUE)
#names(df) <- c("patch_id", "cluster")

#patch_stats <- df %>%
  #group_by(patch_id, cluster) %>%
  #summarise(n_pixels = n(), area_ha = n_pixels * 0.01, .groups = "drop")

#cluster_stats <- patch_stats %>%
  #group_by(cluster) %>%
  #summarise(
    #n_patches = n(),
    #mean_area_ha = mean(area_ha),
    #sd_area_ha = sd(area_ha),
    #mean_area_ge1ha = mean(area_ha[area_ha >= 1], na.rm = TRUE),
   # .groups = "drop"
  #)

#write.csv(cluster_stats, "cluster_summary_window10_k10_wardD2.csv", row.names = FALSE)
#write_xlsx(cluster_stats, "cluster_summary_window10_k10_wardD2.xlsx")

# ─────────────────────────────────────────────────────────────────────────────
# 7. Species Composition and Stand Type per Cluster
# ─────────────────────────────────────────────────────────────────────────────
species_r <- rast(raster_path)
r_df <- as.data.frame(c(species_r, cluster_raster), na.rm = TRUE) %>%
  setNames(c("species", "cluster"))

tab_named <- r_df %>%
  group_by(cluster, species) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(cluster) %>%
  mutate(percent = round(100 * count / sum(count), 1)) %>%
  ungroup() %>%
  left_join(tibble(
    species = c(0, 1, 2, 3, 4, 6, 7, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 23, 24, 25),
    name = c("PicAb", "PicAb_AbiAl", "PicAb_LarDe", "PicAb_PinSy", "PicAb_PinCe",
             "LarDe", "LarDe_PinCe", "PinSy", "PinNi", "PicAb_FagSy",
             "PicAb_OthDec", "LarDe_OthDec", "PinSy_QueSp", "PinSy_OthDec", "PinNi_OthDec",
             "FagSy_PicAb", "FagSy", "QueSp", "OthDec", "PinMu", "AlnVi", "LowVeg")
  ), by = "species")

final_summary <- tab_named %>%
  group_by(cluster) %>%
  summarise(
    Species = paste0(sort(unique(name)), collapse = ", "),
    Percent = paste0(paste0(percent, "% * ", name), collapse = " + "),
    .groups = "drop") %>%
  arrange(cluster)

top3_labels <- tab_named %>%
  group_by(cluster) %>%
  arrange(desc(percent)) %>%
  slice_head(n = 3) %>%
  mutate(pct_int = round(percent), rank = row_number()) %>%
  dplyr::select(cluster, rank, name, pct_int) %>%
  pivot_wider(names_from = rank, values_from = c(name, pct_int), names_sep = "") %>%
  mutate(stand_type = paste0(pct_int1, "_", name1, " ", pct_int2, "_", name2, " ", pct_int3, "_", name3, " stand")) %>%
  dplyr::select(cluster, stand_type)

final_with_labels <- final_summary %>%
  left_join(top3_labels, by = "cluster")
#write.csv(final_with_labels, "cluster_species_composition_with_stand_type.csv", row.names = FALSE)
#write_xlsx(final_with_labels, "cluster_species_composition_with_stand_type.xlsx")

# ─────────────────────────────────────────────────────────────────────────────
# 8. Merge for Final Export
# ─────────────────────────────────────────────────────────────────────────────
cluster_combined <- left_join(cluster_stats, final_with_labels, by = "cluster") %>%
  dplyr::select(-n_patches, -sd_area_ha, -Species, -patches_above_1ha, -pct_patches_above_1ha, -mean_area_ha, -area_above_1ha, -pct_area_above_1ha)
write_xlsx(cluster_combined, "cluster_analysis_summary.xlsx")

# ─────────────────────────────────────────────────────────────────────────────
# 9. Visualization – Cluster Map + Species Composition Bar Chart
# ─────────────────────────────────────────────────────────────────────────────
cluster_labeled_sf <- as.polygons(cluster_raster, dissolve = TRUE) %>%
  st_as_sf() %>%
  left_join(top3_labels, by = c("clust" = "cluster")) %>%
  mutate(
    stand_type = factor(
      stand_type,
      levels = top3_labels %>% arrange(cluster) %>% pull(stand_type)
    )
  )

tab_vis <- tab_named %>%
  mutate(species2 = if_else(percent < 5, "Other", name)) %>%
  group_by(cluster, species2) %>%
  summarise(percent2 = sum(percent), .groups = "drop")

map_plot <- tm_shape(cluster_labeled_sf) +
  tm_polygons(
    "stand_type",               # your field
    title      = "Forest_Type", # ← legend title
    palette    = "Set1",
    textNA     = "No Data"
  ) +
  tm_layout(
    title            = "Cluster Forest Types",  # map heading
    #legend.position   = c("right", "bottom"),
    legend.title.size = 1.4,
    #legend.size  = 5,
    legend.outside = TRUE,
    legend.frame = FALSE
  )

#w/o legend
map_plot_nolegend <- tm_shape(cluster_labeled_sf) +
  tm_polygons(
    "stand_type",               # your field
    title      = "Forest_Type", # ← legend title
    palette    = "Set1",
    textNA     = "No Data"
  ) +
  tm_layout(
    title            = "Cluster Forest Types",  # map heading
    legend.show = FALSE
  )

#just the legend
map_plot_legend <- tm_shape(cluster_labeled_sf) +
  tm_polygons(
    "stand_type",               # your field
    title      = "Forest_Type", # ← legend title
    palette    = "Set1",
    textNA     = "No Data"
  ) +
  tm_layout(
    title            = "Cluster Forest Types",  # map heading
    legend.position   = c("center"),
    legend.title.size = 1.4,
    legend.text.size  = 1,
    legend.only = TRUE,
    legend.frame = FALSE
  )

#Barplot
bar_plot <- ggplot(tab_vis, aes(
  x    = factor(cluster),
  y    = percent2,
  fill = species2
)) +
  geom_col(width = 0.7, color = "gray30") +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 100),
    oob    = scales::squish
  ) +
  scale_fill_viridis_d(
    option = "plasma",
    drop   = FALSE,
    name   = "Pixel Types"
  ) +
  labs(x = "Cluster", y = "Percent (%)") +
  theme_minimal() +
  theme(
    axis.text.x        = element_text(size = 9),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom"
  )

# Ensure tmap is in 'plot' mode (not 'view' or mixed)
tmap_mode("plot")

# Save the map plot
tmap_save(
  tm       = map_plot,
  filename = "map_plot.jpg",
  width    = 10,
  height   = 10,
  dpi= 300,
  
)
tmap_save(
  tm       = map_plot_legend,
  filename = "map_plot_legend.jpg",
  width    = 10,
  height   = 10,
  dpi= 300,
)
tmap_save(
  tm       = map_plot_nolegend,
  filename = "map_plot_nolegend.jpg",
  width    = 10,
  height   = 10,
  dpi= 300,
)
ggsave("barplot.jpg", bar_plot, width = 10, height = 6, dpi= 300, bg = "white")


