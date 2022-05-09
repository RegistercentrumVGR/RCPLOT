# Use internal coordinate datasets
region_coords <- counties
municip_coords <- municipalities

# Map with labels

map_plot(
  df = region_coords,
  fill_var = "id",
  label_var = "Name"
)

# Add some "interresting" data to coordinate dataset and plot
region_coords[["var"]] <- sample(c("Bäst", "Bra", "Dålig", "Sämst"),
                                 size = 21, replace = TRUE)

map_plot(
  df = region_coords,
  fill_var = "var"
)

# Plot both municipalities and region outlines
ggplot2::ggplot() +
  ggplot2::geom_sf(data = municipalities, ggplot2::aes(fill = as.numeric(id))) +
  ggplot2::geom_sf(data = counties, fill = NA, color = "red") +
  ggplot2::theme_void()

# Plot municipalities of Västra Götalands Län, with and without labels
vgregion <- dplyr::filter(municipalities, RegionID == "14")

vgregion[["var"]] <- sample(c("Bäst", "Bra", "Dålig", "Sämst"),
                            size = nrow(vgregion), replace = TRUE)

map_plot(
  df = vgregion,
  fill_var = "var"
)
map_plot(
  df = vgregion,
  fill_var = "var",
  label_var = "Name"
)


