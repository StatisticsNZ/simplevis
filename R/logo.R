# library(tidyverse)
# library(hexSticker)
# library(simplevis)
# 
# nz <- simplevis::nz %>%
#   mutate(dummy = 1) %>%
#   sf::st_as_sf()
# 
# ggplot(nz) +
#   geom_sf() +
#   scale_y_continuous(limits = c(-48, -34), expand = c(0, 0)) +
#   scale_x_continuous(limits = c(166, 180), expand = c(0, 0))
# 
# plot <- ggplot_sf_col(nz, dummy, title = "",
#                       pal = "transparent",
#                       legend_labels = "") +
#   theme(panel.background = element_rect(fill = "#deebf7")) +
#   scale_y_continuous(limits = c(-48, -34), expand = c(0, 0)) +
#   scale_x_continuous(limits = c(166, 180), expand = c(0, 0))
# 
# plot
# 
# sticker(plot, package="simplevis",
#         p_color = "#08519c",
#         h_color = "#08519c",
#         p_size=8, p_family = "sans serif", p_y = 1.0,
#         p_x = 1.0,
#         s_x=1.0, s_y=0.89, s_width = 2.5, s_height=3.5,
#         filename="inst/figures/logo.png", white_around_sticker = TRUE)
