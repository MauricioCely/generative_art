library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(imager) # Image Processing Library Based on 'CImg'
library(pals) # Color Palettes, Colormaps, and Tools to Evaluate Them
library(patchwork) # The Composer of Plots 

img <- imager::load.image("supp/vitruvio.jpg") 

data <-
img %>%
  as.data.frame(wide = "c") %>% 
  mutate(color = round((c.1+c.2+c.3)^3/3, 2),
         dark  = round((c.1+c.2+c.3)/3, 2)) %>%
  filter(y %% 3 == 0)
  # filter(y %% 7 == 0) # Marilyn

plot <-   
ggplot(data = data, 
       aes(x, - y, group = y, color = color)) +
  geom_path(size = (1 - data$dark)*3, show.legend = FALSE, alpha = .5) +
  coord_fixed(expand = F) +
  theme_void() +
  theme(plot.background  = element_rect(fill = "#252a32"),
        panel.background = element_rect(fill = "#252a32"))

a <- plot + scale_color_gradientn(colours = rev(brewer.greys(20)))
b <- plot + scale_color_gradientn(colours = inferno(20))
c <- plot + scale_color_gradientn(colours = cubehelix(20))
d <- plot + scale_color_gradientn(colours = viridis(20) %>% rev) # Vetruvio
# d <- plot + scale_color_gradientn(colours = (viridis(20))) # Marilyn
e <- plot + scale_color_gradientn(colours = brewer.spectral(20))
f <- plot + scale_color_gradientn(colours = ocean.speed(20) %>% rev) # Vitruvio
# f <- plot + scale_color_gradientn(colours = ocean.haline(20)) # Marilyn

plot <- a + b + c + d + e + f + plot_layout(nrow = 3) & 
  theme_void() +
  theme(plot.background  = element_rect(fill = "#252a32"), # Vitruvio
        panel.background = element_rect(fill = "#252a32"))
  # theme(plot.background  = element_rect(fill = "gray50"), # Marilyn
  #       panel.background = element_rect(fill = "gray50"))

# ggsave(filename = paste0("images/",Sys.Date(), "_marilyn.png"), 
#        plot = plot,
#        height = 15.75,
#        width = 10.5, 
#        dpi = 300)

ggsave(filename = paste0("images/",Sys.Date(), "_vitruvio.png"),
       plot = plot,
       height = 15.8,
       width = 10,
       dpi = 300)
