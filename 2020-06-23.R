library(tidyverse)
library(hrbrthemes)

seq(-3, 3, by = 0.01) %>% 
  expand_grid(x = ., y = .) %>% 
  ggplot(aes(x = y+cos(x^2), y = x-exp(-y^2)*sin(y^2))) +
  geom_point(color = "turquoise3", size = 0.1, shape = 21, stroke = 0.1, alpha = .5) +
  coord_polar(theta = "x") +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"), 
        legend.position = "none")

ggsave(filename = paste0("images/",Sys.Date(), ".png"), 
       plot = last_plot(),
       width = 9,
       height = 9, 
       dpi = 120)
