library(tidyverse)
library(hrbrthemes)

# Polygon Function
polygon <- function(n) {
  tibble(
    x    = accumulate(1:(n-1), ~.x+cos(.y*2*pi/n), .init = 0),
    y    = accumulate(1:(n-1), ~.x+sin(.y*2*pi/n), .init = 0),
    xend = accumulate(2:n,     ~.x+cos(.y*2*pi/n), .init = cos(2*pi/n)),
    yend = accumulate(2:n,     ~.x+sin(.y*2*pi/n), .init = sin(2*pi/n))) }

# Square
square <-
polygon(4) %>% 
  round() %>% 
  # Center square at origin
  mutate(x = x-(-0.5),
         xend = xend-(-0.5),
         y = y-(0.5),
         yend = yend-(0.5)) %>% 
  as.matrix()

# col names
col.names <- names(polygon(4))

# Rotation matrix
rotation_matrix <-
  function(theta){
    matrix(c(cos(theta*pi/180), -sin(theta*pi/180),
             sin(theta*pi/180), cos(theta*pi/180)),
           nrow = 2, ncol = 2, byrow = T) %>%
      round(digits = 2)
  }

# Rotate multiple squares
rotated.square <- list()
i = 1
for (theta in seq(20,360,6)) {
  i = i + 1
## Rotate x and y initial
square.xy <- 
  square[,1:2] %*% rotation_matrix(theta)
## Rotate x and y final
square.xyend <- 
  square[,3:4] %*% rotation_matrix(theta)

rotated.square[[i]] <-
  cbind(square.xy, square.xyend) %>% as.data.frame()*i*.4 # Increase gradually size
}
# Join all squares in a dataframe
rotated.square <- 
rotated.square %>% bind_rows()

names(rotated.square) <- col.names

colored.squares <- list()

for (i in 1:6) {
  
  colored.squares[[i]] <- 
  rotated.square %>% 
    mutate(group = LETTERS[i])
}

colored.squares <- 
  colored.squares %>% bind_rows()




ggplot(colored.squares)+
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend, color = group), size = .2) +
  # geom_segment(aes(x=x*.8, y=y*.8, xend=xend*.8, yend=yend*.8), 
  #              size = .3, color ="red3") +
  geom_curve(aes(x=x, y=y, xend=xend, yend=yend,color = rev(group)),
             curvature = -1, angle = 45, size = .2, alpha = .9) +
  coord_fixed(xlim = c(-15,15),
              ylim = c(-17.5,17.5)) +
  scale_color_manual(values = c("purple", "red3", "gold", "magenta", "green3", "turquoise3")) +
  facet_wrap(~group, ncol = 2) +
  theme_void() +
  theme(#plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"), # bg of the panel
        plot.background = element_rect(fill = "black"),
        legend.position = "none",
        strip.text = element_blank(),
        panel.spacing = unit(0.2, "lines"))


ggsave(filename = paste0("images/",Sys.Date(), ".png"), 
       plot = last_plot(),
       width = 9.15,
       height = 16.02, 
       dpi = 300)
