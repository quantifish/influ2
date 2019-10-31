# devtools::install_github("GuangchuangYu/hexSticker")
library(hexSticker)
library(influ2)

data(iris)
iris <- iris %>% mutate(PetalLength = Petal.Length, SepalLength = factor(round(Sepal.Length)), SepalWidth = factor(round(Sepal.Width)))

p <- plot_bubble(df = iris, group = c("SepalLength", "SepalWidth"), xlab = NULL, ylab = NULL, zlab = NULL, fill = "orange") +
  theme(legend.position = "none", 
        panel.grid = element_line(colour = "black", size = 0.2, linetype = "dotted"),
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = "transparent"))

# Generate sticker

sticker(p, package = "influ2", 
        h_color = "orange", h_fill = "#1881C2", 
        p_y = 1.6, p_size = 20, p_color = "orange",
        s_x = 0.95, s_y = 0.85, s_width = 1.3, s_height = 1.3, 
        filename = "man/figures/logo.png")
