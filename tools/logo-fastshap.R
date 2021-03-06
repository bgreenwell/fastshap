# Load required packages
library(ggplot2)

# Load vip image
img <- grid::rasterGrob(
  png::readPNG("tools/road-runner.png"), interpolate = TRUE, width = 0.85
)

# Hexagon data
hex <- data.frame(x = 1.35 * 1 * c(-sqrt(3) / 2, 0, rep(sqrt(3) / 2, 2), 0,
                                   rep(-sqrt(3) / 2, 2)),
                  y = 1.35 * 1 * c(0.5, 1, 0.5, -0.5, -1, -0.5, 0.5))

# Hexagon logo
hex_logo <- ggplot() +
  geom_polygon(data = hex, aes(x, y), color = "#351D7D", fill = "white",
               size = 3) +
  annotation_custom(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  annotate(geom = "text", label = "fastshap", x = -0.4, y = -0.15,
           family = "Comic Sans MS",
           # fontface = "bold.italic",
           color = "#86AEC8", size = 7) +
  coord_equal(xlim = range(hex$x), ylim = range(hex$y)) +
  scale_x_continuous(expand = c(0.04, 0)) +
  scale_y_reverse(expand = c(0.04, 0)) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
print(hex_logo)

png("man/figures/logo-fastshap.png", width = 181, height = 209,
    bg = "transparent", type = "quartz")
print(hex_logo)
dev.off()

svg("man/figures/logo-fastshap.svg", width = 181 / 72, height = 209 / 72,
    bg = "transparent", family = "Comic Sans MS")
print(hex_logo)
dev.off()
