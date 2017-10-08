
library(methods)

palette <- list(
    red = rgb(206, 31, 37, maxColorValue = 255),
    blue = rgb(22, 55, 108, maxColorValue = 255),
    green = rgb(118, 174, 66, maxColorValue = 255),
    purple = rgb(121, 84, 163, maxColorValue = 255),
    light_blue = rgb(130, 204, 241, maxColorValue = 255),
    light_green = rgb(149, 204, 107, maxColorValue = 255),
    strip_col = grey(0.95)
)

saveRDS(palette,
        file = "out/palette.rds")


