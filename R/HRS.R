library(ggplot2)
library(viridis)
library(dplyr)
library(gridExtra)
library(grid)
data(diamonds)

# Transform a factor or numeric vector x to an integer vector taking values in ttt2
# 0, ..., n-1
transform_to_integer <- function(x, n) {
    if (is.factor(x)){
        if(nlevels(x) != n)
            stop("The value of n must be equal to the number of levels of x")
        integers <- as.integer(x)
    }else {
        integers <- cut(x,
                        breaks = quantile(x, probs = seq(0,1,1/n), na.rm = T),
                        labels = F,
                        include.lowest = T)
    }
    integers - min(integers, na.rm = T)

}

# This function creates a plot which represents a function taking n different
# values and increases/decreases waves times.
# For example, for a plot that increases and then decreases, waves = 2
create_waves <- function(n, waves = 1, color = "firebrick1", location_top = T, title){
    increasing_wave <- expand.grid(x = 1:n, y = 1:n) %>% mutate(fill = (y == x))
    decreasing_wave <- expand.grid(x = 1:n, y = 1:n) %>% mutate(fill = (y == n+1-x))

    num_increasing_waves <- floor((waves+1)/2)
    num_decreasing_waves <- floor(waves/2)

    increasing_waves <- 1:num_increasing_waves %>%
        lapply(function(k) increasing_wave %>% mutate(x = x + 2*n*(k-1))) %>%
        bind_rows()

    if (num_decreasing_waves>0){
        decreasing_waves <- 1:num_decreasing_waves %>%
            lapply(function(k) decreasing_wave %>% mutate(x = x + n + 2*n*(k-1))) %>%
            bind_rows()
    } else {
        decreasing_waves <- NULL
    }

    df <- rbind(increasing_waves, decreasing_waves)
    if(location_top){
        g <- ggplot(df, aes(x, y, fill = fill)) +
             scale_x_continuous(expand = c(0,0))
    } else {
        g <- ggplot(df, aes(y, x, fill = fill)) +
            scale_x_continuous(expand = c(0,0), trans = "reverse")
    }

    wave <- g + geom_tile() +
        scale_fill_manual(values = c("white", color)) +
        scale_y_continuous(expand = c(0,0)) +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank(),
              legend.position = "none",
              panel.margin = unit(0, units = "cm"),
              plot.margin = unit(c(0,0,0,0), units = "cm"))

    if (location_top){
        final <- grid.arrange(textGrob(title), wave, heights = unit(c(5,15),"mm"))
    } else {
        final <- grid.arrange(textGrob(title, rot = 90),
                              wave,
                              widths = unit(c(5,15), "mm"),
                              ncol = 2, newpage = T)
    }
    grid.newpage()
    final
}





df <- diamonds
var1 = "carat"
n1 = 3
var2 = "cut"
n2 = 5
target = "price"

var5 = "clarity"
n5 = 8
var6 = "color"
n6 = 7

df2 <- data.frame(target = df[[target]])
df2$x1 <- transform_to_integer(df[[var1]], n1)
df2$x2 <- transform_to_integer(df[[var2]], n2)
df2$y1 <- transform_to_integer(df[[var5]], n5)
df2$y2 <- transform_to_integer(df[[var6]], n6)
main_g <- df2 %>%
    mutate(
        x_axis = n2*x1 + ifelse(x1%%2 == 0, x2, n2-1-x2),
        y_axis = n6*y1 + ifelse(y1%%2 == 0, y2, n6-1-y2)
    ) %>%
    group_by(x_axis, y_axis) %>%
    summarise(target = mean(target)) %>%
    ggplot(aes(x = x_axis, y = y_axis, fill = target)) +
    geom_tile() +
    scale_fill_viridis() +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none",
          panel.margin = unit(0, units = "cm"),
          plot.margin = unit(c(0,0,0,0), units = "cm"))

top <- grid.arrange(create_waves(n2,n1, "firebrick1", T, var2),
                    create_waves(n1,1, "dodgerblue", T, var1))
left <- grid.arrange(create_waves(n6, n5, "skyblue4", F, var6),
                    create_waves(n5,1, "aquamarine4", F, var5), ncol = 2)
grid.arrange(textGrob(" "), top, left, main_g, ncol = 2,
             heights = unit(c(4,24), c("cm")),
             widths = unit(c(4,36), c("cm")),
             padding = 0)
