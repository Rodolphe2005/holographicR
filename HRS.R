library(ggplot2)
library(viridis)
library(dplyr)
data(diamonds)

transform_to_integer <- function(x, n) {
    if (is.factor(x)){
        if(nlevels(x) != n)
            stop("La valeur de n doit être égale au nombre de niveaux dans tranform_to_integer")
        integers <- as.integer(x)
    }else {
        integers <- cut(x, 
                        breaks = quantile(x, probs = seq(0,1,1/n)), 
                        labels = F, 
                        include.lowest = T)
    }
    integers - min(integers)
    
}

transform_to_integer(diamonds$cut, 5)
transform_to_integer(diamonds$carat, 5)

df <- diamonds
var1 = "carat"
n1 = 5
var2 = "cut"
n2 = 5
target = "price"


df2 <- data.frame(target = df[[target]])
x1 <- transform_to_integer(df[[var1]], n1)
x2 <- transform_to_integer(df[[var2]], n2)
df2$x_axis <- n2*x1 + ifelse(x1%%2 == 0, x2, n2-1-x2)

# Plot of the x axis
my_theme <- theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none",
          panel.margin = unit(0, units = "cm"),
          plot.margin = unit(c(0,0,0,0), units = "cm"))
p1 <- data.frame(expand.grid(x = 1:n1, y = 1:n1)) %>%
    mutate(fill = (x==y)) %>% 
    ggplot(aes(x, y, fill = fill)) +
    geom_tile() +
    scale_fill_manual(values = c("white", "firebrick1")) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    my_theme

p2 <- data.frame(expand.grid(x = 1:(n1*n2), y = 1:n2)) %>%
    mutate(fill = ((x-1)%%n2==(y-1) & ((x-1)%/%n2)%%2 ==0) | 
                  ((x-1)%%n2 == (n2-y) & (((x-1)%/%n2)%%2 == 1))) %>%
    ggplot(aes(x,y,fill = fill)) +
    geom_tile() +
    scale_fill_manual(values = c("white", "dodgerblue")) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    my_theme

p3 <- data.frame(expand.grid(x = 1:(n1*n2), y = 1:n2)) %>%
    mutate(fill = ((x-1)%%n2==(y-1) & ((x-1)%/%n2)%%2 ==0) | 
               ((x-1)%%n2 == (n2-y) & (((x-1)%/%n2)%%2 == 1))) %>%
    ggplot(aes(x,y,fill = fill)) +
    geom_tile() +
    scale_fill_manual(values = c("white", "dodgerblue")) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    my_theme



hlay <- rbind(c(NA,NA,1),
              c(NA,NA,2),
              c(3,4,5))
grid.arrange(grobs=list(p1,p2,p1,p2,p3), 
             layout_matrix=hlay,
             heights = unit(c(1,1,12), c("cm")),
             widths = unit(c(1,1,18), c("cm")),
             padding = 0)









diamonds$carat_cutted <- cut(diamonds$carat, 
                             breaks = quantile(diamonds$carat), 
                             labels = F, 
                             include.lowest = T)
diamonds$x_axis <- nlevels(diamonds$cut)*diamonds$carat_cutted + as.numeric(diamonds$cut)

diamonds %>% 
    group_by(x_axis, color) %>%
    summarise(price = mean(price)) %>%
    ggplot(aes(x = x_axis, y = color, fill = price)) +
    geom_tile() +
    scale_fill_viridis()



