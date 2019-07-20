# theme_partywhale()
# A simple theme based on ggplot2's default theme_grey()
# So I don't need to copy-paste + theme(blah blah blah) all the time


# Call ggplot2 package
library(ggplot2)


# Generate theme_partywhale()
theme_partywhale <- function(base_size = 11,
                             base_family = "",
                             base_line_size = base_size / 22,
                             base_rect_size = base_size / 22){
  half_line <- base_size / 2
  theme_grey(base_size = base_size,
             base_family = base_family) %+replace%
    theme(
      plot.title = element_text(
        size = 16, 
        face = "bold", 
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 11, 
        hjust = 0.5
      ),
      axis.line = element_line(
        size=1, 
        colour = "black"
      ),
      panel.grid.major = element_line(
        linetype = "dotted", 
        color = "gray"
      ),
      panel.grid.minor = element_line(
        linetype = "dotted", 
        color = "gray"
      ),
      panel.background = element_blank(),
      text = element_text(
        size = 12,
        family = base_family,
        face = "plain",
        colour = "black",
        lineheight = 0.9,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = margin(),
        debug = FALSE
      ),
      
      complete = TRUE
    )
}


# Theme test 1, pmf plot:
# 
# y <- dbinom(0:10, 125, 0.008)
# x <- (0:10)
# qplot(x, y, colour = I("dark blue"), size = I(4), shape = I(16)) + 
#   theme_partywhale()


# Theme test 2, Histogram:
# 
# successes <- c(as.numeric(rbinom(100000, 861, 0.008)))
# id <- c(as.numeric(1:length(successes)))
# df <- data.frame("id" = id, "successes" = successes)
# simHistogram <- ggplot(df, aes(x = successes)) + 
#   geom_histogram(binwidth = 1,
#                  alpha = 0.60,
#                  color = "dark red",
#                  aes(fill = ..count..)) + 
#   scale_fill_gradient("Frequency", low = "blue", high = "red") + 
#   labs(title = "Histogram of Number of Mount Drops",
#        subtitle = "Based on 100,000 simulations of 861 dungeon runs",
#        x = "Number of mount drops",
#        y = "Frequency") + 
#   theme_partywhale()
# simHistogram

