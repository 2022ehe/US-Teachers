
# I based this function off of the Census tutorial on ShinyApp. This function
# will plot a U.S. map by state, with the fill color being indicated by percentage.

percent_map <- function(var, color, legend.title, min = 0, max = 100) {
    
    # To generate a gradient of fill colors, I created a vector of color options.
    
    shades <- colorRampPalette(c("white", color))(100)
    
    # I constrained gradient to percents that occur between min and max.
    
    var <- pmax(var, min)
    var <- pmin(var, max)
    percents <- as.integer(cut(var, 100, 
                               include.lowest = TRUE, ordered = TRUE))
    fills <- shades[percents]
    
    # To plot the choropleth map by state, I used the map("state") function.
    
    map("state", fill = TRUE, col = fills, resolution = 0,
        lty = 1, lwd = 1, projection = "polyconic", 
        myborder = 0, mar = c(0,0,0,0))
    
    # This legend is needed to tell users what the colors mean. This is where
    # I specify that 0 means insufficient data.
    
    inc <- (max - min) / 4
    legend.text <- c(paste0("Insufficient data"),
                     paste0(min + inc, " %"),
                     paste0(min + 2 * inc, " %"),
                     paste0(min + 3 * inc, " %"),
                     paste0(max, " %"))
    
    legend("bottomleft", 
           cex = 0.8,
           legend = legend.text, 
           fill = shades[c(1, 25, 50, 75, 100)], 
           title = legend.title)
}

