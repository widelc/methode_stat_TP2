# Import libraries
library("here")

f_plot_pdf <- function(x, title) {
  
  ### Plots the density function of x
  
  #  INPUTS
  #   x     : [vector] (T x 1) of data to plot
  #   title : [string] of the title of the graph
  
  #  OUTPUTS
  #   NONE
  
  #  NOTE
  #   o Limits are set for the x axis (outliers may not appear in graph)
  
  n_breaks = round(10 * log(length(x)))
  chart.Histogram(R = x,
                  breaks = n_breaks,
                  xlab = "Log-Returns",
                  xlim = c(-3, 2),
                  lwd = 1.5,
                  main = title,
                  probability = TRUE,
                  colorset = c("blue","red"),
                  methods = c("add.density"))
  legend(x = "topleft", legend = c("Empirical Density Curve"), fill = c("red"), cex = 0.8)
  
}


f_png_save <- function(x, title, file_name) {
  
  ### Plots the density function of x and saves it in the Output folder
  
  #  INPUTS
  #   x         : [vector] (T x 1) of data to plot
  #   title     : [string] of the title of the graph
  #   file_name : [string] of the name of the .png file
  
  #  OUTPUTS
  #   NONE
  
  png(file = here("Output", paste(file_name, ".png", sep = "")))
  f_plot_pdf(x, title)
  dev.off()
  
}
  
  