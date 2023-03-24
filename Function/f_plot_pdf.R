# Import libraries
library("here")

f_plot_pdf <- function(x, title, xlim, VaR) {
  
  ### Plots the density function of x
  
  #  INPUTS
  #   x     : [vector] (T x 1) of data to plot
  #   title : [string] of the title of the graph
  #   xlim  : [vector] (2 x 1) Specifying the min and max bound for x-axis
  
  #  OUTPUTS
  #   NONE
  
  #  NOTE
  #   o Limits are set for the x axis (outliers may not appear in graph)
  
  n_breaks = round(10 * log(length(x)))
  chart.Histogram(R = x,
                  breaks = n_breaks,
                  xlab = "Log-Returns",
                  xlim = xlim,
                  lwd = 2,
                  main = title,
                  probability = TRUE,
                  colorset = c("azure4","black"),
                  methods = c("add.density"))
  abline(v = VaR, col = "red", lwd = 2)
  legend(x = "topright", legend = c("Empirical Density Curve","VaR at 95%"), 
         fill = c("black", "red"), 
         cex = 0.8)
  
}


f_png_save <- function(x, title, xlim, VaR, file_name) {
  
  ### Plots the density function of x and saves it in the Output folder
  
  #  INPUTS
  #   x         : [vector] (T x 1) of data to plot
  #   title     : [string] of the title of the graph
  #   xlim  : [vector] (2 x 1) Specifying the min and max bound for x-axis
  #   file_name : [string] of the name of the .png file
  
  #  OUTPUTS
  #   NONE
  
  png(file = here("Output", paste(file_name, ".png", sep = "")))
  f_plot_pdf(x, title, xlim, VaR)
  dev.off()
  
}
  
  