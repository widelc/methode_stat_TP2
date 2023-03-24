# Import libraries
library("here")

f_plot_pdf <- function(x, title, VaR, y_max) {
  
  ### Plots the density function of x
  
  #  INPUTS
  #   x      : [vector] (T x 1) of data to plot
  #   title  : [string] of the title of the graph
  #   VaR    : [Scalar] Value at Risk of x
  #   y_max  : [scalar] Specifying the max bound for y-axis
  
  #  OUTPUTS
  #   NONE
  
  #  NOTE
  #   o Limits are set for the x axis (outliers may not appear in graph)
  
  # Histogram
  windowsFonts(A = windowsFont("Times New Roman"))
  n_breaks = round(10 * log(length(x)))
  chart.Histogram(R = x,
                  breaks = n_breaks,
                  xlab = "Profit & Loss ($)",
                  ylim = c(0,y_max),
                  lwd = 2,
                  main = title,
                  probability = TRUE,
                  colorset = c("azure4","black"),
                  methods = c("add.density"),
                  family = "A")
  
  # Graph the VaR of x
  abline(v = VaR, 
         col = "red", 
         lwd = 3)
  
  # Graph the mean of the x
  abline(v = mean(x), 
         col = "purple", 
         lwd = 3)
  
  # Legend
  char_VaR   <- substr(as.character(VaR), 1, 7)
  char_mean <- substr(as.character(mean(x)), 1, 5)
  legend(x = "topright", 
         legend = c("Empirical Density Curve",paste("VaR at 95% =",char_VaR," $"), paste("P&L mean =", char_mean, "$")), 
         fill = c("black", "red","purple"), 
         cex = 0.8)
  
}


f_png_save <- function(x, title, VaR, y_max, file_name) {
  
  ### Plots the density function of x and saves it in the Output folder
  
  #  INPUTS
  #   x         : [vector] (T x 1) of data to plot
  #   title     : [string] of the title of the graph
  #   VaR       : [Scalar] Value at Risk of x
  #   y_max     : [scalar] Specifying the max bound for y-axis
  #   file_name : [string] of the name of the .png file
  
  #  OUTPUTS
  #   NONE
  
  png(file = here("Output", paste(file_name, ".png", sep = "")))
  f_plot_pdf(x, title, VaR, y_max)
  dev.off()
  
}
  
  