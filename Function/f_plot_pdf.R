# Import libraries
library("here")
library("ggplot2")

f_plot_pdf <- function(result, model_name) {
  
  ### Plots the density function of x
  
  #  INPUTS
  #   result      : [list] containing the VaR, the ES and the P&L to plot
  #   model_name  : [string] of the title of the model used
  
  #  OUTPUTS
  #   NONE

  
  # Data preparation
  n_breaks     <- round(10 * log(length(result$PL)))
  df           <- data.frame(result$PL)
  colnames(df) <- "PL"
  
  plot <- ggplot(data = df) + 
    
    # Plot Histogram
    geom_histogram(data = df, 
                   aes(x = PL, y = ..density..), 
                   fill  = "black",
                   color = "white",
                   alpha = 0.25,
                   bins  =  n_breaks) +
    
    # Overlay density
    geom_density(aes(x = PL, colour = "Empirical Density"),
                 lwd = 0.9) +
    
    # VaR vertical line
    geom_vline(aes(xintercept = result$VaR, 
                   colour=paste("VaR at 95% :", substr(as.character(result$VaR), 1, 7),"$")), 
               linetype="dashed", 
               size = 1) +
    
    # ES vertical line
    geom_vline(aes(xintercept = result$ES, 
                   colour=paste("ES at 95% :", substr(as.character(result$ES), 1, 7),"$")), 
               linetype="dashed", 
               size = 1) +
    
    # Mean vertical line
    geom_vline(aes(xintercept = mean(result$PL), 
                   colour=paste("P&L Mean :", substr(as.character(mean(result$PL)), 1, 5),"$")), 
               linetype="dashed", 
               size = 1) +
    
    scale_colour_manual("Legend", values=c("gray10","green", "darkblue", "brown2")) +
    
    theme_minimal() +
    
    xlab("Profit & Loss ($)") +
    ylab("Density") +
    ggtitle(label = "Options portfolio P&L distribution", subtitle = model_name) +
    
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = c(0.8, 0.7),
          legend.background = element_rect(fill = "white"),
          legend.title.align = 0.5,
          legend.box = NULL)
  
  # Output
  plot
}


f_png_save <- function(result, model_name, file_name) {
  
  ### Plots the density function of x and saves it in the Output folder
  
  #  INPUTS
  #   result      : [list] containing the VaR, the ES and the P&L to plot
  #   model_name  : [string] of the title of the model used
  #   file_name : [string] of the name of the .png file
  
  #  OUTPUTS
  #   NONE
  
  png(file = here("Output", paste(file_name, ".png", sep = "")))
  print(f_plot_pdf(result, model_name))
  dev.off()
  
}
  
  