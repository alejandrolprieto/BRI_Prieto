library(shiny)
library(sepkoski)
library(geoscale)
library(mgcv)

ui <- fluidPage(
  titlePanel("Example BRI: Fossil Diversity and Range Size"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_choice", "Select a plot:",
                  choices = c("Sepkoski Curve",
                              "Panel A: Diversity",
                              "Panel B: Range Size")),
      sliderInput("age_range", "Filter by Age (Ma):",
                  min = 0, max = 430,
                  value = c(0, 430), step = 1),
      # NEW: Download button
      downloadButton("download_plot", "Download Plot")
    ),
    
    mainPanel(
      plotOutput("plot_display", height = "600px")
    )
  )
)

server <- function(input, output) {
  
  make_plot <- function() {
    mass_extinctions <- c(444, 372, 252, 201, 66)
    
    if (input$plot_choice == "Sepkoski Curve") {
      sepkoski_curve(fill = TRUE)
      
    } else if (input$plot_choice == "Panel A: Diversity") {
      subset_data <- subset(proxy_counts,
                            avg_ma >= input$age_range[1] &
                              avg_ma <= input$age_range[2])
      log_div <- log(subset_data$count_taxa + 1)
      z_div <- scale(log_div)[,1]
      gam_div <- gam(z_div ~ s(avg_ma, k = 10), data = subset_data)
      ma_seq <- seq(min(subset_data$avg_ma), max(subset_data$avg_ma), length.out = 200)
      pred <- predict(gam_div, newdata = data.frame(avg_ma = ma_seq))
      
      geoscalePlot(subset_data$avg_ma, z_div,
                   units = c("Period"), tick.scale = "Period",
                   boxes = "Period", abbrev = c("Period"),
                   age.lim = c(430, 0), data.lim = c(-5, 5),
                   label = "Z-scored Log(Diversity)",
                   direction = "horizontal",
                   erotate = 45,          # rotate x labels 45 degrees
                   font = 2,              # make labels bold
                   cex.axis = 1.2)        # increase axis font size
      
      lines(subset_data$avg_ma, z_div, col = "black", lwd = 2)
      lines(ma_seq, pred, col = "darkgreen", lwd = 3, lty = 2)
      for (ext in mass_extinctions) abline(v = ext, col = "red", lwd = 2)
      
    } else if (input$plot_choice == "Panel B: Range Size") {
      subset_data <- subset(results,
                            Stage >= input$age_range[1] &
                              Stage <= input$age_range[2])
      log_area <- log(subset_data$Occupied_km2 + 1)
      z_area <- scale(log_area)[,1]
      gam_area <- gam(z_area ~ s(Stage, k = 10), data = subset_data)
      ma_seq <- seq(min(subset_data$Stage), max(subset_data$Stage), length.out = 200)
      pred <- predict(gam_area, newdata = data.frame(Stage = ma_seq))
      
      geoscalePlot(subset_data$Stage, z_area,
                   units = c("Period"), tick.scale = "Period",
                   boxes = "Period", abbrev = c("Period"),
                   age.lim = c(430, 0), data.lim = c(-5, 5),
                   label = "Z-scored Log(Range Size)",
                   direction = "horizontal",
                   erotate = 45,
                   font = 2,
                   cex.axis = 1.2)
      
      lines(subset_data$Stage, z_area, col = "blue", lwd = 2)
      lines(ma_seq, pred, col = "darkorange", lwd = 3, lty = 2)
      for (ext in mass_extinctions) abline(v = ext, col = "red", lwd = 2)
    }
  }
  
  output$plot_display <- renderPlot({
    make_plot()
  })
  
  # NEW: download handler
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", input$plot_choice), ".png")
    },
    content = function(file) {
      png(file, width = 1200, height = 800)
      make_plot()
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)
