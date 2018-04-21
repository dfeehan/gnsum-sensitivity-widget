#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(numDeriv)
library(purrr)
library(directlabels)
library(latex2exp)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Sensitivity: scale-up estimator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         numericInput("estimate",
                      "point estimate:",
                      value=10000),
         sliderInput("delta",
                     "delta:",
                     min = .1,
                     max = 2,
                     step = .01,
                     value = c(.8, 1.2)),
         sliderInput("eta.over.tau",
                     "eta/tau:",
                     min = 0,
                     max = 1.1,
                     step = .01,
                     value = c(.8, 1.02)),
         actionButton(inputId='go',
                       label='Recalculate')
      ),
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type="tabs",
           tabPanel("Surface",
                    #plotOutput("m_plot"),
                    plotOutput("adjusted_plot"),
                    downloadButton("downloadPlot", "Download plot")),
           tabPanel("Raw data",
                    dataTableOutput("tab"))
        )
      )
   )
)

adjust_estimate <- function(estimate,
                            eta.over.tau,
                            delta) {
  return((estimate * eta.over.tau) / (delta))
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # TODO - LEFT OFF HERE: figuring out how to adapt sensitivity analysis
  #   surface
  # maybe axis should be reporting vs structural factors or something?
  #
  #   - errors in known popns
  #   - reporting errors
  #   - structural differences
  
  surfacedata <- eventReactive(input$go, {
    
    m_grid <- 100
    
    eta.over.tau.vals <- seq(from=input$eta.over.tau[1], 
                             to=input$eta.over.tau[2], 
                             length.out=m_grid)
    
    delta.vals <- seq(from=input$delta[1],
                      to=input$delta[2],
                      length.out=m_grid)

    toplot <- expand.grid(eta.over.tau = eta.over.tau.vals,
                          delta = delta.vals)
    
    toplot$estimate <- input$estimate
    
    toplot$adj.est <- pmap_dbl(list(estimate=toplot$estimate, 
                                    eta.over.tau=toplot$eta.over.tau, 
                                    delta=toplot$delta),
                               adjust_estimate)
    
    #toplot$m_v_relerr <- (toplot$m_v - toplot$m) / toplot$m
    #toplot$m_v_relerr_pct <- 100*toplot$m_v_relerr
    
    return(toplot)
  })
  
  adjusted_plot <- reactive({
       ggplot(surfacedata(), aes(x=eta.over.tau, y=delta, z = adj.est)) +
       geom_raster(aes(fill=adj.est)) +
       #geom_contour(color='white', binwidth=1) +
       geom_contour(color='white') +
       theme_minimal() +
       scale_fill_gradient2(low=scales::muted("red"), high=scales::muted("blue"),
                            name="Adjusted\nestimate",
                            labels = scales::comma) +
       xlab(expression(paste("Reporting adjustment factor (", eta/tau, ")"))) +
       ylab(expression(paste("Degree ratio adjustment factor (", delta, ")"))) 
       #ggtitle(TeX("% Relative error in using $M^V$ to estimate $M$"))
       #theme(legend.position='bottom') +
  })
  
   output$adjusted_plot <- renderPlot({
     p <- adjusted_plot()
     return(p)
   })
   
   output$tab <- renderDataTable(surfacedata())

output$downloadPlot<-downloadHandler(
  filename = function() {
    paste('invisible-deaths-sensitivity-illustration', '.png', sep='')
    #paste('plot', '.pdf', sep='')
  },
  content=function(file){
    png(file, height=4, width=6, res=600, units='in')
    print(relerr_plot())
    dev.off()
  },
  contentType='image/png')
  #contentType='application/pdf')

}

# Run the application 
shinyApp(ui = ui, server = server)

