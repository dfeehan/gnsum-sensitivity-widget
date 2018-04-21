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
   titlePanel("Sensitivity to invisible deaths"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("m_v",
                     "Visible death rate (per 1,000):",
                     min = 1,
                     max = 100,
                     step = 1,
                     value = 10),
         sliderInput("K",
                     "Focal factor for invisible death rate (1 = same as visible):",
                     min = 0,
                     max = 5,
                     step = .01,
                     value = 1),
         sliderInput("p_i",
                     "Focal fraction of deaths that is invisible:",
                     min = 0,
                     max = 1,
                     step = .01,
                     value = .2),
         sliderInput("scale_K",
                     "K scale factor:",
                     min = .1,
                     max = 1,
                     step = .01,
                     value = .25),
         sliderInput("scale_p",
                     "p scale factor:",
                     min = .1,
                     max = 1,
                     step = .01,
                     value = .5),
         actionButton(inputId='go',
                       label='Recalculate')
      ),
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type="tabs",
           tabPanel("Surface",
                    #plotOutput("m_plot"),
                    plotOutput("p_k_relerr_plot"),
                    downloadButton("downloadPlot", "Download plot")),
           tabPanel("Raw data",
                    dataTableOutput("tab"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # calculate the harmonic mean of death rates m1 and m2,
  # with p1 the fraction of deaths in group 1
  h_mean <- function(m1, m2, p1) {
    return(1 / ((p1/m1) + ((1-p1)/m2)))
  }
  
  # expression
  agg_factor <- function(K, p) {
    return(K / (p + K*(1-p)))
  }
  
  # return the factor by which the visible dr gets multiplied in
  # the lower bound
  lb_factor <- function(K, p) {
    if (K <= 1) { return(1) }
    return(1 / sqrt((p/K) + (1-p)))
  }
  
  # return the factor by which the visible dr gets multiplied in
  # the upper bound
  ub_factor <- function(K, p) {
    return(K^p)
  }
  
  
  surfacedata <- eventReactive(input$go, {
    
    m_grid <- 100
    
    p.vals <- seq(from=(1-input$scale_p)*input$p_i, 
                  to=(1+input$scale_p)*input$p_i, 
                  length.out=m_grid)
    
    K.vals <- seq(from=(1-input$scale_K)*input$K, 
                  to=(1+input$scale_K)*input$K, 
                  length.out=m_grid)

    toplot <- expand.grid(p_i = p.vals,
                          K = K.vals)
    
    toplot$m_v <- input$m_v
    
    toplot$m_i <- toplot$m_v * toplot$K
    
    toplot$ub_factor <- map2_dbl(toplot$K,
                                 toplot$p_i,
                                 ~ ub_factor(K=.x, p=.y))
    toplot$m_upper <- toplot$m_v * toplot$ub_factor
    
    toplot$lb_factor <- map2_dbl(toplot$K,
                                 toplot$p_i,
                                 ~ lb_factor(K=.x, p=.y))
    toplot$m_lower <- toplot$m_v * toplot$lb_factor
    
    toplot$agg_factor <- map2_dbl(toplot$K,
                                  toplot$p_i,
                                  ~ agg_factor(K=.x, p=.y))
    toplot$m_agg <- toplot$m_v * toplot$agg_factor
    
    toplot$m <- pmap_dbl(list(m1=toplot$m_i, 
                              m2=toplot$m_v, 
                              p1=toplot$p_i),
                         h_mean)
    toplot$m_v_relerr <- (toplot$m_v - toplot$m) / toplot$m
    toplot$m_v_relerr_pct <- 100*toplot$m_v_relerr
    
    return(toplot)
  })
  
  relerr_plot <- reactive({
       ggplot(surfacedata(), aes(x=p_i, y=K, z = m_v_relerr_pct)) +
       geom_raster(aes(fill=m_v_relerr_pct)) +
       #geom_contour(bindwidth=.01, color='white') +
       geom_contour(color='white', binwidth=1) +
       theme_minimal() +
       scale_fill_gradient2(low=scales::muted("red"), high=scales::muted("blue"),
                            name="% Rel. Err.") +
       scale_x_continuous(labels=scales::percent) +
       xlab(expression(p[D[alpha]]^V))
       #ggtitle(TeX("% Relative error in using $M^V$ to estimate $M$"))
       #theme(legend.position='bottom') +
  })
  
   output$p_k_relerr_plot <- renderPlot({
     p <- relerr_plot()
     return(p)
     #p1 <- direct.label(p, list('bottom.pieces', color='black'))
     #return(p1)
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

