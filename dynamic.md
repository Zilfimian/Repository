https://stackoverflow.com/questions/59182646/formatting-shiny-plotly-subplots-individual-titles-and-graph-size

ggplotly(p, height = nrow(d) * 15) %>% layout(xaxis = list(side ="top" ))
output$fancyPlot <- renderPlot({ #I get the plot_list_final which has some plots (1 or 2 or 3 or 4). n <- length(plot_list_final) nCol <- floor(sqrt(n)) p_last = do.call("grid.arrange", c(plot_list_final, ncol=nCol)) return(p_last) }, height = function() { if_else(length(plot_list_final) == 1, 400, 800)})




https://forum.posit.co/t/changing-the-height-of-plot-area-in-plotoutput-in-shiny-based-on-the-number-of-the-figures-automatically/138083

https://stackoverflow.com/questions/50914398/increase-plot-size-in-shiny-when-using-ggplot-facets

https://stackoverflow.com/questions/46423671/r-facet-wrap-does-not-render-correctly-with-ggplotly-in-shiny-app

https://stackoverflow.com/questions/42599953/ggplot-with-overlapping-x-axis-label

https://stackoverflow.com/questions/41225294/avoid-overlapping-x-axis-labels-in-ggplot-facet-grid

https://www.geeksforgeeks.org/how-to-avoid-overlapping-labels-in-ggplot2-in-r/

https://community.plotly.com/t/plotly-shiny-reactive-height-of-plots/1503

https://github.com/plotly/plotly.R/issues/510

lotlyOutput <- function(outputId, width = "100%", height = "400px"),
it appears that plotlyOutput 

it appears that plotlyOutput htmltools::div(style = "display:inline-block", plotlyOutput("y", width = 250, height = 400))
which should serve the same purpose as plotlyOutput("y", width = 250, height = 400, inline = T)box(plotlyOutput(“plotTSNE”, width = “auto”, height = “auto”),
height = NULL,
width = NULL,
status = “warning”
)scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
output$fancyPlot <- renderPlot({ #I get the plot_list_final which has some plots (1 or 2 or 3 or 4). n <- lenght(plot_list_final ) nCol <- floor(sqrt(n)) p_last = do.call("grid.arrange", c(plot_list_final, ncol=nCol)) return(p_last) },height = function() { if_else(length(plot_list_final) == 1, 400, 800)})
On Thu, May 30, 2024 at 12:43 PM Lusine Zilfimian <lusine.zilfimian@gmail.com> wrote:
YouI am working with R shiny. I have generated ggplot, geom_boxplot and facet_wrap and the use ggplotly to make it interactive.My problem is that the number of boxplots can be different based on the user input. I can have 7 boxes or 20 boxes or other numberAnd I will have problem of1. overlaping of srip text2. overlaping axis text3. size of text is not visible if i make them small4. boxes become very large of small based on user input and make them ivissibleProvide best practice to make this plots responsive and visible.

library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  titlePanel("Dynamic Boxplots with ggplotly"),
  sidebarLayout(
    sidebarPanel(
      # Inputs for user to upload data, select variables, etc.
      fileInput('datafile', 'Choose CSV File', accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      uiOutput('varselect')
    ),
    mainPanel(
      plotlyOutput('plot')
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath)
  })

  output$varselect <- renderUI({
    req(data())
    selectInput('facet_var', 'Select Facet Variable', choices = names(data()))
  })

  output$plot <- renderPlotly({
    req(data())
    req(input$facet_var)
    
    n_facets <- length(unique(data()[[input$facet_var]]))
    plot_height <- 400 + (n_facets * 20)
    
    p <- ggplot(data(), aes(x = as.factor(variable_x), y = variable_y)) +
      geom_boxplot() +
      facet_wrap(as.formula(paste('~', input$facet_var))) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.8)),
        axis.text.y = element_text(size = rel(0.8)),
        strip.text = element_text(size = 10),
        legend.position = "none"
      )
    
    ggplotly(p) %>% layout(height = plot_height)
  })
}

shinyApp(ui, server)

