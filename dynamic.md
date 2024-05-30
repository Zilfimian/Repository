https://stackoverflow.com/questions/59182646/formatting-shiny-plotly-subplots-individual-titles-and-graph-size

ggplotly(p, height = nrow(d) * 15) %>% layout(xaxis = list(side ="top" ))
output$fancyPlot <- renderPlot({ #I get the plot_list_final which has some plots (1 or 2 or 3 or 4). n <- length(plot_list_final) nCol <- floor(sqrt(n)) p_last = do.call("grid.arrange", c(plot_list_final, ncol=nCol)) return(p_last) }, height = function() { if_else(length(plot_list_final) == 1, 400, 800)})

https://forum.posit.co/t/changing-the-height-of-plot-area-in-plotoutput-in-shiny-based-on-the-number-of-the-figures-automatically/138083https://stackoverflow.com/questions/50914398/increase-plot-size-in-shiny-when-using-ggplot-facetshttps://stackoverflow.com/questions/46423671/r-facet-wrap-does-not-render-correctly-with-ggplotly-in-shiny-apphttps://stackoverflow.com/questions/42599953/ggplot-with-overlapping-x-axis-labelhttps://stackoverflow.com/questions/41225294/avoid-overlapping-x-axis-labels-in-ggplot-facet-gridhttps://www.geeksforgeeks.org/how-to-avoid-overlapping-labels-in-ggplot2-in-r/https://community.plotly.com/t/plotly-shiny-reactive-height-of-plots/1503https://github.com/plotly/plotly.R/issues/510lotlyOutput <- function(outputId, width = "100%", height = "400px"),
it appears that plotlyOutput htmltools::div(style = "display:inline-block", plotlyOutput("y", width = 250, height = 400))
which should serve the same purpose as plotlyOutput("y", width = 250, height = 400, inline = T)box(plotlyOutput(“plotTSNE”, width = “auto”, height = “auto”),
height = NULL,
width = NULL,
status = “warning”
)scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
output$fancyPlot <- renderPlot({ #I get the plot_list_final which has some plots (1 or 2 or 3 or 4). n <- lenght(plot_list_final ) nCol <- floor(sqrt(n)) p_last = do.call("grid.arrange", c(plot_list_final, ncol=nCol)) return(p_last) },height = function() { if_else(length(plot_list_final) == 1, 400, 800)})
On Thu, May 30, 2024 at 12:43 PM Lusine Zilfimian <lusine.zilfimian@gmail.com> wrote:
YouI am working with R shiny. I have generated ggplot, geom_boxplot and facet_wrap and the use ggplotly to make it interactive.My problem is that the number of boxplots can be different based on the user input. I can have 7 boxes or 20 boxes or other numberAnd I will have problem of1. overlaping of srip text2. overlaping axis text3. size of text is not visible if i make them small4. boxes become very large of small based on user input and make them ivissibleProvide best practice to make this plots responsive and visible.
