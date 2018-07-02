

#TODO: refactor so that the graph area isn't called density plot
#TODO: only show Y axis limits when box plot is selected
#TODO: auto populate axis limtis boxes when checked, and then refactor code so that checkbox determines whether axis limits are used
#TODO: Density plus scatter plot

library(shiny)
library(ggplot2)
require(ggplot2)

ui <- fluidPage(
  
  
   
  fileInput("inputFile", "Choose CSV File",
            multiple = TRUE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  
  selectInput("select", label = h3("Select Chart Type"), 
              choices = list("Density" = 1, "Scatter" = 2, "Density + Scatter" = 3, "Box and Whisker" = 4, "Histogram" = 5), 
              selected = 1),
  
  textInput("GraphTitle", "Graph Title",
            value="Graph Title"
  ),
  
  div(style="display:inline-block",
      textInput("x_axis", "X axis label",
                value=""
      )),
  
  div(style="display:inline-block",
      textInput("y_axis", "Y axis label",
                value=""
      )),
  
  div(),
  
  checkboxInput("axes", "Modify Axis limits"),
  conditionalPanel(
    condition = "input.axes == true",
    div(style="display:inline-block",
        numericInput("x_min", "x axis minimum",
                     value=""
        )),
    
    div(style="display:inline-block",
        numericInput("x_max", "x axis maximum",
                     value=""
        )),
    
    
    div(),
    
    div(style="display:inline-block",
        numericInput("y_min", "y axis minimum",
                     value=""
        )),
    
    div(style="display:inline-block",
        numericInput("y_max", "y axis maximum",
                     value=""
        ))
    
  ),
  

  div(),
  
  conditionalPanel(
    condition = "input.select == 2 || input.select == 4",
      div(style="display:inline-block",
          sliderInput("variation", "Scatter Amount", 0, 1, 0.15
          )),
      
      div(style="display:inline-block",
          sliderInput("point_size", "Point Size", 1, 5, 1.5, 0.1
          ))
  ),
  
 
  
  div(),
  
  conditionalPanel(
    condition = "input.select == 3",
    checkboxInput("boxplot_color", "Color-code box plot", FALSE)
    ),
  
  mainPanel(
    
    plotOutput(outputId = "PlotArea"),
    
    plotOutput(outputId = "TextArea")
    
  )
  
)

server <- function(input, output, session) {
  
  
  library(shiny)
  
  
  
  observe ({

    ############################# Removed for debugging #############################
    
    #    infile <- input$inputFile
 #   if (is.null(infile)) {
  #    # if no file uploaded
   #   return(NULL)
  #    
  #  }
    
    
    
    #read the input datafile
   # dfs <- stack(read.csv(infile$datapath))
    
    ############################# Removed for debugging #############################
    
  dfs <- stack(read.csv("MOCK_DATA.csv"))
    
    
    output$TextArea <- renderText({
      
      "test text 22"
      
    })
    
    
    #make a density plot
    if (input$select == 1) {
      
      output$PlotArea <- renderPlot({
        
        
        updateTextInput(session, "y_axis", label = "Y axis label", value = "Density")
        updateTextInput(session, "x_axis", label = "X axis label", value = "Distance")
        
        
        if (!isTruthy(input$x_min) || !isTruthy(input$x_max) || !isTruthy(input$y_min) || !isTruthy(input$y_max)) {
          density_plot <- ggplot(dfs, aes(x=values)) + geom_density(aes(group=ind, colour=ind, fill=ind), alpha=0.3) + labs(x = input$x_axis) + labs(y = input$y_axis) + labs(title = input$GraphTitle) + theme(legend.title=element_blank())
          show(density_plot)
        } else {
          density_plot <- ggplot(dfs, aes(x=values)) + geom_density(aes(group=ind, colour=ind, fill=ind), alpha=0.3) + labs(x = input$x_axis) + labs(y = input$y_axis) + labs(title = input$GraphTitle) + theme(legend.title=element_blank()) + xlim(input$x_min, input$x_max) +  ylim(input$y_min, input$y_max)
          show(density_plot)
        }
        
      })
      
      
      #make a scatter plot    
    }else if (input$select == 2) {
      
      
      
      output$PlotArea <- renderPlot({
        
        
        updateTextInput(session, "y_axis", label = "Y axis label", value = "")
        updateTextInput(session, "x_axis", label = "X axis label", value = "Distance")
        
        y <- rnorm(nrow(dfs), mean=-0.0, sd=input$variation)
        
        if (!isTruthy(input$x_min) || !isTruthy(input$x_max)) {
          ggplot(dfs, aes(values, y)) + geom_point(aes(group=ind, colour=ind, fill=ind), size=input$point_size, alpha = (1-(input$point_size / 8))) + labs(x = input$x_axis) + labs(title = input$GraphTitle) + theme(legend.title=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank()) + ylim(-2, 2) 
          
        }else{
          ggplot(dfs, aes(values, y)) + geom_point(aes(group=ind, colour=ind, fill=ind), size=input$point_size) + labs(x = input$x_axis, alpha = (1 - (input$point_size / 8))) + labs(title = input$GraphTitle) + theme(legend.title=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks=element_blank()) + ylim(-2, 2) + xlim(input$x_min, input$x_max)
          
        }
      })
      
      #make a box-and-whisker plot  
    }else if (input$select == 3) {
      
      output$PlotArea <- renderPlot({
        
        
        updateTextInput(session, "y_axis", label = "Y axis label", value = "Density")
        updateTextInput(session, "x_axis", label = "X axis label", value = "Distance")
        
        
        if (!isTruthy(input$x_min) || !isTruthy(input$x_max) || !isTruthy(input$y_min) || !isTruthy(input$y_max)) {
          
          
          density_plot <- ggplot(dfs, aes(x=values)) + geom_density(aes(group=ind, colour=ind, fill=ind), alpha=0.3) + labs(x = input$x_axis) + labs(y = input$y_axis) + labs(title = input$GraphTitle) + theme(legend.title=element_blank())
          density_plot + geom_point(aes(x=values, y=0, group=ind, colour=ind, fill=ind), position = position_jitter(height=0.1))
          
        } else {
          density_plot <- ggplot(dfs, aes(x=values)) + geom_density(aes(group=ind, colour=ind, fill=ind), alpha=0.3) + labs(x = input$x_axis) + labs(y = input$y_axis) + labs(title = input$GraphTitle) + theme(legend.title=element_blank()) + xlim(input$x_min, input$x_max) +  ylim(input$y_min, input$y_max)
          
        }
        
        
        show(density_plot)
      })
      
    }else if (input$select == 4) {
      
      updateTextInput(session, "y_axis", label = "Y axis label", value = "Distance")
      updateTextInput(session, "x_axis", label = "X axis label", value = "Neuromast")
      
      output$PlotArea <- renderPlot({
        
        if (input$boxplot_color == 0) {
        
        ggplot(dfs, aes(y = values, x = ind)) + geom_boxplot() + labs(y = input$y_axis, x = input$x_axis)
        
        }else {
          ggplot(dfs, aes(y = values, x = ind, color=ind)) + geom_boxplot() + labs(y = input$y_axis, x = input$x_axis)
        }
          
      })
      
    }
    
  })
  
  
}

shinyApp(ui = ui, server = server)