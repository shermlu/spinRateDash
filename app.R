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

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Spin Rate Comparison"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        'additionalData',
        'Want to import additional data?',
        c('Yes' = 'yes', 'No' = 'no')
      ),
      fileInput('add_file', "Add Additional Data"),
      radioButtons(
        'hist_type',
        'Select Histogram Type',
        c('Counts' = 'count', 'Density' = 'density')
      ),
      sliderInput(
        "limits",
        "Histogram Limits (x-axis):",
        min = -1000,
        max = 1000,
        value = c(-1000, 1000)
      ),
      sliderInput(
        "bins",
        "Number of bins:",
        min = 20,
        max = 200,
        value = 30
      ),
      actionButton('go_button', 'Update Visuals')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel('README',
          h4('README:'),
          textOutput('readme')
        ),
        tabPanel('Data Tab',
         plotOutput('distHist'),
         textOutput('text'),
         br(),
         htmlOutput('testStats'),
         br()
        )
    )
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  plots <- reactiveValues(all=NULL, original=NULL, new=NULL)
  
  #plots$original = 
  
  get_data <- reactive({
    inFile <- input$add_file
    if (input$additionalData == 'yes'){
      if (is.null(inFile)){
        return(NULL)
      }
      df = read.csv(inFile$datapath, header=TRUE)
    } else {
      df = read.csv("./spin_evaluation.csv", header = TRUE)
    }
    return (df)
  })

  output$distHist <- renderPlot({
    if (input$additionalData == 'yes'){
      validate(need(!is.null(get_data()),'No additional csv file uploaded.'))
    }
    data = get_data()
    data$difference = data$new_tech_spin - data$baseline_spin
    if (input$additionalData == 'yes'){
      old_data = read.csv("./spin_evaluation.csv", header = TRUE)
      old_data$difference = old_data$new_tech_spin - old_data$baseline_spin
    }
    base_plot = ggplot(data, aes(x = difference)) + xlim(input$limits[1], input$limits[2]) +
      xlab("Difference in Spin Rate (New - Current)")
    if (input$hist_type == 'density') {
      if (input$additionalData == 'yes'){
        base_plot + geom_density(data = data, color = '#EF2D56', fill = '#EF2D56', alpha = 0.2, 
                                 (aes(y = ..density..))) + ylab("Density") + 
                    geom_density(data = old_data, color = '#3C7BBF', fill = '#3C7BBF', alpha = 0.2)
      } else {
        base_plot + geom_density(color = '#EF2D56', fill = '#EF2D56', alpha = 0.2, 
                              (aes(y = ..density..))) + ylab("Density")
      }
    } else {
      if (input$additionalData == 'yes'){
        base_plot + geom_histogram( color = '#EF2D56', fill = '#EF2D56', alpha = 0.2, 
                                    bins = input$bins) + ylab("Frequency") + 
                    geom_histogram(data=old_data, color = '#3C7BBF', fill = '#3C7BBF', alpha = 0.2) +
          geom_vline(aes(xintercept=mean(difference)), 
                     color="#0CCE6B", size=1, linetype='dashed') + 
          geom_vline(aes(xintercept=median(difference)), 
                     color="#0CCE6B", size=1) +
          geom_vline(aes(xintercept=mean(old_data$difference)), 
                     color="#363537", size=1, linetype='dashed') + 
          geom_vline(aes(xintercept=median(old_data$difference)), 
                     color="#363537", size=1)
      } else {
      base_plot + geom_histogram( color = '#EF2D56', fill = '#EF2D56', alpha = 0.2, 
                                  bins = input$bins) + ylab("Frequency") + 
                  geom_vline(aes(xintercept=mean(difference)), 
                             color="#0CCE6B", size=1, linetype='dashed') + 
                  geom_vline(aes(xintercept=median(difference)), 
                             color="#0CCE6B", size=1)
      }
    }
  })
  
  output$text <- renderText({
    validate(need(!is.null(get_data()),''))
    'Legend: The red represents data added by the user. The green represents the provided data.
      The dashed lines represent the mean values of the difference in spin rate, and the solid
      lines represent the median values. The black vertical line indicates the additional 
      dataset that was provided by the user, and the green vertical line indicates the built-in 
    dataset.'
  })
  output$readme <- renderText({
    'To properly run this application, please select on the left hand side if you would
    like to upload additional data. If you want to upload data, select browse under the 
    "Add additional data" heading. Be sure that the data is formatted with two columns, with 
    one named "new_tech_spin" and the other named "baseline_spin". Two histograms or density 
    curves will appear, depending on which option you want to view. The first slider sets the 
    histogram axis. The limits are set from -1000 to 1000, but they may be made narrower if 
    desired. The second slider sets the number of bins for the histogram. The default is 30, 
    but you may chooseanywhere from 20 to 200 bins. All visualizations appear in the "Data Tab" 
    above.'
  })
  output$testStats <- renderUI({
    validate(need(!is.null(get_data()),''))
    data = read.csv("./spin_evaluation.csv", header = TRUE)
    data$difference = data$new_tech_spin - data$baseline_spin
    #data_test = t.test(data$difference)
    if (input$additionalData == 'yes'){
      new_data = get_data()
      new_data$difference = new_data$new_tech_spin - new_data$baseline_spin
      data = rbind(data, new_data)
    }
    data_test = t.test(data$difference)
    line1 = 'Hypothesis testing: '
    line2 = 'Ho: mean difference = 0'
    line3 = 'Ha: mean difference ≠ 0'
    line4 = paste("p-value of all data: ", round(data_test$p.val, digits=5))
    line5 = 'A p-value less than 0.05 suggests that there is enough evidence to reject the null
           hypothesis.'
    line6 = 'A p-value greater than 0.05 suggests that there is insufficient evidence to reject the
    null hypothesis. Therefore, you fail to reject the null hypothesis.'
    HTML(paste(line1, line2, line3, line4, line5, line6, sep='<br/>'))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
