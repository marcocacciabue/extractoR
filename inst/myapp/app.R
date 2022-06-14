

library(shiny)

# Define UI for application that plots random distributions
ui <-shinyUI(fluidPage(

  # Application title
  titlePanel("Hello Shiny!"),

  # Sidebar with a slider input for number of observations
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Submit a file with your query sequences in FASTA format",
                accept = c(".text",".fasta",".fas",".fasta")),
      actionButton("go", "RUN"),

    ),

    # Show a plot of the generated distribution
    mainPanel(
      msaR::msaROutput("msa", width="100%"),
    )
  )
))



# Define server logic required to generate and plot a random distribution
server<- shinyServer(function(input, output) {

  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  #

  data_reactive<- eventReactive(input$go,{
    sample_data<-Biostrings::readDNAStringSet(input$file[1,4],format="fasta")
    sample_data_reduced_10_30<-extractoR::extract_multi_gap(sample_data,10,30)

    list(message="Done!",sample_data_reduced_10_30=sample_data_reduced_10_30)
  })

  output$msa <- msaR::renderMsaR(
    msaR::msaR(data_reactive()$sample_data_reduced_10_30)
  )


})

shinyApp(ui = ui, server = server)

