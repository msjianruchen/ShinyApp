#ShinyApp - Get Started
#install.packages("shiny")
#create user interface in ui.R file: Control the layout, appearance, widgets that capture user innputs, displays the output. Eg, the title, page layout, text input, radio buttons, graphics etc.
#Create the server.R file for computation purpose: Set of instructions that uses the input provided by the user, process them and produces the required output. 


# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

#Structure of ui.r
#Difine UI for the shiny application.
shinyUI(fluidPage(
  #Application title
  titlePanel(),
  sidebarLayout(
  #Sidebar panel
  sidebarPanel(),
  #Main Panel(eg, graphs, barcharts,using output from server.r)
  mainPanel()
)
  )
)


#Structure of server.r
shinyServer(
  function(input,output){
    
  }
    
)

#Save the files in the working directory & run the app

####Simple Example
#(1)Check working directory, if is not what you want, then setwd()
getwd()
#(2) Define UI
shinyUI(fluidPage(
  
  titlePanel(),
  sidebarLayout(
    sidebarPanel(),
    mainPanel()
    
  )
)
)





# Load the shiny package
library(shiny)

# Define UI for the application
ui <- fluidPage(
  # Add a sidebar layout to the application
  sidebarLayout(
    # Add a sidebar panel around the text and inputs
    sidebarPanel(
      h4("Plot parameters"),
      textInput(inputId ="title", label = "Plot title",value = "Car speed vs distance to stop"), # inputId=Unique ID, used to retrieve the value of the input.
      numericInput("num", "Number of cars to show", 30,min = 1, max= nrow(cars)),
      sliderInput("size", "Point size", 1, 5, 2, 0.5)
    ),
    # Add a main panel around the plot and table
    mainPanel(
      em("Plot goes here:"),
      plotOutput(outputId= "plot"),
      tableOutput("table")
    )
  )
)
# Define the server logic
# Outputs: Plots, tables, text - anything R creates & users see
# Build object inside render function (renderPlot(),renderText(),etc); Save object to output$<outputTd>
server <- function(input, output) {
  output$plot <- renderPlot({
    plot(cars[1:input$num, ], main = input$title, cex = input$size) # Use input$<inputId> to access value of input.
  })
  output$table <- renderTable({
    cars[1:input$num, ]
  })
}
# Run the application
shinyApp(ui = ui, server = server)



# Reactivity Basics: Outputs react to changes in input: When value of variable X changes, anything relies on X is re-evaluated.
# Any render*() function is a reactive context, other than that, we should use observe({...})
# observe({ ... }) to access reactive variable
server <- function(input,output){
  observe({
      print(input$num)
  })
}
# reactive({ ... }) to create reactive variable
server <- function(input,output){
  x <- reactive({
      input$num +1
  })
  observe({
    print(input$num)
    print( x() )
  })
}

### Make the perfect plot using Shiny
# Load the ggplot2 package for plotting
library(ggplot2)
# Define UI for the application
uui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Add a title text input
      textInput("title", "Title", "GDP vs life exp")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)
# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(gapminder, aes(gdpPercap, lifeExp)) +
      geom_point() +
      scale_x_log10() +
      # Use the input value as the plot's title
      ggtitle(input$title)
  })
}
shinyApp(ui = ui, server = server)




# Define UI for the application
# Checkbox inputs are limited to only two possible values: TRUE or FALSE.
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      numericInput("size", "Point size", 1, 1),
      # Add a checkbox for line of best fit
      checkboxInput("fit","Add line of best fit",value=FALSE)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)
# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    p <- ggplot(gapminder, aes(gdpPercap, lifeExp)) +
      geom_point(size = input$size) +
      scale_x_log10() +
      ggtitle(input$title)
# When the "fit" checkbox is checked, add a line of best fit
#Add code to the server so that when the input is checked, a line of best fit is added to the plot. 
    if (input$fit==TRUE) {
      p <- p + geom_smooth(method = "lm")
    }
    p
  })
}
shinyApp(ui = ui, server = server)


#More input types
#Slider inputs
sliderInput("slider","choose a number",value= 15, min= 5, max= 20)
sliderInput("slider","choose a number",value= c(10,15), min= 5, max= 20)
radioButtons("radio","choose your favourite time of the day",choices = c("Morning","Afternoon","Evening"),selected = "Afternoon")
selectInput("select","choose your favourite time of the day",choices = c("Morning","Afternoon","Evening"),selected = "Afternoon")
selectInput("select","choose your favourite time of the day",choices = c("Morning","Afternoon","Evening"),selected = c("Afternoon","Evening"),multiple=TRUE)


library(colourpicker)
colourInput("colour", "Point colour","blue")

# Define UI for the application
library(shiny)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      numericInput("size", "Point size", 1, 1),
      checkboxInput("fit", "Add line of best fit", FALSE),
      # Add radio buttons for colour
      radioButtons("colour", "Point colour", c("blue","red","green","black"), selected="blue")),
    mainPanel(
      plotOutput("plot",width ="100%", height = "100px",
                 click = NULL, dblclick= NULL, hover = NULL,
                 hoverDelay = NULL, hoverDelayType = NULL,
                 brush = NULL, clickId = NULL, hoverId = NULL,
                 inline = FALSE)
    )
  )
)
server <- function(input, output) {
  output$plot <- renderPlot({
    p <- ggplot(gapminder, aes(gdpPercap, lifeExp)) +
      # Use the value of the colour input as the point colour
      geom_point(size = input$size, col = input$colour) +
      scale_x_log10() +
      ggtitle(input$title)
    
    if (input$fit) {
      p <- p + geom_smooth(method = "lm")
    }
    p
  })
}

shinyApp(ui = ui, server = server)


##Add a year filter: numeric slider input
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      numericInput("size", "Point size", 1, 1),
      checkboxInput("fit", "Add line of best fit", FALSE),
      radioButtons("colour", "Point colour",
                   choices = c("blue", "red", "green", "black")),
      selectInput("continents", "Continents",
                  choices = levels(gapminder$continent),
                  multiple = TRUE,
                  selected = "Europe"),
      # Add a slider selector for years to filter
      sliderInput("years", "Years", value=c(1977,2002), min(gapminder$year), max(gapminder$year))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    # Subset the gapminder data by the chosen years
    data <- subset(gapminder,
                   continent %in% input$continents &
                     year >= input$years[1] & year <= input$years[2])
    
    p <- ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point(size = input$size, col = input$colour) +
      scale_x_log10() +
      ggtitle(input$title)
    
    if (input$fit) {
      p <- p + geom_smooth(method = "lm")
    }
    p
  })
}
shinyApp(ui = ui, server = server)

### Advanced features to improve your plot
# Inetractive plots: plotly() package.
# Load the plotly package to zoom in / zoom out
library(plotly)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title", "GDP vs life exp"),
      numericInput("size", "Point size", 1, 1),
      checkboxInput("fit", "Add line of best fit", FALSE),
      colourInput("colour", "Point colour", value = "blue"),
      selectInput("continents", "Continents",
                  choices = levels(gapminder$continent),
                  multiple = TRUE,
                  selected = "Europe"),
      sliderInput("years", "Years",
                  min(gapminder$year), max(gapminder$year),
                  value = c(1977, 2002))
    ),
    mainPanel(
      # Replace the `plotOutput()` with the plotly version
      plotlyOutput("plot")
    )
  )
)
# Define the server logic
server <- function(input, output) {
  # Replace the `renderPlot()` with the plotly version
  output$plot <- renderPlotly({
    # Convert the existing ggplot2 to a plotly plot
    ggplotly({
      data <- subset(gapminder,
                     continent %in% input$continents &
                       year >= input$years[1] & year <= input$years[2])
      
      p <- ggplot(data, aes(gdpPercap, lifeExp)) +
        geom_point(size = input$size, col = input$colour) +
        scale_x_log10() +
        ggtitle(input$title)
      
      if (input$fit) {
        p <- p + geom_smooth(method = "lm")
      }
      p
    })
  })
}
shinyApp(ui = ui, server = server)

### Shiny Data Expoler:
tableOutput("my_table") #Output use output placeholder functions in UI
output$my_table <- renderTable({    #Outputs use render functions in the server
  dapminder
})
#To filter table data:
selectInput("country","Country",choices = levels(gapminder&country)[1:10]) #In ui
# selectInput("country","Country",choices = c("any",levels(gapminder&country)))    to add new levels
output$my_table <- renderTable({
  subset(gapminder,country==input$country)    #In server
})


### Plot Data
#Downloading is supported using download button (treated as output): can create any type od file to download
downloadButton(outputId="download_data", label = "Download data") #In ui
output$download_data <- downloadHandler(  #In server
  filename = "data.csv",
  content= function(file){
    #Code that creates a file in the path <file>
    write.csv(gapminder,file)
  }
)             

## Exercise (with Code Duplication case)
ui <- fluidPage(
  h1("Gapminder"),
  sliderInput(inputId = "life", label = "Life expectancy",
              min = 0, max = 120,
              value = c(30, 50)),
  selectInput("continent", "Continent",
              choices = c("All", levels(gapminder$continent))),
  # Add a download button
  downloadButton(outputId = "download_data", label = "Download"),
  plotOutput("plot"),
  tableOutput("table")
)
server <- function(input, output) {
  output$table <- renderTable({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    data
  })
  # Create a download handler
  output$download_data <- downloadHandler(
    # The downloaded file is named "gapminder_data.csv"
    filename = "gapminder_data.csv",
    content = function(file) {
      # The code for filtering the data is copied from the
      # renderTable() function
      data <- gapminder
      data <- subset(
        data,
        lifeExp >= input$life[1] & lifeExp <= input$life[2]
      )
      if (input$continent != "All") {
        data <- subset(
          data,
          continent == input$continent
        )
      }
      # Write the filtered data into a CSV file
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$plot <- renderPlot({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point() +
      scale_x_log10()
  })
}
shinyApp(ui, server)

### Reactive Variables reduce code duplication: reactive() to cache their values. Do not run again if dependencies didn't change
my_data <- reactive({
  data <- gapminder
  data <- subset(data,lifeExp >= input$life[1] & lifeExp <= input$life[2])
  if (input$continent != "All") {
    data <- subset(
      data,
      continent == input$continent
    )
  }
})
 output$table <- renderTable({
   my_data()
 })
 ### Visual Enhancements
 # make better tables with DT
 DT::dataTableOutput("table")
 output$table <- DT::renderDataTable({
   gapminder
 })
 #Split the ui into tabs
 ui <- fluidPage(
   tabsetPanel(
   tabPanel(title = "tab 1","content goes here"),
    tabPanel(title = "tab 2","content goes here",plotoutput("plot")),
     tabPanel(title = "tab 1",textInput("text","Name",""))
           )
      )
 # CSS: Fine-une your app's look (Cascading Style Sheets). e.g. Background colour, text colour, text size, whitespace,fonts...
 # CSS syntax:
ui <- fluidPage(
  tags$style("
    #ID{
    property: value;
    property: valur;
    }
  ")
)
# CSS example
css <- "
#name {
color:red;
}
#table {
background: yellow;
font-size:  24px;
}"
ui <- fluidPage(
  tags$style(css),
  textInput("name","Enter your name","Dean"),
  tableInput("table")
) 
### Example
ui <- fluidPage(
  h1("Gapminder"),
  # Create a container for tab panels
  tabsetPanel(
    # Create an "Inputs" tab
    tabPanel(
      title = "Inputs",
      sliderInput(inputId = "life", label = "Life expectancy",
                  min = 0, max = 120,
                  value = c(30, 50)),
      selectInput("continent", "Continent",
                  choices = c("All", levels(gapminder$continent))),
      downloadButton("download_data")
    ),
    # Create a "Plot" tab
    tabPanel(
      title = "Plot",
      plotOutput("plot")
    ),
    # Create "Table" tab
    tabPanel(
      title = "Table",
      DT::dataTableOutput("table")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    data
  })
  output$table <- DT::renderDataTable({
    data <- filtered_data()
    data
  })
  output$download_data <- downloadHandler(
    filename = "gapminder_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  output$plot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point() +
      scale_x_log10()
  })
}
shinyApp(ui, server)
 
### Example
my_css <- "
#download_data {
/* Change the background colour of the download button
to orange. */
background: orange;
/* Change the text size to 20 pixels. */
font-size: 20px;
}
#table {
/* Change the text colour of the table to red. */
color: red;
}"
ui <- fluidPage(
  h1("Gapminder"),
  # Add the CSS that we wrote to the Shiny app
  tags$style(my_css),
  tabsetPanel(
    tabPanel(
      title = "Inputs",
      sliderInput(inputId = "life", label = "Life expectancy",
                  min = 0, max = 120,
                  value = c(30, 50)),
      selectInput("continent", "Continent",
                  choices = c("All", levels(gapminder$continent))),
      downloadButton("download_data")
    ),
    tabPanel(
      title = "Plot",
      plotOutput("plot")
    ),
    tabPanel(
      title = "Table",
      DT::dataTableOutput("table")
    )
  )
)

server <-zc {
  filtered_data <- reactive({
    data <- gapminder
    data <- subset(
      data,
      lifeExp >= input$life[1] & lifeExp <= input$life[2]
    )
    if (input$continent != "All") {
      data <- subset(
        data,
        continent == input$continent
      )
    }
    data
  })
  output$table <- DT::renderDataTable({
    data <- filtered_data()
    data
  })
  output$download_data <- downloadHandler(
    filename = "gapminder_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  output$plot <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(gdpPercap, lifeExp)) +
      geom_point() +
      scale_x_log10()
  })
}
shinyApp(ui, server)


### Word Clouds
## Visual representation of text: BIG WORDS = COMMON, small words = rare
library(wordcloud2)
ui <- fluidPage(
  h1("Word Cloud"),
  # Add a numeric input for the number of words
  numericInput(inputId = "num", label = "Maximum number of words",
               value = 100, min = 5),
  # Add a colour input for the background colour
  colourpicker::colourInput("col","Background colour","white"),
  wordcloud2Output("cloud")
)
server <- function(input, output) {
  output$cloud <- renderWordcloud2({
    # Use the values from the two inputs as
    # parameters to the word cloud
    create_wordcloud(artofwar,
                     num_words = input$num, background = input$col)
  })
}
shinyApp(ui = ui, server = server)
 

# Exercise (2)
ui <- fluidPage(
  h1("Word Cloud"),
  # Add a sidebar layout to the UI
  sidebarLayout(
    # Define a sidebar panel around the inputs
    sidebarPanel(
      numericInput("num", "Maximum number of words",
                   value = 100, min = 5),
      colourInput("col", "Background colour", value = "white")
    ),
    # Define a main panel around the output
    mainPanel(
      wordcloud2Output("cloud")
    )
  )
)
server <- function(input, output) {
  output$cloud <- renderWordcloud2({
    create_wordcloud(artofwar,
                     num_words = input$num, background = input$col)
  })
}
shinyApp(ui = ui, server = server)
 

## Adding Word Sources

# Text input
textAreaInput(inputId,label,value,row) #Provides multiple row for text input box 
# File input: uploading a file
fileInput(inputId,label,...) #File inputs: input$<inputId> is NOT a file, is dataframe with 1 row per file
#File input variables: name, size, type, datapath

#Exercise
ui <- fluidPage(
  h1("Word Cloud"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("text", "Enter text", rows = 7),
      # Add a file input
      fileInput("file","Select a file"),
      numericInput("num", "Maximum number of words",
                   value = 100, min = 5),
      colourInput("col", "Background colour", value = "white")
    ),
    mainPanel(
      wordcloud2Output("cloud")
    )
  )
)

server <- function(input, output) {
  output$cloud <- renderWordcloud2({
    create_wordcloud(input$text, num_words = input$num,
                     background = input$col)
  })
}

shinyApp(ui = ui, server = server)
 
#Exercise 2
ui <- fluidPage(
  h1("Word Cloud"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("text", "Enter text", rows = 7),
      fileInput("file", "Select a file"),
      numericInput("num", "Maximum number of words",
                   value = 100, min = 5),
      colourInput("col", "Background color", value = "white")
    ),
    mainPanel(
      wordcloud2Output("cloud")
    )
  )
)
server <- function(input, output) {
  # Define a reactive variable named `input_file`
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    # Read the text in the uploaded file
    readLines(input$file$datapath)
  })
  
  output$cloud <- renderWordcloud2({
    # Use the reactive variable as the word cloud data source
    create_wordcloud(data = input_file(), num_words = input$num,
                     background = input$col)
  })
}
shinyApp(ui = ui, server = server)
 
### Combine all the word sources use radioButtons()
ui <- fluidPage(
  h1("Word Cloud"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "source",label = "Word source",choices = c(
          "Art of War" = "book",
          "Use your own words" = "own",
          "Upload a file" = "file" )
      ),
      textAreaInput("text", "Enter text", rows = 7),
      fileInput("file", "Select a file"),
      numericInput("num", "Maximum number of words",
                   value = 100, min = 5),
      colourInput("col", "Background color", value = "white") ),
    mainPanel(
      wordcloud2Output("cloud")
    )
  )
)
server <- function(input, output) {
  # Create a "data_source" reactive variable
  data_source <- reactive({
    # Return the appropriate data source depending on
    # the chosen radio button
    if (input$source == "book") {
      data <- artofwar
    } else if (input$source == "own") {
      data <- input$text
    } else if (input$source == "file") {
      data <- input_file()
    }
    return(data)
  })
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })
  output$cloud <- renderWordcloud2({
    # Use the data_source reactive variable as the data
    # in the word cloud function
    create_wordcloud(data = data_source(), num_words = input$num,
                     background = input$col)
  })
}
shinyApp(ui = ui, server = server)
### Conditional panels: Show/hide UI elements based on input value use
conditionalPanel(condition="input.time_of_day == 'Morning'",...) #condition is input.<id> instead of input$<id>

### Fine tune the reactivity
#reactive() and input$ are reactive
Isolate()   # Use isolate() to not create reactive dependency
x <- reactive({
   y()*isolate({ input$num1 })* input$num2
})

### TRIGGER X TO RE-RUN: 
#In ui
actionButton(inputId = "calculate_x", label = "Calculate x!",...) #reacticity triggers
#In server
x<- reactive({
  input$calculate_x
  isolate({
    y()*input$num1*input$num2
  })
})

# Exercise
ui <- fluidPage(
  h1("Word Cloud"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "source",
        label = "Word source",
        choices = c(
          "Art of War" = "book",
          "Use your own words" = "own",
          "Upload a file" = "file"
        )
      ),
      conditionalPanel(
        condition = "input.source == 'own'",
        textAreaInput("text", "Enter text", rows = 7)
      ),
      conditionalPanel(
        condition = "input.source == 'file'",
        fileInput("file", "Select a file")
      ),
      numericInput("num", "Maximum number of words",
                   value = 100, min = 5),
      colourInput("col", "Background color", value = "white"),
      # Add a "draw" button to the app
      actionButton(inputId = "draw", label = "Draw!")
    ),
    mainPanel(
      wordcloud2Output("cloud")
    )
  )
)
server <- function(input, output) {
  data_source <- reactive({
    if (input$source == "book") {
      data <- artofwar
    } else if (input$source == "own") {
      data <- input$text
    } else if (input$source == "file") {
      data <- input_file()
    }
    return(data)
  })
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })
  output$cloud <- renderWordcloud2({
    # Add the draw button as a dependency to
    # cause the word cloud to re-render on click
    input$draw
    isolate({
      create_wordcloud(data_source(), num_words = input$num,
                       background = input$col)
    })
  })
}
shinyApp(ui = ui, server = server)


# Dashboard Overview --------------------------------------------
### Structure: Header; Sidebar; Body
library(shinydashboard)
header<- dashboardHeader()
sidebar<- dashboardSidebar()
body<- dashboardBody()
ui<- dashboardPage(header,sidebar,body) #Combine the dashboard 3 pieces above

### Header: 3 types of drop down value: Messages;Notifications;Tasks
header<- dashboardHeader(
  dropdownMenu(type="messages",
               messageItem(
                 from="Angela",
                 message="Check out datacamp!",
                 href= "http://www.datacamp.com" #An optional URL to link to
               ) #Add another messageItem() if we want to add another message
           ) )

header<- dashboardHeader(
  dropdownMenu(type="notifications",
               notificationItem(
                 from="Angela",
                 text="Check out datacamp again!",
                 href= "http://www.datacamp.com" #An optional URL to link to
               ) ))

header<- dashboardHeader(
  dropdownMenu(type="tasks",
               taskItem(
                 text="Check out your datacamp progress!",
                 value= 15
               ) ))

# Exercise 1
header <- dashboardHeader(
  dropdownMenu(
    type = "messages",
    messageItem(
      from = "Lucy",
      message = "You can view the International Space Station!",
      href = "https://spotthestation.nasa.gov/sightings/"
    ),
    # Add a second messageItem() 
    messageItem(
      from = "Lucy",
      message = "Learn more about the International Space Station",
      href = "https://spotthestation.nasa.gov/faq.cfm"
    )  ))
ui <- dashboardPage(header = header,
                    sidebar = dashboardSidebar(),
                    body = dashboardBody()
)
shinyApp(ui, server)

### Siderbar & Body:

sidebar<- dashboardSidebar(
  sidebarMenu(
    menuItem(text= "Data",
              tabName="data"),
    menuItem(text= "Dashboard",
             tabName="dashboard") #tabName to link the tab in the body
    ))

body<- dashboardBody(
  tabItems(
    tabItem(tabName="data","Input data here"),
    tabItem(tabName="dashboard","Look at my cool dashboard")
  )
)

body<- dashboardBody(
  tabBox(
    title="My first box",
    tabPanel("Tab1","Content for the first tab"),
    tabPanel("Tab2","Content for the second tab")
  )
)

# Exercise 2
library("shiny")
body <- dashboardBody(
  # Create a tabBox
  tabItems(
    tabItem(
      tabName = "dashboard",
      tabBox(
        title="International Space Station Fun Facts",
        tabPanel("Fun Fact 1"),
        tabPanel("Fun Fact 2")
      ),
      tabItem(tabName = "inputs")
    )
  ))
ui <- dashboardPage(header = dashboardHeader(),
                    sidebar = sidebar,
                    body = body
)
shinyApp(ui, server)













 

