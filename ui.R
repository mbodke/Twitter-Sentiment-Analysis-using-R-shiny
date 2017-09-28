# Functions for creating fluid page layouts in shiny Application.
# A fluid page layout consists of rows which in turn include columns
################################################################################
ui = fluidPage(
  
  #Setting a theme for the Shiny app| The theme that is used here is called 'darkly'
  #package used here is shinytheme
  theme = shinytheme("darkly"),
  # Defining the header Panel on the shiny application 
  #h3- argument is used to obtain a specific size for the header/ title.
  #windowTitle - The title that should be displayed by the browser window. 
  headerPanel(h3("Twitter Sentiment Analysis: Team abs(R)"), windowTitle = "abs(R)"),
  #Sidebar Layout - used to create a layout with a sidebar and main area in the Shiny Aplication.
  sidebarLayout(
    #Create a sidebar panel containing input controls that can in turn be passed to sidebarLayout.
    #img argument is used to load an image into the sodeba panel of the shiny Application
    sidebarPanel(
                 # radioButtons -Create a set of radio buttons used to select an item from a list.          
                 radioButtons("typeInput", "Extract tweets by: ",
                              list("Hashtag & Location" = "hashtag", "Twitter Username"= "username")),
                 #sliderInput -Constructs a slider widget to select a numeric value from a range.
                 sliderInput("numberInput", "Select number of tweets",
                             min = 0, max = 3000, value = 100),
                 #Creates a panel that is visible or not, depending on the value of the input.
                 #Condition 1 - Only show this panel if input type is "Hashtag & Location"    
                 conditionalPanel(
                   condition = "input.typeInput == 'hashtag'",
                   #Create an input control for entry of unstructured text values
                   textInput("hashtagInput", "Enter search string","", placeholder = "input search string"),
                   textInput("zipInput", "Enter zipcode (from 00210 to 99950)", placeholder = "input ZIP code"),
                   textInput("radiusInput", "Enter radius (miles)", placeholder = "input miles")),
                 
                 #Condition 2 - Only show this panel if Input type is "Twitter Username"
                 conditionalPanel(
                   condition = "input.typeInput == 'username'",
                   textInput("usernameInput", "Username", placeholder = "input username")),
                 #actionButton - Used to create a go button, that allows the shiny Application the execute the input
                   actionButton("goButton", "Search", icon("twitter"),
                                style="color: #fff; background-color: #337ab7") ,width = 3),
    
    #Panel to display output
    # mainPanel - Create a main panel containing output elements that can in turn be passed to sidebarLayout.
    mainPanel(
      #Tabsets - used for dividing output into multiple independently viewable sections.
      #Dividing the main panel into multiple tabs
      tabsetPanel(
        #tabPanel - Create a tab panel that can be included within a tabsetPanel.
        #Argument plotOutput - used to create a plot as an output element based on the inputid that is passed to it
        tabPanel("Sentiment Plots TM", plotOutput("plot1")),
        tabPanel("Sentiments Plots TFIDF", plotOutput("plot3")),
        tabPanel("+/- Plots TM", plotOutput("plot2")),
        tabPanel("+/- Plots TFIDF", plotOutput("plot4")),
        #navbarMenu - Creates a page that contains a top level navigation bar that can be used to toggle a set of tabPanel elements.
        navbarMenu("Word Clouds TM",
                   #tabPanel - Create a tab panel that can be included within a tabsetPanel
                   tabPanel("Positive", wordcloud2Output("wordCloud1", width = "100%", height = "400px")),
                   #wordcloud2Output -used to render a wordcloud object| uses library - wordcloud2
                   tabPanel("Negative", wordcloud2Output("wordCloud2", width = "100%", height = "400px"))),
        navbarMenu("Word Clouds TFIDF",
                   tabPanel("Positive", wordcloud2Output("wordCloud3", width = "100%", height = "400px")),
                   tabPanel("Negative", wordcloud2Output("wordCloud4", width = "100%", height = "400px"))),
        #dataTableOutput -used to render a table as an output
        tabPanel("Tweets", dataTableOutput("tweetTable"))
        ,type = "pills"), width = 9)
  )
)

