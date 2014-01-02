
shinyUI(fluidPage(
  titlePanel("Whisky tastings"),
  sidebarLayout(
    sidebarPanel(
      HTML("This app is based on the article 
<a href=http://blog.revolutionanalytics.com/2013/12/k-means-clustering-86-single-malt-scotch-whiskies.html>
K-means clustering 86 Single Malt Scotch Whiskeys</a>\n"),
      HTML("As in the original article, the data is from 
<a href>https://www.mathstat.strath.ac.uk/outreach/nessie/nessie_whisky.html </a>"),
      h1("Inputs"),
      sliderInput("numClust", "Number of Clusters", value=4,
                  min = 1, max = 14, step = 1),
      uiOutput("UIwhichClust"),
      width = 3
    ),
    mainPanel(
      h1("Profiles"),
      p("The cluster centers show the typical taste notes"),
      tableOutput("clustCenters"),
      htmlOutput("listDists"),
      textOutput("tabListDists"),
      textOutput("characterClust"),
      plotOutput("plotCenters"),
      h1("Representants"),
      p("The following whiskeys most closely resemble the cluster centers"),
      tableOutput("representants"),
      
      h1("Notes for distillery"),
      p("You can choose multiple distilleries with CTRL + Click."),
      uiOutput("uiSelDist"),
      tableOutput("tabDistDetails"),

      h1("Map of distilleries"), 
      checkboxInput("filterMap", "Filter map on cluster?", value=FALSE),
      p("The map shows that the taste clusters are not regionally clustered as well"),
      plotOutput("mapDists"),
      width = 9
    )
    
    
    
    
  )
  
  
))


