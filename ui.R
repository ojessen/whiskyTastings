
shinyUI(fluidPage(
  titlePanel("Whisky tastings"),
  sidebarLayout(
    sidebarPanel(
      HTML("This app is based on the article 
<a href=http://blog.revolutionanalytics.com/2013/12/k-means-clustering-86-single-malt-scotch-whiskies.html>
K-means clustering 86 Single Malt Scotch Whiskeys</a>\n"),
      HTML("As in the original article, the data is from 
<a href=https://www.mathstat.strath.ac.uk/outreach/nessie/nessie_whisky.html>
           Classification of whiskies</a>\n"),
      HTML("You can download the code for the app from 
           <a href=https://github.com/ojessen/whiskyTastings>
           github.com</a>"),
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
      
      h1("Next best whiskey"),
      p("Inspired by Raoul Kübler, you can measure similarity by the euclidean distance matrix"),
      uiOutput("uiSelDistEuclid"),
      tableOutput("euclidDistance"),
      textOutput("absClosest"),
      p("What is, tastewise, the closest whiskey to your favorite?"),
      uiOutput("uiSelDistClose"),
      textOutput("nextClosest"),
      

      h1("Map of distilleries"), 
      checkboxInput("filterMap", "Filter map on cluster?", value=FALSE),
      p("The map shows that the taste clusters are not regionally clustered as well"),
      plotOutput("mapDists"),
      width = 9
    )
    
    
    
    
  )
  
  
))


