shinyUI(basicPage(
  
  HTML("This app is based on the article <a href=http://blog.revolutionanalytics.com/2013/12/k-means-clustering-86-single-malt-scotch-whiskies.html>
                                                 K-means clustering 86 Single Malt Scotch Whiskeys</a>\n"),
  HTML("As in the original article, the data is from 
                                                <a href>https://www.mathstat.strath.ac.uk/outreach/nessie/nessie_whisky.html </a>"),
  h1("Inputs"),
  sliderInput("numClust", "Number of Clusters", value=4,
              min = 1, max = 9, step = 1),
  
  
  h1("Profiles"),
  p("The cluster centers show the typical taste notes"),
  tableOutput("clustCenters"),
  plotOutput("plotCenters"),
  h1("Representants"),
  p("The following whiskeys most closely resemble the cluster centers"),
  tableOutput("representants"),
  h1("List of distilleries"),
  uiOutput("UIwhichClust"),
  htmlOutput("listDists"),
  tableOutput("tabListDists"),
  h1("Map of distilleries"), 
  checkboxInput("filterMap", "Filter map on cluster?", value=FALSE),
  p("The map shows that the taste clusters are not regionally clustered as well"),
  plotOutput("mapDists")
  
))

