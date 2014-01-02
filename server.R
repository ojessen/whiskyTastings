
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(ggplot2)
set.seed(1)

whiskies <- read.csv("whiskies.txt", row.names = 1, stringsAsFactors = FALSE)
whiskies_k <- scale(whiskies[2:13])  # rescale selected vars for kmeans

shinyServer(function(input, output) {
  
  fit = reactive({
    set.seed(1)
    fit = kmeans(whiskies_k, input$numClust)
    fit
  })
  
  output$UIwhichClust <- renderUI({
    selectInput("whichClust", "Filter on Cluster", choices=1:input$numClust)
    
    
  })
  
  output$listDists = renderUI({
    p(paste("The following distilleries belong to cluster ", input$whichClust,":", sep=""))
  })
  
  output$clustCenters = renderTable({
    fit = fit()
    t(fit$centers)
  })
  
  output$plotCenters = renderPlot({
    fit = fit()
    df = fit$centers
    dfOut = data.frame(Cluster = c(), Taste = c(), Value = c())
    for(cl in 1:ncol(df))
    {
      
      tmpDf = data.frame(Cluster = rownames(df), Taste = colnames(df)[cl], Value = df[,cl])
      dfOut = rbind(dfOut, tmpDf)
    }
    p = ggplot(dfOut, aes(x = Cluster, y = Value, fill = Cluster)) + 
      geom_bar(stat="identity") +
      facet_grid(. ~ Taste)
    print(p)
  })
  
  output$representants = renderTable({
    fit = fit()
    whiskies <- data.frame(whiskies, fit$cluster)
    whiskies$fit.cluster <- as.factor(whiskies$fit.cluster)
    whiskies_r <- whiskies[c(2:13, 17)]
    # extract just flavor variables & cluster
    candidates <- by(whiskies_r[-13], whiskies_r[13], function(data) {
      # we apply this function to observations for each level of fit.cluster
      dists <- sapply(data, function(x) (x - mean(x))^2)
      # for each variable, calc each observation's deviation from average of the
      # variable across observations
      dists <- rowSums(dists)
      # for each observation, sum the deviations across variables
      rownames(data)[dists == min(dists)]
      # obtain the row number of the smallest sum
    })
    
    candidates <- as.numeric(unlist(candidates))
    whiskies[candidates, c("Distillery", "fit.cluster")]
    
  })
  
  output$tabListDists = renderText({
    fit = fit()
    whiskies <- data.frame(whiskies, fit$cluster)
    
    paste0(paste(whiskies[which(whiskies$fit.cluster == input$whichClust),1], collapse=", "),".")
    
  })
  
  output$characterClust = renderText({
    fit = fit()
    extHighVals =  colnames(fit$centers)[which(fit$center[input$whichClust,]>1)]
    extHighValsTxt = paste("extreme high values of:",paste(extHighVals, collapse = ", "))
    extLowVals = colnames(fit$centers)[which(fit$center[input$whichClust,]< -1)]
    extLowValsTxt =  paste("extreme low values of:",paste(extLowVals, collapse = ", "))
    
    highVals =  colnames(fit$centers)[which(fit$center[input$whichClust,]>0.5)]
    if(length(highVals) > 0) 
       highVals = highVals[which(!(highVals %in% extHighVals))]
    highValsTxt = paste("high values of:",paste(highVals, collapse = ", "))
    lowVals = colnames(fit$centers)[which(fit$center[input$whichClust,]< -0.5)]
    if(length(lowVals) > 0)
      lowVals = lowVals[which(!(lowVals %in% extLowVals))]
    lowValsTxt =  paste("low values of:",paste(lowVals, collapse = ", "))
    
    if(length(extHighVals)>0 & length(extLowVals)>0)
    {
      out1 = paste0("Cluster ", input$whichClust, " is characterized by ", extHighValsTxt," and ", extLowValsTxt,".")
    } else if(length(extHighVals) > 0)
    {
      out1 = paste0("Cluster ", input$whichClust, " is characterized by ", extHighValsTxt,".")
    }else if(length(extLowVals) > 0)
    {
      out1 = paste0("Cluster ", input$whichClust, " is characterized by ", extLowValsTxt,".")
    }else
    {
      out1 = paste0("Cluster ", input$whichClust, " is not characterized by extreme high or low values in any taste.")
    }
    
    if(length(highVals)>0 & length(lowVals)>0)
    {
      out2 = paste0("Cluster ", input$whichClust, " is characterized by ", highValsTxt," and ", lowValsTxt,".")
    } else if(length(highVals) > 0)
    {
      out2 = paste0("Cluster ", input$whichClust, " is characterized by ", highValsTxt,".")
    }else if(length(lowVals) > 0)
    {
      out2 = paste0("Cluster ", input$whichClust, " is characterized by ", lowValsTxt,".")
    }else
    {
      out2 = paste0("Cluster ", input$whichClust, " is not characterized by high or low values in any taste.")
    }
    paste(out1, out2,sep = "\n")
  })
  
  output$uiSelDist = renderUI({
    selectInput("selDist","Select Distillery", choices=whiskies$Distillery, multiple=TRUE)  
  })
  
  output$tabDistDetails = renderTable({
    fit = fit()
    rows = which(whiskies$Distillery %in% input$selDist)
    df = data.frame(Distillery = whiskies$Distillery[rows], whiskies_k[rows,], Cluster = fit$cluster[rows])
    df
  })
  
  output$mapDists = renderPlot({
    library("ggmap")
    fit = fit()
    whiskies <- data.frame(whiskies, fit$cluster)
    whiskies$fit.cluster <- as.factor(whiskies$fit.cluster)
    
    if(input$filterMap)
    {
      whiskies = subset(whiskies, fit.cluster == input$whichClust)
    }
    
    library(maptools)
    
    library(rgdal)
    
    
    whiskies.coord <- data.frame(whiskies$Latitude, whiskies$Longitude)
    coordinates(whiskies.coord) = ~whiskies.Latitude + whiskies.Longitude
    
    proj4string(whiskies.coord) = CRS("+init=epsg:27700")  # Specify that our coords are in osgb grid coord
    
    whiskies.coord <- spTransform(whiskies.coord, CRS("+init=epsg:4326"))  # spTransform to convert osgb grid to lat/lon
    
    whiskies <- cbind(whiskies, whiskies.coord)
    
    
    whiskies <- cbind(whiskies, geocode(paste(whiskies$Location, "Scotland", sep = " ,")))
    
    
    whiskyMap <- qmap(location = "Scotland", zoom = 6, legend = "topleft", maptype = "terrain", 
                      color = "bw", darken = 0.5)
    
    
    p = whiskyMap + geom_point(data = whiskies, aes(x = whiskies.Latitude, y = whiskies.Longitude, 
                                                    colour = fit.cluster, size = 2))
    
    print(p)
  })
})
