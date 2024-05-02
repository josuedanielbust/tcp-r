# Dependencies
library(data.table)
library(ggplot2)

# Project Settings
dir <- "~/universidad/analisis/"
setwd(dir)
dataset <- 25
exportPlots <- TRUE
plotsRecord <- list()

# "not in" operator
'%!in%' <- function(x,y)!('%in%'(x,y))

# Plot nodes
plotNodes <- function(route, nodes, distanceValue=Inf, filename="plot.png") {
  # Create new nodes object order by route
  nodesToPlot <- orderRoute(route=route, nodes=nodes)
  
  # Create plot
  plot <- ggplot(nodesToPlot, aes(x=V1, y=V2, label=index))
  # Plot route points
  plot <- plot + geom_point(color="blue") + geom_text(color='blue',hjust=0, vjust=-0.5)
  # Plot route lines
  plot <- plot + geom_segment(aes(xend=c(tail(V1, n=-1), NA), yend=c(tail(V2, n=-1), NA)), na.rm = TRUE)
  # Add distance of the route
  plot <- plot + ggtitle(paste("Distance:", distanceValue, sep=" "))
  
  # Save plot to file
  if (exportPlots) {
    filepath <- sprintf("results/%s/%s", dataset, filename)
    ggsave(filepath)
  }
  
  # Return plot
  return(plot)
}

# Calculate route distance
routeDistance <- function(dist, route) {
  d <- 0
  for(i in 2:length(route)) {
    d <- d + dist[route[i-1], route[i]]
  }
  return(d)
}

# Print Result
printResult <- function(result, startTime, endTime) {
  # Create dataset results directory if doesn't exists
  dir.create(sprintf("results/%s/", dataset), showWarnings = FALSE)
  
  timeDifference <- (endTime - startTime)
  
  # Print the data to a file
  sink(sprintf("results/%s/result.txt", dataset))
  print(sprintf("Running time: %.5f %s", timeDifference, units(timeDifference)))
  print(sprintf("Distance: %.5f", result$distance))
  print(sprintf("Route: %s", paste(result$route, collapse=" -> ")))
  
  if (exportPlots) {
    startTime <- Sys.time()
    for(i in 1:(length(plotsRecord))) {
      plotNodes(
        route=plotsRecord[[i]]$route,
        nodes=plotsRecord[[i]]$nodes,
        distanceValue=plotsRecord[[i]]$distanceValue,
        filename=plotsRecord[[i]]$filename
      )
    }
    endTime <- Sys.time()
    timeDifference <- (endTime - startTime)
    print(sprintf("Plots generation time: %.5f %s", timeDifference, units(timeDifference)))
  }
  
  sink()
}

# Order route
orderRoute <- function(route, nodes) {
  orderedNodes <- data.table()
  for (i in 1:length(route)-1) {
    orderedNodes <- rbindlist(list(orderedNodes, nodes[route[i]]))
  }
  orderedNodes <- rbindlist(list(orderedNodes, orderedNodes[1]))
  return(orderedNodes)
}

# Algorithm
findNearest <- function(nodes, dist) {
  # Start route on first node
  currentNode <- as.numeric(rownames(nodes)[1])
  route <- c(currentNode)
  
  # Add nearest neighbor
  for(i in 1:(nrow(nodes)-2)) {
    distances <- dist[currentNode, -which(rownames(nodes) %in% route)]
    currentNode <- as.numeric(gsub('V', '', names(which(distances == min(distances)))))
    route <- c(route, currentNode)
  }
  
  # End route on first node
  last <- which(rownames(nodes) %!in% route)
  route <- c(route, as.numeric(rownames(nodes)[last]))
  route <- c(route, as.numeric(rownames(nodes)[1]))
  
  # Get distance
  distanceValue <- routeDistance(dist, route)
  
  # Plot
  if (exportPlots) {
    plotsRecord <<- append(plotsRecord, list(list(
      route=route,
      nodes=nodes,
      distanceValue=distanceValue,
      filename=format(Sys.time(), "%H-%M-%OS3.png")
    )), after=0)
  }

  return(list(distance=distanceValue, route=route, plot=plot))
}

swap <- function(route, i, j) {
  newRoute <- route[1:(i-1)]
  newRoute <- c(newRoute, route[j:(i)])
  newRoute <- c(newRoute, route[(j+1):length(route)])
  return(newRoute)
}

optimize <- function(nodes, dist, actualRoute) {
  # Control Variables
  maxTimes <- 0
  bestRoute <- actualRoute$route
  minDistance <- actualRoute$distance
  
  # Track the distance of the route
  trackDistance <- c()
  
  # While loop performing swaps
  while(TRUE) {
    previousDistance <- minDistance
    
    breakLoop <- FALSE
    
    for(i in 2:(nrow(nodes)-1)) {
      for(j in (i+1):(nrow(nodes)-1)) {
        # Swap
        route <- swap(route=bestRoute, i=i, j=j)
        newDistance <- routeDistance(dist, route)
        
        # Update distance and plot
        if(newDistance < minDistance) {
          maxTimes <- 0
          minDistance <- newDistance
          bestRoute <- route
          
          # Plot
          if (exportPlots) {
            plotsRecord <<- append(plotsRecord, list(list(
              route=bestRoute,
              nodes=nodes,
              distanceValue=minDistance,
              filename=format(Sys.time(), "%H-%M-%OS3.png")
            )), after=0)
          }
          breakLoop <- TRUE
          break()
        }
      }
      if(breakLoop) break()
    }
    
    # Update tracking variable
    trackDistance <- c(trackDistance, minDistance)
    
    # Validate optimization
    if ((i/n) > (1/exp(1))) { break() }
    # if(previousDistance == newDistance) {
    #   maxTimes <- maxTimes + 1
    #   if (maxTimes > 10) break()
    # }
  }
  
  return(list(distance=minDistance, route=route, trackDistance=trackDistance, plot=plot))
}

main <- function() {
  startTime <- Sys.time()
  # Load nodes from file
  nodes <- read.csv(file=sprintf("data/%s/nodes.csv", dataset), header=FALSE, sep="\t")
  nodes <- data.table(nodes)
  nodes <- nodes[,index:=c(1:nrow(nodes))]
  
  # Load distances from file
  dist <- read.csv(file=sprintf("data/%s/dist.csv", dataset), header=FALSE, sep="\t")
  dist <- as.matrix(dist)
  
  result <- findNearest(nodes, dist)
  result <- optimize(nodes, dist, result)
  endTime <- Sys.time()
  
  printResult(result=result, startTime=startTime, endTime=endTime)
}

main()
