# Dependencies
library(data.table)
library(ggplot2)

# Project Settings
dir <- "~/universidad/analisis/"
setwd(dir)
exportPlots <- TRUE

# "not in" operator
'%!in%' <- function(x,y)!('%in%'(x,y))

# Plot nodes
plotNodes <- function(nodesToPlot, dist=NULL, distance_value=Inf) {
  # Create plot
  plot <- ggplot(nodesToPlot, aes(x=V1, y=V2, label=index))
  # Plot route points
  plot <- plot + geom_point(color="blue") + geom_text(color='blue',hjust=0, vjust=-0.5)
  # Plot route lines
  plot <- plot + geom_segment(aes(xend=c(tail(V1, n=-1), NA), yend=c(tail(V2, n=-1), NA)), na.rm = TRUE)
  # Add distance of the route
  plot <- plot + ggtitle(paste("Distance:", distance_value, sep=" "))
  
  # Save plot to file
  if (exportPlots) {
    filename <- format(Sys.time(), "%H-%M-%OS3.png")
    ggsave(filename)
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
printResult <- function(resultInfo, stime, etime) {
  print(paste("Running time:", (etime - stime), sep=" "))
  print(paste("Distance:", result$distance, sep=" "))
  print(paste("Route:", paste(result$route, collapse=" -> "), sep=" "))
  result$plot
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
  distance_value <- routeDistance(dist, route)
  
  # Create new nodes object order by route
  orderedNodes <- orderRoute(route=route, nodes=nodes)
  
  # Plot
  plot <- plotNodes(nodesToPlot=orderedNodes, dist=dist, distance_value=distance_value)
  
  return(list(distance=distance_value, route=route, plot=plot))
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
          orderedNodes <- orderRoute(route=bestRoute, nodes=nodes)
          plot <- plotNodes(nodesToPlot=orderedNodes, dist=dist, distance_value=minDistance)
          breakLoop <- T
          break()
        }
      }
      if(breakLoop) break()
    }
    
    # Update tracking variable
    trackDistance <- c(trackDistance, minDistance)
    
    # Validate optimization
    if(previousDistance == newDistance) {
      maxTimes <- maxTimes + 1
      if (maxTimes > 10) break()
    }
  }
  
  return(list(distance=minDistance, route=route, trackDistance=trackDistance, plot=plot))
}

start_time <- Sys.time()
# Load nodes from file
nodes <- read.csv(file="coords.csv", header=FALSE, sep="\t")
nodes <- data.table(nodes)
nodes <- nodes[,index:=c(1:nrow(nodes))]

# Load distances from file
dist <- read.csv(file="dist.csv", header=FALSE, sep="\t")
dist <- as.matrix(dist)

result <- findNearest(nodes, dist)
result <- optimize(nodes, dist, result)
end_time <- Sys.time()

printResult(resultInfo=result, stime=start_time, etime=end_time)
