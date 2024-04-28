# Project Settings
dir <- "~/universidad/analisis/"
setwd(dir)
n <- 200

createNodes <- function(n = 20) {
  set.seed(131881)
  nodes <- as.data.frame(matrix(runif(n=(n*2), min=1, max=n), nrow=n))
  names(nodes) <- c('V1', 'V2')
  return(nodes)
}

saveNodes <- function() {
  dir.create(sprintf("data/%s/", n), showWarnings = FALSE)
  write.csv(nodes, sprintf("data/%s/nodes.csv", n), row.names = FALSE, col.names = FALSE, sep="\t")
  write.csv(dist, sprintf("data/%s/dist.csv", n), row.names = FALSE, col.names = FALSE, sep="\t")
}

nodes <- createNodes(n=n)
dist <- as.matrix(dist(nodes))
saveNodes()
