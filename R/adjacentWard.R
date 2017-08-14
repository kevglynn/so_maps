#' Adjacent Ward Cluster Algorithm.
#' 
#' Use the Adjacent Ward Cluster Algorithm to prevent discontiguous cluster 
#' segments. The implemented algorithm is described in further details in 
#' \url{https://www.viscovery.net/download/public/The-SOM-Ward-cluster-algorithm.pdf}.
#' 
#' @param somobj an object of class "kohonen"
#' @param toroidal if TRUE, the edges of the map are joined. Note that in a hexagonal toroidal map, the number of rows must be even.
#' @return List with cluster membership for different number of clusters.
#' 
#' @examples 
#' som_model <- som(as.matrix(iris[,1:4]), 
#'                  grid=somgrid(5,5,"hexagonal"))
#' adjacentWard(som_model, toroidal = FALSE)
#' @export
adjacentWard <- function(somobj, toroidal = FALSE) {
  
  ### Initialise ---------------------------------------------------------------
  
  # extract information out of the SOM object
  somCodes <- somobj$codes
  Mapping  <- somobj$unit.classif
  somGrid  <- somobj$grid
  
  # number of nodes/clusters in SOM grid
  numClusters <- nrow(somCodes)

  # number of observations per node/cluster
  Cluster.Size <- matrix(data=0, nrow=1, ncol=numClusters)
  Cluster.Size[sort(unique(Mapping))] <- as.numeric(data.matrix(table(Mapping)))

  # allocations of each node per number of clusters
  ClusterSegments <- list()

  # start with each node as its own cluster
  Clusters <- list()
  for(i in 1:numClusters) Clusters[[i]] <- i
  
  ### Find the neighbours of each cluster --------------------------------------
  
  # distances between the node centers
  Neighbours.Dist <- unit.distances(somGrid, toroidal)
  
  # which clusters are each others neighbours
  Neighbours <- mat.or.vec(numClusters, numClusters)
  for (i in 1:numClusters) {
    Neighbours[i,which(Neighbours.Dist[i,] > 0.99 & Neighbours.Dist[i,] < 1.01)] <- 1
  }
  
  ### Calculate Ward Distance --------------------------------------------------
  
  # euclidean distance between centers of gravity in the nodes
  Dists <- as.matrix(stats::dist(somCodes, method = "euclidean", diag = TRUE, upper = TRUE))
  
  # https://www.viscovery.net/download/public/The-SOM-Ward-cluster-algorithm.pdf
  # distance measure characterizing Ward's method is based on the variance criterion
  MatrixDataRow      <- matrix(rep(Cluster.Size,length(Cluster.Size)), nrow=length(Cluster.Size))
  MatrixDataColumn   <- matrix(rep(Cluster.Size,length(Cluster.Size)), nrow=length(Cluster.Size), byrow=TRUE)
  MatrixAddData      <- MatrixDataRow + MatrixDataColumn
  MatrixMultiplyData <- MatrixDataRow * MatrixDataColumn
  DataMeasureMatrix  <- MatrixMultiplyData / MatrixAddData
  TotalDistanceMeasure <- DataMeasureMatrix*(Dists^2)
  
  DistMeasure <- matrix(data=Inf, nrow=numClusters, ncol=numClusters)
  # set distance between two empty clusters/nodes to 0
  DistMeasure[which(MatrixAddData == 0 & Neighbours == 1)] <- 0
  # set distance between non empty clusters
  DistMeasure[which(MatrixAddData != 0 & Neighbours == 1)] <- TotalDistanceMeasure[which(MatrixAddData != 0 & Neighbours == 1)]
  # only upper half of matrix is needed
  DistMeasure[lower.tri(DistMeasure)] <- Inf 
  
  ### Create Clusters ----------------------------------------------------------
  
  while (length(Clusters) > 2) {      
    
    # identify smallest ward-distance
    SmallDist <- arrayInd(which.min(DistMeasure), dim(DistMeasure))
    mergeA <- SmallDist[1,1]
    mergeB <- SmallDist[1,2]
    
    # calculate mean attributes of new cluster
    somCodes[mergeA,] <- (1/(Cluster.Size[mergeA]+Cluster.Size[mergeB]))*(Cluster.Size[mergeA]*somCodes[mergeA,] + Cluster.Size[mergeB]*somCodes[mergeB,])
    # drop second cluster part (now obsolete)
    somCodes <- somCodes[-mergeB,]
    # calculate number of observations in new cluster
    Cluster.Size[mergeA] = Cluster.Size[mergeA]+Cluster.Size[mergeB]
    # drop second cluster part (now obsolete)
    Cluster.Size <- Cluster.Size[-mergeB]
    # combine the two clusters in Cluster list
    Clusters[[mergeA]] = c(Clusters[[mergeA]],Clusters[[mergeB]])
    # drop second cluster part (now obsolete)
    Clusters[[mergeB]] <- NULL
    # clean distance measure matrix
    DistMeasure <- DistMeasure[-mergeB,]
    DistMeasure <- DistMeasure[,-mergeB]
    # update neighbours
    DummyRow <- ceiling(colMeans(Neighbours[c(mergeA,mergeB),]))
    DummyRow[c(mergeA,mergeB)] <- 0
    Neighbours[mergeA,] <- DummyRow
    Neighbours[,mergeA] <- DummyRow
    # clean neighbours
    Neighbours <- Neighbours[-mergeB,]
    Neighbours <- Neighbours[,-mergeB]
    
    # recalculate distance
    Dists <- as.matrix(stats::dist(somCodes, method = "euclidean", diag = TRUE, upper = TRUE))
    
    # update distances betweeen new cluster and their neighbours
    for(j in which(Neighbours[mergeA,] == 1)){
      # set distance to infinite if the clusters are not neighbours
      if (Cluster.Size[mergeA] == 0 && Cluster.Size[j] == 0) { 
        # if both neighboured clusters don't contain any points set d_ij = 0
        if (j > mergeA) { DistMeasure[mergeA,j] <- 0 } else { DistMeasure[j,mergeA] <- 0 }
      } else { 
        if (j > mergeA) { 
          DistMeasure[mergeA,j] <- (Cluster.Size[mergeA]*Cluster.Size[j])/(Cluster.Size[mergeA]+Cluster.Size[j])*(Dists^2)[mergeA,j] 
        } else { 
          DistMeasure[j,mergeA] <- (Cluster.Size[mergeA]*Cluster.Size[j])/(Cluster.Size[mergeA]+Cluster.Size[j])*(Dists^2)[j,mergeA] 
        }             
      }
    }
    
    #Vectorized method to know which nodes where mapped to which cluster
    ClusterOrder <- unlist(Clusters)
    ClusterIDs   <- unlist(sapply(c(1:length(Clusters)), function(x) rep(x, length(Clusters[[x]]))))
    ClusterCombi <- cbind(ClusterOrder, ClusterIDs)
    ClusterCombi <- ClusterCombi[order(ClusterCombi[,1]),]
    
    # update ClusterSegments
    ClusterSegments[[length(Clusters)]] <- ClusterCombi[,2]
    
    # update number of Clusters
    numClusters <- numClusters - 1

  }# end while
  
  return(ClusterSegments) 
  
}