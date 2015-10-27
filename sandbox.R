hcluster <- function(clusters, dist.df, 
                     index1='index1', index2='index2', 
                     dist.col='dist') {
  dist.df <- dist.df[order(dist.df[dist.col]), ]
  assign.order <- do.call(rbind, apply(dist.df, 1, function(r) {
    id1 <- r[index1]
    id2 <- r[index2]
    cluster.union <- union(which(clusters == clusters[id1]),
                           which(clusters == clusters[id2]))
    clusters[cluster.union] <<- clusters[clusters[id1]]
    return(list(clusters))
  }))
  return(do.call(rbind, assign.order))
  
}