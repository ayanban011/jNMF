findcomponent <- function(graph,vertex)
  {
     # Function to find the connected component that contains a particular vertex
     require("RBGL")
     found <- 0
     myconnectedcomponents <- connectedComp(graph)
     numconnectedcomponents <- length(myconnectedcomponents)
     for (i in 1:numconnectedcomponents)
     {
        componenti <- myconnectedcomponents[[i]]
        numvertices <- length(componenti)
        for (j in 1:numvertices)
        {
           vertexj <- componenti[j]
           if (vertexj == vertex)
           {
              found <- 1
              return(componenti)
           }
        }
     }
     print("ERROR: did not find vertex in the graph")
  }
