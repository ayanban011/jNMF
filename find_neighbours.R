findcommunities2 <- function(mygraph,cnt,plot,myvector,minsize)
  {
     # Function to find network communities in a connected component of a graph
     # Find the number of nodes in the input graph
     nodes <- nodes(mygraph)
     numnodes <- length(nodes)
     # Record the vertex number for each vertex name
     myvector <- vector()
     for (i in 1:numnodes)
     {
        node <- nodes[i] # "node" is the vertex name, i is the vertex number
        myvector[`node`] <- i  # Add named element to myvector
     }
     # Create a graph in the "igraph" library format, with numnodes nodes:
     newgraph <- graph.empty(n=numnodes,directed=FALSE)
     # First record which edges we have seen already in the "mymatrix" matrix,
     # so that we don't add any edge twice:
     mymatrix <- matrix(nrow=numnodes,ncol=numnodes)
     for (i in 1:numnodes)
     {
        for (j in 1:numnodes)
        {
           mymatrix[i,j] = 0
           mymatrix[j,i] = 0
        }
     }
     # Now add edges to the graph "newgraph":
     for (i in 1:numnodes)
     {
        node <- nodes[i] # "node" is the vertex name, i is the vertex number
        # Find the nodes that this node is joined to:
        neighbours <- adj(mygraph, node)
        neighbours <- neighbours[[1]] # Get the list of neighbours
        numneighbours <- length(neighbours)
        if (numneighbours >= 1) # If this node "node" has some edges to other nodes
        {
           for (j in 1:numneighbours)
           {
              neighbour <- neighbours[j]
              # Get the vertex number
              neighbourindex <- myvector[neighbour]
              neighbourindex <- neighbourindex[[1]]
              # Add a node in the new graph "newgraph" between vertices i and neighbourindex
              # In graph "newgraph", the vertices are counted from 0 upwards.
              indexi <- i
              indexj <- neighbourindex
              # If we have not seen this edge already:
              if (mymatrix[indexi,indexj] == 0 && mymatrix[indexj,indexi] == 0)
              {
                 mymatrix[indexi,indexj] <- 1
                 mymatrix[indexj,indexi] <- 1
                 # Add edges to the graph "newgraph"
                 newgraph <- add.edges(newgraph, c(i, neighbourindex))
              }
           }
        }
     }
     # Set the names of the vertices in graph "newgraph":
     newgraph <- set.vertex.attribute(newgraph, "name", value=nodes)
     # Now find communities in the graph:
     communities <- spinglass.community(newgraph)
     # Find how many communities there are:
     sizecommunities <- communities$csize
     numcommunities <- length(sizecommunities)
     # Find which vertices belong to which communities:
     membership <- communities$membership
     # Get the names of vertices in the graph "newgraph":
     vertexnames <- get.vertex.attribute(newgraph, "name")
     # Print out the vertices belonging to each community:
     for (i in 1:numcommunities)
     {
        cnt <- cnt + 1
        nummembers <- 0
        printout <- paste("Community",cnt,":")
        for (j in 1:length(membership))
        {
           community <- membership[j]
           if (community == i) # If vertex j belongs to the ith community
           {
              vertexname <- vertexnames[j]
              if (plot == FALSE)
              {
                 nummembers <- nummembers + 1
                 # Print out the vertices belonging to the community
                 printout <- paste(printout,vertexname)
              }
              else
              {
                 # Colour in the vertices belonging to the community
                 myvector[`vertexname`] <- cnt
              }
           }
         }
         if (plot == FALSE && nummembers >= minsize)
         {
            print(printout)
         }
      }
      return(list(cnt,myvector))
   }
