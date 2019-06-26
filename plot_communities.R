plotcommunities <- function(mygraph)
  {
     # Function to plot network communities in a graph
     # Load the "igraph" package:
     require("igraph")
     # Make a plot of the graph
     graphplot <- layoutGraph(mygraph, layoutType="neato")
     renderGraph(graphplot)
     # Get the names of the nodes in the graph:
     vertices <- nodes(mygraph)
     numvertices <- length(vertices)
     # Now record the colour of each vertex in a vector "myvector":
     myvector <- vector()
     colour <- "red"
     for (i in 1:numvertices)
     {
        vertex <- vertices[i]
        myvector[`vertex`] <- colour   # Add named element to myvector
     }
     # Set the counter for the number of communities:
     cnt <- 0
     # First find the connected components in the graph:
     myconnectedcomponents <- connectedComp(mygraph)
     # For each connected component, find the communities within that connected component:
     numconnectedcomponents <- length(myconnectedcomponents)
     for (i in 1:numconnectedcomponents)
     {
        component <- myconnectedcomponents[[i]]
        # Find the number of nodes in this connected component:
        numnodes <- length(component)
        if (numnodes > 1) # We can only find communities if there is more than one node
        {
           mysubgraph <- subGraph(component, mygraph)
           # Find the communities within this connected component:
           mylist <- findcommunities2(mysubgraph,cnt,"TRUE",myvector,0)
           cnt <- mylist[[1]]
           myvector <- mylist[[2]]
        }
      }
      # Get a set of cnt colours, where cnt is equal to the number of communities found:
      mycolours <- rainbow(cnt)
      # Set the colour of the vertices, so that vertices in each community are of the same colour,
      # and vertices in different communities are different colours:
      myvector2 <- vector()
      for (i in 1:numvertices)
      {
         vertex <- vertices[i]
         community <- myvector[vertex]
         mycolour <- mycolours[community]
         myvector2[`vertex`] <- mycolour
     }
     nodeRenderInfo(graphplot) = list(fill=myvector2)
     renderGraph(graphplot)
 }
