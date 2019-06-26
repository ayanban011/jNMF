findcommunities <- function(mygraph,minsize)
  {
     # Function to find network communities in a graph
     # Load up the igraph library:
     require("igraph")
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
           # print(component)
           myvector <- vector()
           mylist <- findcommunities2(mysubgraph,cnt,"FALSE",myvector,minsize)
           cnt <- mylist[[1]]
           myvector <- mylist[[2]]
        }
     }
     print(paste("There were",cnt,"communities in the input graph"))
  }
