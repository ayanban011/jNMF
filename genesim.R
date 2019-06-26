geneSim <- function(gene1, gene2, info, method = "max", ...) {

    if (length(unique(gene1)) != 1L | length(unique(gene2)) != 1L) {
        stop("Introduce just one gene!\n",
             "If you want to calculate several similarities ",
             "between genes use mgeneSim")
    }

    if (!is.character(gene1) | !is.character(gene2)) {
        stop("Please use character")
    }

    if (!is.list(info)) {
        stop("Please introduce info as a list")
    }

    if (any(!c(gene1, gene2) %in% names(info))) {
        return(NA)
    }

    comb <- c(gene1, gene2)

    if (any(is.na(comb))) {
        return(NA)
    }

        # Extract all pathways for each gene
    pathways <- lapply(comb, function(x) {
        y <- info[[x]]
        y[!is.na(y)]
    })
    names(pathways) <- comb

    # Check that we have pathways info for this combination
    if (any(lengths(pathways) == 0L)) {
        return(NA)
    }
    # Subseting just the important pathways
    pathways_all <- unique(unlist(pathways, use.names = FALSE))


    sim <- mpathSim(pathways_all, info = info, method = NULL, ...)
    sim <- sim[pathways[[1]], pathways[[2]], drop = FALSE]

    # Combine or not
    if (is.null(method)) {
        sim
    }
    else {
        combineScoresPar(sim, method = method, ...)
    }
}


#' @describeIn geneSim Calculates all the similarities of the GeneSetCollection
#' and combine them using \code{\link{combineScoresPar}}
#' @export
setMethod("geneSim",
          c(info = "GeneSetCollection", gene1 = "character",
            gene2 = "character"),
          function(gene1, gene2, info, method, ...) {
              if (length(gene1) != 1 | length(gene2) != 1) {
                  stop("Introduce just one gene!\n",
                       "If you want to calculate several similarities ",
                       "between genes use mgeneSim")
              }
              # Extract the ids
              origGenes <- geneIds(info)
              # Check that the genes are in the GeneSetCollection
              genes <- unique(unlist(origGenes, use.names = FALSE))
              if (any(!c(gene1, gene2) %in% genes)) {
                  return(NA)
              }
              # Simplify the GeneSetCollection
              keep <- sapply(origGenes, function(x) {
                  any(c(gene1, gene2) %in% x)
              })
              gscGenes <- info[names(keep[keep])]

              # Search for the paths of each gene
              paths <- sapply(c(gene1, gene2), function(x){
                  keepPaths <- sapply(geneIds(gscGenes), function(y) {
                      any(x %in% y)
                  })
                  names(keepPaths[keepPaths])
              })

              # Calculate the pathSim of all the implied pathways
              pathsSim <- mpathSim(info = gscGenes, method = NULL)

              # Summarize the information
              if (is.null(method)) {
                  pathsSim[paths[[1]], paths[[2]], drop = FALSE]
              } else {
                  out <- combineScoresPar(pathsSim, method, subSets = paths, ...)
                  out[gene1, gene2]
              }
          }
}
