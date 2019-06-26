
jNMF <- function (nmf_res, np=100, ncores=parallel::detectCores(), fdr=FALSE, top=1000, verbose=FALSE) {
    
    W <- nmf_res$W
    rownames(W) <- NULL
    rnk0 <- nmf_res$rnk
    rnk <- rnk0[which(abs(rnk0)>sort(abs(rnk0), decreasing=TRUE)[top] )]
    
    
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    if (verbose) {print(paste("getDoParWorkers:", foreach::getDoParWorkers()))}
    strt<-Sys.time()
    
    j <- NULL
    matP <- foreach::foreach(1:np, .combine='cbind') %:%
        foreach::foreach(j=rnk, .combine='c') %dopar% {
            length(which(abs(j)< abs(sample(W[,2], length(rnk0)) - W[,1])))
        }
    print(Sys.time()-strt)
    parallel::stopCluster(cl)
    
    p <- rowMeans(matP)/ length(rnk0)
    if (fdr) {
        fdr <- p.adjust(p, method="fdr")
        dat <- cbind(rnk, p, fdr)
    } else {
        dat <- cbind(rnk, p)
    }
    
    return (dat)
}
