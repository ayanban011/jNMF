#code for prepare data for clusters and find the optimum numbers of clusters.
#here it is prepare for join nmf.we can use it for each separately only using one delta component.
prop <- c(0.20,0.30,0.27,0.23)#we can change it after getting opt.k matrix.
effect <- 2.5

library(InterSIM)
sim.D <- InterSIM(n.sample=50, cluster.sample.prop=prop, delta.data1corr=effect,
delta.add=effect, delta.sim_matrix=effect, p.DMP=0.25, p.DEG=NULL, p.DEP=NULL,
do.plot=FALSE, sample.cluster=TRUE, feature.cluster=TRUE)
dat1 <- sim.D$dat.data1corr
dat2 <- sim.D$dat.add
dat3 <- sim.D$dat.sim_matrix
true.cluster.assignment <- sim.D$clustering.assignment
## Make all data positive by shifting to positive direction.
## Also rescale the datasets so that they are comparable.
if (!all(dat1>=0)) dat1 <- pmax(dat1 + abs(min(dat1)), .Machine$double.eps)
dat1 <- dat1/max(dat1)
if (!all(dat2>=0)) dat2 <- pmax(dat2 + abs(min(dat2)), .Machine$double.eps)
dat2 <- dat2/max(dat2)
if (!all(dat3>=0)) dat3 <- pmax(dat3 + abs(min(dat3)), .Machine$double.eps)
dat3 <- dat3/max(dat3)
dat <- list(dat1,dat2,dat3)
# Find optimum number of clusters for the data
opt.k <- nmf.opt.k(dat=dat, n.runs=5, n.fold=5, k.range=2:7, result=TRUE,
make.plot=TRUE,progress=TRUE)
# Find clustering assignment for the samples
fit <- nmf.mnnals(dat=dat, k=length(prop), maxiter=200, st.count=20, n.ini=15,
ini.nndsvd=TRUE, seed=TRUE)
