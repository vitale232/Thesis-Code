
morantable.mc = function(object) { #a function to calculate and display correlograms using a set increment.  Requires the sdep library and that data be in a list with 1) the number of permutations, 2) the bin size increment, and 3) data in the following column format: col 1 = east, col 2 = north, col 3 = variable.  Currently not operational for Lat-Long datasets.
  data=na.omit(object[[3]]) #extracts x,y, and residuals from list, omits any observations missing coordinates or residual data
  inc=object[[2]] #extracts increment value from list
  perms=object[[1]] #extracts the number of desired permutations from list
  maxprop=0.5
  library(spdep)
  xymat=cbind(data[,1],data[,2]) #creates a 2 x n matrix of coordinates
  dist=as.matrix(dist(xymat)) #distance matrix for use in Moran's I calcs
  inv.dist=1/dist #create inverse distance table
  diag(inv.dist)=0 #zero-out diagonal
  md=max(dist)*maxprop #sets the maximum lag distance to half the maximum distance between pairs of sites in the dataset
  upper=c(seq(inc,(md-inc),inc),md) #creates a vector of upper bin increment values, with the final value being the maximum lag distance (see above)
  binvect=c(0,upper) #modifies the upper boundary vector of bin breaks, by adding 0 at lower end
  numbins=length(binvect)-1 # number of bins for loop
  #This section creates a set of empty vectors to be populated in the loop
  binpairs=numeric(numbins) #empty pvalue vector
  binmoran.obs=numeric(numbins) #empty moran's I vector
  binmoran.exp=numeric(numbins) #empty moran's I sd vector
  binmoran.sd=numeric(numbins) #empty moran's I expected vector
  binmoran.rank=numeric(numbins) #empty moran's I expected vector
  binmoran.pval=numeric(numbins) #empty pvalue vector
  for(j in 1:numbins) {  #loop to calculate moran's I values in each bin
    dist.binary=(dist > binvect[j] & dist <= binvect[j+1]) #get pairs with distances within bin range
    binpairs[j]=length(subset(as.vector(dist.binary),as.vector(dist.binary)=="TRUE")) #count the number of inter-pair distance with non-zero weights
    inv.dist.bin=dist.binary*inv.dist #'zero-out' the distances outside the bin of interest
    lw=mat2listw(inv.dist.bin) #create a spdep weight list from the weighting matrix
    m=moran.mc(data[,3],lw,nsim=perms,zero.policy=TRUE) #run the permutation test
    binmoran.obs[j]=m$statistic  #extract the observed Moran's I
    binmoran.exp[j]=mean(m$res[1:perms]) #calculate the expected Moran's I just for kicks (I chose mean over all permutations)
    binmoran.sd[j]=sd(m$res[1:perms]) #calculate the standard deviation of permutation values for kicks
    binmoran.rank[j]=m$parameter #extract the rank of the observed I value in the in the set of permutations
    binmoran.pval[j]=m$p.value #extract the one-tailed (positive I) p-value of this ranking 
  }
  #needs work: p.critical=.05/seq(1,numbins)  #sequential Bonferroni correction, in descending order (following Legendre
  #needs work: p.crit.true=(binmoran.pval <= p.critical)
  obs.exp=(binmoran.obs-binmoran.exp)
  moran.table = data.frame(binpairs,round(upper,1),round(binmoran.obs,3),round(binmoran.exp,3),round(binmoran.sd,3),round(binmoran.rank,0),round(binmoran.pval,5))
  colnames(moran.table)=c("n","upper","obs","exp","sd","rank","P")
  return(moran.table)
}