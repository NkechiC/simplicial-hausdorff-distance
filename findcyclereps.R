library("Hmisc")
Rcpp::sourceCpp('findduplicates.cpp')

findcyclereps <- function(cycle,filtration,birth, n) {
  # convert ripsdiag cycle location output into pairs of coordinates representing line segments(thanks Adam)
  
  linesegments <- NULL
  
  for (i in 1:dim(cycle)[1]) {
    linesegments <- rbind(linesegments,cycle[i,,])
  }
  
  #### convert coordinates in linesegments to the index associated with the coordinate in ripsfiltration ####
  
  cycle_indices <- find.matches(linesegments, filtration$coordinates)$matches
  
  #### define initial cycle boundary based on indices found above ####
  
  cycle_boundary <- matrix(cycle_indices, ncol=2, byrow = TRUE)
  
  
  #### initialize list of cyclereps, starting with cycle representative provided by the user ####
  
  cycleverts <- list()
  cycleverts[[1]] <- cycle_boundary[order(cycle_boundary[ ,1], cycle_boundary[ , 2]),]
  
  #### subset the 1-chains that have filtration less than the death radius of homology class ####
  onechains <- vector()
  for(c in 1:length(filtration$cmplx)){
    if(length(filtration$cmplx[[c]]) == 2 && filtration$values[[c]] <= birth){
      onechains <- c(onechains,c)
    }
  }
  
  #### subset the 2-chains that have filtration less than the death radius of homology class ####
  twochains <- vector()
  for(c in 1:length(filtration$cmplx)){
    if(length(filtration$cmplx[[c]]) == 3 && filtration$values[[c]] <= birth){
      twochains <- c(twochains,c)
    }
  }  
  
  
  #### build list of 2-chains that fit criteria to be added for each 1-chain ####
  ref <- list() 
  
  length(ref) <- length(onechains)
  
  for(c in onechains){
    
    ref[[c]] <- as.vector(which(lapply(filtration$cmplx[twochains], function(x) all(filtration$cmplx[[c]] %in% x)) == TRUE))
  }
  
  # recover all cycle representatives by adding the 1-boundary of the given cycle representative to the 1-boundary of each 2-chain that contains
  # an index in our given cycle representative and whose filtration < the death radius of the cycle representative
  # computation done in GF(2)
  
  rep_count <- vector()
  rep_count[1] <- 0
  rep_count[2] <- 1
  
  while( rep_count[length(rep_count)] < n) { #rep_count[length(rep_count)]
    for(k in rep_count[length(rep_count)-1]:rep_count[length(rep_count)]){
      if(k == 0) {
        k <- 1
      }
      combined <- matrix()
      for (i in 1:nrow(cycleverts[[k]])){
        onechainlist <- which(lapply(filtration$cmplx[onechains], function(x) all(cycleverts[[k]][i,] == x)) == TRUE)
        if(length(onechainlist) != 0) {
        for (j in ref[[onechains[onechainlist]]]){
          combined <- rbind(cycleverts[[k]],t(combn(filtration$cmplx[twochains][[j]], 2)))
          
          dups<-findduplicates(combined) 
          if (length(dups) == 0){
          combined<-combined[order(combined[ ,1], combined[ , 2]),]
          #no good silly
          #cycleverts[[length(cycleverts)+1]] <- combined[rle(combined)$lengths==1,]
          } else {
            combined <- combined[-dups,]
            cycleverts[[length(cycleverts)+1]] <- combined[order(combined[,1], combined[, 2]),]
          }
        }
      }
    }
    }
    cycleverts <- unique(cycleverts)
    rep_count[length(rep_count)+1] <- length(cycleverts)
  }

  return(lapply(cycleverts, function(x) filtration$coordinates[as.vector(t(x)),]))
}
    
