get.removals.allWA <- function(n.wolves.all.fxn, removal_rate){
  
  #in this function, we are removing wolves by pack across all of WA based on a removal rate
  Nsamples <- dim(n.wolves.all.fxn)[1]
  
  for(i in 1:Nsamples){    

    #removals are calculated at level of population but removed from E Wash only
    n_remove <- ceiling(sum(n.wolves.all.fxn[i,,]) * removal_rate)
    #if there are no wolves in the state (and therefore none to remove), move on to next i
    if(n_remove==0){
      next}
    #if there are no sites in WA with >=2 adults, move on to next i
    else if(length(which(n.wolves.all.fxn[i,2,] + n.wolves.all.fxn[i,3,]>=2)) ==0){
      n_remove==0} else{#if there are sites left with 2+ adults
        # so long as there are wolves to remove and packs that qualify
        while(n_remove>0 & length(which(n.wolves.all.fxn[i,2,] + n.wolves.all.fxn[i,3,]>=2))>0){ 
        #n_remove is the number of wolves to remove, and we will remove at pack level in E Wash only
        packs_qualify <- which(n.wolves.all.fxn[i,2,] + n.wolves.all.fxn[i,3,]>=2) #packs that qualify again here
        if (length(packs_qualify) >1) { 
          pack_selected <- sample(packs_qualify,1) } else {pack_selected <- packs_qualify}
          trial <- sum(n.wolves.all.fxn[i,,pack_selected]) #this will be the number of wolves in the pack, which will all be removed
        if(n_remove - trial>0){ #if there are still wolves left to remove after removing that pack
          n.wolves.all.fxn[i,,pack_selected] <- 0 #make all age classes 0 for removed pack
          n_remove <- n_remove-trial} #there are either 0 or 1+ wolves to remove still
        else { #if we used up all N_remove on a pack, such that n_remove-trial is <=0
          #a help vector that repeats 1, 2, 3 for number of wolves in those age classes in that pack
          help <- c(rep(1, n.wolves.all.fxn[i,1,pack_selected]), rep(2, n.wolves.all.fxn[i,2,pack_selected]), rep(3, n.wolves.all.fxn[i,3,pack_selected]))
          indivs <- sample(help, n_remove, replace=F) #these are the specific individuals removed
          n.wolves.all.fxn[i,1,pack_selected] <- n.wolves.all.fxn[i,1,pack_selected] - length(which(indivs==1)) #and adjust age class counts
          n.wolves.all.fxn[i,2,pack_selected] <- n.wolves.all.fxn[i,2,pack_selected] - length(which(indivs==2))
          n.wolves.all.fxn[i,3,pack_selected] <- n.wolves.all.fxn[i,3,pack_selected] - length(which(indivs==3))
          n_remove <- 0 }}} #all removals have been used
    
  } #close i loop
  
  n.wolves.all.fxn[n.wolves.all.fxn==-1] <- 0

  #to bring back to model    
return (n.wolves.all.fxn)    

}

# set.seed(101)
# removal_rate <- 0.03
# n.wolves.all.fxn <- array(rpois(33600,1), dim=c(50,3,224))
# ceiling(sum(n.wolves.all.fxn[5,,]) * removal_rate) #will remove 20
# n.wolves.EWash.fxn <- n.wolves.all.fxn[,,EWash]
# # #
# sum(n.wolves.EWash.fxn[5,,]) #from 213 to 193
# sum(which(n.wolves.EWash.fxn[,,]<0, arr.ind=T)) #from 213 to 193

#
# dim(n.wolves.EWash.fxn)