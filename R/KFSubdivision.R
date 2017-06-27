GloutonSubdivision <- function(Xscaled, distMatrix,k=10) 
{
}

RandomSubdivision <- function(XScaled,k=10,seed=1) 
{
	iNumDP   <- nrow(XScaled)
	if (iNumDP <= k) 
	{
		return (list(LOO= TRUE, DPs <- XScaled) )
	}
	
	ListIndices 			<- list();
	ordinair    			<- iNumDP%/%k  
	plusone     			<- iNumDP%%k
 
	vectorofindices 		<- 1:iNumDP
	
	if (plusone > 0) 
	{		
		for ( iFold in 1:plusone) 
		{
			ListIndices[[iFold]] 	<- sample(vectorofindices, ordinair+1, replace=F)
			vectorofindices         <- vectorofindices[-ListIndices[[iFold]]]
		}
	}
	if (plusone<k) 
	{	
		for ( iFold in  (plusone+1):k) 
		{	 
			ListIndices[[iFold]] 	<- sample(vectorofindices, ordinair, replace=F)
			vectorofindices         <- vectorofindices[-ListIndices[[iFold]]]
		}
	}

	return(ListIndices) 
}

OBOSubdivision <- function(XScaled,k=10) 
{
	iNumDP   	<- nrow(XScaled)
	if (iNumDP 	<= k) 
	{
		return (list(LOO= TRUE, DPs <- XScaled) )
	}
	ListIndices <-list();
	for (iFold in 1:k)
	{
		ListIndices[[iFold]] <- c(k)
	}
	for (i in 1:iNumDP)  
	{
		foldnumber  				<- (i-1)%%k +1 
		tempVect 					<- ListIndices[[foldnumber]]
		tempVect[1+ (i-1)%/%k] 		<- i 
		ListIndices[[foldnumber]]	<- tempVect
	}

	return(ListIndices) 
}
