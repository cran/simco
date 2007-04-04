"SimCoef" <-
function(mydata){
#what permutations of MATRIX PAIRS are possible?
	nm<-levels(mydata[,dim(mydata)[2]])
	perm.pairs<-combinations(length(nm), 2, v=nm, set=TRUE, repeats.allowed=FALSE)
	cat("These are the MATRIX permutations:\n")
	print(perm.pairs)
	cat("\n\n")
	
	#column permutations
	#make a matrix of all possible permutations of column order...
	datamat<-as.matrix(mydata[,3:(dim(mydata)[2]-1)])
	col.perm<-permutations(dim(datamat)[2], dim(datamat)[2], v=1:dim(datamat)[2], set=TRUE,repeats.allowed=FALSE)
	
	cat("These are the column permutations:\n")
	print(col.perm)
	cat("\n\n")
	
	
	simcoef<-NULL
	allresults<-NULL
	
	for (j in 1:dim(perm.pairs)[1]){  #The START of the "j" loop (combinations of matrix pairs)
		#This loops through each possible pair of matrices in turn. 
		#Then a loop further down loops through the possible orderings of columns.
		
		##subset the matrices that are to be compared (m1 and m2) from "mydata"
		m1<-data.frame(subset(mydata,mydata[,dim(mydata)[2]]==perm.pairs[j,1]))
		m2<-data.frame(subset(mydata,mydata[,dim(mydata)[2]]==perm.pairs[j,2]))
		m1<-as.matrix(m1[,3:(dim(m1)[2]-1)])
		m2<-as.matrix(m2[,3:(dim(m2)[2]-1)])
		
		numerator<-NULL
		denominator<-NULL
		
		for (i in 1:dim(col.perm)[1]){ 
			m2.reordered<-m2[,col.perm[i,]]

			num1<-sqrt(sum((m1-m2.reordered)^2))
			numerator[i]<-num1
			denominator<-sqrt(sum((m1-(1/dim(col.perm)[2]))^2))
			}
		
		simcoef[j]<-1-(min(numerator)/denominator)
		} 
	
	cat("Number of populations (K) =", dim(m1)[2],"\n")
	cat("Number of individuals (I) =", dim(m1)[1],"\n")
	cat("Number of Structure runs =", length(nm),"\n\n")
	cat("Range =", min(simcoef),"-",max(simcoef),"\n")
	cat("Median Similarity Coefficient =", median(simcoef),"\n")
	cat("Mean Similarity Coefficient =", mean(simcoef),"\n")
	cat("SEM of Similarity Coefficient =", sqrt(var(simcoef)/length(simcoef)),"\n\n")
	cat("The similarity coefficients were: ",simcoef,"\n")
	cat("Summary: \n")
	
	print.table(cbind(perm.pairs,signif(simcoef,3)))

	}

