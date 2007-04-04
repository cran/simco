"SimCoImport" <-
function(FILES){
runs<-FILES #Import the list of input files
	nruns<-length(runs)
	
	allruns<-NULL
	
	for (i in 1:nruns){
		
		xx<-read.delim(file=(paste(runs[i])),header=TRUE) #import the ith file
		names(xx)<-"A"
		rown<-1:dim(xx)[1]
		
		#Extract the required data
		startn<-rown[xx$A=="Inferred ancestry of individuals:"]+2
		endn<-rown[xx$A=="Estimated Allele Frequencies in each population"]-1
		
		ndf<-as.data.frame(xx[startn:endn,])
		names(ndf)<-"A"
		a<-strsplit(as.character(ndf$A),c(" "))
		
		#get rid of blanks, row by row
		data1<-NULL
		
		for (j in 1:length(a)){
			data1<-c(data1,a[[j]])}
		
		data1<-data1[data1!=""]
		
		#put the data into a matrix
		m1<-matrix(data1,nrow=length(a),byrow=TRUE)
		
		#add an identifier column to the matrix
		m1<-cbind(m1,paste(LETTERS[i]))
		allruns<-rbind(allruns,m1)
		}
	
	#########################################################################
	allruns<-as.data.frame(cbind(allruns[,2:3],allruns[,5:dim(allruns)[2]]))
	
	#ensure that the assignment probabilities are numeric
	for (x in 3:(dim(allruns)[2]-1)){
		allruns[,x]<-as.numeric(levels(allruns[,x])[allruns[,x]])
		}
	
	#ensure that the run ID is a factor
	allruns[,dim(allruns)[2]]<-as.factor(allruns[,dim(allruns)[2]])
	#########################################################################
	return(allruns)}

