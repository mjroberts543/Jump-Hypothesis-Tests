balancetrain=function(train){
	BJsAT=which(train$target==1)
	SJsAT=which(train$target==0)
	
	sampled=sample(SJsAT,length(BJsAT))
	balancedtrain=train[sort(c(BJsAT,sampled)),]
	return(balancedtrain)
}

confmat=function(X,Y,cutoff=.5){
	if (cutoff>0){
		X=sign(X%/%cutoff)
		Y=sign(Y%/%cutoff)
	}
	if (cutoff==0){
		X=sign(X)
		Y=sign(Y)
	}
	CM=matrix(nrow=2,ncol=2)
	CM[1,1]=sum(as.numeric((X+Y)==0))
	CM[2,2]=sum(as.numeric((X+Y)==2))
	CM[1,2]=sum(as.numeric((X-Y)==-1))
	CM[2,1]=sum(as.numeric((Y-X)==-1))
	return(CM)
}

trainon=function(closes,percent=100){

	#Percent Controls whether working with percents not
	pdchange=percent*diff(closes)/closes[1:length(closes)-1]
	npdchange=-na.omit(pdchange[pdchange<0])
	mu=mean(npdchange)
	npdchange

	breaks=seq(0,percent,.001*percent)
	nu=hist(npdchange,breaks=breaks,plot=FALSE)$density/sum(hist(npdchange,breaks=breaks,plot=FALSE)$density)

	for (i in seq(length(nu),1,-1)){if (nu[i]!=0){break}}
	nu=nu[1:i]
	
	#Don't want to fit 0
	x=breaks[seq(2,i+1)]
	

	#Inverse Gaussian PDF
	igpdf=function(x,l,m){
		sqrt(l/(2*pi*x**3))*exp(-l*(x-m)**2/(2*m**2*x))
	}
	
	Dl=.0000001
	errors=numeric(0)
	for (l in seq(0,.01,Dl)){
		errors=c(errors,sum((igpdf(x,l,mu)-nu)^2))
	}

	#Fit lambda to get closest full set training density
	
	lambda=Dl*(which.min(errors)-1)
	print(lambda)
	#Discretize it
	return(igpdf(x,lambda,mu))
}



bigorsmall=function(closes,nu,percent=100){
	
		breaks=seq(0,percent,.001*percent)
		x=breaks[seq(2,length(nu)+1)]

		groupsize=10

		T=length(closes)
		percent=100

		as=numeric(0)
		zs=numeric(0)

		for(j in seq(1,T-groupsize-1)){
			test=closes[seq(j,j+groupsize)]
			pdtest=percent*diff(test)/test[1:length(test)-1]
			npdtest=-pdtest[pdtest<0]

			testdensity=hist(npdtest,breaks=breaks,plot=FALSE)$density/sum(hist(npdtest,breaks=breaks,plot=FALSE)$density)	
			testdensity=testdensity[1:length(nu)]

		start=0
		dom=which(testdensity>0)
		
		dom=subset(dom,dom>start)		
		
		
		#If no signficant negative jumps, just make a=0.
		if(length(dom)>0){
			Edens=function(a){sum((nu[dom]*(1+a*x[dom])-testdensity[dom])^2)}
			a=optimize(Edens,interval=c(0,1000),tol = .Machine$double.eps)$minimum
		}else{
			a=0
		}
		
		as=c(as,a)
		dx=x[3]-x[2]
		pointmin=function(y){min(1,y)}
		sigma=sd(pdtest)
		beta=-a*sum(unlist(lapply(x,pointmin))*1/sigma*x*nu*dx)
		dom=which(x>1)
		m=a*sum(x[dom]*nu[dom]*dx)
		dom=which(x<1)
		gamma=m-beta**2/2+sum((log(1+x[dom])**2-x[dom])*a*nu[dom]*dx)
		K=a*log(1+x)*nu

		#higher z, bigger jumps
		z=(gamma-sum(x*K*dx))/abs(beta)
		if(z=="NaN"){z=100*sign(gamma-sum(x*K*dx))}
		zs=c(zs,z)
	}

	#.75 worked well here before taking mean
	minz=quantile(zs,.9)
	print(minz)
	bigjumps=-sign(zs%/%minz)+1
	
	return(bigjumps)
}

followplot1=function(TestStart,TestStop,close,borses,groupsize){
	plot(seq(TestStart,TestStop),close[seq(TestStart,TestStop)],type="l")
	for (i in seq(TestStop-TestStart-groupsize)){
		if (borses[i]){
			points(seq(TestStart+i-1,TestStart+i+groupsize-1),close[seq(TestStart+i-1,TestStart+i+groupsize-1)],col="red",type="l",lwd=3)
			#Sys.sleep(.1)
			#readline()
			#plot(seq(TestStart,TestStop),all6[ind,seq(TestStart,TestStop)],type="l")
		}
	}
}










followplot=function(TestStart,TestStop,all6,ind,borses,groupsize){
	plot(seq(TestStart,TestStop),all6[ind,seq(TestStart,TestStop)],type="l")
	for (i in seq(TestStop-TestStart-groupsize)){
		if (borses[ind,i]){
			points(seq(TestStart+i-1,TestStart+i+groupsize-1),all6[ind,seq(TestStart+i-1,TestStart+i+groupsize-1)],col="red",type="l",lwd=3)
			#Sys.sleep(.1)
			#readline()
			#plot(seq(TestStart,TestStop),all6[ind,seq(TestStart,TestStop)],type="l")
		}
	}
}


createstaggerdf=function(bors,groupsize){
	T=length(bors)
	sdf=matrix(nrow=T-2*groupsize,ncol=groupsize)
	for (i in seq(groupsize)){
		for (j in seq(T-2*groupsize)){
			sdf[j,i]=bors[i+j-1]
		}
	}
	return(as.data.frame(sdf))
}




