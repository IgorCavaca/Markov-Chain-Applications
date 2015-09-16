##############MCMC################
#####Author: Jiachuan Tian########
#####Time: 0423105        ########
##################################

#a
set.seed(1)

f <- function(theta,miu,rou){
	1*(1-rou^2)/(2*pi*(1-2*rou*cos(theta-miu)+rou^2));
	}
g <- function(theta){
	theta^2;
	}
q <- function(){
	1/(2*pi);
	}

miu_IS <- function(miu, rou, N){
		x <- runif(N,miu-pi, miu+pi);
		u <- c();
		v <- c();
		for (i in 1:N){
			theta <- x[i]
			u[i] <- f(theta,miu,rou)*g(theta)/q();
			v[i] <- f(theta,miu,rou)/q();
			}
		return(sum(u)/sum(v));
		}
miu <- 3;
rou <- 0.7;
N <- 10000;

miu_IS(miu,rou,N);

#b
g <- function(theta){
	theta;
	}
rhovals <- seq(-.95, .95, by=.02);
result <- lapply(rhovals, miu_IS, miu=3, N=10000);
plot(rhovals, result,title="Plot of miu_IS vs pho");


#a
set.seed(1)

f <- function(alpha,ys){
	exp(alpha*sum(ys)-length(ys)*exp(alpha)-alpha^2/200);
	}
g <- function(alpha){
	alpha;
	}
q1 <- function(alpha,ys){
	sum(ys)*exp(-alpha*sum(ys));
	}
#q2 <- function(alpha,ys){
#	exp(-(alpha-100*sum(ys))*(alpha-100*sum(ys))/200)/sqrt(200*pi);
#	}
q3 <- function(alpha){
	dnorm(alpha,sd=0.0001)
	}
miu_IS_1 <- function(ys, N){
		x <- rexp(N,rate=sum(ys));
		u <- c();
		v <- c();
		for (i in 1:N){
			alpha <- x[i]
			u[i] <- f(alpha,ys)*g(alpha)/q1(alpha,ys);
			v[i] <- f(alpha,ys)/q1(alpha,ys);
			}
		return(sum(u)/sum(v));
		}
#miu_IS_2 <- function(ys, N){
#		x <- rnorm(N,mean=100*sum(ys), sd=10)
#		u <- c();
#		v <- c();
#		for (i in 1:N){
#			alpha <- x[i]
#			u[i] <- f(alpha,ys)*g(alpha)/q2(alpha,ys);
#			v[i] <- f(alpha,ys)/q2(alpha,ys);
#			}
#		return(sum(u)/sum(v));
#		}
miu_IS_3 <- function(ys,N){
		x <- rnorm(N,sd=0.0001);
		u <- c();
		v <- c();
		for (i in 1:N){
			alpha <- x[i];
			u[i] <- f(alpha,ys)*g(alpha)/q3(alpha);
			v[i] <- f(alpha,ys)/q3(alpha);
			}
		return(sum(u)/sum(v));
		}
ys <- scan("http://www.stat.psu.edu/~mharan/515/hwdir/hw09.dat")
y1 <- rep(ys,10)
N <- 1000;
n <- 1000;
#miu_IS_1(ys[1],N);
#miu_IS_2(ys,N);
miu_IS_diff <- matrix(data=NA, ncol=1000, nrow=2);
for (i in 1:n){
 miu_IS_diff[1,i] <- miu_IS_1(y1[1:i],N);
  miu_IS_diff[2,i] <- miu_IS_3(y1[1:i],N);
}


#plot
plot(miu_IS_diff[2,]*2500,col="white");
lines(miu_IS_diff[1,]/miu_IS_diff[1,1],col="green");
lines(miu_IS_diff[2,]*2500, col="red");


#lines(n,lapply(n,miu_MCMC,x=x));


###Gumbel##
#install.packages("gumbel")
#library(gumbel)
#q4 <- function(alpha,ys){
#	exp(-length(ys)*exp(alpha))
#	}
#miu_IS_1 <- function(ys, N){
#		x <- rexp(N,rate=sum(ys));
#		u <- c();
#		v <- c();
#		for (i in 1:N){
#			alpha <- x[i]
#			u[i] <- f(alpha,ys)*g(alpha)/q1(alpha,ys);
#			v[i] <- f(alpha,ys)/q1(alpha,ys);
#			}
#		return(sum(u)/sum(v));
#		}	
	


#b
g <- function(alpha){
	as.numeric(alpha>=2)
	}



