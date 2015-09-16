##############MCMC################
#####Author: Jiachuan Tian########
#####Time: 0423105        ########
##################################

set.seed(1);

f <- function(alpha,ys){
	exp(alpha*sum(ys)-length(ys)*exp(alpha))*((1+alpha^2/300)^(-2));
#  exp(alpha*sum(ys) - 100*exp(alpha))*(1 + alpha^2/300)^(-2);
  
	}
##Check Function f
##nn <- runif(10000,-5,5);
##results <- lapply(nn, f, ys=ys);
##plot(nn,results);

g_miu <- function(alpha){
	alpha;
	}
g_miu_square <- function(alpha){
	alpha^2;
	}

x <- c();
x[1] <- 0;
i <- 1;
tau_square <- 0.00000001;
N <- 10000;
ys <- scan("http://www.stat.psu.edu/~mharan/515/hwdir/hw09.dat")

while (length(x)<N){
	y <- rnorm (1, mean=x[i],sd=sqrt(tau_square));
	if (f(x[i],ys)==0){
	x[i+1] <- y;
	}
	else{
	u <- runif(1, 0, 1);
	if (u<min(1,(f(y,ys)/f(x[i],ys)))){
	x[i+1] <- y;
	}
	else{
	x[i+1] <- x[i];
	}	
	}
	i <- i+1;
	}
plot(x)


#results <- lapply(x, g_miu)

miu_MCMC <- function(x,N){
		sum(as.numeric(lapply(x, g_miu)))/N;
		}
miu_square_MCMC <- function(x,N){
  sum(as.numeric(lapply(x, g_miu_square)))/N;
}

#Change number of MC runs
n = seq(100, 5000, by = 100);

#plot miu
plot(n,lapply(n,miu_MCMC,x=x));
lines(n,lapply(n,miu_MCMC,x=x));

#plot square_bar
plot(n, lapply(n,miu_square_MCMC,x=x));
lines(n, lapply(n,miu_square_MCMC,x=x));
