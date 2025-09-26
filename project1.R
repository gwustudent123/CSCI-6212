library(ggplot2)
library(dplyr)

f = function(n) {
Sum=0
niterations=0
j = 2 
a=round(runif(n), digits=0)
b=round(runif(n), digits=0)
start_time = Sys.time()
while (j < n) { 
  k = j 
  ninner=0
  while (k < n) { 
    Sum = Sum+a[k]*b[k] 
    k = k + n^(1/3) * log2(n) 
    ninner = ninner+1
  } 
  j = 2*j 
  if (ninner==0) {niterations=niterations+1}
  else {niterations=niterations+ninner}
} 
end_time = Sys.time()
time_elapsed = (10^9)*as.numeric(end_time - start_time)
time_per_iter = time_elapsed/niterations
data.frame(n,niterations,time_elapsed, time_per_iter, 
           time_complexity=n^(2/3))
}

nvalues = floor(10^(seq(1,8,length.out=100)))
nvalues=data.frame(nvalues,r=rnorm(length(nvalues))) %>% arrange(r) 
nvalues=nvalues$nvalues
nvalues

for (i in 1:length(nvalues)) {
  n = nvalues[i]
  print(paste0("n=",n))
  temp = f(n)
  if (i==1) {df=temp}
  else {df=rbind(df,temp)}
}

scale_factor = median(df$time_per_iter)
scale_factor
df$scaled_time_complexity = df$time_complexity*scale_factor
df = df %>% subset(time_per_iter<2000)

exp = df %>% mutate(logn=log10(n), grp='Experimental Result') %>% rename(time=time_elapsed) %>%
  select(grp, n, logn, time)
theoretical = df %>% mutate(logn=log10(n), grp='Theoretical Result') %>% 
  rename(time=scaled_time_complexity) %>% select(grp, n, logn, time)
df2 = rbind(exp,theoretical)

ggplot(df2, aes(x=n, y=time)) + geom_point(aes(color=grp)) + geom_line(aes(color=grp)) + labs(x="n", y="nanoseconds", color="") + theme_minimal() 

ggplot(df2, aes(x=logn, y=time)) + geom_point(aes(color=grp)) + geom_line(aes(color=grp)) + labs(x="log(n)", y="nanoseconds", color="") + theme_minimal() 


