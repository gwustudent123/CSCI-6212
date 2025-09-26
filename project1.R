
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
for (n in nvalues) {
print(f(n))
}
