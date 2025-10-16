
rightShiftMax = function(a, i=NA, j=NA) {
  if (is.na(i)) {i=1}
  if (is.na(j)) {j=length(a)}
  
  #increment counter
  assign("ncalls", ncalls+1, envir = .GlobalEnv)
  
  #print(paste0("rightShiftMax(a, ", i, ", ", j, ")"))
  
  #Base case
  if (j-i<=1) {
    paste0("comparing ",i,":", a[i], " to ", j, ":", a[j])
    if (a[i]>=a[j]) {return(a[i])}
    else {return(a[j])}
  }
  
  else {
  #Recursive calls
  k = floor((i+j)/2)
  #print(paste0("comparing ", a[i], " at position ", i, " to ", a[k], " at position ", k))
  if (a[i]>a[k]) {rightShiftMax(a, i, k-1)}
  else if (a[i]<a[k]) {rightShiftMax(a, k, j)}
  }
}

rightShift = function(x, p) {
  c(x[(length(x)-p+1):length(x)], x[1:(length(x)-p)])
}

nvalues=seq(10, 100000000, length=100)
for (i in 1:length(nvalues)) {
n=nvalues[i]
a = 1:n
shift=floor(n/3)
a = rightShift(a, shift)
print(a)
ncalls=0
t1=Sys.time()
max=rightShiftMax(a)
t2=Sys.time()
df1 = data.frame(n=length(a), shift=shift, ncalls=ncalls, time=as.numeric(t2-t1), max=max, max2=max(a))
if (i==1) {df=df1}
else {df=rbind(df,df1)}
}

