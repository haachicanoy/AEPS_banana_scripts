




distDtwMV <- function(listObje)
{
  require(dtw)
  len      <-  length(listObje)
  listDist <- matrix(0,nrow=len,ncol=len)
  
  disDtw <- array(0,len)
 # pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)
 pb <- txtProgressBar(min = 0, max = len, style = 3)
  for(i in 1:len){ 
    
    for(j in i:len){
      listDist[j,i]<- dtw(listObje[[i]],listObje[[j]])$distance
    }
    setTxtProgressBar(pb, i)
  #  info <- sprintf("%d%% done", round((i/(len)*100)))
   # setWinProgressBar(pb, i/(len)*100, label=info)
  }
 close(pb)
  #close(pb)
  rownames <- names(listObje)
  colnames <- names(listObje)
  return(as.dist(listDist))
}










