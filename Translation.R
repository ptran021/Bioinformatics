#### Get reverse/ complement or both
seq <- c("ATGCATTGGACGTTAG")

revComp <- function(x=seq, mode="RC"){
    replace <- NULL  
    for(i in seq(along=(x[]))){
      if(mode=="C") {
        replace[i] <- chartr("ATGC", "TACG", x[i])
      }
      
      if(mode=="RC" | mode=="R"){
        vecstring <- strsplit(x[i],"")[[1]]
        reverse <- rev(vecstring)
        pasted <- paste(reverse, collapse="")
        
        if(mode=="RC")
          replace <- chartr("ATGC", "TACG", pasted)
        }  
      }
    return(replace)    
}
## reading in from type data frame
mydf <- DNAseq
myvec <- NULL
reverselist <- function(x, mode="RC"){
  for ( i in seq(along=mydf[,1])){
    myvec <- c(myvec,revComp(as.character(mydf[i,2])))
      
  }
  return(myvec)
}

###### Translation
aac2 <- AA[,2]; names(aac2) <- AA[,1]


translate <- function(x=sequence, orf, list=aac2){
  split <- function(x){
    seqin <- gsub("(...)", "\\1_", x)
    seqin <- unlist(strsplit(seqin, "_"))
    seqin <- seqin[grep("^...$", seqin)]
    return(seqin)
  }
  if (orf==1){
      seqin<- split(x)
      tr <- as.character(aac2[seqin])
  }
  if (orf==2){
    x <- gsub("^.", "", x)
    seqin<- split(x)
    tr <- as.character(aac2[seqin])
  }
  if (orf==3){
    x <- gsub("^..", "", x)
    seqin<- split(x)
    tr <- as.character(aac2[seqin])
  }
  return(tr)
}




