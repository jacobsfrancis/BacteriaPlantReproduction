setwd("~/Documents/ScienceProjects/2023/KtProjects/Sequences/Trimmed/")

library(sangerseqR)

ITS<-read.scf("001_134777_799F_H01.scf")

forBlast <- ITS@basecalls

Files <- list.files(pattern="*.scf")

extractseqences <- function(x){
  file<-read.scf(x)
  return(c(ID=x,SEQ=file@basecalls))
}

data<-plyr::ldply(.data = Files, .fun = extractseqences)

myWrite<- function (x) {paste(">",x["ID"],"\n",x["SEQ"],"\n")}
myWrite(data[1,])

out<-paste(apply(X=data,MARGIN = 1, FUN = function(x){myWrite(x)}))

write(out,file="test.fasta")

?