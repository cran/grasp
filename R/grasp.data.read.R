"grasp.data.read" <-
function(path=""){
XXX<-read.delim(paste(path, "XXX.txt",sep=""),sep=" ")
YYY<-read.delim(paste(path, "YYY.txt",sep=""),sep=" ")
XXXpred<- read.delim(paste(path, "XXXpred.txt",sep=""),sep=" ")
assign("XXX", XXX, env = .GlobalEnv)
assign("YYY", YYY, env = .GlobalEnv)
assign("XXXpred", XXXpred, env = .GlobalEnv)
}

