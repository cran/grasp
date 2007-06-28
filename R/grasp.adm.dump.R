"grasp.adm.dump" <-
function()
{
      destination <- "graspdump.txt"
      cat("", "\n")
      cat("", "\n")
      cat("*************************", "\n")
      print(objects(pattern = "grasp*", envir=.GlobalEnv))
      dump(list=c(objects(pattern = "grasp*", envir=.GlobalEnv)), file = paste(OPTIONS$PATH ,"graspdump.txt",sep=""))
      cat("All GRASP functions dumped at: ")
      cat(paste(OPTIONS$PATH, "\\", destination, sep = ""), "\n")
      cat("*************************", "\n")
}

