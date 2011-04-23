library(tools)
for (src in list.files("R")) source(paste("R", src, sep = "/"))

getBoardList <- r2ch.getBoardList
getSubjectList <- r2ch.getSubjectList
getThread <- r2ch.getThread
printThread <- r2ch.printThread
exported <- c("getBoardList", "getSubjectList", "getThread", "printThread")

package.skeleton(list = c(ls(pattern = "r2ch.*"), exported), name = "R2ch")
