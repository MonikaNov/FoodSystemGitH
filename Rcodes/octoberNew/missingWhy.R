complete.cases(isdataTS)
isdataTS[which(complete.cases(isdataTS)==FALSE & !is.na(isdataTS$Yield)),]
nrow(isdataTS[which(complete.cases(isdataTS)==FALSE & !is.na(isdataTS$Yield)==TRUE),])