

full <- read.csv2(file = file.choose(new = FALSE))

cat <- "StudentMC1"
output <- paste0(cat,".gift")


write("\n",file=paste(output),append=FALSE) #deletes file!




for (i in 1:dim(full)[1]){
  df <- full[i, ]
  attach(df)
  subcat <- ifelse(is.na(Einheit),"uncategorized",Einheit)
  
  numT <- sum(df[grep(pattern = "TF", x = names(df))])
  
  if(numT==5){
    Tpercent <- "~%20%"
    Fpercent <- "~%0%"}
  if(numT==4){
    Tpercent <- "~%25%"
    Fpercent <- "~%-100%"}
  if(numT==3){
    Tpercent <- "~%33.33333%"
    Fpercent <- "~%-50%"}
  if(numT==2){
    Tpercent <- "~%50%"
    Fpercent <- "~%-33.33333%"}
  if(numT==1){
    Tpercent <- "~%100%"
    Fpercent <- "~%-25%"}
  
  write(paste("$CATEGORY: ",cat,"/",Einheit, sep=""),file=paste(output),append=TRUE)
  write("\n",file=paste(output),append=TRUE)
  
  qid <- paste(Einheit,
               substr(MATNR,start = 2, stop = 5),
               round(runif(n = 1, min = 1000, max = 9999),0), sep = "")
  
  q <- paste("::",qid,"::",
             df$Question, " {",
             ifelse(TF1==1,Tpercent,Fpercent),Answer1,"#",FB1,
             ifelse(TF2==1,Tpercent,Fpercent),Answer2,"#",FB2,
             ifelse(TF3==1,Tpercent,Fpercent),Answer3,"#",FB3,
             ifelse(TF4==1,Tpercent,Fpercent),Answer4,"#",FB4,
             ifelse(TF5==1,Tpercent,Fpercent),Answer5,"#",FB5,
             "####","Falls du mehr Informationen benoetigst oder andere Antworten als richtig identifizierst, frage doch im Forum nach! Nenne dabei die FragenID: ",qid,".",
             "}",
             sep = "")
  
  write(q,file=paste(output),append=TRUE)
  write("\n",file=paste(output),append=TRUE)

}
