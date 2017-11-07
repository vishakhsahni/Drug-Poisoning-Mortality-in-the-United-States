library(micromapST)

####################Data Read####################
mydata <- read.csv("redesignr.csv", header=T, as.is=TRUE)
head(mydata)
View(mydata)

####################Micromap Functions####################
micromap1 <- function(stateDF,stateId=NULL,
                      ref=stateNamesFips){
  if(is.null(stateId)) 
    nam <- row.names(stateDF) else
      nam <- stateDF[,stateId]
    nam <- ifelse(nam=="District of Columbia","D.C.",nam)
    check <-  match(nam,row.names(ref)) 
    bad <- is.na(check)
    good <- !bad
    nbad <- sum(bad)
    if(nbad>0){
      warning(paste(nbad,"Unmatch Names Removed",nam[bad]))
      stateDF <- stateDF[!bad,]
      nam <- nam[!bad]
      check <- check[!bad]
      good <- good[!bad]
    }
    ngood <- sum(good)
    if(ngood < 51)warning(paste("Only",ngood,"State Ids"))
    row.names(stateDF) <- ref[check,2]
    return(stateDF)
}

#####################Main Code####################
design <- micromap1(mydata, "State")
head(design)

stateDf <- design
nam<- colnames(stateDf)
names(nam) =1:length(nam)
nam

####################For Years 1999-2003####################
panelDesc <- data.frame(
  type=c('mapcum','id','dot','dot','dot','dot'),
  lab1=rep("",6),
  lab2=c('' ,'',"1999","2000","2001","2002"),
  col1 = nam[c(NA,NA,3,4,5,6)]
)
t(panelDesc)
####################Export File in PDF Output####################
fName = "MP.pdf"
pdf(file=fName,width=7.5,height=10)
micromapST(design, panelDesc,
           sortVar=2,ascend=FALSE,
           title=c("Age-Adjusted Death Rates for Drug Poisoning, 1994-2014" ))
dev.off()

####################For Years 2004-2007####################
panelDesc <- data.frame(
  type=c('mapcum','id','dot','dot','dot','dot'),
  lab1=rep("",6),
  lab2=c('' ,'','2003',"2004","2005","2006"),
  col1 = nam[c(NA,NA,7,8,9,10)]
)
t(panelDesc)

####################Export File in PDF Output####################
fName = "MP1.pdf"
pdf(file=fName,width=7.5,height=10)
micromapST(design, panelDesc,
           sortVar=2,ascend=FALSE,
           title=c("Age-Adjusted Death Rates for Drug Poisoning, 1994-2014" ))
dev.off()

####################For Years 2008-2011####################
panelDesc <- data.frame(
  type=c('mapcum','id','dot','dot','dot','dot'),
  lab1=rep("",6),
  lab2=c('' ,'','2007',"2008","2009","2010"),
  col1 = nam[c(NA,NA,11,12,13,14)]
)
t(panelDesc)

####################Export File in PDF Output####################
fName = "MP2.pdf"
pdf(file=fName,width=7.5,height=10)
micromapST(design, panelDesc,
           sortVar=2,ascend=FALSE,
           title=c("Age-Adjusted Death Rates for Drug Poisoning, 1994-2014" ))
dev.off()

####################For Years 2012-2014####################
panelDesc <- data.frame(
  type=c('mapcum','id','dot','dot','dot','dot'),
  lab1=rep("",6),
  lab2=c('' ,'',"2011","2012","2013","2014"),
  col1 = nam[c(15,16,17)]
)
t(panelDesc)

####################Export File in PDF Output####################
fName = "MP3.pdf"
pdf(file=fName,width=7.5,height=10)
micromapST(design, panelDesc,
           sortVar=2,ascend=FALSE,
           title=c("Age-Adjusted Death Rates for Drug Poisoning, 1994-2014" ))
dev.off()




