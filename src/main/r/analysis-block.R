d <- read.csv("/Users/thijs/Desktop/flat-data21.csv",
              sep=",",header=FALSE,
              col.names=c("label","numCharacters","relativeLength","startRelative","endRelative",
                          "ratioPeriods","ratioPunctuation","ratioDashes","numWords",
                          "averageWordLength","ratioCapitalizedWords","endsInPeriod","ratioCharsInLink",
                          "ratioStopWords","medSentenceLength","startPositionInBody","MzUnlikelyCandidate",
                          "MzOkMaybeItsACandidate","MzPositive","MzNegative","MzExtraneous","MzByline"))
d$label = as.factor(d$label)

d <- read.csv("/Users/thijs/Desktop/flat-data3.csv",
              sep=",",header=FALSE,
              col.names=c("label","numCharacters","ratioCharsInLink","ratioStopWords"))
d$label = as.factor(d$label)


library(caret)

s <- sample.int(dim(d)[1],size=20000)
sam <- d[s,]
#slab <- d[s,1]
#cols <- ifelse(slab==1,"red","blue")
#pairs(sam,col=cols)
tres <- train(label~.,data=sam,"svmLinear")
tres$results
varImp(tres)

table(sam$label)
