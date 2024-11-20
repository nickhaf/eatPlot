# remotes::install_github("weirichs/eatTools", upgrade="never")
# remotes::install_github("weirichs/eatRep", upgrade="never")

library(eatRep)
data(lsa)
rd     <- lsa[which(lsa[,"domain"] == "reading"),]
rdN1   <- rd[which(rd[,"nest"] == 1),]
rdN1y10<- rdN1[which(rdN1[,"year"] == 2010),]
means1 <- repMean(datL = rdN1y10, ID="idstud", wgt="wgt", type = "JK2", PSU = "jkzone", repInd = "jkrep", imp="imp", groups = "country", group.splits = 0:1, dependent = "score", na.rm=FALSE, doCheck=TRUE, engine = "BIFIEsurvey")
trend_2  <- report2(means1, add = list(domain = "reading"))

usethis::use_data(trend_2, overwrite = TRUE)
