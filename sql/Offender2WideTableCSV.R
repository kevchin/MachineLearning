infile <- "middle_DRIPS_Surface1.csv"


basedir <- "./"

library(reshape)
# read data file into data.frame "d"
filename <- paste(basedir, infile, sep="/")
d <- read.table(filename, sep=",", comment.char="", quote="", header=TRUE, as.is=TRUE)

L <- d$OffenderType != 'Device'
d2 <- d[L,]
s.df <- d2[c("ProductName", "OSBuildNumber", "Duration", "uSessionId", "EnergyChangeRate", "OffenderName", "TopOffenderActiveTimePercentage")]


#offender <- offender[L,]


#colnames(s.df)
#
# Shorten the Activator Percentage Name -- > 'A'
#
colnames(s.df) <- c("ProductName", "OSBuildNumber", "Duration", "uSessionId", "EnergyChangeRate", "OffenderName", "A")

s2.df <- reshape(s.df, 
                idvar=c("ProductName", "OSBuildNumber", "Duration", "uSessionId", "EnergyChangeRate"), 
                timevar="OffenderName", 
                direction="wide")
#
# Change the NA to 0
#
s2.df[is.na(s2.df)] <- 0

colSums(s2.df[sapply(s2.df, is.numeric)], na.rm = TRUE)

# result in s2.df
#
write.csv(s2.df, file="offender3.csv", row.names=FALSE)


