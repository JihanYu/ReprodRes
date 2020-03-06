workingpath <- "C:\\Users\\MED1\\ReprodRes"
setwd(workingpath)

##### read data & preprocessing data #####
download.file ("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "stormdata.csv.bz2")
storm <- read.csv("stormdata.csv.bz2", header=TRUE)

# types of events #
event <- c("ASTRONOMICAL LOW TIDE", "AVALANCHE", "BLIZZARD", "COASTAL FLOOD", "COLD/WIND CHILL",
           "DEBRIS FLOW", "DENSE FOG", "DENSE SMOKE", "DROUGHT", "DUST DEVIL",
		   "DUST STORM", "EXCESSIVE HEAT", "EXTREME COLD/WIND CHILL", "FLASH FLOOD", "FLOOD",
		   "FROST/FREEZE", "FUNNEL CLOUD", "FREEZING FOG", "HAIL", "HEAT",
		   "HEAVY RAIN", "HEAVY SNOW", "HIGH SURF", "HIGH WIND", "HURRICANE",
		   "ICE STORM", "LAKE-EFFECT SNOW", "LAKESHORE FLOOD", "LIGHTNING", "MARINE HAIL",
		   "MARINE HIGH WIND", "MARINE STRONG WIND", "MARINE THUNDERSTORM WIND", "RIP CURRENT", "SEICHE",
		   "SLEET", "STORM SURGE/TIDE", "STRONG WIND", "THUNDERSTORM WIND", "TORNADO",
		   "TROPICAL DEPRESSION", "TROPICAL STORM", "TSUNAMI", "VOLCANIC ASH", "WATERSPOUT",
		   "WILDFIRE", "WINTER STORM", "WINTER WEATHER")

name.select <- c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
storm.sub <- subset(storm, select=name.select)
rm(storm)

# select cases according to events #
n.event <- length(event)
event.id <- vector()
for (i in c(1:n.event)){
	event.id <- c(event.id, which(storm.sub$EVTYPE==event[i]))
}
storm.sub <- storm.sub[event.id,]

# change types of variables #
as.numeric.factor <- function (x) { as.numeric(levels(x))[x] }
storm.sub$FATALITIES <- as.numeric.factor(storm.sub$FATALITIES)
storm.sub$INJURIES <- as.numeric.factor(storm.sub$INJURIES)
storm.sub$PROPDMG <- as.numeric.factor(storm.sub$PROPDMG)
storm.sub$CROPDMG <- as.numeric.factor(storm.sub$CROPDMG)

storm.sub$EVTYPE <- as.character(storm.sub$EVTYPE)
storm.sub$PROPDMGEXP <- as.character(storm.sub$PROPDMGEXP)
storm.sub$CROPDMGEXP <- as.character(storm.sub$CROPDMGEXP)

# calculate the amounts of damage #
PROPDMG.K.index <- which(storm.sub$PROPDMGEXP == "K")
PROPDMG.M.index <- which(storm.sub$PROPDMGEXP == "M")
PROPDMG.B.index <- which(storm.sub$PROPDMGEXP == "B")
CROPDMG.K.index <- which(storm.sub$CROPDMGEXP == "K")
CROPDMG.M.index <- which(storm.sub$CROPDMGEXP == "M")
CROPDMG.B.index <- which(storm.sub$CROPDMGEXP == "B")

storm.sub$PROPDMG[PROPDMG.K.index] <- storm.sub$PROPDMG[PROPDMG.K.index] * 10^3
storm.sub$PROPDMG[PROPDMG.M.index] <- storm.sub$PROPDMG[PROPDMG.M.index] * 10^6
storm.sub$PROPDMG[PROPDMG.B.index] <- storm.sub$PROPDMG[PROPDMG.B.index] * 10^9
storm.sub$CROPDMG[CROPDMG.K.index] <- storm.sub$CROPDMG[CROPDMG.K.index] * 10^3
storm.sub$CROPDMG[CROPDMG.M.index] <- storm.sub$CROPDMG[CROPDMG.M.index] * 10^6
storm.sub$CROPDMG[CROPDMG.B.index] <- storm.sub$CROPDMG[CROPDMG.B.index] * 10^9

# total amount of health problems & economic problems #
storm.sub$HEALTHPOP <- storm.sub$FATALITIES + storm.sub$INJURIES
storm.sub$DMGTOTAL <- storm.sub$PROPDMG + storm.sub$CROPDMG


##### Data analysis #####
library(dplyr)

storm.evtype <- group_by(storm.sub, EVTYPE)

# total amount of damages - population health & economic consequences #
storm.evtype.healthpop.sum <- summarise(storm.evtype, sum.health = sum(HEALTHPOP, na.rm=TRUE))
storm.evtype.dmgtotal.sum <- summarise(storm.evtype, sum.dmg = sum(DMGTOTAL, na.rm=TRUE))

storm.evtype.healthpop.sum$EVTYPE[which(storm.evtype.healthpop.sum$sum.health == max(storm.evtype.healthpop.sum$sum.health))]
storm.evtype.dmgtotal.sum$EVTYPE[which(storm.evtype.dmgtotal.sum$sum.dmg == max(storm.evtype.dmgtotal.sum$sum.dmg))]

par(mfrow=c(1, 2))
barplot(storm.evtype.healthpop.sum$sum.health, main="Health Problems(casuality)", ylab="persons")
barplot(storm.evtype.dmgtotal.sum$sum.dmg, main="Economic Damage", ylab="Dollars")
par(mfrow=c(1,1))

# individual effects of fatality & injury #
storm.evtype.healthpop.sum$EVTYPE[which(storm.evtype.healthpop.sum$sum.health == max(storm.evtype.healthpop.sum$sum.health))]
storm.evtype.dmgtotal.sum$EVTYPE[which(storm.evtype.dmgtotal.sum$sum.dmg == max(storm.evtype.dmgtotal.sum$sum.dmg))]

head( storm.evtype.healthpop.sum[order(storm.evtype.healthpop.sum$sum.health, decreasing=TRUE),], 5 )
head( storm.evtype.dmgtotal.sum[order(storm.evtype.dmgtotal.sum$sum.dmg, decreasing=TRUE),], 5 )

# individual effects of product & crop #
storm.evtype.fatality.sum <- summarise(storm.evtype, sum.fatality = sum(FATALITIES, na.rm=TRUE))
storm.evtype.injuries.sum <- summarise(storm.evtype, sum.injuries = sum(INJURIES, na.rm=TRUE))
storm.evtype.propdmg.sum <- summarise(storm.evtype, sum.propdmg = sum(PROPDMG, na.rm=TRUE))
storm.evtype.cropdmg.sum <- summarise(storm.evtype, sum.cropdmg = sum(CROPDMG, na.rm=TRUE))

par(mfrow=c(1, 2))
barplot(storm.evtype.fatality.sum$sum.fatality, main="FATALITIES", ylab="persons")
barplot(storm.evtype.injuries.sum$sum.injuries, main="INJURIES")
par(mfrow=c(1,1))

storm.evtype.fatality.sum$EVTYPE[which(storm.evtype.fatality.sum$sum.fatality == max(storm.evtype.fatality.sum$sum.fatality))]
storm.evtype.injuries.sum$EVTYPE[which(storm.evtype.injuries.sum$sum.injuries == max(storm.evtype.injuries.sum$sum.injuries))]

par(mfrow=c(1, 2))
barplot(storm.evtype.propdmg.sum$sum.propdmg, main="PRODUCTS", ylab="Dollars")
barplot(storm.evtype.cropdmg.sum$sum.cropdmg, main="CROP")
par(mfrow=c(1,1))

storm.evtype.propdmg.sum$EVTYPE[which(storm.evtype.propdmg.sum$sum.propdmg == max(storm.evtype.propdmg.sum$sum.propdmg))]
storm.evtype.cropdmg.sum$EVTYPE[which(storm.evtype.cropdmg.sum$sum.cropdmg == max(storm.evtype.cropdmg.sum$sum.cropdmg))]



# title : briefly summarizes the data analysis
# synopsis : describes & summarizes the data < 10 sentences
# data processing : how the data were loaded into R & processed for analysis
# results : main results
# figure : 1-3
# start from the raw data file
# which types of events(EVTYPE) are most harmful to population health
# which types of events have the greatest economic consequences
# reproducible
# figure(s) - descriptive captions



write.csv(storm.sub, "stormsub3.csv")

tb_propdmg <- sort(table(storm.sub$PROPDMG), decreasing=TRUE)
tb_propdmgexp <- sort(table(storm.sub$PROPDMGEXP), decreasing=TRUE)
tb_cropdmg <- sort(table(storm.sub$CROPDMG), decreasing=TRUE)
tb_cropdmgexp <- sort(table(storm.sub$CROPDMGEXP), decreasing=TRUE)

write.csv(tb_propdmg, "tb_propdmg.csv")
write.csv(tb_propdmgexp, "tb_propdmgexp.csv")
write.csv(tb_cropdmg, "tb_cropdmg.csv")
write.csv(tb_cropdmgexp, "tb_cropdmgexp.csv")

