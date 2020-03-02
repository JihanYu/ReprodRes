workingpath <- "C:\\Users\\MED1\\ReprodRes"
setwd(workingpath)

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

##### read data #####
download.file ("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "stormdata.csv.bz2")
stormdata <- read.csv("stormdata.csv.bz2", header=TRUE)

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
