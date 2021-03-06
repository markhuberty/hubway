setwd("~/projects/hubway")
source("./code/hubway_functions.R")

## Load up the data
stations <- read.csv("./data/stations.csv")
trips <- read.csv("./data/trips.csv")

## Format the datetime data correctly
timedate.format <- "%y-%m-%d %H:%M:%S"
trips$start.datetime<- strptime(trips$start_date,
                                format=timedate.format
                                )
trips$end.datetime<- strptime(trips$end_date,
                              format=timedate.format
                              )

## Match the station names and locations
trips$start.station.name <-
  as.character(stations$name[match(trips$start_station_id,
                      stations$id)
                ]
               )
trips$end.station.name <-
  as.character(stations$name[match(trips$end_station_id,
                      stations$id)
                ]
               )

trips$start.lat <-
  stations$lat[match(trips$start_station_id,
                     stations$id
                     )
               ]
trips$start.long <-
  stations$lng[match(trips$start_station_id,
                     stations$id
                     )
               ]
trips$end.lat <-
  stations$lat[match(trips$end_station_id,
                     stations$id
                     )
               ]
trips$end.long <-
  stations$lng[match(trips$end_station_id,
                     stations$id
                     )
               ]


## Calculate the traveler age
trips$age <- 2012 - trips$birth_date

## People going places together: look for pairs of bikes checked out
## from the same station in the same n-minute interval, going to the
## same station.

## Define the blocking vector
station.min.block <- paste(trips$start_station_id,
                           trips$end_station_id,
                           round(trips$start.datetime, "min"),
                           round(trips$end.datetime, "min"),
                           sep="."
                           )

## Determine which blocks might have pairs of travelers
block.tripcount <- table(station.min.block)
pair.blocks.idx <- which(block.tripcount == 2)

blocks.to.examine <- names(block.tripcount)[pair.blocks.idx]

## Pull back the indices in trips
block.idx.groups <- sapply(blocks.to.examine,
                           function(x) which(station.min.block == x)
                           )

## Extract and combine the trip pairs
trips.1 <- trips[block.idx.groups[1,],
                 c("duration",
                   "start.station.name",
                   "end.station.name",
                   "start.datetime",
                   "end.datetime",
                   "start.lat",
                   "end.lat",
                   "start.long",
                   "end.long",
                   "age",
                   "subscription_type",
                   "gender"
                   ),
                 ]
trips.2 <- trips[block.idx.groups[2,],
                 c("age",
                   "subscription_type",
                   "gender"
                   ),]
names(trips.1) <- c("duration",
                   "start.station.name",
                   "end.station.name",
                   "start.datetime",
                   "end.datetime",
                    "start.lat",
                    "end.lat",
                    "start.long",
                    "end.long",
                   "age1",
                   "sub1",
                   "gender1"
                   )
names(trips.2) <- c("age2",
                    "sub2",
                    "gender2"
                    )
trips.together <- cbind(trips.1,
                        trips.2
                        )
trips.alone <- trips[-c(block.idx.groups[1,],
                        block.idx.groups[2,]
                        ),
                     ]
rm(trips.1, trips.2, block.idx.groups, station.min.block)


## Compute distance as the great circle routes between
## start and end points
trips.together$trip.distance <- deg.dist(trips.together$start.long,
                                         trips.together$start.lat,
                                         trips.together$end.long,
                                         trips.together$end.lat
                                         )
trips.alone$trip.distance <- deg.dist(trips.alone$start.long,
                                      trips.alone$start.lat,
                                      trips.alone$end.long,
                                      trips.alone$end.lat
                                      )

## Calculate the speed in km/hour and take the trimmed means
trips.together$speed <-
  trips.together$trip.distance / (trips.together$duration / 3600)
trips.alone$speed <-
  trips.alone$trip.distance / (trips.alone$duration / 3600)
mean(trips.together$speed, trim=0.10, na.rm=TRUE)
mean(trips.alone$speed, trim=0.10, na.rm=TRUE)

## Plot the speed
df.speed <- data.frame(c(rep("together", nrow(trips.together)),
                         rep("alone", nrow(trips.alone))),
                       c(trips.together$speed, trips.alone$speed)
                       )
names(df.speed) <- c("group", "speed")
levels(df.speed$group) <- c("Trips alone", "Trips together")


plot.speed <- ggplot(df.speed[df.speed$speed > 0,],
                     aes(x=speed,
                         colour=group
                         )
                     ) +
  geom_density() +
  scale_x_continuous("Speed (km/h)",
                     limits=c(0,20)
                     ) +
  scale_y_continuous("Proportion of trips at speed") +
  scale_colour_manual("Trip group", values=c("Trips alone" = "blue",
                                      "Trips together" = "red")) +
  theme_bw()
ggsave(plot.speed, file="./figures/plot_trip_speed.pdf")



## Tabulate gender and subscription type
together.gender <- prop.table(table(trips.together$gender1,
                                    trips.together$gender2
                                    )
                              )
together.sub <- table(trips.together$sub1,
                      trips.together$sub2
                      )

## Sample random genders and see what we get
random.gender.pairs <- matrix(ncol=2,
                              sample(trips.alone$gender,
                                     5000,
                                     replace=FALSE)
                              )
prop.table(table(random.gender.pairs[,1],
                 random.gender.pairs[,2]
                 )
           )

## Compute age differences and mean ages
together.mean.age <- mean(c(trips.together$age1, trips.together$age2),
                          na.rm=TRUE
                          )
alone.mean.age <- mean(trips.alone$age, na.rm=TRUE)

together.age.diff <- abs(trips.together$age1 - trips.together$age2)

## Sample age diffs to get age diff in expectation if pairing
## were random
random.ages <- matrix(ncol=2, sample(trips.alone$birth_date,
                        5000,
                        replace=FALSE)
                      )
random.agediff <- abs(random.ages[,1] - random.ages[,2])

df.agediff <- rbind(cbind("together", together.age.diff),
                    cbind("random", random.agediff)
                    )
df.agediff <- as.data.frame(df.agediff)
names(df.agediff) <- c("trip.type", "age.diff")
df.agediff$age.diff <- as.numeric(as.character(df.agediff$age.diff))
levels(df.agediff$trip.type) <- c("Random pair", "Companions")

plot.age.diff <- ggplot(df.agediff,
                        aes(x=age.diff)
                        ) +
  geom_density() +
  facet_wrap(~ trip.type) +
  scale_x_continuous("Age difference (years)") +
  scale_y_continuous("Trip frequency") +
  theme_bw()
ggsave(plot.age.diff, file="./figures/plot_age_diff.pdf")
print(plot.age.diff)

## Test the agediff
## Note there's something a touch confounded here: if the age
## distribution is more compressed, then the age diff will by
## definition be more compressed, so the random draw isn't the right check.
t.test.agediff <- t.test(together.age.diff, random.agediff)

## Plot the age differences for the entire population
## Versus the together subset
plot(density(together.age.diff, na.rm=TRUE))
lines(density(random.agediff, na.rm=TRUE), col="red")

## check the trip duration
t.test.duration <-
  t.test(trips.alone$duration[trips.alone$duration < 24*60],
         trips.together$duration[trips.together$duration < 24*60]
         )


## Generate the heatmap for station usage by
## pairs
tab <- as.data.frame(table(trips.together$start.station.name,
                           trips.together$end.station.name
                           )
                     )
names(tab) <- c("start.station", "end.station", "trip.freq")
tab <- tab[tab$trip.freq > 0,]
tab$start.station <- reorder(substr(tab$start.station, 1, 15), tab$trip.freq)
tab$end.station <- reorder(substr(tab$end.station, 1, 15), tab$trip.freq)

plot.station.pairs <- ggplot(tab,
                             aes(x=substr(start.station, 1, 15),
                                 y=substr(end.station, 1, 25),
                                 fill=trip.freq)
                             ) +
  geom_tile() +
  opts(axis.text.x=theme_text(angle=90, hjust=0))
print(plot.station.pairs)


## Most popular stations for pairs
pairs.popular.end <- sort(table(trips.together$end.station),
                          decreasing=TRUE
                          )[1:10]
alone.popular.end <- sort(table(trips.alone$end.station),
                          decreasing=TRUE
                          )[1:10]
pairs.popular.start <- sort(table(trips.together$start.station),
                            decreasing=TRUE
                            )[1:10]
alone.popular.start <- sort(table(trips.alone$start.station),
                            decreasing=TRUE
                            )[1:10]

pairs.station.popularity <-
  aggregate(rep(1, nrow(trips.together)),
            by=list(trips.together$start.station.name,
              trips.together$end.station.name),
            FUN="sum"
            )
alone.station.popularity <-
  aggregate(rep(1, nrow(trips.alone)),
            by=list(trips.alone$start.station.name,
              trips.alone$end.station.name),
            FUN="sum"
            )
names(pairs.station.popularity) <-
  names(alone.station.popularity)<-
  c("start_station", "end_station", "trip_count")
pairs.station.popularity$trip_count <-
  pairs.station.popularity$trip_count /
  min(pairs.station.popularity$trip_count)
pairs.station.popularity$type <- "pair"
alone.station.popularity$type <- "alone"
alone.station.popularity$trip_count <-
  alone.station.popularity$trip_count /
  min(alone.station.popularity$trip_count)

popularity.out <- rbind(pairs.station.popularity,
                        alone.station.popularity
                        )

write.csv(popularity.out,
          "./data/pairwise_station_popularity.csv",
          row.names=FALSE
          )

## When do they leave?
## Function

median.pair.departure.time <-
  median(hourfun(trips.together$start.datetime))
median.alone.departure.time <- median(hourfun(trips.alone$start.datetime))

mean.pair.arrival.time <- mean(hourfun(trips.together$end.datetime))
mean.alone.arrival.time <- mean(hourfun(trips.alone$end.datetime))

## Generate a consolidated time dataframe and plot
## times and days of departure
together.time.df <- data.frame("together",
                               trips.together$start.datetime,
                               trips.together$end.datetime
                               )
alone.time.df <- data.frame("all",
                          trips.alone$start.datetime,
                          trips.alone$end.datetime
                          )
names(alone.time.df) <- names(together.time.df) <-
  c("group", "start.time", "end.time")
time.df <- rbind(together.time.df, alone.time.df)
time.df <- melt(time.df, id.var="group")

levels(time.df$variable) <- c("Arrival Time", "Departure Time")
levels(time.df$group) <- c("Trips together", "Trips alone")

time.df$weekday <- weekdays(time.df$value)
time.df$weekday <- factor(time.df$weekday,
                          levels=c("Sunday",
                            "Monday",
                            "Tuesday",
                            "Wednesday",
                            "Thursday",
                            "Friday",
                            "Saturday"
                            )
                          )

plot.times <- ggplot(time.df,
                     aes(x=hourfun(value),
                         group=group,
                         colour=group
                         )
                     ) +
  geom_density() +
  scale_x_continuous(limits=c(0,24),
                     breaks=c(6,12,18),
                     labels=c("6 AM", "Noon", "6 PM")
                     ) +
  xlab("") +
  ylab("Trip Frequency") +
  #opts(axis.text.x=theme_text(angle=90, hjust=0)) +
  scale_colour_manual("Trip Group", values=c("red", "blue")) +
  facet_wrap(~ variable) +
  theme_bw()##  +
  ## opts(strip.text.x=theme_text(size=12),
  ##      strip.text.y=theme_text(size=12),
  ##      scale.text.x=theme_text(size=12),
  ##      axis.text.y=theme_blank(),
  ##      axis.text.x=theme_text(size=12, colour="black"),
  ##      axis.title.y=theme_text(size=12),
  ##      panel.background=theme_rect(fill="transparent", colour=NA),
  ##      panel.grid.minor=theme_blank(),
  ##      panel.grid.major=theme_blank(),
  ##      plot.background=theme_rect(fill="transparent", colour=NA)
  ##      )
ggsave(plot.times,
       file="./figures/plot_departure_arrival_times.pdf"## ,
       ## bg="transparent"
       )

plot.days <- ggplot(time.df[time.df$variable=="Arrival Time",],
                    aes(x=weekday
                        )
                    ) +
  geom_histogram() +
  facet_grid(group ~ ., scales="free_y") +
  scale_y_continuous("Trip Count") +
  scale_x_discrete("") +
  theme_bw()
  ## opts(strip.text.x=theme_text(size=14),
  ##      strip.text.y=theme_text(size=14),
  ##      axis.text.y=theme_blank(),
  ##      axis.text.x=theme_text(size=14, colour="black"),
  ##      panel.background=theme_rect(fill="transparent", colour=NA),
  ##      panel.grid.minor=theme_blank(),
  ##      panel.grid.major=theme_blank(),
  ##      plot.background=theme_rect(fill="transparent", colour=NA))
ggsave(plot.days,
       file="./figures/plot_trip_weekdays.pdf"## ,
       ## bg="transparent"
       )


## Leaving by day of week

plot.time.days <- ggplot(time.df[time.df$variable=="Arrival Time",],
                         aes(x=hourfun(value),
                             colour=group
                             )
                         ) +
  geom_density() +
  facet_grid(. ~ weekday) +
  scale_x_continuous("Departure Time",
                     breaks=c(12),
                     labels=c("Noon")
                     ) +
  scale_y_continuous("Trip Frequency") +
  scale_colour_manual("Trip Group", values=c("red", "blue")) +
  theme_bw()##  +
  ## opts(strip.text.x=theme_text(size=14),
  ##      axis.text.y=theme_blank(),
  ##      panel.background=theme_rect(fill="transparent", colour=NA),
  ##      panel.grid.minor=theme_blank(),
  ##      panel.grid.major=theme_blank(),
  ##      plot.background=theme_rect(fill="transparent", colour=NA)
  ##      )
ggsave(plot.time.days,
       file="./figures/plot.time.weekdays.pdf"## ,
       ## bg="transparent"
       )
