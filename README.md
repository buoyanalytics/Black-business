# Black-business

I wrote a Medium article on Black entrepreneurship in the US, using economic and demographic data from the 2010 US census.  The data are distributed as county-level observations of economic activity or county-level population data.  I used the data to construct a metric of business community health, H. using the following equation.

H = p*N

where N is the number of Black owned businesses in a county, and p is the potential of the counties Black business community. p is a measure of Black businesses potential to remain solvent, and is equal to: 

p = Total county-level Black business assets/total county-level Black business payouts (costs).  

In the article I calculate H for all US counties with sufficient data and map H onto a US county-level map.

The link to the article can be found here:

 https://medium.com/@joe_63278/top-ten-best-cities-for-black-entrepreneurship-in-america-c9d1e918c5bf 
 
 
 The R code for this project is here.
 
 
#getting the data
biz <- read.csv("biz_data.csv")
black_biz <- biz[biz$RACE_GROUP.display.label=="Black or African American",]
black_biz <- black_biz[black_biz$NAICS.display.label=="Total for all sectors",]
library(dplyr)



# gsub scripts to change text
black_biz$state <- gsub("^.*, ", "",black_biz[,3])
black_biz$county <- gsub(" .*","",black_biz[,3])


#Get statewide percapita black economic activity
levels(black_biz$RCPALL)[levels(black_biz$RCPALL)=='S'] <- NA
black_biz$total.cash <- as.character(black_biz$RCPALL)
black_biz$total.cash <- as.numeric(black_biz$total.cash)

black_biz$PAYANN <- na_if(black_biz$PAYANN, 'S')
black_biz$PAYANN <- as.character(black_biz$PAYANN)
black_biz$PAYANN <- as.numeric(black_biz$PAYANN)

black_biz$poten <- black_biz$total.cash/black_biz$PAYANN
black_biz$poten <- na_if(black_biz$poten, 'Inf')


black_biz$activ <- black_biz$poten*black_biz$FIRMALL



# get sum of black economic activity
state_econ <- as.data.frame(as.table(tapply(X=black_biz$total.cash, INDEX=list(black_biz$state), FUN=sum, na.rm=T)))
names(state_econ) <- c("state","cash")
# Get black population data
AA_pop <- read.csv("AA_state.csv")
names(AA_pop) <- c("abb.state", "state","pop","propblack")
AA_pop$npop <- as.numeric(gsub(",","",AA_pop$pop))
AA_pop$state <- trimws(AA_pop$state, which="right")
#merge the tables
levels(state_econ$state)[9] <- "DC"
state_econ <- merge(AA_pop, state_econ, by="state")
state_econ$percap <- state_econ$cash/state_econ$npop




#get county biz data

county_biz <-  cbind(black_biz$activ,black_biz$poten)
county_biz <- as.data.frame(county_biz)

county_biz <- cbind(county_biz, gsub(",.*","",black_biz[,3]))
county_biz <- cbind(county_biz,black_biz$GEO.id2)

names(county_biz) <- c("activ","poten","county", "fips",)
#county business maps
county_biz$logactive <- log10(county_biz$activ)
plot_usmap("counties", data=county_biz, values = "logactive", lines="white") +
  scale_fill_gradient(low = "yellow", high = "red", name = "Business activity", label = scales::comma) + 
  theme(legend.position = "right") + labs(title = "Black Business Activity") + geom_polygon(data=state.bounds, mapping =aes(long, lat, group=group), size=1, fill = NA, color = "#ffffff")


#county population map
pop<- read.csv("2010_black_pop.csv")
names(pop) <- c("county","fips","pop")

plot_usmap("counties", data=pop, values = "pop") +
  scale_fill_continuous(low = "blue", high = "red", name = "Population", label = scales::comma) + 
  theme(legend.position = "right") + labs(title = "Black Population")

PP <- pop[order(-pop$pop),]
PP <- PP[c(1:10),]

ggplot(PP, aes(x=reorder(county, -pop), y=pop)) + geom_bar(stat="identity") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank())


# black biz histogram
head(black_biz[order(-black_biz$activ),],10)

BB <- black_biz[order(-black_biz$activ),]
BB <- BB[c(1:10),]

ggplot(BB, aes(x=reorder(county, -activ), y=log10(activ))) + geom_bar(stat="identity") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank())

# compare population size and business activity
pop_biz <- merge(pop, county_biz, by="fips")
pop_biz <- pop_biz[complete.cases(pop_biz),]
plot(pop_biz$activ, pop_biz$pop)
pop_biz$pop.rank <- rank(pop_biz$pop, na.last="keep")
pop_biz$biz.rank <- rank(pop_biz$activ, na.last="keep")
pop_biz$dif <- pop_biz$pop.rank - pop_biz$biz.rank
cor.test(pop_biz$activ, pop_biz$pop)

plot_usmap("counties", data=pop_biz, values = "dif") +
  scale_fill_continuous(low = "blue", high = "red", name = "dif", label = scales::comma) + 
  theme(legend.position = "right") + labs(title = "dif")

PB <- pop_biz[order(-pop_biz$dif),]
PB <- PB[c(1:10),]


# another map
library(socviz)
library(ggthemes)
library(sp)
dat.sp <- county_map
dat.sp <- SpatialPointsDataFrame(c(dat.sp[,c('long','lat')]), data = dat.sp)
library(mapproj)
mapproject(dat.sp$long,dat.sp$lat,projection ="albers", par=c(39,49))
names(dat.sp) <- c("long","lat","order","hole","piece","group","fips")

library(dplyr)
county_biz$fips <- as.character(county_biz$fips)
county_biz$fips[c(1:184)] <- paste("0",county_biz$fips, sep="")
county_full <- left_join(dat.sp, county_biz, by = "fips")
p <- ggplot(data=county_full, mapping = aes(x=long, y=lat, fill=activ, group=group))
p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal() + geom_polygons()
p2 <- p1 + scale_fill_brewer(palette="Blues",
                             labels = c("0-10", "10-50", "50-100", "100-500",
                                        "500-1,000", "1,000-5,000", ">5,000"))
p2 + labs(fill = "biz") + theme_map() +
  guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom")

# county map with state borders
library(usmap)
counties <- us_map("counties")
state.bounds <- us_map("states")
county_biz$fips[c(1:184)] <- paste("0",county_biz$fips, sep="")
county_all <- left_join(counties, county_biz, by="fips")
p <- ggplot(data=county_all, mapping = aes(x=long, y=lat, fill=log10(activ), group=group)) + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor= element_blank(), 
                                                                                                                axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_blank()) + ylim(-3e+06,2e+06)
p1 <- p + geom_polygon(color = "gray95", size = 0.05) + geom_polygon(data=state.bounds, mapping =aes(long, lat, group=group), fill = NA, color = "#ffffff")  +
  scale_fill_gradient2(low = "white", high = "red", na.value = "gray50")

