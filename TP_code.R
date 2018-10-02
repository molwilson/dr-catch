## Catch Data Analysis, Compiled
# Tyler Pavlowich
# 30 July 2014


# packages
library(ggplot2)
library(gridExtra)
library(plyr)
library(reshape2)


# Data -----
# d.all <- read.csv("Users/Tyler/Desktop/Buen Hombre Research/Data files/Catch Data/Catch Data, compiled 2.csv")

d.all <- Catch_Data_compiled_2
names(d.all) <- make.names(names(d.all), unique=TRUE)
d.all$Year <- as.factor(as.character(d.all$Year))
d.all$Method <- factor(d.all$Method, levels=c("Compressor","Free","Gillnet","Trunk","Offshore"))
d.videos <- d.all[!(is.na(d.all$First.number)),]

# # d.fleet <- read.csv("Users/Tyler/Desktop/Buen Hombre Research/Data files/Catch Data/Fleet effort, 2013 and 2014 2.csv")
# d.fleet <- 
# d.fleet$Year <- as.factor(as.character(d.fleet$Year))
# d.fleet.sum <- ddply(d.fleet, c("Date", "Year", "Method"), summarise,
#                      Fishers.per.day = sum(Number.of.fishermen))
# d.fleet.sum$Method <- factor(d.fleet.sum$Method, levels=c("comp", "free", "gillnet", "trunk", "offshore", "cukes"))
# d.fleet.sum2 <- ddply(d.fleet.sum, c("Date", "Year"), summarise, 
#                       Total.fishers.per.day = sum(Fishers.per.day))




##  Data Visualization  -----

# # Yield, first and second class per capita
# ggplot() +  scale_x_discrete(breaks=c(2013,2014), labels=c("2013","2014")) +
#   geom_point(data=d.all[d.all$Method=="Free" & d.all$Year%in% c("2013","2014"), ], aes(x=Year-0.1, y=First.weight/Number.of.Fishermen), colour="blue") +
#   geom_point(data=d.all[d.all$Method=="Compressor"& d.all$Year%in% c("2013","2014"),], aes(x=Year+0.1, y=First.weight/Number.of.Fishermen), colour="red") +
#   labs(title="First class weight, per capita", x="Year", y="Catch weight (lbs)") +
#   scale_x_discrete(breaks=c(2013,2014), labels=c("2013","2014"))
#   theme(legend.background=element_blank())
#   
#   
# ggplot() +
#   geom_point(data=d.all[d.all$Method=="Free", ], aes(x=Year-0.1, y=Second.weight/Number.of.Fishermen), colour="blue") +
#   geom_point(data=d.all[d.all$Method=="Compressor",], aes(x=Year+0.1, y=Second.weight/Number.of.Fishermen), colour="red") +
#   ggtitle("Second class weight, per capita")


# Here's the other way to do it

# Total  ** This plot includes all methods, but those data are hard/impossible to visualize in a meaningful way **
p.total <- ggplot(data=d.all[d.all$Method!="cukes",], aes(x=Year, y=(First.weight+Second.weight+Third.weight+Lionfish.weight+Lobster.large.weight+Lobster.small.weight+Octopus.weight+Crab.weight+Conch.weight+Eel.weight)/Number.of.Fishermen, colour=Method, shape=Method, fill=Method)) +
  stat_summary(fun.data="mean_cl_boot", geom="crossbar", colour="black") +
  geom_point(size=4, position=position_jitter(width=0.5)) +
  facet_grid(.~Method) +
  labs(x="Year", y="", title="Total Catch") +    # no y axis for panel figure
  scale_colour_manual(name="Gear Type", values=c("red", "blue", "green", "orange", "purple"), labels=c("compressor", "freediving", "gillnet", "trunkfish", "offshore")) +
  scale_shape_discrete(name="Gear Type", labels=c("compressor", "freediving", "gillnet", "trunkfish", "offshore")) +
  scale_fill_manual(name="Gear Type", values=c("white", "white","white","white","white"), labels=c("compressor", "freediving", "gillnet", "trunkfish", "offshore")) +
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none",            #remove legend for panel figure
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14),
        title=element_text(size=18)) +
        strip.background=element_rect(fill="white")+
        strip.text=elemen_text(size=14) +
  coord_cartesian(ylim=c(0,70.5))  

                             
# Total  ***** This plot is the same as what I'm presenting to Freddie in the October 2014 report, but the widths are different. The formatting of the different classes is as it appears in the final figure. 
p.total <- ggplot(data=d.all[d.all$Method%in% c("Free","Compressor") & d.all$Year%in% c("2013","2014"),], aes(x=Year, y=(First.weight+Second.weight+Third.weight+Lionfish.weight+Lobster.large.weight+Lobster.small.weight+Octopus.weight+Crab.weight+Conch.weight+Eel.weight)/Number.of.Fishermen, colour=Method, shape=Method, fill=Method)) +
  stat_summary(fun.data="mean_cl_boot", geom="crossbar", position="dodge", colour="black") +
  geom_point(size=4, position=position_jitterdodge(dodge.width=0.9, jitter.width=1)) +
  labs(x="Year", y="Weight caught, per person (lbs)", title="Total Catch") +
  scale_colour_manual(name="Gear Type", values=c("red","blue"), labels=c("compressor","freediving")) +
  scale_shape_discrete(name="Gear Type", labels=c("compressor","freediving")) +
  scale_fill_manual(name="Gear Type", values=c("white", "white"), labels=c("compressor","freediving"), guide=FALSE) +
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position=c(0.85,0.85),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        title=element_text(size=18)) +
  coord_cartesian(ylim=c(0,71))



# First class
p.first <- ggplot(data=d.all[d.all$Method%in% c("Free","Compressor") & d.all$Year%in% c("2013","2014"),], aes(x=Year, y=First.weight/Number.of.Fishermen, colour=Method, shape=Method, fill=Method)) +
  stat_summary(fun.data="mean_cl_boot", geom="crossbar", position="dodge", colour="black") +
  geom_point(size=4, position=position_jitterdodge(dodge.width=0.9, jitter.width=1)) +
  labs(x="", y="Weight caught, per person (lbs)", title="First Class Fish") +
  scale_colour_manual(name="Gear Type", values=c("red","blue"), labels=c("compressor","freediving")) +
  scale_shape_discrete(name="Gear Type", labels=c("compressor","freediving")) +
  scale_fill_manual(name="Gear Type", values=c("white", "white"), labels=c("compressor","freediving"), guide=FALSE) +
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position=c(0.85,0.7),              #site legend for panel figure
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        title=element_text(size=18)) +
  coord_cartesian(ylim=c(0,40))  

# Second class
p.second <- ggplot(data=d.all[d.all$Method%in% c("Free","Compressor") & d.all$Year%in% c("2013","2014"),], aes(x=Year, y=Second.weight/Number.of.Fishermen, colour=Method, shape=Method, fill=Method)) +
  stat_summary(fun.data="mean_cl_boot", geom="crossbar", position="dodge", colour="black") +
  geom_point(size=4, position=position_jitterdodge(dodge.width=0.9, jitter.width=1)) +
  labs(x="", y="", title="Second Class Fish") +
  scale_colour_manual(name="Gear Type", values=c("red", "blue")) +
  scale_shape_discrete(name="Gear Type") +
  scale_fill_manual(name="Gear Type", values=c("white", "white")) +
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none",             #remove legend for panel figure
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14),
        title=element_text(size=18)) +
  coord_cartesian(ylim=c(0,40))  

# Third class
p.third <- ggplot(data=d.all[d.all$Method%in% c("Free","Compressor") & d.all$Year%in% c("2013","2014"),], aes(x=Year, y=Third.weight/Number.of.Fishermen, colour=Method, shape=Method, fill=Method)) +
  stat_summary(fun.data="mean_cl_boot", geom="crossbar", position="dodge", colour="black") +
  geom_point(size=4, position=position_jitterdodge(dodge.width=0.9, jitter.width=1)) +
  labs(x="Year", y="Weight caught, per person (lbs)", title="Third Class Fish") +
  scale_colour_manual(name="Gear Type", values=c("red","blue")) +
  scale_shape_discrete(name="Gear Type") +
  scale_fill_manual(name="Gear Type", values=c("white", "white")) +
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none",
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14),
        title=element_text(size=18)) +
  coord_cartesian(ylim=c(0,40))  


# Invertebrates
p.invert <- ggplot(data=d.all[d.all$Method%in% c("Free","Compressor"),], aes(x=Year, y=(Lobster.large.weight + Lobster.small.weight + Conch.weight + Crab.weight + Octopus.weight)/Number.of.Fishermen, colour=Method, shape=Method, fill=Method)) +
  stat_summary(fun.data="mean_cl_boot", geom="crossbar", position="dodge", colour="black") +
  geom_point(size=4, position=position_jitterdodge(jitter.width=0.6)) +
  labs(x="Year", y="", title="Lobster, Conch, and Crab") +
  scale_colour_manual(name="Gear Type", values=c("red","blue")) +
  scale_shape_discrete(name="Gear Type") +
  scale_fill_manual(name="Gear Type", values=c("white", "white")) +
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="none",
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14),
        title=element_text(size=18)) +
  coord_cartesian(ylim=c(0,40)) 


# arrange into a five-panel figure
grid.arrange(p.first, p.second, p.third, p.invert, ncol=2)

windows(width=6.5, height=9)



## Stats -----
t.test((First.weight+Second.weight+Third.weight)~Method, data=d.all[d.all$Method%in%c("Compressor","Free"),])

summary(lm((First.weight+Second.weight+Third.weight+Lionfish.weight+Lobster.large.weight+Lobster.small.weight+Octopus.weight+Crab.weight+Conch.weight)/Number.of.Fishermen~Method, data=d.all[d.all$Method%in%c("Compressor","Free")&d.all$Year=="2013",]))
summary(lm((First.weight+Second.weight+Third.weight+Lionfish.weight+Lobster.large.weight+Lobster.small.weight+Octopus.weight+Crab.weight+Conch.weight)/Number.of.Fishermen~Method, data=d.all[d.all$Method%in%c("Compressor","Free")&d.all$Year=="2014",]))
summary(lm((First.weight+Second.weight+Third.weight+Lionfish.weight+Lobster.large.weight+Lobster.small.weight+Octopus.weight+Crab.weight+Conch.weight)/Number.of.Fishermen~Method*Year, data=d.all[d.all$Method%in%c("Compressor","Free"),]))

# lm(formula = (First.weight + Second.weight + Third.weight + Lionfish.weight + 
#                Lobster.large.weight + Lobster.small.weight + Octopus.weight + 
#                Crab.weight + Conch.weight)/Number.of.Fishermen ~ Method * 
#     Year, data = d.all[d.all$Method %in% c("Compressor", "Free"), 
#                        ])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -19.189  -6.676  -1.622   5.676  32.107 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           26.855      1.881  14.280   <2e-16 ***
#   MethodFree            -5.712      2.210  -2.585   0.0105 *  
#   Year2014              -1.176      2.789  -0.421   0.6739    
# MethodFree:Year2014   -3.666      3.186  -1.151   0.2513    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 9.213 on 186 degrees of freedom
# Multiple R-squared:  0.1588,  Adjusted R-squared:  0.1453 
# F-statistic: 11.71 on 3 and 186 DF,  p-value: 4.624e-07


# Fleet Effort -----
p.fleet <- ggplot(data=d.fleet.sum[d.fleet.sum$Method!="cukes",], aes(x=Year, y=Fishers.per.day, colour=Method, shape=Method, fill=Method)) +
  stat_summary(fun.data="mean_cl_boot", geom="crossbar", width=0.6, position=position_dodge(width=.75)) +
  geom_point(size=4, position=position_jitterdodge(dodge.width=.75)) +
  labs(x="Year", y="Fishermen per Day", title="Fleet Effort") +
  scale_colour_manual(name="Gear Type", values=c("red", "blue", "green", "orange", "purple"), labels=c("compressor", "freediving", "gillnet", "trunkfish", "offshore")) +
  scale_shape_discrete(name="Gear Type", labels=c("compressor", "freediving", "gillnet", "trunkfish", "offshore")) +
  scale_fill_manual(name="Gear Type", labels=c("compressor", "freediving", "gillnet", "trunkfish", "offshore"), values=c("white","white","white","white","white")) +
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14),
        title=element_text(size=18)) +
  coord_cartesian(ylim=c(0,21)) 


p.fleet.total <- ggplot(data=d.fleet.sum2, aes(x=Year, y=Total.fishers.per.day)) +
  stat_summary(fun.data="mean_cl_boot", geom="crossbar") +
  geom_point(size=4, position=position_jitter(width=0.3)) +
  labs(x="Year", y="Fishermen per Day", title="Fleet Effort") +
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14),
        title=element_text(size=18)) +
  coord_cartesian(ylim=c(0,41)) 



# Parrotfish analysis -----
d.parrot <- data.frame(d.videos$Year, d.videos$Method, d.videos$X2nd.number.parrot/d.videos$Second.number*100, d.videos$X3rd.number.parrot/d.videos$Third.number*100)
names(d.parrot) <- c("Year", "Method", "Second Class", "Third Class")
d.parrot.long <- melt(d.parrot, na.rm=TRUE, 
                      id.vars=c("Year", "Method"),
                      variable.name="Class",
                      value.name="Percent.Parrot")



p.parrot <- ggplot(data=d.parrot.long[d.parrot.long$Method%in% c("Free","Compressor"),], aes(x=Year, y=Percent.Parrot, colour=Method, shape=Method, fill=Method)) +
  stat_summary(fun.data="mean_cl_boot", geom="crossbar", position="dodge", colour="black") +
  geom_point(size=4, position=position_jitterdodge(dodge.width=0.9, jitter.width=1)) +
  facet_grid(.~Class) +
  labs(x="Year", y="% Parrotfish") +
  scale_colour_manual(name="Gear Type", values=c("red","blue"), labels=c("compressor","freediving")) +
  scale_shape_discrete(name="Gear Type", labels=c("compressor","freediving")) +
  scale_fill_manual(name="Gear Type", values=c("white", "white"), labels=c("compressor","freediving"), guide=FALSE) +
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        title=element_text(size=18),
        strip.text=element_text(size=14),
        strip.background=element_blank(),
        panel.border=element_rect(colour="black")) +
  coord_cartesian(ylim=c(0,105))  



# Size distributions -----
summary.size <- data.frame(matrix(vector(), nrow=120))
summary.size$avg.first.weight.all <- d.videos[,11]/d.videos[,21]
summary.size$avg.second.weight.all <- d.videos[,12]/d.videos[,22]
summary.size$avg.third.weight.all <- d.videos[,13]/d.videos[,23]
summary.size$avg.lobster.weight.all <- (d.videos[,15]+d.videos[,16])/(d.videos[,25]+d.videos[,26])
summary.size$avg.octopus.weight.all <- d.videos[,17]/d.videos[,27]
summary.size$avg.spider.crab.weight.all <- d.videos[,18]/d.videos[,28]
summary.size$avg.conch.weight.all <- d.videos[,19]/d.videos[,29]
summary.size$avg.eel.weight.all <- d.videos[,20]/d.videos[,30]

hist(summary.size$avg.first.weight.all)
mean(summary.size$avg.first.weight.all, na.rm=T)
median(summary.size$avg.first.weight.all, na.rm=T)

hist(summary.size$avg.second.weight.all)
mean(summary.size$avg.second.weight.all, na.rm=T)
median(summary.size$avg.second.weight.all, na.rm=T)

hist(summary.size$avg.third.weight.all)
mean(summary.size$avg.third.weight.all, na.rm=T)
median(summary.size$avg.third.weight.all, na.rm=T)

hist(summary.size$avg.lobster.weight.all)
mean(summary.size$avg.lobster.weight.all, na.rm=T)
median(summary.size$avg.lobster.weight.all, na.rm=T)

hist(summary.size$avg.octopus.weight.all)
mean(summary.size$avg.octopus.weight.all, na.rm=T)
median(summary.size$avg.octopus.weight.all, na.rm=T)

hist(summary.size$avg.spider.crab.weight.all)
mean(summary.size$avg.spider.crab.weight.all, na.rm=T)
median(summary.size$avg.spider.crab.weight.all, na.rm=T)

hist(summary.size$avg.conch.weight.all)
mean(summary.size$avg.conch.weight.all, na.rm=T)
median(summary.size$avg.conch.weight.all, na.rm=T)

hist(summary.size$avg.eel.weight.all)
mean(summary.size$avg.eel.weight.all, na.rm=T)
median(summary.size$avg.eel.weight.all, na.rm=T)
#ddply these stats into a table if I need to use them ever


# ***2014*** Size distributions
summary.size.2014 <- data.frame(matrix(vector(), nrow=71))
summary.size.2014[,1:10] <- d.videos[d.videos$Year=="2014",1:10]
summary.size.2014$avg.first.weight.2014 <- d.videos[d.videos$Year=="2014",11]/d.videos[d.videos$Year=="2014",21]
summary.size.2014$avg.second.weight.2014 <- d.videos[d.videos$Year=="2014",12]/d.videos[d.videos$Year=="2014",22]
summary.size.2014$avg.third.weight.2014 <- d.videos[d.videos$Year=="2014",13]/d.videos[d.videos$Year=="2014",23]
summary.size.2014$avg.lobster.weight.2014 <- (d.videos[d.videos$Year=="2014",15]+d.videos[d.videos$Year=="2014",16])/(d.videos[d.videos$Year=="2014",25]+d.videos[d.videos$Year=="2014",26])
summary.size.2014$avg.octopus.weight.2014 <- d.videos[d.videos$Year=="2014",17]/d.videos[d.videos$Year=="2014",27]
summary.size.2014$avg.spider.crab.weight.2014 <- d.videos[d.videos$Year=="2014",18]/d.videos[d.videos$Year=="2014",28]
summary.size.2014$avg.conch.weight.2014 <- d.videos[d.videos$Year=="2014",19]/d.videos[d.videos$Year=="2014",29]
summary.size.2014$avg.eel.weight.2014 <- d.videos[d.videos$Year=="2014",20]/d.videos[d.videos$Year=="2014",30]


hist(summary.size.2014$avg.first.weight.2014)
mean(summary.size.2014$avg.first.weight.2014, na.rm=T)
median(summary.size.2014$avg.first.weight.2014, na.rm=T)
quantile(summary.size.2014$avg.first.weight.2014, probs=c(0.25, 0.75), na.rm=T)

hist(summary.size.2014$avg.second.weight.2014)
mean(summary.size.2014$avg.second.weight.2014, na.rm=T)
median(summary.size.2014$avg.second.weight.2014, na.rm=T)
quantile(summary.size.2014$avg.second.weight.2014, probs=c(0.25, 0.75), na.rm=T)

hist(summary.size.2014$avg.third.weight.2014)
mean(summary.size.2014$avg.third.weight.2014, na.rm=T)
median(summary.size.2014$avg.third.weight.2014, na.rm=T)
quantile(summary.size.2014$avg.third.weight.2014, probs=c(0.25, 0.75), na.rm=T)

hist(summary.size.2014$avg.lobster.weight.2014)
mean(summary.size.2014$avg.lobster.weight.2014, na.rm=T)
median(summary.size.2014$avg.lobster.weight.2014, na.rm=T)
quantile(summary.size.2014$avg.lobster.weight.2014, probs=c(0.25, 0.75), na.rm=T)

hist(summary.size.2014$avg.octopus.weight.2014)
mean(summary.size.2014$avg.octopus.weight.2014, na.rm=T)
median(summary.size.2014$avg.octopus.weight.2014, na.rm=T)
quantile(summary.size.2014$avg.octopus.weight.2014, probs=c(0.25, 0.75), na.rm=T)

hist(summary.size.2014$avg.spider.crab.weight.2014)
mean(summary.size.2014$avg.spider.crab.weight.2014, na.rm=T)
median(summary.size.2014$avg.spider.crab.weight.2014, na.rm=T)
quantile(summary.size.2014$avg.spider.crab.weight.2014, probs=c(0.25, 0.75), na.rm=T)

hist(summary.size.2014$avg.conch.weight.2014)
mean(summary.size.2014$avg.conch.weight.2014, na.rm=T)
median(summary.size.2014$avg.conch.weight.2014, na.rm=T)
quantile(summary.size.2014$avg.conch.weight.2014, probs=c(0.25, 0.75), na.rm=T)

hist(summary.size.2014$avg.eel.weight.2014)
mean(summary.size.2014$avg.eel.weight.2014, na.rm=T)
median(summary.size.2014$avg.eel.weight.2014, na.rm=T)
quantile(summary.size.2014$avg.eel.weight.2014, probs=c(0.25, 0.75), na.rm=T)
#ddply these stats into a table if I need to use them ever
# Need to first melt them




# Taxonomic and Class plots of catch (to accompany foraging paper) -----

d.all.numbers.taxon <- d.all[,c(1:10)]
d.all.numbers.taxon$parrotfish <- d.all[,34]+d.all[,40]
d.all.numbers.taxon$snapper <- d.all[,31]+d.all[,35]+d.all[,41]
d.all.numbers.taxon$grouper <- d.all[,32]+d.all[,36]+d.all[,42]
d.all.numbers.taxon$grunt <- d.all[,38]+d.all[,43]
d.all.numbers.taxon$lionfish <- d.all[,37]+d.all[,24]
d.all.numbers.taxon$squirrelfish <- d.all[,44]
d.all.numbers.taxon$invertebrates <- d.all[,25]+d.all[,26]+d.all[,27]+d.all[,28]+d.all[,29]
d.all.numbers.taxon$other <- d.all[,33]+d.all[,39]+d.all[,45]+d.all[,47]

d.all.numbers.taxon.long <- melt(d.all.numbers.taxon[!is.na(d.all$First.number),c(1:10,11:18)], 
                                id.vars = names(d.all.numbers.taxon)[1:10], 
                                variable.name = "Taxon",
                                value.name = "Number" )

harvest.taxon.plot <- ggplot(d.all.numbers.taxon.long[d.all.numbers.taxon.long$Method%in%c("Free","Compressor"),], aes(y=Number, x=Taxon, colour=Method, fill=Method)) +
  stat_summary(fun.data="mean_cl_boot", geom="crossbar", width=0.3, position=position_dodge(width=0.3), colour="black") +
  geom_point(size=3, position=position_dodge(width=0.3)) +
  labs(x="Taxon", y="Number caught per Day") +
  scale_colour_manual(name="Method", values=c("red","blue"), labels=c("compressor","freediving")) +
  scale_fill_manual(name="Method", values=c("white", "white"), labels=c("compressor","freediving"), guide=FALSE) +
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(size=14, colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position=c(0.85,0.7),           #site legend for panel figure
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        strip.background=element_blank(),
        strip.text=element_text(size=18),
        title=element_text(size=18))


d.all.numbers.class <- d.all[,c(1:10,21:30)]
d.all.numbers.class$Lobster.number <- d.all.numbers.class[,15] + d.all.numbers.class[,16] 
d.all.numbers.class$first.or.second <- NA
d.all.numbers.class$second.or.third <- NA

names(d.all.numbers.class)[11:23] <- c("1","2","3","lionfish","LL","LS","octopus","crab","conch","eel","lobster","1or2","2or3")
d.all.numbers.class <- d.all.numbers.class[,c(1:11,22,12,23,13,21,17,19,18,20)]

d.all.numbers.class.long <- melt(d.all.numbers.class[!is.na(d.all$First.number),c(1:10,11:20)], 
                                 id.vars = names(d.all.numbers.class)[1:10], 
                                 variable.name = "Class",
                                 value.name = "Number" )

harvest.class.plot <- ggplot(d.all.numbers.class.long[d.all.numbers.class.long$Method%in%c("Free","Compressor"),], aes(y=Number, x=Class, colour=Method, fill=Method)) +
  stat_summary(fun.data="mean_cl_boot", geom="crossbar", width=0.3, position=position_dodge(width=0.3), colour="black") +
  geom_point(size=3, position=position_dodge(width=0.3)) +
  labs(x="Class", y="Number caught per Day") +
  scale_colour_manual(name="Method", values=c("red","blue"), labels=c("compressor","freediving")) +
  scale_fill_manual(name="Method", values=c("white", "white"), labels=c("compressor","freediving"), guide=FALSE) +
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(size=14, colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position=c(0.85,0.7),           #site legend for panel figure
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        strip.background=element_blank(),
        strip.text=element_text(size=18),
        title=element_text(size=18))


multiplot(harvest.class.plot, harvest.taxon.plot, cols=1)



# Fishermen average catches (to accompany foraging paper) -----
ddply(d.all, "Fisherman", summarise,
      total.number=mean(First.number+Second.number+Third.number+Lionfish.number+Lobster.large.number+Lobster.small.number+Octopus.number+Crab.number+Conch.number+Eel.number),
      total.weight=mean(First.weight+Second.weight+Third.weight+Lionfish.weight+Lobster.large.weight+Lobster.small.weight+Octopus.weight+Crab.weight+Conch.weight+Eel.weight),
      n=length(Fisherman))


ari 208.75/2 (1) 49.13 (3) = 75.22(4)
chib 44.87 (3) 80.4/2 (1) 119.83/2 (1) 102.75/2 (1) 145.5/2 (1) 135/2 (1) = 75.5(8)
mell 60.35 (1) 96.75/2 (1) 77.13/2 (2) 67.2 (1) 91.63/2 (1) = 49.81(6)
nancomp 91.63/2 (2) = 45.32(2)
nan 25.23 (10) 
neo 14.5 (1) 65.5 (1) 63.75/2 (1) 64/2 (1) 60 (1) = 40.78(5)
cua 23.57 (14)
par 18.93 (10)
wan 13.88 (4)
wil 17.5 (2)

fisherman.daily.averages <- data.frame(
  fisherman=c("Ari","ChiB","Mell","Neo","NanComp","Nan","Cua","Par","Wan","Wil"),
  mean.total.weight=c(75.22, 75.5, 49.81, 40.78, 45.32, 25.23, 23.57, 18.93, 13.88, 17.5),
  n=c(4, 8, 6, 5, 2, 10, 14, 10, 4, 2))





