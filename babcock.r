library(dplyr)
library(ggplot2)
library(kjhutils)

data <- read.csv("data/combined-clean.csv", header=TRUE)


#data %>% group_by(Babcock) %>%


#data %>% group_by(Babcock, PubPriv, Rank)

## out <- ggplot(data, aes(y=Rank, x=PubPriv))


## out + geom_histogram() + facet_wrap(~Babcock)


data.unis <- subset(data, Babcock!="None" & Type=="University")
#data.unis <- droplevels(data)

data.unis <- within(data.unis, {
    School <- reorder(School, -Rank)
})


data.unis$Dummy <- 1

p <- ggplot(data.unis, aes(x=Dummy, y=School, color=Babcock))

p1 <- p + geom_point(size=2.8) + facet_grid( ~ PubPriv) + xlab("") + xlim(1,1) + guides(color=guide_legend(title="Babcock\nClassification\nin 1911")) + ylab("US News & World Report Ranking in 2014") + theme_bw() + theme(axis.text.y = element_text(size=rel(0.5)), axis.ticks=element_blank(), axis.text.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())


pdf(file="figures/babcock-universities.pdf", height=12, width=5)
p1
credit("\nKieran Healy, http://kieranhealy.org")
dev.off()

ggsave(
  "figures/babcock-universities.png",
  width = 4.5,
  height = 12,
  dpi = 300
)



## p <- ggplot(data.unis, aes(x=Rank, y=School, color=Babcock))

## p1 <- p + geom_point(size=2.5) + facet_grid( ~ PubPriv) + xlab("US News & World Report National University Rank in 2014\n") + xlim(1,max(data.unis$Rank)+5) + guides(color=guide_legend(title="Babcock Classification in 1911")) + ylab("") + theme_bw() + theme(legend.position="top", axis.text.y = element_text(size=rel(0.5)))


## pdf(file="figures/babcock-universities.pdf", height=12, width=7)
## p1
## credit("\nKieran Healy, http://kieranhealy.org")
## dev.off()

## ggsave(
##   "figures/babcock-universities.png",
##   width = 7,
##   height = 12,
##   dpi = 300
## )




data.coll <- subset(data, Babcock!="None" & Type=="College")
data.coll$School <- gsub(" College", "", data.coll$School)

data.coll <- within(data.coll, {
    School <- reorder(School, -Rank)
})

data.coll$Dummy <- 1

p <- ggplot(data.coll, aes(x=Dummy, y=School, color=Babcock))


p1 <- p + geom_point(size=2.8) + xlab("") + xlim(1,1) + guides(color=guide_legend(title="Babcock\nClassification\nin 1911")) + ylab("US News & World Report Ranking in 2014") + theme_bw() + theme(axis.text.y = element_text(size=rel(0.5)), axis.ticks=element_blank(), axis.text.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())


## p1 <- p + geom_point(size=2.5) + guides(color=guide_legend(title="Babcock Classification in 1911")) + xlab("US News & World Report Liberal Arts Colleges Rank in 2014\n") + ylab("") + theme_bw() + theme(legend.position="top", axis.text.y = element_text(size=rel(0.7)))

pdf(file="figures/babcock-colleges.pdf", height=12, width=3.5, pointsize = 11)
p1
credit("\nKieran Healy, http://kieranhealy.org")

dev.off()

ggsave(
  "figures/babcock-colleges.png",
  p1,
  width = 3.5,
  height = 12,
  dpi = 300
)




### ALl of the Unis

data.allu <- subset(data, Type=="University")
#data.allu <- droplevels(data)

data.allu <- within(data.allu, {
    School <- reorder(School, -Rank)
})

data.allu$Dummy <- 1

p <- ggplot(data.allu, aes(x=Dummy, y=School, color=Babcock))


p1 <- p + geom_point(size=2.8) + facet_grid( ~ PubPriv) + xlab("") + xlim(1,1) + guides(color=guide_legend(title="Babcock\nClassification\nin 1911")) + ylab("US News & World Report Ranking in 2014") + theme_bw() + theme(axis.text.y = element_text(size=rel(0.5)), axis.ticks=element_blank(), axis.text.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())



## p1 <- p + geom_point(size=2.5) + facet_grid( ~ PubPriv) + xlab("US News & World Report National University Rank in 2014\n") + xlim(1,max(data.allu$Rank)+5) + guides(color=guide_legend(title="Babcock 1911")) + ylab("") + theme_bw() + theme(legend.position="top", axis.text.y = element_text(size=rel(0.5)))


pdf(file="figures/babcock-all-universities.pdf", height=18, width=5)
p1
credit("\nKieran Healy, http://kieranhealy.org")
dev.off()

ggsave(
  "figures/babcock-all-universities.png",
  width = 4.75,
  height = 18,
  dpi = 300
)
