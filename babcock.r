library(tidyverse)
library(ggrepel)

babcock <- read_csv("data/combined-clean.csv")

library(showtext)
showtext_auto()

library(myriad)
import_myriad()
import_myriad_semi()

theme_set(theme_myriad_semi())



## This function is morally wrong but I'm in a rush

short_names <- function(x){
    x <- stringr::str_trim(x)
    x <- stringr::str_replace(x, "New York University", "NYU")
    x <- stringr::str_replace(x, "^University of ", "")
    x <- stringr::str_replace(x, "Polytechnic Institute", "PI")
    x <- stringr::str_replace(x, "the Pacific", "U. of the Pacific")
    x <- stringr::str_replace(x, "Massachusetts Institute of Technology", "MIT")
    x <- stringr::str_replace(x, "Rutgers New Jersey at New Brunswick", "Rutgers")
    x <- stringr::str_replace(x, "North Carolina at", "UNC")
    x <- stringr::str_replace(x, "University at College Station", "")
    x <- stringr::str_replace(x, "Washington University in St. Louis St.", "WUSTL")
    x <- stringr::str_replace(x, " University$", "")
    x <- stringr::str_replace(x, "Institute of Technology", "Tech")
    x <- stringr::str_replace(x, "The Catholic University of America", "Catholic U. of Am.")
    x <- stringr::str_replace(x, "^Pennsylvania$", "U Penn")
    x <- stringr::str_replace(x, "Indiana University at Bloomington", "Indiana")
    x <- stringr::str_replace(x, "at Ann Arbor", "")
    x <- stringr::str_replace(x, "Illinois at Urbana-Champaign", "UIUC")
    x <- stringr::str_replace(x, "Wisconsin at Madison", "Wisconsin")
    x <- stringr::str_replace(x, "at Twin Cities", "")
    x <- stringr::str_replace(x, "University", "U.")
    x <- stringr::str_replace(x, " School of", "")
    x <- stringr::str_replace(x, "California Tech", "Caltech")
    x <- stringr::str_replace(x, "California at Los Angeles", "UCLA")
    x <- stringr::str_replace(x, "California at Santa Barbara", "UCSB")
    x <- stringr::str_replace(x, "California at Davis", "UC Davis")
    x <- stringr::str_replace(x, "California at San Diego", "UCSD")
    x <- stringr::str_replace(x, "Southern California", "USC")
    x <- stringr::str_replace(x, "California at Irvine", "UC Irvine")
    x <- stringr::str_replace(x, "LowellLowell", "Lowell")
    x <- stringr::str_replace(x, "Las Cruces", "")
    x <- stringr::str_replace(x, "U. Bowling", "U.")
    x <- stringr::str_replace(x, "Maryland at Baltimore County", "UMBC")
    x <- stringr::str_replace(x, "North Carolina State U. at Raleigh", "NCSU")
    x <- stringr::str_replace(x, "College of Environmental Science and Forestry", "CESF")
    x <- stringr::str_replace(x, "U. at Baton Rouge Baton", "")
    x <- stringr::str_replace(x, "Southern Illinois U. at Carbondale", "SIU Carbondale")
    x <- stringr::str_replace(x, "at Provo", "")
    x <- stringr::str_replace(x, "Missouri U. of Science & Technology", "Missouri Sci & Tech")
    x
}


data_unis <- subset(babcock, Babcock!="None" & Type=="University")
data_unis$sname <- short_names(data_unis$School)

data_unis <- within(data_unis, {
    School <- reorder(School, -Rank)
})


data_unis$Dummy <- 1

data_unis$usnwr_grp <- cut_number(data_unis$Rank, 4)

levels(data_unis$usnwr_grp) <- c("USNWR 1-28" , "USNWR 29-62", "USNWR 63-101", "USNWR 102-200")

babcolors <- c("#FDE540", "#66C668", "#2E908B", "#3B5489")

p <- ggplot(data_unis, aes(x=Dummy, y=reorder(sname, -Rank), fill = Babcock, label = sname))

p1 <- p + geom_tile() + facet_wrap( ~ usnwr_grp, nrow = 1, scales = "free_y") +
    geom_label(fill = "#FFFFFF", alpha = 0.7, size = rel(2.2)) +
    guides(fill=guide_legend(title="Babcock Class in 1911 (Schools founded after 1911 not shown)", title.position = "top")) +
    labs(x = NULL, y = NULL,
         title = "The Persistence of the Old Regime",
         subtitle = "1911 Babcock Classification and 2014 US News Rankings",
         caption = "Kieran Healy. http://kieranhealy.org") +
#    scale_fill_viridis_d(option = "D", direction = -1) +
    scale_fill_manual(values = babcolors) +
    theme(strip.text.x = element_text(size = rel(0.9), face = "bold"),
          axis.ticks=element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_text(size = rel(0.9)),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "top",
          legend.justification = "left")



pdf(file="figures/babcock-universities-tile.pdf", height=9, width=6)
p1
dev.off()

ggsave(
    "figures/babcock-universities-tile.png",
  p1,
  width = 6,
  height = 9,
  dpi = 300
)


### ALl of the Unis

data_allu <- subset(babcock, Type=="University")
data_allu$Babcock <- stringr::str_replace(data_allu$Babcock, "None", "Not Rated/Not Yet Founded")
#data_allu <- droplevels(data)

data_allu <- within(data_allu, {
    School <- reorder(School, -Rank)
})

data_allu$Dummy <- 1
data_allu$sname <- short_names(data_allu$School)
data_allu$usnwr_grp <- cut_number(data_allu$Rank, 4)

levels(data_allu$usnwr_grp) <- c("USNWR 1-52" , "USNWR 53-101", "USNWR 101-152", "USNWR 153-200")

p <- ggplot(data_allu, aes(x=Dummy, y=reorder(sname, -Rank), fill = Babcock, label = sname))

p1 <- p + geom_tile() + facet_wrap( ~ usnwr_grp, nrow = 1, scales = "free_y") +
    geom_label(fill = "#FFFFFF", alpha = 0.9, size = rel(1.8)) +
    guides(fill=guide_legend(title="Babcock Class in 1911", title.position = "top")) +
    labs(x = NULL, y = NULL,
         title = "The Persistence of the Old Regime",
         subtitle = "1911 Babcock Classification and 2014 US News Rankings",
         caption = "Kieran Healy. http://kieranhealy.org") +
    scale_fill_viridis_d(option = "D", direction = -1) +
#    scale_fill_manual(values = babcolors) +
    theme(strip.text.x = element_text(size = rel(0.8), face = "bold"),
          axis.ticks=element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_text(size = rel(0.9)),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "top",
          legend.justification = "left")

pdf(file="figures/babcock-universities-tile-all.pdf", height=11, width=6)
p1
dev.off()

ggsave(
    "figures/babcock-universities-tile-all.png",
  p1,
  width = 6,
  height = 11,
  dpi = 300
)
