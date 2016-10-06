# ===========================================================================================
# Troubleshooting & Prelimnary Informaton
# ===========================================================================================

# 1.) Set working directory to the file where the csvs are located.
# 2.) Ensure all packages have been installed.
# 3.) Ensure csvs have the correct column headers: "name, dem, rep, sum". Note, "sum"
#     is not necessary for projections and vote goals.
# 4.) Ensure the data is in its rawest form with no TOTAL or OTHER rows. For counties,
#     this means having all the counties. Not just the top 15.

# ===========================================================================================
# Loading Data & Preliminary Cleaning
# ===========================================================================================

# load packages
library(ggplot2) # needed for ggplot2
library(stringr) # needed for str_split and str_sub
library(gridExtra) # needed to rearrange plots
library(grid) # needed for unit()
library(cowplot) # needed for switch_axis_position()
library(reshape) # needed for melt()
library(scales) # needed for labels = comma/percent

# load custom variables
filename <- 'in_county_gov_2012.csv' # [state]_[split_type]_[elec_type]_[year].csv
width <- 10 # number of counties shown

# variables relevant if NOT vote goals or projections
dem_name <- 'Gregg' # democratic candidate name
rep_name <- 'Pence' # republican candidate name
analysis <- '3' # 2-way or 3-way

# download the data
df <- read.csv(filename)

# format columns
df$name <- gsub('(.{1,12})(\\s|$)', '\\1\n', gsub('-', ' ', gsub(',.[A-Z][A-Z]', '', toupper(as.character(df$name)))))
df$dem <- as.numeric(df$dem)
df$rep <- as.numeric(df$rep)
df$sum <- as.numeric(df$sum)

# ===========================================================================================
# Build Important Vectors
# ===========================================================================================

# build vectors using title data
splitname <- unlist(str_split(filename, '_'))
state <- toupper(splitname[1])
if(splitname[2] == 'county') {
  split_type <- 'County'
} else if (splitname[2] == 'dma') {
  split_type <- 'DMA'
}
if(splitname[3] == 'gov') {
  elec_type <- 'Gubernatorial'
} else if (splitname[3] == 'pres') {
  elec_type <- 'Presidential'
} else if (splitname[3] == 'sen') {
  elec_type <- 'Senate'
}
year <- str_sub(splitname[4], 0, 4)
data_type <- str_sub(splitname[4], 5, 6)

# build vectors using set data
competition <- paste0(' ', '(', dem_name, ' v. ', rep_name, ')')
data_type <- toupper(data_type)
if (year == str_sub(Sys.Date(), 0, 4) & data_type == 'PJ')  {
  data_type_2 <- 'Projections'
  competition <- NULL
  analysis <- 2
  df$sum <- (df$rep + df$dem)
} else if (year == str_sub(Sys.Date(), 0, 4) & data_type == 'VG') {
  data_type_2 <- 'Vote Goals'
  competition <- NULL
  analysis <- 2
  df$sum <- (df$rep + df$dem)
} else {
  data_type_2 <- 'Results'
  data_type <- NULL
}
if(analysis == '2') {
  df$pdem <- df$dem/(df$dem + df$rep)
  analysis_level <- 'Two-way'
} else if(analysis == '3') {
  df$pdem <- df$dem/df$sum
  analysis_level <- 'Three-way'
} else {
  stop('Input invalid.')
}

# build title
title <- paste0(year, ' ', elec_type,  ' ', data_type_2, ' ', 'by', ' ', split_type, competition)

# ===========================================================================================
# Build Axes
# ===========================================================================================

# pull maximum value & number of digits
comb_max  <- max(max(df$dem), max(df$rep))
n_digits <- nchar(as.character(comb_max)) - 1

# build axes increments
y_axis_false_top <- round(comb_max, -n_digits)
y_axis_step <- y_axis_false_top/4
y_axis_minor_step <- y_axis_step/4

# sequence the two axes, then filter the minor to be mutually exclusive
y_axis <- seq(0, y_axis_false_top + y_axis_step, y_axis_step)
y_axis_minor <- seq(0, max(y_axis), y_axis_minor_step)
y_axis_minor <- y_axis_minor[!(y_axis_minor %in% y_axis)]

# ===========================================================================================
# Prepare the Data Frames
# ===========================================================================================

# vote graph dataframe
if (split_type == 'County') {
  ref_basic <- df[order(df$sum, decreasing = TRUE)[1:width], 1:3]
} else if (split_type == 'DMA') {
  ref_basic <- df[order(df$sum, decreasing = TRUE), 1:3]
}
ref_basic$name <- factor(ref_basic$name, as.character(ref_basic$name))
vote_basic <- melt(ref_basic, id.vars = "name")

# democratic support graph dataframe
if (split_type == 'County') {
  support_basic <- df[order(df$sum, decreasing = TRUE)[1:width], c(1, 4, 5)]
} else if (split_type == 'DMA') {
  support_basic <- df[order(df$sum, decreasing = TRUE), c(1, 4, 5)]
}
support_basic$name <- factor(support_basic$name, as.character(support_basic$name))

# 'other' value-graph dataframes
if (split_type == 'County') {
  ref_other <- df[(width + 1):length(df$sum), ]
  
  vote_other <- data.frame('name' = 'OTHER', 'dem' = sum(ref_other$dem),'rep' = sum(ref_other$rep), 'sum' = sum(ref_other$sum))
  if(analysis == '2W') {
    vote_other$pdem <- vote_other$dem/(vote_other$dem + vote_other$rep)
  } else if (analysis == '3W') {
    vote_other$pdem <- vote_other$dem/vote_other$sum
  }
  vote_other <- melt(vote_other[1:3], id.vars = "name")
  
  support_other <- data.frame('name' = 'OTHER', 'pdem' = sum(ref_other$dem)/sum(ref_other$sum))
  
  # build the second y axis used in p2
  comb_max_2 <- max(vote_other$value)
  n_digits_2 <- nchar(as.character(max(vote_other$value))) -1
  y_axis_2_false_top <- round(comb_max_2, -n_digits_2)
  y_axis_2_step <- y_axis_2_false_top/4
  y_axis_2 <- seq(0, y_axis_2_false_top + y_axis_2_step, y_axis_2_step)
} else if (split_type == 'DMA') {
  NULL
}

# ===========================================================================================
# Plot Important Graphs
# ===========================================================================================

# plot the vote graph
p1 <- ggplot(vote_basic, aes(name, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity", width = 0.75) +
  scale_fill_manual(values = c('#4F81BD', '#C0504D')) +
  labs(x = "", y = "Votes\n") + 
  scale_y_continuous(labels = comma, limit = c(0, max(y_axis)), minor_breaks = y_axis_minor, breaks = y_axis, expand = c(0,0)) +
  theme(plot.title = element_text(face="bold"),
        plot.margin = unit(c(.5,0,0,.8), 'cm'),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour="black", size=.1),
        panel.grid.minor = element_line(colour="black", size=.05),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(face="bold"),
        axis.ticks = element_line(),
        axis.text.x = element_text(color = 'black', size = 9, angle = 45, vjust = 0.5),
        axis.text.y = element_text(color = 'black', size = 12))
if(split_type == 'DMA') {p1 <- p1 + theme(axis.text.x = element_text(angle = 0))}

# plot the 'other' value vote graph
p2 <- ggplot(vote_other, aes(name, value)) + 
  geom_bar(aes(fill = variable), position = "dodge", stat = "identity", width = .75) +
  scale_fill_manual(values = c('#4F81BD', '#C0504D')) +
  scale_y_continuous(labels = comma, expand = c(0,0), breaks = y_axis_2, limit = c(0, max(y_axis_2))) +
  theme(legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 0),
        axis.ticks = element_line(),
        aspect.ratio = 10.7,
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(color = 'black', size = 10, angle = 45, vjust = 0))
p2 <- ggdraw(switch_axis_position(p2, axis = "y"))

# plot the democratic support graph
p3 <- ggplot(support_basic, aes(name, pdem, fill = as.factor(ref_basic$dem < ref_basic$rep))) + 
  geom_bar(stat = "identity", width = .9) +
  geom_text(aes(y = 0.01, label = (paste(signif((pdem*100), 3), '%', sep = "")), vjust = -.6, fontface = "bold"), colour = '#FFFFFF') +
  scale_fill_manual(values = c('FALSE' = '#4F81BD', 'TRUE' = '#C0504D')) +
  labs(title = "", x = "", y = paste(analysis_level, '\n', "Democratic Support\n")) +
  scale_y_continuous(labels = percent, limits = c(0, 1), expand = c(0,0)) +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,0,.3), 'cm'),
        panel.background = element_rect(fill = "white"),
        axis.title.y = element_text(face="bold"),
        axis.ticks = element_line(), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = 'black', size = 12),
        panel.grid.major = element_line(colour="black", size=.15),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# plot the 'other' value democratic support graph
p4 <- ggplot(support_other, aes(name, pdem, fill = as.factor(pdem < 0.5))) +
  geom_bar(stat = "identity", width = .8) +
  scale_fill_manual(values = c('FALSE' = '#4F81BD', 'TRUE' = '#C0504D')) +
  geom_text(aes(y = 0.01, label = (paste(signif((pdem*100), 3), '%', sep = "")), vjust = -.6, fontface = "bold"), colour = '#FFFFFF') +
  scale_y_continuous(labels = percent, limits = c(0, 1), expand = c(0,0)) +
  theme(legend.position = "none",
        aspect.ratio = 2.4,
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 0),
        axis.ticks = element_line(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = 'black', size = 12))
p4 <- ggdraw(switch_axis_position(p4, axis = "y"))

# save graph files
dir.create('images')
if (split_type == 'County'){
  ggsave(filename = paste0('images', '/', state, "_", split_type, "_", elec_type, "_", year, data_type, "_", analysis, "WY", ".png"), 
         plot = grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2, widths = c(14, 2.2), heights = c(5, 2), top = textGrob(paste0(title, '\n'), gp = gpar(fontsize = 26, font = 2))),
         width = 10.67, height = 8, dpi = 300)
} else if (split_type == 'DMA') {
  ggsave(filename = paste0('images', '/', state, "_", split_type, "_", elec_type, "_", year, data_type, "_", analysis, "WY", ".png"), 
         plot = grid.arrange(p1, p3, ncol = 1, nrow = 2, heights = c(5, 2), top = textGrob(paste0(title, '\n'), gp = gpar(fontsize = 24, font = 2))),
         width = 10.67, height = 8, dpi = 300)
}

# ===========================================================================================
# Adjusting the Graphs
# ===========================================================================================

# 1.) If the other bar is shrunk, try plaing with the aspect.ratio variable for that specific
#     graph. That will allow you to change the height. This should only really be a problem 
#     for the 'Other' graphs.
# 2.) If one of the bars isn't showing up, the y axis might be too small. In that case, feel
#     free to add the increment to the end of the y axis variable as a quick work-around. 