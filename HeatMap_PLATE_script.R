# Heatmap FACET GRID

# ------------------ TRACKING ORGANOIDS AREA IN A WELL -------------------------

# TO DO:
#1 - change basePath to the directory where your excel sheet is.
#2 - change the name of the well
#3 - write the name of the excel title file


# INPUT
plateName <- "Name of the plate"
basePath <- "/path/to/your/directory"
organoIDDataFile_track <- "NameOfExcelFiles.xlsx"

#----------------------------OrganoID settings----------------------------------
#Threshold 0.2
#Batch Size 16
#Minimum Area 35
#Edge Sigma 2.00
#Edge Minimum 0.01
#Edge Maximum 0.05
#------------------------------load packages------------------------------------
library(ggpubr)
library(ggplot2)
library(qpdf)
library(scales)
library(dplyr)
library(ggExtra)
library(patchwork)
library(gridExtra)
library(viridisLite)
library(viridis)
library(openxlsx)
setwd(basePath)


#--------------------------Data import from EXCEL------------------------------



# Creation of data frame from excel containing area of all organoid of all well
# tracked organoid have the same ID (are on the same line)

data <- read.xlsx(organoIDDataFile_track)
allAreaTrack <- data[,-1] #extract without ID


# Create an empty list to store the dataframes

areaTrack_list <- list()


# Loop over columns in steps of 2 to separate all well (day1 and day3 together)
#change here if you have more time points.
# output: areaTrack_list containing a list of all datframe of day1 and day by well

for(i in seq(1, ncol(allAreaTrack), by = 2)) {
  if(i == ncol(allAreaTrack)) {
    areaTrack_list[[length(areaTrack_list) + 1]] <- allAreaTrack[, i, drop = FALSE]
  } else {
    areaTrack_list[[length(areaTrack_list) + 1]] <- allAreaTrack[, i:(i+1)]
  }
}


# ADD Plate position for all dataframe
# Generate the plate positions
plateX <- 1:24
plateY <- LETTERS[1:16]

# Create the combinations of X and Y positions for 384 wells (ex:A03,D22,...)
positions <- expand.grid(X = plateX, Y = plateY)



#------------------------Selection and calculation------------------------------


# for all well: change column name, only keep tracked organoid (line without NA) 
# calculate the area diff and add the heat map positions.

for (well in seq_along(areaTrack_list)) {
  #changing column name
  colname<-paste0("day", c(1,3))
  colnames(areaTrack_list[[well]]) <- colname
  #adding heatmap position
  areaTrack_list[[well]]$plateX <- positions$X[well]
  areaTrack_list[[well]]$plateY <- positions$Y[well]
  #remove organoid that doesnt have tracking (have NA)
  areaTrack_list[[well]] <- areaTrack_list[[well]][complete.cases(areaTrack_list[[well]]), ]
  if (nrow(areaTrack_list[[well]]) == 0){
    zero_row <- data.frame(matrix(rep(0, ncol(areaTrack_list[[well]])), nrow = 1))
    colnames(zero_row) <- colnames(areaTrack_list[[well]])
    areaTrack_list[[well]]<- rbind(areaTrack_list[[well]], zero_row)
    areaTrack_list[[well]]$plateX <- positions$X[well]
    areaTrack_list[[well]]$plateY <- positions$Y[well]
  }
  areaTrack_list[[well]]$day1<- as.numeric(areaTrack_list[[well]]$day1)
  areaTrack_list[[well]]$day3 <- as.numeric(areaTrack_list[[well]]$day3)
  #calculate the diff
  areaTrack_list[[well]]$diff <-areaTrack_list[[well]]$day3 - areaTrack_list[[well]]$day1
}



#------------------------------PLOT--------------------------------------------


# grid creation for the heat map.
# output: every well's dataframe in the list areaTrack will have x and y column
          # to allowed to plot heatmap

for (well in seq_along(areaTrack_list)) {
  ngrd <- sqrt(nrow(areaTrack_list[[well]]))
  
  # solve problem if root of nrow is not a plain numbre
  if (ngrd %% 1 != 0) {
    ngrd <- ceiling(ngrd) #round up the numbre
    #nbr of row to add
    toAdd <- (ngrd ^ 2) - nrow(areaTrack_list[[well]])
    dfToAdd <- data.frame(matrix(NA,    # Create empty data frame
                                 nrow = toAdd,
                                 ncol = 5))
    colnames(dfToAdd) <- c(colname, "plateX", "plateY", "diff")
    areaTrack_list[[well]] <- rbind(areaTrack_list[[well]], dfToAdd)
  }
  
  
  # create grid
  x <- rep(1:ngrd, ngrd)
  y <- rep(1:ngrd, each = ngrd)
  areaTrack_list[[well]]$x <- x
  areaTrack_list[[well]]$y <- y
}


# Function to get the well plate position of any well's dataframe in areaTrack list
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# replace NA to proper well name
for (well in seq_along(areaTrack_list)) {
  areaTrack_list[[well]]$plateX[is.na(areaTrack_list[[well]]$plateX)] <-
    get_mode(areaTrack_list[[well]]$plateX)
  
  areaTrack_list[[well]]$plateY[is.na(areaTrack_list[[well]]$plateY)] <-
    get_mode(areaTrack_list[[well]]$plateY)
}


# combined all well's dataframes together to allowed big plot (facet grid)

combined <- do.call(rbind, areaTrack_list)

combined$diff <- scale(combined$diff) #scale the diff


# add a max/min cut off
maxcut <- 1
mincut <- -1
combined$diff[combined$diff > maxcut] <- maxcut
combined$diff[combined$diff < mincut] <- mincut

# Plot
plot <- ggplot(combined, aes(x, y, fill = diff)) +
  geom_tile() +
  facet_grid(plateY ~ plateX, scales = "free") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(),
    strip.background = element_rect(fill = "gray20"),
    strip.text = element_text(colour = "white")
  ) +
  scale_fill_viridis(na.value = "white", limits = c(mincut, maxcut))

ggsave(plot, filename = paste0(plateName,"_tracking_ScaleGeneral.pdf"), width = 24, height = 18)



#------------------------- Mean area Plot --------------------------------------

#calculate mean of diff of same well
combined <- combined %>%
  group_by(plateY, plateX) %>%
  mutate(mean_diff = mean(diff, na.rm = TRUE)) %>%
  ungroup()

plot2 <- ggplot(combined, aes(1, 1, fill = mean_diff)) +
  geom_tile() +
  facet_grid(plateY ~ plateX, scales = "free") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(),
    strip.background = element_rect(fill = "gray20"),
    strip.text = element_text(colour = "white")
  ) +
  scale_fill_viridis(na.value = "white", limits = c(mincut, maxcut))


ggsave(plot2, filename = paste0(plateName,"_meanArea.pdf"), width = 24, height = 18)

#Merge everything in one PDF

qpdf::pdf_combine(input = c(paste0(plateName,"_meanArea.pdf"),
                            paste0(plateName,"_tracking_ScaleGeneral.pdf")),
                  output = paste0(plateName,"_trackingArea.pdf"))

