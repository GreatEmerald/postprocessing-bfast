library(ggplot2)
library(reshape2)
source("../utils/covariate-names.r")

# Visualise a model comparison as a bar chart,
# reading from CSVs in the format of the AccucayStatsTable
# 
# InputDir: where the CSVs are (TLD)
# InputPattern: pattern to find only the relevant CSVs
# InputFiles: filenames to CSVs, overrides InputDir/InputPattern
# ModelMap: named character vector, where the name is the model codename
#   in the format "directory filename.csv" and the value is the human readable
#   name. Only the mentioned models will be used.
#   The legend will be ordered in the supplied order.
# RemovePattern: pattern to remove from class names, so it can be prettified
# Statistics: character vector of column names to visualise, NULL means all
# main: the title
# filename: file to save it to
# plot: whether to plot the result, or to return the table before plotting
VisualiseModelStats = function(InputDir=NULL, InputPattern=NULL, InputFiles=NULL,
    ModelMap, RemovePattern="\\..", Statistics=NULL, main="", filename="",
    xlab="Class", plot=TRUE)
{
    if (is.null(InputFiles))
        InputFiles = list.files(InputDir, pattern=InputPattern, recursive = TRUE, full.names = TRUE)

    # Read files
    InputList = lapply(InputFiles, read.csv)
    # Filter away statistics we don't need
    if (!is.null(Statistics))
        InputList = lapply(InputList, `[`, c("X", Statistics))
    # Rename to pretty names
    InputList = lapply(InputList, function(x){x$X = PrettifyNames(x$X, RemovePattern); x})
    # Convert to long format for ggplot
    InputList = lapply(InputList, melt, variable.name = "Statistic", value.name = "Error", id.vars="X")
    # Create a "model" column in the format of "directory filename" for traceback
    InputList = lapply(1:length(InputList), function(idx) {
            InputList[[idx]]$model = paste(basename(dirname(InputFiles[idx])), basename(InputFiles[idx]), sep="/")
            InputList[[idx]]
        })
    StatTable = do.call(rbind, InputList)

    # Filter table:
    # Models of interest
    StatTable = StatTable[StatTable$model %in% names(ModelMap),]
    # Rename into human-readable name
    for (ModelNameIdx in 1:length(ModelMap))
    {
        StatTable[StatTable$model == names(ModelMap[ModelNameIdx]), "model"] = ModelMap[ModelNameIdx]
    }
    # Order as given
    StatTable$model = ordered(StatTable$model, levels=ModelMap)
    
    if (plot)
    {
        # Plot
        Plot = ggplot(StatTable, aes(x=X, y=Error, fill=model)) +
            geom_bar(stat="identity", position="dodge", width=0.8) +
            scale_fill_brewer(palette="Set1") +
            facet_wrap(~Statistic) +
            ggtitle(main) +
            xlab("Class") +
            #theme_dark() +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        print(Plot)
        if (filename != "")
            ggsave(filename)
        
        return(Plot)
    }
    return(StatTable)
}

# Palette for models

# Blues are DW, greens are LOESS/BEST, reds are BFAST Lite
ModelPalette = c("Dynamic World" = "#0000FF", "Dynamic World + LOESS" = "#00AAFF", "Dynamic World + BFAST Lite" = "#AA00FF",
                "Random Forest regression" = "#000000", "Random Forest + LOESS" = "#00FF00", "Random Forest + BFAST Lite" = "#FF0000",
                "RF + NDVI-only BFAST Lite" = "#880000", "Random Forest + BEAST" = "#008800", "Reference" = "#000088")

# shift_legend from Z.Lin of https://stackoverflow.com/a/54438496
library(gtable)
library(cowplot)

shift_legend <- function(p){

  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }

  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }

  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")

  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")

  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")

  return(gp)
}
