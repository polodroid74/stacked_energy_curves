library(ggplot2)


plot_energy <- function(colors, stacked, img_file){

  energies=c("nuke",
             "solar",
             "wind",
             "hydro",
             "other_renew",
             "biomass",
             "coal",
             "petrol",
             "gaz")
  
  time  <- rep(csv_data$time, each=length(energies))
  value <- stacked
  group <- rep(energies,times=length(stacked)/length(energies))
  
  data_to_plot <- data.frame(time, value, group)
  
  energies_ordered=c("biomass",
                     "coal",
                     "petrol",
                     "gaz",
                     "hydro",
                     "nuke",
                     "wind",
                     "solar",
                     "other_renew")
  data_to_plot$group <- factor(group, levels=rev(energies_ordered))
  # stacked area chart
  ggplot(data_to_plot, aes(x=time, y=value, fill=group)) +
    scale_fill_manual(values = rev(colors)) +
    geom_area(colour = "black", linewidth = .2, alpha = 1.0) +
    theme_bw() +
    theme(legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank()) +
    scale_x_continuous(breaks = seq(min(data_to_plot$time), max(data_to_plot$time), by = 20))
  ggsave(img_file,
         plot = last_plot(),
         width=1920,
         height=1080,
         unit="px")
}


#Here starts the script

twh_to_tep=86000
tep_to_gtep=1e9

args <- commandArgs(trailingOnly = TRUE)
filename=args[1]
out_pdf=args[2]
print(filename)

csv_data=read.table(filename,
                    header=TRUE,
                    sep=",")

energies_raw=c("other_renew",
               "biomass",
               "solar",
               "wind",
               "hydro",
               "nuke", 
               "gaz",
               "oil",
               "coal",
               "biomass_trad")
data_names = c("time", energies_raw)
names(csv_data) = data_names

nuke=c(as.numeric(as.vector(csv_data[["nuke"]])))*twh_to_tep/tep_to_gtep
solar=c(as.numeric(as.vector(csv_data[["solar"]])))*twh_to_tep/tep_to_gtep
wind=c(as.numeric(as.vector(csv_data[["wind"]])))*twh_to_tep/tep_to_gtep
hydro=c(as.numeric(as.vector(csv_data[["hydro"]])))*twh_to_tep/tep_to_gtep
other_renew=c(as.numeric(as.vector(csv_data[["other_renew"]])))*twh_to_tep/tep_to_gtep
biomass=c(as.numeric(as.vector(csv_data[["biomass"]]))+as.numeric(as.vector(csv_data[["biomass_trad"]])))*twh_to_tep/tep_to_gtep
coal=c(as.numeric(as.vector(csv_data[["coal"]])))*twh_to_tep/tep_to_gtep
petrol=c(as.numeric(as.vector(csv_data[["oil"]])))*twh_to_tep/tep_to_gtep
gaz=c(as.numeric(as.vector(csv_data[["gaz"]])))*twh_to_tep/tep_to_gtep

stacked_data=c()
for(r in 1: nrow(csv_data)){
    stacked_data=c(stacked_data, nuke[r], solar[r], wind[r], hydro[r], other_renew[r], biomass[r], coal[r], petrol[r], gaz[r])
}

colors=c("gray45",
         "gray45",
         "gray45",
         "gray45",
         "gray45",
         "gray45",
         "gray45",
         "gray45",
         "gray45")
plot_energy(colors, stacked_data, paste("1", args[2], sep="_"))

colors=c("forestgreen",
         "gray45",
         "gray45",
         "gray45",
         "gray45",
         "gray45",
         "gray45",
         "gray45",
         "gray45")
plot_energy(colors, stacked_data, paste("2", args[2], sep="_"))

colors=c("forestgreen",
         "orangered4",
         "gray8",
         "darkslategray",
         "gray45",
         "gray45",
         "gray45",
         "gray45",
         "gray45")
plot_energy(colors, stacked_data, paste("3", args[2], sep="_"))


colors=c("forestgreen",
         "orangered4",
         "gray8",
         "darkslategray",
         "navyblue",
         "magenta4",
         "steelblue1",
         "yellow2",
         "palegreen1")
plot_energy(colors, stacked_data, paste("4", args[2], sep="_"))
