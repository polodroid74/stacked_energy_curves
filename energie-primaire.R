library(ggplot2)


plot_energy <- function(colors, img_file){

  time  <- rep(csv_data$t, each=length(energies))
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

args <- commandArgs(trailingOnly = TRUE)
filename=args[1]
out_pdf=args[2]
print(filename)

csv_data=read.table(filename,
                    header=TRUE)

energies=c("nuke",
           "solar",
           "wind",
           "hydro",
           "other_renew",
           "biomass",
           "coal",
           "petrol",
           "gaz")

data_names = c("time",
               energies)
names(csv_data) = data_names

stacked=c()
for(r in 1: nrow(csv_data)){
  for(c in 1 : length(energies)){
    column=as.vector(csv_data[[energies[c]]])
    stacked=c(stacked, as.numeric(column[r]))
  }
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
plot_energy(colors, paste("1", args[2], sep="_"))

colors=c("forestgreen",
         "gray45",
         "gray45",
         "gray45",
         "gray45",
         "gray45",
         "gray45",
         "gray45",
         "gray45")
plot_energy(colors, paste("2", args[2], sep="_"))

colors=c("forestgreen",
         "orangered4",
         "gray8",
         "darkslategray",
         "gray45",
         "gray45",
         "gray45",
         "gray45",
         "gray45")
plot_energy(colors, paste("3", args[2], sep="_"))


colors=c("forestgreen",
         "orangered4",
         "gray8",
         "darkslategray",
         "navyblue",
         "magenta4",
         "steelblue1",
         "yellow2",
         "palegreen1")
plot_energy(colors, paste("4", args[2], sep="_"))
