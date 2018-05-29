# Custom R Functions
# Written by James Harper, PE, ENV SP of the University of Colorado Boulder
# Started October 1, 2017
# Last updated April 4, 2018

freqs.1way = function(data, metric1, return = 0) {
  
  # Create temporary data vector and name variable
  if (is.numeric(metric1)) {
    A = data[metric1][[1]]
    name = paste("freqs_1way_", names(data)[[metric1]], sep = "")
    title = paste("A = ", names(data)[[metric1]], sep = "")
    }
  else if (is.character(metric1)) {
    A = unlist(data[metric1])
    name = paste("freqs_1way_", metric1, sep = "")
    title = paste("A = ", metric1, sep = "")
  }
  else {stop("ERROR: Variable not found in data.")}
  
  # Perform categorical analyses and store results for printing below
  freqs = table(A)
  freqs_prop = prop.table(freqs)
  freqs_prop_df = data.frame(Response = names(freqs_prop), 
                             Proportion = as.numeric(freqs_prop))
  freqs = freqs[order(-freqs)]
  freqs_prop_df = freqs_prop_df[order(-freqs_prop_df$Proportion),]
  
  # Start sending text output to text file
  file1 = file(paste(getwd(),"/output/", name, ".txt", sep = ""))
  sink(file1, append = TRUE)
  sink(file1, append = TRUE, type = "message")

  # Add title to text file
  print(title)

  # Print results of analyses to text file
  print(summary(freqs))
  print(freqs)
  print(freqs_prop_df)

  # Stop sending text output to file
  sink()
  sink(type = "message")
  closeAllConnections()
  
  # Return frequencies if user selected
  if (return == 1) {return(list(freqs, freqs_prop_df))}
  
}
freqs.1way.prov = function(data, metric1, prov, return = 0) {
  
  # Create temporary data vector and name variable
  if (is.numeric(metric1)) {
    A = data[metric1][[1]]
    name = paste("freqs_1way", prov, names(data)[[metric1]], sep = "_")
    title = paste("A = ", names(data)[[metric1]], sep = "")
  }
  else if (is.character(metric1)) {
    A = unlist(data[metric1])
    name = paste("freqs_1way", prov, metric1, sep = "_")
    title = paste("A = ", metric1, sep = "")
  }
  else {stop("ERROR: Variable not found in data.")}
  
  # Perform categorical analyses and store results for printing below
  freqs = table(A)
  freqs_prop = prop.table(freqs)
  freqs_prop_df = data.frame(Response = names(freqs_prop),
                             Proportion = as.numeric(freqs_prop))
  freqs = freqs[order(-freqs)]
  freqs_prop_df = freqs_prop_df[order(-freqs_prop_df$Proportion),]
  
  # Start sending text output to text file
  folder = create_folder(subfolder = prov)
  file1 = file(paste(folder, "/", name, ".txt", sep = ""))
  sink(file1, append = TRUE)
  sink(file1, append = TRUE, type = "message")
  
  # Print title and results to text file
  print(title)
  print(summary(freqs))
  print(freqs)
  print(freqs_prop_df)
  
  # Stop sending text output to file
  sink()
  sink(type = "message")
  closeAllConnections()
  
  # Return results if selected
  if (return == 1) {return(list(freqs, freqs_prop_df))}
  
}
freqs.2way = function(data, metric1, metric2, return = 0) {
  # Also used for stratified one-way categorical analysis
  
  # Create temporary data vector and name variable
  if (is.numeric(metric1) && is.numeric(metric2)) {
    A = data[metric1][[1]]
    B = data[metric2][[1]]
    name_metric1 = names(data)[[metric1]]
    name_metric2 = names(data)[[metric2]]
  }
  else if (is.character(metric1) && is.character(metric2)) {
    A = unlist(data[metric1])
    B = unlist(data[metric2])
    name_metric1 = metric1
    name_metric2 = metric2
  }
  else {stop("ERROR: Variable not found in data, and metrics not same type.")}
  
  # Start sending text output to dump file
  file1 = file(paste(getwd(),"/Output/dump.txt", sep = ""))
  sink(file1, append = TRUE)
  sink(file1, append = TRUE, type = "message")
  
  # Perform categorical analyses
  freqs = table(A, B)
  # print(freqs)
  # freqs = freqs[order(-freqs[,1]),]    # Sort table by freq in 1st column
  # print(freqs)
  # print(fisher.test(freqs))            # Fisher Exact test
  chisq_cramv = assocstats(freqs)        # Calculate chi squared and Cramer's V
  
  # Stop sending text output to dump file
  sink()
  sink(type = "message")
  
  # Create file name and plot name variables using p-value and Cramer's V
  p_value = round(chisq_cramv$chisq_tests[2,3], digits = 3)
  chisqd = round(chisq_cramv$chisq_tests[2,1], digits = 3)
  cramer_v = round(chisq_cramv$cramer, digits = 3)
  name = paste("freqs_2way_", p_value, "_", chisqd, "_", cramer_v, "_",
               name_metric1, "_", name_metric2, sep = "")
  plot_name = paste(name_metric1, "_", name_metric2, "_", p_value, "_",
                    chisqd, "_", cramer_v, sep = "")
  
  # Start sending text output to text file in a given folder based on p_values
  if (is.nan(p_value)) {
    folder = create_folder(subfolder = "p is NaN")       # Create output folder
    file1 = file(paste(folder, "/", name,                # Save text to file 
                       ".txt", sep = ""))
    sink(file1, append = TRUE)
    sink(file1, append = TRUE, type = "message")
  } else if (p_value > 0.05) {
    folder = create_folder(subfolder = "p above 0.05")
    file1 = file(paste(folder, "/", name, ".txt", sep = ""))
    sink(file1, append = TRUE)
    sink(file1, append = TRUE, type = "message")
  } else {
    folder = create_folder(subfolder = "")
    file1 = file(paste(folder, "/", name, ".txt", sep = ""))
    sink(file1, append = TRUE)
    sink(file1, append = TRUE, type = "message")
  }
  
  # Print title
  print(paste("A = ", name_metric1, sep = ""))
  print(paste("B = ", name_metric2, sep = ""))
  
  # Print results of analyses
  print(CrossTable(A, B))
  # print(ftable(freqs))
  print(summary(freqs))
  print(chisq_cramv)
  
  # Stop sending text output to file
  sink()
  sink(type = "message")
  
  # Start saving plot to PDF in a given folder based on p_values
  if (is.nan(p_value)) {
    folder = create_folder(subfolder = "p is NaN")       # Create output folder
    pdf(paste(folder, "/", name, ".pdf", sep = ""))      # Save plot to PDF
  } else if (p_value > 0.05) {
    folder = create_folder(subfolder = "p above 0.05")
    pdf(paste(folder, "/", name, ".pdf", sep = ""))
  } else {
    folder = create_folder(subfolder = "")
    pdf(paste(folder, "/", name, ".pdf", sep = ""))
  }
  
  # Generate categorical analysis plots
  if (length(freqs[,1]) < 50) {
    mosaic(freqs, shade = TRUE, legend = TRUE, main = plot_name)
    balloonplot(freqs, main = name)
  } else {
    mosaic(freqs, shade = TRUE, legend = TRUE, main = plot_name)
    balloonplot(freqs, main = name)
    # Stacked bar plot with legend
    # barplot(table(B,A), main=name, legend = colnames(freqs))
  }
  
  # Stop sending plot output to file
  dev.off()
  closeAllConnections()
  
  # Return frequencies if user selected
  if (return == 1) {return(list(name, freqs, chisq_cramv))}
  
}
freqs.2way.prov = function(data, metric1, metric2, prov, return = 0) {
  
  # Create temporary data vector and name variable
  if (is.numeric(metric1) && is.numeric(metric2)) {
    A = data[metric1][[1]]
    B = data[metric2][[1]]
    name_metric1 = names(data)[[metric1]]
    name_metric2 = names(data)[[metric2]]
  }
  else if (is.character(metric1) && is.character(metric2)) {
    A = unlist(data[metric1])
    B = unlist(data[metric2])
    name_metric1 = metric1
    name_metric2 = metric2
  }
  else {stop("ERROR: Variable not found in data, and metrics not same type.")}
  
  # Start sending text output to dump file
  file1 = file(paste(getwd(),"/Output/dump.txt", sep = ""))
  sink(file1, append = TRUE)
  sink(file1, append = TRUE, type = "message")
  
  # Perform categorical analyses
  freqs = table(A, B)
  # print(fisher.test(freqs))            # Fisher Exact test for small n
  chisq_cramv = assocstats(freqs)        # Chi squared test and Cramer's v
  
  # Stop sending text output to dump file
  sink()
  sink(type = "message")
  
  # Create file name and plot name variables
  p_value = round(chisq_cramv$chisq_tests[2,3], digits = 3)
  chisqd = round(chisq_cramv$chisq_tests[2,1], digits = 3)
  cramer_v = round(chisq_cramv$cramer, digits = 3)
  name = paste("freqs_2way_", prov, "_", p_value, "_", chisqd, "_", cramer_v, "_",
               name_metric1, "_", name_metric2, sep = "")
  plot_name = paste(prov, "_", name_metric1, "_", name_metric2, "_", p_value, "_",
                    chisqd, "_", cramer_v, sep = "")
  
  # Start sending text output to text file
  folder = create_folder(subfolder = prov)
  file1 = file(paste(folder, "/", name, ".txt", sep = ""))
  sink(file1, append = TRUE)
  sink(file1, append = TRUE, type = "message")
  
  # Print title and results
  print(paste("A = ", name_metric1, sep = ""))
  print(paste("B = ", name_metric2, sep = ""))
  print(CrossTable(A, B))
  # print(ftable(freqs))
  print(summary(freqs))
  print(chisq_cramv)
  
  # Stop sending text output to file
  sink()
  sink(type = "message")
  
  # Start saving plot to PDF in a given folder based on p_values
  pdf(paste(folder, "/", name, ".pdf", sep = ""))
  
  # Generate categorical analysis plots
  if (length(freqs[,1]) < 50) {
    mosaic(freqs, shade = TRUE, legend = TRUE, main = plot_name)
    balloonplot(freqs, main = name)
  } else {
    mosaic(freqs, shade = TRUE, legend = TRUE, main = plot_name)
    balloonplot(freqs, main = name)
    # Stacked bar plot with legend
    # barplot(table(B,A), main=name, legend = colnames(freqs))
  }
  
  # Stop sending plot output to file
  dev.off()
  closeAllConnections()
  
  # Return results if user selected
  if (return == 1) {return(list(name, freqs, chisq_cramv))}
  
}
freqs.3way = function(data, metric1, metric2, metric3) {
  
  # Create temporary vectors from data
  A = data[metric1][[1]]
  B = data[metric2][[1]]
  C = data[metric3][[1]]
  
  # Start sending text output to dump file
  file1 = file(paste(getwd(),"/Output/dump.txt", sep = ""))
  sink(file1, append = TRUE)
  sink(file1, append = TRUE, type = "message")
  
  # Perform categorical analyses
  freqs = table(A, B, C)
  freqs_prop = prop.table(freqs, 3)
  # freqs = freqs[order(-freqs[,1]),]              # Sort table by frequency in first column
  # print(fisher.test(freqs))                      # Fisher Exact test
  chisq_cramv = assocstats(freqs)                # Calculate chi squared and Cramer's V
  
  # Stop sending text output to dump file
  sink()
  sink(type = "message")
  
  # Create file name and plot name variables that includes first 2 p-values
  p_value1 = round(chisq_cramv[[1]]$chisq_tests[2,3], digits = 3)
  p_value2 = round(chisq_cramv[[2]]$chisq_tests[2,3], digits = 3)
  name = paste("freqs_3way_", p_value1, "_", p_value2, "_", names(data)[[metric1]], "_", names(data)[[metric2]], "_", 
               names(data)[[metric3]], sep = "")
  plot_name = paste(names(data)[[metric1]], "_", names(data)[[metric2]], "_", names(data)[[metric3]], sep = "")
  
  # Start sending text output to text file in a given folder based on p_values
  if (is.nan(p_value1) || is.nan(p_value2)) {
    
    # Create output folder
    folder = create_folder(subfolder = "p is NaN")
    
    # Start sending text output to text file in folder
    file1 = file(paste(folder, "/", name, ".txt", sep = ""))
    sink(file1, append = TRUE)
    sink(file1, append = TRUE, type = "message")
    
  } else if (p_value1 > 0.05 || p_value2 > 0.05) {
    
    # Create output folder
    folder = create_folder(subfolder = "p above 0.05")
    
    # Start sending text output to text file in folder
    file1 = file(paste(folder, "/", name, ".txt", sep = ""))
    sink(file1, append = TRUE)
    sink(file1, append = TRUE, type = "message")
    
  } else {
    
    # Create output folder
    folder = create_folder(subfolder = "")
    
    # Start sending text output to text file in folder
    file1 = file(paste(folder, "/", name, ".txt", sep = ""))
    sink(file1, append = TRUE)
    sink(file1, append = TRUE, type = "message")
    
  }
  
  # Add title to text file
  print(paste("A = ", names(data)[[metric1]], sep = ""))
  print(paste("B = ", names(data)[[metric2]], sep = ""))
  print(paste("C = ", names(data)[[metric3]], sep = ""))
  
  # Print results of analyses to text file
  print(ftable(freqs_prop))
  print(ftable(freqs))
  print(summary(freqs))
  print(chisq_cramv)
  
  # Stop saving text output to file
  sink()
  sink(type = "message")
  
  # Start saving plot to PDF in a given folder based on p_values
  if (is.nan(p_value1) || is.nan(p_value2)) {
    folder = create_folder(subfolder = "p is NaN")
    pdf(paste(folder, "/", name, ".pdf", sep = ""))
  } else if (p_value1 > 0.05 || p_value2 > 0.05) {
    folder = create_folder(subfolder = "p above 0.05")
    pdf(paste(folder, "/", name, ".pdf", sep = ""))
  } else {
    folder = create_folder(subfolder = "")
    pdf(paste(folder, "/", name, ".pdf", sep = ""))
  }
  
  # Create dataframe from data
  data_frame = data.frame(A, B, C)
  names(data_frame) = c(names(data)[[metric1]], names(data)[[metric2]], 
                        names(data)[[metric3]])
  
  # Generate categorical analysis plots
  scpcp(data_frame, sel = "data[,3]")
  mosaic(freqs, shade = TRUE,                     # Mosaic plot
         legend = TRUE, main = plot_name)
  rmb(freqs)                                      # RMB plot
  rmbmat(freqs, tv = 1)                           # RMB matrix plot
  fluctile(freqs)                                 # Fluctuation plot
  barplot(table(B,A), main = name,                # Stacked bar plot with legend
          legend = colnames(freqs))
  balloonplot(freqs, main = name)
  
  # Stop saving plot to PDF
  dev.off()
  closeAllConnections()
  
}
correspondence = function(data, metric1, metric2) {
  
  # Create temporary data vector and name variable
  if (is.numeric(metric1) && is.numeric(metric2)) {
    A = data[metric1][[1]]
    B = data[metric2][[1]]
    name_metric1 = names(data)[[metric1]]
    name_metric2 = names(data)[[metric2]]
  }
  else if (is.character(metric1) && is.character(metric2)) {
    A = unlist(data[metric1])
    B = unlist(data[metric2])
    name_metric1 = metric1
    name_metric2 = metric2
  }
  else {stop("ERROR: Variable not found in data, and metrics not same type.")}
  
  # Create file name and plot name variables
  name = paste("ca_", name_metric1, "_", name_metric2, sep = "")
  plot_name = paste(name_metric1, "_", name_metric2, sep = "")
  
  # Start sending text output to text file in folder
  file1 = file(paste(getwd(), "/output/", name, ".txt", sep = ""))
  sink(file1, append = TRUE)
  sink(file1, append = TRUE, type = "message")
  
  # Add title to text file
  print(paste("A = ", name_metric1, sep = ""))
  print(paste("B = ", name_metric2, sep = ""))
  
  # Perform correspondence analyses
  CrossTable(A, B)
  freqs = table(A, B)
  print(freqs)
  print(prop.table(freqs, 1))
  print(prop.table(freqs, 2))
  freqs.ca = ca(freqs)
  print(freqs.ca)
  print(summary(freqs.ca))
  
  # Stop saving text output to file
  sink()
  sink(type = "message")
  
  # Generate plots and store in variables
  windowsFonts(A = windowsFont("Times New Roman"))
  plot1 = fviz_ca_biplot(freqs.ca, map = "symbiplot", 
                         arrow = c(TRUE, TRUE), repel = TRUE,
                         family = "A")
  plot2 = fviz_screeplot(freqs.ca, addlabels = TRUE, 
                                    ylim = c(0, 50))
  row = get_ca_row(freqs.ca)
  plot3 = fviz_ca_row(freqs.ca, col.row = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
  plot4 = corrplot(row$cos2, is.corr = FALSE)
  plot5 = fviz_cos2(freqs.ca, choice = "row", axes = 1:2)
  plot6 = corrplot(row$contrib, is.corr = FALSE)    
  plot7 = fviz_contrib(freqs.ca, choice = "row", axes = 1, top = 10)
  plot8 = fviz_contrib(freqs.ca, choice = "row", axes = 2, top = 10)
  plot9 = fviz_contrib(freqs.ca, choice = "row", axes = 1:2, top = 10)
  plot10 = fviz_ca_row(freqs.ca, col.row = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)
  
  col = get_ca_col(freqs.ca)
  plot11 = fviz_ca_col(freqs.ca, col.col = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)
  plot12 = corrplot(col$cos2, is.corr = FALSE)
  plot13 = fviz_cos2(freqs.ca, choice = "col", axes = 1:2)
  plot14 = corrplot(col$contrib, is.corr = FALSE)
  plot15 = fviz_contrib(freqs.ca, choice = "col", axes = 1, top = 10)
  plot16 = fviz_contrib(freqs.ca, choice = "col", axes = 2, top = 10)
  plot17 = fviz_contrib(freqs.ca, choice = "col", axes = 1:2, top = 10)
  plot18 = fviz_ca_col(freqs.ca, col.row = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)
  
  # Save plots to PDF
  ggexport(plotlist = list(plot1, plot2, plot3, plot4, plot5, plot6, plot7,
                           plot8, plot9, plot10, plot11, plot12, plot13,
                           plot14, plot15, plot16, plot17, plot18), 
           filename = paste(getwd(), "/output/", name, ".pdf", sep = ""))
}
correspondence.prov = function(data, metric1, metric2, prov, return = 0) {
  
  # Create temporary vectors and name variable from data
  A = data[metric1][[1]]
  B = data[metric2][[1]]
  
  # Create file name and plot name variables
  name = paste("ca_", prov, "_", names(data)[[metric1]], "_",
               names(data)[[metric2]], sep = "")
  plot_name = paste(prov, "_", names(data)[[metric1]], "_",
                    names(data)[[metric2]], sep = "")
  
  # Start sending text output to text file in folder
  file1 = file(paste(getwd(), "/output/", name, ".txt", sep = ""))
  sink(file1, append = TRUE)
  sink(file1, append = TRUE, type = "message")
  
  # Add title to text file
  print(paste("A = ", names(data)[[metric1]], sep = ""))
  print(paste("B = ", names(data)[[metric2]], sep = ""))
  
  # Perform correspondence analyses
  CrossTable(A, B)
  freqs = table(A, B)
  print(freqs)
  print(prop.table(freqs, 1))
  print(prop.table(freqs, 2))
  freqs.ca = ca(freqs)
  print(freqs.ca)
  print(summary(freqs.ca))
  
  # Stop saving text output to file
  sink()
  sink(type = "message")
  
  # Generate plots and store in variables
  plot1 = fviz_ca_biplot(freqs.ca, map = "symbiplot",
                         arrow = c(TRUE, TRUE), repel = TRUE)
  plot2 = fviz_screeplot(freqs.ca, addlabels = TRUE,
                         ylim = c(0, 50))
  row = get_ca_row(freqs.ca)
  plot3 = fviz_ca_row(freqs.ca, col.row = "cos2",
                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
  plot4 = corrplot(row$cos2, is.corr = FALSE)
  plot5 = fviz_cos2(freqs.ca, choice = "row", axes = 1:2)
  plot6 = corrplot(row$contrib, is.corr = FALSE)
  plot7 = fviz_contrib(freqs.ca, choice = "row", axes = 1, top = 10)
  plot8 = fviz_contrib(freqs.ca, choice = "row", axes = 2, top = 10)
  plot9 = fviz_contrib(freqs.ca, choice = "row", axes = 1:2, top = 10)
  plot10 = fviz_ca_row(freqs.ca, col.row = "contrib",
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                       repel = TRUE)
  
  col = get_ca_col(freqs.ca)
  plot11 = fviz_ca_col(freqs.ca, col.col = "cos2",
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                       repel = TRUE)
  plot12 = corrplot(col$cos2, is.corr = FALSE)
  plot13 = fviz_cos2(freqs.ca, choice = "col", axes = 1:2)
  plot14 = corrplot(col$contrib, is.corr = FALSE)
  plot15 = fviz_contrib(freqs.ca, choice = "col", axes = 1, top = 10)
  plot16 = fviz_contrib(freqs.ca, choice = "col", axes = 2, top = 10)
  plot17 = fviz_contrib(freqs.ca, choice = "col", axes = 1:2, top = 10)
  plot18 = fviz_ca_col(freqs.ca, col.row = "contrib",
                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                       repel = TRUE)
  
  # Save plots to PDF
  folder = create_folder(subfolder = prov)
  ggexport(plotlist = list(plot1, plot2, plot3, plot4, plot5, plot6, plot7,
                           plot8, plot9, plot10, plot11, plot12, plot13,
                           plot14, plot15, plot16, plot17, plot18),
           filename = paste(folder, "/", name, ".pdf", sep = ""))
  
  # Return results if user selected
  if (return == 1) {return(list(name, freqs.ca))}
  
}
# LIKELY DELETE multiple_correspondence
multiple_correspondence = function(data, quali_sup, quanti_sup) {
  library(FactoMineR)
  
  # Perform multiple correspondence analysis
  results = MCA(data, quali.sup = quali_sup, quanti.sup = quanti_sup)
  
  # Create file name and plot name variables
  name = "mca"
  plot_name = name
  
  # Start sending text output to text file in folder
  file1 = file(paste(getwd(), "/output/", name, ".txt", sep = ""))
  sink(file1, append = TRUE)
  sink(file1, append = TRUE, type = "message")
  
  # Print results from multiple correspondence analysis
  summary(results, ncp = 3, nbelements = Inf)
  dimdesc(results)
  
  # Stop saving text output to file
  sink()
  sink(type = "message")
  
  # Generate plots and store in variables
  plot1 = recordPlot(plot(results, label = c("var","quali.sup"), cex = 0.7))
  plot2 = recordPlot(plot(results, invisible = c("var","quali.sup"), cex = 0.7))
  plot3 = recordPlot(plot(results, invisible = c("ind","quali.sup"), autoLab = "y", cex = 0.7, title = "Active Categories"))
  plot4 = recordPlot(plot(results, invisible = c("ind","quali.sup"), autoLab = "y", cex = 0.7, title = "Active Categories", selectMod = "contrib 20"))
  plot5 = recordPlot(plot(results, invisible = c("ind","quali.sup"), cex = 0.7, title = "Active Categories"))
  plot6 = recordPlot(plot(results, invisible = c("ind","var"), autoLab = "y", cex = 0.7, title = "Supplementary Categories"))
  plot7 = recordPlot(plot(results, invisible = "ind", autoLab = "y", cex = 0.7, selectMod = "cos2 10"))
  plot8 = recordPlot(plot(results, invisible = "ind", autoLab = "y", cex = 0.7, selectMod = "contrib 20"))
  plot9 = recordPlot(plot(results, invisible = c("var","quali.sup"), autoLab = "y", cex = 0.7, select = "cos2 10"))
  plot10 = recordPlot(plot(results, autoLab = "y", cex = 0.7, selectMod = "cos2 20", select = "cos2 10"))
  plot11 = recordPlot(plot(results, choix = "var", xlim = c(0,0.6), ylim = c(0,0.6)))
  plot12 = recordPlot(plot(results, choix = "var", xlim = c(0,0.6), ylim = c(0,0.6), invisible = c("ind","quali.sup")))
  plot13 = recordPlot(plot(results, invisible = c("var","quali.sup"), cex = 0.7, select = "contrib 20", axes = 3:4))
  plot14 = recordPlot(plot(results, invisible = c("ind"), cex = 0.7, select = "contrib 20", axes = 3:4))
  plot15 = recordPlot(plotellipses(results, keepvar = c(1:4)))
  
  # Save plots to PDF
  ggexport(plotlist = list(plot1, plot2, plot3, plot4, plot5, plot6, plot7,
                           plot8, plot9, plot10, plot11, plot12, plot13,
                           plot14, plot15), 
           filename = paste(getwd(), "/output/", name, ".pdf", sep = ""))
}
multiple.correspondence = function(data, quali_sup = "", quanti_sup = "",
                                   return = 0) {
  
  # Perform multiple correspondence analysis
  results = MCA(data, quali.sup = quali_sup, quanti.sup = quanti_sup)
  
  # Create file name and plot name variables
  name = paste("mca", prov, sep = "_")
  plot_name = name
  
  # Start sending text output to text file in folder
  folder = create_folder(subfolder = prov)
  file1 = file(paste(folder, "/", name, ".txt", sep = ""))
  sink(file1, append = TRUE)
  sink(file1, append = TRUE, type = "message")
  
  # Print results from multiple correspondence analysis
  summary(results, ncp = 3, nbelements = Inf)
  dimdesc(results)
  
  # Stop saving text output to file
  sink()
  sink(type = "message")
  
  # Generate plots and store in variables
  plot1 = recordPlot(plot(results, label = c("var","quali.sup"), cex = 0.7))
  plot2 = recordPlot(plot(results, invisible = c("var","quali.sup"), cex = 0.7))
  plot3 = recordPlot(plot(results, invisible = c("ind","quali.sup"), autoLab = "y", cex = 0.7, title = "Active Categories"))
  plot4 = recordPlot(plot(results, invisible = c("ind","quali.sup"), autoLab = "y", cex = 0.7, title = "Active Categories", selectMod = "contrib 20"))
  plot5 = recordPlot(plot(results, invisible = c("ind","quali.sup"), cex = 0.7, title = "Active Categories"))
  plot6 = recordPlot(plot(results, invisible = c("ind","var"), autoLab = "y", cex = 0.7, title = "Supplementary Categories"))
  plot7 = recordPlot(plot(results, invisible = "ind", autoLab = "y", cex = 0.7, selectMod = "cos2 10"))
  plot8 = recordPlot(plot(results, invisible = "ind", autoLab = "y", cex = 0.7, selectMod = "contrib 20"))
  plot9 = recordPlot(plot(results, invisible = c("var","quali.sup"), autoLab = "y", cex = 0.7, select = "cos2 10"))
  plot10 = recordPlot(plot(results, autoLab = "y", cex = 0.7, selectMod = "cos2 20", select = "cos2 10"))
  plot11 = recordPlot(plot(results, choix = "var", xlim = c(0,0.6), ylim = c(0,0.6)))
  plot12 = recordPlot(plot(results, choix = "var", xlim = c(0,0.6), ylim = c(0,0.6), invisible = c("ind","quali.sup")))
  plot13 = recordPlot(plot(results, invisible = c("var","quali.sup"), cex = 0.7, select = "contrib 20", axes = 3:4))
  plot14 = recordPlot(plot(results, invisible = c("ind"), cex = 0.7, select = "contrib 20", axes = 3:4))
  plot15 = recordPlot(plotellipses(results, keepvar = c(1:4)))
  
  # Save plots to PDF
  ggexport(plotlist = list(plot1, plot2, plot3, plot4, plot5, plot6, plot7,
                           plot8, plot9, plot10, plot11, plot12, plot13,
                           plot14, plot15),
           filename = paste(folder, "/", name, ".pdf", sep = ""))
  
  # Return results if user selected
  if (return == 1) {return(list(name, results))}
  
}
genlinmod = function(data, formula, iter = 1, perc_train = 0.8, return = 0) {
  iter = 1:iter
  accuracy = rep(0, times = length(iter))
  len = length(data[,1])
  for (i in iter) {
  
    # Create training and testing sets using random sampling
    indices_all = 1:len
    continue = 0
    while (continue == 0) {
      indices_train = sort(sample(x = indices_all, size = round(perc_train*len, 0),
                                  replace = F), decreasing = F)
      indices_test = indices_all[!(indices_all %in% indices_train)]
      # data.frame(indices_train[1:100], indices_test[1:100])
      train = data[indices_train,]; test = data[indices_test,]
      # print(summary(train)); print(summary(test))
      
      # Check if each variable in training and testing sets has two factors
      continue = 1
      for (var in 1:(length(data) - 1)) {
        if (var == 33 | var == 41 | var == 42 | var == 43 ) {next}
        if (nlevels(train[,var]) < 2) { print(paste("Train", var)); continue = 0 }
        if (nlevels(test[,var]) < 2) { print(paste("Test", var)); continue = 0 }
      }
    }
    
    # Run glm model
    model = glm(formula = formula,
                data = train,
                family = binomial(link = "logit"),
                na.action = na.omit)
    
    # Analyze model fit
    if (length(iter) == 1) {
      print(summary(model))
      print(anova(model, test = "Chisq"))
      # print(pR2(model))
      PseudoR2(model)
    }
    
    # Test predictive power of model on test data
    if (perc_train != 1) {
      fitted.results = try(predict(object = model, newdata = test,
                                   type = "response"), silent = T)
      if (inherits(fitted.results, "try-error")) {
        next
      }
      fitted.results = ifelse(fitted.results > 0.5, 1, 0)
      # data.frame(test$IntndPitFullDes[!is.na(fitted.results)],
      #            fitted.results[!is.na(fitted.results)])
      misclass.error = mean(fitted.results[!is.na(fitted.results)] !=
                              test$IntndPitFullDes[!is.na(fitted.results)])
      accuracy[i] = 1 - misclass.error
      
      # Calculate AUC and plot ROC
      if (length(iter) == 1) {
        pr = prediction(fitted.results, test$IntndPitFullDes)
        prf = performance(pr, measure = "tpr", x.measure = "fpr")
        plot(prf)
        auc = performance(pr, measure = "auc")
        auc = auc@y.values[[1]]
        print(auc)
      }
    }
    
    
  
  }
  print(accuracy)
  hist(accuracy)
  print(mean(accuracy[accuracy != 0]))
  
  # Return correspondence results if user selected
  if (return == 1) {return(list(accuracy, model))}
  
}
save_text_output_to_file = function(subfolder, name) {

  # Name folder to store output
  folder = paste(getwd(),"/Output/", subfolder, sep = "")
  
  # Create folder if it does not exist
  if (!dir.exists(folder)) {
    dir.create(folder)
  }
  
  
  
}
create_folder = function(subfolder) {
  
  # Name folder to store output
  folder = paste(getwd(),"/output/", subfolder, sep = "")
  
  # Create folder if it does not exist
  if (!dir.exists(folder)) {
    dir.create(folder)
  }
  
  return(folder)
  
}
sleep_time = function(seconds) {
  p1 = proc.time()
  Sys.sleep(seconds)
  proc.time() - p1 # The cpu usage should be negligible
}
load_libraries = function(libs) {
  for (p in libs) {
    if (!require(p, character.only = TRUE)) {
      install.packages(p)
      require(p, character.only = TRUE)
    }
  }
}
cor.mtest <- function(mat, ...) {
  # From http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
  # Compute the p-value matrix of a cor() matrix
  # mat : is a matrix of data
  # ... : further arguments to pass to the native R cor.test function
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
