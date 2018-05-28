print("Hello, world!")

print "Hello, world!"

"a" + 1

x = 1
pet = "dog"

x <- 1
pet <- "dog"

###############################################################################
# INITIALIZE
###############################################################################
rm(list = ls())                                      # Clear global environment
cat("\014")                                          # Clear console window
file.remove(dir(paste(getwd(),"/output/", sep = ""), # Clear output folder
                full.names = TRUE))    
source("functions.R")                                # Load custom functions
load_libraries(c("rio", "Amelia"))                   # Install & load libraries

###############################################################################
# LOAD DATA
###############################################################################
file.to.import = paste(getwd(), "/data/data_oct2017.xlsx", sep = "")
data = import(file.to.import)
# names(data)

###############################################################################
# CLEAN DATA
###############################################################################
old.col.names = names(data)                        # Save original column names
names(data) = c("ID", "Creatr", "Date")            # Change column names
print(data.frame(names(data), var_desc))           # Verify consistency

# See presentation for more examples of cleaning data



