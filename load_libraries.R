# Load Libraries
# Install and load a set of libraries
# Written by James Harper, PE, ENV SP of the University of Colorado Boulder
# Started October 1, 2017; Last updated May 28, 2018

load_libraries = function(libs) {
  for (p in libs) {
    if (!require(p, character.only = TRUE)) {
      install.packages(p)
      require(p, character.only = TRUE)
    }
  }
}
