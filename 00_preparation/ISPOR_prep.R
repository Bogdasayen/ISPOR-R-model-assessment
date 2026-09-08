### Run all code in this script to download the desired R packages
### from CRAN
# The entire process will take several minutes
#############################################################
# Run the code LINE BY LINE, in the specified order below
############################################################

# Check which version of R you have? You can check if you have the newest version on this website: https://www.r-project.org
getRversion()

# first download and use this package to conveniently install other packages
install.packages('pacman')
library(pacman)

# load (install if required) packages from CRAN
p_load("BCEA", "ggplot2")

# When you get the question:
#"There is a binary version available but the source version is later: binary source needs_compilation # tm  0.7-8  0.7-9              TRUE"
# Do you want to install from sources the package which needs compilation (Yes/no/cancel)
# Type YES
# click "Enter" when they ask about what to update
