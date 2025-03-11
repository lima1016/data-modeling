# Install and load the FrF2 package if not already installed
# install.packages("FrF2")
library(FrF2)

# Define the fractional factorial design
# 10 factors, 16 runs
design <- FrF2(nruns = 16, nfactors = 10, 
               factor.names = c("LargeYard", "SolarRoof", "Pool", "Garage", 
                                "NewKitchen", "BigWindows", "Garden", 
                                "SecuritySystem", "Basement", "CentralAC"))

# View the design
print(design)

# Optional: Visualize the design matrix
plot(design)