library(sp)
library(raster)
library(dplyr)
library(ggplot2)

# Create an empty data box to store simulated output
ClusteredDesign <- data.frame()

# Set parameters
area_size <- 5000
grid_size <- 10
total_animals <- 25*10*60*24 # number of animal locations to be simulated. 25km2 x 10ind*km2-2(given density) x 60 (monitoring days) x 24 (1loc/h)
n_simulations <- 100
n_sampling_points <- 15
camera_per_point <- 3
total_cameras <- n_sampling_points * camera_per_point

# Create a grid for the sampling area
sampling_area <- raster(nrows=area_size/grid_size, ncols=area_size/grid_size, 
                        xmn=0, xmx=area_size, ymn=0, ymx=area_size)
res(sampling_area) <- grid_size

# Generate uniformly distributed sampling points (15 points)
sampling_points <- expand.grid(
  E = c(1250, 2500, 3750),
  N = c(500, 1500, 2500, 3500, 4500)
)

# Generate camera positions (3 for each sampling point, for a total of 45)
set.seed(123) # Set random seeds
camera_positions <- data.frame()
for (i in 1:nrow(sampling_points)) {
  camera_positions <- rbind(camera_positions, data.frame(
    E = runif(camera_per_point, sampling_points$E[i] - 25, sampling_points$E[i] + 25),
    N = runif(camera_per_point, sampling_points$N[i] - 25, sampling_points$N[i] + 25)
  ))
}

# Experiment cycle 100 times
for (i in 1:n_simulations) {
  # Randomly generate animal positions
  animal_positions <- data.frame(
    E = runif(total_animals, 0, area_size),
    N = runif(total_animals, 0, area_size)
  )
  
  # Calculate the number of animals in each grid 
  animal_counts <- rasterize(animal_positions, sampling_area, fun='count', background=0)
  
  # Calculate encounter rate at camera position
  encounter_rates <- extract(animal_counts, camera_positions, df=TRUE)$layer
  encounter_rates[is.na(encounter_rates)] <- 0
  
  # Calculate statistical measures
  mean_er <- mean(encounter_rates)
  se_er <- sd(sample(encounter_rates, replace = TRUE, size = 999)) / sqrt(length(encounter_rates))
  cv_er <- se_er / mean_er
  n_ct <- total_cameras
  n_ct_detected <- sum(encounter_rates > 0)
  total_n_encounters <- sum(encounter_rates)
  
  # Store results
  ClusteredDesign <- rbind(ClusteredDesign, data.frame(
    Simulation = i,
    MEAN_ER = mean_er,
    CV_ER = cv_er,
    N_CT = n_ct,
    N_CT_DETECTED = n_ct_detected,
    TOTAL_N_ENCOUNTERS = total_n_encounters
  ))
}

# Output the results to a txt file
write.table(ClusteredDesign, "Simulation_Random_Cluster_results.txt", row.names=FALSE, sep="\t")

