library(sp)
library(raster)
library(dplyr)
library(ggplot2)

# Create an empty data box to store simulated output
RotatingDesign <- data.frame()

# Set parameters
area_size <- 5000
grid_size <- 10
total_animals <- 25*10*60*24 # number of animal locations to be simulated. 25km2 x 10ind*km2-2(given density) x 60 (monitoring days) x 24 (1loc/h)
n_simulations <- 100
n_sampling_points <- 45
n_cameras <- 15
cluster_radius <- 500
n_clusters <- 10

# Create a grid for the sampling area
sampling_area <- raster(nrows=area_size/grid_size, ncols=area_size/grid_size, 
                        xmn=0, xmx=area_size, ymn=0, ymx=area_size)
res(sampling_area) <- grid_size

# Generate sampling points (45 evenly distributed points)
E_coords_phase1 <- c(555, 1110, 1665)
E_coords_phase2 <- c(2220, 2775, 3330)
E_coords_phase3 <- c(3885, 4440, 4995)
N_coords <- c(500, 1500, 2500, 3500, 4500)

sampling_points <- expand.grid(E = c(E_coords_phase1, E_coords_phase2, E_coords_phase3), N = N_coords)

# Experiment cycle 100 times
for (i in 1:n_simulations) {
  all_encounter_rates <- c()
  detected_cameras <- c()
  
  # Simulate camera position movement
  for (phase in 1:3) {
    cluster_centers <- data.frame(
      E = runif(n_clusters, 0, area_size),
      N = runif(n_clusters, 0, area_size)
    )
    
    # Generate animal positions around the center of each cluster
    animal_positions <- data.frame(E = numeric(), N = numeric())
    for (j in 1:n_clusters) {
      cluster_animals <- data.frame(
        E = rnorm(total_animals/n_clusters, mean = cluster_centers$E[j], sd = cluster_radius),
        N = rnorm(total_animals/n_clusters, mean = cluster_centers$N[j], sd = cluster_radius)
      )
      animal_positions <- rbind(animal_positions, cluster_animals)
    }
    
    # Calculate the number of animals in each grid
    animal_counts <- rasterize(animal_positions, sampling_area, fun='count', background=0)
    
    if (phase == 1) {
      camera_positions <- expand.grid(E = E_coords_phase1, N = N_coords)
    } else if (phase == 2) {
      camera_positions <- expand.grid(E = E_coords_phase2, N = N_coords)
    } else {
      camera_positions <- expand.grid(E = E_coords_phase3, N = N_coords)
    }
    
    # Calculate the encounter rate of camera position and use binary distribution, p=1/3
    encounter_rates <- extract(animal_counts, camera_positions, df=TRUE)$layer
    encounter_rates[is.na(encounter_rates)] <- 0
    encounter_rates <- rbinom(length(encounter_rates), size=encounter_rates, prob=1/3)
    
    all_encounter_rates <- c(all_encounter_rates, encounter_rates)
    detected_cameras <- c(detected_cameras, encounter_rates > 0)
  }
  
  # Integrate statistical measures
  mean_er <- mean(all_encounter_rates)
  se_er <- sd(sample(all_encounter_rates, replace = TRUE, size = 999)) / sqrt(length(all_encounter_rates))
  cv_er <- se_er / mean_er
  n_ct <- 3 * n_cameras
  n_ct_detected <- sum(detected_cameras)
  total_n_encounters <- sum(all_encounter_rates)
  
  # Store results
  RotatingDesign <- rbind(RotatingDesign, data.frame(
    Simulation = i,
    MEAN_ER = mean_er,
    CV_ER = cv_er,
    N_CT = n_ct,
    N_CT_DETECTED = n_ct_detected,
    TOTAL_N_ENCOUNTERS = total_n_encounters
  ))
}

# Output the results to a txt file
write.table(RotatingDesign, "simulation_Aggregated_Rotating_Results.txt", row.names=FALSE, sep="\t")
