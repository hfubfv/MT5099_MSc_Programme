library(sp)
library(raster)
library(dplyr)
library(ggplot2)

# Create an empty data box to store simulated output
SingleDesign <- data.frame()

# Set parameters
area_size <- 5000
grid_size <- 10
total_animals <- 25 * 10 * 60 * 24. # number of animal locations to be simulated. 25km2 x 10ind*km2-2(given density) x 60 (monitoring days) x 24 (1loc/h)
n_simulations <- 100
n_sampling_points <- 45

# Create a grid for the sampling area
sampling_area <- raster(nrows=area_size/grid_size, ncols=area_size/grid_size, 
                        xmn=0, xmx=area_size, ymn=0, ymx=area_size)
res(sampling_area) <- grid_size

# Generate uniformly distributed sampling points 
E_coords <- c(1250, 2500, 3750)
N_coords <- seq(333, 4995, by = 333)
sampling_points <- expand.grid(E = E_coords, N = N_coords)

# Experiment cycle 100 times
for (i in 1:n_simulations) {
  # Randomly generate animal positions
  animal_positions <- data.frame(
    E = runif(total_animals, 0, area_size),
    N = runif(total_animals, 0, area_size)
  )
  
  # Calculate the number of animals in each grid 
  animal_counts <- rasterize(animal_positions, sampling_area, fun='count', background=0)
  
  # Calculate encounter rate at sampling points
  encounter_rates <- extract(animal_counts, sampling_points, df=TRUE)$layer
  encounter_rates[is.na(encounter_rates)] <- 0
  
  # Calculate statistical measures
  mean_er <- mean(encounter_rates)
  se_er <- sd(sample(encounter_rates, replace = TRUE, size = 999)) / sqrt(length(encounter_rates))
  cv_er <- se_er / mean_er
  n_ct <- n_sampling_points
  n_ct_detected <- sum(encounter_rates > 0)
  total_n_encounters <- sum(encounter_rates)
  
  # Store results
  SingleDesign <- rbind(SingleDesign, data.frame(
    Simulation = i,
    MEAN_ER = mean_er,
    CV_ER = cv_er,
    N_CT = n_ct,
    N_CT_DETECTED = n_ct_detected,
    TOTAL_N_ENCOUNTERS = total_n_encounters
  ))
}

# Output the results to a txt file
write.table(SingleDesign, "Simulation_Random_Single_Result.txt", row.names=FALSE, sep="\t")


# Visualize animal distribution points and sampling points
ggplot() +
  geom_point(data = animal_positions, aes(x = E, y = N), color = 'yellow', size = 0.001) +
  geom_point(data = sampling_points, aes(x = E, y = N), color = 'blue', size = 2) +
  xlim(0, area_size) +
  ylim(0, area_size) +
  labs(title = 'Animal Distribution and Sampling Points',
       x = 'E (meters)',
       y = 'N (meters)') +
  theme_minimal()

