library(sp)
library(raster)
library(dplyr)
library(ggplot2)

# Create an empty data box to store simulated output
SingleDesign <- data.frame()

# Set parameters
area_size <- 5000
grid_size <- 10
total_animals <- 25*10*60*24 # number of animal locations to be simulated. 25km2 x 10ind*km2-2(given density) x 60 (monitoring days) x 24 (1loc/h)
n_simulations <- 100
n_sampling_points <- 45
n_trails <- 66
buffer_distance <- 10

# Create a grid for the sampling area
sampling_area <- raster(nrows=area_size/grid_size, ncols=area_size/grid_size, 
                        xmn=0, xmx=area_size, ymn=0, ymx=area_size)
res(sampling_area) <- grid_size

# Generate uniformly distributed sampling points 
E_coords <- c(1250, 2500, 3750)
N_coords <- seq(333, 4995, by = 333)
sampling_points <- expand.grid(E = E_coords, N = N_coords)

# Generate trails
set.seed(456)
trails <- data.frame(
  E_start = runif(n_trails, 0, area_size),
  N_start = runif(n_trails, 0, area_size),
  E_end = runif(n_trails, 0, area_size),
  N_end = runif(n_trails, 0, area_size)
)

# Experiment cycle 100 times
for (i in 1:n_simulations) {
  cat("Simulation", i, "\n") # 输出当前模拟次数
  
  # Generate animal location
  set.seed(i)
  animal_positions <- data.frame(
    E = numeric(total_animals),
    N = numeric(total_animals)
  )
  
  # 50% of animals are distributed within the buffer zone near the trajectory
  n_buffer_animals <- total_animals * 0.5
  for (j in 1:n_buffer_animals) {
    trail_index <- sample(1:n_trails, 1)
    trail <- trails[trail_index, ]
    t <- runif(1)
    animal_positions[j, ] <- c(
      trail$E_start + t * (trail$E_end - trail$E_start) + rnorm(1, 0, buffer_distance),
      trail$N_start + t * (trail$N_end - trail$N_start) + rnorm(1, 0, buffer_distance)
    )
  }
  
  # Another 50% of the animals are randomly distributed throughout the entire area
  animal_positions[(n_buffer_animals+1):total_animals, ] <- data.frame(
    E = runif(total_animals - n_buffer_animals, 0, area_size),
    N = runif(total_animals - n_buffer_animals, 0, area_size)
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
  
  # Output some results of the current iteration for debugging purposes
  cat("Mean ER:", mean_er, "CV ER:", cv_er, "N CT Detected:", n_ct_detected, "Total N Encounters:", total_n_encounters, "\n")
}

# Output the results to a txt file
write.table(SingleDesign, "Simulation_TrailBased_Single_Results.txt", row.names=FALSE, sep="\t")
