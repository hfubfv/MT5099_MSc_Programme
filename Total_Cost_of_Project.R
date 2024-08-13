# Define cost calculation function
calculate_total_cost <- function(buying_one_camera_cost, buying_camera_num, project_distance, 
                                 setting_cameras_cost, walking_to_point_cost_per_kilometer, 
                                 cost_of_processing_a_day_of_camera_data, project_days) {
  # Part 1: Cost of purchasing a camera
  cost_buy_cameras <- buying_one_camera_cost * buying_camera_num
  
  # Part 2: Camera Installation Cost
  cost_setting_cameras <- setting_cameras_cost * buying_camera_num
  
  # Part 3: Cost of Walking to Project Location
  cost_walk_to_point <- project_distance * walking_to_point_cost_per_kilometer
  
  # Part 4: Cost of Watching Videos
  cost_watch_video <- cost_of_processing_a_day_of_camera_data * project_days * buying_camera_num
  
  # Calculate total cost
  total_cost <- cost_buy_cameras + cost_setting_cameras + cost_walk_to_point + cost_watch_video
  
  return(total_cost)
}

# Sample input
buying_one_camera_cost <- 300                 # The cost of purchasing each camera (in USD)
buying_camera_num <- 10                       # Number of cameras (in USD)
project_distance <- 5                         # Project distance (Km)
setting_cameras_cost <- 50                    # Set the cost per camera (in USD)
walking_to_point_cost_per_kilometer <- 1      # Set camera travel cost per kilometer (in USD)
cost_of_processing_a_day_of_camera_data <- 5  #The cost of processing camera data for a day (in USD)
project_days <- 60                            #Project duration (days)
  

# Calculate total cost
total_cost <- calculate_total_cost(buying_one_camera_cost, buying_camera_num, project_distance, 
                                   setting_cameras_cost, walking_to_point_cost_per_kilometer, 
                                   cost_of_processing_a_day_of_camera_data, project_days)

# Print result
cat("The total cost is:", total_cost, "dollars\n")
