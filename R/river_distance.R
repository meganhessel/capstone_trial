river_distance <- function(line, p1, p2) {
  # Get measure along line for each point
  m1 <- st_line_project(line, p1)
  m2 <- st_line_project(line, p2)
  
  # Convert to numeric positions along the line
  pos1 <- as.numeric(m1)
  pos2 <- as.numeric(m2)
  
  # Get total line length
  total_len <- st_length(line) 
  
  # Distance along the line
  abs(pos2 - pos1) * total_len
}
