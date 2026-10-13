library(data.table)
AU <- 149597871
#Planet 8
#Orbit
v <- 5.0*AU
e_7 <- .0086
w <- v * sqrt(1 - e_7^2) #semi minor axis
z <- v * e_7
t <- seq(0, 2*pi, length.out = 365.25)
x_7 <- v * cos(t) - z
y_7 <- w * sin(t)

#Plant Position
planet_8 <- runif(1, 0, 2*pi) #angle
planet_x7 <- v * cos(planet_8) - z
planet_y7 <- w *sin(planet_8)

plot(x_7,y_7, type = "l", asp = 1, col = ("#FF00FF"), lwd = .25, xlab = "X Position (AU)", ylab = "Y Position (AU)", main = "Orbital Paths and Asteriod Locations" )
points(planet_x7, planet_y7, pch = 19, col = "#DA70D6", cex = 1.5)  # planet
points(0,0, pch = "*", col = ("yellow"), cex = .25)

#Planet 7
s <- 3.5*AU
e_6 <- .047
aa <- s * sqrt(1 - e_6^2) #semi minor axis
u <- s * e_6
t <- seq(0, 2*pi, length.out = 365.25)
x_6 <- s * cos(t) - u
y_6 <- aa * sin(t)

#Plant Position
planet_7 <- runif(1, 0, 2*pi) #angle
planet_x6 <- s * cos(planet_7) - u
planet_y6 <- aa *sin(planet_7)

points(x_6,y_6, type = "l", asp = 1, col = ("#DDA0DD"), lwd = .25)
points(planet_x6, planet_y6, pch = 19, col = "#FFE1FF", cex = 1.5)  # planet


#Planet 6
p <- 2.7*AU
e_5 <- .056
q <- p * sqrt(1 - e_5^2) #semi minor axis
r <- p * e_5
t <- seq(0, 2*pi, length.out = 365.25)
x_5 <- p * cos(t) - r
y_5 <- q * sin(t)

#Plant Position
planet_6 <- runif(1, 0, 2*pi) #angle
planet_x5 <- p * cos(planet_6) - r
planet_y5 <- q *sin(planet_6)

points(x_5,y_5, type = "l", asp = 1, col = ("#00F5FF"), lwd = .25)
points(planet_x5, planet_y5, pch = 19, col = "#87CEFA", cex = 1.5)  # planet


#Planet 5
m <- 2.0*AU
e_4 <- .048
n <- m * sqrt(1 - e_4^2) #semi minor axis
o <- m * e_4
t <- seq(0, 2*pi, length.out = 365.25)
x_4 <- m * cos(t) - o
y_4 <- n * sin(t)

#Plant Position
planet_5 <- runif(1, 0, 2*pi) #angle
planet_x4 <- m * cos(planet_5) - o
planet_y4 <- n *sin(planet_5)

points(x_4,y_4, type = "l", asp = 1, col = ("blue"), lwd = .25)
points(planet_x4, planet_y4, pch = 19, col = "#1E90FF", cex = 1.5)  # planet

#Planet 4
j <- 1.524*AU
e_3 <- .09
k <- j * sqrt(1 - e_3^2) #semi minor axis
l <- j * e_3
t <- seq(0, 2*pi, length.out = 365.25)
x_3 <- j * cos(t) - l
y_3 <- k * sin(t)

#Plant Position
planet_4 <- runif(1, 0, 2*pi) #angle
planet_x3 <- j * cos(planet_4) - l
planet_y3 <- k *sin(planet_4)

points(x_3,y_3, type = "l", asp = 1, col = ("green"), lwd = .25)
points(planet_x3, planet_y3, pch = 19, col = "#7FFF00", cex = 1.5)  # planet

#Planet 3
g <- AU
e_2 <- .0167
h <- g * sqrt(1 - e_2^2) #semi minor axis
i <- g * e_2
t <- seq(0, 2*pi, length.out = 365.25)
x_2 <- g * cos(t) - i
y_2 <- h * sin(t)

#Plant Position
planet_3 <- runif(1, 0, 2*pi) #angle
planet_x2 <- g * cos(planet_3) - i
planet_y2 <- h *sin(planet_3)

points(x_2,y_2, type = "l", asp = 1, col = ("pink"), lwd = .25)
points(planet_x2, planet_y2, pch = 19, col = "hotpink", cex = 1.5)  # planet

#Planet 2
#orbit
d <- .723*AU #semi major axis
e_1 <- .007
e <- d * sqrt(1 - e_1^2) #semi minor axis
f <- d * e_1 
t <- seq(0, 2*pi, length.out = 365.25)
x_1 <- d * cos(t) - f
y_1 <- e * sin(t)

#Plant Position
planet_2 <- runif(1, 0, 2*pi) #angle
planet_x1 <- d * cos(planet_2) - f
planet_y1 <- e *sin(planet_2)

points(x_1,y_1, type = "l", asp = 1, col = ("red"), lwd = .25)
points(planet_x1, planet_y1, pch = 19, col = "salmon", cex = 1.5)  # planet

#Planet 1 
#Eclipse 
a <- .387*AU #semi major axis
e_0 <- .206
b <- a * sqrt(1 - e_0^2) #semi minor axis
c <- a * e_0 
t <- seq(0, 2*pi, length.out = 365.25)
x_0 <- a * cos(t) - c
y_0 <- b * sin(t)

#Plant Position
planet_1 <- runif(1, 0, 2*pi) #angle
planet_x <- a * cos(planet_1) - c
planet_y <- b *sin(planet_1)

points(x_0,y_0, type = "l", asp = 1, col = ("#FFD700"), lwd = .25) #orbit
points(planet_x, planet_y, pch = 19, col = "#FFB90F", cex = 1.5)  # planet

planet_radii <- c(
  2440, 6052, 6371, 3390, 7000, 6500, 6000, 8000
)

planets <- data.frame(
  name = c("planet_1", "planet_2", "planet_3", "planet_4",
           "planet_5", "planet_6", "planet_7", "planet_8")
)
#Initialized System

planets$temperature <- c(440, 300, 288, 210, 165, 134, 76, 72)
life_temp_min <- 0
life_temp_max <- 394

is_temp_habitable <- function(temp) temp >= life_temp_min & temp <= life_temp_max

planets$radiation <- c(5, 3, 3, 3, 4, 4, 4, 5)
radiation_limit <- 4

is_rad_habitable <- function(rad) rad <= radiation_limit

planet_is_habitable <- function(temp, rad) {
  is_temp_habitable(temp) & is_rad_habitable(rad)
}

planets$habitable <- planet_is_habitable(
  planets$temperature,
  planets$radiation
)

planets$can_have_life <- TRUE
planet_index <- setNames(seq_len(nrow(planets)), planets$name)

#timesteps 
timestep <- 50
event_table <- data.frame(
  run = integer(),
  year = integer(),
  source = character(),
  target = character(),
  type = character(),       #asteriod or dispersal from planet
  distance_AU = numeric(),
  stringsAsFactors = FALSE
)


#pre results
n_runs <- 100
results <- data.frame(
  run = 1:n_runs,
  asteroid_hits = integer(n_runs),
  avg_extinction_time = numeric(n_runs),
  planet_2_asteroid_hits = integer(n_runs),
  planet_3_asteroid_hits = integer(n_runs),
  planet_4_asteroid_hits = integer(n_runs),
  planet_5_asteroid_hits = integer(n_runs),
  planet_6_asteroid_hits = integer(n_runs),
  Planet_7_asteroid_hits = integer(n_runs),
  
  planet_2_dispersal_hits = integer(n_runs),
  planet_3_dispersal_hits = integer(n_runs),
  planet_4_dispersal_hits = integer(n_runs),
  planet_5_dispersal_hits = integer(n_runs),
  planet_6_dispersal_hits = integer(n_runs),
  Planet_7_dispersal_hits = integer(n_runs),
  
  planet_2_first_appearance = numeric(n_runs),
  planet_3_first_appearance = numeric(n_runs),
  planet_4_first_appearance = numeric(n_runs),
  planet_5_first_appearance = numeric(n_runs),
  planet_6_first_appearance = numeric(n_runs),
  Planet_7_first_appearance = numeric(n_runs),
  
  planet_2_extinction = numeric(n_runs),
  planet_3_extinction = numeric(n_runs),
  planet_4_extinction = numeric(n_runs),
  planet_5_extinction = numeric(n_runs),
  planet_6_extinction = numeric(n_runs),
  planet_7_extinction = numeric(n_runs),
  
  planet_2_colonized = integer(n_runs),
  planet_3_colonized = integer(n_runs),
  planet_4_colonized = integer(n_runs),
  planet_5_colonized = integer(n_runs),
  planet_6_colonized = integer(n_runs),
  planet_7_colonized = integer(n_runs),
  
  planet_2_colonization_time = numeric(n_runs),
  planet_3_colonization_time = numeric(n_runs),
  planet_4_colonization_time = numeric(n_runs),
  planet_5_colonization_time = numeric(n_runs),
  planet_6_colonization_time = numeric(n_runs),
  planet_7_colonization_time = numeric(n_runs),
  
  planet_2_colonization_events = integer(n_runs),
  planet_3_colonization_events = integer(n_runs),
  planet_4_colonization_events = integer(n_runs),
  planet_5_colonization_events = integer(n_runs),
  planet_6_colonization_events = integer(n_runs),
  planet_7_colonization_events = integer(n_runs),
  
  planet_2_persistence = integer(n_runs),
  planet_3_persistence = integer(n_runs),
  planet_4_persistence = integer(n_runs),
  planet_5_persistence = integer(n_runs),
  planet_6_persistence = integer(n_runs),
  planet_7_persistence = integer(n_runs),
  
  system_mean_persistence = numeric(n_runs),
  
  total_hits = integer(n_runs)
)

solve_kepler <- function(M, e, tol = 1e-10, max_iter = 15) {
  E <- M
  for(i in 1:max_iter) {
    delta <- (E - e * sin(E) - M) / (1 - e * cos(E))
    E <- E - delta
    if (max(abs(delta)) < tol) break
  }
  E
}

#Table of Planets 1 to 100 years and where to find them in this time frame
planet_orbits <- data.frame(
  name = c("planet_1", "planet_2", "planet_3", "planet_4", "planet_5", "planet_6", "planet_7", "planet_8"),
  A = c(.387, .723, 1.0, 1.524, 5.204, 9.58, 19.2, 30.06) * AU, #Semi major axis
  E = c(.206, .007, .0167, .09, .048, .056, .047, .0086),  #eccentricity
  T = c(.24, .62, 1.0, 1.88, 11.86, 29.46, 84.01, 164.8),  # orbital periods in years
  t0 = runif(8, 0, 2*pi))  # random starting angles

planet_orbits$B <- planet_orbits$A * sqrt(1 - planet_orbits$E^2)            #B is the semi minor axis
planet_orbits$c <- planet_orbits$A * planet_orbits$E   #c is the focal distance
planet_orbits$collision_radius <- planet_radii * 100
planet_orbits$capture_radius   <- planet_radii * 1.5

planet_orbits_dt <- as.data.table(planet_orbits)
planet_orbits_dt[, name := as.character(name)]
setkey(planet_orbits_dt, name)

gpp <- function(t) {  #get planet position
  out <- vector("list", nrow(planet_orbits))
  for (i in 1:nrow(planet_orbits)) {
    p <- planet_orbits[i, ]
    n <- 2 * pi / p$T
    M <- (p$t0 + t * n) %% (2 * pi)
    E <- solve_kepler(M, p$E)
    out[[i]] <- data.frame(
      planet = p$name,
      x_planet= p$A * cos(E) - p$c,
      y_planet = p$B * sin(E)
    )
  }
  do.call(rbind, out)
}

timestep <- 50

#interplanetary distance table in relation to planets as well as tracking 
event_log <- data.table(
  run = integer(),
  year = numeric(),
  event_type = character(),
  source = character(),
  target = character(),
  distance_AU = numeric(),
  size = numeric(),
  had_life = logical(),
  resulted_in_colonization = logical(),
  notes = character()
)

for (r in 1:n_runs) {  #beginning of main loop
  
  life_status <- data.frame(planet = planets$name, has_life = FALSE)
  life_history <- data.frame(
    planet = planets$name,
    first_appearance = rep(NA, nrow(planets)),
    extinction_events = I(vector("list", nrow(planets))),
    final_state = rep(NA, nrow(planets)),
    asteroid_colonized = rep(FALSE, nrow(planets)),
    dispersal_colonized = rep(FALSE, nrow(planets))
  )
  
  persistence <- rep(0, nrow(planets))
  pending_dispersal <- data.frame(planet=character(), arrival_year=numeric(), has_life=logical())
  pending_asteroid  <- data.frame(planet=character(), arrival_year=numeric(), has_life=logical())
  
  # counters
  planet_3_asteroid_hits <- 0
  planet_4_asteroid_hits <- 0
  planet_2_asteroid_hits <- 0
  planet_5_asteroid_hits <- 0
  planet_6_asteroid_hits <- 0
  planet_7_asteroid_hits <- 0
  
  planet_3_dispersal_hits <- 0
  planet_4_dispersal_hits <- 0
  planet_2_dispersal_hits <- 0
  planet_5_dispersal_hits <- 0
  planet_6_dispersal_hits <- 0
  planet_7_dispersal_hits <- 0
  
  ex_prob <- function(size) {1/(1+exp(-(size-3)))}
  local_extinction <- 0.05
  p_carry <- .15
  planet_xy <- function(planet_idx, at_time) {
    orbit <- planet_orbits[planet_idx, ]
    ma <- (orbit$t0 + at_time * 2 * pi / orbit$T) %% (2 * pi) #mean anomaly
    ea  <- solve_kepler(ma, orbit$E) #eccentricity anomaly
    list(x = orbit$A * cos(ea) - orbit$c,
         y = orbit$B * sin(ea))
  }
  
  #General Asteroid Info
  
  total_asteroids <- sample(1000:10000, 1)
  frac_IS <- 0.35 #the percent of asteroids that are interstellar asteroids
  frac_SS <- 0.35 #the percent of asteroids that are already in the solar system (like from our asteroid belt)
  frac_DI <- 0.30 #the dispersal asteroids that come from the planets
  
  num_IS <- round(total_asteroids * frac_IS)
  num_SS <- round(total_asteroids * frac_SS)
  num_DI <- total_asteroids - num_SS - num_IS
  
  #Interstellar Asteroids
  IS_entry_dist <- runif(num_IS, 5*AU, 15*AU)
  IS_entry_angle <- runif(num_IS, 0, 2*pi)
  IS_speed <- runif(num_IS, 1.7, 7) * AU
  
  IS <- data.frame(
    id = paste0("IS_", 1:num_IS),
    entry_time = runif(num_IS, 0, 250),
    entry_angle = IS_entry_angle,
    speed = IS_speed,
    start_x = IS_entry_dist * cos(IS_entry_angle),
    start_y = IS_entry_dist * sin(IS_entry_angle),
    has_life = runif(num_IS) < 0.05
  )
  
  IS$size <- runif(num_IS,.1, 10)
  IS$travel_angle <- atan2(-IS$start_y, -IS$start_x) + rnorm(num_IS, 0, 0.1)
  #Solar System Asteroids (Asteroid Belt/Random in Solar System Asteroids)
  a_SS <- runif(num_SS, 1*AU, 3.5*AU) #semi major axis of the asteroids 
  e_SS <- runif(num_SS, 0.1, 0.4) #eccentricity
  entry_angle <- runif(num_SS, 0, 2*pi)
  travel_angle <- entry_angle + pi + rnorm(num_SS, 0, 0.5)
  speed <- runif(num_SS, 1, 6) * AU
  
  SS <- data.frame(
    id = paste0("SS_", 1:num_SS),
    entry_time = runif(num_SS, 0, 250),
    entry_angle = entry_angle,
    travel_angle = travel_angle,
    speed = speed,
    start_x = a_SS * cos(entry_angle),
    start_y = a_SS * sin(entry_angle),
    has_life = runif(num_SS) < 0.05
  )
  SS$size <- runif(num_SS, .1, 10)
  #Dispersal Asteroids from planets
  DI_entry_dist <- runif(num_DI, .7*AU, 2.0*AU)
  DI_entry_angle <- runif(num_DI, 0, 2*pi)
  DI_travel_angle <- DI_entry_angle + rnorm(num_DI, 0, 0.5)
  DI_speed <- runif(num_DI, .1, 1.5) * AU
  
  DI <- data.frame(
    id = paste0("DI_", 1:num_DI),
    entry_time = runif(num_DI, 0, 250),
    entry_angle = DI_entry_angle,
    travel_angle = DI_travel_angle,
    speed = DI_speed,
    start_x = DI_entry_dist * cos(DI_entry_angle),
    start_y = DI_entry_dist * sin(DI_entry_angle),
    has_life = FALSE
  )
  DI$size <- runif(num_DI, .1, 10)
  
  asteroids <- rbind(IS, SS, DI)
  asteroids$already_hit <- FALSE
  asteroids_dt <- as.data.table(asteroids)
  setkey(asteroids_dt, id)
  
  #Subset loop
  for (year in 1:250) { 
    
    #life can only be dispersed from planets with life
    sp <- which(life_status$has_life) #seeded planets
    
    if (length(sp) > 0) { 
      ejecta <- asteroids_dt[grepl("^DI_", id) &
                                        entry_time > year - 1 &
                                        entry_time <= year, id]
      # ejecta id
      for (ei in ejecta) {
        source_idx    <- sp[sample.int(length(sp), 1)]
        launch_time   <- asteroids_dt[id == ei, entry_time]
        source_xy     <- planet_xy(source_idx, launch_time)
        launch_offset <- 1.05 * planet_orbits$collision_radius[source_idx]
        
        asteroids_dt[id == ei, `:=`(
          start_x  = source_xy$x + launch_offset * cos(entry_angle),
          start_y  = source_xy$y + launch_offset * sin(entry_angle),
          has_life = runif(1) < p_carry
        )]
      }
    }
    
    for(s in 1:timestep) {
      t <- (year - 1) + s/timestep
      planet_pos_ts <- gpp(t)
      
      active <- asteroids_dt[entry_time <= t]
      if(nrow(active) > 0) {
        e <-t -active$entry_time #elapsed 
        active[, x_asteroid := start_x + speed * e *cos(travel_angle)]
        active[, y_asteroid := start_y + speed * e * sin(travel_angle)]
      }
      
      #Collisions detection
      c <- active[already_hit == FALSE] #candidates
      if (nrow(c) > 0){
        hits <- rbindlist(lapply(seq_len(nrow(planet_pos_ts)), function(k) {
          d <- sqrt((c$x_asteroid - planet_pos_ts$x_planet[k])^2 +
                      (c$y_asteroid - planet_pos_ts$y_planet[k])^2)
          idx_hit <- which(d < planet_orbits$collision_radius[k])
          if (length(idx_hit) == 0) return(NULL)
          out <- c[idx_hit]
          out[, `:=`(planet = planet_pos_ts$planet[k],
                     distance = d[idx_hit],
                     collision_radius = planet_orbits$collision_radius[k],
                     capture_radius = planet_orbits$capture_radius[k])]
          out
        }))
        
        if(nrow(hits) > 0){
          for( i in 1:nrow(hits)) {
            p_name <- hits$planet[i]
            a_name <- hits$id[i]
            idx <- planet_index[p_name]
            
            asteroids_dt[id == a_name, already_hit := TRUE]
            
            if (p_name == "planet_2") planet_2_asteroid_hits <- planet_2_asteroid_hits + 1
            if (p_name == "planet_3") planet_3_asteroid_hits <- planet_3_asteroid_hits + 1
            if (p_name == "planet_4") planet_4_asteroid_hits <- planet_4_asteroid_hits + 1
            if (p_name == "planet_5") planet_5_asteroid_hits <- planet_5_asteroid_hits + 1
            if (p_name == "planet_6") planet_6_asteroid_hits <- planet_6_asteroid_hits + 1
            if (p_name == "planet_7") planet_7_asteroid_hits <- planet_7_asteroid_hits + 1
            
            asteroid_row <- hits[i, ]
            tx <- planet_pos_ts$x_planet[planet_pos_ts$planet == p_name]
            ty <- planet_pos_ts$y_planet[planet_pos_ts$planet == p_name]
            
            colonized_this_hit <- isTRUE(asteroid_row$has_life) && planets$habitable[idx]
            
            event_log <- rbind(event_log, data.table(
              run = r,
              year = t,
              event_type = "asteroid_hit",
              source = a_name,
              target = p_name,
              distance_AU = asteroid_row$distance / AU,
              size = asteroid_row$size,
              had_life = asteroid_row$has_life,
              resulted_in_colonization = colonized_this_hit,
              notes = ""
            ))
            if (asteroid_row$has_life) {
              arrival <- min(year + sample(1:5, 1), 250)
              pending_asteroid <- rbind(pending_asteroid,
                                        data.frame(planet = p_name, arrival_year = arrival, has_life = TRUE))
            }
          }
        }
      }
      #timestep dispersal loop  
      for(og_planet in planets$name) {
        idx <- planet_index[og_planet]
        if(!life_status$has_life[idx]) next
        
        og <- planet_pos_ts[planet_pos_ts$planet == og_planet, ]
        dx <- planet_pos_ts$x_planet - og$x_planet
        dy <- planet_pos_ts$y_planet - og$y_planet
        planet_pos_ts$dist <- sqrt(dx^2 + dy^2)
        
        targets <- subset(planet_pos_ts, planet != og_planet)
        target_indices <- planet_index[targets$planet]
        targets <- targets[planets$habitable[target_indices], ]
        
        if (nrow(targets) > 0) {
          base_rate <- 1 - (1 - 0.20)^(1/timestep)
          scale <- 3*AU
          probs <- base_rate * exp(-targets$dist / scale)
          hits <- runif(nrow(targets)) < probs
          hits_planets <- targets$planet[hits]
          for (tp in hits_planets) {
            dis_to_tp <- targets$dist[targets$planet == tp]
            travel_years <- dis_to_tp/ (.5 * AU)
            arrival_year <- min(year + ceiling(travel_years + runif(1, 0, 3)), 250)
            
            pending_dispersal <- rbind(pending_dispersal,
                                       data.frame(planet = tp, 
                                                  arrival_year = arrival_year, 
                                                  has_life = TRUE, 
                                                  source_planet = og_planet))
            tp_pos <- targets[targets$planet == tp, ] #target position
            event_log <- rbind(event_log, data.table(
              run = r,
              year = year,
              event_type = "dispersal_hit",
              source = og_planet,
              target = tp,
              distance_AU = dis_to_tp / AU,
              size = NA_real_,
              had_life = TRUE,
              resulted_in_colonization = FALSE,
              notes = "attempt"
            ))
          }
        }
      }
    }
    #Asteroid Arrivals
    a_arrivals <- pending_asteroid[pending_asteroid$arrival_year == year, ]
    if (nrow(a_arrivals) > 0) {
      for (j in 1:nrow(a_arrivals)) {
        tp <- a_arrivals$planet[j]
        idx <- planet_index[tp]  #tp = target planet            
        if (is.na(idx)) next
        
        # if asteroid has life then it has the chance to seed life on the planet
        if (a_arrivals$has_life[j] && planets$habitable[idx] && life_status$has_life[idx]) {
          life_status$has_life[idx] <- TRUE
          life_history$asteroid_colonized[idx] <- TRUE
          
          # if life is seeded it is record the first time
          if (is.na(life_history$first_appearance[idx])) {
            life_history$first_appearance[idx] <- year}
          
          add_event(
            run = r,
            year = year,
            event_type = "colonization",
            source = "asteroid",
            target = tp,
            distance_AU = NA_real_,
            size = NA_real_,
            had_life = TRUE,
            resulted_in_colonization = TRUE,
            notes = "asteroid arrival"
          )
        }
        if(a_arrivals$has_life[j] && planets$habitable[idx] && !life_status$has_life[idx]) {
          add_event(
            run = r,
            year = year,
            event_type = "rescue",
            source = "asteroid",
            target = tp,
            distance_AU = NA_real_,
            size = NA_real_,
            had_life = TRUE,
            resulted_in_colonization = FALSE,
            notes = "rescue event"
          )
        }
      }
    }
    pending_asteroid <- pending_asteroid[pending_asteroid$arrival_year != year, ]
    
    #Dispersal collisions
    d_arrivals <- pending_dispersal[pending_dispersal$arrival_year == year, ] 
    if (nrow(d_arrivals) > 0) {
      for (j in 1:nrow(d_arrivals)) {
        tp <- d_arrivals$planet[j]
        idx <- planet_index[tp]
        if (planets$habitable[idx] && !life_status$has_life[idx]) { 
          life_status$has_life[idx] <- TRUE
          life_history$dispersal_colonized[idx] <- TRUE
          
          if (tp == "planet_2") planet_2_dispersal_hits <- planet_2_dispersal_hits + 1
          if (tp == "planet_3") planet_3_dispersal_hits <- planet_3_dispersal_hits + 1
          if (tp == "planet_4") planet_4_dispersal_hits <- planet_4_dispersal_hits + 1
          if (tp == "planet_5") planet_5_dispersal_hits <- planet_5_dispersal_hits + 1
          if (tp == "planet_6") planet_6_dispersal_hits <- planet_6_dispersal_hits + 1
          if (tp == "planet_7") planet_7_dispersal_hits <- planet_7_dispersal_hits + 1
          
          if (is.na(life_history$first_appearance[idx])) life_history$first_appearance[idx] <- year
          
          add_event(
            run = r,
            year = year,
            event_type = "colonization",
            source = d_arrivals$source_planet[j],
            target = tp,
            distance_AU = NA_real_,
            size = NA_real_,
            had_life = TRUE,
            resulted_in_colonization = TRUE,
            notes = "dispersal arrival"
          )
        }
        if(planets$habitable[idx] && life_status$has_life[idx]) {
          add_event(
            run = r,
            year = year,
            event_type = "rescue",
            source = d_arrivals$source_planet[j],
            target = tp,
            distance_AU = NA_real_,
            size = NA_real_,
            had_life = TRUE,
            resulted_in_colonization = FALSE,
            notes = "rescue event"
          )
        }
      }
    }
    pending_dispersal <- pending_dispersal[pending_dispersal$arrival_year != year, ]
    
    #extinction on impact
    this_year <- year
    year_hits <- event_log[run == r & event_type == "asteroid_hit" & ceiling(year) == this_year]
    
    if (nrow(year_hits) > 0) {
      for (i in 1:nrow(year_hits)) {
        p_name <- year_hits$target[i]
        idx <- planet_index[p_name]
        if (!is.na(idx) &&
            isTRUE(life_status$has_life[idx]) &&
            !is.na(life_history$first_appearance[idx])) {
          
          p_ext <- ex_prob(year_hits$size[i])
          if (runif(1) < p_ext) {
            life_history$extinction_events[[idx]] <-
              c(life_history$extinction_events[[idx]], year)
            life_status$has_life[idx] <- FALSE
            
            event_log <- rbind(event_log, data.table(
              run = r,
              year = year,
              event_type = "extinction",
              source = "asteroid",
              target = p_name,
              distance_AU = NA_real_,
              size = year_hits$size[i],
              had_life = TRUE,
              resulted_in_colonization = FALSE,
              notes = "impact extinction"
            ))
          }
        }
      }
    }
    for (k in seq_len(nrow(planets))) {
      if (life_status$has_life[k] && runif(1) < local_extinction) {
        life_history$extinction_events[[k]] <- c(life_history$extinction_events[[k]], year)
        life_status$has_life[k] <- FALSE
      }
    }
    for (k in seq_len(nrow(planets))) {
      if (life_status$has_life[k]) persistence[k] <- persistence[k] + 1
    }
  }
  
  # extinction metrics 
  # total asteroid collisions this run
  asteroid_hits <- nrow(event_log[run == r & event_type == "asteroid_hit"])
  
  # extinction duration for each planet (time from first appearance to each extinction event)
  ext_times <- sapply(seq_len(nrow(life_history)), function(i) {
    fa <- life_history$first_appearance[i]
    ev <- life_history$extinction_events[[i]]
    if (is.na(fa) || length(ev) == 0) return(NA_real_)
    mean(ev - fa)
  })
  
  avg_extinction_time <- mean(ext_times, na.rm = TRUE)
  
  #extinction duration for a specific planet
  get_extinction_duration <- function(pname) {
    idx <- match(pname, life_history$planet)
    fa  <- life_history$first_appearance[idx]
    ev  <- life_history$extinction_events[[idx]]
    if (is.na(fa) || length(ev) == 0) return(NA_real_)
    mean(ev - fa)
  }
  
  planet_2_extinction <- get_extinction_duration("planet_2")
  planet_3_extinction <- get_extinction_duration("planet_3")
  planet_4_extinction <- get_extinction_duration("planet_4")
  planet_5_extinction <- get_extinction_duration("planet_5")
  planet_6_extinction <- get_extinction_duration("planet_6")
  planet_7_extinction <- get_extinction_duration("planet_7")
  
  # first appearance times 
  fa2 <- life_history$first_appearance[match("planet_2", life_history$planet)]
  fa3 <- life_history$first_appearance[match("planet_3", life_history$planet)]
  fa4 <- life_history$first_appearance[match("planet_4", life_history$planet)]
  fa5 <- life_history$first_appearance[match("planet_5", life_history$planet)]
  fa6 <- life_history$first_appearance[match("planet_6", life_history$planet)]
  fa7 <- life_history$first_appearance[match("planet_7", life_history$planet)]
  
  # colonized flags
  planet_2_colonized <- ifelse(!is.na(fa2), 1, 0)
  planet_3_colonized <- ifelse(!is.na(fa3), 1, 0)
  planet_4_colonized <- ifelse(!is.na(fa4), 1, 0)
  planet_5_colonized <- ifelse(!is.na(fa5), 1, 0)
  planet_6_colonized <- ifelse(!is.na(fa6), 1, 0)
  planet_7_colonized <- ifelse(!is.na(fa7), 1, 0)
  
  # colonization times
  planet_2_colonization_time <- fa2
  planet_3_colonization_time <- fa3
  planet_4_colonization_time <- fa4
  planet_5_colonization_time <- fa5
  planet_6_colonization_time <- fa6
  planet_7_colonization_time <- fa7
  
  # number of colonization events (first appearance + recolonizations)
  get_colonization_events <- function(pname) {
    idx <- match(pname, life_history$planet)
    fa  <- life_history$first_appearance[idx]
    ev  <- life_history$extinction_events[[idx]]
    if (is.na(fa)) return(0)
    1 + length(ev)
  }
  
  planet_2_colonization_events <- get_colonization_events("planet_2")
  planet_3_colonization_events <- get_colonization_events("planet_3")
  planet_4_colonization_events <- get_colonization_events("planet_4")
  planet_5_colonization_events <- get_colonization_events("planet_5")
  planet_6_colonization_events <- get_colonization_events("planet_6")
  planet_7_colonization_events <- get_colonization_events("planet_7")
  
  table(
    event_log[event_type == "dispersal_hit"]$source,
    event_log[event_type == "dispersal_hit"]$target
  )
  
  #store results
  
  results$asteroid_hits[r]                <- asteroid_hits
  results$avg_extinction_time[r]          <- avg_extinction_time
  
  results$planet_2_asteroid_hits[r]       <- planet_2_asteroid_hits
  results$planet_3_asteroid_hits[r]       <- planet_3_asteroid_hits
  results$planet_4_asteroid_hits[r]       <- planet_4_asteroid_hits
  results$planet_5_asteroid_hits[r]       <- planet_5_asteroid_hits
  results$planet_6_asteroid_hits[r]       <- planet_6_asteroid_hits
  results$planet_7_asteroid_hits[r]       <- planet_7_asteroid_hits
  
  results$planet_2_dispersal_hits[r]      <- planet_2_dispersal_hits
  results$planet_3_dispersal_hits[r]      <- planet_3_dispersal_hits
  results$planet_4_dispersal_hits[r]      <- planet_4_dispersal_hits
  results$planet_5_dispersal_hits[r]      <- planet_5_dispersal_hits
  results$planet_6_dispersal_hits[r]      <- planet_6_dispersal_hits
  results$planet_7_dispersal_hits[r]      <- planet_7_dispersal_hits
  
  results$total_hits[r] <- planet_2_asteroid_hits + planet_3_asteroid_hits + 
    planet_4_asteroid_hits + planet_5_asteroid_hits + planet_6_asteroid_hits + 
    planet_7_asteroid_hits + planet_2_dispersal_hits + planet_3_dispersal_hits +
    planet_4_dispersal_hits + planet_5_dispersal_hits + planet_6_dispersal_hits +
    planet_7_dispersal_hits
  
  results$planet_2_first_appearance[r]    <- fa2
  results$planet_3_first_appearance[r]    <- fa3
  results$planet_4_first_appearance[r]    <- fa4
  results$planet_5_first_appearance[r]    <- fa5
  results$planet_6_first_appearance[r]    <- fa6
  results$planet_7_first_appearance[r]    <- fa7
  
  results$planet_2_extinction[r]          <- planet_2_extinction
  results$planet_3_extinction[r]          <- planet_3_extinction
  results$planet_4_extinction[r]          <- planet_4_extinction
  results$planet_5_extinction[r]          <- planet_5_extinction
  results$planet_6_extinction[r]          <- planet_6_extinction
  results$planet_7_extinction[r]          <- planet_7_extinction
  
  results$planet_2_colonized[r]           <- planet_2_colonized
  results$planet_3_colonized[r]           <- planet_3_colonized
  results$planet_4_colonized[r]           <- planet_4_colonized
  results$planet_5_colonized[r]           <- planet_5_colonized
  results$planet_6_colonized[r]           <- planet_6_colonized
  results$planet_7_colonized[r]           <- planet_7_colonized
  
  results$planet_2_colonization_time[r]   <- planet_2_colonization_time
  results$planet_3_colonization_time[r]   <- planet_3_colonization_time
  results$planet_4_colonization_time[r]   <- planet_4_colonization_time
  results$planet_5_colonization_time[r]   <- planet_5_colonization_time
  results$planet_6_colonization_time[r]   <- planet_6_colonization_time
  results$planet_7_colonization_time[r]   <- planet_7_colonization_time
  
  results$planet_2_colonization_events[r] <- planet_2_colonization_events
  results$planet_3_colonization_events[r] <- planet_3_colonization_events
  results$planet_4_colonization_events[r] <- planet_4_colonization_events
  results$planet_5_colonization_events[r] <- planet_5_colonization_events
  results$planet_6_colonization_events[r] <- planet_6_colonization_events
  results$planet_7_colonization_events[r] <- planet_7_colonization_events
  
  results$planet_2_persistence[r] <- persistence[planet_index["planet_2"]]
  results$planet_3_persistence[r] <- persistence[planet_index["planet_3"]]
  results$planet_4_persistence[r] <- persistence[planet_index["planet_4"]]
  results$planet_5_persistence[r] <- persistence[planet_index["planet_5"]]
  results$planet_6_persistence[r] <- persistence[planet_index["planet_6"]]
  results$planet_7_persistence[r] <- persistence[planet_index["planet_7"]]
  results$system_mean_persistence[r] <- mean(persistence[planets$habitable])
}