###############################################
#Global variables for Tabu Search Algorithm
###############################################
# tabu list must be global because run_intermediate_tabu_search_process, wich implements tabu search algorithm,
# run incrementally (by steps)
tabu_list <<- list() 
tabu_list_item_expiration <<- 15 
tabu_list_init <<- FALSE #mark if tabu_list was initialized
###############################################

miles_per_meter = 100 / 2.54 / 12 / 5280


if (!exists("all_cities")) all_cities = readRDS("data/cities.rds")
if (!exists("usa_cities")) usa_cities = readRDS("data/usa_cities.rds")

generate_random_cities = function(n = 10, min_dist = 250, usa_only=FALSE) {
  if (usa_only) {
    candidates = usa_cities
  } else {
    candidates = all_cities
  }

  cities = candidates[sample(nrow(candidates), 1),]
  candidates = subset(candidates, !(full.name %in% cities$full.name))
  i = 0

  while (nrow(cities) < n & i < nrow(all_cities)) {
    candidate = candidates[sample(nrow(candidates), 1),]
    candidate_dist_matrix = distm(rbind(cities, candidate)[, c("long", "lat")]) * miles_per_meter

    if (min(candidate_dist_matrix[candidate_dist_matrix > 0]) > min_dist) {
      cities = rbind(cities, candidate)
      candidates = subset(candidates, !(candidates$full.name %in% cities$full.name))
    }

    i = i + 1
  }

  cities = cities[order(cities$full.name),]
  cities$n = 1:nrow(cities)

  return(cities)
}

plot_base_map = function(map_name="world") {
  margins = c(3.5, 0, 3.5, 0)
  if (map_name == "world") {
    map("world", col="#f3f3f3", fill=TRUE, lwd=0.2, mar=margins)
  } else if (map_name == "usa") {
    map("usa", col="#f3f3f3", border=FALSE, fill=TRUE, mar=margins) #, projection="albers", parameters=c(29.5, 45.5))
    map("state", add=TRUE, col="#999999", fill=FALSE) #, projection="albers", parameters=c(29.5, 45.5))
  }
}

plot_city_map = function(cities, map_name="world", label_cities=TRUE) {
  plot_base_map(map_name)
  # TODO: maptools pointLabel() for better label placement
  map.cities(cities, pch=19, cex=1.1, label=label_cities)
}

plot_tour = function(cities, tour, great_circles, map_name="world", label_cities=TRUE) {
  plot_city_map(cities, map_name, label_cities=label_cities)

  if (length(tour) > 1) {
    closed_tour = c(tour, tour[1])
    keys = apply(embed(closed_tour, 2), 1, function(row) paste(sort(row), collapse="_"))
    invisible(sapply(great_circles[keys], lines, lwd=0.8))
  }
}

calculate_great_circles = function(cities) {
  great_circles = list()
  if (nrow(cities) == 0) return(great_circles)

  pairs = combn(cities$n, 2)

  for(i in 1:ncol(pairs)) {
    key = paste(sort(pairs[,i]), collapse="_")
    pair = subset(cities, n %in% pairs[,i])
    pts = gcIntermediate(c(pair$long[1], pair$lat[1]), c(pair$long[2], pair$lat[2]), n=25, addStartEnd=TRUE, breakAtDateLine=TRUE, sp=TRUE)

    great_circles[[key]] = pts
  }

  return(great_circles)
}

calculate_tour_distance = function(tour, distance_matrix) {
  sum(distance_matrix[embed(c(tour, tour[1]), 2)])
}

######################################################################
################# TABU LIST MANIPULATION METHODS #####################
######################################################################
# Tabu structure inspired by article from below URL:                 #  
# http://www.eng.uwaterloo.ca/~sjayaswa/projects/MSCI703_project.pdf #
######################################################################
check_if_is_tabu = function(tabu_struct, i, j, max) {
  return(tabu_struct[i, j] > 0)
}

resetTabuList = function() {
  tabu_list <<- list() 
  tabu_list_init <<- FALSE
}

setTabuExpiration = function(tabu_item_expiration) {
  tabu_list_item_expiration <<- tabu_item_expiration
}

decrement_tabu = function(tabustr) {
  dt = dim(tabustr)
  for(i in 1:dt[1]) {
    for(j in 1:dt[2]) {
      if (tabustr[i,j] > 0) {
        tabustr[i, j] <- tabustr[i, j] - 1
      }
    }
  }
  return(tabustr);
}
#########################################################################

get_neighborhood = function(current_tour) {
  items = list();
  if (length(current_tour) > 0) {
    for (i in 1:(length(current_tour) - 1)) {
      for(j in (i+1):length(current_tour)) {
        candidate = current_tour
        aux = candidate[i]
        candidate[i] = candidate[j]
        candidate[j] = aux
        #insert candidate to neighborhood list, 
        #store with the candidate i, j swap positions that generated the candidate
        items = append(items, list(list(candidate, i, j) )) #use items[[k]][[1]] to retrieve k-candidate
                                                            #use items[[k]][[2]] to retrieve the first swap position used to generate the k-candidate
                                                            #use items[[k]][[3]] to retrieve the second swap position used to generate the k-candidate
      }
    }
  }
  return(items);
}

run_tabu_intermediate_searching_process = function(cities, distance_matrix, tour, tour_distance, best_tour, best_distance,
                                                   starting_iteration, number_of_iterations) {


  n_cities = nrow(cities)
  if (!tabu_list_init) {
    tabu_list <<- matrix(0, n_cities, n_cities)
    tabu_list_init <<- TRUE
  }
  
  for (i in 1:number_of_iterations) {
      iter = starting_iteration + i
      candidates = get_neighborhood(tour)
      best_candidate_tour = list()
      best_candidate_tour_distance = Inf
      best_candidate_tour_found = FALSE  
      best_candidate_tour_i = 0
      best_candidate_tour_j = 0
      if (length(candidates) > 0) {
        for (k in 1:length(candidates)) {
          candidate_tour = candidates[[k]][[1]]
          candidate_tour_i = candidates[[k]][[2]] #first swap position used to generate the candidate
          candidate_tour_j = candidates[[k]][[3]] #second swap position used to generate the candidate
          candidate_tour_distance = calculate_tour_distance(candidate_tour, distance_matrix)
          if(!check_if_is_tabu(tabu_list, candidate_tour_i, candidate_tour_j, tabu_list_max_size) && candidate_tour_distance < best_candidate_tour_distance) {
            best_candidate_tour = candidate_tour
            best_candidate_tour_i = candidate_tour_i
            best_candidate_tour_j = candidate_tour_j
            best_candidate_tour_distance = candidate_tour_distance
            best_candidate_tour_found = TRUE
          }
        }
      }
      
      if (best_candidate_tour_found) {
        tour = best_candidate_tour
        tour_distance = best_candidate_tour_distance
        tabu_list[best_candidate_tour_i, best_candidate_tour_j] <<- tabu_list_item_expiration
        if (tour_distance < best_distance) {
          best_tour = tour
          best_distance = tour_distance
        }
      }
      
      tabu_list <<- decrement_tabu(tabu_list)
  }
  return(list(tour=tour, tour_distance=tour_distance, best_tour=best_tour, best_distance=best_distance))
}

ensure_between = function(num, min_allowed, max_allowed) {
  max(min(num, max_allowed), min_allowed)
}

seed_cities = c(
  "Buenos Aires, Argentina",
  "Sydney, Australia",
  "Rio de Janeiro, Brazil",
  "Montreal, Canada",
  "Beijing, China",
  "Moroni, Comoros",
  "Cairo, Egypt",
  "Paris, France",
  "Athens, Greece",
  "Budapest, Hungary",
  "Reykjavik, Iceland",
  "Delhi, India",
  "Baghdad, Iraq",
  "Rome, Italy",
  "Tokyo, Japan",
  "Bamako, Mali",
  "Mexico City, Mexico",
  "Kathmandu, Nepal",
  "Oslo, Norway",
  "Port Moresby, Papua New Guinea",
  "Lima, Peru",
  "Kigali, Rwanda",
  "San Marino, San Marino",
  "Singapore, Singapore",
  "Moscow, Russia",
  "Colombo, Sri Lanka",
  "Bangkok, Thailand",
  "Istanbul, Turkey",
  "London, UK",
  "New York, USA"
)
