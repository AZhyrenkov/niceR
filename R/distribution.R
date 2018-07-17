# Function to redistribute NAs within managers honestly
distribute_honestly <- function(pool){
  # @pool: two-column dataframe with user_id and mnager columns. To redistribution columns must be NA
  # basic inputs
  total_capacity <- nrow(pool)
  capacity_per_manager <- ceiling(total_capacity/(n_distinct(pool$manager)-1))
  to_redist <- nrow(pool[is.na(pool$manager),])

  # capacity dataframe
  capacity <- pool %>%
    filter(!is.na(manager)) %>%
    group_by(manager) %>%
    summarise(users = n()) %>%
    ungroup() %>%
    arrange(desc(users)) %>%
    rowwise() %>%
    mutate(to_redist = to_redist,
           capacity = max(capacity_per_manager-users,0)) %>%
    ungroup() %>%
    mutate(cumsum = cumsum(capacity)) %>%
    rowwise() %>%
    mutate(capacity = capacity+min(to_redist-cumsum,0))

  # initialize empty dataframe for results storing
  distribution_result <- data.frame(user_id = numeric(),manager=character())

  # filter out zero-capacity managers
  capacity_work <- filter(capacity, capacity > 0)

  # Rearrange pool randomly (uniform distribution)
  randomized_pool <- pool %>%
    filter(is.na(manager)) %>%
    rowwise() %>%
    mutate(rand = runif(1)) %>%
    arrange(rand) %>%
    select(-rand)

  for(i in 1:nrow(capacity_work)){
    # get a name of manager with free capacity
    managerTo = capacity_work[i, ]$manager

    # get a number of users to reassign to the aforementioned manager
    users = capacity_work[i, ]$capacity

    # slice the required number of rows
    sampled <- slice(randomized_pool, 1:users)

    # add manager's name into the resulting set
    sampled <- mutate(sampled, manager=rep(managerTo, users))

    # filter the resulting set out of the distribution dataset
    randomized_pool <- filter(randomized_pool,
                              !(user_id %in% sampled$user_id))

    # unite result and sliced data
    distribution_result <- bind_rows(distribution_result, sampled)
  }
  distribution_result <- pool %>%
    filter(!is.na(manager)) %>%
    bind_rows(.,distribution_result)

  return(list(
    total_capacity = total_capacity,
    capacity_per_manager = capacity_per_manager,
    to_redist = to_redist,
    distribution_result = distribution_result,
    capacity = capacity))
}

# Distribution with capacity
distribute <- function(vector_pool, capacity){
  # @vector_pool: vector with users ti distribute
  # @capacity: two-column dataset with manager and capacity columns

  # initialize empty dataframe for results storing
  distribution_result <- data.frame(user_id = numeric(),manager=character())

  # filter out zero-capacity managers
  capacity_work <- filter(capacity, capacity > 0)

  # Rearrange pool randomly (uniform distribution)
  randomized_pool <- tibble(user_id = unique(vector_pool)) %>%
    rowwise() %>%
    mutate(rand = runif(1)) %>%
    arrange(rand) %>%
    select(-rand)

  for(i in 1:nrow(capacity_work)){
    # get a name of manager with free capacity
    managerTo = capacity_work[i, ]$manager

    # get a number of users to reassign to the aforementioned manager
    users = capacity_work[i, ]$capacity

    # slice the required number of rows
    sampled <- slice(randomized_pool, 1:users)

    # add manager's name into the resulting set
    sampled <- mutate(sampled, manager=rep(managerTo, users))

    # filter the resulting set out of the distribution dataset
    randomized_pool <- filter(randomized_pool,
                              !(user_id %in% sampled$user_id))

    # unite result and sliced data
    distribution_result <- bind_rows(distribution_result, sampled)
  }

  return(distribution_result)
}
