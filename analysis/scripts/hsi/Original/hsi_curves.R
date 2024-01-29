# Functions for calculating HSI for Chinook salmon ("chnk") and steelhead ("sthd")
# and for the juvenile ("juv") and spawning ("spw") life stages
# from Maret et al. 2006

## Chinook salmon, juvenile, summer, depth
chnk_juv_d <- function(x) {
  y = if_else(x <= 0.061, 0,
              if_else(x > 0.061 & x <= 0.1219, 0.8202 * x -0.03,
                      if_else(x > 0.1219 & x <= 0.1829, 1.4764 * x -0.11,
                              if_else(x > 0.1829 & x <= 0.2438, 1.6404 * x -0.14,
                                      if_else(x > 0.2438 & x <= 0.3048, 1.8045 * x -0.18,
                                              if_else(x > 0.3048 & x <= 0.3658, 1.8045 * x -0.18,
                                                      if_else(x > 0.3658 & x <= 0.4267, 1.8045 * x -0.18,
                                                              if_else(x > 0.4267 & x <= 0.4877, 1.4764 * x -0.04,
                                                                      if_else(x > 0.4877 & x <= 0.5486, 1.3123 * x + 0.04,
                                                                              if_else(x > 0.5486 & x <= 0.6096, 3.937 * x - 1.4,
                                                                                      if_else(x > 0.6096, 1,
                                                                                              as.numeric(NA))))))))))))
  return(y)
} # end chnk_juv_d

# Chinook salmon, juvenile, summer, velocity
chnk_juv_v <- function(x) {
  y = if_else(x >= 0 & x <= 0.0305, 2.2966 * x + 0.88,
              if_else(x > 0.0305 & x <= 0.0610, 1.3123 * x + 0.91,
                      if_else(x > 0.0610 & x <= 0.0914, 0.3281 * x + 0.97,
                              if_else(x > 0.0914 & x <= 0.1219, -0.3281 * x + 1.03,
                                      if_else(x > 0.1219 & x <= 0.1524, -1.3123 * x + 1.15,
                                              if_else(x > 0.1524 & x <= 0.3962, -2.9364 * x + 1.4205,
                                                      if_else(x > 0.3962 & x <= 0.4572, -1.8045 * x + 0.9733,
                                                              if_else(x > 0.4572 & x <= 0.5182, -1.3123 * x + 0.75,
                                                                      if_else(x > 0.5182 & x <= 0.5791, -0.6562 * x + 0.41,
                                                                              if_else(x > 0.5791 & x <= 0.6401, -0.3281 * x + 0.22,
                                                                                      if_else(x > 0.6401 & x <= 0.6706, 0.01,
                                                                                              if_else(x > 0.6706 & x <= 0.7010, -0.3281 * x + 0.23,
                                                                                                      if_else(x > 0.7010, 0,
                                                                                                             as.numeric(NA))))))))))))))
  return(y)
} # end chnk_juv_v

# Chinook salmon, spawning, depth
chnk_spw_d <- function(x) {
  y = if_else(x < 0.06096, 0,
              if_else(x > 0.06096 & x <= 0.09144, 6.5617 * x - 0.4,
                      if_else(x > 0.09144 & x <= 0.17618, 2.3435 * x - 0.0143,
                              if_else(x > 0.17618 & x <= 0.24384, 5.9652 * x - 0.6546,
                                      if_else(x > 0.24384 & x <= 0.28956, 4.3745 * x - 0.2667,
                                              if_else(x > 0.28956, 1, as.numeric(NA)))))))
  return(y)
} # end chnk_spw_d

# Chinook salmon, spawning, velocity
chnk_spw_v <- function(x) {
  y = if_else(x <= 0.15240, 0,
              if_else(x > 0.15240 & x <= 0.3048, 6.5617 * x - 1,
                      if_else(x > 0.3048 & x <= 0.9144, 1,
                              if_else(x > 0.9144 & x <= 1.2192, -3.2808 * x + 4,
                                      if_else(x > 1.2192, 0,
                                              as.numeric(NA))))))
  return(y)
} # end chnk_spw_v

# Steelhead, juvenile, summer, depth
sthd_juv_d <- function(x) {
  y = if_else(x >= 0 & x <= 0.0914, 2.1872 * x,
              if_else(x > 0.0914 & x <= 0.1829, 4.9213 * x - 0.25,
                      if_else(x > 0.1829 & x <= 0.3048, 2.5427 * x + 0.185,
                              if_else(x > 0.3048 & x <= 0.3658, 0.6562 * x + 0.76,
                                      if_else(x > 0.3658, 1,
                                              as.numeric(NA))))))
  return(y)
} # end sthd_juv_d

# Steelhead, juvenile, summer, velocity
sthd_juv_v <- function(x) {
  y = if_else(x >= 0 & x <= 0.061, 2.4606 * x,
              if_else(x > 0.061 & x <= 0.0914, 24.606 * x - 1.35,
                      if_else(x > 0.0914 & x <= 0.1524, 1.1483 * x + 0.795,
                              if_else(x > 0.1524 & x <= 0.2134, 0.4921 * x + 0.895,
                                      if_else(x > 0.2134 & x <= 0.3658, 1,
                                              if_else(x > 0.3658 & x <= 0.6096, -4.101 * x + 2.5,
                                                      if_else(x > 0.6096, 0, # Updated this to be correct instead of ifelse = 1
                                                              as.numeric(NA))))))))
  return(y)
} # end sthd_juv_v



# Create a sequence of x values
x_values <- seq(0, 2, length.out = 10000)

# Apply the sthd_win_v function to the x values
y_values <- sthd_juv_v(x_values)

# Create a data frame for plotting
df <- data.frame(x = x_values, y = y_values)

# Plot the function using ggplot2
ggplot(df, aes(x, y)) +
  geom_line() +
  labs(title = "Visualization of chnk_win_d Function",
       x = "x values",
       y = "y values")


# Steelhead, spawning, depth
sthd_spw_d <- function(x) {
  y = if_else(x <= 0.061, 0,
              if_else(x > 0.061 & x <= 0.0914, 6.5617 * x - 0.4,
                      if_else(x > 0.0914 & x <= 0.1768, 2.3435 * x + -0.0143,
                              if_else(x > 0.1768 & x <= 0.2438, 5.9652 * x - 0.6545,
                                      if_else(x > 0.2438 & x <= 0.2896, 4.3745 * x - 0.2667,
                                              if_else(x > 0.2896, 1,
                                                      as.numeric(NA)))))))
  return(y)
} # end sthd_spw_d

# Steelhead, spawning, velocity
sthd_spw_v <- function(x) {
  y = if_else(x <= 0.1524, 0,
              if_else(x > 0.1524 & x <= 0.3048, 6.5617 * x - 1,
                      if_else(x > 0.3048 & x <= 0.9144, 1,
                              if_else(x > 0.9144 & x <= 1.2192, -3.2808 * x + 4,
                                      if_else(x < 1.2192, 0,
                                              as.numeric(NA))))))
  return(y)
} # end sthd_spw_v

#####################################################################################
#
# The juvenile winter models below are just modified summer models.
# Primarily, slower velocity water = 1 suitability and shallower depths have a slight
# bump as well. The modifications are based off similar modifications done for
# a HSA done in the Tucannon River. The modification and implementation of the winter model
# was done at the request of the client.
#
###################################################################################

## Chinook salmon, winter, juvenile, depth
chnk_win_d <- function(x) {
  y <- ifelse(x <= 0.06096, 0 + (x - 0) / (0.06096 - 0) * (0.3 - 0),
              ifelse(x <= 0.32004, 0.3 + (x - 0.06096) / (0.32004 - 0.06096) * (0.3 - 0.3),
                     ifelse(x <= 0.50292, 0.3 + (x - 0.32004) / (0.50292 - 0.32004) * (0.85 - 0.3),
                            ifelse(x <= 0.62484, 0.85 + (x - 0.50292) / (0.62484 - 0.50292) * (0.95 - 0.85),
                                   ifelse(x <= 0.74676, 0.95 + (x - 0.62484) / (0.74676 - 0.62484) * (1 - 0.95),
                                          ifelse(x <= 3.048, 1 + (x - 0.74676) / (3.048 - 0.74676) * (1 - 1),
                                                 NA
                                          )
                                   )
                            )
                     )
              ))
              return(y)
}

# Create a sequence of x values
x_values <- seq(0, 2, length.out = 10000)

# Apply the sthd_win_v function to the x values
y_values <- chnk_win_d(x_values)

# Create a data frame for plotting
df <- data.frame(x = x_values, y = y_values)

# Plot the function using ggplot2
ggplot(df, aes(x, y)) +
  geom_line() +
  labs(title = "Visualization of chnk_win_d Function",
       x = "x values",
       y = "y values")

# Chinook salmon, winter, juvenile, velocity
chnk_win_v <- function(x) {
  y <- ifelse(x <= 0.32004, 1 + (x - 0) / (0.32004 - 0) * (1 - 1),
              ifelse(x <= 0.56388, 1 + (x - 0.32004) / (0.56388 - 0.32004) * (0.45 - 1),
                     ifelse(x <= 1.11252, 0.45 + (x - 0.56388) / (1.11252 - 0.56388) * (0 - 0.45),
                            ifelse(x <= 3.048, 0 + (x - 1.11252) / (3.048 - 1.11252) * (0 - 0),
                                   NA
                            )
                     )
              ))
              return(y)
}


# Create a sequence of x values
x_values <- seq(0, 2, length.out = 10000)

# Apply the sthd_win_v function to the x values
y_values <- chnk_win_v(x_values)

# Create a data frame for plotting
df <- data.frame(x = x_values, y = y_values)

# Plot the function using ggplot2
ggplot(df, aes(x, y)) +
  geom_line() +
  labs(title = "Visualization of chnk_win_v Function",
       x = "x values",
       y = "y values")

# Steelhead, winter, juvenile, depth
sthd_win_d <- function(x) {
  y <- ifelse(x <= 0.06096, 0 + (x - 0) / (0.06096 - 0) * (0.1 - 0),
              ifelse(x <= 0.19812, 0.1 + (x - 0.06096) / (0.19812 - 0.06096) * (0.1 - 0.1),
                     ifelse(x <= 0.41148, 0.1 + (x - 0.19812) / (0.41148 - 0.19812) * (0.63 - 0.1),
                            ifelse(x <= 0.80772, 0.63 + (x - 0.41148) / (0.80772 - 0.41148) * (1 - 0.63),
                                   ifelse(x <= 3.048, 1 + (x - 0.80772) / (3.048 - 0.80772) * (1 - 1),
                                          NA
                                   )
                            )
                     )
              ))
              return(y)
}

# Create a sequence of x values
x_values <- seq(0, 2.5, length.out = 10000)

# Apply the sthd_win_v function to the x values
y_values <- sthd_win_d(x_values)

# Create a data frame for plotting
df <- data.frame(x = x_values, y = y_values)

# Plot the function using ggplot2
ggplot(df, aes(x, y)) +
  geom_line() +
  labs(title = "Visualization of sthd_win_d Function",
       x = "x values",
       y = "y values")

sthd_win_v <- function(x) {
  y <- ifelse(x <= 0.2286, 1,
              ifelse(x > 0.2286 & x <= 0.28956, 1,
                     ifelse(x > 0.28956 & x <= 0.35052, 0.87 + (x - 0.35052) / (0.28956 - 0.35052) * (1 - 0.87),
                            ifelse(x > 0.35052 & x <= 0.47244, 0.78 + (x - 0.47244) / (0.35052 - 0.47244) * (0.87 - 0.78),
                                   ifelse(x > 0.47244 & x <= 0.56388, 0.54 + (x - 0.56388) / (0.47244 - 0.56388) * (0.78 - 0.54),
                                          ifelse(x > 0.56388 & x <= 0.96012, 0.3 + (x - 0.96012) / (0.56388 - 0.96012) * (0.54 - 0.3),
                                                 ifelse(x > 0.96012 & x <= 1.17348, 0.07 + (x - 1.17348) / (0.96012 - 1.17348) * (0.3 - 0.07),
                                                        ifelse(x > 1.17348 & x <= 1.524, 0 + (x - 1.524) / (1.17348 - 1.524) * (0.07 - 0),
                                                               ifelse(x > 1.524 & x <= 3.048, 0 + (x - 3.048) / (1.524 - 3.048) * (0 - 0),
                                                                      NA
                                                               )
                                                        )
                                                 )
                                          )
                                   )
                            )
                     )
              ))
              return(y)
}

# Create a sequence of x values
x_values <- seq(0, 2, length.out = 10000)

# Apply the sthd_win_v function to the x values
y_values <- sthd_win_v(x_values)

# Create a data frame for plotting
df <- data.frame(x = x_values, y = y_values)

# Plot the function using ggplot2
ggplot(df, aes(x, y)) +
  geom_line() +
  labs(title = "Visualization of sthd_win_v Function",
       x = "x values",
       y = "y values")

## END hsi_curves.R
