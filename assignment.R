library(tidyverse)
library(dplyr)
library(ggplot2)
iris <- as_tibble(iris) # so it prints a little nicer

# Question 1
iris <- rename(iris, sepal_length = Sepal.Length , sepal_width = Sepal.Width, 
       petal_length = Petal.Length, petal_width = Petal.Width, species = Species)

# Question 2
iris <- mutate(iris, sepal_length = sepal_length * 10)

# Question 3
iris <- mutate(iris, sepal_area = sepal_length * sepal_width,
               petal_area = petal_length * petal_width)
iris_selected <- select(iris, sepal_area, petal_area, species)
print(iris_selected)

# Question 4 
sepal_stats <- iris %>%
  summarise(
    sample_size = n(),
    maximum = max(sepal_length),
    minimum = min(sepal_length),
    range = maximum - minimum,
    median = median(sepal_length),
    q1 = quantile(sepal_length, 0.25),
    q3 = quantile(sepal_length, 0.75),
    interquartile_range = IQR(sepal_length)
  )
print(sepal_stats)

# Question 5
petal_width_stats <- iris %>%
  group_by(species) %>%
  summarise(
    sample_size = n(),
    mean = mean(petal_width),
    std_dev = sd(petal_width),
    variance = var(petal_width),
    std_error = std_dev / sqrt(sample_size),
    ci_95 = 1.96 * std_error  
  )
  
# Question 6
ggplot(iris, aes(x = species, y = petal_width)) +
  geom_jitter(width = 0.2, size = 1.75, color = "red") +  
  labs(
    title = "Species Related Petal Width",
    x = "Species",
    y = "Petal Width (mm)"
  ) + theme_minimal()

# Question 7
petal_grouped <- iris %>%
  group_by(species)
petal_summary <- summarize(petal_grouped, 
          mean_width = mean(petal_width),
          sem = sd(petal_width) / sqrt(n()),
          ci_upper_limit = mean_width + 1.96 * sem,
          ci_lower_limit = mean_width - 1.96 * sem)
ggplot(iris, aes(x = species, y = petal_width)) +
  geom_crossbar(data = petal_summary, 
  mapping = aes(x = species, y = mean_width, ymax = ci_upper_limit,
                ymin = ci_lower_limit), color = "black") +
  geom_jitter(width = 0.2, size = 1.75, color = "red") +  
  labs(
    title = "Species Related Petal Width",
    x = "Species",
    y = "Petal Width (mm)"
  ) + theme_minimal()

# Question 8
ggplot(data = iris) +
  geom_point(mapping = aes(x = petal_length, y = petal_length, color = species),
             alpha = 0.3) +
  labs(title = "Petal Size Compared by Species", 
       x = "Petal Length (mm)", y = "Petal Width (mm)")

