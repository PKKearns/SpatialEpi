
library(tidymodels)

set.seed(27)

#### Nest - Map - Unnest

centers <- tibble(
  cluster = factor(1:3), 
  num_points = c(100, 150, 50),  # number points in each cluster
  x1 = c(7, 0.5, -3),              # x1 coordinate of cluster center
  x2 = c(-2, 2, -2)              # x2 coordinate of cluster center
)



labelled_points <- 
  centers %>%
  mutate(
    x1 = map2(num_points, x1, rnorm),
    x2 = map2(num_points, x2, rnorm)
  ) %>% 
  dplyr::select(-num_points) %>% 
  unnest(cols = c(x1, x2))



ggplot(labelled_points, aes(x1, x2, color = cluster)) +
  geom_point(alpha = 0.3)


points <- 
  labelled_points %>% 
  dplyr::select(-cluster)

kclust <- kmeans(points, centers = 3)
kclust
summary(kclust)

augment(kclust, points)
tidy(kclust)
glance(kclust)





#### Explore what happens when you choose different clusters

kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = purrr::map(k, ~kmeans(points, .x)),
    tidied = purrr::map(kclust, tidy),
    glanced = purrr::map(kclust, glance),
    augmented = purrr::map(kclust, augment, points)
  )

clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))


p1 <- 
  ggplot(assignments, aes(x = x1, y = x2)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1


p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2


ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()

