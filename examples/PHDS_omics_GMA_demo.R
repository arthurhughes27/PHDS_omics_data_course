library(ggplot2)
library(dplyr)
library(here)

# Figures are saved to <project root>/figures (the folder is created if needed)
figure_directory = here("output", "figures")
dir.create(figure_directory, showWarnings = FALSE)

# Plot 1 : motivation - disease subtypes by severity and number of symptoms

# Simulate very clearly separated symptom data
set.seed(123)

n <- 20  # points per cluster

# Cluster 1: high symptoms, low severity
c1 <- data.frame(
  symptoms = round(rnorm(n, mean = 9, sd = 1), digits = 0),
  severity = rnorm(n, mean = 1, sd = 0.3)
)

# Cluster 2: high symptoms, high severity
c2 <- data.frame(
  symptoms = round(rnorm(n, mean = 9, sd = 1), digits = 0),
  severity = rnorm(n, mean = 9, sd = 0.3)
)

# Cluster 3: low symptoms, high severity
c3 <- data.frame(
  symptoms = round(rnorm(n, mean = 1, sd = 1), digits = 0),
  severity = rnorm(n, mean = 9, sd = 0.3)
)

# Combine clusters and add cluster label
symptom_data <- rbind(c1, c2, c3)
symptom_data$cluster <- factor(rep(1:3, each = n))

# Clip values to 0-10 range
symptom_data$symptoms <- pmax(pmin(symptom_data$symptoms, 10), 0)
symptom_data$severity <- pmax(pmin(symptom_data$severity, 10), 0)

# ---- Plot 1: points uncolored ----
p1 <- ggplot(symptom_data, aes(x = symptoms, y = severity)) +
  geom_point(size = 3) +
  theme_minimal(base_size = 18) +
  labs(title = "Symptom data",
       x = "Number of symptoms",
       y = "Severity of symptoms") + 
  theme(plot.title = element_text(hjust = 0.5))

# ---- Plot 2: points colored by cluster ----
p2 <- ggplot(symptom_data, aes(x = symptoms, y = severity, color = cluster)) +
  geom_point(size = 3) +
  # stat_ellipse(level = 0.95, linetype = "dashed", size = 1) +
  theme_minimal(base_size = 18) +
  labs(title = "Symptom data coloured by cluster",
       x = "Number of symptoms",
       y = "Severity of symptoms") +
  scale_color_manual(values = c("1" = "blue", "2" = "red", "3" = "green")) +
  scale_x_continuous(breaks = scales::breaks_pretty()) +
  theme(plot.title = element_text(hjust = 0.5))

print(p2)

# Example of distribution of two genes

n <- 1000  # number of samples

# Gene 1: small mean, small variance
mu1 <- 5
size1 <- 10  # larger size -> lower variance
gene1 <- rnbinom(n, mu = mu1, size = size1)

# Gene 2: large mean, large variance
mu2 <- 25
size2 <- 3   # smaller size -> higher variance
gene2 <- rnbinom(n, mu = mu2, size = size2)

# Combine into a data frame for ggplot
df <- data.frame(
  counts = c(gene1, gene2),
  gene = factor(rep(c("Gene1", "Gene2"), each = n))
)

# Smoothed histograms (density plots) with transparency
p3 = ggplot(df, aes(x = counts, fill = gene)) +
  geom_density(alpha = 0.5) +   # alpha controls transparency
  scale_fill_manual(values = c("Gene1" = "blue", "Gene2" = "red")) +
  theme_minimal(base_size = 18) +
  labs(title = "Distribution of Two Genes",
       x = "Counts",
       y = "Density") +
  xlim(0, max(df$counts)) + 
  theme(plot.title = element_text(hjust = 0.5))

print(p3)

# Poisson vs negative binomial distributions

# Poisson
# Parameters
lambda_small <- 2
lambda_medium <- 10
lambda_high <- 25
n <- 1000  # number of samples per group

# Labels as strings
labels <- c(paste0("λ = ", lambda_small),
            paste0("λ = ", lambda_medium),
            paste0("λ = ", lambda_high))

# Simulate Poisson data
df <- data.frame(
  counts = c(
    rpois(n, lambda_small),
    rpois(n, lambda_medium),
    rpois(n, lambda_high)
  ),
  rate = factor(rep(labels, each = n))
)

# Plot superposed densities
p4 <- ggplot(df, aes(x = counts, fill = rate)) +
  geom_density(alpha = 0.5, bw = 2) +   # increased bandwidth for smoother curves
  scale_fill_manual(values = c("λ = 2" = "blue", 
                               "λ = 10" = "red", 
                               "λ = 25" = "green")) +
  theme_minimal(base_size = 18) +
  labs(title = "Three Poisson Distributions with Different Rates",
       x = "Counts",
       y = "Density") +
  xlim(0, max(df$counts)) + 
  theme(plot.title = element_text(hjust = 0.5))

print(p4)

# Negative binomial 

# Parameters
mu_values <- c(2, 10, 25)       # low, medium, high mean
theta_values <- c(1, 5, 20)     # low, medium, high dispersion
n <- 1000                       # samples per group

# Labels
labels <- c(
  paste0("μ = ", mu_values[1], ", θ = ", theta_values[1]),
  paste0("μ = ", mu_values[2], ", θ = ", theta_values[2]),
  paste0("μ = ", mu_values[3], ", θ = ", theta_values[3])
)

# Simulate Negative Binomial data
df_nb <- data.frame(
  counts = c(
    rnbinom(n, mu = mu_values[1], size = theta_values[1]),
    rnbinom(n, mu = mu_values[2], size = theta_values[2]),
    rnbinom(n, mu = mu_values[3], size = theta_values[3])
  ),
  parameters = factor(rep(labels, each = n))
)

# Plot superposed densities
p5 <- ggplot(df_nb, aes(x = counts, fill = parameters)) +
  geom_density(alpha = 0.5, bw = 1.5) +   # smoother curves
  scale_fill_manual(values = c("μ = 2, θ = 1" = "blue", 
                               "μ = 10, θ = 5" = "red", 
                               "μ = 25, θ = 20" = "green")) +
  theme_minimal(base_size = 18) +
  labs(title = "Negative Binomial Distributions with Different Means and Dispersions",
       x = "Counts",
       y = "Density") +
  xlim(0, max(df_nb$counts)) +
  theme(plot.title = element_text(hjust = 0.5))

print(p5)


# Display plots
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)

ggsave(
  plot = p1,
  path = figure_directory,
  filename = "clustering_demo1.png",
  units = "cm",
  width =  25,
  height = 15
)

ggsave(
  plot = p2,
  path = figure_directory,
  filename = "clustering_demo2.png",
  units = "cm",
  width =  25,
  height = 15
)

ggsave(
  plot = p3,
  path = figure_directory,
  filename = "dge_demo1.png",
  units = "cm",
  width =  25,
  height = 15
)

ggsave(
  plot = p4,
  path = figure_directory,
  filename = "dge_demo2.png",
  units = "cm",
  width =  25,
  height = 15
)

ggsave(
  plot = p5,
  path = figure_directory,
  filename = "dge_demo3.png",
  units = "cm",
  width =  35,
  height = 15
)