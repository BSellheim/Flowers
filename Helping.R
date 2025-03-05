library(ggplot2)
library(tidyverse)
library(corrplot)
library(ggcorrplot)
library(GGally)
library(stats)
# a
data <- read_csv("StimulusPackage.csv")  
subset_data <- data[, c("Revenue", "Stimulus", "Employees")]
cor_matrix <- cor(subset_data, use = "complete.obs", method = "pearson")
corrplot(cor_matrix, method = "color", type = "lower", tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("blue", "white", "red"))(200))
# b
print(cor_matrix)
# c
mean_vector <- colMeans(subset_data, na.rm = TRUE)
cov_matrix <- cov(subset_data, use = "complete.obs")
mahal_dist <- mahalanobis(subset_data, center = mean_vector, cov = cov_matrix)
threshold <- quantile(mahal_dist, 0.90)  
outliers <- data[mahal_dist > threshold, ]
print(n = 30, outliers)  
# d

# Convert 'Period' to a factor with appropriate labels
data$Period <- factor(data$Period, levels = c(1, 2), labels = c("Pre-COVID", "Post-COVID"))

ggplot(data, aes(x = Period, y = Revenue, fill = factor(State))) +  
  geom_boxplot(outlier.color = "purple", outlier.shape = 16, outlier.size = 3) +  
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 4, color = "black") +  
  labs(title = "Revenue Comparison: MN vs ND and Pre-COVID vs Post-COVID",
       x = "COVID19 Period", y = "Revenue (USD)",
       fill = "State") +
  scale_fill_manual(values = c("1" = "red", "2" = "darkorange"), 
                    labels = c("Minnesota", "North Dakota")) +  
  facet_wrap(~ State) +  
  theme_minimal() +
  theme(axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 14))
