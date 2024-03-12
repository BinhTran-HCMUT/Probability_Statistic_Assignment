---
title: "Computer hardware dataset - Activity 2"
output: html_notebook
---

```{r}
library(dplyr)
library(ggplot2)
library(matlib)
library(qqplotr)
```

## Data import

```{r}
original_data <- read.csv("Computer hardware.data", header = FALSE)
names(original_data) <- c("vendor name", "model name", "MYCT", "MMIN", "MMAX", "CACH", "CHMIN", "CHMAX", "PRP", "ERP")
original_data
```

## Data cleaning

### Relevant variable extraction

```{r}
new_data <- original_data %>% select(3:9)
new_data
```

### Missing value check

```{r}
new_data %>% sapply(function(col) sum(is.na(col)))
```

No missing values in the data.

## Descriptive statistics & Data visualization

### Variable transformation

```{r}
new_data %>% ggplot(aes(x=MYCT)) +
             geom_histogram(binwidth = 50, color="darkblue", fill="lightblue") +
             labs(title="Histogram plot of Machine cycle time",x="Machine cycle time (ns)", y = "Frequency")
```

```{r}
new_data %>% ggplot(aes(x=MMIN)) +
             geom_histogram(color="darkred", fill="pink") +
             labs(title="Histogram plot of Minimum main memory size", x="Minimum main memory size (KB)", y = "Frequency")
```

```{r}
new_data %>% ggplot(aes(x=MMAX)) +
             geom_histogram(color="darkblue", fill="lightblue") +
             labs(title="Histogram plot of Maximum main memory size", x="Maximum main memory size (KB)", y = "Frequency")
```

```{r}
new_data %>% ggplot(aes(x=CACH)) +
             geom_histogram(color="darkred", fill="pink") +
             labs(title="Histogram plot of Cache size", x="Cache size (KB)", y = "Frequency")
```

```{r}
new_data %>% ggplot(aes(x=CHMIN)) +
             geom_histogram(color="darkblue", fill="lightblue") +
             labs(title="Histogram plot of Minimum units of channel", x="Channels (units)", y = "Frequency")
```

```{r}
new_data %>% ggplot(aes(x=CHMAX)) +
             geom_histogram(color="darkred", fill="pink") +
             labs(title="Histogram plot of Maximimum units of channel", x="Channels (units)", y = "Frequency")
```

```{r}
new_data %>% ggplot(aes(x=PRP)) +
             geom_histogram(color="darkblue", fill="lightblue") +
             labs(title="Histogram plot of Published Relative Performance", x="Relative performance", y = "Frequency")
```

The variables are all left-skewed -\> taking logarithm of the variables so that the variables are distributed more uniformly over an interval.

```{r}
new_data$MYCT <- new_data$MYCT %>% log()
new_data$MMIN <- new_data$MMIN %>% log()
new_data$MMAX <- new_data$MMAX %>% log()
new_data$CACH <- (new_data$CACH + 1) %>% log()
new_data$CHMIN <- (new_data$CHMIN + 1) %>% log()
new_data$CHMAX <- (new_data$CHMAX + 1) %>% log()
new_data$PRP <- new_data$PRP %>% log()
```

### Univariate analysis

```{r}
means <- new_data %>% sapply(mean)
medians <- new_data %>% sapply(median)
sds <- new_data %>% sapply(sd)
mins <- new_data %>% sapply(min)
maxs <- new_data %>% sapply(max)
uniques <- new_data %>% sapply(function(col) length(unique(col)))
summary <- as.data.frame(cbind(means, medians, sds, mins, maxs, uniques))
names(summary) <- c("mean", "median", "sd", "min", "max", "unique")
summary
```

```{r}
new_data %>% ggplot(aes(x=MYCT)) +
             geom_histogram(color="darkblue", fill="lightblue") +
             labs(title="Histogram plot of Machine cycle time",x="Machine cycle time (ns)", y = "Frequency")
```

```{r}
new_data %>% ggplot(aes(x=MMIN)) +
             geom_histogram(color="darkred", fill="pink") +
             labs(title="Histogram plot of Minimum main memory size", x="Minimum main memory size (KB)", y = "Frequency")
```

```{r}
new_data %>% ggplot(aes(x=MMAX)) +
             geom_histogram(color="darkblue", fill="lightblue") +
             labs(title="Histogram plot of Maximum main memory size", x="Maximum main memory size (KB)", y = "Frequency")
```

```{r}
new_data %>% ggplot(aes(x=CACH)) +
             geom_histogram(color="darkred", fill="pink") +
             labs(title="Histogram plot of Cache size", x="Cache size (KB)", y = "Frequency")
```

```{r}
new_data %>% ggplot(aes(x=CHMIN)) +
             geom_histogram(color="darkblue", fill="lightblue") +
             labs(title="Histogram plot of Minimum units of channel", x="Channels (units)", y = "Frequency")
```

```{r}
new_data %>% ggplot(aes(x=CHMAX)) +
             geom_histogram(color="darkred", fill="pink") +
             labs(title="Histogram plot of Maximimum units of channel", x="Channels (units)", y = "Frequency")
```

```{r}
new_data %>% ggplot(aes(x=PRP)) +
             geom_histogram(color="darkblue", fill="lightblue") +
             labs(title="Histogram plot of Published Relative Performance", x="Relative performance", y = "Frequency")
```

### Multivariate Analysis

```{r}
new_data %>% cov() %>% as.data.frame()
```

```{r}
new_data %>% cor() %>% as.data.frame()
```

```{r}
new_data %>% pairs()
```

## Linear Regression

### Regression coefficients estimation and standard error

```{r}
linreg <- lm(PRP ~ MYCT + MMIN + MMAX + CACH + CHMIN + CHMAX, new_data)
summary(linreg)$coefficients %>% as.data.frame() %>% select(1:2)
```

#### SSE & SSR & SST

```{r}
SSE <- (linreg$residuals ^ 2) %>% sum()
SSR <- ((linreg$fitted.values - mean(new_data$PRP)) ^ 2) %>% sum()
SST <- ((new_data$PRP - mean(new_data$PRP)) ^ 2) %>% sum()
```

```{r}
ss <- as.data.frame(c(SSE, SSR, SST), row.names = c("SSE", "SSR", "SST"))
names(ss) <- c("Value")
ss
```

#### Confidence interval of Regression Coefficients

```{r}
X <- new_data %>% select(1:6) %>% as.matrix()
X <- cbind(matrix(1, nrow = 209, ncol = 1), X)
S <- (t(X) %*% X) %>% inv() * (error.sd ^ 2)
reg.sd <- sqrt(diag(S))
reg.sd
```

```{r}
confint(linreg, level = 0.95) %>% as.data.frame()
```

#### Coefficient adequacy checking

```{r}
summary(linreg)$coefficients %>% as.data.frame() %>% select(3:4)
```

#### Model adequacy checking

```{r}
ggplot(mapping = aes(sample = linreg$residuals)) +
                         stat_qq_point(size = 2) +
                         labs(title = "Normal probability plot")
```

```{r}
shapiro.test(linreg$residuals)
```
