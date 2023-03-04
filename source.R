
# requred packages
if (!require(report))
  install.packages("report")
if (!require(ggcorrplot))
  install.packages("ggcorrplot")
if (!require(plotly))
  install.packages("plotly")
if (!require(multcomp))
  install.packages("multcomp")
library(multcomp)
library(report)
library(ggcorrplot)
library(plotly)

## DATA HANDLING
Arrhythmia <- read.csv("data/Arrhythmia.csv", sep = ";", fileEncoding="latin1")
Arrhythmia$label <- ifelse(Arrhythmia$label > 1, 1, 0)
Arrhythmia$Sex <- ifelse(Arrhythmia$Sex == 0, "Male", "Female")
Arrhythmia <- Arrhythmia[-c(111, 231),]
Arrhythmia <- Arrhythmia[, c(280, 15, 1:9)]
# a <- which(Arrhythmia$Sex == "Male" & Arrhythmia$Age %in% c(57,58,59))
# a <- Arrhythmia[a,]
# mean(a[-10,]$Weight)
Arrhythmia[301, 6] <- 70
Arrhythmia$Sex <- as.factor(Arrhythmia$Sex)
Arrhythmia$label <- as.factor(Arrhythmia$label)

# add BMI and BMI groups
Arrhythmia$BMI <-
  round(Arrhythmia$Weight / (((Arrhythmia$Height) / 100) ^ 2), 2)
Arrhythmia$BMIGroups <-
  ifelse(
    Arrhythmia$BMI < 18.5,
    "Underweight",
    ifelse(
      Arrhythmia$BMI <= 24.9,
      "Normal Weight",
      ifelse(
        Arrhythmia$BMI <= 29.9,
        "Pre-obesity",
        ifelse(
          Arrhythmia$BMI <= 34.9,
          "Obesity Class I",
          ifelse(
            Arrhythmia$BMI <= 39.9,
            "Obesity Class II",
            ifelse(Arrhythmia$BMI > 40, "Obesity Class III", "Undefined")
          )
        )
      )
    )
  )
Arrhythmia$BMIGroups <- as.factor(Arrhythmia$BMIGroups)

# data only continuous vars
Arrhythmia2 <- Arrhythmia[, -c(1, 4, 13)]

# add AGE groups
Arrhythmia$AgeGroups <- Arrhythmia$Age
Arrhythmia$AgeGroups <-
  ifelse(
    Arrhythmia$AgeGroups < 19,
    "Age 18 and below",
    ifelse(
      Arrhythmia$Age <= 29,
      "Age 19-29",
      ifelse(
        Arrhythmia$Age <= 39,
        "Age 30-39",
        ifelse(
          Arrhythmia$Age <= 49,
          "Age 40-49",
          ifelse(
            Arrhythmia$Age <= 59,
            "Age 50-59",
            ifelse(
              Arrhythmia$Age <= 69,
              "Age 60-69",
              ifelse(Arrhythmia$Age >= 70, "Age 70 and Above", "Undefined")
            )
          )
        )
      )
    )
  )
Arrhythmia$AgeGroups <- as.factor(Arrhythmia$AgeGroups)

# data for visualization
Arr <- Arrhythmia
Arr$label <- ifelse(Arr$label == 0, "Normal", "Arrhythmia")
Arr$label <- as.factor(Arr$label)
Arr$Sex <- as.factor(Arr$Sex)

# data divided by label
Arr_normal <- Arr[which(Arr$label == "Normal"),]
Arr_arr <- Arr[which(Arr$label == "Arrhythmia"),]

Arrhythmia3 <- Arrhythmia[, -c(2, 3, 6)]

Arrhythmia3$Age <- c(scale(Arrhythmia$Age))
Arrhythmia3$Weight <- c(scale(Arrhythmia$Weight))
Arrhythmia3$Heart.rate <- c(scale(Arrhythmia$Heart.rate))


model_prediction <-
  glm(
    label ~ Age + Sex + Weight + Heart.rate + QRSDuration + T.interval +
      P.interval + Age:Weight + Age:Heart.rate,
    family = binomial(link = "logit"),
    data = Arrhythmia3
  )