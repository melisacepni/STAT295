# Recitation 1

# Question 1
# Part A
seq(from=40, to=200, by=20)
# Part B
seq(10, -32, -6)
# Part C
rep(seq(-2, 3, 1)*1, each=3)
# Part D
letters[seq(2, length(letters), 2)]

# Question 2 
# Part A
A <- matrix(rep(c(2,3,2),3), nrow = 3, byrow = TRUE)
B <- matrix(c(4,-2,-4,2,2,1,-4,5,2,6,4,6), nrow = 4, byrow = TRUE)
C <- matrix(c(3,-2,2,4,-3,5,5,-4,6), nrow = 3, byrow = TRUE)
D <- diag(2:4)
# Part B 
A + D

solve(D) %*% C

A %*% t(C)

max(c(det(A), det(C), det(D)))
# Part C
A[2,1] <- -1
A[3,2] <- 1
B[2,1] <- -1
B[3,2] <- 1
C[2,1] <- -1
C[3,2] <- 1

# Another way:

# replace_matrix <- function(matrix) {
#   matrix[2,1] <- -1
#   matrix[3,2] <- 1
#   return(matrix)
# }
# 
# A <- replace_matrix(A)
# B <- replace_matrix(B)
# C <- replace_matrix(C)

M <- rbind(A,B)
dim(M)

L <- rbind(C,D)
dim(L)

rownames(L) <- paste("row.", 1:nrow(L), sep = "")
colnames(L) <- paste("col.", 1:ncol(L), sep = "")

part_4_list <- list(B,C,D)
part_4_list$favorite_films <- c("Stalker", "The Shining", "Kış Uykusu")

solve(A) %*% C + A %*% t(D)

part_6_array <- array(c(A,C), dim = c(nrow(A), ncol(A),2))

# Question 3 
set.seed(16)

values <- c()

while (TRUE) {
  x <- sample(1:10, 1)
  values <- c(values, x)
  print(x)
  if(x == 7) {
    break
  }
}

#create table with these values
value_counts <- table(values)
value_counts

#Plots
hist(values, breaks = seq(0.5, 10.5, by = 1),
     main = "Histogram of Generated Values",
     xlab = "Values", ylab = "Frequency", col = "darkblue", border = "red")

# Question 4
zeromat <- matrix(0, nrow = 6, ncol = 6)

#nested for loop
for (i in 1:dim(zeromat)[1]) {
  for (j in 1:dim(zeromat)[2]) {
    zeromat[i,j] = i*j
  }
}
zeromat

# Question 5 
matrix_size <- 4
q5 <- matrix(0, nrow = matrix_size, ncol = matrix_size)

#nested for loop
for (i in 1:matrix_size) {
  for (j in 1: matrix_size) {
    if (i == j) {
      q5[i,j] <- 0
    } else {
      q5[i,j] <- abs(i - j)
    }
  }
}
q5

# Question 6 
currency_convertor <- function(TL) {
  currency_vector <- c(TL = 1, USD = 0.033, CAD = 0.044, EURO = 0.030, CHF = 0.029)
  result_matrix <- matrix(currency_vector * TL,
                          dimnames = list(c(names(currency_vector)),"Amount"))
  return(result_matrix)
}
currency_convertor(8000)


calculate_mortgage_payment <- function(principal, interest_rate, number_of_payments) {
  #Convert annual interest rate to monthly interest rate
  monthly_interest_rate <- interest_rate / 12 / 100 #convert to monthly rate and also take decimal 
  
  #Apply the formula
  monthly_payment <- principal * (monthly_interest_rate * (1 + monthly_interest_rate) ^ number_of_payments) / ((1 + monthly_interest_rate) ^ number_of_payments - 1)
  
  return(monthly_payment)
}

#Test it 
principal <- 200000 #  US dollars
interest_rate <- 4.5 # based on the USA interest rate
number_of_payments <- 30 # loan term in years

monthly_payment <- calculate_mortgage_payment(principal, interest_rate, number_of_payments)
print(paste("Monthly mortgage payment: $", round(monthly_payment, 2)))


calculate_daily_calories <- function(age, gender, weight_kg, height_cm, activity_level) {
  
  #define activity levels
  activity_levels <- c("sedentary" = 1.2, "lightly active" = 1.37, "moderately active" =
                         1.55, "very_active" = 1.72, "extra active" = 1.9)
  #Convert height from cm to meters
  height_m <- height_cm / 100
  #Calculate Basal Metabolic Rate by using the Mifflin - St Jeor Equation
  if (gender == "male") {
    bmr <- 10 * weight_kg + 6.25 * height_cm - 5 * age + 5
  } else if (gender == "female") {
    bmr <- 10 * weight_kg + 6.25 * height_cm - 5 * age -161
  } else {
    stop("Invalid category. Please specify 'female' or 'male'.")
  }
  #Calculate Total Daily Energy Expenditure
  total_dee <- bmr * activity_levels[activity_level]
  return(total_dee)
}

#test the function
age <- 30
gender <- "male"
weight <- 70
height <- 175
activity <- "moderately active"

calories <- calculate_daily_calories(age, gender, weight, height, activity)
print(paste("Estimated daily calorie needs:", round(calories, 2), "calories"))

# Question 7
data <- read.csv("https://users.ssc.wisc.edu/~hemken/Rworkshops/read/classm.csv")

str(data)

head(data, 4)

tail(data, 2)

sum(is.na(data))

fill_na_with_mean <- function(fill_column){
  mean_val <- mean(fill_column, na.rm = TRUE)
  fill_column[is.na(fill_column)] <- mean_val
  return(fill_column)
}
#using lapply
data_fill <- as.data.frame(lapply(data,fill_na_with_mean))

#using loops
# for (col in names(data)) {
#   data[[col]] <- fill_na_with_mean(data[[col]])
# }

data_fill$Height <- data_fill$Height * 0.0254 #change from inch to meter. (1 inch = 0.0254 meters)
data_fill$BMI <- (data_fill$Weight) / ((data_fill$Height)^2)

data_fill$BMI_class <- ifelse(data_fill$BMI >= 30 & data_fill$BMI <= 34.9,
                              "Obesity Class 1",
                              ifelse(data_fill$BMI >= 35 & data_fill$BMI <= 39.9,
                                     "Obesity Class 2",
                                     ifelse(data_fill$BMI >= 40,
                                            "Obesity Class 3",
                                            "Not Classified")))

# Question 8
install.packages("babynames")
library(babynames)

dim(babynames)

summary(babynames)

colnames(babynames) <- c("Year", "Sex", "Name", "Count", "Proportion")

grouped_q6 <- aggregate(Count ~ Name + Sex, data = babynames, FUN = sum)
#sort data by Total in descending order
sorted_names <- grouped_q6[order(-grouped_q6$Count), ]
#convert to data frame to visualize better
sorted_names <- data.frame(sorted_names)
#take the top 20 baby names
top_20 <- head(sorted_names, 20)

#Plot
#first create colors by gender
selected_colors <- ifelse(top_20$Sex == "F", "brown1", "skyblue")
#bar plot
barplot(top_20$Count, names.arg = paste(top_20$Name, top_20$Sex),
        xlab = "Baby Names", ylab = "Total Count", las = 2, col = selected_colors,
        main = "Top 20 Baby Names and Counts by Gender", cex.names = 0.7)
#add a legend
legend("topright", legend = c("Male", "Female"), fill = c("skyblue", "brown1"), title = "Gender")

