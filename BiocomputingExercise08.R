# Biocomputing Exercise 8 - Fall 2020

# Challenge 1:
# generate a matrix or data frame with a cumulative score for each 
# team whenever either team scores

# read in data and separate UW and MSU data
sportData <- read.table(file = "UWvMSU_1-22-13.txt", header = TRUE, sep = "")
UW_scores <- subset(x = sportData, sportData$team == "UW")
MSU_scores <- subset(x = sportData, sportData$team == "MSU")
# calculate cumulative scores for each team using a for loop
UW_scores <- UW_scores[order(UW_scores$time) , ]
MSU_scores <- MSU_scores[order(MSU_scores$time) , ]
head(UW_scores)
head(MSU_scores)
cUW = 0
for(i in 1:length(UW_scores$score)){
  cUW[i] = sum(UW_scores$score[1:i])
}
cMSU = 0
for(i in 1:length(MSU_scores$score)){
  cMSU[i] = sum(MSU_scores$score[1:i])
}
# plot cumulative scores for both teams over time, MSU (blue line)
# UW (red line)
plot(x = MSU_scores$time, y = cMSU, type = 'l', col = 'blue')
lines(x = UW_scores$time, y = cUW, type = 'l', col = 'red')


# Challenge 2:
# code to play random number guessing game in R

# Start the function and name your game:
numberGame <- function(x) {
# Here you can adjust the number range, by default it's 1 - 100
  x <- sample(1:100, 1)
# This part will be the opening message once you start the game:
  prompt <- paste0(
    "Can you guess what number I'm thinking of? ",
    "It's a number between 1 and 100")
  cat(prompt, ".\n\n", sep="")
# This is the "meat" of the code; it allows the user to type in a number
# computer replies “Lower” if the number it generated is lower than the 
# guess, “Higher” if the random number is higher, and “You are correct!” 
# if the guess is correct. The player can continue guessing up to 10 times.
  maxAttempts <- 1
  guess <- readline("Make your guess: ")
  while (maxAttempts < 10) {
    if (guess < x) {
      print(paste0("Higher."))
      m <- readline("Try again: ")
      guess <- as.numeric(m)
      maxAttempts = maxAttempts + 1
    } else if (guess > x) {
      print(paste0("Lower."))
      m <- readline("Try again: ")
      guess <- as.numeric(m)
      maxAttempts = maxAttempts + 1
    } else if (guess == x) {
      return(cat("\nYou are correct! Now go make real friends."))
      opt <- options(show.error.messages = FALSE)
      stop()
    }
  }
  print(paste0("Sorry. You've run out of attempts. The correct answer is: ", x))
}
# after running the code above, simply type this into the console to start:
numberGame()