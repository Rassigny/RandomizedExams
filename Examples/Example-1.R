source('../Randomized_Quizzes.R',  chdir =FALSE)

questionID <- "Example-1-OUTPUT.txt"
questionString <- "Let's assume that the distribution of gas price on a given day in a town is normally <br>
distributed with mean of $%s with a standard deviation of $%s. What proportion <br>
of gas stations are charging less than $%s?<br>"
poolOfQuestions <- c()
answerVector <- c()

questionString <- gsub("[\r\n]", "", questionString)

# Function to create answer format 


for (questionNr in seq(1, No.Of.Questions, by=1 )){
  
  # Generate random values
  mean <- roundUP(runif(1, min = 3, max = 6)) 
  SD <- roundUP(runif(1, min = 0.2, max = 1.0))
  deviation <- runif(1, min = 0.1, max = 0.5)
  val <- roundUP(mean - deviation*2)

  question.Text <- sprintf(questionString, mean, SD, val)

  # Correct answer is the first one element of answer vector.

  answerVector[1] <- paste(roundUP(pnorm(val,mean, SD) * 100), "%", sep = "")

  # All other wrong answers.
  answerVector[2] <- paste(roundUP((pnorm(val,mean, SD) * 100)/ 2), "%", sep = "")
  answerVector[3] <- paste(roundUP((1 - pnorm(val, mean, SD))* 100), "%", sep = "")
  answerVector[4] <-paste(roundUP(pnorm(val, mean, SD)* 100 *2 ), "%", sep = "")
  answerVector[5] <- paste(0, "%", sep = "")
  
  # Use RandomAndAddToPool function to generate txt question pool file. 
  # The last parameter 1 , means here that this is a multiple choice question and not a multiple answer. 
  poolOfQuestions<-randomAndAddToPool(answerVector, question.Text, poolOfQuestions, 1)
  
}

writeItToFile(questionID, poolOfQuestions)