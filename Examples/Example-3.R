
source('../Randomized_Quizzes.R',  chdir =FALSE)

questionID <- "Example-3-OUTPUT.txt"
questionString <- "Calculate the correlation coefficient for the following data: <br>
<table  border=\"1\">
<tr><th>x</th><th>y</th></tr>
<tr><td>%s</td><td>%s</td></tr>
<tr><td>%s</td><td>%s</td></tr>
<tr><td>%s</td><td>%s</td></tr>
<tr><td>%s</td><td>%s</td></tr>
</table>"
poolOfQuestions <- c()
answerVector <- c()

questionString <- gsub("[\r\n]", "", questionString)
for (questionNr in seq(1, No.Of.Questions, by=1 )){
  
  # Generate Data 
  x <- sample(1:30, 4, replace=TRUE)
  y <- sample(1:30, 4, replace=TRUE)
  
  question.Text <- sprintf(questionString, x[1], y[1], x[2], y[2], x[3], y[3], x[4], y[4])
  
  
  # Correct answer is the first one here.
  
  answerVector[1] <- roundUP(cor(x,y))
  
  
  # Vector for answers

  # All other wrong answers.
  answerVector[2] <- roundUP(-cor(x,y))
  answerVector[3] <- roundUP((cor(x,y) + 1))
  answerVector[4] <- roundUP(-(cor(x,y) - 1))

  poolOfQuestions<-randomAndAddToPool(answerVector, question.Text, poolOfQuestions, 1)
  
}

writeItToFile(questionID, poolOfQuestions)
