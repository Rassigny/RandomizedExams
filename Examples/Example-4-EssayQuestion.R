# Original Question
# What is the mean of the numbers below

source('../Randomized_Quizzes.R',  chdir =FALSE)

questionID <- "Example-4.txt"

questionString <- "Describe briefly what is the main problem that <strong>  %s </strong> addresses."

Variations <- c("Stragegy Pattern", "Observer Pattern", "Factory Method Pattern", "Abstract Factory Pattern", 
               "Decorator Pattern", "Adapter Pattern", "Composite Pattern", "Singleton Pattern", "Facade Pattern", 
               "Command Pattern", "Proxy Pattern", "State Pattern",  "Template Pattern", "Mediator Pattern",  "Visitor Pattern")

# No.Of.Variations <-length(Variations)

poolOfQuestions <- c()
answerVector <- c()

for (questionPart in Variations){
  
  
  question.Text <- sprintf(questionString, questionPart)
  
  ## This is an Essay Question. There is no Answer. 
  answerVector[1] <- "Write your answer here. "
  # answerVector will not be used because this is an essay question. 
  
  
  poolOfQuestions<-randomAndAddToPool(answerVector, question.Text, poolOfQuestions, 0)
  
}


writeItToFile(questionID, poolOfQuestions)

