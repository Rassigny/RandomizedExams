# CONSTANTS First 

No.Of.Questions <- 100




# This is a simple function to swap elements of a vector 
swap <- function(x,i,j) {x[c(i,j)] <- x[c(j,i)]; x} 


# This function 
randomOrder <- function(choices, num.answers){
  lapply(choices, str)
  for (i in seq(1, length(choices), by=1)) {
    if( i %in% 1:num.answers){
      choices[i] <- paste(choices[i], "\t", "correct" , sep = "")
    }else{
      choices[i] <- paste(choices[i], "\t", "incorrect", sep = "")
    }
  }
  
  # pick a random position for the correct answer. 
  len <- length(choices) 
  answerPosition <- sample(1:len, 1)
  # randomly swap the correct answer. 
  choices<-swap(choices , 1, answerPosition)
  
  # add a tab to each of them 
  choices<-paste(choices, collapse="\t", sep = "" )
  choices<-paste("\t", choices, sep = "")
  return (choices)
} 



# roundUP to 4 digists
roundUP <- function(number){
  
  result <- round(number, 4)
  
  return (result)
}



# Write it to file

writeItToFile <- function(questionID, poolOfQuestions ){
  
  fileConn <- file(questionID)
  writeLines(poolOfQuestions, fileConn , useBytes=T)
  close(fileConn)
}


# This function generates the random question. 
# 
randomAndAddToPool<- function(answerVector, question.Text, poolOfQuestions, num.answer){
  
  # Randomize the order of correct answer.
  multipleChoiceAnswers <-randomOrder(answerVector, num.answer) 
  
  # Multiple Answer should start with MC
  if (num.answer == 1) {
    question.Text <- paste("MA\t", question.Text, sep = "" )
  } else if (num.answer == 0 ) {
    # Essay Question 
    question.Text <- paste("ESS\t", question.Text, sep = "" )
    # If it is an essay question then we do not have any answers. 
    multipleChoiceAnswers <- ""
    
  } else if (num.answer > 1) {
    # Multiple Choice questions should start with MC
    question.Text <- paste("MC\t", question.Text, sep = "" )
    
  } else {
    #ERROR
    print("ERROR: Number of answers must be 0 for Essay, 1 for Multiple Choice or larger than one for Multiple Answer.")
    question.Text <- "ERROR"
  }
  
  finalQuestion <- paste(question.Text , multipleChoiceAnswers , sep = "")
  return(c(poolOfQuestions, finalQuestion))
} 






