source('../Randomized_Quizzes.R', chdir=FALSE)

questionID = "Example-2-OUTPUT.txt"

questionString <- "As part of quality control, a pharmaceutical company tests a sample of manufacturer pills to see 
the amount of active drug they contain is consistent with the labelled amount. 
That is, they are interested in testing the following hypotheses: <br> <br> 
H0: &mu; = %s mg (the mean levels are as labelled) <br> 
H1: &mu; &ne; %s  mg (the mean levels are not as labelled) <br> <br>
Assume that the population standard deviation of drug levels is %i mg. For testing, they take a sample of %s pills randomly from 
the manufacturing lines and would like to use a significance level of &alpha;=0.05. <br> <br>
They find that the sample mean is %s mg.  Calculate the z statistic."

questionString <- gsub("[\r\n]", "", questionString)


poolOfQuestions <- c()
answerVector <- c()
wrong.answers <- c()

for (questionNr in seq(1, No.Of.Questions, by=1)){
    
    # Generate Random data 
    muNull <- sample(seq(90,100, by=2), size = 1, replace=TRUE)
    sd <- sample(5:10, size = 1, replace=TRUE)
    n <- sample(32:45, size = 1, replace=TRUE)
    xbar <- round(muNull + runif(1, 1, 7), 2) 
    
    question.Text <- sprintf(questionString, muNull, muNull,  sd, n, xbar)
    
    
    # Correct answer is the first one here.
    z = (xbar - muNull)/(sd/sqrt(n))
    answerVector[1] <- roundUP(z)
    

    # All other wrong answers.
    answerVector[2] <- 1-pnorm(abs(z))
    answerVector[3] <- 2*(pnorm(abs(z)))
    answerVector[4] <- roundUP((xbar - muNull)/(sd/n))
    answerVector[5] <- pnorm(abs(z))
    answerVector[6] <- roundUP((xbar + muNull)/(sd/n))
    answerVector[7] <- roundUP((xbar + muNull)/(sd/sqrt(n)))
    
    # Check duplicates
    for (a in answerVector) {
        if(a %in% answerVector[answerVector != a]) stop("There are duplicates")
    } 
   
    poolOfQuestions <-randomAndAddToPool(answerVector, question.Text, poolOfQuestions, 1)
}
writeItToFile(questionID, poolOfQuestions)
