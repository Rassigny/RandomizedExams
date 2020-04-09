# Generating Randomized Exams/Quizzes for Backboard Learn. 

This project is an implementation of randomized exams for Blackboard Learn. 

# How to Run 
This is an implementation in R

clone the github project and do the following on the command line. 

```
cd Examples/
Rscript Example-1.R
```

It includes implemented R functions from  ../Randomized_Quizzes.R file. 

source('../Randomized_Quizzes.R',  chdir =FALSE)


This implementation can be convert to any programing language. The only important concept is that generated output txt file 
should follow the instructions for question pools in Blackboard. It should be a tab seperate file, see following documentation.  


# Blackboard Documentation about the question pools 

https://help.blackboard.com/Learn/Instructor/Tests_Pools_Surveys/Reuse_Questions/Upload_Questions


# How to import question pool into Blackboard. 

## Step-1

1. Go to Control Panel 
2. Go to "Course Tools"
3. Go to "Tests, Surveys, and Pools"
4. Go to "Pools"
5. Go to "POOL IMPORT"
6. Click Browse to locate a file to import. Select the generated .txt output file and submit. 
A new question pool will be generated. 

## Step-2 

1. Go to Assessments
2. Create a Quiz or Exam or edit one of the exisitng ones 
3. Add a question and select "Create Random Block"
4. Select the generated Question pool 

For each student exam/quiz, one question from the question pool will be randomly picked up and assigned during the exam run. 
In this way each student will get a different question.  



# Follow the following coding instructions. 

* The correct answer is the first element in answer vector. 

* Wrong answers should not be fixed values, it should be generated based on some potential mistakes that one might do.

* We should pick random numbers in some meaningful ranges and work with it. If we have any numbers on the questions, try to randomize them as well so that the questions look really different.

* Document your code and how you are generating values. We need to be able to understand the code of each other and also be able to understand it in a year or so.

* Provide a garantee that wrong answers can not be the same or not the same as corrected answers. In the case that answers are fixed number, this case can easily be possible because we 
generate values by using random numbers. 

* All of the numbers in the questin text should be randomized to make generated quizzes different. It does not matter of if the numbers are important to answer the question or not. 



# Special Charachters like mu, sigma 

https://www.whatsmyip.org/html-characters/

H<sub>0</sub>: &mu; = %s <br> H<sub>1</sub>: &mu; &ne; %s  <br>

alpha is  &alpha; 
mu    is  &mu; 
beta  is &beta; 

notEqual is &ne; 

https://www.geeksforgeeks.org/html-subscript-superscript-tags/


# USING LATEX 
Test the following example 



We can also use Latex 


Like following code 

\\( \\hat{y}=1.2x-20 \\) 

<script src=\"https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\" type=\"text/javascript\"></script>

Note-1: you have to reject every backslash like \\ (use double backslash instead of one)

Note-2: Start and end of Latex MathJax should be \\(   HERE YOUR LATEX \\) 


# Acknowledgment 
This work is done at Boston University, Metropolitan College, Computer Science Department 
By Shinsaku Okazaki, Paritosh Shirodkar, Sahil Gupta and Kia Teymourian 


