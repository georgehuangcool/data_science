##############################################
## Introductory Data Science for Innovation ##
##            Seminar 2                     ##
##        By Frédérique Bone                ##
##############################################


# importing the library for working with tidy data
library(tidyverse) #you may need to install this package if this has not already done before

# Getting the dataset for the seminar
install.packages("titanic") # more info here: https://www.rdocumentation.org/packages/titanic/versions/0.1.0
library(titanic) #call the library
Titanic <- as.data.frame(titanic_train) # this is the main dataset we are going to use throughout the exercise. 

#------------------------------
# 1. Explore the given dataset
#------------------------------

## (REPLACE __ by the appropriate value) ##

# 1.1 Look at size of dataset on the right, how many rows and columns?
#  891 rows and 12 columns

# 1.2 You can also use the function 'nrow()' and 'ncol()' on the dataset Titanic
nrow(Titanic) # use this function to find the number of rows
ncol(Titanic)  # use this function to find the number of columns 

# 1.3 Inspect the columns names of the dataset using 'names()'
names(Titanic) # Use this function to find ou the column names of the dataframe

# 1.4 Use 'head()' and 'tail()' on Titanic to check the 5 first /last rows (using n=) of the data
head(Titanic, n=5) 
tail(Titanic, n=8)

# 1.5 Check the overall structure of the dataset use the function 'str()' 
#     and summary statistics for each variables using 'summary()'
str(Titanic)
summary(Titanic)

# 1.6 Identify which variables may be considered as factors

# 1.7 Change these variables to factor
Titanic$Sex <- as.factor(Titanic$Sex)
class(Titanic$Sex) # check whether this column has been transformed to a factor
Titanic$Embarked <- as.factor(Titanic$Embarked)
class(Titanic$Embarked) # check whether this column has been transformed to a factor

# 1.8 Rerun the 'summary()' function and see what has changed for the two ammended variables
summary(Titanic)

#------------------------------
# 2. Reshape the given dataset 
#------------------------------

# 2.0 Remove variables that not of high relevance and has many blanks

# Example of select
Titanic_clean <- Titanic %>%    # %>% this is the pipe symbol it enable you to do a series of operations on the dataframes iteratively. 
  select(-Cabin)                # Here we selected out the vairable Cabin as it is not very informative
                                # Using select with '-' means remove this specific column, use without minus, with a list separated by commas 
                                # to tell the computer which columns you want to keep. 


# 2.1 removing, renaming and creating new variables
Titanic_clean <- Titanic %>% 
  select(-Ticket, -Cabin) %>%                       # Using 'select' remove the column 'Ticket' and Cabin
  rename(Class=Pclass, sibling_spouse=SibSp, parents_children=Parch) %>%     # Using the 'rename' function change the name of Pclass to Class, 
                                         # SibSp (siblings or spouse onboard) to something more explicit,
                                         # Parch (parents or children onboard) to something more explicit
  separate(Name, into=c("Surname","Firstname"),      # separate the column 'Name' into 'Surname' and 'Firstname', also looking into the column find the right character to use as a separator
           sep=", ", remove=FALSE) %>% 
  mutate(Overall_relatives = sibling_spouse+parents_children)                   # Using 'mutate' add a new column 'Overall_relatives' which merges siblings and spouses and parents and children


## If you have time - try to separate again firstname to move out the title e.g. Mr Mrs etc. ##

# 2.2 Practice reshaping dataset.
Titanic_reshape <- Titanic_clean %>%                 # create a new dataset for reshape
  pivot_longer(cols=c(Overall_relatives,sibling_spouse,parents_children),  # using pivot longer put all the relatives columns into a unique column, using pivot longer
               names_to="family_type",                                   # name the new column 'family_type' (don't forget the quotation marks)
               values_to = "family_count") %>%                          # name the new value column 'family_count'
  select(PassengerId, family_type, family_count)                        # keep only the passenger ids and the 'family_type' and 'family_count'



#------------------------------
# 3. Using logic in your code
#------------------------------

# 3.1 Dealing with unexplicit variables in Embarked (town where people embarked)
unique(Titanic_clean$Embarked)        # Use unique to check all the possible values of Embarked
value_embarked <- as.vector(unique(Titanic_clean$Embarked))   # Associate all unique values to the vector and then 'print()' it
print(value_embarked)

# 3.2 Check which entries have missing values
embarked_missing <- Titanic_clean %>%  # Create a dataframe containing only these missing values using Titanic_clean
  filter (Embarked=="")           # using filter, select only the obs. where Embarked is of value ""

head(embarked_missing, n=2) # Using head() inspect the two first rows of this dataframe

# Create a new column in Titanic clean where for the value of the new variable to be entered
Titanic_clean <- Titanic_clean %>%
  mutate(Entry_port=NA)

# 3.3 Iterate over the dataset to create the new column with the right
for (i in 1:nrow(Titanic_clean)){ # Create a loop iterating over rows 1 to nrow() of the dataframe to 
  
  if (Titanic_clean$Embarked[i] == "S"){ # 
    Titanic_clean$Entry_port[i] <-"Southampton"
  } else if(Titanic_clean$Embarked[i] == "C"){  # create an entry for the C = Cherbourg, Q=Queenstown, ""=NA
    Titanic_clean$Entry_port[i] <-"Cherbourg"
  } else if(Titanic_clean$Embarked[i] == "Q"){
    Titanic_clean$Entry_port[i] <- "Queenstown"
  } else if(Titanic_clean$Embarked[i] == ""){
    Titanic_clean$Entry_port[i] <-NA
  }
  
}

# 3.4 Remove the variable Embarked using select, make the column 'Entry_port' a factor
#     and reassign the results back to Titanic_clean
Titanic_clean <- Titanic_clean %>%
  select(-Embarked) %>%
  mutate(Entry_port = as.factor(Entry_port))

#--------------------------------
# 4. Dealing with missing values
#--------------------------------

# 4.1 Look again at the summary of Titanic clean to identify the 
#     column which has NA values
summary(Titanic_clean)

# 4.2 Remove the NAs in for all the relevant variables
Titanic_clean <- Titanic_clean %>%     # Use Titanic_clean and re-assign it to itself
  filter(!is.na(Entry_port)) %>%  # use the right function to select the rows which have NAs for a specific variable
  filter(!is.na(Age))       # repeat the above step for all the variables that you identified which have missing NAs


#--------------------------------
# 5. Creating summary statistics
#--------------------------------

# 5.1 Create a dataframe with sum, and count for the people who died or survived

Tit_summary_surv <- __ __      # Use Titanic_clean as a baseline
  group_by(__) __                       # Group by Class
  __(survived=__(__),         # using 'sum()' on survived create a summary on how many survived (using summarise)
            Nb_people=__) %>%              # using 'n()' count the number of people who were on the boat
  __(Perc_died = __*(__-__)/__)  # Generate a percentage of those who died


# 5.2 Create a dataframe to check whether there is a difference in percentage of death for sex (copy and alter the code above)

# 5.3 Create a dataframe to check whether there is a difference in percentage of death for both sex and class
#     Copy and alter the code above, use group_by both sex and class

# 5.4 Create a dataframe which looks at summary for age groups on top of class and sex

    # Create categorical variables for Child and Adult
    # e.g. Child < 18 and Adult > 18 
    # Create a summary table to check whether survival in children is more likely than in adults

    # Create a new summary table with summary statstics for all Class, Sex and Age






