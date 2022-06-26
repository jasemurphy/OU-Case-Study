#Data files downloaded from: https://analyse.kmi.open.ac.uk/open_dataset


assessments <- read_csv("~/Downloads/anonymisedData/assessments.csv")
courses <- read_csv("~/Downloads/anonymisedData/courses.csv")
studentAssessment <- read_csv("~/Downloads/anonymisedData/studentAssessment.csv")
studentInfo <- read_csv("~/Downloads/anonymisedData/studentInfo.csv")
studentRegistration <- read_csv("~/Downloads/anonymisedData/studentRegistration.csv")
studentvle <- read_csv("~/Downloads/anonymisedData/studentvle.csv")
vle <- read_csv("~/Downloads/anonymisedData/vle.csv")

#First task is to explore the data with some visualisations. What correlates with student results? 
#The following function, passfactor() permits us to visualise demographic factors contained in the studentInfo dataset. 
#Inputs x and y are a variable name, encoded without and with inverted commas.


passfactor<- function(x,y){
  
  studentInfo %>%
   mutate(subject = paste0(code_module, "_", code_presentation)) %>% #this line lets us use subject as an input to the function
    group_by({{x}}) %>%
    mutate(count = n()) %>%   #to calculate result rates by demographic group we first need the numbers in that group
    ungroup() %>%
    group_by({{x}}, final_result) %>% 
    mutate(resultcount = n()) %>%  #finding numbers within each group with each result [fail, pass,etc]
    mutate(  percent = resultcount/count) %>%  
    select({{x}}, percent, count, final_result) %>%  #dropping rows and columns with redundant info
    distinct() %>%   
    mutate(passrate = if_else(final_result == "Pass", percent,0 )) %>% #creating a new column with the passrate to make it 
    ungroup() %>%                                                      # easier to sort the chart using fct_reorder()
    group_by({{x}}) %>% 
    mutate(passrate = sum(passrate)) %>% 
    ungroup() %>% 
    mutate(order = fct_reorder({{x}}, passrate)) %>% 
    ggplot()+
    aes(x=order, colour= final_result, size = count, y = percent)+
    geom_point()+
    scale_x_discrete (labels = label_wrap(7))+
    scale_y_continuous(labels = scales::percent)+
    labs(title = paste( "Result rate by", y))+    
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(size = .08, colour = "grey"),
          axis.title = element_blank())
}

passfactor(region, "region")
passfactor(imd_band, "imd_band") #imd_band is ~socio-economic status. this chart shows seeming correlation with student success
passfactor(age_band, "age_band")
passfactor(disability, "disability")
passfactor(gender, "gender")
passfactor(highest_education, "highest_education")  seeming correlation with student success
passfactor(subject, "subject")+coord_flip()  #seeming correlation with student success, raises risk of confounding:
                                             # what conclusions can we draw if different subjects attract different demographics???


#For the two remaining info columns in the StudentInfo dataset we can't use the above function as they are numeric. 
#Instead we plot them each with minor variations of the above code, as follows:

  studentInfo %>%
    group_by(num_of_prev_attempts) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    group_by(num_of_prev_attempts, final_result) %>%
    mutate(resultcount = n()) %>%
    mutate(  percent = resultcount/count) %>%
    select(num_of_prev_attempts, percent, count, final_result) %>% 
    distinct() %>% 
    mutate(passrate = if_else(final_result == "Pass", percent,0 )) %>% 
    ungroup() %>%
    group_by(num_of_prev_attempts) %>% 
    mutate(passrate = sum(passrate)) %>% 
    ungroup() %>% 
    ggplot()+
    aes(x=num_of_prev_attempts, colour= final_result, size = count, y = percent)+
    geom_point()+
    scale_y_continuous(labels = scales::percent)+
    labs(title = "Result rate by number of previous attempts" )+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(size = .08, colour = "grey"),
          axis.title = element_blank()
          
#if at first you don't succeed, try, try, try again. So they say. Apparently in reality it doesn't work that well.

 studentInfo %>%
    group_by(studied_credits) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    group_by(studied_credits, final_result) %>%
    mutate(resultcount = n()) %>%
    mutate(  percent = resultcount/count) %>%
    select(studied_credits, percent, count, final_result) %>% 
    distinct() %>% 
    mutate(passrate = if_else(final_result == "Pass", percent,0 )) %>% 
    ungroup() %>%
    group_by(studied_credits) %>% 
    mutate(passrate = sum(passrate)) %>% 
    ungroup() %>% 
    ggplot()+
    aes(x=studied_credits, colour= final_result, size = count, y = percent)+
    geom_smooth()+
    scale_y_continuous(labels = scales::percent)+
    labs(title = "Result rate by number of credits studied" )+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(size = .08, colour = "grey"),
          axis.title = element_blank() )
    #heavy course loads are not a good idea.
    
    




