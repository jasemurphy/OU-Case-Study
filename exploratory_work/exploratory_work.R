library(tidyverse)
library(hexbin)

#Data files downloaded from: https://analyse.kmi.open.ac.uk/open_dataset


assessments <- read_csv("~/Downloads/anonymisedData/assessments.csv")
courses <- read_csv("~/Downloads/anonymisedData/courses.csv")
studentAssessment <- read_csv("~/Downloads/anonymisedData/studentAssessment.csv")
studentInfo <- read_csv("~/Downloads/anonymisedData/studentInfo.csv")
studentRegistration <- read_csv("~/Downloads/anonymisedData/studentRegistration.csv")
studentvle <- read_csv("~/Downloads/anonymisedData/studentvle.csv")
vle <- read_csv("~/Downloads/anonymisedData/vle.csv")

#First task is to explore the data with some visualisations. What correlates with student results? 
#The following function, passfactor() permits us to visualise the relationship of demographic factors 
#contained in the studentInfo dataset with student results.

passfactor<- function(x,y){                #Inputs x and y are a variable name, encoded without and with inverted commas.
  
  studentInfo %>%
   mutate(subject = paste0(code_module, "_", code_presentation)) %>% #this line will let us use subject as an input to the function
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
passfactor(highest_education, "highest_education")  #seeming correlation with student success
passfactor(subject, "subject")+coord_flip()  #seeming correlation with student success, raises risk of confounding:
                                             # what conclusions can we draw if different subjects attract different demographics???


#For the two remaining info columns in the StudentInfo dataset we can't use the above function as they are numeric. 
#Instead we plot them each with variations of the above code, as follows:

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
          
#Try, try, try again. So they say. Repeated attempts are correlated with failure in our dataset.

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
          
    #heavy course loads are not correlated with success.
    
          # So the early investigations seem to show a) demographic factors matter and b) fail/withdraw rates are high overall. 
          # e.g. When we make a table of results for a group defined to have multiple risk factors, over 75% of them fail or withdraw:

studentInfo %>%
  filter(imd_band %in% c("0-10%", "10-20%")) %>%
  filter(disability == "Y") %>%
  filter(highest_education %in% c("Lower Than A Level", "No Formal quals" )) %>%
  group_by(final_result) %>%
  mutate(resultcount = n()) %>%
  ungroup() %>%
  select(final_result, resultcount) %>% unique() %>% 
  mutate(n= sum(resultcount),
         percent = resultcount/n)
          
    # Exploring the student registration datatset next.
          
    # 1. we see that Open University has a brief relationship with the typical student.
    # The following code gives a table showing the vast majority of students (88%) do just one course. 
    # Another 11% do two - the pareto rule does not apply here.
          
studentRegistration %>%
  group_by(id_student) %>%
  mutate(count= n()) %>%
  ungroup() %>%
  select(id_student, count) %>%
 distinct() %>%
  group_by(count) %>%
  mutate(numbers = n()) %>%
  ungroup() %>%
  select(count, numbers) %>%
  unique %>%
  arrange(desc(count)) %>%
  mutate(sum = sum(numbers),
         percent = numbers/sum)

#2. Students can register very early for courses, and they pull out late. 
          # One possible conclusion is withdrawal from a course may be an alternative to failing.
          
          studentRegistration %>%
  filter(!is.na(date_unregistration)) %>%
  ggplot()+
  aes(x=date_registration, y= date_unregistration)+
  geom_hex(bins = 25)+
  scale_x_continuous(limits = c(-350, 50))+
  scale_y_continuous(limits = c(-300, 250))+scale_fill_gradientn(colours = c("pale goldenrod", "gold", "orangered", "red", "firebrick"))

#I need to improve my visualisations of the vlestuff and put them here.
          
          #assessments
          
          # Exploring the idea that assessment format affects the result. The data hints that computer-based exams are correlated with success.
          # Are these multiple choice, as opposed to tutor-marked assessments which may require long-format answers? Could that hint at 
          # support for written communication being an idea that would support success?
          
studentAssessment %>%

  left_join(assessments, by = "id_assessment") %>% #this dataset has the assessment type.
  group_by(id_assessment) %>%
  mutate(av_score = mean(score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(subject = paste0(code_module, code_presentation)) %>%
  mutate(id_assessment = as.factor(id_assessment)) %>%
  mutate(order = fct_reorder(id_assessment, date)) %>%

  ggplot()+
  aes(x= order, y= score, colour = assessment_type)+
  geom_point(alpha  = .5)+
  geom_point(aes(y= av_score), colour = "black")+
  coord_flip()+facet_wrap(~subject, scales = "free")+
  labs(title = "are students performing better on computer-marked assessments (CMA) than on tutor-marked assessments (TMA) ? ")


