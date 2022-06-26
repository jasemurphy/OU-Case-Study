# This code produces a chart with 3 facets showing the resource access and assessment results for 3 students. 
# It is designed to illusrate the main part of the report and bring some of the findings back to a relatable, human level

learningjourneys <- function(x,y,z){


  subject1 <- studentInfo %>%    #the code needs many inputs, many of which are made 3 times, once for each student. This one gets the subject studied
    filter(id_student == x) %>%
    mutate(subject = paste(code_module, code_presentation)) %>%
    slice_max(1) %>%
    select(subject) %>% pull()

  subject2 <- studentInfo %>%
    filter(id_student == y) %>%
    mutate(subject = paste(code_module, code_presentation)) %>%
    slice_max(1) %>%
    select(subject) %>% pull()

  subject3 <- studentInfo %>%
    filter(id_student == z) %>%
    mutate(subject = paste(code_module, code_presentation)) %>%
    slice_max(1) %>%
    select(subject) %>% pull()


  studentAssessments1<- studentAssessment %>%  these get the assessments the student submitted 
    filter(id_student == x) %>%
    mutate(date = date_submitted ) %>%
    select(-id_student, -date_submitted)

  studentAssessments2<- studentAssessment %>%
    filter(id_student == y) %>%
    mutate(date = date_submitted ) %>%
    select(-id_student, -date_submitted)

  studentAssessments3<- studentAssessment %>%
    filter(id_student == z) %>%
    mutate(date = date_submitted ) %>%
    select(-id_student, -date_submitted)


  courses2<- courses %>% mutate(subject = paste(code_module, code_presentation)) #this line gives us a unique strng for each course.


  assessments1 <- assessments %>% mutate(subject = paste(code_module, code_presentation)) %>%  
    select(-code_module, -code_presentation) %>%                             #getting dates for exams
    left_join(courses2, by = "subject") %>%
    mutate(date = if_else(assessment_type == "Exam", module_presentation_length, date)) %>%
    filter(subject == subject1)

  assessments2 <- assessments %>% mutate(subject = paste(code_module, code_presentation)) %>%
    select(-code_module, -code_presentation) %>%
    left_join(courses2, by = "subject") %>%
    mutate(date = if_else(assessment_type == "Exam", module_presentation_length, date)) %>%
    filter(subject == subject2)

  assessments3 <- assessments %>% mutate(subject = paste(code_module, code_presentation)) %>%
    select(-code_module, -code_presentation) %>%
    left_join(courses2, by = "subject") %>%
    mutate(date = if_else(assessment_type == "Exam", module_presentation_length, date)) %>%
    filter(subject == subject3)


  studentinfofortitle1<- studentInfo %>%          #creating a facet title for each student
    filter(id_student == x) %>%
    mutate(gendertitle =  if_else(gender == "M", paste("male"), paste("female"))) %>%
    mutate(fakename =  if_else(gender == "M", paste("Alan*"), paste("Sarah*"))) %>%
    mutate(disabtitle =  if_else(disability == "Y", paste(" and has a disability"), paste(""))) %>%
    mutate(agetitle = if_else(age_band == "55<=", "over 55", NA_character_ ),
           agetitle = if_else(age_band == "35-55", "35 to 55", agetitle ),
           agetitle = if_else(age_band == "0-35", "under 35", agetitle )) %>% 
    mutate(imdtext = if_else(imd_band %in% c("0-10%", "10-20%"), "a heavily disadvantaged part of ", NA_character_),
           imdtext = if_else(imd_band %in% c("20-30%", "30-40%"), "a disadvantaged part of ", imdtext),
           imdtext = if_else(imd_band %in% c("40-50%", "50-60%"), "a socio-economically median part of ", imdtext),
           imdtext = if_else(imd_band %in% c("60-70%", "70-80%"), "an advantaged part of ", imdtext),
           imdtext = if_else(imd_band %in% c("80-90%", "90-100%"), "a heavily advantaged part of ", imdtext),
           imdtext = if_else(is.na(imd_band), "", imdtext)) %>%
    mutate(startdate = if_else(code_presentation == "2013J", "October 2013", NA_character_),
           startdate = if_else(code_presentation == "2013B", "February 2013", startdate),
           startdate = if_else(code_presentation == "2014B", "October 2013", startdate),
           startdate = if_else(code_presentation == "2014J", "February 2013", startdate)) %>%
    mutate(title = paste0(fakename, " is aged ", age_band, disabtitle, 
                          ". They identify as ", gendertitle, " \nand lived in ", imdtext, region," while taking the course.", 
                          "\nThey enrolled in the ", startdate, " presentation of course code ",code_module, 
                          ". Their final result was\n", final_result)) %>%
    select(title) %>% unique() %>% pull()

  studentinfofortitle2<- studentInfo %>%
    filter(id_student == y) %>%
    mutate(gendertitle =  if_else(gender == "M", paste("male"), paste("female"))) %>%
    mutate(fakename =  if_else(gender == "M", paste("Mohammed*"), paste("Julie*"))) %>%
    mutate(disabtitle =  if_else(disability == "Y", paste(" and has a disability"), paste(""))) %>%
    mutate(agetitle = if_else(age_band == "55<=", "over 55", NA_character_ ),
           agetitle = if_else(age_band == "35-55", "35 to 55", agetitle ),
           agetitle = if_else(age_band == "0-35", "under 35", agetitle )) %>% 
    mutate(imdtext = if_else(imd_band %in% c("0-10%", "10-20%"), "a heavily disadvantaged part of ", NA_character_),
           imdtext = if_else(imd_band %in% c("20-30%", "30-40%"), "a disadvantaged part of ", imdtext),
           imdtext = if_else(imd_band %in% c("40-50%", "50-60%"), "a socio-economically median part of ", imdtext),
           imdtext = if_else(imd_band %in% c("60-70%", "70-80%"), "an advantaged part of ", imdtext),
           imdtext = if_else(imd_band %in% c("80-90%", "90-100%"), "a heavily advantaged part of ", imdtext),
           imdtext = if_else(is.na(imd_band), "", imdtext)) %>%
    mutate(startdate = if_else(code_presentation == "2013J", "October 2013", NA_character_),
           startdate = if_else(code_presentation == "2013B", "February 2013", startdate),
           startdate = if_else(code_presentation == "2014B", "October 2013", startdate),
           startdate = if_else(code_presentation == "2014J", "February 2013", startdate)) %>%
    mutate(title = paste0(fakename, " is aged ", age_band, disabtitle, 
                          ". They identify as ", gendertitle, " \nand lived in ", imdtext, region," while taking the course.", 
                          "\nThey enrolled in the ", startdate, " presentation of course code ",code_module, 
                          ". Their final result was\n", final_result)) %>%
    select(title) %>% unique() %>% pull()

  studentinfofortitle3<- studentInfo %>%
    filter(id_student == z) %>%
    mutate(gendertitle =  if_else(gender == "M", paste("male"), paste("female"))) %>%
    mutate(fakename =  if_else(gender == "M", paste("Robin*"), paste("Yasmin*"))) %>%
    mutate(disabtitle =  if_else(disability == "Y", paste(" and has a disability"), paste(""))) %>%
    mutate(agetitle = if_else(age_band == "55<=", "over 55", NA_character_ ),
           agetitle = if_else(age_band == "35-55", "35 to 55", agetitle ),
           agetitle = if_else(age_band == "0-35", "under 35", agetitle )) %>% 
    mutate(imdtext = if_else(imd_band %in% c("0-10%", "10-20%"), "a heavily disadvantaged part of ", NA_character_),
           imdtext = if_else(imd_band %in% c("20-30%", "30-40%"), "a disadvantaged part of ", imdtext),
           imdtext = if_else(imd_band %in% c("40-50%", "50-60%"), "a socio-economically median part of ", imdtext),
           imdtext = if_else(imd_band %in% c("60-70%", "70-80%"), "an advantaged part of ", imdtext),
           imdtext = if_else(imd_band %in% c("80-90%", "90-100%"), "a heavily advantaged part of ", imdtext),
           imdtext = if_else(is.na(imd_band), "", imdtext)) %>%
    mutate(startdate = if_else(code_presentation == "2013J", "October 2013", NA_character_),
           startdate = if_else(code_presentation == "2013B", "February 2013", startdate),
           startdate = if_else(code_presentation == "2014B", "October 2013", startdate),
           startdate = if_else(code_presentation == "2014J", "February 2013", startdate)) %>%
    mutate(title = paste0(fakename, " is aged ", age_band, disabtitle, 
                          ". They identify as ", gendertitle, " \nand lived in ", imdtext, region," while taking the course.", 
                          "\nThey enrolled in the ", startdate, " presentation of course code ",code_module, 
                        ". Their final result was\n", final_result)) %>% 
    select(title) %>% unique() %>% pull()


  date1 <- c(-15:249) %>% as_tibble() %>% rename(date=value) %>% mutate(date = as.numeric(date)) %>% mutate(two =2)  #getting a consistent x-axis for each facet
  date2 <- c(-15:249) %>% as_tibble() %>% rename(date=value) %>% mutate(date = as.numeric(date)) %>% mutate(two =2)
  date3 <- c(-15:249) %>% as_tibble() %>% rename(date=value) %>% mutate(date = as.numeric(date)) %>% mutate(two =2)


  studentvle1 <- studentvle %>%
    mutate(subject = paste(code_module, code_presentation)) %>% #data on each students online resource access
    filter(subject == subject1) %>%
    filter(id_student == x) %>%
    select(-id_student)

  studentvle2 <- studentvle %>%
    mutate(subject = paste(code_module, code_presentation)) %>%
    filter(subject == subject2) %>%
    filter(id_student == y) %>%
    select(-id_student)

  studentvle3 <- studentvle %>%
    mutate(subject = paste(code_module, code_presentation)) %>%
    filter(subject == subject3) %>%
    filter(id_student == z) %>%
    select(-id_student)

  data1 <- date1 %>%                        #combining pieces and tidying for student 1 in readiness to make the final chart.
    left_join(studentvle1, by = "date") %>%
    left_join(vle, by = "id_site") %>%
    select(-code_module.x, -code_presentation.x, -code_module.y, -code_presentation.y) %>%
    left_join(assessments1, by = "date") %>%
    left_join(studentAssessments1, by = "id_assessment") %>%
    mutate(score = if_else(is.na(score), paste(""), as.character(score))) %>%
    mutate(assessment_type2 =  if_else(!is.na(assessment_type) & assessment_type == "CMA", paste("Computer-\nmarked\nassessment\n", score), paste(""))) %>% 
    mutate(assessment_type = if_else(is.na(assessment_type), paste(""), assessment_type),
           assessment_type = if_else(assessment_type == "TMA", paste("Tutor-\nmarked\nassessment\n", score), assessment_type),
           assessment_type = if_else(assessment_type == "CMA", paste(""), assessment_type))%>%
    rename(date = date.x) %>% select(-subject.x) %>%
    select(-date.y, -subject.y, -two) %>%
    mutate(marker = studentinfofortitle1)

  data2 <- date2 %>%
    left_join(studentvle2, by = "date") %>%
    left_join(vle, by = "id_site") %>%
    select(-code_module.x, -code_presentation.x, -code_module.y, -code_presentation.y) %>%
    left_join(assessments2, by = "date") %>%
    left_join(studentAssessments2, by = "id_assessment") %>%
    mutate(score = if_else(is.na(score), paste(""), as.character(score))) %>%
    mutate(assessment_type2 =  if_else(!is.na(assessment_type) & assessment_type == "CMA", paste("Computer-\nmarked\nassessment\n", score), paste(""))) %>% 
    mutate(assessment_type = if_else(is.na(assessment_type), paste(""), assessment_type),
           assessment_type = if_else(assessment_type == "TMA", paste("Tutor-\nmarked\nassessment\n", score), assessment_type),
           assessment_type = if_else(assessment_type == "CMA", paste(""), assessment_type))%>%
    rename(date = date.x) %>% select(-subject.x) %>%
   
    select(-date.y, -subject.y, -two) %>%
    mutate(marker = studentinfofortitle2)

  final <- date3 %>%
    left_join(studentvle3, by = "date") %>%
    left_join(vle, by = "id_site") %>%
    select(-code_module.x, -code_presentation.x, -code_module.y, -code_presentation.y) %>%
    left_join(assessments3, by = "date") %>%
    left_join(studentAssessments3, by = "id_assessment") %>%
    mutate(score = if_else(is.na(score), paste(""), as.character(score))) %>%
    mutate(assessment_type2 =  if_else(!is.na(assessment_type) & assessment_type == "CMA", paste("Computer-\nmarked\nassessment\n", score), paste(""))) %>% 
    mutate(assessment_type = if_else(is.na(assessment_type), paste(""), assessment_type),
           assessment_type = if_else(assessment_type == "TMA", paste("Tutor-\nmarked\nassessment\n", score), assessment_type),
           assessment_type = if_else(assessment_type == "CMA", paste(""), assessment_type))%>%
    rename(date = date.x) %>% select(-subject.x) %>%
    select(-date.y, -subject.y, -two) %>%
    mutate(marker = studentinfofortitle3) %>%
  rbind(data2, data1) %>% 
    mutate(activity_type = if_else(activity_type == "externalquiz", " \nExternal quiz\n ", activity_type),
           activity_type = if_else(activity_type == "forumng", " \nForum\n ", activity_type),
           activity_type = if_else(activity_type == "homepage", " \nHome page\n ", activity_type),
           activity_type = if_else(activity_type == "oucontent", " \nOpen Uni\ncontent\n ", activity_type),
           activity_type = if_else(activity_type == "ouwiki", " \nOpen Uni Wiki\n ", activity_type),
           activity_type = if_else(activity_type == "page", " \nPage\n ", activity_type),
           activity_type = if_else(activity_type == "resource", " \nResource\n ", activity_type),
           activity_type = if_else(activity_type == "subpage", " \nSub-page\n ", activity_type),
           activity_type = if_else(activity_type == "url", " \nURL\n ", activity_type),
           activity_type = if_else(activity_type == "ouelluminate", "Open Uni\nOnline \nTutorial \nsoftware", activity_type))
  
  final %>% 
    mutate(dateblocks = if_else(date%%7 == 0, as.character(date),NA_character_ )) %>%
    mutate(dateblocks = as.numeric(dateblocks)) %>% 
    tidyr::fill(dateblocks) %>% 
    ggplot()+
    geom_col(aes(x=dateblocks, y= sum_click, fill= activity_type))+
    geom_text(aes(x=date, y= 60, label = paste(assessment_type)), size = 2.5)+
    geom_text(aes(x=date, y= 170, label = paste(assessment_type2)), size = 2.5)+
   facet_wrap(~marker, ncol = 1)+
    labs(title = "Three Learning Journeys",
         caption ="*Names are illustrative, data is anonymised.",
         fill = "Materials \naccessed")+
    xlab("Day relative to start of course. \nStarts below zero as students have access to resources before class begins.")+
    ylab("Number of clicks on course materials per week")+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "grey", size =.1),
          strip.background.x = element_rect(fill = "peachpuff"),
          legend.key.size = unit(.15,"cm"),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 11),
          axis.text.x = element_text(size = 13)
          )+
    scale_fill_manual(values = c("cyan", "gold", "skyblue1", "orangered", "lemon chiffon", "grey40", "navy", "green", "lavender", "thistle"))
}

#use this snippet to find student numbers to put into the function below

studentInfo %>%   filter( imd_band =="0-10%") %>% filter(code_module =="DDD") %>% 
  filter(code_presentation == "2014B") %>%select(final_result, id_student, everything()) %>%  print(n=20)


learningjourneys(550711, 515595, 487711)

