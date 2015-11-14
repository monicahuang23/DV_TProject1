require("jsonlite")
require("RCurl")
require('dplyr')
require('ggplot2')

# Change the USER and PASS below to be your UTEid
final_grades <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from final_grade where SCHOOLNUMBER is not null"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

enrl_working <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from enrl_working where SCHOOL_CODE is not null"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

# Changes column names to join by
names(final_grades)[names(final_grades)=="SCHOOLNUMBER"] <- "SCHOOL_CODE"

#Convert columns to the correct type
final_names <- c("SCHOOLNAME","DISTRICTNAME","EMH","EMH_COMBINED","INITIAL_PLANTYPE","FINAL_PLANTYPE","NOTES")
final_nums <- setdiff(names(final_grades),final_names)
final_grades[final_nums] <- lapply(final_grades[final_nums], function(x) as.numeric(as.character(x)))
final_grades[final_names] <- lapply(final_grades[final_names], function(x) as.character(x))

enrl_names <- c("ORGANIZATION_NAME", "SCHOOL_NAME")
enrl_working[enrl_names] <- lapply(enrl_working[enrl_names], function(x) as.character(x))

# join by school code
x <- final_grades %>% inner_join(., enrl_working, by="SCHOOL_CODE", copy=TRUE) %>% filter(DISTRICTNAME == 'ASPEN 1') 
x <- x %>% select(SCHOOLNAME, MATH_GROWTH_GRADE, PCT_ASIAN)

require(extrafont)
p <- ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_continuous() +
  labs(title='Blended Data') +
  labs(x="School Name", y=paste("Math Growth Grade")) +
  layer(data= x, 
        mapping=aes(x=as.character(SCHOOLNAME), y=MATH_GROWTH_GRADE, fill=PCT_ASIAN),
        stat="identity", 
        stat_params=list(), 
        geom="bar",
        geom_params=list(), 
        position=position_identity(),
  ) +
  annotate("text", label = '12' , x = 1, 12.3, size = 5, colour = "black") +
  annotate("text", label = '7' , x = 2, 7.3, size = 5, colour = "black") +
  annotate("text", label = '8' , x = 3, 8.3, size = 5, colour = "black") +
  annotate("text", label = '9' , x = 4, 9.3, size = 5, colour = "black") +
  theme(axis.text.x = element_text(size  = 8, angle = 20, hjust = 1, vjust = 1))

print(p) 
