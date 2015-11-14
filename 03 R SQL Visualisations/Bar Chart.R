require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)

# Change the USER and PASS below to be your UTEid
df <- data.frame(fromJSON(getURL(URLencode('skipper.cs.utexas.edu:5001/rest/native/?query="select * from final_grade where overall_weighted_growth_grade is not null"'),httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))

#column is wrong type so convert it to numerical
df["RANK_TOT"] <- lapply(df["RANK_TOT"], function(x) as.numeric(as.character(x)))

new_df <- df %>% select(FINAL_PLANTYPE, OVERALL_WEIGHTED_GROWTH_GRADE, RANK_TOT) %>% filter(FINAL_PLANTYPE != 'null' & RANK_TOT != 'null') %>% group_by(FINAL_PLANTYPE) %>% summarise_each(funs(mean))

avg_read <- mean(df[["READ_GROWTH_GRADE"]])
avg_math <- mean(df[["MATH_GROWTH_GRADE"]])

require(extrafont)
p <- ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_continuous() +
  labs(title='Overall Weighted Growth Average for Final Plantype') +
  labs(x="Final Plantype", y=paste("Avg Overall Weighted Growth Grade")) +
  layer(data=new_df, 
        mapping=aes(x=as.character(FINAL_PLANTYPE), y=OVERALL_WEIGHTED_GROWTH_GRADE, fill=RANK_TOT),
        stat="identity", 
        stat_params=list(), 
        geom="bar",
        geom_params=list(), 
        position=position_identity(),
  ) +
  geom_hline(aes(yintercept=avg_read, color = "red"))+
  geom_hline(aes(yintercept=avg_math))+
  annotate("text", label = "Avg Reading Growth Grade", x = 2, 6.3, size = 4, colour = "red") +
  annotate("text", label = "Avg Math Growth Grade", x = 2, 7, size = 4, colour = "black") +
  scale_fill_gradient(low = "#132B43", high = "#56B1F7")+
  theme(axis.text.x = element_text(size  = 10, angle = 45, hjust = 1, vjust = 1))
  
print(p) 
