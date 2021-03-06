require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)

KPI_average = 10000     
KPI_great = 1000

df <- data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
                                                "select EMH, DISTRICTNAME, kpi,
                                                case
                                                when kpi < "p1" then \\\'Great\\\'
                                                when kpi < "p2" then \\\'Average\\\'
                                                else \\\'Ok\\\'
                                                end kpi
                                                from (select EMH, DISTRICTNAME, sum(RANK_TOT) as kpi
                                                from FINAL_GRADE 
                                                group by EMH, DISTRICTNAME)
                                                order by DISTRICTNAME;"
                                                ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_great, p2=KPI_average), verbose = TRUE)))

p <- ggplot() + 
  coord_cartesian() + 
  scale_x_discrete() +
  scale_y_discrete() +
  labs(title='EMH and District Name Crosstab') +
  labs(x=paste("DISTRICTNAME"), y=paste("EMH")) +
  layer(data=df, 
        mapping=aes(x=DISTRICTNAME, y=EMH, fill=KPI.1), 
        stat="identity", 
        stat_params=list(), 
        geom="tile",
        geom_params=list(alpha=0.50), 
        position=position_identity()
  )
print(p)
