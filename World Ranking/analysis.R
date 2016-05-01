##
## Kaggle World Ranking Analysis
##

#Load libraries
library(ggplot2)

#Read data
input_dir = "input/"
cwurData = read.csv(paste0(input_dir, "cwurData.csv")) #CWUR rating
#ed_attain_sup = read.csv(paste0(input_dir, "educational_attainment_supplementary_data.csv")) #Population schooling
#ed_expen_sup = read.csv(paste0(input_dir, "education_expenditure_supplementary_data.csv")) #Funding for schools
school_country = read.csv(paste0(input_dir, "school_and_country_table.csv")) #Country/School table
shanghai = read.csv(paste0(input_dir, "shanghaiData.csv")) #Shanghai rating
times = read.csv(paste0(input_dir, "timesData.csv")) #Times rating

#Fix some of the names for the shanghai scores
school_country = rbind(school_country, data.frame(school_name = "University of California, San Francisco", country = "United States of America"))
shanghai$university_name[shanghai$university_name == "University of California-Berkeley"] = "University of California, Berkeley"

#Merge the shanghai data to get countries
##Lost datasets due to naming issues with school_country, as well as not in dataset
shanghai = merge(shanghai, school_country, by.x = "university_name", by.y = "school_name")

##
## Investigate the ratings of both Kyoto and UCSF in regards to Yamanaka's first paper on iPS cells and nobel prize
##

#Subset out data pertaining to UCSF and Kyoto Dai
kyoto_ucsf.shang = shanghai[shanghai$university_name == "University of California, San Francisco" | 
                        shanghai$university_name == "Kyoto University",]
kyoto_ucsf.cwur = cwurData[cwurData$institution == "University of California, San Francisco" | 
                             cwurData$institution == "Kyoto University",]
kyoto_ucsf.times = times[times$university_name == "University of California, San Francisco" | 
                             times$university_name == "Kyoto University",]

#Sort based on institute and year
kyoto_ucsf.shang = kyoto_ucsf.shang[order(kyoto_ucsf.shang$university_name,kyoto_ucsf.shang$year),]
kyoto_ucsf.cwur = kyoto_ucsf.cwur[order(kyoto_ucsf.cwur$institution, kyoto_ucsf.cwur$year),]
kyoto_ucsf.times = kyoto_ucsf.times[order(kyoto_ucsf.times$university_name, kyoto_ucsf.times$year),]
  
#Look at the times rating for Kyoto University
ggplot(data=kyoto_ucsf.times, aes(x=year, group=university_name)) + 
    geom_line(aes(y=as.numeric(levels(kyoto_ucsf.times$international)[as.numeric(kyoto_ucsf.times$international)]), color="international")) + 
    geom_line(aes(y=research, color="research"), size=1) + 
    geom_line(aes(y=citations, color="citations"), size=1) + 
    labs(x="Year", y="Score", title="Times rating", size=1) +
    theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.title = element_text(size=14, face="bold"),
        legend.title = element_blank()
    )

#Look at CWUR rating
ggplot(data=kyoto_ucsf.cwur, aes(x=year, col=institution)) + 
    geom_line(aes(y=quality_of_faculty, color="quality_of_faculty"), size=1) + 
    geom_line(aes(y=publications, color="publications"), size=1) + 
    geom_line(aes(y=influence, color="influence"), size=1) + 
    geom_line(aes(y=citations, color="citations"), size=1) + 
    geom_line(aes(y=broad_impact, color="broad_impact"), size=1) +
    labs(x="Year", y="Score", title="CWUR rating") +
    theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.title = element_text(size=14, face="bold")
    ) + 
    facet_grid(institution~.,scales="free_x")


#Look at the Shanghai rating
##Ignore alumni as UCSF has 0s
ggplot(data=kyoto_ucsf.shang, aes(x=year, col=university_name)) + 
    geom_line(aes(y=hici, group=university_name, color="hici"), size=1) + 
    geom_line(aes(y=ns, group=university_name, color="ns"), size=1) + 
    geom_line(aes(y=pub, group=university_name, color="pub"), size=1) + 
    geom_line(aes(y=award, group=university_name, color="award"), size=1) +
    labs(x="Year", y="Score", title="Shanghai rating") +
    theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        plot.title = element_text(size=14, face="bold")
    ) + 
    facet_grid(university_name~.,scales="free_x")
