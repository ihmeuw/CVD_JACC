##########################################################################################################################
##########################################################################################################################
# Author: Megan Lindstrom 
# Date: First-June 27, 2022, Last- Oct 19, 2022
# Purpose: Produce pdf files with plots and maps for JACC PAH manuscripts
# Description: This code creates the figures and table for the JACC Almanac. 
#              It can be launched from an array script, or if running for a 
#              single region use the regionname/locid variables (currently 
#              set to pull from the launch script). The code has three sections
#              The first calls in libraries and general arguments. Following is
#              where all data is pulled in for each figure/table. The last 
#              section creates all of the figures, saving them to a specified
#              output location. If trying to create a single figure and not run
#              the entire code it is suggested to still run the entire data step,
#              as there are shared calls between figures/table.
##########################################################################################################################
##########################################################################################################################
rm(list=ls())

###### Libraries ######
packages <- c("data.table", "R.utils", "raster", "data.table", "ggplot2", "rgdal", "sf", "stringr", "tidyverse",
              "cowplot", "scales", "ggrepel", "kableExtra", "dplyr", "gridExtra", "formattable", "extrafont")
suppressMessages(invisible(lapply(packages, library, character.only = TRUE)))

# Load central libraries
suppressMessages(sourceDirectory("ADDRESS", modifiedOnly=FALSE))


###### Arguments for parallelization ######
args <- commandArgs(trailingOnly = TRUE)

task_id <- ifelse(is.na(as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))), 1, as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID")))
message(paste0("Task ID is ", task_id))

region_path <- args[1]
message(region_path)

regions <- fread(region_path)

regionname <-regions[task_id, regionname]
locid <- regions[task_id, locid]

message(paste0(regionname, ", ", locid))

###### Functions ######
`%ni%` <- Negate(`%in%`)
date <- gsub("-", "_", Sys.Date())

###### Paths ######
outdir <- "ADDRESS"

###### Arguments to pull the data ######
gbd_round <- 7
decomp <- 'iterative'
year <- 2021
codcorrect_version <- 7785 
como_version <- 7786 
daly_version <- 7790 
burdenator_version <- 7789 


# list the causes and risks we are creating maps for
causes <- 491

##########################################################################################################################
# Pull data
##########################################################################################################################

# pull in cause/risk/location/age metadata
cause_metadata <- get_cause_metadata(cause_set_id = 3, gbd_round_id = gbd_round, decomp_step = decomp) 
risk_metadata <- get_rei_metadata(rei_set_id = 1, gbd_round_id = gbd_round, decomp_step = decomp) 
locs <- get_location_metadata(location_set_id = 91, gbd_round_id = gbd_round, decomp_step = decomp) 
locs1 <- get_location_metadata(location_set_id = 107, gbd_round_id = gbd_round, decomp_step = decomp) 
age_meta <- get_age_metadata(age_group_set_id = 19, gbd_round_id=gbd_round)

#########################################################
##########Map data 
query2020 <- get_outputs(topic='cause', gbd_round_id = gbd_round, decomp_step=decomp, compare_version_id=como_version,
                      cause_id = 491,  year_id = 2021, age_group_id = 27, sex_id = 3, 
                      metric_id = 3, measure_id = 1, location_id='all')
query1 <-query2020
setnames(query1, old="val", new="mapvar2")
query1 <- merge(query1, locs1[, c("location_id", "region_id")], by="location_id")
# multiple by 100000 for rates
query1$mapvar2 <- query1$mapvar2*100000
basicmap <- st_read("ADDRESS/map.shp")
plotdata2 <-  merge(basicmap, query1, by.x = "location_i", by.y = "location_id")
#join loc data
plotdata2 <- merge(plotdata2, locs, by.x="location_i", by.y= "location_id")


#########################################################
##########SDI SCATTERPLOT 
#pull sdi covariate for al locations
globalsdi <- get_covariate_estimates(
  gbd_round_id=7,
  covariate_id=881,
  location_id = 'all',
  decomp_step='iterative',
  year_id=2021)
#Merge sdi covariate to plotdata 2 (from regional map data above)
globalsdi2 <- merge(globalsdi, plotdata2, by.x = "location_id", by.y = "loc_id")
#times globalsdi by 100
globalsdi2$SDI <- globalsdi2$mean_value*100
globalsdi2$super_region_name[globalsdi2$super_region_name== "North Africa and Middle East"] <-  "North Africa and Middle East "
globalsdi2$super_region_name[globalsdi2$super_region_name== "South Asia"] <-  "South Asia "


#########################################################
##########RISK BARCHARTS
#round function 
roundup_to <- function(x, to = 10, up = FALSE){
  if(up) round(.Machine$double.eps^0.5 + x/to)*to else round(x/to)*to
}

#call cvd causes in
causes <- get_cause_metadata(cause_set_id=3, release_id=9)
cvd_cause_ids <- causes[grepl('cvd', acause)&most_detailed==1, cause_id]
causescvd <- cause_metadata[cause_metadata$cause_id %in% cvd_cause_ids]
causenames <- reorder(causescvd$cause_name, causescvd$sort_order)
# pull in daly data for 2020 then merge with locations data which is pulled from locs 
querydaly<- get_outputs(topic='cause', gbd_round_id= 7, decomp_step='iterative', compare_version_id=daly_version, 
                        cause_id = cvd_cause_ids, location_id='all', year_id=2021, sex_id=3, age_group_id=27, 
                        measure_id=2, metric_id=3)
querydaly <- merge(querydaly, locs, by= "location_id")
querydaly <- merge(querydaly, cause_metadata, by = "acause")

dalyw_EU <- subset(querydaly, region_name == regionname)
dalyw_EU <- subset(dalyw_EU, location_type.x == "admin0" | location_type.x == "nonsovereign")
#times daly by 100,000 then round to 2 digits
dalyw_EU$valrate<-dalyw_EU$val*100000
dalyw_EU$valrate<- round(dalyw_EU$valrate, digits=2)
c25 <- c(
  "dodgerblue2", "#FF6666", 
  "#339993",
  "#6A3D9A",
  "#FF7F00", 
  "black", "gold1",
  "skyblue2", "#FB9A99", 
  "palegreen2",
  "#CAB2D6", 
  "#FDBF6F", 
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1","darkorange4")
setcol <- set_names(c25, causenames)
dalyw_EU$ordering <- as.integer(factor(with(dalyw_EU, sort_order.y)))

# Attributable burden for cvd causes and risks
burden<-get_outputs(topic="rei",
                    measure_id=2, # DALYS
                    metric_id = 3,
                    location_id=locid, # all locations including global, super region, regions
                    compare_version_id = burdenator_version,
                    year_id=2021,
                    gbd_round_id=7,
                    decomp_step="iterative",
                    rei_id = "all", #cholera
                    cause_id = cvd_cause_ids,
                    age_group_id= 27)

#merge with risk metadata and subset to all causes but level 1
burden2 <- merge(burden, risk_metadata, by= "rei_name")

burden2$valrate <- burden2$val*100000
burden2 <- merge(burden2, cause_metadata, by= "acause")
keep <- c("temperature_low", "temperature_high", "smoking_shs", "smoking_direct","metab_sbp", 
          "metab_ldl", "metab_ikf", "metab_fpg", "metab_bmi", "envir_lead","drugs_alcohol","diet","air_pm","air_hap","activity")
burden2 <- dplyr::filter(burden2, rei.y %in% keep)
burden2[is.na(burden2)] <- 0

burden3 <- aggregate(burden2$valrate, by=list(Category=burden2$rei_name), FUN=sum)
burdenmax <- max(burden3$x)+250
burdenmax <- roundup_to(burdenmax, to = 100)

burdenmin <- min(burden2$valrate)-50
burdenmin <- roundup_to(burdenmin, to = 50)
#subset by risk type (environmental, metabolic, and behavioral)
keep1 <- c("temperature_low", "temperature_high", "envir_lead","air_pm","air_hap")
env_burd <- dplyr::filter(burden2, rei.y %in% keep1)
#env_burd<- merge(env_burd, causescvd, by= "acause",all.y = TRUE)
keep2 <-c("metab_sbp", "metab_ldl", "metab_ikf", "metab_fpg", "metab_bmi")
met_burd <- dplyr::filter(burden2, rei.y %in% keep2)
#met_burd<- merge(met_burd, causescvd, by= "acause", all.y = TRUE)
keep3 <- c("smoking_shs", "smoking_direct","drugs_alcohol","diet","activity")
beh_burd <- dplyr::filter(burden2, rei.y %in% keep3)
#beh_burd<- merge(env_burd, causescvd, by= "acause", all.y = TRUE)


#########################################################
##########BOX AND WHISKER
#merge each get outputs with locs, then specify region of interest and administrative level)
query2021 <- get_outputs(topic='cause', gbd_round_id = gbd_round, decomp_step=decomp, compare_version_id=como_version,
                         cause_id = cvd_cause_ids,  year_id = 2021, age_group_id = 27, sex_id = 3, 
                         metric_id = 3, measure_id = 1, location_id = "all")
query2021 <- merge(query2021, locs, by.x="location_id", by.y= "location_id")
query2021 <- subset(query2021, region_name == regionname)
query2comp <- subset(query2021, location_type.x == "admin0"| location_type.x == "nonsovereign")
# multiple by 100000 for rates
query2comp$valrate<-query2comp$val*100000
query2comp$valrate<- round(query2comp$valrate, digits=2)
#drop nas
query3 <- query2comp %>%
  drop_na(valrate)
#merge on cause data
query3 <- merge(query3, causes, by = "acause")


#########################################################
##########PYRAMID PLOT
query2020pyr <- get_outputs(topic='cause', gbd_round_id= 7, decomp_step='iterative', compare_version_id=como_version, 
                            cause_id=491, location_id=locid, year_id=c(1990,2021), sex_id=c(1,2), age_group_id=c(1, 188, 9:20, 30:32, 235),
                            measure_id=1, metric_id=3)
sortageframe <- data.frame(age_group_name = c("Under 5", "5 to 19","20 to 24", "25 to 29", "30 to 34", "34 to 39", "40 to 44",
                     "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",
                     "85 to 89", "90 to 94", "95 plus"), sortage = c(1:18))
query2020pyr <- merge(query2020pyr, sortageframe)

query2020pyr$valrate <- round(query2020pyr$val*100000, digits = 0)

query2020pyr$valrate <- ifelse(query2020pyr$sex_id==2,  query2020pyr$valrate*(-1),
                              query2020pyr$valrate*1)

#########################################################
##########TABLE DATA
query2020daly <- get_outputs(topic='cause', gbd_round_id= 7, decomp_step='iterative', compare_version_id=daly_version, 
                             cause_id=cvd_cause_ids, location_id=locid, year_id=2021, sex_id=3, age_group_id=27,
                             measure_id=2, metric_id=3)
query2020comp <- get_outputs(topic='cause', gbd_round_id= 7, decomp_step='iterative', compare_version_id=como_version, 
                             cause_id=cvd_cause_ids, location_id=locid, year_id=2021, sex_id=3, age_group_id=27,
                             measure_id=c(1,5), metric_id=3)
query2020all <- rbind(query2020daly, query2020comp)
query2020all <-query2020all[, `:=` (val=val*100000, lower=lower*100000, upper=upper*100000)]
query2020all <-query2020all[, sex := paste0(sex, "s")]
query2020all$val <- format(round(query2020all$val, 1), nsmall = 1)

query2020all <- dplyr::rename(query2020all, Sex = sex)
query2020all <- dplyr::rename(query2020all, Age = age_group_name)

query2020count <- get_outputs(topic='cause', gbd_round_id= 7, decomp_step='iterative', compare_version_id=como_version, 
                              cause_id=cvd_cause_ids, location_id=locid, year_id=2021, sex_id=3, age_group_id=22,
                              measure_id=c(1,5), metric_id=1)
query2020count$valr <- as.numeric(round(query2020count$val, digits = 0))
query2020count$lowr <- as.numeric(round(query2020count$lower, digits = 0))
query2020count$upr <- as.numeric(round(query2020count$upper, digits = 0))

q2020count <- query2020count[,c(10,14,9,16,20)]
q2020count <- dplyr::rename(q2020count, Sex = sex)
q2020count <- dplyr::rename(q2020count, Age = age_group_name)
q2020count$Metric <- "Count"
q2020count <- dplyr::rename(q2020count, val = valr)
q2020all <- query2020all[,c(10,14,9,16,17)]
q2020all$Metric <- "Rate"
q2020count <- rbind(q2020all, q2020count)
causescvd <- cause_metadata[cause_metadata$cause_id %in% cvd_cause_ids]
cvdorder<- causescvd[,c(7,13)]
q2020count <- merge(q2020count, cvdorder)

q2020count$val <- gsub(" ", "", q2020count$val)
q2020count[is.na(q2020count)] ='Not Estimated'
q2020count$val <- gsub("NA", "Not Estimated", q2020count$val)
q2020count$val <- gsub("^0.0$", "<0.1", q2020count$val)
collapse_rows_df <- function(df, variable){
  
  group_var <- enquo(variable)
  
  df %>%
    group_by(!! group_var) %>%
    mutate(groupRow = 1:n()) %>%
    ungroup() %>%
    mutate(!!quo_name(group_var) := ifelse(groupRow == 1, as.character(!! group_var), "")) %>%
    select(-c(groupRow))
}


q2020count$metric2 <- q2020count$Metric


##########################################################################################################################
# Create Graphs
##########################################################################################################################

#########################################################
##########SDI SCATTER 
#plots sdi against countries which are labeled at the super region level. Highlights the region of interest
sdiscatter <- ggplot(globalsdi2, aes(x= SDI, y=mapvar2, col=super_region_name)) +
  geom_point(size = 3, alpha=.50)+
  geom_point(data = globalsdi2[region_name == regionname], aes(x=SDI, y=mapvar2, col= region_name), shape = 17, size = 2)+

  scale_colour_manual(breaks = c("Central Europe, Eastern Europe, and Central Asia", "High-income", "Latin America and Caribbean", 
                                 "North Africa and Middle East ", "South Asia ", "Southeast Asia, East Asia, and Oceania", "Sub-Saharan Africa", regionname),
                     values = c("palevioletred1", "gray73", "darkseagreen3", "darkgoldenrod1", "cyan2","mediumorchid", "burlywood3", "black"),
                     labels = c("Central Europe, Eastern Europe, and Central Asia", "High-income", "Latin America and Caribbean",
                                "North Africa and Middle East ", "South Asia ", "Southeast Asia, East Asia, and Oceania", "Sub-Saharan Africa", regionname))+
  labs(shape="Super Region", colour="Global Regions and Focus Sub-Region")+
  ylab(str_wrap("Mortality per 100,000",40))+
  xlab("Socio-Demographic Index")+
  theme_bw(base_size = 7)+
  theme(text=element_text(size=7))+
  theme(legend.key.height=unit(.3, "cm"))+
  theme(legend.position="bottom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  guides(color = guide_legend( override.aes = list(size = c(3,3,3,3,3,3,3,2), shape = c(16,16,16,16,16,16,16,17)), 
                               title.position="top", title.hjust = 0.5, nrow=4 ) )
plot(sdiscatter)

#save plot out to png 
ggsave(paste0(outdir, locid, "/", "Figure3.png"), plot=sdiscatter, height=4, width=6, dpi=300)

#########################################################
##########BAR CHART
bar1 <-dalyw_EU %>%
  ggplot(aes(x = reorder(str_wrap(location_name.x,24),-valrate,sum), y = valrate)) +
  geom_col(aes(fill = factor(reorder(cause_name.x, sort_order.y), levels = causenames)),width = 0.5,)+
  guides(fill = guide_legend(title = "Cardiovascular Diseases",ncol =1))+
  theme_bw(base_size=7)+
  theme(text=element_text(size=7))+
  theme(axis.title.x = element_blank(),
        axis.text.x=element_text(angle=45,hjust=1))+
  theme(legend.text=element_text(size=7)) +
  theme(legend.key.height= unit(.3, "cm")) +
  theme(legend.key.width = unit(.3, "cm"))+
  theme(legend.spacing.y = unit(.1, 'cm'),
        legend.text = element_text(margin = margin(r = 1, unit = 'cm')))+
  labs(y= "Age-Standardized DALYs per 100,000", x = "Country", title = "(A)")+
  scale_fill_manual(values=setcol)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title.position = "plot",)
  

plot(bar1)

####Risk stacked bar charts by type of risk###

#environmental risk
bar2 <- env_burd %>%
  filter(!acause %in% "cvd") %>%
  ggplot(aes(x = reorder(str_wrap(rei_name,20),valrate,sum), y = valrate)) +
  geom_col(aes(fill = factor(reorder(cause_name.x, sort_order.y), levels = causenames)), width = 0.7)+
  guides(fill = guide_legend(title = "CVD Cause"))+
  theme_bw(base_size=7)+
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = "none")+
  labs(y= "", x = "", title = "(B)                     Environmental Risks")+
  ylim(burdenmin, burdenmax)+
  coord_flip()+
  scale_fill_manual( values=setcol)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title.position = "plot",text=element_text(size=7))
  
plot(bar2)

#metabolic risk
bar3 <- met_burd %>%
  filter(!acause %in% "cvd") %>%
  ggplot(aes(x = reorder(str_wrap(rei_name,20),valrate,sum), y = valrate)) +
  geom_col(aes(fill = factor(reorder(cause_name.x, sort_order.y), levels = causenames)), width = 0.7)+
  guides(fill = guide_legend(title = "CVD Cause"))+
  theme_bw(base_size=7)+
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = "none")+
  labs(y= "Age-Standardized DALYs per 100,000", x = " ", title = "Metabolic Risks")+
  ylim(burdenmin, burdenmax)+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title=element_text(size=6),text=element_text(size=7))+
  scale_fill_manual( values=setcol)
plot(bar3)

#behavioral risk
bar4 <- beh_burd %>%
  filter(!acause %in% "cvd") %>%
  ggplot(aes(x = reorder(str_wrap(rei_name,20),valrate,sum), y = valrate)) +
  geom_col(aes(fill = factor(reorder(cause_name.y, sort_order.y), levels = causenames)), width = 0.7)+
  guides(fill = guide_legend(title = "CVD Cause"))+
  theme_bw(base_size=7)+
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position = "none")+
  labs(y= "", x = " ", title = "Behavioral Risks")+
  ylim(burdenmin, burdenmax)+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),text=element_text(size=7))+
  scale_fill_manual( values=setcol)
plot(bar4)


#arrange bar1-bar4 for final plot
my_layout <- rbind(c(1, 1, 1,1,1,1), c(1,1,1,1,1,NA),c(2,2,3,3,4,4))
riskplot <- grid.arrange(grobs = list(bar1, bar2, bar3, bar4), layout_matrix = my_layout)
#save to image
ggsave(paste0(outdir, locid, "/", "Figure4.png"), plot=riskplot, height=5, width=8, dpi=300)

#########################################################
##########BOX AND WHISKER 

c25_2 <- c(
  "dodgerblue2", "#FF6666", # red
  "#339993",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2","darkorange4",  "yellow4", # lt pink
  "palegreen2",
  "#FDBF6F", # lt orange
  "gray70", "brown",
   "orchid1", "deeppink1", "steelblue4",
  "darkturquoise", "green1", "yellow3", "#FB9A99",
  "#CAB2D6", "khaki", "lightslateblue", "darksalmon", "magenta",
  "mediumblue", "plum", "mistyrose", "indianred"
  
)

#creat box and whisker plot for all cvd causes that compare 2010 and 2020 rates
p1 <- query3[year_id == 2021] %>%
  ggplot(aes(x=reorder(str_wrap(cause_name.x,30), -sort_order.y), y=valrate)) +
  geom_boxplot(outlier.shape=NA)+
  geom_point(aes(color = str_wrap(location_name.y, 20)),shape =17, size = 2)+
  coord_flip()+
  theme_bw(base_size=8)+
  scale_color_manual(values=c25_2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),text=element_text(size=8),
        legend.key=element_blank(),legend.background=element_blank())+
  theme(legend.position = c(0.75, 0.40),
        legend.text = element_text(size = 7.5))+
  theme(legend.spacing.y = unit(0.02, 'pt'))+
  labs(y="Age-Standardized Mortality Rate per 100,000", x="Cardiovascular Diseases", color = "Country Name")+
  guides(color=guide_legend(nrow=11,byrow=TRUE))
  p1

ggsave(paste0(outdir, locid, "/", "Figure5.png"), plot=p1, height=4, width=8, dpi=300)

#########################################################
##########PYRAMID PLOT
equal_breaks <- function(n = 3, s = 0.5, ...){
  function(x){
    # rescaling
    d <- s * diff(range(x)) / (1+2*s)
    seq(min(x)+d, max(x)-d, length=n)
  }
}

xmin <- -max(abs(query2020pyr$valrate))
xmax <- max(abs(query2020pyr$valrate))


pyrplot <- ggplot(query2020pyr, aes(fill = as.factor(year_id), y = valrate, x = reorder(age_group_name,sortage))) + 
  geom_bar(data=query2020pyr[sex == "Female"], position="dodge", stat = "identity") + 
  geom_bar(data=query2020pyr[sex == "Male"], position="dodge", stat = "identity") +
   scale_y_continuous(labels = abs, breaks = scales::pretty_breaks(5), expand = c(0.05, 0), limits= c(xmin, xmax))+
  coord_flip() + 
  scale_fill_manual(values=c("#9999FF", "#99CCCC"))+
  labs(title = "Female                                     Male", 
       x="Age Group", y="Cardiovascular Disease Mortality Rate per 100,000")+
  geom_hline(yintercept = 0, size=.25)+
  theme_bw() +
  theme(plot.title = element_text(size = 9),
        legend.position = "bottom",
        panel.spacing.x = unit(0, "pt"), 
        strip.background = element_rect(colour = "black"),
        legend.text=element_text(size=6),
        legend.key.size = unit(0.2, "cm"))+
  theme(text = element_text(size =9))+
  theme(legend.title=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.margin = margin(.2,.5,.2,.2, "cm"))+
  theme(plot.title = element_text(hjust = 0.5))
pyrplot


ggsave(paste0(outdir, locid, "/", "Figure2.png"), plot=pyrplot, height=4, width=4.5, dpi=300)

#########################################################
##########TABLE
library(dplyr)
table2 <- q2020count %>%
  mutate(cause_name = reorder(factor(cause_name), sort_order) )%>%
  unite(Metric_Measure, metric2, measure_name)%>%
  group_by(cause_name)%>%
  summarize(Metric_Measure, val)%>%
  spread(Metric_Measure, val)%>%
  collapse_rows_df(cause_name)%>%
  formattable()

table2$Count_Deaths <- as.character(table2$Count_Deaths)
table2$Count_Prevalence <- as.character(table2$Count_Prevalence)
table2$Rate_Prevalence <- as.character(table2$Rate_Prevalence)
table2[c("Count_Deaths", "Count_Prevalence", "Rate_Prevalence")][is.na(table2[c("Count_Deaths", "Count_Prevalence", "Rate_Prevalence")])] <- "Not Estimated"
table2 <- table2[,c(1,3,2, 6,5,4)]


names(table2) <- c("Cardiovascular Disease Type", "Prevalent Cases (Count)", "Deaths (Count)", 
                               "Prevalence (Rate)", "Deaths (Rate)", "Disability Adjusted Life Years (DALYs, Rate)")

table2 <- apply(table2, 2, function(x) prettyNum(x, big.mark = ","))

write.csv(table2, file = paste0(outdir, locid, "/", "Table1.csv"),row.names=FALSE)


