###########################################################################
# 1. Create a new SQL DB straight from R 
#(note that the code my_db <- dbConnect(RSQLite::SQLite(), "my_db.db")) 
#both creates a new database and connects to it! 
#No need to open SQLite to even create the DB); 
# 
# 2. Create all the tables and enforce relationship between them;
# 
# 3. Import data files into R;
# 
# 4. Plug data into tables. 
# 
# If you don't have data because you have not collected them yet, 
#stop at step 2.
# 
# Submit the R script (.R file) you used to build your database by 
#pushing it to your project repository. 
###########################################################################
library(DBI)

#make database
field_db <- dbConnect(RSQLite::SQLite(),
                      "field.db")


#create all tables and enforce relationships
dbExecute(field_db, "CREATE TABLE cover (
          plot_id varchar(3) NOT NULL,
          seed_mix varchar(2) NOT NULL,
          density char(1) NOT NULL,
          block char(1) NOT NULL,
          date text,
          PHAU_cover varchar(2),
          DISP_cover varchar(2),
          EUOC_cover varchar(2),
          SCAM_cover varchar(2),
          BICE_cover varchar(2),
          PRIMARY KEY (plot_id)
          );")

dbExecute(field_db, "CREATE TABLE biomass (
          sample_id varchar(3) NOT NULL,
          plot_id varchar(3) NOT NULL,
          PHAU_biomass float,
          DISP_biomass float,
          EUOC_biomass float,
          SCAM_biomass float,
          BICE_biomass float,
          PRIMARY KEY (sample_id)
          FOREIGN KEY (plot_id) REFERENCES cover(plot_id)
          );")

dbExecute(field_db, "CREATE TABLE functional_group (
          group_id char(1) NOT NULL,
          seed_mix varchar(2) NOT NULL,
          grp varchar(20),
          PRIMARY KEY (group_id)
          FOREIGN KEY (seed_mix) REFERENCES cover(seed_mix)
          );")

#import data files into R and plug into tables
####NOTE: For this assignment, I am only using a subset of my data as a practice.
#This subset of my data is a new CSV called fb_sql
cover <- read.csv("/Users/elanafeldman/Documents/USUClasses/Thesis_Code/Field22/fb_sql.csv")

cover$plot_id <- 1:nrow(cover) #create the plot_id

head(cover)
cover <- cover[,c("plot_id", "Group", "Density", "Block", 
                  "Date", "PHAU", "DISP", "EUOC", "SCAM", "BICE")]
names(cover)[2:10] <- c("seed_mix", "density", "block", "date", 
                       "PHAU_cover", "DISP_cover", "EUOC_cover", 
                       "SCAM_cover", "BICE_cover")

dbWriteTable(field_db, "cover", cover, append = TRUE)

dbGetQuery(field_db, "SELECT * FROM cover LIMIT 10;")

####NOTE: I do not have my biomass data yet, so I will leave that table unpopulated

functional_group <- read.csv("/Users/elanafeldman/Documents/USUClasses/Thesis_Code/Field22/functional_group.csv")

functional_group$group_id <- 1:nrow(functional_group)

head(functional_group)
functional_group <- functional_group[,c("group_id", "seed_mix", "grp")]

dbWriteTable(field_db, "functional_group", functional_group, append = TRUE)

dbGetQuery(field_db, "SELECT * FROM functional_group LIMIT 10;")






