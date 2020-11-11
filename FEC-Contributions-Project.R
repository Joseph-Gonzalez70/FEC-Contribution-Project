#STA 141B 
#Joseph Gonzalez
# Assignment 1 Reading Rectangular/Tabular data---------------------------------

#Project Description: 
#This is a “real world” case study, using data from the US Federal Election Committee.

#The file I will analyze is the Federal ELection Commission's 2019-2020 individual contributions data file.
#URL: https://www.fec.gov/data/browse-data/?tab=bulk-data

#The Federal Election Commission describes the file as:
#"The individual contributions file contains each contribution from an individual to a federal committee.
#It includes the ID number of the committee receiving the contribution, the name, city, state, zip code, 
#and place of business of the contributor along with the date and amount of the contribution."

#After reading the "Data description for this file" link, I found that there are 21 fields in the data set. 
#For this assignment, the most important fields are Filer identification number, Contributor/Lender/Transfer Name,
#State, Transaction date, and Transaction amount.

#getwd()
#setwd("/Users/joseph_gonzalez/Desktop/STA 141B")

#File download:
#URL = "https://www.fec.gov/files/bulk-downloads/2020/indiv20.zip"
#file_dest = "/Users/joseph_gonzalez/Desktop/STA 141B/indiv20.zip"
#download.file(url=URL, destfile = file_dest)

library(XML)
library(RCurl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(magrittr)



#Read in the data:
#Since the data set is massive, I will explore with just a few lines:
contribution_data = read.table('itcont.txt', nrows = 1)
contribution_data = read.table('itcont.txt', sep = "|", nrows = 10)
field_classes = sapply(contribution_data, class)
field_classes[c(5, 11, 14, 19)] = "character"
field_classes = as.vector(field_classes)
#I also checked the total number of lines in the data using wc -l itcont.txt
#I found there is a total of 37387961 lines.

#From the first line, we see that '|' is used as a delimiter in the data.
#There also appears to be just a "" for values that are missing.
contribution_data = read.table('itcont.txt', sep = "|", nrows = 100)
contribution_data = read.table('itcont.txt', sep = "|", nrows = 300)
#contribution_data = read.table('itcont.txt', sep="|", nrows=800)

#When reading 800 rows of data we run into an error.
#The error identifies line 555 as not having 21 elements
#We must identify what the error is. First, I want to confirm that
#there are no issues with the delimiter "|":
contribution_data = readLines('itcont.txt', n = 650)
contribution_data[1] #Each observation is a string
contribution_data = strsplit(contribution_data, split = '|', fixed = TRUE)
sapply(contribution_data, length)
all(sapply(contribution_data, length)) 

#There doesn't appear to be a problem with the delimiter.
#Now, we can check if there is a problem with read.table identifying
#the number of fields. We can use count.fields() to count the number 
#of fields in the object
contribution_data = readLines('itcont.txt', n = 650)
field_count = count.fields(textConnection(contribution_data), sep = "|")
field_count

#Using count.fields(), we now see that there are issues with identifying
#the fields in many observations. 
(err_locate = which(is.na(field_count))[1])
(err_locate = which(is.na(field_count)))

#The error starts at line 30. I will compare line 30 to the previous line(29)
#and at least one line after(31,32,33 etc.). I will also look at any further
#entries as needed.
contribution_data[27:33]
contribution_data[38:40]
contribution_data[c(30,239)]

#After looking at the lines above a found a few details:
#For the lines that returned "NA" from the count.fields() function,
# 1) There doesn't appear to be an error in the data entry. In other words,
#they all appear to have 21 fields .
#2) Looking at the 30th line(Where the error starts), we can see that there is
#an apostrophe in the name "O'NEILL". Looking at the line where the error 
#stops, another apostrophe occurs in the same name "O'NEILL".
#My idea: I believe the error is occurring because R reads the apostrophe as a
#quotation and reads all the content in as one item unil it reaches the next 
#quote. I also saw the same occurrence with lines 563 to 564.
#When we read the data, we need to have it ignore the apostrophe.
contribution_data = readLines('itcont.txt', n = 650)
field_count = count.fields(textConnection(contribution_data), sep = "|", quote = "\"")
all(field_count == 21)
#Adjusting the quote argument appears to solve the issue:
contribution_data= read.table('itcont.txt', sep = "|", quote = "\"", colClasses = field_classes,  nrows = 800)
#No errors appear! 

#contribution_data = read.table('itcont.txt', sep = "|", quote = "\"", colClasses = field_classes,  nrows = 3000)
#After reading in more lines, another error occurs at line 2910:
contribution_data = readLines('itcont.txt', n = 3000)
field_count = count.fields(textConnection(contribution_data), sep = "|", quote = "\"")
all(field_count == 21)
which(field_count != 21)
field_count[2910] #says that it only read 12 fields in 2910
contribution_data[2910] 

#Looks like the '|' separates 21 fields. There is a '#' symbol that may be an issue
#and we can also check 2909 for any weird occurrences or special characters.
#After looking at the help table for read.table, I found that there is a 
#comment.char = "#". I will change this to see if it corrects the error.
contribution_data = read.table('itcont.txt', sep = "|", quote = "\"", colClasses = field_classes,  nrows = 3000, comment.char = "")
contribution_data[2907:2912,]
#The error appears to be resolved.

#Now, we see how much data we can load and decide if we need to split:
#contribution_data = read.table('itcont.txt', sep = "|", quote = "\"", colClasses=field_classes,  nrows = 7000000, comment.char = "")
rm(contribution_data)
#On my machine, I was successful reading in 7,000,000 observations.
#I will split the data at least 6 times. I used the command: split -l 7000000 itcont.txt in the terminal.
#The data was split into 6 files labeled: xaa, xab, xac, xad, xae, xaf.

#Get the column names reading from the CSV file:
URL_colnames = "https://www.fec.gov/files/bulk-downloads/data_dictionaries/indiv_header_file.csv"
#header_file_dest="/Users/joseph_gonzalez/Desktop/STA 141B/indiv_header_file.csv"
header_file_dest = paste(getwd(),"/indiv_header_file.csv", sep = "")
download.file(url = URL_colnames, destfile = header_file_dest)
colnames_1 = readLines("indiv_header_file.csv")
colnames_1 = strsplit(colnames_1, ",")
colnames_1 = colnames_1[[1]]

#Other way: Get the column names reading from the HTML in the “Data description for this file”:
#?XML::readHTMLTable
#library(XML)
#library(RCurl)
#URL_Table= htmlParse(getURLContent("https://www.fec.gov/campaign-finance-data/contributions-individuals-file-description/"))
#colnames_2= readHTMLTable(URL_Table, header=TRUE, as.data.frame=TRUE)
#colnames_2=colnames_2[[1]][,1]

#Before I start analyzing the data, I wanted to make sure R could read in the split data files correctly:
#Read_practice = read.table('xaf', sep = "|", quote = "", colClasses = field_classes, comment.char = "")
rm(err_locate, field_count, header_file_dest, URL_colnames)
#I had to disable the quote parameter completely for it to read xaa fully.
#I tested all file reads and no further errors occurred.

#---------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------
#PLOTS AND DATA ANALYSIS
#---------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------------------------
#Contributions by Date
#---------------------------------------------------------------------------------------------------------------------------------

#First, we will plot the number of contributions by DATE:
#My plan for this analysis:
# 1: read in each part of the full data set separately
# 2: subset the necessary information into a new data frame and remove the full data set(Hopefully helps with memory)
# 3: organize the data to get the necessary insights and return the insights in a data frame.
# I will try to display the number of contributions by date
# I understand "the number of contributions" as the number of times a contribution was made and
# not the total dollar amount of the contribution.

read_file = function(f_name) #f_name is just the name of the file
  {
    data_file = read.table(f_name, sep = "|", quote = "", colClasses = field_classes, nrows = 2387961, comment.char = "")
    return(data_file)
  }
data_file_names = c('xaa', 'xab', 'xac', 'xad', 'xae', 'xaf')
#I believe my code is capable to read in all the data.
#For this project, I decided to only read in the max observations from the smallest
#split for each data set. I made this decision to reduce run time.

contributions_by_date= function(file_name){
  full_data = read_file(file_name)
  colnames(full_data) = colnames_1
  data_date_trans = data.frame(TRANSACTION_DT = full_data$TRANSACTION_DT, 
                               TRANSACTION_AMT = full_data$TRANSACTION_AMT)
  rm(full_data)

  grouped_dates = data_date_trans %>%
                filter(TRANSACTION_AMT >= 0) %>%
                group_by(TRANSACTION_DT) %>%
                summarize(transact_count = n())
  
  grouped_dates$TRANSACTION_DT = mdy(grouped_dates$TRANSACTION_DT) #year-month-day
  
  trans_count_filtered = grouped_dates %>%
                       filter(TRANSACTION_DT >"2018-12-30", 
                              TRANSACTION_DT < "2020-11-01") 
                      #The filter dates were selected to capture the bulk of the observations and
                      #for graph interpretability.
  return(trans_count_filtered)
}

list_date_data = list()
i = 1 #index for the repeat loop
j = 1
repeat{
    if(i > length(data_file_names)){break}
    list_date_data[[j]] = contributions_by_date(data_file_names[i])
    j = j+1
    i = i+1
    }

#Combine the lists:
full_count_date = rbind(list_date_data[[1]], list_date_data[[2]],
                           list_date_data[[3]], list_date_data[[4]],
                           list_date_data[[5]], list_date_data[[6]])

#The code below is to group by dates that were(possibly) separated by the splits in data
#and, therefore, were unable to be group in the contributions_by_date function:
combined_count_date_data = full_count_date %>%
                          group_by(TRANSACTION_DT) %>%
                          summarise(contribution_count = sum(transact_count))

#Times series plot for date and count:
ggplot(combined_count_date_data, aes(TRANSACTION_DT, contribution_count)) + 
  geom_line() + 
  xlab("Dates(Month Year)") + 
  ylab("Number of Contribtuions") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "FEC Number of Contributions by Date For The 2020 Election",
       subtitle = "Contribution number movement from 2018 to present day") 


#---------------------------------------------------------------------------------------------------------------------------------
#Contributions By State
#---------------------------------------------------------------------------------------------------------------------------------

#PLot By State and by state per capita (i.e., adjust by the state population)
#We will include Washington DC into the analysis as state/region.
#Neccessary Function:
get_state_abbrev = function(url_states){
  URL_abbr = htmlParse(url_states)
  state_abbr_read = readHTMLTable(URL_abbr, header = TRUE, trim = TRUE)
  state_abbr_table = na.omit(as.matrix(state_abbr_read[[1]][3]))
  state_abbr_table = as.vector(state_abbr_table)
  state_abbr_table = state_abbr_table[4:53]
  return(c(state_abbr_table[1:8],'DC',state_abbr_table[9:50]))
}
get_URL_content_state = getURLContent("https://www.stateabbreviations.us")

#Preliminary analysis of states in itcont.txt:
Read_practice=read.table('xaf', sep = "|", quote = "", colClasses = field_classes, nrows = 500000, comment.char = "")
colnames(Read_practice) = colnames_1
Read_practice = data.frame(Read_practice$CITY, Read_practice$STATE, 
                           Read_practice$TRANSACTION_AMT)
n_distinct(Read_practice$Read_practice.STATE) #There are 60 distinct state abbreviations(and possibly a few more)
#Since there are 60, I want to see the specific names. My goal is to do just the 50 states plus D.C.
table(Read_practice$Read_practice.STATE)
blank_states = which(Read_practice$Read_practice.STATE=="")
Read_practice[blank_states[1:10],]
#The abbreviations include Guam, Micronesia, Northern Marianas, and Puerto Rico.
#Since voting procedures are different in these territories, I will not include these
#areas in  my analysis. I also discovered there are missing values for states.
#I will remove these items because the corresponding cities represent areas that
#are outside of the US or too vague to identify.
rm(Read_practice)

#Now, I will create a function that organizes the data by state, filters out any non-states(if necessary) and 
#counts the number of contributions:

contribution_by_state = function(file_name){
  full_data=read_file(file_name)
  colnames(full_data) = colnames_1
  data_state_trans=data.frame(STATE = full_data$STATE,
                              TRANSACTION_AMT = full_data$TRANSACTION_AMT)
  rm(full_data)
  state_abbr = get_state_abbrev(get_URL_content_state)
  grouped_states_cont_count = data_state_trans%>%
                            filter(STATE %in% state_abbr, TRANSACTION_AMT > 0) %>%
                            group_by(STATE) %>%
                            summarise(contrib_count = n())
  return(grouped_states_cont_count)
}

list_state_data = list()
i = 1 #index for the repeat loop
j = 1
repeat
  {
  if(i > length(data_file_names)){break}
    list_state_data[[j]] = contribution_by_state(data_file_names[i])
    j=j+1
    i=i+1
  }


full_count_state= rbind(list_state_data[[1]], list_state_data[[2]],
                       list_state_data[[3]], list_state_data[[4]],
                       list_state_data[[5]], list_state_data[[6]])


combined_count_state_data = full_count_state%>%
  group_by(STATE) %>%
  summarise(contribution_count = sum(contrib_count))

#Dot plot for number of contributions by state:
combined_count_state_data %>% 
        mutate(STATE= factor(STATE, 
              levels = sort(get_state_abbrev(get_URL_content_state), decreasing = TRUE))) %>% 
        ggplot(mapping = aes(STATE, contribution_count)) + 
        geom_point(colour = "blue", size = 3) +
        coord_flip() + 
        xlab("US States") + 
        ylab("Number of Contributions") +
        theme_minimal()+
        theme(axis.text.x = element_text(size=9),
              axis.text.y = element_text(size=9))+
          labs(title = "FEC Number of Contributions by State For The 2020 Election",
               subtitle = "CA has the most individual contributions") 
          

#---------------------------------------------------------------------------------------------------------------------------------
#Contributions By State Per Capita
#---------------------------------------------------------------------------------------------------------------------------------

#To plot the number of contributions by state per capita, we must adjust by state population.
#The following data is from the United states census bureau. From this data set, we can
#estimate the state population. The estimates reflect the population estimates for July 2019.
URL_states = "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/state/detail/SCPRC-EST2019-18+POP-RES.csv"
#file_dest_states="/Users/joseph_gonzalez/Desktop/STA 141B/SCPRC-EST2019-18+POP-RES.csv"
file_dest_states = paste(getwd(), "/SCPRC-EST2019-18+POP-RES.csv", sep = "")
download.file(url = URL_states, destfile = file_dest_states)

#Read in data from the file:
states_data = read.csv("SCPRC-EST2019-18+POP-RES.csv", header = TRUE, sep = ",")
#Going to remove Puertp Rico since it is not a state or part of the states:
names(states_data)
us_all = states_data[1,]
puerto_rico_ = states_data[53,]
states_data = states_data[2:52,]

#Since the contribution data set uses the state postal abbreviations, I am obtaining the 
#state postal abbreviations in alphabetical order according to the full state name
state_abbr_vect = get_state_abbrev(get_URL_content_state)
#This data frame was to confirm an accurate read:
state_pop_data = data.frame(states=states_data$NAME, Postal_abbrev = state_abbr_vect , population = states_data$POPESTIMATE2019) 
#now we must organize alphabetically by the abreviations
state_pop_data = arrange(state_pop_data, state_pop_data$Postal_abbrev) 

#Number of contributions by state per capita: 
#To avoid small decimals, I will multiply the number by 10,000 to get the number
#of contributions per 100,000
count_per_cap = data.frame(states = combined_count_state_data$STATE,count_per_capita = ((combined_count_state_data$contribution_count/state_pop_data$population)*100000))


#With DC:
count_per_cap  %>% 
  mutate(states= factor(states, 
                       levels = sort(get_state_abbrev(get_URL_content_state), decreasing = TRUE))) %>% 
  ggplot(mapping = aes(states, count_per_capita)) + 
  geom_point(colour = "blue", size = 3) +
  coord_flip() + 
  xlab("US States") + 
  ylab("Number of Contributions Per 100,000 People") +
  theme_minimal() +
  theme(axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9)) +
  labs(title = "FEC Number of Contributions by State Per Capita For The 2020 Election",
       subtitle = "DC has the most contributions per 100,000 people") 

#Without DC:
count_per_cap[-8,]  %>% 
  mutate(states= factor(states, 
                        levels = sort(get_state_abbrev(get_URL_content_state), decreasing = TRUE))) %>% 
  ggplot(mapping = aes(states, count_per_capita)) + 
  geom_point(colour = "blue", size = 3) +
  coord_flip() + 
  xlab("US States") + 
  ylab("Number of Contributions Per 100,000 People") +
  theme_minimal() +
  theme(axis.text.x= element_text(size = 9),
        axis.text.y=element_text(size = 9)) +
  labs(title = "FEC Number of Contributions by State Per Capita For The 2020 Election",
       subtitle = "State only Contributions Per Capita are comparative") 



