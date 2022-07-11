# Code used in the NEAIR workshop

# all necessary packages in a script should be loaded at the top
library(tidyverse)

# read_csv is for csv files; the readxl package is useful for Excel files

faculty <- read_csv("faculty.csv")
courses <- read_csv("courses.csv")

# 2. Basic data manipulation----
# filter----
# keeps or discards rows (aka observations)

# the `==` operator tests for equality

faculty %>%
  filter(dept1 == "Sociology")

# the `|` operator signifies "or"

faculty %>%
  filter(dept1 == "Sociology" |
           dept1 == "Physics")

# the `%in%` operator allows for multiple options in a list

faculty %>%
  filter(dept1 %in% c("Sociology",
                      "Physics",
                      "Music"))

# the `&` operator combines conditions

faculty %>%
  filter(dept1 %in% c("Sociology","Physics","Music")
         & rank == "Professor")

# select----
# keeps or discards columns (aka variables)

faculty %>%
  select(id, dept1, rank)

# use - to drop columns

faculty %>%
  select(-dept2)

# can use everything() to rearrange

faculty %>%
  select(id, everything())

# pipe multiple functions together

faculty %>%
  select(id, dept1, rank) %>%
  filter(rank == "Professor")

# arrange----
# sorts data set by certain variable(s)

# use desc() to get descending order

courses %>%
  arrange(desc(enrollment))

# can include multiple variables

courses %>%
  arrange(dept, desc(enrollment))

# count----
# tallies data set by certain variable(s)

# can use sort = TRUE to order results

courses %>%
  count(dept)

courses %>%
  count(dept, level, sort = TRUE)

# mutate----
# creates new variables

# use a single `=` to create new variables

faculty %>%
  mutate(new = "hello!")

# ifelse works if it's a simple conditional

faculty %>%
  mutate(prof = ifelse(rank == "Professor",
                       1, 0))

# the `!` operator means "not" and is.na() identifies null values as T/F

faculty %>%
  mutate(joint = ifelse(!is.na(dept2),
                        "joint", NA))

# case_when is a better alternative to ifelse when there are multiple conditions

faculty %>%
  mutate(division = case_when(dept1 %in% c("Sociology","Political Science") ~
                                "Social Sciences",
                              dept1 %in% c("Music","English") ~
                                "Humanities",
                              dept1 %in% c("Chemistry","Physics") ~
                                "Sciences"))

# group_by/summarize----
# aggregates data (like pivot tables!)

# group_by identifies the grouping variable(s) and summarize specifies the aggregation

courses %>%
  group_by(dept, semester) %>%
  summarize(enr = sum(enrollment))

# useful aggregation options: mean/median, sd, min/max, n

courses %>%
  group_by(dept, semester) %>%
  summarize(enr = sum(enrollment),
            count = n_distinct(course_id))

# 4. More data manipulation----
# stringr functions----
# functions from stringr (which all start with str_) are useful for working with text data

faculty %>%
  filter(str_detect(rank, "Professor"))

courses %>%
  mutate(year = str_c(str_sub(semester, 1, 4),
                      "-",
                      str_sub(semester, 5, 6))) %>%
  select(semester, year) %>%
  unique()

# pivoting data----

faculty %>%
  pivot_longer(dept1:dept2,
               names_to = "dept_no",
               values_to = "dept",
               values_drop_na = TRUE) %>%
  select(-year, -rank)

courses %>%
  pivot_wider(names_from = "level",
              values_from = "enrollment")

# joining data----

courses_UG <- courses %>%
  filter(level == "UG") %>%
  mutate(year = str_c(str_sub(semester, 1, 4),
                      "-",
                      str_sub(semester, 5, 6))) %>%
  group_by(year, faculty_id) %>%
  summarize(enr = sum(enrollment))

fac_enr <- faculty %>%
  left_join(courses_UG, by = c("id" = "faculty_id",
                               "year" = "year")) %>%
  group_by(year, rank) %>%
  summarize(avg_enr = mean(enr, na.rm = TRUE))

# 5. Data visualization----
# bar chart----

faculty %>%
  count(rank) %>%
  ggplot(aes(x = rank, y = n)) +
  geom_bar(stat = "identity")

faculty %>%
  count(rank) %>%
  ggplot(aes(x = reorder(rank, -n), y = n)) +
  # the fill argument specifies a constant color
  geom_bar(stat = "identity", fill = "#cc0000") +
  # ensures the bars touch the x-axis
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  # adds a label of the n value
  geom_text(aes(label = n), vjust = -0.5) +
  # removes the x and y axis titles and adds a plot title
  labs(x = NULL, y = NULL,
       title = "Count of faculty by rank, 2018-2021") +
  # uses a built-in theme
  theme_linedraw() +
  # removes some grid lines and the axis ticks
  theme(panel.grid.major.x = element_blank(),
        axis.ticks = element_blank())

# line graph----

fac_enr %>%
  filter(!is.na(avg_enr)) %>%
  ggplot(aes(x = year, y = avg_enr, group = rank, color = rank)) +
  geom_line()

fac_enr %>%
  filter(!is.na(avg_enr)) %>%
  ggplot(aes(x = year, y = avg_enr, group = rank, color = rank)) +
  geom_line() +
  # adds another layer of points for emphasis
  geom_point() +
  # changes the color scale for the color aesthetic (for rank)
  scale_color_brewer(type = "qual", palette = "Dark2") +
  # removes the x-axis title and adds titles for plot and y-axis
  labs(x = NULL, y = "Average enrollment",
       title = "Average undergraduate enrollment per rank over time") +
  # uses a built-in theme
  theme_linedraw() +
  theme(panel.grid.major.x = element_blank(),
        axis.ticks = element_blank(),
        # removes the standard axis title
        legend.title = element_blank(),
        # ensures the legend backgrounds are transparent
        legend.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA),
        # so the legend can be moved onto the plot
        legend.position = c(0.85, 0.82))
