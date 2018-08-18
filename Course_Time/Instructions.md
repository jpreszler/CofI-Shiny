## Description
This app uses enrollment data from the College of Idaho to display the number of classes occuring at a given time and the average enrollment in those classes. The average enrollment is the total enrollement of all classes being counted divided by the number of classes.

## Instructions
You may select the buildings, subjects, years (as *graduation* years, so 2017 would include Fall 2016 and Spring 2017), and day combinations. Multiple selections are allowed, but for Subjects to be used you must select the "Subjects" radio button (instead of "Divisions").

The number of classes, and total enrollment, are computed for each group by either adding or subtracting a course's information to a running total depending on whether the timestamp is a "start time" or "end time" for the course.

The **plot** tab shows the plots of course counts and average enrollment. The **data** tab shows the data-set used to construct the plot so specific courses and enrollments can be seen.

### Author
Code on [GitHub](https://github.com/jpreszler/CofI-Shiny/Course_Time/). Created by Jason Preszler, Aug. 2018. If you encounter problems, find errors, or would like additional features please submit a github issue, or contact me via email.
