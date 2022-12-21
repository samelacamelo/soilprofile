# Overall package usage
The user activates the R package, selects the desired option (for the present work, only one selectable option was implemented), inserts a CSV file that is read by the R system embedded in the R Studio suite, and then processed. During file processing, the system looks for assets in the root folder, which will be used to assemble the final report. At the end of the execution, .png and .html files are generated that can be consulted by the user.


# Usage 
## Option selection
The first interaction once the package is loaded prompts the user to select which functionality they want to use from the package. This structure was created to facilitate the development of future features. In the present work, only the first functionality is executable. The others inform the user that the functionality is not yet implemented and the execution of the package is terminated.

## CSV file choose
For the insertion of the CSV file in the package routine, the native library “file” was used, which allows the user to select any file from his computer.

## CSV file especification
To ensure the correct interpretation of the data, the following assumptions must be obeyed by the user:
The CSV file must have the .csv extension and be saved with UTF-8 encoding, to avoid interpretation problems with accents and special characters.
The header of the CSV file must contain the columns soil, description, equipment, soil_moisture_condition and textural_class; followed by as many columns as necessary with the ground depth value, with the decimal places separated by a dot.
The values, both in the header and in the lines, must be separated by a comma.

## File to dataframe

Soon after, it is necessary to load the chosen file into memory, and interpret it in a native R data structure called Dataframe. This structure allows working with data in table format, and has resources that facilitate the analysis of this type of data, such as ordering columns and selecting cell values.
The Dataframe is cleaned and separated into parts. One part only contains measurement values. An empty Dataframe is created that stores the result of the calculations for each line of the original file. After that, a repetition structure is created that iterates over each row of the Dataframe.

## Loop through each dataframe row
For each line of the Dataframe, the functions that calculate and draw the graphics on screen are called. At the end of each interaction, the results are saved in a temporary Dataframe, which is merged with the main Results Dataframe.
A separate file has been created that contains all the calculation functions for each of the numerical integration methods. As the files belong to the same package, it is not necessary to import them manually. All mathematical calculations were created for this package, it is not necessary to import external packages to carry out these calculations.
All calculation functions receive as an input parameter the series of values corresponding to the X and Y axes present in a line of the original CSV file. Where X is the depth of the measurement point in the soil and Y is the soil moisture value read by the sensor. At the end of each calculation, the native “plot” function of R is used, which draws the graph on the screen. The code snippet responsible for drawing the figures has no direct relationship with the calculation snippet that determines the area.

## Mean value theorem

For the function using the mean value theorem, all the Y values were summed and divided by the number of elements in the series of values. An initial point was artificially added, with a value equal to X[0], according to the assumption that the first measurement point and the soil surface have the same amount of water (LIBARDI, 2018).

## Trapezoid rule
For the function using the trapezoid rule, individual trapezoids were calculated for each section in the X and Y intervals, and the areas obtained were summed. To facilitate the visualization of the method, the polygons were drawn with interspersed colors. The first polygon has been painted in a highlighted color to illustrate that it is a suppository data.

## Simpson rule
For the function using Simpson's rule of one third, individual sections were calculated at intervals every three points of X, and the areas obtained were summed. To facilitate the visualization of the method, the polygons were drawn with interspersed colors.
If the number of soil moisture points as a function of depth is greater than three and even, a polygon is created at the end, with a highlighted color, corresponding to the section of points X[n] and X[n-1], the area of this last polygon is calculated using the trapezoid rule and, finally, added to the other previously calculated areas (LIBARDI, 2018).

## Spline method

For the function that uses the spline method, the process is different from the other three. In it, the X and Y data are used in the “splinefun” function, from the “splinefun” library, available in the CRAN repository. The function takes arbitrary points on the x and y axes, and returns a function of R that represents a mathematical function equivalent to the given points.
Next, the “integrate” function, native to R, is used, which calculates the definite integral between two limits of a given function. The result of the definite integral is returned, corresponding to the area under the curve of the mathematical function.

## Results append

At the end of the calculation of the water content profile with the four methods, and after the generation of the four images, the R package changes the content of the model html file, present in the “assets” table, and adds the dynamic content obtained after the calculations .
An html table is generated, corresponding to the Dataframe containing the result of the water content profile with the four methods, for each row corresponding to each measurement field. The html table is added to the end of the report.
Finally, the R package runs a routine that opens the newly generated html file in the user's web browser.

# HTML Report
To visualize the data provided, as well as the result of the calculations obtained through the package, a web report was created for the user. Through this web interface, the user has access to the table with the data inserted for analysis, the graphs and the results of the storage of water in the soil. As soon as the R package routine finishes, the results report opens in the user's browser. The report features the following sections:

## Header
The header has the logo and title of the package, and the logo of the Federal Institute of Bahia.

## Dataset preview
This section presents a table of user-supplied input data in the .csv file. Through CSS (Cascading Style Sheets) resources, it was possible to create a behavior that allows individual mouse scrolling within the table, keeping the header fixed, improving the user's reading experience.

## Individual probe points
For each measurement point, a section of individual points is generated in the report. Each section contains the following elements: Soil type, soil description, equipment used, soil moisture condition and textural class. Each text field is populated with the value corresponding to the user-supplied data in the .csv file.
Below the text fields, a division of images is drawn, with the graphics obtained during the calculation of each mathematical method. Below each image, the result of each corresponding calculation is drawn, with values in meters and millimeters.

## Results
This section presents a table with the consolidated results of the analysis. The table has the following columns: index (the measurement point identifier number, or line number), soil description, soil moisture condition, and a column for each calculated method. For the presentation of the results, five decimal places are considered for water storage values, with the meter as the unit of measurement. The behavior that allows individual mouse scrolling was also applied to this table. Using the html anchor technique, it was possible to create a link in the index column that allows navigating to the selected point.

## Footer
The footer has the author's name and a link to the official website of IF Baiano.