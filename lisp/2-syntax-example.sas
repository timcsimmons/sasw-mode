** sasclass.dfpm.utah.edu ;
** BOX PLOTS: Quantiles, but what are the bars?;
** the bottom and top stuff are outliers (extraordinary extreme;
** mistake): check outliers for quality;
** but the main lesson is outlier may or may not belong.;
******************************************************************;
*** FPMD 6101 Data Analysis Using SAS;
*** Lecture 9, September 22, 2010;
******************************************************************

Review homework questions and assignment for Monday - BRFSS

1	FREQ, tests of association
2 Date and time variables, processing and formats
3	working with date and time, functions;
	
Options /*center*/ nocenter /*date*/ nodate number /*nonumber*/ linesize=80 pagesize=120 
        pageno=1;

libname class 'c:\jim\sas10\classdata\';
libname wds 'c:\jim\wdsanalysis\';

*******************************************************************;
**** 9.1. PROC FREQ - measures of association;
*******************************************************************;

***********************************;
** DETOUR - levels of measurement;
**
** nominal: categories (e.g. 1=male 2=female);
**  no order;
** ordinal: there is a natural order by value;
**   (e.g. Likert scale) (scales need not be linear); 
** interval: equal distance between values, but no zero, e.g., dates;
** ratio: natural zeros ;
***********************************;

proc contents data=class.brfss; run;

proc freq  data=class.brfss ;

tables exerany2*genhlth ; */ nopercent nocol;
tables sex*exerany2 ;
run;



*** is there a relationship? 
** how would you know?;

*** what do we need to do first?;
** exerany2 looks nominal, but it measures an ordinal variable;
** ditto genhlth;
libname class '/home/tim/Documents/FPMD7107/';

data brfss2; set class.brfss;
    select (exerany2);
        when (1) exer = 1;
        when (2) exer = 2;
        when (7) exer = .a;
        when (9) exer = .b;
        otherwise exer = .;
    end;
    select (genhlth);
        when (1) hlth = 1;
        when (2) hlth = 2;
        when (3) hlth = 3;
        when (4) hlth = 4;
        when (5) hlth = 5;
        when (6) hlth = 6;
        when (7) hlth = .a;
        when (9) hlth = .b;
        otherwise hlth = .;
    end;

run;

proc freq;
    tables exerany2*exer genhlth*hlth / missing;
run;

** In the next bit, proc freq has chi-sq capability;
** \chi^2 = \sum \frac{(obs-exp)^2}{exp};
proc freq  data=brfss2 ;
tables exer*hlth /  expected nopercent nocol norow deviation chisq cellchi2 measures;
run;
** keyword measures gives extra stats at the end;
*** one way;
** Here we are testing for uniformity of distribution ;
proc freq  data=brfss2 ;
tables sex hlth / chisq;
run;

** _n_ is SAS var which counts the number of times we go through the
** data loop ;
** output says write the data before hitting run ;
** RMK : SAS executes all the sets, but it will not output beyond 99 ;

data little; set brfss2;
if _n_<100 then output;
run;


*****************************************************************;
*** detour to _n_ and OUTPUT;
*****************************************************************;

*** one way;
proc freq  data=little;
tables sex 	hlth / chisq;
run;

** For small numbers like this, use exact. DO NOT USE CHI, of course;
proc freq  data=little;
tables exer*hlth / expected deviation nopercent nocol norow chisq cellchi2 exact measures;
tables exer*sex / expected deviation  nopercent nocol norow chisq cellchi2 exact;
run;



******************************************************************;
**** 9.2. Date/Time variables;
******************************************************************;

* SAS date values: ;
** stored as numbers, number of days since 1/1/1960 (integer);
/* specifying a date constant, '06OCT2009'd (number: 3, string:
*'jim');*/
/*    ** example x = '13JUN2010'd; */

** SAS time values: # seconds since midnight of current day    0 to 86400;

*** SAS date-time values: # seconds from 1/1/1960;

data date;
z=2;

*** to assign dates;
x='26JUL1982'd;
y='08oct2009'd; /* 18187 days since 1/1/1960 */

run;

proc print;
run;
**** Formats
datew. dayw. downamew. dttmonyyw. e8601daw. juldayw.  ;

proc print; 
*datew. dayw. downamew. dtmonyyw. e8601daw. juldayw.  ;
format y downame12. x julday16. ; run;

proc print;
    format y date10. x day10.;
run;

proc print;
    format y date10. x e8601daw10.;
run;

proc print data=date;
format date mmddyy10. date2. date5.;
run;


**************************************************************************;
**** 9.3. Date/Time functions;
*************************************************************************************;

**** Dat/time functions;
** creating dates: mdy(m, d, y)

*** extracting parts of dates;
* day(x)
* weekday(x)
*month(x)
*Year(x)
* today()
* YRDIF  differnce in years;

data date;
do i=6 to 30 by 1;
date=today()+i;
date2=mdy(11,i,2009);
day1=day(date);
day2=weekday(date2);
mon=month(date);
yr=year(date2);
diff=yrdif(date2, date, 'Actual');
g=intck('week',date,'31dec2009'd);
h=intnx('qtr',today()+i,10);
output;
end;
run;

proc print data=date;
format date date2 mmddyy10. h date10.;
run;

*** intervals;
* INTCK number of intervals between two dates;
** INTCK(unit, start, end);

* INTNX date after set number of intervals
INTNX(unit, start, # intervals)

units are : year month qtr week weekday;


/*
************    HW 9 Due Wed Sept 29 with HW 7 and 8

1. Create histograms of the number of days exercised in last month (original variable) by gender 
using side-by-side bars or side-by-side graphs. 

2. Generate gender-specific boxplots of BMI.

3. Calculate the correlation between BMI and number of days of exercise, by gender. 
Plot these variables against one another. Interpret the results (1-2 sentences).

4. Using PROC FREQ describe the relationship between education (using your variable created above) 
and BMI categories. Is there a statistically significant association?

5. Using PROC Tabulate, create an easily-interpretable table showing the mean and 
std dev of BMI in a way that allows the reader to assess gender differences in BMI at each level of income.

6. (7101 students only) Please write up the results as if you are writing a paper focused on BMI and 
the relationship with gender, income and exercise and education. Refer to the results by Table or figure #. 
Make it sound like it is in a journal. If you feel that there are  other analyses that must be done to address 
this issue, then please conduct them. 

See handout for format of homework.
*/

