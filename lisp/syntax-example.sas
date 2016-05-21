/****************************************************************************
* program:  Puffs in CINC and CINL ADMIN ONLY!! 2014 06 19.sas
* Author:   Tomo Funai
* Project:
* Start Date:
* updates:
* Description:
* Dependencies or Macro Calls:
* Reviewed by: 
* Date:
*****************************************************************************;
* Revisions: A revision block must be completed for each change post Code QC
*****************************************************************************;
* Programmer: 
* Date: 
* Reason for Change: 
* QC Completed By (Date): 
****************************************************************************/

* xyz;
* Heres is another SAS comment ;
* Here is nother comment;

options linesize=80;

** And this is another comment;

proc tabulate data=x;
    class x;
    var z;
    table x*z, n;
    format x wkl.;
run;

proc fcmp outlib=sasuser.funcs.trial;

   function study_day(intervention_date, event_date);

      n=event_date - intervention_date;
         if n >= 0 then
            n=n + 1;
         return(n);
   endsub;
run;


libname _all_ clear;



libname bob "path to bob";

filename sue "path to sue";

data der.x;
run;

data _null_;
    put "Hi there";
    x = a*b;
run;

proc ttest data=x;
run;


proc print data=sashelp.cars(obs=10);
    var Invoice;
run;

proc sql;
    create table work.bob as
    select *
    from sue
        left join work.x
            on sue.id = x.id
            and sue.name = x.name
    where a = 1
    group by x
    having count(*) > 2
    ;

    drop table bob
    ;

    create view x;
    drop view x;
quit;

/* ----- some sample code ----- */

%include %unquote(&open.gtestit&close);

filename xyz printer "HP Laserjet IIISi" altdet='c:\out.prn';
data tut1(abc) tut2;
     input dept  acct  qtr mon budget;
     length dept 8;
     drop dept;
     rename acct = account;
cards;
01  01345  1 1 12980
01  01345  1 2 14009
;
run;

%macro test;
%do i = 1 %to 10;
    %* This is a nasty macro comment statement;
    %put Iteration &i;
    %let x = %sysfunc(countw(a b c));
    put _n_;
%end;
%mend;

* this is a nasty comment statement;

proc tabulate data=tut1 order=data out = bob;
     class qtr mon dept acct;
     classlev qtr / style=[paddingleft=0.25in];
     var budget; ** Here is another nasty comment;
     table all='total'*f=9.
     	   dept=' ' * (acct='Sub-Total')*f=9.,
	   ((mon='Monthly Expenditures' all='Quarterly Total')
	   	all='Year To Date Total')*budget=' '*sum=' ' /
	box='Department and Account Number'
    ;
run;
