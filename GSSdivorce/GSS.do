#delimit ;

   infix
      year     1 - 20
      divlaw   21 - 40
      cohort   41 - 60
using GSS.dat;

label variable year     "Gss year for this respondent                       ";
label variable divlaw   "Divorce laws";
label variable cohort   "Year of birth";


label define gsp001x
   9        "No answer"
   8        "Don't know"
   3        "Stay same"
   2        "More difficult"
   1        "Easier"
   0        "Not applicable"
;
label define gsp002x
   9999     "No answer"
   0        "Not applicable"
;


label values divlaw   gsp001x;
label values cohort   gsp002x;


