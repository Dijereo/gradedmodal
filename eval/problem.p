%------------------------------------------------------------------------ 
%----All (hu)men are created equal. John is a human. John got an F grade. 
%----There is someone (a human) who got an A grade. An A grade is not 
%----equal to an F grade. Grades are not human. Therefore there is 
%----another human created equal to John. 
fof(all_created_equal,axiom,( 
    ! [H1,H2] : 
      ( ( human1(H1) 
         & human1(H2) ) 
     => created_equal(H1,H2) ) )). 

fof(john,axiom,( 
    human1(john) )). 

fof(john_failed,axiom,( 
    grade(john) = f )). 

fof(someone_got_an_a,axiom,( 
    ? [H] : 
      ( human1(H) 
      & grade(H) = a ) )). 

fof(distinct_grades,axiom,( 
    a != f )). 

fof(grades_not_human,axiom,( 
    ! [G] : ~ human1(grade(G)) )). 

fof(someone_not_john,conjecture,( 
    ? [H] : 
      ( human1(H) 
      & created_equal(H,john)
      & H != john ) )). 
%-------------------------------------------------------------------- 
