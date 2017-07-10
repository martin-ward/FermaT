;;; Scheme translation of WSL code
(define (/foreach-reverse_all_ifs-1 //Depth //A/S_/Type)
 (cond
  ((@Trans? //T/R_/Reverse_/If)
   (@Trans //T/R_/Reverse_/If ""))))

(define (@Reverse_All_Ifs_Test)
 (@Pass))

(define (@Reverse_All_Ifs_Code //Data)
 (@Foreach_Statement /foreach-reverse_all_ifs-1 0 (@AS_Type) 0)
 (cond
  ((null? (@Program))
   (@New_Program (@Skips)))))

#t
