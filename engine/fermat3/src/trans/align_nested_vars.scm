;;; Scheme translation of WSL code
;Doni Pracner 2016
;This transformation tries to align nested VAR blocks into
;one if possible. It only looks at blocks directly next to
;one another. It will stop looking further if it finds a
;same variable in a lower block. It needs to be called
;on a VAR statement.
(define (@Align_Nested_Vars_Test)
 (cond
  ((not (= (@ST (@I)) //T_/Var))
   (@Fail "Current item not a VAR block"))
  ((not (= (@ST (@Get_n (@Get_n (@I) 2) 1)) //T_/Var))
   (@Fail "Current item not a nested VAR block"))
  ((not (null? (intersection-n (@Assigned (@Get_n (@I) 1)) (@Assigned (@Get_n (@Get_n (@Get_n (@I) 2) 1) 1)))))
   (@Fail "The two VARs have variables in common"))
  (#t
   (@Pass))))

(define (@Align_Nested_Vars_Code //Data)
 (cond
  ((= (@ST (@I)) //T_/Var)
   (let ((/start '())
         (/other '())
         (//L '())
         (/cont 1)
         (/v1 '())
         (/v2 '()))
    (while (= /cont 1) 
     (begin
      (set! /cont 0)
      (set! /start (@Posn))
      (@Down)
      (set! /v1 (@Assigned (@I)))
      (@Right)
      (@Down)
      (cond
       ((= (@ST (@I)) //T_/Var)
        (set! /other (@Posn))
        ;get assigns
        (@Down)
        (@Down)
        ; to assigns 
        ;check if the variables are not the same in the blocks
        (cond
         ((null? (intersection-n (@Assigned (@I)) /v1))
          (set! /cont 1)
          (@Cut_Rest)
          (set! //L (@Buffer))
          (@Cut)
          ;move
          (@Goto /start)
          (@Down)
          (@Down_Last)
          (@Splice_After //L)
          (@Paste_After (@Buffer))
          ;get statements and move
          (@Goto /other)
          (@Down_To 2)
          (@Down)
          (@Cut_Rest)
          (set! //L (@Buffer))
          (@Cut)
          (@Goto /start)
          (@Down_To 2)
          (@Down_Last)
          (@Splice_After //L)
          (@Paste_After (@Buffer))
          ;remove the now empty VAR block
          (@Goto /other)
          (@Delete)
          (@Goto /start)))))))))))

#t
