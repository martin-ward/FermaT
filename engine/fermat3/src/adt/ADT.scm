;;; Scheme translation of WSL code
;
;==========================================================================
;FermaT Transformation System
;Copyright (C) 2001 Software Migrations Limited.
;Email: martin@gkc.org.uk
;
;This program is free software; you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation; either version 3 of the License, or
;(at your option) any later version.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;==========================================================================
; Functions which define the MetaWSL ADT 
; NOTE: these do _no_ syntax checking (for efficiency - when called by 
; a transformation, it is the transformation's responsibility to 
; ensure syntactic correctness). 
; TODO: Write some syntax-checking versions of the movement and selection 
; functions, which can be linked in for testing purposes 
; TODO: Compare the effect of storing adt_Posn and adt_Path_Items as shared lists, 
; unshared (destructively updated) lists and arrays. 
; Arrays would need to be rebuilt if they got too big! 
; It is the WSL to Lisp translators job to translate specified 
; WSL functions and procedures into Lisp macros. 
; Macros are: @Size, @Get_n, @Spec_Type, @ST, @Gen_Type, @GT, 
; @Value, @V, @Components, @Cs, @Components?, @Cs?, @Parent, @Posn, 
; @Posn_n, @Data, @Buffer, @Make, @Name, @Make_Name, 
; @Right?, @Right, @Left?, @Left, @Up?, @Up, @Down?, @Down, 
; @Down_To, @Down_Last, @To, @To_Last, 
; @Dtable_Get, @Dtable_Value_Part, @Dtable_Put 
; NB: The translator should also optimise the condition NOT EMPTY?(foo) 
; to FOO and EMPTY?(foo) to (NULL? FOO) 
; Functions which return the value of an internal variable should simply translate 
; to a direct access of the variable 
; @Make_Name(''xxx'') should be optimised at compile time -- replace by 'xxx 
; <elt> ++ list should be optimised to (cons elt list) 
; This has to be set here, rather than in date_find.wsl, 
; so that it doesn't get clobbered when date_find.src is dynamically loaded 
; into gambit.  It is set by the calling script when needed. 
(set! //Options_/File "options")
; *** Internal data *** 
; The structure of a WSL item is either: 
; CONS(dtable, CONS(spec_type, value)) OR: 
; CONS(dtable, CONS(spec_type, component_list)) 
; NB only this file `knows' about this structure, if we change 
; the structure and change this file, then everything else should 
; still work (without even noticing!). 
; NB an item can have a value OR components, but not both. 
; In other words, only atoms have a value. 
(set! /adt_/Program '())
(set! /adt_/Posn '())
; TAIL(REVERSE(@Posn)) 
(set! /adt_/Posn_n 0)
; LAST(@Posn) (=0 if @Posn is empty) 
; Currently selected item: 
(set! /adt_/Item '())
; @Get(@Program, @Posn) 
; = @Program if @Posn = < > 
; HEAD(adt_Path_Items)^HEAD(adt_Posn) otherwise 
; Ancestors of the selected item: <Parent, Grandparent, ...> 	
; These are the items encountered after each step on the path 
; from @Posn to the root: 
(set! /adt_/Path_/Items '())
; <@Get(@Program, BUTLAST(@Posn)), 		
;  @Get(@Program, BUTLAST(BUTLAST(@Posn))), 	
;  ...,					
;  @Get(@Program, < >) >			
; LAST(adt_Path_Items) = @Program		
; LENGTH(adt_Path_Items) = LENGTH(@Posn)	
; @Parent = HEAD(adt_Path_Items)		
; ??? Store the list of components to the right of adt_Item ??? 
; This makes moving @Right efficient, as well as the @Right? test! 
; Note that currently, the loop: DO ... IF @Right? THEN @Right ELSE EXIT(1) FI OD 
; is O(n^2) where n is the number of elements in the list. 
; If we want to make @Left efficient as well, then we need to keep a (reversed) 
; list of items to the left of adt_Item 
; @Cs(@Parent) = REVERSE(adt_Left) ++ <adt_Item> ++ adt_Right 
(set! /adt_/Program_/History '())
(set! /adt_/Posn_/History '())
(set! /adt_/Command_/History '())
(set! /adt_/Program_/Future '())
(set! /adt_/Posn_/Future '())
(set! /adt_/Command_/Future '())
(set! /adt_/In_/A_/Trans 0)
(set! /adt_/Data '())
(set! /adt_/Buffer '())
(set! /adt_/Edit_/Program_/Stack '())
(set! /adt_/Edit_/Posn_/Stack '())
(set! /adt_/Edit_/To_/Stack '())
; *** Access Functions which are independent of internal data *** 
; See ADT2.wsl 
(define (@V1 //I)
 
 (@V (@Get_n //I 1)))

; *** Access Functions which use internal data *** 
(define (@Program)
 
 /adt_/Program)

(define (@Item)
 
 /adt_/Item)

(define (@I)
 
 /adt_/Item)

(define (@Parent)
 
 (car /adt_/Path_/Items))

(define (@GParent)
 
 (car (cdr /adt_/Path_/Items)))

(define (@GGParent)
 
 (car (cdr (cdr /adt_/Path_/Items))))

(define (@Posn)
 
 (if (= /adt_/Posn_n 0) '() (reverse (cons /adt_/Posn_n /adt_/Posn))))

(define (@Posn_n)
 
 /adt_/Posn_n)

(define (@Command_List)
 
 /adt_/Command_/List)

(define (@Data)
 
 /adt_/Data)

(define (@Buffer)
 
 /adt_/Buffer)

(define (@Span)
 
 0)

; *** Creation Functions *** 
;
;MW_FUNCT @Make(ST, value, comps) == : SKIP; (<< >, ST, value> ++ comps) END;
;
(define (@Name /value)
 
 (@Make //T_/Name /value '()))

; Convert a string to a symbol 
; @Make_Name -- see Macros.scm 
; *** Update Procedures *** 
; For the larger procedures there is a trade-off between: 
; macro definition - larger code size, avoids a function call; and 
; function definition - smaller code size, less efficient. 
(define (@Right?)
 
 (and (> /adt_/Posn_n 0) (< /adt_/Posn_n (@Size (car /adt_/Path_/Items)))))

(define (@Right)
 (set! /adt_/Posn_n (+ /adt_/Posn_n 1))
 (set! /adt_/Item (@Get_n (car /adt_/Path_/Items) /adt_/Posn_n)))

(define (@Left?)
 
 (> /adt_/Posn_n 1))

(define (@Left)
 (set! /adt_/Posn_n (- /adt_/Posn_n 1))
 (set! /adt_/Item (@Get_n (car /adt_/Path_/Items) /adt_/Posn_n)))

(define (@Up?)
 
 (> /adt_/Posn_n 0))

(define (@Up)
 (cond
  ((not (null? /adt_/Posn))
   (set! /adt_/Posn_n (car /adt_/Posn))
   (set! /adt_/Posn (cdr /adt_/Posn)))
  (#t
   (set! /adt_/Posn_n 0)))
 ; The new @Item is the old parent: 
 (set! /adt_/Item (car /adt_/Path_/Items))
 (set! /adt_/Path_/Items (cdr /adt_/Path_/Items)))

(define (@Up_To_Statement)
 (while (and (@Up?) (not (= (@GT /adt_/Item) //T_/Statement))) 
  (@Up)))

; We can move down if the current item has components: 
(define (@Down?)
 
 (@Cs? /adt_/Item))

(define (@Down)
 (@Down_To 1))

(define (@Down_Last)
 (@Down_To (@Size /adt_/Item)))

(define (@To /n)
 (@Up)
 (@Down_To /n))

(define (@To_Last)
 (@Up)
 (@Down_Last))

(define (@Down_To /n)
 (cond
  ((> /adt_/Posn_n 0)
   (set! /adt_/Posn (cons /adt_/Posn_n /adt_/Posn))))
 (set! /adt_/Posn_n /n)
 (set! /adt_/Path_/Items (cons /adt_/Item /adt_/Path_/Items))
 (set! /adt_/Item (@Get_n /adt_/Item /n)))

(define (@Goto /posn)
 (set! /adt_/Posn '())
 (set! /adt_/Path_/Items '())
 ; Walk down from the root, rebuilding adt_Path_Items from adt_Program 
 (set! /adt_/Item /adt_/Program)
 (while (and (not (null? /posn)) (@Cs? /adt_/Item)) 
  (begin
   (set! /adt_/Posn (cons (car /posn) /adt_/Posn))
   (set! /adt_/Path_/Items (cons /adt_/Item /adt_/Path_/Items))
   (set! /adt_/Item (@Get_n /adt_/Item (car /posn)))
   (set! /posn (cdr /posn))))
 (cond
  ((not (null? /adt_/Posn))
   (set! /adt_/Posn_n (car /adt_/Posn))
   (set! /adt_/Posn (cdr /adt_/Posn)))
  (#t
   (set! /adt_/Posn_n 0))))

(define (@Valid_Posn? //I /posn)
 
 (or (null? /posn) (and (>= (@Size //I) (car /posn)) (@Valid_Posn? (@Get_n //I (car /posn)) (cdr /posn)))))

; *** Editing Procedures *** 
; These will `cons up' a new program structure, built out of fragments 
; of the old one, plus possibly a new component. 
; @Cut and @Delete preserve adt_Posn even if the result is invalid. 
(define (@Cut)
 (set! /adt_/Buffer /adt_/Item)
 (@Delete))

(define (@Delete)
 (cond
  ((> /adt_/Posn_n 0)
   ; Build a new adt_Path_Items list (in reverse) and a new adt_Program 
   (let ((/parent (car /adt_/Path_/Items)))
    (set! /parent (@Make (@ST /parent) '() (@Delete_nth (@Cs /parent) /adt_/Posn_n)))
    (@Restore_Path_Items /parent)))
  (#t
   ; Delete everything: 
   (set! /adt_/Program '())
   (set! /adt_/Item '()))))

(define (@Restore_Path_Items /parent)
 (let ((/posn /adt_/Posn)
       (/new_path '()))
  (set! /new_path (cons /parent /new_path))
  (set! /adt_/Path_/Items (cdr /adt_/Path_/Items))
  (set! /adt_/Program /parent)
  (while (not (null? /posn)) 
   (begin
    (set! /parent (car /adt_/Path_/Items))
    (set! /parent (@Make (@ST /parent) '() (@Replace_nth (@Cs /parent) (car /posn) /adt_/Program)))
    (set! /posn (cdr /posn))
    (set! /new_path (cons /parent /new_path))
    (set! /adt_/Path_/Items (cdr /adt_/Path_/Items))
    (set! /adt_/Program /parent)))
  (set! /adt_/Path_/Items (reverse /new_path))
  (cond
   ((<= /adt_/Posn_n (@Size (car /adt_/Path_/Items)))
    (set! /adt_/Item (@Get_n (car /adt_/Path_/Items) /adt_/Posn_n)))
   (#t
    (set! /adt_/Item '())))))

; NB: Check if @Delete_nth/@Replace_nth are available in Scheme. 
; Return a new list with the nth element of the given list removed: 
; (may assume list is not empty and n >= 1) 
(define (@Delete_nth //L /n)
 
 (if (= /n 1) (cdr //L) (cons (car //L) (@Delete_nth (cdr //L) (- /n 1)))))

; Return a new list with the nth element of the given list replaced: 
; (may assume list is not empty and n >= 1) 
(define (@Replace_nth //L /n /v)
 
 (if (= /n 1) (cons /v (cdr //L)) (cons (car //L) (@Replace_nth (cdr //L) (- /n 1) /v))))

; Return a new list with the nth element of the given list replaced by a list: 
; (may assume list is not empty and n >= 1) 
(define (@Splice_nth //L /n //Lv)
 
 (if (= /n 1) (concat //Lv (cdr //L)) (cons (car //L) (@Splice_nth (cdr //L) (- /n 1) //Lv))))

; Return a new list with the value inserted before the nth element: 
(define (@Paste_Before_nth //L /n /v)
 
 (if (= /n 1) (cons /v //L) (cons (car //L) (@Paste_Before_nth (cdr //L) (- /n 1) /v))))

; Return a new list with the values inserted before the nth element: 
(define (@Splice_Before_nth //L /n //Lv)
 
 (if (= /n 1) (concat //Lv //L) (cons (car //L) (@Splice_Before_nth (cdr //L) (- /n 1) //Lv))))

; @Cut_Rest deletes everything to the right of the selected item, 
; preserving the selected item and the current position. 
; The sequence of deleted items is stored in adt_Buffer 
(define (@Cut_Rest)
 (set! /adt_/Buffer (nthcdr (@Posn_n) (@Cs (@Parent))))
 (@Delete_Rest))

; @Delete_Rest deletes everything to the right of the current item, 
; preserving the selected item and the current position. 
(define (@Delete_Rest)
 (cond
  ((> /adt_/Posn_n 0)
   ; Build a new adt_Path_Items list (in reverse) and a new adt_Program 
   (let ((/parent (car /adt_/Path_/Items)))
    (set! /parent (@Make (@ST /parent) '() (firstn /adt_/Posn_n (@Cs /parent))))
    (@Restore_Path_Items /parent)))
  (#t
   ; Delete everything: 
   (set! /adt_/Program '())
   (set! /adt_/Item '()))))

; @Paste_Over replaces the selected item with the given item: 
(define (@Paste_Over /new)
 (cond
  ((null? /new)
   (error "@Paste_Over called with empty item")))
 (cond
  ((eq? /new /adt_/Item)
   #t)
  ((> /adt_/Posn_n 0)
   ; Build a new adt_Path_Items list (in reverse) and a new adt_Program 
   (let ((/parent (car /adt_/Path_/Items)))
    (set! /parent (@Make (@ST /parent) '() (@Replace_nth (@Cs /parent) /adt_/Posn_n /new)))
    (@Restore_Path_Items /parent)))
  (#t
   ; Replace whole program 
   (set! /adt_/Program /new)
   (set! /adt_/Item /new))))

; @Splice_Over replaces the selected item with the given list of items: 
(define (@Splice_Over /new)
 (cond
  ((> /adt_/Posn_n 0)
   ; Build a new adt_Path_Items list (in reverse) and a new adt_Program 
   (let ((/parent (car /adt_/Path_/Items)))
    (set! /parent (@Make (@ST /parent) '() (@Splice_nth (@Cs /parent) /adt_/Posn_n /new)))
    (@Restore_Path_Items /parent)))
  (#t
   ; Replace whole program 
   (set! /adt_/Program /new)
   (set! /adt_/Item /new))))

; @Paste_Before inserts the given item before the selected item. 
; We may assume adt_Posn is non null. 
(define (@Paste_Before /new)
 (cond
  ((null? /new)
   (error "@Paste_Before called with empty item")))
 (let ((/parent (car /adt_/Path_/Items)))
  (set! /parent (@Make (@ST /parent) '() (@Paste_Before_nth (@Cs /parent) /adt_/Posn_n /new)))
  (@Restore_Path_Items /parent)))

(define (@Paste_After /new)
 (cond
  ((null? /new)
   (error "@Paste_After called with empty item")))
 (let ((/parent (car /adt_/Path_/Items)))
  (set! /parent (@Make (@ST /parent) '() (@Paste_Before_nth (@Cs /parent) (+ /adt_/Posn_n 1) /new)))
  (@Restore_Path_Items /parent)))

(define (@Splice_Before /new)
 (let ((/parent (car /adt_/Path_/Items)))
  (set! /parent (@Make (@ST /parent) '() (@Splice_Before_nth (@Cs /parent) /adt_/Posn_n /new)))
  (@Restore_Path_Items /parent)))

(define (@Splice_After /new)
 (let ((/parent (car /adt_/Path_/Items)))
  (set! /parent (@Make (@ST /parent) '() (@Splice_Before_nth (@Cs /parent) (+ /adt_/Posn_n 1) /new)))
  (@Restore_Path_Items /parent)))

; *** History and future procedures *** 
(define (@Clear_State)
 (set! /adt_/Program_/History '())
 (set! /adt_/Posn_/History '())
 (set! /adt_/Command_/History '())
 (set! /adt_/Program_/Future '())
 (set! /adt_/Command_/List '()))

(define (@Save_State)
 (set! /adt_/Program_/Future '())
 (set! /adt_/Program_/History (cons /adt_/Program /adt_/Program_/History))
 (set! /adt_/Command_/History (cons /adt_/Command_/List /adt_/Command_/History))
 (set! /adt_/Posn_/History (if (> /adt_/Posn_n 0) (cons (cons /adt_/Posn_n /adt_/Posn) /adt_/Posn_/History) (cons '() /adt_/Posn_/History))))

(define (@Undo)
 (cond
  ((not (null? /adt_/Program_/History))
   (set! /adt_/Program_/Future (cons /adt_/Program /adt_/Program_/Future))
   (set! /adt_/Posn_/Future (if (> /adt_/Posn_n 0) (cons (cons /adt_/Posn_n /adt_/Posn) /adt_/Posn_/Future) (cons '() /adt_/Posn_/Future)))
   (set! /adt_/Command_/Future (cons /adt_/Command_/List /adt_/Command_/Future))
   (set! /adt_/Program (car /adt_/Program_/History))
   ; @Goto will update the related variables: 
   (@Goto (reverse (car /adt_/Posn_/History)))
   (set! /adt_/Command_/List (car /adt_/Command_/History))
   (set! /adt_/Program_/History (cdr /adt_/Program_/History))
   (set! /adt_/Posn_/History (cdr /adt_/Posn_/History))
   (set! /adt_/Command_/History (cdr /adt_/Command_/History)))))

(define (@Undoable?)
 
 (not (null? /adt_/Program_/History)))

(define (@Redo)
 (cond
  ((not (null? /adt_/Program_/Future))
   (set! /adt_/Program_/History (cons /adt_/Program /adt_/Program_/History))
   (set! /adt_/Posn_/History (if (> /adt_/Posn_n 0) (cons (cons /adt_/Posn_n /adt_/Posn) /adt_/Posn_/History) (cons '() /adt_/Posn_/History)))
   (set! /adt_/Command_/History (cons (@Command_List) /adt_/Command_/History))
   (set! /adt_/Program (car /adt_/Program_/Future))
   ; @Goto will update the related variables: 
   (@Goto (car /adt_/Posn_/Future))
   (set! /adt_/Command_/List (car /adt_/Command_/Future))
   (set! /adt_/Program_/Future (cdr /adt_/Program_/Future))
   (set! /adt_/Posn_/Future (cdr /adt_/Posn_/Future))
   (set! /adt_/Command_/Future (cdr /adt_/Command_/Future)))))

(define (@Redoable?)
 
 (not (null? /adt_/Program_/Future)))

(define (@Save_Command /c)
 (set! /adt_/Command_/List (cons /c /adt_/Command_/List)))

(define (@Pop_Command)
 (set! /adt_/Command_/List (cdr /adt_/Command_/List)))

(define (@Initialise_ADT)
 (set! /adt_/Program_/History '())
 (set! /adt_/Posn_/History '())
 (set! /adt_/Command_/History '())
 (set! /adt_/Program_/Future '())
 (set! /adt_/Posn_/Future '())
 (set! /adt_/Command_/Future '())
 (set! /adt_/Command_/List '())
 (set! /adt_/Data '())
 (set! /adt_/Buffer '())
 (set! /adt_/Program (@Make //T_/Statements '() (list (@Make //T_/Stat_/Pat_/One '() '()))))
 (@Goto '()))

(define (@New_Program //I)
 (set! /adt_/Program //I)
 (@Goto '()))

(define (@Original_Program)
 
 (if (not (null? /adt_/Program_/History)) (last-1 /adt_/Program_/History) /adt_/Program))

; *** Dtable access and update *** 
; Access Functions: 
; Get the dotted pair stored under the given name in the dtable of the given item 
(define (@Dtable_Get //I /name)
 
 (@Assoc /name (car //I)))

; A function to return the value part of the result returned by @Dtable_Get: 
(define (@Dtable_Value_Part /result)
 
 (cdr /result))

; Update Procedures 
; Add the name value pair to the dtable of the given item. 
; This should not be called if a value is already stored under 
; that name (it is never necessary to update a value in a dtable: 
; if the item is edited, a new item is constructed with an empty dtable) 
(define (@Dtable_Put //I /name /value)
 (set_car //I (cons (cons /name /value) (car //I))))

(define (@prpr)
 (@Print_WSL (@Program) ""))

(define (@prit)
 (@Print_WSL (@Item) ""))

(define (@Print_WSL //I /indent)
 (cond
  ((@Has_Value_Type? (@ST //I))
   (display-list /indent (@Type_Name (@ST //I)) " " (@Value_String //I)))
  (#t
   (display-list /indent (@Type_Name (@ST //I)))
   (let ((/c (@Cs //I)))
    (while (not (null? /c)) 
     (begin
      (@Print_WSL (car /c) (concat /indent (if (= (@GT //I) //T_/Statement) ":  " "   ")))
      (set! /c (cdr /c))))))))

; Write a WSL item in raw Scheme format (suitable for an argument to @Match): 
(define (@Write_Raw_Item //I /port)
 (let ((/comp '()))
  (@Write "(() " /port)
  (@Write (@ST //I) /port)
  (cond
   ((@Has_Value_Type? (@ST //I))
    (cond
     ((not (null? (@V //I)))
      (@Write " . " /port)
      (cond
       ((or (= (@ST //I) //T_/String) (= (@ST //I) //T_/Comment))
        (@Write (concat (concat //Quote (@V //I)) //Quote) /port))
       ((or (= (@ST //I) //T_/Number) (= (@ST //I) //T_/Exit))
        (@Write (@V //I) /port))
       (#t
        (@Write (string-append (concat (concat (string-append ",(@Make_Name " //Quote) (@N_String (@V //I))) //Quote) ")") /port))))))
   (#t
    (for-in /comp (@Cs //I) 
     (begin
      (@Write " " /port)
      (@Write_Raw_Item /comp /port)))))
  (@Write ")" /port)))

; ============================================================================== 

