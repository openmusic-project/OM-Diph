;;;======================
;;; SOurce code of the OM-Diph library
;;; by Jean Lochard
;;;======================

(in-package diph)


;************************************************* Diphonescript *********************************


;****************************************
;;;; objet DiphCreateseq
;****************************************


(om::defmethod!  diph::DiphCreateseq ((nomseq string))

                 :initvals '("MySequenceName") 
                 :indoc '("Name OR list of names of the sequence to create")
                 :doc "Generate script to create new sequences windows."
                 :icon 128
                 (list (list (concatenate 'string "CreateSequence \"" nomseq "\"")))
                 )

(om::defmethod! diph::DiphCreateseq ((listseq list))
                (mapcar #'(lambda (elmt) (list elmt))
                        (mapcar #'(lambda (nomseq) (concatenate 'string "CreateSequence \"" nomseq "\"")) (remove-duplicates listseq :test 'equalp))
                        )
                )

;****************************************
;;;; objet DiphCreateDico
;****************************************


(om::defmethod!  diph::DiphCreateDico ((nomdic string))

:initvals '("MyDictionaryName") 
:indoc '("Name OR list of names of dictionaries to create")
:doc "Generate script to create new dicos."
:icon 128


  (list (list(concatenate 'string "CreateDictionary \"" nomdic "\"")))
)

(om::defmethod!  diph::DiphCreateDico ((listdic list))

(mapcar #'(lambda (elmt) (list elmt))
        (mapcar #'(lambda (nomdic) (concatenate 'string "CreateDictionary \"" nomseq "\"")) (remove-duplicates listdic :test 'equalp))
)
)

;****************************************
;;;; objet DiphOpendico
;****************************************


(om::defmethod!  diph::DiphOpendico ((listdico list))

:initvals '("MyDico.dico") 
:indoc '("Name of a dictionary OR list of dictionaries")
:doc "Generate script to open the <listdico> dictionaries in Diphone."
:icon 128

(mapcar #'(lambda (elmt) (list elmt))(mapcar #'(lambda (undico) (concatenate 'string "OpenDictionary \"" (namestring om::*diphone-folder*) "Dico&Seq/" undico "\"")) (remove-duplicates listdico :test 'equalp))
)
)

(om::defmethod!  diph::DiphOpendico ((undico string))
(list (list (concatenate 'string "OpenDictionary \"" (namestring om::*diphone-folder*) "Dico&Seq/" undico "\""))
))


;****************************************
;;;; objet DiphOpenSequence
;****************************************


(om::defmethod!  diph::DiphOpenSequence ((listseq list))

:initvals '("MySequence.seq") 
:indoc '("Name of a sequence OR list of sequences")
:doc "Generate script to open the sequence <seqname> in Diphone."
:icon 128

(mapcar #'(lambda (elmt) (list elmt))(mapcar #'(lambda (uneseq) (concatenate 'string "OpenSequence \"" (namestring om::*diphone-folder*) "Dico&Seq/" uneseq "\"")) (remove-duplicates listseq :test 'equalp))
)
)

(om::defmethod!  diph::DiphOpenSequence ((uneseq string))


(list (list (concatenate 'string "OpenSequence \"" (namestring om::*diphone-folder*) "Dico&Seq/" uneseq "\""))

))


;***************************************************************************************
;;;; objet DiphSave (les élémments sont sauvés au même endroit que l'application Diphone)
;***************************************************************************************


(om::defmethod!  diph::DiphSave ((listitem list))

:initvals '("MySeq") 
:indoc '("Name of a dictionary/sequence OR list of dictionaries/sequences")
:doc "Generate script to save the <listdico> dictionaries/sequences in Diphone"
:icon 128

(mapcar #'(lambda (elmt) (list elmt))(mapcar #'(lambda (undico) (concatenate 'string "Save \"" undico"\"")) (remove-duplicates listitem :test 'equalp))
)
)

(om::defmethod!  diph::DiphSave ((undico string))


(list (list (concatenate 'string "Save \"" undico "\""))

))

;***********************************************************
;;;; objet DiphSaveAs (ici, on donne le nom du fichier cible)
;***********************************************************

;on ajoute l'extension .seq au nom du dico à sauver, sinon message d'erreur avec "SaveAs"


(om::defmethod!  diph::DiphSaveAs ((listitem list) (path string))

:initvals '("MySeq" "use-diphone-folder")
:numouts 2 
:indoc '("Name of a dictionary/sequence OR list of dictionaries/sequences" "Path : if nil, it use the default diphone folder")
:doc "Generate script to save the <listdico> dictionaries/sequences in Diphone"
:icon 128

(if (equal "use-diphone-folder" path)
  (values
   (mapcar #'(lambda (elmt) (list elmt))
           (mapcar #'(lambda (undico) (concatenate 'string "SaveAs " undico " \"" (namestring om::*diphone-folder*) "Dico&Seq/" undico ".seq""\"")) listitem) 
         )

   (mapcar #'(lambda (undico) (concatenate 'string undico ".seq")) listitem)
         
   )

  (values
   (mapcar #'(lambda (elmt) (list elmt))(mapcar #'(lambda (undico) (concatenate 'string "SaveAs " undico " \"" path undico ".seq""\"")) listitem) 
         )

   (mapcar #'(lambda (undico) (concatenate 'string undico ".seq")) listitem)
         
   )
)
)


(om::defmethod!  diph::DiphSaveAs ((undico string) (path string))

(if (equal "use-diphone-folder" path)
  (values
   (list (list (concatenate 'string "SaveAs " undico " \"" (namestring om::*diphone-folder*) "Dico&Seq/" undico ".seq" "\"")))

   (list (concatenate 'string undico ".seq" ))

   )
  
  (values
   (list (list (concatenate 'string "SaveAs " undico " \"" path undico ".seq" "\"")))

   (list (concatenate 'string   undico ".seq" ))

   )
)
)


;****************************************
;;;; objet DiphCopyto
;****************************************


(om::defmethod!  diph::DiphCopyto ((listseg list) (nomseq string) (connectmode string) )

:menuins '( (2 (("InsertAfter" "InsertAfter") ("InsertBefore" "InsertBefore") ("FirstInSub" "FirstInSub") ("LastInSub" "LastInSub") )))
:initvals '( '("MyDico"("inst1"("Segm1" "Segm8" "Segm3") "Inst2" ("Segm1" "Seg5" "segm1"))) "my-seq-name.seq" "LastInSub") 
:indoc '("List of segments : (MyDico(inst1(Segm1 Segm8 Segm3) inst2 (Segm1 Seg5 segm1)))" "Name of the target sequence OR (MySeq(Sgm)) for InsertBefore and InsertAfter connect mode." "Connect mode")
:doc "Generate script to copy a list of segments <listseg> in the sequence window <nomseq> with <connectmode> method. <listseg> form is : (MyDico(inst1(Segm1 
Segm8 Segm3 ...) Inst2 (Segm1 Seg5 segm1 ...)))"
:icon 128

(if (listp (first listseg))
  (mapcar #'(lambda (elmt) (list elmt)) 
          (mapcar #'(lambda (unseg) (format () "CopyTo ~A ~A (~A)" (OM::num2string unseg) connectmode  nomseq) ) listseg)
  )
  

  (list (list (format () "CopyTo ~A ~A (~A)" (OM::num2string listseg) connectmode  nomseq)
  ))
)

)


;****************************************
;;;; objet DiphSegmentSet
;****************************************


;;; changer les paramètres de segments dans une séquence (avec les index des segments)


(om::defmethod!  diph::DiphSegmentSet ( (newvaluelist list) (segindex list) (segmentcarac string) (loopmode string))
:menuins '( (2 (("Length" "Len") ("Center" "Ctr") ("Next center" "Nxc") ("Inter Left Dur" "Ldi") ("Inter Right Dur" "Rdi") ("Scaler" "Sca") ))
           (3 (("No Loop" "noloop") ("Loop" "loop"))))
:initvals '('(0.5 1.0 2.0 1.0 1.0) '("MySeq" 10) "len" "noloop") 
:indoc '("New value OR list of new values" "(MySeq <number of segments in the sequences>) OR list of segment index : (MySeq (1 12 3 2 9 ...)))" "Set this parameter..." "Loop list of values OR not.")
:doc "Generate script to set parameters of segments placed in a sequence. <segindex> is a sequence name following by number of segments in the sequence : (MySeq 100) OR list of exact segment index in the sequence : (MySeq (5 2 8 9)).
Numbers will be replaced by #5, #2, #8, #9 according to index syntax of Diphone scripting."
:icon 128

(mapcar #'(lambda (elmt) (list elmt)) 

        (if (numberp (second segindex))

          (if (equal "noloop" loopmode) 

            (loop for x in newvaluelist
                  for i from 1  to (length newvaluelist)
                  collect (concatenate 'string "SegmentSet " segmentcarac " " (OM::num2string x) " (" (first segindex) "(#" (OM::num2string i ) "))" ))

            (loop for i from 1  to (second segindex)
                  collect (concatenate 'string "SegmentSet " segmentcarac " " (OM::num2string (nth (mod (- i 1) (length newvaluelist))  newvaluelist)) " (" (first segindex) "(#" (OM::num2string i) "))" ))
  
          )

          (if (equal "noloop" loopmode) 

            (loop for x in newvaluelist
                  for i from 1  to (length newvaluelist)
                  collect (concatenate 'string "SegmentSet " segmentcarac " " (OM::num2string x) " (" (first segindex) "(#" (OM::num2string (nth (- i 1) (second segindex))) "))" ))

            (loop for i from 1  to  (length (second segindex))
                  collect (concatenate 'string "SegmentSet " segmentcarac " " (OM::num2string (nth (mod (- i 1) (length newvaluelist) ) newvaluelist)) " (" (first segindex) "(#" (OM::num2string (nth (- i 1) (second segindex))) "))" ))
  
          )

        )
)

)

(om::defmethod!  diph::DiphSegmentSet ( (newvalue number) (segindex list) (segmentcarac string) (loopmode string))



        (if (numberp (second segindex))

            (list (concatenate 'string "SegmentSet " segmentcarac " " (OM::num2string newvalue) " (" (first segindex) "(#All))" ))
            (mapcar #'(lambda (elmt) (list elmt))
   
                    (loop for i from 1  to (length (second segindex))
                          collect (concatenate 'string "SegmentSet " segmentcarac " " (OM::num2string newvalue) " (" (first segindex) "(#" (OM::num2string (nth (- i 1) (second segindex))) "))" ))
           ) 
        )             
)

(om::defmethod!  diph::DiphSegmentSet ( (newvalue number) (seqname string) (segmentcarac string) (loopmode string))

            (list(list (concatenate 'string "SegmentSet " segmentcarac " " (OM::num2string newvalue) " (" seqname "(#All))" )))
                     
)




;****************************************
;;;; objet DiphSegmentAdd
;****************************************


;;; changer les paramètres de segments dans une séquence (avec les index des segments)


(om::defmethod!  diph::DiphSegmentAdd ( (newvaluelist list) (segindex list) (segmentcarac string) (loopmode string))
:menuins '( (2 (("Length" "Len") ("Center" "Ctr") ("Next center" "Nxc") ("Inter Left Dur" "Ldi") ("Inter Right Dur" "Rdi") ("Scaler" "Sca") ))
           (3 (("No Loop" "noloop") ("Loop" "loop"))))
:initvals '('(0.5 1.0 2.0 1.0 1.0) '("MySeq" 10) "len" "noloop") 
:indoc '("New value OR list of new values" "(MySeq <number of segments in the sequences>) OR list of segment index : (MySeq (1 12 3 2 9 ...)))" "Add value to this parameter..." "Loop list of values OR not.")
:doc "Generate script to add values to parameters of segments placed in a sequence. <segindex> is a sequence name following by number of segments in the sequence : (MySeq 100) OR list of exact segment index in the sequence : (MySeq (5 2 8 9)).
Numbers will be replaced by #5, #2, #8, #9 according to index syntax of Diphone scripting."
:icon 128

(mapcar #'(lambda (elmt) (list elmt)) 

        (if (numberp (second segindex))

          (if (equal "noloop" loopmode) 

            (loop for x in newvaluelist
                  for i from 1  to (length newvaluelist)
                  collect (concatenate 'string "SegmentAdd " segmentcarac " " (OM::num2string x) " (" (first segindex) "(#" (OM::num2string i ) "))" ))

            (loop for i from 1  to (second segindex)
                  collect (concatenate 'string "SegmentAdd " segmentcarac " " (OM::num2string (nth (mod (- i 1) (length newvaluelist))  newvaluelist)) " (" (first segindex) "(#" (OM::num2string i) "))" ))
  
          )

          (if (equal "noloop" loopmode) 

            (loop for x in newvaluelist
                  for i from 1  to (length newvaluelist)
                  collect (concatenate 'string "SegmentAdd " segmentcarac " " (OM::num2string x) " (" (first segindex) "(#" (OM::num2string (nth (- i 1) (second segindex))) "))" ))

            (loop for i from 1  to  (length (second segindex))
                  collect (concatenate 'string "SegmentAdd " segmentcarac " " (OM::num2string (nth (mod (- i 1) (length newvaluelist) ) newvaluelist)) " (" (first segindex) "(#" (OM::num2string (nth (- i 1) (second segindex))) "))" ))
  
          )

        )
)

)

(om::defmethod!  diph::DiphSegmentAdd ( (newvalue number) (segindex list) (segmentcarac string) (loopmode string))



        (if (numberp (second segindex))

            (list (concatenate 'string "SegmentAdd " segmentcarac " " (OM::num2string newvalue) " (" (first segindex) "(#All))" ))
            (mapcar #'(lambda (elmt) (list elmt))
   
                    (loop for i from 1  to (length (second segindex))
                          collect (concatenate 'string "SegmentAdd " segmentcarac " " (OM::num2string newvalue) " (" (first segindex) "(#" (OM::num2string (nth (- i 1) (second segindex))) "))" ))
           ) 
        )             
)

(om::defmethod!  diph::DiphSegmentAdd ( (newvalue number) (seqname string) (segmentcarac string) (loopmode string))

            (list (list (concatenate 'string "SegmentAdd " segmentcarac " " (OM::num2string newvalue) " (" seqname "(#All))" )))
                     
)


;****************************************
;;;; objet DiphSegmentMul
;****************************************


;;; changer les parametres de segments dans une sequence (avec les index des segments)


(om::defmethod!  diph::DiphSegmentMul ( (newvaluelist list) (segindex list) (segmentcarac string) (loopmode string))
:menuins '( (2 (("Length" "Len") ("Center" "Ctr") ("Next center" "Nxc") ("Inter Left Dur" "Ldi") ("Inter Right Dur" "Rdi") ("Scaler" "Sca") ))
           (3 (("No Loop" "noloop") ("Loop" "loop"))))
:initvals '((0.5 1.0 2.0 1.0 1.0) "MySeq" "len" "noloop") 
:indoc '("New value OR list of new values" "(MySeq <number of segments in the sequences>) OR list of segment index : (MySequence (1 12 3 2 9 ...)))" "Multiply this parameter by value..." "Loop list of values OR not.")
:doc "Generate script to multiply by values the parameters of segments placed in a sequence. <segindex> is a sequence name following by number of segments in the sequence : (MySeq 100) OR list of exact segment index in the sequence : (MySeq (5 2 8 9)).
Numbers will be replaced by #5, #2, #8, #9 according to index syntax of Diphone scripting."
:icon 128

(mapcar #'(lambda (elmt) (list elmt)) 

        (if (numberp (second segindex))

          (if (equal "noloop" loopmode) 

            (loop for x in newvaluelist
                  for i from 1  to (length newvaluelist)
                  collect (concatenate 'string "SegmentMul " segmentcarac " " (OM::num2string x) " (" (first segindex) "(#" (OM::num2string i ) "))" ))

            (loop for i from 1  to (second segindex)
                  collect (concatenate 'string "SegmentMul " segmentcarac " " (OM::num2string (nth (mod (- i 1) (length newvaluelist))  newvaluelist)) " (" (first segindex) "(#" (OM::num2string i) "))" ))
  
          )

          (if (equal "noloop" loopmode) 

            (loop for x in newvaluelist
                  for i from 1  to (length newvaluelist)
                  collect (concatenate 'string "SegmentMul " segmentcarac " " (OM::num2string x) " (" (first segindex) "(#" (OM::num2string (nth (- i 1) (second segindex))) "))" ))

            (loop for i from 1  to  (length (second segindex))
                  collect (concatenate 'string "SegmentMul " segmentcarac " " (OM::num2string (nth (mod (- i 1) (length newvaluelist) ) newvaluelist)) " (" (first segindex) "(#" (OM::num2string (nth (- i 1) (second segindex))) "))" ))
  
          )

        )
)

)



(om::defmethod!  diph::DiphSegmentMul ( (newvalue number) (segindex list) (segmentcarac string) (loopmode string))



        (if (numberp (second segindex))

            (list (concatenate 'string "SegmentMul " segmentcarac " " (OM::num2string newvalue) " (" (first segindex) "(#All))" ))
            (mapcar #'(lambda (elmt) (list elmt))
   
                    (loop for i from 1  to (length (second segindex))
                          collect (concatenate 'string "SegmentMul " segmentcarac " " (OM::num2string newvalue) " (" (first segindex) "(#" (OM::num2string (nth (- i 1) (second segindex))) "))" ))
           ) 
        )             
)
                    


(om::defmethod!  diph::DiphSegmentMul ( (newvalue number) (seqname string) (segmentcarac string) (loopmode string))

            (list (list (concatenate 'string "SegmentMul " segmentcarac " " (OM::num2string newvalue) " (" seqname "(#All))" )))
                     
)

;****************************************
;;;; objet DiphSegmentRatio
;****************************************


(om::defmethod!  diph::DiphSegmentRatio ((newvaluelist list) (segindex list) (loopmode string))
:menuins '( (2 (("No Loop" "noloop") ("Loop" "loop"))))
:initvals '( 2.0 '("MySeq" 10) "Loop") 
:indoc '("Ratio OR list of ratio" "(MySequence <number of segments in the sequences>) OR list of segment index : (MySequence (1 12 3 2 9 ...)))" "Loop list of values OR not.")
:doc "Generate script to multiply length, center position and interpolations durations by a ratio OR list of rotio, of segments placed in a sequence. <segindex> is a sequence name following by number of segments in the sequence : (MySeq 100) ; a single 0.0 value means that all the segments of the sequence will be be glue with no interleave between each one. OR list of exact segment index in the sequence : (MySeq (5 2 8 9)).
Numbers will be replaced by #5, #2, #8, #9 according to index syntax of Diphone scripting. It's also possible to give only the name of the sequence. Then, all the segments of the sequence will be glue."
:icon 128

(mapcar #'(lambda (elmt) (list elmt)) 

        (if (numberp (second segindex))

          (if (equal "noloop" loopmode) 

            (loop for x in newvaluelist
                  for i from 1  to (length newvaluelist)
                  collect (concatenate 'string "SegmentRatio " (OM::num2string x) " (" (first segindex) "(#" (OM::num2string i ) "))" ))

            (loop for i from 1  to (second segindex)
                  collect (concatenate 'string "SegmentRatio " (OM::num2string (nth (mod (- i 1) (length newvaluelist))  newvaluelist)) " (" (first segindex) "(#" (OM::num2string i) "))" ))
  
          )

          (if (equal "noloop" loopmode) 

            (loop for x in newvaluelist
                  for i from 1  to (length newvaluelist)
                  collect (concatenate 'string "SegmentRatio " (OM::num2string x) " (" (first segindex) "(#" (OM::num2string (nth (- i 1) (second segindex))) "))" ))

            (loop for i from 1  to  (length (second segindex))
                  collect (concatenate 'string "SegmentRatio " (OM::num2string (nth (mod (- i 1) (length newvaluelist) ) newvaluelist)) " (" (first segindex) "(#" (OM::num2string (nth (- i 1) (second segindex))) "))" ))
  
          )

        )
)
)

(om::defmethod!  diph::DiphSegmentRatio ( (newvalue number) (seqname string) (loopmode string))

            (list (list (concatenate 'string "SegmentRatio " (OM::num2string newvalue) " (" seqname  "(#All))" )))
             
)




;****************************************
;;;; objet DiphSegmentGlue
;****************************************


(om::defmethod!  diph::DiphSegmentGlue ((newvaluelist list) (segindex list) (loopmode string))
:menuins '( (2 (("No Loop" "noloop") ("Loop" "loop"))))
:initvals '( 0.0 '("MySeq" 10) "Loop") 
:indoc '("New value OR list of new values" "(MySequence <number of segments in the sequences>) OR list of segment index : (MySequence (1 12 3 2 9 ...)))" "Loop list of values OR not.")
:doc "Generate script to glue segments placed in a sequence. <segindex> is a sequence name following by number of segments in the sequence : (MySeq 100) ; a single 0.0 value means that all the segments of the sequence will be be glue with no interleave between each one. OR list of exact segment index in the sequence : (MySeq (5 2 8 9)).
Numbers will be replaced by #5, #2, #8, #9 according to index syntax of Diphone scripting. It's also possible to give only the name of the sequence. Then, all the segments of the sequence will be glue."
:icon 128

(mapcar #'(lambda (elmt) (list elmt)) 

        (if (numberp (second segindex))

          (if (equal "noloop" loopmode) 

            (loop for x in newvaluelist
                  for i from 1  to (length newvaluelist)
                  collect (concatenate 'string "SegmentGlue " (OM::num2string x) " (" (first segindex) "(#" (OM::num2string i ) "))" ))

            (loop for i from 1  to (second segindex)
                  collect (concatenate 'string "SegmentGlue " (OM::num2string (nth (mod (- i 1) (length newvaluelist))  newvaluelist)) " (" (first segindex) "(#" (OM::num2string i) "))" ))
  
          )

          (if (equal "noloop" loopmode) 

            (loop for x in newvaluelist
                  for i from 1  to (length newvaluelist)
                  collect (concatenate 'string "SegmentGlue " (OM::num2string x) " (" (first segindex) "(#" (OM::num2string (nth (- i 1) (second segindex))) "))" ))

            (loop for i from 1  to  (length (second segindex))
                  collect (concatenate 'string "SegmentGlue " (OM::num2string (nth (mod (- i 1) (length newvaluelist) ) newvaluelist)) " (" (first segindex) "(#" (OM::num2string (nth (- i 1) (second segindex))) "))" ))
  
          )

        )
)
)

(om::defmethod!  diph::DiphSegmentGlue ( (newvalue number) (segindex list) (loopmode string))

        (if (numberp (second segindex))

            (list (concatenate 'string "SegmentGlue " (OM::num2string newvalue) " (" (first segindex) "(#All))" ))
            (mapcar #'(lambda (elmt) (list elmt))
   
                    (loop for i from 1  to (length (second segindex))
                          collect (concatenate 'string "SegmentGlue "(OM::num2string newvalue) " (" (first segindex) "(#" (OM::num2string (nth (- i 1) (second segindex))) "))" ))
           ) 
        )             
)


(om::defmethod!  diph::DiphSegmentGlue ( (newvalue number) (seqname string) (loopmode string))

            (list (list (concatenate 'string "SegmentGlue " (OM::num2string newvalue) " (" seqname  "(#All))" )))
             
)  
           

;****************************************
;;;; objet DiphSegmentCoorMode
;****************************************


(om::defmethod! diph::DiphSegmentCoorMode ( (bpfcoormode string) (segindex list) )

:menuins '( (0 (("Elastic" "Elastic") ("Hard" "Hard") ("ElacticReverse" "ElacticReverse") ("HardReverse" "HardReverse") )))
:initvals '( "Elastic" "MySeq" )
:indoc '( "Coor Mode" "Name of a séquence OR list of segment index : (MySequence (1 12 3 2 9 ...)))")
:doc "Generate script to change the way of managing data in segments."
:icon 128


            (mapcar #'(lambda (elmt) (list elmt))
                    (loop for i from 1  to (length (second segindex))
                          collect (concatenate 'string "SegmentCoorMode " bpfcoormode " (" (first segindex) "(#" (OM::num2string (nth (- i 1) (second segindex))) "))" ))
            ) 
                     
)

(om::defmethod! diph::DiphSegmentCoorMode ( (bpfcoormode string) (seqname string)  )


            (list(list (concatenate 'string "SegmentCoorMode " bpfcoormode " (" seqname "(#All))" ))) 
)             

;****************************************
;;;; objet DiphSegmentShape
;****************************************


(om::defmethod! diph::DiphSegmentShape ((shapeindex number) (segindex list) )

:menuins '( (0 (("10-80-10" 0) ("25-50-25" 1) ("33-33-33" 2) ("40-20-40" 3) ("50-00-50" 4))))
:initvals '( 0 '"MySequence" ) 
:indoc '( "Shape Index" "\"MySequence\"  OR list of segment index : (MySequence (1 2 3 2 9 8 ...)))")
:doc "Generate script to apply a new shape on segments."
:icon 128


            (mapcar #'(lambda (elmt) (list elmt))

                    (loop for i from 0  to (- (length (second segindex)) 1)
                          collect

                            (concatenate 'string "SegmentShape #" (OM::num2string shapeindex) " (" (first segindex) "(#" (OM::num2string (nth i (second segindex))) "))" )
                          ) 
            )
)
                  


(om::defmethod! diph::DiphSegmentShape ( (shapeindex number) (seqname string)  )


            (list (list (concatenate 'string "SegmentShape #" (OM::num2string shapeindex) " (" seqname "(#All))" )))
)            

;****************************************
;;;; objet DiphSegmentArticul
;****************************************


(om::defmethod! diph::DiphSegmentArticul (  (articulindex number) (segindex list) )

:menuins '( (0 (("Linear" 1) ("Smooth" 2) ("Logarithmic" 3) ("Exponential" 4) ("2Smooth" 5) ("Truncate" 6) ("Medium" 7) ("TruncMed" 8) )))
:initvals '( 1 '"MySequence" ) 
:indoc '( "Articul Index" "\"MySequence\"  OR list of segment index : (MySequence (1 2 3 2 9 8 ...)))")
:doc "Generate script to change the interpolation mode between segments."
:icon 128


            (mapcar #'(lambda (elmt) (list elmt))

                    (remove  'nil
                    (loop for i from 0  to (- (length (second segindex)) 2)
                          when (= (nth (+ i 1) (second segindex)) (+ (nth i (second segindex)) 1))

                          collect
                         
                            (concatenate 'string "SegmentArticul #" (OM::num2string articulindex) " (" (first segindex) "(#" (OM::num2string (nth i (second segindex))) " #" (OM::num2string (nth (+ i 1) (second segindex))) "))" )
                          
                    )
                    :test 'equalp)
            )
                  
)

(om::defmethod! diph::DiphSegmentArticul ( (articulindex number) (seqname string)  )


            (list (list (concatenate 'string "SegmentArticul #" (OM::num2string articulindex) " (" seqname "(#All))" )))
)            

;****************************************
;;;; objet DiphSavedescription
;****************************************


(om::defmethod! diph::DiphSavedescription ((listdico list)(path string))

:initvals '(("MyDico1" "MyDico2") "use-diphone-folder") 
:indoc '("Name or list of dictionaries" "Name of the script file")
:doc "Generate script to open a dictionary and save a description of it in a text file. The name
of the description file is generated by adding a .desc extension to the original name of 
the dictionary.
" 
:icon 129

(if (equal "use-diphone-folder" path)

  (mapcar #'(lambda (elmt) (list elmt)) (OM::flat (list 
                                                     (DiphOpendico listdico)
                                                     (mapcar #'(lambda (nomdico) (concatenate 'string "SaveDescription " nomdico  " \"" (namestring om::*diphone-folder*) "OM_Diph/" nomdico ".desc""\"" )) (remove-duplicates listdico :test 'equalp))
                                               )
                                      )
  )

  (mapcar #'(lambda (elmt) (list elmt)) (OM::flat (list 
                                                     (DiphOpendico listdico)
                                                     (mapcar #'(lambda (nomdico) (concatenate 'string "SaveDescription " nomdico  " \"" path nomdico ".desc""\"" )) (remove-duplicates listdico :test 'equalp))
                                               )
                                      )
   )
)
)

(om::defmethod!  DiphSavedescription ((dico string) (path string))

(if (equal "use-diphone-folder" path)

  (mapcar #'(lambda (elmt) (list elmt)) (om::flat (list 
                                                      (DiphOpendico dico)
                                                      (concatenate 'string "SaveDescription " dico  " \"" (namestring om::*diphone-folder*) "OM_Diph/" dico ".desc""\"" )))
     )
  (mapcar #'(lambda (elmt) (list elmt)) (om::flat (list 
                                                      (DiphOpendico dico)
                                                      (concatenate 'string "SaveDescription " dico  " \"" path dico ".desc""\"" )))
     )

)
)

;************************************************* DiphoneMACRO *********************************


;****************************************
;;;; objet DiphBuildSequence
;****************************************


(om::defmethod!  diph::DiphBuildSequence ((listseg list) (seqname string))


:initvals '( ("MyDico"("inst1"("Segm1" "Segm8" "Segm3") "Inst2" ("Segm1" "Segm5" "Segm1"))) "my-seq-name.seq") 
:indoc '("List of segments : (MyDico(inst1(Segm1Segm8 Segm3) Inst2 (Segm1 Segm5 segm1)))" "Name of the new sequence")
:doc "Generate a complete script to build a sequence in Diphone. It opens the nedeed dictionaries and create the sequence <nomseq> window automaticaly. It copies a list of segments <listseg> in the target sequence <nomseq> with <connectmode> method. <listseg> form is : (MyDico(inst1(Segm1 
Segm8 Segm3 ...) Inst2 (Segm1 Seg5 segm1 ...)))"
:icon 130

(mapcar #'(lambda (elmt) (list elmt)) (OM::flat(list 
                                                     (DiphCreateseq seqname)
                                                     (if (listp (first listseg))
                                                       (DiphOpendico (mapcar #'(lambda (liste) (first liste)) listseg))
                                                       (DiphOpendico (first listseg))
                                                     )
                                                     (DiphCopyto listseg seqname "LastInSub")
                                      ))
)
)


;****************************************
;;;; objet DiphBuildRythme
;****************************************


(om::defmethod!  diph::DiphBuildRythme ((durlist list) (intensitylist list) (seqname string) (seqlength number) (loopmode string) (interdurlist list) (bpfcoormode string) (method number))
  :menuins '( (4 (("No Loop" "noloop") ("Loop" "loop")))
             (6 (("Elastic" "Elastic") ("Hard" "Hard") ("ElacticReverse" "ElacticReverse") ("HardReverse" "HardReverse") ))
             (7 (("Use sides" 1) ("Use centers" 2) ("Use centers & sides" 3))))
  :initvals '( '(0.5 1.0 2.0 1.0 1.0) '(0) "MySeq" 10 "loop" '(0.1 0.05 0.025) "Elastic" 1) 
  :indoc '("list of length values OR ChordSeq Or Voice" "list of scale values (0 means no change)" "MySeq" "number of segments in the sequences OR list of segment index : (1 12 3 2 9 ...)" "Loop list of values OR not." "Interpolation duration" "Coor mode" "Method used")
  :doc "Generate a complete script to apply a rythme AND (OR) intensity changes to a sequence, made with the <DiphBuildSequence> object."
  :icon 130

  (case method 
    
    (1
     
     (mapcar #'(lambda (elmt) (list elmt))
             
             (OM::flat(list
                       (DiphSegmentSet 0 (list seqname seqlength) "Ldi" loopmode)
                       (DiphSegmentSet 0 (list seqname seqlength) "Rdi" loopmode)
                       (DiphSegmentSet (+ (nth 0 durlist) (/ (nth 0 interdurlist) 2)) (list seqname (list 1)) "Len" loopmode)
                       (DiphSegmentSet (/ (+ (nth 0 durlist) (/ (nth 0 interdurlist) 2)) 2)  (list seqname (list 1)) "Ctr" loopmode)
                       (DiphSegmentSet (nth 0 interdurlist) (list seqname (list 1)) "Rdi" loopmode)
                       
                       (if (equal "noloop" loopmode) 
                         
                         
                         (loop 
                           for i from 1  to (- seqlength 1)
                           collect 
                           (list 
                            
                            (DiphSegmentSet  (+ (nth i durlist) (/ (+ (nth i interdurlist) (nth (- i 1) interdurlist)) 2)) (list seqname (list (+ i 1))) "Len" loopmode)
                            (DiphSegmentSet  (/ (+ (nth i durlist) (/ (+ (nth i interdurlist) (nth (- i 1) interdurlist)) 2)) 2) (list seqname (list (+ i 1))) "Ctr" loopmode)   
                            
                            (DiphSegmentSet (nth (- i 1) interdurlist) (list seqname (list (+ i 1))) "Ldi" loopmode)
                            
                            (DiphSegmentSet (nth i interdurlist) (list seqname (list (+ i 1))) "Rdi" loopmode)
                            (DiphSegmentSet (nth i intensitylist) (list seqname (list (+ i 1))) "Sca" loopmode)
                            
                            )
                           )
                         
                         (loop 
                           for i from 1  to (- seqlength 1)
                           collect 
                           (list 
                            
                            (DiphSegmentSet  (+ (nth (mod i (length durlist)) durlist) (/ (+ (nth (mod i (length interdurlist)) interdurlist) (nth (mod (- i 1) (length interdurlist)) interdurlist)) 2)) (list seqname (list (+ i 1))) "Len" loopmode)
                            (DiphSegmentSet  (/ (+ (nth (mod i (length durlist)) durlist) (/ (+ (nth (mod i (length interdurlist)) interdurlist) (nth (mod (- i 1) (length interdurlist)) interdurlist)) 2)) 2) (list seqname (list (+ i 1))) "Ctr" loopmode)   
                            
                            (DiphSegmentSet (nth (mod (- i 1) (length interdurlist)) interdurlist) (list seqname (list (+ i 1))) "Ldi" loopmode)
                            
                            (DiphSegmentSet (nth (mod i (length interdurlist)) interdurlist) (list seqname (list (+ i 1))) "Rdi" loopmode)
                            (DiphSegmentSet (nth (mod i (length intensitylist)) intensitylist) (list seqname (list (+ i 1))) "Sca" loopmode)
                            
                            )
                           )
                         
                         
                         
                         )
                       
                       (DiphSegmentGlue  0.0 seqname loopmode)
                       (DiphSegmentCoorMode bpfcoormode seqname)
                       
                       ))
             )
     )
    
    (2
     
     
     (mapcar #'(lambda (elmt) (list elmt))
             
             (OM::flat(list
                       (DiphSegmentSet 0 (list seqname seqlength) "Ldi" loopmode)
                       (DiphSegmentSet 0 (list seqname seqlength) "Rdi" loopmode)
                       (DiphSegmentSet (/ (+ (nth 0 durlist) (nth 0 interdurlist)) 2) (list seqname (list 1)) "Len" loopmode)
                       (DiphSegmentSet 0  (list seqname (list 1)) "Ctr" loopmode)
                       (DiphSegmentSet (nth 0 interdurlist) (list seqname (list 1)) "Rdi" loopmode)
                       
                       (if (equal "noloop" loopmode) 
                         
                         (loop 
                           for i from 1  to (- seqlength 1)
                           collect (list 
                                    
                                    (DiphSegmentSet  (/  (+ (nth (- i 1) durlist) (nth i durlist) (nth i interdurlist) (nth (- i 1) interdurlist)) 2 ) (list seqname (list (+ i 1))) "Len" loopmode)    
                                    (DiphSegmentSet  (+ (/ (nth (- i 1) interdurlist) 2) (/ (nth (- i 1) durlist) 2)) (list seqname (list (+ i 1))) "Ctr" loopmode)
                                    (DiphSegmentSet (nth (- i 1)  interdurlist) (list seqname (list (+ i 1))) "Ldi" loopmode)
                                    (DiphSegmentSet (nth i interdurlist) (list seqname (list (+ i 1))) "Rdi" loopmode)
                                    (DiphSegmentSet (nth i intensitylist) (list seqname (list (+ i 1))) "Sca" loopmode)
                                    )
                           )     
                         
                         
                         (loop 
                           for i from 1  to (- seqlength 1)
                           collect (list 
                                    
                                    (DiphSegmentSet  (/  (+ (nth (mod (- i 1) (length durlist)) durlist) (nth (mod i (length durlist)) durlist) (nth (mod i (length interdurlist)) interdurlist) (nth (mod (- i 1) (length interdurlist)) interdurlist)) 2 ) (list seqname (list (+ i 1))) "Len" loopmode)    
                                    (DiphSegmentSet  (+ (/ (nth (mod (- i 1) (length interdurlist)) interdurlist) 2) (/ (nth (mod (- i 1) (length durlist)) durlist) 2)) (list seqname (list (+ i 1))) "Ctr" loopmode)
                                    (DiphSegmentSet (nth (mod (- i 1) (length interdurlist)) interdurlist) (list seqname (list (+ i 1))) "Ldi" loopmode)
                                    (DiphSegmentSet (nth (mod i (length interdurlist)) interdurlist) (list seqname (list (+ i 1))) "Rdi" loopmode)
                                    (DiphSegmentSet (nth (mod i (length intensitylist)) intensitylist) (list seqname (list (+ i 1))) "Sca" loopmode)
                                    )
                           )
                         )
                       
                       (DiphSegmentGlue  0.0 seqname loopmode)
                       (DiphSegmentCoorMode "hard" seqname)
                       
                       
                       )
                      
                      
                      )
             
             )
     
     )
    
    (3
     
     
     (mapcar #'(lambda (elmt) (list elmt))
             
             (OM::flat(list
                       (DiphSegmentSet 0 (list seqname seqlength) "Ldi" loopmode)
                       (DiphSegmentSet 0 (list seqname seqlength) "Rdi" loopmode)
                       (DiphSegmentSet (+ (nth 0 durlist) (nth 1 durlist) (/ (nth 0 interdurlist) 2)) (list seqname (list 1)) "Len" loopmode)
                       (DiphSegmentSet (nth 0 durlist)  (list seqname (list 1)) "Ctr" loopmode)
                       (DiphSegmentSet (nth 0 interdurlist) (list seqname (list 1)) "Rdi" loopmode)
                       
                       (if (equal "noloop" loopmode) 
                         
                         (loop 
                           for i from 1  to (- seqlength 1)
                           collect (list 
                                    
                                    (DiphSegmentSet  (+ (nth (- (* 2 i) 1) durlist) (nth (* 2 i) durlist) (/ (+ (nth i interdurlist) (nth (- i 1) interdurlist)) 2 )) (list seqname (list (+ i 1))) "Len" loopmode)    
                                    (DiphSegmentSet  (+ (nth (- (* 2 i) 1) durlist) (/ (nth (- i 1) interdurlist) 2) ) (list seqname (list (+ i 1))) "Ctr" loopmode)
                                    (DiphSegmentSet (nth (- i 1)  interdurlist) (list seqname (list (+ i 1))) "Ldi" loopmode)
                                    (DiphSegmentSet (nth i interdurlist) (list seqname (list (+ i 1))) "Rdi" loopmode)
                                    (DiphSegmentSet (nth i intensitylist) (list seqname (list (+ i 1))) "Sca" loopmode)
                                    )
                           )     
                         
                         
                         (loop 
                           for i from 1  to (- seqlength 1)
                           collect (list 
                                    
                                    (DiphSegmentSet  (+ (nth (mod (- (* 2 i) 1) (length durlist)) durlist) (nth (mod (* 2 i) (length durlist)) durlist) (/ (+ (nth (mod i (length interdurlist)) interdurlist) (nth (mod (- i 1) (length interdurlist)) interdurlist) ) 2 )) (list seqname (list (+ i 1))) "Len" loopmode)    
                                    (DiphSegmentSet  (+ (nth (mod (- (* 2 i) 1) (length durlist)) durlist) (/ (nth (mod (- i 1) (length interdurlist)) interdurlist) 2) ) (list seqname (list (+ i 1))) "Ctr" loopmode)
                                    (DiphSegmentSet (nth (mod (- i 1) (length interdurlist)) interdurlist) (list seqname (list (+ i 1))) "Ldi" loopmode)
                                    (DiphSegmentSet (nth (mod i (length interdurlist)) interdurlist) (list seqname (list (+ i 1))) "Rdi" loopmode)
                                    (DiphSegmentSet (nth (mod i (length intensitylist)) intensitylist) (list seqname (list (+ i 1))) "Sca" loopmode)
                                    )
                           )
                         )
                       
                       (DiphSegmentGlue  0.0 seqname loopmode)
                       (DiphSegmentCoorMode bpfcoormode seqname)
                       (DiphSegmentCoorMode "hard" seqname)
                       
                       )
                             
                      )
             
             )
     
     )
    
    )
  )


(om::defmethod!  diph::DiphBuildRythme ((cseq om::chord-seq) (intensitylist list) (seqname string) (seqlength number) (loopmode string) (interdurlist list) (bpfcoormode string) (method number))

(let ((newvaluelist (om::x->dx (om::om/ (om::lonset cseq) 1000.0)))
        (intensitylist (om::om-scale (mapcar #'car (om::lvel cseq)) -1.0 +1.0 0 127)))
    
    (diph::DiphBuildRythme newvaluelist intensitylist seqname seqlength loopmode interdurlist bpfcoormode method))  
  
)

(om::defmethod!  diph::DiphBuildRythme ((cseq om::voice) (intensitylist list) (seqname string) (seqlength number) (loopmode string) (interdurlist list) (bpfcoormode string) (method number))
  (diph::DiphBuildRythme (om::objfromobjs cseq (make-instance 'om::chord-seq)) intensitylist seqname seqlength loopmode interdurlist bpfcoormode method))


;****************************************
;;;; objet DiphSeq&Rythme
;****************************************


(om::defmethod!  diph::DiphSeq&Rythme ((listseg list) (newvaluelist list) (intensitylist list) (seqname string) (loopmode string) (interdurlist list) (bpfcoormode string) (method number))
  :menuins '( (4 (("No Loop" "noloop") ("Loop" "loop")))
             (6 (("Elastic" "Elastic") ("Hard" "Hard") ("ElacticReverse" "ElacticReverse") ("HardReverse" "HardReverse") ))
             (7 (("Use sides" 1) ("Use centers" 2)("Use centers & sides" 3))))
  :initvals '( '("MyDico"("inst1"("Segm1" "Segm8" "Segm3") "Inst2" ("Segm1" "Segm5" "Segm1"))) '(0.5 1.0 2.0 1.0 1.0) '(-0.75 0 .25) "MySeq" "loop" '(0.1) "Elastic" 1) 
  :indoc '("List of segments : (MyDico(inst1(Segm1Segm8 Segm3) Inst2 (Segm1 Segm5 segm1)))" "list of length values OR ChordSeq OR Voice" "list of scale values (0 means no change)" "MySeq" "Loop list of values OR not." "Interpolation duration" "Coor mode" "Method for rythme")
  :doc "Generate a complete script to build a sequence in Diphone and apply a rythme AND (OR) intensity changes to the sequence."
  :icon 130

;(print loopmode)
(mapcar #'(lambda (elmt) (list elmt))
          
          (OM::flat(list

                        (DiphCreateseq seqname)
                        (if (listp (first listseg))
                          (DiphOpendico (mapcar #'(lambda (liste) (first liste)) listseg))
                          (DiphOpendico (first listseg)) 
                        )
                        (DiphCopyto listseg seqname "LastInSub")

                        (diph::DiphBuildRythme newvaluelist intensitylist seqname (DiphSequenceLength listseg) loopmode interdurlist bpfcoormode method)
                     
                        )
                   )
   )

)

(om::defmethod!  diph::DiphSeq&Rythme ((listseg list) (cseq om::chord-seq) (intensitylist list) (seqname string) (loopmode string) (interdurlist list) (bpfcoormode string) (method number) )
  
  (let ((newvaluelist (om::x->dx (om::om/ (om::lonset cseq) 1000.0)))
        (intensitylist (om::om-scale (mapcar #'car (om::lvel cseq)) -1.0 +1.0 0 127)))
    
    (diph::DiphSeq&Rythme listseg newvaluelist intensitylist seqname loopmode interdurlist bpfcoormode method))
    
    )

(om::defmethod!  diph::DiphSeq&Rythme ((listseg list) (cseq om::voice) (intensitylist list) (seqname string) (loopmode string) (interdurlist list) (bpfcoormode string) (method number))
  (diph::DiphSeq&Rythme listseg (om::objfromobjs cseq (make-instance 'om::chord-seq)) intensitylist seqname loopmode interdurlist bpfcoormode method))



 
;****************************************
;;;; objet DiphBuildTimeStretch
;****************************************


(om::defmethod!  diph::DiphTimeStretch ((scalefactor number) (segindex list) (bpfcoormode string) )
  :menuins '((2 (("Elastic" "Elastic") ("Hard" "Hard") ("ElacticReverse" "ElacticReverse") ("HardReverse" "HardReverse") )))
  :initvals '( 2.0 ' "MySeq" "Elastic") 
  :indoc '("Scale factor" "Name of a sequence OR (MySeq <number of segments in the sequences>) OR list of segment index : (MySeq (1 12 3 2 9 ...)))" "Coor mode")
  :doc "Generate a complete script to stretch a sequence OR a part of a sequence."
  :icon 130
  
  (mapcar #'(lambda (elmt) (list elmt))
          
          (OM::flat(list
                        (DiphSegmentRatio scalefactor segindex "loop")
                        (DiphSegmentGlue  0.0 segindex "loop")
                        (DiphSegmentCoorMode  segindex bpfcoormode)
                        )
          )
   )
)

(om::defmethod!  diph::DiphTimeStretch ((scalefactor number) (seqname string) (bpfcoormode string) )
 
  
  (mapcar #'(lambda (elmt) (list elmt))
          
          (OM::flat(list
                        (DiphSegmentRatio scalefactor seqname "loop")
                        (DiphSegmentGlue  0.0 seqname "loop")
                        (DiphSegmentCoorMode  seqname bpfcoormode)
                        )
                   )
          )
  )




;************************************************* DiphoneUTILITIES *********************************




;****************************************
;;;; objet GetDescription 27_03_2006
;****************************************

(om::defmethod! diph::GetDescription ((dico string))

:initvals '(("MyDico") "use-default-folder") 
:indoc '("Name of a dictionary")
:doc "Generate the description of a dictionnary and return the path to fill a textfile object." 
:icon 129

(diph::ExecuteDiphScript (diph::DiphSavedescription dico "use-diphone-folder"))

(concatenate 'string (namestring om::*diphone-folder*) "OM_Diph/" dico ".desc")
)

;****************************************
;;;; objet DiphExtract
;****************************************
 

(om::defmethod! diph::DiphExtract ( (descrfile list))
  
  
  :initvals '("Descriptionfile" 1) 
  :indoc '("Description file readed by a Textfile object.")
  :numouts 9
;;  :outdoc '("Dictionaru name" "Instruments list" "List of segments Name" "List of segments Lenght" "List of segments Center" "List of segments Next Center" "List of segments Interpolation Begin" "List of segments Interpolation End" "List of segments Scale")
  :doc "This object is used to extract informations from a description file loaded in an TextFile object. Choose what you want to extract with the outlets."
  :icon 129 
  
  (values (first (om::flat (cdr(cdr(cdr descrfile))) 1))

          (mapcar #'(lambda (uninstr) (first uninstr)) (cdr (om::flat (cdr(cdr(cdr descrfile))) 1) ))

          (mapcar #'(lambda (uninstr) 
                   (mapcar #'(lambda (unseg) (first unseg)) (cdr uninstr)))
               (cdr (om::flat (cdr(cdr(cdr descrfile))) 1 )))

          (mapcar #'(lambda (uninstr) (mapcar #'(lambda (unseg) (second unseg)) (cdr uninstr)))
               (cdr (om::flat (cdr(cdr(cdr descrfile))) 1)))

          (mapcar #'(lambda (uninstr) (mapcar #'(lambda (unseg) (third unseg)) (cdr uninstr)))
               (cdr (om::flat (cdr(cdr(cdr descrfile))) 1)))

          (mapcar #'(lambda (uninstr) (mapcar #'(lambda (unseg) (fourth unseg)) (cdr uninstr)))
               (cdr (om::flat (cdr(cdr(cdr descrfile))) 1)))

          (mapcar #'(lambda (uninstr) (mapcar #'(lambda (unseg) (nth 4 unseg)) (cdr uninstr)))
               (cdr (om::flat (cdr(cdr(cdr descrfile))) 1)))

          (mapcar #'(lambda (uninstr) (mapcar #'(lambda (unseg) (nth 5 unseg)) (cdr uninstr)))
               (cdr (om::flat (cdr(cdr(cdr descrfile))) 1)))

          (mapcar #'(lambda (uninstr) (mapcar #'(lambda (unseg) (nth 6 unseg)) (cdr uninstr)))
               (cdr (om::flat (cdr(cdr(cdr descrfile))) 1)))

          (mapcar #'(lambda (uninstr) (mapcar #'(lambda (unseg) (nth 7 unseg)) (cdr uninstr)))
               (cdr (om::flat (cdr(cdr(cdr descrfile))) 1)))
)
     
)

;****************************************
;;;; objet DiphPack
;****************************************
 

(om::defmethod! diph::DiphPack ((diconame string) (instname string) (seglist list))
  
  :initvals '("MyDico" "Inst" '("Segm1" "Segm2" "Segm4")) 
  :indoc '("Dictionary name" "Instrument name" "segments list")
  :doc "Format segments list correctly for DiphCopyTo OR DiphBuidSequence objects. After executing this object, you will have a list of that form : (MyDico(Inst1(segm1 segm2 segm3 ...)))"
  :icon 129 
  
  
(om::x-append diconame (list (om::x-append instname (list seglist))))

)


(om::defmethod! diph::DiphPack ( (diconame string) (instname string) (unseg string))
     
(om::x-append diconame (list (om::x-append instname (list (list unseg)))))

)
        

(om::defmethod! diph::DiphPack ( (diconame string) (instname string) (unseg number))
     
(om::x-append diconame (list (om::x-append instname (list (list unseg)))))

)

;****************************************
;;;; objet WriteScript
;****************************************

;************************************************* WriteScript : M. Pur Malt**********************

(om::defmethod! diph::WriteDiphScript  ( (donnees list ) (myfilename string))
  :initvals '( '(1 2 3) "MyDiphScrpit.txt")
  :indoc '("list of script lines" "filename")
  :icon 129 
  :doc  "Write a Diphone script on the hard drive. It opens a file selector to ask you the name of the file."

  (let (fichier) 
    (setf donnees2 (om::x-append '(("Diphone_ExecScript")("Version 1")) donnees ))
    (setq fichier (om-choose-new-file-dialog  :directory (concatenate 'string (namestring om::*diphone-folder*) "OM_Diph/" myfilename) :prompt "Save your Diphone script"))
   (delete-file fichier)
    (when fichier
      (with-open-file  (fd fichier
                           :direction :output :if-exists :supersede 
                           :if-does-not-exist :create)
        (dotimes (n (length donnees2))
          (dotimes (j (length (nth n donnees2)))
            (format fd "~D "(nth j (nth n donnees2))))
          (format fd "~%"))
   ;;; (set-mac-file-creator fichier :dih\a)
   fichier
))))


;****************************************
;;;; objet WriteScript2
;****************************************

;************************************************* WriteScript : M. Pur Malt**********************

(om::defmethod! diph::WriteDiphScript2  ( (donnees list ) (myfilename string))
  :initvals '( '(1 2 3) "MyDiphScrpit.txt")
  :indoc '("list of script lines" "PATH:filename")
  :icon 129 
  :doc  "Write a Diphone script on the hard drive without opening the file selector. The object writes the file in the default Diphone folder."

(let (fichier) 
    (setf donnees2 (om::x-append '(("Diphone_ExecScript")("Version 1")) donnees ))
    (setq fichier (concatenate 'string (namestring om::*diphone-folder*) "OM_Diph/" myfilename ))
    (delete-file fichier)
    (when fichier
      (with-open-file  (fd fichier
                           :direction :output :if-exists :supersede 
                           :if-does-not-exist :create)
        (dotimes (n (length donnees2))
          (dotimes (j (length (nth n donnees2)))
            (format fd "~D "(nth j (nth n donnees2))))
          (format fd "~%"))
    ;; (set-mac-file-creator fichier :dih\a)
    fichier
))))


;****************************************
;;;; objet WriteAppleScript
;****************************************

;************************************************* WriteScript : M. Pur Malt**********************

(om::defmethod! diph::WriteAppleScript  ( (listseq list ) (myfilename string))
  :initvals '( '(1 2 3) "MyAppleScript.txt")
  :indoc '("list" "Name of the applescript")
  :icon 129 
  :doc  "Cet objet genere un fichier AppleScript pour Diphone. Il accepte un nom de séquence ou une liste de séquences."
  

  (setf listforas 
        
        (reduce (lambda (a b) (concatenate 'string a b)) (loop for i from 0  to (- (length listseq) 1)
                                                               collect 
                                                               (if (equal i (- (length listseq) 1))
                                                                 
                                                                 (concatenate 'string  "\"" (nth i listseq) "\"" )
                                                                 (concatenate 'string  "\"" (nth i listseq) "\"," )
                                                                 )
                                                               )
                )
        )
                    


(let (fichier) 
  (setf donnees (mapcar #'(lambda (unestring) (list unestring)) 
                        (om::flat (list 
                                   
                                   "on run"

                                   "tell application \"Diphone\""
                                   "activate"
                                   "end tell"

	                           (concatenate 'string "set seqlist to {" listforas "}")
	                           
	                           "repeat with currentseq in seqlist"
		                   "set aFileName to currentseq as string"
		                   "DoThat(aFileName)"
                          
	                           "end repeat"

                                   "tell application \"Diphone\""
                                   "quit"
                                   "end tell"
	                           
                                   "end run"                                        
                                   "on DoThat(inFileName)"
	                           "tell application \"Diphone\""
	
                (concatenate 'string "set diphfolder to \"" (namestring om::*diphone-folder*)  "\"")
		"do synthesis diphfolder & \"Dico&Seq/\" & inFileName  with sound name diphfolder & \"OM_DIph/\" & inFileName & \".aiff\""
		                   "repeat"
			           "delay 3"
			           "working"
			           "set workvar to result"
			           "if workvar = 0.0 then exit repeat"
		                   "end repeat"
                                   "close window inFileName with saving"
	                           "end tell"
                                   "end DoThat"
                                   )
                                  
                                  )
                        )
        )

    (setq fichier (om-choose-new-file-dialog  :directory (concatenate 'string  (namestring om::*diphone-folder*) "OM_Diph/" myfilename) :prompt "Name of applescipt"))
   (delete-file fichier)
    (when fichier
      (with-open-file  (fd fichier
                           :direction :output :if-exists :supersede 
                           :if-does-not-exist :create)
        (dotimes (n (length donnees))
          (dotimes (j (length (nth n donnees)))
            (format fd "~D "(nth j (nth n donnees))))
          (format fd "~%"))
    ; (set-mac-file-creator fichier :t\o\ys)
    fichier
))))

;****************************************
;;;; objet WriteAppleScript2
;****************************************

;************************************************* WriteScript : M. Pur Malt**********************

(om::defmethod! diph::WriteAppleScript2  ( (listseq list ) (myfilename string))
  :initvals '( '(1 2 3) "MyApplescript.txt")
  :indoc '("list" ""Name of the applescript"")
  :icon 129 
  :doc  "Cet objet genere un fichier AppleScript pour Diphone. Il accepte un nom de séquence ou une liste de séquences."

(setf listforas 
        
        (reduce (lambda (a b) (concatenate 'string a b)) (loop for i from 0  to (- (length listseq) 1)
                                                               collect 
                                                               (if (equal i (- (length listseq) 1))
                                                                 
                                                                 (concatenate 'string  "\"" (nth i listseq) "\"" )
                                                                 (concatenate 'string  "\"" (nth i listseq) "\"," )
                                                                 )
                                                               )
                )
        )

  (let (fichier) 
    (setf donnees (mapcar #'(lambda (unestring) (list unestring)) 
                        (om::flat (list 
                                   
                                   "on run"

                                   "tell application \"Diphone\""
                                   "activate"
                                   "end tell"

	                           (concatenate 'string "set seqlist to {" listforas "}")
	                           
	                           "repeat with currentseq in seqlist"
		                   "set aFileName to currentseq as string"
		                   "DoThat(aFileName)"
                          
	                           "end repeat"

                                   "tell application \"Diphone\""
                                   "quit"
                                   "end tell"
	                           
                                   "end run"                                        
                                   "on DoThat(inFileName)"
	                           "tell application \"Diphone\""
	
                (concatenate 'string "set diphfolder to \"" (namestring om::*diphone-folder*)  "\"")
		"do synthesis diphfolder & \"Dico&Seq/\" & inFileName  with sound name diphfolder & \"OM_DIph/\" & inFileName & \".aiff\""
		                   "repeat"
			           "delay 3"
			           "working"
			           "set workvar to result"
			           "if workvar = 0.0 then exit repeat"
		                   "end repeat"
                                   "close window inFileName with saving"
	                           "end tell"
                                   "end DoThat"
                                   )
                                  
                                  )
                        )
        )

    (setq fichier (concatenate 'string (namestring om::*diphone-folder*) "OM_Diph/" myfilename))
   (delete-file fichier)
    (when fichier
      (with-open-file  (fd fichier
                           :direction :output :if-exists :supersede 
                           :if-does-not-exist :create)
        (dotimes (n (length donnees))
          (dotimes (j (length (nth n donnees)))
            (format fd "~D "(nth j (nth n donnees))))
          (format fd "~%"))
    ; (set-mac-file-creator fichier :t\o\ys)
    fichier
))))


;****************************************
;;;; objet DiphSequenceLength
;****************************************

(om::defmethod! diph::DiphSequenceLength ((listseg list))
  
  :initvals '('("MyDico"("inst1"("Segm1" "Segm8" "Segm3")))) 
  :indoc '("List of segments")
  :doc "Calculates the number of segments in a segment list."
  :icon 129 

  (if (listp (first listseg))
    (apply '+ (mapcar #'(lambda (elmt) (length (second(second elmt)))) listseg))
    (length (second(second listseg)))
  )

)


;****************************************
;;;; objet ExecuteScript 27_03_2006
;****************************************

(om::defmethod! diph::ExecuteDiphScript ((script-name pathname))

  :initvals '("MyDiphScrpit.txt") 
  :indoc '("Name of the script to execute.")
  :doc "Execute a Diphone Script"
  :icon 129

(setq cmd (format nil "~A ~s ~A ~s ~A" "osascript -e 'tell application" "Diphone" "to do xcript" (namestring script-name) "'"))
(om::om-cmd-line cmd)
(print cmd)

;(om::om-cmd-line (format nil "~A ~s ~A" "osascript -e 'tell application" "Diphone" "to quit'"))

)

(om::defmethod! diph::ExecuteDiphScript ((script-name string))
  (diph::ExecuteDiphScript (pathname script-name)))

(om::defmethod! diph::ExecuteDiphScript ((line-list list))
  (diph::ExecuteDiphScript (diph::WriteDiphScript2 line-list "MyTempScript.txt"))
)



;****************************************
;;;; objet DoSynthesis 27_03_2006
;****************************************

(om::defmethod! diph::DoSynthesis ((script-name pathname))

  :initvals '("MyAppleScrpit.txt") 
  :indoc '("Name of the Apple script to execute.")
  :doc "Execute an Apple Script"
  :icon 129

(setq cmd (format nil "~A ~s" "osascript" (namestring script-name)))
(om::om-cmd-line cmd)
)

(om::defmethod! diph::DoSynthesis ((script-name string))
  (diph::DoSynthesis (pathname script-name)))


(om::defmethod! diph::DoSynthesis ((list-seq list))

(diph::DoSynthesis (diph::WriteAppleScript2 list-seq "MyTempApplescript.txt"))

(if (< (length list-seq) 2)
(concatenate 'string (namestring om::*diphone-folder*) "OM_Diph/" (nth 0 list-seq) ".aiff")
(mapcar #'(lambda (elmt) (concatenate 'string (namestring om::*diphone-folder*) "OM_Diph/" elmt ".aiff")) list-seq)
)

)

;****************************************
;;;; objet McToNoteName 27_03_2006
;****************************************
  
(om::defmethod! diph::McToNoteName ((mclist list))

  :initvals '('(6000 6050 6100)) 
  :indoc '("List of midicents")
  :doc "Convert midicent to a note name."
  :icon 129
  

(mapcar #'(lambda (elmt)

(let 
    ((noteang (list "C" "C+" "C#" "D-" "D" "D+" "D#" "E-" "E" "E+" "F" "F+" "F#" "F-" "G" "G+" "G#" "G-" "A" "A+" "A#" "B-" "B" "B+")))
    (format () "~A~D"  (nth (round (/ (mod elmt 1200) 50.0)) noteang)
                 (- (floor (/ elmt 1200.)) 2)
                 )
    )) mclist)
)

(om::defmethod! diph::McToNoteName ((mc number))
(diph::McToNoteName (list mc))
)

;****************************************
;;;; objet DiphRenameSeg 27_03_2006
;****************************************

(om::defmethod! diph::DiphRenameSeg (objet f0file (approx-conf number) generic-name (final-string number))
 
                :initvals '(nil nil 2 nil 3)
                :numouts 4
                :indoc '("Self of diph-script class" "F0 if different from the one located in your Diphone project folder." "approx 2->1/2 ton 4->1/4 ton" "String to add to the name - if not defined intrument name will be use" "final string form")
                :doc "Rename segments regarding the f0 during interpolations."
                :menuins '((4 (("original_name+string" 1) ("pitch+string" 2)("pitch+original_name+string" 3))))
                :icon 131 


                (setq pitch-list nil)
                (setq notename-list nil)

                (if (stringp generic-name)
                    (setf generic-name-string generic-name)
                  (setf generic-name-string (om::instrument objet))
                  )

                (unless (< final-string 2)

                  ;;; remplacer if par cond

                  (if (equal (type-of f0file) 'om::sdiffile)
                      (setq f0-sdif-file f0file)
                    (if (pathnamep f0file)
                        (setq f0-sdif-file (om::load-sdif-file f0file))
                      (if (probe-file (format nil "~AImpExport/Fundamental/~A.f0.sdif" (namestring om::*diphone-folder*) (om::instrument objet)))
                          (setq f0-sdif-file (om::load-sdif-file (pathname (format nil "~AImpExport/Fundamental/~A.f0.sdif" (namestring om::*diphone-folder*) (om::instrument objet)))))
                        (setq f0-sdif-file nil)
                        )
                      )
                    )
                  
                  (if f0-sdif-file
                  
                      (progn 
                        (setq pitch-list (mapcar #'(lambda (1seg-beg 1seg-end 1ctr 1interp-beg 1interp-end)
                                                     (let ((data-beg (if (zerop 1interp-beg) 
                                                                         (list 1seg-beg (+ 1seg-beg (om::om/ (- 1ctr 1seg-beg) 2.0)))
                                                                       (list 1seg-beg (+ 1seg-beg 1interp-beg))))
                                                           (data-end (if (zerop 1interp-end) 
                                                                         (list (- 1seg-end (om::om/ (- 1seg-end 1ctr) 2.0)) 1seg-end )
                                                                       (list (- 1seg-end 1interp-end) 1seg-end))))

                                                       (list (om::approx-m (om::om-mean (om::f->mc (remove-if #'(lambda (x) (zerop x)) 
                                                                                                              (om::flat 
                                                                                                               (om::getsdifdata f0-sdif-file 0 "1FQ0" "1FQ0" 0 nil nil (nth 0 data-beg) (nth 1 data-beg))))))
                                                                           approx-conf)
                                                             (om::approx-m (om-mean (om::f->mc (remove-if #'(lambda (x) (zerop x)) 
                                                                                                          (om::flat (om::getsdifdata f0-sdif-file 0 "1FQ0" "1FQ0" 0 nil nil (nth 0 data-end) (nth 1 data-end)))))) approx-conf)
                                                             )
                
                                                       )
                                                     ) 
                                                 (om::segs-beg objet) (om::segs-end objet) (om::segs-ctr objet) (om::segs-interpbeg objet) (om::segs-interpend objet)
            
                                                 ))

                        (setq notename-list (mapcar #'(lambda (elmt) (diph::McToNoteName elmt)) pitch-list))
                        )
                    
                    (om-beep-msg "Warning : NO F0 SDIF FILE!!!")
                    
                    )
                  )
                (case final-string

                  (1

                   (setq name-list (mapcar #'(lambda (elmt) (format () "~A-~A" elmt generic-name-string)) (om::segs-name objet)))
                   )

                  (2
                   (setq name-list (mapcar #'(lambda (elmt elmt2) (format () "~A_~A~A" (nth 0 elmt2) (nth 1 elmt2) generic-name-string)) (om::segs-name objet) notename-list))
                   )

                  (3
                   (setq name-list (mapcar #'(lambda (elmt elmt2) (format () "~A_~A-~A~A" (nth 0 elmt2) (nth 1 elmt2) elmt generic-name-string)) (om::segs-name objet) notename-list))
                   )
                  )


                (values
                 name-list
                 notename-list
                 pitch-list
                 (mapcar #'(lambda (elmt) (om::x->dx elmt)) pitch-list)
                 )

                )




;*******************************************************
;;;; from Lanza 27_03_2006 modified by lochard 29_6_06
;*******************************************************

(in-package :om)


(defun rec-read-from-string (string)
  "utilissimo!"
  (labels ((fun (x)
             (multiple-value-list (read-from-string x nil))))
    (if (null (read-from-string string nil))
      nil
      (cons (car (fun string))
            (rec-read-from-string (coerce
                                   (nthcdr
                                    (cadr (fun string))
                                    (coerce string 'list))
                                   'string))))))

(defun diph-find (indicator diphone-script)
  (with-open-file (diph-stream diphone-script
                               :direction
                               :input)
    (do ((linea nil (rec-read-from-string (read-line diph-stream nil))))
        ((equalp (car linea) indicator) (cadr linea)))))

(defun dictionary-name (diphone-script)
  (diph-find 'dictionary diphone-script))

(defun plugin-name (diphone-script)
  (diph-find 'plugin diphone-script))

(defun instrument-name (diphone-script)
  (diph-find 'instrument diphone-script))

(defun diph-files (diphone-script)
  (let ((budello nil))
    (with-open-file (diph-stream diphone-script
                                 :direction
                                 :input)
      (do ((linea nil (rec-read-from-string (read-line diph-stream nil)))
           (segs nil (if (equalp (car linea) 'file) (progn (setf budello t) (cons (cdr linea) segs)) segs)))
          ((and budello (null linea)) 
           (reverse segs))))))

;;;;; (diph-files (choose-file-dialog)) ;;;;


;;;;; (instrument-name (choose-file-dialog)) ;;;;


#|

(defun trasforma-segmento (seg)
  (list 
   (nth  0 seg)
   (- (nth 4 seg) (nth 2 seg))
   (- (nth 6 seg) (nth 2 seg))
   0.0
   (nth  8 seg)
   (nth 10 seg)
   0.0))
|#

;;;; (trasforma-segmento '("9" beg 3.85 end 4.8 ctr 4.71 interpbeg 0.43 interpend 0.0)) ;;;;


(defun segments (diphone-script)
  (let ((budello nil))
    (with-open-file (diph-stream diphone-script
                                 :direction
                                 :input)
      (do ((linea nil (rec-read-from-string (read-line diph-stream nil)))
           (segs nil (if (equalp (car linea) 'segment) (progn (setf budello t) (cons (cdr linea) segs)) segs)))
          ((and budello (null linea)) 
           (reverse segs))))))


;;;; (segments (choose-file-dialog)) ;;;;;

(defclass! diph-script ()
  ((dictionary :initform "beneditimus.cont" :accessor dictionary :initarg :dictionary :type string)
   (plugin :initform "Additive" :accessor plugin :initarg :plugin :type string)
   (instrument :initform "beneditimus" :accessor instrument :initarg :instrument :type string)
   (file-1 :initform "" :accessor file-1 :initarg :file-1 :type string)
   (file-2 :initform "" :accessor file-2 :initarg :file-2 :type string)
   (file-3 :initform "" :accessor file-3 :initarg :file-3 :type string)
   (segs-name :initform nil :accessor segs-name :initarg :segs-name :type list)
   (segs-beg :initform nil :accessor segs-beg :initarg :segs-beg :type list)
   (segs-end :initform nil :accessor segs-end :initarg :segs-end :type list)
   (segs-ctr :initform nil :accessor segs-ctr :initarg :segs-ctr :type list)
   (segs-interpbeg :initform nil :accessor segs-interpbeg :initarg :segs-interpbeg :type list)
   (segs-interpend :initform nil :accessor segs-interpend :initarg :segs-interpend :type list))
  (:icon 131))

(defun read-diphone-script (name)
  (let ((segs (segments name))
        (files (diph-files name)))
  (make-instance 'diph-script
    :dictionary (dictionary-name name)
    :plugin (plugin-name name)
    :instrument (instrument-name name)
    :file-1 (caar files)
    :file-2 (caadr files)
    :file-3 (caaddr files)
    :segs-name (mapcar #'car segs)
    :segs-beg (mapcar #'caddr segs)
    :segs-end (mapcar #'fifth segs)
    :segs-ctr (mapcar #'seventh segs)
    :segs-interpbeg (mapcar #'ninth segs)
    :segs-interpend (mapcar #'(lambda (x) (nth 10 x)) segs))
))

(defmethod get-type-of-ed-box ((self diph-script))  'diph-scriptBox)

(defclass diph-scriptBox (OMBoxEditCall) ())

(defmethod import-box-editor ((self diph-scriptBox))
  (catch-cancel
    (let ((name (om-CHOOSE-FILE-DIALOG)))
      (when (and name (probe-file name))
        (setf (value self) (read-diphone-script name))))))

;;;; (read-diphone-script (choose-file-dialog)) ;;;;

(defmethod write-diphone-script ((self diph-script) (name pathname))
  (om-create-directory (om-make-pathname :directory name) :if-exists nil)
  (with-open-file (outstream name
                             :direction
                             :output
                             :if-does-not-exist :create :if-exists :supersede)
    
    (progn 
      (format outstream "Dictionary ~S~A" (dictionary self) #\LineFeed)
      (format outstream "Plugin ~S~A~A" (plugin self) #\LineFeed #\LineFeed)
      (format outstream "Instrument ~S~A~A" (instrument self) #\LineFeed #\LineFeed) 
      (format outstream "File ~S~A" (file-1 self) #\LineFeed)
      (when (not (null (file-2 self)))
        (format outstream "File ~S~A" (file-2 self) #\LineFeed))
      (when (not (null (file-3 self)))
        (format outstream "File ~S~A~A" (file-3 self) #\LineFeed #\LineFeed))
      
      (dotimes (i (length (segs-name self)))
        (format outstream 
                "Segment ~S beg ~S end ~S ctr ~S interpBeg ~S interpEnd ~S~A" 
                (nth i (segs-name self))
                (nth i (segs-beg self))
                (nth i (segs-end self))
                (nth i (segs-ctr self))
                (nth i (segs-interpbeg self))
                (nth i (segs-interpend self))
                #\LineFeed))))
  ;(set-mac-file-creator name :dih\a)
  )

(defmethod write-diphone-script ((self diph-script) (name string))
  (write-diphone-script self (pathname name)))

(defmethod write-diphone-script ((self diph-scriptBox) (name pathname))
  (write-diphone-script (value self) name))

(defmethod write-diphone-script ((self diph-scriptBox) (name pathname))
  (write-diphone-script (value self) name))

(defmethod export-box-editor ((self diph-scriptBox))
  (catch-cancel
    (let ((name (om-choose-new-file-dialog    
                 :prompt "Save the Diphone script file here")))
      (write-diphone-script self name))))

#|
(defmethod* Objfromobjs ((Self textfile) (Type diph-script))
  (read-diphone-script (file-name Self)))

|#


;************************************************* DiphoneANALYSYS *********************************

;****************************************
;;;; objet GetAiffFileList 27_07_2006
;****************************************

(om::defmethod! diph::GetAiffFileList ((folderpath pathname))
  :initvals '(nil) 
  :indoc '("folder with soundfiles")
  :doc "Make a list of the AIFF files in a folder"
  :icon 129
(remove nil (mapcar #'(lambda (onesound) 
                        (if (or (string-equal "aiff" (pathname-type onesound))
                                (string-equal "aif" (pathname-type onesound)))
                            onesound)) (om::OM-directory folderpath))))

;****************************************
;;;; objet PsolaAN 27_07_06
;****************************************

(defun make-psola-command (srcpath f0 method lpcorder seuilvois alpha beta)
  (let  ((srcpath (pathname srcpath))
         (cmd (format nil "psola_analyse -i\"~A\" ~A -e ~A -o ~D -v ~D -a ~A -b ~A -TEUMS" (namestring srcpath) f0 method lpcorder seuilvois alpha beta)))
    (print cmd)
    cmd))

(defun make-cp-command (path add-name name destination)
  (if destination
      (setq cmd (format nil "cp \"~A~A~A/~A.1PSO.sdif\" \"~A\"" (namestring path) add-name name name (format nil "~AImpExport/Psola/" (namestring om::*diphone-folder*))))
    (setq cmd (format nil "cp \"~A~A~A/~A.1PSO.sdif\" \"~A\"" (namestring path) add-name name name (namestring path)))
    )
  (print cmd)
  cmd)

(defun make-rm-command (path add-name name)
  (let  ((cmd (format nil "rm -R -f \"~A~A~A\"" (namestring path) add-name name)))
    (print cmd)
    cmd))

(defun make-export-command (var path)
  (let  ((cmd (format nil "export ~A=\"~A\"" var path)))
    (print cmd)
    cmd))

(defun make-psola-analyse (srcpath f0_param method lpcorder seuilvois alpha beta name path supervppath rm-add destination)
  (om::om-cmd-line (format nil "~A ; ~A ; ~A ; ~A ; ~A ; ~A"
                           (make-export-command "PATH" (format nil "$PATH:~A" supervppath))
                           (make-export-command "PATH" "$PATH:/Applications/Psola-Analyse/")
                           (make-export-command "SFDIR" (namestring path)) 
                           (make-psola-command srcpath f0_param method lpcorder seuilvois alpha beta) 
                           (make-cp-command path "PSO" name destination) 
                           (make-rm-command path "PSO" name)
                           ))
  (when rm-add  
    (om::om-cmd-line (make-rm-command path "ADD" name) t)
    ))

(defvar *SVP-PATH* nil)

(om::defmethod! diph::PsolaAnn ((srcpath pathname) f0 (method string)(lpcorder number)(seuilvois number)(alpha number)(beta number) destination)

   :initvals '(nil nil "ener" 10 0.6 4 0.05 t) 
   :indoc '("Pathname OR folder to analysis" "f0analysis OR f0 sdiffile pathname" "Analysis method" "" "Seuil voisement" "alpha" "beta" "Destination folder")
   :doc "Perform a Psola_analysis from a sound OR a folder of sounds.

THIS FUNCTIONS REQUIRES THE VARIABLE *SVP-PATH* TO BE CORRECTLY SET, EITHER BY LOADING OM-SUPERVP LIBRARY, OR WITH (SETF *SVP-PATH* ...).
"
   :menuins '((2 (("Energie" "ener") ("Frobenius" "frob") ("Reallocation" "realloc")))
              (7 (("Diphone Project Folder" t) ("Same as soundfile" nil))))
   :icon 131
   (let ((name (pathname-name srcpath))
         (path (make-pathname :directory (pathname-directory srcpath)))
         (supervppath (namestring (make-pathname :directory (pathname-directory om::*SVP-PATH*)))))

     (setq f0path (format nil "~A~A.f0.sdif" path name))
     (setq f0path-diphfolder (format nil "~AImpExport/Fundamental/~A.f0.sdif" (namestring om::*diphone-folder*) name))
     (print f0path-diphfolder)

     (if (= (length f0) 2)
         (make-psola-analyse srcpath (format nil "-O -f~D -F~D" (nth 0 f0) (nth 1 f0)) method lpcorder seuilvois alpha beta name path supervppath t destination)
       (if (pathnamep f0)
           (make-psola-analyse srcpath (format nil "-I\"~A\"" (namestring f0)) method lpcorder seuilvois alpha beta name path supervppath nil destination)
         (if (probe-file (pathname f0path))
             (make-psola-analyse srcpath (format nil "-I\"~A\"" (namestring f0path)) method lpcorder seuilvois alpha beta name path supervppath nil destination)
           (if (probe-file (pathname f0path-diphfolder))
               (make-psola-analyse srcpath (format nil "-I\"~A\"" (namestring f0path-diphfolder)) method lpcorder seuilvois alpha beta name path supervppath nil destination)
             (print "F0 file not found")
             )
           )
         )
       )
     )
 )


(om::defmethod! diph::PsolaAnn ((soundlist list) f0 (method string)(lpcorder number)(seuilvois number)(alpha number)(beta number) destination)
   (mapcar #'(lambda (onesound) (diph::PsolaAnn onesound f0 method lpcorder seuilvois alpha beta destination)) soundlist)
   )

;****************************************
;;;; objet BuidDiphScriptFileNames 30_08_06
;****************************************

(defun diph::aif_aiff_test (myname)
  (if (probe-file (make-pathname :device (pathname-device om::*diphone-folder*)
                                 :directory (append (pathname-directory om::*diphone-folder*) (list "ImpExport" "Sound"))
                                 :name myname 
                                 :type "aif"))                        
"aif"
"aiff"
   )
)

(om::defmethod! diph::BuidDiphScriptFileNames ((objet om::diph-script))

  :initvals '(nil)
  :numouts 5
  :indoc '("Self of diph-script class")
  :doc "Construct the file names needed to build a Diphone Script"
  :icon 131
;(print (om::instrument objet))
  (values
   (om::file-1 objet)
   (om::file-2 objet)
   (om::file-3 objet)
   (format nil "$USERHOME/ImpExport/Psola/~A.1PSO.sdif" (om::instrument objet))
   (format nil "$USERHOME/ImpExport/Sound/~A.~A" (om::instrument objet) (diph::aif_aiff_test (om::instrument objet)))  
   ))


;****************************************
;;;; objet DiphCreateDictionary 31_08_2006
;****************************************

(om::defmethod! diph::DiphCreateDictionary ((objet om::diph-script))

  :initvals '(nil) 
  :indoc '("Self of diph-script class OR slots object")
  :doc "Create a dictionary in Diphone from a diphScript class OR a slots object."
  :icon 131

(let 
  ((script-name (format nil "~A/ImpExport/Script/~A-om.script" (namestring om::*diphone-folder*) (om::instrument objet))))

  (om::write-diphone-script objet script-name)

  (setq cmd (format nil "~A ~s ~A ~s ~A" "osascript -e 'tell application" "Diphone" "to create dictionary" (namestring script-name) "'"))
  (om::om-cmd-line cmd)
  (print cmd)
)

)

;****************************************
;;;; objet ScriptToSoundName 31_08_2006
;****************************************

(om::defmethod! diph::ScriptToSoundName ((objet om::diph-script))

  :initvals '(nil) 
  :indoc '("Soundfile name")
  :doc "Rebuid the soundfile name from the script informations."
  :icon 131

(pathname (format nil "~A/ImpExport/Sound/~A.~A" (namestring om::*diphone-folder*) (om::instrument objet) (diph::aif_aiff_test (om::instrument objet))))

)


