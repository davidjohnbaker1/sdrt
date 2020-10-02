;------------------------------------------------
; Model 1 

(idyom:idyom 767 '(cpitch) '(cpint)
                       :output-path  "/Users/davidjohnbaker/Desktop/projects/sdrt/"
                      :overwrite t
                      :separator #\tab
                      :detail 3)


;------------------------------------------------
; Model 1 
(idyom:idyom 767 '(cpitch) '(cpintfref)
                       :output-path  "/Users/davidjohnbaker/Desktop/projects/sdrt/"
                      :overwrite t
                      :separator #\tab
                      :detail 3)


;------------------------------------------------
; Model 3 (CogMIR) Model  
(idyom:idyom 767 '(cpitch) '(cpint cpintfref)
                       :output-path  "/Users/davidjohnbaker/Desktop/projects/sdrt/"
                      :overwrite t
                      :separator #\tab
                      :detail 3)
