(defun c:stabl () 
  (setvar "DIMZIN" 0) ;精度補0
  ;---------------------副程式-------------------------------------
  ;---------------------line num------------------------------------------------
  (defun linenum (num) 
    (setq soilstr (strcat "請選取土層" (rtos num 2 0) ":"))
    (setq soil (car (entsel soilstr)))
    (setq allsoil (cons soil allsoil))
    (setq ent_pline (entget soil))
    (setq gr_pline (car ent_pline))
    (setq nums 0)
    (while gr_pline 
      (setq nr (car gr_pline)) ;setq1 end
      (if (= nr 10) 
        (setq nums (+ nums 1))
      ) ;endif
      (setq ent_pline (cdr ent_pline) ;setq4
            gr_pline  (car ent_pline)
      ) ;setq4
      (setq denum nums)
      (if (= num 1) 
        (setq firstnum nums)
      )
    ) ;endwhile
  )
  ;---------------------line num end------------------------------------------------
  ;---------------------將數列從0開始------------------------------------------------
  (defun start2zero (ent_pline) 
    (setq ent2zero   ent_pline
          ent2zerogo ent_pline
    )
    (setq gr_pline (car ent2zerogo))
    (setq cs  1
          csw 1
    )
    (while csw 
      (setq nr (car gr_pline)) ;setq1 end
      (if (= nr 10) 
        (progn 
          (if (= cs 1) 
            (progn 
              (setq pt_xva (cadr gr_pline))
            )
          ) ;endif
          (if (= cs 2) 
            (progn 
              (setq pt_xvb (cadr gr_pline))
              (setq csw nil)
            )
          ) ;endif
          (if (< pt_xvb pt_xva) 
            (setq ent2zero (reverse ent2zero))
          ) ;endif
          (setq cs (+ cs 1))
        ) ;progn end
      ) ;endif
      (setq ent2zerogo (cdr ent2zerogo) ;setq4
            gr_pline   (car ent2zerogo)
      ) ;setq4 end
    ) ;endwhile
  )
  ;---------------------將數列從0開始------------------------------------------------
  ;---------------------line coordinate------------------------------------------------
  (defun linecoor (num) 
    (setq soil (car allsoil))
    (setq ent_pline (entget soil))
    (start2zero ent_pline)
    (setq gr_pline (car ent2zero))
    (setq wr 0)
    (while gr_pline 
      (setq nr (car gr_pline)) ;setq1 end
      (if (= nr 10) 
        (progn 
          (setq wr (+ wr 1))
          (setq pt_xv (cadr gr_pline) ;setq2
                pt_yv (caddr gr_pline)
          ) ;setq2 end
          (if (> wr 1) 
            (progn 
              (setq pt_string (strcat (rtos x1 2 2) 
                                      " "
                                      (rtos y1 2 2)
                                      " "
                                      (rtos pt_xv 2 2)
                                      " "
                                      (rtos pt_yv 2 2)
                              )
              ) ;setq3 end
              (write-line pt_string filename)
            )
          ) ;end if
          (setq x1 pt_xv)
          (setq y1 pt_yv)
        ) ;progn end
      ) ;endif
      (setq ent2zero (cdr ent2zero) ;setq4
            gr_pline (car ent2zero)
      ) ;setq4 end
    ) ;endwhile
    (setq allsoil (cdr allsoil))
  )
  ;---------------------line coordinate end------------------------------------------------
  ;---------------------water coordinate------------------------------------------------
  (defun waternum (num) 
    (setq wat (car (entsel "請選地下水")))
    (setq ent_pline (entget wat))
    (setq gr_pline (car ent_pline))
    (setq nums 0)
    (while gr_pline 
      (setq nr (car gr_pline)) ;setq1 end
      (if (= nr 10) 
        (setq nums (+ nums 1))
      ) ;endif
      (setq ent_pline (cdr ent_pline) ;setq4
            gr_pline  (car ent_pline)
      ) ;setq4
      (setq denum nums)
    ) ;endwhile
  )

  (defun watercoor (num) 
    (setq ent_pline (entget wat))
    (start2zero ent_pline)
    (setq gr_pline (car ent2zero))
    (while gr_pline 
      (setq nr (car gr_pline)) ;setq1 end
      (if (= nr 10) 
        (progn 
          (setq pt_xv (cadr gr_pline) ;setq2
                pt_yv (caddr gr_pline)
          ) ;setq2 end
          (progn 
            (setq pt_string (strcat (rtos pt_xv 2 2) " " (rtos pt_yv 2 2))) ;setq3 end
            (write-line pt_string filename)
          )
        ) ;progn end
      ) ;endif
      (setq ent2zero (cdr ent2zero) ;setq4
            gr_pline (car ent2zero)
      ) ;setq4 end
    ) ;endwhile
    (setq allsoil (cdr allsoil))
  )
  ;---------------------water coordinate end------------------------------------------------

  ;---------------------副程式end-------------------------------------

  ;---------------------主程式------------------------------------------------------------
  (setq filename (open "d:/output.si" "w"))
  (write-line "PROFIL  c:\file\output.SI Version 5M [STABL5M.EXE]" filename)
  (write-line "output" filename)
  ;-----------------土層界線---------------
  (setq soilnum (getint "請輸入土層數"))
  ;---------------------土層----------------------
  ;------------------土層總點數--------------
  (setq num 0)
  (setq allnum 0)
  (setq allsoil (list))
  (while (< num soilnum) 
    (setq num (+ num 1))
    (linenum num)
    (setq allnum (+ allnum denum -1)) ;allnum 總點數
  )
  (setq allsoil (reverse allsoil))
  (setq firstnum (- firstnum 1))
  (setq str (strcat (rtos allnum 2 0) " " (rtos firstnum 2 0)))
  (write-line str filename)
  ;------------------土層總點數end--------------
  ;------------------土層座標--------------
  (setq num 0)
  (while (< num soilnum) 
    (setq num (+ num 1))
    (linecoor num)
  )


  ;------------------土層座標end--------------
  ;------------------地下水座標--------------
  (write-line "WATER" filename)
  (write-line "1 9.8" filename)
  (setq num 1)
  (waternum num)
  (setq str (rtos denum 2 0))
  (write-line str filename)
  (watercoor num)
  ;------------------地下水座標end--------------
  ;---------------------土層end--------------------


  (alert "所選取的聚合線, 已匯出點座標數據到 d:/pline_points.txt")
  (princ)
  (close filename)
);defun end

(c:stabl)

  