;****************************************
;;;; objet SetDiphoneFolder from MAURO LANZA/Jean Bresson
;****************************************

(in-package :om)

(pushr 'diphone *external-prefs*)

(defvar *diphone-folder* nil)

(defun init-diph-folder ()
  (unless (and *diphone-folder* (probe-file *diphone-folder*))
    (om-create-directory *diphone-folder*))
  (unless (probe-file (make-pathname :device (pathname-device *diphone-folder*)
                                     :directory (append (pathname-directory *diphone-folder*) (list "OM_Diph"))))
    (om-create-directory (make-pathname :device (pathname-device *diphone-folder*)
                                        :directory (append (pathname-directory *diphone-folder*) (list "OM_Diph")))))
  (unless (probe-file (make-pathname :device (pathname-device *diphone-folder*)
                                     :directory (append (pathname-directory *diphone-folder*) (list "ImpExport" "Psola"))))
    (om-create-directory (make-pathname :device (pathname-device *diphone-folder*)
                                        :directory (append (pathname-directory *diphone-folder*) (list "ImpExport" "Psola")))))
  )

;;; deprecated...
(om::defmethod! diph::SetDiphoneFolder ()
  :icon 129
  :doc "To specify your current Diphone project folder"

 (let  ((newpath (om-namestring (or path (om-choose-directory-dialog)))))
   (when newpath
    (setf *diphone-folder* (namestring newpath))
    (init-diph-folder))))




(defmethod get-external-name ((module (eql 'diphone))) "Diphone")

(defmethod get-external-module-vals ((module (eql 'diphone)) modulepref) (get-pref modulepref :diph-options))
(defmethod get-external-module-path ((module (eql 'diphone)) modulepref) (get-pref modulepref :diph-path))
(defmethod set-external-module-vals ((module (eql 'diphone)) modulepref vals) (set-pref modulepref :diph-options vals))
(defmethod set-external-module-path ((module (eql 'diphone)) modulepref path) 
  (set-pref modulepref :diph-path path))

(defmethod get-external-def-vals ((module (eql 'diphone))) 
    (list :diph-path (make-pathname :directory (append (pathname-directory (mypathname om::*current-workspace*))
                                                       (list "diphfolder")))
          ))

(defmethod save-external-prefs ((module (eql 'chant))) 
  `(:diph-path ,(om-save-pathname *diphone-folder*) 
    ;:diph-options nil
    ))


(defmethod put-external-preferences ((module (eql 'diphone)) moduleprefs)
  (let ((list-prefs (get-pref moduleprefs :diph-options)))
    (when (get-pref moduleprefs :diph-path)
      (setf *diphone-folder* (get-pref moduleprefs :diph-path))
      (init-diph-folder)))
  )

(when (find-pref-module :externals)
  (put-external-preferences 'diphone (find-pref-module :externals)))




