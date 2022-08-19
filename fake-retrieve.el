;;;  -*- lexical-binding: t -*-

(defcustom fake-retrieve-endpoints nil
  "list of endpoint to match. Each element is of form
(:match string :delay number :file string)

Ex: ((:match \"https://api.stackexchange.com/2.3/search?tagged=javascript&intitle=sort&site=stackoverflow\"
			:delay 0.5
			:file \"~/emacs-packages/fake-retrieve/stackoverflow.mock\")
	(:match \"https://github.com/test/\"
			:delay 0.5
			:file \"~/.mock/response\")))"
  :type '(plist :value-type (:match string :delay number :file string))
  :group 'ewaser)

;; TODO it should return something like #<buffer  *http api.stackexchange.com:443*-488730>
(defun fake-retrieve--buffer-name (url)
  url)

;; TODO do we need the exact same shape than what url-retrieve returns?
;; do people rely on it?
(defun fake-retrieve--generate-status ()
  200)

(defun fake-retrieve--mock-function (file url callback &optional delay)
  (let ((response (with-current-buffer (find-file-noselect file) (buffer-string))))
	(lambda (&rest args)
	  (run-at-time (or delay 0) nil
				   (lambda ()
					 (with-current-buffer (get-buffer-create (fake-retrieve--buffer-name url))
					   (erase-buffer)
					   (insert response)
					   (funcall callback (fake-retrieve--generate-status))))))))


(defmacro with-fake-retrieve-file (file &rest body)
  `(cl-flet
	   ((url-retrieve (url callback)
		  (funcall (fake-retrieve--mock-function ,file url callback))))
	 ,@body))

;; TODO should match with regex not string=
(defun fake-retrieve--find-match (url)
  (catch 'match
	(dolist (mock fake-retrieve-endpoints)
	  (when (string= url (plist-get mock :match))
		(throw 'match mock)))))


(defmacro with-fake-retrieve (&rest body)
  `(cl-flet
	   ((url-retrieve (url callback)
		  (let ((match (fake-retrieve--find-match url)))
			(unless match
			  (user-error (format "No mock file found for %s" url)))
			(let ((file (plist-get match :file))
				  (delay (or (plist-get match :delay) 0)))
			  (funcall (fake-retrieve--mock-function file url callback delay))))))
	 ,@body))
