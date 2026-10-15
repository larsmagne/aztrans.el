;;; aztrans.el --- Translating phrases -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>

;; aztrans.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;;; Commentary:

;; Usage: Get an API key from aztrans.ai and:

;; (setq aztrans-api-key "API_KEY")
;; and so on.
;;
;; Then query away:
;;
;; (aztrans "cheville")
;; => (:source "cheville" :translations ((:target "ankle" :type "NOUN") (:target "peg" :type "OTHER") (:target "pin" :type "NOUN")))

;;; Code:

(require 'url)
(require 'libinput)

(defvar aztrans-api-key nil
  "The key to the Azure service.")

(defvar aztrans-region "northeurope"
  "The region you're using.")

(defvar aztrans-url "https://api-free.aztrans.com"
  "URL to use.")

(defvar aztrans-keyboard-identifier "XING WEI"
  "Regexp that matches the name of the keyboard we're listening to.")

(defface aztrans-input
  '((t :height 10.0
       :weight bold))
  "Face for aztrans text inputs."
  :version "31."
  :group 'utilities)

(defface aztrans-response
  '((t :inherit aztrans-input
       :foreground "yellow"
       :weight bold))
  "Face for aztrans text inputs."
  :version "31."
  :group 'utilities)

(cl-defun aztrans (string &key (from "fr") (to "en"))
  (let* ((phrase (string-match-p " " string))
	 (url-request-method "POST")
         (url-request-data
	  (encode-coding-string (json-serialize
				 `[(:text ,string)])
				'utf-8))
         (url-request-extra-headers
          `(("Connection" . "close")
            ("Content-Type" ."application/json")
	    ("Accept" . "application/json")
	    ("Ocp-Apim-Subscription-Key" . ,aztrans-api-key)
	    ("Ocp-Apim-Subscription-Region" .,aztrans-region))))
    (with-current-buffer (url-retrieve-synchronously
			  (format
			   "https://api.cognitive.microsofttranslator.com/%s?api-version=3.0&from=%s&to=%s"
			   (if phrase "translate" "dictionary/lookup")
			   from to)
			  t)
      (goto-char (point-min))
      (unwind-protect
	  (and (search-forward "\n\n" nil t)
	       (aztrans--summarize (json-parse-buffer :object-type 'plist)
				   string phrase))
	(kill-buffer (current-buffer))))))

(defun aztrans--summarize (json string &optional phrase)
  (if phrase
      (list :source string
	    :translations (cl-loop for trans across (plist-get (elt json 0)
							       :translations)
				   collect (list :target (plist-get trans :text)
						 :type "PHRASE")))
    (let ((source (elt json 0)))
      (list :source (plist-get source :normalizedSource)
	    :translations
	    (cl-loop for trans across (plist-get source :translations)
		     collect (list :target (plist-get trans :normalizedTarget)
				   :type (plist-get trans :posTag)))))))

(defun aztrans--setup ()
  "Setup the system to ignore an extra keyboard and then listen to events from it."
  (let ((ids
	 (cl-loop for line in (with-temp-buffer
				(call-process "xinput" nil t nil "list")
				(split-string (buffer-string) "\n"))
		  when (string-match-p aztrans-keyboard-identifier line)
		  collect (and (string-match "id=\\([0-9]+\\)" line)
			       (match-string 1 line)))))
    ;; Disabled the matching devices.
    (dolist (id ids)
      (call-process "xinput" nil nil nil "--set-prop" id "Device Enabled" "0"))
    
    (libinput-grab-devices
     #'aztrans--handle-event aztrans-keyboard-identifier)))

(defvar aztrans--window-configuration nil)
(defvar aztrans--restore-timer nil)

(defun aztrans--handle-event (event)
  ;;(message "%S" event)
  (when-let* ((keyname (plist-get event :key))
	      (key (and (string-match "\\`KEY_\\(.*\\)" keyname)
			(match-string 1 keyname))))
    (let ((window (get-buffer-window (get-buffer-create "*aztrans*"))))
      (unless window
	(setq aztrans--window-configuration (current-window-configuration))
	(when aztrans--restore-timer
	  (cancel-timer aztrans--restore-timer))
	(delete-other-windows)
	(switch-to-buffer "*aztrans*")
	(erase-buffer))
      (with-current-buffer "*aztrans*"
	(when (bobp)
	  (erase-buffer))
	(goto-char (point-max))
	(cond
	 ((length= key 1)
	  (insert (propertize (downcase key) 'face 'aztrans-input)))
	 ((equal key "SPACE")
	  (insert (propertize " " 'face 'aztrans-input)))
	 ((equal key "BACKSPACE")
	  (delete-char -1))
	 ;; Abort.
	 ((equal key "ESC")
	  (set-window-configuration aztrans--window-configuration))
	 ;; Do the translation.
	 ((equal key "ENTER")
	  (let ((phrase (buffer-string)))
	    (erase-buffer)
	    (insert "...")
	    (let ((result (aztrans phrase)))
	      (erase-buffer)
	      (insert (propertize (plist-get result :source)
				  'face 'aztrans-input)
		      "\n")
	      (dolist (trans (plist-get result :translations))
		(insert "\n" (propertize (plist-get trans :target)
					 'face 'aztrans-response)))
	      (goto-char (point-min))
	      ;; Restore the old window layout after some seconds.
	      (setq aztrans--restore-timer
		    (run-at-time
		     5 nil (lambda ()
			     (set-window-configuration
			      aztrans--window-configuration))))))))))))

(provide 'aztrans)

;;; aztrans.el ends here.
