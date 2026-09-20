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

(defvar aztrans-api-key nil
  "The key to the Azure service.")

(defvar aztrans-region "northeurope"
  "The region you're using.")

(defvar aztrans-url "https://api-free.aztrans.com"
  "URL to use.")

(cl-defun aztrans (phrase &key (from "fr") (to "en"))
  (let* ((url-request-method "POST")
         (url-request-data
	  (encode-coding-string (json-serialize
				 `[(:text ,phrase)])
				'utf-8))
         (url-request-extra-headers
          `(("Connection" . "close")
            ("Content-Type" ."application/json")
	    ("Accept" . "application/json")
	    ("Ocp-Apim-Subscription-Key" . ,aztrans-api-key)
	    ("Ocp-Apim-Subscription-Region" .,aztrans-region))))
    (with-current-buffer (url-retrieve-synchronously
			  (format
			   "https://api.cognitive.microsofttranslator.com/dictionary/lookup?api-version=3.0&from=%s&to=%s"
			   from to)
			  t)
      (goto-char (point-min))
      (unwind-protect
	  (and (search-forward "\n\n" nil t)
	       (aztrans--summarize (json-parse-buffer :object-type 'plist)))
	(kill-buffer (current-buffer))))))

(defun aztrans--summarize (json)
  (let ((source (elt json 0)))
    (list :source (plist-get source :normalizedSource)
	  :translations
	  (cl-loop for trans across (plist-get source :translations)
		   collect (list :target (plist-get trans :normalizedTarget)
				 :type (plist-get trans :posTag))))))


				 

(provide 'aztrans)

;;; aztrans.el ends here.
