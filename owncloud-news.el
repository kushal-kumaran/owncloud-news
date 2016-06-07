(require 'json)

(defgroup owncloud-news nil
  "Access the Owncloud news app from emacs."
  :group 'convenience)

(defcustom owncloud-news-path
  ""
  "Base path to Owncloud news API."
  :group 'owncloud-news
  :type 'string)

(setq oc-news-curl-program "/usr/bin/curl")
(setq oc-news-cookie-file (expand-file-name "~/.oc-news-cookies"))

(defun oc-news-feeds-url()
  (concat owncloud-news-path "/feeds"))

(defun oc-news-unread-items-url()
  (concat owncloud-news-path "/items?type=3&getRead=false"))

(defun oc-news-mark-read-url()
  (concat owncloud-news-path "/items/read/multiple"))

(defun oc-news-folders-url()
  (concat owncloud-news-path "/folders"))

(defun oc-news-http-get(oc-news-url)
  (with-temp-buffer
    (erase-buffer)
    (call-process oc-news-curl-program nil t nil
		  "--insecure"
		  "--silent" "--cookie-jar" oc-news-cookie-file
		  "--cookie" oc-news-cookie-file
		  "--netrc"
		  "--header" "Accept: application/json"
		  "--header" "Content-Type: application/json"
		  "--write-out" "%{http_code}"
		  oc-news-url)
    (end-of-buffer)
    (let ((http-code-start (re-search-backward "[0-9]\\{3\\}"))
	  (http-code (string-to-number (match-string 0))))
      (delete-region http-code-start (point-max))
      (if (eq http-code 200)
	  (let ((json-object-type 'hash-table))
	    (json-read-from-string (buffer-string)))))))

(defun oc-news-http-put(oc-news-url content)
  (let
      ((content-filename (make-temp-file "oc-news-put")))
    (with-temp-buffer
      (insert (json-encode content))
      (append-to-file (point-min) (point-max) content-filename)
      (erase-buffer)
      (call-process oc-news-curl-program nil t nil
		    "--insecure"
		    "--silent" "--cookie-jar" oc-news-cookie-file
		    "--cookie" oc-news-cookie-file
		    "--netrc"
		    "--header" "Accept: application/json"
		    "--header" "Content-Type: application/json"
		    "--upload-file" content-filename
		    "--write-out" "%{http_code}"
		    oc-news-url)
      (delete-file content-filename)
      (end-of-buffer)
      (let ((http-code-start (re-search-backward "[0-9]\\{3\\}"))
	    (http-code (string-to-number (match-string 0))))
	(delete-region http-code-start (point-max))
	(if (eq http-code 200)
	    (let ((json-object-type 'hash-table))
	      (json-read-from-string (buffer-string))))))))

(defun oc-news-http-post(oc-news-url content)
  (let
      ((content-filename (make-temp-file "oc-news-post")))
    (with-temp-buffer
      (insert (json-encode content))
      (append-to-file (point-min) (point-max) content-filename)
      (erase-buffer)
      (call-process oc-news-curl-program nil t nil
		    "--insecure"
		    "--silent" "--cookie-jar" oc-news-cookie-file
		    "--cookie" oc-news-cookie-file
		    "--netrc"
		    "--header" "Accept: application/json"
		    "--header" "Content-Type: application/json"
		    "--data" (format "@%s" content-filename)
		    "--write-out" "%{http_code}"
		    oc-news-url)
      (delete-file content-filename)
      (end-of-buffer)
      (let ((http-code-start (re-search-backward "[0-9]\\{3\\}"))
	    (http-code (string-to-number (match-string 0))))
	(delete-region http-code-start (point-max))
	(if (eq http-code 200)
	    (let ((json-object-type 'hash-table))
	      (json-read-from-string (buffer-string))))))))

(defun oc-news-get-feeds-int()
  (oc-news-http-get (oc-news-feeds-url)))

(defun oc-news-get-feeds()
  (let ((raw-feeds (oc-news-get-feeds-int))
	(feed-data (make-hash-table :test 'equal)))
    (mapcar
     (lambda (item)
       (puthash (gethash "id" item) item feed-data))
     (gethash "feeds" raw-feeds))
    feed-data))

(defun oc-news-get-folders()
  (let ((raw-folders (oc-news-http-get (oc-news-folders-url)))
	(folder-data (make-hash-table :test 'equal)))
    (mapcar
     (lambda (item)
       (puthash (gethash "name" item) (gethash "id" item) folder-data))
     (gethash "folders" raw-folders))
    folder-data))

(defun oc-news-get-hash-keys(hash-table)
  (let ((keys '()))
    (maphash
     (lambda (k v)
       (setq keys (cons k keys)))
     hash-table)
    keys))

(defun oc-news-get-unread()
  (oc-news-http-get (oc-news-unread-items-url)))

(defun oc-news/render-body(item)
  (with-temp-buffer
    (insert (gethash "body" item))
    (call-process-region
     (point-min) (point-max)
     "/usr/bin/elinks" t t t
     "-dump" "1")
    (buffer-string)))

(defun oc-news()
  (interactive)
  (switch-to-buffer (get-buffer-create "*owncloud-news*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (move-to-column 0 t)
  (let ((unread-items (append (gethash "items" (oc-news-get-unread)) '()))
	(feed-data (oc-news-get-feeds))
	(raw-data (make-hash-table :test 'equal)))
    (setq unread-items
	  (sort unread-items
		(lambda (elt1 elt2)
		  (< (gethash "feedId" elt1)
		     (gethash "feedId" elt2)))))
    (mapcar
     (lambda (item)
       (puthash (gethash "id" item) (gethash "body" item) raw-data)
       (insert "* ")
       (insert "(" (gethash "title" (gethash (gethash "feedId" item) feed-data)) ") ")
       (insert (gethash "title" item) "\n")
       (insert ":PROPERTIES:\n")
       (insert ":id: " (number-to-string (gethash "id" item)) "\n")
       (insert ":url: " (if (gethash "url" item) (gethash "url" item) "") "\n")
       (insert ":END:\n")
       (insert (oc-news/render-body item)))
     unread-items)
    (oc-news-mode)
    (setq oc-news-raw-items raw-data)))

(defun oc-news-done()
  (interactive)
  (let* ((entry-ids (org-map-entries
		     (lambda ()
		       (org-entry-get (point) "id"))
		     nil 'region))
	 (post-data (make-hash-table :test 'equal)))
    (puthash "items" entry-ids post-data)
    (oc-news-http-put (oc-news-mark-read-url) post-data)))

(defun oc-news-add-feed(feed-url)
  (interactive "sEnter feed url: ")
  (let* ((folder-info (oc-news-get-folders))
	 (folder-name (completing-read "Enter folder name: " (oc-news-get-hash-keys folder-info) nil t))
	 (folder-id (gethash folder-name folder-info))
	 (new-feed (make-hash-table :test 'equal)))
    (message "Adding feed %s to folder id %d" feed-url folder-id)
    (puthash "url" feed-url new-feed)
    (puthash "folderId" folder-id new-feed)
    (oc-news-http-post (oc-news-feeds-url) new-feed)))

(defun oc-news/open-in-browser()
  (interactive)
  (browse-url (org-entry-get nil "url")))

(defun oc-news/render()
  (interactive)
  (let* ((entry-id (string-to-number (org-entry-get nil "id")))
	 (entry-text (gethash entry-id oc-news-raw-items))
	 (buffer-name (format "*owncloud-item-%d" entry-id)))
    (switch-to-buffer (get-buffer-create buffer-name))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert entry-text)
    (shr-render-region (point-min) (point-max))
    (goto-char (point-min))))

(define-derived-mode oc-news-mode
  org-mode "Owncloud News"
  "Major mode for viewing owncloud news.
          \\{oc-news-mode-map}"

  (setq buffer-read-only t)
  (goto-char (point-min))
  (make-local-variable 'oc-news-raw-items))

(define-key oc-news-mode-map "\C-c\C-b"
  'oc-news/render)
(define-key oc-news-mode-map "\C-c\C-u"
  'oc-news/open-in-browser)

(provide 'owncloud-news)
