;;; fairway --- Discourse client -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'rx)
(require 'request)
(require 'shr)
(require 'page-break-lines)

;;;; Custom variables
(defcustom fair/server-buffer-template "*fair-server/%s*"
  "Template for server buffer names."
  :type '(string) :group 'fair)
(defcustom fair/category-buffer-template "*fair-category/%s/%c*"
  "Template for category buffer names."
  :type '(string) :group 'fair)
(defcustom fair/topic-buffer-template "*fair-topic/%s/%c/%t*"
  "Template for category buffer names."
  :type '(string) :group 'fair)

(defcustom fair/servers
  (list
   (cons 'tsuki "https://forum.tsuki.games")
   (cons 'nixos "https://discourse.nixos.org"))
  "Alist mapping server names to URLs."
  :type '((list string)) :group 'fair)

;;; Major modes
(define-derived-mode fair/server-mode special-mode "discourse server list"
  "Major mode for displaying a server category list."
  :group 'fair)
(define-derived-mode fair/category-mode special-mode "discourse category list"
  "Major mode for displaying a server category list."
  :group 'fair)
(define-derived-mode fair/topic-mode special-mode "discourse topic list"
  "Major mode for displaying a server category list."
  :group 'fair
  (page-break-lines-mode))

;;; Faces
(defface fair/username
  '((t
     :weight bold
     ))
  "Face for usernames."
  :group 'wasp)

(defface fair/topic-title
  '((t
     :weight bold
     ))
  "Face for topic titles."
  :group 'wasp)

;;;; State
(defvar-local fair/buffer-server nil)
(defvar-local fair/buffer-category nil)
(defvar-local fair/buffer-topic nil)

;;;; Utility functions
(defun fair/wipe-buffer ()
  "Erase the current buffer, including read-only text."
  (let ((inhibit-read-only t))
    (delete-all-overlays)
    (set-text-properties (point-min) (point-max) nil)
    (erase-buffer)))

(defun fair/write (text &optional face)
  "Write TEXT to the current buffer and apply FACE."
  (let ((text-final (if face (propertize text 'face face) text)))
    (insert text-final)))

(defun fair/write-line (line &optional face)
  "Write LINE and a newline to the current buffer and apply FACE."
  (fair/write (concat line "\n") face))

;;;; Buffer names
(defun fair/get-server-buffer (server)
  "Return the buffer for SERVER."
  (let ((nm (format-spec
             fair/server-buffer-template
             `((?s . ,(format "%s" server))))))
    (unless (get-buffer nm)
      (with-current-buffer (get-buffer-create nm)
        (fair/server-mode)))
    (get-buffer nm)))

(defun fair/get-category-buffer (server category)
  "Return the buffer for SERVER and CATEGORY."
  (let ((nm (format-spec
             fair/category-buffer-template
             `((?s . ,(format "%s" server))
               (?c . ,(ht-get category "slug"))))))
    (unless (get-buffer nm)
      (with-current-buffer (get-buffer-create nm)
        (fair/category-mode)))
    (get-buffer nm)))

(defun fair/get-topic-buffer (server category topic)
  "Return the buffer for SERVER, CATEGORY, and TOPIC."
  (let ((nm (format-spec
             fair/topic-buffer-template
             `((?s . ,(format "%s" server))
               (?c . ,(ht-get category "slug"))
               (?t . ,(ht-get topic "slug"))))))
    (unless (get-buffer nm)
      (with-current-buffer (get-buffer-create nm)
        (fair/topic-mode)))
    (get-buffer nm)))

;;;; Network requests
(defvar fair/last-response nil)
(defun fair/fetch (server loc k)
  "Retrieve LOC from SERVER and pass the resulting JSON to K."
  (when-let ((url (alist-get server fair/servers)))
    (request
      (s-concat url loc)
      :type "GET"
      :headers
      `(("Content-Type" . "application/json"))
      :parser #'json-parse-buffer
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
         (setq fair/last-response data)
         (funcall k data)))))
  nil)
(defun fair/fetch-server-categories (server k)
  "Retrieve the list of categories for SERVER and pass them to K."
  (fair/fetch
   server "/categories.json"
   (lambda (d)
     (funcall
      k
      (-some-> d
        (ht-get "category_list")
        (ht-get "categories"))))))
(defun fair/fetch-category-by-id (server id k)
  "Retrieve the category ID on SERVER and pass it to K."
  (fair/fetch
   server (format "/c/%s/show.json" id)
   (lambda (d)
     (funcall k d))))
(defun fair/fetch-category-topics-by-slug-id (server slug id k)
  "Retrieve the category SLUG and ID on SERVER and pass it to K."
  (fair/fetch
   server (format "/c/%s/%s.json" slug id)
   (lambda (d)
     (funcall k d))))
(defun fair/fetch-category-topics (server category k)
  "Retrieve the list of topics in CATEGORY on SERVER and pass them to K."
  (fair/fetch-category-topics-by-slug-id
   server
   (ht-get category "slug") (ht-get category "id")
   (lambda (d)
     (funcall
      k
      (-some-> d
        (ht-get "topic_list")
        (ht-get "topics"))))))
(defun fair/fetch-topic-by-id (server id k)
  "Retrieve the topic ID on SERVER and pass it to K."
  (fair/fetch
   server (format "/t/%s.json" id)
   (lambda (d)
     (funcall k d))))
(defun fair/fetch-topic-posts (server topic k)
  "Retrieve the list of posts in TOPIC on SERVER and pass them to K."
  (fair/fetch-topic-by-id
   server
   (ht-get topic "id")
   (lambda (d)
     (funcall
      k
      (-some-> d
        (ht-get "post_stream")
        (ht-get "posts"))))))

;;;; Rendering
(defun fair/render-post (post)
  "Display POST in the current buffer."
  (let ((inhibit-read-only t)
        (start (point)))
    (fair/write-line (ht-get post "username") 'fair/username)
    (shr-insert-document
     (with-temp-buffer
       (insert (ht-get post "cooked"))
       (libxml-parse-html-region (point-min) (point-max))))
    (fair/write-line "\f")
    (add-text-properties
     start (point)
     (list 'fair-post post))))
(defun fair/render-topic (server category topic posts)
  "Display POSTS in the buffer for SERVER, CATEGORY, and TOPIC."
  (with-current-buffer (fair/get-topic-buffer server category topic)
    (fair/wipe-buffer)
    (setq-local fair/buffer-server server)
    (setq-local fair/buffer-category category)
    (setq-local fair/buffer-topic topic)
    (--each (seq-into posts 'list)
      (fair/render-post it))
    (goto-char (point-min))))
(defun fair/display-topic (server category topic posts)
  "Render and display the topic for SERVER, CATEGORY, TOPIC, and POSTS."
  (fair/render-topic server category topic posts)
  (switch-to-buffer (fair/get-topic-buffer server category topic)))
(defun fair/display-topic-by-id (server id &optional postnum)
  "Render and display the topic for SERVER, and ID.
Optionally scroll to POSTNUM."
  (fair/fetch-topic-by-id
   server
   id
   (lambda (topic)
     (fair/fetch-category-by-id
      server
      (ht-get topic "category_id")
      (lambda (cat)
        (let ((category (ht-get cat "category")))
          (fair/display-topic
           server category topic
           (-some-> topic
             (ht-get "post_stream")
             (ht-get "posts")))
          (with-current-buffer (fair/get-topic-buffer server category topic)
            (--each (-iota (- postnum 1))
              (fair/move-to-next-post)))))))))

(defun fair/topic-button-action (b)
  "Action when a topic button B in the category buffer is clicked."
  (let ((topic (get-text-property (button-start b) 'fair-topic)))
    (fair/fetch-topic-posts
     fair/buffer-server topic
     (lambda (posts)
       (fair/display-topic fair/buffer-server fair/buffer-category topic posts)))))
(defun fair/render-category (server category topics)
  "Display TOPICS in the buffer for SERVER and CATEGORY."
  (with-current-buffer (fair/get-category-buffer server category)
    (fair/wipe-buffer)
    (setq-local fair/buffer-server server)
    (setq-local fair/buffer-category category)
    (let ((inhibit-read-only t))
      (--each (seq-into topics 'list)
        (insert-text-button
         (ht-get it "title")
         'face 'fair/topic-title
         'fair-topic it
         'action #'fair/topic-button-action)
        (fair/write-line "")))
    (goto-char (point-min))))
(defun fair/display-category (server category topics)
  "Render and display the category for SERVER, CATEGORY, and TOPICS."
  (fair/render-category server category topics)
  (switch-to-buffer (fair/get-category-buffer server category)))
(defun fair/display-category-by-slug-id (server slug id)
  "Render and display the category for SERVER, SLUG, and ID."
  (fair/fetch-server-categories
   server
   (lambda (categories)
     (when-let ((category
                 (--find
                  (and (s-equals? (ht-get it "slug") slug) (= (ht-get it "id") id))
                  (seq-into categories 'list))))
       (fair/fetch-category-topics
        server category
        (lambda (topics)
          (fair/display-category server category topics)))))))

(defun fair/category-button-action (b)
  "Action when a category button B in the server buffer is clicked."
  (let ((category (get-text-property (button-start b) 'fair-category)))
    (fair/fetch-category-topics
     fair/buffer-server category
     (lambda (topics)
       (fair/display-category fair/buffer-server category topics)))))
(defun fair/render-server (server categories)
  "Display CATEGORIES in the buffer for SERVER."
  (with-current-buffer (fair/get-server-buffer server)
    (fair/wipe-buffer)
    (setq-local fair/buffer-server server)
    (let ((inhibit-read-only t))
      (--each (seq-into categories 'list)
        (insert-text-button
         (ht-get it "name")
         'face (list :foreground (s-concat "#" (ht-get it "color")) :weight 'bold)
         'fair-category it
         'action #'fair/category-button-action)
        (fair/write-line "")))
    (goto-char (point-min))))
(defun fair/browse-server (server)
  "Open the category list for SERVER."
  (fair/fetch-server-categories
   server
   (lambda (categories)
     (fair/render-server server categories)
     (switch-to-buffer (fair/get-server-buffer server)))))

;;;; Navigation
(defun fair/open-link-at-point ()
  "Open the link under POINT."
  (interactive)
  (when-let ((url (get-text-property (point) 'shr-url)))
    (cond
     ((or (s-prefix? "/" url) (s-prefix? (alist-get fair/buffer-server fair/servers) url))
      (if-let* ((matches (s-match-strings-all
                          (rx "/" (group alpha) "/" (group (one-or-more (or alpha "-"))) "/" (group (one-or-more num)) (? "/" (group (one-or-more num))))
                          (s-chop-prefix (alist-get fair/buffer-server fair/servers) url)))
                (match (car matches))
                (sort (cadr match))
                (slug (caddr match))
                (id (-some-> match (cadddr) (string-to-number))))
          (let ((postnum (nth 4 match)))
            (cond
             ((s-equals? sort "t") (fair/display-topic-by-id fair/buffer-server id (and postnum (string-to-number postnum))))
             ((s-equals? sort "c") (fair/display-category-by-slug-id fair/buffer-server slug id))
             (t (browse-url url))))
        (browse-url url)))
     (t (browse-url url)))))
(defun fair/move-to-next-post ()
  "Navigate point to the next post in the current topic buffer."
  (interactive)
  (let ((pos (next-single-property-change (point) 'fair-post)))
    (goto-char (or pos (point-max)))))
(defun fair/move-to-previous-post ()
  "Navigate point to the previous post in the current topic buffer."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'fair-post)))
    (goto-char (or pos (point-min)))))

;;;; Default keybindings
(define-key fair/topic-mode-map (kbd "<return>") #'fair/open-link-at-point)

(provide 'fairway)
;;; fairway.el ends here
