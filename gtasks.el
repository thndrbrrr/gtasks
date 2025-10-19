;;; gtasks.el --- Google Tasks API (sync)  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Synchronous, pure Emacs Lisp client for the Google Tasks REST API.
;;
;;; Code:

(require 'json)
(require 'url)

;; ------------------------------ Customization ------------------------------

(defgroup gtasks nil
  "Google Tasks API."
  :group 'convenience
  :prefix "gtasks-")

(defcustom gtasks-client-id nil
  "Google OAuth2 client id (Desktop/Installed app)."
  :type 'string)

(defcustom gtasks-client-secret nil
  "Client secret for Google OAuth2.
Either a string, or a zero-argument function that returns the secret."
  :type '(choice
          (string   :tag "Literal secret")
          (function :tag "Function returning secret")))

(defcustom gtasks-token-directory
  (locate-user-emacs-file ".gtasks/")
  "Directory used to persist tokens (created 0700)."
  :type 'directory)

(defcustom gtasks-token-file
  (expand-file-name "token.json" gtasks-token-directory)
  "File used to store the refresh token (written 0600)."
  :type 'file)

(defcustom gtasks-user-agent
  (format "Emacs/%s gtasks.el/0.1" emacs-version)
  "User-Agent header string sent with requests."
  :type 'string)

(defcustom gtasks-timeout 60
  "Timeout (seconds) for HTTP requests."
  :type 'integer)

(defcustom gtasks-api-root "https://www.googleapis.com/tasks/v1"
  "Base URL for the Google Tasks API."
  :type 'string)

;; ------------------------------ Errors ---------------------------------

(define-error 'gtasks-error "gtasks error")
(define-error 'gtasks-auth-error "gtasks auth error" 'gtasks-error)
(define-error 'gtasks-http-error "gtasks HTTP error" 'gtasks-error)

;; --------------------------------- Auth ----------------------------------

(defconst gtasks--oauth-auth-url
  "https://accounts.google.com/o/oauth2/v2/auth")

(defconst gtasks--oauth-token-url
  "https://oauth2.googleapis.com/token")

(defconst gtasks--oauth-scope
  "https://www.googleapis.com/auth/tasks")

(defconst gtasks--redirect-uri
  "urn:ietf:wg:oauth:2.0:oob")

(defvar gtasks--access-token nil)
(defvar gtasks--access-token-expiry 0)

(defun gtasks--ensure-token-directory ()
  "Ensure token directory exists with 0700 permissions."
  (unless (file-directory-p gtasks-token-directory)
    (make-directory gtasks-token-directory t)
    (ignore-errors (set-file-modes gtasks-token-directory #o700))))

(defun gtasks--write-file-secure (file content)
  "Write CONTENT to FILE with 0600 permissions."
  (gtasks--ensure-token-directory)
  (with-temp-file file (insert content))
  (ignore-errors (set-file-modes file #o600)))

(defun gtasks--read-refresh-token ()
  "Read refresh token from `gtasks-token-file' or nil if missing."
  (when (file-exists-p gtasks-token-file)
    (with-temp-buffer
      (insert-file-contents gtasks-token-file)
      (goto-char (point-min))
      (let ((json-object-type 'plist)
            (json-array-type 'list)
            (json-key-type 'keyword))
        (let ((obj (ignore-errors
                     (json-parse-buffer :object-type 'plist :array-type 'list))))
          (when obj (plist-get obj :refresh_token)))))))

(defun gtasks--write-refresh-token (refresh)
  "Persist REFRESH token to `gtasks-token-file'."
  (gtasks--write-file-secure
   gtasks-token-file
   (json-encode `((refresh_token . ,refresh)
                  (saved_at . ,(format-time-string "%FT%T%z"))))))

(defun gtasks--client-secret ()
  "Return the client secret from `gtasks-client-secret'."
  (let ((v gtasks-client-secret))
    (cond
     ((stringp v) v)
     ((and v (symbolp v) (fboundp v))
      (let ((s (funcall v)))
        (unless (and s (stringp s))
          (signal 'gtasks-auth-error '("Secret provider did not return a string")))
        s))
     ((functionp v)
      (let ((s (funcall v)))
        (unless (and s (stringp s))
          (signal 'gtasks-auth-error '("Secret provider did not return a string")))
        s))
     ((null v)
      (signal 'gtasks-auth-error '("`gtasks-client-secret' is not set")))
     (t (signal 'gtasks-auth-error (list (format "Invalid secret kind: %S" v)))))))

(defun gtasks-authenticated-p ()
  "Non-nil if an access token is cached and not expired."
  (and gtasks--access-token
       (> gtasks--access-token-expiry (+ (float-time (current-time)) 60))))

(defun gtasks--form-urlencode (alist)
  "Return application/x-www-form-urlencoded body for ALIST of (string . string)."
  (mapconcat (lambda (kv)
               (concat (url-hexify-string (car kv))
                       "=" (url-hexify-string (cdr kv))))
             alist "&"))

(defun gtasks--ensure-access-token ()
  "Return a valid access token, refreshing if necessary."
  (if (gtasks-authenticated-p)
      gtasks--access-token
    (let ((refresh (gtasks--read-refresh-token)))
      (unless refresh
        (signal 'gtasks-auth-error '("No refresh token; run `gtasks-authorize'")))
      (let* ((url-request-method "POST")
             (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
             (url-request-data
              (encode-coding-string
               (gtasks--form-urlencode
                `(("client_id"     . ,(or gtasks-client-id ""))
                  ("client_secret" . ,(or (gtasks--client-secret) ""))
                  ("refresh_token" . ,refresh)
                  ("grant_type"    . "refresh_token")))
               'utf-8))
             (buf (url-retrieve-synchronously gtasks--oauth-token-url t t gtasks-timeout)))
        (unwind-protect
            (progn
              (unless buf (signal 'gtasks-auth-error '("Token refresh: no response")))
              (with-current-buffer buf
                (goto-char (point-min))
                (re-search-forward "\r?\n\r?\n" nil 'move)
                (let* ((json-object-type 'plist)
                       (json-array-type 'list)
                       (json-key-type 'keyword)
                       (data (ignore-errors
                               (json-parse-buffer :object-type 'plist :array-type 'list)))
                       (access (and data (plist-get data :access_token)))
                       (expires (or (and data (plist-get data :expires_in)) 3600)))
                  (unless access
                    (signal 'gtasks-auth-error (list (format "Token refresh failed: %S" data))))
                  (setq gtasks--access-token access
                        gtasks--access-token-expiry (+ (float-time (current-time)) (float expires)))
                  access)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(defun gtasks-authorize ()
  "Run OAuth2 device/web flow and store the refresh token securely."
  (interactive)
  (unless (and gtasks-client-id gtasks-client-secret)
    (signal 'gtasks-auth-error '("Set `gtasks-client-id' and `gtasks-client-secret' first")))
  (let* ((auth-url (concat gtasks--oauth-auth-url
                           "?client_id="  (url-hexify-string gtasks-client-id)
                           "&response_type=code"
                           "&redirect_uri=" (url-hexify-string gtasks--redirect-uri)
                           "&scope=" (url-hexify-string gtasks--oauth-scope)
                           "&access_type=offline&prompt=consent"))
         (code (progn (browse-url auth-url)
                      (read-string "Paste the authorization code from your browser: "))))
    (let* ((url-request-method "POST")
           (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
           (url-request-data
            (encode-coding-string
             (gtasks--form-urlencode
              `(("client_id"     . ,gtasks-client-id)
                ("client_secret" . ,(gtasks--client-secret))
                ("code"          . ,code)
                ("redirect_uri"  . ,gtasks--redirect-uri)
                ("grant_type"    . "authorization_code")))
             'utf-8))
           (buf (url-retrieve-synchronously gtasks--oauth-token-url t t gtasks-timeout)))
      (unwind-protect
          (progn
            (unless buf (signal 'gtasks-auth-error '("Authorization: no response")))
            (with-current-buffer buf
              (goto-char (point-min))
              (re-search-forward "\r?\n\r?\n" nil 'move)
              (let* ((json-object-type 'plist)
                     (json-array-type 'list)
                     (json-key-type 'keyword)
                     (data (ignore-errors
                             (json-parse-buffer :object-type 'plist :array-type 'list)))
                     (refresh (and data (plist-get data :refresh_token))))
                (unless refresh
                  (signal 'gtasks-auth-error (list (format "No refresh_token in response: %S" data))))
                (gtasks--write-refresh-token refresh)
                (message "gtasks: refresh token saved → %s" gtasks-token-file))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

;; -------------------------------- HTTP core --------------------------------

(defun gtasks--build-query (params)
  "Return query string (without '?') from PARAMS (alist of (string . string))."
  (when params
    (mapconcat (lambda (kv)
                 (concat (url-hexify-string (car kv))
                         "=" (url-hexify-string (cdr kv))))
               params "&")))

(defun gtasks--build-url (path params)
  "Join PATH onto `gtasks-api-root' and add PARAMS.  PATHs start with '/'."
  (let ((qs (gtasks--build-query params)))
    (concat (replace-regexp-in-string "/$" "" gtasks-api-root)
            path
            (if qs (concat "?" qs) ""))) )

(defun gtasks--parse-json-current-buffer ()
  "Parse JSON from current buffer point (after headers) to plist, or nil."
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-key-type 'keyword))
    (ignore-errors (json-parse-buffer :object-type 'plist :array-type 'list))) )

(defun gtasks--http-request (method url headers json-body)
  "Execute HTTP request with METHOD, URL, HEADERS, and JSON-BODY.
Return plist (:status :data :headers)."
  (let* ((body (when json-body
                 (encode-coding-string (json-encode json-body) 'utf-8)))
         (hdrs headers))
    (unless (assoc "Accept" hdrs)
      (setq hdrs (cons '("Accept" . "application/json") hdrs)))
    ;; Ensure Content-Type header when there is a body
    (if body (let* ((ct-pair (assoc "Content-Type" hdrs))
		    (ct (and ct-pair (cdr ct-pair))))
               (when ct-pair (setq hdrs (assoc-delete-all "Content-Type" hdrs)))
               (push (cons "Content-Type"
			   (if (and ct (string-match-p "charset=" ct))
                               ct
			     (if ct (concat ct "; charset=UTF-8")
                               "application/json; charset=UTF-8")))
		     hdrs))
      ;; Set header Content-Length to 0 if no body, otherwise Google not happy ...
      (setq hdrs (cons '("Content-Length" . "0") hdrs)))
    (let* ((url-request-method method)
           (url-request-extra-headers hdrs)
           (url-request-data body)
           (buf (url-retrieve-synchronously url t t gtasks-timeout)))
      (unless buf
        (signal 'gtasks-http-error
                (list (format "HTTP request failed: %s %s" method url))))
      (unwind-protect
          (with-current-buffer buf
            (let ((status (or (bound-and-true-p url-http-response-status) 0)))
              (goto-char (point-min))
              (re-search-forward "\r?\n\r?\n" nil 'move)
              (let ((data (gtasks--parse-json-current-buffer))
                    (hdrs-str (buffer-substring (point-min) (point))))
                (list :status status :data data :headers hdrs-str))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(defun gtasks--http (method path params json-body &optional extra-headers)
  "Execute HTTP request with METHOD, PATH, PARAMS, JSON-BODY.
Optionally EXTRA-HEADERS can be provided.
Retries once on 401 after refreshing token."
  (let* ((access (gtasks--ensure-access-token))
         (url (gtasks--build-url path params))
         (headers (append
                   `(("Authorization" . ,(concat "Bearer " access))
                     ("User-Agent"  . ,gtasks-user-agent))
                   ;; Content-Type/Length handled in gtasks--http-request
                   extra-headers))
         (resp (gtasks--http-request method url headers json-body))
         (status (plist-get resp :status)))
    (if (eq status 401)
        (let* ((_ (progn (setq gtasks--access-token nil
                                gtasks--access-token-expiry 0)
                         nil))
               (new (gtasks--ensure-access-token))
               (headers2 (append
                          `(("Authorization" . ,(concat "Bearer " new))
                            ("User-Agent"  . ,gtasks-user-agent))
                          extra-headers)))
          (gtasks--http-request method url headers2 json-body))
      resp)))

(defun gtasks--rfc3339-date-start (date)
  "Convert DATE string \"YYYY-MM-DD\" to RFC3339 midnight UTC, or nil."
  (when (and (stringp date)
             (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" date))
    (format "%sT00:00:00.000Z" date)))

;; ----------------------------- Tasklists API -------------------------------

(defun gtasks-tasklist-clear (tasklist-id)
  "Clear all completed tasks from TASKLIST-ID.  Return t on 204 or 200.

NOTE: Google calls this \"tasks.clear\", but it makes more sense to call it
gtasks-tasklist-clear here.

See https://developers.google.com/workspace/tasks/reference/rest/v1/tasks/clear"
  (let* ((resp (gtasks--http "POST"
                             (format "/lists/%s/clear" (url-hexify-string tasklist-id))
                             nil nil))
         (status (plist-get resp :status)))
    (or (eq status 204) (eq status 200))))

(defun gtasks-tasklist-delete (tasklist-id)
  "DELETE tasklist TASKLIST-ID.  Return t on HTTP 204."
  (let* ((resp (gtasks--http "DELETE"
                             (format "/users/@me/lists/%s" (url-hexify-string tasklist-id))
                             nil nil))
         (status (plist-get resp :status)))
    (eq status 204)))

(defun gtasks-tasklist-get (tasklist-id)
  "Fetch a single tasklist by TASKLIST-ID.  Return plist or nil on 404."
  (let* ((resp (gtasks--http "GET"
                             (format "/users/@me/lists/%s" (url-hexify-string tasklist-id))
                             nil nil))
         (status (plist-get resp :status))
         (data (plist-get resp :data)))
    (and (= status 200) data)))

(defun gtasks-tasklist-insert (tasklist)
  "Create a tasklist from TASKLIST plist; return created tasklist plist.
TASKLIST is a plist of fields accepted by the API, e.g. (:title \"Groceries\")."
  (let* ((resp   (gtasks--http "POST" "/users/@me/lists" nil tasklist))
         (status (plist-get resp :status))
         (data   (plist-get resp :data)))
    (unless (and (>= status 200) (< status 300) data)
      (signal 'gtasks-http-error
              (list (format "Failed to create tasklist (status %s): %S" status data))))
    data))

(defun gtasks-tasklist-list ()
  "Return all tasklists as plist (:items LIST).
Paginates through the API responses."
  (let ((items nil)
        (params nil)
        resp data next)
    (catch 'done
      (while t
        (setq resp (gtasks--http "GET" "/users/@me/lists" params nil)
              data (plist-get resp :data))
        (unless (and data (plist-member data :items))
          (throw 'done (list :items items)))
        (setq items (nconc items (plist-get data :items))
              next  (plist-get data :nextPageToken))
	(if next
	    (setq params `(("pageToken" . ,next)))
	  (throw 'done (list :items items)))))))

(defun gtasks-tasklist-patch (tasklist-id payload)
  "PATCH tasklist TASKLIST-ID with PAYLOAD plist.
Return updated tasklist as plist."
  (let* ((resp (gtasks--http "PATCH"
                             (format "/users/@me/lists/%s" (url-hexify-string tasklist-id))
                             nil payload))
         (status (plist-get resp :status))
         (data (plist-get resp :data)))
    (unless (= status 200)
      (signal 'gtasks-http-error (list (format "Patch tasklist failed (%s): %S" status data))))
    data))

(defun gtasks-tasklist-update (tasklist)
  "PUT full TASKLIST plist (must include :id); return updated tasklist plist."
  (let* ((id (plist-get tasklist :id)))
    (unless (and id (stringp id))
      (signal 'gtasks-error '("gtasks-tasklist-update: TASKLIST missing string :id")))
    (let* ((resp (gtasks--http "PUT"
                               (format "/users/@me/lists/%s" (url-hexify-string id))
                               nil tasklist))
           (status (plist-get resp :status))
           (data (plist-get resp :data)))
      (unless (= status 200)
        (signal 'gtasks-http-error (list (format "Update tasklist failed (%s): %S" status data))))
      data)))

;; --------------------------------- Tasks API --------------------------------

(defun gtasks-task-delete (tasklist-id task-id)
  "DELETE task TASK-ID from tasklist TASKLIST-ID.  Return t on success."
  (let* ((resp (gtasks--http "DELETE"
                             (format "/lists/%s/tasks/%s"
                                     (url-hexify-string tasklist-id)
                                     (url-hexify-string task-id))
                             nil nil))
         (status (plist-get resp :status)))
    (eq status 204)))

(defun gtasks-task-get (tasklist-id task-id)
  "GET task by TASKLIST-ID and TASK-ID.
Return task as plist or nil if not found."
  (let* ((resp (gtasks--http "GET"
                             (format "/lists/%s/tasks/%s"
                                     (url-hexify-string tasklist-id)
                                     (url-hexify-string task-id))
                             nil nil))
         (status (plist-get resp :status))
         (data (plist-get resp :data)))
    (and (= status 200) data)))

(defun gtasks-task-insert (tasklist-id task)
  "Create a Google Task in TASKLIST-ID from TASK plist.

TASK maps :title to title, :due (YYYY-MM-DD) to due (RFC3339), :body to notes.
Return created task as plist."
  (let* ((title (or (plist-get task :title) "(no title)"))
         (due   (gtasks--rfc3339-date-start (plist-get task :due)))
         (body  (plist-get task :body))
         (payload (delq nil
                        `((title . ,title)
                          ,(when due  `(due  . ,due))
                          ,(when body `(notes . ,body)))))
         (resp (gtasks--http "POST"
                             (format "/lists/%s/tasks" (url-hexify-string tasklist-id))
                             nil payload))
         (status (plist-get resp :status))
         (data   (plist-get resp :data)))
    (unless (= status 200)
      (signal 'gtasks-http-error (list (format "Create task failed (%s): %S" status data))))
    data))

(defun gtasks-task-list (tasklist-id &optional show-completed show-deleted show-hidden)
  "Return all tasks in tasklist TASKLIST-ID as plist (:items LIST).
Paginates through the API responses, 100 tasks per page.

Optional filters SHOW-COMPLETED, SHOW-DELETED, SHOW-HIDDEN all default to nil.

NOTE: Google's defaults for these options are showCompleted=True,
showDeleted=False, and showHidden=False, but opting for consistency here.

See https://developers.google.com/workspace/tasks/reference/rest/v1/tasks/list"
  (let* ((params `(("showHidden"    . ,(if show-hidden "true" "false"))
                   ("showDeleted"   . ,(if show-deleted "true" "false"))
                   ("showCompleted" . ,(if show-completed "true" "false"))
                   ("maxResults"    . "100")))
         (items nil)
         resp data next)
    (catch 'done
      (while t
        (setq resp (gtasks--http "GET"
                                 (format "/lists/%s/tasks" (url-hexify-string tasklist-id))
                                 params nil)
              data (plist-get resp :data))
        (unless (and data (plist-member data :items))
          (throw 'done (list :items items)))
        (let ((page (plist-get data :items)))
          (setq items (nconc items page)))
        (setq next (plist-get data :nextPageToken))
        (if next
            (setf (alist-get "pageToken" params nil 'remove #'string=) next)
          (throw 'done (list :items items)))))))

(defun gtasks-task-patch (tasklist-id task-id payload)
  "PATCH task TASK-ID in tasklist TASKLIST-ID with PAYLOAD plist.
Return updated task as plist."
  (let* ((resp (gtasks--http "PATCH"
                             (format "/lists/%s/tasks/%s"
                                     (url-hexify-string tasklist-id)
                                     (url-hexify-string task-id))
                             nil payload))
         (status (plist-get resp :status))
         (data (plist-get resp :data)))
    (unless (= status 200)
      (signal 'gtasks-http-error (list (format "Patch task failed (%s): %S" status data))))
    data))

(defun gtasks-task-move (tasklist-id task-id &optional parent previous dest-tasklist-id)
  "Move task TASK-ID in TASKLIST-ID within the current list or to another list.
PARENT is the parent task's id, PREVIOUS is sibling task's id.
DEST-TASKLIST-ID is the destination tasklist ID.
Return moved task as plist."
  (let ((params nil))
    (when parent   (push (cons "parent" parent) params))
    (when previous (push (cons "previous" previous) params))
    (when dest-tasklist-id (push (cons "destinationTasklist" dest-tasklist-id) params))
    (let* ((resp (gtasks--http "POST"
                               (format "/lists/%s/tasks/%s/move"
                                       (url-hexify-string tasklist-id)
                                       (url-hexify-string task-id))
                               params nil))
           (status (plist-get resp :status))
           (data (plist-get resp :data)))
      (unless (= status 200)
        (signal 'gtasks-http-error (list (format "Move task failed (%s): %S" status data))))
      data)))

(defun gtasks-task-update (tasklist-id task-id task)
  "PUT TASK for TASK-ID in TASKLIST-ID; TASK is a plist of fields.
Return updated task as plist."
  (let* ((resp (gtasks--http "PUT"
                             (format "/lists/%s/tasks/%s"
                                     (url-hexify-string tasklist-id)
                                     (url-hexify-string task-id))
                             nil task))
         (status (plist-get resp :status))
         (data (plist-get resp :data)))
    (unless (= status 200)
      (signal 'gtasks-http-error (list (format "Update task failed (%s): %S" status data))))
    data))

;; ---------------------------- Convenience helpers ---------------------------

(defun gtasks-tasklist-id-by-title (title)
  "Return tasklist id for tasklist named TITLE, or nil if not found."
  (let* ((lists (plist-get (gtasks-tasklist-list) :items))
         (found nil)
         (xs lists))
    (while (and xs (not found))
      (let ((it (car xs)))
        (when (and (listp it) (string= (plist-get it :title) title))
          (setq found it)))
      (setq xs (cdr xs)))
    (when found (plist-get found :id))))

(defun gtasks-task-complete (tasklist-id task-id)
  "Mark task TASK-ID in TASKLIST-ID as completed.  Return t on success."
  (let ((payload `(:status "completed")))
    (ignore (gtasks-task-patch tasklist-id task-id payload))
    t))

(provide 'gtasks)
;;; gtasks.el ends here
