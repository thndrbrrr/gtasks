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
  "Ensure the token directory exists with 0700 permissions.

Arguments:
- None.

Returns:
- Nil."
  (unless (file-directory-p gtasks-token-directory)
    (make-directory gtasks-token-directory t)
    (ignore-errors (set-file-modes gtasks-token-directory #o700))))

(defun gtasks--write-file-secure (file content)
  "Write CONTENT to FILE with 0600 permissions.

Arguments:
- FILE: Destination file path.
- CONTENT: String to write securely.

Returns:
- Nil."
  (gtasks--ensure-token-directory)
  (with-temp-file file (insert content))
  (ignore-errors (set-file-modes file #o600)))

(defun gtasks--read-refresh-token ()
  "Retrieve the cached refresh token from `gtasks-token-file'.

Arguments:
- None.

Returns:
- Refresh token string or nil when absent."
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
  "Persist REFRESH token to `gtasks-token-file'.

Arguments:
- REFRESH: Refresh token string to cache.

Returns:
- Nil."
  (gtasks--write-file-secure
   gtasks-token-file
   (json-encode `((refresh_token . ,refresh)
                  (saved_at . ,(format-time-string "%FT%T%z"))))))

(defun gtasks--client-secret ()
  "Resolve the client secret configured in `gtasks-client-secret'.

Arguments:
- None.

Returns:
- Client secret string.

Errors:
- Signals `gtasks-auth-error' when the secret is unavailable."
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
  "Check whether a cached access token remains valid.

Arguments:
- None.

Returns:
- Non-nil when a valid token is cached; otherwise nil."
  (and gtasks--access-token
       (> gtasks--access-token-expiry (+ (float-time (current-time)) 60))))

(defun gtasks--form-urlencode (alist)
  "Encode ALIST into an application/x-www-form-urlencoded string.

Arguments:
- ALIST: Alist of string pairs.

Returns:
- URL-encoded string suitable for HTTP bodies."
  (mapconcat (lambda (kv)
               (concat (url-hexify-string (car kv))
                       "=" (url-hexify-string (cdr kv))))
             alist "&"))

(defun gtasks--ensure-access-token ()
  "Return a valid access token, refreshing when necessary.

Arguments:
- None.

Returns:
- Access token string ready for HTTP requests.

Errors:
- Signals `gtasks-auth-error' when no refresh token is available."
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
  "Complete the OAuth2 flow and cache the refresh token.

Arguments:
- None (prompts interactively for the authorization code).

Returns:
- Nil.

Errors:
- Signals `gtasks-auth-error' when credentials are missing."
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
  "Assemble an HTTP query string from PARAMS.

Arguments:
- PARAMS: Alist of string pairs.

Returns:
- Query string without the leading question mark or nil when empty."
  (when params
    (mapconcat (lambda (kv)
                 (concat (url-hexify-string (car kv))
                         "=" (url-hexify-string (cdr kv))))
               params "&")))

(defun gtasks--build-url (path params)
  "Create a full request URL from PATH and PARAMS.

Arguments:
- PATH: API path beginning with a slash.
- PARAMS: Alist for query parameters.

Returns:
- Fully qualified URL string."
  (let ((qs (gtasks--build-query params)))
    (concat (replace-regexp-in-string "/$" "" gtasks-api-root)
            path
            (if qs (concat "?" qs) ""))) )

(defun gtasks--parse-json-current-buffer ()
  "Parse JSON from the current buffer into a plist.

Arguments:
- None (expects point positioned after HTTP headers).

Returns:
- Parsed plist or nil when parsing fails."
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-key-type 'keyword))
    (ignore-errors (json-parse-buffer :object-type 'plist :array-type 'list))) )

(defun gtasks--http-request (method url headers json-body)
  "Perform a synchronous HTTP request against the Tasks API.

Arguments:
- METHOD: HTTP method string such as \"GET\" or \"POST\".
- URL: Fully qualified endpoint URL.
- HEADERS: Alist of request headers.
- JSON-BODY: Lisp object to encode as JSON or nil.

Returns:
- Plist containing :status, :data, and :headers keys.

Errors:
- Signals `gtasks-http-error' when the request fails."
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
  "Send an authenticated HTTP request to the Google Tasks API.

Arguments:
- METHOD: HTTP method string such as \"GET\".
- PATH: Endpoint path beginning with a slash.
- PARAMS: Alist of query parameters.
- JSON-BODY: Lisp object encoded as JSON or nil.
- EXTRA-HEADERS: Optional additional headers.

Returns:
- Plist produced by `gtasks--http-request'."
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
  "Convert DATE into an RFC3339 midnight timestamp.

Arguments:
- DATE: String formatted as \"YYYY-MM-DD\".

Returns:
- RFC3339 timestamp string or nil when DATE is invalid."
  (when (and (stringp date)
             (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'" date))
    (format "%sT00:00:00.000Z" date)))

;; ----------------------------- Tasklists API -------------------------------

(defun gtasks-tasklist-clear (tasklist-id)
  "Remove all completed tasks from TASKLIST-ID.

Arguments:
- TASKLIST-ID: Identifier of the tasklist to clear.

Returns:
- Non-nil when the API responds with HTTP 200 or 204.

Notes:
- Google labels this endpoint \"tasks.clear\".
- See https://developers.google.com/workspace/tasks/reference/rest/v1/tasks/clear"
  (let* ((resp (gtasks--http "POST"
                             (format "/lists/%s/clear" (url-hexify-string tasklist-id))
                             nil nil))
         (status (plist-get resp :status)))
    (or (eq status 204) (eq status 200))))

(defun gtasks-tasklist-delete (tasklist-id)
  "Delete the tasklist identified by TASKLIST-ID.

Arguments:
- TASKLIST-ID: Identifier of the tasklist to remove.

Returns:
- Non-nil when the API responds with HTTP 204."
  (let* ((resp (gtasks--http "DELETE"
                             (format "/users/@me/lists/%s" (url-hexify-string tasklist-id))
                             nil nil))
         (status (plist-get resp :status)))
    (eq status 204)))

(defun gtasks-tasklist-get (tasklist-id)
  "Fetch a tasklist identified by TASKLIST-ID.

Arguments:
- TASKLIST-ID: Identifier of the tasklist to retrieve.

Returns:
- Tasklist plist on success or nil when not found."
  (let* ((resp (gtasks--http "GET"
                             (format "/users/@me/lists/%s" (url-hexify-string tasklist-id))
                             nil nil))
         (status (plist-get resp :status))
         (data (plist-get resp :data)))
    (and (= status 200) data)))

(defun gtasks-tasklist-insert (tasklist)
  "Create a new tasklist using TASKLIST fields.

Arguments:
- TASKLIST: Plist accepted by the API, such as (:title \"Groceries\").

Returns:
- Plist describing the created tasklist.

Errors:
- Signals `gtasks-http-error' when creation fails."
  (let* ((resp   (gtasks--http "POST" "/users/@me/lists" nil tasklist))
         (status (plist-get resp :status))
         (data   (plist-get resp :data)))
    (unless (and (>= status 200) (< status 300) data)
      (signal 'gtasks-http-error
              (list (format "Failed to create tasklist (status %s): %S" status data))))
    data))

(defun gtasks-tasklist-list ()
  "List all tasklists for the current user.

Arguments:
- None.

Returns:
- Plist containing :items with every tasklist.

Notes:
- Automatically paginates through API responses."
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
  "Apply a partial update PAYLOAD to TASKLIST-ID.

Arguments:
- TASKLIST-ID: Identifier of the tasklist to update.
- PAYLOAD: Plist of fields to patch.

Returns:
- Updated tasklist plist.

Errors:
- Signals `gtasks-http-error' when the patch fails."
  (let* ((resp (gtasks--http "PATCH"
                             (format "/users/@me/lists/%s" (url-hexify-string tasklist-id))
                             nil payload))
         (status (plist-get resp :status))
         (data (plist-get resp :data)))
    (unless (= status 200)
      (signal 'gtasks-http-error (list (format "Patch tasklist failed (%s): %S" status data))))
    data))

(defun gtasks-tasklist-update (tasklist)
  "Replace tasklist with TASKLIST.

Arguments:
- TASKLIST: Plist describing the tasklist.  Must include :id.

Returns:
- Updated tasklist plist.

Errors:
- Signals `gtasks-error' or `gtasks-http-error' on failure."
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
  "Delete TASK-ID from TASKLIST-ID.

Arguments:
- TASKLIST-ID: Identifier of the tasklist.
- TASK-ID: Identifier of the task to delete.

Returns:
- Non-nil when the API responds with HTTP 204."
  (let* ((resp (gtasks--http "DELETE"
                             (format "/lists/%s/tasks/%s"
                                     (url-hexify-string tasklist-id)
                                     (url-hexify-string task-id))
                             nil nil))
         (status (plist-get resp :status)))
    (eq status 204)))

(defun gtasks-task-get (tasklist-id task-id)
  "Retrieve TASK-ID from TASKLIST-ID.

Arguments:
- TASKLIST-ID: Identifier of the tasklist.
- TASK-ID: Identifier of the task to fetch.

Returns:
- Task plist when found or nil otherwise."
  (let* ((resp (gtasks--http "GET"
                             (format "/lists/%s/tasks/%s"
                                     (url-hexify-string tasklist-id)
                                     (url-hexify-string task-id))
                             nil nil))
         (status (plist-get resp :status))
         (data (plist-get resp :data)))
    (and (= status 200) data)))

(defun gtasks-task-insert (tasklist-id task)
  "Create a task within TASKLIST-ID using TASK fields.

Arguments:
- TASKLIST-ID: Identifier of the destination tasklist.
- TASK: Plist mapping :title, :due (YYYY-MM-DD), and :body to task data.

Returns:
- Created task plist.

Errors:
- Signals `gtasks-http-error' when creation fails."
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
  "List tasks in TASKLIST-ID with optional visibility filters.

Arguments:
- TASKLIST-ID: Identifier of the tasklist to inspect.
- SHOW-COMPLETED: Non-nil to include completed tasks.
- SHOW-DELETED: Non-nil to include deleted tasks.
- SHOW-HIDDEN: Non-nil to include hidden tasks.

Returns:
- Plist containing :items with every matching task.

Notes:
- Requests at most 100 tasks per page and paginates automatically.
- Google's defaults differ; this wrapper defaults the filters to nil.
- See https://developers.google.com/workspace/tasks/reference/rest/v1/tasks/list"
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
  "Apply partial PAYLOAD update to TASK-ID in TASKLIST-ID.

Arguments:
- TASKLIST-ID: Identifier of the tasklist.
- TASK-ID: Identifier of the task to modify.
- PAYLOAD: Plist of fields to update.

Returns:
- Updated task plist.

Errors:
- Signals `gtasks-http-error' when the patch fails."
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
  "Move TASK-ID within TASKLIST-ID or to another list.

Arguments:
- TASKLIST-ID: Identifier of the current tasklist.
- TASK-ID: Identifier of the task to move.
- PARENT: Optional parent task identifier.
- PREVIOUS: Optional previous sibling identifier.
- DEST-TASKLIST-ID: Optional destination tasklist identifier.

Returns:
- Updated task plist.

Errors:
- Signals `gtasks-http-error' when the move fails."
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
  "Replace TASK-ID in TASKLIST-ID with TASK.

Arguments:
- TASKLIST-ID: Identifier of the tasklist.
- TASK-ID: Identifier of the task to replace.
- TASK: Plist representing the updated task payload.  Must include :id.

Returns:
- Updated task plist.

Errors:
- Signals `gtasks-http-error' when the update fails."
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
  "Find the tasklist identifier whose title matches TITLE.

Arguments:
- TITLE: Tasklist title string to search for.

Returns:
- Tasklist identifier string or nil when no match is found."
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
  "Mark TASK-ID in TASKLIST-ID as completed.

Arguments:
- TASKLIST-ID: Identifier of the tasklist.
- TASK-ID: Identifier of the task to complete.

Returns:
- t when the patch succeeds."
  (let ((payload `(:status "completed")))
    (ignore (gtasks-task-patch tasklist-id task-id payload))
    t))

(provide 'gtasks)
;;; gtasks.el ends here
