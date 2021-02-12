;;; gcal.el --- Google Calendar Interface            -*- lexical-binding: t; -*-

;; Copyright (C) 2016  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Version: 0.9.0
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/misohena/gcal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To use this package, add below code in your init.el.

;;   (require 'gcal)
;;   (setq gcal-client-id "xxxxxxxxx.apps.googleusercontent.com")
;;   (setq gcal-client-secret "xxxx-XxxxXxxXXXxx") ; API-KEY

;; gcal has below APIs

;;   ;; list my calendars
;;   (gcal-calendar-list-list) ;; Calendar List

;;   ;; list events
;;   (gcal-events-list
;;    "example@gmail.com" ;;<- calendar-id
;;     `((timeMin . ,(gcal-datetime 2016 5 1))
;;       (timeMax . ,(gcal-datetime 2016 6 1))))

;;   ;; insert event
;;   (gcal-events-insert
;;    "example@gmail.com"
;;    `((start . ,(gcal-gtime 2016 5 27))
;;      (end . ,(gcal-gtime 2016 5 28))
;;     (summary . "My Special Holiday")))

;;   ;; delete event
;;   (gcal-events-delete "example@gmail.com" "xxx{event id}xxx")

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-util)
(require 'json)
(require 'parse-time)

(defgroup gcal nil
  "Google Calendar Interface.

Reference: https://developers.google.com/calendar/v3/reference"
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/gcal.el"))

(defcustom gcal-client-id ""
  "Google Calendar API client-id.

Like 000000000000-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.apps.googleusercontent.com"
  :group 'gcal
  :type 'string)

(defcustom gcal-client-secret ""
  "Google Calendar API client-secret.

Like xxxxxxxxxxxxxxxxxxxxxxxx"
  :group 'gcal
  :type 'string)

(defcustom gcal-token-file (locate-user-emacs-file ".gcal-token")
  "Path to access token file."
  :group 'gcal
  :type 'file)

(defcustom gcal-default-time-zone "America/New_York"
  "Default time zone, one of IANA Time Zone Database name."
  :group 'gcal
  :type 'string)

(defconst gcal-auth-url "https://accounts.google.com/o/oauth2/auth")
(defconst gcal-token-url "https://www.googleapis.com/oauth2/v3/token")
(defconst gcal-scope-url "https://www.googleapis.com/auth/calendar")
(defconst gcal-calendar-url "https://www.googleapis.com/calendar/v3")


;;;; HTTP

(defun gcal-http-make-query-string (params)
  "Build query string from PARAMS.

Example:
  (gcal-http-make-query-string '((\"a\" . \"b\") (\"c\" . \"d\")))
  ;;=> \"a=b&c=d\"

  (gcal-http-make-query-string '((a . \"b\") (c . \"d\")))
  ;;=> \"a=b&c=d\"

  (gcal-http-make-query-string '((a . \"b\") (c . (\"d\" \"e\"))))
  ;;=> \"a=b&c=d&c=e\""
  (cl-flet ((concat-args
             (elm)
             (format "%s=%s"
                     (url-hexify-string (format "%s" (car elm)))
                     (url-hexify-string (format "%s" (cdr elm)))))
            (concat-seqs (sequence) (mapconcat 'identity sequence "&")))
    (thread-last params
      (mapcan (lambda (elm)
                (if (atom (cdr elm))
                    (list elm)
                  (mapcar (lambda (e) (cons (car elm) e)) (cdr elm)))))
      (mapcar #'concat-args)
      (concat-seqs))))

(defun gcal-http-make-query-url (url params)
  "Build URL with query PARAMS."
  (concat
   url
   (when params
     (concat "?" (gcal-http-make-query-string params)))))

(defun gcal-http-parse-response-buffer (&optional buf)
  "Parse HTTP response BUF."
  (with-current-buffer (or buf (current-buffer))
    (goto-char (point-min))

    (let (code status headers body)
      ;; HTTP/1.1 200 OK
      (looking-at "^HTTP/[^ ]+ \\([0-9]+\\) ?\\(.*\\)$")
      (setq code (string-to-number (match-string 1)))
      (setq status (match-string 2))

      ;; headers
      ;; Content-Type: application/json; charset=UTF-8
      ;; Content-Length: 2134
      (forward-line)
      (while (not (eolp))
        (when (looking-at "^\\([^:]+\\): \\(.*\\)$")
          (push `(,(match-string 1) . ,(match-string 2)) headers))
        (forward-line))
      
      ;; body
      (forward-line)
      (setq body (buffer-substring (point) (point-max)))

      (list code status (nreverse headers) body))))

(defun gcal-http-response-to-json (response)
  "Convert HTTP RESPONSE to parsed JSON object."
  (let ((status (nth 0 response))
        (body (nth 3 response)))
    (cond
     ((= status 204) nil)               ; 204 No Content
     (t
      (let ((json-array-type 'list))
        (json-read-from-string (decode-coding-string body 'utf-8)))))))

;;; request wrapper

(defun gcal-http-request (method url &optional params headers req-body)
  "Request URL via METHOD and parse response.

Optional:
  PARAMS alist: URL query
  HEADERS alist: Extra request headers
  REQ-BODY string: Request body"
  (declare (indent 2))
  (let ((url-request-method
         (cond
          ((stringp method) method)
          ((symbolp method)
           (if (keywordp method)
               (upcase (substring (symbol-name method) 1))
             (upcase (symbol-name method))))
          (t
           (error "Unknown type: %s" method))))
        (url-request-extra-headers headers)
        (url-request-data req-body))
    (with-current-buffer (url-retrieve-synchronously (gcal-http-make-query-url url params))
      (unwind-protect
          (gcal-http-parse-response-buffer)
        (kill-buffer)))))

(defun gcal-http-request-json (method url &optional params json)
  "Send request via METHOD (with PARAMS and JSON BODY) to URL."
  (declare (indent 2))
  (gcal-http-request method url
    params
    '(("Content-Type" . "application/json"))
    (encode-coding-string (json-encode json) 'utf-8)))

(defun gcal-http-post-www-form (url &optional params)
  "Send POST request (with x-www-form-url-encoded PARAMS) to URL."
  (declare (indent 1))
  (gcal-http-request "POST" url
    nil
    '(("Content-Type" . "application/x-www-form-urlencoded"))
    (gcal-http-make-query-string params)))

;;; request/response wrapper

(defun gcal-retrieve-json-request (method url &optional params headers req-body)
  "Send HTTP request and return JSON object.

See `gcal-http' for METHOD URL PARAMS HEADERS REQ-BODY docstring."
  (declare (indent 2))
  (gcal-http-response-to-json
   (gcal-http-request method url params headers req-body)))

(defun gcal-retrieve-json-request-json (method url &optional params json)
  "Send request via METHOD (with PARAMS and JSON BODY) to URL."
  (declare (indent 2))
  (gcal-http-response-to-json
   (gcal-http-request-json method url params json)))

(defun gcal-retrieve-json-post-www-form (url &optional params)
  "Send HTTP POST request (x-www-form-url-encoded) and return JSON object.

See `gcal-http' for URL PARAMS docstring."
  (declare (indent 1))
  (gcal-http-response-to-json
   (gcal-http-post-www-form url params)))


;;;; OAuth

(cl-defstruct (gcal-oauth-token
               (:constructor gcal-oauth-token-new)
               (:copier nil))
  access expires refresh url)

(defun gcal-oauth-get (auth-url token-url client-id client-secret scope)
  "Get oauth token, return `gcal-oauth-token'.

Argumemnts:
  AUTH-URL string: URL to auth
  TOKEN-URL string: URL to token
  CLIENT-ID string: client id
  CLIENT-SECRET string: client secret
  SCOPE string: scope"
  (let ((result (gcal-retrieve-json-post-www-form token-url
                  `(("client_id" . ,client-id)
                    ("client_secret" . ,client-secret)
                    ("redirect_uri" . "urn:ietf:wg:oauth:2.0:oob")
                    ("grant_type" . "authorization_code")
                    ("code" . ,(progn
                                 (browse-url
                                  (gcal-http-make-query-url
                                   auth-url
                                   `(("client_id" . ,client-id)
                                     ("response_type" . "code")
                                     ("redirect_uri" . "urn:ietf:wg:oauth:2.0:oob")
                                     ("scope" . ,scope))))
                                 (read-string "Enter the authentication code your browser displayed: ")))))))
    (let-alist result
      (gcal-oauth-token-new
       :access .access_token
       :expires (time-add (current-time) (seconds-to-time .expires_in))
       :refresh .refreshtoken
       :url token-url))))

(defun gcal-oauth-refresh (token token-url client-id client-secret)
  "Refresh TOKEN, return `gcal-oauth-token'.

See `gcal-oauth-get' for TOKEN-URL CLIENT-ID CLIENT-SECRET."
  (when (and token
             (time-less-p (gcal-oauth-token-expires token) (current-time)))
    (let ((result (gcal-retrieve-json-post-www-form token-url
                    `(("client_id" . ,client-id)
                      ("client_secret" . ,client-secret)
                      ("redirect_uri" . "urn:ietf:wg:oauth:2.0:oob")
                      ("grant_type" . "refresh_token")
                      ("refresh_token" . ,(gcal-oauth-token-refresh token))))))
      (let-alist result
        (when (and .access_token .expires_in)
          (setf (gcal-oauth-token-access token) .access_token)
          (setf (gcal-oauth-token-expires token)
                (time-add (current-time) (seconds-to-time .expires_in)))
          token)))))

(defun gcal-oauth-save-token (file token)
  "Save TOKEN into FILE."
  (with-temp-file file
    (insert (pp-to-string token))))

(defun gcal-oauth-load-token (file)
  "Load token from FILE, return `gcal-oauth-token'."
  (ignore-errors
    (read
     (with-temp-buffer
       (insert-file-contents file)
       (buffer-string)))))

(defun gcal-oauth-token (token auth-url token-url client-id client-secret scope token-file)
  "Get oauth token.

Arguments:
  TOKEN AUTH-URL TOKEN-URL CLIENT-ID CLIENT-SECRET SCOPE TOKEN-FILE"
  (or
   (gcal-oauth-load-token token-file)
   (let ((token (or
                 (gcal-oauth-refresh token token-url client-id client-secret)
                 (gcal-oauth-get auth-url token-url client-id client-secret scope))))
     (gcal-oauth-save-token token-file token)
     token)))


;;;; Google Calendar OAuth

(defvar gcal-oauth-token nil)

(defun gcal-access-token ()
  "Get Google Calendar access token."
  (setq gcal-oauth-token
        (gcal-oauth-token gcal-oauth-token
                          gcal-auth-url
                          gcal-token-url
                          gcal-client-id
                          gcal-client-secret
                          gcal-scope-url
                          gcal-token-file))
  (gcal-oauth-token-access gcal-oauth-token))

(defun gcal-access-token-params ()
  "Get Google Calendar access token param."
  `(("access_token" . ,(gcal-access-token))))


;;;; API URL Builder

(defun gcal-calendar-api-backend (method resource &rest args)
  "Get Google Calendar API method and URL.

Argumemnt:
  METHOD symbol: (:get :delete ...)
  RESOURCE symbol: (:acl :calendar-list :calendars :channels
                    :colors :events :freebusy :settings)
  ARGS (string): Arguments

Return:
  (METHOD URL)

See https://developers.google.com/calendar/v3/reference"
  (pcase-let
      ((`(,method ,url)
        (cl-case resource
          (:acl
           (cl-case method
             (:delete
              '(:delete "/calendars/%a/acl/%b"))
             (:get
              '(:get "/calendars/%a/acl/%b"))
             (:insert
              '(:post "/calendars/%a/acl"))
             (:list
              '(:get "/calendars/%a/acl"))
             (:patch
              '(:patch "/calendars/%a/acl/%b"))
             (:update
              '(:put "/calendars/%a/acl/%b"))
             (:watch
              '(:post  "/calendars/%a/acl/watch"))))

          (:calendar-list
           (cl-case method
             (:delete
              '(:delete "/users/me/calendarList/%a"))
             (:get
              '(:get "/users/me/calendarList/%a"))
             (:insert
              '(:post "/users/me/calendarList"))
             (:list
              '(:get "/users/me/calendarList"))
             (:patch
              '(:patch "/users/me/calendarList/%a"))
             (:update
              '(:update "/users/me/calendarList/%a"))
             (:watch
              '(:post "/users/me/calendarList/watch"))))

          (:calendars
           (cl-case method
             (:clear
              '(:post "/calendars/%a/clear"))
             (:delete
              '(:delete "/calendars/%a"))
             (:get
              '(:get "/calendars/%a"))
             (:insert
              '(:post "/calendars"))
             (:patch
              '(:patch "/calendars/%a"))
             (:update
              '(:put "/calendars/%a"))))

          (:channels
           (cl-case method
             (:stop
              '(:post "/channels/stop"))))

          (:colors
           (cl-case method
             (:get
              '(:get "/colors"))))

          (:events
           (cl-case method
             (:delete
              '(:delete "/calendars/%a/events/%b"))
             (:get
              '(:get "/calendars/%a/events/%b"))
             (:import
              '(:post "/calendars/%a/events/import"))
             (:insert
              '(:post "/calendars/%a/events"))
             (:instances
              '(:get "/calendars/%a/events/%b/instances"))
             (:list
              '(:get "/calendars/%a/events"))
             (:move
              '(:post "/calendars/%a/events/%b/move"))
             (:patch
              '(:patch "/calendars/%a/events/%b"))
             (:quick-add
              '(:post "/calendars/%a/events/quickAdd"))
             (:update
              '(:put "/calendars/%a/events/%b"))
             (:watch
              '(:post "/calendars/%a/events/watch"))))

          (:freebusy
           (cl-case method
             (:query
              '(:post "/freeBusy"))))

          (:settings
           (cl-case method
             (:get
              '(:get "/users/me/settings/setting"))
             (:list
              '(:get "/users/me/settings"))
             (:watch
              '(:post "/users/me/settings/watch")))))))
    (list
     method
     (concat gcal-calendar-url
             (format-spec
              url
              `((?a . ,(nth 0 args))
                (?b . ,(nth 1 args))
                (?c . ,(nth 2 args))
                (?d . ,(nth 3 args))
                (?e . ,(nth 4 args))))))))


;;;; API Wrapper

;; CalendarList

(defun gcal-calendar-list-list ()
  "CalendarList: list"
  (gcal-retrieve-json-get (gcal-calendar-api-url 'list 'calendar-list)
    (gcal-access-token-params)))

;; Events

(defun gcal-events-list (calendar-id &optional params)
  "Events: list"
  (gcal-retrieve-json-get (gcal-calendar-api-url 'list 'events calendar-id)
    (append (gcal-access-token-params) params)))

(defun gcal-events-get (calendar-id event-id &optional params)
  "Events: get"
  (gcal-retrieve-json-get (gcal-calendar-api-url 'get 'events calendar-id event-id)
    (append (gcal-access-token-params) params)))

(defun gcal-events-quick-add (calendar-id text &optional params)
  "Events: quickAdd"
  (gcal-retrieve-json-post-json (gcal-calendar-api-url 'quick-add 'events calendar-id)
    (append (gcal-access-token-params) params `(("text" . ,text)))
    nil))

(defun gcal-events-insert (calendar-id event-data &optional params)
  "Events: insert

Example:
  (gcal-events-insert
   \"xxxxxxxxxxxxx@group.calendar.google.com\"
   `((start (date \"2016-05-25\"))
     (end (date \"2016-05-26\"))
     (summary . \"First Test Event\")))"
  (gcal-retrieve-json-post-json (gcal-events-url calendar-id)
    (append (gcal-access-token-params) params)
    event-data))

(defun gcal-events-patch (calendar-id event-id event-data &optional params)
  "Events: patch"
  (gcal-retrieve-json-request-json
      "PATCH" (gcal-events-url calendar-id event-id)
    (append (gcal-access-token-params) params)
    event-data))

(defun gcal-events-update (calendar-id event-id event-data &optional params)
  "Events: update"
  (gcal-retrieve-json-request-json
      "PUT" (gcal-events-url calendar-id event-id)
    (append (gcal-access-token-params) params)
    event-data))

(defun gcal-events-delete (calendar-id event-id &optional params)
  "Events: delete"
  (gcal-retrieve-json-request
      "DELETE" (gcal-events-url calendar-id event-id)
    (append (gcal-access-token-params) params)))


;;;; Time Utilities

;; time = Emacs Internal Time
;;   (ex: (encode-time 0 0 0 31 4 2016) )
;; gtime = Google Calendar Time
;;   (ex: ('date . "2016-05-27") ('dateTime . "2016-05-27T12:34:00+09:00"))
;; datetime = RFC3339
;;   (ex: 2016-05-01T12:34:00+09:00)

(defun gcal-default-time-zone ()
  gcal-default-time-zone)

(defun gcal-time-zone-suffix ()
  (let ((tz (format-time-string "%z")))
    (concat (substring tz 0 3) ":" (substring tz 3))))

(defun gcal-time-format (time date-only)
  (if date-only (format-time-string "%Y-%m-%d" time)
    (concat
     (format-time-string "%Y-%m-%dT%H:%M:%S" time)
     (gcal-time-zone-suffix))))

(defun gcal-time-from-ymdhm (y m d hh mm)
  (encode-time 0 (or mm 0) (or hh 0) d m y))

(defun gcal-time-to-gtime (time date-only)
  (append
   (list
    (cons
     (if date-only 'date 'dateTime)
     (gcal-time-format time date-only))
    (cons
     (if date-only 'dateTime 'date)
     nil))
   (when-let ((name (gcal-default-time-zone)))
     (list (cons 'timeZone name)))))

(defun gcal-gtime (y m d &optional hh mm)
  (gcal-time-to-gtime (gcal-time-from-ymdhm y m d hh mm) (not hh)))

(defun gcal-datetime (y m d &optional hh mm)
  (gcal-time-format (gcal-time-from-ymdhm y m d hh mm) nil))

;; google => emacs

;; (parse-iso8601-time-string "2014-12-13T10:00:00+09:00")
;; (parse-iso8601-time-string "2015-03-06T15:42:32.354Z")

(defun gcal-gtime-date-str (gtime)
  "ex: ((date . \"2016-05-28\")) => \"2016-05-28\" or nil"
  (alist-get 'date gtime))

(defun gcal-gtime-date-time-str (gtime)
  "ex: ((dateTime . \"2009-10-25T11:00:54+09:00\")) => \"2009-10-25T11:00:54+09:00\" or nil"
  (alist-get 'dateTime gtime))

(defun gcal-time-from-gtime (gtime)
  (let ((date (gcal-gtime-date-str gtime)))
    (if (stringp date)
        (let ((d (parse-time-string date)))
          (encode-time 0 0 0 (nth 3 d)(nth 4 d)(nth 5 d)))
      (let ((datetime (gcal-gtime-date-time-str gtime)))
        (when (stringp datetime)
          (parse-iso8601-time-string datetime))))))


;;;; utilities

(defun gcal-get-error-code (response-json)
  (when (listp response-json)
    (let-alist response-json .error.code)))

(defun gcal-succeeded-p (response-json)
  (not (gcal-get-error-code response-json)))

(defun gcal-failed-p (response-json)
  (not (not (gcal-get-error-code response-json))))

(provide 'gcal)
;;; gcal.el ends here
