;;; gcal-tests.el --- Test for gcal                  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/conao3/gcal.el

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

;; Test for gcal.

;;; Code:

(require 'cort)
(require 'gcal)

(cort-deftest gcal/http-make-query-string
  (cort-generate :equal
    '(((gcal-http-make-query-string '(("a" . "b") ("c" . "d")))
       "a=b&c=d")
      ((gcal-http-make-query-string '((a . "b") (c . "d")))
       "a=b&c=d")
      ((gcal-http-make-query-string '((a . "b") (c "d" "e")))
       "a=b&c=d&c=e")
      ((gcal-http-make-query-string nil)
       ""))))

(cort-deftest gcal/http-make-query-url
  (cort-generate :equal
    '(((gcal-http-make-query-url "https://example.com" '((a . "b") (c . "d")))
       "https://example.com?a=b&c=d")
      ((gcal-http-make-query-url "https://example.com" nil)
       "https://example.com"))))

(cort-deftest gcal/gtime-accessors
  (cort-generate :equal
    '(((gcal-gtime-date-str '((date . "2016-05-28")))
       "2016-05-28")
      ((gcal-gtime-date-str '((dateTime . "2009-10-25T11:00:54+09:00")))
       nil)
      ((gcal-gtime-date-time-str '((dateTime . "2009-10-25T11:00:54+09:00")))
       "2009-10-25T11:00:54+09:00")
      ((gcal-gtime-date-time-str '((date . "2016-05-28")))
       nil))))

(cort-deftest gcal/error-helpers
  (cort-generate :equal
    '(((gcal-get-error-code '((error . ((code . 404)))))
       404)
      ((gcal-get-error-code '((items . [])))
       nil)
      ((gcal-succeeded-p '((items . [])))
       t)
      ((gcal-succeeded-p '((error . ((code . 404)))))
       nil)
      ((gcal-failed-p '((error . ((code . 404)))))
       t)
      ((gcal-failed-p '((items . [])))
       nil))))

(provide 'gcal-tests)
;;; gcal-tests.el ends here
