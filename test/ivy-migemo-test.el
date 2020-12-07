;;; ivy-migemo-test.el --- Test for ivy-migemo       -*- lexical-binding: t; -*-

;; Copyright (C) 2020  ROCKTAKEY

;; Author: ROCKTAKEY(require 'undercover) <rocktakey@gmail.com>
;; Keywords:

;; Version: 0.0.0
;; Package-Requires:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(require 'undercover)
(undercover "*.el"
            (:report-file "coverage-final.json")
            (:send-report nil))

(require 'ivy-migemo)

(require 'ert)

(setq migemo-dictionary
      (pcase system-type
        (`windows-nt
         (expand-file-name
          "dict/utf-8/migemo-dict"
          (file-name-directory (locate-file "cmigemo.exe" exec-path))))
        (_ "/usr/share/cmigemo/utf-8/migemo-dict")))
(migemo-init)

(defun test-ivy--filter (regex-seq string)
  (ivy--re-filter regex-seq (list string)))

;; regex
(ert-deftest ivy-migemo--regex-hiragana ()
  (should (test-ivy--filter
           (ivy-migemo--regex "aiueo")
           "あいうえお"))
  (should (test-ivy--filter
           (ivy-migemo--regex "aiueo")
           "I am あいうえお"))
  (should (test-ivy--filter
           (ivy-migemo--regex "aiueo kakikukeko")
           "あいうえお is かきくけこ"))
  (should-not (test-ivy--filter
               (ivy-migemo--regex "aiueo kakikukeko")
               "あいうえお is さしすせそ")))

(ert-deftest ivy-migemo--regex-katakana ()
  (should (test-ivy--filter
           (ivy-migemo--regex "aiueo")
           "アイウエオ"))
  (should (test-ivy--filter
           (ivy-migemo--regex "aiueo")
           "I am アイウエオ"))
  (should (test-ivy--filter
           (ivy-migemo--regex "aiueo kakikukeko")
           "アイウエオ is カキクケコ")))

(ert-deftest ivy-migemo--regex-kanji ()
  (should (test-ivy--filter
           (ivy-migemo--regex "saikou")
           "最高"))
  (should (test-ivy--filter
           (ivy-migemo--regex "saikou")
           "I am 最高"))
  (should (test-ivy--filter
           (ivy-migemo--regex "saikou saitei")
           "最高 最低"))
  (should-not (test-ivy--filter
               (ivy-migemo--regex "saikou teigaku")
               "最高 最低")))

(ert-deftest ivy-migemo--regex-plus-hiragana ()
  (should (test-ivy--filter
           (ivy-migemo--regex-plus "aiueo")
           "あいうえお"))
  (should (test-ivy--filter
           (ivy-migemo--regex-plus "aiueo")
           "I am あいうえお"))
  (should (test-ivy--filter
           (ivy-migemo--regex-plus "aiueo kakikukeko")
           "あいうえお is かきくけこ"))
  (should (test-ivy--filter
           (ivy-migemo--regex-plus "aiueo ! kakikukeko")
           "あいうえお さしすせそ"))
  (should-not (test-ivy--filter
               (ivy-migemo--regex-plus "aiueo ! kakikukeko")
               "あいうえお is かきくけこ")))

(ert-deftest ivy-migemo--regex-plus-katakana ()
  (should (test-ivy--filter
           (ivy-migemo--regex-plus "aiueo")
           "アイウエオ"))
  (should (test-ivy--filter
           (ivy-migemo--regex-plus "aiueo")
           "I am アイウエオ"))
  (should (test-ivy--filter
           (ivy-migemo--regex-plus "aiueo kakikukeko")
           "アイウエオ is カキクケコ"))
  (should (test-ivy--filter
           (ivy-migemo--regex-plus "aiueo ! kakikukeko")
           "アイウエオ サシスセソ"))
  (should-not (test-ivy--filter
               (ivy-migemo--regex-plus "aiueo ! kakikukeko")
               "アイウエオ is カキクケコ")))

(ert-deftest ivy-migemo--regex-plus-kanji ()
  (should (test-ivy--filter
           (ivy-migemo--regex-plus "saikou")
           "最高"))
  (should (test-ivy--filter
           (ivy-migemo--regex-plus "saikou")
           "I am 最高"))
  (should (test-ivy--filter
           (ivy-migemo--regex-plus "saikou saitei")
           "最高 最低"))
  (should-not (test-ivy--filter
               (ivy-migemo--regex-plus "saikou teigaku")
               "最高 最低"))
  (should (test-ivy--filter
           (ivy-migemo--regex-plus "saikou ! teigaku")
           "最高 最低"))
  (should-not (test-ivy--filter
               (ivy-migemo--regex-plus "saikou ! saitei")
               "最高 is 最低")))

;; fuzzy
(ert-deftest ivy-migemo--regex-fuzzy-hiragana ()
  (should (test-ivy--filter
           (ivy-migemo--regex-fuzzy "aiueo")
           "あいうえお"))
  (should (test-ivy--filter
           (ivy-migemo--regex-fuzzy "aiueo")
           "I am あいうえお"))
  (should-not (test-ivy--filter
               (ivy-migemo--regex-fuzzy "aiueo kakikukeko")
               "あいうえお is かきくけこ")))

(ert-deftest ivy-migemo--regex-fuzzy-katakana ()
  (should (test-ivy--filter
           (ivy-migemo--regex-fuzzy "aiueo")
           "アイウエオ"))
  (should (test-ivy--filter
           (ivy-migemo--regex-fuzzy "aiueo")
           "I am アイウエオ"))
  (should-not (test-ivy--filter
               (ivy-migemo--regex-fuzzy "aiueo kakikukeko")
               "アイウエオ is カキクケコ")))

(ert-deftest ivy-migemo--regex-fuzzy-kanji ()
  (should (test-ivy--filter
           (ivy-migemo--regex-fuzzy "saikou")
           "最高"))
  (should (test-ivy--filter
           (ivy-migemo--regex-fuzzy "saikou")
           "I am 最高"))
  (should-not (test-ivy--filter
               (ivy-migemo--regex-fuzzy "saikou saitei")
               "最高 最低"))
  (should-not (test-ivy--filter
               (ivy-migemo--regex-fuzzy "saikou saitei")
               "最高 is 最低")))



(provide 'ivy-migemo-test)
;;; ivy-migemo-test.el ends here
