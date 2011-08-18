

(defvar typejr-mondai-alist
  '(("懇親会" . "こんしんかい")
    ("日付" . "ひづけ")
    ("東京特許許可局" . "とうきょうとっきょきょかきょく")
    ))

(defvar typejr-cur-mondai "きゃんぷふぁいあー")
(defvar typejr-cur-inputed-mondai "")
(defvar typejr-cur-hiragana "")
(defvar typejr-cur-hiragana-pos 0)
(defvar typejr-cur-romaji-candidates nil)
(defvar typejr-cur-romaji-pos 0)
(defvar typejr-cur-char nil)
(defvar typejr-romaji-definition nil)

(defface typejr-good-romaji-face
  '((t (:foreground "red"))) nil)

(defun typejr-initial ()
  ""
  (typejr-read-romaji-definition))

(defun typejr-mondai-loop ()
  ""
  (interactive)
  (setq typejr-cur-hiragana-pos 0)
  (setq typejr-cur-inputed-mondai "")
  (typejr-rewrite-mondai)
  (typejr-rewrite-evaluation "")
  (while (not (typejr-end-of-mondai-p))
    (setq typejr-cur-hiragana
          (substring typejr-cur-mondai typejr-cur-hiragana-pos
                     (1+ typejr-cur-hiragana-pos)))
    (setq typejr-cur-romaji-candidates
          (typejr-get-romaji-candidates typejr-cur-hiragana))
    (typejr-rewrite-inputed-mondai)
    (typejr-rewrite-hiragana)
    (typejr-hiragana-loop)
    (setq typejr-cur-hiragana-pos (1+ typejr-cur-hiragana-pos))
    (setq typejr-cur-inputed-mondai
          (concat typejr-cur-inputed-mondai typejr-cur-hiragana))))

(defun typejr-hiragana-loop ()
  ""
  (setq typejr-cur-romaji-pos 0)
  (while (not (typejr-end-of-hiragana-p))
    (typejr-rewrite-romaji-candidates)
    (typejr-rewrite-romaji-pos)
    (let* ((inputed-romaji (read-char))
           (correct-list (typejr-get-correct-candidate-list inputed-romaji)))
      (if (> (length correct-list) 0)
          (progn
            (setq typejr-cur-romaji-pos
                  (1+ typejr-cur-romaji-pos))
            (setq typejr-cur-romaji-candidates correct-list)
            (insert inputed-romaji)
            (typejr-rewrite-evaluation "Good!"))
        (let ((visible-bell nil)
              (ring-bell-function 'beep))
          (ding)
          (typejr-rewrite-evaluation "Bad"))))))

(defun typejr-end-of-mondai-p ()
  ""
  (<= (length typejr-cur-mondai) typejr-cur-hiragana-pos))

(defun typejr-end-of-hiragana-p ()
  ""
  (<= (car (sort (mapcar 'length typejr-cur-romaji-candidates) '<))
      typejr-cur-romaji-pos))

(defun typejr-get-cur-hiragana ()
  ""
  (let* ((len (length typejr-cur-mondai))
         (two-hiragana
          (substring typejr-cur-mondai
                     typejr-cur-hiragana-pos (min (+ 2 typejr-cur-hiragana-pos) len)))
         (one-hiragana
          (substring typejr-cur-mondai
                     typejr-cur-hiragana-pos (min (1+ typejr-cur-hiragana-pos) len))))
    (if (assoc two-hiragana typejr-romaji-definition)
        (car (assoc two-hiragana typejr-romaji-definition))
      (car (assoc one-hiragana typejr-romaji-definition)))
    ))

(defun typejr-get-correct-candidate-list (romaji)
  "Pick up correct candidate list from `typejr-cur-romaji-candidates'."
  (let (lst)
    (mapc '(lambda (candidate)
             (when (= (aref candidate typejr-cur-romaji-pos) romaji)
               (add-to-list 'lst candidate)))
          typejr-cur-romaji-candidates)
    (reverse lst)))

(defun typejr-get-romaji-candidates (hiragana)
  "Return list of candidates of romaji definition corresponding to HIRAGANA."
  (cdr (assoc hiragana typejr-romaji-definition)))

(defun typejr-read-romaji-definition ()
  "Read romaji henkan definition from \"romaji-definition.txt\"."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents "romaji-definition.txt")
      (let ((str (buffer-string)))      ; process buffer per line
        (mapc '(lambda (line)
                 (unless (string= line "") ; ignore empty line
                   (add-to-list 'typejr-romaji-definition (split-string line))))
              (split-string str "\n"))
        typejr-romaji-definition))))

(defun typejr-rewrite-mondai ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "ひらがな問題文                ：")
    (delete-region (point) (point-at-eol))
    (insert typejr-cur-mondai)))

(defun typejr-rewrite-inputed-mondai ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "いままで入力した問題文        ：")
    (delete-region (point) (point-at-eol))
    (insert typejr-cur-inputed-mondai)))

(defun typejr-rewrite-hiragana ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "次にうつべきひらがな          ：")
    (delete-region (point) (point-at-eol))
    (insert typejr-cur-hiragana)))

(defun typejr-rewrite-romaji-candidates ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "次にうつべきローマ字の候補    ：")
    (delete-region (point) (point-at-eol))
    (mapc '(lambda (candidate)
             (let ((pt (point)))
               (insert (concat candidate " "))
               (put-text-property pt (+ pt typejr-cur-romaji-pos)
                                  'face 'typejr-good-romaji-face)))
          typejr-cur-romaji-candidates)))

(defun typejr-rewrite-romaji-pos ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "ローマ字何文字目まで入力したか：")
    (delete-region (point) (point-at-eol))
    (insert (number-to-string typejr-cur-romaji-pos))))

(defun typejr-rewrite-evaluation (evaluation)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "Evaluation: ")
    (delete-region (point) (point-at-eol))
    (insert evaluation)))





