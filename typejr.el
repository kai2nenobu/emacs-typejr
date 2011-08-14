

(defvar typejr-mondai-alist
  '(("懇親会" . "こんしんかい")))

(defvar typejr-current-mondai "こんしんかい")
(defvar typejr-current-inputed-mondai "")
(defvar typejr-current-hiragana "")
(defvar typejr-current-hiragana-position 0)
(defvar typejr-current-romaji-candidates nil)
(defvar typejr-current-romaji-position 0)
(defvar typejr-current-char nil)
(defvar typejr-romaji-definition nil)

(defface typejr-good-romaji-face
  '((t (:foreground "red"))) nil)

(defun typejr-initial ()
  ""
  (typejr-read-romaji-definition))

(defun typejr-mondai-loop ()
  ""
  (setq typejr-current-hiragana-position 0)
  (setq typejr-current-inputed-mondai "")
  (typejr-rewrite-mondai)
  (typejr-rewrite-evaluation "")
  (while (not (typejr-end-of-mondai-p))
    (setq typejr-current-hiragana
          (substring typejr-current-mondai typejr-current-hiragana-position
                     (1+ typejr-current-hiragana-position)))
    (setq typejr-current-romaji-candidates
          (typejr-get-romaji-candidates typejr-current-hiragana))
    (typejr-rewrite-inputed-mondai)
    (typejr-rewrite-hiragana)
    (typejr-hiragana-loop)
    (setq typejr-current-hiragana-position (1+ typejr-current-hiragana-position))
    (setq typejr-current-inputed-mondai
          (concat typejr-current-inputed-mondai typejr-current-hiragana))))

(defun typejr-hiragana-loop ()
  ""
  (setq typejr-current-romaji-position 0)
  (while (not (typejr-end-of-hiragana-p))
    (typejr-rewrite-romaji-candidates)
    (typejr-rewrite-romaji-position)
    (let* ((inputed-romaji (read-char))
           (correct-list (typejr-get-correct-candidate-list inputed-romaji)))
      (if (> (length correct-list) 0)
          (progn
            (setq typejr-current-romaji-position
                  (1+ typejr-current-romaji-position))
            (setq typejr-current-romaji-candidates correct-list)
            (typejr-rewrite-evaluation "Good!"))
        (let ((visible-bell nil)
              (ring-bell-function 'beep))
          (ding)
          (typejr-rewrite-evaluation "Bad"))))))

(defun typejr-end-of-mondai-p ()
  ""
  (<= (length typejr-current-mondai) typejr-current-hiragana-position))

(defun typejr-end-of-hiragana-p ()
  ""
  (<= (car (sort (mapcar 'length typejr-current-romaji-candidates) '<))
      typejr-current-romaji-position))

(defun typejr-get-correct-candidate-list (romaji)
  "Pick up correct candidate list from typejr-current-romaji-candidates."
  (let (lst)
    (mapc '(lambda (candidate)
             (when (= (aref candidate typejr-current-romaji-position) romaji)
               (add-to-list 'lst candidate)))
          typejr-current-romaji-candidates)
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
    (insert typejr-current-mondai)))

(defun typejr-rewrite-inputed-mondai ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "いままで入力した問題文        ：")
    (delete-region (point) (point-at-eol))
    (insert typejr-current-inputed-mondai)))

(defun typejr-rewrite-hiragana ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "次にうつべきひらがな          ：")
    (delete-region (point) (point-at-eol))
    (insert typejr-current-hiragana)))

(defun typejr-rewrite-romaji-candidates ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "次にうつべきローマ字の候補    ：")
    (delete-region (point) (point-at-eol))
    (mapc '(lambda (candidate)
             (let ((pt (point)))
               (insert (concat candidate " "))
               (put-text-property pt (+ pt typejr-current-romaji-position)
                                  'face 'typejr-good-romaji-face)))
          typejr-current-romaji-candidates)))

(defun typejr-rewrite-romaji-position ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "ローマ字何文字目まで入力したか：")
    (delete-region (point) (point-at-eol))
    (insert (number-to-string typejr-current-romaji-position))))

(defun typejr-rewrite-evaluation (evaluation)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "Evaluation: ")
    (delete-region (point) (point-at-eol))
    (insert evaluation)))





