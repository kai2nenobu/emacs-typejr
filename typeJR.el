
(defvar typeJR-mondai-alist nil)
(defvar typeJR-kanji-list nil)
(defvar typeJR-random-times 20)
(defvar typeJR-cur-mondai nil)
(defvar typeJR-cur-inputed-mondai "")
(defvar typeJR-cur-hiragana "")
(defvar typeJR-cur-hiragana-pos 0)
(defvar typeJR-cur-romaji-candidates nil)
(defvar typeJR-cur-romaji-pos 0)
(defvar typeJR-cur-char nil)
(defvar typeJR-romaji-definition nil)
(defvar typeJR-sokuon "っ")
(defvar typeJR-hatuon "ん")
(defvar typeJR-na-gyou '("な" "に" "ぬ" "ね" "の" "にゃ" "にぃ" "にゅ" "にぇ" "にょ" "ん"))
(defvar typeJR-boin '("あ" "い" "う" "え" "お"))
(defvar typeJR-nn-flag nil)
(defvar typeJR-wrong-flag nil)
(defvar typeJR-mode-map nil "local map for typeJR mode")

(defface typeJR-good-romaji-face
  '((t (:foreground "gray60"))) nil)

(defface typeJR-wrong-romaji-face
  '((t (:foreground "cyan" :background "red"))) nil)

(defface typeJR-cur-romaji-face
  '((t (:foreground "cyan"))) nil)


(defun typeJR-practice-prepare ()
  ""
  (interactive)
  (erase-buffer)
  (goto-char (point-min))
  (insert "

Time: 
Evaluation: 
Type: 

ひらがな問題文                ：
いままで入力した問題文        ：
次にうつべきひらがな          ：
次にうつべきローマ字の候補    ：
ローマ字何文字目まで入力したか："))

(defun typeJR-initial ()
  ""
  (interactive)
  ;; configuration buffer and major mode
  (switch-to-buffer "*typeJR*")
  (setq major-mode 'typeJR-mode)
  (setq mode-name "typeJR")
  (setq typeJR-mode-map (make-sparse-keymap))
  (define-key typeJR-mode-map (kbd "C-c C-p") 'typeJR-practice)
  (use-local-map typeJR-mode-map)
  (typeJR-read-romaji-definition)
  (typeJR-read-mondai-alist)
  (typeJR-practice-prepare)
  (font-lock-mode 0))

(defun typeJR-practice ()
  ""
  (interactive)
  (typeJR-clear-type)
  (random t)                            ; change seed
  (setq typeJR-kanji-list
        (mapcar '(lambda (x)
                   (car (nth (random x) typeJR-mondai-alist)))
                (make-list typeJR-random-times (length typeJR-mondai-alist))))
  (typeJR-rewrite-kanji)
  (condition-case err
      (progn
        (typeJR-stop-watch-start)
        (let ((cnt 0)
              (echo-keystrokes 0))            ; don't echo key type in echo area
          (while (< cnt typeJR-random-times)
            (setq typeJR-cur-mondai
                  (cdr (assoc (nth cnt typeJR-kanji-list) typeJR-mondai-alist)))
            (typeJR-mondai-loop)
            ;; input space between mondais
            (unless (= cnt (1- typeJR-random-times))
              (let (inputed-char)
                (while (not (= (setq inputed-char (read-char)) ? ))
                  (if (and typeJR-nn-flag (= inputed-char ?n))
                      ;; accept "n"
                      (progn
                        (setq typeJR-nn-flag nil)
                        (insert inputed-char))
                    (setq typeJR-wrong-flag t)
                    (typeJR-beep-when-wrong)
                    (typeJR-rewrite-romaji-candidates)))
                (insert inputed-char)))                       ; insert space
            (setq cnt (1+ cnt)))
          (cc:signal-send typeJR-stop-watch-channel 'stop)
          (typeJR-rewrite-evaluation "Finish!")))
    (quit
     (cc:signal-send typeJR-stop-watch-channel 'stop)
     (error "Typing is interrupted"))))

(defun typeJR-mondai-loop ()
  ""
  (interactive)
  ;; pre processing
  (setq typeJR-cur-hiragana-pos 0)
  (setq typeJR-cur-inputed-mondai "")
  (setq typeJR-nn-flag nil)
  (setq typeJR-wrong-flag nil)
  (typeJR-rewrite-inputed-mondai)
  (typeJR-rewrite-evaluation "")
  (while (not (typeJR-end-of-mondai-p))
    (setq typeJR-cur-hiragana
          (typeJR-get-cur-hiragana))
    (setq typeJR-cur-romaji-candidates
          (typeJR-get-romaji-candidates typeJR-cur-hiragana))
    (typeJR-rewrite-mondai)
    ;; deal with special mora
    (cond ((string= typeJR-cur-hiragana typeJR-sokuon)
           (typeJR-deal-with-sokuon))
          ((string= typeJR-cur-hiragana typeJR-hatuon)
           (typeJR-deal-with-hatuon)))
    (typeJR-rewrite-hiragana)
    (typeJR-hiragana-loop)
    ;; deal with nn-flag
    (typeJR-deal-with-nn-flag)
    (setq typeJR-cur-hiragana-pos
          (+ typeJR-cur-hiragana-pos (length typeJR-cur-hiragana)))
    (setq typeJR-cur-inputed-mondai
          (concat typeJR-cur-inputed-mondai typeJR-cur-hiragana))
    (typeJR-rewrite-inputed-mondai))
  ;; post processing
  (typeJR-rewrite-mondai)
  (typeJR-rewrite-inputed-mondai)
  (typeJR-rewrite-evaluation "Mondai ends!"))

(defun typeJR-hiragana-loop ()
  ""
  (setq typeJR-cur-romaji-pos 0)
  (typeJR-rewrite-romaji-candidates)
  (typeJR-rewrite-romaji-pos)
  (while (not (typeJR-end-of-hiragana-p))
    (let* ((inputed-romaji (read-char)) ; recieve keyboard input
           (correct-list (typeJR-get-correct-candidate-list inputed-romaji)))
      (cond ((or (and typeJR-nn-flag (= inputed-romaji ?n))   ; accept "n"
                 (> (length correct-list) 0))           ; correct input
             (when (> (length correct-list) 0)
                 (setq typeJR-cur-romaji-pos
                       (1+ typeJR-cur-romaji-pos))
                 (setq typeJR-cur-romaji-candidates correct-list))
             (setq typeJR-nn-flag nil)
             (setq typeJR-wrong-flag nil)
             (insert inputed-romaji)
             (typeJR-rewrite-romaji-candidates)
             (typeJR-rewrite-mondai)
             (typeJR-rewrite-romaji-pos)
             (typeJR-rewrite-evaluation "Good!"))
            (t                          ; wrong input
             (setq typeJR-wrong-flag t)
             (typeJR-beep-when-wrong)
             (typeJR-rewrite-romaji-candidates)
             (typeJR-rewrite-mondai))))))

(defun typeJR-beep-when-wrong ()
  (let ((visible-bell nil)
        (ring-bell-function 'beep))
    (beep)
    (typeJR-rewrite-evaluation "Wrong")))

(defun typeJR-deal-with-sokuon ()
  "Add initial romaji of next hiragana to `typeJR-cur-romaji-candidates' to deal with sokuon."
  (let* ((next-hiragana (typeJR-get-next-hiragana))
         (next-candidates (typeJR-get-romaji-candidates next-hiragana))
         new-candidates)
    (if (or (member next-hiragana typeJR-na-gyou)
            (member next-hiragana typeJR-boin))
        nil
      ;; version to concat sokuon and next hiragana.
      ;; (mapc '(lambda (pre)
      ;;          (mapc '(lambda (post)
      ;;                   (add-to-list 'new-candidates (concat pre post)))
      ;;                next-candidates))
      ;;       typeJR-cur-romaji-candidates)
      ;; (nreverse new-candidates)
      (setq new-candidates (mapcar '(lambda (candidate) (substring candidate 0 1))
                                   next-candidates)) ; get initial romaji
      (let ((cnt 0))   ; delete duplicate elements
        (while (< cnt (length new-candidates))
          (let* ((elm (nth cnt new-candidates)))
            (setq new-candidates (delete elm new-candidates))
            (if (= cnt 0)
                (setq new-candidates (cons elm new-candidates))
              (setcdr (nthcdr (1- cnt) new-candidates)
                      (cons elm (nthcdr cnt new-candidates)))))
          (setq cnt (1+ cnt))))
      (setq typeJR-cur-romaji-candidates
            (append typeJR-cur-romaji-candidates new-candidates))
      )))

(defun typeJR-deal-with-hatuon ()
  "Remove \"n\" from candidates of hatuon when next hiragana is na-gyou or boin."
  (let ((next-hiragana (typeJR-get-next-hiragana)))
    (when (or (member next-hiragana typeJR-na-gyou)
              (member next-hiragana typeJR-boin))
      (setq typeJR-cur-romaji-candidates
            (delete "n" typeJR-cur-romaji-candidates)))))

(defun typeJR-deal-with-nn-flag ()
  "\"nn\""
  (let ((next-hiragana (typeJR-get-next-hiragana)))
    (when (and (not (member next-hiragana typeJR-na-gyou))
               (not (member next-hiragana typeJR-boin))
               (member "n" typeJR-cur-romaji-candidates))
      (setq typeJR-nn-flag t))))

(defun typeJR-end-of-mondai-p ()
  ""
  (<= (length typeJR-cur-mondai) typeJR-cur-hiragana-pos))

(defun typeJR-end-of-hiragana-p ()
  ""
  (<= (car (sort (mapcar 'length typeJR-cur-romaji-candidates) '<))
      typeJR-cur-romaji-pos))

(defun typeJR-get-cur-hiragana ()
  ""
  (let* ((len (length typeJR-cur-mondai))
         (two-hiragana
          (substring typeJR-cur-mondai
                     typeJR-cur-hiragana-pos (min (+ 2 typeJR-cur-hiragana-pos) len)))
         (one-hiragana
          (substring typeJR-cur-mondai
                     typeJR-cur-hiragana-pos (min (1+ typeJR-cur-hiragana-pos) len))))
    (if (assoc two-hiragana typeJR-romaji-definition)
        (car (assoc two-hiragana typeJR-romaji-definition))
      (car (assoc one-hiragana typeJR-romaji-definition)))
    ))

(defun typeJR-get-next-hiragana ()
  ""
  (let ((typeJR-cur-hiragana-pos
         (+ typeJR-cur-hiragana-pos (length typeJR-cur-hiragana))))
    (typeJR-get-cur-hiragana)))

(defun typeJR-get-correct-candidate-list (romaji)
  "Pick up correct candidate list from `typeJR-cur-romaji-candidates'."
  (let (lst)
    (mapc '(lambda (candidate)
             (when (= (aref candidate typeJR-cur-romaji-pos) romaji)
               (add-to-list 'lst candidate)))
          typeJR-cur-romaji-candidates)
    (nreverse lst)))

(defun typeJR-get-romaji-candidates (hiragana)
  "Return list of candidates of romaji definition corresponding to HIRAGANA."
  (cdr (assoc hiragana typeJR-romaji-definition)))

(defun typeJR-read-mondai-alist ()
  "Read mondai list from \"mondai-list.txt\"."
  (setq typeJR-mondai-alist nil)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      ;; Is this a good method to find exting file?
      (insert-file-contents (locate-library "mondai-list.txt"))
      (let ((str-per-line (split-string (buffer-string) "\n")))  ; process buffer per line
        (delete "" str-per-line) ; ignore empty line
        (let ((cnt 0))
          (while (< cnt (length str-per-line))
            (add-to-list 'typeJR-mondai-alist (cons (nth cnt str-per-line)
                                                    (nth (1+ cnt) str-per-line)))
            (setq cnt (+ 2 cnt))))
        typeJR-mondai-alist))))

(defun typeJR-read-romaji-definition ()
  "Read romaji henkan definition from \"romaji-definition.txt\"."
  (setq typeJR-romaji-definition nil)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      ;; Is this a good method to find exting file?
      (insert-file-contents (locate-library "romaji-definition.txt"))
      (let ((str-per-line (split-string (buffer-string) "\n")))  ; process buffer per line
        (delete "" str-per-line) ; ignore empty line
        (mapc '(lambda (line)
                   (add-to-list 'typeJR-romaji-definition (split-string line)))
              str-per-line)
        typeJR-romaji-definition))))

(defun typeJR-rewrite-kanji ()
  ""
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^$")
    (unless (= (point) (point-min))
      (backward-char 1)
      (delete-region (point-min) (point)))
    (insert (mapconcat 'eval typeJR-kanji-list "　"))
    (fill-paragraph)))

(defun typeJR-rewrite-mondai ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "ひらがな問題文                ：")
    (delete-region (point) (point-at-eol))
    (let* ((beg-pt (point))
           (cur-pt (+ beg-pt typeJR-cur-hiragana-pos)))
      (insert typeJR-cur-mondai)
      (put-text-property beg-pt cur-pt 'face 'typeJR-good-romaji-face)
      (put-text-property cur-pt (min (+ (length typeJR-cur-hiragana) cur-pt)
                                     (point-at-eol))
                         'face (if typeJR-wrong-flag
                                   'typeJR-wrong-romaji-face
                                 'typeJR-cur-romaji-face)))))

(defun typeJR-rewrite-inputed-mondai ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "いままで入力した問題文        ：")
    (delete-region (point) (point-at-eol))
    (insert typeJR-cur-inputed-mondai)))

(defun typeJR-rewrite-hiragana ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "次にうつべきひらがな          ：")
    (delete-region (point) (point-at-eol))
    (insert typeJR-cur-hiragana)))

(defun typeJR-rewrite-romaji-candidates ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "次にうつべきローマ字の候補    ：")
    (delete-region (point) (point-at-eol))
    (mapc '(lambda (candidate)
             (let* ((beg-pt (point))
                    (cur-pt (+ beg-pt typeJR-cur-romaji-pos)))
               (insert (concat candidate " "))
               (put-text-property beg-pt cur-pt
                                  'face 'typeJR-good-romaji-face)
               (put-text-property cur-pt (1+ cur-pt)
                                  'face (if typeJR-wrong-flag
                                            'typeJR-wrong-romaji-face
                                          'typeJR-cur-romaji-face))))
          typeJR-cur-romaji-candidates)))

(defun typeJR-rewrite-romaji-pos ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "ローマ字何文字目まで入力したか：")
    (delete-region (point) (point-at-eol))
    (insert (number-to-string typeJR-cur-romaji-pos))))

(defun typeJR-rewrite-evaluation (evaluation)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "Evaluation: ")
    (delete-region (point) (point-at-eol))
    (insert evaluation)))

(defun typeJR-clear-type ()
  (goto-char (point-min))
  (re-search-forward "Type: ")
  (delete-region (point) (point-at-eol)))


(defun typeJR-stop-watch-start ()
  (setq typeJR-stop-watch-channel (cc:signal-channel))
  (cc:signal-connect typeJR-stop-watch-channel 'stop 'typeJR-stop-watch-stop)
  (unless typeJR-stop-watch-thread
    (setq typeJR-stop-watch-thread t))
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^Time: ")
    (lexical-let ((beg-time (current-time))
                  (pos (point)))
      (cc:thread
       50
       (message "Stop watch start.")
       (while typeJR-stop-watch-thread
         (save-excursion
           (goto-char pos) (delete-region pos (point-at-eol))
           (insert (concat (sub-time-string beg-time) " sec"))))))))

(defvar typeJR-stop-watch-thread nil)
(defvar typeJR-stop-watch-channel nil)

(defun typeJR-stop-watch-stop ()
  (setq typeJR-stop-watch-thread nil)
  (message "Stop watch stop."))

(defun sub-time-string (beg-time)
  (let ((time (current-time)))
  (format "%6.3f" (+ (- (nth 1 time) (nth 1 beg-time)) ; sec
                    (/ (- (nth 2 time) (nth 2 beg-time)) 1000000.0))))) ; msec