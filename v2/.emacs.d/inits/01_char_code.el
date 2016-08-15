;; --------------------------------------------------
;; 文字コード
;; --------------------------------------------------
;; --------------------------------------
;; 表:Emacsでのエンコーディング名       |
;; | 名前      | Emacsでの名前          |
;; |-----------+------------------------|
;; | UTF-8     | utf-8                  |
;; | Shift_JIS | sjis, shift_jis, cp932 |
;; | EUC-JP    | euc-jp, euc-japan      |
;; | JIS       | junet, iso-2022-jp     |
;; --------------------------------------
;; 表:改行コードと接尾辞
;; | OS      | 改行 | 接尾辞 |
;; |---------+------+--------|
;; | Windows | \r\n | -dos   |
;; | Mac     | \r   | -mac   |
;; | Linux等 | \n   | -unix  |
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; windows-file character-code
(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))
