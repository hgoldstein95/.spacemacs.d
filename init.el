(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(python
     ruby
     (c-c++ :variables
            c-c++-backend 'lsp-clangd
            c-c++-default-mode-for-headers 'c++-mode)
     csv
     dap
     yaml
     sql
     (haskell :variables
              haskell-completion-backend 'company-gchi)
     graphviz
     octave
     auto-completion
     better-defaults
     emacs-lisp
     git
     helm
     html
     javascript
     (latex :variables
            latex-build-command "LaTeX")
     lsp
     markdown
     ocaml
     org
     (rust :variables
           rust-backend 'lsp)
     (shell :variables
            shell-default-height 20
            shell-default-position 'bottom
            shell-default-shell 'eshell
            shell-enable-smart-eshell t)
     spotify
     syntax-checking
     (theming :variables
              theming-headings-same-size 'all)
     twitter
     version-control
     )
   dotspacemacs-frozen-packages '()
   dotspacemacs-additional-packages '(helm-rg base16-theme)
   dotspacemacs-excluded-packages '(evil-search-highlight-persist)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((projects . 3)
                                (recents . 4))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(base16-nord
                         zenburn
                         solarized-light
                         material
                         flatland
                         spacemacs-light
                         spacemacs-dark
                         material-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Noto Mono"
                               :size 14.0
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   dotspacemacs-ex-substitute-global t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize t
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.6
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing))

(defun dotspacemacs/user-init ()
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (setq ranger-override-dired t)
  (setq custom-file "~/.spacemacs.d/custom.el")
  (load custom-file 'noerror)
  (setq package-check-signature nil))

(defun dotspacemacs/user-config ()
  (evil-ex-define-cmd "q[uit]" 'evil-window-delete)

  (setq-default fill-column 100)
  (add-hook 'prog-mode-hook 'spacemacs/toggle-fill-column-indicator-on)
  (add-hook 'text-mode-hook 'spacemacs/toggle-fill-column-indicator-on)

  (setq projectile-enable-caching t)

  (defun hjg/split-window-right-and-helm ()
    (interactive)
    (split-window-right-and-focus)
    (helm-mini))

  (spacemacs/set-leader-keys "wV" 'hjg/split-window-right-and-helm)
  (spacemacs/set-leader-keys "dd" 'kill-buffer-and-window)
  (spacemacs/set-leader-keys "p*" 'helm-projectile-rg)
  (spacemacs/set-leader-keys "," 'comment-or-uncomment-region)

  ; Coq
  ;; (load "~/.spacemacs.d/packages/proof-general/generic/proof-site")
  ;; (with-eval-after-load "proof-script"
  ;;   (define-key proof-mode-map (kbd "<M-down>") 'proof-assert-next-command-interactive)
  ;;   (define-key proof-mode-map (kbd "<M-up>") 'proof-undo-last-successful-command))

  ; LaTeX
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  (setq LaTeX-command "latex -shell-escape")

  ; Org
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "el" 'org-latex-export-to-pdf)
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (defun hjg/config-export ()
    (defadvice org-export-output-file-name
        (before org-add-export-dir activate)
      (when (not pub-dir)
        (setq pub-dir "export")
        (when (not (file-directory-p pub-dir))
          (make-directory pub-dir)))))
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "dot")))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)
                                 (octave . t)
                                 (dot . t)))
  (with-eval-after-load 'org
    (hjg/config-export)
    (setq org-export-backends '(beamer html latex md gfm))
    (setq org-ellipsis "⬎")
    (setq org-directory "~/org")
    (setq org-bullets-bullet-list '("▣" "►" "■" "▸" "▪"))
    (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
    (setq org-agenda-files (list "~/org" "~/Research/notes")))

  ; Markdown
  (add-hook 'markdown-mode-hook 'auto-fill-mode)

  ; Rust
  ; NOTE This was pulled from the rust-mode source, and patched manually. Once #269 is merged, this
  ; can be deleted and we can simply set the rust-format-on-save variable for the rust layer.
  (defun patched-rust-format-buffer ()
    "Format the current buffer using rustfmt."
    (interactive)
    (unless (executable-find rust-rustfmt-bin)
      (error "Could not locate executable \"%s\"" rust-rustfmt-bin))

    (let* ((current (current-buffer))
           (base (or (buffer-base-buffer current) current))
           buffer-loc
           window-loc)
      (dolist (buffer (buffer-list))
        (when (or (eq buffer base)
                  (eq (buffer-base-buffer buffer) base))
          (push (list buffer
                      (rust--format-get-loc buffer nil))
                buffer-loc)))
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (when (or (eq buffer base)
                    (eq (buffer-base-buffer buffer) base))
            (let ((start (window-start window))
                  (point (window-point window)))
              (push (list window
                          (rust--format-get-loc buffer start)
                          (rust--format-get-loc buffer point))
                    window-loc)))))
      (unwind-protect
          (let ((w-start (window-start)))
            (rust--format-call (current-buffer))
            (set-window-start (selected-window) w-start)
            )
        (dolist (loc buffer-loc)
          (let* ((buffer (pop loc))
                 (pos (rust--format-get-pos buffer (pop loc))))
            (with-current-buffer buffer
              (goto-char pos))))
        (dolist (loc window-loc)
          (let* ((window (pop loc))
                 (buffer (window-buffer window))
                 (start (rust--format-get-pos buffer (pop loc)))
                 (pos (rust--format-get-pos buffer (pop loc))))
            (unless (eq buffer current)
              (set-window-start window start))
            (set-window-point window pos))))))

  (defun generate-rusty-tags ()
    (interactive)
    (shell-command "rusty-tags -O TAGS emacs"))
  (spacemacs/set-leader-keys-for-major-mode 'rust-mode "G" 'generate-rusty-tags)
  (add-hook 'rust-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'patched-rust-format-buffer nil 'make-it-local)))

  ; Haskell
  (spacemacs/set-leader-keys-for-major-mode 'haskell-mode "f" 'hindent-reformat-buffer)
  (with-eval-after-load 'intero
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

  ; C++
  (spacemacs/set-leader-keys-for-major-mode 'c++-mode "b" 'compile)
  (spacemacs/set-leader-keys-for-major-mode 'c++-mode "f" 'clang-format-buffer)
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq flycheck-clang-include-path (list "/usr/local/boost_1_71_0/"))
              (setq compilation-read-command nil)
              (setq flycheck-clang-language-standard "c++17")
              (add-hook 'before-save-hook 'clang-format-buffer nil 'make-it-local)))

  ; Emacs Lisp
  (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "b" 'eval-buffer))
