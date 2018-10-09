;;; ein-process.el --- Notebook list buffer

;; Copyright (C) 2018- John M. Miller

;; Authors: Takafumi Arakaki <aka.tkf at gmail.com>
;;          John M. Miller <millejoh at mac.com>

;; This file is NOT part of GNU Emacs.

;; ein-process.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-process.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-process.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:

(eval-when-compile (require 'cl))

(require 'ein-core)
(require 'ein-notebook)
(require 'ein-file)
(require 'deferred)
(require 'dash)

(defstruct ein:$process
  "Hold process variables.

`ein:$process-pid'
  PID.

`ein:$process-port'
  Arg of --port or .

`ein:$process-ip'
  Arg of --notebook-ip or 'localhost'.

`ein:$process-dir'
  Arg of --notebook-dir or 'readlink -e /proc/<pid>/cwd'."
  pid
  port
  dir
)

(ein:deflocal ein:%processes% (make-hash-table :test #'equal)
  "Process table of `ein:$process' keyed on dir.")

(defvar ein:process-buffer-name-template "*ein:process %s*")

(defvar ein:process-map (make-hash-table :test 'equal)
  "Data store for `ein:process-list'.
Mapping from URL-OR-PORT to an instance of `ein:$process'.")

(defun ein:process-list ()
  "Get a list of opened `ein:$process'."
  (ein:hash-vals ein:process-map))

(defun ein:process-list-add (nblist)
  "Register notebook list instance NBLIST for global lookup.
This function adds NBLIST to `ein:process-map'."
  (puthash (ein:$process-url-or-port nblist)
           nblist
           ein:process-map))

(defun ein:process-list-get (url-or-port)
  "Get an instance of `ein:$process' by URL-OR-PORT as a key."
  (gethash url-or-port ein:process-map))

;; TODO: FIXME. Use content API.
(defun ein:process-open-notebook-by-name (name &optional url-or-port
                                                    callback cbargs)
  "Open notebook named NAME in the server URL-OR-PORT.
If URL-OR-PORT is not given or `nil', and the current buffer is
the notebook list buffer, the notebook is searched in the
notebook list of the current buffer.

When used in lisp, CALLBACK and CBARGS are passed to `ein:notebook-open'.
To suppress popup, you can pass `ignore' as CALLBACK."
  (loop with nblist = (if url-or-port
                          (ein:process-list-get url-or-port)
                        ein:%process%)
        for note in (ein:$process-data nblist)
        for notebook-name = (plist-get note :name)
        for notebook-path = (plist-get note :path)
        when (equal notebook-name name)
        return (ein:notebook-open (ein:$process-url-or-port nblist)
                                  notebook-path nil callback cbargs)))

(defun ein:process-url (url-or-port version &optional path)
  (let ((base-path (cond ((= version 2) "api/notebooks")
                         ((>= version 3) "api/contents"))))
    (if path
        (ein:url url-or-port base-path (or path ""))
      (ein:url url-or-port base-path))))

(defun ein:process-new-url (url-or-port version &optional path)
  (let ((base-path (cond ((= version 2) "api/notebooks")
                         ((>= version 3) "api/contents"))))
    (ein:log 'info "New notebook %s" (concat (file-name-as-directory url-or-port) path))
    (if (and path (not (string= path "")))
        (ein:url url-or-port base-path path)
      (ein:url url-or-port base-path))))

(defun ein:process-get-buffer (url-or-port)
  (get-buffer-create
   (format ein:process-buffer-name-template url-or-port)))

(defun ein:process-ask-url-or-port ()
  (let* ((url-or-port-list (mapcar (lambda (x) (format "%s" x))
                                   ein:url-or-port))
         (default (format "%s" (ein:aif (ein:get-notebook)
                                   (ein:$notebook-url-or-port it)
                                 (ein:aif ein:%process%
                                     (ein:$process-url-or-port it)
                                   (ein:default-url-or-port)))))
         (url-or-port
          (completing-read (format "URL or port number (default %s): " default)
                           url-or-port-list
                           nil nil nil nil
                           default)))
    (if (string-match "^[0-9]+$" url-or-port)
        (string-to-number url-or-port)
      (unless (string-match "^https?:" url-or-port)
        (error "EIN doesn't want to assume what protocol you are using (http or https), so could you please specify the full URL (e.g http://my.jupyter.url:8888?"))
      url-or-port)))

;;;###autoload
(defun ein:process-open (url-or-port &optional path no-popup resync)
  "Open notebook list buffer."
  (interactive (list (ein:process-ask-url-or-port)))
  (unless path (setq path ""))
  (if (and (stringp url-or-port) (not (string-match-p "^https?" url-or-port)))
      (setq url-or-port (format "http://%s" url-or-port)))
  (ein:subpackages-load)
  (lexical-let ((url-or-port url-or-port)
                (path path)
                (success (if no-popup
                             #'ein:process-open--finish
                           (lambda (content)
                             (pop-to-buffer
                              (funcall #'ein:process-open--finish content))))))
    (if (or resync (not (ein:process-list-get url-or-port)))
        (deferred:$
          (deferred:parallel
            (lexical-let ((d (deferred:new #'identity)))
              (ein:query-ipython-version url-or-port (lambda ()
                                                       (deferred:callback-post d)))
              d)
            (lexical-let ((d (deferred:new #'identity)))
              (ein:query-kernelspecs url-or-port (lambda ()
                                                   (deferred:callback-post d)))
              d)
            (lexical-let ((d (deferred:new #'identity)))
              (ein:content-query-hierarchy url-or-port (lambda (tree)
                                                         (deferred:callback-post d)))
              d))
          (deferred:nextc it
            (lambda (&rest ignore)
              (ein:content-query-contents url-or-port path success))))
      (ein:content-query-contents url-or-port path success)))
  )

;; point of order (poo): ein:process-refresh-kernelspecs requeries the kernelspecs and calls ein:process-reload.  ein:process-reload already requeries the kernelspecs in one of its callbacks, so this function seems redundant.

;; (defun ein:process-refresh-kernelspecs (&optional url-or-port)
;;   (interactive (list (or (and ein:%process% (ein:$process-url-or-port ein:%process%))
;;                          (ein:process-ask-url-or-port))))
;;   (unless url-or-port
;;     (if ein:%process%
;;         (setq url-or-port (ein:$process-url-or-port ein:%process%))
;;       (setq url-or-port (ein:default-url-or-port))))
;;   (ein:query-kernelspecs url-or-port)
;;   (when ein:%process%
;;     (ein:process-reload ein:%process%))
;; )

(defcustom ein:process-keepalive-refresh-time 1
  "When the notebook keepalive is enabled, the frequency, IN
HOURS, with which to make calls to the jupyter content API to
refresh the notebook connection."
  :type 'float
  :group 'ein)

(defcustom ein:enable-keepalive nil
  "When non-nil, will cause EIN to automatically call
  `ein:process-enable-keepalive' after any call to
  `ein:process-open'."
  :type 'boolean
  :group 'ein)

(defcustom ein:process-date-format "%x"
  "The format spec for date in process mode.
See `ein:format-time-string'."
  :type '(or string function)
  :group 'ein)

(defvar ein:process--keepalive-timer nil)

;;;###autoload
(defun ein:process-enable-keepalive (&optional url-or-port)
  "Enable periodic calls to the notebook server to keep long running sessions from expiring.
By long running we mean sessions to last days, or weeks. The
frequency of the refresh (which is very similar to a call to
`ein:process-open`) is controlled by
`ein:process-keepalive-refresh-time`, and is measured in
terms of hours. If `ein:enable-keepalive' is non-nil this will
automatically be called during calls to `ein:process-open`."
  (interactive (list (ein:process-ask-url-or-port)))
  (unless ein:process--keepalive-timer
    (message "Enabling process keepalive...")
    (let ((success
           (lambda (content)
             (ein:log 'info "Refreshing process connection.")))
          (refresh-time (* ein:process-keepalive-refresh-time 60 60)))
      (setq ein:process--keepalive-timer
            (run-at-time 0.1 refresh-time #'ein:content-query-contents url-or-port "" success)))))

;;;###autoload
(defun ein:process-disable-keepalive ()
  "Disable the process keepalive calls to the jupyter notebook server."
  (interactive)
  (message "Disabling process keepalive...")
  (cancel-timer ein:process--keepalive-timer)
  (setq ein:process--keepalive-timer nil))

(defun ein:process-open--finish (content)
  "Called via `ein:process-open'."
  (let ((url-or-port (ein:$content-url-or-port content))
        (path (ein:$content-path content))
        (ipy-version (ein:$content-ipython-version content))
        (data (ein:$content-raw-content content)))
    (with-current-buffer (ein:process-get-buffer url-or-port)
      (let ((already-opened-p (ein:process-list-get url-or-port))
            (orig-point (point)))
        (setq ein:%process%
              (make-ein:$process :url-or-port url-or-port
                                      :path path
                                      :data data
                                      :api-version ipy-version))
        (ein:process-list-add ein:%process%)
        (ein:process-render ipy-version)
        (goto-char orig-point)
        (ein:log 'verbose "Opened process at %s" (concat (file-name-as-directory url-or-port) path))
        (unless already-opened-p
          (run-hooks 'ein:process-first-open-hook))
        (when ein:enable-keepalive
          (ein:process-enable-keepalive (ein:$content-url-or-port content)))
        (current-buffer)))))

(defun* ein:process-open-error (url-or-port path
                                     &key error-thrown
                                     &allow-other-keys)
  (ein:log 'error
    "ein:process-open-error %s: ERROR %s DATA %s" (concat (file-name-as-directory url-or-port) path) (car error-thrown) (cdr error-thrown)))

;;;###autoload
(defun ein:process-reload (process &optional resync)
  "Reload current Notebook list."
  (interactive (list ein:%process%))
  (when process
    (ein:process-open (ein:$process-url-or-port process)
                           (ein:$process-path process) t resync)))

(defun ein:process-refresh-related ()
  "Reload notebook list in which current notebook locates.
This function is called via `ein:notebook-after-rename-hook'."
  (ein:process-open (ein:$notebook-url-or-port ein:%notebook%)
                         (ein:$notebook-notebook-path ein:%notebook%) t))

(add-hook 'ein:notebook-after-rename-hook 'ein:process-refresh-related)

(defun ein:process-open-notebook (nblist path &optional callback cbargs)
  (ein:notebook-open (ein:$process-url-or-port nblist)
                     path
                     nil
                     callback
                     cbargs))

(defun ein:process-open-file (url-or-port path)
  (ein:file-open url-or-port
                 path))

;;;###autoload
(defun ein:process-upload-file (upload-path)
  (interactive "fSelect file to upload:")
  (unless ein:%process%
    (error "Only works when called from an ein:process buffer."))
  (let ((nb-path (ein:$process-path ein:%process%)))
    (ein:content-upload nb-path upload-path)))

;;;###autoload
(defun ein:process-new-notebook (&optional url-or-port kernelspec path callback cbargs)
  "Ask server to create a new notebook and open it in a new buffer."
  (interactive (list (ein:process-ask-url-or-port)
                     (completing-read
                      "Select kernel [default]: "
                      (ein:list-available-kernels (ein:$process-url-or-port ein:%process%)) nil t nil nil "default" nil)))
  (let ((path (or path (ein:$process-path (or ein:%process%
                                                   (ein:process-list-get url-or-port)))))
        (version (ein:$process-api-version (or ein:%process%
                                                    (ein:process-list-get url-or-port)))))
    (unless url-or-port
      (setq url-or-port (ein:$process-url-or-port ein:%process%)))
    (assert url-or-port nil
            (concat "URL-OR-PORT is not given and the current buffer "
                    "is not the notebook list buffer."))
    (let ((url (ein:process-new-url url-or-port
                                         version
                                         path)))
      (ein:query-singleton-ajax
       (list 'process-new-notebook url-or-port path)
       url
       :type "POST"
       :data (json-encode '((:type . "notebook")))
       :parser #'ein:json-read
       ;; (lambda ()
       ;;   (ein:html-get-data-in-body-tag "data-notebook-id"))
       :error (apply-partially #'ein:process-new-notebook-error
                               url-or-port path callback cbargs)
       :success (apply-partially #'ein:process-new-notebook-callback
                                 url-or-port kernelspec path callback cbargs)))))

(defun* ein:process-new-notebook-callback (url-or-port
                                                kernelspec
                                                path
                                                callback
                                                cbargs
                                                &key
                                                data
                                                &allow-other-keys
                                                &aux
                                                (no-popup t))
  (if data
      (let ((name (plist-get data :name))
            (path (plist-get data :path)))
        (if (= (ein:need-ipython-version url-or-port) 2)
            (if (string= path "")
                (setq path name)
              (setq path (format "%s/%s" path name))))
        (ein:notebook-open url-or-port path kernelspec callback cbargs))
    (ein:log 'info (concat "Oops. EIN failed to open new notebook. "
                           "Please find it in the notebook list."))
    (setq no-popup nil))
  ;; reload or open notebook list
  (ein:process-open url-or-port path no-popup))

(defun* ein:process-new-notebook-error
    (url-or-port callback cbargs
                 &key response &allow-other-keys
                 &aux
                 (no-popup t)
                 (error (request-response-error-thrown response))
                 (dest (request-response-url response)))
  (ein:log 'verbose
    "PROCESS-NEW-NOTEBOOK-ERROR url-or-port: %S; error: %S; dest: %S"
    url-or-port error dest)
  (ein:log 'error
    "Failed to open new notebook (error: %S). \
You may find the new one in the notebook list." error)
  (setq no-popup nil)
  (ein:process-open url-or-port "" no-popup))

;;;###autoload
(defun ein:process-new-notebook-with-name (name kernelspec url-or-port &optional path)
  "Open new notebook and rename the notebook."
  (interactive (let* ((url-or-port (or (ein:get-url-or-port)
                                       (ein:default-url-or-port)))
                      (kernelspec (completing-read
                                   "Select kernel [default]: "
                                   (ein:list-available-kernels url-or-port) nil t nil nil "default" nil))
                      (name (read-from-minibuffer
                             (format "Notebook name (at %s): " url-or-port))))
                 (list name kernelspec url-or-port)))
  (let ((path (or path (ein:$process-path
                        (or ein:%process%
                            (ein:get-notebook)
                            (gethash url-or-port ein:process-map))))))
    (ein:process-new-notebook
     url-or-port
     kernelspec
     path
     (lambda (notebook created name)
       (assert created)
       (with-current-buffer (ein:notebook-buffer notebook)
         (ein:notebook-rename-command name)
         ;; As `ein:notebook-open' does not call `pop-to-buffer' when
         ;; callback is specified, `pop-to-buffer' must be called here:
         (pop-to-buffer (current-buffer))))
     (list name))))

(defun ein:process-delete-notebook-ask (path)
  (when (y-or-n-p (format "Delete notebook %s?" path))
    (ein:process-delete-notebook path)))

(defun ein:process-delete-notebook (path)
  (ein:query-singleton-ajax
   (list 'process-delete-notebook
         (ein:$process-url-or-port ein:%process%) path)
   (ein:notebook-url-from-url-and-id
    (ein:$process-url-or-port ein:%process%)
    (ein:$process-api-version ein:%process%)
    path)
   :type "DELETE"
   :success (apply-partially (lambda (path process &rest ignore)
                               (ein:log 'info
                                 "Deleted notebook %s" path)
                               (ein:process-reload process))
                             path ein:%process%)))

;; Because MinRK wants me to suffer (not really, I love MinRK)...
(defun ein:get-actual-path (path)
  (ein:aif (cl-position ?/ path :from-end t)
      (substring path 0 it)
    ""))

(defun generate-breadcrumbs (path)
  "Given process path, generate alist of breadcrumps of form (name . path)."
  (let* ((paths (split-string path "/" t))
         (current-path "/")
         (pairs (list (cons "Home" ""))))
    (dolist (p paths pairs)
      (setf current-path (concat current-path "/" p)
            pairs (append pairs (list (cons p current-path)))))))

(defun* ein:nblist--sort-group (group by-param order)
  (sort group #'(lambda (x y)
                  (cond ((eql order :ascending)
                         (string-lessp (plist-get x by-param)
                                       (plist-get y by-param)))
                        ((eql order :descending)
                         (string-greaterp (plist-get x by-param)
                                          (plist-get y by-param)))))))

(defun ein:process--order-data (nblist-data sort-param sort-order)
  "Try to sanely sort the process data for the current path."
  (let* ((groups (-group-by #'(lambda (x) (plist-get x :type))
                            nblist-data))
         (dirs (ein:nblist--sort-group (cdr (assoc "directory" groups))
                                       sort-param
                                       sort-order))
         (nbs (ein:nblist--sort-group (cdr (assoc "notebook" groups))
                                      sort-param
                                      sort-order))
         (files (ein:nblist--sort-group (-flatten-n 1 (-map #'cdr (-group-by
                                             #'(lambda (x) (car (last (s-split "\\." (plist-get x :name)))))
                                             (cdr (assoc "file" groups)))))
                                        sort-param
                                        sort-order)))
    (-concat dirs nbs files)))

(defun render-header-ipy2 (&rest args)
  "Render the header (for ipython2)."
  ;; Create notebook list
  (widget-insert (format "IPython %s Notebook list\n\n" (ein:$process-api-version ein:%process%)))

  (let ((breadcrumbs (generate-breadcrumbs (ein:$process-path ein:%process%))))
    (dolist (p breadcrumbs)
      (lexical-let ((name (car p))
                    (path (cdr p)))
        (widget-insert " | ")
        (widget-create
         'link
         :notify (lambda (&rest ignore) (ein:process-open
                                         (ein:$process-url-or-port ein:%process%)
                                         path))
         name)))
    (widget-insert " |\n\n"))

  (widget-create
   'link
   :notify (lambda (&rest ignore) (ein:process-new-notebook
                                   (ein:$process-url-or-port ein:%process%)))
   "New Notebook")
  (widget-insert " ")
  (widget-create
   'link
   :notify (lambda (&rest ignore) (ein:process-reload ein:%process% t))
   "Reload List")
  (widget-insert " ")
  (widget-create
   'link
   :notify (lambda (&rest ignore)
             (browse-url
              (ein:url (ein:$process-url-or-port ein:%process%))))
   "Open In Browser")
  (widget-insert "\n"))

(defun render-header* (url-or-port &rest args)
  "Render the header (for ipython>=3)."
  (with-current-buffer (ein:process-get-buffer url-or-port)
    (widget-insert
     (if (< (ein:$process-api-version ein:%process%) 4)
         (format "IPython v%s Notebook list (%s)\n\n" (ein:$process-api-version ein:%process%) url-or-port)
       (format "Jupyter v%s Notebook list (%s)\n\n" (ein:$process-api-version ein:%process%) url-or-port)))

    (let ((breadcrumbs (generate-breadcrumbs (ein:$process-path ein:%process%))))
      (dolist (p breadcrumbs)
        (lexical-let ((url-or-port url-or-port)
                      (name (car p))
                      (path (cdr p)))
          (widget-insert " | ")
          (widget-create
           'link
           :notify (lambda (&rest ignore)
                     (ein:process-open url-or-port path))
           name)))
      (widget-insert " |\n\n"))

    (lexical-let* ((url-or-port url-or-port)
                   (kernels (ein:list-available-kernels url-or-port)))
      (if (null ein:%process-new-kernel%)
          (setq ein:%process-new-kernel% (ein:get-kernelspec url-or-port (caar kernels))))
      (widget-create
       'link
       :notify (lambda (&rest ignore) (ein:process-new-notebook
                                       url-or-port
                                       ein:%process-new-kernel%))
       "New Notebook")
      (widget-insert " ")
      (widget-create
       'link
       :notify (lambda (&rest ignore) (ein:process-reload ein:%process% t))
       "Resync")
      (widget-insert " ")
      (widget-create
       'link
       :notify (lambda (&rest ignore)
                 (browse-url (ein:url url-or-port)))
       "Open In Browser")

      (widget-insert "\n\nCreate New Notebooks Using Kernel:\n")
      (let* ((radio-widget (widget-create 'radio-button-choice
                                          :value (and ein:%process-new-kernel% (ein:$kernelspec-name ein:%process-new-kernel%))
                                          :notify (lambda (widget &rest ignore)
                                                    (setq ein:%process-new-kernel%
                                                          (ein:get-kernelspec url-or-port (widget-value widget)))
                                                    (message "New notebooks will be started using the %s kernel."
                                                             (ein:$kernelspec-display-name ein:%process-new-kernel%))))))
        (if (null kernels)
            (widget-insert "\n  No kernels found.")
          (dolist (k kernels)
            (widget-radio-add-item radio-widget (list 'item :value (car k)
                                                      :format (format "%s\n" (cdr k)))))
          (widget-insert "\n"))))))

(defun render-opened-notebooks (url-or-port &rest args)
  "Render the opened notebooks section (for ipython>=3)."
  ;; Opened Notebooks Section
  (with-current-buffer (ein:process-get-buffer url-or-port)
    (widget-insert "\n---------- All Opened Notebooks ----------\n\n")
    (loop for buffer in (ein:notebook-opened-buffers)
          do (progn (widget-create
                     'link
                     :notify (lexical-let ((buffer buffer))
                               (lambda (&rest ignore)
                                 (if (buffer-live-p buffer)
                                     (switch-to-buffer buffer)
                                   (ein:process-reload ein:%process%))))
                     "Open")
                    (widget-create
                     'link
                     :notify (lexical-let ((buffer buffer))
                               (lambda (&rest ignore)
                                 (if (buffer-live-p buffer)
                                     (kill-buffer buffer))
                                 (run-at-time 1 nil
                                              #'ein:process-reload
                                              ein:%process%)))
                     "Close")
                    (widget-insert " : " (buffer-name buffer))
                    (widget-insert "\n")))))

(defun ein:format-nbitem-data (name last-modified)
  (let ((dt (date-to-time last-modified)))
    (format "%-40s%+20s" name
            (ein:format-time-string ein:process-date-format dt))))

(defun render-directory (url-or-port sessions)
  (with-current-buffer (ein:process-get-buffer url-or-port)
    (widget-insert "\n------------------------------------------\n\n")
    (ein:make-sorting-widget "Sort by" ein:process-sort-field)
    (ein:make-sorting-widget "In Order" ein:process-sort-order)
    (widget-insert "\n")
    (loop for note in (ein:process--order-data (ein:$process-data ein:%process%)
                                                    ein:process-sort-field
                                                    ein:process-sort-order)
          for name = (plist-get note :name)
          for path = (plist-get note :path)
          for last-modified = (plist-get note :last_modified)
          ;; (cond ((= 2 api-version)
          ;;        (plist-get note :path))
          ;;       ((= 3 api-version)
          ;;        (ein:get-actual-path (plist-get note :path))))
          for type = (plist-get note :type)
          for opened-notebook-maybe = (ein:notebook-get-opened-notebook url-or-port path)
          do (widget-insert " ")
          if (string= type "directory")
          do (progn (widget-create
                     'link
                     :notify (lexical-let ((url-or-port url-or-port)
                                           (name name))
                               (lambda (&rest ignore)
                                 ;; each directory creates a whole new process
                                 (ein:process-open url-or-port
                                                        (ein:url (ein:$process-path ein:%process%) name))))
                     "Dir")
                    (widget-insert " : " name)
                    (widget-insert "\n"))
          if (and (string= type "file") (> (ein:need-ipython-version url-or-port) 2))
          do (progn (widget-create
                     'link
                     :notify (lexical-let ((url-or-port url-or-port)
                                           (path path))
                               (lambda (&rest ignore)
                                 (ein:process-open-file url-or-port path)))
                     "Open")
                    (widget-insert " ------ ")
                    (widget-create
                     'link
                     :notify (lexical-let ((path path))
                               (lambda (&rest ignore)
                                 (message "[EIN]: NBlist delete file command. Implement me!")))
                     "Delete")
                    (widget-insert " : " (ein:format-nbitem-data name last-modified))
                    (widget-insert "\n"))
          if (string= type "notebook")
          do (progn (widget-create
                     'link
                     :notify (lexical-let ((name name)
                                           (path path))
                               (lambda (&rest ignore)
                                 (run-at-time 3 nil
                                              #'ein:process-reload ein:%process%) ;; TODO using deferred better?
                                 (ein:process-open-notebook
                                  ein:%process% path)))
                     "Open")
                    (widget-insert " ")
                    (if (gethash path sessions)
                        (widget-create
                         'link
                         :notify (lexical-let ((session (car (gethash path sessions)))
                                               (nblist ein:%process%))
                                   (lambda (&rest ignore)
                                     (run-at-time 1 nil
                                                  #'ein:process-reload
                                                  ein:%process%)
                                     (ein:kernel-kill (make-ein:$kernel :url-or-port (ein:$process-url-or-port nblist)
                                                                        :session-id session))))
                         "Stop")
                      (widget-insert "------"))
                    (widget-insert " ")
                    (widget-create
                     'link
                     :notify (lexical-let ((path path))
                               (lambda (&rest ignore)
                                 (ein:process-delete-notebook-ask
                                  path)))
                     "Delete")
                    (widget-insert " : " (ein:format-nbitem-data name last-modified))
                    (widget-insert "\n")))))

(defun ein:process-render (ipy-version)
  "Render notebook list widget.
Notebook list data is passed via the buffer local variable
`ein:process-data'."
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  (let ((url-or-port (ein:$process-url-or-port ein:%process%)))
    (ein:content-query-sessions url-or-port
                                (apply-partially #'ein:process-render--finish ipy-version url-or-port))))

(defun ein:process-render--finish (ipy-version url-or-port sessions)
  (cl-letf (((symbol-function 'render-header) (if (< ipy-version 3) 
                                                  #'render-header-ipy2 
                                                #'render-header*)))
    (mapc (lambda (x) (funcall (symbol-function x) url-or-port sessions))
          ein:process-render-order))
  (with-current-buffer (ein:process-get-buffer url-or-port)
    (ein:process-mode)
    (widget-setup)
    (goto-char (point-min))))

;;;###autoload

(defun ein:process-list-notebooks ()
  "Return a list of notebook path (NBPATH).  Each element NBPATH
is a string of the format \"URL-OR-PORT/PATH\"."
  (apply #'append
         (loop for nblist in (ein:process-list)
               for url-or-port = (ein:$process-url-or-port nblist)
               collect
               (loop for content in (ein:content-need-hierarchy url-or-port)
                     when (string= (ein:$content-type content) "notebook")
                     collect (format "%s/%s" url-or-port
                                     (ein:$content-path content)))
               ;; (if (= api-version 3)
               ;;     (loop for note in (ein:content-need-hierarchy url-or-port)
               ;;           collect (format "%s/%s" url-or-port
               ;;                           (ein:$content-path note)
               ;;                           ))
               ;;   (loop for note in (ein:$process-data nblist)
               ;;         collect (format "%s/%s"
               ;;                         url-or-port
               ;;                         (plist-get note :name))))
               )))

;;FIXME: Code below assumes notebook is in root directory - need to do a better
;;       job listing notebooks in subdirectories and parsing out the path.
;;;###autoload

(defun ein:process-open-notebook-global (nbpath &optional callback cbargs)
  "Choose notebook from all opened notebook list and open it.
Notebook is specified by a string NBPATH whose format is
\"URL-OR-PORT/NOTEBOOK-NAME\".

When used in lisp, CALLBACK and CBARGS are passed to `ein:notebook-open'."
  (interactive
   (list (if noninteractive
             (car (ein:process-list-notebooks)) 
           (completing-read
            "Open notebook [URL-OR-PORT/NAME]: "
            (ein:process-list-notebooks)))))
  (let* ((parsed (url-generic-parse-url nbpath))
         (path (url-filename parsed)))
    (ein:notebook-open (substring nbpath 0 (- (length nbpath) (length path))) 
                       (substring path 1) nil callback cbargs)))

;;;###autoload

(defun ein:process-load (&optional url-or-port)
  "Load notebook list but do not pop-up the notebook list buffer.

For example, if you want to load notebook list when Emacs starts,
add this in the Emacs initialization file::

  (add-to-hook 'after-init-hook 'ein:process-load)

or even this (if you want fast Emacs start-up)::

  ;; load notebook list if Emacs is idle for 3 sec after start-up
  (run-with-idle-timer 3 nil #'ein:process-load)

You should setup `ein:url-or-port' or `ein:default-url-or-port'
in order to make this code work.

See also:
`ein:connect-to-default-notebook', `ein:connect-default-notebook'."
  (ein:process-open url-or-port "" t))

(defun ein:process-nbpath-of-filepath (filepath)
  "Find \"URL-OR_PORT/PATH\" of FILEPATH"
  (car (mapcar (lambda (nbpath) 
                     (let* ((parsed (url-generic-parse-url nbpath))
                            (path (substring (url-filename parsed) 1)))
                       (string= (subseq filepath (- (length path))) path)))
                   (ein:process-list-notebooks))))

(defun ein:process-find-server-by-notebook-name (name)
  "Find a notebook named NAME and return a list (URL-OR-PORT PATH)."
  (loop named outer
        for nblist in (ein:process-list)
        for url-or-port = (ein:$process-url-or-port nblist)
        for ipython-version = (ein:$process-api-version nblist)
        do
        (if (>= ipython-version 3)
            (loop for note in (ein:content-need-hierarchy url-or-port)
                  when (equal (ein:$content-name note) name)
                  do (return-from outer
                       (list url-or-port (ein:$content-path note))))
          (loop for note in (ein:$process-data nblist)
                when (equal (plist-get note :name) name)
                do (return-from outer
                     (list url-or-port
                           (format "%s/%s" (plist-get note :path) (plist-get note :name))))))))

(defun ein:process-open-notebook-by-file-name
    (&optional filename noerror buffer-callback)
  "Find the notebook named as same as the current file in the servers.
Open the notebook if found.  Note that this command will *not*
upload the current file to the server.

.. When FILENAME is unspecified the variable `buffer-file-name'
   is used instead.  Set NOERROR to non-`nil' to suppress errors.
   BUFFER-CALLBACK is called after opening notebook with the
   current buffer as the only one argument."
  (interactive (progn (assert buffer-file-name nil "Not visiting a file.")
                      nil))
  (unless filename (setq filename buffer-file-name))
  (assert filename nil "No file found.")
  (let* ((name (file-name-sans-extension
                (file-name-nondirectory (or filename))))
         (found (ein:process-find-server-by-notebook-name name))
         (callback (lambda (-ignore-1- -ignore-2- buffer buffer-callback)
                     (ein:notebook-pop-to-current-buffer) ; default
                     (when (buffer-live-p buffer)
                       (funcall buffer-callback buffer))))
         (cbargs (list (current-buffer) (or buffer-callback #'ignore))))
    (unless noerror
      (assert found nil "No server has notebook named: %s" name))
    (destructuring-bind (url-or-port path) found
      (ein:notebook-open url-or-port path nil callback cbargs))))

(defvar ein:process-find-file-buffer-callback #'ignore)

(defun ein:process-find-file-callback ()
  "A callback function for `find-file-hook' to open notebook.

FIMXE: document how to use `ein:process-find-file-callback'
       when I am convinced with the API."
  (ein:and-let* ((filename buffer-file-name)
                 ((string-match-p "\\.ipynb$" filename)))
                (ein:process-open-notebook-by-file-name
                 filename t ein:process-find-file-buffer-callback)))


;;; Login

;;;###autoload

(defun ein:process-login (url-or-port password &optional retry-p)
  "Login to IPython notebook server."
  (interactive (list (ein:process-ask-url-or-port)
                     (read-passwd "Password: ")))
  (ein:query-singleton-ajax
   (list 'process-login url-or-port)
   (ein:url url-or-port "login")
   :type "POST"
   :data (concat "password=" (url-hexify-string password))
   :parser #'ein:process-login--parser
   :error (apply-partially #'ein:process-login--error url-or-port password retry-p)
   :success (apply-partially #'ein:process-login--success url-or-port)))

(defun ein:process-login--parser ()
  (goto-char (point-min))
  (list :bad-page (re-search-forward "<input type=.?password" nil t)))

(defun ein:process-login--success-1 (url-or-port)
  (ein:log 'info "Login to %s complete. \
Now you can open notebook list by `ein:process-open'." url-or-port))

(defun ein:process-login--error-1 (url-or-port)
  (ein:log 'info "Failed to login to %s" url-or-port))

(defun* ein:process-login--success (url-or-port &key
                                                     data
                                                     &allow-other-keys)
  (if (plist-get data :bad-page)
      (ein:process-login--error-1 url-or-port)
    (ein:process-login--success-1 url-or-port)))

(defun* ein:process-login--error
    (url-or-port password retry-p &key
                 data
                 symbol-status
                 response
                 &allow-other-keys
                 &aux
                 (response-status (request-response-status-code response)))
  (if (and (eq response-status 403)
           (not retry-p))
      (ein:process-login url-or-port password t))
  (if (or
       ;; workaround for url-retrieve backend
       (and (eq symbol-status 'timeout)
            (equal response-status 302)
            (request-response-header response "set-cookie"))
       ;; workaround for curl backend
       (and (equal response-status 405)
            (ein:aand (car (request-response-history response))
                      (request-response-header it "set-cookie"))))
      (ein:process-login--success-1 url-or-port)
    (ein:process-login--error-1 url-or-port)))

;;;###autoload

(defun ein:process-change-url-port (new-url-or-port)
  "Update the ipython/jupyter notebook server URL for all the
notebooks currently opened from the current process buffer.

This function works by calling `ein:notebook-update-url-or-port'
on all the notebooks opened from the current process."
  (interactive (list (ein:process-ask-url-or-port)))
  (unless (eql major-mode 'ein:process-mode)
    (error "This command needs to be called from within a process buffer."))
  (let* ((current-nblist ein:%process%)
         (old-url (ein:$process-url-or-port current-nblist))
         (new-url-or-port new-url-or-port)
         (open-nb (ein:notebook-opened-notebooks #'(lambda (nb)
                                                     (equal (ein:$notebook-url-or-port nb)
                                                            (ein:$process-url-or-port current-nblist))))))
    (ein:process-open new-url-or-port "" t)
    (loop for x upfrom 0 by 1
          until (or (get-buffer (format ein:process-buffer-name-template new-url-or-port))
                    (= x 100))
          do (sit-for 0.1))
    (dolist (nb open-nb)
      (ein:notebook-update-url-or-port new-url-or-port nb))
    (kill-buffer (ein:process-get-buffer old-url))
    (ein:process-open new-url-or-port "" nil)))

(defun ein:process-change-url-port--deferred (new-url-or-port)
  (lexical-let* ((current-nblist ein:%process%)
                 (old-url (ein:$process-url-or-port current-nblist))
                 (new-url-or-port new-url-or-port)
                 (open-nb (ein:notebook-opened-notebooks
                           (lambda (nb)
                             (equal (ein:$notebook-url-or-port nb)
                                    (ein:$process-url-or-port current-nblist))))))
    (deferred:$
      (deferred:next
        (lambda ()
          (ein:process-open new-url-or-port "" t)
          (loop until (get-buffer (format ein:process-buffer-name-template new-url-or-port))
                do (sit-for 0.1))))
      (deferred:nextc it
        (lambda ()
          (dolist (nb open-nb)
            (ein:notebook-update-url-or-port new-url-or-port nb))))
      (deferred:nextc it
        (lambda ()
          (kill-buffer (ein:process-get-buffer old-url))
          (ein:process-open new-url-or-port "" nil))))))

;;; Generic getter



(defun ein:get-url-or-port--process ()
  (when (ein:$process-p ein:%process%)
    (ein:$process-url-or-port ein:%process%)))


;;; Notebook list mode


(defun ein:process-prev-item () (interactive) (move-beginning-of-line 0))
(defun ein:process-next-item () (interactive) (move-beginning-of-line 2))

(defvar ein:process-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap widget-keymap
                                                 special-mode-map))
    (define-key map "\C-c\C-r" 'ein:process-reload)
    (define-key map "p" 'ein:process-prev-item)
    (define-key map "n" 'ein:process-next-item)
    map)
  "Keymap for ein:process-mode.")

(easy-menu-define ein:process-menu ein:process-mode-map
  "EIN Notebook List Mode Menu"
  `("EIN Notebook List"
    ,@(ein:generate-menu
       '(("Reload" ein:process-reload)
         ("New Notebook" ein:process-new-notebook)
         ("New Notebook (with name)"
          ein:process-new-notebook-with-name)
         ("New Junk Notebook" ein:junk-new)))))

(defun ein:process-revert-wrapper (&optional ignore-auto noconfirm preserve-modes)
  (ein:process-reload ein:%process%))

(define-derived-mode ein:process-mode special-mode "ein:process"
  "IPython notebook list mode.
Commands:
\\{ein:process-mode-map}"
  (set (make-local-variable 'revert-buffer-function)
       'ein:process-revert-wrapper))


(provide 'ein-process)

;;; ein-process.el ends here
