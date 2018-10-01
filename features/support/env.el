(require 'f)
(require 'cl)
(require 'espuds)
(require 'ert)

(let* ((support-path (f-dirname load-file-name))
       (root-path (f-parent (f-parent support-path))))
  (add-to-list 'load-path (concat root-path "/lisp")))

(require 'ein-loaddefs)
(require 'ein-notebooklist)
(require 'ein-jupyter)

(ein:deflocal ein:%testing-port% nil)

(defun ein:testing-wait-until (predicate &optional predargs ms)
  "Wait until PREDICATE function returns non-`nil'.
  PREDARGS is argument list for the PREDICATE function.
  MS is milliseconds to wait."
  (let* ((subms 300)
         (count (max 1 (if ms (truncate (/ ms subms)) 25))))
    (unless (loop repeat count
                  when (apply predicate predargs)
                  return t
                  do (sleep-for 0 subms))
      (error "Timeout: %s" predicate))))

(Setup
 (setq ein:force-sync t)
 (setq ein:jupyter-server-args '("--no-browser" "--debug"))
 (deferred:sync! (ein:jupyter-server-start (executable-find "jupyter") (concat default-directory "log")))
 (assert (processp %ein:jupyter-server-session%) t "notebook server defunct")
 (setq ein:%testing-url% (car (ein:jupyter-server-conn-info))
))

(After
 (with-current-buffer (ein:notebooklist-get-buffer ein:%testing-url%)
   (loop for buffer in (ein:notebook-opened-buffers)
         do (let ((kill-buffer-query-functions nil))
              (with-current-buffer buffer (not-modified))
              (kill-buffer buffer)))
   (let ((sessions #s(hash-table test equal data (:pending t)))
         (urlport (ein:$notebooklist-url-or-port ein:%notebooklist%)))
     (ein:content-query-sessions sessions urlport)
     (loop repeat 4
           until (null (gethash :pending sessions))
           do (sleep-for 0 50))
     (loop for note in (ein:$notebooklist-data ein:%notebooklist%)
           for path = (plist-get note :path)
           for session = (car (gethash path sessions))
           if (not (null session)) 
             do (ein:kernel-kill (make-ein:$kernel 
                                  :url-or-port urlport
                                  :session-id session))
                (ein:notebooklist-delete-notebook path)
           end))))

(Teardown
 (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
   (ein:jupyter-server-stop t "log/ecukes.server"))
 (assert (not (processp %ein:jupyter-server-session%)) t "notebook server orphaned"))

(Fail
 (if (not noninteractive)
     (keyboard-quit))) ;; useful to prevent emacs from quitting
