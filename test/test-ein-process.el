(eval-when-compile (require 'cl))
(require 'ert)
(require 'ein-process)

(ert-deftest ein:process-check-suitable ()
  (should (not (equal (ein:process-suitable-notebook-dir (concat default-directory "features/support")) (concat default-directory "features/support")))))
