(require 'ert)
(require 'my-package)

(ert-deftest sample-test ()
  (ert-info ("test function my-package-add-numers")
    (should (eq 3 (my-package-add-numers 1 2))

