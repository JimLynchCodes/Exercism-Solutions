
(load-file "hello-world.el")

(ert-deftest hello-world-test ()
  (should (string= (hello) "Hello, World!")))

(provide 'hello-world-test)

