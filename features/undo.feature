Scenario: Collapse doesn't break undo
  Given new python2 notebook
  When I type "from time import sleep"
  And I press "RET"
  And I press "C-c C-b"
  And I type "1 + 1"
  And I press "RET"
  And I press "C-<up>"
  And I press "C-n"
  And I type "print "abba\nabba""
  And I press "RET"
  And I type "1.618"
  And I execute cell
  And I press "C-<down>"
  And I press "C-n"
  And I type "undo meee"
  And I press "C-<up>"
  And I press "C-c C-e"
  And I press "C-/"
  Then the cursor should be at point "75"
  And I undo again
  Then the cursor should be at point "54"

Scenario: Clear output doesn't break undo
  Given new python2 notebook
  When I type "from time import sleep"
  And I press "RET"
  And I press "C-c C-b"
  And I type "1 + 1"
  And I press "RET"
  And I press "C-<up>"
  And I press "C-n"
  And I type "print "abba\nabba""
  And I press "RET"
  And I type "1.618"
  And I execute cell
  And I press "C-<down>"
  And I press "C-n"
  And I type "undo meee"
  And I press "C-<up>"
  And I press "C-c C-l"
  And I press "C-/"
  Then the cursor should be at point "73"
  And I undo again
  Then the cursor should be at point "54"

Scenario: Moving cells doesn't break undo
  Given new python2 notebook
  When I type "100"
  And I press "C-c C-b"
  And I type "200"
  And I press "C-c C-b"
  And I type "print "hello""
  And I execute cell
  And I press "C-<up>"
  And I execute cell
  And I press "C-<down>"
  And I press "C-c <up>"
  And I press "C-/"
  Then the cursor should be at point "53"
  And I press "C-<up>"
  And I press "C-<up>"
  And I execute cell
  And I press "C-c <down>"
  And I press "C-/"
  Then the cursor should be at point "66"

Scenario: Split and merge don't break undo
  Given new python2 notebook
  When I type "print "hello""
  And I press "C-c C-b"
  And I type "abba"
  And I press "RET"
  And I press "RET"
  And I press "RET"
  And I type "abab"
  And I press "RET"
  And I type "baba"
  And I press "C-c C-b"
  And I type "bbaa"
  And I press "C-<up>"
  And I press "C-n"
  And I press "C-c C-s"
  And I execute cell
  And I press "C-<up>"
  And I execute cell
  And I press "C-<up>"
  And I execute cell
  And I press "C-/"
  And I press "C-<up>"
  And I type "aabb"
  And I press "RET"
  And I type "aabb"
  And I execute cell
  And I press "C-/"
  And I undo again
  And I undo again
  And I undo again
  And I undo again
  Then the cursor should be at point "218"
  And I press "C-c C-m"
  And I press "C-c C-m"
  And I press "C-/"
  And I undo again
  And I undo again
  And I undo again
  Then the cursor should be at point "199"
