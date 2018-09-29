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
