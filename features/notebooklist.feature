Scenario: No warnings
Given I switch to log expr "ein:log-all-buffer-name"
Then I should see "[info]"
And I should not see "[warn]"
And I should not see "[error]"

Scenario: Breadcrumbs
  Given I am in notebooklist buffer
  When I click on dir "step-definitions"
  Then I should see "ein-steps"
  And I click on "Home"
  Then I should see "support"

Scenario: New Notebook
  Given I am in notebooklist buffer
  When I clear log expr "ein:log-all-buffer-name"
  And I click on "New Notebook"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see "Opened notebook Untitled"

Scenario: Resync
  Given I am in notebooklist buffer
  When I clear log expr "ein:log-all-buffer-name"
  And I click on "Resync"
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see "kernelspecs--complete"

@foo
Scenario: Global notebooks
  Given I am in notebooklist buffer
  When I clear log expr "ein:log-all-buffer-name"
  And I call "ein:notebooklist-open-notebook-global"
  And I wait 0.9 seconds
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should see "Opened notebook"

@login
Scenario: notebooklist-open works interactively
  Given I start the server
  Given I am in buffer "*scratch*"
  When I clear log expr "ein:log-all-buffer-name"
  And I login if necessary
  And I open notebooklist
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see "[error]"

@login
Scenario: notebooklist-login works interactively
  Given I am in buffer "*scratch*"
  When I clear log expr "ein:log-all-buffer-name"
  And I login if necessary
  And I open notebooklist
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see "[error]"

@login
Scenario: Logging into nowhere
Given I login to 0
Then I should see message "demoted: (error Connection refused: [error] http://127.0.0.1:0)"

@login
Scenario: Opening into nowhere
Given I open to 0
Then I should see message "demoted: (error Connection refused: [error] http://127.0.0.1:0)"
