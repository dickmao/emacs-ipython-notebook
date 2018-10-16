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
Scenario: No token server
  Given I start the server configured "c.NotebookApp.token = u''\n"
  And I open notebooklist
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see "[error]"

@login
Scenario: With token server
  Given I start the server configured "\n"
  And I login if necessary
  And I open notebooklist
  And I switch to log expr "ein:log-all-buffer-name"
  Then I should not see "[warn]"
  And I should not see "[error]"

@login
Scenario: Logging into nowhere
  Given I login to 0
  Then I should see message "ein: [info] Failed to login to http://127.0.0.1:0"
