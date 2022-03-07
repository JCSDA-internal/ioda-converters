---
name: 'Sprint: Upgrade Converters to Use the JEDI Data Conventions'
about: Use this template to create a new issue for the sprint
title: Upgrade converter for (CONVERTER NAME) to follow the JEDI Data Conventions
labels: Sprint
assignees: ''

---

Update the converter identified in the title (hereafter referred to as the **target converter**) to follow the JEDI Data Conventions.

- Update variable names
- Ensure variable units match the units in the conventions table
- Update the global attributes dictionary as needed
- Run the converter test to make sure it runs (but will fail because of unupdated test reference)
- Run ```ioda-validate.x``` on your output file and verify that there are no errors or warnings
- Update test reference file
- Run unit tests making use of the target converter and verify that they pass
- Make sure you verify the results by examining the file using ```ncdump -t```, ```h5dump```, ```HDFview```, or a similar application.


## ZenHub organization

Please make sure that you have properly set:
- Labels: OBS3
- Estimate: An estimate of how long this issue will take to resolve. 1 = half a day. 2 = full day. 3 = two days, 5 = three days.
- Epics: [Sprint: Implement the JEDI Data Conventions in the Converters](https://github.com/JCSDA-internal/ioda-converters/issues/813)
- Assignees: Anyone **working** on this issue. Anyone who is not working directly on the issue, but
  who should receive notifications, should be listed below.
- Reviewers

## Notifications

Fill in a list of GitHub handles here. People here are not *assigned*, but instead receive notifications
about this issue's progress.

Notifications should always go to: @rhoneyager.

Other people:
