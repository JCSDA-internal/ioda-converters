---
name: "Sprint: Upgrade Python Converters to Use the IODA-v2 API"
about: Use this template to create a new issue for the sprint
title: "Upgrade Python converter for (CONVERTER NAME) to IODA-v2:"
labels: 'Sprint'
assignees: ''

---

Update the converter identified in the title (hereafter referred to as the **target converter**) to use the IODA-v2 API.

- Update import statement
- Variable dimensions dictionary
- Dimensions dictionary
- Global attributes dictionary
- Remove record numbers
- Update keyDict
- Units, Fill values, other metadata
- Change order of when writer is called
- Remove ExtractoObsData if possible
- Update CMakeLists.txt with environment variable
- Run test to make sure it runs (but will fail because of unupdated test reference)
- Update test reference file
- run unit tests making use of the target converter and verify that they pass. Make sure you verify the results by examining the file using ```ncdump```, ```h5dump```, ```HDFview```, or a similar utility.


## ZenHub organization

Please make sure that you have properly set:
- Labels: OBS3
- Milestone: OBS - October 2021
- Estimate: An estimate of how long this issue will take to resolve. 1 = half a day. 2 = full day.
- Epics: Modify converters to IODAv2 format
- Assignees: Anyone **working** on this issue. Anyone who is not working directly on the issue, but
  who should receive notifications, should be listed below.
- Reviewers: 

## Notifications

Fill in a list of GitHub handles here. People here are not *assigned*, but instead receive notifications
about this issue's progress.

Notifications should always go to: @rhoneyager.

Other people:

