# University of Warwick API client

Haskell bindings for various University of Warwick APIs. Note that this library does not yet implement client functions for all supported endpoints. Please open an issue if there is an endpoint you require to be supported. Pull requests are also welcome.

## Tabula API

The Tabula API client currently supports the following endpoints:

* [Retrieve module](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/admin/retrieve-module)

```haskell
withTabula Live config $ retrieveModule "cs141"
```

* [List departments](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/admin/list-departments)

```haskell
withTabula Live config $ listDepartments
```

* [Retrieve department](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/admin/retrieve-department)

```haskell
withTabula Live config $ retrieveDepartment "cs"
```

* [List modules by department](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/admin/list-modules)

```haskell
withTabula Live config $ listDepartmentModules "cs"
```

* [List all assignments](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/coursework/assignments/list-assignments), including the ability to specify the academic year for which to list assignments:

```haskell
-- retrieve all assignments for cs141
withTabula Live config $ 
   listAssignments "cs141" Nothing
```

```haskell
-- retrieve all assignments for cs141 in 18/19
withTabula Live config $ 
   listAssignments "cs141" (Just "18/19")
```

* [Get a person's assignment information](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/coursework/assignments/get-member-assignments). You can optionally specify the academic year for which to retrieve assignments.

```haskell
-- retrieve all assignments for "1234567"
withTabula Live config $ 
   personAssignments "1234567" Nothing
```

```haskell
-- retrieve all assignments for "1234567" in the 18/19 academic year
withTabula Live config $ 
   personAssignments "1234567" (Just "18/19")
```

* [List all submissions](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/coursework/submissions/list-submissions). The assignment UUID can either be obtained via `listAssignments` or directly from Tabula.

```haskell
-- retrieve all submissions for an assignment
withTabula Live config $ 
   listSubmissions "cs141" "00000000-0000-0000-0000-000000000000"
```

```haskell
-- retrieve all submissions for every assignment for cs141 in 18/19
withTabula Live config $ do
   result <- listAssignments "cs141" (Just "18/19")

   case result of 
    Left err -> print err
    Right TabulaOK{..} -> 
        forM_ tabulaData $ \a ->
            listSubmissions "cs141" assignmentID
```

* [Upload marks](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/coursework/assignments/upload-marks)

```haskell
marks :: Marks 
marks = Marks [FeedbackItem { fiId="1234567", fiMark=Just "73", fiGrade=Nothing, fiFeedback=Just "Good work" }]

withTabula Live config $
   postMarks "cs141" "00000000-0000-0000-0000-000000000000" marks
```

* [Download a submission](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/coursework/submissions/download-submission-file)

```haskell
-- download a submission
withTabula Live config $ downloadSubmission 
    -- the University ID of the student
    "1234567"
    -- the module code
    "cs141" 
    -- the assignment UUID
    "00000000-0000-0000-0000-000000000000"
    -- the submission UUID
    "00000000-0000-0000-0000-000000000000"
    -- the attachment filename
    "attachment-name.ext"
    -- the path to which the file should be saved
    "local-filename"
```

An alternative version of `downloadSubmission` with callbacks to monitor download progress is available as `downloadSubmissionWithCallbacks`. This function additionally takes a parameter of type `TabulaDownloadCallbacks`. See the module documentation for `Warwick.DownloadSubmission` to see which callbacks are available.

* [List all small groups for a module](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/groups/smallgroupsets/list-smallgroupsets), including the ability to specify the academic year.

```haskell
-- retrieve all small group sets for CS141 for the current academic year
withTabula Live config $ listSmallGroupSets "cs141" Nothing
```

```haskell
-- retrieve all small group sets for CS141 for the 19/20 academic year
withTabula Live config $ listSmallGroupSets "cs141" (Just "19/20")
```

* [Retrieve small group set allocations](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/groups/smallgroupsets/retrieve-allocations)

```haskell
-- retrieve all allocations for the small group set whose ID is "00000000-0000-0000-0000-000000000000"
withTabula Live config $ retrieveSmallGroupAllocations "cs141" "00000000-0000-0000-0000-000000000000"
```

* [Retrieve small group attendance](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/groups/smallgroups/attendance-for-group)

```haskell
-- retrieve attendance information for all events belonging to the small group identified by "00000000-0000-0000-0000-000000000000"
withTabula Live config $ retrieveSmallGroupAttendance "00000000-0000-0000-0000-000000000000"
```

* [Retrieve a member](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/member/retrieve-member), including support for specifying which fields should be retrieved:

```haskell
-- retrieve all fields
withTabula Live config $ 
   retrieveMember "1234567" []
```

```haskell
-- only retrieve "member.fullname"
withTabula Live config $ 
   retrieveMember "1234567" ["member.fullname"]
```

* [Retrieve multiple members](#), including support for specifying which fields should be retrieved:

```haskell
-- retrieve all fields
withTabula Live config $ 
   retrieveMembers ["1234567", "7654321"] []
```

* [Retrieve a member's relationships](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/member/retrieve-member-relationships)

```haskell
-- list all relationships that "1234567" has
withTabula Live config $ 
   listRelationships "1234567"
```

* [Search for members](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/member/search-for-members), see the documentation for `Warwick.Tabula.MemberSearchFilter` to see which filter options are available. Use `defaultMemberSearch` as a starting value for which all filters are initially set to `[]`.

```haskell
settings :: MemberSearchFilter
settings = defaultMemberSearch {
   filterDepartment = ["cs"]
}

withTabula Live config $ listMembers settings 0 10
```

* [Retrieve a student's attendance](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/monitoring-points/member-attendance)

```haskell
-- list the attendance for "1234567" in the academic year starting in 2018
withTabula Live config $ retrieveAttendance "1234567" "2018"
```

* [Retrieve term dates](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/timetabling/termdates), including the ability to specify the academic year:

```haskell
-- Retrieve term dates for the current academic year
withTabula Live config $ retrieveTermDates
```

```haskell
-- Retrieve term dates for the academic year starting in 2020 (20/21)
withTabula Live config $ retrieveTermDatesFor "2020"
```

* [Retrieve term weeks](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/timetabling/termweeks), including the ability to specify the numbering system and the academic year:

```haskell
-- Retrieve term weeks for the current academic year
withTabula Live config $
    retrieveTermWeeks Nothing
```

```haskell
-- Retrieve term weeks for the current academic year, named using termly numbering
withTabula Live config $
    retrieveTermWeeks (Just TermNumbering)
```

```haskell
-- Retrieve term weeks for the 20/21 academic year
withTabula Live config $
    retrieveTermWeeksFor "2020" Nothing
```

```haskell
-- Retrieve term weeks for the 20/21 academic year, named using termly numbering
withTabula Live config $
    retrieveTermWeeksFor "2020" (Just TermNumbering)
```

* [Retrieve holiday dates](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/timetabling/holidaydates)

```haskell
-- Retrieve all holiday dates Tabula knows of
withTabula Live config $ retrieveHolidays
```

### Partially implemented endpoints

The following API endpoints are supported internally, but are not currently exposed by `Warwick.Tabula`:

* [Retrieve an attachment's information](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/fileattachments/retrieve-attachment)

* [Retrieve a job's status](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/jobs/retrieve-job)

## Peoplesearch API

* *Search profiles* 

```haskell
-- Search profiles for the string "michael"
withAPI Live config $ searchProfiles "michael"
```

* *Lookup users* 

```haskell
-- Retrieve profiles for "1234567"
withAPI Live config $ lookupProfile "1234567"
```

## My Warwick API

The endpoints for posting items to the alert and activity streams are supported. Example for `postAlert`:

```haskell
recipients :: StreamRecipients
recipients = StreamRecipients {
   srUsers = Just ["u1234567"],
   srGroups = Nothing
}

item :: StreamItem
item = StreamItem {
   siType = "test-alert",
   siTitle = "Title",
   siText = "Test body",
   siURL = Just "http://example.com",
   siRecipients = recipients
}

withAPI Live config $ postAlert "example-provider" item
```

To add the item to the activity stream instead of the alert stream, simply replace `postAlert` with `postActivity` in the example above.

## SSO API 

The endpoint for searching users is implemented:

```haskell
withSSO Live "[API KEY]" $ userSearch defaultSearch{ ssoSearchID = Just "1234567" }
```

## Sitebuilder API

An existing Sitebuilder page can be edited ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/pages-and-files/edit-content)). You can either specify the new contents of the file by hand (first example) or load the new contents from a file (second example):

```haskell
update :: PageUpdate
update = PageUpdate {
    puContents = "<html><body>Test</body></html>",
    puOptions = defaultPageOpts { poEditComment = Just "change notes" }
}

-- replace the contents of the page at /fac/sci/dcs/test with data from `update`
withAPI Live config $ editPage "/fac/sci/dcs/test" update

-- replace the contents of the page at /fac/sci/dcs/test with the contents of test.html
withAPI Live config $ editPageFromFile "/fac/sci/dcs/test" "change notes" "./test.html"
```

Available page options ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/pages-and-files/change-page-properties/)):
- `poSearchable`: whether the page is visible in search engines
- `poVisible`: whether the page is visible in the navigation list of its parent page
- `poSpanRHS`: whether the page should span its right hand side
- `poDeleted`: whether the page is marked as deleted
- `poDescription`: the page's description, sometimes shown in Google results and on other Sitebuilder pages
- `poKeywords`: keywords for a page, used for search and categorisation
- `poLinkCaption`: the link caption for a page (as used in local navigation)
- `poPageHeading`: the page heading
- `poTitleBarCaption`: the title bar caption
- `poPageOrder`: the page (sort) order for a page, with lower numbers appearing first in local navigation
- `poCommentable`: whether the page allows comments
- `poCommentsVisibleToCommentersOnly`: whether comments for a page should only be visible to users who can post comments
- `poLayout`: the layout of an ID6 page
- `poEditComment`: the comment to show in the edit history for this edit

Files can be uploaded ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/pages-and-files/upload-file)):

```haskell
-- uploads the file README.md to the directory at /fac/sci/dcs/test using the name README
withAPI Live cfg $ uploadFile "/fac/sci/dcs/test" "README" "./README.md"
```

Pages ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/pages-and-files/purge-page)) and files ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/pages-and-files/purge-file)) can be purged:

```haskell
-- purges the page at /fac/sci/dcs/test 
withAPI Live cfg $ purge "/fac/sci/dcs/test"
```

Files can be uploaded ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/pages-and-files/upload-file)):

```haskell
-- uploads "/Users/example/test.pdf" to "/fac/sci/dcs/test" as "test.pdf"
withAPI Live cfg $ uploadFile "/fac/sci/dcs/test" "test.pdf" "/Users/example/test.pdf"
```