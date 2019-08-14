# University of Warwick API client

Haskell bindings for various University of Warwick APIs. Note that this library does not yet implement client functions for all supported endpoints. Please open an issue if there is an endpoint you require to be supported. Pull requests are also welcome.

## Tabula API

The Tabula API client currently supports the following endpoints:

* [List all assignments](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/coursework/assignments/list-assignments), including the ability to specify the academic year for which to list assignments:

```haskell
-- retrieve all assignments for cs141
withAPI Live config $ 
   listAssignments "cs141" Nothing
```

```haskell
-- retrieve all assignments for cs141 in 18/19
withAPI Live config $ 
   listAssignments "cs141" (Just "18/19")
```

* [Get a person's assignment information](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/coursework/assignments/get-member-assignments)

```haskell
-- retrieve all assignments for "1234567"
withAPI Live config $ 
   personAssignments "1234567"
```

* [List all submissions](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/coursework/submissions/list-submissions). The assignment UUID can either be obtained via `listAssignments` or directly from Tabula.

```haskell
-- retrieve all submissions for an assignment
withAPI Live config $ 
   listSubmissions "cs141" "00000000-0000-0000-0000-000000000000"
```

```haskell
-- retrieve all submissions for every assignment for cs141 in 18/19
withAPI Live config $ do
   result <- listAssignments "cs141" (Just "18/19")

   case result of 
    Left err -> print err
    Right TabulaOK{..} -> 
        forM_ tabulaData $ \a ->
            listSubmissions "cs141" assignmentID
```

* [Download a submission](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/coursework/submissions/download-submission-file)

```haskell
-- download a submission
withAPI Live config $ downloadSubmission 
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

* [Retrieve a member](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/member/retrieve-member), including support for specifying which fields should be retrieved:

```haskell
-- retrieve all fields
withAPI Live config $ 
   retrieveMember "1234567" []
```

```haskell
-- only retrieve "member.fullname"
withAPI Live config $ 
   retrieveMember "1234567" ["member.fullname"]
```

* [Retrieve a member's relationships](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/member/retrieve-member-relationships)

```haskell
-- list all relationships that "1234567" has
withAPI Live config $ 
   listRelationships "1234567"
```

* [Retrieve term dates](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/timetabling/termdates), including the ability to specify the academic year:

```haskell
-- Retrieve term dates for the current academic year
withAPI Live config $ retrieveTermDates
```

```haskell
-- Retrieve term dates for the academic year starting in 2020 (20/21)
withAPI Live config $ retrieveTermDatesFor "2020"
```

* [Retrieve term weeks](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/timetabling/termweeks), including the ability to specify the numbering system and the academic year:

```haskell
-- Retrieve term weeks for the current academic year
withAPI Live config $
    retrieveTermWeeks Nothing
```

```haskell
-- Retrieve term weeks for the current academic year, named using termly numbering
withAPI Live config $
    retrieveTermWeeks (Just TermNumbering)
```

```haskell
-- Retrieve term weeks for the 20/21 academic year
withAPI Live config $
    retrieveTermWeeksFor "2020" Nothing
```

```haskell
-- Retrieve term weeks for the 20/21 academic year, named using termly numbering
withAPI Live config $
    retrieveTermWeeksFor "2020" (Just TermNumbering)
```

* [Retrieve holiday dates](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/timetabling/holidaydates)

```haskell
-- Retrieve all holiday dates Tabula knows of
withAPI Live config $ retrieveHolidays
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

## MyWarwick API

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