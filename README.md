# University of Warwick API client

A client program (`uow-util`) and Haskell library (`uow-apis`) for various University of Warwick APIs. Please [open an issue](https://github.com/mbg/uow-apis/issues/new) if there is an endpoint or a feature you require, but that is not yet supported. Pull requests are also welcome.

## Contents

1. [Usage](#usage)
1. [Tabula API](#tabula-api)
1. [Peoplesearch API](#peoplesearch-api)
1. [MyWarwick API](#my-warwick-api)
1. [SSO API](#sso-api)
1. [Sitebuilder API](#sitebuilder-api)
1. [Campus API](#campus-api)
1. [Postroom API](#postroom-api)
1. [AEP API](#aep-api)
1. [Exams API](docs/Exams.md)

## Usage

Pre-built binaries for linux (x86-64) can be downloaded from the [releases page](https://github.com/mbg/uow-apis/releases) and [Docker images are also available](https://github.com/mbg/uow-apis/packages/468684).

The `uow-util` tool currently only provides a small number of commands to interact with Tabula and SiteBuilder. For anything else, the `uow-apis` library can be used.

### Configuration

The `uow-util` tool requires user credentials to function. ITS recommend that you use an [external user account](https://warwick.ac.uk/services/its/servicessupport/web/sign-on/externalusers) for this. Note that any external user accounts you create will not have the same permissions you have! You will need to grant any required permissions on Tabula or SiteBuilder as required. 

Once you have suitable user credentials, there are two ways to configure `uow-util` with them. You can create a `uow-util.json` file with the following structure:

```json
{
    "username": "username_goes_here",
    "password": "password_goes_here"
}
```

By default, `uow-util` looks for `uow-util.json` in the current working directory. You can specify a different filename with the `--credentials /path/to/your/config.json` option.

The second option for providing user credentials to `uow-util` is to set environment variables named `UOW_USER` and `UOW_PASSWORD` with the username and password, respectively.

### Tabula commands

#### Download coursework submissions

You can use `uow-util` to download all submissions for a particular coursework. To do this, run `uow-util tabula download MODULE --year YEAR` where `MODULE` is the module code of the relevant module and `YEAR` is the academic year in `yy/yy` format. Your user account will need to have `Permissions.Module.ManageAssignments` on the module and `Permissions.Submission.Read` on the assignment. The program will list all assignments for the module and you can then choose which one to download submissions for.

You can also specify the following options:

- `--unpack` to automatically unpack archives (`.zip` and `.tar.gz`) that are downloaded

- `--only-pdf` to only download `.pdf` files

#### View information about tutees

`uow-util tabula tutees` will show a summary of assignments that have been submitted late or are current late for the personal tutees of a given member of staff. This command requires department-level permissions: 

- `Permissions.Profiles.StudentRelationship.Read`
- `Profiles.Read.Coursework`
- `Profiles.Submission.Read`
- `Profiles.AssignmentFeedback.Read`
- `Profiles.Extension.Read`

You will be prompted for the University ID of the member of staff to view this information for.

#### View information about enrolments 

`uow-util tabula enrolments DEPARTMENT --academic-year YEAR` will show an overview of enrolments for the department identified by `DEPARTMENT` in the academic year given by `YEAR`. For example: `uow-util tabula enrolments cs --academic-year 20/21`. Several options are available which restrict the search:

- `--year YEAR` filters by a specific year group. For example `--year 1` will only return the enrolment status for students in their first year.

- `--course-type TYPE` filters by a specific course type. For example `--course-type PGT` will only return the enrolment status for postgraduate taught students.

The above filters can be combined so that e.g. `uow-util tabula enrolments cs --academic-year 20/21 --year 1 --course-type UG` returns the enrolment status of all first-year undergraduates in Computer Science for 20/21.

### SiteBuilder commands

#### Edit a page

To edit an existing SiteBuilder page (i.e. update it with new HTML), you can run `uow-util sitebuilder edit PAGE FILE` where `PAGE` is the path of the SiteBuilder page to edit and `FILE` is the name of the file to load the new HTML from.

- `--comment 'Fix typo'` can optionally be provided to specify a comment for SiteBuilder's history view about the change

#### Upload a file

To upload a file, such as an image or a PDF, to SiteBuilder, you can use `uow-util sitebuilder upload PAGE FILE` where `PAGE` is the path of the SiteBuilder page under which the file should be uploaded to and `FILE` is the name of the file to upload. 

- `--name FILENAME` can be used to specify a custom name for the file, different from the name of the file that is being uploaded.

#### Syncing a site

To sync a site from a configuration file, you can run `uow-util sitebuilder sync --config FILE`, where `FILE` is the path to the configuration file. See [here](docs/Sitebuilder/Sync.md) for more information about the configuration

### Docker

To acquire the latest version of the Docker image, run `docker pull docker.pkg.github.com/mbg/uow-apis/uow-util:latest` (you may need to be authenticated to the GitHub registry). The image can then be run as usual with `docker run --rm docker.pkg.github.com/mbg/uow-apis/uow-util:latest` and options/commands can be specified as if you were running `uow-util` directly. Using this has the advantage that you do not need to compile the binaries yourself if you are on a platform for which we do not provide pre-built executables.

### Compiling

If you want to compile the library or program yourself, you will first need [Stack](https://docs.haskellstack.org/en/stable/README/). The library can then be built with `stack build` and the library+program can be built with `stack build --flag uow-apis:build-exe`. The program can then be invoked with `stack exec uow-util` or installed with `stack install`.

### Library

The library is deliberately not on Hackage/Stackage due to its niche use. You will need to download the sources and compile it locally. You can then point at the library in e.g. the `stack.yaml` for your project with:

```yaml
packages:
- .
- path/to/uow-apis
```

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
   listRelationships "1234567" Nothing
```

```haskell
-- list all relationships that "1234567" has as personal tutor
withTabula Live config $ 
   listRelationships "1234567" (Just "personalTutor")
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

* Retrieve relationship types

```haskell
-- list all relationship types that Tabula knows 
withTabula Live config listRelationshipTypes
```

* [List relationship agents in department](https://warwick.ac.uk/services/its/servicessupport/web/tabula/api/member/list-relationship-agents)

```haskell
-- list all members who are dissertation supervisors in Computer Science 
withTabula Live config $ listAgents "cs" "dissertation-supervisor"
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

- An existing Sitebuilder page can be edited ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/pages-and-files/edit-content)). You can either specify the new contents of the file by hand (first example) or load the new contents from a file (second example):

```haskell
update :: PageUpdate
update = PageUpdate {
    puContents = "<html><body>Test</body></html>",
    puRhsContent = Nothing,
    puOptions = defaultPageOpts { poEditComment = Just "change notes" }
}

-- replace the contents of the page at /fac/sci/dcs/test with data from `update`
withAPI Live config $ editPage "/fac/sci/dcs/test" update

-- replace the contents of the page at /fac/sci/dcs/test with the contents of test.html
withAPI Live config $ editPageFromFile "/fac/sci/dcs/test" "change notes" "./test.html"
```

- Pages can be created ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/pages-and-files/create-page/)). As above you can either specify the contents by hand or load them from a file
```haskell
opts :: Page
opts = Page{
    pcTitle = "Page Title",
    pcContents = "<html><body>Test</body></html>",
    pcRhsContents = Nothing,
    pcPageName = "testpage",
    pcOptions = defaultPageOpts 
}

-- create a new page at /fac/sci/dcs using data from `create`
withAPI Live config $ createPage "/fac/sci/dcs" opts

-- create a new page with title "Page Title" at /fac/sci/dcs with name "testpage" with the contents of test.html
withAPI Live config $ createPageFromFile "/fac/sci/dcs" "Page Title" "testpage" "./test.html"
```

- Files can be uploaded ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/pages-and-files/upload-file)):

```haskell
-- uploads the file README.md to the directory at /fac/sci/dcs/test using the name README
withAPI Live cfg $ uploadFile "/fac/sci/dcs/test" "README" "./README.md"
```

- Properties of existing files can be edited:
```haskell
opts :: FileOptions
opts = defaultFileOpts{
   foTitle = Just "New Caption",
   foVisible = Just False
}

-- changes /fac/sci/dcs/test.pdf to not be visible in parent navigation and to have link caption "New Caption"
withAPI Live cfg $ editFileProps "/fac/sci/dcs/test.pdf" opts
```

- Pages ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/pages-and-files/mark-page-deleted/)) can be marked as deleted:
```haskell
-- marks the page at /fac/sci/dcs/test as deleted
withAPI Live cfg $ deletePage "/fac/sci/dcs/test"
```

-  Files ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/pages-and-files/mark-file-deleted/)) can be marked as deleted:
```haskell
-- marks the file at /fac/sci/dcs/test.pdf as deleted
withAPI Live cfg $ deleteFile "/fac/sci/dcs/test.pdf"
```

- Pages ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/pages-and-files/mark-page-deleted/)) can be marked as not deleted:
```haskell
-- marks the page at /fac/sci/dcs/test as not deleted
withAPI Live cfg $ restorePage "/fac/sci/dcs/test"
```

- Files ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/pages-and-files/mark-file-deleted/)) can be marked as not deleted:
```haskell
-- marks the file at /fac/sci/dcs/test.pdf as deleted
withAPI Live cfg $ restoreFile "/fac/sci/dcs/test.pdf"
```

- Pages ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/pages-and-files/purge-page)) can be purged:

```haskell
-- purges the page at /fac/sci/dcs/test 
withAPI Live cfg $ purgePage "/fac/sci/dcs/test"
```

- Files ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/pages-and-files/purge-file)) can be purged
```haskell
-- purges the file at /fac/sci/dcs/test.pdf
withAPI Live cfg $ purgeFile "/fac/sci/dcs/test.pdf"
```

- Files can be uploaded ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/pages-and-files/upload-file)):

```haskell
-- uploads "/Users/example/test.pdf" to "/fac/sci/dcs/test" as "test.pdf"
withAPI Live cfg $ uploadFile "/fac/sci/dcs/test" "test.pdf" "/Users/example/test.pdf"
```

- Infomation about existing pages can be retrieved ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/getting-started)):
```haskell
-- retrieves information about page at /fac/sci/dcs
withAPI Live cfg $ pageInfo "/fac/sci/dcs"
```

- Information about existing files can be retreived ([API docs](https://warwick.ac.uk/services/its/servicessupport/web/sitebuilder2/faqs/api/getting-started)):
```haskell
-- retrieves information about page at /fac/sci/dcs/test.pdf
withAPI Live cfg $ fileInfo "/fac/sci/dcs/test.pdf"
```

For details about page/file options see [here](docs/Sitebuilder/Types.md)

## Campus API 

The endpoint for searching rooms is implemented as `listRooms` which accepts two parameters: one determining the maximum number of results that should be returned by the API and one which specifies the (partial) search query. For example, the following will retrieve up to ten results where the room names match `"CS2."`:

```haskell
withCampus Live "[API TOKEN]" $ listRooms 10 "CS2."
```

## Postroom API

- A list of postroom hubs can be retrieved:
```haskell
withAPI Live cfg getHubs
```

- The opening times of postroom hubs, and a list of which accommodations use with hubs can be retrieved:
```haskell
withAPI Live cfg getOpeningTimes
```

- The active addresses, if any, of the current user can be retrieved:
```haskell
withAPI Live cfg getMyAddresses
```

- The post items for the current user can be retrieved:
```haskell
withAPI Live cfg getMyItems
```

## AEP API

**Please note this endpoint is not an official API endpoint, and its use is not supported. Do not contact ITS about any issues regarding this endpoint. Note that using this endpoint directly lacks the enhanced logging provided by the AEP (such as logging the start time of uploads, and logging failed upload attempts) and all use of it is at your own risk. You should always be prepared to upload normally via the AEP interface.**

- A file can be uploaded to an assessment. The last parameter determines if existing files with the same name should be overwritten or not:
```haskell
withAEP Live sscCookie $ uploadFile assessmentUUID "path/to/file" False
```
