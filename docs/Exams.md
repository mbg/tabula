# exams.warwick.ac.uk

## `getExamProfiles`

The `getExamProfiles` client retrieves a list of exam profiles. This is a public API which requires no authentication and can be called using `withPublicAPI`:

```haskell
withPublicAPI (Live @Exams) getExamProfiles
```

A successful result will be a standard response containing a list of `ExamProfile` values as its payload. For example:

```haskell
Right (MkStandardResponse {responsePayload = [...,MkExamProfile {examProfileCode = "EXJUN21", examProfileName = "EXJUN21 - May/June Examinations 2021 (weeks 4-9 Summer Term)", examProfileAcademicYear = "20/21", examProfileStartDate = Date {getDate = 2021-05-17 00:00:00 UTC}, examProfileEndDate = Date {getDate = 2021-06-26 00:00:00 UTC}, examProfilePublished = False, examProfileSeatNumbersPublished = False},MkExamProfile {examProfileCode = "EXJUN21_DA", examProfileName = "EXJUN21_DA - Degree Apprenticeship Examinations  2021 ( Week 10", examProfileAcademicYear = "20/21", examProfileStartDate = Date {getDate = 2021-06-30 00:00:00 UTC}, examProfileEndDate = Date {getDate = 2021-07-02 00:00:00 UTC}, examProfilePublished = False, examProfileSeatNumbersPublished = False}]})
```
