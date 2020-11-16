# Sitebuilder Sync Config

The configuration for the sync function consists of a yaml file containing a list of page configurations. If a page doesn't exist it is created, and if it does exist it is updated. Each page configuration consists of the following fields:
- `page`: The path to the page on SiteBuilder
- `content`: The path to the file containing the page contents with which the page should be created or updated
- `rhsContent`: (optional) The path to the file containing the page content with which the RHS of the page should be created or updated
- `properties`: (optional) A list of properties to set for the page (for a list of all options see [here](Types.md#pageoptions), the name of the property in the config here is the name of the option without the `po` prefix)
- `files`: (optional) A list of files to upload under the page. [File patterns](https://hackage.haskell.org/package/filepattern-0.1.2/docs/System-FilePattern.html#v:-63--61--61-) are accepted
- `children`: (optional) A list of child pages configurations. These configurations are the same as for top-level page configurations as described here, except the paths are relative to their parent

It is important to note if you are are configuring sub-pages at the top-level, rather than as children of a given page, then the sub-page configurations must come after the parent in the configuration file, otherwise creating the pages will fail.

### Example Config

```yaml
- page: /fac/sci/dcs/materials/cs141
  content: cs141-material.html
  properties:
    spanRHS: true
    linkCaption: CS141
    pageHeading: CS141 - Functional Programming
    titleBarCaption: CS141 - Functional Programming
  files:
  - files/*
  - img/image1.png
  children:
    - page: term3/
      content: term3.html
      rhsContent: term3-rhs.html
      properties:
        spanRHS: false
- page: /fac/sci/dcs/materials/cs264
  content: cs264-material.html
```

Would create/update 3 pages:
- `/fac/sci/dcs/materials/cs141` with the content from `cs141-material.html` and with the `spanRHS` property set and the captions set as specified, and upload the file `img/image1.png` and every file under `files/`
- `/fac/sci/dcs/materials/cs141` with the content from `term3.html` and the `spanRHS` property set to `false` where the RHS content comes from `term3-rhs.html`
- `/fac/sci/dcs/materials/cs264` with the content from `cs264-material.html`
