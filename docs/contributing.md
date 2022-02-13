# Contributing

Any help is welcome!

If you encounter a problem using Notoy, a task is not as easy as you'd like it to be or you'd like something added to it: open an issue at GitHub, see section [Report Issues](#report-issues-bugs-and-feature-requests).

- [Contributing](#contributing)
  - [Report Issues (Bugs and Feature Requests)](#report-issues-bugs-and-feature-requests)
  - [Forking the Repository](#forking-the-repository)
    - [Github Documentation on Collaborating with Issues and Pull Requests](#github-documentation-on-collaborating-with-issues-and-pull-requests)
  - [Developing Notoy](#developing-notoy)
    - [Changing and Generating Documentation](#changing-and-generating-documentation)
      - [Installing Dependencies](#installing-dependencies)
      - [MkDocs Files](#mkdocs-files)
      - [Read the Docs Configuration](#read-the-docs-configuration)
      - [GitHub Documentation](#github-documentation)
    - [Source Code](#source-code)
      - [NPM and Gulp](#npm-and-gulp)
      - [Pipenv and MkDocs](#pipenv-and-mkdocs)
  - [GitHub Workflows](#github-workflows)
  - [GitHub Issue Templates](#github-issue-templates)
  - [What is What? - List of all Files](#what-is-what---list-of-all-files)
    - [GitHub Workflows & Issue Templates](#github-workflows--issue-templates)
    - [MkDocs documentation](#mkdocs-documentation)
    - [Purescript sources, HTML and CSS](#purescript-sources-html-and-css)

## Report Issues (Bugs and Feature Requests)

Please help making Notoy better by filing bug reports and feature requests.

- File a bug report at [GitHub bug report](https://github.com/Release-Candidate/Notoy-PWA/issues/new?assignees=&labels=&template=bug_report.md&title=).
- Add a feature request at [GitHub feature request](https://github.com/Release-Candidate/Notoy-PWA/issues/new?assignees=&labels=&template=feature_request.md&title=).
- Take a look at the [Issue Tracker at GitHub](https://github.com/Release-Candidate/Notoy-PWA/issues)

## Forking the Repository

If you'd like to contribute directly, e.g. better the documentation, add another language or write some source code: fork Notoy by clicking the `Fork` button in the upper right corner of the GitHub project website. Check out your fork of Notoy using the URL from the `Code` button of your fork on Github. The URL should be something like github.com/YOUR_USERNAME/Notoy.git.

Details about how to fork a repository on Github are [here](https://docs.github.com/en/github/getting-started-with-github/fork-a-repo).

Make your changes, push them to your forked repository and make a pull-request (e.g. using the Pull request-button above and right of GitHubs source file view).

See [GitHub on Pull-Requests](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests/proposing-changes-to-your-work-with-pull-requests) and another [How-To](https://github.com/MarcDiethelm/contributing/blob/master/README.md).

### Github Documentation on Collaborating with Issues and Pull Requests

See GitHub's documentation about how to contribute for details: [Collaborating with issues and pull requests](https://docs.github.com/en/github/collaborating-with-issues-and-pull-requests).

## Developing Notoy

### Changing and Generating Documentation

#### Installing Dependencies

To generate the documentation using MkDocs, a virtual Python environment is needed. First you need to install Python, if you don't have it installed already - either from your distributions repository, using the XCode or [Homebrew](https://brew.sh/) version, or getting it from [Python.org](https://www.python.org/downloads/).

See

- [Using Python on Windows](https://docs.python.org/3/using/windows.html)
- [Using Python on a Macintosh](https://docs.python.org/3/using/mac.html)
- [Using Python on Unix Platforms](https://docs.python.org/3/using/unix.html)

Install `pipenv` using the package manager pip

```shell
pip install pipenv
```

Now you're ready to download and install the needed packages using pipenv

```shell
pipenv install --dev
```

After that you can use MkDocs.

Call

```shell
pipenv run mkdocs serve
```

in the root directory of Notoy and connect to the running webserver at [http://127.0.0.1:8000](http://127.0.0.1:8000).
This preview shows changes in realtime, so any changes to the markdown files in `docs` you see as preview as soon as you save the file. The generated HTML files are saved in the directory `sites`.

#### MkDocs Files

- `mkdocs.yml` - the MkDocs configuration, specially the configuration of the navigation sidebar in `nav` which you may need to edit

```yml
  nav:
    - Home: index.md
    - Project Links:
        - "App": "https://release-candidate.github.io/Notoy-PWA/http/index.html"
        - "GitHub Project Page": "https://github.com/Release-Candidate/Notoy-PWA"
        - "Report a Bug or a Feature Request": "https://github.com/Release-Candidate/Notoy-PWA/issues/new/choose"
        - "Issue Tracker at GitHub": "https://github.com/Release-Candidate/Notoy-PWA/issues"
        - "Notoy Chrome Extension": "https://chrome.google.com/webstore/detail/notoy/ejmcdafpeijhmmmfebmdpcmgaaoaminc"
        - "Notoy Edge Add-On": "https://microsoftedge.microsoft.com/addons/detail/notoy/nnocnobndadkcpggkegckgcnehmnohbl"
        - "Notoy Firefox Add-on": "https://addons.mozilla.org/addon/roland-csaszar"
    - "Installation & Usage":
        - "Installation & Usage": usage.md
        - "License": license.md
    - "Contributing":
        - "Contributing": contributing.md
```

- `docs/` - the markdown files that are used to generate the
   HTML sites in the directory `sites/`

#### Read the Docs Configuration

- `.readthedocs.yaml` the configuration for Read the Docs
- `docs/requirements.txt` the packages needed by MkDocs
   when generating the documentation at Read the Docs.
   Locally needed packages are configured in `Pipfile`

Read the Docs automatically generates the MkDocs documentation after each `git push`.

#### GitHub Documentation

The Markdown documentation for GitHub are the files [README.md](https://github.com/Release-Candidate/Notoy-PWA/blob/main/README.md) and [CHANGELOG.md](https://github.com/Release-Candidate/Notoy-PWA/blob/main/CHANGELOG.md) in the project root directory.

### Source Code

Before you can use the configured Tools of this project, you have to download and install the needed tools.

#### NPM and Gulp

Notoy uses NPM and Gulp to build the packages.

To install NPM, download and install Node.js for your OS: [Node.js Website](https://nodejs.org/)

To install all needed tools, run (`npm install --dev` should not be needed, `npm install` should do the same).

```shell
npm install
npm install --dev
```

Now you have to install the purescript compiler, this is better installed 'globally', as administrator or root.

```shell
npm install -g purescript
```

Now you can use the following NPM scripts:

- `npm run list` - To see the list of all Gulp tasks
- `npm run clean` - To delete all generated files and compiled Purescript packages
- `npm run bundle` - To build the app, run `ESBuild` on the generated Javascript files and copy and process all files from the directory `assets` to `http`. Generates the CSS using TailwindCSS from the file `/src/input.css`. The progressive web app is configured to be run in the root `/` of the HTTPS server. If you want to change this, change the constant `navScopePWA` in `./gulpfile.js` from `/` to the path you need.
- `npm run bundleGitHub` - To bundle the app to be served from GitHub Pages, in the directory `/Notoy-PWA/http/` - see constant `navScopeGitHub` in `./gulpfile.js`
- `npm run serve` - Starts a HTTPS server with the certificate `../https_cert.pem` and the key `../https_cert-key.pem`. You have to provide your own certificate and key. To change the path and name, change the constants `httpsCertificate` and `httpsCertificateKey` in `./gulpfile.js`
- `npm run watch` - Bundles the app, starts the HTTPS server and rebuilds and rebundles everything is a file has been changed.
- `npm run doc` - Generates the local Purescript documentation, using `spago doc`
- `npm run build` - To build the Purescript app, only compiles the app to the file `app.js`
- `npm run test` - Runs all tests from the directory `test`.
- `npm run repl` - Starts a Purescript REPL
- `npm run css` - Runs TailwindCSS on the app.

#### Pipenv and MkDocs

To generate the documentation using MkDocs (see [Changing and Generating Documentation](#changing-and-generating-documentation)), a virtual Python environment is needed. So, first you need to install Python, if you don't have it installed already - either from your distributions repository, using the XCode or [Homebrew](https://brew.sh/) version, or getting it from [Python.org](https://www.python.org/downloads/).

Install `pipenv` using the package
manager pip

```shell
pip install pipenv
```

Now you're ready to download and install the needed packages using pipenv

```shell
pipenv install --dev
```

After that you should be able to use the executable `mkdocs` in the local virtual Python environment in your project root using `pipenv run`:

```shell
pipenv run mkdocs --version
```

## GitHub Workflows

All tests and builds are executed on Linux.

These are the GitHub workflows defined in the directory `.github/workflows`

- `create_packages.yml` zips the files of the bundled app and
  generates a new GitHUb release with this archive appended. Runs automatically after tagging
  the source with a release tag of the form `v?.?.?`. Appends the newest entry in [CHANGELOG.md](https://github.com/Release-Candidate/Notoy-BrowserExtensions/blob/main/CHANGELOG.md) to the release - script [`scripts/get_changelog.sh`][(https://github.com/Release-Candidate/Notoy/blob/main/scripts/get_changelog.sh](https://github.com/Release-Candidate/Notoy-BrowserExtensions/blob/main/scripts/get_changelog.sh))
  See the [releases on GitHub](https://github.com/Release-Candidate/Notoy-PWA/releases) as an example
- `build.yml` - Runs the build and bundle gulp targets
- `test.yml` - Runs the tests

## GitHub Issue Templates

Issue templates for GitHub in `.github/ISSUE_TEMPLATE/`

- `bug_report.md` Bug report template
- `feature_request.md` Feature request template

## What is What? - List of all Files

A list of all files in this repository and what they do or configure.

- `./README.md` - The main documentation file.
- `./LICENSE` - The project'S license, GPLv3.
- `./CHANGELOG.md` - The project's changelog.
- `./.prettierrc.json` - Configuration file for Prettier, a source code formatter.
- `./package.json`, `package-lock.json` - List of NPM packages (and Gulp plugins) needed by Notoy
- `./gulpfile.js` - Gulp configuration script
- `./eslintrc.json` - ESLint configuration
- `../tailwind.config.js` - TailwindCSS configuration
- `./notoy-pwa.code-workspace` - The Visual Studio Code workspace file.
- `./.vscode/` - Directory containing additional Visual Studio Code configuration.

### GitHub Workflows & Issue Templates

Directory `./github/ISSUE_TEMPLATE/`:

- `./.github/ISSUE_TEMPLATE/bug_report.md` - Bug report template for GitHub
- `./.github/ISSUE_TEMPLATE/feature_request.md` - Feature request template for GitHub

Directory `./.github/workflows/`:

- `create_packages.yml` zips the files of the bundled app and
  generates a new GitHUb release with this archive appended. Runs automatically after tagging
  the source with a release tag of the form `v?.?.?`. Appends the newest entry in [CHANGELOG.md](https://github.com/Release-Candidate/Notoy-BrowserExtensions/blob/main/CHANGELOG.md) to the release - script [`scripts/get_changelog.sh`][(https://github.com/Release-Candidate/Notoy/blob/main/scripts/get_changelog.sh](https://github.com/Release-Candidate/Notoy-BrowserExtensions/blob/main/scripts/get_changelog.sh))
  See the [releases on GitHub](https://github.com/Release-Candidate/Notoy-PWA/releases) as an example
- `build.yml` - Runs the build and bundle gulp targets
- `test.yml` - Runs the tests

### MkDocs documentation

- `./Pipfile` - Packages nedded by MkDocs to install using `pipenv` and the package `mkdocs` itself.
- `./mkdocs.yml` - The configuration file for MkDocs, contains the website's index:

```YML
nav:
  - Home: index.md
  - Project Links:
      - "App": "https://release-candidate.github.io/Notoy-PWA/http/index.html"
      - "GitHub Project Page": "https://github.com/Release-Candidate/Notoy-PWA"
      - "Report a Bug or a Feature Request": "https://github.com/Release-Candidate/Notoy-PWA/issues/new/choose"
      - "Issue Tracker at GitHub": "https://github.com/Release-Candidate/Notoy-PWA/issues"
      - "Notoy Chrome Extension": "https://chrome.google.com/webstore/detail/notoy/ejmcdafpeijhmmmfebmdpcmgaaoaminc"
      - "Notoy Edge Add-On": "https://microsoftedge.microsoft.com/addons/detail/notoy/nnocnobndadkcpggkegckgcnehmnohbl"
      - "Notoy Firefox Add-on": "https://addons.mozilla.org/addon/roland-csaszar"
  - "Installation & Usage":
      - "Installation & Usage": usage.md
      - "License": license.md
  - "Contributing":
      - "Contributing": contributing.md
```

- `https://readthedocs.org/`, to host the generated documentation.

Directory `./docs`:

- `docs/requirements.txt` - Packages (plugins for MkDocs) that have to be installed by Read the Docs to generate the documentation.
- `./docs/index.md` - The documentation's home page.
- `./docs/usage.md` - Usage information.
- `./docs/contributing.md` - Information on how to contribute to the project.
- `./docs/license.md` - The license of the Notoy browser extensions, GPLv3

### Purescript sources, HTML and CSS

- `./src` contains all Purescript sources and the Javascript files for the FFI
- `./src/input.css` is CSS file for TailwindCSS to process
- `./test` contains the tests
- `./assets` holds all needed assets of the website, the icons, images, HTML, ...
- `./assets/manifest.json` is the progressive web apps manifest. CAUTION: the values of `start_url`, `id`, `scope` and `action` get set by the gulp targets `bundle` and `bundleGitHub`, do not edit these in the manifest!
