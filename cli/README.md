# git-overleaf-cli MVP

This directory contains an experimental native C command-line client for
git-overleaf.  It is intentionally separate from the Emacs package files and is
not meant to be included in the MELPA package recipe.

## Scope

Implemented in this MVP:

- store a raw Overleaf Cookie header in a local file;
- list accessible Overleaf projects;
- clone a project snapshot into a new Git repository;
- bind an existing Git repository to an Overleaf project;
- pull the latest Overleaf snapshot into an existing Git repository.

Not implemented yet:

- webdriver/browser authentication;
- Firefox cookie import;
- push/overwrite;
- Overleaf WebSocket project tree fetch;
- ShareJS/OT text updates that preserve remote document ids.

`pull` uses only the snapshot download path, so it does not need WebSocket/OT.
When a pull produces merge conflicts, it writes the same pending-pull Git config
keys used by the Emacs package.  Resolve the merge, commit it, then finish with
the Emacs `git-overleaf-push` command until native CLI push lands.

## Build

Dependencies:

- C11 compiler;
- `pkg-config`;
- `libcurl`;
- `jansson`;
- `git`;
- `unzip`.

Build:

```sh
make -C cli
```

The executable is written to:

```sh
cli/build/git-overleaf-cli
```

## Usage

Save cookies manually:

```sh
cli/build/git-overleaf-cli auth \
  --cookie 'connect.sid=...; overleaf_session=...' \
  --cookie-file ~/.git-overleaf-cookies
```

List projects:

```sh
cli/build/git-overleaf-cli list
```

Clone by project id:

```sh
cli/build/git-overleaf-cli clone \
  --project-id PROJECT_ID \
  --project-name 'Project Name' \
  ./project-name
```

Bind an existing Git repository without changing its working tree:

```sh
cli/build/git-overleaf-cli init \
  --project-id PROJECT_ID \
  --project-name 'Project Name' \
  --repo /path/to/repo
```

Pull remote Overleaf changes:

```sh
cli/build/git-overleaf-cli pull --repo /path/to/repo
```

Use a self-hosted Overleaf URL:

```sh
cli/build/git-overleaf-cli --url https://latex.example.edu list
```

## Compatibility

The CLI writes the same repository metadata keys as the Emacs package:

- `git-overleaf.projectId`
- `git-overleaf.projectName`
- `git-overleaf.url`
- `git-overleaf.baseRef`
- `git-overleaf.pendingAction`
- `git-overleaf.pendingRemoteCommit`

It also uses the same base ref:

```text
refs/git-overleaf/base
```

The reserved remote metadata file remains:

```text
.git-overleaf-sync.json
```

Downloaded snapshots remove that file before local Git comparisons, matching
the Emacs implementation.

## Security

The cookie file contains account-bearing Overleaf session cookies.  The `auth`
command writes it with mode `0600`, but users should still keep it outside Git
repositories and avoid pasting real cookies into logs or issue reports.
