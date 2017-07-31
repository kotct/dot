# Contributing to Dot

First of all, thanks for taking the time to contribute to Dot!
New ideas from everyone are what make our project what it is.

Dot aims to be a welcoming and inclusive project, and we welcome the suggestions and contributions of all.
However, being an active part of the development process will help the core developers understand your changes.

We really like to see the following types of contributions:

- Issues containing feature suggestions, reports of any bugs/bad features.
- Pull Requests containing new features, fixes for edge cases or problems, new test examples, refactoring, and documentation.

We'd rather not see any poorly-worded issues or stuff that doesn't work.

<!-- TODO: Add Code of Conduct/link to CoC .md file here? -->

## Workflow

It is a good idea to open an Issue for any new features or support you want to see before doing any work to make sure you don't waste your time.

In general, here's a suggested workflow:

1. Fork `dot` to your user or organization account.
2. Clone your fork of this repository onto your machine.
3. Create a branch on your project for the *specific* change that you wish to make.
4. Work your magic, testing and committing everything that you change, being as verbose in your commits as needed to clearly communicate your changes.
5. Create a pull request once you have made your changes and committed them, with the `base` set to `kotct/dot`'s `master` branch.
   Describe your changes in detail, including **what** you changed, **why** you changed it.
   Feel free to request a review from @[rye](https://github.com/rye) and/or @[cg505](https://github.com/cg505).
6. Actively participate in your pull request.
   If the maintainers ask for a revision to your proposed changes, make it promptly to help get your PR merged as soon as possible.
   Inactive PR's or Issues may be marked as Deferred or closed if no response is given.

## Branching/Versioning Scheme

***Note: This guide is intended mainly for core contributors.***

Dot uses a development flow very similar to [GitLab Flow](https://about.gitlab.com/2014/09/29/gitlab-flow/) as a model for development.
For versioning, Dot complies with [SemVer v2](http://semver.org/spec/v2.0.0.html).
In our context, a "breaking change" is something that causes users who used previous functions and such to have to change their personal configuration in order to continue working.

Our branching workflow consists of two persistent branches:

- `master`, which contains all of the latest changes and should be in a usable state for testing.
  This is used as the base for all of the `release/*` branches (see below.)
- `stable`, which is a tracking branch that gets releases merged into it.
  This is the branch to be used by users who want only releases that have been deemed stable.

We use the following scheme for additional branches:

- `feature/` can be used as a prefix for feature branches, but this is not recommended.
  (Just use `kebab-case` for your branch names, we assume that anything without a special prefix is a normal feature PR.)
- `hotfix/` should be used as a prefix for hotfix branches.
  Hotfixes should _always_ target and be based off of `master`;
  they will be cherry-picked into a `release` branch (see below) if a new patch version is necessary.
- `release/vX.Y.Z` branches are used to control versioning.
  The contents of the `VERSION` file on `master` will _always_ be something like `X.Y.0-pre`, as we do not do releasing on `master`.
  There are two major situations in which `release/` branches get used, and here are the corresponding guidelines for the release process:
  - When releasing a new minor version from `master`,
    1. Create a `release/vX.Y.0` branch off of the latest version of `master`.
    2. Change the `VERSION` file to contain `X.Y.0`.
    3. If necessary, add any release notes here.
    4. Merge the branch into `stable` via PR.
    5. Create a release targeting the PR merge commit into `stable`.
    6. Delete your `release/vX.Y.0` branch.
  - When releasing a patch version of an existing minor version.
    1. Create a `release/vX.Y.Z` branch off the previous `vX.Y.Z` tag.
    2. Use `git cherry-pick` to cherry-pick the hotfix commits necessary to satisfy your patch requirements into the `release/vX.Y.Z` branch.
       If the `hotfix` commits depend on any features (which is a nasty situation to get into) make sure to cherry-pick those features in order with the commits.
    3. Change the `VERSION` file to contain `X.Y.Z`,
    4. If necessary, add any release notes here.
    5. Merge the branch into `stable` via PR.
    6. Create a release targeting the PR merge commit into `stable`.
    7. Delete your `release/vX.Y.Z` branch.
