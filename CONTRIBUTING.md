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

Dot uses [SemVer v2](http://semver.org/spec/v2.0.0.html) as a model to standardize our versioning system.
A "breaking change" in our context is something that might cause a user to have to change their personal config; any change to the "API" (which is the set of functions exposed to the user) is considered as such.

In general, our branching workflow consists of two persistent branches:

- `master`, which contains all of the latest changes and should be in a stable state.
  This is used as the base for all of the `release-` branches (see below.)
- `stable`, which is a tracking branch that gets releases merged into it.
  This is the branch to be used by users who only want stable features to be added to their configuration.

In addition, we use the following scheme for additional branches:

- `release-vX.Y` and `release-vX.Y.Z` branches are branches used to control versioning.
  - To release a new version from `master`,
    1. after merging all PR's into `master`,
    2. Create your `release-v` branch.
    3. Bump the version on the `release-v` branch, either through a PR on a separate branch or something like that.
       If you have any release notes to add, do so here.
    4. Merge the branch back into `master` via a PR, including writing release notes and such.
    5. Create a release targeting the PR merge commit.
- `feature/` can be used as a prefix for feature branches, but this is not recommended.
  (Just use `kebab-case` for your branch names, we assume that anything without a special prefix is a normal feature PR.)
- `hotfix/` should be used as a prefix for hotfix branches.
  If a hotfix targets a release, (typical) a `release-vX.Y.Z` branch should be used as the base of the PR instead; submit a PR for both the `release-vX.Y.Z` and the `hotfix/` branch.
