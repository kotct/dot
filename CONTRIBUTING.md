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
  This is used as the base for all of the `release/*` branches (see below.)
- `stable`, which is a tracking branch that gets releases merged into it.
  This is the branch to be used by users who only want stable features to be added to their configuration.

In addition, we use the following scheme for additional branches:

- `release/vX.Y` and `release/vX.Y.Z` branches are branches used to control versioning.
  - To release a new version from `master`,
    1. After merging all PR's into `master`,
    2. Create your `release/vX.Y(.Z)` branch.
    3. Bump the version on the `release/vX.Y(.Z)` branch in a commit.
       If you have any release notes to add, do so here.
    4. Merge the branch into `stable` via a PR.
    5. Create a release targeting the PR merge commit into `stable`.
    6. Make a PR to merge back into `master`.
- `feature/` can be used as a prefix for feature branches, but this is not recommended.
  (Just use `kebab-case` for your branch names, we assume that anything without a special prefix is a normal feature PR.)
- `hotfix/` should be used as a prefix for hotfix branches.
  In the discussion of a problem, the maintainers will decide what version to release the hotfixes as and will create a `release/vX.Y.Z` branch for you to use as your base.
  If a hotfix targets a release, (which is the typical case) the nascent `release/vX.Y.Z` branch should thus be used; submit a PR for the `hotfix/` branch into the `release/vX.Y.Z` branch.
