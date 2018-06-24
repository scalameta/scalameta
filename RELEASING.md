* Choose the right version number:
  * `x.0.0` is reserved for incompatible changes and require a milestone cycle.
  * `x.y.0` is reserved for compatible changes.
  * `x.y.z` is reserved for bugfixes that don't change the public API,
    the SemanticDB schema and major/minor versions of our dependencies.
  * For more details, consult [versioning policy](VERSIONING.md).
* Tag the release:
  * The tag must be called `vx.y.z`, e.g. `v3.0.0`.
  * `git tag -a vx.y.z -m "vx.y.z"`
  * `git push upstream --tags`
  * Do not create a release on GitHub just yet. Creating a release on GitHub
    sends out a notification to repository watchers, and the release isn't ready
    for that yet.
* Wait for [the Travis CI job](https://travis-ci.org/scalameta/scalameta/branches)
  in Active Branches to build the binaries and stage them to Sonatype.
* While waiting for Travis, update the milestones:
  * https://github.com/scalameta/scalameta/milestones
  * Close the milestone or milestones corresponding to the release.
    For example, for 3.3.0, we closed both 3.2.1 and 3.3.0 (we never
    released 3.2.1, so all its tickets went straight to 3.3.0).
  * Create the milestone or milestones corresponding to future releases.
    For example, for 3.3.0, we created both 3.3.1 and 3.4.0.
* While waiting for Travis, draft a release on GitHub:
  * https://github.com/scalameta/scalameta/releases/new.
  * In the dropdown, pick the recently pushed tag.
  * In the release title, say `Scalameta vx.y.z`.
  * Write the release notes using [3.7.0 release notes](https://github.com/scalameta/scalameta/releases/edit/v3.7.0)
    as a template. In the future, we may automate this step.
  * Click "Save draft". The release is still not ready for an announcement,
    so we shouldn't "Publish release" just yet.
* Finalize the release on Sonatype:
  * `sbt sonatypeReleaseAll`
  * Alternatively:
    * Go to [Staging Repositories](https://oss.sonatype.org/#stagingRepositories).
    * Close the repositories that have been created by the Travis CI job.
      There can be several respositories produced by the job.
    * Release the repositories.
* Verify the Sonatype release:
  * Make sure that the release shows up at https://oss.sonatype.org/content/repositories/releases/org/scalameta/.
  * Run `./bin/test-release.sh $VERSION` to ensure that all artifacts have successfully been released.
* Update the website:
  * Submit a pull request like https://github.com/scalameta/tutorial/pull/33.
  * Wait for the Travis CI validation.
  * Merge the pull request immediately without waiting for approvals.
  * Apply the website hotfix (https://github.com/scalameta/scalameta.github.com/commit/adc7920ee56dfbe2cb615492554808ded922ae34).
* Update the SemanticDB guide:
  * Update version numbers in https://github.com/scalameta/scalameta/blob/master/semanticdb/semanticdb3/guide.md
  * Submit a pull request to scalameta/scalameta.
  * Merge the pull request immediately without waiting for travis or approvals.
* Publish the release on GitHub:
  * https://github.com/scalameta/scalameta/releases
  * Select the previously drafted release.
  * Click "Publish release".
* Verify the Maven Central release:
  * Wait for up to several hours (for 3.2.0, we waited for 15 minutes;
    for 3.3.0, we waited for 1.5 hours).
  * Make sure that the release shows up at https://search.maven.org/#search%7Cga%7C1%7Corg.scalameta%20a%3A%22scalameta_2.12%22.
  * Make sure that `coursier fetch org.scalameta:semanticdb-scalac-core_a.b.c:x.y.z`
    succeeds for the supported Scala versions.
  * If it's been a while, and the release doesn't show up at Maven Central,
    ping Sonatype at [OSSRH-10192](https://issues.sonatype.org/browse/OSSRH-10192).
* Upgrade the downstream projects:
  * https://github.com/scalacenter/scalafix
    (upgrade to Scalameta 3.4+ is currently blocked, so feel free to
    skip upgrading this project for the time being).
  * https://github.com/scalameta/metadoc
    (upgrade to Scalameta 3.0+ is currently blocked, so feel free to
    skip upgrading this project for the time being).
  * https://github.com/scalameta/metals
    (upgrade to Scalameta 3.0+ is currently blocked, so feel free to
    skip upgrading this project for the time being).
