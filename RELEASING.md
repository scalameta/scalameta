* Choose the right version number:
  * `x.0.0` is reserved for incompatible changes and require a milestone cycle.
  * `x.y.0` is reserved for compatible changes.
  * `x.y.z` is reserved for bugfixes that don't change the public API or the SemanticDB schema.
  * For more details, consult [versioning policy](VERSIONING.md).
* Tag the release:
  * The tag must be called `vx.y.x`, e.g. `v3.0.0`.
  * To create the tag:
    * https://github.com/scalameta/scalameta/releases/new (don't spend time
      filling in the release notes - leave this field empty and edit it later;
      you'll have plenty of time for that while waiting for Travis).
    * Or, `git tag -a vx.y.z -m "vx.y.z"` && `git push upstream --tags`
* Wait for [the Travis CI job](https://travis-ci.org/scalameta/scalameta/branches)
  in Active Branches to build the binaries and stage them to Sonatype.
* While waiting for Travis, update the milestones:
  * https://github.com/scalameta/scalameta/milestones
  * Close the milestone or milestones corresponding to the release.
    For example, for 3.3.0, we closed both 3.2.1 and 3.3.0 (we never
    released 3.2.1, so all its tickets went straight to 3.3.0).
  * Create the milestone or milestones corresponding to future releases.
    For example, for 3.3.0, we created both 3.3.1 and 3.4.0.
* While waiting for Travis, write the release notes. Use the
  [3.3.0 release notes](https://github.com/scalameta/scalameta/releases/tag/v3.3.0)
  as a template.
* Finalize the release on Sonatype:
  * `sbt sonatypeReleaseAll`
  * Alternatively:
    * Go to [Staging Repositories](https://oss.sonatype.org/#stagingRepositories).
    * Close the repositories that have been created by the Travis CI job.
    There can be several respositories produced by the job (currently, there are three).
    * Release the repositories.
* Verify the release:
  * Make sure that the release shows up at https://oss.sonatype.org/content/repositories/releases/org/scalameta/.
  * Wait for 15-30 minutes.
  * Make sure that the release shows up at https://search.maven.org/#search%7Cga%7C1%7Corg.scalameta%20a%3A%22scalameta_2.12%22.
  * If it's been a while, and the release doesn't show up at Maven Central,
    ping Sonatype at [OSSRH-10192](https://issues.sonatype.org/browse/OSSRH-10192).
* Update the website:
  * Submit a pull request like https://github.com/scalameta/tutorial/pull/33.
  * Wait for the Travis CI validation.
  * Merge the pull request immediately without waiting for approvals.
  * Apply the website hotfix (https://github.com/scalameta/scalameta.github.com/commit/adc7920ee56dfbe2cb615492554808ded922ae34).
* Upgrade the downstream projects:
  * https://github.com/scalameta/semanticdb-sbt
  * https://github.com/scalacenter/scalafix
  * https://github.com/scalameta/metadoc
  * https://github.com/scalameta/metals
