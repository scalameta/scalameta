* Choose the right version number:
  * `x.0.0` is reserved for incompatible changes and require a milestone cycle.
  * `x.y.0` is reserved for compatible changes.
  * `x.y.z` is reserved for bugfixes that don't change the public API or the SemanticDB schema.
  * For more details, consult [versioning policy](VERSIONING.md).
* Tag the release:
  * The tag must be called `vx.y.x`, e.g. `v3.0.0`.
  * To create the tag:
    * https://github.com/scalameta/scalameta/releases/new
    * Or, `git tag -a vx.y.z -m "vx.y.z"` && `git push upstream --tags`
* Close the milestone corresponding to the release.
* Wait for [the Travis CI job](https://travis-ci.org/scalameta/scalameta/branches)
   in Active Branches to build the binaries and stage them to Sonatype.
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
