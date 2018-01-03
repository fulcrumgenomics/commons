val gitHeadCommitSha = settingKey[String]("current git commit SHA")
gitHeadCommitSha in ThisBuild := Process("git rev-parse --short HEAD").lines.head

// *** IMPORTANT *** 
// One of the two "version" lines below needs to be uncommented.
// version in ThisBuild := "0.3.0" // the release version
version in ThisBuild := s"0.4.0-${gitHeadCommitSha.value}-SNAPSHOT" // the snapshot version
