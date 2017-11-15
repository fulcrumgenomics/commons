val gitHeadCommitSha = settingKey[String]("current git commit SHA")
gitHeadCommitSha in ThisBuild := Process("git rev-parse --short HEAD").lines.head

// version in ThisBuild := "0.3.0"
// version in ThisBuild := s"0.4.0-${gitHeadCommitSha.value}-SNAPSHOT"
