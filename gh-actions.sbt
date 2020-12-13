ThisBuild / githubWorkflowJavaVersions := List("adopt@1.8", "adopt@1.11", "adopt@1.15")
ThisBuild / githubWorkflowScalaVersions := Dependencies.rhoCrossScalaVersions

ThisBuild / githubWorkflowBuild := Seq(
  //WorkflowStep.Sbt(List("scalafmtCheckAll"), name = Some("Check formatting")),
  WorkflowStep.Sbt(List("compile"), name = Some("Compile")),  
  WorkflowStep.Sbt(List("test:compile"), name = Some("Test Compile")),
  //WorkflowStep.Sbt(List("mimaReportBinaryIssues"), name = Some("Check binary compatibility")),
  //WorkflowStep.Sbt(List("unusedCompileDependenciesTest"), name = Some("Check unused dependencies")),
  WorkflowStep.Sbt(List("test"), name = Some("Run tests")),
  WorkflowStep.Sbt(List("doc"), name = Some("Build docs"))
)

ThisBuild / githubWorkflowTargetBranches :=
  // "*" doesn't include slashes
  List("*", "series/*")
ThisBuild / githubWorkflowPublishPreamble ++= {
  Seq(
    WorkflowStep.Run(List("git status"))
  )
}
ThisBuild / githubWorkflowPublishTargetBranches := Seq(
  RefPredicate.Equals(Ref.Branch("master")),
  RefPredicate.StartsWith(Ref.Tag("v"))
)
// this results in nonexistant directories trying to be compressed
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowAddedJobs := Seq()
