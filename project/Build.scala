import sbt._

import com.typesafe.sbt.SbtScalariform.scalariformSettings
import net.virtualvoid.sbt.graph.Plugin.graphSettings
import org.scalastyle.sbt.ScalastylePlugin.{ Settings => scalaStyleSettings }

import sbt.Keys._
import scoverage.ScoverageSbtPlugin._
import scoverage.ScoverageSbtPlugin.ScoverageKeys._


object SharedBuild extends Build {
  lazy val shared = Project(
    id = "shared",
    base = file("."),
    aggregate = Seq(sharedCore, sharedExamples),
    settings = commonSettings ++ Seq(
      moduleName := "shared-root",

      (unmanagedSourceDirectories in Compile) := Nil,
      (unmanagedSourceDirectories in Test) := Nil,

      publish := (),
      publishLocal := ()
    )
  )

  lazy val sharedCore =
    Project(
      id = "shared-core",
      base = file("core"),
      settings = commonSettings ++ Seq(
        moduleName := "shared",

        managedSourceDirectories in Test := Nil,

        libraryDependencies <++= scalaVersion { sv =>
          Seq(
            "org.scala-lang" % "scala-compiler" % sv,
            "org.scalaz" % "scalaz-core_2.10" % "7.1.0"
        )},

        initialCommands in console := """import sjc.shared._""",

        mappings in (Compile, packageSrc) <++=
          (sourceManaged in Compile, managedSources in Compile) map { (base, srcs) =>
            (srcs pair (Path.relativeTo(base) | Path.flat))
          },

        mappings in (Compile, packageSrc) <++=
          (mappings in (Compile, packageSrc) in LocalProject("shared-examples"))
      )
    )

  lazy val sharedExamples = Project(
    id = "shared-examples",
    base = file("examples"),
    dependencies = Seq(sharedCore),

    settings = commonSettings ++ Seq(
      libraryDependencies <++= scalaVersion(sv => Seq("org.scala-lang" % "scala-compiler" % sv)),

      runAllIn(Compile),

      publish := (),
      publishLocal := ()
    )
  )

  lazy val runAll = TaskKey[Unit]("run-all")

  def runAllIn(config: Configuration) = {
    runAll in config <<= (discoveredMainClasses in config, runner in run, fullClasspath in config, streams) map {
      (classes, runner, cp, s) => classes.foreach(c => runner.run(c, Attributed.data(cp), Seq(), s.log))
    }
  }

  def commonSettings = graphSettings ++ Defaults.defaultSettings ++
  // uncomment when you want to reset the formatting of the project
  // scalariformSettings ++
  scalaStyleSettings ++ instrumentSettings ++ Seq(
    organization        := "com.github.stacycurl",
    scalaVersion        := "2.10.3",

    (unmanagedSourceDirectories in Compile) <<= (scalaSource in Compile)(Seq(_)),
    (unmanagedSourceDirectories in Test) <<= (scalaSource in Test)(Seq(_)),

    scalacOptions       := Seq(
      "-feature",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-Xfatal-warnings",
      "-deprecation",
      "-unchecked"),

    resolvers           ++= Seq(
      Classpaths.typesafeSnapshots,
      "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
    ),
    libraryDependencies += "org.scalaz"     % "scalaz-scalacheck-binding_2.10" % "7.1.0"  % "test",
    libraryDependencies += "org.scalacheck" % "scalacheck_2.10"                % "1.11.5" % "test",
    libraryDependencies += "com.novocode"   % "junit-interface"                % "0.7"    % "test",

    highlighting := true,
    minimumCoverage := 100,
    failOnMinimumCoverage := true
  )
}
