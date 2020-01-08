import java.io.File

lazy val checkCopyrights = taskKey[Unit]("Checks copyrights of source files")
lazy val buildParsers = taskKey[Unit]("Builds parsers")
lazy val deleteParserDir = taskKey[Unit]("Delete java parser directory")

// phase test
lazy val parseTest = taskKey[Unit]("Launch parse tests")
lazy val cloneTest = taskKey[Unit]("Launch clone vector generator tests")

lazy val root = (project in file(".")).
  settings(
    name := "SAF",
    version := "2.0",
    organization := "kr.ac.kaist.saf",
    scalaVersion := "2.12.3",
    checkCopyrights in Compile := {
      val violated: String = (baseDirectory.value + "/bin/checkCopyrights.sh" !!)
      if (violated != "") {
        throw new Error("\nFix the copyright(s) of the following:\n" + violated)
      }
    },
    buildParsers in Compile := {
      // xtc
      val xtcFile = new File("./lib/xtc.jar")
      if (!xtcFile.exists) {
        // TODO exception handling: not downloaded
        IO.download(new URL("https://cs.nyu.edu/rgrimm/xtc/xtc.jar"), xtcFile)
      }

      val options = ForkOptions(bootJars = Seq(xtcFile))
      val srcDir = baseDirectory.value + "/src/main"
      val inDir = srcDir + "/scala/kr/ac/kaist/saf/parser/"
      val outDir = srcDir + "/java/kr/ac/kaist/saf/parser/"
      val outFile = file(outDir)
      if (!outFile.exists) IO.createDirectory(outFile)
      val arguments = Seq("-in", srcDir + "/scala", "-enc-out", "UTF-8",
                          "-out", outDir, inDir + "SOL.rats")
      val mainClass = "xtc.parser.Rats"
      val cache = FileFunction.cached(outFile,
                                      FilesInfo.lastModified,
                                      FilesInfo.exists) {
        in: Set[File] => {
          Fork.java(options, mainClass +: arguments)
          Set(file(inDir + "SOL.rats"))
        }
      }
      cache(file(inDir).asFile.listFiles.toSet)
    },
    testOptions in Test += Tests.Argument("-fDG", baseDirectory.value + "/tests/detail"),
    compile <<= (compile in Compile) dependsOn (buildParsers in Compile, checkCopyrights in Compile),
    test <<= (testOnly in Test).toTask(s" -- -n ParseTest") dependsOn compile,
    parseTest <<= (testOnly in Test).toTask(s" -- -n ParseTest") dependsOn compile,
    cloneTest <<= (testOnly in Test).toTask(s" -- -n CloneTest") dependsOn compile
  )

scalacOptions in ThisBuild ++= Seq("-deprecation", "-feature",
                                   "-language:postfixOps",
                                   "-language:implicitConversions")

unmanagedJars in Compile ++= Seq(file("lib/xtc.jar"))
cleanFiles ++= Seq(file("src/main/java/kr/ac/kaist/saf/parser/"))

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % scalaVersion.value,
  "org.scala-lang" % "scala-compiler" % scalaVersion.value % "scala-tool",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test" withSources,
  "com.typesafe.akka" %% "akka-http" % "10.0.10",
  "io.spray" %% "spray-json" % "1.3.2",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.9.2",
  "com.fasterxml.jackson.module" % "jackson-module-scala_2.12" % "2.9.1"
)

javacOptions ++= Seq("-encoding", "UTF-8")

retrieveManaged := true
