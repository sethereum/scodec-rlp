name := "scodec-rlp"

organization := "com.github.sethereum"

version := "0.1"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.scodec"        %% "scodec-core"    % "1.8.3",

  "org.scalatest"     %% "scalatest"      % "2.2.1" % "test"
)

homepage := Some(url("https://github.com/sethereum/scodec-rlp"))

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

mainClass in (Compile, run) := Some("Main")