Compossible: Extensible records and type-indexed maps.

Compossible is a new, easy to use library implementing extensible
records and type-indexed maps (TMaps) based on intersection types.
Extensible records are a more modular alternative to case classes.
Type-indexed maps are type-safe maps without the need for key names.

Example usage:
src/test/scala/RecordTest.scala

src/test/scala/TMapTest.scala

Sbt setup:

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.cvogt" %% "compossible" % "0.2"
