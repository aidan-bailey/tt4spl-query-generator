# tt4spl-query-generator (ttqg)

`ttqg` is a test set generator for propositional logical property checking, written in `Scala 2.13.3`, requires `Java 16` to compile.

## Building && Usage

`ttqg` uses [sbt](https://www.scala-sbt.org/1.x/docs/Setup.html) as the project build tool.

From the project directory run

```sh
sbt assembly
```

to compile the project. This will generate a `JAR` in the `target/scala-2.13`, which can be run using

```sh
scala target/scala-2.13/ttqg-assembly-*.*.jar
```

presenting the usage text.

## Parameters

|-----------|-------------|
| Parameter | Description |
| &#8211;&#8211;| |
|-----------|-------------|
