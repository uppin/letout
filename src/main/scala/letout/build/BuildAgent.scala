package letout.build

import java.io._
import java.net.URLClassLoader
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import coursier._
import coursier.cache._
import org.specs2.control.ExecuteActions.attemptExecuteAction
import org.specs2.control.consoleLogging
import org.specs2.fp.Name
import org.specs2.fp.syntax._
import org.specs2.main.Arguments
import org.specs2.reporter.LineLogger.consoleLogger
import org.specs2.specification.core.Env

import scala.tools.nsc.Main
import scala.util.{Failure, Success}

object BuildAgent {
  def main(args: Array[String]): Unit = BuildAgent.synchronized {
    val options = ScalaBuilderOptions.parse(args)
    val builder = new BuildAgent(options)
    val output = System.out

    if (options.automation) {
      val result = withOutputInterception {
        builder.build()
      }

      output.println(result.mainClasspath)

    } else {
      builder.build()
    }
  }

  private def withOutputInterception[R](fn: => R): R = {
    val originalOutput = System.out
    val originalError = System.err

    val output = new ByteArrayOutputStream()
    val error = new ByteArrayOutputStream()

    val interceptedOutputStream = new PrintStream(output)
    val interceptedErrorStream = new PrintStream(error)

    System.setOut(interceptedOutputStream)
    System.setErr(interceptedErrorStream)

    try fn
    catch {
      case e: Throwable =>
        output.flush()
        error.flush()
        originalOutput.write(output.toByteArray)
        originalError.write(error.toByteArray)

        e.printStackTrace(originalError)
        throw e
    }
    finally {
      System.setOut(originalOutput)
      System.setErr(originalError)
    }
  }
}

object ScalaBuilderOptions {
  val parser = new scopt.OptionParser[ScalaBuilderOptions]("build-agent") {
    head("build-agent", "1.0")

    opt[String]('o', "output").unbounded.action { (outputPath, c) =>
      c.copy(outputPath = Some(Paths.get(outputPath)))
    }

    arg[Seq[String]]("<sources>").unbounded.action { (sources, c) =>
      c.copy(sources = c.sources ++ sources)
    }

    opt[Seq[String]]('d', "dep").unbounded.action { (deps, c) =>
      c.copy(deps = c.deps ++ deps)
    }

    opt[Unit]('a', "automation").unbounded.action { (_, c) =>
      c.copy(automation = true)
    }

    opt[Seq[String]]('t', "test-sources").unbounded.action { (testSources, c) =>
      c.copy(testSources = c.testSources ++ testSources)
    }

    opt[Seq[String]]("test-dep").unbounded.action { (deps, c) =>
      c.copy(testDeps = c.testDeps ++ deps)
    }

    opt[Unit]("commands").action { (deps, c) =>
      c.copy(commands = true)
    }
  }

  def parse(args: Array[String]): ScalaBuilderOptions =
    parser.parse(args, ScalaBuilderOptions()).get
}

case class ScalaBuilderOptions(
    outputPath: Option[Path] = None,
    sources: Seq[String] = Nil,
    testSources: Seq[String] = Nil,
    deps: Set[String] = Set.empty,
    testDeps: Set[String] = Set.empty,
    commands: Boolean = false,
    automation: Boolean = false)

class BuildAgent(options: ScalaBuilderOptions) {
  private val depResolver = new DepResolver

  def build(): BuildResult = {
    val outputDirFile = new File(new File(".").getParentFile, s"target/main.jar").getAbsoluteFile
    val testOutputDirFile = new File(new File(".").getParentFile, s"target/test-classes").getAbsoluteFile

    val testSourceFiles = listOfSourceFiles(options.testSources)
    val sourceFiles = listOfSourceFiles(options.sources) -- testSourceFiles
    val mainClasspath = resolveClasspath(options.deps, Nil)

    if (options.sources.nonEmpty)
      withExecutionTime("Compile", compile(outputDirFile, sourceFiles, mainClasspath))

    if (options.testSources.nonEmpty) {
      val combinedDeps = options.deps ++ options.testDeps
      val testClasspath = resolveClasspath(combinedDeps, Seq(outputDirFile))

      withExecutionTime("Test Compile", compile(testOutputDirFile, testSourceFiles, testClasspath))

      val runClasspath = depResolver.resolve(combinedDeps) :+ outputDirFile :+ testOutputDirFile

      withExecutionTime("Tests", runTests(new File(".").getAbsoluteFile, runClasspath))

      options.outputPath.map(outputPath =>
        Files.copy(outputDirFile.toPath, outputPath, StandardCopyOption.REPLACE_EXISTING))
    }

    BuildResult(mainClasspath)
  }

  def compile(outputDirFile: File, sourceFiles: Set[File], classpath: String, excludeSources: Seq[String] = Nil): Unit = {
    if (!outputDirFile.getName.endsWith(".jar")) outputDirFile.mkdirs()
    else outputDirFile.getParentFile.mkdirs()

    //compileWithInterpreter(sourceRootFile, sourceFiles, outputDirFile)
    compileWithCompiler(sourceFiles, classpath, outputDirFile)
  }

  private def resolveClasspath(deps: Set[String], extraClasspath: Seq[File]): String = {
    (depResolver.resolve(deps) ++ extraClasspath).map(_.getAbsolutePath).mkString(":")
  }

//  private def compileWithInterpreter(sourceRootFile: File, sourceFiles: Seq[SourceFile], outputDirFile: File): Unit = {
//    val interpreter = createInterpreter(outputDirFile)
//
//    if (!interpreter.compileSources(sourceFiles:_*)) {
//      throw new Exception("Compilation failed")
//    }
//
//    println("Summary:")
//
//    interpreter.allDefinedNames.foreach((name) => println(name))
//    interpreter.classLoader.getPackages().foreach(printPackage(_))
//
//    val loadedClass = interpreter.classLoader.loadClass("org.greeter.Greeter")
//
//    println(s"Loaded class: ${loadedClass.getName}")
//  }

  private def compileWithCompiler(sourceFiles: Set[File], classpath: String, outputDirFile: File): Unit = {
    if (!Main.process(Array("-d", outputDirFile.getAbsolutePath, "-cp", classpath) ++ sourceFiles.map(_.getAbsolutePath)))
      throw new Exception("Compilation failed")
  }

  private def runTests(basepath: File, classpath: Seq[File]): Unit = {
    val currentThread = java.lang.Thread.currentThread()
    val currentClassLoader = currentThread.getContextClassLoader

    val testsClassLoader = new URLClassLoader(classpath.map(_.toURI.toURL).toArray, currentClassLoader)

    currentThread.setContextClassLoader(testsClassLoader)

    try {
      runTestsWith(Array("filesrunner.basepath", basepath.getAbsolutePath, ".*Spec"))

//      println("== RESULT", basepath, classpath, result)
    }
    finally {
//      executionEnv.shutdown()
      currentThread.setContextClassLoader(currentClassLoader)
    }
  }

  private def runTestsWith(args: Array[String]): Unit = {
    val env = Env(arguments = Arguments(args: _*),
      lineLogger = consoleLogger)
    val logging = (s: String) => Name(consoleLogging(s))

    import org.specs2.runner.Runner._

    try attemptExecuteAction(org.specs2.runner.files.run(env), consoleLogging)(env.specs2ExecutionEnv) match {
      case Left(t) =>
        logThrowable(t, env.arguments)(logging).value
        throw t

      case Right((result, warnings)) =>
        result.fold(
          error => error.fold(
            t => logUserWarnings(warnings)(logging) >> logThrowable(t, env.arguments)(logging) map { _ => Failure(t) },
            m => logUserWarnings(warnings)(logging) >> logging(m) map { _ => Failure(new Exception(m)) }
          ),
          ok => logUserWarnings(warnings)(logging) >>
            (if (ok.isSuccess) Name(Success(ok)) else Name(Failure(new Exception(s"Tests failed $ok"))))
        ).value.get
    }
    finally env.shutdown
  }
//  private def printPackage(pkg: Package): Unit = {
//    println(pkg.getName)
//
    //pkg.loader.getPackages.foreach(printPackage(_))
//  }

//  private def createInterpreter(outputDir: File) = {
//    val settings = new GenericRunnerSettings(println)
//
//    settings.classpath.value = classpath
//    settings.outdir.value = outputDir.getAbsolutePath
//    settings.d.value = outputDir.getAbsolutePath
//    settings.deprecation.value = true
//    //settings.Ylogcp.value = true
//
//    println(s"Output: ${settings.d.value}")
//
//    new IMain(settings)
//  }

  private def listOfSourceFiles(sources: Seq[String]): Set[File] =
    expandSources(sources).toSet[String].flatMap(source => {
      val d = new File(source)

      if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isFile).toList
      } else {
        List[File](d).filter(_.isFile)
      }
    })

  private def expandSources(sources: Seq[String]) =
    sources.flatMap {
      case source if new File(source).exists() => Seq(source)
      case source => expandSource(source)
    }

  private def expandSource(source: String): Seq[String] = {
    val matcher = FileSystems.getDefault().getPathMatcher(s"glob:$source")
    val files = Seq.newBuilder[String]

    Files.walkFileTree(Paths.get(""), new SimpleFileVisitor[Path]() {
      override def visitFile(file: Path, attrs: BasicFileAttributes) = {
        if (matcher.matches(file)) {
          files += file.toString
        }

        FileVisitResult.CONTINUE
      }

      override def visitFileFailed(file: Path, exc: IOException) = {
        FileVisitResult.CONTINUE
      }
    });

    files.result
  }

//  private def toSourceFile(file: File): SourceFile = {
//    val fileDir = file.getParent
//    val filename = file.getName
//    val content = new String(Files.readAllBytes(Paths.get(fileDir, filename)))
//
//    new BatchSourceFile(new VirtualFile(filename, fileDir), content.toCharArray)
//  }

  private def withExecutionTime(name: String, getValue: => Unit) = {
    val start = System.currentTimeMillis

    getValue

    println(s"$name took ${System.currentTimeMillis - start}ms")
  }
}

class DepResolver {
  def resolve(deps: Set[String]): Seq[File] = {
    val repositories = Seq(
      MavenRepository("https://repo1.maven.org/maven2")
    )

    Fetch()
      .addDependencies(deps.map(dependencyOf).toSeq:_*)
      .withCache(Cache.default)
      .withRepositories(repositories)
      .run()
  }

  private def dependencyOf(dep: String): Dependency = {
    val Array(groupId, artifactId, version) = dep.split(':')

    Dependency(
      Module(Organization(groupId), ModuleName(artifactId)), version
    )
  }
}

case class BuildResult(
    mainClasspath: String
)