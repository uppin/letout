package letout.build

import java.io._
import java.net.URLClassLoader
import java.nio.file._
import java.nio.file.attribute.{BasicFileAttributes, PosixFilePermission}
import java.time.Clock

import coursier._
import coursier.cache._
import org.specs2.control.ExecuteActions.attemptExecuteAction
import org.specs2.control.consoleLogging
import org.specs2.fp.Name
import org.specs2.fp.syntax._
import org.specs2.main.Arguments
import org.specs2.reporter.LineLogger.consoleLogger
import org.specs2.specification.core.Env

import scala.collection.JavaConverters._
import scala.tools.nsc.Main
import scala.util.{Failure, Success}

object BuildAgent {

  def main(args: Array[String]): Unit = BuildAgent.synchronized {
    val options = ScalaBuilderOptions.parse(args)
    val builder = new BuildAgent(options)

    builder.build()
  }
}

object ScalaBuilderOptions {
  val parser = new scopt.OptionParser[ScalaBuilderOptions]("build-agent") {
    head("build-agent", "1.0")

    opt[String]('o', "output").unbounded.action { (outputPath, c) =>
      c.copy(outputPath = Some(Paths.get(outputPath)))
    }

    opt[String]('h', "home-dir").unbounded.action { (homeDir, c) =>
      c.copy(scriptOptions = Some(c.scriptOptions.getOrElse(ScriptOptions.Empty).copy(homeDir = Some(Paths.get(homeDir)))))
    }

    opt[String]('n', "script-name").unbounded.action { (scriptName, c) =>
      c.copy(scriptOptions = Some(c.scriptOptions.getOrElse(ScriptOptions.Empty).copy(name = scriptName)))
    }

    opt[String]('m', "main-class").unbounded.action { (mainClass, c) =>
      c.copy(scriptOptions = Some(c.scriptOptions.getOrElse(ScriptOptions.Empty).copy(mainClass = mainClass)))
    }

    opt[Seq[String]]("script-boot-dep").unbounded.action { (deps, c) =>
      c.copy(scriptOptions = Some(c.scriptOptions.getOrElse(ScriptOptions.Empty).copy(bootDeps = deps.toSet)))
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

    opt[Unit]("commands").action { (_, c) =>
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
    scriptOptions: Option[ScriptOptions] = None,
    commands: Boolean = false,
    automation: Boolean = false)

case class ScriptOptions(
    name: String,
    mainClass: String,
    homeDir: Option[Path] = None,
    bootDeps: Set[String] = Set.empty
)

object ScriptOptions {
  val Empty = ScriptOptions("", "")
}

class BuildAgent(options: ScalaBuilderOptions) {
  private val depResolver = new DepResolver

  def build(): Unit = {
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

    options.scriptOptions.foreach { options =>
      createScript(options, mainClasspath + ":" + outputDirFile.getAbsolutePath)
    }
  }

  def compile(outputDirFile: File, sourceFiles: Set[File], classpath: String, excludeSources: Seq[String] = Nil): Unit = {
    if (!outputDirFile.getName.endsWith(".jar")) outputDirFile.mkdirs()
    else outputDirFile.getParentFile.mkdirs()

    //compileWithInterpreter(sourceRootFile, sourceFiles, outputDirFile)
    compileWithCompiler(sourceFiles, classpath, outputDirFile)
  }

  private def createScript(scriptOptions: ScriptOptions, mainClasspath: String): Unit = {
    val homeDir = scriptOptions.homeDir.getOrElse(Paths.get("."))
    val scriptPath = homeDir.resolve(scriptOptions.name)
    val bootClasspath = resolveClasspath(scriptOptions.bootDeps + "com.facebook:nailgun-server:1.0.0")
    val version = Clock.systemUTC().millis().toString

    if (!homeDir.resolve("scriptsCommon").toFile.exists())
      createScriptBase(homeDir)

    Files.write(scriptPath,
      s"""#!/usr/bin/env python
        |
        |import sys
        |
        |from scriptsCommon import ensure_ng, run_class_ng
        |
        |mainClass = '${scriptOptions.mainClass}'
        |bootClasspath = None
        |classpath = '${mainClasspath}:${bootClasspath}'
        |scriptName = '${scriptOptions.name}'
        |domainSocket = '${homeDir.resolve(s"${scriptOptions.name}.socket")}'
        |versionKey = '${version}'
        |
        |ensure_ng(domainSocket, versionKey, bootClasspath, classpath)
        |run_class_ng(domainSocket, mainClass, sys.argv[1:])
      """.stripMargin.getBytes("UTF-8"))

    val scriptPermissions = Files.getPosixFilePermissions(scriptPath).asScala

    Files.setPosixFilePermissions(scriptPath, (scriptPermissions + PosixFilePermission.OWNER_EXECUTE).asJava)
  }

  private def createScriptBase(homeDir: Path): Unit = {
    Files.write(homeDir.resolve("scriptsCommon.py"),
      s"""import subprocess, sys, time, os
         |from ng import NailgunConnection, NailgunException
         |
         |def run_class(bootClasspath, classpath, className, args = []):
         |  commandArgs = mk_java_command(bootClasspath, classpath, className, args)
         |
         |  print(' '.join(commandArgs))
         |
         |  result = subprocess.call(commandArgs)
         |
         |  if result > 0:
         |    sys.exit(result)
         |
         |def mk_java_command(bootClasspath, classpath, className, args = []):
         |  if classpath is None:
         |    classpathArg = []
         |  else:
         |    classpathArg = ['-cp', classpath]
         |
         |  return list(filter(lambda arg: len(arg) > 0, ['java', boot_classpath_for(bootClasspath)] + classpathArg + [className] + args))
         |
         |def boot_classpath_for(classpath):
         |  if classpath is not None:
         |    return '-Xbootclasspath/a:{0}'.format(classpath)
         |  else:
         |    return ''
         |
         |def run_class_ng(domainSocket, className, args = []):
         |  with get_nailgun_connection(domainSocket) as c:
         |    exitCode = c.send_command(className, args)
         |
         |    if exitCode == 898:
         |      raise Exception('No such command ' + className)
         |
         |    return exitCode
         |
         |def ensure_ng(domainSocket, versionKey, bootClasspath, classpath = None):
         |  conn = get_nailgun_connection(domainSocket)
         |
         |  if conn is not None and version_ok_or_kill(conn, domainSocket, versionKey):
         |    return conn
         |
         |  try:
         |    os.unlink(domainSocket)
         |  except OSError:
         |    if os.path.exists(domainSocket):
         |        raise
         |
         |  write_version(domainSocket, versionKey)
         |
         |  commandArgs = mk_java_command(bootClasspath, classpath, 'com.facebook.nailgun.NGServer', [get_transport_address(domainSocket)])
         |
         |  p = subprocess.Popen(commandArgs,
         |                       cwd=os.getcwd(),
         |                       stdout=subprocess.PIPE,
         |                       stderr=subprocess.PIPE)
         |
         |  conn = wait_for_connection(domainSocket, 1000)
         |
         |  print('Nailgun server started on "' + domainSocket + '". PID: ' + str(p.pid))
         |
         |  return conn
         |
         |def write_version(domainSocket, versionKey):
         |  with open(get_version_key_file(domainSocket), 'w') as f:
         |    f.write(versionKey)
         |
         |def version_ok_or_kill(conn, domainSocket, versionKey):
         |  try:
         |    with open(get_version_key_file(domainSocket)) as f:
         |      currentVersionKey = f.read()
         |
         |      if currentVersionKey != versionKey:
         |        print('Stopping server on ' + domainSocket)
         |        return kill(conn)
         |      else:
         |        return True
         |  except IOError:
         |    return True
         |
         |def get_version_key_file(domainSocket):
         |  pre, ext = os.path.splitext(domainSocket)
         |
         |  return pre + '.version'
         |
         |def kill(conn):
         |  conn.send_command('ng-stop')
         |
         |def get_transport_address(domainSocket):
         |  return 'local:' + domainSocket
         |
         |def wait_for_connection(domainSocket, timeout, start_time = time.time()):
         |  elapsed_time = 0
         |
         |  while elapsed_time < timeout:
         |    elapsed_time = (time.time() - start_time) * 1000
         |
         |    conn = get_nailgun_connection(domainSocket)
         |
         |    if conn is not None:
         |      return conn
         |
         |    time.sleep(0.01)
         |
         |  raise Exception('Timeout of ' + str(timeout) + ' occurred. Elapsed ' + str(elapsed_time))
         |
         |def get_nailgun_connection(domainSocket):
         |  try:
         |    return mk_nailgun_connection(domainSocket)
         |  except NailgunException:
         |    return None
         |
         |def mk_nailgun_connection(domainSocket):
         |  return NailgunConnection(
         |      get_transport_address(domainSocket),
         |      cwd=os.getcwd()
         |  )
         |
      """.stripMargin.getBytes("UTF-8"))
  }

  private def resolveClasspath(deps: Set[String], extraClasspath: Seq[File] = Nil): String = {
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