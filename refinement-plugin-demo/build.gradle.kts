plugins {
    kotlin("jvm") version "2.1.20"
}

group = "org.example"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
    mavenLocal()
}

dependencies {
    testImplementation(kotlin("test"))

    compileOnly("com.example:refinement-plugin:1.0-SNAPSHOT")
}

tasks.withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile>().configureEach {
    compilerOptions {
        freeCompilerArgs.addAll(
            listOf(
                "-Xplugin=${System.getProperty("user.home")}/.m2/repository/com/example/refinement-plugin/1.0-SNAPSHOT/refinement-plugin-1.0-SNAPSHOT.jar",
                "-P", "plugin:org.example.refinement:refinementAnnotations=org.example.Refinement",
                "-Wextra", "-Xuse-fir-experimental-checkers", "-Xrender-internal-diagnostic-names",
                "-verbose"
            )
        )
    }
}

tasks.test {
    useJUnitPlatform()
}
kotlin {
    jvmToolchain(21)
}