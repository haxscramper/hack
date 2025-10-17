plugins {
    kotlin("jvm") version "1.9.10"
    kotlin("plugin.serialization") version "1.9.10"
    application
}

group = "com.example"
version = "1.0.0"

repositories {
    mavenCentral()
}

dependencies {
    // Eclipse ELK dependencies
    implementation("org.eclipse.elk:org.eclipse.elk.core:0.8.1")
    implementation("org.eclipse.elk:org.eclipse.elk.graph:0.8.1")
    implementation("org.eclipse.elk:org.eclipse.elk.alg.layered:0.8.1")
    
    // Kotlin serialization for JSON
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.6.0")
    
    // CLI argument parsing
    implementation("com.github.ajalt.clikt:clikt:4.2.1")
    
    // Logging
    implementation("org.slf4j:slf4j-simple:1.7.36")
    
    // Testing
    testImplementation(kotlin("test"))
    testImplementation("org.junit.jupiter:junit-jupiter:5.9.2")
}

tasks.test {
    useJUnitPlatform()
}

kotlin {
    jvmToolchain(17)  
}

application {
    mainClass.set("com.example.elk.GraphLayoutAppKt")
}

tasks.jar {
    manifest {
        attributes["Main-Class"] = "com.example.elk.GraphLayoutAppKt"
    }
    configurations["compileClasspath"].forEach { file: File ->
        from(zipTree(file.absoluteFile))
    }
    duplicatesStrategy = DuplicatesStrategy.INCLUDE
}


tasks.withType<Jar> {
    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
    
    // Exclude signature files
    exclude("META-INF/*.SF")
    exclude("META-INF/*.DSA")
    exclude("META-INF/*.RSA")
}

// If you're using the application plugin's distZip/distTar
application {
    applicationDefaultJvmArgs = listOf("-Djava.security.egd=file:/dev/./urandom")
}

// If creating a fat JAR
tasks.register<Jar>("fatJar") {
    archiveClassifier.set("all")
    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
    
    manifest {
        attributes["Main-Class"] = "com.example.elk.MainKt"
    }
    
    from(sourceSets.main.get().output)
    
    dependsOn(configurations.runtimeClasspath)
    from({
        configurations.runtimeClasspath.get().filter { it.name.endsWith("jar") }.map { zipTree(it) }
    }) {
        exclude("META-INF/*.SF")
        exclude("META-INF/*.DSA")
        exclude("META-INF/*.RSA")
    }
}
