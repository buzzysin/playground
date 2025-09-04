# Simple convenience Makefile for java-playground
# It prefers the project Maven wrapper (`./mvnw`) if present, otherwise falls back to system `mvn`.

MVN := $(shell if [ -x ./mvnw ]; then echo ./mvnw; else echo mvn; fi)

.PHONY: help build test package install-core run-demo exec-demo clean

help:
	@echo "Available targets: build test package install-core run-demo exec-demo clean"

build:
	@echo "Running: $(MVN) -pl app -am package"
	$(MVN) -pl app -am package

test:
	@echo "Running: $(MVN) -pl app -am test"
	$(MVN) -pl app -am test

package: build

install-core:
	@echo "Installing core to local repo: $(MVN) -pl libs/core install"
	$(MVN) -pl libs/core install

# Run via exec plugin using the app POM (exec runs in module context)
run-demo:
	@echo "Running DependencyDemo via exec plugin (app/pom.xml)"
	$(MVN) -f app/pom.xml org.codehaus.mojo:exec-maven-plugin:3.5.1:java -Dexec.mainClass=com.example.app.dependency.DependencyDemo

# Run directly using compiled classes (fast, no plugin)
exec-demo:
	@echo "Running DependencyDemo using java with compiled classes"
	java -cp app/target/classes:libs/core/target/classes com.example.app.dependency.DependencyDemo

clean:
	$(MVN) clean
