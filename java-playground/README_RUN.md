## Running common Maven tasks

This project provides a small `Makefile` with convenient targets to run common Maven commands. The Makefile uses the project's `mvnw` wrapper if present, otherwise falls back to the system `mvn`.

Examples (run from `java-playground`):

```sh
# build core and app
make build

# run tests for app (builds core first)
make test

# install core to local Maven repo
make install-core

# run the demo via the exec-maven-plugin (exec runs in app's context)
make run-demo

# run the demo directly using compiled classes (faster)
make exec-demo
```

If you prefer to run Maven directly, use the same commands used in the Makefile, for example:

```sh
mvn -pl app -am package
mvn -f app/pom.xml org.codehaus.mojo:exec-maven-plugin:3.5.1:java -Dexec.mainClass=com.example.app.dependency.DependencyDemo
```
