@echo off
REM Minimal mvnw.cmd shim: prefer bundled wrapper if present, otherwise call system mvn
nIF EXIST "%~dp0\.mvn\wrapper\maven-wrapper.jar" (
  java -jar "%~dp0\.mvn\wrapper\maven-wrapper.jar" %*
) ELSE (
  mvn %*
)
