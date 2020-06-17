
organization := Settings.Organization
name         := "std-data-iso"
version      := Settings.Version

scalaVersion := "2.12.8"
addCompilerPlugin(Dependencies.Plugin.kindProjector)

Settings.standalone := true
publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)
publishM2Configuration    := publishM2Configuration.value.withOverwrite(true)

libraryDependencies ++= {
  if (Settings.standalone.value) Nil
  else List[ModuleID]()
}