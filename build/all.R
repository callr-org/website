library("R.rsp")

for (script in c("install", "rfile")) {
  filename <- sprintf("%s.rsp", script)
  pathname <- rfile(filename, path="rsp/", workdir="site")
  print(pathname)
}
