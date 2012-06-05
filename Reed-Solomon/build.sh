rm -r bin
mkdir bin
scalac $(find ./src | grep .scala) -deprecation -unchecked -d bin