rm -r doc
mkdir doc
scaladoc -d doc $(find ./src | grep .scala) -doc-title "Reed-Solomon error correction"