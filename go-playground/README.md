Go playground

Run the small HTTP server that exposes:

- GET / -> greeting
- GET /sort?nums=[1,2,3] (JSON array) or nums=1,2,3 (CSV) -> sorted JSON array

Build and run:

```bash
cd go-playground
go build -o bin/playground
./bin/playground
```

Run tests:

```bash
cd go-playground
go test ./...
```
