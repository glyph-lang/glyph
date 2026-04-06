# Apex

A lightweight, single-threaded HTTP/1.1 static file server written in Glyph.
Inspired by the gatling webserver: minimalist, correct, no bloat.

## Build and run

From the `examples/apex/` directory:

```bash
glyph build
./target/debug/apex
```

Then visit [http://localhost:8080](http://localhost:8080).

Test with curl:

```bash
curl -v http://localhost:8080/
curl -v http://localhost:8080/about.html
curl -v http://localhost:8080/style.css
curl -I http://localhost:8080/           # HEAD request
curl -v http://localhost:8080/nope.txt   # 404
```

## Configuration

All configuration is hardcoded (v1):

| Setting       | Value       |
|---------------|-------------|
| Host          | `0.0.0.0`  |
| Port          | `8080`      |
| Document root | `www/`      |

## Project structure

```
examples/apex/
  glyph.toml         Package manifest
  src/
    main.glyph       Entry point: bind, accept loop, recv/send/close
    http.glyph       HTTP request parsing + response building
    server.glyph     Path resolution, MIME types, file reading
  www/                Static content served by the server
```

## Limitations

- **Single-threaded** — one connection at a time, no concurrency
- **No TLS** — plain HTTP only
- **No keep-alive** — every response sends `Connection: close`
- **~8 KB max request** — recv buffer is 8192 bytes
