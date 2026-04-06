# Apex Web Server — Architecture

A lightweight, single-threaded HTTP/1.1 static file server written in Glyph.
Inspired by the gatling webserver: minimalist, correct, no bloat.

## Design Principles

1. **Do one thing well**: serve static files over HTTP. No CGI, no proxying, no WebSocket.
2. **Correctness over features**: proper HTTP/1.1 response formatting, proper error codes, proper path security.
3. **No connection reuse**: accept → recv → parse → serve → send → close → loop. Connection: close on every response. Keep-alive adds complexity with zero benefit for a single-threaded server.
4. **Binary-capable via `send_file`**: The C runtime's `glyph_net_tcp_send_file` reads files in 8KB chunks and writes directly to the socket, bypassing Glyph's null-terminated string system. Headers are sent as a String, then the file body is sent via `send_file`. This supports all file types including images, fonts, and PDFs.
5. **Fail fast, fail clearly**: malformed requests get 400, bad paths get 403, missing files get 404. No silent failures.

## Limitations

- **No concurrent connections** — single-threaded blocking accept loop
- **No TLS** — plain HTTP only
- **No chunked encoding** — Content-Length always set, full file read into memory
- **No range requests** — always serves full file
- **Max ~8KB request headers** — recv buffer is 8192 bytes

## Module Layout

```
examples/apex/
  glyph.toml
  src/
    main.glyph      # Entry point: CLI arg parsing, server accept loop
    http.glyph       # HTTP request parsing + response building
    server.glyph     # Static file serving: path resolution, MIME types, file reading
  www/
    index.html       # Sample static content
```

---

## Module: `http.glyph` — HTTP Request Parser + Response Builder

### Struct: HttpRequest

Parsed representation of an incoming HTTP request.

```glyph
struct HttpRequest {
  method: String,      // "GET", "HEAD", etc.
  path: String,        // URL path, e.g. "/index.html"
  version: String,     // "HTTP/1.1" or "HTTP/1.0"
  host: String,        // Host header value (may be empty)
  valid: i32           // 1 = successfully parsed, 0 = malformed
}
```

### Function: `parse_request(raw: str) -> HttpRequest`

Parses a raw HTTP request string into an HttpRequest struct.

**Algorithm:**
1. Split `raw` by `"\r\n"` to get lines
2. Parse request line (first line): split by `" "` → method, path, version
3. Scan remaining lines for `Host:` header
4. Set `valid = 1` if method and path are present, `valid = 0` otherwise

**Edge cases:**
- Empty or whitespace-only input → valid = 0
- Missing HTTP version → default to "HTTP/1.1", still valid
- Unknown headers → ignored (we only extract Host)

### Function: `build_response(status_code: i32, status_text: str, content_type: str, body: str) -> String`

Builds a complete HTTP response string with headers and body.

**Response format:**
```
HTTP/1.1 {status_code} {status_text}\r\n
Server: Apex\r\n
Content-Type: {content_type}\r\n
Content-Length: {body.len()}\r\n
Connection: close\r\n
\r\n
{body}
```

### Function: `build_head_response(status_code: i32, status_text: str, content_type: str, content_length: i32) -> String`

Same as `build_response` but with no body (for HEAD requests). Content-Length is set to what the body *would* be.

### Function: `build_error_response(status_code: i32, status_text: str) -> String`

Convenience wrapper: builds an HTML error page as the body, calls `build_response`.

**Error body format:**
```html
<html><body><h1>{status_code} {status_text}</h1></body></html>
```

---

## Module: `server.glyph` — Static File Server

### Function: `handle_request(request: HttpRequest, doc_root: str) -> String`

Top-level request handler. Takes a parsed request and document root path, returns a complete HTTP response string.

**Logic:**
1. If `request.valid == 0` → return 400 Bad Request
2. If method is not "GET" and not "HEAD" → return 405 Method Not Allowed
3. Call `resolve_path(request.path, doc_root)` to get filesystem path
4. If path is empty (security violation) → return 403 Forbidden
5. Try to open and read the file
6. If file not found → return 404 Not Found
7. Determine MIME type from extension via `guess_content_type(path)`
8. If method is "HEAD" → return head response (headers only, Content-Length = body length)
9. Return 200 OK with file contents as body

### Function: `resolve_path(url_path: str, doc_root: str) -> String`

Converts a URL path to a safe filesystem path.

**Security rules (reject, never sanitize):**
1. Reject if path does not start with `"/"` → return empty string
2. Reject if path contains `".."` → return empty string
3. Reject if path contains `"\0"` → return empty string
4. Reject dotfiles: if any path segment starts with `"."` (e.g. `/.git/config`, `/.env`) → return empty string
5. Strip leading `/` from url_path
6. If path is empty or ends with `"/"` → append `"index.html"`
7. Concatenate: `doc_root + "/" + cleaned_path`
8. **Belt-and-suspenders check**: verify the resolved path still starts with `doc_root` prefix

**No directory listing.** If a path resolves to a directory without an index file, return 404.

Returns empty String on security violation (caller checks `.len() == 0`).

### Function: `guess_content_type(path: str) -> str`

Returns MIME type string based on file extension.

**Supported types:**
| Extension | MIME Type |
|-----------|-----------|
| `.html`   | `text/html` |
| `.css`    | `text/css` |
| `.js`     | `application/javascript` |
| `.json`   | `application/json` |
| `.txt`    | `text/plain` |
| `.svg`    | `image/svg+xml` |
| `.xml`    | `application/xml` |
| default   | `application/octet-stream` |

Implementation: use `string_index_of(path, ".")` to find last dot, then `.slice()` to extract extension, match against known types.

### Function: `read_file(path: str) -> Result<String, String>`

Reads file contents. Wraps File::open → read_to_string → close with error handling.

Returns `Ok(contents)` or `Err(error_message)`.

---

## Module: `main.glyph` — Entry Point

### Function: `main() -> i32`

Server entry point.

**Startup:**
1. Print banner: `"Apex serving <doc_root> on <host>:<port>"`
2. Bind TCP listener: `tcp_listen(host, port, 128)`
3. On bind failure → print error, exit 1

**Accept loop (infinite):**
```
while true {
  accept connection → TcpStream
  recv request data (8192 bytes max)
  parse request → HttpRequest
  handle request → response String  
  send response
  close connection
}
```

**Configuration (hardcoded v1):**
- Host: `"0.0.0.0"`
- Port: `8080`
- Document root: `"www"`

Error handling in accept loop: if accept/recv fails, log and continue to next connection. Never crash the server loop.

**Access logging:** Print one line per request to stdout: `METHOD PATH STATUS_CODE`. This is the access log. Errors (accept/recv failures) go to stderr via `puts`. Keep it simple — if you can grep it, it's good enough.

**Logging function in `server.glyph`:**
- `log_request(method: str, path: str, status: i32)` — prints `"GET /index.html 200"` format
- Called from `handle_request` before returning the response

---

## Data Flow

```
Client                  Apex
  |                      |
  |--- TCP connect ----->|  accept() → TcpStream
  |--- HTTP request ---->|  recv() → raw string
  |                      |  parse_request() → HttpRequest
  |                      |  handle_request() → resolve path, read file, build response
  |<-- HTTP response ----|  send() → response string  
  |--- connection closed-|  close()
```

## Task Breakdown & Ownership

| Task | Module | Owner | Depends On |
|------|--------|-------|------------|
| #6 Scaffolding (glyph.toml, empty files, www/) | all | junior-dev | #1 |
| #2 HTTP request parser | http.glyph | senior-dev | #6 |
| #3 HTTP response builder | http.glyph | junior-dev | #6 |
| #4 Static file server | server.glyph | senior-dev | #2, #3 |
| #5 Main loop + CLI | main.glyph | senior-dev | #2, #3, #4 |
| #7 Integration tests | tests | test-engineer | #2, #3, #5 |
| #8 README | README.md | junior-dev | #5 |
