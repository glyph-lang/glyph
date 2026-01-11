# std_net.txt — Glyph Standard Library Networking (std::net) Design Doc

## Scope
Provide a minimal, safe(ish), token-efficient networking API for Glyph:
- TCP client/server
- basic DNS resolution
- blocking I/O in v0, with a path to non-blocking + event loops
- cross-platform: Unix (Linux/macOS) first; Windows next

Non-goals (v0):
- TLS (do via library: mbedTLS/OpenSSL/rustls-equivalent later)
- async/await runtime
- full-featured sockets API surface (sockopts zoo)
- UDP (can be v0.2)

---

## High-level architecture
Layering (mirrors stdlib split):
- core: fundamental types, no OS calls
- alloc: String/Vec/Box
- std::sys::net::{unix, win}: platform bindings and error translation
- std::net: public, safe wrapper API (RAII, Result, minimal footguns)

API policy:
- Keep public API small and orthogonal.
- Provide escape hatches to raw handles and syscalls for power users.
- Keep naming conventional: TcpStream, TcpListener, SocketAddr.

---

## Platform strategy
### Unix (Linux/macOS)
Use libc/POSIX socket API:
- socket, connect, bind, listen, accept
- getaddrinfo/freeaddrinfo for DNS + service resolution
- read/write or recv/send
- close
- setsockopt, fcntl (later, for non-blocking / timeouts)

Linking:
- link against libc / libSystem automatically by toolchain.

### Windows
Use WinSock2:
- WSAStartup / WSACleanup (handled once by std)
- socket, connect, bind, listen, accept
- recv, send, closesocket
- getaddrinfo/freeaddrinfo
- WSAGetLastError for error codes

Linking:
- link against ws2_32

---

## FFI boundary requirements (language-level)
Glyph must support:
- extern "C" declarations
- raw pointers (*T, *mut T)
- fixed-width ints (i32/u32/u64/i64 etc.)
- repr(C) for structs used across FFI
- link annotations or build-system linking flags

No macros required.

---

## Error model
Public API uses:
- Result[T, NetErr]

NetErr is a compact ADT:
  enum NetErr {
    Os(code: i32)          // errno on Unix, WSA error on Windows
    InvalidAddr            // parse/format errors in std::net layer
    DnsFail                // getaddrinfo failed (still may carry Os)
    WouldBlock             // when non-blocking is introduced
    TimedOut               // when timeouts are introduced
    Closed                 // peer closed / EOF in read wrappers (optional)
  }

Design notes:
- Preserve raw OS code for debugging.
- Provide NetErr.message() as best-effort string (stdlib mapping table optional).

---

## Address types
### IpAddr
  enum IpAddr {
    V4(u32)     // network byte order or host order? (choose host order + helpers)
    V6([u8;16])
  }

### SocketAddr
  struct SocketAddr { ip: IpAddr, port: u16 }

### Parsing / formatting
- SocketAddr.parse("1.2.3.4:80") -> Result[SocketAddr, NetErr]
- SocketAddr.to_str() -> str (allocating) OR fmt integration

FFI conversion lives in std::sys::net:
- sockaddr_in / sockaddr_in6 for Unix
- SOCKADDR_IN / SOCKADDR_IN6 for Windows
- Provide internal helpers to pack/unpack.

---

## Public API surface (v0)
### TcpStream
  struct TcpStream { handle: RawSocket }     // RawSocket = i32 on Unix, usize on Win (opaque)

Methods:
- fn connect(addr: &str) -> Result[TcpStream, NetErr]
  - addr supports:
    - "host:port" (DNS via getaddrinfo)
    - "ip:port" (fast path)
- fn connect_addr(addr: SocketAddr) -> Result[TcpStream, NetErr]

I/O:
- fn read(self: &mut Self, buf: &mut [u8]) -> Result[usize, NetErr]
- fn write(self: &mut Self, buf: &[u8]) -> Result[usize, NetErr]
- fn flush(self: &mut Self) -> Result[(), NetErr]   // no-op for TCP; present for trait parity

Convenience:
- fn shutdown(self: &mut Self, how: Shutdown) -> Result[(), NetErr]

Ownership / RAII:
- impl Drop for TcpStream: closes socket

### TcpListener
  struct TcpListener { handle: RawSocket }

Methods:
- fn bind(addr: &str) -> Result[TcpListener, NetErr]
- fn bind_addr(addr: SocketAddr) -> Result[TcpListener, NetErr]
- fn accept(self: &mut Self) -> Result[(TcpStream, SocketAddr), NetErr]
- fn local_addr(self: &Self) -> Result[SocketAddr, NetErr]  (optional)

Ownership / RAII:
- impl Drop for TcpListener: closes socket

### Shutdown
  enum Shutdown { Read, Write, Both }

### Raw handles (escape hatch)
- type RawSocket = i64 | usize (opaque alias per target in std::sys)
- fn TcpStream.into_raw(self) -> RawSocket        // transfers ownership (no Drop)
- fn TcpStream.from_raw(h: RawSocket) -> TcpStream
- fn TcpListener.into_raw / from_raw similarly

---

## DNS / resolution behavior
connect("host:port"):
- parse host/port
- call getaddrinfo(host, port, hints)
- iterate returned addresses:
  - try socket() + connect()
  - stop on first success
- return last error if all fail

Policy:
- prefer IPv6 then IPv4 (or configurable later)
- no caching in v0
- keep allocation minimal (reuse small buffers; but correctness first)

Optional exported helper (v0.1):
- fn resolve(host: &str, port: u16) -> Result[Vec[SocketAddr], NetErr]

---

## Blocking vs non-blocking (evolution plan)
### v0: Blocking only
- simplest semantics: read/write block until progress or error
- accept blocks until connection
- connect blocks until established or error

### v0.2: Non-blocking mode + WouldBlock
Add:
- fn set_nonblocking(self: &mut Self, on: b) -> Result[(), NetErr]

Unix implementation:
- fcntl(fd, F_SETFL, O_NONBLOCK)

Windows implementation:
- ioctlsocket(FIONBIO)

### v0.3: Polling primitives (still no async runtime)
Expose minimal poll layer:
- enum PollEvent { Readable, Writable, Error }
- struct Poll { ... }
- fn Poll.add(sock: RawSocket, events: ...) -> ...
- fn Poll.wait(timeout_ms: i32) -> Result[Vec[PollResult], NetErr]

Backend mapping:
- Linux: poll/epoll
- macOS: poll/kqueue
- Windows: WSAPoll (or select as fallback)

---

## Timeouts (future)
Add:
- set_read_timeout(ms: i32)
- set_write_timeout(ms: i32)
- set_connect_timeout(ms: i32)   // may require non-blocking connect + poll

Unix: setsockopt(SO_RCVTIMEO/SO_SNDTIMEO) or poll-based
Windows: setsockopt or poll-based

---

## Implementation plan (suggested)
1) Implement std::sys::net::unix
   - minimal FFI for socket/connect/bind/listen/accept/read/write/close/getaddrinfo
   - errno capture via *__errno_location / errno symbol access (platform-specific)
   - translate to NetErr::Os(code)

2) Implement std::net wrappers
   - TcpStream/TcpListener structs + Drop
   - connect/bind helpers
   - parse host:port fast path for numeric IPs
   - tests: connect localhost, echo server, accept loop

3) Add Windows backend once core stabilizes
   - WSA init done in std once (lazy singleton)
   - map WSAGetLastError to NetErr::Os

4) Add non-blocking + WouldBlock as a clean extension (no breaking API)

---

## Safety + correctness notes
- Ensure Drop is idempotent-safe after into_raw moves ownership.
- Handle EINTR by retrying syscalls where appropriate (read/write/accept/connect).
- Handle partial writes: write returns number written; provide write_all helper later.
- EOF semantics:
  - read() returning 0 indicates peer closed; either return Ok(0) or NetErr::Closed (choose Ok(0) for POSIX compatibility).
- Avoid implicit allocations in hot paths; keep conversions internal.

---

## Token-efficiency / ergonomics notes
- Keep method names conventional and short: connect/bind/accept/read/write.
- Provide connect(addr: &str) as primary entry point for LLM simplicity.
- Avoid requiring explicit builder objects or many config calls in v0.
- Prefer Result + ? for error propagation.

---

## Example usage
### TCP client
  fn main() -> Result[(), NetErr] {
    let mut s = std::net::TcpStream.connect("example.com:80")?
    s.write(b"GET / HTTP/1.0\r\n\r\n")?
    let mut buf = [0u8; 4096]
    let n = s.read(&mut buf)?
    std::io.print_bytes(&buf[..n])
    Ok(())
  }

### TCP server
  fn main() -> Result[(), NetErr] {
    let mut l = std::net::TcpListener.bind("0.0.0.0:8080")?
    while true {
      let (mut c, a) = l.accept()?
      c.write(b"hello\n")?
    }
  }

End of file.
