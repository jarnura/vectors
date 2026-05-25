// Minimal static file server for local development.
// Usage: node dev-server.js  → serves cwd on 0.0.0.0:47474

import { createServer } from "node:http";
import { readFile } from "node:fs/promises";
import { extname } from "node:path";

const PORT = 47474;
const HOST = "0.0.0.0";

const MIME = {
  ".html": "text/html",
  ".js":   "text/javascript",
  ".css":  "text/css",
  ".json": "application/json",
  ".svg":  "image/svg+xml",
  ".ico":  "image/x-icon",
};

createServer(async (req, res) => {
  const path = req.url === "/" ? "/index.html" : req.url;
  const file = "." + path;
  const type = MIME[extname(file)] || "application/octet-stream";
  try {
    const data = await readFile(file);
    res.writeHead(200, { "content-type": type });
    res.end(data);
  } catch {
    res.writeHead(404);
    res.end("not found");
  }
}).listen(PORT, HOST, () => {
  console.log(`http://localhost:${PORT}`);
});
