# -*- coding: utf-8 -*-
from http.server import SimpleHTTPRequestHandler
from socketserver import TCPServer
from sys import argv

port = 8000

if len(argv) == 2:
	port = int(argv[1])


handler = SimpleHTTPRequestHandler
handler.extensions_map = {
	'.manifest': 'text/cache-manifest',
	'.html': 'text/html',
	'.png': 'image/png',
	'.jpg': 'image/jpg',
	'.svg':	'image/svg+xml',
	'.css':	'text/css',
	'.js':	'application/x-javascript',
	'.wasm': 'application/wasm',
	'': 'application/octet-stream'
}

try:
	httpd = TCPServer(("localhost", port), handler)
	print(f"serving at port {port}")
	httpd.serve_forever()
except KeyboardInterrupt:
	print("Server stopped, good bye!")

